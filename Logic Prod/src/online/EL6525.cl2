       ID DIVISION.
       PROGRAM-ID. EL6525.
      *AUTHOR.     PABLO.
      *            COLLEYVILLE, TEXAS.

      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *                                                                *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *                                                                 
      *REMARKS.  TRANSACTION - EXDF - COMPENSATION BILLING INSTRUCTIONS
      *                                                                 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
081808* 081808    2008061100001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
                                                                        
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    EL6525 WORKING STORAGE    *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
                                                                        
       01  WS-DATE-AREA.                                                
           05  WS-SAVE-DATE                PIC X(8) VALUE SPACES.       
           05  WS-SAVE-BIN-DT              PIC XX   VALUE SPACES.       
           05  WS-EFF-YMD                  PIC X(6).                    
           05  WS-TRM-YMD                  PIC X(6).                    
           05  SUPPRESS-MAP-SW             PIC X    VALUE SPACE.        
               88  DO-NOT-MOVE-TO-MAP          VALUE 'N'.               
               88  MOVE-TO-MAP                 VALUE 'Y'.               
           05  MAP-CHANGED-SW              PIC X    VALUE 'N'.          
               88  MAP-NOT-CHANGED             VALUE 'N'.               
               88  MAP-CHANGED                 VALUE 'Y'.               
           05  BILL-INST-SW                PIC X.
               88  BILL-INST-CHANGED           VALUE 'Y'.

       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

       01  STANDARD-AREAS.                                              
           12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.  
           12  WS-SUB                      PIC S9(4) COMP VALUE +0.     
           12  MAP-NAME                    PIC X(8) VALUE 'EL6525A'.    
           12  MAPSET-NAME                 PIC X(8) VALUE 'EL6525S'.    
           12  SCREEN-NUMBER               PIC X(4) VALUE '652F'.       
           12  TRANS-ID                    PIC X(4) VALUE 'EXDF'.       
           12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.       
           12  THIS-PGM                    PIC X(8) VALUE 'EL6525'.     
           12  PGM-NAME                    PIC X(8).                    
           12  TIME-IN                     PIC S9(7).                   
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER                  PIC X.                       
               16  TIME-OUT                PIC 99V99.                   
               16  FILLER                  PIC XX.                      
           12  XCTL-005                    PIC X(8) VALUE 'EL005'.      
           12  XCTL-010                    PIC X(8) VALUE 'EL010'.      
           12  XCTL-626                    PIC X(8) VALUE 'EL626'.      
           12  XCTL-611                    PIC X(8) VALUE 'EL611'.      
           12  XCTL-652                    PIC X(8) VALUE 'EL652'.      
           12  LINK-001                    PIC X(8) VALUE 'EL001'.      
           12  LINK-004                    PIC X(8) VALUE 'EL004'.      
           12  QID.                                                     
               16  QID-TERM                PIC X(4) VALUE SPACES.       
               16  FILLER                  PIC X(4) VALUE '525A'.       
           12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.  
           12  ERCOBI-FILE-ID              PIC X(8) VALUE 'ERCOBI'.     
           12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.     
           12  ELACHP-FILE-ID              PIC X(8) VALUE 'ELACHP'.     
           12  ELBANK-FILE-ID              PIC X(8) VALUE 'ELBANK'.     

           12  DEEDIT-FIELD                PIC X(15).                   
           12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      

           12  WS-BILL-INST-SW            PIC X  VALUE SPACES.
               88  BILLING-INSTRUCTIONS-FOUND  VALUE 'Y'.
           12  ERROR-MESSAGES.                                          
               16  ER-0000                 PIC X(4) VALUE '0000'.       
               16  ER-0004                 PIC X(4) VALUE '0004'.       
               16  ER-0008                 PIC X(4) VALUE '0008'.       
               16  ER-0029                 PIC X(4) VALUE '0029'.       
               16  ER-0068                 PIC X(4) VALUE '0068'.       
               16  ER-0070                 PIC X(4) VALUE '0070'.       
               16  ER-0348                 PIC X(4) VALUE '0348'.       
               16  ER-0454                 PIC X(4) VALUE '0454'.       
               16  ER-0876                 PIC X(4) VALUE '0876'.
               16  ER-1228                 PIC X(4) VALUE '1228'.       
               16  ER-1626                 PIC X(4) VALUE '1626'.       
               16  ER-1629                 PIC X(4) VALUE '1629'.       
               16  ER-2039                 PIC X(4) VALUE '2039'.       
               16  ER-2056                 PIC X(4) VALUE '2056'.
               16  ER-2057                 PIC X(4) VALUE '2057'.
               16  ER-2233                 PIC X(4) VALUE '2233'.       
               16  ER-7430                 PIC X(4) VALUE '7430'.       
               16  ER-7431                 PIC X(4) VALUE '7431'.       
               16  ER-7432                 PIC X(4) VALUE '7432'.       
               16  ER-7434                 PIC X(4) VALUE '7434'.       
               16  ER-7435                 PIC X(4) VALUE '7435'.       
               16  ER-7436                 PIC X(4) VALUE '7436'.       
               16  ER-7438                 PIC X(4) VALUE '7438'.       
               16  ER-7440                 PIC X(4) VALUE '7440'.       
               16  ER-7447                 PIC X(4) VALUE '7447'.       
               16  ER-7449                 PIC X(4) VALUE '7449'.       
               16  ER-7462                 PIC X(4) VALUE '7462'.       
               16  ER-7465                 PIC X(4) VALUE '7465'.       
               16  ER-7468                 PIC X(4) VALUE '7468'.       
               16  ER-7469                 PIC X(4) VALUE '7469'.       
               16  ER-8799                 PIC X(4) VALUE '8799'.
               16  ER-9388                 PIC X(4) VALUE '9388'.       
               16  ER-9399                 PIC X(4) VALUE '9399'.       
               16  ER-9999                 PIC X(4) VALUE '9999'.
                                                                        
           12  ELCNTL-KEY.                                              
               16  CNTL-COMP-ID            PIC X(3) VALUE SPACES.       
               16  CNTL-REC-TYPE           PIC X    VALUE SPACES.       
               16  CNTL-ACCESS             PIC X(4) VALUE SPACES.       
               16  CNTL-SEQ-NO             PIC S9(4) VALUE +0  COMP.    
                                                                        

           COPY ELCLOGOF.                                               

           COPY ELCDATE.                                                

           COPY ELCATTR.                                                

           COPY ELCEMIB.                                                

           COPY ELCSCTM.                                                

           COPY ELCSCRTY.                                               

           COPY ELCINTF.                                                
           12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           
               16  PI-MAINT                PIC  X.                      
               16  PI-CHECK-TYPE           PIC  X.                      
               16  PI-CHECK-CARRY-BAL      PIC  X.                      
               16  PI-FIRST-TIME-SW        PIC  X.                      
                   88  FIRST-TIME                  VALUE 'Y'.           
               16  PI-ERCOMP-EOF-SW        PIC  X.                      
                   88  ERCOMP-EOF                  VALUE 'Y'.           
               16  PI-SAVE-PHONE           PIC  X(10).                  
               16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
               16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.   
               16  PI-ERCOMP-KEY.                                       
                   20  PI-ERC-GROUP-CD     PIC  X.                      
                   20  PI-ERC-CARRIER      PIC  X.                      
                   20  PI-ERC-GROUP        PIC  X(6).                   
                   20  PI-ERC-RESP         PIC  X(10).                  
                   20  PI-ERC-ACCT         PIC  X(10).                  
                   20  PI-ERC-TYPE         PIC  X.                      
               16  PI-SAVE-ERCOMP-KEY      PIC  X(29).                  
               16  PI-BANK-TRANSIT-NUMBER.                              
                   20  PI-BANK-COMPANY-CD  PIC X.                       
                   20  PI-FEDERAL-NUMBER   PIC X(4).                    
                   20  PI-BANK-NUMBER      PIC X(4).                    
               16  PI-BANK-ACCOUNT-NO      PIC X(17).                   
               16  PI-BANK-ACTION-CODE     PIC X.
               16  PI-ERCOBI-KEY.
                   20  PI-ERCOBI-COMPANY-CD PIC X.
                   20  PI-ERCOBI-STMT-OWNER PIC X(4).
                   20  PI-ERCOBI-RGID      PIC X(12).
               16  PI-SAVE-ERCOBI-KEY      PIC X(17).
               16  FILLER                  PIC  X(500).                 

           COPY ELCAID.                                                 

       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER                      PIC X(8).                    
           12  PF-VALUES                   PIC X       OCCURS 2.        

                                       COPY EL6525S.

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 

           COPY ERCCOMP.                                                
           COPY ERCCOBI.
           COPY ELCACHP.                                                
           COPY ELCBANK.                                                

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
           MOVE '5'                    TO DC-OPTION-CODE.               
           PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.                    
           MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.                 
           MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.               
                                                                        
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
           MOVE EIBTRMID               TO QID-TERM.                     
           MOVE +2                     TO EMI-NUMBER-OF-LINES.          
           MOVE SPACE                  TO SUPPRESS-MAP-SW.              
                                                                        
           IF EIBCALEN = 0                                              
               GO TO 8800-UNAUTHORIZED-ACCESS.                          
                                                                        
           MOVE LOW-VALUES             TO EL6525AI.                     
                                                                        
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
               MOVE PI-SAVED-PROGRAM-5 TO  PI-SAVED-PROGRAM-6           
               MOVE PI-SAVED-PROGRAM-4 TO  PI-SAVED-PROGRAM-5           
               MOVE PI-SAVED-PROGRAM-3 TO  PI-SAVED-PROGRAM-4           
               MOVE PI-SAVED-PROGRAM-2 TO  PI-SAVED-PROGRAM-3           
               MOVE PI-SAVED-PROGRAM-1 TO  PI-SAVED-PROGRAM-2           
               MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1        
               MOVE PI-CALLING-PROGRAM TO  PI-RETURN-TO-PROGRAM         
               MOVE THIS-PGM           TO  PI-CALLING-PROGRAM.          

           IF EIBTRNID NOT = TRANS-ID
              MOVE 'S'                 TO MAINTYPO
              MOVE AL-UANON            TO MAINTYPA
              MOVE -1                  TO MAINTYPL
              MOVE PI-ERCOBI-STMT-OWNER
                                       TO DELTOI
              MOVE +4                  TO DELTOL
              MOVE AL-UANON            TO DELTOA
              MOVE PI-ERCOBI-RGID      TO RGIDI
              MOVE +12                 TO RGIDL
              MOVE AL-UANON            TO RGIDA
              GO TO 4000-SHOW
           END-IF

           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR  (9600-PGMID-ERROR)                             
               ERROR     (9990-ABEND)                                   
           END-EXEC.                                                    
                                                                        
           IF EIBAID = DFHCLEAR                                         
               GO TO 9400-CLEAR.                                        

           .
       0200-RECEIVE.                                                    
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
               MOVE ER-0008            TO EMI-ERROR                     
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                 TO PFENTERL                      
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           EXEC CICS RECEIVE                                            
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               INTO     (EL6525AI)                                      
           END-EXEC.                                                    
                                                                        
           IF PFENTERL = 0                                              
               GO TO 0300-CHECK-PFKEYS.                                 
                                                                        
           IF EIBAID NOT = DFHENTER                                     
               MOVE ER-0004            TO EMI-ERROR                     
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
           IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   
               MOVE PF-VALUES (PFENTERI) TO EIBAID                      
           ELSE                                                         
               MOVE ER-0029              TO EMI-ERROR                   
               GO TO 0320-INPUT-ERROR.                                  

       0300-CHECK-PFKEYS.                                               

           IF EIBAID = DFHPF23                                          
               GO TO 8810-PF23.                                         
           IF EIBAID = DFHPF24                                          
               GO TO 9200-RETURN-MAIN-MENU.                             
           IF EIBAID = DFHPF12                                          
               GO TO 9500-PF12.                                         
           IF EIBAID = DFHENTER                                         
               GO TO 0330-CHECK-MAINTYP.                                
                                                                        
           MOVE ER-0029                TO EMI-ERROR.                    
       0320-INPUT-ERROR.                                                

           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE AL-UNBON               TO PFENTERA.                     
           MOVE -1                     TO PFENTERL.                     
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       0330-CHECK-MAINTYP.                                              
                                                                        
           IF MAINTYPL > 0
              IF MAINTYPI = 'S' OR 'C' OR 'A' OR 'D'
                 MOVE AL-UANON       TO MAINTYPA                      
                 MOVE MAINTYPI       TO PI-MAINT                      
              ELSE                                                     
                 MOVE -1             TO MAINTYPL                      
                 MOVE AL-UABON       TO MAINTYPA                      
                 MOVE ER-2039        TO EMI-ERROR                     
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
                 GO TO 8200-SEND-DATAONLY
              END-IF
           ELSE                                                         
              MOVE -1                 TO MAINTYPL                      
              MOVE AL-UABON           TO MAINTYPA                      
              MOVE ER-2039            TO EMI-ERROR                     
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           IF DELTOL > 0
              MOVE DELTOI              TO PI-ERCOBI-STMT-OWNER
           END-IF

           IF RGIDL > 0
              MOVE RGIDI               TO PI-ERCOBI-RGID
           END-IF

           IF PI-MAINT = 'S'                                            
               GO TO 4000-SHOW.                                         
                                                                        
           IF PI-MAINT = 'D'
              GO TO 4600-DELETE
           END-IF

           IF PI-MAINT = 'A'
              GO TO 4200-ADD
           END-IF

           IF PI-MAINT = 'C'
              GO TO 4400-CHANGE
           END-IF

           .
       4000-SHOW.                                                       
                                                                        
           MOVE PI-COMPANY-CD          TO PI-ERCOBI-COMPANY-CD
           IF DELTOL > 0
              MOVE DELTOI              TO PI-ERCOBI-STMT-OWNER
           END-IF
           IF RGIDL > 0
              MOVE RGIDI               TO PI-ERCOBI-RGID
           END-IF
           PERFORM 7100-READ-ERCOBI    THRU 7100-EXIT

           IF RESP-NOTOPEN
              MOVE ER-2233             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           ELSE
              IF RESP-NOTFND
                 MOVE ER-7462          TO EMI-ERROR
                 MOVE -1               TO DELTOL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE LOW-VALUES             TO EL6525AO
                                                                        
           GO TO 5000-BUILD-INITIAL-SCREEN

           .
       4200-ADD.                                                        

           IF PI-ERCOBI-KEY = PI-SAVE-ERCOBI-KEY
              MOVE ER-2057             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           PERFORM 7000-EDIT  THRU  7000-EXIT.                          
                                                                        
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 7100-READ-ERCOBI    THRU 7100-EXIT

           IF RESP-NOTOPEN
              MOVE ER-2233             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           ELSE
              IF RESP-NOTFND
                 GO TO 4250-CONT
              END-IF
           END-IF
           
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE ER-2057                TO  EMI-ERROR.                   
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
                                                                        
           MOVE LOW-VALUES             TO  PI-SAVE-ERCOBI-KEY.          
                                                                        
           MOVE -1                     TO  MAINTYPL.                    
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       4250-CONT.                                                       

           PERFORM 7150-ERCOBI-GETMAIN  THRU  7150-EXIT.                
                                                                        
           MOVE SPACES                 TO  COMP-BILLING-INSTRUCTIONS
           MOVE 'BL'                   TO BL-RECORD-ID
           MOVE PI-COMPANY-CD          TO BL-COMPANY-CD
                                                                        
           PERFORM 6000-CHECK-FOR-UPDATE  THRU  6000-EXIT
                                                                        
           MOVE PI-PROCESSOR-ID        TO  BL-LAST-MAINT-USER.          
           MOVE EIBTIME                TO  BL-LAST-MAINT-HHMMSS.        
                                                                        
           MOVE WS-SAVE-BIN-DT         TO  BL-LAST-MAINT-DT             

           EXEC CICS WRITE                                              
               DATASET  (ERCOBI-FILE-ID)                                  
               FROM     (COMP-BILLING-INSTRUCTIONS)
               RIDFLD   (BL-CONTROL-PRIMARY)                            
           END-EXEC.                                                    

           MOVE ER-0000                TO  EMI-ERROR.                   
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
                                                                        
           MOVE LOW-VALUES             TO  EL6525AO
           MOVE PI-ERCOBI-STMT-OWNER   TO DELTOO
           MOVE PI-ERCOBI-RGID         TO RGIDO
           MOVE AL-UANON               TO DELTOA RGIDA
                                                                        
           GO TO 5000-BUILD-INITIAL-SCREEN.                             
                                                                        
       4200-EXIT.                                                       
           EXIT.                                                        

       4400-CHANGE.                                                     

           IF PI-ERCOBI-KEY = PI-SAVE-ERCOBI-KEY                        
              CONTINUE
           ELSE                                                         
              MOVE ER-2056             TO EMI-ERROR                    
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT               
              MOVE -1                  TO MAINTYPL                     
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           PERFORM 7000-EDIT  THRU  7000-EXIT
                                                                        
           IF EMI-NO-ERRORS                                             
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           PERFORM 7300-READ-ERCOBI-UPDATE  THRU  7300-EXIT
                                                                        
                                                                       
           PERFORM 6000-CHECK-FOR-UPDATE  THRU  6000-EXIT
                                                                        
           IF (BL-LAST-MAINT-USER   = PI-UPDATE-BY)
              OR (BL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (ERCOBI-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           MOVE PI-PROCESSOR-ID        TO  BL-LAST-MAINT-USER.          
           MOVE EIBTIME                TO  BL-LAST-MAINT-HHMMSS.        
                                                                        
           MOVE WS-SAVE-BIN-DT         TO  BL-LAST-MAINT-DT

           MOVE SPACE                  TO  DC-OPTION-CODE.              

           EXEC CICS REWRITE                                            
               DATASET  (ERCOBI-FILE-ID)                                  
               FROM     (COMP-BILLING-INSTRUCTIONS)
           END-EXEC

           MOVE ER-0000                TO  EMI-ERROR.                   
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
                                                                        
           MOVE LOW-VALUES             TO EL6525AO
           MOVE PI-ERCOBI-STMT-OWNER   TO DELTOO
           MOVE PI-ERCOBI-RGID         TO RGIDO
           MOVE AL-UANON               TO DELTOA RGIDA

           GO TO 5000-BUILD-INITIAL-SCREEN.                             
                                                                        
       4400-EXIT.                                                       
           EXIT.                                                        

       4600-DELETE.

           IF PI-ERCOBI-KEY = PI-SAVE-ERCOBI-KEY                        
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 7300-READ-ERCOBI-UPDATE THRU 7300-EXIT

           IF (BL-LAST-MAINT-USER   = PI-UPDATE-BY)
              AND (BL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE                                                         
              EXEC CICS UNLOCK                                         
                 DATASET  (ERCOBI-FILE-ID)                              
              END-EXEC                                                 
              MOVE ER-0068             TO EMI-ERROR                    
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT               
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS DELETE
              DATASET  (ERCOBI-FILE-ID)
           END-EXEC

           MOVE ER-0000                TO  EMI-ERROR                    
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   

           MOVE LOW-VALUES             TO EL6525AO                      
           MOVE PI-ERCOBI-STMT-OWNER   TO DELTOO
           MOVE PI-ERCOBI-RGID         TO RGIDO
           MOVE AL-UANON               TO DELTOA RGIDA
                                                                        
           MOVE LOW-VALUES             TO PI-SAVE-ERCOBI-KEY
                                                                        
           GO TO 8100-SEND-INITIAL-MAP                                  
           .                                                            
       4600-EXIT.                                                       
           EXIT.                                                        

       5000-BUILD-INITIAL-SCREEN.

           MOVE PI-ERCOBI-KEY          TO PI-SAVE-ERCOBI-KEY
           MOVE BL-LAST-MAINT-USER     TO PI-UPDATE-BY
                                                                        
           IF BL-LAST-MAINT-HHMMSS NUMERIC                              
              MOVE BL-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF

           MOVE BL-STMT-OWNER          TO DELTOO
           MOVE BL-REPORT-GROUP-ID     TO RGIDO
121208     MOVE BL-ACCOUNT-NAME        TO ACCTNO
           MOVE BL-CONTACT-NAME        TO CONTNO
           MOVE BL-ADDR1               TO ADDR1O
           MOVE BL-ADDR2               TO ADDR2O
           MOVE BL-CITY                TO CITYO
           MOVE BL-STATE               TO STO
           MOVE BL-ZIP                 TO ZIPO

           IF BL-CHECKS-SEPARATE
              MOVE 'X'                 TO SEPO
              MOVE ' '                 TO NETO
           ELSE
              MOVE 'X'                 TO NETO
              MOVE ' '                 TO SEPO
           END-IF

           MOVE BL-SI-LINE-1           TO SI1O
           MOVE BL-SI-LINE-2           TO SI2O
           MOVE BL-SI-LINE-3           TO SI3O
           MOVE BL-SI-LINE-4           TO SI4O
           MOVE BL-SI-LINE-5           TO SI5O


           MOVE PI-MAINT               TO MAINTYPO
           MOVE AL-UANOF               TO MAINTYPA
           MOVE -1                     TO MAINTYPL

           GO TO 8100-SEND-INITIAL-MAP

           .
       5099-EXIT.
           EXIT.

       6000-CHECK-FOR-UPDATE.                                           

           IF DELTOL > ZERO
              MOVE DELTOI              TO BL-STMT-OWNER
           END-IF
           IF RGIDL > ZERO
              MOVE RGIDI               TO BL-REPORT-GROUP-ID
           END-IF
121208     IF ACCTNL > ZERO
121208        MOVE ACCTNI              TO BL-ACCOUNT-NAME
121208     END-IF
           IF CONTNL > ZERO
              MOVE CONTNI              TO BL-CONTACT-NAME
           END-IF
           IF ADDR1L > ZERO
              MOVE ADDR1I              TO BL-ADDR1
           END-IF
           IF ADDR2L > ZERO
              MOVE ADDR2I              TO BL-ADDR2
           END-IF
           IF CITYL > ZERO
              MOVE CITYI               TO BL-CITY
           END-IF
           IF STL > ZERO
              MOVE STI                 TO BL-STATE
           END-IF
           IF ZIPL > ZERO
              MOVE ZIPI                TO BL-ZIP
           END-IF
           IF (NETL > ZERO)
              AND (NETI = 'X' OR 'Y')
              MOVE '1'                 TO BL-CHECK-HANDLING
           ELSE
              IF (SEPL > ZERO)
                 AND (SEPI = 'X' OR 'Y')
                 MOVE '2'              TO BL-CHECK-HANDLING
              END-IF
           END-IF
           IF SI1L > ZERO
              MOVE SI1I                TO BL-SI-LINE-1
           END-IF
           IF SI2L > ZERO
              MOVE SI2I                TO BL-SI-LINE-2
           END-IF
           IF SI3L > ZERO
              MOVE SI3I                TO BL-SI-LINE-3
           END-IF
           IF SI4L > ZERO
              MOVE SI4I                TO BL-SI-LINE-4
           END-IF
           IF SI5L > ZERO
              MOVE SI5I                TO BL-SI-LINE-5
           END-IF

           .
       6000-EXIT.
           EXIT.

       7000-EDIT.                                                       

           IF NETL > 0
              IF NETI = ' ' OR 'X' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1               TO NETL
                 MOVE AL-UABON         TO NETA
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF SEPL > 0
              IF SEPI = ' ' OR 'X' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1               TO SEPL
                 MOVE AL-UABON         TO SEPA
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           .
       7000-EXIT.
           EXIT.

       7100-READ-ERCOBI.                                                
                                                                        
           EXEC CICS READ                                               
                DATASET  (ERCOBI-FILE-ID)                               
                SET      (ADDRESS OF COMP-BILLING-INSTRUCTIONS)         
                RIDFLD   (PI-ERCOBI-KEY)                                
                RESP     (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
              MOVE BL-LAST-MAINT-USER     TO PI-UPDATE-BY
              MOVE BL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS
              MOVE BL-CONTROL-PRIMARY     TO PI-ERCOBI-KEY
              MOVE BL-STMT-OWNER          TO DELTOO
              MOVE BL-REPORT-GROUP-ID     TO RGIDO
           END-IF

           .
       7100-EXIT.                                                       
           EXIT.                                                        

       7150-ERCOBI-GETMAIN.

           EXEC CICS GETMAIN
              SET  (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
              LENGTH  (650)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       7150-EXIT.
           EXIT.

       7300-READ-ERCOBI-UPDATE.                                         
           EXEC CICS HANDLE CONDITION                                   
               NOTFND  (7300-NOTFND)                                    
               NOTOPEN (7300-NOTOPEN)                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
                DATASET  (ERCOBI-FILE-ID)                               
                SET      (ADDRESS OF COMP-BILLING-INSTRUCTIONS)         
                RIDFLD   (PI-ERCOBI-KEY)                                
                UPDATE                                                  
           END-EXEC.                                                    
           GO TO 7300-EXIT.                                             
                                                                        
       7300-NOTOPEN.                                                    
                                                                        
           MOVE ER-2233               TO EMI-ERROR                      
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 7300-EXIT.                                             
       7300-NOTFND.                                                     
                                                                        
           MOVE ER-7462               TO EMI-ERROR                      
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    

           .                                                                        
       7300-EXIT.                                                       
           EXIT.                                                        

                                                                        
      ******************************************************************
       8100-SEND-INITIAL-MAP.                                           
           MOVE WS-SAVE-DATE           TO DATEO.                        
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO TIMEO.                        
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
           MOVE -1                     TO MAINTYPL.                     
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
                                                                        
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL6525AO)                                      
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
       EJECT                                                            
       8200-SEND-DATAONLY.                                              

           IF EIBTRNID NOT = TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           END-IF


           MOVE WS-SAVE-DATE           TO DATEO.                        
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO TIMEO.                        
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      
                                                                        
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL6525AO)                                      
               DATAONLY                                                 
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
       EJECT                                                            
       8300-SEND-TEXT.                                                  
           EXEC CICS SEND TEXT                                          
               FROM     (LOGOFF-TEXT)                                   
               LENGTH   (LOGOFF-LENGTH)                                 
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       EJECT                                                            
       8600-DEEDIT.                                                     
           EXEC CICS BIF DEEDIT                                         
                FIELD(DEEDIT-FIELD)                                     
                LENGTH(15)                                              
           END-EXEC.                                                    
                                                                        
       EJECT                                                            
       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID                 TO PI-ENTRY-CD-1.                
           MOVE XCTL-005               TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
       EJECT                                                            
                                                                        
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         
           EXEC CICS RETURN                                             
               TRANSID    (TRANS-ID)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (WS-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9200-RETURN-MAIN-MENU.                                           
           MOVE XCTL-626               TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       EJECT                                                            
       9300-XCTL.                                                       
           EXEC CICS XCTL                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (WS-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-010               TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR    (8300-SEND-TEXT)                             
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
           MOVE ' '                    TO PI-ENTRY-CD-1.                
           MOVE XCTL-005               TO PGM-NAME.                     
           MOVE PGM-NAME               TO LOGOFF-PGM.                   
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
           GO TO 9300-XCTL.                                             
                                                                        
       9700-DATE-CONVERT.                                               
           EXEC CICS LINK                                               
               PROGRAM    ('ELDATCV')                                   
               COMMAREA   (DATE-CONVERSION-DATA)                        
               LENGTH     (DC-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9700-EXIT.                                                       
           EXIT.                                                        
                                                                        
       EJECT                                                            
       9900-ERROR-FORMAT.                                               
                                                                        
           IF NOT EMI-ERRORS-COMPLETE                                   
               MOVE LINK-001           TO PGM-NAME                      
               EXEC CICS LINK                                           
                   PROGRAM    (PGM-NAME)                                
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
                   LENGTH     (EMI-COMM-LENGTH)                         
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE LINK-004               TO PGM-NAME.                     
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
