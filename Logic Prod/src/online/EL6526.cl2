       ID DIVISION.
       PROGRAM-ID. EL6526.
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
      *REMARKS.  TRANSACTION - EXDG - RDS EMAIL ADDRESS DISPLAY ONLY   
      *                                                                 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
022211* 022211    2010012100001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
                                                                        
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    EL6526 WORKING STORAGE    *'. 
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

       01  EADR-KEY.
           05  EADR-COMPANY-CD                   PIC X.
           05  EADR-REC-TYPE                     PIC XX.
           05  EADR-ERCOMP-KEY.
               10  EADR-CARRIER                  PIC X.
               10  EADR-GROUPING                 PIC X(6).
               10  EADR-FIN-RESP                 PIC X(10).
               10  EADR-ACCOUNT                  PIC X(10).
               10  EADR-TYPE                     PIC X.
               10  FILLER                        PIC X(17).

       01  STANDARD-AREAS.                                              
           12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.  
           12  WS-SUB                      PIC S9(4) COMP VALUE +0.     
           12  MAP-NAME                    PIC X(8) VALUE 'EL6526A'.    
           12  MAPSET-NAME                 PIC X(8) VALUE 'EL6526S'.    
           12  SCREEN-NUMBER               PIC X(4) VALUE '652G'.       
           12  TRANS-ID                    PIC X(4) VALUE 'EXDG'.       
           12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.       
           12  THIS-PGM                    PIC X(8) VALUE 'EL6526'.     
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
               16  FILLER                  PIC X(4) VALUE '526A'.       
           12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.  
           12  EREADR-FILE-ID              PIC X(8) VALUE 'EREADR'.
           12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.

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

                                       COPY EL6526S.

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 

                                       COPY ERCEADR.

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
           MOVE '5'                    TO DC-OPTION-CODE.               
           PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.                    
           MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.                 
           MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.               
                                                                        
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
           MOVE EIBTRMID               TO QID-TERM.                     
           MOVE +1                     TO EMI-NUMBER-OF-LINES.          
           MOVE SPACE                  TO SUPPRESS-MAP-SW.              
                                                                        
           IF EIBCALEN = 0                                              
               GO TO 8800-UNAUTHORIZED-ACCESS.                          
                                                                        
           MOVE LOW-VALUES             TO EL6526AI.                     
                                                                        
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
               INTO     (EL6526AI)                                      
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

           .
       4000-SHOW.                                                       

           MOVE PI-COMPANY-CD          TO EADR-KEY
           MOVE 'CO'                   TO EADR-REC-TYPE
           MOVE PI-ERC-CARRIER         TO EADR-CARRIER
           MOVE PI-ERC-GROUP           TO EADR-GROUPING
           MOVE PI-ERC-RESP            TO EADR-FIN-RESP
           MOVE PI-ERC-ACCT            TO EADR-ACCOUNT
           MOVE PI-ERC-TYPE            TO EADR-TYPE

           display ' key = ' eadr-key (2:47)

           EXEC CICS READ
              DATASET   (EREADR-FILE-ID)
              SET       (ADDRESS OF EMAIL-ADDRESS)
              RIDFLD    (EADR-KEY)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              display ' not normal ' ws-response
              MOVE ER-9999             TO EMI-ERROR
              MOVE -1                  TO CARRIERL
              MOVE AL-SABOF            TO CARRIERA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           display ' good read ' ea-co-account
           display ' ea email ' ea-persons-email (1)

           MOVE LOW-VALUES             TO EL6526AO

           MOVE EA-CO-CARRIER          TO CARRIERO
           MOVE EA-CO-GROUPING         TO GROUPO
           MOVE EA-CO-FIN-RESP         TO FINRESPO
           MOVE EA-CO-ACCOUNT          TO COACCTO
           MOVE EA-CO-TYPE             TO TYPEO

           MOVE EA-PERSONS-NAME (1)    TO NAME1O
           MOVE EA-PERSONS-EMAIL (1)   TO EADR1O
           MOVE EA-PERSONS-NAME (2)    TO NAME2O
           MOVE EA-PERSONS-EMAIL (2)   TO EADR2O
           MOVE EA-PERSONS-NAME (3)    TO NAME3O
           MOVE EA-PERSONS-EMAIL (3)   TO EADR3O
           MOVE EA-PERSONS-NAME (4)    TO NAME4O
           MOVE EA-PERSONS-EMAIL (4)   TO EADR4O
           MOVE EA-PERSONS-NAME (5)    TO NAME5O
           MOVE EA-PERSONS-EMAIL (5)   TO EADR5O
           MOVE EA-PERSONS-NAME (6)    TO NAME6O
           MOVE EA-PERSONS-EMAIL (6)   TO EADR6O
           MOVE EA-PERSONS-NAME (7)    TO NAME7O
           MOVE EA-PERSONS-EMAIL (7)   TO EADR7O
           MOVE EA-PERSONS-NAME (8)    TO NAME8O
           MOVE EA-PERSONS-EMAIL (8)   TO EADR8O
           MOVE EA-PERSONS-NAME (9)    TO NAME9O
           MOVE EA-PERSONS-EMAIL (9)   TO EADR9O
           MOVE EA-PERSONS-NAME (10)   TO NAME10O
           MOVE EA-PERSONS-EMAIL (10)  TO EADR10O

           MOVE AL-SANOF               TO CARRIERA
           MOVE -1                     TO CARRIERL

           GO TO 8100-SEND-INITIAL-MAP

           .
       5099-EXIT.
           EXIT.


      ******************************************************************
       8100-SEND-INITIAL-MAP.                                           

           display ' made 8100 '

           MOVE WS-SAVE-DATE           TO DATEO.                        
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO TIMEO.                        
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
           MOVE -1                     TO CARRIERL
      *    MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
                                                                        
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL6526AO)                                      
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      

       8200-SEND-DATAONLY.                                              

           IF EIBTRNID NOT = TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           MOVE WS-SAVE-DATE           TO DATEO.                        
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO TIMEO.                        
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
      *    MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      
                                                                        
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL6526AO)                                      
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
