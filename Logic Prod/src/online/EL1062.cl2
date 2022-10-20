       ID DIVISION.                                                     
       PROGRAM-ID.                 EL1062.                               
      *AUTHOR.     Cowtown.
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.    TRANSACTION- EX1B - COMMISSION CAP STATE MAINTENANCE
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * Changes are marked by the Change Effective date.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 102717  CR2017062000003  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL1062 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '********* VMOD=2.001 ***********'. 

                                       COPY ELCSCTM.
                                       COPY ELCSCRTY.
                                       COPY MPCSCRT.

       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
           05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
                                                                        
       01  WS.                                                          
           05  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +29.
           05  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
           05  WS-COMM-LENGTH      PIC S9(4)  COMP VALUE +1500.
           05  WS-MAP-LENGTH       PIC S9(4)  COMP VALUE +500.
           05  RETURNED-FROM       PIC X(8)    VALUE SPACES.            
           05  QID.                                                     
               16  QID-TERM        PIC X(4).                            
               16  FILLER          PIC X(4)    VALUE '106A'.            
                                                                        
           05  WS-ACOMSL               PIC S9V9(4) VALUE +0.             
           05  WS-ACOMJL               PIC S9V9(4) VALUE +0.             
           05  WS-ACOMSA               PIC S9V9(4) VALUE +0.             
           05  WS-ACOMJA               PIC S9V9(4) VALUE +0.             

           05  WS-GCOMSL               PIC S9V9(4) VALUE +0.             
           05  WS-GCOMJL               PIC S9V9(4) VALUE +0.             
           05  WS-GCOMSA               PIC S9V9(4) VALUE +0.             
           05  WS-GCOMJA               PIC S9V9(4) VALUE +0.             

           05  WS-TCOMSL               PIC S9V9(4) VALUE +0.             
           05  WS-TCOMJL               PIC S9V9(4) VALUE +0.             
           05  WS-TCOMSA               PIC S9V9(4) VALUE +0.             
           05  WS-TCOMJA               PIC S9V9(4) VALUE +0.             
                                                                        
       01  STANDARD-AREAS.                                              
           12  MAP-NAME            PIC X(8)    VALUE 'EL106C'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL1062S'.          
           12  TRANS-ID            PIC X(4)    VALUE 'EX1B'.
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
           12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.          
           12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
           12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
           12  THIS-PGM            PIC X(8)    VALUE 'EL1062'.
           12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.          
           12  ELLETR-ID           PIC X(8)    VALUE 'ELLETR'.          
           12  SUB                 PIC S9(4)   COMP.                    
           12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
           12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.               
               88  FIRST-TIME                  VALUE 'Y'.               
           12  WS-DISPLAY-SW       PIC X       VALUE 'N'.               
               88  RETURN-DISPLAY              VALUE 'Y'.               
                                                                        
       01  ACCESS-KEYS.                                                 
           12  ELCNTL-KEY.                                              
               16  CK-COMP-ID      PIC X(3).                            
               16  FILLER          PIC X       VALUE '3'.               
               16  CK-STATE-CD     PIC X(4)    VALUE SPACES.            
               16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.        
           12  W-WORKING-TEXT-KEY.                                      
               16  W-TEXT-COMPANY-CD                                    
                                   PIC X.                               
               16  W-TEXT-FORM-NO  PIC X(12).                           
               16  W-TEXT-LINE-SEQ PIC S9(4)   COMP.                    
                                                                        
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
           12  ER-3030                 PIC X(4)  VALUE '3030'.          
           12  ER-3031                 PIC X(4)  VALUE '3031'.          
           12  ER-3032                 PIC X(4)  VALUE '3032'.          
           12  ER-3033                 PIC X(4)  VALUE '3033'.          
           12  ER-3034                 PIC X(4)  VALUE '3034'.          
           12  ER-3035                 PIC X(4)  VALUE '3035'.          
           12  ER-3036                 PIC X(4)  VALUE '3036'.
           12  ER-3040                 PIC X(4)  VALUE '3040'.
           12  ER-3067                 PIC X(4)  VALUE '3067'.
           12  ER-7008                 PIC X(4)  VALUE '7008'.          
           12  ER-7346                 PIC X(4)  VALUE '7346'.          
           12  ER-7531                 PIC X(4)  VALUE '7531'.          
           12  ER-7532                 PIC X(4)  VALUE '7532'.          
           12  ER-7536                 PIC X(4)  VALUE '7536'. 
           12  ER-7578                 PIC X(4)  VALUE '7578'.         
           12  ER-7581                 PIC X(4)  VALUE '7581'.
           12  ER-7717                 PIC X(4)  VALUE '7717'.          
           12  ER-7735                 PIC X(4)  VALUE '7735'.          
           12  ER-8159                 PIC X(4)  VALUE '8159'.          
           12  ER-9074                 PIC X(4)  VALUE '9074'.          
           12  ER-9097                 PIC X(4)  VALUE '9097'.
           12  ER-9447                 PIC X(4)  VALUE '9447'.          
           12  ER-9448                 PIC X(4)  VALUE '9448'.          
           12  ER-9478                 PIC X(4)  VALUE '9478'.          
           12  ER-9999                 PIC X(4)  VALUE '9999'.          

                                       COPY ELCDATE.                                                
                                       COPY ELCLOGOF.                                               
                                       COPY ELCATTR.                                                
                                       COPY ELCEMIB.                                                
                                       COPY ELCJPFX.                                                
                                       PIC X(530).                                          

           COPY ELCINTF.                                                
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
               16  FILLER              PIC X(101).                      
               16  PI-WS-STATE         PIC XX.                          
               16  PI-WS-CLASS         PIC XX.                          
               16  PI-WS-DEV           PIC XXX.                         
               16  PI-WS-TYPE          PIC X.                           
               16  PI-WS-PLAN          PIC XX.                          
               16  PI-PREV-STATE       PIC X(4).                        
               16  FILLER              PIC X(525).                      

                                       COPY ELCAID.

       01  FILLER REDEFINES DFHAID.                                     
           12  FILLER                  PIC X(8).
           12  PF-VALUES               PIC X    OCCURS 2.

                                       COPY EL1062S.

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA             PIC X(1500).                         

                                       COPY ELCCNTL.                                                
                                       COPY ELCTEXT.                                                

       PROCEDURE DIVISION.                                              

           MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
           MOVE '5'                   TO DC-OPTION-CODE.                
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
                                                                        
           MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.       
           MOVE 2                     TO EMI-NUMBER-OF-LINES            
                                         EMI-SWITCH2.                   
                                                                        
           MOVE EIBTRMID              TO QID-TERM.                      
                                                                        
           IF EIBCALEN = 0                                              
               GO TO 8800-UNAUTHORIZED-ACCESS.                          
                                                                        
           IF PI-RETURN-TO-PROGRAM = THIS-PGM
              MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM
           ELSE
              MOVE SPACES              TO RETURNED-FROM
           end-if

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
                 PERFORM 9910-INITIALIZE-SECURITY
                                           THRU 9910-EXIT
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
               DUPREC  (8850-DUPREC)                                    
               NOTOPEN (8870-NOTOPEN)                                   
               NOTFND  (8880-NOT-FOUND)                                 
               PGMIDERR(9600-PGMID-ERROR)                               
               ERROR   (9990-ABEND)                                     
           END-EXEC.                                                    

           IF EIBTRNID NOT = TRANS-ID
              MOVE LOW-VALUES          TO EL106CO
              move pi-ws-state         to stcdi
              move 'Y'                 to ws-display-sw
              go to 1000-show-state
           END-IF

           IF EIBAID = DFHCLEAR                                         
               GO TO 9400-CLEAR.                                        

           IF NOT DISPLAY-CAP                                    
               MOVE 'READ'         TO SM-READ                           
               PERFORM 9995-SECURITY-VIOLATION                          
               MOVE ER-0070        TO EMI-ERROR                         
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 8100-SEND-INITIAL-MAP.                             

       0200-RECEIVE.                                                    

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
              MOVE LOW-VALUES          TO EL106CI                               
              MOVE ER-7008             TO EMI-ERROR                             
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
              MOVE -1                  TO MAINTL                                        
              GO TO 8200-SEND-DATAONLY.

           EXEC CICS RECEIVE                                            
               MAP   (MAP-NAME)                                         
               MAPSET(MAPSET-NAME)                                      
               INTO  (EL106CI)                                          
           END-EXEC.                                                    
                                                                        
           IF ENTERPFL = 0                                              
               GO TO 0300-CHECK-PFKEYS.                                 
                                                                        
           IF EIBAID NOT = DFHENTER                                     
               MOVE ER-0004 TO EMI-ERROR                                
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
           IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)   
               MOVE PF-VALUES (ENTERPFI) TO EIBAID                      
           ELSE                                                         
               MOVE ER-0029 TO EMI-ERROR                                
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
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
                                                                        
           IF EIBAID = DFHPF3
              MOVE PI-PREV-STATE       TO PI-WS-STATE
              MOVE SPACES              TO PI-WS-CLASS
                                          PI-WS-DEV  
                                          PI-WS-TYPE 
                                          PI-WS-PLAN 
              MOVE XCTL-EL106          TO PGM-NAME   
              GO TO 9300-XCTL
           end-if

           IF EIBAID = DFHENTER                                         
               GO TO 0330-EDIT-DATA.                                    
                                                                        
           MOVE ER-0029 TO EMI-ERROR.                                   
       0320-INPUT-ERROR.                                                
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE AL-UNBON TO ENTERPFA.                                   
                                                                        
           IF ENTERPFL = 0                                              
               MOVE -1 TO MAINTL                                        
           ELSE                                                         
               MOVE -1 TO ENTERPFL.                                     
                                                                        
           GO TO 8200-SEND-DATAONLY

           .
       0330-EDIT-DATA.                                                  

           IF (STCDL = ZERO)
              and (STABRL = ZERO)
               MOVE ER-0144       TO EMI-ERROR                          
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1            TO STCDL                              
               MOVE AL-UABON      TO STCDA                              
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           IF STCDL = ZERO AND                                          
              STABRL NOT = ZERO                                         
               PERFORM 0500-GET-STATE-CD THRU 0600-EXIT.                
                                                                        
           IF MAINTI = 'S'                                              
               GO TO 1000-SHOW-STATE.                                   
                                                                        
           IF MODIFY-CAP                                         
              NEXT SENTENCE                                             
             ELSE                                                       
              IF MAINTI = 'C'
               MOVE 'UPDATE'       TO SM-READ                           
               PERFORM 9995-SECURITY-VIOLATION                          
               MOVE ER-0070        TO EMI-ERROR                         
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE LOW-VALUES     TO EL106CO                           
               GO TO 8100-SEND-INITIAL-MAP.                             

           IF MAINTI = 'C'                                              
               GO TO 2000-CHANGE-STATE.                                 

           MOVE ER-0023 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1       TO MAINTL.                                     
           MOVE AL-UABON TO MAINTA.                                     
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           EJECT                                                        
       0500-GET-STATE-CD.                                               
           MOVE PI-COMPANY-ID  TO CK-COMP-ID.                           
           MOVE LOW-VALUES     TO CK-STATE-CD.                          
           MOVE +0             TO CK-SEQ.                               
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               ENDFILE(8880-NOT-FOUND)                                  
               NOTFND (8880-NOT-FOUND)                                  
           END-EXEC.                                                    
                                                                        
           EXEC CICS STARTBR                                            
               DATASET  (ELCNTL-ID)                                     
               RIDFLD   (ELCNTL-KEY)                                    
           END-EXEC.                                                    
                                                                        
       0510-GET-NEXT-CD.                                                
           EXEC CICS READNEXT                                           
               DATASET(ELCNTL-ID)                                       
               SET    (ADDRESS OF CONTROL-FILE)                         
               RIDFLD (ELCNTL-KEY)                                      
           END-EXEC.                                                    
                                                                        
           IF CF-COMPANY-ID NOT = PI-COMPANY-ID                         
               EXEC CICS ENDBR                                          
                   DATASET  (ELCNTL-ID)                                 
               END-EXEC                                                 
               GO TO 8880-NOT-FOUND.                                    
                                                                        
           IF CF-RECORD-TYPE NOT = '3'                                  
               GO TO 0510-GET-NEXT-CD.                                  
                                                                        
           IF CF-STATE-ABBREVIATION = STABRI                            
               NEXT SENTENCE                                            
             ELSE                                                       
               GO TO 0510-GET-NEXT-CD.                                  
                                                                        
           MOVE CF-STATE-CODE      TO STCDI.                            
           MOVE AL-UANON           TO STCDA.                            
           MOVE +2                 TO STCDL.                            
                                                                        
           EXEC CICS ENDBR                                              
               DATASET  (ELCNTL-ID)                                     
           END-EXEC.                                                    
                                                                        
       0600-EXIT.                                                       
            EXIT.                                                       
           EJECT                                                        
       1000-SHOW-STATE.                                                 
           IF RETURN-DISPLAY                                            
               EXEC CICS HANDLE CONDITION                               
                   NOTFND  (8100-SEND-INITIAL-MAP)                      
               END-EXEC.                                                
                                                                        
           MOVE PI-COMPANY-ID TO CK-COMP-ID.                            
           MOVE STCDI         TO CK-STATE-CD                            
                                 PI-WS-STATE.                           
                                                                        
           EXEC CICS READ                                               
               DATASET(ELCNTL-ID)                                       
               SET    (ADDRESS OF CONTROL-FILE)                         
               RIDFLD (ELCNTL-KEY)                                      
           END-EXEC.                                                    
                                                                        
           GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
           EJECT                                                        
       2000-CHANGE-STATE.                                               
           IF STCDI NOT = PI-PREV-STATE                                 
               MOVE ER-0145 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1       TO STCDL                                   
               MOVE AL-UABON TO STCDA                                   
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.                 
                                                                        
           IF NOT EMI-NO-ERRORS                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           MOVE PI-COMPANY-ID TO CK-COMP-ID.                            
           MOVE STCDI         TO CK-STATE-CD                            
                                 PI-WS-STATE.                           
                                                                        
           EXEC CICS READ                                               
               UPDATE                                                   
               DATASET(ELCNTL-ID)                                       
               SET    (ADDRESS OF CONTROL-FILE)                         
               RIDFLD (ELCNTL-KEY)                                      
           END-EXEC.                                                    
                                                                        
           IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR             
              CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS               
               EXEC CICS UNLOCK                                         
                   DATASET(ELCNTL-ID)                                   
               END-EXEC                                                 
               MOVE ER-0068 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-STATE.                                   
                                                                        
           MOVE 'B'                    TO JP-RECORD-TYPE.               
           MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
           PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              
                                                                        
           MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY.                    
           MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS.                
           MOVE EIBDATE         TO DC-JULIAN-YYDDD.                     
           MOVE '5'             TO DC-OPTION-CODE.                      
           MOVE LINK-ELDATCV    TO PGM-NAME.                            
                                                                        
           EXEC CICS LINK                                               
               PROGRAM (PGM-NAME)                                       
               COMMAREA(DATE-CONVERSION-DATA)                           
               LENGTH  (DC-COMM-LENGTH)                                 
           END-EXEC.                                                    
                                                                        
           IF DATE-CONVERSION-ERROR
              MOVE LOW-VALUES          TO CF-LAST-MAINT-DT
           ELSE
              MOVE DC-BIN-DATE-1       TO CF-LAST-MAINT-DT
           END-IF

           if cceoptl <> zeros
              move cceopti             to cf-comm-cap-limit-to
           end-if

           IF (WS-ACOMSL NUMERIC)
              AND (WS-ACOMSL NOT = ZEROS)
              MOVE WS-ACOMSL           TO CF-ST-COMM-CAP-SL
           ELSE
              MOVE ZEROS               TO CF-ST-COMM-CAP-SL
           END-IF
                                                                        
           IF (WS-ACOMJL NUMERIC)
              AND (WS-ACOMJL NOT = ZEROS)
              MOVE WS-ACOMJL           TO CF-ST-COMM-CAP-JL
           ELSE
              MOVE ZEROS               TO CF-ST-COMM-CAP-JL
           END-IF

           IF (WS-ACOMSA NUMERIC)
              AND (WS-ACOMSA NOT = ZEROS)
              MOVE WS-ACOMSA           TO CF-ST-COMM-CAP-SA
           ELSE
              MOVE ZEROS               TO CF-ST-COMM-CAP-SA
           END-IF

           IF (WS-ACOMJA NUMERIC)
              AND (WS-ACOMJA NOT = ZEROS)
              MOVE WS-ACOMJA           TO CF-ST-COMM-CAP-JA
           ELSE
              MOVE ZEROS               TO CF-ST-COMM-CAP-JA
           END-IF

           IF (WS-GCOMSL NUMERIC)
              AND (WS-GCOMSL NOT = ZEROS)
              MOVE WS-GCOMSL           TO CF-ST-GA-COMM-CAP-SL
           ELSE
              MOVE ZEROS               TO CF-ST-GA-COMM-CAP-SL
           END-IF
                                                                        
           IF (WS-GCOMJL NUMERIC)
              AND (WS-GCOMJL NOT = ZEROS)
              MOVE WS-GCOMJL           TO CF-ST-GA-COMM-CAP-JL
           ELSE
              MOVE ZEROS               TO CF-ST-GA-COMM-CAP-JL
           END-IF

           IF (WS-GCOMSA NUMERIC)
              AND (WS-GCOMSA NOT = ZEROS)
              MOVE WS-GCOMSA           TO CF-ST-GA-COMM-CAP-SA
           ELSE
              MOVE ZEROS               TO CF-ST-GA-COMM-CAP-SA
           END-IF

           IF (WS-GCOMJA NUMERIC)
              AND (WS-GCOMJA NOT = ZEROS)
              MOVE WS-GCOMJA           TO CF-ST-GA-COMM-CAP-JA
           ELSE
              MOVE ZEROS               TO CF-ST-GA-COMM-CAP-JA
           END-IF

           IF (WS-TCOMSL NUMERIC)
              AND (WS-TCOMSL NOT = ZEROS)
              MOVE WS-TCOMSL           TO CF-ST-TOT-COMM-CAP-SL
           ELSE
              MOVE ZEROS               TO CF-ST-TOT-COMM-CAP-SL
           END-IF
                                                                        
           IF (WS-TCOMJL NUMERIC)
              AND (WS-TCOMJL NOT = ZEROS)
              MOVE WS-TCOMJL           TO CF-ST-TOT-COMM-CAP-JL
           ELSE
              MOVE ZEROS               TO CF-ST-TOT-COMM-CAP-JL
           END-IF

           IF (WS-TCOMSA NUMERIC)
              AND (WS-TCOMSA NOT = ZEROS)
              MOVE WS-TCOMSA           TO CF-ST-TOT-COMM-CAP-SA
           ELSE
              MOVE ZEROS               TO CF-ST-TOT-COMM-CAP-SA
           END-IF

           IF (WS-TCOMJA NUMERIC)
              AND (WS-TCOMJA NOT = ZEROS)
              MOVE WS-TCOMJA           TO CF-ST-TOT-COMM-CAP-JA
           ELSE
              MOVE ZEROS               TO CF-ST-TOT-COMM-CAP-JA
           END-IF

           MOVE 'C'                    TO JP-RECORD-TYPE.               
           MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
           EXEC CICS REWRITE                                            
               DATASET(ELCNTL-ID)                                       
               FROM   (CONTROL-FILE)                                    
           END-EXEC.                                                    

           PERFORM 8400-LOG-JOURNAL-RECORD
                                       THRU 8400-EXIT
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES             TO EL106CO
           MOVE -1                     TO MAINTL
           MOVE SPACES                 TO PI-PREV-STATE
           MOVE CK-STATE-CD            TO STCDO
           MOVE AL-UANON               TO STCDA
           GO TO 1000-SHOW-STATE

           .
       5000-FIND-NEXT-STATE.                                            

           MOVE PI-COMPANY-ID          TO CK-COMP-ID

           IF STCDL = 0
              MOVE LOW-VALUES          TO CK-STATE-CD
              MOVE +0                  TO CK-SEQ
           ELSE
              MOVE STCDI               TO CK-STATE-CD
              MOVE +1                  TO CK-SEQ
           END-IF

           MOVE SPACES TO PI-PREV-STATE.                                
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (8860-ENDFILE)                                    
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               DATASET(ELCNTL-ID)                                       
               SET    (ADDRESS OF CONTROL-FILE)                         
               RIDFLD (ELCNTL-KEY)                                      
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
           IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    
              CF-RECORD-TYPE NOT = '3'                                  
               GO TO 8860-ENDFILE.                                      
                                                                        
           IF STCDL = 0                                                 
               MOVE ER-0146 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
                                                                        
           GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
           EJECT                                                        
       5500-FIND-PREV-STATE.                                            
           MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  
           MOVE PI-PREV-STATE          TO  CK-STATE-CD.                 
                                                                        
           IF STCDL GREATER +0                                          
               MOVE STCDI              TO  CK-STATE-CD.                 
                                                                        
           MOVE SPACES                 TO  PI-PREV-STATE.               
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND(8860-ENDFILE)                                     
           END-EXEC.                                                    
                                                                        
           EXEC CICS STARTBR                                            
               DATASET  (ELCNTL-ID)                                     
               RIDFLD   (ELCNTL-KEY)                                    
           END-EXEC.                                                    
                                                                        
       5600-READ-PREV-STATE-RECORD.                                     
           EXEC CICS READPREV                                           
               DATASET  (ELCNTL-ID)                                     
               SET      (ADDRESS OF CONTROL-FILE)                       
               RIDFLD   (ELCNTL-KEY)                                    
           END-EXEC.                                                    
                                                                        
           IF FIRST-TIME                                                
               MOVE 'N'                TO  WS-FIRST-TIME-SW             
               GO TO 5600-READ-PREV-STATE-RECORD.                       
                                                                        
           IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    
              CF-RECORD-TYPE NOT = '3'                                  
               GO TO 8860-ENDFILE.                                      
                                                                        
           IF STCDL = 0                                                 
               MOVE ER-0146 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
                                                                        
           GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
           EJECT                                                        
       6000-EDIT-INPUT-DATA.                                            

           IF CCEOPTL <> ZEROS
              IF CCEOPTI = 'A' OR 'G' OR 'B' OR ' '
                 CONTINUE
              ELSE
                 MOVE ER-3067          TO EMI-ERROR
                 MOVE -1               TO CCEOPTL
                 MOVE AL-UABON         TO CCEOPTA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ACOMSLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (ACOMSLI)
                 LENGTH(6)
              END-EXEC
              IF ACOMSLI NUMERIC
                 MOVE AL-UNNON         TO ACOMSLA
                 MOVE ACOMSLI          TO WS-ACOMSL
                                          ACOMSLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO ACOMSLL
                 MOVE AL-UNBON         TO ACOMSLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ACOMJLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (ACOMJLI)
                 LENGTH(6)
              END-EXEC
              IF ACOMJLI NUMERIC
                 MOVE AL-UNNON         TO ACOMJLA
                 MOVE ACOMJLI          TO WS-ACOMJL
                                          ACOMJLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO ACOMJLL
                 MOVE AL-UNBON         TO ACOMJLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ACOMSAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (ACOMSAI)
                 LENGTH(6)
              END-EXEC
              IF ACOMSAI NUMERIC
                 MOVE AL-UNNON         TO ACOMSAA
                 MOVE ACOMSAI          TO WS-ACOMSA
                                          ACOMSAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO ACOMSAL
                 MOVE AL-UNBON         TO ACOMSAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ACOMJAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (ACOMJAI)
                 LENGTH(6)
              END-EXEC
              IF ACOMJAI NUMERIC
                 MOVE AL-UNNON         TO ACOMJAA
                 MOVE ACOMJAI          TO WS-ACOMJA
                                          ACOMJAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO ACOMJAL
                 MOVE AL-UNBON         TO ACOMJAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF GCOMSLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (GCOMSLI)
                 LENGTH(6)
              END-EXEC
              IF GCOMSLI NUMERIC
                 MOVE AL-UNNON         TO GCOMSLA
                 MOVE GCOMSLI          TO WS-GCOMSL
                                          GCOMSLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO GCOMSLL
                 MOVE AL-UNBON         TO GCOMSLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF GCOMJLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (GCOMJLI)
                 LENGTH(6)
              END-EXEC
              IF GCOMJLI NUMERIC
                 MOVE AL-UNNON         TO GCOMJLA
                 MOVE GCOMJLI          TO WS-GCOMJL
                                          GCOMJLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO GCOMJLL
                 MOVE AL-UNBON         TO GCOMJLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF GCOMSAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (GCOMSAI)
                 LENGTH(6)
              END-EXEC
              IF GCOMSAI NUMERIC
                 MOVE AL-UNNON         TO GCOMSAA
                 MOVE GCOMSAI          TO WS-GCOMSA
                                          GCOMSAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO GCOMSAL
                 MOVE AL-UNBON         TO GCOMSAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF GCOMJAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (GCOMJAI)
                 LENGTH(6)
              END-EXEC
              IF GCOMJAI NUMERIC
                 MOVE AL-UNNON         TO GCOMJAA
                 MOVE GCOMJAI          TO WS-GCOMJA
                                          GCOMJAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO GCOMJAL
                 MOVE AL-UNBON         TO GCOMJAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF TCOMSLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (TCOMSLI)
                 LENGTH(6)
              END-EXEC
              IF TCOMSLI NUMERIC
                 MOVE AL-UNNON         TO TCOMSLA
                 MOVE TCOMSLI          TO WS-TCOMSL
                                          TCOMSLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO TCOMSLL
                 MOVE AL-UNBON         TO TCOMSLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF TCOMJLL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (TCOMJLI)
                 LENGTH(6)
              END-EXEC
              IF TCOMJLI NUMERIC
                 MOVE AL-UNNON         TO TCOMJLA
                 MOVE TCOMJLI          TO WS-TCOMJL
                                          TCOMJLO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO TCOMJLL
                 MOVE AL-UNBON         TO TCOMJLA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF TCOMSAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (TCOMSAI)
                 LENGTH(6)
              END-EXEC
              IF TCOMSAI NUMERIC
                 MOVE AL-UNNON         TO TCOMSAA
                 MOVE TCOMSAI          TO WS-TCOMSA
                                          TCOMSAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO TCOMSAL
                 MOVE AL-UNBON         TO TCOMSAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF TCOMJAL <> ZEROS
              EXEC CICS BIF
                 DEEDIT
                 FIELD (TCOMJAI)
                 LENGTH(6)
              END-EXEC
              IF TCOMJAI NUMERIC
                 MOVE AL-UNNON         TO TCOMJAA
                 MOVE TCOMJAI          TO WS-TCOMJA
                                          TCOMJAO
              ELSE
                 MOVE ER-1614          TO EMI-ERROR
                 MOVE -1               TO TCOMJAL
                 MOVE AL-UNBON         TO TCOMJAA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           .                                                            
       6099-EXIT.                                                       
           EXIT.                                                        

       7000-BUILD-OUTPUT-MAP.                                           

      ***************************************************************   
      *                                                             *   
      *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *   
      *                                                             *   
      ***************************************************************   

           MOVE LOW-VALUES            TO EL106CO.                       
           MOVE CF-STATE-CODE         TO STCDO                          
                                         PI-WS-STATE
           move cf-state-abbreviation  to stabro
           move cf-state-name          to stnameo

           IF CF-comm-cap-limit-to = 'A' OR 'G' OR 'B'
              move cf-comm-cap-limit-to to CCeOPTO
           else
              move ' '                  to cceopto
           end-if

           IF CF-ST-COMM-CAP-SL NUMERIC
              IF CF-ST-COMM-CAP-SL NOT = ZEROS
                 MOVE CF-ST-COMM-CAP-SL
                                       TO ACOMSLO
              ELSE
                 MOVE ZEROS            TO ACOMSLO
              END-IF
           ELSE
              MOVE ZEROS               TO ACOMSLO
           END-IF

           IF CF-ST-COMM-CAP-JL NUMERIC
              IF CF-ST-COMM-CAP-JL NOT = ZEROS
                 MOVE CF-ST-COMM-CAP-JL
                                       TO ACOMJLO
              ELSE
                 MOVE ZEROS            TO ACOMJLO
              END-IF
           ELSE
              MOVE ZEROS               TO ACOMJLO
           END-IF

           IF CF-ST-COMM-CAP-SA NUMERIC
              IF CF-ST-COMM-CAP-SA NOT = ZEROS
                 MOVE CF-ST-COMM-CAP-SA
                                       TO ACOMSAO
              ELSE
                 MOVE ZEROS            TO ACOMSAO
              END-IF
           ELSE
              MOVE ZEROS               TO ACOMSAO
           END-IF

           IF CF-ST-COMM-CAP-JA NUMERIC
              IF CF-ST-COMM-CAP-JA NOT = ZEROS
                 MOVE CF-ST-COMM-CAP-JA
                                       TO ACOMJAO
              ELSE
                 MOVE ZEROS            TO ACOMJAO
              END-IF
           ELSE
              MOVE ZEROS               TO ACOMJAO
           END-IF

           IF CF-ST-GA-COMM-CAP-SL NUMERIC
              IF CF-ST-GA-COMM-CAP-SL NOT = ZEROS
                 MOVE CF-ST-GA-COMM-CAP-SL
                                       TO GCOMSLO
              ELSE
                 MOVE ZEROS            TO GCOMSLO
              END-IF
           ELSE
              MOVE ZEROS               TO GCOMSLO
           END-IF

           IF CF-ST-GA-COMM-CAP-JL NUMERIC
              IF CF-ST-GA-COMM-CAP-JL NOT = ZEROS
                 MOVE CF-ST-GA-COMM-CAP-JL
                                       TO GCOMJLO
              ELSE
                 MOVE ZEROS            TO GCOMJLO
              END-IF
           ELSE
              MOVE ZEROS               TO GCOMJLO
           END-IF

           IF CF-ST-GA-COMM-CAP-SA NUMERIC
              IF CF-ST-GA-COMM-CAP-SA NOT = ZEROS
                 MOVE CF-ST-GA-COMM-CAP-SA
                                       TO GCOMSAO
              ELSE
                 MOVE ZEROS            TO GCOMSAO
              END-IF
           ELSE
              MOVE ZEROS               TO GCOMSAO
           END-IF

           IF CF-ST-GA-COMM-CAP-JA NUMERIC
              IF CF-ST-GA-COMM-CAP-JA NOT = ZEROS
                 MOVE CF-ST-GA-COMM-CAP-JA
                                       TO GCOMJAO
              ELSE
                 MOVE ZEROS            TO GCOMJAO
              END-IF
           ELSE
              MOVE ZEROS               TO GCOMJAO
           END-IF

           IF CF-ST-TOT-COMM-CAP-SL NUMERIC
              IF CF-ST-TOT-COMM-CAP-SL NOT = ZEROS
                 MOVE CF-ST-TOT-COMM-CAP-SL
                                       TO TCOMSLO
              ELSE
                 MOVE ZEROS            TO TCOMSLO
              END-IF
           ELSE
              MOVE ZEROS               TO TCOMSLO
           END-IF

           IF CF-ST-TOT-COMM-CAP-JL NUMERIC
              IF CF-ST-TOT-COMM-CAP-JL NOT = ZEROS
                 MOVE CF-ST-TOT-COMM-CAP-JL
                                       TO TCOMJLO
              ELSE
                 MOVE ZEROS            TO TCOMJLO
              END-IF
           ELSE
              MOVE ZEROS               TO TCOMJLO
           END-IF

           IF CF-ST-TOT-COMM-CAP-SA NUMERIC
              IF CF-ST-TOT-COMM-CAP-SA NOT = ZEROS
                 MOVE CF-ST-TOT-COMM-CAP-SA
                                       TO TCOMSAO
              ELSE
                 MOVE ZEROS            TO TCOMSAO
              END-IF
           ELSE
              MOVE ZEROS               TO TCOMSAO
           END-IF

           IF CF-ST-TOT-COMM-CAP-JA NUMERIC
              IF CF-ST-TOT-COMM-CAP-JA NOT = ZEROS
                 MOVE CF-ST-TOT-COMM-CAP-JA
                                       TO TCOMJAO
              ELSE
                 MOVE ZEROS            TO TCOMJAO
              END-IF
           ELSE
              MOVE ZEROS               TO TCOMJAO
           END-IF
                                                                        
           MOVE CF-LAST-MAINT-BY      TO LSTUSRO.                       
           MOVE ' '                   TO DC-OPTION-CODE.                
           MOVE CF-LAST-MAINT-DT      TO DC-BIN-DATE-1.                 
           MOVE LINK-ELDATCV          TO PGM-NAME.                      
           EXEC CICS LINK                                               
               PROGRAM (PGM-NAME)                                       
               COMMAREA(DATE-CONVERSION-DATA)                           
               LENGTH  (DC-COMM-LENGTH)                                 
           END-EXEC.                                                    
                                                                        
           IF DATE-CONVERSION-ERROR                                     
               MOVE ZEROS              TO LSTDTEO                       
           ELSE                                                         
               MOVE DC-GREG-DATE-1-EDIT                                 
                                       TO LSTDTEO.                      
                                                                        
           MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN.                      
           MOVE TIME-OUT               TO LSTTIMEO.                     
           MOVE -1                     TO MAINTL.                       
           MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.                 
           MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             
           MOVE CF-STATE-CODE          TO PI-PREV-STATE.                
           MOVE AL-UANOF               TO MAINTA.                       
           MOVE AL-UANON               TO STCDA                         
                                        STNAMEA                         
                                        STABRA                          
                                        CCEOPTA.                        
                                                                        
           MOVE AL-UNNON               TO ACOMSLA
                                          ACOMJLA
                                          ACOMSAA
                                          ACOMJAA
                                          GCOMSLA
                                          GCOMJLA
                                          GCOMSAA
                                          GCOMJAA
                                          TCOMSLA
                                          TCOMJLA
                                          TCOMSAA
                                          TCOMJAA

           IF RETURN-DISPLAY                                            
               GO TO 8100-SEND-INITIAL-MAP                              
           ELSE                                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
       8100-SEND-INITIAL-MAP.                                           

           MOVE SAVE-DATE              TO RUNDTEO.                      
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO RUNTIMEO.                     
           MOVE -1                     TO MAINTL.                       
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     

           EXEC CICS SEND                                               
               MAP   (MAP-NAME)                                         
               MAPSET(MAPSET-NAME)                                      
               FROM  (EL106CO)                                          
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      

       8200-SEND-DATAONLY.                                              

           MOVE SAVE-DATE      TO RUNDTEO.                              
           MOVE EIBTIME        TO TIME-IN.                              
           MOVE TIME-OUT       TO RUNTIMEO.                             
                                                                        
                                                                        
                                                                        
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     
                                                                        
           EXEC CICS SEND                                               
               MAP   (MAP-NAME)                                         
               MAPSET(MAPSET-NAME)                                      
               FROM  (EL106CO)                                          
               DATAONLY                                                 
               ERASEAUP                                                 
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
           EJECT                                                        
       8300-SEND-TEXT.                                                  
           EXEC CICS SEND TEXT                                          
               FROM  (LOGOFF-TEXT)                                      
               LENGTH(LOGOFF-LENGTH)                                    
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
           EJECT                                                        
       8400-LOG-JOURNAL-RECORD.                                         
           IF PI-JOURNAL-FILE-ID = 0                                    
               GO TO 8400-EXIT.                                         
                                                                        
           MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   
           MOVE ELCNTL-ID              TO JP-FILE-ID.                   
           MOVE THIS-PGM               TO JP-PROGRAM-ID.                
      *    EXEC CICS JOURNAL
      *         JFILEID(PI-JOURNAL-FILE-ID)
      *         JTYPEID('EL')
      *         FROM   (JOURNAL-RECORD)
      *         LENGTH (773)
      *    END-EXEC.
                                                                        
       8400-EXIT.                                                       
           EXIT.                                                        
                                                                        
       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG TO LOGOFF-MSG.                             
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID   TO PI-ENTRY-CD-1.                              
           MOVE XCTL-EL005 TO PGM-NAME.                                 
           GO TO 9300-XCTL.                                             
                                                                        
       8850-DUPREC.                                                     
           MOVE ER-0147 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1       TO STCDL.                                      
           MOVE AL-UABON TO STCDA.                                      
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       8860-ENDFILE.                                                    
           MOVE LOW-VALUES TO EL106CO.                                  
           MOVE ER-0148    TO EMI-ERROR.                                
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1 TO MAINTL.                                           
           MOVE SPACES TO PI-WS-STATE.                                           
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       8870-NOTOPEN.                                                    
           MOVE ER-0042 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1 TO MAINTL.                                           
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       8880-NOT-FOUND.                                                  
           MOVE ER-0149 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1       TO STCDL.                                      
           MOVE AL-UABON TO STCDA.                                      
           GO TO 8200-SEND-DATAONLY.                                    

       9000-RETURN-CICS.                                                
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
           MOVE '106C'                 TO PI-CURRENT-SCREEN-NO.         
                                                                        
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
              END-IF
           END-IF

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
           MOVE '106A'                 TO PI-CURRENT-SCREEN-NO
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
           END-EXEC.                                                    
                                                                        
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
                 MOVE ER-0070          TO EMI-ERROR                    
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT         
                 GO TO 8100-SEND-INITIAL-MAP
              end-if
           end-if

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
                                                                        
