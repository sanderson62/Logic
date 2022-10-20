       ID DIVISION.                                                     
                                                                        
       PROGRAM-ID.                 EL1041.
      *                                                                 
      *AUTHOR.     PABLO                                                
      *            OMAHA NE                                             
                                                                        
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
                                                                        
      *REMARKS.                                                         
                                                                        
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED   
      *    FOR THE Z LETTER CONTROLS
                                                                        
      *    SCREENS     - EL104Z - LETTER (Z) CONTROL MAINTENANCE                
                                                                        
      *    ENTERED BY  - EL1042 - TEXT MAINT
                                                                        
      *    EXIT TO     - EL1042 - TEXT MAINT
                                                                        
      *    INPUT FILE  - ELLETR -              - LETTER TEXT
                                                                        
      *    OUTPUT FILE - ELLETR -              - LETTER TEXT
                                                                        
      *    COMMAREA    - PASSED                                         
                                                                        
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1042. ON     
      *                  FIRST ENTRY, WE READ THE ELLETR FILE USING THE 
      *                  LETTER ID PASSED FROM EL1042.     ON SUBSEQUENT
      *                  ENTRIES (XCTL FROM CICS VIA     ) THE SCREEN   
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE   
      *                  MAINTENANCE TYPE INDICATED.                    
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
041513* 041513    2013011500003  AJRA  VALIDATE ENC CODE AGAINST ELENCC TBL
092413* 092413    2013062000003  AJRA  ADD BARCODE IND (USE ZREC COPYBOOK)
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
      *-----------------------------------------------------------------
                                                                        
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL1041 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
                                                                        
       77  M1                          PIC S999 COMP-3 VALUE +0.
       77  M2                          PIC S999 COMP-3 VALUE +0.
       77  WS-STOP-SW                  PIC X  VALUE ' '.
           88  I-SAY-TO-STOP              VALUE 'Y'.
       77  WS-FIND-SW                  PIC X  VALUE ' '.
           88  WE-FOUND-IT                VALUE 'Y'.
041513 77  WS-DONE-WITH-ENCC           PIC X  VALUE ' '.
041513     88  DONE-WITH-ENCC             VALUE 'Y'.

       77  WS-SEQ-NO-TO-USE            PIC S9(4) COMP VALUE +0.

       01  ACCESS-KEYS.
           12  ELLETR-KEY.
               16  ELLETR-COMPANY-CD   PIC X.
               16  ELLETR-ACCESS-CD    PIC X(12).
               16  ELLETR-SEQ-NO       PIC S9(4) COMP.
041513
041513     12  ELENCC-KEY.
041513         16  ELENCC-COMPANY-CD    PIC X.
041513         16  ELENCC-REC-TYPE      PIC X.
041513         16  ELENCC-ENC-CODE      PIC X(5).
041513         16  F                    PIC X(09).


       01  WS-DATE-AREA.                                                
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.    
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.    
                                                                        
       01  MISC-WORK-AREAS.                                             
                                                                        
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-9V999-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).                   
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(11).                  
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(09)V99.       
041513
041513     12  WS-TEST-STATE               PIC X(2) VALUE SPACES.                                           
041513         88 STATE-SPECIFIC    VALUES 'AK','AL','AR','AS','AZ',
041513            'CA','CO','CT','DC','DE','FL','GA','GU','HI','IA',
041513            'ID','IL','IN','KS','KY','LA','MA','MD','ME','MI',
041513            'MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM',
041513            'NV','NY','OH','OK','OR','PA','PR','RI','SC','SD',
041513            'TN','TX','UT','VA','VI','VT','WA','WI','WV','WY'.

       01  STANDARD-AREAS.                                              
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.  
           12  TRANS-ID                    PIC X(04)   VALUE 'EX1E'.
           12  PGM-NAME                    PIC X(08).                   
           12  TIME-IN                     PIC S9(07).                  
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER                  PIC X(01).                   
               16  TIME-OUT                PIC 99V99.                   
               16  FILLER                  PIC X(02).                   
           12  XCTL-005                    PIC X(08)   VALUE 'EL005'.   
           12  XCTL-010                    PIC X(08)   VALUE 'EL010'.   
           12  XCTL-155                    PIC X(08)   VALUE 'EL155'.   
           12  XCTL-626                    PIC X(08)   VALUE 'EL626'.   
           12  LINK-001                    PIC X(08)   VALUE 'EL001'.   
           12  LINK-004                    PIC X(08)   VALUE 'EL004'.   
           12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'. 
           12  THIS-PGM                    PIC X(08)   VALUE 'EL1041'.   
           12  ELLETR-FILE-ID              PIC X(08)   VALUE 'ELLETR'.
           12  ELLETR-LENGTH               PIC S9(04)  VALUE +100  COMP.
041513     12  ELENCC-FILE-ID              PIC X(08)   VALUE 'ELENCC'.
041513     12  ELENCC-LENGTH               PIC S9(04)  VALUE +400  COMP.
           12  SUB                         PIC 9(02).                   
           12  SUB-1                       PIC 9(02).                   
           12  SUB2                        PIC 9(02).                   
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.     
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL1041S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL104Z'.  
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.     
041513     12  HOLD-ENC-CODES              PIC X(120)  VALUE SPACES.
041513     12  WS-SUB1                     PIC S9(03).                   
041513     12  WS-SUB2                     PIC S9(03).                   
041513     12  WS-SUB3                     PIC S9(03).                   

092413****  Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
092413                                     COPY ELCZREC.


                                           COPY ELCSCTM.                
                                           COPY ELCSCRTY.               

       01  ERROR-MESSAGES.                                              
           12  ER-0000                     PIC X(04)   VALUE '0000'.    
           12  ER-0005                     PIC X(04)   VALUE '0005'.
           12  ER-0023                     PIC X(04)   VALUE '0023'.    
           12  ER-0029                     PIC X(04)   VALUE '0029'.    
           12  ER-0050                     PIC X(04)   VALUE '0050'.    
           12  ER-0068                     PIC X(04)   VALUE '0068'.    
           12  ER-0070                     PIC X(04)   VALUE '0070'.    
           12  ER-0130                     PIC X(04)   VALUE '0130'.    
           12  ER-0131                     PIC X(04)   VALUE '0131'.    
           12  ER-0132                     PIC X(04)   VALUE '0132'.    
           12  ER-0138                     PIC X(04)   VALUE '0138'.    
           12  ER-0144                     PIC X(04)   VALUE '0144'.    
           12  ER-0145                     PIC X(04)   VALUE '0145'.
           12  ER-0184                     PIC X(04)   VALUE '0184'.
           12  ER-0418                     PIC X(04)   VALUE '0418'.    
           12  ER-0491                     PIC X(04)   VALUE '0491'.
           12  ER-0582                     PIC X(04)   VALUE '0582'.    
           12  ER-0627                     PIC X(04)   VALUE '0627'.
           12  ER-0701                     PIC X(04)   VALUE '0701'.    
           12  ER-0702                     PIC X(04)   VALUE '0702'.    
           12  ER-0703                     PIC X(04)   VALUE '0703'.    
           12  ER-0704                     PIC X(04)   VALUE '0704'.    
           12  ER-0705                     PIC X(04)   VALUE '0705'.    
           12  ER-0706                     PIC X(04)   VALUE '0706'.    
           12  ER-0707                     PIC X(04)   VALUE '0707'.    
           12  ER-0708                     PIC X(04)   VALUE '0708'.    
           12  ER-0709                     PIC X(04)   VALUE '0709'.    
           12  ER-0710                     PIC X(04)   VALUE '0710'.    
           12  ER-0711                     PIC X(04)   VALUE '0711'.    
           12  ER-0712                     PIC X(04)   VALUE '0712'.    
           12  ER-0713                     PIC X(04)   VALUE '0713'.    
           12  ER-0717                     PIC X(04)   VALUE '0717'.    
           12  ER-0718                     PIC X(04)   VALUE '0718'.    
           12  ER-0719                     PIC X(04)   VALUE '0719'.    
           12  ER-0720                     PIC X(04)   VALUE '0720'.    
           12  ER-0721                     PIC X(04)   VALUE '0721'.    
           12  ER-0722                     PIC X(04)   VALUE '0722'.    
           12  ER-0723                     PIC X(04)   VALUE '0723'.    
           12  ER-0724                     PIC X(04)   VALUE '0724'.    
           12  ER-0725                     PIC X(04)   VALUE '0725'.    
           12  ER-0726                     PIC X(04)   VALUE '0726'.    
           12  ER-0727                     PIC X(04)   VALUE '0727'.    
           12  ER-0729                     PIC X(04)   VALUE '0729'.    
           12  ER-0754                     PIC X(04)   VALUE '0754'.
           12  ER-1560                     PIC X(04)   VALUE '1560'.
           12  ER-1562                     PIC X(04)   VALUE '1562'.
           12  ER-2241                     PIC X(04)   VALUE '2241'.    
           12  ER-2276                     PIC X(04)   VALUE '2276'.    
           12  ER-7008                     PIC X(04)   VALUE '7008'.    
           12  ER-7031                     PIC X(04)   VALUE '7031'.    
           12  ER-7123                     PIC X(04)   VALUE '7123'.    
           12  ER-8150                     PIC X(04)   VALUE '8150'.    
           12  ER-9999                     PIC XXXX    VALUE '9999'.
                                           COPY ELCDATE.                

                                           COPY ELCLOGOF.               

                                           COPY ELCATTR.                

                                           COPY ELCEMIB.                

                                           COPY ELCJPFX.                
                                           PIC X(530).                  
                                                                        

                                           COPY ELCINTF.                

           12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
                                           COPY ELC1042.
               16  PI-PREV-LETR-KEY        PIC X(15).
               16  PI-CURR-LETR-KEY        PIC X(15).
               16  PI-UPDATE-DT            PIC XX.
041513         16  PI-ENC-CODES            PIC X(120).
041513         16  PI-SPLIT-SUB            PIC S9(3)  COMP.
041513         16  FILLER                  PIC X(434).

                                           COPY ELCAID.                 
       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER                      PIC X(08).                   
           12  PF-VALUES                   PIC X         OCCURS 24.     

                                           COPY EL1041S.

       LINKAGE SECTION.                                                 
                                                                        
       01  DFHCOMMAREA                     PIC X(1024).                 

                                           COPY ELCTEXT.
041513                                     COPY ELCENCC.

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE

           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS.

           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
           MOVE 1                      TO EMI-NUMBER-OF-LINES

           EXEC CICS HANDLE CONDITION                                   
               DUPREC     (8850-DUPREC)                                 
               NOTOPEN    (8870-NOTOPEN)                                
               NOTFND     (8880-NOT-FOUND)                              
               PGMIDERR   (9600-PGMID-ERROR)                            
               ERROR      (9990-ABEND)                                  
           END-EXEC.                                                    

           .
       0150-SET-PROGRAM-SAVES.                                          
                                                                        
           IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM                     
               IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM               
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
               ELSE                                                     
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6      
           ELSE                                                         
               GO TO 0200-RECEIVE.

           DISPLAY ' COMM KEY PASSED ' PI-COMM-CONTROL
           MOVE PI-COMPANY-CD          TO ELLETR-KEY
           MOVE PI-COMM-CONTROL        TO ELLETR-KEY (2:12)
           MOVE ZEROS                  TO ELLETR-SEQ-NO
           MOVE ELLETR-KEY             TO PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY
041513
041513     PERFORM 6500-BUILD-CURRENT-CODES THRU 6500-EXIT
041513
           GO TO 1000-SHOW-LETR-RECORD

           .
       0200-RECEIVE.

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
                                                                        
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   
               MOVE LOW-VALUES             TO  EL104ZI                  
               MOVE ER-7008                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                     TO  MAINTL                   
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           IF PI-PROCESSOR-ID EQUAL 'LGXX'                              
               NEXT SENTENCE                                            
           ELSE                                                         
               EXEC CICS READQ TS                                       
                   QUEUE  (PI-SECURITY-TEMP-STORE-ID)                   
                   INTO   (SECURITY-CONTROL)                            
                   LENGTH (SC-COMM-LENGTH)                              
                   ITEM   (SC-ITEM)                                     
               END-EXEC                                                 
               MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP           
               MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.           
                                                                        
           EXEC CICS RECEIVE                                            
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               INTO     (EL104ZI)
           END-EXEC
                                                                        
           IF PFKEYL = +0                                     
               GO TO 0300-CHECK-PFKEYS.                                 
                                                                        
           IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)       
               MOVE PF-VALUES (PFKEYI)     TO  EIBAID                   
           ELSE                                                         
               MOVE ER-0029                TO  EMI-ERROR                
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
                                                                        
       0300-CHECK-PFKEYS.                                               
                                                                        
           DISPLAY ' MADE IT TO 0300 '
           IF EIBAID EQUAL DFHPF23                                      
               GO TO 8810-PF23.                                         
                                                                        
           IF EIBAID EQUAL DFHPF24                                      
               GO TO 9200-RETURN-MAIN-MENU.                             
                                                                        
           IF EIBAID EQUAL DFHPF12                                      
               GO TO 9500-PF12.                                         
                                                                        
           IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)      
               MOVE ER-0050            TO EMI-ERROR                     
               GO TO 0320-INPUT-ERROR.                                  

           IF EIBAID EQUAL DFHENTER                                     
               GO TO 0330-EDIT-DATA.                                    
                                                                        
           MOVE ER-0029                    TO  EMI-ERROR.               
                                                                        
       0320-INPUT-ERROR.                                                

           DISPLAY ' MADE IT TO 0320 '                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE AL-UNBON                   TO  PFKEYA.                  
           MOVE -1                         TO  PFKEYL.                  
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           EJECT                                                        
       0330-EDIT-DATA.                                                  
                                                                        
           IF NOT DISPLAY-CAP                                           
               MOVE 'READ'                 TO  SM-READ                  
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           
               MOVE ER-0070                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                     TO  MAINTL                   
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
           IF MAINTI EQUAL 'S'                                          
              GO TO 1000-SHOW-LETR-RECORD.                             
                                                                        
           IF MAINTI = 'C' OR 'A'
              IF NOT MODIFY-CAP                                         
                 PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                 MOVE ER-0070          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE LOW-VALUES       TO EL104ZO
                 MOVE -1               TO MAINTL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           IF MAINTI = 'C'
              GO TO 2000-CHANGE-LETR-RECORD
           END-IF

           IF MAINTI = 'A'
              GO TO 3000-ADD-LETR-RECORD
           END-IF

           MOVE ER-0023                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  MAINTL.                  
           MOVE AL-UABON                   TO  MAINTA.                  
           GO TO 8200-SEND-DATAONLY.                                    

       1000-SHOW-LETR-RECORD.

           MOVE PI-CURR-LETR-KEY       TO ELLETR-KEY

           EXEC CICS STARTBR
               DATASET   (ELLETR-FILE-ID)
               RIDFLD    (ELLETR-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              PERFORM UNTIL I-SAY-TO-STOP
                 OR WE-FOUND-IT
                 EXEC CICS READNEXT
                     DATASET    (ELLETR-FILE-ID)
                     SET        (ADDRESS OF TEXT-FILES)
                     RIDFLD     (ELLETR-KEY)
                     RESP       (WS-RESPONSE)
                 END-EXEC
                 IF (RESP-NORMAL)
                    AND (TX-LETTER-NO = PI-COMM-CONTROL (1:4))
                    IF (TX-LINE-SQUEEZE-CONTROL = 'Z')
                       SET WE-FOUND-IT   TO TRUE
                    END-IF
                 ELSE
                    SET I-SAY-TO-STOP  TO TRUE
                 END-IF
              END-PERFORM
           END-IF

           IF WE-FOUND-IT
              MOVE TX-CONTROL-PRIMARY  TO PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY
              GO TO 7000-BUILD-OUTPUT-MAP
           END-IF

           MOVE ER-0418                TO EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE -1                     TO MAINTL
           MOVE 'A'                    TO MAINTI
           MOVE AL-UANON               TO MAINTA
           MOVE PI-COMM-CONTROL (1:4)  TO ZLETRI
           MOVE +4                     TO ZLETRL
           MOVE AL-UANON               TO ZLETRA

           GO TO 8100-SEND-INITIAL-MAP

           .
       2000-CHANGE-LETR-RECORD.

           display ' made it to 2000 '
           display ' curr   ' pi-curr-letr-key
           display ' prev   ' pi-prev-letr-key

           IF PI-CURR-LETR-KEY NOT = PI-PREV-LETR-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT

           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.

           MOVE PI-CURR-LETR-KEY       TO ELLETR-KEY

           .
       2000-CHANGE-CONTINUE.

           EXEC CICS READ
               DATASET    (ELLETR-FILE-ID)
               SET        (ADDRESS OF TEXT-FILES)
               RIDFLD     (ELLETR-KEY)
               UPDATE
               RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NOTFND
              IF ELLETR-SEQ-NO = +0
                 GO TO 3000-CONTINUE-ADD
              END-IF
           END-IF

           IF TX-LAST-MAINTENANCED-BY NOT = PI-UPDATE-BY OR            
              TX-LAST-MAINTENANCED-DT NOT = PI-UPDATE-DT
               EXEC CICS UNLOCK                                         
                   DATASET   (ELLETR-FILE-ID)                           
               END-EXEC                                                 
               MOVE ER-0068                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-LETR-RECORD.                             
                                                                        
           MOVE PI-PROCESSOR-ID        TO TX-LAST-MAINTENANCED-BY

           MOVE SAVE-BIN-DATE          TO TX-LAST-MAINTENANCED-DT

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

           IF ZCOPIESL > +0
              MOVE ZCOPIESO            TO W-NUMBER-OF-COPIES
           END-IF

           IF ZFADAYSL > +0
              MOVE ZFADAYSO            TO W-DAYS-TO-FOLLOW-UP
           END-IF

           IF ZRDAYSL > +0
092413        MOVE ZRDAYSO             TO W-DAYS-TO-RESEND
           END-IF

           IF ZFORML > +0
              MOVE ZFORMI              TO W-FORM-TO-RESEND
           END-IF

           IF ZPROMPTL > +0
              MOVE ZPROMPTI            TO W-PROMPT-LETTER
           END-IF

           IF ZENCCDL > +0
              MOVE ZENCCDI             TO W-ENCLOSURE-CD
           END-IF

           IF ZACLOSEL > +0
              MOVE ZACLOSEI            TO W-AUTO-CLOSE-IND
           END-IF

           IF ZBENEL > +0
              MOVE ZBENEI              TO W-LETTER-TO-BENE
           END-IF
092413
092413     IF ZBARCODL > +0
092413        MOVE ZBARCODI            TO W-ADD-BAR-CODE
092413     END-IF
092413
092413     IF ZRETENVL > +0
092413        MOVE ZRETENVI            TO W-HAS-RETURN-ENV
092413     END-IF

           MOVE W-Z-CONTROL-DATA       TO TX-TEXT-LINE

           .
       2000-CONTINUE-CHANGE.                                            

           DISPLAY ' ABOUT TO REWRITE '
           EXEC CICS REWRITE                                            
               DATASET   (ELLETR-FILE-ID)
               FROM      (TEXT-FILES)
           END-EXEC.                                                    
                                                                        
           MOVE ER-0000                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    

           GO TO 1000-SHOW-LETR-RECORD

           .                                                            
       3000-ADD-LETR-RECORD.

           display ' made it to 3000 '
           display ' curr   ' pi-curr-letr-key
           display ' prev   ' pi-prev-letr-key

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT

           .
       3000-CONTINUE-ADD.

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE SPACES                 TO ELLETR-KEY
           MOVE PI-COMPANY-CD          TO ELLETR-COMPANY-CD
           MOVE ZLETRI                 TO ELLETR-ACCESS-CD
           MOVE +0                     TO ELLETR-SEQ-NO

           EXEC CICS READ
               DATASET    (ELLETR-FILE-ID)
               SET        (ADDRESS OF TEXT-FILES)
               RIDFLD     (ELLETR-KEY)
               GTEQ
               RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              IF TX-LETTER-NO = ZLETRI
                 IF TX-LINE-SQUEEZE-CONTROL = 'Z'
                    MOVE ER-0132       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    MOVE -1            TO ZLETRL
                    MOVE AL-UABON      TO ZLETRA
                    GO TO 8200-SEND-DATAONLY
                 ELSE
                    MOVE +0            TO WS-SEQ-NO-TO-USE
                 END-IF
              ELSE
                 MOVE +1               TO WS-SEQ-NO-TO-USE
              END-IF
           ELSE
              MOVE +1                  TO WS-SEQ-NO-TO-USE
           END-IF

           EXEC CICS GETMAIN
              SET(ADDRESS OF TEXT-FILES)
              LENGTH(ELLETR-LENGTH)
           END-EXEC

           MOVE SPACES                 TO TEXT-FILES
           MOVE 'TL'                   TO TEXT-FILE-ID
           MOVE PI-COMPANY-CD          TO TX-COMPANY-CD
           MOVE ZLETRI                 TO TX-LETTER-NO
           MOVE WS-SEQ-NO-TO-USE       TO TX-LINE-SEQUENCE
           MOVE 'Z'                    TO TX-LINE-SQUEEZE-CONTROL
           MOVE PI-PROCESSOR-ID        TO TX-LAST-MAINTENANCED-BY
           MOVE SAVE-BIN-DATE          TO TX-LAST-MAINTENANCED-DT

           MOVE SPACES                 TO W-Z-CONTROL-DATA

           IF ZCOPIESL > +0
              MOVE ZCOPIESO            TO W-NUMBER-OF-COPIES
           ELSE
              MOVE 1                   TO W-NUMBER-OF-COPIES
           END-IF

           IF ZFADAYSL > +0
              MOVE ZFADAYSO            TO W-DAYS-TO-FOLLOW-UP
           ELSE
              MOVE ZEROS               TO W-DAYS-TO-FOLLOW-UP
           END-IF

           IF ZRDAYSL > +0
092413        MOVE ZRDAYSO             TO W-DAYS-TO-RESEND
           ELSE
092413        MOVE ZEROS               TO W-DAYS-TO-RESEND
           END-IF

           IF ZFORML > +0
              MOVE ZFORMI              TO W-FORM-TO-RESEND
           END-IF

           IF ZPROMPTL > +0
              MOVE ZPROMPTI            TO W-PROMPT-LETTER
           END-IF

           IF ZENCCDL > +0
              MOVE ZENCCDI             TO W-ENCLOSURE-CD
           END-IF

           IF ZACLOSEL > +0
              MOVE ZACLOSEI            TO W-AUTO-CLOSE-IND
           END-IF

           IF ZBENEL > +0
              MOVE ZBENEI              TO W-LETTER-TO-BENE
           END-IF
092413
092413     IF ZBARCODL > +0
092413        MOVE ZBARCODI            TO W-ADD-BAR-CODE
092413     END-IF
092413
092413     IF ZRETENVL > +0
092413        MOVE ZRETENVI            TO W-HAS-RETURN-ENV
092413     END-IF

           MOVE W-Z-CONTROL-DATA       TO TX-TEXT-LINE

           DISPLAY ' ABOUT TO WRITE '

           MOVE TX-CONTROL-PRIMARY     TO ELLETR-KEY
                                          PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY

           EXEC CICS WRITE
              DATASET (ELLETR-FILE-ID)
              FROM    (TEXT-FILES)
              RIDFLD  (ELLETR-KEY)
           END-EXEC
                                                                        
           MOVE ER-0000                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    

           GO TO 1000-SHOW-LETR-RECORD

           .                                                            
       5000-FIND-NEXT-PROD-RECORD.                                      
                                                                        
      *    MOVE PI-PROD-KEY            TO ELLETR-KEY
      *
      *    EXEC CICS STARTBR                                            
      *        DATASET   (ELLETR-FILE-ID)                               
      *        RIDFLD    (ELLETR-KEY)                                   
      *        GTEQ                                                     
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *
      *    IF NOT RESP-NORMAL
      *       MOVE -1                  TO PFKEYL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *       GO TO 8200-SEND-DATAONLY
      *    END-IF

           .
       5000-READNEXT-LOOP.                                              

      *    EXEC CICS READNEXT                                           
      *        DATASET   (ELLETR-FILE-ID)                               
      *        SET       (ADDRESS OF PRODUCT-MASTER)                    
      *        RIDFLD    (ELLETR-KEY)                                   
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *                                                                 
      *    IF (NOT RESP-NORMAL)
      *       OR (PD-COMPANY-CD NOT = PI-COMPANY-CD)
      *       PERFORM 5000-END-BROWSE
      *       MOVE -1                  TO PFKEYL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *       GO TO 1000-SHOW-LETR-RECORD
      *    END-IF
      *
      *    IF ELLETR-KEY = PI-PREV-LETR-KEY
      *       GO TO 5000-READNEXT-LOOP
      *    END-IF
      *                                                                 
      *    MOVE ELLETR-KEY             TO PI-PROD-KEY
      *
      *    PERFORM 5000-END-BROWSE
      *    GO TO 7000-BUILD-OUTPUT-MAP
           .
       5000-END-BROWSE.                                                 
                                                                        
      *    EXEC CICS ENDBR                                              
      *        DATASET   (ELLETR-FILE-ID)                               
      *    END-EXEC

           .
       5100-FIND-PREV-PROD-RECORD.                                      
                                                                        
      *    MOVE PI-PREV-LETR-KEY       TO ELLETR-KEY
                                                                        
      *    IF STATEL > +0
      *       MOVE STATEI              TO ELLETR-STATE
      *    END-IF
      *    IF PRODCDL > +0
      *       MOVE PRODCDI             TO ELLETR-PROD-CD
      *    END-IF
      *    IF BENTYPL > +0                                 
      *        MOVE BENTYPI                TO  ELLETR-BEN-TYPE.         
      *    IF BENCODEL > +0                                  
      *        MOVE BENCODEI               TO  ELLETR-BEN-CODE.         
      *    IF EXPDTL IS GREATER THAN +0                                 
      *        MOVE EXPDTI                 TO  DEEDIT-FIELD             
      *        PERFORM 8600-DEEDIT THRU 8600-EXIT                       
      *        IF WS-NUMVAL-OF-DEEDIT >= 999999               
      *            MOVE HIGH-VALUES        TO  ELLETR-EXP-DT            
      *        ELSE                                                     
      *          STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:12)
      *             DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
      *          END-STRING
      *           MOVE 'L'                    TO  DC-OPTION-CODE        
      *            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
      *            IF NO-CONVERSION-ERROR                               
      *                MOVE DC-BIN-DATE-1  TO  ELLETR-EXP-DT            
      *            ELSE                                                 
      *                MOVE LOW-VALUES     TO  ELLETR-EXP-DT.           
      *                                                                 
      *    EXEC CICS HANDLE CONDITION                                   
      *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)                       
      *    END-EXEC.                                                    
      *                                                                 
      *    EXEC CICS STARTBR                                            
      *        DATASET   (ELLETR-FILE-ID)                               
      *        RIDFLD    (ELLETR-KEY)                                   
      *        GTEQ                                                     
      *    END-EXEC.                                                    
                                                                        
       5100-READPREV-LOOP.                                              
      *    EXEC CICS READPREV                                           
      *        DATASET   (ELLETR-FILE-ID)                               
      *        SET       (ADDRESS OF PRODUCT-MASTER)                       
      *        RIDFLD    (ELLETR-KEY)                                   
      *    END-EXEC.                                                    
      *                                                                 
      *    IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
      *        GO TO 5100-UNSUCCESSFUL-SEARCH.                          
      *                                                                 
      *    IF ELLETR-KEY IS EQUAL TO PI-PREV-LETR-KEY
      *        GO TO 5100-READPREV-LOOP.                                
      *
      *    MOVE ELLETR-KEY             TO PI-PROD-KEY
      *
      *    GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
       5100-END-BROWSE.                                                 
      *    EXEC CICS ENDBR                                              
      *        DATASET   (ELLETR-FILE-ID)                               
      *    END-EXEC.                                                    
                                                                        
       5100-UNSUCCESSFUL-SEARCH.                                        
                                                                        
           PERFORM 5100-END-BROWSE.                                     

           MOVE ER-0131                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 1000-SHOW-LETR-RECORD.                                 

           .                                                            
       6000-EDIT-INPUT-DATA.                                            

           IF MAINTI = 'A'
              IF ZLETRL > +0
                 AND ZLETRI NOT = SPACES
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZCOPIESA
                 MOVE -1               TO ZCOPIESL
                 MOVE ER-0005          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           IF ZCOPIESL > +0
              IF ZCOPIESI NUMERIC
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZCOPIESA
                 MOVE -1               TO ZCOPIESL
                 MOVE ER-0184          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ZFADAYSL > +0
              IF ZFADAYSI NUMERIC
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZFADAYSA
                 MOVE -1               TO ZFADAYSL
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ZRDAYSL > +0
              IF ZRDAYSI NUMERIC
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZRDAYSA
                 MOVE -1               TO ZRDAYSL
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ZPROMPTL > +0
              IF ZPROMPTI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE AL-UABON         TO ZPROMPTA
                 MOVE -1               TO ZPROMPTL
                 MOVE ER-0627          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF MAINTI = 'A'
              AND ZENCCDL <= 0
              MOVE AL-UABON            TO ZENCCDA
              MOVE -1                  TO ZENCCDL
              MOVE ER-1560             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF ZENCCDL > +0
041513        IF ZENCCDI = '@'
041513            CONTINUE
041513        ELSE
041513           MOVE SPACES             TO ELENCC-KEY
041513           MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
041513           MOVE '1'                TO ELENCC-REC-TYPE
041513           MOVE ZENCCDI            TO ELENCC-ENC-CODE
041513
041513           EXEC CICS READ
041513               DATASET    (ELENCC-FILE-ID)
041513               SET        (ADDRESS OF ENCLOSURE-CODES)
041513               RIDFLD     (ELENCC-KEY)
041513               RESP       (WS-RESPONSE)
041513           END-EXEC
041513
041513           IF RESP-NORMAL
041513              CONTINUE
041513           ELSE
041513              MOVE AL-UABON         TO ZENCCDA
041513              MOVE -1               TO ZENCCDL
041513              MOVE ER-1560          TO EMI-ERROR
041513              PERFORM 9900-ERROR-FORMAT
041513                                 THRU 9900-EXIT
041513           END-IF
              END-IF
           END-IF

           IF ZACLOSEL > +0
              IF ZACLOSEI = 'S' OR 'C' OR 'B' OR ' '
                 CONTINUE
              ELSE
                 MOVE -1               TO ZACLOSEL
                 MOVE AL-UABON         TO ZACLOSEA
                 MOVE ER-1562          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF ZBENEL > +0
              IF ZBENEI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE AL-UABON         TO ZBENEA
                 MOVE -1               TO ZBENEL
                 MOVE ER-0627          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
092413
092413     IF ZBARCODL > +0
092413        IF ZBARCODI = 'Y' OR 'N' OR ' '
092413           CONTINUE
092413        ELSE
092413           MOVE AL-UABON         TO ZBARCODA
092413           MOVE -1               TO ZBARCODL
092413           MOVE ER-0627          TO EMI-ERROR
092413           PERFORM 9900-ERROR-FORMAT
092413                                 THRU 9900-EXIT
092413        END-IF
092413     END-IF
092413
092413     IF ZRETENVL > +0
092413        IF ZRETENVI = 'Y' OR 'N' OR ' '
092413           CONTINUE
092413        ELSE
092413           MOVE AL-UABON         TO ZRETENVA
092413           MOVE -1               TO ZRETENVL
092413           MOVE ER-0627          TO EMI-ERROR
092413           PERFORM 9900-ERROR-FORMAT
092413                                 THRU 9900-EXIT
092413        END-IF
092413     END-IF

           .
       6000-EXIT.                                                       
           EXIT.                                                        
041513
041513     EJECT                                                        
041513 6500-BUILD-CURRENT-CODES.                                           
041513      MOVE SPACES             TO ELENCC-KEY
041513      MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
041513      MOVE '1'                TO ELENCC-REC-TYPE
041513      MOVE LOW-VALUES         TO ELENCC-ENC-CODE
041513      MOVE SPACES             TO HOLD-ENC-CODES
041513      MOVE '@'                TO HOLD-ENC-CODES (1:1)
041513      MOVE HOLD-ENC-CODES     TO PI-ENC-CODES
041513
041513      EXEC CICS STARTBR
041513         DATASET   (ELENCC-FILE-ID)
041513         RIDFLD    (ELENCC-KEY)
041513         GTEQ
041513         RESP      (WS-RESPONSE)
041513      END-EXEC
041513
041513      IF RESP-NORMAL
041513        PERFORM UNTIL DONE-WITH-ENCC
041513           MOVE PI-ENC-CODES TO HOLD-ENC-CODES
041513           EXEC CICS READNEXT
041513               DATASET    (ELENCC-FILE-ID)
041513               SET        (ADDRESS OF ENCLOSURE-CODES)
041513               RIDFLD     (ELENCC-KEY)
041513               RESP       (WS-RESPONSE)
041513           END-EXEC
041513           IF (RESP-NORMAL)
041513              AND (ELENCC-COMPANY-CD = PI-COMPANY-CD)
041513              AND (ELENCC-REC-TYPE = '1')
041513                 PERFORM VARYING WS-SUB1 FROM 5 BY -1 
041513                      UNTIL ELENCC-ENC-CODE (WS-SUB1:1) <> ' '
041513                 END-PERFORM
041513                 IF WS-SUB1 > 2
041513                     SUBTRACT +1 FROM WS-SUB1
041513                 END-IF
041513                 MOVE ELENCC-ENC-CODE (WS-SUB1:2) TO 
041513                             WS-TEST-STATE
041513                 IF STATE-SPECIFIC AND 
041513                    ELENCC-ENC-CODE NOT = 'ENV'
041513                    CONTINUE    
041513                 ELSE
041513                    STRING HOLD-ENC-CODES DELIMITED BY ' '
041513                         ','
041513                         ELENCC-ENC-CODE DELIMITED BY ' '
041513                    INTO PI-ENC-CODES
041513                 end-if
041513           ELSE
041513              SET DONE-WITH-ENCC  TO TRUE
041513           END-IF
041513        END-PERFORM
041513     END-IF
041513
041513     MOVE +53 TO PI-SPLIT-SUB
041513     PERFORM UNTIL PI-ENC-CODES (PI-SPLIT-SUB:1) = ' ' OR ','
041513         SUBTRACT +1 FROM PI-SPLIT-SUB
041513     END-PERFORM
041513
041513     .
041513 6500-EXIT.                                                       
041513     EXIT.                                                        
                                                                        
           EJECT                                                        
       7000-BUILD-OUTPUT-MAP.                                           
                                                                        
           MOVE LOW-VALUES             TO EL104ZO
           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

           MOVE TX-LETTER-NO           TO ZLETRO
           MOVE W-NUMBER-OF-COPIES     TO ZCOPIESO
           MOVE W-DAYS-TO-FOLLOW-UP    TO ZFADAYSO
092413     MOVE W-DAYS-TO-RESEND       TO ZRDAYSO
           MOVE W-FORM-TO-RESEND       TO ZFORMO
           MOVE W-PROMPT-LETTER        TO ZPROMPTO
           MOVE W-ENCLOSURE-CD         TO ZENCCDO
           MOVE W-AUTO-CLOSE-IND       TO ZACLOSEO
           MOVE W-LETTER-TO-BENE       TO ZBENEO
092413     MOVE W-ADD-BAR-CODE         TO ZBARCODO
092413     MOVE W-HAS-RETURN-ENV       TO ZRETENVO
041513     MOVE PI-SPLIT-SUB           TO WS-SUB1
041513     COMPUTE WS-SUB2 = PI-SPLIT-SUB + 1
041513     COMPUTE WS-SUB3 = 120 - PI-SPLIT-SUB           
041513     MOVE PI-ENC-CODES (1:WS-SUB1) TO ZCODES1O
041513     MOVE PI-ENC-CODES (WS-SUB2:WS-SUB3) TO ZCODES2O

           MOVE TX-LAST-MAINTENANCED-BY TO LSTUSRO
                                          PI-UPDATE-BY
           MOVE TX-LAST-MAINTENANCED-DT TO PI-UPDATE-DT
                                          DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-DAYS          
                                           DC-ELAPSED-MONTHS.       
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO
           ELSE
              MOVE LOW-VALUES          TO LSTDTEO
           END-IF

           MOVE -1                     TO MAINTL

           .
       8100-SEND-INITIAL-MAP.                                           
                                                                        
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSGO.
           MOVE EIBTIME                    TO  TIME-IN.                 
           MOVE SAVE-DATE                  TO  DATEO.                   
           MOVE TIME-OUT                   TO  TIMEO
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
020816     IF PI-COMPANY-ID = 'DCC' OR 'VPP'
041513        MOVE '6-IU CLM FORM,   7-CONTINUING IU,  ' TO DCCOPTO
           END-IF
                                                                        
           EXEC CICS SEND                                               
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL104ZO)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           MOVE '104Z'                 TO PI-CURRENT-SCREEN-NO

           GO TO 9100-RETURN-TRAN

           .
       8200-SEND-DATAONLY.                                              

           DISPLAY ' MADE IT TO 8200 '
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSGO.
           MOVE EIBTIME                    TO  TIME-IN.                 
           MOVE SAVE-DATE                  TO  DATEO.                   
           MOVE TIME-OUT                   TO  TIMEO.                   
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
                                                                        
           EXEC CICS SEND                                               
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL104ZO)                                       
               DATAONLY                                                 
               CURSOR                                                   
           END-EXEC.                                                    

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
               END-EXEC.                                                

           .
       8500-DEEDIT.                                                     

           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-9V999-OF-DEEDIT

           .
       8500-EXIT.                                                       
           EXIT.                                                        

       8600-DEEDIT.                                                     

           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT

           .
       8600-EXIT.                                                       
           EXIT.                                                        

       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG               TO  LOGOFF-MSG.              
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID                     TO  PI-ENTRY-CD-1.           
           MOVE XCTL-005                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       8850-DUPREC.                                                     
           MOVE ER-0132                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  MAINTL
           MOVE AL-UABON                   TO  ZLETRA
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       8870-NOTOPEN.                                                    
                                                                        
           MOVE LOW-VALUES                 TO  EL104ZO.                 
           MOVE -1                         TO  MAINTL.                  
           MOVE ER-0701                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       8880-NOT-FOUND.                                                  

           MOVE ER-0702                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  MAINTL
           MOVE AL-UABON                   TO  ZLETRA
           GO TO 8100-SEND-INITIAL-MAP.                                 

           .
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.        
                                                                        
           EXEC CICS RETURN                                             
               TRANSID    (TRANS-ID)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9200-RETURN-MAIN-MENU.                                           
                                                                        
           MOVE XCTL-626                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9300-XCTL.                                                       
           EXEC CICS XCTL                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-010                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR   (8300-SEND-TEXT)                              
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.      
           MOVE ' '                        TO  PI-ENTRY-CD-1.           
           MOVE XCTL-005                   TO  PGM-NAME.                
           MOVE PGM-NAME                   TO  LOGOFF-PGM.              
           MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.             
           GO TO 9300-XCTL.                                             
                                                                        
           EJECT                                                        
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
               MOVE LINK-001               TO  PGM-NAME                 
               EXEC CICS LINK                                           
                   PROGRAM    (PGM-NAME)                                
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
                   LENGTH     (EMI-COMM-LENGTH)                         
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE LINK-004                   TO  PGM-NAME.                
           MOVE DFHEIBLK                   TO  EMI-LINE1.               
           EXEC CICS LINK                                               
               PROGRAM   (PGM-NAME)                                     
               COMMAREA  (EMI-LINE1)                                    
               LENGTH    (72)                                           
           END-EXEC.                                                    
                                                                        
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
       9995-SECURITY-VIOLATION.
                                   COPY ELCSCTP.                        

       9995-EXIT.                                                       
           EXIT.                                                        
