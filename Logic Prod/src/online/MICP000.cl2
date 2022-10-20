       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. MICP000.                                             
      *REMARKS.                                                         
120803****************************************
120803*
120803*  This program does not run in Logic
120803*  and the code is no longer maintained
120803*
120803****************************************
      * MICP000 - CSO MICR DRAFT PRINT PROGRAM.                         
      * MODS:                                                           
      * JWBA 6/93 CR#93113-0073 (93116-6802 TSO#) ---                   
      * (CSO882)  DRAFT REPRINT REPORT FORM IS CHANGED                  
      * (CSO882)  FROM 'DRFT' TO 'LX64' IN P4000-PRINT-REPORT.          
      * DMB  6/93 CR#931530089                                          
      * (CSO884)  PRINT ONLY 1 COPY OF THE 'F420C' REPORT.              
DAN01 * DAN  4/97 CR#970180050                                          
      * (DAN01)   ADD MICR0031 FOR MSA/AP CHECKS                        
DAN02 * DAN  5/97 CR#970800037                                          
      * (DAN02)   ADD MICR3CSI FOR FREEDOM/AP CHECKS                    
DAN03 * DAN  6/97 CR#963310009                                          
      * (DAN03)   ADD MICR750  FOR AGENT COMMISSION CHECKS              
JXN01 * JXNA 8/97 LOGIC UPGRADE PROJECT                                 
JXN01 * (JXN01)   ADD MICR420E FOR CLAIM DRAFT CHECKS -                 
JXN01 * (JXN01)   (DO NOT PRINT COPY OF BENEFIT DRAFT)                  
CSOY2K* DJNA 1997 Y2K PHASE II                                          
CS1113* PJYA 1/98 ELIMINATED COPY OF REPORT FOR SAVE-FORM '420D'        
CS1113*           ROUTED TO PRINTER DMD4.                               
JXN02 * JXNA 8/98 REMOVED FORM MICR420D (NO LONGER USED)                
ESP01 * ESPA 9/98 CR# 1998090100008                                     
ESP01 *           PUT DRAFT PRINT REPORT FOR FORM '420E' BACK IN.       
ESP01 *           REPORT HAD BEEN REMOVED (WHEN FORM WAS '420D'.)       
DAN04 * DANA 3/00 CR# 2000022300004 - ADD FORM CEN1 - CENSTAT DEALER    
DAN05 * DANA 3/00 CR# 2000022300004 - ADD FORM CEN2 - CENSTAT, INC.     
CSODJN* DJNA 4/01/00 CR#2000030100009 - DRAFT NUMBER EXPANSION.         
DAN06 * DANA 5/07/00 CR#2000021500004 - NEW CID DRAFT MESSAGES          
DAN07 * DANA 4/26/01 CR#1998020500005 - STOP PRINTING 420C COPIES       
      ***************************************************************** 
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
      ***************************************************************** 
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  WS-MICR-FLAG.                                                
           05  WS-CSO-PROGRAM     PIC X(8)  VALUE SPACES.               
           05  WS-CSO-MESSAGE     PIC X(10) VALUE SPACES.               
                                                                        
       01  ERROR-MSG.                                                   
           02  ERR-MSG            PIC X(80).                            
           02  FILLER             PIC X(160) VALUE SPACES.              
           02  FILLER             PIC X(80)                             
               VALUE 'DEPRESS ANY KEY TO CONTINUE'.                     
       01  ERR001                 PIC X(80).                            
       01  ERR101A                PIC X(80).                            
       01  ERR101B                PIC X(80).                            
       01  ERR101C                PIC X(80).                            
       01  ERR201                 PIC X(80).                            
       01  SAVE-SEL001            PIC X(1)      VALUE ' '.              
       01  SAVE-SEL101              PIC X(1)      VALUE ' '.            
       01  LOW-VAL                  PIC X(1)      VALUE LOW-VALUES.     
       01  FORM-KEY.                                                    
           05  SAVE-FORM            PIC X(4).                           
CSODJN     05  FILLER               PIC X(15).                          
       01  COPYA                    PIC X(8).                           
       01  COPYB                    PIC X(8).                           
       01  SEQ-NUMBER               PIC Z(3)9.                          
       01  PRINT-LINE               PIC X(133).                         
       01  PRINT-TIME               PIC S9(15) COMP-3.                  
       01  TIME-MMDDYY              PIC X(8).                           
       01  TIME-CLOCK               PIC X(8).                           
       01  REPORT-LINE              PIC X(80).                          
       01  REPORT-PRINTER           PIC X(8).                           
       01  REPORT-FORM              PIC X(8).                           
       01  REPORT-CLASS             PIC X(1).                           
       01  REPORT-NAME              PIC X(8).                           
       01  COPY-NAME                PIC X(8).                           
       01  CARRIAGE-CTL             PIC X(1).                           
       01  LINE-SKIP                PIC S9(4) COMP.                     
       01  LINES-LEFT               PIC S9(4) COMP.                     
       01  WORK-ADDRESS.                                                
           05  WORK-ADDRESS-CHAR    PIC X(1) OCCURS 50 TIMES.           
       01  ADDRESS-ONE.                                                 
           05  ADDRESS-ONE-1        PIC X(50).                          
           05  ADDRESS-ONE-2        PIC X(50).                          
           05  ADDRESS-ONE-3        PIC X(50).                          
           05  ADDRESS-ONE-4        PIC X(50).                          
           05  ADDRESS-ONE-ZIP      PIC X(9).                           
       01  ADDRESS-ONE-RED REDEFINES ADDRESS-ONE.                       
           05  ADDRESS-ONE-LINE     PIC X(50) OCCURS 4 TIMES.           
       01  ADDRESS-TWO.                                                 
           05  ADDRESS-TWO-1        PIC X(50).                          
           05  ADDRESS-TWO-2        PIC X(50).                          
           05  ADDRESS-TWO-3        PIC X(50).                          
           05  ADDRESS-TWO-4        PIC X(50).                          
           05  ADDRESS-TWO-ZIP      PIC X(9).                           
       01  ADDRESS-TWO-RED REDEFINES ADDRESS-TWO.                       
           05  ADDRESS-TWO-LINE     PIC X(50) OCCURS 4 TIMES.           
       01  ADDRESS-THREE.                                               
           05  ADDRESS-THREE-1      PIC X(50).                          
           05  ADDRESS-THREE-2      PIC X(50).                          
           05  ADDRESS-THREE-3      PIC X(50).                          
           05  ADDRESS-THREE-4      PIC X(50).                          
           05  ADDRESS-THREE-ZIP    PIC X(9).                           
       01  ADDRESS-THREE-RED REDEFINES ADDRESS-THREE.                   
           05  ADDRESS-THREE-LINE   PIC X(50) OCCURS 4 TIMES.           
       01  ADDRESS-WORK.                                                
           05  ADDRESS-WORK-1       PIC X(50).                          
           05  ADDRESS-WORK-2       PIC X(50).                          
           05  ADDRESS-WORK-3       PIC X(50).                          
           05  ADDRESS-WORK-4       PIC X(50).                          
           05  ADDRESS-WORK-ZIP     PIC X(9).                           
       01  ADDRESS-WORK-RED REDEFINES ADDRESS-WORK.                     
           05  ADDRESS-WORK-LINE    PIC X(50) OCCURS 4 TIMES.           
       01  WORK-ZIP.                                                    
           05  WORK-ZIP-CODE        PIC X(5).                           
           05  WORK-ZIP-SUFFIX      PIC X(4).                           
       01  WORK-ZIP-1 REDEFINES WORK-ZIP.                               
           05  WORK-ZIP-PREFIX      PIC X(4).                           
           05  WORK-ZIP-CODE-1      PIC X(5).                           
                                                                        
DAN01  COPY SPX.                                                        
                                                                        
       01  OPER-ID                PIC X(3).                             
       01  SAVE-FLAG              PIC X(1).                             
       01  ERROR-FLAG             PIC X(1).                             
           88  IS-ERROR           VALUE 'Y'.                            
           88  NO-ERROR           VALUE 'N'.                            
       01  BROWSE-FLAG            PIC X(1).                             
           88  BROWSE-IS-DONE     VALUE 'Y'.                            
           88  BROWSE-NOT-DONE    VALUE 'N'.                            
       01  MAIN-DONE-FLAG         PIC X(1).                             
           88  MAIN-DONE          VALUE 'Y'.                            
           88  MAIN-NOT-DONE      VALUE 'N'.                            
       01  SECOND-DONE-FLAG       PIC X(1).                             
           88  SECOND-DONE        VALUE 'Y'.                            
           88  SECOND-NOT-DONE    VALUE 'N'.                            
       01  THIRD-DONE-FLAG        PIC X(1).                             
           88  THIRD-DONE         VALUE 'Y'.                            
           88  THIRD-NOT-DONE     VALUE 'N'.                            
       01  DONE-FLAG              PIC X(1).                             
           88  DONE               VALUE 'Y'.                            
           88  NOT-DONE           VALUE 'N'.                            
       01  PRT-FLAG               PIC X(1).                             
           88  IS-PRINT-TO-DO     VALUE 'Y'.                            
       01  TABLE-FLAG             PIC X(1).                             
           88  TABLE-FULL         VALUE 'Y'.                            
       01  OUTPUT-LINES.                                                
           02  OUTPUT-LINE        PIC X(50) OCCURS 16 TIMES.            
       01  PRINT-FLAGS.                                                 
           05  PRT-IT             PIC X(1) OCCURS 16 TIMES.             
       01  DRAFT-NOS.                                                   
CSODJN     05  DRAFT-NO           PIC X(10) OCCURS 16 TIMES.            
       01  TOT-DRAFTS             PIC S9(4) COMP.                       
       01  UNPRTED-DRAFTS         PIC S9(4) COMP.                       
       01  EDIT-DRAFTS            PIC Z(3)9.                            
       01  MIC1-COUNT             PIC S9(4) COMP.                       
       01  MIC1-AMOUNT-PAID       PIC S9(9)V9(2) COMP-3.                
       01  MICR-PRINT-COUNT       PIC S9(4) COMP.                       
       01  MICR-PRINT-AMOUNT      PIC S9(9)V9(2) COMP-3.                
       01  EDIT-NUMBER            PIC Z(3)9.                            
       01  EDIT-AMOUNT            PIC ZZZ,ZZ9.99.                       
       01  EDIT-AMOUNT2           PIC ZZZ,ZZZ,ZZ9.99.                   
       01  EDIT-CHECK             PIC ***,**9.99.                       
       01  EDIT-CHECK2            PIC ***,***,**9.99.                   
       01  MIC1-COMMAREA.                                               
           05  MIC1-ECB-ADDRESS     PIC S9(8) COMP.                     
           05  MICR-QUEUE             PIC X(8).                         
           05  MICR-QUEUE-RED REDEFINES MICR-QUEUE.                     
             10  MICR-QUEUE-NAME    PIC X(4).                           
             10  MICR-QUEUE-TERM    PIC X(4).                           
           05  MIC1-QUEUE             PIC X(8).                         
           05  MIC1-QUEUE-RED REDEFINES MIC1-QUEUE.                     
             10  MIC1-QUEUE-NAME    PIC X(4).                           
             10  MIC1-QUEUE-TERM    PIC X(4).                           
       01  LINE-INFO.                                                   
           05  QUEUE-LOW          PIC Z(4).                             
           05  FILLER             PIC X(4) VALUE ' TO '.                
           05  QUEUE-HIGH         PIC Z(4).                             
           05  FILLER             PIC X(4) VALUE ' OF '.                
           05  QUEUE-TOTAL        PIC Z(4).                             
       01  SAVE-AID               PIC X(1).                             
       01  SAVE-FREE              PIC X(1).                             
       01  HI-SEQ-NUMBER          PIC S9(4) COMP.                       
       01  LOW-SEQ-NUMBER         PIC S9(4) COMP.                       
       01  QUEUE-ITEM             PIC S9(4) COMP.                       
       01  QUEUE-NUMBER           PIC S9(4) COMP.                       
       01  QUEUE-LENGTH           PIC S9(4) COMP.                       
       01  ERROR-CODE             PIC S9(4) COMP.                       
       01  REPRINT-COUNT          PIC S9(4) COMP.                       
CSODJN 01  MAX-LGTH               PIC S9(4) COMP    VALUE +2862.        
       01  REC-LGTH               PIC S9(4) COMP.                       
       01  MICR-LGTH              PIC S9(4) COMP.                       
       01  INDX1                  PIC S9(4) COMP.                       
       01  INDX2                  PIC S9(4) COMP.                       
       01  INDX3                  PIC S9(4) COMP.                       
       01  RESP                   PIC S9(5) COMP.                       
       01  SVRESP                 PIC S9(5) COMP.                       
       01  MICR-ENQ               PIC X(12) VALUE 'MICR PRINTER'.       
           COPY MICM000.                                                
           COPY MICM100.                                                
           COPY MICM200.                                                
       01  MIC1-QUEUE-RECORD.                                           
           05  MIC1-MICR-QUE-NUMBER         PIC S9(4) COMP.             
           05  MIC1-MICR-ERROR-CODE         PIC S9(4) COMP.             
           05  MIC1-MICR-SEQ-NUMBER         PIC S9(4) COMP.             
       01  FILLER    PIC X(20)  VALUE '***** MICR QUEUE REC'.           
       01  MICR-QUEUE-RECORD.                                           
           05  MICR-FILE-FINAL-PRINT-COUNT  PIC S9(4) COMP.             
           05  MICR-FILE-RECORD.                                        
               10  MICR-FILE-KEY.                                       
                 15  MICR-FILE-KEY-FORM         PIC X(4).               
                 15  MICR-FILE-KEY-ORDER        PIC 9(5).               
CSODJN           15  MICR-FILE-KEY-DRAFT        PIC X(10).              
               10  MICR-FILE-PRINT-COUNT        PIC S9(4) COMP.         
               10  MICR-HI-SEQ-NUMBER REDEFINES                         
                   MICR-FILE-PRINT-COUNT        PIC S9(4) COMP.         
               10  MICR-FILE-SEQ-NUMBER         PIC S9(4) COMP.         
               10  MICR-AMOUNT-PAID             PIC S9(9)V9(2) COMP-3.  
DAN01          10  FILLER                       PIC X(2833).            
           05  MICR-FILE-RECORD-416 REDEFINES MICR-FILE-RECORD.         
           COPY MICR416.                                                
           05  MICR-FILE-RECORD-417 REDEFINES MICR-FILE-RECORD.         
           COPY MICR417.                                                
           05  MICR-FILE-RECORD-420C REDEFINES MICR-FILE-RECORD.        
           COPY MICR420C.                                               
ESP01      05  MICR-FILE-RECORD-420E REDEFINES MICR-FILE-RECORD.        
ESP01      COPY MICR420E.                                               
DAN01      05  MICR-FILE-RECORD-0031 REDEFINES MICR-FILE-RECORD.        
DAN01      COPY MICR0031.                                               
DAN02      05  MICR-FILE-RECORD-3CSI REDEFINES MICR-FILE-RECORD.        
DAN02      COPY MICR3CSI.                                               
DAN03      05  MICR-FILE-RECORD-750  REDEFINES MICR-FILE-RECORD.        
DAN03      COPY MICR750.                                                
DAN04      05  MICR-FILE-RECORD-CEN1 REDEFINES MICR-FILE-RECORD.        
DAN04      COPY MICRCEN1.                                               
DAN05      05  MICR-FILE-RECORD-CEN2 REDEFINES MICR-FILE-RECORD.        
DAN05      COPY MICRCEN2.                                               
       01  MIC1-ERR-MSGS.                                               
           05  FILLER                           PIC X(80)               
           VALUE 'INTERNAL ERROR - MIC1 COULD NOT PROCESS MICR QUEUE'.  
           05  FILLER                           PIC X(80)               
               VALUE 'INTERNAL ERROR - MIC1 DRAFT BUFFER OVERFLOW'.     
           05  FILLER                           PIC X(80)               
               VALUE 'INTERNAL ERROR - MIC1 ERROR WITH MICR PRINTER'.   
           05  FILLER                           PIC X(80)               
           VALUE 'INTERNAL ERROR - MIC1 COULD NOT UPDATE MICR QUEUE'.   
       01  MIC1-ERR-MESSAGES REDEFINES MIC1-ERR-MSGS.                   
           05  MIC1-ERR-MSG                   PIC X(80) OCCURS 4 TIMES. 
       01  DRS-COMM-AREA.                                               
           COPY DRSDRRBC.                                               
           COPY DRSDRIBX.                                               
           05  DRS-PRINT-LINE REDEFINES DRS-DRIBX-INIT-BLOCK            
                                   PIC X(133).                          
           COPY DFHAID.                                                 
      ***************************************************************** 
       LINKAGE SECTION.                                                 
       01  PARMLIST.                                                    
           05  FILLER                           PIC S9(8) COMP.         
           05  MIC1-ECB-ADDR                    PIC S9(8) COMP.         
       01  MIC1-ECB                             PIC S9(8) COMP.         
      ***************************************************************** 
       PROCEDURE DIVISION.                                              
           EXEC CICS HANDLE CONDITION ERROR(ERRORS) END-EXEC.           
           MOVE +12 TO REC-LGTH.                                        
           EXEC CICS ENQ RESOURCE(MICR-ENQ) LENGTH(REC-LGTH) RESP(RESP) 
              NOSUSPEND END-EXEC.                                       
           IF   RESP EQUAL DFHRESP(ENQBUSY) THEN                        
                STRING 'THE MICR PRINTER IS IN USE - SIMULTANEOUS '     
                   'OPERATIONS ARE NOT ALLOWED' DELIMITED BY SIZE       
                      INTO ERR-MSG                                      
                PERFORM P9000-ERRMSG THRU P9000-ERRMSG-EXIT             
                GO TO ENDTASK.                                          
           PERFORM P1000-CHECK-SECURITY THRU P1000-CHECK-SECURITY-EXIT. 
           IF   IS-ERROR THEN                                           
                MOVE 'YOU ARE NOT AUTHORIZED TO PERFORM MICR OPERATIONS'
                   TO ERR-MSG                                           
                PERFORM P9000-ERRMSG THRU P9000-ERRMSG-EXIT             
                GO TO ENDTASK.                                          
           PERFORM P1000-INITIALIZE THRU P1000-INITIALIZE-EXIT.         
           PERFORM P1000-MAINLINE THRU P1000-MAINLINE-EXIT              
               UNTIL MAIN-DONE.                                         
           MOVE SPACES TO ERR-MSG.                                      
           EXEC CICS SEND TEXT FROM(ERROR-MSG) LENGTH(1) ERASE          
                FREEKB END-EXEC.                                        
       ENDTASK.                                                         
           EXEC CICS RETURN END-EXEC.                                   
           GOBACK.                                                      
       ERRORS.                                                          
           EXEC CICS HANDLE CONDITION ERROR END-EXEC                    
           EXEC CICS PURGE MESSAGE END-EXEC                             
           EXEC CICS ABEND ABCODE('ERRS') END-EXEC.                     
      *                                                                 
       P1000-CHECK-SECURITY.                                            
           EXEC CICS ASSIGN OPID(OPER-ID) END-EXEC.                     
           IF   OPER-ID = 'IS6' THEN                                    
                NEXT SENTENCE                                           
           ELSE MOVE 'Y' TO ERROR-FLAG.                                 
       P1000-CHECK-SECURITY-EXIT. EXIT.                                 
      *                                                                 
       P1000-MAINLINE.                                                  
           MOVE 'Y' TO ERROR-FLAG.                                      
           PERFORM P1100-FIRST-SCREEN THRU P1100-FIRST-SCREEN-EXIT      
                UNTIL NO-ERROR OR MAIN-DONE.                            
           IF   IS-ERROR OR MAIN-DONE THEN                              
                GO TO P1000-MAINLINE-EXIT.                              
           PERFORM P1050-INITIALIZE THRU P1050-INITIALIZE-EXIT          
           IF   IS-ERROR THEN                                           
                GO TO P1000-MAINLINE-EXIT.                              
           MOVE 'N' TO ERROR-FLAG.                                      
           PERFORM P1150-SECOND-SCREEN THRU P1150-SECOND-SCREEN-EXIT    
                UNTIL SECOND-DONE.                                      
           IF   NO-ERROR THEN                                           
                MOVE SPACE TO SAVE-SEL001.                              
       P1000-MAINLINE-DONE.                                             
           EXEC CICS DELETEQ TS QUEUE(MICR-QUEUE) RESP(RESP) END-EXEC.  
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
       P1000-MAINLINE-EXIT. EXIT.                                       
      *                                                                 
       P1000-INITIALIZE.                                                
           MOVE 'N' TO MAIN-DONE-FLAG.                                  
           EXEC CICS IGNORE CONDITION QIDERR END-EXEC.                  
           EXEC CICS IGNORE CONDITION MAPFAIL END-EXEC.                 
           MOVE 'MICR' TO MICR-QUEUE-NAME.                              
           MOVE EIBTRMID TO MICR-QUEUE-TERM.                            
           MOVE 'MIC1' TO MIC1-QUEUE-NAME.                              
           MOVE EIBTRMID TO MIC1-QUEUE-TERM.                            
           EXEC CICS GETMAIN SET(MIC1-ECB-ADDR) LENGTH(4)               
              INITIMG(LOW-VAL) END-EXEC.                                
           MOVE MIC1-ECB-ADDR TO MIC1-ECB-ADDRESS.                      
           EXEC CICS DELETEQ TS QUEUE(MICR-QUEUE) RESP(RESP) END-EXEC.  
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
       P1000-INITIALIZE-EXIT.                                           
           EXIT.                                                        
      *                                                                 
       P1050-INITIALIZE.                                                
           MOVE 'N' TO ERROR-FLAG.                                      
           MOVE 'N' TO DONE-FLAG.                                       
           MOVE 'N' TO SECOND-DONE-FLAG.                                
           MOVE ' ' TO SAVE-SEL101.                                     
           MOVE 1 TO QUEUE-NUMBER.                                      
           MOVE ZERO TO TOT-DRAFTS.                                     
           MOVE ZERO TO UNPRTED-DRAFTS.                                 
           MOVE LOW-VALUES TO FORM-KEY.                                 
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * DETERMINE FORM SELECTED FROM FIRST MICR SCREEN.                 
ESP01 *-----------------------------------------------------------------
DAN05      IF   SAVE-SEL001 = '9' THEN                                  
                MOVE 'CEN2' TO SAVE-FORM.                               
DAN04      IF   SAVE-SEL001 = '8' THEN                                  
                MOVE 'CEN1' TO SAVE-FORM.                               
DAN03      IF   SAVE-SEL001 = '7' THEN                                  
                MOVE '750 ' TO SAVE-FORM.                               
DAN02      IF   SAVE-SEL001 = '6' THEN                                  
                MOVE '3CSI' TO SAVE-FORM.                               
DAN01      IF   SAVE-SEL001 = '5' THEN                                  
                MOVE '0031' TO SAVE-FORM.                               
           IF   SAVE-SEL001 = '4' THEN                                  
                MOVE '416' TO SAVE-FORM.                                
           IF   SAVE-SEL001 = '3' THEN                                  
                MOVE '417' TO SAVE-FORM.                                
           IF   SAVE-SEL001 = '2' THEN                                  
                MOVE '420C' TO SAVE-FORM.                               
ESP01      IF   SAVE-SEL001 = '1' THEN                                  
ESP01           MOVE '420E' TO SAVE-FORM.                               
           PERFORM P1075-BROWSE-MICRDRFT THRU                           
                   P1075-BROWSE-MICRDRFT-EXIT.                          
           IF   NO-ERROR THEN                                           
                IF   TOT-DRAFTS = ZERO THEN                             
                     MOVE 'Y' TO ERROR-FLAG                             
                     STRING 'NO DRAFTS WERE FOUND FOR FORM ' SAVE-FORM  
                          DELIMITED BY SIZE INTO ERR001.                
       P1050-INITIALIZE-EXIT.                                           
           EXIT.                                                        
      *                                                                 
       P1075-BROWSE-MICRDRFT.                                           
CSOMLF*DETERMINE IF THE RELEASE FROM CL177 OR EL177 HAS COMPLETED.      
CSOMLF     PERFORM CSO1-CHECK-FLAG THRU CSO1-EXIT.                      
CSOMLF     IF WS-CSO-MESSAGE = 'WAIT'                                   
CSOMLF          PERFORM P9000-ERRMSG THRU P9000-ERRMSG-EXIT             
CSOMLF          GO TO P1075-BROWSE-MICRDRFT-EXIT.                       
CSOMLF                                                                  
                                                                        
           MOVE FORM-KEY TO MICR-FILE-KEY.                              
           EXEC CICS STARTBR FILE('MICRDRFT') KEYLENGTH(4) GENERIC GTEQ 
                RESP(RESP) RIDFLD(MICR-FILE-KEY) NOHANDLE END-EXEC.     
           IF   RESP EQUAL DFHRESP(NOTFND) THEN                         
                GO TO P1075-BROWSE-MICRDRFT-EXIT.                       
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P1075-FILE-ERROR.                                 
           MOVE 'N' TO BROWSE-FLAG.                                     
           PERFORM P1080-READNEXT-MICRDRFT THRU                         
                   P1080-READNEXT-MICRDRFT-EXIT UNTIL BROWSE-IS-DONE.   
           EXEC CICS ENDBR FILE('MICRDRFT') END-EXEC.                   
           MOVE TOT-DRAFTS TO QUEUE-LENGTH.                             
           GO TO P1075-BROWSE-MICRDRFT-EXIT.                            
       P1075-FILE-ERROR.                                                
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'ERROR READING MICR DRAFT FILE' TO ERR001.              
       P1075-BROWSE-MICRDRFT-EXIT.                                      
           EXIT.                                                        
      *                                                                 
       P1080-READNEXT-MICRDRFT.                                         
DAN01      MOVE MAX-LGTH TO REC-LGTH.                                   
           EXEC CICS READNEXT FILE('MICRDRFT') LENGTH(REC-LGTH)         
                RIDFLD(MICR-FILE-KEY)                                   
                INTO(MICR-FILE-KEY) NOHANDLE                            
                RESP(RESP) END-EXEC.                                    
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P1080-READNEXT-DONE.                              
           IF   MICR-FILE-KEY-FORM NOT = SAVE-FORM THEN                 
                GO TO P1080-READNEXT-DONE.                              
           ADD +1 TO TOT-DRAFTS.                                        
           IF   MICR-FILE-PRINT-COUNT = ZERO THEN                       
                ADD +1 TO UNPRTED-DRAFTS.                               
           MOVE MICR-FILE-PRINT-COUNT TO MICR-FILE-FINAL-PRINT-COUNT.   
           ADD +2 TO REC-LGTH.                                          
           EXEC CICS WRITEQ TS QUEUE(MICR-QUEUE)                        
                FROM(MICR-QUEUE-RECORD)                                 
                LENGTH(REC-LGTH) END-EXEC.                              
           GO TO P1080-READNEXT-MICRDRFT-EXIT.                          
       P1080-READNEXT-DONE.                                             
           MOVE 'Y' TO BROWSE-FLAG.                                     
       P1080-READNEXT-MICRDRFT-EXIT.                                    
           EXIT.                                                        
      *                                                                 
       P1100-FIRST-SCREEN.                                              
           MOVE 'N' TO ERROR-FLAG.                                      
           EXEC CICS ASKTIME END-EXEC.                                  
           MOVE SAVE-SEL001 TO SEL001O OF MICM001O.                     
           MOVE ERR001 TO ERR001O OF MICM001O.                          
           MOVE -1 TO SEL001L OF MICM001I.                              
           MOVE SPACES TO ERR001.                                       
           EXEC CICS SEND MAP('MICM001') MAPSET('MICM000')              
                CURSOR RESP(RESP) ERASE WAIT END-EXEC.                  
           IF RESP NOT EQUAL DFHRESP(NORMAL) GO TO P1100-MAP-ERROR.     
           EXEC CICS RECEIVE MAP('MICM001') MAPSET('MICM000')           
                RESP(RESP) END-EXEC.                                    
           IF   EIBAID = DFHPF3                                         
                MOVE 'Y' TO MAIN-DONE-FLAG                              
                GO TO P1100-FIRST-SCREEN-EXIT.                          
           IF   SEL001L OF MICM001I IS LESS THAN 1 THEN                 
                MOVE SPACES TO SAVE-SEL001                              
           ELSE MOVE SEL001I OF MICM001I TO SAVE-SEL001.                
           IF   SAVE-SEL001 = SPACES THEN                               
                MOVE 'SELECTION IS REQUIRED INPUT' TO ERR001            
                MOVE 'Y' TO ERROR-FLAG                                  
                MOVE -1 TO SEL001L OF MICM001I                          
           ELSE IF  SAVE-SEL001 = '1' OR '2' OR '3'                     
DAN03                          OR '4' OR '5' OR '6' OR '7'              
DAN05                          OR '8' OR '9'                            
                    NEXT SENTENCE                                       
                ELSE                                                    
                     MOVE 'SELECTION MUST BE ONE OF SPECIFIED OPTIONS'  
                        TO ERR001                                       
                     MOVE 'Y' TO ERROR-FLAG                             
                     MOVE -1 TO SEL001L OF MICM001I.                    
           GO TO P1100-FIRST-SCREEN-EXIT.                               
       P1100-MAP-ERROR.                                                 
           MOVE 'Y' TO MAIN-DONE-FLAG.                                  
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'ERROR PRESENTING MAP MICM001 BY MICP000' TO ERR-MSG.   
           PERFORM P9000-ERRMSG THRU P9000-ERRMSG-EXIT.                 
       P1100-FIRST-SCREEN-EXIT.                                         
           EXIT.                                                        
      *                                                                 
       P1150-SECOND-SCREEN.                                             
           EXEC CICS ASKTIME END-EXEC.                                  
           MOVE SAVE-SEL101 TO SEL101O OF MICM101O.                     
           MOVE -1 TO SEL101L OF MICM101I.                              
           MOVE ERR101A TO ERR101AO OF MICM101O.                        
           MOVE ERR101B TO ERR101BO OF MICM101O.                        
           MOVE ERR101C TO ERR101CO OF MICM101O.                        
           MOVE SPACES TO ERR101A ERR101B ERR101C.                      
           MOVE TOT-DRAFTS TO EDIT-DRAFTS.                              
           MOVE EDIT-DRAFTS TO TDRFTSO OF MICM101O.                     
           MOVE UNPRTED-DRAFTS TO EDIT-DRAFTS.                          
           MOVE EDIT-DRAFTS TO UDRFTSO OF MICM101O.                     
           MOVE SAVE-FORM TO FORM101O OF MICM101O.                      
           EXEC CICS SEND MAP('MICM101') MAPSET('MICM100')              
                CURSOR RESP(RESP) ERASE WAIT END-EXEC.                  
           IF RESP NOT EQUAL DFHRESP(NORMAL) GO TO P1150-MAP-ERROR.     
           EXEC CICS RECEIVE MAP('MICM101') MAPSET('MICM100')           
                RESP(RESP) END-EXEC.                                    
           IF   EIBAID = DFHPF3                                         
                MOVE 'Y' TO SECOND-DONE-FLAG                            
                GO TO P1150-SECOND-SCREEN-EXIT.                         
           MOVE 'N' TO ERROR-FLAG.                                      
           IF   SEL101L OF MICM101I IS LESS THAN 1 THEN                 
                MOVE SPACES TO SAVE-SEL101                              
           ELSE MOVE SEL101I OF MICM101I TO SAVE-SEL101.                
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * VERIFY SELECTION OPTION ENTERED IN SECOND MICR SCREEN.          
ESP01 *-----------------------------------------------------------------
           IF   SAVE-SEL101 = SPACES THEN                               
                MOVE 'SELECTION IS REQUIRED INPUT' TO ERR101A           
                MOVE 'Y' TO ERROR-FLAG                                  
ESP01 *-----------------------------------------------------------------
ESP01 * VALID RESPONSES IN SECOND SCREEN: 'P' - PRINT; 'R' - REPRINT    
ESP01 *-----------------------------------------------------------------
           ELSE IF   SAVE-SEL101 = 'P' OR SAVE-SEL101 = 'R' THEN        
                     NEXT SENTENCE                                      
                ELSE                                                    
                     MOVE 'SELECTION MUST BE ONE OF SPECIFIED OPTIONS'  
                        TO ERR101A                                      
                     MOVE 'Y' TO ERROR-FLAG.                            
           IF   IS-ERROR THEN                                           
                GO TO P1150-SECOND-SCREEN-EXIT.                         
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * PRINT NEW CHECKS.                                               
ESP01 *-----------------------------------------------------------------
           IF   SAVE-SEL101 = 'P' THEN                                  
                IF   UNPRTED-DRAFTS = ZERO THEN                         
                     MOVE 'Y' TO ERROR-FLAG                             
                     MOVE 'NO UNPRINTED DRAFTS ARE AVAILABLE TO PRINT'  
                        TO ERR101A                                      
                ELSE PERFORM P2000-PRINT-NEW THRU P2000-PRINT-NEW-EXIT. 
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * REPRINT CHECKS.                                                 
ESP01 *-----------------------------------------------------------------
           IF   SAVE-SEL101 = 'R' THEN                                  
                IF   UNPRTED-DRAFTS > ZERO THEN                         
                     MOVE 'Y' TO ERROR-FLAG                             
                     STRING 'NO REPRINT ALLOWED UNTIL ALL UNPRINTED '   
                        'DRAFTS ARE PRINTED' DELIMITED BY SIZE          
                           INTO ERR101A                                 
                ELSE MOVE 'N' TO THIRD-DONE-FLAG                        
                     MOVE +1 TO QUEUE-NUMBER                            
                     MOVE ZERO TO REPRINT-COUNT                         
                     PERFORM P3000-REPRINT THRU P3000-REPRINT-EXIT      
                        UNTIL THIRD-DONE-FLAG = 'Y'                     
                     IF   REPRINT-COUNT > ZERO THEN                     
                          PERFORM P4000-PRINT-REPORT THRU               
                                  P4000-PRINT-REPORT-EXIT.              
           IF   NO-ERROR THEN                                           
                MOVE SPACES TO SAVE-SEL101                              
           ELSE IF   ERR101A = SPACES THEN                              
                     MOVE ERR001 TO ERR101A.                            
           GO TO P1150-SECOND-SCREEN-EXIT.                              
       P1150-MAP-ERROR.                                                 
           MOVE 'Y' TO MAIN-DONE-FLAG.                                  
           MOVE 'Y' TO SECOND-DONE-FLAG.                                
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'ERROR PRESENTING MAP MICM101 BY MICP000' TO ERR-MSG.   
           PERFORM P9000-ERRMSG THRU P9000-ERRMSG-EXIT.                 
       P1150-SECOND-SCREEN-EXIT.                                        
           EXIT.                                                        
      *                                                                 
       P2000-PRINT-NEW.                                                 
      *  GET NEXT SEQUENTIAL NUMBER.                                    
DAN01      MOVE MAX-LGTH TO MICR-LGTH.                                  
           MOVE LOW-VALUES TO MICR-FILE-KEY.                            
           MOVE ZERO TO LOW-SEQ-NUMBER HI-SEQ-NUMBER.                   
CSODJN*    CHANGED KEYLENGTH FROM 17 TO 19                              
           EXEC CICS READ FILE('MICRDRFT') INTO(MICR-FILE-RECORD)       
                KEYLENGTH(19) EQUAL                                     
                LENGTH(MICR-LGTH) RIDFLD(MICR-FILE-KEY) RESP(RESP)      
                END-EXEC.                                               
           IF   RESP EQUAL DFHRESP(NORMAL) THEN                         
                MOVE MICR-HI-SEQ-NUMBER TO HI-SEQ-NUMBER                
           ELSE MOVE ZERO TO MICR-HI-SEQ-NUMBER                         
                MOVE LOW-VALUES TO MICR-FILE-KEY                        
CSODJN          MOVE +21 TO MICR-LGTH                                   
                EXEC CICS WRITE FILE('MICRDRFT') FROM(MICR-FILE-RECORD) 
                     LENGTH(MICR-LGTH) RESP(RESP)                       
                     RIDFLD(MICR-FILE-KEY)                              
                     END-EXEC                                           
                IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                
                     GO TO P2000-MICR-ERROR.                            
           MOVE ZERO TO MIC1-COUNT MIC1-AMOUNT-PAID.                    
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
           PERFORM P2100-BUILD-QUEUE THRU P2100-BUILD-QUEUE-EXIT        
              VARYING QUEUE-NUMBER FROM 1 BY 1                          
                 UNTIL IS-ERROR OR QUEUE-NUMBER > TOT-DRAFTS.           
           IF   IS-ERROR THEN                                           
                GO TO P2000-PRINT-NEW-EXIT.                             
           IF   MIC1-COUNT = ZERO THEN                                  
                MOVE 'Y' TO ERROR-FLAG                                  
                MOVE 'INTERNAL ERROR - FOUND NO DRAFTS TO PRINT' TO     
                   ERR001                                               
                GO TO P2000-PRINT-NEW-EXIT.                             
           MOVE +20 TO REC-LGTH.                                        
           MOVE ZERO TO MIC1-ECB.                                       
           EXEC CICS START TRANSID('MIC1') TERMID('B001')               
              FROM(MIC1-COMMAREA) LENGTH(REC-LGTH) END-EXEC.            
           EXEC CICS WAIT EVENT ECADDR(MIC1-ECB-ADDRESS) END-EXEC.      
           PERFORM P4000-PRINT-REPORT THRU P4000-PRINT-REPORT-EXIT.     
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
DAN01      MOVE MAX-LGTH TO MICR-LGTH.                                  
           MOVE LOW-VALUES TO MICR-FILE-KEY.                            
CSODJN*    CHANGED KEYLENGTH FROM 17 TO 19                              
           EXEC CICS READ FILE('MICRDRFT') INTO(MICR-FILE-RECORD)       
                KEYLENGTH(19) EQUAL UPDATE                              
                LENGTH(MICR-LGTH) RIDFLD(MICR-FILE-KEY) RESP(RESP)      
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P2000-MICR-ERROR.                                 
           MOVE HI-SEQ-NUMBER TO MICR-HI-SEQ-NUMBER.                    
           EXEC CICS REWRITE FILE('MICRDRFT') FROM(MICR-FILE-RECORD)    
                LENGTH(MICR-LGTH) RESP(RESP)                            
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P2000-MICR-ERROR.                                 
           GO TO P2000-PRINT-NEW-EXIT.                                  
       P2000-MICR-ERROR.                                                
           MOVE 'Y' TO ERROR-FLAG                                       
           STRING 'INTERNAL ERROR WRITING MICRDRFT FILE - '             
              'CONTACT HELP DESK'                                       
                 DELIMITED BY SIZE INTO ERR001.                         
       P2000-PRINT-NEW-EXIT. EXIT.                                      
      *                                                                 
       P2100-BUILD-QUEUE.                                               
CSODJN     MOVE +2864 TO REC-LGTH.                                      
           EXEC CICS READQ TS QUEUE(MICR-QUEUE) INTO(MICR-QUEUE-RECORD) 
                LENGTH(REC-LGTH) ITEM(QUEUE-NUMBER) RESP(RESP)          
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P2100-ERROR.       
           IF   MICR-FILE-PRINT-COUNT NOT = ZERO THEN                   
                GO TO P2100-BUILD-QUEUE-EXIT.                           
           MOVE LOW-VALUES TO MIC1-QUEUE-RECORD.                        
           MOVE QUEUE-NUMBER TO MIC1-MICR-QUE-NUMBER.                   
           ADD +1 TO HI-SEQ-NUMBER.                                     
           MOVE HI-SEQ-NUMBER TO MIC1-MICR-SEQ-NUMBER.                  
           IF   LOW-SEQ-NUMBER = ZERO THEN                              
                MOVE HI-SEQ-NUMBER TO LOW-SEQ-NUMBER.                   
           MOVE +6 TO REC-LGTH.                                         
           EXEC CICS WRITEQ TS QUEUE(MIC1-QUEUE)                        
                FROM(MIC1-QUEUE-RECORD) RESP(RESP)                      
                LENGTH(REC-LGTH) END-EXEC.                              
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P2100-ERROR        
           ELSE ADD +1 TO MIC1-COUNT                                    
                ADD MICR-AMOUNT-PAID TO MIC1-AMOUNT-PAID.               
           GO TO P2100-BUILD-QUEUE-EXIT.                                
       P2100-ERROR.                                                     
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'INTERNAL ERROR HANDLING MICR QUEUE - CONTACT HELP DESK'
              TO ERR001.                                                
       P2100-BUILD-QUEUE-EXIT. EXIT.                                    
      *                                                                 
       P3000-REPRINT.                                                   
           EXEC CICS ASKTIME END-EXEC.                                  
           IF   NO-ERROR                                                
                PERFORM P3200-GET-QUEUE THRU P3200-GET-QUEUE-EXIT       
                IF   IS-ERROR                                           
                     MOVE 'Y' TO THIRD-DONE-FLAG                        
                     GO TO P3000-REPRINT-EXIT                           
                ELSE PERFORM P3300-BUILD-SCREEN THRU                    
                             P3300-BUILD-SCREEN-EXIT.                   
           EXEC CICS SEND MAP('MICM201') MAPSET('MICM200')              
                RESP(RESP) ERASE WAIT END-EXEC.                         
           IF RESP NOT EQUAL DFHRESP(NORMAL) GO TO P3000-MAP-ERROR.     
           EXEC CICS RECEIVE MAP('MICM201') MAPSET('MICM200')           
                RESP(RESP) END-EXEC.                                    
           IF RESP EQUAL DFHRESP(NORMAL) OR RESP EQUAL DFHRESP(EOC) OR  
              RESP EQUAL DFHRESP(MAPFAIL)                               
              NEXT SENTENCE                                             
           ELSE GO TO P3000-MAP-ERROR.                                  
           IF EIBAID = DFHPF3                                           
              MOVE 'Y' TO THIRD-DONE-FLAG                               
              GO TO P3000-REPRINT-EXIT.                                 
           MOVE 'N' TO ERROR-FLAG.                                      
           MOVE SPACES TO PRINT-FLAGS.                                  
           MOVE 'N' TO PRT-FLAG.                                        
           IF PR1L OF MICM201I GREATER THAN ZERO AND                    
              PR1I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(1).                                    
           IF PR2L OF MICM201I GREATER THAN ZERO AND                    
              PR2I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(2).                                    
           IF PR3L OF MICM201I GREATER THAN ZERO AND                    
              PR3I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(3).                                    
           IF PR4L OF MICM201I GREATER THAN ZERO AND                    
              PR4I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(4).                                    
           IF PR5L OF MICM201I GREATER THAN ZERO AND                    
              PR5I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(5).                                    
           IF PR6L OF MICM201I GREATER THAN ZERO AND                    
              PR6I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(6).                                    
           IF PR7L OF MICM201I GREATER THAN ZERO AND                    
              PR7I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(7).                                    
           IF PR8L OF MICM201I GREATER THAN ZERO AND                    
              PR8I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(8).                                    
           IF PR9L OF MICM201I GREATER THAN ZERO AND                    
              PR9I OF MICM201I GREATER THAN SPACES THEN                 
              MOVE 'X' TO PRT-IT(9).                                    
           IF PR10L OF MICM201I GREATER THAN ZERO AND                   
              PR10I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(10).                                   
           IF PR11L OF MICM201I GREATER THAN ZERO AND                   
              PR11I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(11).                                   
           IF PR12L OF MICM201I GREATER THAN ZERO AND                   
              PR12I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(12).                                   
           IF PR13L OF MICM201I GREATER THAN ZERO AND                   
              PR13I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(13).                                   
           IF PR14L OF MICM201I GREATER THAN ZERO AND                   
              PR14I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(14).                                   
           IF PR15L OF MICM201I GREATER THAN ZERO AND                   
              PR15I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(15).                                   
           IF PR16L OF MICM201I GREATER THAN ZERO AND                   
              PR16I OF MICM201I GREATER THAN SPACES THEN                
              MOVE 'X' TO PRT-IT(16).                                   
           MOVE EIBAID TO SAVE-AID.                                     
           PERFORM P3050-CHECK-PRINT THRU P3050-CHECK-PRINT-EXIT        
                VARYING INDX1 FROM 1 BY 1 UNTIL INDX1 > 16.             
           IF   IS-PRINT-TO-DO THEN                                     
                PERFORM P3100-REPRINT THRU P3100-REPRINT-EXIT           
                IF   IS-ERROR THEN                                      
                     GO TO P3000-REPRINT-EXIT.                          
           IF SAVE-AID = DFHPF7                                         
              IF QUEUE-NUMBER - 16 IS LESS THAN 1                       
                 MOVE 1 TO QUEUE-NUMBER                                 
                 GO TO P3000-REPRINT-EXIT                               
              ELSE SUBTRACT 16 FROM QUEUE-NUMBER                        
                 GO TO P3000-REPRINT-EXIT.                              
           IF SAVE-AID = DFHPF8                                         
              IF QUEUE-NUMBER + 16 IS GREATER THAN QUEUE-LENGTH         
                 MOVE QUEUE-LENGTH TO QUEUE-NUMBER                      
                 GO TO P3000-REPRINT-EXIT                               
              ELSE ADD 16 TO QUEUE-NUMBER                               
                 GO TO P3000-REPRINT-EXIT.                              
           GO TO P3000-REPRINT-EXIT.                                    
       P3000-MAP-ERROR.                                                 
           MOVE 'Y' TO THIRD-DONE-FLAG.                                 
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'ERROR PRESENTING MAP MICM201 BY MICP000' TO ERR001.    
       P3000-REPRINT-EXIT.                                              
           EXIT.                                                        
                                                                        
       P3050-CHECK-PRINT.                                               
           IF   PRT-IT(INDX1) EQUAL SPACES OR                           
                DRAFT-NO(INDX1) EQUAL SPACES THEN                       
                NEXT SENTENCE                                           
           ELSE MOVE 'Y' TO PRT-FLAG.                                   
       P3050-CHECK-PRINT-EXIT. EXIT.                                    
                                                                        
       P3200-GET-QUEUE.                                                 
           MOVE SPACES TO OUTPUT-LINES.                                 
           MOVE SPACES TO DRAFT-NOS.                                    
CSODJN     MOVE +2864 TO MICR-LGTH.                                     
           EXEC CICS READQ TS QUEUE(MICR-QUEUE) INTO(MICR-QUEUE-RECORD) 
                LENGTH(MICR-LGTH) ITEM(QUEUE-NUMBER) RESP(RESP)         
                END-EXEC.                                               
           IF RESP NOT EQUAL DFHRESP(NORMAL) GO TO P3200-ERROR.         
           MOVE 'N' TO TABLE-FLAG.                                      
           PERFORM P3400-BUILD-LINE THRU P3400-BUILD-LINE-EXIT          
              VARYING INDX1 FROM 1 BY 1                                 
                 UNTIL TABLE-FULL OR INDX1 > 16.                        
           GO TO P3200-GET-QUEUE-EXIT.                                  
       P3200-ERROR.                                                     
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'TROUBLE READING TEMP STORAGE QUEUE' TO ERR001.         
       P3200-GET-QUEUE-EXIT.                                            
           EXIT.                                                        
                                                                        
       P3300-BUILD-SCREEN.                                              
           MOVE LOW-VALUES TO MICM201O.                                 
           MOVE OUTPUT-LINE(1) TO LINE1O.                               
           MOVE OUTPUT-LINE(2) TO LINE2O.                               
           MOVE OUTPUT-LINE(3) TO LINE3O.                               
           MOVE OUTPUT-LINE(4) TO LINE4O.                               
           MOVE OUTPUT-LINE(5) TO LINE5O.                               
           MOVE OUTPUT-LINE(6) TO LINE6O.                               
           MOVE OUTPUT-LINE(7) TO LINE7O.                               
           MOVE OUTPUT-LINE(8) TO LINE8O.                               
           MOVE OUTPUT-LINE(9) TO LINE9O.                               
           MOVE OUTPUT-LINE(10) TO LINE10O.                             
           MOVE OUTPUT-LINE(11) TO LINE11O.                             
           MOVE OUTPUT-LINE(12) TO LINE12O.                             
           MOVE OUTPUT-LINE(13) TO LINE13O.                             
           MOVE OUTPUT-LINE(14) TO LINE14O.                             
           MOVE OUTPUT-LINE(15) TO LINE15O.                             
           MOVE OUTPUT-LINE(16) TO LINE16O.                             
           MOVE QUEUE-NUMBER TO QUEUE-LOW.                              
           IF QUEUE-NUMBER + 15 IS GREATER THAN QUEUE-LENGTH THEN       
              MOVE QUEUE-LENGTH TO QUEUE-HIGH                           
           ELSE MOVE QUEUE-NUMBER TO INDX1                              
                ADD 15 TO INDX1                                         
                MOVE INDX1 TO QUEUE-HIGH.                               
           MOVE QUEUE-LENGTH TO QUEUE-TOTAL.                            
           MOVE LINE-INFO TO CRP201O OF MICM201O.                       
           MOVE ERR201 TO ERR201O OF MICM201O.                          
           MOVE SAVE-FORM TO FORM201O OF MICM201O.                      
           MOVE SPACES TO ERR201.                                       
       P3300-BUILD-SCREEN-EXIT.                                         
           EXIT.                                                        
                                                                        
       P3400-BUILD-LINE.                                                
           MOVE SPACES TO REPORT-LINE.                                  
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE MICR-AMOUNT-PAID TO EDIT-AMOUNT2.                       
           MOVE MICR-FILE-FINAL-PRINT-COUNT TO EDIT-NUMBER.             
CSODJN** CHANGED SP5 TO SP3                                             
CSODJN     STRING SEQ-NUMBER SP3 MICR-FILE-KEY-DRAFT SP1 EDIT-AMOUNT2   
              SP5 EDIT-NUMBER DELIMITED BY SIZE INTO REPORT-LINE.       
           MOVE REPORT-LINE TO OUTPUT-LINE(INDX1).                      
           MOVE MICR-FILE-KEY-DRAFT TO DRAFT-NO(INDX1).                 
CSODJN     MOVE +2864 TO MICR-LGTH.                                     
           EXEC CICS READQ TS QUEUE(MICR-QUEUE) INTO(MICR-QUEUE-RECORD) 
                LENGTH(MICR-LGTH) NEXT RESP(RESP)                       
                END-EXEC.                                               
           IF RESP EQUAL DFHRESP(ITEMERR)                               
              MOVE 'Y' TO TABLE-FLAG.                                   
              GO TO P3400-BUILD-LINE-EXIT.                              
           IF RESP NOT EQUAL DFHRESP(NORMAL) GO TO P3400-ERROR.         
           GO TO P3400-BUILD-LINE-EXIT.                                 
       P3400-ERROR.                                                     
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'TROUBLE READING TEMP STORAGE QUEUE' TO ERR001.         
       P3400-BUILD-LINE-EXIT.                                           
           EXIT.                                                        
                                                                        
       P3100-REPRINT.                                                   
           MOVE ZERO TO MIC1-COUNT MIC1-AMOUNT-PAID.                    
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
           PERFORM P3100-BUILD-QUEUE THRU P3100-BUILD-QUEUE-EXIT        
                VARYING INDX1 FROM 1 BY 1                               
                UNTIL INDX1 > 16 OR IS-ERROR.                           
           IF   IS-ERROR THEN                                           
                GO TO P3100-REPRINT-EXIT.                               
           IF   MIC1-COUNT = ZERO THEN                                  
                MOVE 'Y' TO ERROR-FLAG                                  
                MOVE 'INTERNAL ERROR - FOUND NO DRAFTS TO PRINT' TO     
                   ERR001                                               
                GO TO P3100-REPRINT-EXIT.                               
           MOVE +20 TO REC-LGTH.                                        
           MOVE ZERO TO MIC1-ECB.                                       
           EXEC CICS START TRANSID('MIC1') TERMID('B001')               
              FROM(MIC1-COMMAREA) LENGTH(REC-LGTH) END-EXEC.            
           EXEC CICS WAIT EVENT ECADDR(MIC1-ECB-ADDRESS) END-EXEC.      
           PERFORM P3100-CHECK-ERRORS THRU P3100-CHECK-ERRORS-EXIT      
              VARYING INDX1 FROM 1 BY 1                                 
                 UNTIL INDX1 > MIC1-COUNT OR IS-ERROR.                  
           EXEC CICS DELETEQ TS QUEUE(MIC1-QUEUE) RESP(RESP) END-EXEC.  
       P3100-REPRINT-EXIT.  EXIT.                                       
                                                                        
       P3100-BUILD-QUEUE.                                               
           EXEC CICS ASKTIME END-EXEC.                                  
           IF   PRT-IT(INDX1) EQUAL SPACES THEN                         
                GO TO P3100-BUILD-QUEUE-EXIT.                           
           ADD INDX1 -1 QUEUE-NUMBER GIVING QUEUE-ITEM.                 
CSODJN     MOVE +2864 TO REC-LGTH.                                      
           EXEC CICS READQ TS QUEUE(MICR-QUEUE) INTO(MICR-QUEUE-RECORD) 
                LENGTH(REC-LGTH) ITEM(QUEUE-ITEM) RESP(RESP)            
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P3100-ERROR.       
           IF   MICR-FILE-PRINT-COUNT = ZERO THEN                       
                GO TO P3100-BUILD-QUEUE-EXIT.                           
           MOVE LOW-VALUES TO MIC1-QUEUE-RECORD.                        
           MOVE QUEUE-ITEM TO MIC1-MICR-QUE-NUMBER.                     
           MOVE MICR-FILE-SEQ-NUMBER TO MIC1-MICR-SEQ-NUMBER.           
           MOVE +6 TO REC-LGTH.                                         
           EXEC CICS WRITEQ TS QUEUE(MIC1-QUEUE)                        
                FROM(MIC1-QUEUE-RECORD) RESP(RESP)                      
                LENGTH(REC-LGTH) END-EXEC.                              
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P3100-ERROR        
           ELSE ADD +1 TO MIC1-COUNT                                    
                ADD +1 TO REPRINT-COUNT                                 
                ADD MICR-AMOUNT-PAID TO MIC1-AMOUNT-PAID.               
           GO TO P3100-BUILD-QUEUE-EXIT.                                
       P3100-ERROR.                                                     
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'INTERNAL ERROR HANDLING MICR QUEUE - CONTACT HELP DESK'
              TO ERR001.                                                
       P3100-BUILD-QUEUE-EXIT. EXIT.                                    
      *                                                                 
       P3100-CHECK-ERRORS.                                              
           MOVE +6 TO REC-LGTH.                                         
           EXEC CICS READQ TS QUEUE(MIC1-QUEUE) INTO(MIC1-QUEUE-RECORD) 
                LENGTH(REC-LGTH) ITEM(INDX1) RESP(RESP)                 
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                MOVE 'Y' TO ERROR-FLAG                                  
                GO TO P3100-CHECK-ERRORS-EXIT.                          
           IF   MIC1-MICR-ERROR-CODE > ZERO THEN                        
                MOVE 'Y' TO ERROR-FLAG                                  
                MOVE MIC1-MICR-ERROR-CODE TO ERROR-CODE                 
                PERFORM P9000-SHOW-CODE THRU P9000-SHOW-CODE-EXIT       
                MOVE ERR-MSG TO ERR001.                                 
       P3100-CHECK-ERRORS-EXIT. EXIT.                                   
      *                                                                 
       P4000-PRINT-REPORT.                                              
           EXEC CICS ASKTIME ABSTIME(PRINT-TIME) END-EXEC.              
           EXEC CICS FORMATTIME ABSTIME(PRINT-TIME) MMDDYY(TIME-MMDDYY) 
              TIME(TIME-CLOCK) DATESEP TIMESEP END-EXEC.                
                                                                        
ESP01 *---------------------------------------------------------------- 
ESP01 * DEFAULT PARAMETERS FOR DRAFT PRINT REPORT.                      
ESP01 *---------------------------------------------------------------- 
           MOVE 'ISD1' TO REPORT-PRINTER.                               
           MOVE 'A' TO REPORT-CLASS.                                    
           MOVE LOW-VALUES TO REPORT-FORM.                              
                                                                        
ESP01 *---------------------------------------------------------------- 
ESP01 * CALL ROUTINES TO PERFORM CICS OPENS FOR APPROPRIATE REPORT DD   
ESP01 * STATEMENTS.                                                     
ESP01 *---------------------------------------------------------------- 
           IF   SAVE-FORM = '416' THEN                                  
                MOVE 'BEN2' TO REPORT-PRINTER                           
                PERFORM P4100-OPEN-REPORT THRU P4100-OPEN-REPORT-EXIT   
           ELSE IF   SAVE-FORM = '417' THEN                             
                MOVE 'BEN2' TO REPORT-PRINTER                           
                PERFORM P4100-OPEN-REPORT THRU P4100-OPEN-REPORT-EXIT   
           ELSE IF   SAVE-FORM = '420C' THEN                            
                MOVE 'F420CA' TO COPYA                                  
                MOVE 'F420CB' TO COPYB                                  
                MOVE 'CID1OM' TO REPORT-PRINTER                         
                PERFORM P4100-OPEN-REPORT THRU P4100-OPEN-REPORT-EXIT   
                PERFORM P4100-OPEN-COPY THRU P4100-OPEN-COPY-EXIT       
ESP01      ELSE IF   SAVE-FORM = '420E' THEN                            
ESP01           MOVE 'F420EA' TO COPYA                                  
ESP01           MOVE 'F420EB' TO COPYB                                  
ESP01           PERFORM P4100-OPEN-REPORT-420E.                         
ESP01 *-----------------------------------------------------------------
ESP01 * COPIES OF DRAFTS ARE NOT CURRENTLY GENERATED FOR FORM 420E.     
ESP01 *-----------------------------------------------------------------
ESP01 *         PERFORM P4100-OPEN-COPY THRU P4100-OPEN-COPY-EXIT.      
                                                                        
           MOVE ZERO TO MICR-PRINT-AMOUNT MICR-PRINT-COUNT LINES-LEFT.  
           MOVE ERROR-FLAG TO SAVE-FLAG.                                
           MOVE 'N' TO ERROR-FLAG.                                      
                                                                        
ESP01 *---------------------------------------------------------------- 
ESP01 * PRINT CHECKS.                                                   
ESP01 *---------------------------------------------------------------- 
           IF   SAVE-SEL101 = 'P' THEN                                  
                PERFORM P4100-PROCESS-QUEUE THRU                        
                        P4100-PROCESS-QUEUE-EXIT                        
                   VARYING QUEUE-NUMBER FROM 1 BY 1                     
                      UNTIL IS-ERROR OR QUEUE-NUMBER > MIC1-COUNT       
ESP01 *---------------------------------------------------------------- 
ESP01 * REPRINT CHECKS.                                                 
ESP01 *---------------------------------------------------------------- 
           ELSE PERFORM P4150-PROCESS-QUEUE THRU                        
                        P4150-PROCESS-QUEUE-EXIT                        
                   VARYING MIC1-MICR-QUE-NUMBER FROM 1 BY 1             
                      UNTIL IS-ERROR OR                                 
                         MIC1-MICR-QUE-NUMBER > QUEUE-LENGTH.           
                                                                        
           MOVE MICR-PRINT-COUNT TO EDIT-NUMBER.                        
           MOVE MICR-PRINT-AMOUNT TO EDIT-AMOUNT.                       
           MOVE +2 TO LINE-SKIP.                                        
           STRING SP4 'TOTAL:' SP1 EDIT-NUMBER SP1 EDIT-AMOUNT          
              DELIMITED BY SIZE INTO REPORT-LINE.                       
ESP01      IF   SAVE-FORM = '416' OR '417' OR '420C' OR '420E'          
              PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.  
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * ERR101A MESSAGE                                                 
ESP01 *-----------------------------------------------------------------
           IF   NO-ERROR THEN                                           
                STRING 'SUCCESSFULLY PRINTED ' EDIT-NUMBER              
                   ' DRAFTS FOR '                                       
                   EDIT-AMOUNT DELIMITED BY SIZE INTO ERR101A.          
ESP01 *-----------------------------------------------------------------
ESP01 * ERR101B MESSAGE (FOR 'P'RINT OPTION ONLY).                      
ESP01 *-----------------------------------------------------------------
           IF   NO-ERROR AND SAVE-SEL101 = 'P' THEN                     
                MOVE LOW-SEQ-NUMBER TO EDIT-NUMBER                      
                MOVE +1 TO INDX1                                        
                STRING '   STARTING SEQUENCE NUMBER: ' EDIT-NUMBER      
                   DELIMITED BY SIZE INTO ERR101B WITH POINTER INDX1    
                MOVE HI-SEQ-NUMBER TO EDIT-NUMBER                       
                STRING '   ENDING SEQUENCE NUMBER: ' EDIT-NUMBER        
                   DELIMITED BY SIZE INTO ERR101B WITH POINTER INDX1.   
ESP01 *-----------------------------------------------------------------
ESP01 * ERR101C MESSAGE                                                 
ESP01 *-----------------------------------------------------------------
           IF   NO-ERROR AND                                            
                SAVE-FORM = '420C' THEN                                 
                MOVE '      CHECK SYSTEM PRINTER FOR DRAFT COPIES' TO   
                   ERR101C.                                             
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * CLOSE REPORTS.                                                  
ESP01 *-----------------------------------------------------------------
           IF   SAVE-FORM = '416' THEN                                  
                PERFORM P4100-CLOSE-REPORT THRU P4100-CLOSE-REPORT-EXIT 
           ELSE IF   SAVE-FORM = '417' THEN                             
                PERFORM P4100-CLOSE-REPORT THRU P4100-CLOSE-REPORT-EXIT 
           ELSE IF   SAVE-FORM = '420C' THEN                            
                PERFORM P4100-CLOSE-REPORT THRU P4100-CLOSE-REPORT-EXIT 
                PERFORM P4100-CLOSE-COPY THRU P4100-CLOSE-COPY-EXIT     
ESP01      ELSE IF   SAVE-FORM = '420E' THEN                            
ESP01           PERFORM P4100-CLOSE-REPORT THRU P4100-CLOSE-REPORT-EXIT 
ESP01 *-----------------------------------------------------------------
ESP01 * COPIES OF DRAFTS ARE NOT CURRENTLY GENERATED FOR FORM 420E.     
ESP01 *-----------------------------------------------------------------
ESP01 *         PERFORM P4100-CLOSE-COPY THRU P4100-CLOSE-COPY-EXIT.    
           MOVE SAVE-FLAG TO ERROR-FLAG.                                
                                                                        
       P4000-PRINT-REPORT-EXIT. EXIT.                                   
      *                                                                 
       P4100-OPEN-REPORT.                                               
      *-----------------------------------------------------------------
      * TWO COPIES OF THE DRAFT PRINT REPORT ARE PRODUCED.              
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE LOW-VALUES TO DRS-DRIBX-INIT-BLOCK.                     
           MOVE +2               TO DRS-DRIBX-NBR-ATTR-GRPS.            
           MOVE 'Y'              TO DRS-DRIBX-EXTENDED-ATTR.            
           MOVE 'Y'              TO DRS-DRIBX-DELETE-INCMPL.            
           MOVE 'F'              TO DRS-DRIBX-RFT.                      
           MOVE 'A'              TO DRS-DRIBX-RFC.                      
           MOVE +81              TO DRS-DRIBX-LRECL.                    
           MOVE +81              TO DRS-DRIBX-BLKSIZE.                  
                                                                        
      *-----------------------------------------------------------------
      * REPORT COPY # 1 - PROFS PRINTER.                                
      *-----------------------------------------------------------------
           MOVE REPORT-FORM      TO DRS-DRIBX-FORM(1).                  
           MOVE REPORT-CLASS     TO DRS-DRIBX-CLASS(1).                 
           MOVE 'N'              TO DRS-DRIBX-HOLD(1).                  
           MOVE +1               TO DRS-DRIBX-COPIES(1).                
           MOVE 'N3'             TO DRS-DRIBX-DEST(1).                  
           MOVE REPORT-PRINTER   TO DRS-DRIBX-USERID(1).                
                                                                        
      *-----------------------------------------------------------------
      * REPORT COPY # 2 - SYSTEM PRINTER: CLASS T, HOLD.                
      *-----------------------------------------------------------------
           MOVE 'T'              TO DRS-DRIBX-CLASS(2).                 
           MOVE 'Y'              TO DRS-DRIBX-HOLD(2).                  
           MOVE +1               TO DRS-DRIBX-COPIES(2).                
           PERFORM DRS-INIT-CALL THRU DRS-INIT-CALL-EXIT.               
           MOVE DRS-DRRB-REPORT-ID TO REPORT-NAME.                      
                                                                        
       P4100-OPEN-REPORT-EXIT. EXIT.                                    
                                                                        
ESP01  P4100-OPEN-REPORT-420E.                                          
ESP01 *-----------------------------------------------------------------
ESP01 * OPEN DRAFT PRINT REPORT FOR FORM 420E.                          
ESP01 *-----------------------------------------------------------------
ESP01      MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
ESP01      MOVE LOW-VALUES TO DRS-DRIBX-INIT-BLOCK.                     
ESP01      MOVE +1               TO DRS-DRIBX-NBR-ATTR-GRPS.            
ESP01      MOVE 'Y'              TO DRS-DRIBX-EXTENDED-ATTR.            
ESP01      MOVE 'Y'              TO DRS-DRIBX-DELETE-INCMPL.            
ESP01      MOVE 'F'              TO DRS-DRIBX-RFT.                      
ESP01      MOVE 'A'              TO DRS-DRIBX-RFC.                      
ESP01      MOVE +81              TO DRS-DRIBX-LRECL.                    
ESP01      MOVE +81              TO DRS-DRIBX-BLKSIZE.                  
ESP01                                                                   
ESP01 *-----------------------------------------------------------------
ESP01 * REPORT ROUTED TO SYSTEM PRINTER: CLASS A, NOHOLD.               
ESP01 *-----------------------------------------------------------------
ESP01      MOVE 'A'              TO DRS-DRIBX-CLASS(1).                 
ESP01      MOVE 'N'              TO DRS-DRIBX-HOLD(1).                  
ESP01      MOVE +1               TO DRS-DRIBX-COPIES(1).                
ESP01                                                                   
ESP01      PERFORM DRS-INIT-CALL THRU DRS-INIT-CALL-EXIT.               
ESP01      MOVE DRS-DRRB-REPORT-ID TO REPORT-NAME.                      
                                                                        
      *-----------------------------------------------------------------
      * TWO COPIES OF THE DRAFT COPIES ARE PRODUCED.                    
      *   REPORT(1) UTILIZES THE COPYA OUTPUT STATEMENT.                
      *   REPORT(2) UTILIZES THE COPYB OUTPUT STATEMENT.                
      *-----------------------------------------------------------------
       P4100-OPEN-COPY.                                                 
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE LOW-VALUES TO DRS-DRIBX-INIT-BLOCK.                     
           MOVE +2               TO DRS-DRIBX-NBR-ATTR-GRPS.            
           MOVE 'Y'              TO DRS-DRIBX-EXTENDED-ATTR.            
           MOVE 'Y'              TO DRS-DRIBX-DELETE-INCMPL.            
           MOVE 'F'              TO DRS-DRIBX-RFT.                      
           MOVE 'A'              TO DRS-DRIBX-RFC.                      
           MOVE +133             TO DRS-DRIBX-LRECL.                    
           MOVE +133             TO DRS-DRIBX-BLKSIZE.                  
ESP01                                                                   
ESP01 *-----------------------------------------------------------------
ESP01 * COPY 1 - UTILIZES COPYA OUTPUT STATEMENT.                       
ESP01 *-----------------------------------------------------------------
           MOVE 'BLNK'           TO DRS-DRIBX-FORM(1).                  
           MOVE 'C'              TO DRS-DRIBX-CLASS(1).                 
           MOVE 'N'              TO DRS-DRIBX-HOLD(1).                  
           MOVE +1               TO DRS-DRIBX-COPIES(1).                
           MOVE COPYA            TO DRS-DRIBX-OUTPUT-STMT-NAME(1).      
ESP01                                                                   
ESP01 *-----------------------------------------------------------------
ESP01 * COPY 2 - UTILIZES COPYB OUTPUT STATEMENT.                       
ESP01 *-----------------------------------------------------------------
           MOVE 'BLNK'           TO DRS-DRIBX-FORM(2).                  
CSO884     IF   COPYB  EQUAL  'F420CB'                                  
CSO884         MOVE 'Z'          TO DRS-DRIBX-CLASS(2)                  
CSO884      ELSE                                                        
CSO884         MOVE 'C'          TO DRS-DRIBX-CLASS(2).                 
CSO884*    MOVE 'C'          TO DRS-DRIBX-CLASS(2).                     
           MOVE 'N'              TO DRS-DRIBX-HOLD(2).                  
           MOVE +1               TO DRS-DRIBX-COPIES(2).                
           MOVE COPYB            TO DRS-DRIBX-OUTPUT-STMT-NAME(2).      
           PERFORM DRS-INIT-CALL THRU DRS-INIT-CALL-EXIT.               
           MOVE DRS-DRRB-REPORT-ID TO COPY-NAME.                        
       P4100-OPEN-COPY-EXIT. EXIT.                                      
      *                                                                 
ESP01                                                                   
ESP01 *-----------------------------------------------------------------
ESP01 * CLOSE DRAFT PRINT REPORT.                                       
ESP01 *-----------------------------------------------------------------
       P4100-CLOSE-REPORT.                                              
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE REPORT-NAME TO DRS-DRRB-REPORT-ID.                      
           PERFORM DRS-TERM-CALL THRU DRS-TERM-CALL-EXIT.               
       P4100-CLOSE-REPORT-EXIT. EXIT.                                   
      *                                                                 
ESP01                                                                   
ESP01 *-----------------------------------------------------------------
ESP01 * CLOSE DRAFT COPIES REPORT.                                      
ESP01 *-----------------------------------------------------------------
       P4100-CLOSE-COPY.                                                
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE COPY-NAME TO DRS-DRRB-REPORT-ID.                        
           PERFORM DRS-TERM-CALL THRU DRS-TERM-CALL-EXIT.               
       P4100-CLOSE-COPY-EXIT. EXIT.                                     
      *                                                                 
       P4100-PROCESS-QUEUE.                                             
           MOVE +6 TO REC-LGTH.                                         
           EXEC CICS READQ TS QUEUE(MIC1-QUEUE) INTO(MIC1-QUEUE-RECORD) 
                LENGTH(REC-LGTH) ITEM(QUEUE-NUMBER) RESP(RESP)          
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P4100-MIC1-ERROR.  
           IF   MIC1-MICR-ERROR-CODE NOT EQUAL ZERO THEN                
                MOVE 'Y' TO ERROR-FLAG                                  
                MOVE MIC1-MICR-ERROR-CODE TO ERROR-CODE                 
                PERFORM P9000-SHOW-CODE THRU P9000-SHOW-CODE-EXIT       
                MOVE ERR-MSG TO ERR001                                  
                GO TO P4100-PROCESS-QUEUE-EXIT.                         
           PERFORM P4150-PROCESS-QUEUE THRU P4150-PROCESS-QUEUE-EXIT.   
           GO TO P4100-PROCESS-QUEUE-EXIT.                              
       P4100-MIC1-ERROR.                                                
           MOVE 'Y' TO ERROR-FLAG.                                      
           MOVE 'INTERNAL ERROR HANDLING MIC1 QUEUE - CONTACT HELP DESK'
              TO ERR001.                                                
           GO TO P4100-PROCESS-QUEUE-EXIT.                              
       P4100-PROCESS-QUEUE-EXIT. EXIT.                                  
      *                                                                 
       P4150-PROCESS-QUEUE.                                             
CSODJN     MOVE +2864 TO MICR-LGTH.                                     
           EXEC CICS READQ TS QUEUE(MICR-QUEUE) INTO(MICR-QUEUE-RECORD) 
                LENGTH(MICR-LGTH) ITEM(MIC1-MICR-QUE-NUMBER) RESP(RESP) 
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) GO TO P4150-MICR-ERROR.  
           IF   MICR-FILE-FINAL-PRINT-COUNT = MICR-FILE-PRINT-COUNT THEN
                GO TO P4150-PROCESS-QUEUE-EXIT.                         
           IF   SAVE-SEL101 = 'P' THEN                                  
                MOVE MIC1-MICR-SEQ-NUMBER TO MICR-FILE-SEQ-NUMBER.      
           IF   SAVE-FORM = '416' THEN                                  
                PERFORM P4200-PRINT-416 THRU P4200-PRINT-416-EXIT       
           ELSE IF   SAVE-FORM = '417' THEN                             
                PERFORM P4200-PRINT-417 THRU P4200-PRINT-417-EXIT       
           ELSE IF   SAVE-FORM = '420C' THEN                            
                PERFORM P4200-PRINT-420C THRU P4200-PRINT-420C-EXIT     
ESP01      ELSE IF   SAVE-FORM = '420E' THEN                            
ESP01           PERFORM P4200-PRINT-420E THRU P4200-PRINT-420E-EXIT     
           ELSE IF   SAVE-FORM = '0031' THEN                            
DAN01           PERFORM P4200-PRINT-0031 THRU P4200-PRINT-0031-EXIT     
           ELSE IF   SAVE-FORM = '3CSI' THEN                            
DAN02           PERFORM P4200-PRINT-3CSI THRU P4200-PRINT-3CSI-EXIT     
           ELSE IF   SAVE-FORM = '750 ' THEN                            
DAN03           PERFORM P4200-PRINT-750  THRU P4200-PRINT-750-EXIT      
           ELSE IF   SAVE-FORM = 'CEN1' THEN                            
DAN04           PERFORM P4200-PRINT-CEN1 THRU P4200-PRINT-CEN1-EXIT     
           ELSE IF   SAVE-FORM = 'CEN2' THEN                            
DAN05           PERFORM P4200-PRINT-CEN2 THRU P4200-PRINT-CEN2-EXIT.    
                                                                        
           MOVE MICR-FILE-FINAL-PRINT-COUNT TO MICR-FILE-PRINT-COUNT.   
           ADD +1 TO MICR-PRINT-COUNT.                                  
           ADD MICR-AMOUNT-PAID TO MICR-PRINT-AMOUNT.                   
           IF   SAVE-SEL101 = 'P' THEN                                  
                SUBTRACT 1 FROM UNPRTED-DRAFTS.                         
      * UPDATE THE MICR TS QUEUE.                                       
           EXEC CICS WRITEQ TS QUEUE(MICR-QUEUE)                        
                ITEM(MIC1-MICR-QUE-NUMBER)                              
                FROM(MICR-QUEUE-RECORD) RESP(RESP) REWRITE              
                LENGTH(MICR-LGTH) END-EXEC.                             
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P4150-MICR-ERROR.                                 
      * UPDATE THE VSAM FILE NOW.                                       
           MOVE MAX-LGTH TO MICR-LGTH.                                  
CSODJN* CHANGED KEYLENGTH FROM 17 TO 19.                                
           EXEC CICS READ FILE('MICRDRFT') INTO(MICR-FILE-RECORD)       
                KEYLENGTH(19) UPDATE EQUAL                              
                LENGTH(MICR-LGTH) RIDFLD(MICR-FILE-KEY) RESP(RESP)      
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P4150-VSAM-ERROR.                                 
           MOVE MICR-FILE-FINAL-PRINT-COUNT TO MICR-FILE-PRINT-COUNT.   
           IF   SAVE-SEL101 = 'P' THEN                                  
                MOVE MIC1-MICR-SEQ-NUMBER TO MICR-FILE-SEQ-NUMBER.      
           EXEC CICS REWRITE FILE('MICRDRFT') FROM(MICR-FILE-RECORD)    
                LENGTH(MICR-LGTH) RESP(RESP)                            
                END-EXEC.                                               
           IF   RESP NOT EQUAL DFHRESP(NORMAL) THEN                     
                GO TO P4150-VSAM-ERROR.                                 
           GO TO P4150-PROCESS-QUEUE-EXIT.                              
       P4150-VSAM-ERROR.                                                
           MOVE 'Y' TO ERROR-FLAG.                                      
           STRING 'INTERNAL ERROR HANDLING MICRDRFT FILE '              
              '- CONTACT HELP DESK' DELIMITED BY SIZE INTO ERR001.      
           GO TO P4150-PROCESS-QUEUE-EXIT.                              
       P4150-MICR-ERROR.                                                
           MOVE 'Y' TO ERROR-FLAG.                                      
           STRING 'INTERNAL ERROR PRINTING MICR DRAFT '                 
              MICR-FILE-KEY-DRAFT                                       
              ' - CONTACT HELP DESK' DELIMITED BY SIZE INTO ERR001.     
           GO TO P4150-PROCESS-QUEUE-EXIT.                              
       P4150-PROCESS-QUEUE-EXIT. EXIT.                                  
      *                                                                 
       P4200-PRINT-416.                                                 
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M416-AMOUNT-PAID TO EDIT-AMOUNT EDIT-CHECK.             
           MOVE +1 TO LINE-SKIP.                                        
           STRING SP2 SEQ-NUMBER SP1 M416-DRAFT SP1 EDIT-AMOUNT SP1     
CSOY2K        M416-CLAIM-NO SP1 M416-POLICY-NO SP1 M416-PAYEE-NAME      
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
       P4200-PRINT-416-EXIT. EXIT.                                      
      *                                                                 
       P4200-PRINT-417.                                                 
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M417-AMOUNT-PAID TO EDIT-AMOUNT EDIT-CHECK.             
           MOVE +1 TO LINE-SKIP.                                        
           STRING SP2 SEQ-NUMBER SP1 M417-DRAFT SP1 EDIT-AMOUNT SP1     
CSOY2K        M417-CLAIM-NO SP1 M417-POLICY-NO SP1 M417-PAYEE-NAME      
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
       P4200-PRINT-417-EXIT. EXIT.                                      
      *                                                                 
       P4200-PRINT-420C.                                                
DAN07      GO TO P4200-PRINT-420C-REPORT.                               
           MOVE SPACES TO ADDRESS-ONE.                                  
           MOVE M420C-MEMBER-ADDRESS1 TO ADDRESS-ONE-1.                 
           MOVE M420C-MEMBER-ADDRESS2 TO ADDRESS-ONE-2.                 
           MOVE M420C-MEMBER-ADDRESS3 TO ADDRESS-ONE-3.                 
           MOVE M420C-MEMBER-ADDRESS4 TO ADDRESS-ONE-4.                 
           MOVE M420C-MEMBER-ZIP-CODE TO ADDRESS-ONE-ZIP.               
           MOVE SPACES TO ADDRESS-TWO.                                  
           MOVE M420C-PAYEE-ADDRESS1 TO ADDRESS-TWO-1.                  
           MOVE M420C-PAYEE-ADDRESS2 TO ADDRESS-TWO-2.                  
           MOVE M420C-PAYEE-ADDRESS3 TO ADDRESS-TWO-3.                  
           MOVE M420C-PAYEE-ADDRESS4 TO ADDRESS-TWO-4.                  
           MOVE M420C-PAYEE-ZIP-CODE TO ADDRESS-TWO-ZIP.                
           MOVE SPACES TO ADDRESS-THREE.                                
           MOVE M420C-3RDADD-LINE1 TO ADDRESS-THREE-1.                  
           MOVE M420C-3RDADD-LINE2 TO ADDRESS-THREE-2.                  
           MOVE M420C-3RDADD-LINE3 TO ADDRESS-THREE-3.                  
           MOVE M420C-3RDADD-ZIP TO ADDRESS-THREE-ZIP.                  
           MOVE ADDRESS-ONE TO ADDRESS-WORK.                            
           PERFORM P5000-ADJUST-ADDRESS THRU P5000-ADJUST-ADDRESS-EXIT. 
           MOVE ADDRESS-WORK TO ADDRESS-ONE.                            
           MOVE ADDRESS-TWO TO ADDRESS-WORK.                            
           PERFORM P5000-ADJUST-ADDRESS THRU P5000-ADJUST-ADDRESS-EXIT. 
           MOVE ADDRESS-WORK TO ADDRESS-TWO.                            
           MOVE ADDRESS-THREE TO ADDRESS-WORK.                          
           PERFORM P5000-ADJUST-ADDRESS THRU P5000-ADJUST-ADDRESS-EXIT. 
           MOVE ADDRESS-WORK TO ADDRESS-THREE.                          
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M420C-AMOUNT-PAID TO EDIT-AMOUNT EDIT-CHECK.            
           MOVE SPACES TO PRINT-LINE.                                   
CSODJN* CHANGE SP7 TO SP5                                               
CSODJN     STRING '1' SP1 SEQ-NUMBER SP6 M420C-COMPANY-NAME SP5         
              M420C-DRAFT                                               
                 DELIMITED BY SIZE INTO PRINT-LINE.                     
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP18 M420C-CSO-ADDRESS                                
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP2 M420C-CLAIM-NO SP2 M420C-CERT-NO SP3              
              M420C-PLAN-CODE SP3 M420C-PAID-FROM-DATE SP2              
              M420C-PAID-THRU-DATE SP1 EDIT-AMOUNT SP2                  
              M420C-PAYMENT-TYPE SP1 M420C-ACCT-NO                      
                 DELIMITED BY SIZE INTO PRINT-LINE.                     
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 M420C-CC-ACCT                                     
              M420C-CC-ACCT-NUMBER SP21 M420C-TYPE-MESSAGE              
                 DELIMITED BY SIZE INTO PRINT-LINE.                     
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06***   STRING SP51 M420C-FINAL-MESS9                                
DAN06***      DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06***   STRING SP51 M420C-FINAL-MESS10                               
DAN06***      DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP21 M420C-DFT-NOTES1                                 
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP21 M420C-DFT-NOTES2                                 
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
                                                                        
           STRING SP5 M420C-MEMBER-NAME SP16 M420C-FINAL-MESS11         
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-ONE-1                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-ONE-2                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-ONE-3                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-ONE-4                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
                                                                        
DAN06      STRING '2' SP4 M420C-DRAFT-MESSAGE(1)                        
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06      STRING SP5 M420C-DRAFT-MESSAGE(2)                            
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06      STRING SP5 M420C-DRAFT-MESSAGE(3)                            
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06      STRING SP5 M420C-DRAFT-MESSAGE(4)                            
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06      STRING SP5 M420C-DRAFT-MESSAGE(5)                            
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
                                                                        
           STRING '3' SP4 M420C-3RDADD-NAME                             
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-THREE-1                                   
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-THREE-2                                   
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-THREE-3                                   
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06***   STRING '3' SP36 M420C-REPLY-DATE                             
DAN06***      DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06***   PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING '4' SP4 M420C-PAYEE-NAME                              
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-1                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-2                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-3                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-4                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
                                                                        
           STRING '5' M420C-COMPANY-NAME SP26 M420C-DRAFT               
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 M420C-CHECK-DATE SP7 M420C-CLAIM-NO SP2           
              M420C-CC-ACCT M420C-CC-ACCT-NUMBER                        
                 DELIMITED BY SIZE INTO PRINT-LINE.                     
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP20 'INSURED:' SP1 M420C-MEMBER-NAME                 
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN06      STRING SP20 M420C-LOAN-NUMBER                                
DAN06         DELIMITED BY SIZE INTO PRINT-LINE.                        
DAN06      PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 M420C-PAYEE-NAME SP33 EDIT-CHECK                  
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-1                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-2                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-3                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           STRING SP5 ADDRESS-TWO-4                                     
              DELIMITED BY SIZE INTO PRINT-LINE.                        
           PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
DAN07  P4200-PRINT-420C-REPORT.                                         
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH M420C-DRAFT EXPANDED              
           STRING SP2 SEQ-NUMBER SP1 M420C-DRAFT SP1 EDIT-AMOUNT SP1    
              M420C-CLAIM-NO SP2 M420C-CERT-NO SP2 M420C-PAYEE-NAME     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
       P4200-PRINT-420C-EXIT. EXIT.                                     
                                                                        
ESP01 *-----------------------------------------------------------------
ESP01 * THIS PARAGRAPH PRINTS REPORTS FOR FORM 420E.  COMMENTED LINES   
ESP01 * APPLY TO DRAFT COPIES - NOT CURRENTLY NEEDED.                   
ESP01 *-----------------------------------------------------------------
       P4200-PRINT-420E.                                                
      *    MOVE SPACES TO ADDRESS-ONE.                                  
      *    MOVE M420E-MEMBER-ADDRESS1 TO ADDRESS-ONE-1.                 
      *    MOVE M420E-MEMBER-ADDRESS2 TO ADDRESS-ONE-2.                 
      *    MOVE M420E-MEMBER-ADDRESS3 TO ADDRESS-ONE-3.                 
      *    MOVE M420E-MEMBER-ADDRESS4 TO ADDRESS-ONE-4.                 
      *    MOVE M420E-MEMBER-ZIP-CODE TO ADDRESS-ONE-ZIP.               
      *    MOVE SPACES TO ADDRESS-TWO.                                  
      *    MOVE M420E-PAYEE-ADDRESS1 TO ADDRESS-TWO-1.                  
      *    MOVE M420E-PAYEE-ADDRESS2 TO ADDRESS-TWO-2.                  
      *    MOVE M420E-PAYEE-ADDRESS3 TO ADDRESS-TWO-3.                  
      *    MOVE M420E-PAYEE-ADDRESS4 TO ADDRESS-TWO-4.                  
      *    MOVE M420E-PAYEE-ZIP-CODE TO ADDRESS-TWO-ZIP.                
      *    MOVE ADDRESS-ONE TO ADDRESS-WORK.                            
      *    PERFORM P5000-ADJUST-ADDRESS THRU P5000-ADJUST-ADDRESS-EXIT. 
      *    MOVE ADDRESS-WORK TO ADDRESS-ONE.                            
      *    MOVE ADDRESS-TWO TO ADDRESS-WORK.                            
      *    PERFORM P5000-ADJUST-ADDRESS THRU P5000-ADJUST-ADDRESS-EXIT. 
      *    MOVE ADDRESS-WORK TO ADDRESS-TWO.                            
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M420E-AMOUNT-PAID TO EDIT-AMOUNT EDIT-CHECK.            
           MOVE SPACES TO PRINT-LINE.                                   
      *    STRING '1' SP1 SEQ-NUMBER SP6 M420E-COMPANY-NAME SP7         
      *       M420E-DRAFT                                               
      *          DELIMITED BY SIZE INTO PRINT-LINE.                     
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP18 M420E-CSO-ADDRESS                                
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP2 M420E-CLAIM-NO SP4 M420E-CERT-NO SP4              
      *       M420E-PLAN-CODE SP3 M420E-PAID-FROM-DATE SP2              
      *       M420E-PAID-THRU-DATE SP1 EDIT-AMOUNT                      
      *          DELIMITED BY SIZE INTO PRINT-LINE.                     
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-CC-ACCT                                     
      *       M420E-CC-ACCT-NUMBER SP21 M420E-TYPE-MESSAGE              
      *          DELIMITED BY SIZE INTO PRINT-LINE.                     
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP51 M420E-FINAL-MESS9                                
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP51 M420E-FINAL-MESS10                               
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-MEMBER-NAME SP16 M420E-FINAL-MESS11         
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-ONE-1                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-ONE-2                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-ONE-3                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-ONE-4                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING '2' SP4 M420E-3RDADD-LINE1                            
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-3RDADD-LINE2                                
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-3RDADD-LINE3                                
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING '3' SP36 M420E-REPLY-DATE                             
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING '4' SP4 M420E-PAYEE-NAME                              
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-1                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-2                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-3                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-4                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING '5' M420E-COMPANY-NAME SP26 M420E-DRAFT               
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-CHECK-DATE SP7 M420E-CLAIM-NO SP2           
      *       M420E-CC-ACCT M420E-CC-ACCT-NUMBER                        
      *          DELIMITED BY SIZE INTO PRINT-LINE.                     
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP20 'INSURED:' SP1 M420E-MEMBER-NAME                 
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 M420E-PAYEE-NAME SP33 EDIT-CHECK                  
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-1                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-2                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-3                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
      *    STRING SP5 ADDRESS-TWO-4                                     
      *       DELIMITED BY SIZE INTO PRINT-LINE.                        
      *    PERFORM PRINT-DRAFT-COPY THRU PRINT-DRAFT-COPY-EXIT.         
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH M420E-DRAFT EXPANDED              
           STRING SP2 SEQ-NUMBER SP1 M420E-DRAFT SP1 EDIT-AMOUNT SP1    
              M420E-CLAIM-NO SP2 M420E-CERT-NO SP5 M420E-PAYEE-NAME     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
       P4200-PRINT-420E-EXIT.  EXIT.                                    
      *                                                                 
DAN01  P4200-PRINT-0031.                                                
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M0031-AMOUNT-PAID TO EDIT-AMOUNT2 EDIT-CHECK2.          
           MOVE +1 TO LINE-SKIP.                                        
           STRING SP2 SEQ-NUMBER SP1 M0031-DRAFT-NO SP1 EDIT-AMOUNT2    
              SP23 M0031-PAYEE-NAME                                     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
DAN01  P4200-PRINT-0031-EXIT. EXIT.                                     
      *                                                                 
DAN02  P4200-PRINT-3CSI.                                                
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M3CSI-AMOUNT-PAID TO EDIT-AMOUNT2 EDIT-CHECK2.          
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH M3CSI-DRAFT-NO EXPANDED           
           STRING SP2 SEQ-NUMBER SP1 M3CSI-DRAFT-NO SP1 EDIT-AMOUNT2    
              SP20 M3CSI-PAYEE-NAME                                     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
DAN02  P4200-PRINT-3CSI-EXIT. EXIT.                                     
      *                                                                 
      *                                                                 
DAN03  P4200-PRINT-750.                                                 
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE M750-AMOUNT-PAID TO EDIT-AMOUNT2 EDIT-CHECK2.           
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH M750-DRAFT-NO EXPANDED            
           STRING SP2 SEQ-NUMBER SP1 M750-DRAFT-NO SP1 EDIT-AMOUNT2     
              SP20 M750-AGENT-NAME                                      
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
DAN03  P4200-PRINT-750-EXIT. EXIT.                                      
      *                                                                 
DAN04  P4200-PRINT-CEN1.                                                
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE MCEN1-AMOUNT-PAID TO EDIT-AMOUNT2 EDIT-CHECK2.          
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH MCEN1-DRAFT-NO EXPANDED           
           STRING SP2 SEQ-NUMBER SP1 MCEN1-DRAFT-NO SP1 EDIT-AMOUNT2    
              SP23 MCEN1-PAYEE-NAME                                     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
DAN04  P4200-PRINT-CEN1-EXIT. EXIT.                                     
      *                                                                 
DAN05  P4200-PRINT-CEN2.                                                
           MOVE MICR-FILE-SEQ-NUMBER TO SEQ-NUMBER.                     
           MOVE MCEN2-AMOUNT-PAID TO EDIT-AMOUNT2 EDIT-CHECK2.          
           MOVE +1 TO LINE-SKIP.                                        
CSODJN* NO CHANGE IN SPACING ALTHOUGH MCEN2-DRAFT-NO EXPANDED           
           STRING SP2 SEQ-NUMBER SP1 MCEN2-DRAFT-NO SP1 EDIT-AMOUNT2    
              SP23 MCEN2-PAYEE-NAME                                     
                 DELIMITED BY SIZE INTO REPORT-LINE.                    
           PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.     
           IF   SAVE-SEL101 = 'R' AND MICR-FILE-FINAL-PRINT-COUNT -     
                MICR-FILE-PRINT-COUNT > 1 THEN                          
                SUBTRACT MICR-FILE-PRINT-COUNT FROM                     
                   MICR-FILE-FINAL-PRINT-COUNT GIVING SEQ-NUMBER        
                STRING SP4 '*** WARNING *** DRAFT WAS REPRINTED '       
                   SEQ-NUMBER ' TIMES' DELIMITED BY SIZE INTO           
                      REPORT-LINE                                       
                PERFORM PRINT-DRAFT-REPORT THRU PRINT-DRAFT-REPORT-EXIT.
DAN04  P4200-PRINT-CEN2-EXIT. EXIT.                                     
      *                                                                 
       P5000-ADJUST-ADDRESS.                                            
           IF   ADDRESS-WORK-LINE(3) = SPACES THEN                      
                MOVE ADDRESS-WORK-LINE(4) TO ADDRESS-WORK-LINE(3)       
                MOVE SPACES TO ADDRESS-WORK-LINE(4).                    
           IF   ADDRESS-WORK-LINE(2) = SPACES THEN                      
                MOVE ADDRESS-WORK-LINE(3) TO ADDRESS-WORK-LINE(2)       
                MOVE ADDRESS-WORK-LINE(4) TO ADDRESS-WORK-LINE(3)       
                MOVE SPACES TO ADDRESS-WORK-LINE(4).                    
           IF   ADDRESS-WORK-LINE(1) = SPACES THEN                      
                MOVE ADDRESS-WORK-LINE(2) TO ADDRESS-WORK-LINE(1)       
                MOVE ADDRESS-WORK-LINE(3) TO ADDRESS-WORK-LINE(2)       
                MOVE ADDRESS-WORK-LINE(4) TO ADDRESS-WORK-LINE(3)       
                MOVE SPACES TO ADDRESS-WORK-LINE(4).                    
           MOVE +1 TO INDX1.                                            
           PERFORM P5070-LAST-ADDRESS THRU                              
                   P5070-LAST-ADDRESS-EXIT                              
              VARYING INDX1 FROM 1 BY 1                                 
                 UNTIL INDX1 > 4 OR ADDRESS-WORK-LINE(INDX1) = SPACES.  
           PERFORM P5080-MOVE-ZIP THRU P5080-MOVE-ZIP-EXIT.             
       P5000-ADJUST-ADDRESS-EXIT.  EXIT.                                
      *                                                                 
       P5070-LAST-ADDRESS.                                              
       P5070-LAST-ADDRESS-EXIT. EXIT.                                   
      *                                                                 
       P5080-MOVE-ZIP.                                                  
      * INDX1 CONTAINS THAT FIRST LINE OF SPACES.                       
           MOVE ADDRESS-WORK-ZIP TO WORK-ZIP.                           
           IF   WORK-ZIP-PREFIX = SPACES THEN                           
                MOVE WORK-ZIP-CODE-1 TO WORK-ADDRESS                    
                MOVE SPACES TO WORK-ZIP                                 
                MOVE WORK-ADDRESS TO WORK-ZIP-CODE.                     
           IF   INDX1 = 1 THEN                                          
                MOVE WORK-ZIP TO ADDRESS-WORK-1                         
                GO TO P5080-MOVE-ZIP-EXIT.                              
           SUBTRACT +1 FROM INDX1.                                      
           MOVE ADDRESS-WORK-LINE(INDX1) TO WORK-ADDRESS.               
           MOVE 50 TO INDX2.                                            
           PERFORM P5085-FIND-CHAR THRU P5085-FIND-CHAR-EXIT            
              VARYING INDX2 FROM 50 BY -1                               
                 UNTIL WORK-ADDRESS-CHAR(INDX2) > SPACE OR INDX2 = 0.   
           IF   INDX2 < 50 THEN                                         
                ADD +1 TO INDX2                                         
                MOVE LOW-VALUES TO WORK-ADDRESS-CHAR(INDX2).            
           STRING WORK-ADDRESS '  ' WORK-ZIP                            
              DELIMITED BY LOW-VALUES INTO ADDRESS-WORK-LINE(INDX1).    
       P5080-MOVE-ZIP-EXIT. EXIT.                                       
      *                                                                 
       P5085-FIND-CHAR.                                                 
       P5085-FIND-CHAR-EXIT. EXIT.                                      
      *                                                                 
       PRINT-DRAFT-COPY.                                                
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE COPY-NAME TO DRS-DRRB-REPORT-ID.                        
           MOVE PRINT-LINE TO DRS-PRINT-LINE.                           
           PERFORM DRS-PUT-CALL THRU DRS-PUT-CALL-EXIT.                 
           MOVE SPACES TO PRINT-LINE.                                   
       PRINT-DRAFT-COPY-EXIT. EXIT.                                     
      *                                                                 
       PRINT-DRAFT-REPORT.                                              
           IF   LINE-SKIP > LINES-LEFT THEN                             
                PERFORM PRINT-REPORT-HEADS THRU PRINT-REPORT-HEADS-EXIT.
           MOVE REPORT-LINE TO PRINT-LINE.                              
           PERFORM PRINT-REPORT-LINES THRU PRINT-REPORT-LINES-EXIT.     
           MOVE SPACES TO REPORT-LINE.                                  
       PRINT-DRAFT-REPORT-EXIT. EXIT.                                   
      *                                                                 
       PRINT-REPORT-LINES.                                              
           IF   LINE-SKIP = 1 OR LINE-SKIP > 3 THEN                     
                MOVE ' ' TO CARRIAGE-CTL                                
                MOVE +1 TO LINE-SKIP                                    
           ELSE IF   LINE-SKIP = 2 THEN                                 
                     MOVE '0' TO CARRIAGE-CTL                           
                ELSE MOVE '-' TO CARRIAGE-CTL.                          
           MOVE PRINT-LINE TO DRS-PRINT-LINE.                           
           STRING CARRIAGE-CTL DRS-PRINT-LINE DELIMITED BY SIZE         
              INTO PRINT-LINE.                                          
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
           SUBTRACT LINE-SKIP FROM LINES-LEFT.                          
       PRINT-REPORT-LINES-EXIT. EXIT.                                   
      *                                                                 
       PRINT-REPORT-LINE.                                               
           MOVE LOW-VALUES TO DRS-DRRB-REQUEST-BLOCK.                   
           MOVE REPORT-NAME TO DRS-DRRB-REPORT-ID.                      
           MOVE PRINT-LINE TO DRS-PRINT-LINE.                           
           PERFORM DRS-PUT-CALL THRU DRS-PUT-CALL-EXIT.                 
           MOVE SPACES TO PRINT-LINE.                                   
       PRINT-REPORT-LINE-EXIT. EXIT.                                    
      *                                                                 
       PRINT-REPORT-HEADS.                                              
           MOVE '1' TO PRINT-LINE.                                      
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
           IF   SAVE-SEL101 = 'P' THEN                                  
                STRING SP31 'DRAFT PRINT REPORT'                        
                   DELIMITED BY SIZE INTO PRINT-LINE                    
           ELSE STRING SP30 'DRAFT REPRINT REPORT'                      
                   DELIMITED BY SIZE INTO PRINT-LINE.                   
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
           STRING SP5 TIME-MMDDYY SP22 ' FORM ' SAVE-FORM SP22          
              TIME-CLOCK                                                
                   DELIMITED BY SIZE INTO PRINT-LINE.                   
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
CSODJN* CHANGED SP8 TO SP10 BEFORE DRAFT                                
CSODJN     STRING '0' SP2 'SEQ#' SP1 'DRAFT' SP10 'AMOUNT'              
              SP1 'CLAIM' SP4 'POLICY' SP7 'PAYEE'                      
                 DELIMITED BY SIZE INTO PRINT-LINE.                     
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
           PERFORM PRINT-REPORT-LINE THRU PRINT-REPORT-LINE-EXIT.       
           MOVE +50 TO LINES-LEFT.                                      
       PRINT-REPORT-HEADS-EXIT. EXIT.                                   
      *                                                                 
                                                                        
       DRS-INIT-CALL.                                                   
           MOVE 'INIT'           TO DRS-DRRB-FUNCTION.                  
           MOVE 'DRRB'           TO DRS-DRRB-ID.                        
           MOVE 'DRIB'           TO DRS-DRIBX-ID.                       
           EXEC CICS LINK PROGRAM('DRS1INTC')                           
                          COMMAREA(DRS-COMM-AREA)                       
                          LENGTH(112)                                   
                          END-EXEC.                                     
       DRS-INIT-CALL-EXIT.                                              
           EXIT.                                                        
      *----------------------------------------------------------------*
       DRS-PUT-CALL.                                                    
           MOVE 'PUT'            TO DRS-DRRB-FUNCTION.                  
           MOVE 'DRRB'           TO DRS-DRRB-ID.                        
           MOVE 1                TO DRS-DRRB-DATA-LINE-CNT.             
           EXEC CICS LINK PROGRAM('DRS1INTC')                           
                          COMMAREA(DRS-COMM-AREA)                       
                          LENGTH(181)                                   
                          END-EXEC.                                     
       DRS-PUT-CALL-EXIT.                                               
           EXIT.                                                        
      *----------------------------------------------------------------*
       DRS-TERM-CALL.                                                   
           MOVE 'TERM'           TO DRS-DRRB-FUNCTION.                  
           MOVE 'DRRB'           TO DRS-DRRB-ID.                        
           EXEC CICS LINK PROGRAM('DRS1INTC')                           
                          COMMAREA(DRS-COMM-AREA)                       
                          LENGTH(48)                                    
                          END-EXEC.                                     
       DRS-TERM-CALL-EXIT.                                              
           EXIT.                                                        
      *                                                                 
       DRS-REQ-FAILURE.                                                 
           EXEC CICS DUMP DUMPCODE('DRSX') TASK END-EXEC.               
           EXEC CICS RETURN END-EXEC.                                   
       DRS-REQ-EXIT.                                                    
           EXIT.                                                        
      *                                                                 
       P9000-SHOW-CODE.                                                 
           IF   ERROR-CODE = ZERO GO TO P9000-SHOW-CODE-EXIT.           
           IF   ERROR-CODE > 4 THEN                                     
                MOVE +1 TO ERROR-CODE.                                  
           MOVE MIC1-ERR-MSG(ERROR-CODE) TO ERR-MSG.                    
       P9000-SHOW-CODE-EXIT. EXIT.                                      
      *                                                                 
       P9000-ERRMSG.                                                    
           EXEC CICS SEND TEXT FROM(ERROR-MSG) LENGTH(320) ERASE        
                FREEKB END-EXEC.                                        
           MOVE 80 TO REC-LGTH.                                         
           EXEC CICS RECEIVE INTO(ERR-MSG) MAXLENGTH(80)                
                LENGTH(REC-LGTH) END-EXEC.                              
           MOVE SPACES TO ERR-MSG.                                      
           EXEC CICS SEND TEXT FROM(ERROR-MSG) LENGTH(80) ERASE         
                FREEKB END-EXEC.                                        
           MOVE 'Y' TO MAIN-DONE-FLAG.                                  
           MOVE 'Y' TO DONE-FLAG.                                       
           MOVE 'Y' TO ERROR-FLAG.                                      
       P9000-ERRMSG-EXIT.                                               
           EXIT.                                                        
                                                                        
       CSO1-CHECK-FLAG.                                                 
                                                                        
           MOVE 'MICR    ' TO WS-CSO-PROGRAM                            
                                                                        
           EXEC CICS READ                                               
               DATASET ('MICRFLAG')                                     
               RIDFLD (WS-CSO-PROGRAM)                                  
               INTO (WS-MICR-FLAG)                                      
           END-EXEC.                                                    
                                                                        
           IF WS-CSO-MESSAGE = 'DONE'                                   
               GO TO CSO1-EXIT                                          
           ELSE                                                         
               STRING 'THE DRAFTS HAVE NOT BEEN WRITTEN TO THE   '      
                   'MICR FILE - TRY AGAIN LATER' DELIMITED BY SIZE      
                      INTO ERR-MSG.                                     
                                                                        
       CSO1-EXIT.                                                       
           EXIT.                                                        
