       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.                 EL128 .                                      
      *              PROGRAM CONVERTED BY                                       
      *              COBOL CONVERSION AID PO 5785-ABJ                           
      *              CONVERSION DATE 02/07/95 10:52:52.                         
      *                            VMOD=2.014                                   
      *                                                                         
      *                                                                         
      *AUTHOR.    CENTRAL STATES HEALTH AND LIFE                                
      *           OMAHA, NEBRASKA                                               
                                                                                
      *DATE-COMPILED.                                                           
                                                                                
      *SECURITY.   *****************************************************        
      *            *                                                   *        
      *            *   THIS PROGRAM IS THE PROPERTY OF CNETRAL STATES  *        
      *            *   HEALTH AND LIFE                                 *        
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *        
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *        
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *        
      *            *                                                   *        
      *            *****************************************************        
                                                                                
      *REMARKS.                                                                 
      *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR            
      *    THE PURGED CERT LOOK-UP.                                             
                                                                                
      *    SCREENS     - EL128A - PURGED CERT LOOK-UP QUALIFICATION             
                                                                                
      *    ENTERED BY  - EL126 - MASTER MENU                                    
                                                                                
      *    EXIT TO     - CALLING PROGRAM                                        
                                                                                
      *    INPUT FILE  - ELPURG - PURGED CERT MASTER FILE                       
      *                  ERACCT2 - CREDIT ACCOUNT MASTER FILE                   
                                                                                
      *    OUTPUT FILE - NONE                                                   
                                                                                
      *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE             
      *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE           
      *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR                
      *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM         
      *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE          
      *                  RECORD KEY INFORMATION NEEDED BY EL1282 TO             
      *                  LOCATE THE CERTIFICATE.                                
                                                                                
                                                                                
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON             
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE         
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE        
      *                  ENTRIES (XCTL FROM CICS VIA EX15) THE SCREEN           
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE           
      *                  MAINTENANCE TYPE INDICATED.                            
                                                                                
           EJECT                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
                                                                                
       77  FILLER  PIC X(32)  VALUE '********************************'.         
       77  FILLER  PIC X(32)  VALUE '*    EL128 WORKING STORAGE     *'.         
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.014 *********'.         
                                                                                
                                           COPY ELCSCTM.                        
                                                                                
                                           COPY ELCSCRTY.                       
                                                                                
       01  WS-DATE-AREA.                                                        
           05  SAVE-DATE                   PIC X(8)     VALUE SPACES.           
           05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.           
                                                                                
       01  FILLER                          COMP-3.                              
           05  WS-READNEXT-SW              PIC S9       VALUE ZERO.             
           05  TIME-IN                     PIC S9(7)    VALUE ZERO.             
           05  TIME-OUT                    REDEFINES                            
               TIME-IN                     PIC S9(3)V9(4).                      
                                                                                
       01  FILLER         COMP SYNC.                                            
           05  SC-ITEM                     PIC S9(4)    VALUE +0001.            
                                                                                
       01  FILLER.                                                              
           05  XCTL-725                    PIC X(8)     VALUE 'EL725'.          
           05  QID.                                                             
               10  QID-TERM                PIC X(4).                            
               10  FILLER                  PIC X(4)     VALUE '128A'.           
           05  QID-ITEM                    PIC S9(4)    VALUE +1 COMP.          
           05  WS-KEY-LENGTH               PIC S9(4)    VALUE +0 COMP.          
                                                                                
           05  PART-KEY-ON-SW              PIC X(01)    VALUE 'N'.              
               88  PART-KEY-ON                          VALUE 'Y'.              
                                                                                
           05  PART-FIELD-ON-SW            PIC X(01)    VALUE ' '.              
               88  PART-FIELD-ACCT                      VALUE 'A'.              
               88  PART-FIELD-STATE                     VALUE 'S'.              
               88  PART-FIELD-CERT                      VALUE 'C'.              
                                                                                
           05  WS-CNTL-KEY.                                                     
               10  WS-CNTL-ID              PIC X(3).                            
               10  WS-CNTL-TYPE            PIC X.                               
               10  WS-CNTL-USER            PIC X(4)     VALUE SPACES.           
               10  WS-CNTL-SEQ             PIC S9(4)    VALUE +0 COMP.          
                                                                                
           05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL128S'.         
           05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL128A'.         
                                                                                
           05  FILLER                      REDEFINES                            
               WS-MAP-NAME.                                                     
               10  FILLER                  PIC XX.                              
               10  WS-MAP-NUMBER           PIC X(4).                            
               10  FILLER                  PIC XX.                              
                                                                                
           05  THIS-PGM                    PIC X(8)     VALUE 'EL128'.          
                                                                                
           05  WS-CNTL-REC-FOUND-SW        PIC X(01)    VALUE SPACE.            
           05  WS-NEXT-COMPANY-ID          PIC X(03)    VALUE SPACES.           
                                                                                
           05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.         
           05  WS-ACCOUNT-MASTER-DSID      PIC X(8)     VALUE 'ERACCT2'.        
           05  WS-CERT-MASTER-DSID         PIC X(8)     VALUE 'ELPURG'.         
           05  WS-CERT-AIX01-DSID          PIC X(8)     VALUE 'ELPURG2'.        
           05  WS-CERT-AIX02-DSID          PIC X(8)     VALUE 'ELPURG3'.        
           05  WS-CERT-AIX03-DSID          PIC X(8)     VALUE 'ELPURG4'.        
           05  WS-CERT-AIX04-DSID          PIC X(8)     VALUE 'ELPURG5'.        
           05  WS-CERT-AIX05-DSID          PIC X(8)     VALUE 'ELPURG6'.        
                                                                                
           05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXXC'.           
                                                                                
           05  WK-SC-STATE.                                                     
               12  WK-SC-STATE-1           PIC X.                               
               12  WK-SC-STATE-2           PIC X.                               
                                                                                
           05  WK-SC-CERT.                                                      
               12  WK-SC-CERT-1            PIC X.                               
               12  WK-SC-CERT-2            PIC X.                               
               12  WK-SC-CERT-3            PIC X.                               
               12  WK-SC-CERT-4            PIC X.                               
               12  WK-SC-CERT-5            PIC X.                               
               12  WK-SC-CERT-6            PIC X.                               
               12  WK-SC-CERT-7            PIC X.                               
               12  WK-SC-CERT-8            PIC X.                               
               12  WK-SC-CERT-9            PIC X.                               
               12  WK-SC-CERT-10           PIC X.                               
                                                                                
           05  WS-DEEDIT-FIELD             PIC X(15)    VALUE ZERO.             
                                                                                
           05  WS-DEEDIT-FIELD-V0          REDEFINES                            
               WS-DEEDIT-FIELD             PIC S9(15).                          
                                                                                
           05  WS-INPUT-FIELD              PIC X(50)    VALUE SPACES.           
                                                                                
           05  WS-INPUT-CHAR               REDEFINES                            
               WS-INPUT-FIELD              PIC X                                
               OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.              
                                                                                
       01  WS-FIRST-NAME.                                                       
           05  WS-FIRST-INITIAL            PIC X        VALUE SPACES.           
           05  WS-FIRST-REST               PIC X(14)    VALUE SPACES.           
                                                                                
       01  WS-INITIALS.                                                         
           05  WS-INITIAL-FIRST            PIC X        VALUE SPACES.           
           05  WS-INITIAL-MIDDLE           PIC X        VALUE SPACES.           
                                                                                
           05  PI-ACCOUNT-KEY.                                                  
               10  PI-AK-COMPANY-CD        PIC X.                               
               10  PI-AK-CARRIER           PIC X.                               
               10  PI-AK-GROUP             PIC X(06).                           
               10  PI-AK-STATE             PIC XX.                              
               10  PI-AK-ACCOUNT           PIC X(10).                           
               10  PI-AK-EXPIRE-DATE       PIC XX.                              
                                                                                
           EJECT                                                                
           05  ERROR-MESSAGES.                                                  
               10  ER-0004                 PIC X(4)     VALUE '0004'.           
               10  ER-0008                 PIC X(4)     VALUE '0008'.           
               10  ER-0019                 PIC X(4)     VALUE '0019'.           
               10  ER-0022                 PIC X(4)     VALUE '0022'.           
               10  ER-0029                 PIC X(4)     VALUE '0029'.           
               10  ER-0070                 PIC X(4)     VALUE '0070'.           
               10  ER-0089                 PIC X(4)     VALUE '0089'.           
               10  ER-0194                 PIC X(4)     VALUE '0194'.           
               10  ER-0195                 PIC X(4)     VALUE '0195'.           
               10  ER-0196                 PIC X(4)     VALUE '0196'.           
               10  ER-0197                 PIC X(4)     VALUE '0197'.           
               10  ER-0198                 PIC X(4)     VALUE '0198'.           
               10  ER-0201                 PIC X(4)     VALUE '0201'.           
               10  ER-0210                 PIC X(4)     VALUE '0210'.           
               10  ER-0215                 PIC X(4)     VALUE '0215'.           
               10  ER-0216                 PIC X(4)     VALUE '0216'.           
               10  ER-0228                 PIC X(4)     VALUE '0228'.           
               10  ER-0488                 PIC X(4)     VALUE '0488'.           
               10  ER-0671                 PIC X(4)     VALUE '0671'.           
               10  ER-0764                 PIC X(4)     VALUE '0764'.           
               10  ER-0765                 PIC X(4)     VALUE '0765'.           
               10  ER-2370                 PIC X(4)     VALUE '2370'.           
               10  ER-2371                 PIC X(4)     VALUE '2371'.           
               10  ER-2373                 PIC X(4)     VALUE '2373'.           
               10  ER-8100                 PIC X(4)     VALUE '8100'.           
               10  ER-8101                 PIC X(4)     VALUE '8101'.           
               10  ER-8102                 PIC X(4)     VALUE '8102'.           
               10  ER-8103                 PIC X(4)     VALUE '8103'.           
               10  ER-8104                 PIC X(4)     VALUE '8104'.           
               10  ER-8105                 PIC X(4)     VALUE '8105'.           
               10  ER-8106                 PIC X(4)     VALUE '8106'.           
               10  ER-8107                 PIC X(4)     VALUE '8107'.           
                                                                                
           EJECT                                                                
                                           COPY ELCINTF.                        
                                                                                
                                           COPY ELC127PI.                       
               16  FILLER                  PIC X(167).                          
               16  PI-PART-KEY-SW          PIC X(01).                           
               16  PI-PART-FIELD-SW        PIC X(01).                           
               16  FILLER                  PIC X(138).                          
                                                                                
                                                                                
           EJECT                                                                
                                           COPY ELCEMIB.                        
                                                                                
           EJECT                                                                
                                           COPY ELCDATE.                        
                                                                                
           EJECT                                                                
                                           COPY ELCLOGOF.                       
                                                                                
           EJECT                                                                
                                           COPY EL128S.                         
                                                                                
           EJECT                                                                
                                           COPY ELCATTR.                        
                                                                                
           EJECT                                                                
                                           COPY ELCAID.                         
                                                                                
       01  FILLER                      REDEFINES                                
           DFHAID.                                                              
           05  FILLER                      PIC X(8).                            
                                                                                
           05  PF-VALUES                   PIC X                                
               OCCURS 24 TIMES.                                                 
                                                                                
           EJECT                                                                
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA                     PIC X(1024).                         
                                                                                
      *01 PARMLIST                         COMP                                 
      *                                    SYNC.                                
      *    05  FILLER                      PIC S9(9).                           
      *    05  ELPURG-POINTER              PIC S9(9).                           
      *    05  ERACCT-POINTER              PIC S9(9).                           
      *    05  ELCNTL-POINTER              PIC S9(9).                           
                                                                                
           EJECT                                                                
                                           COPY ELCPURG.                        
                                                                                
           EJECT                                                                
                                           COPY ERCACCT.                        
                                                                                
           EJECT                                                                
                                           COPY ELCCNTL.                        
                                                                                
           EJECT                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
           CONTINUE.                                                            
                                                                                
           MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                     
           MOVE '5'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                           
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                       
                                                                                
           MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.             
                                                                                
           MOVE +2                     TO  EMI-NUMBER-OF-LINES                  
                                           EMI-SWITCH2.                         
                                                                                
      *    NOTE *******************************************************         
      *         *                                                     *         
      *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *         
      *         *  FROM ANOTHER MODULE.                               *         
      *         *                                                     *         
      *         *******************************************************.        
                                                                                
           IF EIBCALEN NOT GREATER THAN ZERO                                    
               MOVE UNACCESS-MSG       TO  LOGOFF-MSG                           
               GO TO 8300-SEND-TEXT.                                            
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               PGMIDERR (9600-PGMIDERR)                                         
               NOTFND   (0030-MAIN-LOGIC)                                       
               ENDFILE  (0030-MAIN-LOGIC)                                       
               ERROR    (9990-ERROR)                                            
           END-EXEC.                                                            
                                                                                
           IF PI-CALLING-PROGRAM NOT = 'EL1282'                                 
               MOVE ZERO                TO PI-ALT-NAME-COUNT.                   
                                                                                
           EJECT                                                                
       0010-MAIN-LOGIC.                                                         
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
                   PERFORM 7000-BUILD-SCREEN                                    
             ELSE                                                               
               GO TO 0020-CONTINUE-PROCESSING.                                  
                                                                                
       0015-INITIALIZE.                                                         
      *    NOTE *******************************************************         
      *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *         
      *         *  INTERFACE BLOCK FOR THIS MODULE.                   *         
      *         *******************************************************.        
           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA                 
                                           PI-CONTROL-IN-PROGRESS.              
                                                                                
           MOVE ZERO                   TO  PI-1ST-TIME-SW                       
                                           PI-LINE-COUNT                        
                                           PI-BROWSE-SW                         
                                           PI-KEY-LENGTH                        
                                           PI-TS-ITEM                           
                                           PI-END-OF-FILE                       
                                           PI-START-SW                          
                                           PI-AIX-RECORD-COUNT.                 
                                                                                
      *    NOTE *******************************************************         
      *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *         
      *         *  FOR EL128.                                         *         
      *         *******************************************************.        
                                                                                
           MOVE LOW-VALUES             TO  EL128AO.                             
                                                                                
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
           EJECT                                                                
       0020-CONTINUE-PROCESSING.                                                
           IF PI-1ST-TIME-SW NOT = ZERO                                         
               GO TO 0015-INITIALIZE.                                           
                                                                                
      *    NOTE *******************************************************         
      *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *         
      *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *         
      *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *         
      *         *******************************************************.        
                                                                                
           IF EIBAID = DFHCLEAR                                                 
               GO TO 9400-CLEAR.                                                
                                                                                
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                               
               MOVE LOW-VALUES         TO  EL128AO                              
               MOVE -1                 TO  APFKL                                
               MOVE ER-0008            TO  EMI-ERROR                            
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           EXEC CICS RECEIVE                                                    
               INTO   (EL128AI)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
           END-EXEC.                                                            
                                                                                
           IF APFKL GREATER ZERO                                                
               IF EIBAID NOT = DFHENTER                                         
                   MOVE ER-0004        TO  EMI-ERROR                            
                   MOVE AL-UNBOF       TO  APFKA                                
                   MOVE -1             TO  APFKL                                
                   GO TO 8200-SEND-DATAONLY                                     
               ELSE                                                             
                   IF APFKO IS NUMERIC                                          
                   IF APFKO GREATER 0 AND LESS 25                               
                       MOVE PF-VALUES (APFKI)  TO  EIBAID                       
                     ELSE                                                       
                       MOVE ER-0029        TO  EMI-ERROR                        
                       MOVE AL-UNBOF       TO  APFKA                            
                       MOVE -1             TO  APFKL                            
                       GO TO 8200-SEND-DATAONLY.                                
                                                                                
           IF EIBAID = DFHPF12                                                  
               MOVE 'EL010'         TO  THIS-PGM                                
               GO TO 9300-XCTL.                                                 
                                                                                
           IF EIBAID = DFHPF23                                                  
               GO TO 9000-RETURN-CICS.                                          
                                                                                
           IF EIBAID = DFHPF24                                                  
               MOVE 'EL126'         TO  THIS-PGM                                
               GO TO 9300-XCTL.                                                 
                                                                                
           IF EIBAID NOT = DFHENTER                                             
               MOVE ER-0008            TO  EMI-ERROR                            
               MOVE -1                 TO  APFKL                                
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           EJECT                                                                
       0025-MAIN-LOGIC.                                                         
                                                                                
           MOVE SPACES                 TO  PI-SELECTION-CRITERIA                
                                           PI-CERTIFICATE-KEY.                  
                                                                                
           MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD                     
                                           PI-CK-COMPANY-CD.                    
                                                                                
           MOVE 'EL1282'               TO  THIS-PGM.                            
                                                                                
           IF PI-PROCESSOR-ID = 'LGXX'                                          
               NEXT SENTENCE                                                    
           ELSE                                                                 
               EXEC CICS READQ TS                                               
                   QUEUE   (PI-SECURITY-TEMP-STORE-ID)                          
                   INTO    (SECURITY-CONTROL)                                   
                   LENGTH  (SC-COMM-LENGTH)                                     
                   ITEM    (SC-ITEM)                                            
               END-EXEC                                                         
               MOVE SC-CREDIT-DISPLAY (31)  TO  PI-DISPLAY-CAP                  
               MOVE SC-CREDIT-UPDATE  (31)  TO  PI-MODIFY-CAP                   
               IF NOT DISPLAY-CAP                                               
                   MOVE 'READ'              TO  SM-READ                         
                   PERFORM 9995-SECURITY-VIOLATION                              
                   MOVE ER-0070             TO  EMI-ERROR                       
                   GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                                
           EJECT                                                                
      ******************************************************************        
      *           O P T I O N  1  P R O C E S S I N G                  *        
      ******************************************************************        
                                                                                
           IF ACRTNO4L GREATER ZERO  OR                                         
              ACRTSX4L GREATER ZERO                                             
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 0027-MAIN-LOGIC.                                           
                                                                                
           MOVE WS-CERT-AIX04-DSID     TO  PI-DSID.                             
           MOVE '1'                    TO  PI-OPTION.                           
                                                                                
           IF ACRTNO4L NOT GREATER ZERO  AND                                    
              ACRTSX4L GREATER ZERO                                             
               MOVE ER-0210            TO  EMI-ERROR                            
               MOVE -1                 TO  ACRTNO4L                             
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           MOVE ACRTNO4I               TO  PI-SC-CERT-PRIME-A4.                 
           MOVE +11                    TO  PI-KEY-LENGTH.                       
                                                                                
           IF ACRTSX4L GREATER ZERO                                             
               MOVE ACRTSX4I           TO  PI-SC-CERT-SFX-A4                    
               MOVE +12                TO  PI-KEY-LENGTH.                       
                                                                                
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.                  
                                                                                
           MOVE -1                     TO  ACRTNO4L.                            
           PERFORM 4000-READ-CERT-FILE.                                         
                                                                                
           EJECT                                                                
       0027-MAIN-LOGIC.                                                         
      ******************************************************************        
      *           O P T I O N  2  P R O C E S S I N G                  *        
      ******************************************************************        
                                                                                
           IF ACERTNOL GREATER ZERO  OR                                         
              ACERTSXL GREATER ZERO  OR                                         
              AACCTNOL GREATER ZERO  OR                                         
              ASTATEL  GREATER ZERO  OR                                         
              ACARIERL GREATER ZERO  OR                                         
              AGROUPL  GREATER ZERO  OR                                         
              AEDATEL  GREATER ZERO                                             
               NEXT SENTENCE                                                    
             ELSE                                                               
               GO TO 0100-MAIN-LOGIC.                                           
                                                                                
            ADD +1                    TO  WS-KEY-LENGTH.                        
      ************************************************************              
      *        SECURITY CHECK FOR ACCOUNT AND CARRIER NO         *              
      *                      03/29/84                            *              
      ************************************************************              
                                                                                
           IF  PI-NO-ACCOUNT-SECURITY AND PI-NO-CARRIER-SECURITY                
               GO TO 0028-PROCESS-OPTION-2.                                     
                                                                                
           IF  PI-NO-ACCOUNT-SECURITY                                           
               GO TO 0028-CHECK-CARRIER-SECURITY.                               
                                                                                
           IF  AACCTNOL GREATER ZERO                                            
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 0028-CHECK-CARRIER-SECURITY.                               
                                                                                
           IF  AACCTNOI = PI-ACCOUNT-SECURITY                                   
               MOVE AL-UANON           TO  AACCTNOA                             
           ELSE                                                                 
               MOVE -1                 TO  AACCTNOL                             
               MOVE AL-UABON           TO  AACCTNOA                             
               MOVE ER-2371            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
       0028-CHECK-CARRIER-SECURITY.                                             
           IF  PI-NO-CARRIER-SECURITY                                           
               GO TO  0028-PROCESS-OPTION-2.                                    
                                                                                
           IF ACARIERL GREATER ZERO                                             
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO  0028-ERROR-CHECK.                                         
                                                                                
           IF  ACARIERI = PI-CARRIER-SECURITY                                   
               MOVE AL-UANON            TO  ACARIERA                            
           ELSE                                                                 
               MOVE -1                  TO  ACARIERL                            
               MOVE ER-2370             TO  EMI-ERROR                           
               MOVE AL-UABON            TO  ACARIERA                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
       0028-ERROR-CHECK.                                                        
           IF  EMI-FATAL-CTR GREATER ZERO                                       
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
       0028-PROCESS-OPTION-2.                                                   
           MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.                             
           MOVE '2'                    TO  PI-OPTION.                           
                                                                                
           IF ACARIERL GREATER ZERO                                             
               MOVE ACARIERI           TO  PI-SC-CARRIER                        
           ELSE                                                                 
               IF PI-CERT-ACCESS-CONTROL = ('1' OR '2' OR '4')                  
                   MOVE ER-0194        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT                                    
                   MOVE -1             TO  ACARIERL.                            
                                                                                
           IF ACARIERL GREATER ZERO  AND                                        
              AGROUPL  = ZERO  AND                                              
              ASTATEL  = ZERO  AND                                              
              AACCTNOL = ZERO  AND                                              
              AEDATEL  = ZERO  AND                                              
              ACERTNOL = ZERO  AND                                              
              ACERTSXL = ZERO                                                   
              MOVE SPACES                  TO  PI-SC-GROUP                      
                                               PI-SC-STATE                      
                                               PI-SC-ACCOUNT                    
                                               PI-SC-EFF-DATE                   
                                               PI-SC-CERT-NO                    
              ADD +1                    TO  WS-KEY-LENGTH                       
              MOVE 'Y'                  TO  PART-KEY-ON-SW                      
              GO TO 0400-MAIN-LOGIC.                                            
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.                                 
                                                                                
           IF  AGROUPL  > ZERO  AND                                             
               ACARIERL = ZERO                                                  
               MOVE -1                 TO  AGROUPL                              
               MOVE AL-UABON           TO  AGROUPA                              
               MOVE ER-8101            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF AGROUPL GREATER ZERO                                              
               MOVE AGROUPI            TO  PI-SC-GROUP                          
           ELSE                                                                 
               IF PI-CERT-ACCESS-CONTROL = '1'                                  
                   MOVE ER-0195        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT                                    
                   MOVE -1             TO  AGROUPL.                             
                                                                                
           IF AGROUPL  > ZERO  AND                                              
              ASTATEL  = ZERO  AND                                              
              AACCTNOL = ZERO  AND                                              
              AEDATEL  = ZERO  AND                                              
              ACERTNOL = ZERO  AND                                              
              ACERTSXL = ZERO                                                   
              MOVE SPACES                  TO  PI-SC-STATE                      
                                               PI-SC-ACCOUNT                    
                                               PI-SC-EFF-DATE                   
                                               PI-SC-CERT-NO                    
              ADD +7                    TO  WS-KEY-LENGTH                       
              MOVE 'Y'                  TO  PART-KEY-ON-SW                      
              GO TO 0400-MAIN-LOGIC.                                            
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.                                 
                                                                                
           IF ASTATEL GREATER ZERO AND                                          
               AGROUPL  = ZERO                                                  
               MOVE -1                 TO  ASTATEL                              
               MOVE AL-UABON           TO  ASTATEA                              
               MOVE ER-8102            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF ASTATEL GREATER ZERO                                              
               MOVE ASTATEI            TO  PI-SC-STATE                          
                                           WK-SC-STATE                          
           ELSE                                                                 
               IF PI-CERT-ACCESS-CONTROL = (SPACES OR '1' OR '2')               
                   MOVE ER-0196        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT                                    
                   MOVE -1             TO  ASTATEL.                             
                                                                                
           IF ASTATEL GREATER ZERO                                              
               IF WK-SC-STATE-1 = SPACES OR LOW-VALUES                          
                   MOVE ER-8106        TO  EMI-ERROR                            
                   MOVE -1             TO  ASTATEL                              
                   GO TO 8200-SEND-DATAONLY                                     
               ELSE                                                             
                   IF WK-SC-STATE-2 = SPACES OR LOW-VALUES                      
                      MOVE 'S'         TO  PART-FIELD-ON-SW                     
                      MOVE LOW-VALUES  TO  WK-SC-STATE-2                        
                      MOVE WK-SC-STATE TO  PI-SC-STATE.                         
                                                                                
           IF ASTATEL  > ZERO  AND                                              
              AACCTNOL = ZERO  AND                                              
              AEDATEL  = ZERO  AND                                              
              ACERTNOL = ZERO  AND                                              
              ACERTSXL = ZERO                                                   
              MOVE SPACES                  TO  PI-SC-ACCOUNT                    
                                               PI-SC-EFF-DATE                   
                                               PI-SC-CERT-NO                    
              MOVE 'Y'                  TO  PART-KEY-ON-SW                      
              IF PART-FIELD-STATE                                               
                 ADD +8                 TO  WS-KEY-LENGTH                       
                 GO TO 0400-MAIN-LOGIC                                          
              ELSE                                                              
                 ADD +9                 TO  WS-KEY-LENGTH                       
                 GO TO 0400-MAIN-LOGIC.                                         
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.                                 
                                                                                
           IF  AACCTNOL GREATER ZERO  AND                                       
               ASTATEL  = ZERO                                                  
               MOVE -1                 TO  AACCTNOL                             
               MOVE AL-UABON           TO  AACCTNOA                             
               MOVE ER-8100            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF AACCTNOL GREATER ZERO                                             
               MOVE AACCTNOI           TO  PI-SC-ACCOUNT                        
           ELSE                                                                 
               MOVE ER-0197            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT                                        
               MOVE -1                 TO  AACCTNOL.                            
                                                                                
           IF AACCTNOL > ZERO  AND                                              
              AEDATEL  = ZERO  AND                                              
              ACERTNOL = ZERO  AND                                              
              ACERTSXL = ZERO                                                   
              MOVE SPACES                  TO  PI-SC-EFF-DATE                   
                                               PI-SC-CERT-NO                    
              ADD +19                   TO  WS-KEY-LENGTH                       
              MOVE 'Y'                  TO  PART-KEY-ON-SW                      
              GO TO 0400-MAIN-LOGIC.                                            
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.                                 
                                                                                
           IF  AEDATEL GREATER ZERO   AND                                       
               AACCTNOL = ZERO                                                  
               MOVE -1                 TO  AEDATEL                              
               MOVE AL-UABON           TO  AEDATEA                              
               MOVE ER-8103            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF  AEDATEL GREATER   ZERO   AND                                     
               AEDATEL LESS THAN +6                                             
               MOVE -1                 TO  AEDATEL                              
               MOVE AL-UABON           TO  AEDATEA                              
               MOVE ER-8105            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF AEDATEL GREATER ZERO                                              
               MOVE AEDATEI            TO  WS-DEEDIT-FIELD                      
               PERFORM 8600-DEEDIT                                              
               IF WS-DEEDIT-FIELD-V0 NUMERIC                                    
                   MOVE WS-DEEDIT-FIELD-V0  TO  AEDATEO                         
                   INSPECT AEDATEI CONVERTING SPACES TO '/'                     
                   MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY               
                   MOVE '4'                TO  DC-OPTION-CODE                   
                   PERFORM 8500-DATE-CONVERSION                                 
                   IF DC-ERROR-CODE NOT = SPACES                                
                       MOVE ER-0215        TO  EMI-ERROR                        
                       PERFORM 9900-ERROR-FORMAT                                
                       MOVE -1             TO  AEDATEL                          
                       MOVE AL-UABON       TO  AEDATEA                          
                   ELSE                                                         
                       MOVE AL-UANON       TO  AEDATEA                          
                       MOVE DC-BIN-DATE-1  TO  PI-SC-EFF-DATE                   
               ELSE                                                             
                   MOVE ER-0215        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT                                    
                   MOVE -1             TO  AEDATEL                              
                   MOVE AL-UABON       TO  AEDATEA                              
           ELSE                                                                 
               MOVE ER-0216            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT                                        
               MOVE -1                 TO  AEDATEL                              
               MOVE AL-UABOF           TO  AEDATEA.                             
                                                                                
           IF AEDATEL  > ZERO  AND                                              
              ACERTNOL = ZERO  AND                                              
              ACERTSXL = ZERO                                                   
              ADD +21                   TO  WS-KEY-LENGTH                       
              MOVE 'Y'                  TO  PART-KEY-ON-SW                      
              MOVE SPACES               TO  PI-SC-CERT-NO                       
              GO TO 0400-MAIN-LOGIC.                                            
                                                                                
       0028-PROCESS-OPTION-2-CONT.                                              
           MOVE +22                    TO  PI-KEY-LENGTH                        
                                           WS-KEY-LENGTH.                       
                                                                                
           IF  ACERTNOL GREATER ZERO  AND                                       
               AEDATEL  = ZERO                                                  
               MOVE -1                 TO  ACERTNOL                             
               MOVE AL-UABON           TO  ACERTNOA                             
               MOVE ER-8104            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF ACERTNOL GREATER ZERO                                             
               MOVE ACERTNOI           TO  PI-SC-CERT-PRIME                     
                                           WK-SC-CERT                           
               MOVE +32                TO  PI-KEY-LENGTH.                       
                                                                                
           PERFORM 0029-TEST-CERT-LENGTH THRU 0029-EXIT.                        
                                                                                
           IF ACERTSXL GREATER ZERO                                             
               MOVE ACERTSXI           TO  PI-SC-CERT-SFX                       
               MOVE +33                TO  PI-KEY-LENGTH.                       
                                                                                
           IF EMI-FATAL-CTR GREATER ZERO                                        
               GO TO 0040-MAIN-LOGIC.                                           
                                                                                
           IF PI-SC-CARRIER  NOT = SPACES  AND                                  
              PI-SC-GROUP    NOT = SPACES  AND                                  
              PI-SC-STATE    NOT = SPACES  AND                                  
              PI-SC-ACCOUNT  NOT = SPACES  AND                                  
              PI-SC-EFF-DATE NOT = SPACES  AND                                  
              PI-SC-CERT-NO  NOT = SPACES                                       
               MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY               
               MOVE -1                     TO  ACERTNOL                         
               MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                          
               MOVE +33                    TO  PI-KEY-LENGTH                    
               PERFORM 4000-READ-CERT-FILE.                                     
                                                                                
           IF PI-SC-CARRIER  = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-CARRIER.                   
           IF PI-SC-GROUP    = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-GROUP.                     
           IF PI-SC-STATE    = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-STATE.                     
           IF PI-SC-ACCOUNT  = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-ACCOUNT.                   
           IF PI-SC-EFF-DATE = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-EFF-DATE.                  
           IF PI-SC-CERT-NO  = SPACES                                           
              MOVE LOW-VALUES              TO  PI-SC-CERT-NO.                   
                                                                                
      *       MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY                
      *       MOVE -1                     TO  ACERTNOL                          
      *       MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                           
      *       PERFORM 3900-READNEXT-CERT-FILE.                                  
                                                                                
           MOVE SPACES                 TO  PI-ACCOUNT-KEY.                      
           MOVE PI-COMPANY-CD          TO  PI-AK-COMPANY-CD.                    
                                                                                
           IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'                        
               MOVE PI-SC-CARRIER      TO  PI-AK-CARRIER.                       
                                                                                
           IF PI-CERT-ACCESS-CONTROL = '1'                                      
               MOVE PI-SC-GROUP        TO  PI-AK-GROUP.                         
                                                                                
           IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'                     
               MOVE PI-SC-STATE        TO  PI-AK-STATE.                         
                                                                                
           MOVE PI-SC-EFF-DATE         TO  PI-AK-EXPIRE-DATE.                   
                                                                                
           MOVE PI-SC-ACCOUNT          TO  PI-AK-ACCOUNT.                       
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND   (0030-MAIN-LOGIC)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS STARTBR                                                    
               DATASET   (WS-ACCOUNT-MASTER-DSID)                               
               RIDFLD    (PI-ACCOUNT-KEY)                                       
               GTEQ                                                             
           END-EXEC.                                                            
                                                                                
           MOVE +1                     TO  WS-READNEXT-SW.                      
                                                                                
       0028-MAIN-LOGIC.                                                         
           EXEC CICS READNEXT                                                   
               DATASET   (WS-ACCOUNT-MASTER-DSID)                               
               RIDFLD    (PI-ACCOUNT-KEY)                                       
               SET       (ADDRESS OF ACCOUNT-MASTER)                            
           END-EXEC.                                                            
                                                                                
           CONTINUE.                                                            
                                                                                
           IF PI-SC-COMPANY-CD = PI-AK-COMPANY-CD  AND                          
              PI-SC-CARRIER    = PI-AK-CARRIER     AND                          
              PI-SC-GROUP      = PI-AK-GROUP       AND                          
              PI-SC-STATE      = PI-AK-STATE       AND                          
              PI-SC-ACCOUNT    = PI-AK-ACCOUNT                                  
               NEXT SENTENCE                                                    
             ELSE                                                               
               GO TO 0030-MAIN-LOGIC.                                           
                                                                                
           IF PI-SC-EFF-DATE LESS AM-EFFECTIVE-DT                               
               GO TO 0030-MAIN-LOGIC.                                           
                                                                                
           IF PI-SC-EFF-DATE LESS AM-EXPIRATION-DT                              
               GO TO 0040-MAIN-LOGIC.                                           
                                                                                
           GO TO 0028-MAIN-LOGIC.                                               
                                                                                
       0029-TEST-CERT-LENGTH.                                                   
           IF ACERTNOL NOT GREATER ZERO                                         
              GO TO 0029-EXIT.                                                  
                                                                                
           IF WK-SC-CERT-1 = LOW-VALUES OR SPACES                               
              MOVE ER-8107             TO  EMI-ERROR                            
              MOVE -1                  TO  ACERTNOL                             
              GO TO 8200-SEND-DATAONLY.                                         
                                                                                
           IF WK-SC-CERT-2     = SPACES OR LOW-VALUES                           
              ADD +1                   TO  WS-KEY-LENGTH                        
              MOVE LOW-VALUES          TO  WK-SC-CERT-3                         
                                           WK-SC-CERT-4                         
                                           WK-SC-CERT-5                         
                                           WK-SC-CERT-6                         
                                           WK-SC-CERT-7                         
                                           WK-SC-CERT-8                         
                                           WK-SC-CERT-9                         
                                           WK-SC-CERT-10                        
           ELSE                                                                 
             IF (WK-SC-CERT-3     = SPACES OR LOW-VALUES)                       
                ADD +2                 TO  WS-KEY-LENGTH                        
                MOVE LOW-VALUES        TO  WK-SC-CERT-4                         
                                           WK-SC-CERT-5                         
                                           WK-SC-CERT-6                         
                                           WK-SC-CERT-7                         
                                           WK-SC-CERT-8                         
                                           WK-SC-CERT-9                         
                                           WK-SC-CERT-10                        
             ELSE                                                               
               IF (WK-SC-CERT-4     = SPACES OR LOW-VALUES)                     
                  ADD +3               TO  WS-KEY-LENGTH                        
                  MOVE LOW-VALUES      TO  WK-SC-CERT-5                         
                                           WK-SC-CERT-6                         
                                           WK-SC-CERT-7                         
                                           WK-SC-CERT-8                         
                                           WK-SC-CERT-9                         
                                           WK-SC-CERT-10                        
               ELSE                                                             
                 IF (WK-SC-CERT-5     = SPACES OR LOW-VALUES)                   
                    ADD +4                 TO  WS-KEY-LENGTH                    
                    MOVE LOW-VALUES        TO  WK-SC-CERT-6                     
                                               WK-SC-CERT-7                     
                                               WK-SC-CERT-8                     
                                               WK-SC-CERT-9                     
                                               WK-SC-CERT-10                    
                 ELSE                                                           
                   IF (WK-SC-CERT-6     = SPACES OR LOW-VALUES)                 
                      ADD +5               TO  WS-KEY-LENGTH                    
                      MOVE LOW-VALUES      TO  WK-SC-CERT-7                     
                                               WK-SC-CERT-8                     
                                               WK-SC-CERT-9                     
                                               WK-SC-CERT-10                    
                   ELSE                                                         
                     IF (WK-SC-CERT-7     = SPACES OR LOW-VALUES)               
                        ADD +6             TO  WS-KEY-LENGTH                    
                        MOVE LOW-VALUES    TO  WK-SC-CERT-8                     
                                               WK-SC-CERT-9                     
                                               WK-SC-CERT-10                    
                     ELSE                                                       
                       IF (WK-SC-CERT-8     = SPACES OR LOW-VALUES)             
                          ADD +7               TO  WS-KEY-LENGTH                
                          MOVE LOW-VALUES      TO  WK-SC-CERT-9                 
                                                   WK-SC-CERT-10                
                       ELSE                                                     
                         IF (WK-SC-CERT-9     = SPACES OR LOW-VALUES)           
                            ADD +8             TO  WS-KEY-LENGTH                
                            MOVE LOW-VALUES    TO  WK-SC-CERT-10                
                         ELSE                                                   
                           IF (WK-SC-CERT-10    = SPACES OR LOW-VALUES)         
                              ADD +9           TO  WS-KEY-LENGTH                
                              MOVE LOW-VALUES  TO  WK-SC-CERT-10                
                           ELSE                                                 
                              GO TO 0029-EXIT.                                  
                                                                                
           MOVE WK-SC-CERT          TO  PI-SC-CERT-PRIME                        
           MOVE 'Y'                 TO  PART-KEY-ON-SW                          
           MOVE 'C'                 TO  PART-FIELD-ON-SW                        
           GO TO 0400-MAIN-LOGIC.                                               
       0029-EXIT. EXIT.                                                         
                                                                                
       0030-MAIN-LOGIC.                                                         
      *    MOVE ER-0198                TO  EMI-ERROR.                           
      *    PERFORM 9900-ERROR-FORMAT.                                           
      *    MOVE -1                     TO  AACCTNOL.                            
                                                                                
           IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'                        
               IF ACARIERL GREATER ZERO                                         
                   MOVE AL-UABON           TO  ACARIERA                         
               ELSE                                                             
                   MOVE AL-UABOF           TO  ACARIERA.                        
                                                                                
           IF PI-CERT-ACCESS-CONTROL = '1'                                      
               IF AGROUPL GREATER ZERO                                          
                   MOVE AL-UABON           TO  AGROUPA                          
               ELSE                                                             
                   MOVE AL-UABOF           TO  AGROUPA.                         
                                                                                
           IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'                     
               IF ASTATEL GREATER ZERO                                          
                   MOVE AL-UABON           TO  ASTATEA                          
               ELSE                                                             
                   MOVE AL-UABOF           TO  ASTATEA.                         
                                                                                
           IF AACCTNOL GREATER ZERO                                             
               MOVE AL-UABON           TO  AACCTNOA                             
           ELSE                                                                 
               MOVE AL-UABOF           TO  AACCTNOA.                            
                                                                                
           IF AEDATEL GREATER ZERO                                              
               MOVE AL-UNBON           TO  AEDATEA                              
           ELSE                                                                 
               MOVE AL-UNBOF           TO  AEDATEA.                             
                                                                                
       0040-MAIN-LOGIC.                                                         
           IF EMI-FATAL-CTR GREATER ZERO                                        
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           MOVE AM-CONTROL-PRIMARY     TO  PI-ACCOUNT-KEY.                      
                                                                                
           MOVE AM-CARRIER             TO  PI-SC-CARRIER.                       
           MOVE AM-GROUPING            TO  PI-SC-GROUP.                         
           MOVE AM-STATE               TO  PI-SC-STATE.                         
                                                                                
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.                  
                                                                                
           IF PI-KEY-LENGTH NOT GREATER +22                                     
               PERFORM 4000-READ-CERT-FILE.                                     
                                                                                
           IF WS-READNEXT-SW NOT = ZERO                                         
               EXEC CICS ENDBR                                                  
                   DATASET (WS-ACCOUNT-MASTER-DSID)                             
               END-EXEC.                                                        
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (0050-MAIN-LOGIC)                                         
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET (WS-CERT-MASTER-DSID)                                    
               RIDFLD  (PI-CERTIFICATE-KEY)                                     
               SET     (ADDRESS OF PURGE-CERT-MASTER)                           
           END-EXEC.                                                            
                                                                                
           CONTINUE.                                                            
                                                                                
           MOVE PI-CK-CARRIER          TO  PI-CARRIER.                          
           MOVE PI-CK-GROUPING         TO  PI-GROUPING.                         
           MOVE PI-CK-STATE            TO  PI-STATE.                            
           MOVE PI-CK-ACCOUNT          TO  PI-ACCOUNT.                          
           MOVE PI-CK-CERT-EFF-DT      TO  PI-CERT-EFF-DT.                      
           MOVE PI-CK-CERT-NO          TO  PI-CERT-NO.                          
                                                                                
           MOVE 'EL1283'               TO  THIS-PGM.                            
           MOVE +1                     TO  PI-1ST-TIME-SW.                      
           GO TO 9300-XCTL.                                                     
                                                                                
       0050-MAIN-LOGIC.                                                         
           MOVE ER-0201                TO  EMI-ERROR.                           
           MOVE -1                     TO  ACERTNOL.                            
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
           EJECT                                                                
       0100-MAIN-LOGIC.                                                         
      ******************************************************************        
      *           O P T I O N  3  P R O C E S S I N G                  *        
      ******************************************************************        
                                                                                
           IF ALNAMEL  GREATER ZERO  OR                                         
              AFNAMEL  GREATER ZERO  OR                                         
              AINITALL GREATER ZERO  OR                                         
              AACCT2L  GREATER ZERO  OR                                         
              ACARRL   GREATER ZERO                                             
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 0200-MAIN-LOGIC.                                           
                                                                                
           IF (AFNAMEL  GREATER ZERO OR                                         
               AINITALL GREATER ZERO OR                                         
               AACCT2L  GREATER ZERO OR                                         
               ACARRL   GREATER ZERO)                                           
                      AND                                                       
               ALNAMEL NOT GREATER ZERO                                         
                MOVE ER-0488            TO  EMI-ERROR                           
                MOVE -1                 TO  ALNAMEL                             
                GO TO 8200-SEND-DATAONLY.                                       
                                                                                
           IF AINITALL GREATER ZERO   AND                                       
              AFNAMEL NOT GREATER ZERO                                          
                MOVE ER-0764            TO  EMI-ERROR                           
                MOVE -1                 TO  AFNAMEL                             
                GO TO 8200-SEND-DATAONLY.                                       
                                                                                
      ************************************************************              
      *           SECURITY CHECK FOR ACCOUNT NUMBER              *              
      *                      03/29/84                            *              
      ************************************************************              
                                                                                
           IF  PI-NO-ACCOUNT-SECURITY                                           
               GO TO 0110-CHECK-EDITS.                                          
                                                                                
           IF  AACCT2L GREATER ZERO                                             
               IF  AACCT2I = PI-ACCOUNT-SECURITY                                
                  MOVE AL-UANON        TO  AACCT2A                              
               ELSE                                                             
                  MOVE -1              TO  AACCT2L                              
                  MOVE ER-2371         TO  EMI-ERROR                            
                  MOVE AL-UABON        TO  AACCT2A                              
                  GO TO 8200-SEND-DATAONLY.                                     
                                                                                
       0110-CHECK-EDITS.                                                        
           MOVE WS-CERT-AIX01-DSID     TO  PI-DSID.                             
           MOVE '3'                    TO  PI-OPTION.                           
           MOVE PI-COMPANY-CD          TO  PI-SELECTION-CRITERIA.               
                                                                                
           MOVE +1                     TO  PI-KEY-LENGTH.                       
                                                                                
           IF ALNAMEL GREATER ZERO                                              
               MOVE ALNAMEI            TO  PI-SC-LAST-NAME                      
                                           WS-INPUT-FIELD                       
               PERFORM 0120-MAIN-LOGIC                                          
                  THRU 0120-MAIN-LOGIC-EXIT                                     
                    VARYING INPUT-INDEX FROM ALNAMEL BY -1                      
                      UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE             
               ADD ALNAMEL  TO  PI-KEY-LENGTH.                                  
                                                                                
           IF AFNAMEL GREATER ZERO                                              
               MOVE +17                TO  PI-KEY-LENGTH                        
               MOVE AFNAMEI            TO  WS-FIRST-NAME                        
               MOVE WS-FIRST-INITIAL   TO  WS-INITIAL-FIRST.                    
                                                                                
           IF AFNAMEL GREATER +1                                                
               MOVE AFNAMEI            TO PI-SC-FIRST-NAME.                     
                                                                                
           IF AINITALL GREATER ZERO                                             
               MOVE +18                TO  PI-KEY-LENGTH                        
               MOVE AINITALI           TO  WS-INITIAL-MIDDLE.                   
                                                                                
           IF WS-INITIALS GREATER SPACES                                        
               MOVE WS-INITIALS        TO  PI-SC-INITIALS.                      
                                                                                
           IF AACCT2L GREATER ZERO                                              
               MOVE AACCT2I            TO  PI-SC-ACCT-NO.                       
                                                                                
           IF ACARRL GREATER ZERO                                               
               MOVE ACARRI             TO  PI-SC-CARR.                          
                                                                                
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.                  
                                                                                
           MOVE -1                     TO  ALNAMEL.                             
                                                                                
           PERFORM 4000-READ-CERT-FILE.                                         
                                                                                
       0120-MAIN-LOGIC.                                                         
           SUBTRACT +1 FROM ALNAMEL.                                            
                                                                                
       0120-MAIN-LOGIC-EXIT.                                                    
           EXIT.                                                                
                                                                                
           EJECT                                                                
       0200-MAIN-LOGIC.                                                         
      ******************************************************************        
      *           O P T I O N  4  P R O C E S S I N G                  *        
      ******************************************************************        
           IF ASSNL GREATER ZERO                                                
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 0300-MAIN-LOGIC.                                           
                                                                                
           MOVE '4'                    TO  PI-OPTION.                           
                                                                                
           MOVE WS-CERT-AIX02-DSID     TO  PI-DSID.                             
                                                                                
           MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD                     
                                           PI-CK-COMPANY-CD.                    
           MOVE ASSNI                  TO  PI-CK-SOC-SEC-NO                     
                                           PI-SC-SOC-SEC-NO                     
                                           WS-INPUT-FIELD.                      
                                                                                
           PERFORM 0220-MAIN-LOGIC THRU 0220-MAIN-LOGIC-EXIT                    
               VARYING INPUT-INDEX FROM ASSNL BY -1                             
                   UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.               
           ADD +1  ASSNL  GIVING  PI-KEY-LENGTH.                                
           MOVE -1                     TO  ASSNL.                               
           PERFORM 4000-READ-CERT-FILE.                                         
                                                                                
       0220-MAIN-LOGIC.                                                         
           SUBTRACT +1 FROM ASSNL.                                              
                                                                                
       0220-MAIN-LOGIC-EXIT.                                                    
           EXIT.                                                                
                                                                                
           EJECT                                                                
       0300-MAIN-LOGIC.                                                         
      ******************************************************************        
      *           O P T I O N  5  P R O C E S S I N G                  *        
      ******************************************************************        
           IF AMEMBERL GREATER ZERO                                             
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 0400-MAIN-LOGIC.                                           
                                                                                
           MOVE '5'                    TO  PI-OPTION.                           
                                                                                
           MOVE WS-CERT-AIX05-DSID     TO  PI-DSID.                             
                                                                                
           MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD                     
                                           PI-SC-COMPANY-CD.                    
           MOVE AMEMBERI               TO  PI-CK-MEMBER-NO                      
                                           PI-SC-MEMBER-NO                      
                                           WS-INPUT-FIELD.                      
                                                                                
           PERFORM 0320-MAIN-LOGIC THRU 0320-MAIN-LOGIC-EXIT                    
               VARYING INPUT-INDEX FROM AMEMBERL BY -1                          
                   UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.               
           ADD +1  AMEMBERL  GIVING  PI-KEY-LENGTH.                             
           MOVE -1                     TO  AMEMBERL.                            
           PERFORM 4000-READ-CERT-FILE.                                         
                                                                                
       0320-MAIN-LOGIC.                                                         
           SUBTRACT +1 FROM AMEMBERL.                                           
                                                                                
       0320-MAIN-LOGIC-EXIT.                                                    
           EXIT.                                                                
                                                                                
       0400-MAIN-LOGIC.                                                         
           MOVE +1                     TO  PI-KEY-LENGTH.                       
           IF  PART-KEY-ON                                                      
               MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY               
               MOVE WS-KEY-LENGTH      TO  PI-KEY-LENGTH.                       
           MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.                             
           MOVE ZERO                   TO  PI-OPTION.                           
           MOVE -1                     TO  ACRTNO4L.                            
           PERFORM 4000-READ-CERT-FILE.                                         
                                                                                
           EJECT                                                                
       3900-READNEXT-CERT-FILE SECTION.                                         
           EXEC CICS HANDLE CONDITION                                           
               DUPKEY (9300-XCTL)                                               
               NOTFND (3980-NOTFND)                                             
               DSIDERR (3970-DSIDERR)                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS ENDBR                                                      
                DATASET (WS-ACCOUNT-MASTER-DSID)                                
           END-EXEC.                                                            
                                                                                
           EXEC CICS STARTBR                                                    
               DATASET   (PI-DSID)                                              
               RIDFLD    (PI-CERTIFICATE-KEY)                                   
               KEYLENGTH (PI-KEY-LENGTH)                                        
               GENERIC                                                          
               GTEQ                                                             
           END-EXEC.                                                            
                                                                                
       3900-READNEXT-CONTINUE.                                                  
           EXEC CICS READNEXT                                                   
               DATASET   (PI-DSID)                                              
               RIDFLD    (PI-CERTIFICATE-KEY)                                   
               SET       (ADDRESS OF PURGE-CERT-MASTER)                         
           END-EXEC.                                                            
                                                                                
           GO TO 3999-EXIT.                                                     
                                                                                
       3970-DSIDERR.                                                            
           MOVE ER-0671                TO  EMI-ERROR.                           
           MOVE -1                     TO  APFKL.                               
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
       3980-NOTFND.                                                             
           GO TO 3900-READNEXT-CONTINUE.                                        
                                                                                
       3999-EXIT. EXIT.                                                         
                                                                                
       4000-READ-CERT-FILE SECTION.                                             
           EXEC CICS HANDLE CONDITION                                           
               DUPKEY (9300-XCTL)                                               
               NOTFND (4080-NOTFND)                                             
               DSIDERR (4070-DSIDERR)                                           
           END-EXEC.                                                            
                                                                                
      **   IF  PART-KEY-ON                                                      
      *        MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                          
      *        PERFORM 3900-READNEXT-CERT-FILE THRU 3999-EXIT                   
      *        MOVE 'N'          TO  PART-KEY-ON-SW                             
      *        EXEC CICS ENDBR                                                  
      *           DATASET   (PI-DSID)                                           
      *        END-EXEC                                                         
      *        GO TO 4070-CONTINUE.                                             
                                                                                
           IF (PI-DSID = WS-CERT-MASTER-DSID AND                                
               PI-KEY-LENGTH LESS +33)                                          
             OR                                                                 
              (PI-DSID = WS-CERT-AIX01-DSID AND                                 
               PI-KEY-LENGTH LESS +18)                                          
             OR                                                                 
              (PI-DSID = WS-CERT-AIX02-DSID AND                                 
               PI-KEY-LENGTH LESS +12)                                          
             OR                                                                 
              (PI-DSID = WS-CERT-AIX04-DSID AND                                 
               PI-KEY-LENGTH LESS +12)                                          
             OR                                                                 
              (PI-DSID = WS-CERT-AIX05-DSID AND                                 
               PI-KEY-LENGTH LESS +13)                                          
                   MOVE +1             TO  PI-START-SW                          
                   EXEC CICS READ                                               
                       DATASET   (PI-DSID)                                      
                       RIDFLD    (PI-CERTIFICATE-KEY)                           
                       SET       (ADDRESS OF PURGE-CERT-MASTER)                 
                       GENERIC                                                  
                       EQUAL                                                    
                       KEYLENGTH (PI-KEY-LENGTH)                                
                   END-EXEC                                                     
               ELSE                                                             
                   MOVE ZERO           TO  PI-START-SW                          
                   EXEC CICS READ                                               
                       DATASET   (PI-DSID)                                      
                       RIDFLD    (PI-CERTIFICATE-KEY)                           
                       SET       (ADDRESS OF PURGE-CERT-MASTER)                 
                   END-EXEC.                                                    
                                                                                
       4070-CONTINUE.                                                           
                                                                                
           GO TO 9300-XCTL.                                                     
                                                                                
       4070-DSIDERR.                                                            
           MOVE ER-0671                TO  EMI-ERROR.                           
           MOVE -1                     TO  APFKL.                               
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
       4080-NOTFND.                                                             
      *    MOVE -1                     TO  ACRTNO4L.                            
           MOVE -1                     TO  APFKL.                               
           MOVE ER-0201                TO  EMI-ERROR.                           
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
       4090-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
                                                                                
       5500-WRITE-SECURITY-TEMP-STORE  SECTION.                                 
           EXEC CICS HANDLE CONDITION                                           
               QIDERR   (5501-WRITE-SECURITY)                                   
           END-EXEC.                                                            
                                                                                
           MOVE EIBTRMID               TO  QID.                                 
                                                                                
           EXEC CICS DELETEQ TS                                                 
               QUEUE   (QID)                                                    
           END-EXEC.                                                            
                                                                                
       5501-WRITE-SECURITY.                                                     
                                                                                
           EXEC CICS WRITEQ TS                                                  
               QUEUE   (QID)                                                    
               FROM    (SECURITY-CONTROL)                                       
               LENGTH  (SC-COMM-LENGTH)                                         
               ITEM    (QID-ITEM)                                               
           END-EXEC.                                                            
                                                                                
           MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.           
                                                                                
           IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                                
               MOVE ALL 'Y'            TO  SC-CREDIT-CODES                      
                                           SC-CLAIMS-CODES                      
                                           PI-PROCESSOR-USER-ALMIGHTY.          
                                                                                
       5500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       EJECT                                                                    
       6000-READ-CONTROL SECTION.                                               
           EXEC CICS HANDLE CONDITION                                           
                NOTFND (6000-NOT-FOUND)                                         
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
                DATASET  (WS-CONTROL-FILE-DSID)                                 
                RIDFLD   (WS-CNTL-KEY)                                          
                SET      (ADDRESS OF CONTROL-FILE)                              
           END-EXEC.                                                            
                                                                                
           CONTINUE.                                                            
           MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.                
           GO TO 6000-EXIT.                                                     
                                                                                
       6000-NOT-FOUND.                                                          
                                                                                
           MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.                
                                                                                
       6000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6010-READ-CONTROL-UPDATE.                                                
                                                                                
            EXEC CICS HANDLE CONDITION                                          
                NOTFND    (6010-NOTFND)                                         
            END-EXEC.                                                           
                                                                                
            EXEC CICS READ                                                      
                DATASET   (WS-CONTROL-FILE-DSID)                                
                RIDFLD    (WS-CNTL-KEY)                                         
                SET       (ADDRESS OF CONTROL-FILE)                             
                UPDATE                                                          
            END-EXEC.                                                           
                                                                                
            CONTINUE.                                                           
                                                                                
            MOVE 'Y'                       TO  WS-CNTL-REC-FOUND-SW.            
            GO TO 6010-EXIT.                                                    
                                                                                
       6010-NOTFND.                                                             
                                                                                
            MOVE 'N'                       TO  WS-CNTL-REC-FOUND-SW.            
                                                                                
       6010-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6020-REWRITE-CONTROL.                                                    
                                                                                
           EXEC CICS REWRITE                                                    
               DATASET   (WS-CONTROL-FILE-DSID)                                 
               FROM      (CONTROL-FILE)                                         
           END-EXEC.                                                            
                                                                                
       6020-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       7000-BUILD-SCREEN     SECTION.                                           
      ******************************************************************        
      *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1282    *        
      *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *        
      ******************************************************************        
           IF EIBTRNID = WS-TRANS-ID                                            
              NEXT SENTENCE                                                     
             ELSE                                                               
              GO TO 7099-EXIT.                                                  
                                                                                
      ***                                                                       
      ******** PI-BROWSE-SW = 9  MEANS NO RECORDS FOUND (SET IN EL1282)         
      ***                                                                       
           IF  PI-BROWSE-SW = +9                                                
               NEXT SENTENCE                                                    
              ELSE                                                              
               GO TO 7099-EXIT.                                                 
                                                                                
            IF OPTION-TWO-SELECTED                                              
               GO TO 7099-EXIT.                                                 
                                                                                
           MOVE LOW-VALUES             TO  EL128AO.                             
                                                                                
           MOVE ER-2373                TO  EMI-ERROR.                           
           PERFORM 9900-ERROR-FORMAT.                                           
                                                                                
           IF PI-ALT-NAME-COUNT GREATER 140                                     
               MOVE ER-0765            TO  EMI-ERROR.                           
                                                                                
       7010-OPTION-ONE.                                                         
            IF OPTION-ONE-SELECTED                                              
               NEXT SENTENCE                                                    
              ELSE                                                              
               GO TO 7030-OPTION-THREE.                                         
                                                                                
            MOVE -1                       TO  ACRTNO4L.                         
                                                                                
            IF  PI-SC-CERT-PRIME-A4 GREATER SPACES                              
                MOVE PI-SC-CERT-PRIME-A4  TO  ACRTNO4O                          
                MOVE AL-UANON             TO  ACRTNO4A.                         
                                                                                
            IF  PI-SC-CERT-SFX-A4   GREATER SPACES                              
                MOVE PI-SC-CERT-SFX-A4    TO  ACRTSX4O                          
                MOVE AL-UANON             TO  ACRTSX4A.                         
                                                                                
           GO TO   7090-INITIALIZE-WORK-AREAS.                                  
                                                                                
       7030-OPTION-THREE.                                                       
           IF OPTION-THREE-SELECTED                                             
               NEXT SENTENCE                                                    
             ELSE                                                               
              GO TO 7040-OPTION-FOUR.                                           
                                                                                
           MOVE -1                          TO  ALNAMEL.                        
                                                                                
           IF  PI-SC-LAST-NAME GREATER SPACES                                   
               MOVE PI-SC-LAST-NAME         TO  ALNAMEO                         
               MOVE AL-UANON                TO  ALNAMEA.                        
           IF  PI-SC-FIRST-NAME GREATER SPACES                                  
               MOVE PI-SC-FIRST-NAME        TO  AFNAMEO                         
               MOVE AL-UANON                TO  AFNAMEA.                        
           IF  PI-SC-INITIALS  GREATER SPACES                                   
               MOVE PI-SC-INITIALS          TO  WS-INITIALS                     
               IF  WS-INITIAL-MIDDLE GREATER SPACES                             
                   MOVE WS-INITIAL-MIDDLE   TO  AINITALO                        
                   MOVE AL-UANON            TO  AINITALA.                       
           IF  PI-SC-ACCT-NO   GREATER SPACES                                   
               MOVE PI-SC-ACCT-NO           TO  AACCT2O                         
               MOVE AL-UANON                TO  AACCT2A.                        
                                                                                
           GO TO   7090-INITIALIZE-WORK-AREAS.                                  
                                                                                
       7040-OPTION-FOUR.                                                        
           IF OPTION-FOUR-SELECTED                                              
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 7050-OPTION-FIVE.                                          
                                                                                
           MOVE -1                          TO  ASSNL.                          
                                                                                
           IF PI-SC-SOC-SEC-NO GREATER SPACES                                   
              MOVE PI-SC-SOC-SEC-NO         TO  ASSNO                           
              MOVE AL-UANON                 TO  ASSNA.                          
                                                                                
           GO TO   7090-INITIALIZE-WORK-AREAS.                                  
                                                                                
       7050-OPTION-FIVE.                                                        
           MOVE -1                     TO  AMEMBERL.                            
                                                                                
           IF PI-SC-MEMBER-NO GREATER SPACES                                    
              MOVE PI-SC-MEMBER-NO     TO  AMEMBERO                             
              MOVE AL-UANON            TO  AMEMBERA.                            
                                                                                
                                                                                
       7090-INITIALIZE-WORK-AREAS.                                              
           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA                 
                                           PI-CONTROL-IN-PROGRESS.              
                                                                                
           MOVE ZERO                   TO  PI-1ST-TIME-SW                       
                                           PI-LINE-COUNT                        
                                           PI-BROWSE-SW                         
                                           PI-KEY-LENGTH                        
                                           PI-TS-ITEM                           
                                           PI-END-OF-FILE                       
                                           PI-START-SW                          
                                           PI-AIX-RECORD-COUNT.                 
                                                                                
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
       7099-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
       8100-SEND-INITIAL-MAP SECTION.                                           
                                                                                
           MOVE PI-COMPANY-ID          TO  WS-CNTL-ID.                          
           MOVE '1'                    TO  WS-CNTL-TYPE.                        
           MOVE SPACES                 TO  WS-CNTL-USER.                        
           MOVE +0                     TO  WS-CNTL-SEQ.                         
                                                                                
           PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                            
                                                                                
           IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                              
               MOVE ER-0022            TO  EMI-ERROR                            
               MOVE -1                 TO  APFKL                                
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           IF SOC-SEC-NO-USED                                                   
              MOVE AL-SANOF            TO  AOPT4A ASSOPTA                       
              MOVE AL-UANOF            TO  ASSNA                                
              MOVE 'SOCIAL SECURITY NUMBER ' TO  ASSOPTO                        
              MOVE '** OPTION 4 **'    TO  AOPT4O                               
           ELSE                                                                 
              MOVE AL-SANOF            TO  AOPT4A ASSOPTA ASSNA.                
                                                                                
           IF MEMBER-NO-USED                                                    
              MOVE AL-SANOF            TO  AOPT5A AMEOPTA                       
              MOVE AL-UANOF            TO  AMEMBERA                             
              MOVE '** OPTION 5 **'    TO  AOPT5O                               
              IF CF-MEMBER-CAPTION = SPACES                                     
                 MOVE 'MEMBER NUMBER ' TO  AMEOPTO                              
              ELSE                                                              
                 MOVE CF-MEMBER-CAPTION TO AMEOPTO                              
           ELSE                                                                 
              MOVE AL-SANOF            TO  AOPT5A AMEOPTA AMEMBERA.             
                                                                                
           MOVE -1                     TO  ACRTNO4L                             
                                                                                
           MOVE SAVE-DATE              TO ADATEO                                
           MOVE EIBTIME                TO TIME-IN                               
           MOVE TIME-OUT               TO ATIMEO                                
                                                                                
           IF EMI-ERROR NOT = ZERO                                              
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O                               
           MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O                               
                                                                                
           MOVE PI-COMPANY-ID          TO ACOMPO                                
                                                                                
           EXEC CICS SEND                                                       
               FROM   (EL128AO)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
               ERASE                                                            
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8100-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8200-SEND-DATAONLY SECTION.                                              
           MOVE SAVE-DATE              TO  ADATEO.                              
           MOVE EIBTIME                TO  TIME-IN.                             
           MOVE TIME-OUT               TO  ATIMEO.                              
                                                                                
           IF EMI-ERROR NOT = ZERO                                              
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                            
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                            
                                                                                
           MOVE PI-COMPANY-ID          TO  ACOMPO                               
                                                                                
           EXEC CICS SEND DATAONLY                                              
               FROM   (EL128AO)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8200-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8300-SEND-TEXT SECTION.                                                  
           EXEC CICS SEND TEXT                                                  
               FROM   (LOGOFF-TEXT)                                             
               LENGTH (LOGOFF-LENGTH)                                           
               ERASE                                                            
               FREEKB                                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
                                                                                
       8300-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8500-DATE-CONVERSION SECTION.                                            
           EXEC CICS LINK                                                       
               PROGRAM  ('ELDATCV')                                             
               COMMAREA (DATE-CONVERSION-DATA)                                  
               LENGTH   (DC-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       8500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8600-DEEDIT SECTION.                                                     
           EXEC CICS BIF DEEDIT                                                 
               FIELD  (WS-DEEDIT-FIELD)                                         
               LENGTH (15)                                                      
           END-EXEC.                                                            
                                                                                
       8600-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9000-RETURN-CICS SECTION.                                                
           MOVE 'EL005'                TO  THIS-PGM.                            
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.                       
           GO TO 9300-XCTL.                                                     
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9100-RETURN-TRAN SECTION.                                                
           MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.                     
           MOVE WS-MAP-NUMBER         TO  PI-CURRENT-SCREEN-NO.                 
                                                                                
           EXEC CICS RETURN                                                     
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
               TRANSID  (WS-TRANS-ID)                                           
           END-EXEC.                                                            
                                                                                
       9100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9300-XCTL SECTION.                                                       
           MOVE DFHENTER               TO  EIBAID.                              
           MOVE PART-KEY-ON-SW         TO  PI-PART-KEY-SW.                      
           MOVE PART-FIELD-ON-SW       TO  PI-PART-FIELD-SW.                    
           MOVE ' '                    TO  PART-FIELD-ON-SW.                    
                                                                                
           EXEC CICS XCTL                                                       
               PROGRAM  (THIS-PGM)                                              
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       9300-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9400-CLEAR SECTION.                                                      
           MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                             
           GO TO 9300-XCTL.                                                     
                                                                                
       9400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9600-PGMIDERR SECTION.                                                   
           EXEC CICS HANDLE CONDITION                                           
               PGMIDERR (8300-SEND-TEXT)                                        
           END-EXEC.                                                            
                                                                                
           MOVE THIS-PGM               TO  PI-CALLING-PROGRAM                   
                                           LOGOFF-PGM.                          
                                                                                
           MOVE 'EL005'                TO  THIS-PGM.                            
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                         
           MOVE SPACES                 TO  PI-ENTRY-CD-1.                       
           GO TO 9300-XCTL.                                                     
                                                                                
       9600-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9900-ERROR-FORMAT SECTION.                                               
           EXEC CICS LINK                                                       
               PROGRAM  ('EL001')                                               
               COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                         
               LENGTH   (EMI-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
       9900-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9990-ERROR SECTION.                                                      
           MOVE DFHEIBLK               TO EMI-LINE1.                            
           EXEC CICS LINK                                                       
               PROGRAM  ('EL004')                                               
               COMMAREA (EMI-LINE1)                                             
               LENGTH   (72)                                                    
           END-EXEC.                                                            
                                                                                
           GO TO 8200-SEND-DATAONLY.                                            
                                                                                
       9990-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9995-SECURITY-VIOLATION.                                                 
                                   COPY ELCSCTP.                                
                                                                                
       9995-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9999-LAST-PARAGRAPH SECTION.                                             
           GOBACK.                                                              
