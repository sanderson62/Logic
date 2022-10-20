       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID. SOCK10.
      *                            VMOD=1.001.
      *                                                                 
      *AUTHOR.     LOGIC INC.                                           
      *            DALLAS, TEXAS.                                       
      *REMARKS.                                                         
      *         THIS PROGRAM IS STARTED VIA A SOCKET CALL FROM APP.
      *         Designed to receive key, read thru claim stuff,        
      *         create a line sequential report. send back the          
      *         file name to the app thru socket.
                                                                        
      *       INPUT FILES-   ELMSTR-   CLAIM MASTER                     
      *                      ELTRLR-   ACTIVITY TRAILERS                
      *                      ELCNTL-   CONTROL FILE                     
      *                      ELCERT-   CERTIFICATE MASTER               
      *                      ERACCT-   ACCOUNT MASTER                   
      *                      ELBENE-   BENEFICIARY MASTER               
                                                                        
      *       OUTPUT-                  CLAIM STATUS REPORT              
      *                                (OPEN/CLOSE HISTORY)             
                                                                        
      *       COMMARERA-     RECEIVED FROM THE CALLING SOCKET
      *                      AND ENTRY CODE 1 FOR SHORT FORM AND        
      *                      2  FOR THE LONG FORM--                     
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 042018  CR2017072700004  PEMA  New program from (EL008)
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
       SELECT CLM-STAT-EXT-OUT ASSIGN TO dynamic ws-file-name
          FILE STATUS IS ws-ext-file-status
          organization is line sequential.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  CLM-STAT-EXT-OUT
           BLOCK CONTAINS 0
           RECORDING MODE F.
      
       01  CLM-STAT-EXT-OUT-RECORD     PIC X(132).
      
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '    SOCK10  WORKING-STORAGE     '.  
       77  FILLER  PIC X(32) VALUE '********* VMOD=1.001 ***********'.  
                                                                        
       77  ws-send-msg-size            pic s9(8) comp value 48000.
       77  ws-recv-msg-size            pic s9(8) comp value 19200.
       77  ws-recv-buf                 pic x(4096).
       77  ws-send-buf                 pic x(48000) VALUE SPACES.
       77  ws-recv-total               pic s9(8) comp value 0.
       77  ws-recv-left                pic s9(8) comp value 0.
       77  ws-seq-num                  pic s9(8) comp value 0.
       77  ws-flags                    pic s9(8) comp value 0.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  THIS-PGM               PIC X(5) VALUE 'EL008'.               
       77  ws-line-cntr                pic 999  value 070.
                                                                        
       77  WS-ELMSTR-EOF-SW       PIC X    VALUE SPACE.                 
           88  END-OF-MSTR-FILE            VALUE 'E'.                   
                                                                        
       77  WS-ELACTQ-EOF-SW       PIC X    VALUE SPACE.                 
           88  END-OF-ACTQ-FILE            VALUE 'E'.                   
                                                                        
       77  WS-ELTRLR-EOF-SW       PIC X    VALUE SPACE.                 
           88  END-OF-TRLR-FILE            VALUE 'E'.                   
                                                                        
       77  WS-FULL-PRINT-SW       PIC X    VALUE SPACE.                 
           88  FULL-PRINT-REQUIRED         VALUE 'X'.                   
                                                                        
       77  WS-TRAILER-KEY-CHG-SW  PIC X    VALUE SPACE.                 
           88  TRAILER-KEY-CHANGED         VALUE 'X'.                   
                                                                        
       77  END-OC-TABLE-SW        PIC X    VALUE SPACE.                 
           88  END-OC-TABLE                VALUE 'E'.                   
           88  OC-TABLE-LOADED             VALUE 'X'.                   
                                                                        
       77  END-AD-TABLE-SW        PIC X    VALUE SPACE.                 
           88  END-AD-TABLE                VALUE 'E'.                   
           88  AD-TABLE-LOADED             VALUE 'X'.                   
                                                                        
       77  END-AP-TABLE-SW        PIC X    VALUE SPACE.                 
           88  END-AP-TABLE                VALUE 'E'.                   
           88  AP-TABLE-LOADED             VALUE 'X'.                   
                                                                        
       77  WS-CNTL-ERROR-SW      PIC X    VALUE SPACE.                  
           88  NO-COMPANY-RECORD         VALUE 'X'.                     
                                                                        
       77  WS-MSTR-READ-ERROR-SW PIC X    VALUE SPACE.                  
           88  MSTR-READ-ERROR           VALUE 'X'.                     
                                                                        
       77  WS-CERT-READ-ERROR-SW PIC X    VALUE SPACE.                  
           88  CERT-READ-ERROR           VALUE 'X'.                     

       77  WS-TRLR-BROWSE-ERROR-SW PIC X    VALUE SPACE.                
           88  TRLR-BROWSE-ERROR           VALUE 'X'.                   
                                                                        
       77  WS-ACCT-BROWSE-SW       PIC X    VALUE SPACE.                
           88  ACCT-BROWSE-OKAY            VALUE 'X'.                   
                                                                        
       77  WS-BENE-BROWSE-SW       PIC X    VALUE SPACE.                
           88  BENE-BROWSE-OKAY            VALUE 'X'.                   

       77  WS-LN-SW               PIC X    VALUE SPACE.                 
           88  LAST-NAME-LOADED            VALUE 'X'.                   
                                                                        
       77  WS-FN-SW               PIC X    VALUE SPACE.                 
           88  FIRST-NAME-LOADED           VALUE 'X'.                   
                                                                        
       77  WS-CANC-DT             PIC XX   VALUE LOW-VALUES.            
       77  WS-STATUS              PIC X    VALUE SPACES.                
                                                                        
       77  OC-SUB                 PIC  S9(4)  VALUE ZEROS COMP.         
                                                                        
       77  SUB1                   PIC S9(4)   VALUE ZEROS COMP.         
      
       77  WS-SUB                 PIC 9       VALUE 0.
      
       77  A1                     PIC S999 COMP-3 VALUE +0.
                                                                        
       77  WS-RECORD-TYPE         PIC X     VALUE SPACE.                
                                                                        
       77  WS-SAVED-FORM-NO       PIC X(12) VALUE SPACE.                

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
       01  ws-ext-file-status          pic xx value '00'.
       01  ws-file-name                pic x(60) value spaces.


       01  ws-rm-return-cd         pic s9(8) comp-5 value +0.

       01  ws-rm-string.
           05  f                       pic x(6) value
            'rm -f '.
           05  rm-filename             pic x(54) value spaces.
           05  f                       pic x value low-values.

       01  ws-return-stuff             pic x(100).

       01  ws-work-dynamic.
           05  ws-work-dir             pic x(31) value spaces.
           05  ws-work-env             pic x(8)  value spaces.
           05  ws-work-comp-id         pic xxx   value spaces.
           05  ws-work-rpt-out         pic x(30) value spaces.
           05  ws-work-job-name        pic x(8)  value spaces.
      
      
       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
                                                                        
           05  WS-PHONE-BRKDN              PIC 9(11).                   
           05  FILLER   REDEFINES WS-PHONE-BRKDN.                       
               10  FILLER                  PIC 9.                       
               10  WS-PH-1                 PIC 999.                     
               10  WS-PH-2                 PIC 999.                     
               10  WS-PH-3                 PIC 9999.                    
                                                                        
           05  WS-PHONE-EDIT.                                           
               10  WS-PH-ED-1              PIC XXX.                     
               10  FILLER                  PIC X   VALUE '-'.           
               10  WS-PH-ED-2              PIC XXX.                     
               10  FILLER                  PIC X   VALUE '-'.           
               10  WS-PH-ED-3              PIC XXXX.                    
                                                                        
           05  WS-ZIP-WORK.                                             
               10  WS-ZIP-PRIME        PIC X(5).                        
               10  WS-ZIP-DASH         PIC X.                           
               10  WS-ZIP-PLUS4        PIC X(4).                        
           05  WS-CANADIAN-ZIP-WORK  REDEFINES  WS-ZIP-WORK.            
               10  WS-CAN-POSTAL-1     PIC XXX.                         
               10  FILLER              PIC X.                           
               10  WS-CAN-POSTAL-2     PIC XXX.                         
               10  FILLER              PIC XXX.                         
           05  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.          
           05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 
                                       PIC 9(10).                       
           05  SSP                         PIC X  VALUE ' '.            
           05  DSP                         PIC X  VALUE '0'.            
           05  TSP                         PIC X  VALUE '-'.            
           05  TPG                         PIC X  VALUE '1'.            
                                                                        
           05  WS-TERMINAL-ID.                                          
               10  WS-TERM-PREFIX      PIC XX.                          
               10  FILLER              PIC XX.                          
                                                                        
           05  WS-LAST-NAME    PIC X(15).                               
           05  FILLER REDEFINES WS-LAST-NAME.                           
               10  WS-L-BYTE   PIC X  OCCURS 15                         
                               INDEXED BY L-IND.                        
           05  WS-FIRST-NAME   PIC X(12).                               
           05  FILLER REDEFINES WS-FIRST-NAME.                          
               10  WS-F-BYTE   PIC X  OCCURS 12                         
                               INDEXED BY F-IND.                        
           05  PREVIOUS-BYTE          PIC X  VALUE SPACES.              
                                                                        
           05  WS-AGE             PIC 9(04)  VALUE ZEROS.               
           05  WS-AGE-R REDEFINES WS-AGE.                               
               10  WS-AGE-1-2     PIC 9(02).                            
               10  WS-AGE-3-4     PIC 9(02).                            
                                                                        
           05  WS-ACCESS.                                               
               10  FILLER         PIC XX  VALUE SPACES.                 
               10  WS-BEN-CODE    PIC XX.                               
                                                                        
           05  WS-AT-CONTROL-PRIMARY.                                   
               10  WS-AT-CONTROL-WO-SEQ.                                
                   15  WS-AT-COMPANY-CD   PIC X.                        
                   15  WS-AT-CARRIER      PIC X.                        
                   15  WS-AT-CLAIM-NO     PIC X(7).                     
                   15  WS-AT-CERT-NO      PIC X(11).                    
               10  WS-AT-SEQ-NO           PIC S9(4) COMP.               
                                                                        
           05  WS-NEW-AT-CONTROL-PRIMARY.                               
               10  WS-NEW-AT-CONTROL-WO-SEQ  PIC X(20).                 
               10  FILLER                    PIC S9(4) COMP.            
                                                                        
           05  WS-AT-CONTROL-SAVED.                                     
               10  WS-AT-CONTROL-SAVED-WO-SEQ  PIC X(20).               
               10  FILLER                    PIC S9(4) COMP.            
                                                                        
           05  WS-CF-CONTROL-PRIMARY.                                   
               10  WS-CF-COMPANY-ID   PIC XXX.                          
               10  WS-CF-RECORD-TYPE  PIC X.                            
               10  WS-CF-PROCESSOR    PIC X(4).                         
               10  WS-CF-SEQUENCE-NO  PIC S9(4) COMP.                   
                                                                        
           05  WS-CL-CONTROL-PRIMARY.                                   
               10  WS-CL-COMPANY-CD   PIC X.                            
               10  WS-CL-CARRIER      PIC X.                            
               10  WS-CL-CLAIM-NO     PIC X(7).                         
               10  WS-CL-CERT-NO      PIC X(11).                        

           05  WS-CM-CONTROL-PRIMARY.                                   
               10  WS-CM-COMPANY-CD   PIC X.                            
               10  WS-CM-CERT-DATA    PIC X(21).                        
               10  WS-CM-CERT-NO      PIC X(11).                        
                                                                        
           05  WS-AM-CONTROL-PRIMARY.                                   
               10  WS-AM-COMPANY-CD    PIC X(1).                        
               10  WS-AM-CARRIER       PIC X(1).                        
               10  WS-AM-GROUPING      PIC X(6).                        
               10  WS-AM-STATE         PIC X(2).                        
               10  WS-AM-ACCOUNT       PIC X(10).                       
               10  WS-AM-EXPIRATION-DT PIC X(2).                        
                                                                        
           05  WS-BE-CONTROL-PRIMARY.                                   
               10  WS-BE-COMPANY-CD    PIC X(01).                       
               10  WS-BE-RECORD-TYPE   PIC X(01).                       
               10  WS-BE-BENEFICIARY   PIC X(10).                       
                                                                        
      *---- THIS IS USED SO THAT EL008 WILL COMPILE, THE                
      *---- PI-PRINTER-ID IS USED BY THE CHECK WRITER EL176 AND         
      *---- EL177                                                       
           05  WS-CHECK-ID.                                             
               10  PI-PRINTER-ID PIC X(04).                             

       01  PRINT-CONTROL-WORK-AREA.                                     
           05  WS-MM-DD-YY.                                             
               10  WS-MM     PIC 99.                                    
               10  WS-DD     PIC 99.                                    
               10  WS-YY     PIC 99.                                    
                                                                        
           05  WS-TERM-DATE.                                            
               10  WS-TERM-MM   PIC 999  VALUE ZEROS.                   
               10  WS-TERM-YY   PIC 999  VALUE ZEROS.                   
                                                                        
           05  WS-REM-DATE.                                             
               10  WS-REM-MM    PIC S999  VALUE ZEROS.                  
                                                                        
           05  WS-PAGE-CNT      PIC 9(4)   VALUE 1.                     
                                                                        
           05  WS-BIN-CURRENT-DT    PIC XX.                             
                                                                        
       01  HEAD-LINE-2.                                                 
           05  FILLER             PIC X(31) VALUE SPACE.                
           05  FILLER             PIC X(16) VALUE                       
                                  '- CLAIM STATUS -'.                   
           05  FILLER             PIC X(19) VALUE SPACES.               
           05  HEAD-REPORT-NO     PIC X(10) VALUE SPACES.               
                                                                        
       01  HEAD-LINE-3.                                                 
           05  FILLER             PIC X(25) VALUE SPACE.                
           05  HEAD-COMPANY       PIC X(30) VALUE SPACES.               
           05  FILLER             PIC X(12) VALUE SPACES.               
           05  HEAD-RUN-DATE      PIC X(8).                             
                                                                        
       01  HEAD-LINE-4.                                                 
           05  FILLER             PIC X(31) VALUE SPACE.                
           05  HEAD-RUN-DATE-FULL PIC X(18).                            
           05  FILLER             PIC X(18) VALUE SPACES.               
           05  FILLER             PIC X(4) VALUE 'PAGE'.                
           05  HEAD-PAGE-NO       PIC ZZZ9.                             
                                                                        
       01  HEAD-LINE-7.                                                 
           05  FILLER             PIC X(40) VALUE                       
              'CLAIM NO CARR  CERT NO    TYPE  STATUS'.                 
           05  FILLER             PIC X(40) VALUE                       
              'NAME OF INSURED                PROCESSOR'.               
                                                                        
       01  LINE-9.                                                      
           05  FILLER             PIC X     VALUE SPACES.               
           05  P-CLAIM-NO         PIC X(7).                             
           05  FILLER             PIC X(3)  VALUE SPACES.               
           05  P-CARR             PIC X.                                
           05  FILLER             PIC X(2)  VALUE SPACES.               
           05  P-CERT-NO          PIC X(11).                            
           05  FILLER             PIC XX    VALUE SPACES.               
           05  P-TYPE             PIC XXXX.                             
           05  FILLER             PIC XX    VALUE  SPACES.              
           05  P-STATUS           PIC X(6).                             
           05  FILLER             PIC X(2)  VALUE   SPACES.             
           05  P-NAME-GRP         PIC X(31).                            
           05  P-NAME-ARR REDEFINES P-NAME-GRP                          
                                  OCCURS 31  INDEXED BY P-IND.          
               10  P-NAME         PIC X.                                
           05  FILLER             PIC X(3) VALUE  SPACES.               
           05  P-PROCESSOR        PIC X(4).                             
                                                                        
       01  HEAD-LINE-10.                                                
           05  FILLER             PIC X(20) VALUE                       
              'CREDIT CARD NUMBER:'.                                    
           05  HEAD-CREDIT-CARD   PIC X(16).                            
                                                                        
       01  LINE-12.                                                     
           05  L-12-PD-THRU       PIC X(13) VALUE                       
               'PAID THRU  - '.                                         
           05  P-PAID-THRU-DT     PIC X(8).                             
           05  FILLER             PIC X(5)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'LAST PMT AMT - '.                                       
           05  P-LAST-PMT-AMT     PIC Z(7).99.                          
           05  FILLER             PIC X(3)  VALUE  SPACES.              
           05  FILLER             PIC X(16) VALUE                       
               'LAST PMT DATE - '.                                      
           05  P-LAST-PMT-DT      PIC X(8).                             
                                                                        
       01  LINE-13.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'INCURRED   - '.                                         
           05  P-INCURRED-DT      PIC X(8).                             
           05  FILLER             PIC X(5) VALUE  SPACES.               
           05  FILLER             PIC X(15) VALUE                       
               'REPORTED     - '.                                       
           05  P-REPORTED-DT      PIC X(8).                             
           05  FILLER             PIC X(5) VALUE  SPACES.               
           05  FILLER             PIC X(16) VALUE                       
               'ESTABLISHED   - '.                                      
           05  P-ESTAB-DT         PIC X(8).                             
                                                                        
       01  LINE-14.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'NEXT AUTO  - '.                                         
           05  P-NEXT-AUTO-DT     PIC X(8).                             
           05  FILLER             PIC X(5) VALUE  SPACES.               
           05  FILLER             PIC X(15) VALUE                       
               'PMTS MADE    - '.                                       
           05  P-PMTS-MADE        PIC XXX.                              
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  FILLER             PIC X(16) VALUE                       
               'PREMIUM TYPE  - '.                                      
           05  P-PREM-TYPE        PIC X(7).                             
                                                                        
       01  LINE-15.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'COVERAGE   - '.                                         
           05  P-COVERAGE         PIC X(10).                            
           05  FILLER             PIC X(3) VALUE   SPACES.              
           05  L15-ISSUE-LIT      PIC X(15) VALUE                       
               'CERT ISSUE   - '.                                       
           05  P-CERT-ISSUE-DT    PIC X(8).                             
           05  FILLER             PIC X(5) VALUE  SPACES.               
           05  FILLER             PIC X(15) VALUE                       
               'ORIG BENEFIT  -'.                                       
           05  P-ORIG-BENEF-AMT   PIC Z(8).99.                          
                                                                        
       01  LINE-16.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'EXPIRE DATE- '.                                         
           05  P-EXPIRE           PIC X(12).                            
           05  FILLER             PIC X(1)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'TERM/REMAIN  - '.                                       
           05  P-TERM             PIC ZZ9.                              
           05  FILLER             PIC X     VALUE '/'.                  
           05  P-REM              PIC ZZ9.                              
           05  FILLER             PIC X(6)  VALUE  SPACES.              
           05  L16-STATUS-LIT     PIC X(16) VALUE                       
               'CERT STATUS   - '.                                      
           05  P-CERT-STAT        PIC X(8).                             
                                                                        
       01  LINE-17.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'TOTAL PAID - '.                                         
           05  P-TOT-PAID         PIC Z(8).99.                          
           05  FILLER             PIC X(2)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'CAUSE/DIAG   - '.                                       
           05  P-CAUSE-DIAG       PIC X(26).                            
                                                                        
       01  LINE-19.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'TOT EXPENSE- '.                                         
           05  P-TOT-EXPENSE      PIC ZZZZZZZ.99.                       
           05  FILLER             PIC X(3)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'ORIG MANUAL  - '.                                       
           05  P-ORIG-MANUAL      PIC ZZZZZZ.99.                        
           05  FILLER             PIC X(4)  VALUE  SPACES.              
           05  L19-ACCT-LIT       PIC X(16) VALUE                       
               'ACCOUNT       - '.                                      
           05  P-ACCT             PIC X(10).                            
                                                                        
       01  LINE-20.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'CHG EXPENSE- '.                                         
           05  P-CHG-EXPENSE      PIC ZZZZZZ.99.                        
           05  FILLER             PIC X(4)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'REM MANUAL   - '.                                       
           05  P-REM-MANUAL       PIC ZZZZZZ.99.                        
           05  FILLER             PIC X(4)  VALUE  SPACES.              
           05  FILLER             PIC X(16) VALUE                       
               'STATE         - '.                                      
           05  P-STATE            PIC X(8).                             
                                                                        
       01  LINE-21.                                                     
           05  L21-CANC-LIT       PIC X(13) VALUE                       
               'CERT CANCEL- '.                                         
           05  P-CERT-CANC-DT     PIC X(8).                             
           05  FILLER             PIC X(5)  VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'ADDL RESERVE - '.                                       
           05  P-ADD-RESERVE      PIC ZZZZZZ.99.                        
           05  FILLER             PIC X(4)  VALUE  SPACES.              
           05  FILLER             PIC X(16) VALUE                       
               'GROUPING      - '.                                      
           05  P-GROUP            PIC X(8).                             
                                                                        
       01  LINE-22.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'REIN CODE  - '.                                         
           05  P-REIN-CODE        PIC X(3).                             
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE                       
               'CERT BATCH   - '.                                       
           05  P-CERT-BATCH       PIC X(6).                             
           05  FILLER             PIC X(7)  VALUE  SPACES.              
           05  L22-ENTRY-LIT      PIC X(16) VALUE                       
               'CERT ENTRY    - '.                                      
           05  P-CERT-ENTRY.                                            
               10  P-CERT-ENTRY-MM  PIC XX.                             
               10  P-CERT-SL        PIC X   VALUE '/'.                  
               10  P-CERT-ENTRY-YY  PIC XX.                             
                                                                        
       01  LINE-23.                                                     
           05  FILLER             PIC X(13) VALUE                       
               'MEMBER NO. - '.                                         
           05  P-MEMBER-NO        PIC X(12).                            
           05  FILLER             PIC X(16) VALUE                       
               ' INSURED AGE  - '.                                      
           05  P-INSURED-AGE      PIC XX.                               
           05  FILLER             PIC X(11) VALUE SPACES.               
           05  FILLER             PIC X(16) VALUE                       
               'INSURED SEX   - '.                                      
           05  P-INSURED-SEX      PIC XX.                               
                                                                        
       01  LINE-23A.                                                    
           05  P-USER-HD          PIC X(13) VALUE                       
               'USER CODE  - '.                                         
           05  P-USER-CODE        PIC X.                                
           05  FILLER             PIC X(12) VALUE SPACES.               
           05  P-PURGED-STMT      PIC X(15) VALUE 'PURGED DATE  - '.    
           05  P-PURGED-DATE      PIC X(08) VALUE SPACES.               
                                                                        
       01  LINE-25.                                                     
           05  FILLER             PIC X(41) VALUE                       
               '- - CHRONOLOGICAL LISTING OF ACTIVITY - -'.             
                                                                        
       01  LINE-26.                                                     
           05  FILLER             PIC X(20) VALUE                       
               'TYPE    RECORDED  BY'.                                  
      ****************                                                  
       01  PAYMENT-DESCRIPTION-TABLE.
           12  FILLER              PIC X(11)   VALUE 'PARTIAL PMT'.
           12  FILLER              PIC X(11)   VALUE 'FINAL PMT  '.
           12  FILLER              PIC X(11)   VALUE 'LUMP SM PMT'.
           12  FILLER              PIC X(11)   VALUE 'ADDITNL PMT'.
           12  FILLER              PIC X(11)   VALUE 'CHGABLE EXP'.
           12  FILLER              PIC X(11)   VALUE 'NON-CHG EXP'.
           12  FILLER              PIC X(11)   VALUE 'LF PRM RFND'.
           12  FILLER              PIC X(11)   VALUE 'AH PRM RFND'.
           12  FILLER              PIC X(11)   VALUE 'ENTRY CORR '.
       01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
           12  PAY-DESC            PIC X(11)   OCCURS 9.
      
       01  P-PAY-LINE-1.                                                
           05  P-PAY-ACT-TYPE     PIC X(11) VALUE SPACES.
      *        'PAYMENT '.                                              
           05  FILLER             PIC X(1)  VALUE SPACE.
           05  P-PAY-PMT-DT       PIC X(8).                             
           05  FILLER             PIC X     VALUE SPACES.               
           05  P-PAY-BY           PIC X(4).                             
           05  FILLER             PIC X(10) VALUE                       
               '  PAYEE - '.                                            
           05  P-PAY-PAYEE        PIC X(30).                            
           05  FILLER             PIC X(13) VALUE                       
               '  PAYEE CD - '.                                         
           05  P-PAY-PAYEE-CD     PIC X(02).                            
                                                                        
       01  P-PAY-LINE-2.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(9)  VALUE                       
               'AMOUNT - '.                                             
           05  P-PAY-PMT-AMT      PIC Z(7).99.                          
           05  FILLER             PIC X(10) VALUE                       
               '  CHECK - '.                                            
           05  P-PAY-CHECK        PIC X(7).                             
           05  FILLER             PIC X(12) VALUE                       
               '  WRITTEN - '.                                          
           05  P-PAY-WRIT-DT      PIC X(8).                             
                                                                        
       01  P-PAY-LINE-3.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(9)  VALUE                       
               'TYPE   - '.                                             
           05  P-PAY-TYPE         PIC X(22).                            
           05  FILLER             PIC X(11) VALUE                       
               '  ORIGIN - '.                                           
           05  P-PAY-ORIGIN       PIC X(7).                             
                                                                        
       01  P-PAY-LINE-4.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(9)  VALUE                       
               'RESERVE- '.                                             
           05  P-PAY-RESERVE      PIC Z(6).99.                          
           05  FILLER             PIC X(12) VALUE                       
               '  EXPENSE - '.                                          
           05  P-PAY-EXPEN        PIC Z(5).99.                          
           05  FILLER             PIC X(11) VALUE                       
               '  CREDIT - '.                                           
           05  P-PAY-CREDIT-DT    PIC X(8).                             
                                                                        
       01  P-PAY-LINE-5A.                                               
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(7)  VALUE                       
               'VOID - '.                                               
           05  P-PAY-VOID-DT      PIC X(8).                             
           05  FILLER             PIC X(11) VALUE                       
               '  REASON - '.                                           
           05  P-PAY-REASON       PIC X(30).                            
                                                                        
       01  P-PAY-LINE-5B.                                               
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(7)  VALUE                       
               'FROM - '.                                               
           05  P-PAY-FROM-DT      PIC X(8).                             
           05  L-5B-PD-THRU       PIC X(8)  VALUE                       
               ' THRU - '.                                              
           05  P-PAY-THRU-DT      PIC X(8).                             
           05  FILLER             PIC X(8)  VALUE                       
               '  DAYS -'.                                              
           05  P-PAY-DAYS         PIC ZZZ9.                             
           05  FILLER             PIC X(8)  VALUE                       
               '  RATE -'.                                              
           05  P-PAY-RATE         PIC ZZ9.99.                           
                                                                        
       01  P-PAY-LINE-5C.                                               
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(15) VALUE 'EXPENSE TYPE - '.    
           05  P-EXP-TYPE         PIC X.                                
           05  FILLER             PIC X(41) VALUE SPACES.               
                                                                        
       01  P-LET-LINE-1.                                                
           05  P-LET-ACT-TYPE     PIC X(11) VALUE SPACES.
      *        'LETTER  '.                                              
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-LET-LET-DT       PIC X(8).                             
           05  FILLER             PIC X     VALUE SPACE.                
           05  P-LET-BY           PIC X(4).                             
           05  FILLER             PIC X(14) VALUE                       
               '  ADDRESSEE - '.                                        
           05  P-LET-ADSEE        PIC X(30).                            
           05  FILLER             PIC X(8) VALUE                        
               ' CODE - '.                                              
           05  P-LET-ADSEE-CD     PIC X(3).                             
                                                                        
       01  P-LET-LINE-2.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(7)  VALUE                       
               'FORM - '.                                               
           05  P-LET-FORM         PIC X(4).                             
           05  FILLER             PIC X(9)  VALUE                       
               '  SENT - '.                                             
           05  P-LET-SENT-DT      PIC X(8).                             
           05  FILLER             PIC X(12) VALUE                       
               '  RE-SEND - '.                                          
           05  P-LET-SEND-DT      PIC X(8).                             
                                                                        
       01  P-LET-LINE-3.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(12) VALUE                       
               'FOLLOW UP - '.                                          
           05  P-LET-FOL-DT       PIC X(8).                             
           05  FILLER             PIC X(13) VALUE                       
               '  ANSWERED - '.                                         
           05  P-LET-ANS-DT       PIC X(8).                             
                                                                        
       01  P-LET-LINE-4.                                                
           05  FILLER             PIC X(23) VALUE  SPACES.              
           05  FILLER             PIC X(9)  VALUE                       
               'ORIGIN - '.                                             
           05  P-LET-ORIGIN       PIC X(7).                             
           05  FILLER             PIC X(12) VALUE                       
               '  ARCHIVE - '.                                          
           05  P-LET-ARCH         PIC 9(6).                             
                                                                        
       01  P-LET-LINE-5.                                                
           05  FILLER             PIC X(10) VALUE                       
               '     RE - '.                                            
           05  P-LET-RE           PIC X(70).                            
                                                                        
       01  P-NOT-LINE-1.                                                
           05  P-NOT-ACT-TYPE     PIC X(11) VALUE  SPACES.
      *        'NOTES   '.                                              
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-NOT-NOTE-DT      PIC X(8).                             
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-NOT-BY           PIC X(4).                             
                                                                        
       01  P-NOT-LINE-2.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-NOT-TEXT-1       PIC X(70).                            
                                                                        
       01  P-NOT-LINE-3.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-NOT-TEXT-2       PIC X(70).                            
                                                                        
       01  P-PRO-LINE-1.                                                
           05  FILLER             PIC X(11) VALUE                       
               'REMINDER   '.                                           
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-PRO-NOTE-DT      PIC X(8).                             
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-PRO-BY           PIC X(4).                             
           05  FILLER             PIC X(17) VALUE                       
               '  START NOTIFY - '.                                     
           05  P-PRO-START-DT     PIC X(8).                             
           05  FILLER             PIC X(15) VALUE                       
               '  END NOTIFY - '.                                       
           05  P-PRO-END-DT       PIC X(8).                             
                                                                        
       01  P-PRO-LINE-2.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-PRO-TEXT-1       PIC X(70).                            
                                                                        
       01  P-PRO-LINE-3.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-PRO-TEXT-2       PIC X(70).                            
                                                                        
       01  P-DEN-LINE-1.                                                
           05  FILLER             PIC X(11) VALUE                       
               'DENIAL     '.                                           
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-DEN-DEN-DT       PIC X(8).                             
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-DEN-BY           PIC X(4).                             
           05  FILLER             PIC X(17) VALUE                       
               '  RECONSIDERED - '.                                     
           05  P-DEN-RECON-DT     PIC X(8).                             
           05  FILLER             PIC X(7)  VALUE                       
               '  CODE-'.                                               
           05  P-DEN-CODE         PIC X(4).                             
                                                                        
       01  P-DEN-LINE-2.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-DEN-TEXT-1       PIC X(60).                            
                                                                        
       01  P-DEN-LINE-3.                                                
           05  FILLER             PIC X(10) VALUE  SPACES.              
           05  P-DEN-TEXT-2       PIC X(60).                            
                                                                        
       01  P-CHG-LINE-1.                                                
           05  FILLER             PIC X(11) VALUE                       
               'INCUR CHG  '.                                           
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-CHG-REC-DT       PIC X(8).                             
           05  FILLER             PIC X     VALUE SPACES.               
           05  P-CHG-BY           PIC X(4).                             
           05  FILLER             PIC X(1)  VALUE  SPACES.              
           05  FILLER             PIC X(8)  VALUE                       
               'INCURED-'.                                              
           05  P-CHG-INC-DT       PIC X(8).                             
           05  FILLER             PIC X     VALUE  SPACES.              
           05  L-1-PD-THRU        PIC X(8)  VALUE                       
               'PD THRU-'.                                              
           05  P-CHG-PAID-TO-DT   PIC X(8).                             
           05  FILLER             PIC X(15) VALUE                       
               '  INIT MAN RES-'.                                       
           05  P-CHG-INIT-RES     PIC Z(7).99.                          
                                                                        
       01  P-CHG-LINE-2.                                                
           05  FILLER             PIC X(22) VALUE  SPACES.              
           05  FILLER             PIC X(9)  VALUE                       
               'REPORTED-'.                                             
           05  P-CHG-REP-DT       PIC X(8).                             
           05  FILLER             PIC X(9)  VALUE                       
               ' TOT PD -'.                                             
           05  P-CHG-TOT-PD       PIC Z(5).99.                          
           05  FILLER             PIC X(14) VALUE                       
               ' CUR MAN RES -'.                                        
           05  P-CHG-CUR-RES      PIC Z(7).99.                          
                                                                        
       01  P-CHG-LINE-3.                                                
           05  FILLER             PIC X(13) VALUE                       
               'TOT EXPENSE -'.                                         
           05  P-CHG-TOT-EXP      PIC Z(6).99.                          
           05  FILLER             PIC X(10) VALUE                       
               ' CREATED -'.                                            
           05  P-CHG-CREAT-DT     PIC X(8).                             
           05  FILLER             PIC X(10) VALUE                       
               '  DAYS PD-'.                                            
           05  P-CHG-DAYS-PD      PIC Z(5).                             
           05  FILLER             PIC X(5)  VALUE  SPACE.               
           05  FILLER             PIC X(10) VALUE                       
               'ADD- RES -'.                                            
           05  P-CHG-ADD-RES      PIC Z(7).99.                          
                                                                        
       01  P-CHG-LINE-4.                                                
           05  FILLER             PIC X(13) VALUE                       
               'CHG EXPENSE -'.                                         
           05  P-CHG-CHG-EXP      PIC Z(6).99.                          
           05  FILLER             PIC X(10) VALUE                       
               ' LAST PMT-'.                                            
           05  P-CHG-LAST-PMT-DT  PIC X(8).                             
           05  FILLER             PIC X     VALUE  SPACES.              
           05  FILLER             PIC X(8)  VALUE                       
               'PMTS   -'.                                              
           05  P-CHG-PMTS         PIC ZZ9.                              
           05  FILLER             PIC X(7)  VALUE   SPACES.             
           05  FILLER             PIC X(17) VALUE                       
               'TOT TRLRS   -    '.                                     
           05  P-CHG-TOT-TRLRS    PIC ZZZ9.                             
                                                                        
       01  P-2-LINE-6.                                                  
           05  FILLER             PIC X(28) VALUE                       
               '- - OPEN / CLOSE HISTORY - -'.                          
                                                                        
       01  P-2-LINE-8.                                                  
           05  FILLER             PIC X(27) VALUE                       
               '  DATE    OPEN/CLOSE  CAUSE'.                           
                                                                        
       01  P-2-HIS-DETAIL.                                              
           05  P-2-HIS-DATE       PIC X(8).                             
           05  FILLER             PIC X(4)  VALUE SPACES.               
           05  P-2-HIS-OPCL       PIC X(6).                             
           05  FILLER             PIC X(4)  VALUE SPACES.               
           05  P-2-HIS-CAUSE      PIC X(10).                            
                                                                        
       01  P-2-AUT-LINE-1.                                              
           05  FILLER             PIC X(35) VALUE                       
               '- - AUTOMATIC PAYMENT SCHEDULES - -'.                   
                                                                        
       01  P-2-AUT-LINE-2.                                              
           05  FILLER             PIC X(22) VALUE                       
               'ESTABLISHED ON      - '.                                
           05  P-2-EST-DT         PIC X(8).                             
           05  FILLER             PIC X(10)   VALUE SPACES.             
           05  FILLER             PIC X(22)   VALUE                     
               'ESTABLISHED BY      - '.                                
           05  P-2-EST-BY         PIC X(8).                             
                                                                        
       01  P-2-AUT-LINE-3.                                              
           05  FILLER             PIC X(22)   VALUE                     
               'EFFECTIVE DATE      - '.                                
           05  P-2-EFF-DT         PIC X(8).                             
           05  FILLER             PIC X(10)   VALUE SPACES.             
           05  FILLER             PIC X(22)   VALUE                     
               'ENDED / REPLACED    - '.                                
           05  P-2-END-DT         PIC X(8).                             
                                                                        
       01  P-2-AUT-LINE-4.                                              
           05  P-2-1ST-PMT-DT     PIC X(22)   VALUE                     
               'FIRST PAYMENT DATE  - '.                                
           05  P-2-1ST-PMT-ON     PIC X(8).                             
           05  FILLER             PIC X(10)   VALUE SPACES.             
           05  P-2-LST-PMT-DT     PIC X(22)   VALUE                     
               'LAST PAYMENT ON     - '.                                
           05  P-2-LST-PMT-ON     PIC X(8).                             
                                                                        
       01  P-2-AUT-LINE-5.                                              
           05  FILLER             PIC X(22)   VALUE                     
               'FIRST PAYMENT AMT   - '.                                
           05  P-2-1ST-PMT        PIC ZZZZ,ZZZ.99.                      
           05  FILLER             PIC X(7)    VALUE SPACES.             
           05  FILLER             PIC X(22)   VALUE                     
               'REGULAR PAYMENT AMT - '.                                
           05  P-2-REG-PMT        PIC ZZZZ,ZZZ.99.                      
                                                                        
       01  P-2-AUT-LINE-6.                                              
           05  FILLER             PIC X(22)   VALUE                     
               'DAYS IN 1ST PERIOD  - '.                                
           05  P-2-DAYS-1ST       PIC 9(4).                             
      *    05  FILLER             PIC X(14)   VALUE SPACES.             
      *    05  FILLER             PIC X(22)   VALUE                     
      *        'DAYS IN REGULAR PMT - '.                                
      *    05  P-2-DAYS-REG       PIC X(3).                             
                                                                        
       01  P-2-AUT-LINE-7.                                              
           05  FILLER             PIC X(22)   VALUE                     
               'LAST PAYMENT FINAL  - '.                                
           05  P-2-LAST-FINAL     PIC X(3).                             
           05  FILLER             PIC X(15)   VALUE SPACES.             
           05  FILLER             PIC X(22)   VALUE                     
               'PAYEE               - '.                                
           05  P-2-PAYEE          PIC X(17).                            
                                                                        
       01  P-2-AUT-LINE-8.                                              
           05  FILLER             PIC X(40)   VALUE  SPACES.            
           05  FILLER             PIC X(22)   VALUE                     
               'MONTHS BETWEEN PMTS - '.                                
           05  P-2-MOS-BET        PIC 9(3).                             
                                                                        
       01  P-2-ADD-LINE-1.                                              
           05  FILLER             PIC X(25)   VALUE                     
               '- - ADDRESSES ON FILE - -'.                             
                                                                        
       01  P-2-ADD-LINE-2.                                              
           05  FILLER             PIC X(7)    VALUE                     
               'TYPE - '.                                               
           05  P-2-ADD-TYPE       PIC X(15).                            
           05  FILLER             PIC X(2)    VALUE SPACES.             
           05  FILLER             PIC X(7)    VALUE                     
               'CODE - '.                                               
           05  P-2-ADD-CODE       PIC X.                                
           05  FILLER             PIC X(02)   VALUE SPACES.             
           05  FILLER             PIC X(15)   VALUE                     
               'MAIL TO NAME - '.                                       
           05  P-2-ADD-NAME       PIC X(30).                            
                                                                        
       01  P-2-ADD-LINE-3.                                              
           05  FILLER             PIC X(34)   VALUE  SPACES.            
           05  FILLER             PIC X(13)   VALUE                     
               'ADDRESS 1  - '.                                         
           05  P-2-ADD-ADDR-1     PIC X(30).                            
                                                                        
       01  P-2-ADD-LINE-4.                                              
           05  FILLER             PIC X(34)   VALUE  SPACES.            
           05  FILLER             PIC X(13)   VALUE                     
               'ADDRESS 2  - '.                                         
           05  P-2-ADD-ADDR-2     PIC X(30).                            
                                                                        
       01  P-2-ADD-LINE-5.                                              
           05  FILLER             PIC X(34)   VALUE  SPACES.            
           05  FILLER             PIC X(13)   VALUE                     
               'CITY STATE - '.                                         
           05  P-2-ADD-CITY       PIC X(30).                            
                                                                        
       01  P-2-ADD-LINE-6.                                              
           05  FILLER             PIC X(34)   VALUE  SPACES.            
           05  FILLER             PIC X(13)   VALUE                     
               'ZIP  PHONE - '.                                         
           05  P-2-ADD-ZIP        PIC X(10).                            
           05  FILLER             PIC X(8)    VALUE   SPACES.           
           05  P-2-ADD-PHONE      PIC X(12).                            
                                                                        
                                                                        
       01  P-FORM-LINE-1.                                               
           05  FILLER              PIC X(11)  VALUE                     
               'FORM CTL'.                                              
           05  FILLER             PIC X     VALUE  SPACE.               
           05  P-FORM-LET-DT       PIC X(8).                            
           05  FILLER              PIC X      VALUE SPACE.              
           05  P-FORM-BY           PIC X(4).                            
           05  FILLER              PIC X(14)  VALUE                     
               '  ADDRESSEE - '.                                        
           05  P-FORM-ADSEE        PIC X(30).                           
           05  FILLER              PIC X(8)   VALUE                     
               ' CODE - '.                                              
           05  P-FORM-ADSEE-CD     PIC X(3).                            
                                                                        
       01  P-FORM-LINE-2.                                               
           05  FILLER              PIC X(23)  VALUE  SPACES.            
           05  FILLER              PIC X(7)   VALUE                     
               'FORM - '.                                               
           05  P-FORM-FORM         PIC X(4).                            
           05  FILLER              PIC X(9)   VALUE                     
               '  SENT - '.                                             
           05  P-FORM-SENT-DT      PIC X(8).                            
           05  FILLER              PIC X(12)  VALUE                     
               '  RE-SEND - '.                                          
           05  P-FORM-SEND-DT      PIC X(8).                            
                                                                        
       01  P-FORM-LINE-3.                                               
           05  FILLER              PIC X(23)  VALUE  SPACES.            
           05  FILLER              PIC X(12)  VALUE                     
               'FOLLOW UP - '.                                          
           05  P-FORM-FOL-DT       PIC X(8).                            
           05  FILLER              PIC X(25)  VALUE                     
               '     CLAIMANT ANSWERED - '.                             
           05  P-CLM-FORM-ANS-DT   PIC X(8).                            
                                                                        
       01  P-FORM-LINE-4.                                               
           05  FILLER              PIC X(23)  VALUE  SPACES.            
           05  P-PHY-FORM-COMM     PIC X(17)  VALUE                     
               'PHY.  ANSWERED - '.                                     
           05  P-PHY-FORM-ANS-DT   PIC X(8).                            
           05  P-EMP-FORM-COMM     PIC X(20)  VALUE                     
               '   EMP.  ANSWERED - '.                                  
           05  P-EMP-FORM-ANS-DT   PIC X(8).                            
                                                                        
       01  P-FORM-LINE-5.                                               
           05  FILLER             PIC X(23)   VALUE SPACES.             
           05  FILLER             PIC X(15)   VALUE                     
               'INSTRUCTIONS - '.                                       
           05  P-FORM-INSTRUCT    PIC X(28).                            
                                                                        
       01  P-FORM-LINE-6.                                               
           05  FILLER             PIC X(38)   VALUE SPACES.             
           05  P-FORM-INSTRUCT-1  PIC X(28).                            
                                                                        
       01  P-FORM-LINE-7.                                               
           05  FILLER             PIC X(23)   VALUE SPACES.             
           05  FILLER             PIC X(16)   VALUE                     
               'RELATED CLAIM - '.                                      
           05  P-FORM-CLAIM       PIC X(7).                             
           05  FILLER             PIC X(12)   VALUE                     
               '  CARRIER - '.                                          
           05  P-FORM-CARRIER     PIC X.                                
           05  FILLER             PIC X(9)    VALUE                     
               '  CERT - '.                                             
           05  P-FORM-CERT        PIC X(8).                             
                                                                        
       01  AUTO-PAY-TABLE.                                              
           05  AUTO-PAY-RECORD    OCCURS 10 TIMES  INDEXED BY AP-INDEX. 
               10  AP-TBL-EST-DT             PIC XX.                    
               10  AP-TBL-EST-BY             PIC XXXX.                  
               10  AP-TBL-SCHED-START-DT     PIC XX.                    
               10  AP-TBL-SCHED-END-DT       PIC XX.                    
               10  AP-TBL-TERM-DT            PIC XX.                    
               10  AP-TBL-LAST-TYPE          PIC X.                     
               10  AP-TBL-FIRST-AMT          PIC S9(7)V99 COMP-3.       
               10  AP-TBL-FIRST-DAYS         PIC S9(4) COMP.            
               10  AP-TBL-FIRST-DT           PIC XX.                    
               10  AP-TBL-REG-AMT            PIC S9(7)V99  COMP-3.      
               10  AP-TBL-REG-MO             PIC S9(4) COMP.            
               10  AP-TBL-INT-MO             PIC XX.                    
                                                                        
       01  ADDRESS-TABLE.                                               
           05  ADDRESS-RECORD    OCCURS 60 TIMES  INDEXED BY AD-INDEX.  
               10  AD-TBL-TYPE               PIC X.                     
               10  AD-TBL-NAME               PIC X(30).                 
               10  AD-TBL-ADDR-1             PIC X(30).                 
               10  AD-TBL-ADDR-2             PIC X(30).                 
               10  AD-TBL-CITY               PIC X(30).                 
               10  AD-TBL-ZIP                PIC X(10).                 
               10  AD-TBL-PHONE              PIC 9(11) COMP-3.          
                                                                        
       01  OPEN-CLOSE-TABLE.                                            
           05  AUTO-PAY-RECORD    OCCURS 6 TIMES  INDEXED BY OC-INDEX.  
               10  OC-TBL-OPCL-DT            PIC XX.                    
               10  OC-TBL-OPCL-TYPE          PIC X.                     
               10  OC-TBL-OPCL-REASON        PIC X(5).                  

                                           COPY ELCAID.                 
       01  PF-AID REDEFINES DFHAID.                                     
           05  FILLER                      PIC X(8).                    
           05  PF-VALUES  OCCURS 24    PIC X.                           
           EJECT                                                        
                                       COPY ELCALGND.                   
           EJECT                                                        
                                       COPY ELPRTCVD.                   
           EJECT                                                        
                                       COPY ELCINTF.                    
           12  WS-INT-BLK REDEFINES PI-PROGRAM-WORK-AREA.               
               16  FILLER                 PIC X.                        
               16  WS-PI-NAME             PIC X(30).                    
               16  FILLER                 PIC X(609).                   
                                                                        
           EJECT                                                        
                                       COPY ELCATTR.                    
                                       COPY ELCDATE.                    
                                       COPY ELCEMIB.                    
                                       COPY ELCCALC.                    

                                       COPY ELCCNTL.                    
                                       COPY ELCMSTR.                    
                                       COPY ELCTRLR.                    
                                       COPY ELCCERT.                    
                                       COPY ERCACCT.                    
                                       COPY ELCBENE.                    

       01  tran-data-line1             pic x(80) value spaces.
       01  tran-data-line2             pic x(80) value spaces.
       01  soc-CLIENT-IN-DATA.
           05  soc-client-comp-id      pic xxx.
           05  soc-CLIENT-CAR          PIC X.
           05  soc-CLIENT-CLAIM-NO     PIC X(7).
           05  soc-CLIENT-CERT-NO      PIC X(11).
           05  soc-client-proc-id      pic x(4).
           05  soc-client-full-partial pic x.

       01  filler                      pic x(100) value low-values.      

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA.
           05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
           05 LSTN-NAME                PIC X(8).
           05 LSTN-SUBNAME             PIC X(8).
      **********  SOCK10   ************
           05 CLIENT-IN-DATA           pic x(36).
      **********  SOCK10   ************
           05 SOCKADDR-IN-PARM.
              15 SIN-FAMILY            PIC 9(4) COMP.
              15 SIN-PORT              PIC 9(4) COMP.
              15 SIN-ADDRESS           PIC 9(8) COMP.
              15 SIN-ZERO              PIC X(8).
       01  VAR                         PIC X(30).

       PROCEDURE DIVISION.                                              

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' My env is ' ws-kix-myenv

      * when calling a C function the function returns its value
      * in the system variable return code.

      *    display 'SOCK10:transaction data =', CLIENT-IN-DATA '**'
      *    display 'SOCK10:socket number    =', GIVE-TAKE-SOCKET.
      *    display 'SOCK10:socket name      =', lstn-name ' '
      *       lstn-subname
      *    display 'SOCK10:socket family    =', sin-family
      *    display 'SOCK10:socket port      =', sin-port
      *    display 'SOCK10:socket address   =', sin-address
      *    display 'SOCK10:socket zero      =', sin-zero
      
           exec cics
              asktime
           end-exec

           MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
           MOVE '5'                   TO DC-OPTION-CODE.                
           PERFORM 8100-DATE-RTN  THRU  8100-EXIT.                      
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
                                                                        
           perform 0000-init-contact   thru 0000-exit
           perform 0010-receive        thru 0010-exit
           PERFORM 4000-INITIALIZE     THRU 4000-EXIT
           PERFORM 0021-PROCESS-SOC    THRU 0021-EXIT

           .      
       0000-end.

           perform 1000-send-buffer    thru 1000-exit
           close CLM-STAT-EXT-OUT
           perform 1100-close-socket   thru 1100-exit
           goback

           .
       0000-init-contact.
      
           move spaces                 to ws-return-stuff
           if client-in-data (1:8) = 'SOCKET10'
              continue
           else
              display ' Unknown origin ' client-in-data
              go to 1100-close-socket
           end-if
      
           move 'SOCKET10READY'        to ws-send-buf
           move +25                    to ws-send-msg-size
      
      *    display 'SOCK10:sequence number  =', ws-seq-num.
      *    display 'SOCK10:send buffer      =', ws-send-buf(1:25).
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags

           if return-code <= zero
              display 'SOCK10:send error ' return-code
              go to 1000-socket-error
           end-if

      *    display 'SOCK10:About to recv '

           .
       0000-exit.
           exit.
      
       0010-receive.
      
           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.
      
           if return-code < zero
              display 'SOCK10:recv error ' return-code
              go to 1000-socket-error
           end-if
      
           if return-code = zero
              display 'SOCK10:client disconnected',
              go to 1000-socket-error
           end-if
      
      *    display 'SOCK10:Good recv  '
      *    display 'SOCK10:return code      = ', return-code
      *    display 'SOCK10:receive buffer   = ', ws-recv-buf(1:60)
      
           move +256                   to ws-send-msg-size
      
           move ws-recv-buf (1:27)     to soc-client-in-data

           move soc-client-comp-id     to ws-comp-id

           evaluate true
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'VPP'
                 move X'07'            to ws-comp-cd
              when ws-comp-id = 'FNL'
                 move X'08'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate

           .
       0010-exit.
           exit.
                                                                        
       0021-PROCESS-SOC.                                                

           MOVE WS-COMP-ID             TO WS-CF-COMPANY-ID
           MOVE SPACES                 TO WS-CF-PROCESSOR
           MOVE ZEROS                  TO WS-CF-SEQUENCE-NO
           MOVE '1'                    TO WS-CF-RECORD-TYPE

           MOVE SPACES                 TO WS-CNTL-ERROR-SW
           PERFORM 1300-READ-CNTL      THRU 1300-EXIT

           IF NO-COMPANY-RECORD                                         
              MOVE 'Invalid company ID '
                                       to ws-return-stuff
              go to 0021-exit
           end-if

           MOVE CF-CL-MAIL-TO-NAME      TO WS-UNALIGNED-FIELD.          
           MOVE SPACES                  TO WS-ALIGNED-FIELD.            
           MOVE +30                     TO WS-NAME-LENGTH.              
           PERFORM ELCALGNP              THRU ELCALGNP-EXIT.            
           MOVE WS-ALIGNED-FIELD         TO HEAD-COMPANY.               
                                                                        
           MOVE SPACES TO   END-OC-TABLE-SW                             
                            END-AD-TABLE-SW                             
                            END-AP-TABLE-SW.                            
                                                                        
           MOVE 'CLAIM STA'            TO HEAD-REPORT-NO

      *       BUILD TRAILER KEY AND MSTR KEY                            
           MOVE ws-comp-cd         TO  WS-AT-COMPANY-CD                 
                                       WS-CL-COMPANY-CD.                
           MOVE soc-client-car     TO  WS-AT-CARRIER                    
                                       WS-CL-CARRIER.                   
           MOVE soc-client-claim-no TO  WS-AT-CLAIM-NO                   
                                       WS-CL-CLAIM-NO.                  
           MOVE soc-client-cert-no TO  WS-AT-CERT-NO                    
                                       WS-CL-CERT-NO.                   
           MOVE ZEROS              TO  WS-AT-SEQ-NO.                    
                                                                        
           IF soc-client-full-partial <> 'P'                                  
              MOVE 'X'                 TO WS-FULL-PRINT-SW
           end-if

           PERFORM 0022-PROCESS-3-FILES
                                       THRU 0022-EXIT
           .
       0021-EXIT.                                                       
            EXIT.                                                       

       0022-PROCESS-3-FILES.

           PERFORM 1550-READ-MSTR      THRU 1550-EXIT

           IF MSTR-READ-ERROR                                           
              MOVE    ' Claim not found '
                                       to ws-return-stuff
              MOVE ' '                 TO WS-MSTR-READ-ERROR-SW         
              GO TO 0022-EXIT
           end-if

           MOVE SPACES                 TO P-CAUSE-DIAG
           MOVE WS-CL-CONTROL-PRIMARY  TO WS-AT-CONTROL-PRIMARY
           MOVE +90                    TO WS-AT-SEQ-NO

           EXEC CICS READ                                               
                DATASET ('ELTRLR')                                      
                into    (ACTIVITY-TRAILERS)                  
                RIDFLD  (WS-AT-CONTROL-PRIMARY)
                resp    (ws-response)                         
           END-EXEC
           if resp-normal
              MOVE AT-INFO-LINE-1      TO P-CAUSE-DIAG
           end-if

           MOVE 0 TO WS-PAGE-CNT.                                       
                                                                        
           SET AP-INDEX   TO 1.                                         
           SET AD-INDEX   TO 1.                                         
           SET OC-INDEX   TO 1.                                         
                                                                        
           MOVE +0    TO WS-AT-SEQ-NO.                                  
           PERFORM 2110-MOVE-CLAIM-INFO
                                       THRU 2110-EXIT
                                                                        
      ***  BUILDIN CERT KEY                                             
                                                                        
           MOVE CL-COMPANY-CD      TO  WS-CM-COMPANY-CD             
           MOVE CL-CERT-KEY-DATA   TO  WS-CM-CERT-DATA              
           MOVE CL-CERT-NO         TO  WS-CM-CERT-NO.               
                                                                        
      ***  READ CERT
                                                                        
           PERFORM 1600-READ-CERT   THRU 1600-EXIT.                 
                                                                        
           IF CERT-READ-ERROR                                       
              PERFORM 2120-NO-CERT-INFO  THRU 2120-EXIT            
           ELSE                                                     
              PERFORM 2130-MOVE-CERT-INFO THRU 2130-EXIT.          
                                                                        
           PERFORM  1700-BROWSE-TRLR   THRU 1700-EXIT.                  
                                                                        
           IF TRLR-BROWSE-ERROR                                         
              GO TO 0022-BYPASS-TRLRS.                                  
                                                                        
      *      TRAILER 0 IS THE FIRST REC AFTER START                     
           PERFORM  1750-READNEXT-TRLR   THRU 1750-EXIT.                
                                                                        
           PERFORM 2150-MOVE-TR0-INFO    THRU 2150-EXIT.                
                                                                        
      ***  NOW ALL 3 FILES HAVE BEEN READ                               
                                                                        
       0022-BYPASS-TRLRS.                                               
                                                                        
           PERFORM 2300-PRINT-TO-LINE-26  THRU 2300-EXIT.               
                                                                        
           MOVE AT-CONTROL-PRIMARY TO WS-AT-CONTROL-SAVED.              
           MOVE SPACE              TO WS-TRAILER-KEY-CHG-SW.            
                                                                        
           IF NOT TRLR-BROWSE-ERROR                                     
              PERFORM 2500-PROCESS-N-PRINT-TRAILERS
                                       THRU 2500-EXIT until
                 TRAILER-KEY-CHANGED                              
           ELSE                                                      
              MOVE '* TRLRS HAVE BEEN PURGED    *'
                                       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
              go to 0022-end
           end-if
                                                                        
      ******     BUILD ERACCT/EMPROD KEY                                
                                                                        
           IF NOT CERT-READ-ERROR                                   
              IF FULL-PRINT-REQUIRED                               
                 MOVE CM-CONTROL-PRIMARY
                                       TO WS-AM-CONTROL-PRIMARY 
                 PERFORM 1100-READ-AND-BUILD-ACCT
                                       THRU 1149-EXIT
              end-if
           end-if

      *******     BUILD ELBENE KEY                                      
           IF FULL-PRINT-REQUIRED                                       
              MOVE CL-COMPANY-CD  TO WS-BE-COMPANY-CD                   
              MOVE 'B'            TO WS-BE-RECORD-TYPE                  
              MOVE CL-BENEFICIARY TO WS-BE-BENEFICIARY                  
              PERFORM 1200-READ-AND-BUILD-BENE THRU 1299-EXIT.          
                                                                        
           IF FULL-PRINT-REQUIRED                                       
               PERFORM 2700-PRINT-OPEN-CLOSE-HISTORY THRU 2700-EXIT.    
                                                                        
       0022-END.                                                        
           IF NOT TRLR-BROWSE-ERROR                                     
              PERFORM  1701-ENDBR-TRLR      THRU 1701-EXIT.             
                                                                        
       0022-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       1100-READ-AND-BUILD-ACCT.                                        
           EXEC CICS  HANDLE CONDITION                                  
                  NOTFND  (1140-ENDBR)                                  
                  ENDFILE (1140-ENDBR)                                  
           END-EXEC.                                                    
                                                                        
           EXEC CICS  STARTBR                                           
                  DATASET  ('ERACCT')                                   
                  RIDFLD   (WS-AM-CONTROL-PRIMARY)                      
                  GTEQ                                                  
           END-EXEC.                                                    
                                                                        
           MOVE 'X'         TO WS-ACCT-BROWSE-SW.                       
                                                                        
       1110-READ-NEXT.                                                  
           EXEC CICS  READNEXT                                          
                  into     (ACCOUNT-MASTER)                  
                  DATASET  ('ERACCT')                                   
                  RIDFLD   (WS-AM-CONTROL-PRIMARY)                      
           END-EXEC.                                                    
                                                                        
       1120-CHECK-IF-EQUAL.                                             
           IF CM-COMPANY-CD = AM-COMPANY-CD AND                         
              CM-CARRIER    = AM-CARRIER    AND                         
              CM-GROUPING   = AM-GROUPING   AND                         
              CM-STATE      = AM-STATE      AND                         
              CM-ACCOUNT    = AM-ACCOUNT                                
                 NEXT SENTENCE                                          
           ELSE                                                         
              GO TO 1149-EXIT.                                          
                                                                        
           IF CM-CERT-EFF-DT NOT LESS THAN AM-EXPIRATION-DT             
              GO TO 1110-READ-NEXT.                                     
                                                                        
           IF CM-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT              
              NEXT SENTENCE                                             
           ELSE                                                         
              GO TO 1110-READ-NEXT.                                     
                                                                        
       1130-BUILD-ERACCT-ADDR.                                          
           MOVE 'X' TO END-AD-TABLE-SW.                                 
           MOVE '8'                    TO AD-TBL-TYPE   (AD-INDEX).     
           MOVE AM-NAME                TO AD-TBL-NAME   (AD-INDEX).     
           MOVE AM-ADDRS               TO AD-TBL-ADDR-1 (AD-INDEX).     
           MOVE SPACES                 TO AD-TBL-ADDR-2 (AD-INDEX).     
           MOVE SPACES                 TO AD-TBL-CITY   (AD-INDEX).     
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
           END-STRING
                                                                        
           MOVE SPACES                 TO WS-ZIP-WORK.                  
           IF AM-CANADIAN-POST-CODE                                     
               MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-1               
               MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-2               
           ELSE                                                         
               MOVE AM-ZIP-PRIME       TO WS-ZIP-PRIME                  
               IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
                   MOVE '-'            TO WS-ZIP-DASH                   
                   MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4.                 
           MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).     
                                                                        
           MOVE AM-TEL-NO              TO WS-WORK-PHONE.                
           INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.              
           MOVE WS-NUMERIC-PHONE       TO AD-TBL-PHONE (AD-INDEX).      
           SET AD-INDEX UP BY 1.                                        
                                                                        
       1140-ENDBR.                                                      
           IF ACCT-BROWSE-OKAY                                          
              EXEC CICS  ENDBR                                          
                   DATASET  ('ERACCT')                                  
              END-EXEC.                                                 
                                                                        
       1149-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       1200-READ-AND-BUILD-BENE.                                        
           EXEC CICS  HANDLE CONDITION                                  
                  NOTFND  (1299-EXIT)                                   
                  ENDFILE (1299-EXIT)                                   
           END-EXEC.                                                    
                                                                        
       1210-READ.                                                       
           EXEC CICS  READ                                              
                  into     (BENEFICIARY-MASTER)              
                  DATASET  ('ELBENE')                                   
                  RIDFLD   (WS-BE-CONTROL-PRIMARY)                      
                  EQUAL                                                 
           END-EXEC.                                                    
                                                                        
       1220-BUILD-CLBENE-ADDR.                                          
           MOVE 'X' TO END-AD-TABLE-SW.                                 
           MOVE '9'                    TO AD-TBL-TYPE   (AD-INDEX).     
           MOVE BE-MAIL-TO-NAME        TO AD-TBL-NAME   (AD-INDEX).     
           MOVE BE-ADDRESS-LINE-1      TO AD-TBL-ADDR-1 (AD-INDEX).     
           MOVE BE-ADDRESS-LINE-2      TO AD-TBL-ADDR-2 (AD-INDEX).     
           MOVE SPACES                 TO AD-TBL-CITY   (AD-INDEX).     
           STRING BE-CITY ' ' BE-STATE
              DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
           END-STRING
                                                                        
           MOVE SPACES                 TO WS-ZIP-WORK.                  
           IF BE-CANADIAN-POST-CODE                                     
               MOVE BE-CAN-POSTAL-1    TO WS-CAN-POSTAL-1               
               MOVE BE-CAN-POSTAL-2    TO WS-CAN-POSTAL-2               
           ELSE                                                         
               MOVE BE-ZIP-PRIME       TO WS-ZIP-PRIME                  
               IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
                   MOVE '-'            TO WS-ZIP-DASH                   
                   MOVE BE-ZIP-PLUS4   TO WS-ZIP-PLUS4.                 
           MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).     
                                                                        
           IF BE-PHONE-NO NOT NUMERIC                                   
              MOVE ZEROS TO BE-PHONE-NO.                                
           MOVE BE-PHONE-NO            TO AD-TBL-PHONE  (AD-INDEX).     
                                                                        
       1299-EXIT.                                                       
            EXIT.                                                       
           EJECT                                                        
       1300-READ-CNTL.                                                  
      ******************************************************************
      ***          I/O REQUESTS AGAINST THE CONTROL FILE             ***
      ******************************************************************

           EXEC CICS  READ                                              
              into     (CONTROL-FILE)                    
              DATASET  ('ELCNTL')                                   
              RIDFLD   (WS-CF-CONTROL-PRIMARY)
              resp     (ws-response)                      
           END-EXEC

           if not resp-normal
              MOVE 'X'                 TO WS-CNTL-ERROR-SW
           end-if

           .
       1300-EXIT.                                                       
            EXIT.                                                       

       1550-READ-MSTR.                                                  
      ******************************************************************
      ***       I/O REQUESTS AGAINST THE CLAIM MASTER FILE           ***
      ******************************************************************

           EXEC CICS  READ
              into     (CLAIM-MASTER)
              DATASET  ('ELMSTR')
              RIDFLD   (WS-CL-CONTROL-PRIMARY)
              resp     (ws-response)
           END-EXEC

           if not resp-normal
              move 'X'                 to ws-mstr-read-error-sw
           end-if

           .
       1550-EXIT.                                                       
            EXIT.                                                       

       1600-READ-CERT.                                                  
      ******************************************************************
      ***   I/O REQUESTS AGAINST THE CERTIFICATE MASTER FILE         ***
      ******************************************************************

           EXEC CICS  READ                                              
              into (CERTIFICATE-MASTER)              
              DATASET  ('ELCERT')                                   
              RIDFLD   (WS-CM-CONTROL-PRIMARY)
              resp     (ws-response)                      
           END-EXEC

           if not resp-normal
              move 'X'                 to ws-cert-read-error-sw
           end-if

           .
       1600-EXIT.                                                       
            EXIT.                                                       
                                                                        
       1700-BROWSE-TRLR.                                                
      ******************************************************************
      ***       I/O REQUESTS AGAINST THE ACTIVITY TRAILER FILE       ***
      ******************************************************************

           EXEC CICS  STARTBR                                           
              DATASET  ('ELTRLR')                                   
              RIDFLD   (WS-AT-CONTROL-PRIMARY)                      
              EQUAL                                                 
           END-EXEC

           if resp-normal                                                             
              MOVE SPACE               TO WS-TRLR-BROWSE-ERROR-SW
           else
              MOVE 'X'                 TO WS-TRLR-BROWSE-ERROR-SW
           end-if

           .
       1700-EXIT.                                                       
            EXIT.                                                       
                                                                        
       1701-ENDBR-TRLR.                                                 
           EXEC CICS  ENDBR                                             
              DATASET  ('ELTRLR')                                   
           END-EXEC.                                                    
                                                                        
       1701-EXIT.                                                       
            EXIT.                                                       
                                                                        
       1750-READNEXT-TRLR.                                              
           EXEC CICS  HANDLE CONDITION                                  
                  ENDFILE (1752-ENDFILE)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS  READNEXT                                          
                  DATASET  ('ELTRLR')                                   
                  RIDFLD   (WS-AT-CONTROL-PRIMARY)                      
                  INTO     (ACTIVITY-TRAILERS)               
           END-EXEC.                                                    
                                                                        
           GO TO 1750-EXIT.                                             
                                                                        
       1752-ENDFILE.                                                    
           MOVE  'E'  TO WS-ELTRLR-EOF-SW.                              
                                                                        
       1750-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
                                                                        
           EJECT                                                        
      *ALIGN-RTN.                      COPY ELCALGNP.                   
       ALIGN-RTN.
           COPY ELCALGNP.
           EJECT                                                        
       2110-MOVE-CLAIM-INFO.                                            

           MOVE CL-CLAIM-NO          TO P-CLAIM-NO.                     
           MOVE CL-CARRIER           TO P-CARR.                         
           MOVE CL-CERT-NO           TO P-CERT-NO.                      
           MOVE CL-CCN               TO HEAD-CREDIT-CARD.               
           MOVE CL-INSURED-SEX-CD    TO P-INSURED-SEX.                  
           MOVE SPACES               TO P-STATUS     P-TYPE.            
                                                                        
           EVALUATE TRUE
      
              WHEN CL-CLAIM-TYPE = 'A'
                 MOVE ' AH '
                                       TO P-TYPE
      
              WHEN CL-CLAIM-TYPE = 'I'
                 MOVE ' IU '            TO P-TYPE
      
              WHEN CL-CLAIM-TYPE = 'G'
                 MOVE 'GAP '            TO P-TYPE
      
              WHEN CL-CLAIM-TYPE = 'F'
                 MOVE 'FAM '            TO P-TYPE
      
              WHEN CL-CLAIM-TYPE = 'L'
                 MOVE 'LIFE'
                                       TO P-TYPE
      
           END-EVALUATE
                                                                        
           IF CLAIM-IS-CLOSED                                           
               MOVE 'CLOSED'  TO P-STATUS.                              
                                                                        
           IF CLAIM-IS-OPEN                                             
               MOVE 'OPEN'    TO P-STATUS.                              
                                                                        
           PERFORM 2111-ARRANGE-NAME  THRU 2111-EXIT.                   
                                                                        
           MOVE CL-PROCESSOR-ID TO P-PROCESSOR.                         
           MOVE CL-LAST-PMT-AMT TO P-LAST-PMT-AMT.                      
                                                                        
           IF CL-PAID-THRU-DT  = LOW-VALUES OR SPACES                   
               MOVE SPACE TO P-PAID-THRU-DT                             
               IF PI-USES-PAID-TO                                       
                  MOVE 'PAID  TO   - ' TO L-12-PD-THRU                  
               ELSE                                                     
                  MOVE 'PAID THRU  - ' TO L-12-PD-THRU                  
           ELSE                                                         
               IF NOT PI-USES-PAID-TO                                   
                  MOVE CL-PAID-THRU-DT TO DC-BIN-DATE-1                 
                  MOVE ' '             TO DC-OPTION-CODE                
                  PERFORM 8100-DATE-RTN  THRU 8100-EXIT                 
                  MOVE DC-GREG-DATE-1-EDIT  TO P-PAID-THRU-DT           
               ELSE                                                     
                  MOVE 'PAID  TO   - ' TO L-12-PD-THRU                  
                  MOVE CL-PAID-THRU-DT TO DC-BIN-DATE-1                 
                  MOVE +1  TO DC-ELAPSED-DAYS                           
                  MOVE +0  TO DC-ELAPSED-MONTHS                         
                  MOVE '6' TO DC-OPTION-CODE                            
                  PERFORM 8100-DATE-RTN  THRU 8100-EXIT                 
                  MOVE DC-GREG-DATE-1-EDIT  TO P-PAID-THRU-DT.          
                                                                        
           IF CL-LAST-PMT-DT = LOW-VALUES OR SPACES                     
               MOVE SPACE TO P-LAST-PMT-DT                              
           ELSE                                                         
               MOVE CL-LAST-PMT-DT  TO DC-BIN-DATE-1                    
               MOVE ' '             TO DC-OPTION-CODE                   
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT  TO P-LAST-PMT-DT.              
                                                                        
           IF CL-INCURRED-DT = LOW-VALUES OR SPACES                     
               MOVE SPACE TO P-INCURRED-DT                              
           ELSE                                                         
               MOVE CL-INCURRED-DT  TO DC-BIN-DATE-1                    
               MOVE ' '             TO DC-OPTION-CODE                   
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT  TO P-INCURRED-DT.              
                                                                        
           IF CL-REPORTED-DT = LOW-VALUES OR SPACES                     
               MOVE SPACE TO P-REPORTED-DT                              
           ELSE                                                         
               MOVE CL-REPORTED-DT TO DC-BIN-DATE-1                     
               MOVE ' '            TO DC-OPTION-CODE                    
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT  TO P-REPORTED-DT               
                                                                        
           IF CL-FILE-ESTABLISH-DT = LOW-VALUES OR SPACES               
               MOVE SPACE TO P-ESTAB-DT                                 
           ELSE                                                         
               MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1               
               MOVE ' '                  TO DC-OPTION-CODE              
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT    TO P-ESTAB-DT.               
                                                                        
           IF CL-NEXT-AUTO-PAY-DT = LOW-VALUES OR SPACES                
               MOVE SPACES   TO P-NEXT-AUTO-DT                          
           ELSE                                                         
               IF PI-USES-PAID-TO                                       
                   MOVE CL-NEXT-AUTO-PAY-DT    TO  DC-BIN-DATE-1        
                   MOVE '6'                    TO  DC-OPTION-CODE       
                   MOVE +1                     TO  DC-ELAPSED-DAYS      
                   MOVE +0                     TO  DC-ELAPSED-MONTHS    
                   PERFORM 8100-DATE-RTN      THRU 8100-EXIT            
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT TO P-NEXT-AUTO-DT       
                   ELSE                                                 
                       MOVE SPACES             TO  P-NEXT-AUTO-DT       
               ELSE                                                     
                   MOVE CL-NEXT-AUTO-PAY-DT    TO  DC-BIN-DATE-1        
                   MOVE ' '                    TO  DC-OPTION-CODE       
                   MOVE +0                     TO  DC-ELAPSED-DAYS      
                                                  DC-ELAPSED-MONTHS     
                   PERFORM 8100-DATE-RTN       THRU 8100-EXIT           
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT TO P-NEXT-AUTO-DT       
                   ELSE                                                 
                       MOVE SPACES             TO  P-NEXT-AUTO-DT.      
                                                                        
           MOVE CL-NO-OF-PMTS-MADE     TO P-PMTS-MADE.                  
                                                                        
           MOVE SPACES  TO P-PREM-TYPE.                                 
           IF SINGLE-PREMIUM                                            
               MOVE 'SINGLE'           TO P-PREM-TYPE.                  
                                                                        
           IF O-B-COVERAGE                                              
               MOVE 'OB COVG'          TO P-PREM-TYPE.                  
                                                                        
           IF OPEN-END-COVERAGE                                         
               MOVE 'OPN END'          TO P-PREM-TYPE.                  
                                                                        
           MOVE CL-TOTAL-PAID-AMT      TO P-TOT-PAID.                   
                                                                        
           MOVE SPACES              TO P-PURGED-STMT                    
                                       P-PURGED-DATE.                   
                                                                        
           IF CL-PURGED-DT NOT EQUAL LOW-VALUES                         
              MOVE CL-PURGED-DT         TO DC-BIN-DATE-1                
              MOVE ' '                  TO DC-OPTION-CODE               
              MOVE +0                   TO DC-ELAPSED-DAYS              
                                           DC-ELAPSED-MONTHS            
              PERFORM 8100-DATE-RTN  THRU 8100-EXIT                     
              MOVE 'PURGED DATE  - '    TO P-PURGED-STMT                
              MOVE DC-GREG-DATE-1-EDIT  TO P-PURGED-DATE.               
                                                                        
       2110-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2111-ARRANGE-NAME.                                               
           MOVE SPACES                TO P-NAME-GRP.                    
           MOVE CL-INSURED-LAST-NAME  TO WS-LAST-NAME.                  
           MOVE CL-INSURED-1ST-NAME   TO WS-FIRST-NAME.                 
                                                                        
           SET L-IND  TO 1.                                             
           SET F-IND  TO 1.                                             
           SET P-IND  TO 1.                                             
                                                                        
           MOVE SPACES  TO WS-LN-SW                                     
                           WS-FN-SW                                     
                           PREVIOUS-BYTE.                               
                                                                        
           IF WS-LAST-NAME = SPACE                                      
               NEXT SENTENCE                                            
           ELSE                                                         
               PERFORM 2111A-LOAD-LAST-NAME  THRU 2111A-EXIT            
                UNTIL  LAST-NAME-LOADED                                 
                OR     L-IND  GREATER THAN 15.                          
                                                                        
           MOVE ',' TO P-NAME (P-IND).                                  
           SET P-IND  UP  BY 2.                                         
           IF WS-FIRST-NAME = SPACE                                     
               NEXT SENTENCE                                            
           ELSE                                                         
               PERFORM 2111B-LOAD-FIRST-NAME  THRU 2111B-EXIT           
                UNTIL  FIRST-NAME-LOADED                                
                OR     F-IND  GREATER THAN 12.                          
                                                                        
           IF CL-INSURED-MID-INIT NOT = SPACES                          
               MOVE ','  TO P-NAME (P-IND)                              
               SET P-IND  UP BY 1                                       
               MOVE CL-INSURED-MID-INIT  TO P-NAME (P-IND)              
           ELSE                                                         
               NEXT SENTENCE.                                           
                                                                        
       2111-EXIT.                                                       
            EXIT.                                                       
                                                                        
       2111A-LOAD-LAST-NAME.                                            
           IF  WS-L-BYTE (L-IND) = SPACE                                
               IF PREVIOUS-BYTE = SPACE                                 
                   MOVE 'X' TO WS-LN-SW                                 
                   SET P-IND  DOWN  BY 1                                
                   GO TO 2111A-EXIT                                     
               ELSE                                                     
                   MOVE SPACE TO PREVIOUS-BYTE                          
           ELSE                                                         
               MOVE WS-L-BYTE (L-IND)  TO P-NAME (P-IND)                
                                          PREVIOUS-BYTE.                
                                                                        
           SET L-IND  UP BY 1.                                          
           SET P-IND  UP BY 1.                                          
                                                                        
       2111A-EXIT.                                                      
             EXIT.                                                      
                                                                        
       2111B-LOAD-FIRST-NAME.                                           
           IF  WS-F-BYTE (F-IND) = SPACE                                
               IF PREVIOUS-BYTE = SPACE                                 
                   MOVE 'X' TO WS-FN-SW                                 
                   SET P-IND  DOWN BY 1                                 
                   GO TO 2111B-EXIT                                     
               ELSE                                                     
                   MOVE SPACE TO PREVIOUS-BYTE                          
           ELSE                                                         
               MOVE WS-F-BYTE (F-IND)  TO P-NAME (P-IND)                
                                          PREVIOUS-BYTE.                
                                                                        
           SET F-IND  UP BY 1.                                          
           SET P-IND  UP BY 1.                                          
                                                                        
       2111B-EXIT.                                                      
             EXIT.                                                      
                                                                        
           EJECT                                                        
       2120-NO-CERT-INFO.                                               
           MOVE SPACES     TO P-COVERAGE                                
                              P-EXPIRE                                  
                              WS-CANC-DT                                
                              P-CERT-ISSUE-DT                           
                              P-CERT-ENTRY-MM  P-CERT-SL                
                              P-CERT-ENTRY-YY.                          
           MOVE ZERO       TO P-TERM                                    
                              P-REM.                                    
           MOVE SPACES     TO P-CERT-STAT                               
                              P-ACCT                                    
                              P-STATE                                   
                              P-GROUP                                   
                              P-REIN-CODE                               
                              P-CERT-CANC-DT                            
                              P-MEMBER-NO                               
                              P-CERT-BATCH.                             
           MOVE ZERO       TO P-ORIG-BENEF-AMT.                         
                                                                        
       2120-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2130-MOVE-CERT-INFO.                                             
           MOVE ZEROS                    TO WS-BEN-CODE.                
           MOVE CM-INSURED-ISSUE-AGE     TO P-INSURED-AGE.              
           MOVE CM-USER-FIELD            TO P-USER-CODE.                
                                                                        
           IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC                         
               MOVE ZEROS                TO CM-LF-ALT-BENEFIT-AMT.      
                                                                        
           IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
               MOVE  CM-AH-BENEFIT-CD      TO  WS-BEN-CODE              
               MOVE  '5'                   TO  WS-CF-RECORD-TYPE        
               MOVE  CM-AH-ORIG-TERM       TO  P-TERM                   
                                               CP-ORIGINAL-TERM         
               MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1            
               MOVE  CM-AH-BENEFIT-AMT     TO  P-ORIG-BENEF-AMT         
               MOVE  CM-AH-CURRENT-STATUS  TO  WS-STATUS                
               MOVE WS-BIN-CURRENT-DT      TO  CP-VALUATION-DT          
           ELSE                                                         
               MOVE  CM-LF-BENEFIT-CD      TO  WS-BEN-CODE              
               MOVE  '4'                   TO  WS-CF-RECORD-TYPE        
               MOVE  CM-LF-ORIG-TERM       TO  P-TERM                   
                                               CP-ORIGINAL-TERM         
               MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1            
               COMPUTE P-ORIG-BENEF-AMT =                               
                      CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT         
               MOVE  CM-LF-CURRENT-STATUS  TO  WS-STATUS                
               MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT
           END-IF
                                                                        
           MOVE ' '                       TO DC-OPTION-CODE.            
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       
           MOVE DC-GREG-DATE-1-EDIT       TO P-EXPIRE.                  
                                                                        
           MOVE CM-CERT-EFF-DT           TO CP-CERT-EFF-DT              
                                                                        
           MOVE 'CERT ENTRY    - '        TO L22-ENTRY-LIT.             
           IF CM-ENTRY-DT = LOW-VALUES OR SPACES                        
               MOVE SPACES TO    P-CERT-ENTRY-YY   P-CERT-SL            
                                 P-CERT-ENTRY-MM                        
           ELSE                                                         
               MOVE CM-ENTRY-DT           TO DC-BIN-DATE-1              
               MOVE ' '                   TO DC-OPTION-CODE             
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-MDY    TO WS-MM-DD-YY                
               MOVE WS-MM                 TO P-CERT-ENTRY-MM            
               MOVE WS-YY                 TO P-CERT-ENTRY-YY            
               MOVE '/' TO P-CERT-SL.                                   
                                                                        
           MOVE 'CERT ISSUE   - '         TO L15-ISSUE-LIT.             
           MOVE CM-CERT-EFF-DT            TO DC-BIN-DATE-1.             
           MOVE ' '                       TO DC-OPTION-CODE.            
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       
           MOVE DC-GREG-DATE-1-EDIT       TO P-CERT-ISSUE-DT.           
                                                                        
           MOVE CM-LOAN-1ST-PMT-DT        TO CP-FIRST-PAY-DATE.         
                                                                        
      **     READ CNTL RECORD TO OBTAIN FREE LOOK DAYS                  
                                                                        
           MOVE WS-COMP-ID             TO WS-CF-COMPANY-ID.          
           MOVE cm-STATE                  TO WS-CF-PROCESSOR.           
           MOVE ZEROS                     TO WS-CF-SEQUENCE-NO.         
           MOVE '3'                       TO WS-CF-RECORD-TYPE.         
           MOVE SPACES                    TO WS-CNTL-ERROR-SW.          
                                                                        
           PERFORM 1300-READ-CNTL  THRU 1300-EXIT.                      
                                                                        
           IF NO-COMPANY-RECORD                                         
              MOVE '*** ERROR NO STATE REC ***' TO WS-return-stuff
              GO TO 0000-end
           ELSE                                                         
              MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.              
                                                                        
pemtst     MOVE '1'                       TO CP-REM-TRM-CALC-OPTION.    
           MOVE WS-COMP-ID             TO CP-COMPANY-ID.             
           MOVE '4'                       TO CP-REM-TERM-METHOD.        
           PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                   
                                                                        
           IF CP-REMAINING-TERM-3 NOT GREATER THAN ZEROS                
              MOVE ZEROS      TO  P-REM                                 
             ELSE                                                       
              MOVE CP-REMAINING-TERM-3 TO  P-REM.                       
                                                                        
           MOVE SPACES           TO P-CERT-STAT.                        
                                                                        
           IF WS-STATUS = '1' OR '4'                                    
              IF CP-REMAINING-TERM-3  NOT GREATER THAN ZEROS            
                 MOVE 'EXPIRED'  TO P-CERT-STAT                         
                ELSE                                                    
                 MOVE 'ACTIVE'   TO P-CERT-STAT.                        
                                                                        
           IF WS-STATUS = '2'                                           
               MOVE 'PEND'     TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '3'                                           
               MOVE 'RESTORE ' TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '5'                                           
               MOVE 'REISSUE ' TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '6'                                           
               MOVE 'LMP DIS'  TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '7'                                           
               MOVE 'DEATH'    TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '8'                                           
               MOVE 'CANCEL'   TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = '9'                                           
               MOVE 'RE-ONLY ' TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = 'D'                                           
               MOVE 'DECLINE'  TO P-CERT-STAT.                          
                                                                        
           IF WS-STATUS = 'V'                                           
               MOVE 'VOID'     TO P-CERT-STAT.                          
                                                                        
           MOVE SPACES                 TO WS-CANC-DT.                   
                                                                        
           IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
              GO TO 2130-AH-CHECK
           END-IF
                                                                        
           IF CM-LF-CURRENT-STATUS = '8'                                
              IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       
                  MOVE CM-LF-CANCEL-DT TO WS-CANC-DT.                   
                                                                        
           IF CM-LF-CURRENT-STATUS = '7'                                
              IF CM-LF-DEATH-DT NOT = LOW-VALUES                        
                  MOVE CM-LF-DEATH-DT     TO WS-CANC-DT.                
                                                                        
           GO TO 2130-CONV-DATE.                                        
                                                                        
       2130-AH-CHECK.                                                   
           IF CM-AH-CURRENT-STATUS = '8'                                
              IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       
                  MOVE CM-AH-CANCEL-DT TO WS-CANC-DT.                   
                                                                        
           IF CM-AH-CURRENT-STATUS = '6' OR '7'                         
              IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   
                  MOVE CM-AH-SETTLEMENT-DT TO WS-CANC-DT.               
                                                                        
       2130-CONV-DATE.                                                  
           MOVE 'CERT CANCEL-'       TO L21-CANC-LIT.                   
           IF WS-CANC-DT = LOW-VALUES OR SPACES  OR ZEROS               
               MOVE SPACES TO P-CERT-CANC-DT                            
           ELSE                                                         
               MOVE WS-CANC-DT TO DC-BIN-DATE-1                         
               MOVE ' '        TO DC-OPTION-CODE                        
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT   TO P-CERT-CANC-DT.            
                                                                        
           MOVE 'ACCOUNT       - '        TO L19-ACCT-LIT.              
           MOVE CM-ACCOUNT     TO P-ACCT.                               
           MOVE CM-STATE       TO P-STATE.                              
           MOVE CM-GROUPING    TO P-GROUP.                              
           MOVE CM-REIN-TABLE  TO P-REIN-CODE.                          
           MOVE CM-ENTRY-BATCH TO P-CERT-BATCH.                         
           MOVE CM-MEMBER-NO   TO P-MEMBER-NO.                          
                                                                        
       2130-GET-BENEFIT-DESC.                                           
           IF WS-BEN-CODE = ZERO                                        
               MOVE '** NONE **' TO P-COVERAGE                          
               GO TO 2130-EXIT.                                         
                                                                        
           MOVE WS-ACCESS            TO WS-CF-PROCESSOR.                
           MOVE WS-COMP-ID        TO WS-CF-COMPANY-ID.               
           MOVE +0                   TO WS-CF-SEQUENCE-NO.              
           MOVE SPACES               TO WS-CNTL-ERROR-SW.               
                                                                        
           EXEC CICS HANDLE CONDITION                                   
                ENDFILE (2130-EXIT)                                     
                NOTFND  (2130-EXIT)                                     
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
                DATASET ('ELCNTL')                                      
                into    (CONTROL-FILE)                       
                RIDFLD  (WS-CF-CONTROL-PRIMARY)                         
                GTEQ                                                    
           END-EXEC.                                                    
                                                                        
           IF WS-CF-COMPANY-ID  NOT = CF-COMPANY-ID  OR                 
              WS-CF-RECORD-TYPE NOT = CF-RECORD-TYPE                    
                   GO TO 2130-EXIT.                                     
                                                                        
           PERFORM 2135-DUMMY THRU 2135-EXIT                            
               VARYING SUB1 FROM 1 BY 1 UNTIL                           
               ((SUB1 GREATER 8) OR                                     
                    (CF-BENEFIT-CODE (SUB1) = WS-BEN-CODE)).            
                                                                        
           IF SUB1 NOT = 9                                              
               MOVE CF-BENEFIT-DESCRIP (SUB1) TO P-COVERAGE             
           ELSE                                                         
               MOVE 'CD MISSING'   TO P-COVERAGE.                       
                                                                        
       2130-EXIT.                                                       
            EXIT.                                                       
                                                                        
       2135-DUMMY.                                                      
       2135-EXIT.                                                       
            EXIT.                                                       
            EJECT                                                       
       2150-MOVE-TR0-INFO.                                              
           IF NOT RESERVE-EXPENSE-TR                                    
               MOVE  '* TRAILER 0 NOT FIRST, ABORT*' TO WS-PASSED-DATA  
               GO TO 9999-FINALIZE                                      
           ELSE                                                         
               MOVE AT-ITD-PAID-EXPENSES      TO  P-TOT-EXPENSE         
               MOVE AT-INITIAL-MANUAL-RESERVE TO  P-ORIG-MANUAL         
               MOVE AT-ITD-CHARGEABLE-EXPENSE TO  P-CHG-EXPENSE         
               MOVE AT-CURRENT-MANUAL-RESERVE TO  P-REM-MANUAL          
               MOVE AT-ITD-ADDITIONAL-RESERVE TO  P-ADD-RESERVE.        
                                                                        
           IF FULL-PRINT-REQUIRED                                       
               PERFORM  2510-MOVE-OC-HISTORY  THRU 2510-EXIT.           
                                                                        
       2150-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2300-PRINT-TO-LINE-26.                                           

           MOVE  ZEROS  TO WS-PAGE-CNT.                                 
           PERFORM 4013-HEADING-RTN  THRU 4013-EXIT.                    
                                                                        
           MOVE HEAD-LINE-7            TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-9                 TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE HEAD-LINE-10           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-12                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-13                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-14                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-15                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-16                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-17                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-19                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-20                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-21                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-22                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-23                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-23A               TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-25                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE LINE-26                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .                                                            
       2300-EXIT.                                                       
            EXIT.                                                       
           EJECT                                                        
       2500-PROCESS-N-PRINT-TRAILERS.                                   
      **   ADD  1  TO WS-AT-SEQ-NO.                                     
           PERFORM  1750-READNEXT-TRLR  THRU 1750-EXIT.                 
                                                                        
           IF END-OF-TRLR-FILE                                          
               MOVE  'X'  TO  WS-TRAILER-KEY-CHG-SW                     
               GO TO 2500-EXIT.                                         
                                                                        
           MOVE AT-CONTROL-PRIMARY   TO  WS-NEW-AT-CONTROL-PRIMARY.     
           IF WS-NEW-AT-CONTROL-WO-SEQ  NOT =                           
                                        WS-AT-CONTROL-SAVED-WO-SEQ      
               MOVE 'X'  TO WS-TRAILER-KEY-CHG-SW                       
               GO TO 2500-EXIT.                                         
                                                                        
      *TR2                                                              
           IF PAYMENT-TR                                                
               PERFORM 2520-PROCESS-TR2   THRU 2520-EXIT                
               GO TO 2500-EXIT.                                         
      *TR3                                                              
           IF AUTO-PAY-TR          AND                                  
              FULL-PRINT-REQUIRED  AND                                  
              AP-INDEX  LESS THAN 11                                    
               PERFORM 2530-PROCESS-TR3  THRU 2530-EXIT                 
               GO TO 2500-EXIT.                                         
      *TR4                                                              
           IF CORRESPONDENCE-TR                                         
               PERFORM 2540-PROCESS-TR4    THRU 2540-EXIT               
               GO TO 2500-EXIT.                                         
      *TR5                                                              
           IF ADDRESS-TR            AND                                 
              FULL-PRINT-REQUIRED   AND                                 
              AD-INDEX  LESS THAN 60                                    
               PERFORM 2550-PROCESS-TR5    THRU 2550-EXIT               
               GO TO 2500-EXIT.                                         
      *TR6                                                              
           IF GENERAL-INFO-TR                                           
               PERFORM 2560-PROCESS-TR6     THRU 2560-EXIT              
               GO TO 2500-EXIT.                                         
      *TR7                                                              
           IF AUTO-PROMPT-TR                                            
               PERFORM 2570-PROCESS-TR7     THRU 2570-EXIT              
               GO TO 2500-EXIT.                                         
      *TR8                                                              
           IF DENIAL-TR                                                 
               PERFORM 2580-PROCESS-TR8    THRU 2580-EXIT               
               GO TO 2500-EXIT.                                         
      *TR9                                                              
           IF INCURRED-CHG-TR                                           
               PERFORM 2590-PROCESS-TR9    THRU 2590-EXIT               
               GO TO 2500-EXIT.                                         
      *TRA                                                              
           IF FORM-CONTROL-TR                                           
               PERFORM 2600-PROCESS-TRA    THRU 2600-EXIT               
               GO TO 2500-EXIT.                                         
                                                                        
       2500-EXIT.                                                       
            EXIT.                                                       
                                                                        
       2510-MOVE-OC-HISTORY.                                            
           MOVE  1  TO OC-SUB.                                          
           SET  OC-INDEX TO 1.                                          
           PERFORM  2511-LOAD-OC-TABLE  THRU 2511-EXIT  6 TIMES.        
                                                                        
       2510-EXIT.                                                       
            EXIT.                                                       
                                                                        
       2511-LOAD-OC-TABLE.                                              
           IF AT-OPEN-CLOSE-DATE (OC-SUB) = SPACES OR LOW-VALUES        
                ADD 1 TO OC-SUB                                         
                GO TO 2511-EXIT.                                        
                                                                        
           MOVE  'X'  TO END-OC-TABLE-SW.                               
                                                                        
           MOVE AT-OPEN-CLOSE-DATE (OC-SUB)   TO                        
                              OC-TBL-OPCL-DT (OC-INDEX).                
           MOVE AT-OPEN-CLOSE-TYPE (OC-SUB)   TO                        
                              OC-TBL-OPCL-TYPE (OC-INDEX).              
           MOVE AT-OPEN-CLOSE-REASON (OC-SUB) TO                        
                              OC-TBL-OPCL-REASON (OC-INDEX).            
                                                                        
           SET OC-INDEX  UP BY 1.                                       
           ADD 1  TO OC-SUB.                                            
                                                                        
       2511-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2520-PROCESS-TR2.                                                
      ******************************************************************
      ***          BUILD PAYMENT INFORMATION                         ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           IF AT-PAYMENT-TYPE = 'T'
               MOVE 'TRANSFR PMT' TO P-PAY-ACT-TYPE
           ELSE
             IF AT-PAYMENT-TYPE = 'I'
                MOVE 'INTERST PMT' TO P-PAY-ACT-TYPE
             ELSE
                MOVE AT-PAYMENT-TYPE TO WS-SUB
                IF WS-SUB < 1 OR > 6
                    MOVE 2 TO WS-SUB
                END-IF
                MOVE PAY-DESC (WS-SUB) TO P-PAY-ACT-TYPE
             END-IF
           END-IF.
      
           MOVE AT-PAYEES-NAME   TO P-PAY-PAYEE.                        
           MOVE AT-PAYEE-TYPE-CD TO P-PAY-PAYEE-CD.                     
                                                                        
           IF AT-RECORDED-DT  = LOW-VALUES OR SPACES                    
               MOVE SPACE TO P-PAY-PMT-DT                               
           ELSE                                                         
               MOVE  AT-RECORDED-DT      TO DC-BIN-DATE-1               
               MOVE  ' '   TO DC-OPTION-CODE                            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT  TO P-PAY-PMT-DT.               
                                                                        
           MOVE AT-RECORDED-BY  TO P-PAY-BY.                            
                                                                        
           IF AT-CHECK-WRITTEN-DT  = LOW-VALUES OR SPACES               
               MOVE ZEROS TO P-PAY-WRIT-DT                              
           ELSE                                                         
               MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1               
               MOVE  ' '                TO DC-OPTION-CODE               
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-WRIT-DT.              
                                                                        
           MOVE AT-CHECK-NO         TO  P-PAY-CHECK.                    
           MOVE AT-AMOUNT-PAID      TO  P-PAY-PMT-AMT.                  
           MOVE AT-ADDL-RESERVE     TO  P-PAY-RESERVE.                  
           MOVE AT-EXPENSE-PER-PMT  TO  P-PAY-EXPEN.                    
                                                                        
           IF AT-PMT-ACCEPT-DT  = LOW-VALUES OR SPACES                  
               MOVE SPACES             TO P-PAY-CREDIT-DT               
           ELSE                                                         
               MOVE AT-PMT-ACCEPT-DT   TO DC-BIN-DATE-1                 
               MOVE  ' '               TO DC-OPTION-CODE                
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-CREDIT-DT.            
                                                                        
           IF AT-VOID-DT = LOW-VALUES OR SPACES                         
               MOVE SPACES              TO  P-PAY-VOID-DT               
                                            P-PAY-REASON                
           ELSE                                                         
               MOVE AT-VOID-DT          TO  DC-BIN-DATE-1               
               MOVE  ' '                TO DC-OPTION-CODE               
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-VOID-DT               
               MOVE AT-VOID-REASON      TO  P-PAY-REASON.               
                                                                        
           IF AT-PAYMENT-TYPE EQUAL '5' OR '6'                          
              MOVE AT-EXPENSE-TYPE    TO P-EXP-TYPE                     
              GO TO 2521-CONTINUE.                                      
                                                                        
           MOVE AT-PAID-FROM-DT     TO DC-BIN-DATE-1                    
           MOVE  ' '                TO DC-OPTION-CODE                   
           PERFORM 8100-DATE-RTN    THRU 8100-EXIT                      
           MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-FROM-DT                   
                                                                        
           IF NOT PI-USES-PAID-TO                                       
              MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                 
              MOVE  ' '                TO DC-OPTION-CODE                
              PERFORM 8100-DATE-RTN    THRU 8100-EXIT                   
              MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-THRU-DT                
           ELSE                                                         
              MOVE '  TO  - '          TO L-5B-PD-THRU                  
              MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                 
              MOVE +1                  TO DC-ELAPSED-DAYS               
              MOVE +0                  TO DC-ELAPSED-MONTHS             
              MOVE  '6'                TO DC-OPTION-CODE                
              PERFORM 8100-DATE-RTN    THRU 8100-EXIT                   
              MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-THRU-DT.               
                                                                        
           MOVE AT-DAYS-IN-PERIOD   TO  P-PAY-DAYS                      
           MOVE AT-DAILY-RATE       TO  P-PAY-RATE.                     
                                                                        
       2521-CONTINUE.                                                   
                                                                        
           IF ONLINE-MANUAL-PMT                                         
                MOVE 'ONLINE'   TO P-PAY-ORIGIN.                        
           IF ONLINE-AUTO-PMT                                           
                MOVE 'ONLINE'   TO P-PAY-ORIGIN.                        
           IF OFFLINE-PMT                                               
                MOVE 'OFFLINE'  TO P-PAY-ORIGIN.                        
                                                                        
           IF AT-CV-PMT-CODE IS NOT EQUAL TO ' '                        
               GO TO 2522-CONTINUE.                                     
                                                                        
           IF PARTIAL-PAYMENT                                           
                MOVE 'PARTIAL PAYMENT'         TO P-PAY-TYPE.           
           IF FINAL-PAYMENT                                             
                MOVE 'FINAL PAYMENT'           TO P-PAY-TYPE.           
           IF LUMP-SUM-PAYMENT                                          
                MOVE 'LUMP SUM PAYMENT'        TO P-PAY-TYPE.           
           IF ADDITIONAL-PAYMENT                                        
                MOVE 'ADDITIONAL PAYMENT'      TO P-PAY-TYPE.           
           IF CHARGEABLE-EXPENSE                                        
                MOVE 'CHARGEABLE EXPENSE'      TO P-PAY-TYPE.           
           IF NON-CHARGEABLE-EXPENSE                                    
                MOVE 'NON CHARGEABLE EXPENSE'  TO P-PAY-TYPE.           
                                                                        
           MOVE P-PAY-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-4           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           IF AT-PAYMENT-TYPE = '5' OR '6'                              
              MOVE P-PAY-LINE-5C       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           ELSE                                                         
              IF P-PAY-VOID-DT = SPACES                                
                 MOVE P-PAY-LINE-5B    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
              ELSE                                                     
                 MOVE P-PAY-LINE-5A    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
                 MOVE P-PAY-LINE-5B    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
              end-if
           end-if

           GO TO 2520-EXIT.                                             
                                                                        
       2522-CONTINUE.                                                   
                                                                        
           IF AT-CV-PMT-CODE IS EQUAL TO '1'                            
               MOVE 'FULL DEATH PAYMENT'       TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '2'                            
               MOVE 'HALF DEATH PAYMENT'       TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '3'                            
               MOVE 'FULL AD&D PAYMENT'        TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '4'                            
               MOVE 'HALF AD&D PAYMENT'        TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '5'                            
               MOVE 'FULL RIDER PAYMENT'       TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '6'                            
               MOVE 'HALF RIDER PAYMENT'       TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '7'                            
               MOVE 'NON CHARGEABLE EXPENSE'   TO  P-PAY-TYPE.          
           IF AT-CV-PMT-CODE IS EQUAL TO '8'                            
               MOVE 'ADDITIONAL PAYMENT'       TO  P-PAY-TYPE.          
                                                                        
           MOVE P-PAY-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PAY-LINE-4           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           IF AT-CV-PMT-CODE IS EQUAL TO '8'                            
              MOVE P-PAY-LINE-5C       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           ELSE                                                         
              IF P-PAY-VOID-DT IS EQUAL TO SPACES                      
                 MOVE P-PAY-LINE-5B    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
              ELSE                                                     
                 MOVE P-PAY-LINE-5A    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line   
                                       thru 5000-exit
                 MOVE P-PAY-LINE-5B    TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
              end-if
           end-if

           .
       2520-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2530-PROCESS-TR3.                                                
      ******************************************************************
      ***          BUILD AUTO PAYMENT SCHEDULE INFORMATION           ***
      ******************************************************************
                                                                        
           MOVE  'X'                    TO END-AP-TABLE-SW.             
           MOVE AT-RECORDED-DT          TO AP-TBL-EST-DT (AP-INDEX).    
           MOVE AT-RECORDED-BY          TO AP-TBL-EST-BY (AP-INDEX).    
           MOVE AT-TERMINATED-DT        TO AP-TBL-TERM-DT (AP-INDEX).   
           MOVE AT-SCHEDULE-START-DT TO                                 
                             AP-TBL-SCHED-START-DT (AP-INDEX).          
           MOVE AT-SCHEDULE-END-DT      TO                              
                             AP-TBL-SCHED-END-DT (AP-INDEX).            
           MOVE AT-LAST-PMT-TYPE        TO AP-TBL-LAST-TYPE (AP-INDEX). 
           MOVE AT-FIRST-PMT-AMT        TO AP-TBL-FIRST-AMT (AP-INDEX). 
           MOVE AT-DAYS-IN-1ST-PMT      TO AP-TBL-FIRST-DAYS (AP-INDEX).
           MOVE AT-1ST-PAY-THRU-DT      TO AP-TBL-FIRST-DT (AP-INDEX).  
           MOVE AT-REGULAR-PMT-AMT      TO AP-TBL-REG-AMT (AP-INDEX).   
           MOVE AT-INTERVAL-MONTHS      TO AP-TBL-INT-MO (AP-INDEX).    
           SET  AP-INDEX  UP BY 1.                                      
                                                                        
       2530-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2540-PROCESS-TR4.                                                
      ******************************************************************
      ***          BUILD CORRESPONDENCE INFORMATION                  ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           MOVE AT-RECORDED-DT      TO DC-BIN-DATE-1                    
           MOVE  ' '                TO DC-OPTION-CODE                   
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        
           MOVE DC-GREG-DATE-1-EDIT TO  P-LET-LET-DT.                   
           MOVE AT-RECORDED-BY      TO  P-LET-BY.                       
                                                                        
           MOVE AT-ADDRESEE-TYPE           TO P-LET-ADSEE-CD            
           MOVE AT-STD-LETTER-FORM         TO P-LET-FORM.               
           MOVE AT-LETTER-ARCHIVE-NO       TO P-LET-ARCH.               
           MOVE AT-REASON-TEXT             TO P-LET-RE.                 
                                                                        
           IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES                  
               MOVE 'MAIL RCVD ' TO P-LET-ACT-TYPE
               MOVE ZEROS TO P-LET-SENT-DT                              
           ELSE                                                         
               MOVE 'LETTER    ' TO P-LET-ACT-TYPE
               MOVE AT-LETTER-SENT-DT      TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-LET-SENT-DT.              
                                                                        
           IF AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES                 
               MOVE ZEROS TO P-LET-SEND-DT                              
           ELSE                                                         
               MOVE AT-AUTO-RE-SEND-DT     TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-LET-SEND-DT.              
                                                                        
           IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES               
               MOVE SPACE TO P-LET-FOL-DT                               
           ELSE                                                         
               MOVE AT-RECEIPT-FOLLOW-UP     TO DC-BIN-DATE-1           
               MOVE  ' '                     TO DC-OPTION-CODE          
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-LET-FOL-DT.               
                                                                        
           IF AT-LETTER-ANSWERED-DT  = LOW-VALUES OR SPACES             
               MOVE SPACE TO P-LET-ANS-DT                               
           ELSE                                                         
               MOVE AT-LETTER-ANSWERED-DT    TO DC-BIN-DATE-1           
               MOVE  ' '                     TO DC-OPTION-CODE          
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-LET-ANS-DT.               
                                                                        
           MOVE SPACES  TO P-LET-ORIGIN                                 
                           P-LET-ADSEE.                                 
                                                                        
           IF  ONLINE-CREATION                                          
                MOVE 'ONLINE '       TO P-LET-ORIGIN.                   
           IF  OFFLINE-CREATION                                         
                MOVE 'OFFLINE'       TO P-LET-ORIGIN.                   
           IF  INSURED-ADDRESEE                                         
                MOVE 'INSURED'       TO P-LET-ADSEE.                    
           IF  BENEFICIARY-ADDRESEE                                     
                MOVE 'BENEFICIARY'   TO P-LET-ADSEE.                    
           IF  ACCOUNT-ADDRESEE                                         
                MOVE 'ACCOUNT'       TO P-LET-ADSEE.                    
           IF  PHYSICIAN-ADDRESEE                                       
                MOVE 'PHYSICIAN'     TO P-LET-ADSEE.                    
           IF  EMPLOYER-ADDRESEE                                        
                MOVE 'EMPLOYER'      TO P-LET-ADSEE.                    
           IF  OTHER-ADDRESEE-1                                         
                MOVE 'OTHER 1'       TO P-LET-ADSEE.                    
           IF  OTHER-ADDRESEE-2                                         
                MOVE 'OTHER 2'       TO P-LET-ADSEE.                    
                                                                        
           MOVE P-LET-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-LET-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-LET-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-LET-LINE-4           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           IF P-LET-RE = SPACES                                         
              continue
           ELSE                                                         
              MOVE P-LET-LINE-5        TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           .
       2540-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2550-PROCESS-TR5.                                                
      ******************************************************************
      ***          BUILD ADDRESS INFORMATION                         ***
      ******************************************************************
                                                                        
           MOVE  'X'                  TO END-AD-TABLE-SW.               
           MOVE  AT-ADDRESS-TYPE      TO AD-TBL-TYPE    (AD-INDEX).     
           MOVE  AT-MAIL-TO-NAME      TO AD-TBL-NAME    (AD-INDEX).     
           MOVE  AT-ADDRESS-LINE-1    TO AD-TBL-ADDR-1  (AD-INDEX).     
           MOVE  AT-ADDRESS-LINE-2    TO AD-TBL-ADDR-2  (AD-INDEX).     
           MOVE  SPACES               TO AD-TBL-CITY    (AD-INDEX).     
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
           END-STRING
                                                                        
           MOVE SPACES                 TO WS-ZIP-WORK.                  
           IF AT-CANADIAN-POST-CODE                                     
               MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-1               
               MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-2               
           ELSE                                                         
               MOVE AT-ZIP-CODE        TO WS-ZIP-PRIME                  
               IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
                   MOVE '-'            TO WS-ZIP-DASH                   
                   MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4.                 
           MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).     
                                                                        
           MOVE  AT-PHONE-NO          TO AD-TBL-PHONE   (AD-INDEX).     
           SET AD-INDEX  UP BY 1.                                       
                                                                        
       2550-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2560-PROCESS-TR6.                                                
      ******************************************************************
      ***          BUILD CLAIM NOTES INFORMATION                     ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           IF AT-MAINT-NOTE
               MOVE 'MAINT NOTES'     TO P-NOT-ACT-TYPE
           ELSE
             IF AT-CALL-NOTE
                IF AT-PHONE-CALL-IN
                   MOVE 'CALL  IN   '  TO P-NOT-ACT-TYPE
                ELSE
                   MOVE 'CALL  OUT  '  TO P-NOT-ACT-TYPE
                END-IF
             ELSE
               IF AT-CERT-CHANGE
                   MOVE 'CERT CHANGE'  TO P-NOT-ACT-TYPE
               ELSE
                 IF AT-APPROVAL-NOTE
                    MOVE 'APPROVL REV' TO P-NOT-ACT-TYPE
                 ELSE
                   IF AT-NOTE-FILE-NOTE
                      MOVE 'NOTE & FILE' TO P-NOT-ACT-TYPE
                   ELSE
                      MOVE 'NOTE       ' TO P-NOT-ACT-TYPE
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF.
      
           IF AT-SEQUENCE-NO = +90
               MOVE 'DIAGNOSIS  '     TO P-NOT-ACT-TYPE
           END-IF.
      
           IF AT-SEQUENCE-NO = +91
               MOVE 'LOAN INFO  '     TO P-NOT-ACT-TYPE
           END-IF.
      
           IF AT-SEQUENCE-NO = +92
               MOVE 'SPEC REVIEW'     TO P-NOT-ACT-TYPE
           END-IF.
      
           IF AT-SEQUENCE-NO = +93
               MOVE 'VERIFY SSN '     TO P-NOT-ACT-TYPE
           END-IF.
      
           IF AT-SEQUENCE-NO = +94
               MOVE 'CAUSL STATE'     TO P-NOT-ACT-TYPE
           END-IF.
      
           MOVE  AT-INFO-LINE-1       TO P-NOT-TEXT-1.                  
           MOVE  AT-INFO-LINE-2       TO P-NOT-TEXT-2.                  
           MOVE  AT-RECORDED-BY       TO P-NOT-BY.                      
                                                                        
           IF AT-RECORDED-DT = LOW-VALUES OR SPACES                     
               MOVE SPACES TO P-NOT-NOTE-DT                             
           ELSE                                                         
               MOVE  AT-RECORDED-DT    TO DC-BIN-DATE-1                 
               MOVE  ' '               TO DC-OPTION-CODE                
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-NOT-NOTE-DT
           end-if
                                                                        
           IF (AT-SEQUENCE-NO = +95)
              AND (WS-COMP-ID = 'DCC' OR 'VPP')
              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
                 AT-NOTE-ERROR-NO (A1) = SPACES
                 MOVE AT-NOTE-ERROR-NO (A1)
                                       TO EMI-ERROR
                 MOVE '2'           TO EMI-SWITCH1
                 IF AT-NOTE-ERROR-NO (A1) = '1653'
                    EVALUATE TRUE
                       WHEN CL-CLAIM-TYPE = 'L'
                          MOVE '  LF  '
                                    TO EMI-CLAIM-TYPE
                       WHEN CL-CLAIM-TYPE = 'I'
                          MOVE '  IU  '
                                    TO EMI-CLAIM-TYPE
                       WHEN CL-CLAIM-TYPE = 'F'
                          MOVE '  FL  '
                                    TO EMI-CLAIM-TYPE
                       WHEN OTHER
                          MOVE '  AH  '
                                    TO EMI-CLAIM-TYPE
                    END-EVALUATE
                 END-IF
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE EMI-LINE1 (8:64)
                                   TO P-NOT-TEXT-1
                 MOVE SPACES    TO P-NOT-TEXT-2
      
                 MOVE P-NOT-LINE-1   TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
      
                 MOVE P-NOT-LINE-2   TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
      
                 MOVE P-NOT-LINE-3   TO CLM-STAT-EXT-OUT-RECORD
                 perform 5000-write-a-line
                                       thru 5000-exit
      
              END-PERFORM
           END-IF.
                                                                        
           MOVE P-NOT-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
      
           MOVE P-NOT-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
      
           MOVE P-NOT-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .
       2560-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2570-PROCESS-TR7.                                                
      ******************************************************************
      ***          BUILD AUTO PROMPT INFORMATION                     ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           MOVE  AT-PROMPT-LINE-1     TO P-PRO-TEXT-1.                  
           MOVE  AT-PROMPT-LINE-2     TO P-PRO-TEXT-2.                  
           MOVE  AT-RECORDED-BY       TO P-PRO-BY.                      
                                                                        
           IF AT-RECORDED-DT = LOW-VALUES OR SPACES                     
               MOVE SPACES TO P-PRO-NOTE-DT                             
           ELSE                                                         
               MOVE  AT-RECORDED-DT    TO DC-BIN-DATE-1                 
               MOVE  ' '               TO DC-OPTION-CODE                
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-NOTE-DT.              
                                                                        
           IF AT-PROMPT-START-DT = LOW-VALUES OR SPACES                 
               MOVE SPACE               TO P-PRO-START-DT               
           ELSE                                                         
               MOVE AT-PROMPT-START-DT  TO DC-BIN-DATE-1                
               MOVE  ' '                TO DC-OPTION-CODE               
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-START-DT.             
                                                                        
           IF AT-PROMPT-END-DT = LOW-VALUES OR SPACES                   
               MOVE SPACE               TO P-PRO-END-DT                 
           ELSE                                                         
               MOVE AT-PROMPT-END-DT    TO DC-BIN-DATE-1                
               MOVE  ' '                TO DC-OPTION-CODE               
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-END-DT.               
                                                                        
           MOVE P-PRO-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PRO-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-PRO-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .                                                                        
       2570-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2580-PROCESS-TR8.                                                
      ******************************************************************
      ***          BUILD DENIAL INFORMATION                          ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           IF AT-DENIAL-DT = LOW-VALUES OR SPACES                       
               MOVE SPACE               TO P-DEN-DEN-DT                 
           ELSE                                                         
               MOVE AT-DENIAL-DT        TO DC-BIN-DATE-1                
               MOVE  ' '                TO DC-OPTION-CODE               
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO P-DEN-DEN-DT.                
                                                                        
           MOVE AT-RECORDED-BY          TO P-DEN-BY.                    
                                                                        
           IF AT-RETRACTION-DT = LOW-VALUES OR SPACES                   
               MOVE SPACE                  TO P-DEN-RECON-DT            
           ELSE                                                         
               MOVE AT-RETRACTION-DT       TO DC-BIN-DATE-1             
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT TO  P-DEN-RECON-DT.             
                                                                        
           MOVE AT-DENIAL-REASON-CODE      TO P-DEN-CODE.               
           MOVE AT-DENIAL-INFO-1           TO P-DEN-TEXT-1.             
           MOVE AT-DENIAL-INFO-2           TO P-DEN-TEXT-2.             
                                                                        
           MOVE P-DEN-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-DEN-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-DEN-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .
       2580-EXIT.                                                       
            EXIT.                                                       

       2590-PROCESS-TR9.                                                
      ******************************************************************
      ***          BUILD RESERVE/EXPENSE/HISTORY INFORMATION         ***
      ******************************************************************
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           MOVE  AT-OLD-INIT-MAN-RESV      TO P-CHG-INIT-RES.           
           MOVE  AT-OLD-TOTAL-PAID         TO P-CHG-TOT-PD.             
           MOVE  AT-OLD-CURRENT-MAN-RESV   TO P-CHG-CUR-RES.            
           MOVE  AT-OLD-ITD-PAID-EXPENSE   TO P-CHG-TOT-EXP.            
           MOVE  AT-OLD-DAYS-PAID          TO P-CHG-DAYS-PD.            
           MOVE  AT-OLD-ADDL-MAN-RESV      TO P-CHG-ADD-RES.            
           MOVE  AT-OLD-CHARGABLE-EXPENSE  TO P-CHG-CHG-EXP.            
           MOVE  AT-OLD-NO-OF-PMTS         TO P-CHG-PMTS.               
           MOVE  AT-TRAILER-CNT-AT-CHG     TO P-CHG-TOT-TRLRS.          
           MOVE  AT-RECORDED-BY            TO P-CHG-BY.                 
                                                                        
           MOVE  AT-RECORDED-DT            TO DC-BIN-DATE-1             
           MOVE  ' '                       TO DC-OPTION-CODE            
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        
           MOVE DC-GREG-DATE-1-EDIT        TO P-CHG-REC-DT.             
                                                                        
           IF AT-OLD-INCURRED-DT = LOW-VALUES OR SPACES                 
               MOVE ZEROS                  TO P-CHG-INC-DT              
           ELSE                                                         
               MOVE  AT-OLD-INCURRED-DT    TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-INC-DT.             
                                                                        
           IF AT-OLD-PAID-THRU-DT = LOW-VALUES OR SPACES                
              MOVE ZEROS                  TO P-CHG-PAID-TO-DT           
           ELSE                                                         
              IF NOT PI-USES-PAID-TO                                    
                 MOVE  AT-OLD-PAID-THRU-DT   TO DC-BIN-DATE-1           
                 MOVE  ' '                   TO DC-OPTION-CODE          
                 PERFORM 8100-DATE-RTN  THRU 8100-EXIT                  
                 MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-PAID-TO-DT        
              ELSE                                                      
                 MOVE 'PD TO - ' TO L-1-PD-THRU                         
                 MOVE  AT-OLD-PAID-THRU-DT   TO DC-BIN-DATE-1           
                 MOVE +1                     TO DC-ELAPSED-DAYS         
                 MOVE +0                     TO DC-ELAPSED-MONTHS       
                 MOVE  '6'                   TO DC-OPTION-CODE          
                 PERFORM 8100-DATE-RTN  THRU 8100-EXIT                  
                 MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-PAID-TO-DT.       
                                                                        
           IF AT-OLD-REPORTED-DT = LOW-VALUES OR SPACES                 
               MOVE ZEROS TO P-CHG-REP-DT                               
           ELSE                                                         
               MOVE AT-OLD-REPORTED-DT     TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-REP-DT.             
                                                                        
           IF AT-OLD-ESTABLISHED-DT = LOW-VALUES OR SPACES              
               MOVE ZEROS                  TO P-CHG-CREAT-DT            
           ELSE                                                         
               MOVE  AT-OLD-ESTABLISHED-DT TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-CREAT-DT.           

           IF AT-LAST-PMT-MADE-DT = LOW-VALUES OR SPACES                
               MOVE ZEROS TO P-CHG-LAST-PMT-DT                          
           ELSE                                                         
               MOVE  AT-LAST-PMT-MADE-DT   TO DC-BIN-DATE-1             
               MOVE  ' '                   TO DC-OPTION-CODE            
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-LAST-PMT-DT.        

           MOVE P-CHG-LINE-1           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           MOVE P-CHG-LINE-2           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           MOVE P-CHG-LINE-3           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           MOVE P-CHG-LINE-4           TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .                                                                        
       2590-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2600-PROCESS-TRA.                                                
      ******************************************************************
      ***       BUILD CONTINUING CLAIM FORM INFORMATION              ***
      ******************************************************************
                                                                        
           IF WS-LINE-CNT GREATER THAN 50                               
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                
               PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               
                                                                        
           MOVE AT-RECORDED-DT           TO DC-BIN-DATE-1               
           MOVE  ' '                     TO DC-OPTION-CODE              
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        
           MOVE DC-GREG-DATE-1-EDIT      TO P-FORM-LET-DT.              
           MOVE AT-RECORDED-BY           TO P-FORM-BY.                  
           MOVE AT-FORM-ADDRESS          TO P-FORM-ADSEE-CD.            
                                                                        
           IF INITIAL-FORM                                              
              MOVE 'INIT'                TO P-FORM-FORM                 
             ELSE                                                       
              MOVE 'PROG'                TO P-FORM-FORM.                
                                                                        
           IF AT-FORM-SEND-ON-DT = LOW-VALUES OR SPACES                 
               MOVE SPACES                  TO P-FORM-SENT-DT           
           ELSE                                                         
               MOVE AT-FORM-SEND-ON-DT      TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-SENT-DT.         
                                                                        
                                                                        
           IF AT-FORM-RE-SEND-DT = LOW-VALUES OR SPACES                 
               MOVE SPACES                  TO P-FORM-SEND-DT           
           ELSE                                                         
               MOVE AT-FORM-RE-SEND-DT      TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-SEND-DT.         
                                                                        
                                                                        
           IF AT-FORM-FOLLOW-UP-DT = LOW-VALUES OR SPACES               
               MOVE SPACE                   TO P-FORM-FOL-DT            
           ELSE                                                         
               MOVE AT-FORM-FOLLOW-UP-DT    TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-FOL-DT.          
                                                                        
           IF AT-FORM-ANSWERED-DT = LOW-VALUES OR SPACES                
               MOVE SPACE                   TO P-CLM-FORM-ANS-DT        
           ELSE                                                         
               MOVE AT-FORM-ANSWERED-DT     TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO P-CLM-FORM-ANS-DT.       
                                                                        
           MOVE SPACES                      TO P-FORM-LINE-4            
                                                                        
           IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES OR SPACES            
               MOVE SPACES                  TO P-EMP-FORM-ANS-DT        
               MOVE SPACES                  TO P-EMP-FORM-COMM          
           ELSE                                                         
               MOVE '   EMP.  ANSWERED - '  TO P-EMP-FORM-COMM          
               MOVE AT-EMP-FORM-ANSWERED-DT TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO P-EMP-FORM-ANS-DT.       
                                                                        
           IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES OR SPACES            
               MOVE SPACES                  TO P-PHY-FORM-ANS-DT        
               MOVE SPACES                  TO P-PHY-FORM-COMM          
           ELSE                                                         
               MOVE 'PHY.  ANSWERED - '     TO P-PHY-FORM-COMM          
               MOVE AT-PHY-FORM-ANSWERED-DT TO DC-BIN-DATE-1            
               MOVE  ' '                    TO DC-OPTION-CODE           
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT     TO P-PHY-FORM-ANS-DT.       
                                                                        
           MOVE SPACES                      TO P-FORM-ADSEE.            
                                                                        
           IF  FORM-TO-INSURED                                          
                MOVE 'INSURED'              TO P-FORM-ADSEE.            
           IF  FORM-TO-ACCOUNT                                          
                MOVE 'ACCOUNT'              TO P-FORM-ADSEE.            
           IF  FORM-TO-OTHER-1                                          
                MOVE 'OTHER 1'              TO P-FORM-ADSEE.            
           IF  FORM-TO-OTHER-2                                          
                MOVE 'OTHER 2'              TO P-FORM-ADSEE.            
                                                                        
           IF PROGRESS-FORM                                             
              MOVE AT-INSTRUCT-LN-1    TO P-FORM-INSTRUCT               
              MOVE AT-INSTRUCT-LN-2    TO P-FORM-INSTRUCT-1.            
                                                                        
           MOVE P-FORM-LINE-1          TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-FORM-LINE-2          TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-FORM-LINE-3          TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           IF P-FORM-LINE-4 NOT EQUAL SPACES                            
              MOVE P-FORM-LINE-4       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           IF INITIAL-FORM OR AT-INSTRUCT-LN-1 = SPACES                 
              GO TO 2600-CHECK-RELATED-CLAIMS.                          
                                                                        
           MOVE P-FORM-LINE-5          TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           IF AT-INSTRUCT-LN-2 NOT = SPACES                             
              MOVE P-FORM-LINE-6       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           IF AT-INSTRUCT-LN-3 NOT = SPACES                             
              MOVE AT-INSTRUCT-LN-3    TO P-FORM-INSTRUCT-1             
              MOVE P-FORM-LINE-6       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           .
       2600-CHECK-RELATED-CLAIMS.                                       
                                                                        
           IF AT-RELATED-1 NOT = SPACES                                 
              MOVE AT-REL-CLAIM-1      TO P-FORM-CLAIM                  
              MOVE AT-REL-CARR-1       TO P-FORM-CARRIER                
              MOVE AT-REL-CERT-1       TO P-FORM-CERT                   
              MOVE P-FORM-LINE-7       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if
                                                                        
           IF AT-RELATED-2 NOT = SPACES                                 
              MOVE AT-REL-CLAIM-2      TO P-FORM-CLAIM                  
              MOVE AT-REL-CARR-2       TO P-FORM-CARRIER                
              MOVE AT-REL-CERT-2       TO P-FORM-CERT                   
              MOVE P-FORM-LINE-7       TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           .
       2600-EXIT.                                                       
            EXIT.                                                       

       2700-PRINT-OPEN-CLOSE-HISTORY.                                   
           IF OC-TABLE-LOADED  OR                                       
              AD-TABLE-LOADED  OR                                       
              AP-TABLE-LOADED                                           
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 2700-EXIT.                                         
                                                                        
           PERFORM 4013-HEADING-RTN   THRU 4013-EXIT.                   
           PERFORM 1014-HEADING-CONT  THRU 1014-EXIT.                   
                                                                        
           MOVE P-2-LINE-6             TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           MOVE P-2-LINE-8             TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           SET  AP-INDEX  TO 1.                                         
           SET  AD-INDEX  TO 1.                                         
           SET  OC-INDEX  TO 1.                                         
                                                                        
           IF OC-TABLE-LOADED                                           
              PERFORM  2710-PROCESS-OC  THRU 2710-EXIT                 
                 UNTIL  END-OC-TABLE                                     
                 OR OC-INDEX GREATER THAN 6                              
              MOVE SPACES              TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if
                                                                        
           MOVE SPACES TO END-OC-TABLE-SW.                              
                                                                        
           IF AP-TABLE-LOADED                                           
              PERFORM  2720-PROCESS-AP  THRU 2720-EXIT                 
                UNTIL  END-AP-TABLE                                     
                OR AP-INDEX GREATER THAN 10                             
              MOVE SPACES              TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if

           MOVE SPACES TO END-AP-TABLE-SW.                              
                                                                        
           IF AD-TABLE-LOADED                                           
              PERFORM  2730-PROCESS-AD  THRU 2730-EXIT                 
                UNTIL  END-AD-TABLE                                     
                OR AD-INDEX GREATER THAN  60                            
              MOVE SPACES              TO CLM-STAT-EXT-OUT-RECORD
              perform 5000-write-a-line
                                       thru 5000-exit
           end-if
           MOVE SPACES                 TO END-AD-TABLE-SW

           .
       2700-EXIT.                                                       
            EXIT.                                                       
                                                                        
       2710-PROCESS-OC.                                                 
           IF OC-TBL-OPCL-DT (OC-INDEX)  = SPACES OR LOW-VALUES         
               MOVE   'E'  TO END-OC-TABLE-SW                           
               GO TO 2710-EXIT.                                         
                                                                        
           MOVE SPACES    TO P-2-HIS-OPCL.                              
                                                                        
           IF  OC-TBL-OPCL-TYPE (OC-INDEX) = 'O'                        
               MOVE 'OPEN '   TO P-2-HIS-OPCL.                          
           IF  OC-TBL-OPCL-TYPE (OC-INDEX) = 'C'                        
               MOVE 'CLOSE'   TO P-2-HIS-OPCL.                          
                                                                        
           MOVE OC-TBL-OPCL-REASON (OC-INDEX)   TO P-2-HIS-CAUSE.       
                                                                        
           MOVE OC-TBL-OPCL-DT (OC-INDEX) TO DC-BIN-DATE-1.             
           MOVE  ' '                      TO DC-OPTION-CODE.            
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       
           MOVE DC-GREG-DATE-1-EDIT TO  P-2-HIS-DATE.                   
                                                                        
           MOVE P-2-HIS-DETAIL         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit.
                                                                        
           MOVE SPACES  TO  OC-TBL-OPCL-TYPE   (OC-INDEX)               
                            OC-TBL-OPCL-DT     (OC-INDEX)               
                            OC-TBL-OPCL-REASON (OC-INDEX).              
                                                                        
           SET  OC-INDEX   UP BY 1.                                     
                                                                        
       2710-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2720-PROCESS-AP.                                                 
           IF AP-TBL-SCHED-START-DT (AP-INDEX) = SPACES                 
               MOVE   'E'  TO END-AP-TABLE-SW                           
               GO TO 2720-EXIT.                                         
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT.               
                                                                        
           MOVE AP-TBL-EST-BY     (AP-INDEX)    TO P-2-EST-BY.          
           MOVE AP-TBL-FIRST-DAYS (AP-INDEX)    TO P-2-DAYS-1ST.        
           MOVE AP-TBL-FIRST-AMT  (AP-INDEX)    TO P-2-1ST-PMT.         
           MOVE AP-TBL-REG-AMT    (AP-INDEX)    TO P-2-REG-PMT.         
           MOVE AP-TBL-FIRST-DAYS (AP-INDEX)    TO P-2-DAYS-1ST.        
                                                                        
           MOVE AP-TBL-EST-DT (AP-INDEX)        TO DC-BIN-DATE-1.       
           MOVE  ' '                            TO DC-OPTION-CODE.      
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       
           MOVE DC-GREG-DATE-1-EDIT             TO P-2-EST-DT.          
                                                                        
           IF AP-TBL-SCHED-START-DT (AP-INDEX)  = LOW-VALUES OR         
                                                  SPACES                
               MOVE ZEROS TO P-2-EFF-DT                                 
           ELSE                                                         
               MOVE AP-TBL-SCHED-START-DT (AP-INDEX) TO  DC-BIN-DATE-1  
               MOVE  ' '                           TO  DC-OPTION-CODE   
               MOVE +0                             TO  DC-ELAPSED-DAYS  
                                                       DC-ELAPSED-MONTHS
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               IF NO-CONVERSION-ERROR                                   
                   MOVE DC-GREG-DATE-1-EDIT        TO  P-2-EFF-DT       
               ELSE                                                     
                   MOVE SPACES                     TO  P-2-EFF-DT.      
                                                                        
           IF AP-TBL-SCHED-END-DT (AP-INDEX) = LOW-VALUES OR SPACES     
               MOVE ZEROS                          TO  P-2-END-DT       
               IF PI-USES-PAID-TO                                       
                   MOVE 'LAST PAY TO DATE    - '   TO  P-2-LST-PMT-DT   
               ELSE                                                     
                   MOVE 'LAST PAY THRU DATE  - '   TO  P-2-LST-PMT-DT   
           ELSE                                                         
               IF PI-USES-PAID-TO                                       
                   MOVE 'LAST PAY TO DATE    - '   TO  P-2-LST-PMT-DT   
                   MOVE AP-TBL-SCHED-END-DT (AP-INDEX) TO DC-BIN-DATE-1 
                   MOVE '6'                        TO  DC-OPTION-CODE   
                   MOVE +1                         TO  DC-ELAPSED-DAYS  
                   MOVE +0                         TO  DC-ELAPSED-MONTHS
                   PERFORM 8100-DATE-RTN THRU 8100-EXIT                 
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT    TO  P-2-LST-PMT-ON   
                   ELSE                                                 
                       MOVE SPACES                 TO  P-2-LST-PMT-ON   
               ELSE                                                     
                   MOVE 'LAST PAY THRU DATE  - '   TO  P-2-LST-PMT-DT   
                   MOVE AP-TBL-SCHED-END-DT (AP-INDEX) TO DC-BIN-DATE-1 
                   MOVE  ' '                       TO  DC-OPTION-CODE   
                   MOVE +0                         TO  DC-ELAPSED-DAYS  
                                                       DC-ELAPSED-MONTHS
                   PERFORM 8100-DATE-RTN  THRU 8100-EXIT                
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT    TO  P-2-LST-PMT-ON   
                   ELSE                                                 
                       MOVE SPACES                 TO  P-2-LST-PMT-ON.  
                                                                        
           IF AP-TBL-FIRST-DT (AP-INDEX) = LOW-VALUES OR SPACES         
               MOVE SPACES                         TO  P-2-1ST-PMT-ON   
               IF PI-USES-PAID-TO                                       
                   MOVE 'FIRST PAY TO DATE   - '   TO  P-2-1ST-PMT-DT   
               ELSE                                                     
                   MOVE 'FIRST PAY THRU DATE - '   TO  P-2-1ST-PMT-DT   
           ELSE                                                         
               IF PI-USES-PAID-TO                                       
                   MOVE 'FIRST PAY TO DATE   - '   TO  P-2-1ST-PMT-DT   
                   MOVE AP-TBL-FIRST-DT (AP-INDEX) TO  DC-BIN-DATE-1    
                   MOVE '6'                        TO  DC-OPTION-CODE   
                   MOVE +1                         TO  DC-ELAPSED-DAYS  
                   MOVE +0                         TO  DC-ELAPSED-MONTHS
                   PERFORM 8100-DATE-RTN THRU 8100-EXIT                 
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT    TO  P-2-1ST-PMT-ON   
                   ELSE                                                 
                       MOVE SPACES                 TO  P-2-1ST-PMT-ON   
               ELSE                                                     
                   MOVE 'FIRST PAY THRU DATE - '   TO  P-2-1ST-PMT-DT   
                   MOVE AP-TBL-FIRST-DT (AP-INDEX) TO  DC-BIN-DATE-1    
                   MOVE  ' '                       TO  DC-OPTION-CODE   
                   MOVE +0                         TO  DC-ELAPSED-DAYS  
                                                       DC-ELAPSED-MONTHS
                   PERFORM 8100-DATE-RTN  THRU 8100-EXIT                
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-GREG-DATE-1-EDIT    TO  P-2-1ST-PMT-ON   
                   ELSE                                                 
                       MOVE SPACES                 TO  P-2-1ST-PMT-ON.  
                                                                        
           IF AP-TBL-TERM-DT (AP-INDEX) = LOW-VALUES OR SPACES          
               MOVE SPACES                    TO P-2-END-DT             
           ELSE                                                         
               MOVE AP-TBL-TERM-DT (AP-INDEX) TO DC-BIN-DATE-1          
               MOVE +0                        TO DC-ELAPSED-DAYS        
                                                 DC-ELAPSED-MONTHS      
               MOVE  ' '                      TO DC-OPTION-CODE         
               PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    
               MOVE DC-GREG-DATE-1-EDIT       TO P-2-END-DT.            
                                                                        
           MOVE AP-TBL-INT-MO  (AP-INDEX)    TO P-2-MOS-BET.            
                                                                        
           MOVE  SPACES   TO  P-2-PAYEE.                                
                                                                        
           IF  INSURED-PAID-AUTO                                        
               MOVE  'INSURED PAID AUTO'  TO  P-2-PAYEE.                
           IF  BENEFICIARY-PAID-AUTO                                    
               MOVE  'BENEF-Y PAID AUTO'  TO  P-2-PAYEE.                
           IF  ACCOUNT-PAID-AUTO                                        
               MOVE  'ACCOUNT PAID AUTO'  TO  P-2-PAYEE.                
           IF  OTHER-1-PAID-AUTO                                        
               MOVE  'OTHER-1 PAID AUTO'  TO  P-2-PAYEE.                
           IF  OTHER-2-PAID-AUTO                                        
               MOVE  'OTHER-2 PAID AUTO'  TO  P-2-PAYEE.                
                                                                        
           IF  AP-TBL-LAST-TYPE (AP-INDEX)  = '1'                       
               MOVE  'YES'   TO P-2-LAST-FINAL                          
           ELSE                                                         
               MOVE  'NO '   TO P-2-LAST-FINAL                          
                                                                        
           MOVE P-2-AUT-LINE-1         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-2         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-3         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-4         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-5         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-6         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-7         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-AUT-LINE-8         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE SPACES TO AP-TBL-EST-DT         (AP-INDEX)              
                          AP-TBL-EST-BY         (AP-INDEX)              
                          AP-TBL-SCHED-START-DT (AP-INDEX)              
                          AP-TBL-SCHED-END-DT   (AP-INDEX)              
                          AP-TBL-TERM-DT        (AP-INDEX)              
                          AP-TBL-LAST-TYPE      (AP-INDEX)              
                          AP-TBL-FIRST-DT       (AP-INDEX)              
                          AP-TBL-INT-MO         (AP-INDEX).             
                                                                        
            MOVE ZEROS  TO AP-TBL-FIRST-AMT     (AP-INDEX)              
                           AP-TBL-FIRST-DAYS    (AP-INDEX)              
                           AP-TBL-REG-AMT       (AP-INDEX)              
                           AP-TBL-REG-MO        (AP-INDEX).             
                                                                        
           SET  AP-INDEX   UP BY 1.                                     
                                                                        
       2720-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       2730-PROCESS-AD.                                                 

           IF AD-TBL-TYPE (AD-INDEX) = SPACES                           
               MOVE   'E'  TO END-AD-TABLE-SW                           
               GO TO 2730-EXIT.                                         
                                                                        
           IF  WS-LINE-CNT  GREATER THAN  50                            
               PERFORM  4013-HEADING-RTN  THRU 4013-EXIT.               
                                                                        
           MOVE AD-TBL-TYPE   (AD-INDEX)     TO P-2-ADD-CODE.           
           MOVE AD-TBL-NAME   (AD-INDEX)     TO P-2-ADD-NAME.           
           MOVE AD-TBL-ADDR-1 (AD-INDEX)     TO P-2-ADD-ADDR-1.         
           MOVE AD-TBL-ADDR-2 (AD-INDEX)     TO P-2-ADD-ADDR-2.         
           MOVE AD-TBL-CITY   (AD-INDEX)     TO P-2-ADD-CITY.           
           MOVE AD-TBL-ZIP    (AD-INDEX)     TO P-2-ADD-ZIP.            
           MOVE AD-TBL-PHONE  (AD-INDEX)     TO WS-PHONE-BRKDN.         
           MOVE WS-PH-1                      TO WS-PH-ED-1.             
           MOVE WS-PH-2                      TO WS-PH-ED-2.             
           MOVE WS-PH-3                      TO WS-PH-ED-3.             
           MOVE WS-PHONE-EDIT                TO P-2-ADD-PHONE.          
                                                                        
           IF AD-TBL-TYPE  (AD-INDEX)   = 'I'                           
               MOVE 'INSURED'        TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'B'                           
               MOVE 'BENEFICIARY'    TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'A'                           
               MOVE 'ACCOUNT'        TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'P'                           
               MOVE 'PHYSICIAN'      TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'E'                           
               MOVE 'EMPLOYER'       TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'O'                           
               MOVE 'OTHER 1'        TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = 'Q'                           
               MOVE 'OTHER 2'        TO P-2-ADD-TYPE.                   
           IF AD-TBL-TYPE  (AD-INDEX)   = '8'                           
              MOVE 'FROM ACCT MSTR'   TO P-2-ADD-TYPE.             
           IF AD-TBL-TYPE  (AD-INDEX)   = '9'                           
               MOVE 'FROM BENE MSTR'    TO P-2-ADD-TYPE.                
                                                                        
           MOVE P-2-ADD-LINE-1         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-ADD-LINE-2         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-ADD-LINE-3         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-ADD-LINE-4         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-ADD-LINE-5         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE P-2-ADD-LINE-6         TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE  SPACES  TO AD-TBL-TYPE   (AD-INDEX)                    
                            AD-TBL-NAME   (AD-INDEX)                    
                            AD-TBL-ADDR-1 (AD-INDEX)                    
                            AD-TBL-ADDR-2 (AD-INDEX)                    
                            AD-TBL-CITY   (AD-INDEX).                   
            MOVE ZEROS TO   AD-TBL-ZIP    (AD-INDEX)                    
                            AD-TBL-PHONE  (AD-INDEX).                   
                                                                        
           SET  AD-INDEX   UP BY 1.                                     
                                                                        
       2730-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
       4000-INITIALIZE.                                                 
           MOVE SPACES TO   END-OC-TABLE-SW                             
                            END-AD-TABLE-SW                             
                            END-AP-TABLE-SW
                            PI-CLAIM-PAID-THRU-TO.
                                                                        
           MOVE SAVE-DATE         TO HEAD-RUN-DATE.                     
                                                                        
           MOVE 0  TO WS-PAGE-CNT.                                      
                                                                        
           MOVE +80  TO WS-LINE-LEN.                                    
                                                                        
           SET  AP-INDEX   TO 1.                                        
           SET  AD-INDEX   TO 1.                                        
           SET  OC-INDEX   TO 1.                                        
                                                                        
           PERFORM 4001-CLEAR-OC-TABLE  6   TIMES.                      
           PERFORM 4002-CLEAR-AP-TABLE  10  TIMES.                      
           PERFORM 4003-CLEAR-AD-TABLE  60  TIMES.                      
                                                                        
           MOVE SAVE-DATE     TO DC-GREG-DATE-1-EDIT                    
                                 HEAD-RUN-DATE.                         
                                                                        
           MOVE '2' TO DC-OPTION-CODE.                                  
           PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       
           MOVE DC-GREG-DATE-1-ALPHA  TO WS-UNALIGNED-FIELD.            
           MOVE DC-BIN-DATE-1         TO WS-BIN-CURRENT-DT              
           MOVE +18 TO WS-NAME-LENGTH.                                  
           PERFORM ELCALGNP              THRU ELCALGNP-EXIT.            
           MOVE WS-ALIGNED-FIELD      TO HEAD-RUN-DATE-FULL.            

      *    if ws-kix-myenv = 'cid1p'
      *       move '/data/seqfiles/'   to ws-work-dir
      *       move 'cd /apps/prod/cid1p/jcl'
      *                                to tran-data-line1
020816*       move ws-kix-myenv        to ws-work-env
      *    else
      *       move ws-kix-myenv        to ws-work-env
      **      string
      **         '/data/test/' delimited by size
      **         ws-work-env   delimited by space
      **         '/seqfiles/'  delimited by size
      **            into ws-work-dir
      **      end-string
      *       display ' work dir **' ws-work-dir '**'
      *       string
      *          'cd /apps/test/' delimited by size
      *          ws-work-env      delimited by space
      *          '/jcl'           delimited by size
      *             into tran-data-line1
      *       end-string
      *       display ' line 1 **' tran-data-line1 '**'
      *    end-if

           move '/export/home/mtpadmin/ftpfiles/'
                                       to ws-work-dir
           move function upper-case(ws-comp-id)
                                       to ws-work-comp-id
           move 'cilg201b'             to ws-work-job-name

           move spaces                 to ws-file-name

           string
              soc-client-comp-id
              soc-client-car
              soc-client-claim-no
              soc-client-cert-no(1:10)
              soc-client-proc-id
              '.txt' delimited by size into ws-work-rpt-out
           end-string
      *    display ' work rpt **' ws-work-rpt-out '**'

           string
              ws-work-dir delimited by space
              ws-work-rpt-out delimited by size
                 into ws-file-name
           end-string
      *    display ' out file **' ws-file-name '**'
           
           move ws-file-name to rm-filename
      *    display ' rm string **' ws-rm-string '**'
           call "SYSTEM" using ws-rm-string
              returning ws-rm-return-cd
      *    display ' rm return code ' ws-rm-return-cd

           move spaces to ws-return-stuff
           string
              ' Here is the file '
              ws-file-name
              delimited by size into ws-return-stuff
           end-string
           string
              'unikixjob '     delimited by size
              ws-work-job-name delimited by size
              ' -k '           delimited by size
              ws-work-env      delimited by space
                 into tran-data-line2
           end-string

      *     display ' svr    ' svr
      *     display ' user   ' usr
      *     display ' pw     ' pass
      *     display ' input  ' ws-written-in
      *     display ' output ' ws-written-rpt
      *     display '        '
      *     display ' line 1 ' tran-data-line1
      *     display ' line 2 ' tran-data-line2

           open output CLM-STAT-EXT-OUT

           if ws-ext-file-status not = '00'
              display 'error open on output ' ws-ext-file-status
              move ' Invalid or missing file '
                                       to ws-return-stuff
              go to 0000-end
           end-if

           .
       4000-EXIT.                                                       
            EXIT.                                                       
                                                                        
       4001-CLEAR-OC-TABLE.                                             
           MOVE SPACES  TO  OC-TBL-OPCL-DT     (OC-INDEX)               
                            OC-TBL-OPCL-TYPE   (OC-INDEX)               
                            OC-TBL-OPCL-REASON (OC-INDEX).              
           SET  OC-INDEX   UP BY 1.                                     
                                                                        
       4001-EXIT.                                                       
            EXIT.                                                       
                                                                        
       4002-CLEAR-AP-TABLE.                                             
           MOVE SPACES TO AP-TBL-EST-DT         (AP-INDEX)              
                          AP-TBL-EST-BY         (AP-INDEX)              
                          AP-TBL-SCHED-START-DT (AP-INDEX)              
                          AP-TBL-SCHED-END-DT   (AP-INDEX)              
                          AP-TBL-TERM-DT        (AP-INDEX)              
                          AP-TBL-LAST-TYPE      (AP-INDEX)              
                          AP-TBL-FIRST-DT       (AP-INDEX)              
                          AP-TBL-INT-MO         (AP-INDEX).             
                                                                        
            MOVE ZEROS  TO AP-TBL-FIRST-AMT     (AP-INDEX)              
                           AP-TBL-FIRST-DAYS    (AP-INDEX)              
                           AP-TBL-REG-AMT       (AP-INDEX)              
                           AP-TBL-REG-MO        (AP-INDEX).             
                                                                        
           SET  AP-INDEX   UP BY 1.                                     
                                                                        
       4002-EXIT.                                                       
            EXIT.                                                       
                                                                        
       4003-CLEAR-AD-TABLE.                                             
           MOVE  SPACES  TO AD-TBL-TYPE   (AD-INDEX)                    
                            AD-TBL-NAME   (AD-INDEX)                    
                            AD-TBL-ADDR-1 (AD-INDEX)                    
                            AD-TBL-ADDR-2 (AD-INDEX)                    
                            AD-TBL-CITY   (AD-INDEX).                   
            MOVE ZEROS TO   AD-TBL-ZIP    (AD-INDEX)                    
                            AD-TBL-PHONE  (AD-INDEX).                   
                                                                        
           SET  AD-INDEX   UP BY 1.                                     
                                                                        
       4003-EXIT.                                                       
            EXIT.                                                       
                                                                        
       4013-HEADING-RTN.                                                

           move +1                     to ws-line-cnt
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT            TO HEAD-PAGE-NO

           if ws-page-cnt > 1
              move X'0C'               to clm-stat-ext-out-record
              perform 5000-write-a-line thru 5000-exit
           end-if

           MOVE HEAD-LINE-2            TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           MOVE HEAD-LINE-3            TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE HEAD-LINE-4            TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .                                                                        
       4013-EXIT.                                                       
            EXIT.                                                       
                                                                        
       1014-HEADING-CONT.                                               

           MOVE LINE-25                TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
                                                                        
           MOVE  LINE-26               TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           .
       1014-EXIT.                                                       
            EXIT.                                                       
                                                                        
       5000-write-a-line.
           write CLM-STAT-EXT-OUT-RECORD

           add 1 to ws-line-cnt

           .
       5000-exit.
           exit.

            EJECT                                                       
      *PRINT-RTN.  COPY ELPRTCVP.                                       
       PRINT-RTN.
      *    COPY ELPRTCVP.
                                                                        
           EJECT                                                        
       8100-DATE-RTN.                                                   
           EXEC CICS LINK                                               
                  PROGRAM  ('ELDATCV')                                  
                  COMMAREA (DATE-CONVERSION-DATA)                       
                  LENGTH   (DC-COMM-LENGTH)                             
           END-EXEC.                                                    
                                                                        
       8100-EXIT.                                                       
            EXIT.                                                       
                                                                        
       8300-ABEND.                                                      
           MOVE SPACES                 TO WS-PASSED-DATA.               
           MOVE DFHEIBLK               TO WS-PASSED-DATA.               
           EXEC CICS LINK                                               
               PROGRAM   ('EL004')                                      
               COMMAREA  (WS-PASSED-DATA)                               
               LENGTH    (72)                                           
           END-EXEC.                                                    

           perform 5000-write-a-line   thru 5000-exit                                                                        
           GO TO 9999-FINALIZE.                                         
                                                                        
       8900-PGMIDERR.                                                   
           MOVE '* PROG NOT FOUND, NOTIFY DATA PROCESSING *'            
                               TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
           GO TO 9999-FINALIZE.                                         
                                                                        
       8900-EXIT.                                                       
           EJECT                                                        
                                                                        
       9800-LINK-REM-TERM.                                              
           EXEC CICS LINK                                               
               PROGRAM    ('ELRTRM')                                    
               COMMAREA   (CALCULATION-PASS-AREA)                       
               LENGTH     (CP-COMM-LENGTH)                              
               END-EXEC.                                                
                                                                        
       9800-EXIT.                                                       
            EXIT.                                                       
                                                                        
       9900-ERROR-FORMAT.
            IF NOT EMI-ERRORS-COMPLETE
                EXEC CICS LINK
                    PROGRAM    ('EL001')
                    COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
                    LENGTH     (EMI-COMM-LENGTH)
                END-EXEC.
      
       9900-EXIT.
            EXIT.
      
       1000-send-buffer.
      
           move ws-return-stuff        to ws-send-buf
      *    display 'SOCK10:About to send      '
      *    display 'SOCK10:sequence number  =', ws-seq-num.
      *    display ' msg size ' ws-send-msg-size
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
      
           if return-code <= zero
              display 'SOCK10:send error ',
              go to 1000-socket-error
           end-if
           go to 1000-exit
      
           .
       1000-socket-error.
           if ws-seq-num <> 0
              display "SOCK10:did not complete"
           end-if
      
           .
       1000-exit.
           exit.
      
       1100-close-socket.
      
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK10:done'
           exec cics return end-exec.
           goback
      
           .
       1100-exit.
           exit.

       9999-FINALIZE.                                                   
                                                                        
           move spaces TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit
           MOVE 'X'  TO WS-PROG-END.                                    
           move spaces TO CLM-STAT-EXT-OUT-RECORD
           perform 5000-write-a-line   thru 5000-exit

           goback

           .
       9999-EXIT.                                                       
           GOBACK.                                                      
