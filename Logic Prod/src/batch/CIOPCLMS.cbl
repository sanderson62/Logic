       IDENTIFICATION  DIVISION.                                        
LGC005 PROGRAM-ID.     CIOPCLMS.                                        
       AUTHOR.         CSO.                                             
       INSTALLATION.   CENTRAL STATES HEALTH & LIFE CO. OF OMAHA        
       DATE-WRITTEN.   06/24/88.                                        
       DATE-COMPILED.                                                   
       SECURITY.       COMPANY PRIVATE.                                 
      *REMARKS.                                                         
LGC005******************************************************************
LGC005*****    THIS PROGRAM WAS COPIED COMPLETELY FROM OPECLMS.    *****
LGC005*****    ANY CHANGES MADE TO OPECLMS CAN BE IDENTIFED BY     *****
LGC005*****    LGC005 AT THE BEGINNING OF EACH STATEMENT.          *****
      ******************************************************************
      *                        IMPORTANT                               *
      *            THE EXTRACT-FILE MUST BE                            *
      *        SORTED INTO FA, FB, AND FC ORDER.                       *
      *                        IMPORTANT                               *
      *                                                                *
      * PROGRAM CHANGES:                                               *
      *                                                                *
      * DATE INIT DESCRIPTION                                          *
      * 05/01/1997 CEVA COBOL II CONVERSION                            *
      * 12/13/1997 DJNA Y2K PHASE 2                                    *
      * 08/26/2000 DJNA DATA EXPANSION                                 *
      ******************************************************************
       ENVIRONMENT     DIVISION.                                        
       CONFIGURATION       SECTION.                                     
       SOURCE-COMPUTER.        IBM-370.                                 
       OBJECT-COMPUTER.        IBM-370.                                 
      ******************************************************************
       INPUT-OUTPUT        SECTION.                                     
       FILE-CONTROL.                                                    
LGC005     SELECT ELBENE         ASSIGN TO        ELBENE                
                                 ORGANIZATION    IS  INDEXED            
                                 ACCESS     MODE IS  RANDOM             
LGC005               RECORD      KEY IS  BE-CONTROL-PRIMARY             
                                 FILE     STATUS IS  BENE-STATUS.       
LGC005     SELECT ELCNTL         ASSIGN TO        ELCNTL                
                                 ORGANIZATION    IS  INDEXED            
                                 ACCESS     MODE IS  DYNAMIC            
LGC005               RECORD      KEY IS  CF-CONTROL-PRIMARY             
                                 FILE     STATUS IS  CF-STATUS.         
           SELECT CSPLNCD        ASSIGN TO        CSPLNCD               
                                 ORGANIZATION    IS  INDEXED            
                                 ACCESS     MODE IS  DYNAMIC            
LGC005               RECORD      KEY IS  CSP-PC-PLAN-CODE-KEY           
                                 FILE     STATUS IS  PC-STATUS.         
           SELECT EXTRACT-FILE   ASSIGN TO      S-SYS015                
                                 FILE     STATUS IS  EX-STATUS.         
           SELECT LG-CLM-ACT     ASSIGN TO        CLMACT.               
           EJECT                                                        
       DATA            DIVISION.                                        
       FILE                SECTION.                                     
LGC005******************************************************************
LGC005 FD  ELBENE                                                       
LGC005         RECORD      CONTAINS 500 CHARACTERS.                     
       COPY ELCBENE.                                                    
LGC005                                                                  
LGC005******************************************************************
LGC005 FD  ELCNTL                                                       
LGC005         RECORD      CONTAINS 750 CHARACTERS.                     
       COPY ELCCNTL.                                                    
LGC005                                                                  
LGC005******************************************************************
       FD  CSPLNCD.                                                     
LGC005 01  CSP-PLAN-CODE-RECORD.                                        
           05  FILLER                  PIC X(3).                        
LGC005     05  CSP-PC-HOME-OFFICE-CAT  PIC XXX.                         
LGC005     05  CSP-PC-ANNUAL-STATE-CAT                                  
                                       PIC XXX.                         
           05  FILLER                  PIC X(19).                       
LGC005     05  CSP-PC-PLAN-ID          PIC X(3).                        
           05  FILLER                  PIC X(27).                       
LGC005     05  CSP-PC-OPER-COMPANY         PIC XX.                      
           05  FILLER                  PIC X(9).                        
LGC005     05  CSP-PC-BIZ-CODE         PIC X.                           
LGC005     05  CSP-PC-CLAIM-TYPE       PIC X.                           
           05  FILLER                  PIC XXX.                         
      ******************************************************************
      * NOTE PC-KEY-PT1 = THE PLAN CODE                                *
      ******************************************************************
LGC005     05  CSP-PC-PLAN-CODE-KEY.                                    
LGC005         10  CSP-PC-KEY-PT1      PIC XXX.                         
LGC005         10  CSP-PC-KEY-PT2      PIC XXX.                         
LGC005                                                                  
LGC005                                                                  
LGC005******************************************************************
LGC005 FD  EXTRACT-FILE                                                 
               RECORDING   MODE     IS  F                               
LGC005         RECORD      CONTAINS 314 CHARACTERS                      
LGC005         BLOCK       CONTAINS 000 RECORDS                         
LGC005         LABEL       RECORDS  ARE  STANDARD.                      
       COPY  ELCEXTR.                                                   
LGC005                                                                  
           EJECT                                                        
      ******************************************************************
       FD  LG-CLM-ACT                                                   
               RECORDING   MODE     IS  F                               
               LABEL       RECORDS  ARE  STANDARD.                      
       01  CLAIM-ACTIVITY-RECORD       PIC X(675).                      
           EJECT                                                        
      ******************************************************************
       WORKING-STORAGE     SECTION.                                     
       77  FILLER                      PIC X(37)          VALUE         
LGC005                         'CIOPCLMS  WORKING-STORAGE BEGINS HERE'. 
LGC005 77  A-SW                        PIC X         VALUE 'N'.         
      ******************************************************************
       01  FILLER                      PIC X(15)          VALUE         
                                                     '***************'. 
       01  FILLER                      PIC X(15)          VALUE         
                                                     'WORKING STORAGE'. 
       01  FILLER                      PIC X(15)          VALUE         
                                                     '***************'. 
      ******************************************************************
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '--FA-EXTRACT---'. 
LGC005 01  FA-EXTRACT.                                                  
LGC005     05  FA-RECORD-ID                        PIC XX.              
LGC005     05  FA-SORT-KEY-AREAS.                                       
LGC005         10  FA-POSITIONING-CODE             PIC X.               
LGC005         10  FA-EXTRACT-CODE                 PIC X.               
LGC005         10  FA-COMPANY-CD                   PIC X.               
LGC005         10  FA-COMPANY-ID                   PIC XXX.             
LGC005         10  FA-RECORD-TYPE                  PIC X.               
LGC005         10  FA-SORT-KEY-G.                                       
LGC005             15  FA-SG-CARRIER               PIC X.               
LGC005             15  FA-SG-STATE                 PIC XX.              
LGC005             15  FA-SG-ACCOUNT-NO            PIC X(10).           
LGC005             15  FA-SG-CLAIM-NO              PIC X(7).            
LGC005             15  FA-SG-CERT-NO.                                   
LGC005                 20  FA-SG-CERT-PRIME        PIC X(10).           
LGC005                 20  FA-SG-CERT-SFX          PIC X.               
LGC005             15  FA-SG-TRAILER-SEQ-NO        PIC S9(4)    COMP.   
LGC005******************************************************************
LGC005     05  FA-EXTRACT-RECORD.                                       
LGC005         10  FA-INSURED-NAME              PIC X(30).              
LGC005         10  FA-ACCOUNT-NAME              PIC X(30).              
LGC005         10  FA-INSURED-BIRTH-DT          PIC XX.                 
LGC005         10  FA-INSURED-SEX-CD            PIC X.                  
LGC005         10  FA-INSURED-OCC-CD            PIC XX.                 
LGC005         10  FA-SOC-SEC-NO                PIC X(11).              
LGC005         10  FA-PROCESSOR-ID              PIC X(4).               
LGC005         10  FA-PROCESSING-INFO.                                  
LGC005             15  FA-CLAIM-STATUS         PIC X.                   
LGC005             15  FA-CLAIM-TYPE           PIC X.                   
LGC005             15  FA-CLAIM-PREM-TYPE      PIC X.                   
LGC005             15  FA-INCURRED-DT          PIC XX.                  
LGC005             15  FA-REPORTED-DT          PIC XX.                  
LGC005             15  FA-FILE-ESTABLISH-DT    PIC XX.                  
LGC005             15  FA-EST-END-OF-DISAB-DT  PIC XX.                  
LGC005             15  FA-LAST-PMT-DT          PIC XX.                  
LGC005             15  FA-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.    
LGC005             15  FA-PAID-THRU-DT         PIC XX.                  
LGC005             15  FA-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.    
LGC005             15  FA-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.    
LGC005             15  FA-NO-OF-DAYS-PAID      PIC S9(4)     COMP.      
LGC005             15  FA-PMT-CALC-METHOD      PIC X.                   
LGC005             15  FA-CAUSE-CD             PIC X(6).                
LGC005             15  FA-DIAGNOSIS-DESCRIP    PIC X(26).               
LGC005             15  FA-LAST-REOPEN-DT       PIC XX.                  
LGC005             15  FA-LAST-CLOSE-DT        PIC XX.                  
LGC005             15  FA-LAST-CLOSE-REASON    PIC X.                   
LGC005         10  FA-CERTIFICATE-DATA.                                 
LGC005             15  FA-CERT-ORIGIN          PIC X.                   
LGC005         10  FA-STATUS-CONTROLS.                                  
LGC005             15  FA-PRIORITY-CD          PIC X.                   
LGC005             15  FA-SUPV-ATTN-CD         PIC X.                   
LGC005             15  FA-PURGED-DT            PIC XX.                  
LGC005             15  FA-RESTORED-DT          PIC XX.                  
LGC005             15  FA-NEXT-AUTO-PAY-DT     PIC XX.                  
LGC005             15  FA-NEXT-RESEND-DT       PIC XX.                  
LGC005             15  FA-NEXT-FOLLOWUP-DT     PIC XX.                  
LGC005             15  FILLER                     PIC XX.               
LGC005             15  FA-LAST-MAINT-DT        PIC XX.                  
LGC005             15  FA-LAST-MAINT-USER      PIC X(4).                
LGC005             15  FA-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.    
LGC005             15  FA-LAST-MAINT-TYPE      PIC X.                   
LGC005             15  FA-RELATED-CLAIM-NO     PIC X(7).                
LGC005             15  FA-HISTORY-ARCHIVE-DT   PIC XX.                  
LGC005         10  FA-TRAILER-CONTROLS.                                 
LGC005             15  FA-TRAILER-SEQ-CNT      PIC S9(4)     COMP.      
LGC005             15  FA-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.      
LGC005             15  FILLER                     PIC XX.               
LGC005             15  FA-AUTO-PAY-SEQ         PIC S9(4)     COMP.      
LGC005             15  FA-ACCOUNT-ADDR-CNT     PIC S9(01).              
LGC005             15  FILLER                  PIC X.                   
LGC005         10  FA-FILE-LOCATION            PIC X(4).                
LGC005         10  FA-BENEFICIARY              PIC X(10).               
LGC005         10  FILLER                         PIC X(66).            
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---------------'. 
LGC005**********************************************************        
LGC005**********************************************************        
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '--FB-EXTRACT---'. 
LGC005 01  FB-EXTRACT.                                                  
LGC005     05  FB-RECORD-ID                        PIC XX.              
LGC005     05  FB-SORT-KEY-AREAS.                                       
LGC005         10  FB-POSITIONING-CODE             PIC X.               
LGC005         10  FB-EXTRACT-CODE                 PIC X.               
LGC005         10  FB-COMPANY-CD                   PIC X.               
LGC005         10  FB-COMPANY-ID                   PIC XXX.             
LGC005         10  FB-RECORD-TYPE                  PIC X.               
LGC005         10  FB-SORT-KEY-G.                                       
LGC005             15  FB-SG-CARRIER               PIC X.               
LGC005             15  FB-SG-STATE                 PIC XX.              
LGC005             15  FB-SG-ACCOUNT-NO            PIC X(10).           
LGC005             15  FB-SG-CLAIM-NO              PIC X(7).            
LGC005             15  FB-SG-CERT-NO.                                   
LGC005                 20  FB-SG-CERT-PRIME        PIC X(10).           
LGC005                 20  FB-SG-CERT-SFX          PIC X.               
LGC005             15  FB-SG-TRAILER-SEQ-NO        PIC S9(4)    COMP.   
LGC005******************************************************************
LGC005     05  FB-EXTRACT-RECORD.                                       
LGC005         10  FB-INSURED-NAME              PIC X(30).              
LGC005         10  FB-EFFECTIVE-DT              PIC XX.                 
LGC005         10  FB-SSN                       PIC X(11).              
LGC005         10  FB-MEMBER-NO                 PIC X(12).              
LGC005         10  FB-INSURED-ISSUE-AGE         PIC 99.                 
LGC005         10  FB-SPOUSE-ISSUE-AGE          PIC 99.                 
LGC005         10  FB-INSURED-SEX               PIC X.                  
LGC005         10  FB-LIFE-DATA.                                        
LGC005             15  FB-LF-BENEFIT-CD        PIC XX.                  
LGC005             15  FB-LF-ORIG-TERM         PIC S9(3)     COMP-3.    
LGC005             15  FB-LF-BENEFIT-AMT       PIC S9(9)V99  COMP-3.    
LGC005             15  FB-LF-PREMIUM-AMT       PIC S9(7)V99  COMP-3.    
LGC005             15  FB-LF-REMAINING-AMT     PIC S9(9)V99  COMP-3.    
LGC005             15  FB-LF-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3.    
LGC005             15  FB-LF-ITD-DEATH-AMT     PIC S9(7)V99  COMP-3.    
LGC005             15  FB-LF-DEV-CODE          PIC X(03).               
LGC005         10  FB-AH-DATA.                                          
LGC005             15  FB-AH-BENEFIT-CD        PIC XX.                  
LGC005             15  FB-AH-ORIG-TERM         PIC S9(3)     COMP-3.    
LGC005             15  FB-AH-BENEFIT-AMT       PIC S9(9)V99  COMP-3.    
LGC005             15  FB-AH-PREMIUM-AMT       PIC S9(7)V99  COMP-3.    
LGC005             15  FB-AH-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3.    
LGC005             15  FB-AH-ITD-LUMP-PMT      PIC S9(7)V99  COMP-3.    
LGC005             15  FB-AH-DEV-CODE          PIC X(03).               
LGC005         10  FB-LOAN-INFORMATION.                                 
LGC005             15  FB-LOAN-APR             PIC S999V9999 COMP-3.    
LGC005             15  FB-PAY-FREQUENCY        PIC S99.                 
LGC005             15  FB-LOAN-TERM            PIC S999      COMP-3.    
LGC005             15  FB-RATE-CLASS           PIC XX.                  
LGC005             15  FB-POLICY-FORM-NO       PIC X(12).               
LGC005             15  FB-PREMIUM-TYPE         PIC X.                   
LGC005             15  FB-IND-GRP-TYPE         PIC X.                   
LGC005             15  FB-SKIP-CODE            PIC X.                   
LGC005             15  FB-PAYMENT-MODE         PIC X.                   
LGC005             15  FB-LOAN-NUMBER          PIC X(8).                
LGC005             15  FB-LOAN-BALANCE         PIC S9(7)V99  COMP-3.    
LGC005             15  FB-REIN-TABLE           PIC X(3).                
LGC005             15  FB-SPECIAL-REIN-CODE    PIC X.                   
LGC005         10  FB-STATUS-DATA.                                      
LGC005             15  FB-AH-CANCEL-DT         PIC XX.                  
LGC005             15  FB-LF-CANCEL-DT         PIC XX.                  
LGC005             15  FB-AH-SETTLEMENT-DT     PIC XX.                  
LGC005             15  FB-LF-DEATH-DT          PIC XX.                  
LGC005             15  FB-ENTRY-DT             PIC XX.                  
LGC005             15  FB-AH-CANCEL-EXIT-DT    PIC XX.                  
LGC005             15  FB-AH-SETTLEMENT-EXIT-DT PIC XX.                 
LGC005             15  FB-LF-CANCEL-EXIT-DT    PIC XX.                  
LGC005             15  FB-LF-DEATH-EXIT-DT     PIC XX.                  
LGC005             15  FB-LF-CURRENT-STATUS    PIC X.                   
LGC005             15  FB-LF-STATUS-AT-DEATH   PIC X.                   
LGC005             15  FB-LF-STATUS-AT-CANCEL  PIC X.                   
LGC005             15  FB-AH-CURRENT-STATUS    PIC X.                   
LGC005             15  FB-AH-STATUS-AT-LUMP-SUM PIC X.                  
LGC005             15  FB-AH-STATUS-AT-CANCEL   PIC X.                  
LGC005             15  FB-CLAIM-INTERFACE-SW  PIC X.                    
LGC005             15  FB-CLAIM-ATTACHED-COUNT PIC S9(4)       COMP.    
LGC005             15  FB-ENTRY-BATCH          PIC X(6).                
LGC005             15  FB-LAST-MONTH-END       PIC XX.                  
LGC005             15  FB-AH-PAID-THRU-DT      PIC XX.                  
LGC005         10  FB-CREDIT-INTERFACE-SW-1    PIC X.                   
LGC005         10  FB-CREDIT-INTERFACE-SW-2    PIC X.                   
LGC005         10  FB-ACCOUNT-COMM-PCTS.                                
LGC005             15  FB-LIFE-COMM-PCT        PIC SV9(5)    COMP-3.    
LGC005             15  FB-AH-COMM-PCT          PIC SV9(5)    COMP-3.    
LGC005         10  FILLER                         PIC X(64).            
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---------------'. 
LGC005**********************************************************        
LGC005**********************************************************        
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '--FC-EXTRACT---'. 
LGC005 01  FC-EXTRACT.                                                  
LGC005     05  FC-RECORD-ID                        PIC XX.              
LGC005     05  FC-SORT-KEY-AREAS.                                       
LGC005         10  FC-POSITIONING-CODE             PIC X.               
LGC005         10  FC-EXTRACT-CODE                 PIC X.               
LGC005         10  FC-COMPANY-CD                   PIC X.               
LGC005         10  FC-COMPANY-ID                   PIC XXX.             
LGC005         10  FC-RECORD-TYPE                  PIC X.               
LGC005         10  FC-SORT-KEY-G.                                       
LGC005             15  FC-SG-CARRIER               PIC X.               
LGC005             15  FC-SG-STATE                 PIC XX.              
LGC005             15  FC-SG-ACCOUNT-NO            PIC X(10).           
LGC005             15  FC-SG-CLAIM-NO              PIC X(7).            
LGC005             15  FC-SG-CERT-NO.                                   
LGC005                 20  FC-SG-CERT-PRIME        PIC X(10).           
LGC005                 20  FC-SG-CERT-SFX          PIC X.               
LGC005             15  FC-SG-TRAILER-SEQ-NO        PIC S9(4)    COMP.   
LGC005******************************************************************
LGC005     05  FC-EXTRACT-RECORD.                                       
LGC005         16  FC-INSURED-NAME              PIC X(30).              
LGC005         16  FC-TRAILER-TYPE              PIC X.                  
LGC005         16  FC-RECORDED-DT               PIC XX.                 
LGC005         16  FC-RECORDED-BY               PIC X(4).               
LGC005         16  FC-LAST-MAINT-HHMMSS         PIC S9(6)    COMP-3.    
LGC005         16  FILLER                          PIC X(66).           
LGC005         16  FC-TRAILER-BODY              PIC X(165).             
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---------------'. 
LGC005**********************************************************        
LGC005**********************************************************        
       01  FILLER                      PIC X(15)          VALUE         
                                                     'CLAIM  ACTIVITY'. 
       01  WS-CLAIM-ACTIVITY-RECORD.                                    
           03  CLAIM-WORK-RECORD.                                       
               05  CW-RECORD-KEY.                                       
                   07  CW-CLAIM-NUMBER PIC X(10).                       
               05  CW-CLAIMANT         PIC X(20).                       
               05  CW-CLAIMANT-DOB     PIC X(8).                        
               05  CW-CLAIM-STATUS     PIC X.                           
               05  CW-ENTRY-DATE       PIC X(8).                        
               05  CW-REVISION-DATE    PIC X(8).                        
               05  CW-CLAIMANT-STATE-OF-RES PIC XX.                     
               05  CW-DATE-INCURRED    PIC X(6).                        
               05  CW-FIRST-NOTICE     PIC X(8).                        
               05  CW-DIAGNOSIS        PIC X(50).                       
               05  CW-AMOUNT-PAID      PIC S9(7)V99.                    
               05  CW-LAST-PAID        PIC X(8).                        
               05  FILLER              PIC X(52).                       
               05  CW-CLAIM-TYPE       PIC X.                           
               05  CW-CLAIM-ACTION     PIC X.                           
               05  CW-ACTION-TIME      PIC X(6).                        
               05  CW-CLAIMANT-DOD     PIC X(8).                        
           03  POLICY-WORK-RECORD.                                      
               05  PW-RECORD-KEY.                                       
                   07  PW-CLAIM-NUMBER PIC X(10).                       
                   07  PW-POLICY-NUMBER                                 
                                       PIC X(10).                       
               05  PW-CLAIMANT         PIC X(20).                       
               05  PW-CLAIMANT-DOB     PIC X(8).                        
               05  PW-POLICY-STATUS    PIC X.                           
               05  PW-POLICY-OWNER     PIC X(30).                       
               05  PW-PO-ADDR1         PIC X(30).                       
               05  PW-PO-ADDR2         PIC X(30).                       
               05  PW-PO-CITY          PIC X(20).                       
               05  PW-PO-STATE         PIC XX.                          
               05  PW-PO-ZIP           PIC 9(9).                        
               05  PW-PLAN-NUMBER      PIC X(6).                        
               05  PW-POLICY-TYPE      PIC XX.                          
               05  PW-ISSUE-DATE       PIC X(8).                        
               05  PW-INDEMNITY        PIC S9(7)V99.                    
               05  PW-SELL-AGT-NO      PIC 9(7).                        
               05  FILLER              PIC X(20).                       
               05  PW-PAID-TO          PIC X(8).                        
               05  FILLER              PIC X.                           
               05  PW-COLL-AGT-NO      PIC 9(7).                        
               05  PW-COLL-AGT-STATE   PIC XX.                          
               05  FILLER              PIC XX.                          
               05  PW-COLL-AGT-CODE    PIC X(6).                        
               05  FILLER              PIC X(106).                      
               05  PW-PAYEE-ZIP        PIC 9(9).                        
               05  FILLER              PIC X(50).                       
               05  PW-AMOUNT-PAID      PIC S9(7)V99.                    
               05  PW-LAST-PAID        PIC X(8).                        
               05  FILLER              PIC X.                           
               05  PW-PROOF-DATE       PIC X(8).                        
               05  PW-AUDITOR          PIC XXX.                         
               05  PW-POLICY-ACTION    PIC X.                           
               05  PW-ACTION-TIME      PIC X(6).                        
               05  PW-REVISION-DATE    PIC X(8).                        
               05  PW-HOME-OFFICE-CATEGORY                              
                                       PIC XXX.                         
               05  PW-ANNUAL-STATE-CATEGORY                             
                                       PIC XXX.                         
               05  PW-CLAIM-TYPE       PIC X.                           
               05  PW-OPER-COMPANY     PIC XX.                          
               05  PW-BENEFIT-DURATION PIC 9(3).                        
       01  FILLER                      PIC X(15)          VALUE         
                                                     '---------------'. 
      ******************************************************************
           SKIP3                                                        
           EJECT                                                        
      ******************************************************************
LGC005 01  WS-POLICY-CERT.                                              
LGC005     05  WS-ZERO-FILL            PIC X(01).                       
LGC005     05  WS-POLICY-CERT1.                                         
LGC005         10  WS-CERT-PRIME       PIC X(09).                       
LGC005         10  WS-CERT-SFX         PIC X(01).                       
LGC005 01  WS-POLICY-ACCOUNT.                                           
LGC005     05  WS-POLICY-ZEROS         PIC X(04).                       
LGC005     05  WS-POLICY-ACCOUNT1      PIC X(06).                       
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                    'WS-CLAIM-TYPE-FA'. 
       01  WS-CLAIM-TYPE-FA            PIC X.                           
      ******************************************************************
      *    88  EX-FA-AH-CLAIM               VALUE 'A'.                 *
      *    88  EX-FA-LIFE-CLAIM             VALUE 'L'.                 *
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                      'EXTRACT-EOF-SW'. 
       01  EXTRACT-EOF-SW              PIC X              VALUE 'N'.    
           88  EXTRACT-EOF                                VALUE 'Y'.    
           88  NOT-EXTRACT-EOF                            VALUE 'N'.    
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                  'CORR-TRLR-FOUND-SW'. 
       01  CORR-TRLR-FOUND-SW          PIC X              VALUE 'N'.    
           88  NO-CORR-TRLR-FOUND                         VALUE 'N'.    
           88  CORR-TRLR-FOUND                            VALUE 'Y'.    
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                           'PC-STATUS'. 
       01  PC-STATUS.                                                   
           03  PC-STAT-1               PIC X.                           
           03  PC-STAT-2               PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                           'EX-STATUS'. 
       01  EX-STATUS.                                                   
           03  EX-STAT-1               PIC X.                           
           03  EX-STAT-2               PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                         'BENE-STATUS'. 
       01  BENE-STATUS.                                                 
           03  BENE-STAT-1             PIC X.                           
           03  BENE-STAT-2             PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                           'CF-STATUS'. 
       01  CF-STATUS.                                                   
           03  CF-STAT-1               PIC X.                           
           03  CF-STAT-2               PIC X.                           
      ******************************************************************
LGC005 01  WS-LIT-SEVEN                PIC S99            VALUE +7.     
LGC005******************************************************************
       01  FILLER                      PIC X(08)          VALUE         
                                                             'WS-ZERO'. 
       01  WS-ZERO                     PIC S9(3)   COMP-3 VALUE +0.     
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'ACC-OPEN-CLAIMS'. 
       01  ACC-OPEN-CLAIMS             PIC S9(5)   COMP-3 VALUE +0.     
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                    'WS-ORIGINAL-TERM'. 
       01  WS-ORIGINAL-TERM            PIC S9(3)   COMP-3.              
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                           'WS-HEX-00'. 
       01  WS-HEX-00                   PIC XX             VALUE         
                                                            LOW-VALUES. 
      ******************************************************************
       01  FILLER                      PIC X(08)          VALUE         
                                                             'WS-FIVE'. 
       01  WS-FIVE                     PIC S9(4)   COMP   VALUE +5.     
      ******************************************************************
       01  WS-FIVE-R                                                    
                                REDEFINES WS-FIVE.                      
           03  FILLER                  PIC X.                           
           03  WS-HEX-05               PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                       'WS-CONSTANT-I'. 
       01  WS-CONSTANT-I               PIC X              VALUE 'I'.    
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                       'WS-CONSTANT-D'. 
       01  WS-CONSTANT-D               PIC X              VALUE 'D'.    
      ******************************************************************
       01  FILLER                      PIC X(08)          VALUE 'SUB1'. 
       01  SUB1                        PIC S9(4)          VALUE ZERO    
                                                   COMP.                
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                   'WS-CF-RECORD-TYPE'. 
       01  WS-CF-RECORD-TYPE           PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'WS-DATE-IN-AREA'. 
       01  WS-DATE-IN-AREA             PIC XX             VALUE SPACE.  
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                   'WS-DATE-BACK-AREA'. 
       01  WS-DATE-BACK-AREA            PIC 9(8)          VALUE ZERO.   
       01  WS-DATE-BACK-AREA-R1 REDEFINES WS-DATE-BACK-AREA.            
           05  WS-DATE-BACK-AREA-CC     PIC 9(2).                       
           05  WS-DATE-BACK-AREA-YMD    PIC 9(6).                       
           05  WS-DATE-BACK-AREA-YMD-R REDEFINES WS-DATE-BACK-AREA-YMD. 
               10  WS-DATE-BACK-AREA-YY PIC 9(2).                       
               10  WS-DATE-BACK-AREA-MM PIC 9(2).                       
               10  WS-DATE-BACK-AREA-DD PIC 9(2).                       
       01  WS-DATE-BACK-AREA-R2 REDEFINES WS-DATE-BACK-AREA.            
           05  WS-DATE-BACK-AREA-YM     PIC 9(6).                       
           05  WS-DATE-BACK-AREA-D      PIC 9(2).                       
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                  'WS-CITY-STATE-WORK'. 
       01  WS-CITY-STATE-WORK.                                          
           05  WS-CS-WORK.                                              
               10  WS-CSW              PIC X                            
                                OCCURS 30       TIMES                   
                                INDEXED BY  CSW-INDEX                   
                                  CSW-INDEX2.                           
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                       'WS-CITY-FOUND'. 
       01  WS-CITY-FOUND.                                               
           05  WS-CTY                  PIC X                            
                                OCCURS 20       TIMES                   
                                INDEXED BY  CTY-INDEX.                  
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                      'WS-STATE-FOUND'. 
       01  WS-STATE-FOUND.                                              
           05  WS-STATE-1              PIC X.                           
           05  WS-STATE-2              PIC X.                           
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                   'WS-CERT-TEST-AREA'. 
       01  WS-CERT-TEST-AREA.                                           
           05  WS-CERT-FIRST-2         PIC XX.                          
           05  FILLER                  PIC X(6).                        
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                          'WS-COMPANY'. 
       01  WS-COMPANY                  PIC XXX            VALUE SPACE.  
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'WS-DRAFT-NUMBER'. 
       01  WS-DRAFT-NUMBER             PIC X(7)           VALUE SPACE.  
      ******************************************************************
       01  WS-DRAFT-NUMBER-REDEF                                        
                                REDEFINES WS-DRAFT-NUMBER.              
           05  WS-DNR-1                PIC 9.                           
           05  WS-DNR-2                PIC 9(6).                        
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                   'NUMBER-OF-ADDR-TR'. 
       01  NUMBER-OF-ADDR-TR           PIC 99             VALUE ZERO.   
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'WS-INDIV-GRP-CD'. 
       01  WS-INDIV-GRP-CD             PIC X              VALUE SPACE.  
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                       'WS-BENEFIT-CD'. 
       01  WS-BENEFIT-CD               PIC XX             VALUE SPACE.  
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                 'WS-CHECK-WRITTEN-DT'. 
       01  WS-CHECK-WRITTEN-DT         PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'WS-BIN-MAINT-DT'. 
       01  WS-BIN-MAINT-DT             PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                 'WS-HOLD-CERT-EFF-DT'. 
       01  WS-HOLD-CERT-EFF-DT         PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                     'WS-HOLD-PAID-TO'. 
       01  WS-HOLD-PAID-TO             PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                  'WS-HOLD-INCUR-DATE'. 
       01  WS-HOLD-INCUR-DATE          PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                                   'WS-PLAN-CODE-AREA'. 
       01  WS-PLAN-CODE-AREA.                                           
           05  WS-PC-KEY-PT1           PIC X.                           
           05  WS-PC-KEY-PT2           PIC XX.                          
           05  WS-PC-KEY-PT3           PIC XXX.                         
      ******************************************************************
       01  FILLER                      PIC X(24)          VALUE         
                                               'WS-BE-CONTROL-PRIMARY'. 
       01  WS-BE-CONTROL-PRIMARY.                                       
           05  WS-COMPANY-CD           PIC X.                           
           05  WS-BENEFICIARY-HOLD     PIC X(6).                        
           EJECT                                                        
LGC005**********************************************************        
LGC005**********************************************************        
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '-ADDRESS-TRAIL-'. 
LGC005 01  ACTIVITY-TRAILER1.                                           
LGC005     05  ENTIRE-TRAILER1             PIC X(1400).                 
LGC005     05  ADDRESS-TRAILER1   REDEFINES  ENTIRE-TRAILER1            
LGC005                            OCCURS 7 TIMES                        
LGC005                            INDEXED BY AT-INDEX                   
LGC005                                       AT-INDEX2.                 
LGC005         10  AT-RECORD-ID                PIC XX.                  
LGC005         10  AT-CONTROL-PRIMARY.                                  
LGC005             15  AT-COMPANY-CD           PIC X.                   
LGC005             15  AT-CARRIER              PIC X.                   
LGC005             15  AT-CLAIM-NO             PIC X(7).                
LGC005             15  AT-CERT-NO.                                      
LGC005                 20  AT-CERT-PRIME       PIC X(10).               
LGC005                 20  AT-CERT-SFX         PIC X.                   
LGC005             15  AT-SEQUENCE-NO          PIC S9(4)     COMP.      
LGC005         10  AT-TRAILER-TYPE             PIC X.                   
LGC005         10  AT-RECORDED-DT              PIC XX.                  
LGC005         10  AT-RECORDED-BY              PIC X(4).                
LGC005         10  AT-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.    
LGC005         10  AT-ADDRESS-TR.                                       
LGC005             15  AT-ADDRESS-TYPE         PIC X.                   
LGC005             15  AT-MAIL-TO-NAME         PIC X(30).               
LGC005             15  AT-ADDRESS-LINE-1       PIC X(30).               
LGC005             15  AT-ADDRESS-LINE-2       PIC X(30).               
LGC005             15  AT-CITY-STATE           PIC X(30).               
LGC005             15  AT-ZIP.                                          
LGC005                20  AT-ZIP-CODE          PIC X(5).                
LGC005                20  AT-ZIP-PLUS4         PIC X(4).                
LGC005             15  AT-PHONE-NO             PIC 9(11)     COMP-3.    
LGC005             15  FILLER                  PIC X(23).               
LGC005             15  AT-ADDRESS-LAST-MAINT-DT   PIC X(02).            
LGC065             15  AT-ADDRESS-LAST-UPDATED-BY PIC X(04).            
LGC005                                                                  
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '--CORR-TRAILER-'. 
LGC005 01  ACTIVITY-TRAILER2.                                           
LGC005     05  AT2-RECORD-ID                PIC XX.                     
LGC005     05  AT2-CONTROL-PRIMARY.                                     
LGC005         10  AT2-COMPANY-CD           PIC X.                      
LGC005         10  AT2-CARRIER              PIC X.                      
LGC005         10  AT2-CLAIM-NO             PIC X(7).                   
LGC005         10  AT2-CERT-NO.                                         
LGC005             15  AT2-CERT-PRIME       PIC X(10).                  
LGC005             15  AT2-CERT-SFX         PIC X.                      
LGC005         10  AT2-SEQUENCE-NO          PIC S9(4)     COMP.         
LGC005     05  AT2-TRAILER-TYPE             PIC X.                      
LGC005     05  AT2-RECORDED-DT              PIC XX.                     
LGC005     05  AT2-RECORDED-BY              PIC X(4).                   
LGC005     05  AT2-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.       
LGC005     05  AT2-CORRESPONDENCE-TR.                                   
LGC005         10  AT2-LETTER-SENT-DT       PIC XX.                     
LGC005         10  AT2-RECEIPT-FOLLOW-UP    PIC XX.                     
LGC005         10  AT2-AUTO-RE-SEND-DT      PIC XX.                     
LGC005         10  AT2-LETTER-ANSWERED-DT   PIC XX.                     
LGC005         10  AT2-LETTER-ARCHIVE-NO    PIC S9(8)     COMP.         
LGC005         10  AT2-LETTER-ORIGIN        PIC X.                      
LGC005         10  AT2-STD-LETTER-FORM      PIC X(4).                   
LGC005         10  AT2-REASON-TEXT          PIC X(70).                  
LGC005         10  AT2-ADDRESS-REC-SEQ-NO   PIC S9(4)     COMP.         
LGC005         10  AT2-ADDRESEE-TYPE        PIC X(01).                  
LGC005         10  AT2-ADDRESSEE-NAME       PIC X(30).                  
LGC005         10  AT2-INITIAL-PRINT-DATE   PIC XX.                     
LGC005         10  AT2-RESEND-PRINT-DATE    PIC XX.                     
LGC005         10  AT2-CORR-SOL-UNSOL       PIC X(01).                  
LGC005         10  AT2-LETTER-PURGED-DT     PIC X(02).                  
LGC001*        10  FILLER                  PIC X(32).                   
LGC001         10  AT2-CSO-REDEFINITION.                                
LGC001             20  FILLER                PIC X(27).                 
LGC001             20  AT2-CSO-LETTER-STATUS  PIC X.                    
LGC001             20  AT2-CSO-LETTER-PURGE-DATE   PIC XX.              
LGC001             20  AT2-CSO-LETTER-RELOAD-DATE  PIC XX.              
LGC005         16  AT2-CORR-LAST-MAINT-DT   PIC X(02).                  
LGC005         16  AT2-CORR-LAST-UPDATED-BY PIC X(04).                  
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---------------'. 
LGC005**********************************************************        
LGC005**********************************************************        
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---DATE-CONV---'. 
       COPY ELCDATE.                                                    
LGC005 01  FILLER                      PIC X(15)          VALUE         
LGC005                                               '---------------'. 
LGC005                                                                  
      ******************************************************************
           EJECT                                                        
      ******************************************************************
      *  THIS PROGRAM WAS WRITTEN TO TAKE THE DAILY EXTRACTS FILE      *
      *  FROM CL310 IN CYCLDLY AND FORMAT A CLAIM ACTIVITY ENTRY       *
      *  FOR EVERY OPEN CLAIM.                                         *
      *  THE INPUT TO THIS PROGRAM MUST BE SORTED INTO EXTRACTS        *
      *  FA FB AND FC THESE ARE THE ONLY EXTRACTS USED IN THE          *
      *  ENTIRE PROGRAM.                                               *
      *  THE FA EXTRACTS ARE PROCESSED FIRST, THEN THE FB EXTR.        *
      *  MUST COME NEXT.  THE FCS ARE TRAILERS--- THE ADDRESS          *
      *  TRAILERS ARE PUT INTO TABLES, WE ONLY NEED ONE                *
      *  CORRESPONDENCE TRAILER FOR THE OUTPUTS.                       *
      ******************************************************************
       01  FILLER.                                                      
           05  OS-ZERO                 PIC S9(8)   COMP   VALUE ZERO.   
           05  OS-EOJ                  PIC X(4)           VALUE ZERO.   
           05  OS-LINECTR              PIC S9(5)   COMP-3 VALUE ZERO.   
           05  COMPILATION-DATE        PIC X(20).                       
           05  OS-CC                   PIC X.                           
CSOMOD     05  WS-HOLD-STATE-INPUT     PIC XX.                          
      ******************************************************************
       01  FILLER                      PIC X(16)          VALUE         
                                                    'PLAN-CODE-RECORD'. 
       01  PLAN-CODE-RECORD.                                            
           05  FILLER                  PIC X(3).                        
           05  PC-HOME-OFFICE-CAT      PIC XXX.                         
           05  PC-ANNUAL-STATE-CAT     PIC XXX.                         
           05  FILLER                  PIC X(19).                       
           05  PC-PLAN-ID              PIC X(3).                        
           05  FILLER                  PIC X(27).                       
           05  PC-OPER-COMPANY         PIC XX.                          
           05  FILLER                  PIC X(9).                        
           05  PC-BIZ-CODE             PIC X.                           
           05  PC-CLAIM-TYPE           PIC X.                           
           05  FILLER                  PIC XXX.                         
           05  PC-PLAN-CODE-KEY.                                        
               10  PC-KEY-PT1          PIC XXX.                         
               10  PC-KEY-PT2          PIC XXX.                         
      ******************************************************************
       01  FILLER                      PIC X(37)          VALUE         
                                'OPECLMS   WORKING-STORAGE  ENDS  HERE'.
      ******************************************************************
       LINKAGE                    SECTION.                              
LGC005 01  PARM-D-OS.                                                   
LGC005     05  PARM-LENGTH             PIC S9(4)   COMP.                
LGC005     05  PARM-DATA.                                               
               10  FILLER              PIC X(100).                      
      ******************************************************************
LGC005 PROCEDURE       DIVISION  USING PARM-D-OS.                       
           MOVE WHEN-COMPILED TO COMPILATION-DATE.                      
           DISPLAY 'PRGNUM OPECLMS ' COMPILATION-DATE.                  
                                                                        
       A000-MAINLINE.                                                   
                                                                        
           PERFORM B000-OPEN-FILES THRU B000-EXIT.                      
                                                                        
TSTMOD*    READY TRACE.                                                 
LGC049                                                                  
           PERFORM B020-PROCESSING THRU B020-EXIT                       
               UNTIL EXTRACT-EOF.                                       
                                                                        
TSTMOD*    RESET TRACE.                                                 
LGC049                                                                  
           PERFORM B030-CLOSE-FILES THRU B030-EXIT.                     
           MOVE '0000' TO OS-EOJ                                        
           GO TO 9980-OS-CALL-EOJ.                                      
                                                                        
       A000-EXIT.                                                       
               EXIT.                                                    
                                                                        
       B000-OPEN-FILES.                                                 
           OPEN  INPUT  EXTRACT-FILE                                    
LGC005                  ELBENE                                          
LGC005                  ELCNTL                                          
                        CSPLNCD.                                        
           IF EX-STATUS NOT = '00'                                      
TEST00       IF EX-STATUS NOT = '97'                                    
               DISPLAY 'ERROR--OPEN--EXTRACT FILE --CODE => '           
                       EX-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
           IF PC-STATUS NOT = '00'                                      
TEST00       IF PC-STATUS NOT = '97'                                    
               DISPLAY 'ERROR--OPEN--PLN CD FILE --CODE => '            
                       PC-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
           IF BENE-STATUS NOT = '00'                                    
TEST00       IF BENE-STATUS NOT = '97'                                  
LGC005         DISPLAY 'ERROR--OPEN-ELBENE FILE --CODE => '             
                       BENE-STATUS                                      
               GO TO ABEND-PROGRAM.                                     
           IF CF-STATUS NOT = '00'                                      
CEST00       IF CF-STATUS NOT = '97'                                    
LGC005         DISPLAY 'ERROR--OPEN-ELCNTL FILE --CODE => '             
                       CF-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
           OPEN  OUTPUT LG-CLM-ACT.                                     
                                                                        
       B000-EXIT.                                                       
               EXIT.                                                    
                                                                        
       B020-PROCESSING.                                                 
LGC049                                                                  
LGC049     IF  A-SW  = 'Y'                                              
LGC049         MOVE  'N'  TO A-SW                                       
LGC049           GO  TO  B020-CONT.                                     
LGC049                                                                  
           PERFORM C030-PRE-INIT THRU C030-EXIT.                        
LGC049                                                                  
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT                   
LGC005         UNTIL EX-RECORD-TYPE = 'A' OR EXTRACT-EOF.               
           IF EXTRACT-EOF                                               
               GO TO B020-EXIT.                                         
      ******************************************************************
      * MOVES FA TO WORKING STORAGE HOLD AREA                          *
      ******************************************************************
LGC049                                                                  
LGC049 B020-CONT.                                                       
LGC049                                                                  
LGC005     MOVE REPORTS-EXTRACT-RECORD  TO  FA-EXTRACT.                 
LGC049                                                                  
           IF FA-CLAIM-STATUS = 'O'                                     
             ADD   +1           TO  ACC-OPEN-CLAIMS                     
LGC005       PERFORM INIT-DATE-DATA THRU INIT-DATE-EXIT                 
             PERFORM C020-CLAIM-ACTIVITY-ENTRY                          
           ELSE                                                         
LGC005       MOVE  ' '  TO  FA-RECORD-TYPE.                             
                                                                        
       B020-EXIT.                                                       
               EXIT.                                                    
                                                                        
       C000-READ-EX-F-TAPE.                                             
                                                                        
           READ  EXTRACT-FILE                                           
             AT END                                                     
               MOVE  'Y'  TO  EXTRACT-EOF-SW.                           
LGC049                                                                  
LGC049     IF  EX-RECORD-TYPE  =  'A'                                   
LGC049         MOVE  'Y'  TO A-SW                                       
LGC049      ELSE                                                        
LGC049         MOVE  'N'  TO A-SW.                                      
LGC049                                                                  
LGC049                                                                  
           IF EX-STATUS = '00'                                          
             OR  = '10'                                                 
                 NEXT SENTENCE                                          
           ELSE                                                         
               DISPLAY 'ERROR--READ--EXTRACT FILE --CODE => '           
                       EX-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
                                                                        
       C000-EXIT.                                                       
               EXIT.                                                    
                                                                        
       C020-CLAIM-ACTIVITY-ENTRY.                                       
      ******************************************************************
      * AN FB EXTRACT MUST COME NEXT IN THE PROCESSING FLOW            *
      ******************************************************************
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT.                  
LGC005     IF EX-RECORD-TYPE = 'B'                                      
LGC005         MOVE  EX-SG-STATE            TO  WS-HOLD-STATE-INPUT     
LGC005         MOVE  REPORTS-EXTRACT-RECORD TO  FB-EXTRACT              
           ELSE                                                         
               DISPLAY '*** NO EXTRACT FB OR NOT IN PROPER ORDER ***'   
               GO TO ABEND-PROGRAM.                                     
           SET   AT-INDEX TO 1.                                         
           MOVE  ZERO       TO  NUMBER-OF-ADDR-TR.                      
           PERFORM D060-BUILD-FC THRU D060-EXIT                         
LGC005         UNTIL EX-RECORD-TYPE = 'A' OR EXTRACT-EOF.               
      ******************************************************************
      *  PERFORM PROCESSING OF THE FC TRLRS UNTIL WE COME ACROSS A     *
      *  A NEW SET OF FA FB AND FCS.                                   *
      ******************************************************************
           PERFORM D030-CLAIM-PROCESS-FA THRU D030-EXIT.                
           PERFORM D040-CLAIM-PROCESS-FB THRU D040-EXIT.                
           SET   AT-INDEX2 TO 1.                                        
           PERFORM D050-CLAIM-PROCESS-ADDR THRU D050-EXIT               
               UNTIL NUMBER-OF-ADDR-TR = ZERO                           
               IF CORR-TRLR-FOUND                                       
                   PERFORM E050-CLAIM-CORR-TRLR THRU E050-EXIT          
               ELSE                                                     
                   MOVE '00000000'  TO  PW-PROOF-DATE.                  
LGC005     WRITE CLAIM-ACTIVITY-RECORD FROM                             
                   WS-CLAIM-ACTIVITY-RECORD.                            
                                                                        
       C020-EXIT.                                                       
               EXIT.                                                    
                                                                        
       C030-PRE-INIT.                                                   
LGC005     MOVE  SPACES     TO  REPORTS-EXTRACT-RECORD.                 
LGC005     MOVE  SPACES     TO  ENTIRE-TRAILER1                         
                                WS-CLAIM-ACTIVITY-RECORD.               
           MOVE '00000000'  TO  CW-REVISION-DATE.                       
           MOVE '000000'    TO  CW-ACTION-TIME                          
                                PW-ACTION-TIME.                         
           MOVE  ZERO       TO  PW-SELL-AGT-NO                          
                                PW-PAYEE-ZIP                            
                                PW-PO-ZIP.                              
           MOVE  'N'        TO  CORR-TRLR-FOUND-SW.                     
           MOVE  ZERO       TO  WS-CHECK-WRITTEN-DT                     
                                WS-BIN-MAINT-DT                         
                                WS-HOLD-CERT-EFF-DT                     
                                WS-HOLD-PAID-TO                         
                                WS-HOLD-INCUR-DATE.                     
                                                                        
       C030-EXIT.                                                       
               EXIT.                                                    
                                                                        
       D030-CLAIM-PROCESS-FA.                                           
      ******************************************************************
      * MOVES ALL FA DATA TO THE CLAIM ACTIVITY RECORD                 *
      ******************************************************************
LGC005     MOVE  FA-SG-CLAIM-NO   TO  CW-CLAIM-NUMBER                   
                                      PW-CLAIM-NUMBER.                  
           MOVE  SPACES           TO  PW-POLICY-NUMBER.                 
LGC005     MOVE FA-SG-CERT-NO     TO  WS-POLICY-CERT.                   
LGC005     MOVE WS-POLICY-CERT1   TO  PW-POLICY-NUMBER.                 
LGC005     MOVE  FA-INSURED-BIRTH-DT TO  WS-DATE-IN-AREA.               
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      
           MOVE  19 TO WS-DATE-BACK-AREA-CC.                            
           MOVE  WS-DATE-BACK-AREA      TO  CW-CLAIMANT-DOB             
                                            PW-CLAIMANT-DOB.            
LGC005     MOVE  FA-CLAIM-STATUS     TO  CW-CLAIM-STATUS.               
LGC005     MOVE  FA-FILE-ESTABLISH-DT        TO  WS-DATE-IN-AREA.       
           PERFORM CONVERT-DATE THRU CONVERT-EXIT.                      
           MOVE  WS-DATE-BACK-AREA    TO  CW-ENTRY-DATE.                
LGC005     MOVE  FA-INCURRED-DT      TO  WS-DATE-IN-AREA                
                                            WS-HOLD-INCUR-DATE.         
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA-YM   TO  CW-DATE-INCURRED.           
LGC005     MOVE  FA-REPORTED-DT      TO  WS-DATE-IN-AREA                
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  CW-FIRST-NOTICE             
           MOVE  SPACES       TO  CW-DIAGNOSIS.                         
LGC005     MOVE  FA-DIAGNOSIS-DESCRIP        TO  CW-DIAGNOSIS.          
LGC005     MOVE  FA-TOTAL-PAID-AMT           TO  CW-AMOUNT-PAID.        
LGC005     MOVE  FA-LAST-PMT-DT    TO  WS-DATE-IN-AREA                  
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  CW-LAST-PAID                
LGC005     IF FA-SG-CARRIER = '6'                                       
               MOVE  'W'      TO  CW-CLAIM-TYPE                         
           ELSE                                                         
               MOVE  ' '      TO  CW-CLAIM-TYPE.                        
LGC005     IF FA-COMPANY-CD = WS-HEX-05                                 
               MOVE  'DMD'      TO  WS-COMPANY                          
               MOVE  'CC'       TO  PW-POLICY-TYPE                      
           ELSE                                                         
               MOVE  'CID'      TO  WS-COMPANY                          
               MOVE  'CR'       TO  PW-POLICY-TYPE.                     
LGC005     MOVE FA-SG-ACCOUNT-NO      TO  PW-COLL-AGT-CODE.             
LGC005     MOVE  ZERO                 TO  PW-COLL-AGT-NO.               
LGC005     MOVE  FA-TOTAL-PAID-AMT    TO  PW-AMOUNT-PAID.               
LGC005     MOVE  FA-INSURED-NAME      TO  CW-CLAIMANT                   
                                          PW-CLAIMANT.                  
           MOVE  'O'                  TO  PW-POLICY-STATUS.             
LGC005*        MOVE  'R'  TO  PW-POLICY-STATUS                          
LGC005*    ELSE                                                         
LGC005*        MOVE  'D'  TO  PW-POLICY-STATUS.                         
LGC005     MOVE  FA-LAST-CLOSE-DT    TO  WS-DATE-IN-AREA                
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  PW-LAST-PAID                
LGC005     MOVE  FA-LAST-MAINT-USER  TO  PW-AUDITOR.                    
LGC005     IF FA-LAST-MAINT-TYPE = ' '                                  
               MOVE  'A'      TO  CW-CLAIM-ACTION                       
                                  PW-POLICY-ACTION                      
           ELSE                                                         
               MOVE  'U'  TO  CW-CLAIM-ACTION                           
                              PW-POLICY-ACTION.                         
LGC005     MOVE  FA-LAST-CLOSE-DT    TO  WS-DATE-IN-AREA                
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  PW-REVISION-DATE.           
LGC005     MOVE  FA-LAST-MAINT-DT    TO  WS-BIN-MAINT-DT.               
LGC005     MOVE  FA-CLAIM-TYPE       TO  WS-CLAIM-TYPE-FA.              
                                                                        
       D030-EXIT.                                                       
               EXIT.                                                    
                                                                        
       D040-CLAIM-PROCESS-FB.                                           
      ******************************************************************
      * MOVES ALL FB DATA TO THE CLAIM ACTIVITY RECORD                 *
      ******************************************************************
LGC005     IF FB-IND-GRP-TYPE = 'I'                                     
LGC005         MOVE  FB-IND-GRP-TYPE     TO  WS-INDIV-GRP-CD            
           ELSE                                                         
               MOVE  'G'          TO  WS-INDIV-GRP-CD.                  
           IF WS-CLAIM-TYPE-FA = 'L'                                    
LGC005         MOVE  FB-LF-REMAINING-AMT TO  PW-INDEMNITY               
LGC005         MOVE  FB-LF-ORIG-TERM     TO  WS-ORIGINAL-TERM           
               IF WS-COMPANY = 'CID'                                    
                   PERFORM E080-DFT-CLM-BENEFIT-CD THRU E080-EXIT       
               ELSE                                                     
LGC005             MOVE  FB-LF-BENEFIT-CD    TO  WS-BENEFIT-CD          
           ELSE                                                         
LGC005         MOVE  FB-AH-BENEFIT-AMT       TO  PW-INDEMNITY           
LGC005         MOVE  FB-AH-BENEFIT-CD        TO  WS-BENEFIT-CD          
LGC005         MOVE  FB-AH-ORIG-TERM         TO  WS-ORIGINAL-TERM.      
CSOTST*    MOVE  EX-FB-STATE          TO  PW-COLL-AGT-STATE.            
CSOTST     MOVE  WS-HOLD-STATE-INPUT  TO  PW-COLL-AGT-STATE.            
LGC005     MOVE  FB-EFFECTIVE-DT     TO  WS-DATE-IN-AREA                
                                            WS-HOLD-CERT-EFF-DT.        
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  PW-ISSUE-DATE               
           MOVE  WS-HOLD-CERT-EFF-DT    TO  DC-BIN-DATE-1               
           MOVE  WS-ORIGINAL-TERM       TO  DC-ELAPSED-MONTHS           
           MOVE  '6'          TO  DC-OPTION-CODE                        
LGC005     CALL 'ELDATCX'        USING DATE-CONVERSION-DATA             
           IF NO-CONVERSION-ERROR                                       
               MOVE  DC-BIN-DATE-2          TO  WS-HOLD-PAID-TO         
               MOVE  DC-GREG-DATE-1-YMD     TO  WS-DATE-BACK-AREA-YMD   
               IF WS-DATE-BACK-AREA-YY > 70                             
                   MOVE 19 TO WS-DATE-BACK-AREA-CC                      
               ELSE                                                     
                   MOVE 20 TO WS-DATE-BACK-AREA-CC                      
           ELSE                                                         
               MOVE  ZERO         TO  WS-DATE-BACK-AREA.                
           MOVE  WS-DATE-BACK-AREA      TO  PW-PAID-TO.                 
           MOVE  WS-HOLD-INCUR-DATE     TO  DC-BIN-DATE-1               
           MOVE  WS-HOLD-PAID-TO        TO  DC-BIN-DATE-2               
           MOVE  '1'          TO  DC-OPTION-CODE                        
LGC005     CALL 'ELDATCX'        USING DATE-CONVERSION-DATA             
           IF NO-CONVERSION-ERROR                                       
               IF DC-ODD-DAYS-OVER = 0                                  
                   MOVE  DC-ELAPSED-MONTHS      TO  PW-BENEFIT-DURATION 
               ELSE                                                     
                   ADD   1    TO  DC-ELAPSED-MONTHS                     
                   MOVE  DC-ELAPSED-MONTHS      TO  PW-BENEFIT-DURATION 
           ELSE                                                         
               MOVE  ZERO   TO  PW-BENEFIT-DURATION.                    
           PERFORM E070-CLAIM-ACCESS-PLAN-CODE THRU E070-EXIT.          
                                                                        
       D040-EXIT.                                                       
               EXIT.                                                    
                                                                        
       D050-CLAIM-PROCESS-ADDR.                                         
      ******************************************************************
      * TRIES TO FIND AN INSURED ADDRESS FROM THE ADDRESS TRAILERS     *
      * (FC) THAT WERE BUILT IN A TABLE                                *
      ******************************************************************
           IF AT-ADDRESS-TYPE (AT-INDEX2) = 'I'                         
               MOVE  AT-MAIL-TO-NAME (AT-INDEX2)      TO                
                                                        PW-POLICY-OWNER 
               MOVE  AT-ADDRESS-LINE-1 (AT-INDEX2)      TO  PW-PO-ADDR1 
               MOVE  AT-ADDRESS-LINE-2 (AT-INDEX2)      TO  PW-PO-ADDR2 
               MOVE  AT-CITY-STATE (AT-INDEX2)          TO              
                                                     WS-CITY-STATE-WORK 
               PERFORM G000-BREAK-CITY-STATE THRU G000-EXIT             
               MOVE  WS-CITY-FOUND    TO  PW-PO-CITY                    
               MOVE  WS-STATE-FOUND   TO  PW-PO-STATE                   
                                          CW-CLAIMANT-STATE-OF-RES      
               MOVE  AT-ZIP-CODE (AT-INDEX2)      TO  PW-PO-ZIP.        
           SET   AT-INDEX2 UP BY     1.                                 
           SUBTRACT 1 FROM NUMBER-OF-ADDR-TR.                           
                                                                        
       D050-EXIT.                                                       
               EXIT.                                                    
                                                                        
       D060-BUILD-FC.                                                   
      ******************************************************************
      * BRINGS IN ALL THE FC TRAILERS---PUTS THE PAYENTS AND ADDRESSES *
      * INTO WORKING STORAGE TABLES.  LOOKS FOR ONLY ONE CORRESPONDENCE*
      * TRAILER                                                        *
      ******************************************************************
           PERFORM C000-READ-EX-F-TAPE THRU C000-EXIT.                  
           IF EXTRACT-EOF                                               
LGC005       OR  EX-RECORD-TYPE = 'A'                                   
                 GO TO D060-EXIT.                                       
LGC005     MOVE REPORTS-EXTRACT-RECORD TO  FC-EXTRACT.                  
LGC005     IF FC-TRAILER-TYPE = '5'                                     
LGC005         IF AT-INDEX GREATER THAN WS-LIT-SEVEN                    
LGC005             GO TO D060-EXIT                                      
LGC005         ELSE                                                     
                   ADD   1  TO  NUMBER-OF-ADDR-TR                       
LGC005             MOVE  FC-TRAILER-BODY  TO  AT-ADDRESS-TR (AT-INDEX)  
                   SET   AT-INDEX UP BY     1                           
LGC005     ELSE                                                         
LGC005         NEXT SENTENCE.                                           
LGC005     IF FC-TRAILER-TYPE = '4'                                     
             AND NO-CORR-TRLR-FOUND                                     
LGC005           MOVE  FC-TRAILER-BODY  TO  AT2-CORRESPONDENCE-TR       
                 IF AT2-LETTER-ANSWERED-DT > LOW-VALUES                 
                     MOVE  'Y'          TO  CORR-TRLR-FOUND-SW.         
                                                                        
       D060-EXIT.                                                       
               EXIT.                                                    
                                                                        
       E050-CLAIM-CORR-TRLR.                                            
      ******************************************************************
      * MOVES THE DATE TO THE PROOF DATE ON THE DRAFT RECORD           *
      * IF NO TRAILER CAN BE FOUND THEN ZEROS ARE MOVED                *
      ******************************************************************
           MOVE  AT2-LETTER-ANSWERED-DT       TO  WS-DATE-IN-AREA       
           PERFORM CONVERT-DATE THRU CONVERT-EXIT                       
           MOVE  WS-DATE-BACK-AREA      TO  PW-PROOF-DATE.              
                                                                        
       E050-EXIT.                                                       
               EXIT.                                                    
                                                                        
       E070-CLAIM-ACCESS-PLAN-CODE.                                     
      ******************************************************************
      *  THERE SHOULD ALWAYS BE A PLAN CODE FOUND IF NOT SERIOUS       *
      *  PROBLEMS WILL OCCUR IN OTHER PROGRAMS                         *
      ******************************************************************
           IF WS-COMPANY = 'DMD'                                        
               MOVE  WS-CONSTANT-D    TO  WS-PC-KEY-PT1                 
           ELSE                                                         
               MOVE  WS-INDIV-GRP-CD  TO  WS-PC-KEY-PT1.                
           IF WS-BENEFIT-CD = '00'                                      
             OR  = LOW-VALUES                                           
                 MOVE  '01' TO  WS-PC-KEY-PT2                           
           ELSE                                                         
               MOVE  WS-BENEFIT-CD    TO  WS-PC-KEY-PT2.                
           MOVE  SPACES     TO  WS-PC-KEY-PT3.                          
           MOVE  WS-PLAN-CODE-AREA      TO  PC-PLAN-CODE-KEY.           
LGC005     MOVE PC-PLAN-CODE-KEY       TO CSP-PC-PLAN-CODE-KEY          
           READ  CSPLNCD RECORD                                         
               INTO PLAN-CODE-RECORD.                                   
           IF PC-STATUS = '23'                                          
      ******************************************************************
      * NO PLAN CODE FOR THIS RECORD                                   *
      ******************************************************************
               DISPLAY 'NO PLAN CODE CLAIM#= '                          
LGC005                 FA-SG-CLAIM-NO                                   
               DISPLAY '   PLAN CODE ==> ' PC-PLAN-CODE-KEY             
               MOVE  ALL 'X'  TO  PW-HOME-OFFICE-CATEGORY               
               MOVE  ALL 'X'  TO  PW-ANNUAL-STATE-CATEGORY              
               MOVE  ALL 'X'  TO  PW-PLAN-NUMBER                        
               MOVE  ALL 'X'  TO  PW-OPER-COMPANY                       
               MOVE  ALL 'X'  TO  PW-CLAIM-TYPE                         
           ELSE                                                         
           IF PC-STATUS = '00'                                          
               MOVE  PC-HOME-OFFICE-CAT     TO  PW-HOME-OFFICE-CATEGORY 
               MOVE  PC-ANNUAL-STATE-CAT    TO  PW-ANNUAL-STATE-CATEGORY
               MOVE  PC-KEY-PT1   TO  PW-PLAN-NUMBER                    
               MOVE  PC-OPER-COMPANY  TO  PW-OPER-COMPANY               
               MOVE  PC-CLAIM-TYPE    TO  PW-CLAIM-TYPE                 
           ELSE                                                         
               DISPLAY 'ERROR--READ--CSPLNCD ==> '                      
                       PC-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
                                                                        
       E070-EXIT.                                                       
               EXIT.                                                    
                                                                        
       E080-DFT-CLM-BENEFIT-CD.                                         
      ******************************************************************
      * THIS MODULE USES A NUMERIC BENEFIT CODE TO READ THE CLCNTL     *
      * FILE IN ORDER TO FIND ITS CORRESPONDING ALPHA COUNTERPART.     *
      ******************************************************************
LGC005     MOVE  FA-COMPANY-ID          TO  CF-COMPANY-ID.              
LGC005     MOVE  FB-LF-BENEFIT-CD    TO  CF-HI-BEN-IN-REC.              
LGC005     IF FA-CLAIM-TYPE = 'A'                                       
               MOVE  '5'      TO  WS-CF-RECORD-TYPE                     
           ELSE                                                         
               MOVE  '4'      TO  WS-CF-RECORD-TYPE.                    
           MOVE  +0           TO  CF-SEQUENCE-NO.                       
           MOVE  WS-CF-RECORD-TYPE      TO  CF-RECORD-TYPE.             
LGC005     START ELCNTL KEY NOT < CF-CONTROL-PRIMARY                    
           IF CF-STAT-1 NOT EQUAL TO '0'                                
LGC005         DISPLAY '*** READ ERROR - ELCNTL..STATUS = '             
                       CF-STATUS                                        
LGC005         DISPLAY '*** READ ERROR - ELCNTL..STATUS = '             
                       CF-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
LGC005     READ  ELCNTL NEXT                                            
LGC005*        INTO CONTROL-FILE                                        
           IF CF-STAT-1 NOT EQUAL TO '0'                                
               DISPLAY '*** READ ERROR - CLCNTL..STATUS = '             
                       CF-STATUS                                        
               DISPLAY '*** READ ERROR - CLCNTL..STATUS = '             
                       CF-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
LGC005     IF (FA-COMPANY-ID NOT = CF-COMPANY-ID)                       
             OR  (WS-CF-RECORD-TYPE NOT = CF-RECORD-TYPE)               
                 MOVE  ZERO       TO  WS-BENEFIT-CD                     
      ******************************************************************
      * NEED DEFAULT OR MOVE ZEROS AND DEFAULT LATER                   *
      ******************************************************************
                 GO TO E080-EXIT.                                       
           PERFORM E080-DUMMY THRU E080-DUMMY-EXIT                      
               VARYING SUB1                FROM 1                       
                                             BY 1                       
                 UNTIL ((SUB1 GREATER 8) OR (CF-BENEFIT-NUMERIC (SUB1) =
LGC005             FB-LF-BENEFIT-CD)).                                  
           IF SUB1 NOT = 9                                              
               MOVE  CF-BENEFIT-ALPHA (SUB1)     TO  WS-BENEFIT-CD      
           ELSE                                                         
      ******************************************************************
      * NEED A DEFAULT OR MOVE ZEROS AND CHECK DEFAULT LATER           *
      ******************************************************************
               MOVE  ZERO       TO  WS-BENEFIT-CD.                      
                                                                        
       E080-EXIT.                                                       
               EXIT.                                                    
                                                                        
       E080-DUMMY.                                                      
                                                                        
       E080-DUMMY-EXIT.                                                 
               EXIT.                                                    
                                                                        
       G000-BREAK-CITY-STATE.                                           
      ******************************************************************
      * THIS MODULE TAKES THE CITY-STATE (PIC X(30)) AND BREAKS        *
      * INTO SEPARATE FIELDS---CITY (PIC X(20)) AND STATE (PIC X(2))   *
      ******************************************************************
           MOVE  SPACES     TO  WS-CITY-FOUND.                          
           MOVE  SPACES     TO  WS-STATE-FOUND.                         
           SET   CSW-INDEX TO 30.                                       
                                                                        
       7551-CONTINUE.                                                   
           IF WS-CSW (CSW-INDEX) = SPACE                                
             OR  = '.'                                                  
                 SET   CSW-INDEX DOWN BY     1                          
                 GO TO 7551-CONTINUE.                                   
           MOVE  WS-CSW (CSW-INDEX)      TO  WS-STATE-2.                
           SET   CSW-INDEX DOWN BY     1.                               
           IF WS-CSW (CSW-INDEX) = '.'                                  
               SET   CSW-INDEX DOWN BY     1.                           
           MOVE  WS-CSW (CSW-INDEX)      TO  WS-STATE-1.                
                                                                        
       7552-CONTINUE.                                                   
           SET   CSW-INDEX DOWN BY     1.                               
           IF WS-CSW (CSW-INDEX) = ' '                                  
             OR  ','                                                    
                 GO TO 7552-CONTINUE.                                   
           SET   CTY-INDEX TO 1.                                        
           SET   CSW-INDEX2 TO 1.                                       
                                                                        
       7553-CONTINUE.                                                   
           IF CTY-INDEX > 19                                            
               GO TO G000-EXIT.                                         
           IF CSW-INDEX2 NOT > CSW-INDEX                                
               MOVE  WS-CSW (CSW-INDEX2)     TO  WS-CTY (CTY-INDEX)     
               SET   CSW-INDEX2 UP BY     1                             
               SET   CTY-INDEX UP BY     1                              
               GO TO 7553-CONTINUE.                                     
                                                                        
       G000-EXIT.                                                       
               EXIT.                                                    
                                                                        
       CONVERT-DATE.                                                    
      ******************************************************************
      * CHANGES BINARY TO GREG                                         *
      ******************************************************************
           MOVE  WS-DATE-IN-AREA  TO  DC-BIN-DATE-1.                    
           MOVE  SPACE  TO  DC-OPTION-CODE.                             
LGC005     CALL 'ELDATCX'        USING DATE-CONVERSION-DATA.            
           IF NO-CONVERSION-ERROR                                       
               MOVE  DC-GREG-DATE-1-YMD     TO  WS-DATE-BACK-AREA-YMD   
               IF WS-DATE-BACK-AREA-YY > 70                             
                   MOVE 19 TO WS-DATE-BACK-AREA-CC                      
               ELSE                                                     
                   MOVE 20 TO WS-DATE-BACK-AREA-CC                      
           ELSE                                                         
               MOVE  ZERO         TO  WS-DATE-BACK-AREA.                
                                                                        
       CONVERT-EXIT.                                                    
               EXIT.                                                    
                                                                        
       B030-CLOSE-FILES.                                                
           DISPLAY 'NUMBER OF OPEN CLAIMS   => '                        
                   ACC-OPEN-CLAIMS.                                     
           CLOSE CSPLNCD.                                               
           IF PC-STATUS NOT = '00'                                      
               DISPLAY 'ERROR-CLOSE--PLAN CD FILE--CODE => '            
                       PC-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
LGC005     CLOSE ELBENE.                                                
           IF BENE-STATUS NOT = '00'                                    
LGC005         DISPLAY 'ERROR-CLOSE--ELBENE FILE--CODE => '             
                       BENE-STATUS                                      
               GO TO ABEND-PROGRAM.                                     
LGC005     CLOSE ELCNTL.                                                
           IF CF-STATUS NOT = '00'                                      
LGC005         DISPLAY 'ERROR-CLOSE--ELCNTL FILE--CODE => '             
                       CF-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
           CLOSE EXTRACT-FILE                                           
                 LG-CLM-ACT.                                            
           IF EX-STATUS NOT = '00'                                      
               DISPLAY 'ERROR--CLOSE-EXTRACT FILE --CODE => '           
                       EX-STATUS                                        
               GO TO ABEND-PROGRAM.                                     
                                                                        
       B030-EXIT.                                                       
               EXIT.                                                    
                                                                        
LGC005 INIT-DATE-DATA.                                                  
LGC005     MOVE SPACES   TO  DC-OPTION-CODE                             
LGC005                       DC-ERROR-CODE                              
LGC005                       DC-END-OF-MONTH                            
LGC005                       DC-BIN-DATE-1                              
LGC005                       DC-BIN-DATE-2                              
LGC005                       SLASH1-1                                   
LGC005                       SLASH1-2                                   
LGC005                       SLASH2-1                                   
LGC005                       SLASH2-2                                   
LGC005                       DC-ALPHA-MONTH                             
LGC005                       DC-ALPHA-CENTURY.                          
LGC005                                                                  
LGC005     MOVE ZEROS    TO  DC-EDIT1-MONTH                             
LGC005                       DC-EDIT1-DAY                               
LGC005                       DC-EDIT1-YEAR                              
LGC005                       DC-EDIT2-MONTH                             
LGC005                       DC-EDIT2-DAY                               
LGC005                       DC-EDIT2-YEAR                              
LGC005                       DC-YMD-YEAR                                
LGC005                       DC-YMD-MONTH                               
LGC005                       DC-YMD-DAY                                 
LGC005                       DC-MDY-YEAR                                
LGC005                       DC-MDY-MONTH                               
LGC005                       DC-MDY-DAY                                 
LGC005                       DC-ALPHA-DAY                               
LGC005                       DC-ALPHA-YEAR                              
LGC005                       DC-ELAPSED-MONTHS                          
LGC005                       DC-ODD-DAYS-OVER                           
LGC005                       DC-ELAPSED-DAYS                            
LGC005                       DC-JULIAN-YEAR                             
LGC005                       DC-JULIAN-DAYS                             
LGC005                       DC-DAYS-IN-MONTH                           
LGC005                       DC-DAY-OF-WEEK                             
LGC005                       DC-DAY-OF-WEEK2.                           
LGC005 INIT-DATE-EXIT.                                                  
LGC005     EXIT.                                                        
       ABEND-PROGRAM                   SECTION.                         
           DIVIDE  WS-ZERO BY     WS-ZERO GIVING WS-ZERO.               
                                                                        
       ABEND-EXIT.                                                      
               EXIT.                                                    
      ******************************************************************
                                                                        
       9980-OS-CALL-EOJ SECTION.                                        
           CALL 'EOJ'      USING OS-EOJ.                                
           GOBACK.                                                      
                                                                        
