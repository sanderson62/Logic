       IDENTIFICATION DIVISION.
     
       PROGRAM-ID.                 EL354.
     
     
      *AUTHOR.     AJRA.
      ******************************************************************
      *REMARKS.
      *     THIS PROGRAM IS USED TO RELEASE THE CLAIM CHECKS DURING THE
      *     BATCH CYCLE.
      *     IT IS LOOSELY MODELLED AFTER THE ONLINE PROGRAM EL175.
      *
      *     INPUT:   ELACTQ
      *              ELTRLR
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 080409    2008070200001  AJRA  INITIAL PROGRAM
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
032212* 032212    2011110200001  PEMA  AHL CHANGES
102413* 102413  CR2013100800001  AJRA  RELEASE SPECIAL CHKS FIRST
061617* 061617  IR2017061500002  PEMA  ID TRANS AS ACH VS CHECK
040219* 040219  CR2019032800001  PEMA  STOP SURVEY ON SOME FIN PMTS
100319* 100319  CR2019070900003  PEMA  EXCLUDE CU BUS FROM SURVEY FIN PMTS
070820* 070820  IR2020070800001  PEMA  Initialize previous key field
062821* 063021  CR2021021600001  TANA  ADD FNL COMPANY CODE
      ******************************************************************
      
       ENVIRONMENT DIVISION.
      
       INPUT-OUTPUT SECTION.
      
       FILE-CONTROL.
           SELECT DISK-DATE    ASSIGN TO SYS019-FBA1-S-SYS019.
           
           SELECT ELACTQ       ASSIGN TO SYS011-3380-ELACTQ
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      AQ-CONTROL-PRIMARY
                                                IN ELACTQ
                                FILE STATUS     ACTQ-STATUS.
      
           SELECT ELCNTL       ASSIGN TO SYS012-3380-ELCNTL
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CF-CONTROL-PRIMARY
                                                IN ELCNTL
                                FILE STATUS     CNTL-STATUS.
      
           SELECT ELTRLR       ASSIGN TO SYS013-3380-ELTRLR
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      AT-CONTROL-PRIMARY
                                                IN ELTRLR
                                FILE STATUS     TRLR-STATUS.
           
           SELECT ELCHKQ       ASSIGN TO SYS014-3380-ELCHKQ
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CQ-CONTROL-PRIMARY
                                                IN ELCHKQ
                                FILE STATUS     CHKQ-STATUS.
      
           SELECT DLYACTV      ASSIGN TO SYS015-3380-DLYACTV            
                                ORGANIZATION    INDEXED            
                                ACCESS          DYNAMIC               
                                RECORD KEY      DA-KEY
                                                IN DLYACTV               
                                FILE STATUS     DLYA-STATUS.

100319     SELECT ERACCT               COPY ERCACCTS.
100319     SELECT ELMSTR               COPY ELCMSTRS.

           SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.
      
      
       DATA DIVISION.
      
       FILE SECTION.
      
       FD  DISK-DATE
           COPY ELCDTEFD.
      
       FD  ELACTQ.
           COPY ELCACTQ.
      
       FD  ELCNTL.      
           COPY ELCCNTL.
      
       FD  ELTRLR.      
           COPY ELCTRLR.
      
       FD  ELCHKQ.      
           COPY ELCCHKQ.
           
       FD  DLYACTV.
           COPY ELCDAR.

100319 FD  ERACCT.
100319     COPY ERCACCT.

100319 FD  ELMSTR.
100319     COPY ELCMSTR.

       FD  PRNTR
           COPY ELCPRTFD.

       WORKING-STORAGE SECTION.      

       01  WS-DATE-AREA.                                               
           12  SAVE-DATE           PIC X(8)    VALUE SPACES.           
           12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.           
                                                                       
       01  WS-PREV-CLAIM.                                              
           12  WS-PREV-COMPANY             PIC X.                      
           12  WS-PREV-CARRIER             PIC X.                      
           12  WS-PREV-CLAIM-NO            PIC X(7)   VALUE LOW-VALUES.
           12  WS-PREV-CERT-NO             PIC X(11).                  
           12  WS-PREV-SEQ-NO              PIC S9(4)  COMP VALUE ZEROS.
           12  WS-PREV-RECORD-TYPE         PIC X.                      
                                                                       
                                                                       
       01  FILLER                          COMP-3.                     
           12  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.     
           12  WS-UPDATE-SW                PIC S9      VALUE ZERO.     
           12  WS-PAYMENT-COUNTER          PIC S9(3)   VALUE ZERO.     
           12  WS-PMT-UNAPPROVED-COUNT     PIC S9(3)   VALUE ZERO.
                                                                       
           12  TIME-IN                     PIC S9(7)   VALUE ZERO.     
           12  TIME-OUT REDEFINES TIME-IN  PIC S999V9(4).              
           12  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.               
                                                                       
061617     12  WS-RELEASED-CHK-COUNT       PIC S9(5)   VALUE ZERO.     
061617     12  WS-RELEASED-ACH-COUNT       PIC S9(5)   VALUE ZERO.     
           12  WS-RELEASED-COUNT-TOT       PIC S9(5)   VALUE ZERO.     
           12  WS-UNAPPROVED-COUNT         PIC S9(5)   VALUE ZERO.
                                                                       
061617     12  WS-RELEASED-CHK-AMOUNT      PIC S9(9)V99 VALUE ZERO.    
061617     12  WS-RELEASED-ACH-AMOUNT      PIC S9(9)V99 VALUE ZERO.    
           12  WS-RELEASED-AMOUNT-TOT      PIC S9(9)V99 VALUE ZERO.    
           12  WS-UNAPPROVED-AMOUNT        PIC S9(9)V99 VALUE ZERO.
                                                                       
           12  WS-NON-CASH-REL-CNT         PIC S9(05)   VALUE ZERO.    
           12  WS-NON-CASH-REL-AMT         PIC S9(9)V99 VALUE ZERO.    
           
       01  WS-AREA.
           12  WS-RELEASE-TYPE             PIC X       VALUE '0'.
               88  RELEASE-LIFE                        VALUE '1'.
               88  RELEASE-AUTO                        VALUE '2'.
               88  RELEASE-ALL                         VALUE '3'.
102413         88  RELEASE-SPECIAL                     VALUE '4'.           
                                                                       
          12  WS-CONTROL-UPDATE-IND        PIC X       VALUE 'N'.
              88  CONTROL-UPDATED                      VALUE 'Y'.
              88  CONTROL-NOT-UPDATED                  VALUE 'N'.
                                                                       
       01  FILLER     COMP  SYNC.                                      
           12  SC-ITEM                     PIC S9(4)   VALUE +0001.    
                                                                       
           12  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.     
                                                                       
           12  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.     
           12  WS-CHECK-QUE-COUNTER-2      PIC S9(8)   VALUE ZERO.     
           12  WS-CHECK-QUE-COUNTER-3      PIC S9(8)   VALUE ZERO.     
                                                                       
           12  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.      
           12  WS-CHECK-COUNTER-2          PIC S9(4)   VALUE +10.      
           12  WS-CHECK-COUNTER-3          PIC S9(4)   VALUE +10.      

100319 01  ws-account-business-type        pic xx value spaces.
       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  WS-H1-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(42)     VALUE SPACES.
           05  WS-H1-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(37)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE 'PAGE '.
           05  WS-H1-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(04)     VALUE SPACES.

       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(47)     VALUE SPACES.
           05  WS-H2-TITLE                 PIC X(35)
               VALUE 'CLAIM CHECKS AUTOMATICALLY RELEASED'.
           05  FILLER                      PIC X(39)     VALUE SPACES.
           05  WS-H2-REPORT-ID             PIC X(06)     VALUE 'EL354A'.
           05  FILLER                      PIC X(05)     VALUE SPACES.

                                              
       01  FILLER.                                                     
           12  WS-CONTROL-FILE-KEY.                                    
               16  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.   
               16  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.   
               16  FILLER                  PIC XX      VALUE SPACES.   
               16  WS-CFK-BENEFIT-NO       PIC XX      VALUE SPACES.   
               16  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO      
                                           COMP.                       
                                                                       
           12  WS-ELMSTR-KEY               PIC X(20).                  
           12  WS-ACTIVITY-TRAILERS-KEY.                               
               16  WS-ATK-COMPANY-CD       PIC X.                      
               16  WS-ATK-CARRIER          PIC X.                      
               16  WS-ATK-CLAIM-NO         PIC X(7).                   
               16  WS-ATK-CERT-NO          PIC X(11).                  
               16  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.             
                                                                       
           12  WS-LAST-ACTIVITY-TRAILERS-KEY 
                                          PIC X(22) VALUE LOW-VALUES.
                                                                       
           12  WS-ACTIVITY-QUE-KEY.                                    
               16  WS-AQK-COMPANY-CD      PIC X.                      
               16  WS-AQK-CARRIER         PIC X.                      
               16  WS-AQK-CLAIM-NO        PIC X(7).                   
               16  WS-AQK-CERT-NO         PIC X(11).                  
                                                                       
           12  WS-LAST-ACTIVITY-QUE-KEY   PIC X(20) VALUE LOW-VALUES. 
                                                                       
           12  WS-CLAIM-MASTER-KEY.                                    
               16  WS-CK-COMPANY-CD       PIC X.                       
               16  WS-CK-CARRIER          PIC X.                       
               16  WS-CK-CLAIM-NO         PIC X(7).                    
               16  WS-CK-CERT-NO          PIC X(11).                   
                                                                       
           12  WS-NOTE-KEY.                                            
               16  WS-EN-COMPANY-CD       PIC X.                       
               16  WS-EN-CARRIER          PIC X.                       
               16  WS-EN-CLAIM-NO         PIC X(7).                    
               16  WS-EN-CERT-NO          PIC X(11).                   
               16  WS-EN-PAYMENT-SEQ-NO   PIC S9(4)  COMP.             
               16  WS-EN-RECORD-TYPE      PIC X.                       
                                                                       
           12  W-NOTE-KEY.                                             
               16  W-NOTE-COMP-CD         PIC X.                       
               16  W-NOTE-CERT-KEY.                                    
                   20  W-NOTE-CARRIER     PIC X.                       
                   20  W-NOTE-GROUPING    PIC X(6).                    
                   20  W-NOTE-STATE       PIC XX.                      
                   20  W-NOTE-ACCOUNTG    PIC X(10).                   
                   20  W-NOTE-EFF-DT      PIC XX.                      
                   20  W-NOTE-CERT-NO     PIC X(11).                   
                                                                       
           12  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.   
                                                                       
           12  WS-FORMS-PRINTER            PIC X(4) VALUE SPACES.      
                                                                       
           12  WS-SPACES                   PIC X       VALUE SPACES.   
                                                                       
           12  WS-CLAIM-NO                 PIC X(7)    VALUE SPACES.   
           12  WS-CARRIER                  PIC X       VALUE SPACES.   
                                                                       
           12  WS-CERT-NO.                                             
               16  WS-CERT-PRIME.                                      
                   20  WS-CERT-PRIME-1-3   PIC X(3)    VALUE SPACES.   
                   20  WS-CERT-PRIME-4-10  PIC X(7)    VALUE SPACES.   
               16  WS-CERT-SFX             PIC X       VALUE SPACES.   
                                                                       
           12  WS-JULIAN-YYDDD             PIC 9(5).
                                                                                                                                              
           12  WS-TOTAL-LINE1.                                         
               16  FILLER                  PIC X(01)   VALUE SPACES.
               16  WS-TL1-TYPE             PIC X(15).
102413         16  FILLER                  PIC X(15)   VALUE           
102413             ' CONTROL GROUP '.                                    
               16  WS-TL1-CONTROL-GROUP    PIC 9(7)-   VALUE ZEROS.    
               16  WS-TL1-RELEASE          PIC X(20)   VALUE           
                   ' RELEASED'.                                        
                                                                       
           12  WS-TOTAL-LINE2.                                         
               16  FILLER                  PIC X(01)   VALUE SPACES.
061617         16  WS-TL2-COUNT            PIC ZZ,ZZ9  VALUE ZEROS.
               16  FILLER                  PIC X(6)    VALUE       
                   ' CHECK'.                                           
061617         16  WS-TL2-PLURAL           PIC X       VALUE       
                   'S'.                                                
               16  FILLER                  PIC X(18)   VALUE       
                   ' IN THE AMOUNT OF'.                                
061617         16  WS-TL2-AMOUNT           PIC Z,ZZZ,ZZ9.99.           
                                                                       
061617     12  WS-TOTAL-LINE2b.
061617         16  FILLER                  PIC X(01)   VALUE SPACES.
061617         16  WS-TL2b-COUNT           PIC ZZ,ZZ9  VALUE ZEROS.
061617         16  FILLER                  PIC X(9)    VALUE
061617             ' ACH PMNT'.
061617         16  WS-TL2b-PLURAL          PIC X       VALUE
061617             'S'.
061617         16  FILLER                  PIC X(18)   VALUE
061617             ' IN THE AMOUNT OF'.
061617         16  WS-TL2b-AMOUNT          PIC Z,ZZZ,ZZ9.99.

           12  WS-TOTAL-LINE3.
               16  FILLER                  PIC X(01)   VALUE SPACES.
               16  FILLER                  PIC X(04)   VALUE SPACES.
               16  FILLER                  PIC X(20)   VALUE
                   'AWAITING APPROVAL - '.
               16  WS-TL3-COUNT            PIC ZZ,ZZ9  VALUE ZEROS.
               16  FILLER                  PIC X(6)    VALUE       
                   ' CHECK'.                                           
               16  WS-TL3-PLURAL           PIC X       VALUE       
                   'S'.                                                
               16  FILLER                  PIC X(18)   VALUE       
                   ' IN THE AMOUNT OF'.                                
               16  WS-TL3-AMOUNT           PIC Z,ZZZ,ZZ9.99.           
                                                                             
           12  WS-NON-CASH-TOTAL-LINE.                                 
               16  FILLER                  PIC X(01)   VALUE SPACES.
               16  WS-NC-TL1-COUNT         PIC ZZZZ9.                  
               16  WS-NC-TL1-LIT           PIC X(32)   VALUE SPACES.   
                                                                       
           12  WS-GRAND-TOTAL-LINE1.                                         
               16  FILLER                  PIC X(1)    VALUE '-'.
               16  FILLER                  PIC X(10)   VALUE SPACES.
               16  FILLER                  PIC X(24)   VALUE       
                   'TOTAL CHECKS RELEASED - '.                                           
               16  WS-GTL-COUNT            PIC ZZ,ZZ9  VALUE ZEROS.

           12  WS-GRAND-TOTAL-LINE2.                                         
               16  FILLER                  PIC X(1)    VALUE ' '.
               16  FILLER                  PIC X(10)   VALUE SPACES.
               16  FILLER                  PIC X(24)   VALUE       
                   'TOTAL AMOUNT RELEASED - '.                                
               16  WS-GTL-AMOUNT           PIC Z,ZZZ,ZZ9.99.           
                                                                       
           12  WS-PMT-APPROVAL             PIC X.                      
                88 WS-PMT-APPROVAL-USED            VALUE 'Y' 'G'.      
                                                                       
           12  WS-SAVE-NOTE-RECORD         PIC X(310).                 

           12  WS-MONTH-END-SAVE           PIC XX.                     

       01  MISC.
040219     05  ws-survey-ind           pic x  value ' '.
040219         88  survey-turned-off     value 'Y'.
           05  CHKQ-STATUS          PIC XX            VALUE '00'.
           05  CNTL-STATUS          PIC XX            VALUE '00'.
           05  TRLR-STATUS          PIC XX            VALUE '00'.
           05  ACTQ-STATUS          PIC XX            VALUE '00'.
100319     05  eracct-file-status   PIC XX            VALUE '00'.
100319     05  elmstr-file-status   PIC XX            VALUE '00'.
           05  DLYA-STATUS          PIC XX            VALUE '00'.
           05  WS-RETURN-CODE       PIC X(4)          VALUE ZEROS.
           05  WS-ABEND-MESSAGE     PIC X(80)         VALUE SPACES.
           05  WS-ABEND-FILE-STATUS PIC XX            VALUE SPACES.
           05  WS-ZERO              PIC S9            VALUE ZERO.
           05  PGM-SUB              PIC S999  COMP-3  VALUE +020.
           05  WS-LINE-COUNT        PIC S9(03)        VALUE +0.
           05  WS-LINE-COUNT-MAX    PIC S9(03)        VALUE +55.
                                                                      
           EJECT                                                       
                                       COPY ELCDATE.         

                                       COPY ELCDTECX.
 
                                       COPY ELCDTEVR.

                                                 
       PROCEDURE DIVISION.                                             
                                   COPY ELCDTERX.

           ACCEPT WS-JULIAN-YYDDD FROM DAY
           MOVE WS-JULIAN-YYDDD       TO DC-JULIAN-YYDDD.
           MOVE '5'                   TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERSION THRU 8599-EXIT.                               
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                   
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE
                                          WS-CURRENT-DATE.     

           MOVE COMPANY-NAME                  TO WS-H1-COMPANY-NAME.
           MOVE SAVE-DATE                     TO WS-H1-DATE.
           MOVE 1                             TO WS-H1-PAGE.
                                                                                                                                                                            
       0000-MAIN-LOGIC.                                                
                                                                       
      *    NOTE *******************************************************
      *         *    FOR CID DO LIFE PAYMENTS FIRST FOLLOWED BY AUTO  *
      *         *        PAYMENTS AND FOLLOWED BY ALL OTHER PAYMENTS  *
      *         *    FOR DCC DO ALL PAYMENTS                          *
      *         *******************************************************
           
           PERFORM 0010-INITIALIZE THRU 0099-EXIT.                                                            
           
102413     SET RELEASE-SPECIAL TO TRUE.
102413     MOVE 'SPECIAL RELEASE' TO WS-TL1-TYPE.
102413     PERFORM 0200-PROCESS-CHECK-RELEASE THRU 0299-EXIT.
102413
062821     IF DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL'
               SET RELEASE-LIFE TO TRUE
               MOVE 'LIFE PAYMENTS  ' TO WS-TL1-TYPE
               PERFORM 0200-PROCESS-CHECK-RELEASE THRU 0299-EXIT
               SET RELEASE-AUTO TO TRUE
               MOVE 'AUTO PAYMENTS  ' TO WS-TL1-TYPE
               PERFORM 0200-PROCESS-CHECK-RELEASE THRU 0299-EXIT
           END-IF.

           SET RELEASE-ALL TO TRUE.
062821     IF DTE-CLIENT = 'CID' OR 'AHL' or 'VPP' OR 'FNL'
               MOVE 'ALL OTHER PMTS ' TO WS-TL1-TYPE
           ELSE
               MOVE 'ALL DCC PMTS   ' TO WS-TL1-TYPE
           END-IF.
           PERFORM 0200-PROCESS-CHECK-RELEASE THRU 0299-EXIT.
           
           MOVE WS-RELEASED-COUNT-TOT  TO WS-GTL-COUNT.
           MOVE WS-RELEASED-AMOUNT-TOT TO WS-GTL-AMOUNT.

           DISPLAY WS-GRAND-TOTAL-LINE1     
           MOVE WS-GRAND-TOTAL-LINE1  TO PRT
           PERFORM 2000-WRITE         THRU 2099-EXIT
           DISPLAY WS-GRAND-TOTAL-LINE2     
           MOVE WS-GRAND-TOTAL-LINE2  TO PRT
           PERFORM 2000-WRITE         THRU 2099-EXIT
           
           PERFORM 9999-END-OF-JOB THRU 9999-EXIT.
           
           GOBACK.
           

       0010-INITIALIZE.                                                     

           OPEN OUTPUT PRNTR.
           
           OPEN I-O ELACTQ
           IF ACTQ-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ACTQ-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELACTQ FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O ELCNTL
           IF CNTL-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O ELTRLR
           IF TRLR-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O ELCHKQ
           IF CHKQ-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELCHKQ FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O DLYACTV
           IF DLYA-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE DLYA-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON DLYACTV FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

100319     OPEN INPUT ERACCT
100319     IF ERACCT-FILE-STATUS = '00' OR '97'
100319        CONTINUE
100319     ELSE
100319        MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
100319        MOVE 'OPEN ERROR ON ERACCT FILE'
100319                                 TO WS-ABEND-MESSAGE
100319        PERFORM ABEND-PGM
100319     END-IF

100319     OPEN INPUT ELMSTR
100319     IF ELMSTR-FILE-STATUS = '00' OR '97'
100319        CONTINUE
100319     ELSE
100319        MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
100319        MOVE 'OPEN ERROR ON ELMSTR FILE'
100319                                 TO WS-ABEND-MESSAGE
100319        PERFORM ABEND-PGM
100319     END-IF

           MOVE LOW-VALUES             TO  WS-MONTH-END-SAVE.               

           MOVE WS-HEADING1                 TO PRT.
           PERFORM 2000-WRITE               THRU 2099-EXIT.

           MOVE WS-HEADING2                 TO PRT.
           PERFORM 2000-WRITE               THRU 2099-EXIT.

           MOVE SPACES                      TO PRT.
           MOVE ' '                         TO P-CTL.
           PERFORM 2000-WRITE               THRU 2099-EXIT.
                                                                       

       0099-EXIT.
           EXIT.
           
           
       0200-PROCESS-CHECK-RELEASE.                                     
                                                                       
      *    NOTE *******************************************************
      *         *      GET THE CONTROL GROUP NUMBER FROM THE COMPANY  *
      *         *  CONTROL RECORD OF THE CONTROL FILE.                *
      *         *******************************************************
      
061617     MOVE +0                     TO  WS-RELEASED-CHK-COUNT
061617                                     WS-RELEASED-ACH-COUNT
061617                                     WS-RELEASED-CHK-AMOUNT
061617                                     WS-RELEASED-ACH-AMOUNT
                                           WS-UNAPPROVED-COUNT
                                           WS-UNAPPROVED-AMOUNT.
                                           
070820     move low-values             to WS-LAST-ACTIVITY-QUE-KEY
           SET CONTROL-NOT-UPDATED     TO TRUE.                                           
                                                                       
           MOVE DTE-CLIENT             TO  CF-CONTROL-PRIMARY.
           MOVE '1'                    TO  CF-RECORD-TYPE.
           MOVE ZERO                   TO  CF-SEQUENCE-NO.

           READ ELCNTL KEY IS CF-CONTROL-PRIMARY
           IF CNTL-STATUS = '00'
              CONTINUE
           ELSE
              MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'READ ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF
                                                                       
           ADD +1                      TO  CF-CO-CHECK-QUE-COUNTER.    
                                                                       
           IF CO-QUE-COUNT-RESET                                       
               MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.    
                                                                       
           MOVE CF-FORMS-PRINTER-ID      TO  WS-FORMS-PRINTER          
           MOVE CF-CO-CHECK-QUE-COUNTER  TO  WS-CHECK-QUE-COUNTER.     
           MOVE CF-PAYMENT-APPROVAL-SW   TO  WS-PMT-APPROVAL.          
           MOVE CF-CURRENT-MONTH-END     TO  WS-MONTH-END-SAVE.        
                                                                                                                                              
      *    NOTE *******************************************************
      *         *      BROWSE THE ACTIVITY QUEUE TO FIND ANY CLAIMS   *
      *         *  THAT HAVE PAYMENTS PENDING.                        *
      *         *                                                     *
      *         *      WHEN A PAYMENT IS PENDING, BROWSE THE ACTIVITY *
      *         *  TRAILERS TO LOCATE THE PAYMENT TRAILER RECORDS. IF *
      *         *  THE PAYMENT TRAILER MEETS THE QUALIFICATION FOR    *
      *         *  THIS CHECK RELEASE RUN, GENERATE A CHECK QUEUE     *
      *         *  RECORD.                                            *
      *         *******************************************************
                                                                       
           MOVE LOW-VALUES             TO  AQ-CONTROL-PRIMARY.        
           MOVE DTE-CLASIC-COMPANY-CD  TO  AQ-COMPANY-CD.          
                                                                       
           START ELACTQ KEY IS NOT < AQ-CONTROL-PRIMARY.
032212     if actq-status = '10' or '23'
032212        display ' nothing to process '
032212        go to 0299-exit
032212     else
           IF ACTQ-STATUS NOT = '00'
               MOVE ACTQ-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'START ERROR ON ELACTQ FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.

       0220-READNEXT-ELACTQ.
           READ ELACTQ NEXT RECORD
               AT END  
               GO TO 0270-END-OF-SEARCH.
               
           IF ACTQ-STATUS NOT = '00'
               MOVE ACTQ-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ NEXT ERROR ON ELACTQ FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.
                                                                       
           IF AQ-PAYMENT-COUNTER NOT NUMERIC                           
               MOVE ZEROS              TO AQ-PAYMENT-COUNTER.          
                                                                       
           IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                      
               MOVE ZEROS              TO AQ-PMT-UNAPPROVED-COUNT.     
                                                                       
           IF AQ-CONTROL-PRIMARY = WS-LAST-ACTIVITY-QUE-KEY           
               GO TO 0220-READNEXT-ELACTQ.                             
                                                                       
           MOVE AQ-CONTROL-PRIMARY     TO  WS-LAST-ACTIVITY-QUE-KEY.   
                                                                       
           IF AQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                    
               GO TO 0270-END-OF-SEARCH.                               
                                                                       
           IF NOT PENDING-PAYMENTS                                     
               GO TO 0220-READNEXT-ELACTQ.            
                                
           IF WS-PMT-APPROVAL-USED                                     
              IF AQ-PMT-UNAPPROVED-COUNT NOT EQUAL +0                  
                 PERFORM 3000-TOTAL-UNAPPROVED THRU 3999-EXIT.
                                                                       
       0240-MAIN-LOGIC.                                                

040219     perform 1000-check-for-survey-set-off
040219                                 thru 1000-exit

           MOVE ZERO                 TO  WS-UPDATE-SW.               
                                                                       
           MOVE AQ-PAYMENT-COUNTER   TO  WS-PAYMENT-COUNTER.         
                                                                      
           MOVE AQ-CONTROL-PRIMARY   TO  AT-CONTROL-PRIMARY.   
           MOVE ZERO                 TO  AT-SEQUENCE-NO.         

           MOVE LOW-VALUES           TO  WS-LAST-ACTIVITY-TRAILERS-KEY.

           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY.
           IF TRLR-STATUS NOT = '00'
               MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'START ERROR ON ELTRLR FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.
                                                                       
       0250-MAIN-LOGIC.                                                
           IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO                 
               GO TO 0260-MAIN-LOGIC.                                  
                                                                       
       0255-READNEXT-ELTRLR.                                           
           READ ELTRLR NEXT RECORD.
011410     IF TRLR-STATUS NOT = '00' AND '10'
               MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ NEXT ERROR ON ELTRLR FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.
                                                                       
011410     IF TRLR-STATUS = '10'
011410        GO TO 0260-MAIN-LOGIC
011410     END-IF

           IF AT-CONTROL-PRIMARY = WS-LAST-ACTIVITY-TRAILERS-KEY 
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                       
           MOVE AT-CONTROL-PRIMARY  TO WS-LAST-ACTIVITY-TRAILERS-KEY.  
                                                                       
           IF AQ-COMPANY-CD NOT = AT-COMPANY-CD  OR            
              AQ-CARRIER    NOT = AT-CARRIER     OR            
              AQ-CLAIM-NO   NOT = AT-CLAIM-NO    OR            
              AQ-CERT-NO    NOT = AT-CERT-NO                   
               GO TO 0260-MAIN-LOGIC.                                  
                                                                       
           IF AT-TRAILER-TYPE NOT = '2'                                
               GO TO 0255-READNEXT-ELTRLR.                             

102413     IF RELEASE-SPECIAL AND 
102413        AT-SPECIAL-RELEASE NOT EQUAL 'Y'
102413            GO TO 0255-READNEXT-ELTRLR
102413     END-IF.
102413
           IF RELEASE-LIFE AND NOT PAID-FOR-LIFE              
              GO TO 0255-READNEXT-ELTRLR
           END-IF.

           IF RELEASE-AUTO AND NOT ONLINE-AUTO-PMT              
              GO TO 0255-READNEXT-ELTRLR
           END-IF.
                                                                       
           IF AT-AMOUNT-PAID   NEGATIVE                                
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                                                                                              
           IF OFFLINE-PMT                                              
               SUBTRACT +1 FROM WS-PAYMENT-COUNTER                     
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                       
           IF AT-VOID-DT NOT = LOW-VALUES                              
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                       
           IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                       
           IF AT-CASH-PAYMENT = 'N'                                  
               GO TO 0257-CONTINUE-PAYMENTS.                           
                                                                                                                                              
           IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE         
               GO TO 0255-READNEXT-ELTRLR.                             
                                                                       
           IF AT-CHECK-QUE-CONTROL NOT = ZERO
               GO TO 0255-READNEXT-ELTRLR.                             

           IF WS-PMT-APPROVAL-USED                                     
              IF AT-PAYMENT-APPROVAL-SW NOT = 'A'                      
                 GO TO 0255-READNEXT-ELTRLR.                           

       0257-CONTINUE-PAYMENTS.                                         

      *    NOTE *******************************************************
      *         *      THE PAYMENT TRAILER HAS MET ALL OF THE         *
      *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *
      *         *  GENERATE A CHECK QUEUE RECORD.                     *
      *         *******************************************************

100319     perform 1100-get-elmstr     thru 1100-exit
100319     perform 1200-get-eracct     thru 1200-exit

      *** IF THE CONTROL FILE HAS NOT BEEN UPDATED YET DO IT NOW.                                                                 
           IF CONTROL-NOT-UPDATED
              SET CONTROL-UPDATED TO TRUE 
                                                                         
              REWRITE CONTROL-FILE 
              IF CNTL-STATUS NOT = '00'
                 MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'REWRITE ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
              END-IF
           END-IF.

           SUBTRACT +1 FROM AQ-PAYMENT-COUNTER                         
                            WS-PAYMENT-COUNTER.                        
                                                                       
           MOVE +1                         TO  WS-UPDATE-SW.           
                                                                       
           IF AT-CASH-PAYMENT = 'N'                                  
               ADD +1                      TO  WS-NON-CASH-REL-CNT     
               ADD AT-AMOUNT-PAID          TO  WS-NON-CASH-REL-AMT     
               GO TO 0259-REWRITE-PMT-TRLR
           END-IF.   
                                                                       
061617     IF AT-ACH-PAYMENT = 'Y'
061617        ADD +1                   TO WS-RELEASED-ACH-COUNT
061617        ADD AT-AMOUNT-PAID       TO WS-RELEASED-ACH-AMOUNT
061617     ELSE
061617        ADD +1                   TO WS-RELEASED-CHK-COUNT
061617        ADD AT-AMOUNT-PAID       TO WS-RELEASED-CHK-AMOUNT
061617     END-IF

061617     ADD +1                      TO WS-RELEASED-COUNT-TOT
061617     ADD AT-AMOUNT-PAID          TO WS-RELEASED-AMOUNT-TOT
                                                                                                                                                                                                                     
           MOVE SPACES                 TO  CHECK-QUE.                  
                                                                       
       0258-CONTINUE-ELCHKQ.                                           
                                                                                                                                              
           MOVE 'Q'                    TO  CQ-ENTRY-TYPE.              
           MOVE AT-CARRIER             TO  CQ-CARRIER.
           MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.                
                                                                       
           MOVE 'CQ'                   TO  CQ-RECORD-ID.               
           MOVE AT-COMPANY-CD          TO  CQ-COMPANY-CD               
                                           CQ-COMPANY-CD-A1.           
           MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER           
                                           CQ-CONTROL-NUMBER-A1        
                                           AT-CHECK-QUE-CONTROL.       
           MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER          
                                           AT-CHECK-QUE-SEQUENCE       
                                           CQ-SEQUENCE-NUMBER-A1.      
           ADD +1  TO  WS-CHECK-COUNTER.                               
                                                                       
           PERFORM 0920-WRITE-DLYACTV  THRU 0920-EXIT.                  
                                                                                                                                              
           MOVE AT-CERT-NO             TO  CQ-CERT-NO.                 
           MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.              
           MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.          
           MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.       
           MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.            
           MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.            
           MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.            
           MOVE ZERO                   TO  CQ-TIMES-PRINTED            
                                           CQ-PRINT-AT-HHMM.           
           MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.           
                                                                       
           MOVE LOW-VALUES             TO  CQ-CHECK-WRITTEN-DT.        
                                                                       
           MOVE +3540                  TO  CQ-LAST-UPDATED-BY.         
           MOVE SAVE-BIN-DATE          TO  CQ-LAST-UPDATED-DT.         
           MOVE WS-TIME                TO  CQ-LAST-UPDATED-HHMMSS.     
                                                                       
           WRITE CHECK-QUE.
           IF CHKQ-STATUS NOT = '00'
              MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'WRITE ERROR ON ELCHKQ FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF.
                                                                       
       0259-REWRITE-PMT-TRLR.                                          

040219     if survey-turned-off
040219        if (at-print-survey <> 'N')
040219*          and (at-payment-type <> 'I')
040219           and (at-payment-type = '2')
040219           move 'N'              to at-print-survey
040219        end-if
040219     else
040219** Just in case... There may be more than one payment for this
040219** claim and they turned off survey on only one of them :)
040219        if at-print-survey = 'N'
040219           set survey-turned-off to true
100319        else
100319           if (((dte-client = 'DCC')
100319              and (at-carrier = '7'))
100319              or (ws-account-business-type = '04' and
100319                  cl-cert-account(9:2) = 'OB'))
100319              and at-payment-type = '2'
100319              move 'N'           to at-print-survey
100319           end-if
100319        end-if
040219     end-if

           MOVE 'E354'                 TO  AT-PAYMENT-LAST-UPDATED-BY. 
           MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.   
                                                                       
           IF AT-CASH-PAYMENT = 'N'                                    
               MOVE SAVE-BIN-DATE      TO  AT-CHECK-WRITTEN-DT         
               MOVE WS-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.           
                                                                                                                                            
           REWRITE ACTIVITY-TRAILERS.
           IF TRLR-STATUS NOT = '00'
              MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'REWRITE ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF.
                                                                       
           GO TO 0250-MAIN-LOGIC.                                      
                                                                       
           EJECT                                                       
       0260-MAIN-LOGIC.                                                
           IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO                 
               MOVE SPACES             TO  AQ-PENDING-PAYMENT-FLAG
           END-IF.    
                                                                       
           MOVE +3540                  TO  AQ-LAST-UPDATED-BY.         
                                                                       
           IF WS-UPDATE-SW = ZERO                                      
              GO TO 0220-READNEXT-ELACTQ 
           END-IF
                                                                       
           IF AQ-PENDING-ACTIVITY-FLAGS = SPACES                   
               DELETE ELACTQ RECORD
           ELSE                                                   
               REWRITE ACTIVITY-QUE
               IF ACTQ-STATUS NOT = '00'
                  MOVE ACTQ-STATUS TO WS-ABEND-FILE-STATUS
                  MOVE 'REWRITE ERROR ON ELACTQ FILE' 
                                       TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF
           END-IF
                                                                       
           GO TO 0220-READNEXT-ELACTQ.
                                                                       
           EJECT                                                       
       0270-END-OF-SEARCH.                                             
                                                                                                                                             
           IF CONTROL-NOT-UPDATED
               MOVE ZEROS                TO  WS-TL1-CONTROL-GROUP
           ELSE
               MOVE WS-CHECK-QUE-COUNTER TO  WS-TL1-CONTROL-GROUP
           END-IF.
061617     MOVE WS-RELEASED-CHK-COUNT  TO  WS-TL2-COUNT. 
061617     MOVE WS-RELEASED-CHK-AMOUNT TO  WS-TL2-AMOUNT.       
           MOVE WS-UNAPPROVED-COUNT    TO  WS-TL3-COUNT.
           MOVE WS-UNAPPROVED-AMOUNT   TO  WS-TL3-AMOUNT.
                                                                       
061617     IF WS-RELEASED-CHK-COUNT NOT EQUAL 1                              
061617         MOVE 'S'                TO  WS-TL2-PLURAL               
            ELSE                                                       
061617         MOVE ' '                TO  WS-TL2-PLURAL
           END-IF.              
      
061617     MOVE WS-RELEASED-ACH-COUNT  TO WS-TL2B-COUNT
061617     MOVE WS-RELEASED-ACH-AMOUNT TO WS-TL2B-AMOUNT
061617     IF WS-RELEASED-ACH-COUNT <> 1
061617        MOVE 'S'                 TO WS-TL2b-PLURAL
061617     ELSE
061617        MOVE ' '                 TO WS-TL2b-PLURAL
061617     END-IF

           IF WS-UNAPPROVED-COUNT NOT EQUAL 1
               MOVE 'S'                TO  WS-TL3-PLURAL
           ELSE
               MOVE ' '                TO  WS-TL3-PLURAL
           END-IF.
                                                                       
061617     IF (WS-RELEASED-CHK-COUNT > +0)
061617         OR (WS-UNAPPROVED-COUNT > +0)
061617         OR (WS-RELEASED-ACH-COUNT > +0)
                 MOVE 'RELEASED'                             
                              TO  WS-TL1-RELEASE              

                 DISPLAY WS-TOTAL-LINE1     
                 MOVE WS-TOTAL-LINE1        TO PRT
                 PERFORM 2000-WRITE         THRU 2099-EXIT
                                     
                        
                 DISPLAY WS-TOTAL-LINE2                         
                 MOVE WS-TOTAL-LINE2        TO PRT
                 PERFORM 2000-WRITE         THRU 2099-EXIT
                         
061617           DISPLAY WS-TOTAL-LINE2B                         
061617           MOVE WS-TOTAL-LINE2B       TO PRT
061617           PERFORM 2000-WRITE         THRU 2099-EXIT

                 DISPLAY WS-TOTAL-LINE3
                 MOVE WS-TOTAL-LINE3        TO PRT
                 PERFORM 2000-WRITE         THRU 2099-EXIT
                        
                 MOVE SPACES                TO PRT
                 PERFORM 2000-WRITE         THRU 2099-EXIT
                        
                 GO TO 0299-EXIT
           ELSE                                                
                IF WS-NON-CASH-REL-CNT > +0                     
                   MOVE WS-NON-CASH-REL-CNT                    
                                 TO  WS-NC-TL1-COUNT             
                   MOVE ' NON CASH CHECKS RELEASED'            
                                 TO  WS-NC-TL1-LIT               
                   DISPLAY WS-NON-CASH-TOTAL-LINE                 
                   MOVE WS-NON-CASH-TOTAL-LINE TO PRT
                   PERFORM 2000-WRITE          THRU 2099-EXIT
                   
                   MOVE SPACES                 TO PRT
                   PERFORM 2000-WRITE          THRU 2099-EXIT                   
                          
                   GO TO 0299-EXIT
           END-IF.                  
                                                                       
                                                                       
       0299-EXIT.                                            
           EXIT.                                                            
                                                                       
          EJECT                                                       
       0920-WRITE-DLYACTV.                                                       
           MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.                 
           MOVE AT-CONTROL-PRIMARY     TO DA-KEY                                 
           MOVE AT-SEQUENCE-NO         TO DA-TRAILER-SEQ-NO                      
           MOVE 'P'                    TO DA-RECORD-TYPE                         
                                                                                 
           WRITE DAILY-ACTIVITY-RECORD.
           IF DLYA-STATUS NOT = '00' AND '22'
              MOVE DLYA-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'WRITE ERROR ON DLYACTV FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF.
                                                                                 
                                                                                 
       0920-EXIT.                                                                
           EXIT.                                                                 

040219 1000-check-for-survey-set-off.
040219
040219     move ' '                    to ws-survey-ind
040219
040219     MOVE AQ-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
040219     MOVE ZERO                   TO AT-SEQUENCE-NO
040219
040219     START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY.
040219     IF TRLR-STATUS <> '00'
040219        MOVE TRLR-STATUS         TO WS-ABEND-FILE-STATUS
040219        MOVE 'START ERROR ON ELTRLR FILE'
040219                                 TO WS-ABEND-MESSAGE
040219        PERFORM ABEND-PGM
040219     END-IF
040219
040219     .                                                                       
040219 1010-READNEXT-ELTRLR.                                           
040219
040219     READ ELTRLR NEXT RECORD
040219     IF TRLR-STATUS <> '00' AND '10'
040219        MOVE TRLR-STATUS         TO WS-ABEND-FILE-STATUS
040219        MOVE 'READ NEXT ERROR ON ELTRLR FILE'
040219                                 TO WS-ABEND-MESSAGE
040219         PERFORM ABEND-PGM
040219     END-IF
040219
040219     IF TRLR-STATUS = '10'
040219        GO TO 1000-exit
040219     END-IF
040219
040219     IF AQ-COMPANY-CD NOT = AT-COMPANY-CD  OR            
040219        AQ-CARRIER    NOT = AT-CARRIER     OR            
040219        AQ-CLAIM-NO   NOT = AT-CLAIM-NO    OR            
040219        AQ-CERT-NO    NOT = AT-CERT-NO                   
040219        GO TO 1000-exit
040219     end-if
040219
040219     IF AT-TRAILER-TYPE NOT = '2'                                
040219        GO TO 1010-READNEXT-ELTRLR
040219     end-if
040219
040219     if (
              (at-check-written-dt <> low-values)
040219        or (at-to-be-written-dt <> low-values)
              or (payment-not-selected)
              )
040219        and (at-void-dt = low-values)
040219        and (at-print-survey = 'N')
040219        and (at-payment-type <> 'I')
040219        and (not offline-pmt)
040219        set survey-turned-off to true
040219        go to 1000-exit
040219     end-if
040219
040219     GO TO 1010-READNEXT-ELTRLR
040219
040219     .
040219 1000-exit.
040219     exit.

100319 1100-get-elmstr.
100319
100319     move at-control-primary     to cl-control-primary
100319     read elmstr
100319     if elmstr-file-status <> '00'
100319        display ' error-read-elmstr-' elmstr-file-status ' '
100319           at-carrier ' ' at-claim-no ' ' at-cert-no
100319        perform abend-pgm
100319     end-if
100319
100319     .
100319 1100-exit.
100319     exit.

100319 1200-get-eracct.
100319
100319     move spaces                 to ws-account-business-type
100319     move cl-company-cd          to am-control-primary
100319     move cl-cert-key-data(1:19) to am-control-a
100319     move cl-cert-eff-dt         to am-expiration-dt
100319
100319     START ERACCT KEY >= AM-CONTROL-PRIMARY
100319
100319     IF ERACCT-FILE-STATUS = '00'
100319        READ ERACCT NEXT RECORD
100319        IF ERACCT-FILE-STATUS = '00'
100319           IF (cl-cert-key-data(1:19) = AM-CONTROL-A)
100319              and (cl-company-cd = am-company-cd)
100319              MOVE AM-gpcd       TO ws-account-business-type
100319           END-IF
100319        else
100319           display ' error-read-eracct ' eracct-file-status ' '
100319             am-control-a
100319        END-IF
100319     END-IF
100319
100319     .
100319 1200-exit.
100319     exit.

       2000-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE


           WRITE PRT

           .
       2099-EXIT.
           EXIT.

      
      *****************************************************************
      *    TOTAL THE UNAPPROVED PAYMENTS         
      *****************************************************************
       3000-TOTAL-UNAPPROVED.
           MOVE AQ-PMT-UNAPPROVED-COUNT TO WS-PMT-UNAPPROVED-COUNT.
                                                                     
           MOVE AQ-CONTROL-PRIMARY  TO  AT-CONTROL-PRIMARY. 
           MOVE ZERO                TO  AT-SEQUENCE-NO.
                                                                       
           MOVE LOW-VALUES          TO  WS-LAST-ACTIVITY-TRAILERS-KEY.
                                           
                        
       3100-MAIN-LOGIC. 
           IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO       
               GO TO 3999-EXIT
           END-IF.

           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY.
           IF TRLR-STATUS NOT = '00'
               MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'START ERROR ON ELTRLR FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.
                                                                      
           EJECT                                                      
       3200-READNEXT-ELTRLR.                                          
           IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO       
               GO TO 3999-EXIT
           END-IF.

           READ ELTRLR NEXT RECORD.
011410     IF TRLR-STATUS NOT = '00' AND '10'
               MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ NEXT ERROR ON ELTRLR FILE'
                  TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF.
                                                                      
011410     IF TRLR-STATUS = '10'
011410        GO TO 3999-EXIT
011410     END-IF

           IF AT-CONTROL-PRIMARY = WS-LAST-ACTIVITY-TRAILERS-KEY
               GO TO 3200-READNEXT-ELTRLR.                            
                                                                      
           MOVE AT-CONTROL-PRIMARY  TO  WS-LAST-ACTIVITY-TRAILERS-KEY 
                                                                       
          IF AQ-COMPANY-CD NOT = AT-COMPANY-CD  OR           
              AQ-CARRIER    NOT = AT-CARRIER     OR           
              AQ-CLAIM-NO   NOT = AT-CLAIM-NO    OR           
              AQ-CERT-NO    NOT = AT-CERT-NO                  
               GO TO 3999-EXIT
           END-IF.
                                                                      
           IF AT-TRAILER-TYPE NOT = '2'                               
               GO TO 3200-READNEXT-ELTRLR
           END-IF.
      
           IF AT-AMOUNT-PAID   NEGATIVE                               
               GO TO 3200-READNEXT-ELTRLR
           END-IF.
                                                                      
           IF AT-VOID-DT NOT = LOW-VALUES                             
               GO TO 3200-READNEXT-ELTRLR
           END-IF.
                                                                      
           IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES                    
               GO TO 3200-READNEXT-ELTRLR
           END-IF.
                                                                      
           IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE        
               GO TO 3200-READNEXT-ELTRLR
           END-IF.

102413     IF RELEASE-SPECIAL AND 
102413        AT-SPECIAL-RELEASE NOT EQUAL 'Y'
102413            GO TO 3200-READNEXT-ELTRLR
102413     END-IF.
102413
           IF RELEASE-LIFE AND NOT PAID-FOR-LIFE              
               GO TO 3200-READNEXT-ELTRLR
           END-IF.

           IF RELEASE-AUTO AND NOT ONLINE-AUTO-PMT              
               GO TO 3200-READNEXT-ELTRLR
           END-IF.
           
062821     IF RELEASE-ALL AND (DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL' )
              IF PAID-FOR-LIFE OR ONLINE-AUTO-PMT
                 GO TO 3200-READNEXT-ELTRLR
              END-IF
           END-IF.
102413
102413     IF RELEASE-ALL AND 
102413        AT-SPECIAL-RELEASE = 'Y'
102413            GO TO 3200-READNEXT-ELTRLR
102413     END-IF.
                                            
           IF AT-PAYMENT-APPROVAL-SW NOT = 'U'                        
                 GO TO 3200-READNEXT-ELTRLR
           END-IF.
      
           SUBTRACT +1 FROM WS-PMT-UNAPPROVED-COUNT.
           ADD +1                      TO  WS-UNAPPROVED-COUNT. 
           ADD AT-AMOUNT-PAID          TO  WS-UNAPPROVED-AMOUNT.
          
           GO TO 3200-READNEXT-ELTRLR.
                                                                            
       3999-EXIT.
           EXIT.



       8500-DATE-CONVERSION.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.
                                                                       
       8599-EXIT.                                                      
           EXIT.                                                       
                   
       9999-END-OF-JOB.
       
           CLOSE ELACTQ.
           IF ACTQ-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' ACTQ-STATUS ' ON ELACTQ FILE'
           END-IF.

           CLOSE ELCHKQ.
           IF CHKQ-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' CHKQ-STATUS ' ON ELCHKQ FILE'
           END-IF.

           CLOSE ELCNTL.
           IF CNTL-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' CNTL-STATUS ' ON ELCNTL FILE'
           END-IF.

           CLOSE ELTRLR.
           IF TRLR-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' TRLR-STATUS ' ON ELTRLR FILE'
           END-IF.

           CLOSE DLYACTV.
           IF DLYA-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' DLYA-STATUS ' ON DLYACTV FILE'
           END-IF.

100319     CLOSE ERACCT.
100319     IF ERACCT-FILE-STATUS NOT = '00'
100319        DISPLAY 'CLOSE ERROR ' ERACCT-FILE-STATUS
100319           ' ON ERACCT FILE'
100319     END-IF.

100319     CLOSE ELMSTR.
100319     IF ELMSTR-FILE-STATUS NOT = '00'
100319        DISPLAY 'CLOSE ERROR ' ELMSTR-FILE-STATUS
100319           ' ON ELMSTR FILE'
100319     END-IF.

           CLOSE PRNTR.
           
           IF WS-RELEASED-COUNT-TOT = ZERO
               DISPLAY ' THERE WERE NO CHECKS TO BE RELEASED '
062821         IF DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL'
                   MOVE 'THERE WERE NO CHECKS TO BE RELEASED'
                      TO WS-ABEND-MESSAGE
      *            PERFORM ABEND-PGM
               END-IF
           END-IF.

       9999-EXIT.
           EXIT.

       ABEND-PGM SECTION.
           MOVE +999 TO WS-RETURN-CODE
                                COPY ELCABEND.
