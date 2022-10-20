       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSALTRBL.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM NSADDLTR.cl2

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121802    2009122800001  PEMA  NEW PROGRAM
022212* 022212    2011120900003  AJRA  ADD AHL
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   

       working-storage section.
       
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
       77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
       77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.

       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       01 bl-index              pic 9(8) comp.
       01 max-last-name         pic x(18).
       01 first-initial         pic x.
       01 name-in-range-flag    pic 9.
       01 max-entries           pic s9(8) comp value 100.
       
       01 lower-case    pic x(26) value
                  "abcdefghijklmnopqrstuvwxyz".
       01 upper-case    pic x(26) value
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES      PIC 9.
           05  FILLER                  PIC X.
           05  W-DAYS-TO-FOLLOW-UP     PIC 999.
           05  FILLER                  PIC X.
           05  W-DAYS-TO-RESEND-1      PIC 999.
           05  FILLER                  PIC X.
           05  W-FORM-TO-RESEND        PIC XXXX.
           05  FILLER                  PIC X.
           05  W-PROMPT-LETTER         PIC X.
           05  FILLER                  PIC X.
           05  W-ENCLOSURE-CD          PIC XXX.
           05  FILLER                  PIC X.
           05  W-AUTO-CLOSE-IND        PIC X.
           05  FILLER                  PIC X.
           05  W-LETTER-TO-BENE        PIC X.

       01  WS-ELMSTR-KEY.
           05  WS-ELMSTR-COMPANY-CD    PIC X.
           05  WS-ELMSTR-CARRIER       PIC X.
           05  WS-ELMSTR-CLAIM-NO      PIC X(7).
           05  WS-ELMSTR-CERT-NO       PIC X(11).
       01  WS-ELTRLR-KEY.
           05  WS-ELTRLR-COMPANY-CD    PIC X.
           05  WS-ELTRLR-CARRIER       PIC X.
           05  WS-ELTRLR-CLAIM-NO      PIC X(7).
           05  WS-ELTRLR-CERT-NO       PIC X(11).
           05  WS-ELTRLR-SEQ-NO        PIC S9(4) COMP VALUE +0.
       01  WS-ELLETR-KEY.
           05  WS-ELLETR-COMPANY-CD    PIC X.
           05  WS-ELLETR-LETTER-ID     PIC X(12).
           05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
       01  WS-ELCNTL-KEY.
           05  WS-ELCNTL-COMPANY-ID    PIC XXX.
           05  WS-ELCNTL-REC-TYPE      PIC X.
           05  WS-ELCNTL-GENL.
               10  FILLER              PIC XX  VALUE SPACES.
               10  WS-ELCNTL-BEN-CD    PIC XX.
           05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.

                                       COPY ELCDATE.
                                       COPY ELCMSTR.
                                       COPY ELCTRLR.
                                       COPY ELCTEXT.
                                       COPY ELCCNTL.

                                       COPY ELCNAPS.

       linkage section.
       
       01 dfhcommarea. 
                                       copy ELCLTRSPI.

       procedure division.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE

      *****************************************************
      * Using the information passed to this program,
      * it will add a correspondence eltrlr record  
      * update the next archive no in the elcntl file
      * and update the last maint and follow-up info
      * in the elmstr file.
      *****************************************************

PEMTST*    DISPLAY ' BL INPUT ' BL-INPUT

           evaluate bl-comp-id
              when 'DCC'
                 MOVE X'05'            TO WS-ELMSTR-COMPANY-CD
              when 'AHL'
                 MOVE X'06'            TO WS-ELMSTR-COMPANY-CD
              when 'VPP'
                 MOVE X'07'            TO WS-ELMSTR-COMPANY-CD
061421        when 'FNL'
061421           MOVE X'08'            TO WS-ELMSTR-COMPANY-CD
              when other
                 MOVE X'04'            TO WS-ELMSTR-COMPANY-CD
           end-evaluate

           MOVE BL-CARRIER             TO WS-ELMSTR-CARRIER
           MOVE BL-CLAIM-NO            TO WS-ELMSTR-CLAIM-NO
           MOVE BL-CERT-NO             TO WS-ELMSTR-CERT-NO

           EXEC CICS READ                                               
                DATASET    ('ELMSTR') 
                INTO       (CLAIM-MASTER)                    
                RIDFLD     (WS-ELMSTR-KEY)                                   
                RESP       (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
PEMTST*       DISPLAY ' NSALTRBL - RESP IS NORMAL '
              PERFORM 1600-ADD-CORR-TRLR
                                       THRU 1600-EXIT
              SET BL-OK TO TRUE
           ELSE
              DISPLAY ' BAD READ ELMSTR ' WS-RESPONSE
           END-IF

           exec cics return end-exec.	

       1500-GET-ARCH-NO.

PEMTST*    DISPLAY ' MADE 1500 '
           MOVE BL-ARCHIVE-NO          TO WS-ARCHIVE-NO
           GO TO 1500-EXIT
           MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
           MOVE '1'                    TO WS-ELCNTL-REC-TYPE
           MOVE SPACES                 TO WS-ELCNTL-GENL
           MOVE +0                     TO WS-ELCNTL-SEQ-NO

           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              UPDATE
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = BL-COMP-ID)
              AND (CF-RECORD-TYPE = '1')
              ADD +1                   TO CF-CO-ARCHIVE-COUNTER
              MOVE CF-CO-ARCHIVE-COUNTER TO WS-ARCHIVE-NO
              EXEC CICS REWRITE
                 FROM    (CONTROL-FILE)
                 DATASET ('ELCNTL')
              END-EXEC
           ELSE
              DISPLAY ' NOT NORMAL 1500 '
              MOVE +0                  TO WS-ARCHIVE-NO
           END-IF

          .
       1500-EXIT.
           EXIT.

       1600-ADD-CORR-TRLR.

PEMTST*    DISPLAY ' MADE 1600 '
           EXEC CICS READ
                DATASET    ('ELMSTR')
                INTO       (CLAIM-MASTER)
                RIDFLD     (WS-ELMSTR-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' 1600 READ UPD MSTR ' WS-RESPONSE
              GO TO 1600-EXIT
           END-IF

           SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT


           PERFORM 2000-BUILD-ELNAPS   THRU 2000-EXIT



           IF WS-FOLLOW-UP-DT > CL-NEXT-FOLLOWUP-DT
              MOVE WS-FOLLOW-UP-DT     TO CL-NEXT-FOLLOWUP-DT
           END-IF
                                                                        
           IF WS-RESEND-DT > CL-NEXT-FOLLOWUP-DT
              MOVE WS-RESEND-DT        TO CL-NEXT-FOLLOWUP-DT
           END-IF
           MOVE '2'                    TO CL-LAST-MAINT-TYPE

           MOVE 'AT'                   TO ACTIVITY-TRAILERS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO                
           MOVE  4                     TO AT-TRAILER-TYPE
           MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT                
                                          CL-LAST-MAINT-DT              
                                          AT-CORR-LAST-MAINT-DT         
                                          AT-LETTER-SENT-DT
           MOVE BL-PROC-ID             TO AT-RECORDED-BY                
                                          CL-LAST-MAINT-USER            
                                          AT-CORR-LAST-UPDATED-BY       
           MOVE BL-REGARDING           TO AT-REASON-TEXT
           MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS          
                                          CL-LAST-MAINT-HHMMSS.         
           MOVE WS-FOLLOW-UP-DT        TO AT-RECEIPT-FOLLOW-UP.         
           MOVE WS-RESEND-DT           TO AT-AUTO-RE-SEND-DT.           
           MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT         
                                          AT-LETTER-PURGED-DT

                                          AT-INITIAL-PRINT-DATE
           MOVE WS-ARCHIVE-NO          TO AT-LETTER-ARCHIVE-NO.
           MOVE '3'                    TO AT-LETTER-ORIGIN
                                                                        
           MOVE BL-LETTER-ID           TO AT-STD-LETTER-FORM

           IF BL-PRINT-NOW-SW = 'Y'
              MOVE SAVE-BIN-DATE       TO AT-INITIAL-PRINT-DATE
           END-IF

           MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE
           MOVE W-FORM-TO-RESEND       TO AT-RESEND-LETTER-FORM
           MOVE W-AUTO-CLOSE-IND       TO AT-AUTO-CLOSE-IND
           MOVE W-LETTER-TO-BENE       TO AT-LETTER-TO-BENE

           EXEC CICS WRITE                                              
                DATASET    ('ELTRLR')
                FROM       (ACTIVITY-TRAILERS)                          
                RIDFLD     (AT-CONTROL-PRIMARY)
                RESP       (WS-RESPONSE)
           END-EXEC
                                                                        
           IF NOT RESP-NORMAL
              DISPLAY ' 1600 WRITE TRLR ' WS-RESPONSE
              GO TO 1600-EXIT
           END-IF
       
           EXEC CICS REWRITE                                            
                DATASET    ('ELMSTR')
                FROM       (CLAIM-MASTER)                               
                RESP       (WS-RESPONSE)
           END-EXEC
       
           IF NOT RESP-NORMAL
              DISPLAY ' 1600 REWRITE MSTR ' WS-RESPONSE
           END-IF

           .
       1600-EXIT.
           EXIT.

       2000-BUILD-ELNAPS.

PEMTST*    DISPLAY ' MADE 2000 '
           PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT

           MOVE 'NA'                   TO NAPERSOFT-FILE
           MOVE CL-COMPANY-CD          TO NA-COMPANY-CD
           MOVE BL-CARRIER             TO NA-CARRIER
           MOVE BL-CLAIM-NO            TO NA-CLAIM-NO
           MOVE BL-CERT-NO             TO NA-CERT-NO
           MOVE WS-ARCHIVE-NO          TO NA-ARCHIVE-NO

           MOVE BL-LETTER-ID           TO NA-LETTER-ID
           MOVE BL-PROC-ID             TO NA-PROCESSOR-ID
           MOVE BL-NO-OF-COPIES        TO NA-NO-OF-COPIES
           MOVE BL-ENC-CD              TO NA-ENCLOSURE-CD

           IF BL-PRINT-NOW-SW = 'Y'
              MOVE SAVE-BIN-DATE       TO NA-INITIAL-PRINT-DT
           END-IF

           MOVE SAVE-BIN-DATE          TO NA-CREATION-DT

           MOVE LOW-VALUES             TO NA-FOLLOW-UP-DT
                                          NA-RESEND-DT
                                          NA-RESEND-PRINT-DT
                                          NA-1ST-LTR-PRINT-DT
                                          NA-NEXT-DUE-DT   
                                          NA-AUTOPYDT

           MOVE CL-TRAILER-SEQ-CNT     TO NA-CORR-TRLR-SEQ

           IF BL-FOLLOW-UP-DT NOT = SPACES AND LOW-VALUES
              STRING BL-FOLLOW-UP-DT (7:2) BL-FOLLOW-UP-DT (1:2)
                 BL-FOLLOW-UP-DT (4:2) DELIMITED BY SIZE
                   INTO DC-GREG-DATE-1-YMD-R
              END-STRING
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO NA-FOLLOW-UP-DT
                                          WS-FOLLOW-UP-DT
              ELSE
                 MOVE LOW-VALUES       TO NA-FOLLOW-UP-DT
                                          WS-FOLLOW-UP-DT
              END-IF
           END-IF

           IF BL-RESEND-DT NOT = SPACES AND LOW-VALUES
              STRING BL-RESEND-DT (7:2) BL-RESEND-DT (1:2)
                 BL-RESEND-DT (4:2) DELIMITED BY SIZE
                   INTO DC-GREG-DATE-1-YMD-R
              END-STRING
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO NA-RESEND-DT
                                          WS-RESEND-DT
              ELSE
                 MOVE LOW-VALUES       TO NA-RESEND-DT
                                          WS-RESEND-DT
              END-IF
           END-IF
           MOVE 'Y'                    TO NA-CREATED-IN-NAPERSOFT

           MOVE CL-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
           MOVE BL-LETTER-ID           TO WS-ELLETR-LETTER-ID
           MOVE +0                     TO WS-ELLETR-SEQ-NO

           EXEC CICS READ                                               
                DATASET    ('ELLETR') 
                INTO       (TEXT-FILES)
                RIDFLD     (WS-ELLETR-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

PEMTST*    DISPLAY ' ELLETR READ ' WS-ELLETR-KEY ' ' WS-RESPONSE
           IF RESP-NORMAL
              IF (LETTER-FILE-TEXT)
                 AND (TX-LETTER-NO = BL-LETTER-ID)
                 AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
                 MOVE TX-TEXT-LINE     TO W-Z-CONTROL-DATA
                 
                 IF W-FORM-TO-RESEND > SPACES
                    MOVE W-FORM-TO-RESEND
                                       TO NA-RESEND-LETTER-ID
                 ELSE
                    MOVE SPACES        TO NA-RESEND-LETTER-ID
                 END-IF
              END-IF
           END-IF



           EXEC CICS WRITE
              DATASET   ('ELNAPS')
              FROM      (NAPERSOFT-FILE)
              RIDFLD    (NA-CONTROL-PRIMARY)
              RESP      (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL
              CONTINUE
           ELSE
              DISPLAY ' BAD WRITE ELNAPS ' WS-RESPONSE
                    ' ' NA-CONTROL-PRIMARY (2:19)
           END-IF

          .
       2000-EXIT.
           EXIT.

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.                                                       
                                                                        

