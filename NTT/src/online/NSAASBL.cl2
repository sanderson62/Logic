       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSAASBL.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM NSAASLTR.cl2

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 071111    2011022800001  PEMA  NEW PROGRAM
102212* 102212    2012101700002  AJRA  EXPAND PASSED AREA
012413* 012413    2013012100001  AJRA  ADD AUTO BILLING NOTE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
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
012413 77  NOTE-SUB                    PIC S9(5) COMP-3 VALUE +0.
012413 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
012413     88  NO-CERT-RW                 VALUE 'N'.
012413     88  CERT-RW                    VALUE 'Y'.

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

      *** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
                                COPY ELCZREC.


       01  WS-ERARCH-KEY.
           05  WS-ERARCH-COMPANY-CD    PIC X.
           05  WS-ERARCH-ARCH-NO       PIC S9(8) BINARY.

041320 01  ws-erpndb-key.
041320     05  ws-erpndb-company-cd    pic x.
041320     05  ws-erpndb-batch-no      pic x(6).
041320     05  ws-erpndb-batch-seq-no  pic s9(4) COMP.
041320     05  ws-erpndb-chg-seq-no    pic s9(4) COMP.

       01  WS-ERENDT2-KEY.
           05  WS-ERENDT-COMPANY-CD    PIC X.
           05  WS-ERENDT-ARCH-NO       PIC 9(8) BINARY.

       01  WS-NSASEXTR-KEY.
           05  WS-EXTR-COMPANY-CD    PIC X.
           05  WS-EXTR-ARCH-NO       PIC 9(8) BINARY.
           05  WS-EXTR-SEQ-NO        PIC 9(4) BINARY.

       01  WS-ELLETR-KEY.
           05  WS-ELLETR-COMPANY-CD    PIC X.
           05  WS-ELLETR-LETTER-ID     PIC X(12).
           05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
012413
012413 01  BILL-NOTE-AREA.
041320     12  ws-from-issue-or-cancel pic x.
012413     12  WS-ELEOBC-KEY.
012413         16  WS-EOBC-COMPANY-CD  PIC X.
012413         16  WS-EOBC-REC-TYPE    PIC X.
012413         16  WS-EOBC-CODE        PIC X(4).
012413         16  FILLER              PIC X(9).
012413     12  WS-ERNOTE-KEY.
012413         16  WS-NOTE-COMPANY-CD  PIC X.
012413         16  WS-NOTE-CARRIER     PIC X.
012413         16  WS-NOTE-GROUPING    PIC X(6).
012413         16  WS-NOTE-STATE       PIC XX.
012413         16  WS-NOTE-ACCOUNT     PIC X(10).
012413         16  WS-NOTE-CERT-EFF-DT PIC XX.
012413         16  WS-NOTE-CERT-PRIME  PIC X(10).
012413         16  WS-NOTE-CERT-SFX    PIC X.
041320         16  ws-note-record-type pic x.
012413     12  WS-BILLING-NOTE.
012413         16  WS-BN-NOTE          PIC X(25).
012413         16  WS-BN-LTRID         PIC X(4).
012413         16  FILLER              PIC X(3).
012413         16  WS-BN-DATE          PIC X(8).
012413         16  FILLER              PIC X(3).
012413         16  WS-BN-USERID        PIC X(4).
012413         16  FILLER              PIC X(30).
012413     12  WS-LEN                  PIC S9(5) COMP-3 VALUE +0.
012413     12  WS-ELCERT-KEY.
012413         16  WS-CERT-COMPANY-CD  PIC X.
012413         16  WS-CERT-CARRIER     PIC X.
012413         16  WS-CERT-GROUPING    PIC X(6).
012413         16  WS-CERT-STATE       PIC XX.
012413         16  WS-CERT-ACCOUNT     PIC X(10).
012413         16  WS-CERT-CERT-EFF-DT PIC XX.
012413         16  WS-CERT-CERT-PRIME  PIC X(10).
012413         16  WS-CERT-CERT-SFX    PIC X.
012413     12  WS-SAVE-KEY.
012413         16  WS-SV-COMPANY-CD    PIC X.
012413         16  WS-SV-CARRIER       PIC X.
012413         16  WS-SV-GROUPING      PIC X(6).
012413         16  WS-SV-STATE         PIC XX.
012413         16  WS-SV-ACCOUNT       PIC X(10).
012413         16  WS-SV-CERT-EFF-DT   PIC XX.
012413         16  WS-SV-CERT-PRIME    PIC X(10).
012413         16  WS-SV-CERT-SFX      PIC X.
012413     12  WS-SAVE-DATA.
012413         16  WS-SV-LETTER-ID     PIC X(4).
012413         16  WS-SV-PROC-ID       PIC X(4).


       01 srch-commarea.
                                       copy ELCADLTRSPI.


                                       COPY ELCDATE.
041320                                 copy ERCPNDB.
                                       COPY ERCARCH.
                                       COPY ERCENDT.
                                       COPY NSCASEXTR.
012413                                 COPY ERCNOTE.
012413                                 COPY ELCEOBC.
012413                                 COPY ELCCERT.

       linkage section.
       
102212 01 dfhcommarea                  pic x(218).

       procedure division.

      *    display ' entering nsaasbl '

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
           MOVE DFHCOMMAREA            TO BL-INPUT
      *****************************************************
      * Using the information passed to this program,
      * it will update the erarch record with a "Q"
      * in the status field
      *****************************************************

020816     evaluate bl-comp-id
020816        when 'DCC'
020816           move X'05'            to ws-erarch-company-cd
020816        when 'AHL'
020816           move X'06'            to ws-erarch-company-cd
020816        when 'VPP'
020816           move X'07'            to ws-erarch-company-cd
061421        when 'FNL'
061421           move X'08'            to ws-erarch-company-cd
020816        when other
020816           move X'04'            to ws-erarch-company-cd
020816     end-evaluate

           MOVE WS-ERARCH-COMPANY-CD   TO WS-ERENDT-COMPANY-CD
                                          WS-EXTR-COMPANY-CD
041320                                    ws-erpndb-company-cd
           MOVE BL-ARCHIVE-NO          TO WS-ERARCH-ARCH-NO
                                          WS-ERENDT-ARCH-NO
                                          WS-EXTR-ARCH-NO

           SET BL-FAIL TO TRUE

           EXEC CICS READ
                DATASET    ('ERARCH')
                INTO       (LETTER-ARCHIVE)
                RIDFLD     (WS-ERARCH-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET BL-OK TO TRUE
              IF (BL-PRINT-NOW-SW = 'P')
                 or (bl-process-type = 'cancel')
                 PERFORM 0050-DELETE-RECORDS
                                       THRU 0050-EXIT
              ELSE
                 IF BL-PROCESS-TYPE = 'process'
012413              MOVE LA-COMPANY-CD-A2 TO WS-SV-COMPANY-CD
012413              MOVE LA-CARRIER-A2    TO WS-SV-CARRIER
012413              MOVE LA-GROUPING-A2   TO WS-SV-GROUPING
012413              MOVE LA-STATE-A2      TO WS-SV-STATE
012413              MOVE LA-ACCOUNT-A2    TO WS-SV-ACCOUNT
012413              MOVE LA-EFFECT-DATE-A2 TO WS-SV-CERT-EFF-DT
012413              MOVE LA-CERT-PRIME-A2 TO WS-SV-CERT-PRIME
012413              MOVE LA-CERT-SUFFIX-A2 TO WS-SV-CERT-SFX
012413              MOVE LA-FORM-A3       TO WS-SV-LETTER-ID
012413              MOVE LA-PROCESSOR-CD  TO WS-SV-PROC-ID
                    MOVE 'Q'           TO LA-ARCHIVE-STATUS
                    IF BL-PRINT-NOW-SW = 'Y'
                       MOVE SAVE-BIN-DATE TO LA-INITIAL-PRINT-DATE
072312                 IF (LA-RESEND-DATE EQUAL LOW-VALUES OR SPACES)
072312                  AND (LA-FOLLOW-UP-DATE EQUAL 
072312                                 LOW-VALUES OR SPACES)
072312                    MOVE 'C'             TO LA-STATUS
072312                 END-IF
                    END-IF
                 ELSE
                    MOVE 'T'           TO LA-ARCHIVE-STATUS
                 END-IF
              END-IF
              EXEC CICS REWRITE
                 FROM      (LETTER-ARCHIVE)
                 DATASET   ('ERARCH')
                 RESP       (WS-RESPONSE)
              END-EXEC

012413        IF BL-PROCESS-TYPE = 'process'
012413           PERFORM 0440-ADD-BILLING-NOTE THRU 0440-EXIT
012413        END-IF
           ELSE
              MOVE ' BAD READ UPDATE ON ERARCH '
                                       TO BL-MESSAGE
              display ' bad read on erarch ' ws-response
                ' ' bl-archive-no
           END-IF
           MOVE BL-INPUT               TO DFHCOMMAREA

           exec cics return end-exec.	

       0050-DELETE-RECORDS.

           IF BL-FUNC = 'ClmResc' or 'ClmRefo'
              go to 0050-exit
           end-if

           EXEC CICS DELETE
              DATASET    ('ERARCH')
              RESP       (WS-RESPONSE)
           END-EXEC
           IF NOT RESP-NORMAL
              DISPLAY ' BAD DELETE ERARCH ' WS-RESPONSE
              GO TO 0050-EXIT
           END-IF

           EXEC CICS READ
                DATASET    ('ERENDT2')
                INTO       (ENDORSEMENT-RECORD)
                RIDFLD     (WS-ERENDT2-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ ON ERENDT2 ' WS-RESPONSE
           ELSE
              EXEC CICS DELETE
                 DATASET    ('ERENDT2')
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF NOT RESP-NORMAL
                 DISPLAY ' BAD DELETE ON ERENDT2 ' WS-RESPONSE
              END-IF
           END-IF

           MOVE 0                      TO WS-EXTR-SEQ-NO
           EXEC CICS READ
                DATASET    ('NSASEXTR')
                INTO       (NSAS-EXTRACT-RECORD)
                RIDFLD     (WS-NSASEXTR-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ ON 0 NSASEXTR ' WS-RESPONSE
              GO TO 0050-EXIT
           ELSE
              EXEC CICS DELETE
                 DATASET    ('NSASEXTR')
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF NOT RESP-NORMAL
                 DISPLAY ' BAD DELETE ON 0 NSASEXTR ' WS-RESPONSE
                 GO TO 0050-EXIT
              END-IF
           END-IF

           MOVE 1                      TO WS-EXTR-SEQ-NO
           EXEC CICS READ
                DATASET    ('NSASEXTR')
                INTO       (NSAS-EXTRACT-RECORD)
                RIDFLD     (WS-NSASEXTR-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ ON 1 NSASEXTR ' WS-RESPONSE
           ELSE
              EXEC CICS DELETE
                 DATASET    ('NSASEXTR')
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF NOT RESP-NORMAL
                 DISPLAY ' BAD DELETE ON 1 NSASEXTR ' WS-RESPONSE
              END-IF
           END-IF

           MOVE 2                      TO WS-EXTR-SEQ-NO
           EXEC CICS READ
                DATASET    ('NSASEXTR')
                INTO       (NSAS-EXTRACT-RECORD)
                RIDFLD     (WS-NSASEXTR-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ ON 2 NSASEXTR ' WS-RESPONSE
           ELSE
              EXEC CICS DELETE
                 DATASET    ('NSASEXTR')
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF NOT RESP-NORMAL
                 DISPLAY ' BAD DELETE ON 2 NSASEXTR ' WS-RESPONSE
              END-IF
           END-IF

           .
       0050-EXIT.
           EXIT.
012413
012413
012413 0440-ADD-BILLING-NOTE.
012413
012413     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
012413     MOVE WS-ERARCH-COMPANY-CD   TO WS-EOBC-COMPANY-CD
012413     MOVE '5'                    TO WS-EOBC-REC-TYPE
012413
012413     EXEC CICS STARTBR                                            
012413         DATASET   ('ELEOBC')
012413         RIDFLD    (WS-ELEOBC-KEY)
012413         GTEQ
012413         RESP      (WS-RESPONSE)
012413     END-EXEC
012413
012413     IF NOT RESP-NORMAL
012413        GO TO 0440-EXIT
012413     END-IF
012413      .
012413 0440-READNEXT-ELEOBC.
012413
012413     EXEC CICS READNEXT
012413        INTO    (EOB-CODES)
012413        DATASET ('ELEOBC')
012413        RIDFLD  (WS-ELEOBC-KEY)
012413        RESP    (WS-RESPONSE)
012413     END-EXEC
012413
012413     IF RESP-NORMAL
012413         IF EO-RECORD-TYPE NOT = '5'
012413             GO TO 0440-EXIT
012413         END-IF
012413     ELSE
012413         GO TO 0440-EXIT
012413     END-IF
012413     
012413     IF EO-RECORD-TYPE = '5' AND
012413        EO-EOB-CODE = WS-SV-LETTER-ID
012413           CONTINUE
012413     ELSE
012413         GO TO 0440-READNEXT-ELEOBC
012413     END-IF
012413
012413     MOVE SPACES         TO WS-BILLING-NOTE
012413     MOVE EO-DESCRIPTION TO WS-BN-NOTE
012413     MOVE WS-SV-LETTER-ID TO WS-BN-LTRID
012413     MOVE SAVE-DATE      TO WS-BN-DATE
012413     MOVE WS-SV-PROC-ID  TO WS-BN-USERID
012413     MOVE +25            TO WS-LEN
012413
012413     PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
012413     .
012413 0440-EXIT.
012413     EXIT.
012413
012413 0441-UPDATE-BILLING-NOTE.
012413
041320     move bl-batch-no            to ws-erpndb-batch-no
041320     move bl-batch-seq           to ws-erpndb-batch-seq-no
041320     move +0                     to ws-erpndb-chg-seq-no
041320     exec cics read
041320        dataset   ('ERPNDB')
041320        ridfld    (ws-erpndb-key)
041320        into      (pending-business)
041320        resp      (ws-response)
041320     end-exec
041320     if resp-normal and pb-cancellation
041320        move '2'                 to ws-from-issue-or-cancel
041320*       display ' must be from cancel nsaasbl '
041320     else
041320        move '1'                 to ws-from-issue-or-cancel
041320     end-if
041320
041320     MOVE WS-SAVE-KEY            TO WS-ERNOTE-KEY
041320     move ws-from-issue-or-cancel to ws-note-record-type
041320
041320*    display ' rid fle b4 ' ws-ernote-key(2:33)
012413
012413     EXEC CICS READ
012413        DATASET    ('ERNOTE')
012413        RIDFLD     (WS-ERNOTE-KEY)
012413        INTO       (CERTIFICATE-NOTE)
012413        RESP       (WS-RESPONSE)
012413        UPDATE
012413     END-EXEC
012413
012413     IF RESP-NORMAL
012413       IF CN-BILLING-START-LINE-NO NOT NUMERIC
012413          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
012413       END-IF
012413       IF CN-BILLING-END-LINE-NO NOT NUMERIC
012413          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
012413       END-IF
012413       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
012413           (NOTE-SUB > +10) OR
012413           (CN-LINE (NOTE-SUB) (1:WS-LEN) = 
012413                             WS-BILLING-NOTE (1:WS-LEN))
012413       END-PERFORM
012413       IF CN-LINE (NOTE-SUB) (1:WS-LEN) = 
012413                              WS-BILLING-NOTE (1:WS-LEN)
012413         EXEC CICS UNLOCK
012413            DATASET    ('ERNOTE')
012413         END-EXEC
012413       ELSE
012413         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
012413           (NOTE-SUB > +10) OR
012413           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES) 
012413         END-PERFORM
012413         IF (NOTE-SUB < +11)
012413           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
012413              NOTE-SUB <= CN-BILLING-END-LINE-NO
012413                MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
012413           ELSE 
012413             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
012413              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
012413                MOVE WS-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
012413                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
012413             ELSE
012413               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
012413                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
012413                     MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
012413                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
012413               ELSE
012413                 IF (CN-BILLING-END-LINE-NO = ZEROS)
012413                   MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
012413                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
012413                                       CN-BILLING-START-LINE-NO
012413                 ELSE
012413                    PERFORM 0442-SQUEEZE-IT-IN THRU 0442-EXIT
012413                 END-IF
012413               END-IF                          
012413             END-IF
012413           END-IF
012413           MOVE WS-SV-PROC-ID       TO CN-LAST-MAINT-USER
012413           MOVE SAVE-BIN-DATE       TO CN-LAST-MAINT-DT
012413           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
012413           EXEC CICS REWRITE
012413              DATASET    ('ERNOTE')
012413              FROM       (CERTIFICATE-NOTE)
012413              RESP       (WS-RESPONSE)
012413           END-EXEC
012413           PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
012413         END-IF
012413       END-IF
012413     ELSE
012413        MOVE SPACES              TO CERTIFICATE-NOTE
012413        MOVE 'CN'                TO CN-RECORD-ID
012413        MOVE WS-SAVE-KEY         TO CN-CONTROL-PRIMARY
012413                                    WS-ERNOTE-KEY

041320        move ws-from-issue-or-cancel
041320                                 to cn-record-type
041320                                    ws-note-record-type

012413        MOVE 01                  TO CN-BILLING-START-LINE-NO
012413                                    CN-BILLING-END-LINE-NO
012413        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
012413        MOVE WS-SV-PROC-ID       TO CN-LAST-MAINT-USER
012413        MOVE SAVE-BIN-DATE       TO CN-LAST-MAINT-DT
012413        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
012413        EXEC CICS WRITE
012413           DATASET    ('ERNOTE')
012413           FROM       (CERTIFICATE-NOTE)
041320           RIDFLD     (cn-control-primary)
012413           RESP       (WS-RESPONSE)
012413        END-EXEC
012413
012413        PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
012413     END-IF              
012413
012413     .
012413 0441-EXIT.
012413     EXIT.
012413
012413
012413 0442-SQUEEZE-IT-IN.
012413
012413     IF NOTE-SUB < CN-BILLING-START-LINE-NO
012413        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
012413           NOTE-SUB = +10
012413           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
012413           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
012413             MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
012413             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
012413             MOVE +9 TO NOTE-SUB
012413           END-IF
012413        END-PERFORM
012413     ELSE
012413        IF NOTE-SUB > CN-BILLING-END-LINE-NO
012413           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1 
012413             UNTIL NOTE-SUB = +1
012413             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
012413             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
012413                MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
012413                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
012413                MOVE +2          TO NOTE-SUB
012413             END-IF
012413           END-PERFORM
012413        END-IF  
012413     END-IF
012413
012413     .
012413 0442-EXIT.
012413     EXIT.
012413
012413 0445-CERTIFICATE-UPDATE.
012413
012413     MOVE WS-SAVE-KEY     TO WS-ELCERT-KEY
012413     EXEC CICS READ
012413         DATASET  ('ELCERT')
012413         RIDFLD   (WS-ELCERT-KEY)
012413         INTO     (CERTIFICATE-MASTER)
012413         RESP     (WS-RESPONSE)
012413         UPDATE
012413     END-EXEC
012413
012413     IF RESP-NORMAL
012413        EVALUATE CM-NOTE-SW
012413           WHEN '2'
012413           WHEN '3'
012413           WHEN '6'
012413           WHEN '7'
012413              SET NO-CERT-RW     TO TRUE
012413           WHEN ' '
012413              MOVE '2'           TO CM-NOTE-SW
012413           WHEN '1'
012413              MOVE '3'           TO CM-NOTE-SW
012413           WHEN '4'
012413              MOVE '6'           TO CM-NOTE-SW
012413           WHEN '5'
012413              MOVE '7'           TO CM-NOTE-SW
012413        END-EVALUATE
012413     END-IF
012413
012413     IF NOT NO-CERT-RW
012413        EXEC CICS REWRITE
012413           FROM     (CERTIFICATE-MASTER)
012413           DATASET  ('ELCERT')
012413           RESP     (WS-RESPONSE)
012413        END-EXEC
012413     ELSE
012413        EXEC CICS UNLOCK
012413           DATASET  ('ELCERT')
012413        END-EXEC
012413     END-IF
012413
012413     .
012413 0445-EXIT.
012413     EXIT.
012413

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.                                                       
                                                                        

