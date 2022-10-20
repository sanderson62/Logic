       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   PYRPTBL.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM LORPT

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111810                   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   

       working-storage section.
       
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
       77  WS-TODAY-MINUS-7            PIC XX      VALUE LOW-VALUES.
       77  R1                          PIC S999 COMP-3 VALUE +0.
       77  E1                          PIC S999 COMP-3 VALUE +0.
       77  A1                          PIC S9(5) COMP-3 VALUE +0.
       77  M1                          PIC S999 COMP-3 VALUE +0.
       77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
       77  WS-LAST-ACT-TYPE            PIC X   VALUE ' '.
       77  WS-EOF-SW                   PIC X  VALUE ' '.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-LOAN-OFF-SW              PIC X   VALUE ' '.
           88  LOAN-OFF-GOOD               VALUE 'Y'.
       77  WS-ACCT-SW                  PIC X VALUE ' '.
           88  ACCT-FOUND                  VALUE 'Y'.
           88  ACCT-NOT-FOUND              VALUE 'N'.
       77  WS-PNDB-SW                  PIC X VALUE ' '.
           88  PNDB-FOUND                  VALUE 'Y'.
           88  PNDB-NOT-FOUND              VALUE 'N'.
       77  WS-BROWSE-SW                PIC X  VALUE ' '.
           88  BROWSE-STARTED             VALUE 'Y'.
       77  WS-START-TRYS               PIC S999 COMP-3 VALUE +0.
       77  WS-READ-TRYS                PIC S999 COMP-3 VALUE +0.
       77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.
       77  WS-PREV-PYAJ-KEY            PIC X(28)  VALUE LOW-VALUES.
       77  WS-SB-SW                    PIC X  VALUE ' '.
           88  PNDB-STARTBR              VALUE 'Y'.

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

       01  WS-SAVE-PYAJ-DATA.
           05  WS-SAVE-CARRIER         PIC X.
           05  WS-SAVE-GROUPING        PIC X(6).
           05  WS-SAVE-STATE           PIC XX.
           05  WS-SAVE-RESP-NO         PIC X(10).
           05  WS-SAVE-ACCOUNT         PIC X(10).
           05  WS-SAVE-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-SAVE-TYPE            PIC X.
           05  WS-SAVE-GL-ACCT         PIC X(10).
           05  WS-SAVE-COMMENT         PIC X(13).
           05  WS-SAVE-MAINT-DT        PIC XX   VALUE LOW-VALUES.

       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.



       01  WS-ERPYAJ-KEY.
           05  WS-ERPYAJ-COMPANY-CD    PIC X.
           05  WS-ERPYAJ-CARRIER       PIC X.
           05  WS-ERPYAJ-GROUP         PIC X(6).
           05  WS-ERPYAJ-RESP          PIC X(10).
           05  WS-ERPYAJ-ACCT          PIC X(10).
           05  WS-ERPYAJ-SEQ           PIC S9(8) COMP.
           05  WS-ERPYAJ-TPYE          PIC X.

       01  WS-ERPNDB2-KEY.
           05  WS-ERPNDB2-COMPANY-CD    PIC X.
           05  WS-ERPNDB2-CARRIER       PIC X.
           05  WS-ERPNDB2-GROUP         PIC X(6).
           05  WS-ERPNDB2-STATE         PIC XX.
           05  WS-ERPNDB2-ACCOUNT       PIC X(10).
           05  WS-ERPNDB2-EFF-DT        PIC XX.
           05  WS-ERPNDB2-CERT-NO       PIC X(11).
           05  WS-ERPNDB2-CHG-SEQ       PIC S9(4) COMP.
           05  WS-ERPNDB2-REC-TYPE      PIC X.

       01  WS-ERACCT-KEY.
           05  WS-ERACCT-COMPANY-CD    PIC X.
           05  WS-ERACCT-CARRIER       PIC X.
           05  WS-ERACCT-GROUP         PIC X(6).
           05  WS-ERACCT-STATE         PIC XX.
           05  WS-ERACCT-ACCOUNT       PIC X(10).
           05  WS-ERACCT-EXP-DT        PIC XX.
           05  FILLER                  PIC XXXX.

       01  WS-ERCOMP-KEY.
           05  WS-ERCOMP-COMPANY-CD    PIC X.
           05  WS-ERCOMP-CARRIER       PIC X.
           05  WS-ERCOMP-GROUP         PIC X(6).
           05  WS-ERCOMP-FIN-RESP      PIC X(10).
           05  WS-ERCOMP-ACCOUNT       PIC X(10).
           05  WS-ERCOMP-TYPE          PIC X.

                                       COPY ELCDATE.
                                       COPY ERCPYAJ.
                                       COPY ERCPNDB.
                                       COPY ERCACCT.
                                       COPY ERCCOMP.

       01  FILLER                      PIC X(500)  VALUE SPACES.

       linkage section.
       
       01 dfhcommarea. 
        02  BL-COMMAREA.
           03  BL-OUTPUT OCCURS 200.
               05  BL-CARRIER          PIC X.
               05  BL-STATE            PIC XX.
               05  BL-FIN-RESP         PIC X(10).
               05  BL-ACCOUNT          PIC X(10).
               05  BL-ACCT-NAME        PIC X(33).
               05  BL-NET-AMT          PIC 9(7)V99.
               05  BL-TYPE             PIC X.
               05  BL-GL-ACCT          PIC X(10).
               05  BL-RETURN-CODE REDEFINES BL-GL-ACCT
                                       PIC 9(10).
               05  BL-COMMENT          PIC X(13).
               05  BL-MAINT-DT         PIC X(10).
           03  BL-COUNT                PIC 999.
           03  BL-STATUS.
               88  BL-OK		          VALUE "P".
               88  BL-FAIL		        VALUE "F".
               05  BL-MESSAGE          PIC X(50).


       procedure division.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-GREG-DATE-A-EDIT    TO WS-SAVE-EDIT-A-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE -7                     TO DC-ELAPSED-DAYS
           MOVE +0                     TO DC-ELAPSED-MONTHS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO WS-TODAY-MINUS-7
           ELSE
              DISPLAY ' ERROR CONVERTING DATE '
           END-IF

           PERFORM 0000-GET-STARTED    THRU 0000-EXIT
           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              (END-OF-INPUT)
              OR (R1 > 195)

           MOVE ZEROS TO WS-RESPONSE
           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT

           MOVE R1                     TO BL-COUNT
           IF R1 = ZEROS
              MOVE 1                   TO BL-COUNT
              MOVE ' ALL IS WELL IN PAYMENT AND ADJUSTMENT LAND '
                                       TO BL-MESSAGE
           END-IF

           exec cics return end-exec.	

           .
       0000-GET-STARTED.

           MOVE X'04'                  TO WS-ERPYAJ-KEY

           EXEC CICS STARTBR                                            
               DATASET   ('ERPYAJ')
               RIDFLD    (WS-ERPYAJ-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE 1                   TO R1
              MOVE ALL '?'             TO BL-OUTPUT (R1)
              MOVE ' BAD STARTBR ON ERPYAJ ' TO BL-MESSAGE
              MOVE WS-RESPONSE         TO BL-RETURN-CODE (R1)
              DISPLAY BL-MESSAGE ' ' WS-RESPONSE
              SET END-OF-INPUT         TO TRUE
              GO TO 0000-EXIT
           END-IF

           PERFORM 0110-READNEXT-ERPYAJ THRU 0110-EXIT

           IF RESP-NORMAL
              PERFORM 0120-MOVE-SAVE   THRU 0120-EXIT
              MOVE PY-CONTROL-PRIMARY (1:28)
                                       TO WS-PREV-PYAJ-KEY
              PERFORM 0110-READNEXT-ERPYAJ
                                       THRU 0110-EXIT
           ELSE
              IF RESP-NOTFND
                 OR RESP-ENDFILE
                 MOVE ' NOTHING ON FILE ' TO BL-MESSAGE
                 MOVE WS-RESPONSE TO BL-RETURN-CODE (R1)
                 DISPLAY BL-MESSAGE ' ' WS-RESPONSE
                 SET END-OF-INPUT TO TRUE
                 GO TO 0000-EXIT
              ELSE
                 MOVE 'BAD READNEXT ON ERPYAJ ' TO BL-MESSAGE
                 MOVE WS-RESPONSE TO BL-RETURN-CODE (R1)
                 DISPLAY BL-MESSAGE ' ' WS-RESPONSE
                 SET END-OF-INPUT TO TRUE
                 GO TO 0000-EXIT
              END-IF
           END-IF

           .
       0000-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF (PY-ACCOUNT NOT = LOW-VALUES)
              AND (PY-RECORD-TYPE = 'R' OR 'C')
              AND (PY-COMPANY-CD = X'04')
              PERFORM 0100-PROCESS-INPUT THRU 0100-EXIT
           END-IF

           PERFORM 0110-READNEXT-ERPYAJ THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-INPUT.

           IF RESP-NORMAL
              IF WS-PREV-PYAJ-KEY = PY-CONTROL-PRIMARY (1:28)
                 PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
                 GO TO 0100-EXIT
              END-IF
           ELSE
              GO TO 0100-EXIT
           END-IF

           IF (WS-SAVE-AMT NOT = ZEROS)
              AND (WS-SAVE-MAINT-DT < WS-TODAY-MINUS-7)
              MOVE WS-PREV-PYAJ-KEY    TO WS-ERCOMP-KEY
              MOVE 'A'              TO WS-ERCOMP-TYPE

              EXEC CICS READ
                 INTO    (COMPENSATION-MASTER)
                 DATASET ('ERCOMP')
                 RIDFLD  (WS-ERCOMP-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 IF ZEROS = CO-BAL-FWD AND CO-CUR-COM AND CO-CUR-CHG
                    AND CO-CUR-PMT AND CO-END-BAL
                    CONTINUE
                 ELSE
                    MOVE +0            TO WS-SAVE-AMT
                    MOVE LOW-VALUES    TO WS-SAVE-MAINT-DT
                    PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
                    GO TO 0100-EXIT
                 END-IF
              ELSE
                 MOVE +0            TO WS-SAVE-AMT
                 MOVE LOW-VALUES    TO WS-SAVE-MAINT-DT
                 PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
                 GO TO 0100-EXIT
              END-IF
           ELSE
              MOVE +0            TO WS-SAVE-AMT
              MOVE LOW-VALUES    TO WS-SAVE-MAINT-DT
              PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
              GO TO 0100-EXIT
           END-IF
                    
           PERFORM 0130-FIND-ERACCT    THRU 0130-EXIT

           IF ACCT-FOUND
              PERFORM 0140-PROCESS-ERPNDB2
                                       THRU 0140-EXIT
           ELSE
              PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
              GO TO 0100-EXIT
           END-IF

           IF PNDB-FOUND
              MOVE +0            TO WS-SAVE-AMT
              MOVE LOW-VALUES    TO WS-SAVE-MAINT-DT
              PERFORM 0120-MOVE-SAVE THRU 0120-EXIT
              GO TO 0100-EXIT
           END-IF

           ADD +1 TO R1
           MOVE WS-SAVE-CARRIER        TO BL-CARRIER (R1)
           MOVE WS-SAVE-STATE          TO BL-STATE (R1)
           MOVE WS-SAVE-RESP-NO        TO BL-FIN-RESP (R1)
           MOVE WS-SAVE-ACCOUNT        TO BL-ACCOUNT (R1)
           MOVE AM-NAME                TO BL-ACCT-NAME (R1)
           MOVE WS-SAVE-AMT            TO BL-NET-AMT   (R1)
           MOVE WS-SAVE-TYPE           TO BL-TYPE      (R1)
           MOVE WS-SAVE-GL-ACCT        TO BL-GL-ACCT   (R1)
           MOVE WS-SAVE-COMMENT        TO BL-COMMENT   (R1)
           MOVE WS-SAVE-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE             
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO BL-MAINT-DT (R1)
           ELSE
              MOVE '01/01/1900'        TO BL-MAINT-DT (R1)
           END-IF

           MOVE +0            TO WS-SAVE-AMT
           MOVE LOW-VALUES    TO WS-SAVE-MAINT-DT
           PERFORM 0120-MOVE-SAVE THRU 0120-EXIT

           PERFORM 0500-SCRUB-DATA     THRU 0500-EXIT

           .
       0100-EXIT.
           EXIT.

       0110-READNEXT-ERPYAJ.

           EXEC CICS READNEXT
              INTO    (PENDING-PAY-ADJ)
              DATASET ('ERPYAJ')
              RIDFLD  (WS-ERPYAJ-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              SET END-OF-INPUT         TO TRUE
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-MOVE-SAVE.

           MOVE PY-CARRIER             TO WS-SAVE-CARRIER
           MOVE PY-GROUPING            TO WS-SAVE-GROUPING
           MOVE PY-FIN-RESP            TO WS-SAVE-RESP-NO
           MOVE PY-ACCOUNT             TO WS-SAVE-ACCOUNT
           IF PY-RECORD-TYPE = 'R'
              ADD PY-ENTRY-AMT   TO WS-SAVE-AMT
           ELSE
              SUBTRACT PY-ENTRY-AMT FROM WS-SAVE-AMT
           END-IF
           MOVE PY-RECORD-TYPE         TO WS-SAVE-TYPE
           MOVE PY-GL-ACCOUNT TO WS-SAVE-GL-ACCT
           MOVE PY-GL-COMMENT TO WS-SAVE-COMMENT
           IF PY-LAST-MAINT-DT > WS-SAVE-MAINT-DT
              MOVE PY-LAST-MAINT-DT TO WS-SAVE-MAINT-DT
           END-IF
           MOVE PY-CONTROL-PRIMARY (1:28)
                                       TO WS-PREV-PYAJ-KEY

           .
       0120-EXIT.
           EXIT.

       0130-FIND-ERACCT.

           MOVE ' '                    TO WS-ACCT-SW

           MOVE +0                     TO WS-READ-TRYS
                                          WS-START-TRYS
           MOVE CO-COMPANY-CD          TO WS-ERACCT-KEY
           MOVE CO-CARRIER             TO WS-ERACCT-CARRIER
           MOVE CO-GROUPING            TO WS-ERACCT-GROUP
           MOVE CO-ADDR-STATE          TO WS-ERACCT-STATE
                                          WS-SAVE-STATE
           MOVE CO-ACCOUNT             TO WS-ERACCT-ACCOUNT
           MOVE LOW-VALUES             TO WS-ERACCT-EXP-DT

           .
       0130-STARTBR.

           ADD +1                      TO WS-START-TRYS

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF (RESP-ENDFILE OR RESP-NOTFND)
              PERFORM VARYING A1 FROM +30 BY -1 UNTIL
                 (CO-ACCT-NAME (A1:1) = '*')
                 OR  (A1 < +1)
              END-PERFORM
              IF (A1 > +1 AND < +29)
                 AND (WS-START-TRYS < +2)
                 MOVE CO-ACCT-NAME (A1 + 1 :2)
                                       TO WS-ERACCT-STATE
                                          WS-SAVE-STATE
                 GO TO 0130-STARTBR
              ELSE
                 SET ACCT-NOT-FOUND    TO TRUE
                 GO TO 0130-EXIT
              END-IF
           ELSE
              IF NOT RESP-NORMAL
                 SET ACCT-NOT-FOUND    TO TRUE
                 GO TO 0130-EXIT
              END-IF
           END-IF

           .
       0130-READ-NEXT.

           ADD +1                      TO WS-READ-TRYS

           EXEC CICS READNEXT
              INTO    (ACCOUNT-MASTER)
              DATASET ('ERACCT')
              RIDFLD  (WS-ERACCT-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              AND AM-CARRIER = CO-CARRIER
              AND AM-GROUPING = CO-GROUPING
              AND AM-STATE = WS-SAVE-STATE
              AND AM-ACCOUNT = CO-ACCOUNT
              SET ACCT-FOUND           TO TRUE
           ELSE
              IF RESP-ENDFILE OR RESP-NOTFND
                 PERFORM VARYING A1 FROM +30 BY -1 UNTIL
                    (CO-ACCT-NAME (A1:1) = '*')
                    OR  (A1 < +1)
                 END-PERFORM
                 IF (A1 > +1 AND < +29)
                    AND (WS-READ-TRYS < +2)
                    MOVE CO-ACCT-NAME (A1 + 1 :2)
                                          TO WS-ERACCT-STATE
                                             WS-SAVE-STATE
                    GO TO 0130-READ-NEXT
                 ELSE
                    SET ACCT-NOT-FOUND    TO TRUE
                    GO TO 0130-ENDBR
                 END-IF
              ELSE
                 SET ACCT-NOT-FOUND    TO TRUE
                 GO TO 0130-ENDBR
              END-IF
           END-IF

           .
       0130-ENDBR.

           EXEC CICS ENDBR
               DATASET   ('ERACCT')
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0130-EXIT.
           EXIT.

       0140-PROCESS-ERPNDB2.

           MOVE ' '                    TO WS-SB-SW
           MOVE ' '                    TO WS-PNDB-SW

           MOVE LOW-VALUES             TO WS-ERPNDB2-KEY
           MOVE +0                     TO WS-ERPNDB2-CHG-SEQ
           MOVE AM-CONTROL-PRIMARY (1:20)
                                       TO WS-ERPNDB2-KEY (1:20)

           EXEC CICS STARTBR                                            
               DATASET   ('ERPNDB2')
               RIDFLD    (WS-ERPNDB2-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0140-READNEXT.

           IF RESP-NORMAL
              SET PNDB-STARTBR         TO TRUE
              EXEC CICS READNEXT
                 INTO    (PENDING-BUSINESS)
                 DATASET ('ERPNDB2')
                 RIDFLD  (WS-ERPNDB2-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 AND (PB-CONTROL-BY-ACCOUNT (1:20) =
                      AM-CONTROL-PRIMARY (1:20))
                 IF (PB-RECORD-TYPE = '1' OR '2')
                    SET PNDB-FOUND     TO TRUE
                 ELSE
                    GO TO 0140-READNEXT
                 END-IF
              ELSE
                 SET PNDB-NOT-FOUND    TO TRUE
              END-IF
           ELSE
              SET PNDB-NOT-FOUND       TO TRUE
           END-IF

           IF PNDB-STARTBR
              EXEC CICS ENDBR
                 DATASET  ('ERPNDB2')
              END-EXEC
           END-IF

           .
       0140-EXIT.
           EXIT.

       0500-SCRUB-DATA.

      *   THE DECODER DOES NOT LIKE & AND I TRIED TO USE
      *   A DIFFERENT DELIMITER IN PROGRAM NSREQLTR BUT
      *   COULD NOT GET IT TO WORK SO I AM CONVERTING IT
      *   TO A HEX 26 TO GET BY THE DECODER

           MOVE FUNCTION LENGTH(BL-ACCT-NAME (R1))
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL 
              A1 > M1
              OR BL-ACCT-NAME (R1) (A1:1) = '&'
           END-PERFORM
           IF A1 > M1
              CONTINUE
           ELSE
              MOVE BL-ACCT-NAME (R1) (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
              MOVE '%26'               TO WS-WORK-FIELD (A1:3)
              MOVE BL-ACCT-NAME (R1) (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
              MOVE WS-WORK-FIELD       TO BL-ACCT-NAME (R1)
           END-IF

           MOVE FUNCTION LENGTH(BL-COMMENT (R1))
                                       TO M1
           MOVE SPACES                 TO WS-WORK-FIELD
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL 
              A1 > M1
              OR BL-COMMENT (R1) (A1:1) = '&'
           END-PERFORM
           IF A1 > M1
              CONTINUE
           ELSE
              MOVE BL-COMMENT (R1) (1:A1 - 1)
                                       TO WS-WORK-FIELD (1:A1 - 1)
              MOVE '%26'               TO WS-WORK-FIELD (A1:3)
              MOVE BL-COMMENT (R1) (A1 + 1:M1 - A1)
                                       TO WS-WORK-FIELD (A1 + 3:M1 - A1)
              MOVE WS-WORK-FIELD       TO BL-COMMENT (R1)
           END-IF

           .
       0500-EXIT.
           EXIT.

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
