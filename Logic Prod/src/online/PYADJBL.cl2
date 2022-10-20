       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   LORPTBL.

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
           88  FOUND-ACCOUNT               VALUE 'Y'.
           88  ERROR-ON-ACCOUNT            VALUE 'X'.
           88  NO-ACCOUNT                  VALUE 'N'.
       77  WS-BROWSE-SW                PIC X  VALUE ' '.
           88  BROWSE-STARTED             VALUE 'Y'.
       77  WS-TALLY                    PIC S999 COMP-3 VALUE +0.
       77  WS-TALLY1                   PIC S999 COMP-3 VALUE +0.
       77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.

       01  WS-XML-WORK                 PIC X(2500)  VALUE SPACES.
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



       01  WS-ERLOFC-KEY.
           05  WS-ERLOFC-1             PIC X(20).
           05  WS-ERLOFC-CODE          PIC X(5).

       01  WS-ERPNDB2-KEY.
           05  WS-ERPNDB2-COMPANY-CD    PIC X.
           05  WS-ERPNDB2-CARRIER       PIC X.
           05  WS-ERPNDB2-GROUP         PIC X(6).
           05  WS-ERPNDB2-STATE         PIC XX.
           05  WS-ERPNDB2-ACCOUNT       PIC X(10).
           05  WS-ERPNDB2-EFF-DT        PIC XX.
           05  WS-ERPNDB2-CERT-NO       PIC X(11).
       01  WS-ERACCT-KEY.
           05  WS-ERACCT-COMPANY-CD    PIC X.
           05  WS-ERACCT-CARRIER       PIC X.
           05  WS-ERACCT-GROUP         PIC X(6).
           05  WS-ERACCT-STATE         PIC XX.
           05  WS-ERACCT-ACCOUNT       PIC X(10).
           05  WS-ERACCT-EXP-DT        PIC XX.
           05  FILLER                  PIC XXXX.

                                       COPY ELCDATE.
                                       COPY ERCLOFC.
                                       COPY ERCPNDB.
                                       COPY ERCACCT.

       01  FILLER                      PIC X(500)  VALUE SPACES.

       linkage section.
       
       01 dfhcommarea. 
        02  BL-COMMAREA.
           03  BL-OUTPUT OCCURS 200.
               05  BL-CARRIER          PIC X.
               05  BL-STATE            PIC XX.
               05  BL-ACCOUNT          PIC X(10).
               05  BL-EFF-DT           PIC X(10).
               05  BL-CERT-NO          PIC X(11).
               05  BL-ISS-CAN          PIC X(10).
               05  BL-LOAN-OFF         PIC X(5).
               05  BL-RETURN-CODE REDEFINES BL-LOAN-OFF
                                       PIC 9(5).
               05  BL-BATCH-NO         PIC X(6).
               05  BL-ACCT-NAME        PIC X(30).
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

           PERFORM 0000-GET-STARTED    THRU 0000-EXIT
           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
              (END-OF-INPUT)
              OR (R1 > 195)

           MOVE R1                     TO BL-COUNT
           IF R1 = ZEROS
              MOVE 1                   TO BL-COUNT
              MOVE ' ALL IS WELL IN LOAN OFFICER LAND '
                                       TO BL-MESSAGE
           END-IF

           exec cics return end-exec.	

           .
       0000-GET-STARTED.

           MOVE X'04'                  TO WS-ERPNDB2-KEY

           EXEC CICS STARTBR                                            
               DATASET   ('ERPNDB2')
               RIDFLD    (WS-ERPNDB2-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE 1                   TO R1
              MOVE ALL '?'             TO BL-OUTPUT (R1)
              MOVE 'BAD STARTBR ON ERPNDB2 ' TO BL-MESSAGE
              MOVE WS-RESPONSE         TO BL-RETURN-CODE (R1)
              SET END-OF-INPUT         TO TRUE
              GO TO 0000-EXIT
           END-IF

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              ADD +1                   TO R1
              MOVE ALL '?'             TO BL-OUTPUT (R1)
              MOVE 'BAD STARTBR ON ERACCT  ' TO BL-MESSAGE
              MOVE WS-RESPONSE         TO BL-RETURN-CODE (R1)
              SET END-OF-INPUT         TO TRUE
           ELSE
              EXEC CICS READNEXT
                 INTO    (ACCOUNT-MASTER)
                 DATASET ('ERACCT')
                 RIDFLD  (WS-ERACCT-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
              IF NOT RESP-NORMAL
                 ADD +1                   TO R1
                 MOVE ALL '?'             TO BL-OUTPUT (R1)
                 MOVE 'BAD INIT READNEXT ON ERACCT  ' TO BL-MESSAGE
                 MOVE WS-RESPONSE         TO BL-RETURN-CODE (R1)
                 SET END-OF-INPUT         TO TRUE
              END-IF
           END-IF

           .
       0000-EXIT.
           EXIT.

       0100-PROCESS-INPUT.

           EXEC CICS READNEXT
              INTO    (PENDING-BUSINESS)
              DATASET ('ERPNDB2')
              RIDFLD  (WS-ERPNDB2-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF (NOT RESP-NORMAL)
              OR (PB-COMPANY-CD NOT = X'04')
              MOVE 'REACHED END OF CID '
                                       TO BL-MESSAGE
              SET END-OF-INPUT         TO TRUE
              GO TO 0100-EXIT
           END-IF

           IF (PB-RECORD-TYPE = '1' OR '2')
              AND (NOT PB-FATAL-ERRORS)
              AND (NOT PB-UNFORCED-ERRORS)
              AND (NOT PB-RECORD-ON-HOLD)
              AND (NOT PB-RECORD-RETURNED)
              AND (NOT PB-POLICY-IS-DECLINED)
              AND (NOT PB-REIN-ONLY-CERT)
              AND (NOT PB-REISSUED-CERT)
              AND (NOT PB-POLICY-IS-VOIDED)
              MOVE ' '                 TO WS-ACCT-SW
           ELSE
              GO TO 0100-EXIT
           END-IF

           PERFORM 1200-GET-ERACCT     THRU 1200-EXIT UNTIL
              (FOUND-ACCOUNT)
              OR (ERROR-ON-ACCOUNT)
              OR (NO-ACCOUNT)

           IF ERROR-ON-ACCOUNT
              SET END-OF-INPUT         TO TRUE
              GO TO 0100-EXIT
           END-IF

           IF NO-ACCOUNT
              GO TO 0100-EXIT
           END-IF

           IF AM-EDIT-LOAN-OFC = 'Y'
              PERFORM 0200-CHECK-LOAN-OFFICER
                                       THRU 0200-EXIT
           ELSE
              GO TO 0100-EXIT
           END-IF

           IF LOAN-OFF-GOOD
              GO TO 0100-EXIT
           END-IF

           ADD +1                      TO R1
           MOVE PB-CARRIER             TO BL-CARRIER  (R1)
           MOVE PB-STATE               TO BL-STATE    (R1)
           MOVE PB-ACCOUNT             TO BL-ACCOUNT  (R1)
           MOVE PB-CERT-NO             TO BL-CERT-NO  (R1)
           MOVE PB-ENTRY-BATCH         TO BL-BATCH-NO (R1)
           MOVE AM-NAME                TO BL-ACCT-NAME (R1)
           IF PB-ISSUE
              MOVE 'ISSUE'             TO BL-ISS-CAN  (R1)
              MOVE PB-I-LOAN-OFFICER   TO BL-LOAN-OFF (R1)
           ELSE
              MOVE 'CANCEL'            TO BL-ISS-CAN  (R1)
              MOVE PB-CI-LOAN-OFFICER  TO BL-LOAN-OFF (R1)
           END-IF

           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE             
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO BL-EFF-DT (R1)
           ELSE
              MOVE '01/01/1900'        TO BL-EFF-DT (R1)
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-CHECK-LOAN-OFFICER.

           MOVE PB-CONTROL-BY-ACCOUNT (1:20)
                                       TO WS-ERLOFC-1
           IF PB-ISSUE
              MOVE PB-I-LOAN-OFFICER   TO WS-ERLOFC-CODE
           ELSE
              MOVE PB-CI-LOAN-OFFICER  TO WS-ERLOFC-CODE
           END-IF

           MOVE ' '                    TO WS-LOAN-OFF-SW

           IF WS-ERLOFC-CODE = SPACES OR ZEROS
              CONTINUE
           ELSE
              EXEC CICS READ                                               
                   DATASET    ('ERLOFC') 
                   INTO       (LOAN-OFFICER-MASTER)
                   RIDFLD     (WS-ERLOFC-KEY)
                   RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 SET LOAN-OFF-GOOD     TO TRUE
              END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       1200-GET-ERACCT.

          IF (PB-CONTROL-BY-ACCOUNT (2:19) < AM-CONTROL-A)
             SET NO-ACCOUNT            TO TRUE
             GO TO 1200-EXIT
          ELSE
             IF (PB-CONTROL-BY-ACCOUNT (2:19) > AM-CONTROL-A)
                PERFORM 1210-READNEXT  THRU 1210-EXIT
                GO TO 1200-EXIT
             END-IF
          END-IF

          IF (PB-CERT-EFF-DT >= AM-EFFECTIVE-DT)
             AND (PB-CERT-EFF-DT < AM-EXPIRATION-DT)
             SET FOUND-ACCOUNT      TO TRUE
             GO TO 1200-EXIT
          END-IF

          PERFORM 1210-READNEXT  THRU 1210-EXIT

           .
       1200-EXIT.
           EXIT.

       1210-READNEXT.

           EXEC CICS READNEXT
              INTO    (ACCOUNT-MASTER)
              DATASET ('ERACCT')
              RIDFLD  (WS-ERACCT-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              ADD +1                   TO R1
              MOVE PB-CONTROL-BY-ACCOUNT (2:19)
                                       TO BL-OUTPUT (R1)
              MOVE 'BAD READNEXT ON ERACCT  ' TO BL-MESSAGE
              MOVE WS-RESPONSE         TO BL-RETURN-CODE (R1)
              SET ERROR-ON-ACCOUNT     TO TRUE
           END-IF

          .
       1210-EXIT.
           EXIT.

       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
