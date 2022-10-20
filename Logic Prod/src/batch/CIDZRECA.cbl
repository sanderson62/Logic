       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDZRECA.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM A SPREAD SHEET
      *   AND MATCHES IT TO THE "Z" RECORDS ON THE ELLETR FILE
      *   AND CREATES A FLAT FILE WITH THE NEW Z RECORD AND 
      *   ALSO CREATES ANOTHER RECORD WITH THE DESCRIPTION.
      *   THE INPUT EXTRACT FILE SHOULD BE SORTED IN LETTER SEQ

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELLETR       ASSIGN TO ELLETR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELLETR-FILE-STATUS
                               RECORD KEY IS ELLETR-KEY.

           SELECT LETR-OUT     ASSIGN TO SYS011.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(182).

       FD  LETR-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  LETR-OUT-REC                PIC X(100).

       FD  ELLETR.

       01  ELLETR-IN-REC.
           05  FILLER                  PIC XX.
           05  ELLETR-KEY              PIC X(15).
           05  FILLER                  PIC X(83).
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    PEMTXB1T WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ELLETR-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-ELLETR-SW                PIC X  VALUE ' '.
           88  END-OF-ELLETR              VALUE 'Y'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.
       77  WS-ELLETR-HOLD              PIC X(100) VALUE SPACES.
       77  WS-FOUND-SW                 PIC X    VALUE ' '.
           88  WE-FOUND-IT               VALUE 'Y'.
           88  WRITE-0-SEQ               VALUE '0'.
           88  WRITE-1-SEQ               VALUE '1'.
       77  I1                          PIC S999 COMP-3 VALUE +0.
       77  O1                          PIC S999 COMP-3 VALUE +0.
       77  WS-IN-RECS                  PIC 9(5) VALUE ZEROS.
       77  WS-OUT-RECS                 PIC 9(5) VALUE ZEROS.
       77  WS-MATCH-SW                 PIC X    VALUE ' '.
           88  MATCH-FOUND                VALUE 'Y'.
       77  WS-COMPANY-CD               PIC X.

                                       COPY ELCTEXT.

       01  WS-WORK-DESC.
           05  WS-DESC1                PIC X(70).
           05  WS-DESC2                PIC X(70).
           05  F                       PIC X(10).
       01  WS-MISC.
           05  WS-CHECK-AMT            PIC X(09).
           05  WS-CHECK-AMT-N REDEFINES WS-CHECK-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-ELLETR-OUT           PIC 9(7)   VALUE ZEROS.

                   COPY ELCZREC.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  EXTRACT-RECORD.
           12  EX-COMPANY-ID           PIC X(3).
           12  EX-LETTER-ID            PIC XXXX.
           12  EX-LETTER-DESC          PIC X(150).
           12  EX-COPIES               PIC 9.
           12  EX-FADAYS               PIC 999.
           12  EX-RSDAYS               PIC 999.
           12  EX-FORM                 PIC XXXX.
           12  EX-PROMPT               PIC X.
           12  EX-ENCCODE              PIC XXX.
           12  EX-AUTO-CLOSE           PIC X.
           12  EX-BENE                 PIC X.
           12  EX-ACCT                 PIC X.
           12  EX-TYPE                 PIC X.
           12  EX-PRINT-CERT           PIC X.
           12  EX-REFUND               PIC X.
           12  EX-ONBASE               PIC XX.
           12  EX-ACCT-SUMM            PIC X.
           12  EX-CSO-SUMM             PIC X.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ELLETR)
PEMTST*         OR (WS-EXTR-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-IN-RECS
           DISPLAY ' RECORDS  OUT  ' WS-OUT-RECS
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN ELLETR
               OUTPUT LETR-OUT
               
           IF ELLETR-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ELLETR ' ELLETR-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ELLETR LETR-OUT

           IF ELLETR-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR ELLETR ' ELLETR-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF
               
           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           PERFORM 0120-START-ELLETR   THRU 0120-EXIT
           PERFORM 0130-READ-ELLETR    THRU 0130-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE ' '                    TO WS-MATCH-SW
           
           IF EX-LETTER-ID = TX-LETTER-NO  AND
              WS-COMPANY-CD = TX-COMPANY-CD
              SET MATCH-FOUND          TO TRUE
              DISPLAY ' FOUND EQUAL ' EX-LETTER-ID
              PERFORM 0070-PROCESS-EQUAL THRU 0070-EXIT
              PERFORM 0110-READ-INPUT THRU 0110-EXIT
           ELSE
              IF (TX-LETTER-NO < EX-LETTER-ID    AND 
                 WS-COMPANY-CD = TX-COMPANY-CD)  OR
                 TX-COMPANY-CD < WS-COMPANY-CD
                 DISPLAY ' TX < EX ' TX-LETTER-NO ' ' EX-LETTER-ID
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 PERFORM 0130-READ-ELLETR    THRU 0130-EXIT
              ELSE
                 DISPLAY ' NO MATCH FOUND ' EX-LETTER-ID
                 PERFORM 0060-BUILD-Z-RECORD THRU 0060-EXIT
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 PERFORM 0065-BUILD-D-RECORD THRU 0065-EXIT
                 IF WS-DESC2 NOT = SPACES
                    ADD +1 TO WS-WORK-SEQ
                    MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                    DISPLAY ' SOMETHING IN 2ND DESC ' TX-LETTER-NO
                       ' ' TX-LINE-SEQUENCE
                    PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                    MOVE WS-DESC2         TO TX-TEXT-LINE
                    MOVE 'D'              TO TX-LINE-SQUEEZE-CONTROL
                 END-IF
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                 DISPLAY ' COMING FROM DEFAULT DESC ' TX-LETTER-NO
                       ' ' TX-LINE-SEQUENCE
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 PERFORM 0110-READ-INPUT THRU 0110-EXIT
PEMTST           PERFORM 0130-READ-ELLETR    THRU 0130-EXIT
              END-IF
           END-IF

           .
       0050-EXIT.
           EXIT.

       0060-BUILD-Z-RECORD.

           MOVE 'TL'                TO TEXT-FILES
           MOVE WS-COMPANY-CD       TO TX-COMPANY-CD
           MOVE EX-LETTER-ID        TO TX-LETTER-NO
           MOVE 'Z'                 TO TX-LINE-SQUEEZE-CONTROL
           MOVE +1                  TO TX-LINE-SEQUENCE
                                       WS-WORK-SEQ
           MOVE WS-CURRENT-BIN-DATE    TO TX-LAST-MAINTENANCED-DT
           MOVE 'CONV'                 TO TX-LAST-MAINTENANCED-BY
           IF EX-COPIES NOT NUMERIC    
              MOVE ZEROS               TO EX-COPIES
           END-IF                      
           IF EX-COPIES = ZERO         
              MOVE 1                   TO EX-COPIES      
           END-IF                      
           MOVE EX-COPIES              TO W-NUMBER-OF-COPIES
           MOVE EX-FADAYS              TO W-DAYS-TO-FOLLOW-UP
           MOVE EX-RSDAYS              TO W-DAYS-TO-RESEND
           MOVE EX-FORM                TO W-FORM-TO-RESEND
           MOVE EX-PROMPT              TO W-PROMPT-LETTER
           MOVE FUNCTION UPPER-CASE(EX-ENCCODE)
                                       TO W-ENCLOSURE-CD
           MOVE EX-AUTO-CLOSE          TO W-AUTO-CLOSE-IND
           MOVE EX-BENE                TO W-LETTER-TO-BENE
           MOVE EX-ACCT                TO W-LETTER-TO-ACCT
           MOVE EX-TYPE                TO W-LETTER-TYPE
           MOVE EX-PRINT-CERT          TO W-PRINT-CERTIFICATE
           MOVE EX-REFUND              TO W-REFUND-REQUIRED
           MOVE EX-ONBASE              TO W-ONBASE-CODE
           MOVE EX-ACCT-SUMM           TO W-ACCT-SUMM
           MOVE EX-CSO-SUMM            TO W-CSO-SUMM
           MOVE W-Z-CONTROL-DATA       TO TX-TEXT-LINE

      *    DISPLAY ' ABOUT TO WRITE  Z REC ' TX-LETTER-NO
      *       ' ' TX-TEXT-LINE ' ' TX-LINE-SEQUENCE

           .
       0060-EXIT.
           EXIT.

       0065-BUILD-D-RECORD.

           MOVE 'TL'                TO TEXT-FILES
           MOVE WS-COMPANY-CD       TO TX-COMPANY-CD
           MOVE EX-LETTER-ID        TO TX-LETTER-NO
           MOVE ' '                 TO TX-LINE-SQUEEZE-CONTROL
           MOVE WS-CURRENT-BIN-DATE    TO TX-LAST-MAINTENANCED-DT
           MOVE 'CONV'                 TO TX-LAST-MAINTENANCED-BY
           MOVE FUNCTION UPPER-CASE(EX-LETTER-DESC)
                                       TO WS-WORK-DESC
           MOVE WS-DESC1               TO TX-TEXT-LINE
           MOVE 'D'                 TO TX-LINE-SQUEEZE-CONTROL

      *    DISPLAY ' ABOUT TO WRITE DESC ' TX-LETTER-NO
      *       ' ' WS-WORK-DESC ' ' TX-LINE-SEQUENCE

           .
       0065-EXIT.
           EXIT.

       0070-PROCESS-EQUAL.

           IF TX-LINE-SQUEEZE-CONTROL = 'Z'
              PERFORM 0060-BUILD-Z-RECORD THRU 0060-EXIT
              PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
              PERFORM 0065-BUILD-D-RECORD THRU 0065-EXIT
              IF WS-DESC2 NOT = SPACES
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 MOVE WS-DESC2         TO TX-TEXT-LINE
                 MOVE 'D'              TO TX-LINE-SQUEEZE-CONTROL
              END-IF
              PERFORM UNTIL TX-LETTER-NO NOT = EX-LETTER-ID
                 OR END-OF-ELLETR
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 PERFORM 0130-READ-ELLETR THRU 0130-EXIT
              END-PERFORM
           ELSE
              MOVE TEXT-FILES          TO WS-ELLETR-HOLD
              PERFORM 0060-BUILD-Z-RECORD THRU 0060-EXIT
              PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
              PERFORM 0065-BUILD-D-RECORD THRU 0065-EXIT
              IF WS-DESC2 NOT = SPACES
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 MOVE WS-DESC2         TO TX-TEXT-LINE
                 MOVE 'D'              TO TX-LINE-SQUEEZE-CONTROL
              END-IF
              ADD +1 TO WS-WORK-SEQ
              MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
              PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
              MOVE WS-ELLETR-HOLD TO TEXT-FILES
              PERFORM UNTIL TX-LETTER-NO NOT = EX-LETTER-ID
                 OR END-OF-ELLETR
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO TX-LINE-SEQUENCE
                 PERFORM 0075-WRITE-LETR-OUT THRU 0075-EXIT
                 PERFORM 0130-READ-ELLETR THRU 0130-EXIT
              END-PERFORM
           END-IF

           .
       0070-EXIT.
           EXIT.

       0075-WRITE-LETR-OUT.

           IF (TX-COMPANY-CD = WS-COMPANY-CD)
              AND (TX-LINE-SQUEEZE-CONTROL = 'Z' OR 'D')
              IF TX-LINE-SQUEEZE-CONTROL = 'D'
                 MOVE ' '              TO TX-LINE-SQUEEZE-CONTROL
                 DISPLAY ' ABOUT TO WRITE DESC ' TX-LETTER-NO
                   ' ' TX-TEXT-LINE ' ' TX-LINE-SEQUENCE
              ELSE
                 DISPLAY ' ABOUT TO WRITE  Z REC ' TX-LETTER-NO
                   ' ' TX-TEXT-LINE ' ' TX-LINE-SEQUENCE
              END-IF
              WRITE LETR-OUT-REC FROM TEXT-FILES
              ADD 1 TO WS-OUT-RECS
              MOVE ' '                 TO TX-LINE-SQUEEZE-CONTROL
           ELSE
              IF NOT MATCH-FOUND
                 DISPLAY ' ABOUT TO WRITE UNKNOWNC ' TX-LETTER-NO
                    ' ' TX-TEXT-LINE ' ' TX-LINE-SEQUENCE
                 WRITE LETR-OUT-REC FROM TEXT-FILES
                 ADD 1 TO WS-OUT-RECS
              END-IF
           END-IF

           .
       0075-EXIT.
           EXIT.

       0110-READ-INPUT.

           READ EXTR-IN INTO EXTRACT-RECORD AT END
              SET END-OF-INPUT         TO TRUE
              MOVE HIGH-VALUES         TO EX-LETTER-ID
           END-READ

           IF NOT END-OF-INPUT
              DISPLAY ' BEGIN PROCESSING Z ' EX-LETTER-ID
              ADD +1                   TO WS-EXTR-IN
           END-IF

           IF EX-COMPANY-ID = 'CID'
               MOVE X'04'              TO WS-COMPANY-CD
           ELSE
              IF EX-COMPANY-ID = 'DCC' 
                 MOVE X'05'            TO WS-COMPANY-CD
              ELSE
                 IF EX-COMPANY-ID = 'AHL' 
                    MOVE X'06'         TO WS-COMPANY-CD
                 ELSE
                    MOVE HIGH-VALUES   TO WS-COMPANY-CD
                 END-IF
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ELLETR.

           MOVE WS-COMPANY-CD  TO ELLETR-KEY
           START ELLETR KEY >= ELLETR-KEY
           IF ELLETR-FILE-STATUS NOT = '00'
              DISPLAY 'ELLETR - ERROR - START ' ELLETR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
        0120-EXIT.
            EXIT.

       0130-READ-ELLETR.

           READ ELLETR NEXT RECORD INTO TEXT-FILES

           IF ELLETR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELLETR        TO TRUE
              DISPLAY 'ELLETR - FINISH - READ ' ELLETR-FILE-STATUS
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELLETR - ERROR - READ  ' ELLETR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ELLETR
              ADD 1 TO WS-IN-RECS
           END-IF
           
           .
       0130-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

