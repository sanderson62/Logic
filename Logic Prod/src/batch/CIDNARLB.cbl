       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDNARLB.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS
      *   This program sequentially reads a file from CIDNARLX which
      *    contains records of claims that need a letter resent.
      *    Once a record has been selected the ELLETR file is read
      *    to find the letter to be used. Next, all the possible
      *    variables are resolved.  Finally, the finalized letter
      *    info is written to the ELNAPS file.
      *   This program was modeled from EL350.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
033110* 033110    2009122800001  AJRA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL       ASSIGN TO ELCNTL
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCNTL-FILE-STATUS
                               RECORD KEY IS CF-CONTROL-PRIMARY.
           SELECT ELNAPS       ASSIGN TO ELNAPS
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELNAPS-FILE-STATUS
                               RECORD KEY IS NA-CONTROL-PRIMARY.
           SELECT ELTRLR       ASSIGN TO ELTRLR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELTRLR-FILE-STATUS
                               RECORD KEY IS AT-CONTROL-PRIMARY.
           SELECT ELMSTR       ASSIGN TO ELMSTR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELMSTR-FILE-STATUS
                               RECORD KEY IS CL-CONTROL-PRIMARY.
           SELECT ELLETR       ASSIGN TO ELLETR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELLETR-FILE-STATUS
                               RECORD KEY IS TX-CONTROL-PRIMARY.
           SELECT DISK-DATE    ASSIGN TO SYS019.
           SELECT RESEND-LTR   ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.
                                       COPY ELCCNTL.
           EJECT
       FD  ELNAPS.
                                       COPY ELCNAPS.
           EJECT
       FD  ELTRLR.
                                       COPY ELCTRLR.
           EJECT
       FD  ELMSTR.
                                       COPY ELCMSTR.
           EJECT
       FD  ELLETR.
                                       COPY ELCTEXT.
           EJECT


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  RESEND-LTR
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  RESEND-LTR-REC              PIC X(120).
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDNARLB WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS  VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS     VALUE 'N'.
       77  ELCNTL-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELNAPS-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELTRLR-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELMSTR-FILE-STATUS          PIC XX     VALUE '00'.
       77  ELLETR-FILE-STATUS          PIC XX     VALUE '00'.
       77  WS-RESEND-DATE              PIC XX     VALUE LOW-VALUES.
       77  WS-FOLLOW-UP-DATE           PIC XX     VALUE LOW-VALUES.
       77  WS-NO-OF-COPIES             PIC 99     VALUE ZEROS.
       77  WS-RESEND-FORM-NUMBER       PIC X(4)   VALUE SPACES.
       77  WS-PROMPT-IND               PIC X(1)   VALUE SPACES.
       77  WS-ENCLOSURE-CD             PIC X(3)   VALUE SPACES.
       77  WS-AUTO-CLOSE-IND           PIC X      VALUE SPACES.
       77  WS-LETTER-TO-BENE           PIC X      VALUE SPACES.
       77  S1                          PIC S9     VALUE +0.
       77  S2                          PIC S9     VALUE +0.


       01  RESEND-LTR-RECORD.
           12  RL-COMPANY-CD           PIC X.
           12  RL-CARRIER              PIC X.
           12  RL-CLAIM-NO             PIC X(7).
           12  RL-CERT-NO              PIC X(11).
           12  RL-RESEND-LETTER-ID     PIC X(4).
           12  RL-PROCESSOR-ID         PIC X(4).
           12  RL-ADDRESS-TYPE         PIC X(2).
           12  RL-1ST-LTR-PRINT-DT     PIC X(2).
           12  RL-REASON-TEXT          PIC X(70).
           12  RL-ORIG-ARCHIVE-NO      PIC 9(9).
           12  RL-ORIG-ENCLOSURE-CD    PIC X(3).


       01  W-RECORD-TABLE              PIC  X(21900) VALUE SPACES.
       01  W-REC-TABLE REDEFINES W-RECORD-TABLE.
           12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-NDX
                                       PIC  X(3650).

       01  W-REC-ENTRIES REDEFINES W-RECORD-TABLE.
           12  W-REC-ENT OCCURS 300 TIMES INDEXED BY W-TB-NDX W-TB-NDX1.
               16  W-REC-TEXT          PIC  X(70).
               16  W-REC-PC            PIC  9(02).
               16  FILLER              PIC  X(01).

                                       EJECT
       01  W-PROGRAM-CONSTANTS.
           12  FILLER                  PIC  X(17)
                                       VALUE 'PROGRAM CONSTANTS'.
           12  W-LOWER-CASE
                          PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
           12  W-MAX-LINES             PIC  9(03) VALUE 300.
           12  W-TEXT-ID               PIC  X(08) VALUE 'ELLETR'.
           12  W-TOP-FORM              PIC  X(70)
               VALUE '*****TOP OF FORM *****'.
           12  W-UPPER-CASE
                          PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

       01  WS-MISC.
           12  W-NDX-WORK              PIC 9(02)   VALUE ZEROS.
           12  WS-ARCHIVE-NO           PIC S9(8)   COMP VALUE +0.
           12  WS-CURRENT-TIME         PIC S9(7)   VALUE ZERO.
           12  WS-CURRENT-BIN-DT       PIC XX  VALUE LOW-VALUES.
           12  W-DATE-WORK             PIC  9(07).
           12  W-DT-REDEF REDEFINES W-DATE-WORK.
               16  FILLER              PIC  X(02).
               16  W-DT-WORK           PIC  9(05).

           12  W-CORR-TRLR-SEQ         PIC S9(04) COMP.

           12  W-Z-CONTROL-DATA.
               16  W-NUMBER-OF-COPIES  PIC  9(01).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-FOLLOW-UP PIC  9(03).
               16  FILLER              PIC  X(01).
               16  W-DAYS-TO-RESEND-1  PIC  9(03).
               16  FILLER              PIC  X(01).
               16  W-FORM-TO-RESEND    PIC  X(04).
               16  FILLER              PIC  X(01).
               16  W-PROMPT-LETTER     PIC  X(01).
               16  FILLER              PIC  X(01).
               16  W-ENCLOSURE-CD      PIC  X(03).
               16  FILLER              PIC  X(1).
               16  W-AUTO-CLOSE-IND    PIC  X(1).
               16  FILLER              PIC  X(1).
               16  W-LETTER-TO-BENE    PIC  X(1).                        


           12  W-CURRENT-SAVE          PIC  X(02).
           12  W-EDIT-DATE-1.
               16  W-ED1-MM            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED1-DD            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED1-YY            PIC X(02).
           12  W-EDIT-DATE-2.
               16  W-ED2-DD            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED2-MM            PIC X(02).
               16  FILLER              PIC X(01)    VALUE '/'.
               16  W-ED2-YY            PIC X(02).

           12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
           12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.

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


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +350.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0110-READ-RESEND-LTR THRU 0110-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
               THERE-ARE-NO-MORE-RECORDS

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
             .

       0010-INITIALIZE.

           ACCEPT WS-TIME-OF-DAY       FROM  TIME

           MOVE WS-TIME                TO  WS-CURRENT-TIME

           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.
           MOVE '2'                    TO  DC-OPTION-CODE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
           DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT
           END-IF
           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ELLETR
                      RESEND-LTR
               I-O
                      ELNAPS
                      ELCNTL
                      ELMSTR
                      ELTRLR

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELNAPS-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELNAPS OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELNAPS-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELTRLR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELTRLR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE      ELCNTL
                      ELTRLR
                      ELMSTR
                      ELLETR
                      ELNAPS
                      RESEND-LTR


           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELNAPS-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELNAPS CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELNAPS-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELMSTR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0060-CONTINUE    THRU 0060-EXIT

           PERFORM 0110-READ-RESEND-LTR THRU 0110-EXIT
           .

       0050-EXIT.
           EXIT.

       0060-CONTINUE.

           PERFORM 2000-CREATE-LETTER  THRU 2199-EXIT

           PERFORM 6000-ARCHIVE-LETTER THRU 6000-EXIT

           .

       0060-EXIT.
           EXIT.

       0110-READ-RESEND-LTR.

           READ RESEND-LTR INTO RESEND-LTR-RECORD
              AT END
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE.

       0110-EXIT.
           EXIT.


       2000-CREATE-LETTER.
      ***************************************************************
      *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
      *    TEXT FILE WITH THE FORM CODE FROM THE RESEND LTR RECORD. *
      *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
      *    WILL BE WRITTEN TO THE ELNAPS FILE.                      *
      *                                                             *
      ***************************************************************

           MOVE SPACES                 TO W-RECORD-TABLE

           SET W-TB-NDX                TO 7
           MOVE W-TOP-FORM             TO W-REC-TEXT (W-TB-NDX)
           SET W-TB-NDX UP BY 1

           MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY
           MOVE RL-COMPANY-CD          TO TX-COMPANY-CD
           MOVE RL-RESEND-LETTER-ID    TO TX-LETTER-NO

           START ELLETR KEY IS NOT LESS THAN TX-CONTROL-PRIMARY

           IF ELLETR-FILE-STATUS = '10' OR '23'
              GO TO 2120-ENDBR
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 MOVE ' ELLETR START ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELLETR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .

       2110-READ-NEXT.

           IF W-TB-NDX GREATER THAN W-MAX-LINES
              DISPLAY 'W-TB-NDX > W-MAX-LINES '
              GO TO 2120-ENDBR
           END-IF

           READ ELLETR NEXT RECORD
           IF ELLETR-FILE-STATUS = '10' OR '23'
              GO TO 2120-ENDBR
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 MOVE ' ELLETR READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELLETR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF (RL-COMPANY-CD = TX-COMPANY-CD) AND
              (RL-RESEND-LETTER-ID = TX-LETTER-NO)
              CONTINUE
           ELSE
              GO TO 2120-ENDBR
           END-IF


           IF TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
              INITIALIZE W-Z-CONTROL-DATA
              MOVE LOW-VALUES             TO WS-RESEND-DATE
                                          WS-FOLLOW-UP-DATE
              PERFORM 2800-PROCESS-Z-CONTROLS
                                       THRU 2800-EXIT
              GO TO 2110-READ-NEXT
           END-IF


           MOVE TX-TEXT-LINE           TO W-REC-TEXT (W-TB-NDX)
           MOVE TX-PROCESS-CONTROL     TO W-REC-PC (W-TB-NDX)
                                          W-NDX-WORK
           SET W-TB-NDX UP BY 1

           IF W-NDX-WORK = 99
              MOVE W-TOP-FORM          TO W-REC-TEXT (W-TB-NDX)
              SET W-TB-NDX UP BY 1
              GO TO 2110-READ-NEXT
           ELSE
              SET W-TB-NDX UP BY W-NDX-WORK
              GO TO 2110-READ-NEXT
           END-IF

           .
       2120-ENDBR.


           IF W-TB-NDX = 8
              DISPLAY ' W-TB-NDX = 8 '
              GO TO 2199-EXIT
           END-IF.


       2199-EXIT.
           EXIT.
           
           
       2800-PROCESS-Z-CONTROLS.

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
           
           IF W-NUMBER-OF-COPIES NOT NUMERIC
              MOVE ZEROS               TO W-NUMBER-OF-COPIES
           END-IF
           IF W-NUMBER-OF-COPIES > 0
              MOVE W-NUMBER-OF-COPIES  TO WS-NO-OF-COPIES
           ELSE
              MOVE 1                   TO WS-NO-OF-COPIES 
           END-IF

           IF (W-DAYS-TO-RESEND-1 NUMERIC) AND
              (W-DAYS-TO-RESEND-1 > ZEROS)
              MOVE '6'              TO DC-OPTION-CODE
              MOVE WS-CURRENT-BIN-DT TO DC-BIN-DATE-1
              MOVE ZEROS            TO DC-ELAPSED-MONTHS
              MOVE W-DAYS-TO-RESEND-1
                                    TO DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2 TO WS-RESEND-DATE
              ELSE
                 DISPLAY ' RESEND DATE ERROR '
              END-IF
           END-IF

           IF (W-DAYS-TO-FOLLOW-UP NUMERIC) AND
              (W-DAYS-TO-FOLLOW-UP > ZEROS)
              MOVE '6'              TO DC-OPTION-CODE
              MOVE WS-CURRENT-BIN-DT TO DC-BIN-DATE-1
              MOVE ZEROS            TO DC-ELAPSED-MONTHS
              MOVE W-DAYS-TO-FOLLOW-UP
                                    TO DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2 TO WS-FOLLOW-UP-DATE
              ELSE
                 DISPLAY 'FOLLOW UP DATE ERROR'
              END-IF
           END-IF

           IF W-FORM-TO-RESEND > SPACES
               MOVE W-FORM-TO-RESEND          TO WS-RESEND-FORM-NUMBER
           ELSE
               MOVE LOW-VALUES                TO WS-RESEND-FORM-NUMBER
           END-IF.
           
           MOVE W-PROMPT-LETTER               TO WS-PROMPT-IND
           MOVE W-ENCLOSURE-CD                TO WS-ENCLOSURE-CD
           MOVE W-AUTO-CLOSE-IND              TO WS-AUTO-CLOSE-IND
           MOVE W-LETTER-TO-BENE              TO WS-LETTER-TO-BENE

           .
       2800-EXIT.
           EXIT.

                                       EJECT


       6000-ARCHIVE-LETTER.
      ***************************************************************
      *    THIS ROUTINE WILL BE USED TO PUT PERMANENT RECORDS ONTO  *
      *    THE ELNAPS FILE.                                         *
      *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
      *        1. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
      *        2. BUILD A CORRESPONDENCE TRAILER.                   *
      *        3. BUILD A NAPERSOFT RECORD                          *
      ***************************************************************


           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           MOVE SPACES                 TO CF-ACCESS-CD-GENL


           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              DISPLAY ' COMPANY RECORD, ELCNTL, NOT FOUND'
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 MOVE ' ELCNTL READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           ADD 1                       TO CF-CO-ARCHIVE-COUNTER
           MOVE CF-CO-ARCHIVE-COUNTER  TO WS-ARCHIVE-NO

           REWRITE CONTROL-FILE
           IF ELCNTL-FILE-STATUS NOT = '00'
              MOVE ' ELCNTL REWRITE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF


           PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
           
           PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT.

           .
       6000-EXIT.
           EXIT.

                                       EJECT


       6500-BUILD-CORRESPOND.
      ***************************************************************
      *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
      *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
      *    USING THE NEW SEQUENCE NUMBER.                           *
      *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
      *    TRAILER RECORD.                                          *
      ***************************************************************

           MOVE RL-COMPANY-CD          TO CL-COMPANY-CD
           MOVE RL-CARRIER             TO CL-CARRIER
           MOVE RL-CLAIM-NO            TO CL-CLAIM-NO
           MOVE RL-CERT-NO             TO CL-CERT-NO
           READ ELMSTR
           IF ELMSTR-FILE-STATUS = '10' OR '23'
              MOVE ' ELMSTR  READ   ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 MOVE ' ELMSTR  read   ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.

           IF WS-FOLLOW-UP-DATE > CL-NEXT-FOLLOWUP-DT
              MOVE WS-FOLLOW-UP-DATE   TO CL-NEXT-FOLLOWUP-DT
           END-IF

           IF WS-RESEND-DATE > CL-NEXT-FOLLOWUP-DT
              MOVE WS-RESEND-DATE      TO CL-NEXT-FOLLOWUP-DT
           END-IF

           MOVE '2'                    TO CL-LAST-MAINT-TYPE

           MOVE SPACES                 TO ACTIVITY-TRAILERS
           MOVE 'AT'                   TO AT-RECORD-ID
           MOVE  4                     TO AT-TRAILER-TYPE
           MOVE WS-CURRENT-BIN-DT      TO AT-RECORDED-DT
                                          CL-LAST-MAINT-DT
                                          AT-CORR-LAST-MAINT-DT
           MOVE 'SYST'                 TO AT-RECORDED-BY
                                          CL-LAST-MAINT-USER
                                          AT-CORR-LAST-UPDATED-BY

           MOVE WS-CURRENT-TIME        TO AT-LAST-MAINT-HHMMSS
                                          CL-LAST-MAINT-HHMMSS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE CL-CLAIM-NO            TO AT-CLAIM-NO
           MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
                                          W-CORR-TRLR-SEQ
           MOVE WS-CURRENT-BIN-DT      TO AT-LETTER-SENT-DT
           MOVE WS-FOLLOW-UP-DATE      TO AT-RECEIPT-FOLLOW-UP
           MOVE WS-RESEND-DATE         TO AT-AUTO-RE-SEND-DT
           MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
                                          AT-LETTER-PURGED-DT
           MOVE WS-ARCHIVE-NO          TO AT-LETTER-ARCHIVE-NO
           MOVE '4'                    TO AT-LETTER-ORIGIN

           MOVE RL-RESEND-LETTER-ID    TO AT-STD-LETTER-FORM
           MOVE WS-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM
           MOVE WS-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND
           MOVE WS-LETTER-TO-BENE      TO AT-LETTER-TO-BENE

           MOVE RL-REASON-TEXT         TO AT-REASON-TEXT

           IF RL-ADDRESS-TYPE (1:1) > SPACES
               MOVE RL-ADDRESS-TYPE (2:1) TO AT-ADDRESS-REC-SEQ-NO
               MOVE RL-ADDRESS-TYPE (1:1) TO AT-ADDRESEE-TYPE
           ELSE
               MOVE ZEROS              TO AT-ADDRESS-REC-SEQ-NO
               MOVE SPACES             TO AT-ADDRESEE-TYPE
           END-IF

           MOVE W-REC-TEXT (1)         TO AT-ADDRESSEE-NAME

           MOVE LOW-VALUES             TO AT-INITIAL-PRINT-DATE
                                          AT-RESEND-PRINT-DATE


           WRITE ACTIVITY-TRAILERS
           IF ELTRLR-FILE-STATUS = '22'
              DISPLAY ' ELTRLR DUP RECORD  '
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE ' ELTRLR  WRITE  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           REWRITE CLAIM-MASTER
           IF ELMSTR-FILE-STATUS = '22'
              DISPLAY ' ELMSTR DUP RECORD  '
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 MOVE ' ELMSTR REWRITE ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELMSTR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF.

       6599-EXIT.
            EXIT.
                                       EJECT
       6700-BUILD-NAPERSOFT.
            
           MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
           MOVE 'NA'                  TO  NA-RECORD-ID.
           MOVE RL-COMPANY-CD         TO  NA-COMPANY-CD.
           MOVE RL-CARRIER            TO  NA-CARRIER.
           MOVE RL-CLAIM-NO           TO  NA-CLAIM-NO.
           MOVE RL-CERT-NO            TO  NA-CERT-NO.
           MOVE WS-ARCHIVE-NO         TO  NA-ARCHIVE-NO.
           MOVE RL-RESEND-LETTER-ID   TO  NA-LETTER-ID.
           MOVE 'SYST'                TO  NA-PROCESSOR-ID.
           MOVE WS-CURRENT-BIN-DT     TO  NA-CREATION-DT.
           MOVE LOW-VALUES            TO  NA-INITIAL-PRINT-DT
           MOVE WS-FOLLOW-UP-DATE     TO  NA-FOLLOW-UP-DT.
           MOVE WS-RESEND-DATE        TO  NA-RESEND-DT
           MOVE WS-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
           IF WS-NO-OF-COPIES NOT = ZEROS
               MOVE WS-NO-OF-COPIES   TO  NA-NO-OF-COPIES
           ELSE
               MOVE 1                 TO  NA-NO-OF-COPIES
           END-IF.
           IF RL-ADDRESS-TYPE = ' 0'
               MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
           ELSE
               MOVE RL-ADDRESS-TYPE   TO  NA-ADDRESS-TYPE
           END-IF.
           MOVE W-CORR-TRLR-SEQ       TO  NA-CORR-TRLR-SEQ.
           MOVE RL-1ST-LTR-PRINT-DT   TO  NA-1ST-LTR-PRINT-DT.

           MOVE '6'                   TO  DC-OPTION-CODE
           MOVE WS-CURRENT-BIN-DT     TO  DC-BIN-DATE-1
           MOVE ZEROS                 TO  DC-ELAPSED-MONTHS
           MOVE 14                    TO  DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2      TO  NA-NEXT-DUE-DT
           ELSE
              DISPLAY ' NEXT DUE DATE ERROR '
           END-IF.
           
           IF WS-ENCLOSURE-CD EQUAL '@  '
               MOVE RL-ORIG-ENCLOSURE-CD TO NA-ENCLOSURE-CD
           ELSE
               MOVE WS-ENCLOSURE-CD   TO  NA-ENCLOSURE-CD
           END-IF
           MOVE RL-ORIG-ARCHIVE-NO    TO  NA-ORIG-ARCHIVE-NO
           MOVE RL-ORIG-ENCLOSURE-CD  TO  NA-ORIG-ENCLOSURE-CD
           MOVE WS-PROMPT-IND         TO  NA-RESEND-PROMPT-IND
      
           WRITE NAPERSOFT-FILE
           IF ELNAPS-FILE-STATUS = '22'
              DISPLAY ' ELNAPS DUP RECORD  ' NAPERSOFT-FILE
           ELSE
              IF ELNAPS-FILE-STATUS NOT = '00'
                 MOVE ' ELNAPS  WRITE  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ELNAPS-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF.
      
       6799-EXIT.
            EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
