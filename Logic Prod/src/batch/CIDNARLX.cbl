       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDNARLX.
       AUTHOR.     PABLO.
       DATE-COMPILED.
033110******************************************************************
033110*                   C H A N G E   L O G
033110*
033110* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
033110*-----------------------------------------------------------------
033110*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
033110* EFFECTIVE    NUMBER
033110*-----------------------------------------------------------------
033110* 033110    2009122800001  AJRA  NEW PROGRAM
102510* 102510    2009122800001  AJRA  BYPASS LETTER IF CLAIM CLOSED
102810* 102810    2009122800001  AJRA  BYPASS LETTER IF STOP DATE EXISTS
033110******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELNAPS           ASSIGN TO ELNAPS
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NA-CONTROL-PRIMARY
                                   FILE STATUS IS ELNAPS-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.
102510
102510     SELECT ELMSTR           ASSIGN TO ELMSTR
102510                             ORGANIZATION IS INDEXED
102510                             ACCESS IS DYNAMIC
102510                             RECORD KEY IS CL-CONTROL-PRIMARY
102510                             FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT RESEND-LTR-OUT   ASSIGN TO SYS010
                                   ORGANIZATION IS RECORD SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELNAPS.
       
           COPY ELCNAPS.

       FD  ELTRLR.

           COPY ELCTRLR.
102510
102510 FD  ELMSTR.
102510
102510     COPY ELCMSTR.

       FD  RESEND-LTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  RESEND-LTR-OUT-REC              PIC X(120).


       FD  DISK-DATE
           COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDNARLX  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELNAPS             VALUE 'Y'.
       77  NAPS-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  LTR-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.


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

      ******************************************************************
       01  WS-MISC.
           05  ELNAPS-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
102510     05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
           05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
           05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.
           05  WS-BIN-DATE             PIC X(02)  VALUE LOW-VALUES.
           05  WS-CURRENT-BIN-DT       PIC X(02)  VALUE LOW-VALUES.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                 (END-OF-ELNAPS)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ELNAPS RECORDS READ    '  NAPS-RECS-IN
           DISPLAY ' ELTRLR RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' LETTER RECORDS WRITTEN '  LTR-RECS-OUT
           GOBACK

           .
       0050-PROCESS-FILE.

           IF NA-RESEND-DT NOT EQUAL LOW-VALUES  AND
              NA-RESEND-DT NOT GREATER THAN WS-CURRENT-BIN-DT  AND
              NA-RESEND-PRINT-DT EQUAL LOW-VALUES
      *        DISPLAY 'PROCESSING ARCHIVE ' NA-ARCHIVE-NO
              PERFORM 0100-PROCESS-ELNAPS THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ELNAPS    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ELNAPS.

           IF NA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
               SET END-OF-ELNAPS TO TRUE
               GO TO 0100-EXIT
           END-IF
           
           IF NA-RESEND-LETTER-ID = SPACES OR LOW-VALUES
              DISPLAY 'NO RESEND LETTER ID - ARCHIVE '
                      NA-ARCHIVE-NO
              GO TO 0100-EXIT
           END-IF.
102510           
102510     MOVE LOW-VALUES         TO CLAIM-MASTER.
102510     MOVE NA-COMPANY-CD      TO CL-COMPANY-CD.
102510     MOVE NA-CARRIER         TO CL-CARRIER.
102510     MOVE NA-CLAIM-NO        TO CL-CLAIM-NO.
102510     MOVE NA-CERT-NO         TO CL-CERT-NO.
102510
102510     READ ELMSTR
102510         INVALID KEY
102510           GO TO 0100-EXIT
102510     END-READ.
102510
102510     IF CLAIM-IS-CLOSED
102510         GO TO 0100-EXIT
102510     END-IF.
    
           MOVE LOW-VALUES         TO ACTIVITY-TRAILERS.
           MOVE NA-COMPANY-CD      TO AT-COMPANY-CD.
           MOVE NA-CARRIER         TO AT-CARRIER.
           MOVE NA-CLAIM-NO        TO AT-CLAIM-NO.
           MOVE NA-CERT-NO         TO AT-CERT-NO.
           MOVE NA-CORR-TRLR-SEQ   TO AT-SEQUENCE-NO.

           READ ELTRLR 
             INVALID KEY
               GO TO 0100-EXIT
           END-READ.

           IF AT-LETTER-ANSWERED-DT IS NOT EQUAL TO LOW-VALUES
      *         DISPLAY 'LETTER ANSWERED DATE ENTERED - ARCHIVE '
      *                 NA-ARCHIVE-NO
               GO TO 0100-EXIT
           END-IF.
102810
102810     IF AT-STOP-LETTER-DT NOT EQUAL LOW-VALUES AND SPACES
102810         GO TO 0100-EXIT
102810     END-IF.
                      
           MOVE WS-CURRENT-BIN-DT   TO NA-RESEND-PRINT-DT
                                       AT-RESEND-PRINT-DATE.
                                       
           REWRITE NAPERSOFT-FILE
                INVALID KEY
                    DISPLAY ' CIDNAPEX REWRITE ERROR - ELNAPS'
                    SET END-OF-ELNAPS TO TRUE
                    GO TO ABEND-PGM.

           REWRITE ACTIVITY-TRAILERS
                INVALID KEY
                    DISPLAY ' CIDNAPEX REWRITE ERROR - ELTRLR'
                    SET END-OF-ELNAPS TO TRUE
                    GO TO ABEND-PGM.
                                       
           MOVE NA-COMPANY-CD          TO RL-COMPANY-CD
           MOVE NA-CARRIER             TO RL-CARRIER
           MOVE NA-CLAIM-NO            TO RL-CLAIM-NO
           MOVE NA-CERT-NO             TO RL-CERT-NO
           MOVE NA-RESEND-LETTER-ID    TO RL-RESEND-LETTER-ID
           MOVE NA-PROCESSOR-ID        TO RL-PROCESSOR-ID
           MOVE NA-ADDRESS-TYPE        TO RL-ADDRESS-TYPE
           MOVE NA-INITIAL-PRINT-DT    TO RL-1ST-LTR-PRINT-DT
           MOVE AT-REASON-TEXT         TO RL-REASON-TEXT
           IF NA-ORIG-ARCHIVE-NO > ZEROS
               MOVE NA-ORIG-ARCHIVE-NO TO RL-ORIG-ARCHIVE-NO
           ELSE
               MOVE NA-ARCHIVE-NO      TO RL-ORIG-ARCHIVE-NO
           END-IF
           IF NA-ORIG-ENCLOSURE-CD > SPACES
               MOVE NA-ORIG-ENCLOSURE-CD TO RL-ORIG-ENCLOSURE-CD
           ELSE
               MOVE NA-ENCLOSURE-CD    TO RL-ORIG-ENCLOSURE-CD
           END-IF

           PERFORM 0300-WRITE-MSTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELNAPS.

           READ ELNAPS NEXT RECORD

           IF (ELNAPS-FILE-STATUS = '10' OR '23')
              OR (NA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELNAPS        TO TRUE
           ELSE
              IF ELNAPS-FILE-STATUS NOT = '00'
                 DISPLAY 'ELNAPS READ NEXT ' ELNAPS-FILE-STATUS
                 SET END-OF-ELNAPS     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELNAPS
              ADD 1 TO NAPS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.


       0300-WRITE-MSTR.

           WRITE RESEND-LTR-OUT-REC FROM RESEND-LTR-RECORD
           ADD 1 TO LTR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN I-O ELNAPS ELTRLR
102510              ELMSTR
                OUTPUT RESEND-LTR-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELNAPS ELTRLR RESEND-LTR-OUT
102510           ELMSTR

           .
       0500-EXIT.
           EXIT.

       0550-START-ELNAPS.

           MOVE LOW-VALUES             TO NA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO NA-COMPANY-CD

           START ELNAPS KEY IS NOT < NA-CONTROL-PRIMARY

           IF ELNAPS-FILE-STATUS = '10' OR '23'
              SET END-OF-ELNAPS        TO TRUE
           ELSE
              IF ELNAPS-FILE-STATUS NOT = '00'
                 DISPLAY 'ELNAPS START     ' ELNAPS-FILE-STATUS
                 SET END-OF-ELNAPS     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.
           MOVE '2'                    TO  DC-OPTION-CODE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
           DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

           PERFORM 8510-DATE-CONVERSION.
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT
           END-IF

           INITIALIZE RESEND-LTR-RECORD
           PERFORM 0550-START-ELNAPS   THRU 0550-EXIT
           PERFORM 0200-READ-ELNAPS    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
