       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRERAREX.
       AUTHOR.     PABLO

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 031413    2013011700001  AJRA  NEW PROGRAM
      ******************************************************************

       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERARCH           ASSIGN TO ERARCH
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LA-CONTROL-PRIMARY
                                   FILE STATUS IS ERARCH-FILE-STATUS.

           SELECT ERARCT           ASSIGN TO ERARCT
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ERARCT-FILE-STATUS
                                   RECORD KEY IS LT-CONTROL-PRIMARY.


           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT  ARCH-OUT      ASSIGN TO ARCHOUT
                   ORGANIZATION IS LINE SEQUENTIAL. 


       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ERARCH.

                                   COPY ERCARCH.

       FD  ERARCT.
                                   COPY ERCARCT.


       FD  DISK-DATE
           COPY ELCDTEFD.

       FD  ARCH-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  ARCH-RECORD             PIC X(700).


       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERARCH             VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FIX           PIC 9(9) VALUE ZEROS.
       77  ERARCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  ERARCH-FILE-STATUS      PIC XX    VALUE ZEROS.
       77  PGM-SUB                 PIC S9(04) COMP-3 VALUE +585.
       77  SUB1                    PIC S9(04) COMP-3 VALUE +0.
       77  SUB2                    PIC S9(04) COMP-3 VALUE +0.

       01  WRK-AREA.
           05  WRK-BEG-DATE.
               10  WRK-BEG-YYYY        PIC X(4).
               10  WRK-BEG-MM          PIC X(2).
               10  WRK-BEG-DD          PIC X(2).
           05  WRK-INPUT-DATE.
               10  WRK-INP-YYYY        PIC X(4).
               10  WRK-INP-MM          PIC X(2).
               10  WRK-INP-DD          PIC X(2).
           05  WRK-MOE-DATE.
               10  WRK-MOE-YYYY        PIC X(4).
               10  WRK-MOE-MM          PIC X(2).
               10  WRK-MOE-DD          PIC X(2).
           05  WRK-ME-DATE.
               10  WRK-ME-MM           PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-ME-DD           PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-ME-YYYY         PIC X(4).
           05  WRK-RUN-DATE.
               10  WRK-RUN-MM          PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-RUN-DD          PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-RUN-YYYY        PIC X(4).
               
               
       01  EXTRACT-ARCHIVE.
           12  EX-ARCHIVE-NO           PIC  9(08).
           12  EX-TAB-1                PIC  X(1).
           12  EX-CERT-NO              PIC  X(11).
           12  EX-TAB-2                PIC  X(1).
           12  EX-CARRIER              PIC  X(01).
           12  EX-TAB-3                PIC  X(1).
           12  EX-GROUPING             PIC  X(06).
           12  EX-TAB-4                PIC  X(1).
           12  EX-STATE                PIC  X(02).
           12  EX-TAB-5                PIC  X(1).
           12  EX-ACCOUNT              PIC  X(10).
           12  EX-TAB-6                PIC  X(1).
           12  EX-EFFECT-DATE          PIC  X(10).
           12  EX-TAB-7                PIC  X(1).
           12  EX-FORM                 PIC  X(04).
           12  EX-TAB-8                PIC  X(1).
           12  EX-PROCESSOR-CD         PIC  X(04).
           12  EX-TAB-9                PIC  X(1).
           12  EX-QUE-CONTROL          PIC  9(08).
           12  EX-TAB-10               PIC  X(1).
           12  EX-NUMBER-LABEL-LINES   PIC  9(04).
           12  EX-TAB-11               PIC  X(1).
           12  EX-CREATION-DATE        PIC  X(10).
           12  EX-TAB-12               PIC  X(1).
           12  EX-FOLLOW-UP-DATE       PIC  X(10).
           12  EX-TAB-13               PIC  X(1).
           12  EX-INITIAL-PRINT-DATE   PIC  X(10).
           12  EX-TAB-14               PIC  X(1).
           12  EX-NO-OF-COPIES         PIC  9(01).
           12  EX-TAB-15               PIC  X(1).
           12  EX-NO-OF-TEXT-RECORDS   PIC  9(04).
           12  EX-TAB-16               PIC  X(1).
           12  EX-REPLY-DATE           PIC  X(10).
           12  EX-TAB-17               PIC  X(1).
           12  EX-RESEND-DATE          PIC  X(10).
           12  EX-TAB-18               PIC  X(1).
           12  EX-SENT-DATE            PIC  X(10).
           12  EX-TAB-19               PIC  X(1).
           12  EX-DATA-SOURCE          PIC  X(01).
           12  EX-TAB-20               PIC  X(1).
           12  EX-ADDR-SOURCE          PIC  X(01).
           12  EX-TAB-21               PIC  X(1).
           12  EX-STATUS               PIC  X(01).
           12  EX-TAB-22               PIC  X(1).
           12  EX-LAST-RESENT-PRINT-DATE  PIC  X(10).
           12  EX-TAB-23               PIC  X(1).
           12  EX-PRINT-RESTRICTION    PIC  X(01).
           12  EX-TAB-24               PIC  X(1).
           12  EX-PURGED-DATE          PIC  X(10).
           12  EX-TAB-25               PIC  X(1).
           12  EX-VOIDED-DATE          PIC  X(10).
           12  EX-TAB-26               PIC  X(1).
           12  EX-RESEND-LETR          PIC  X(4).
           12  EX-TAB-27               PIC  X(1).
           12  EX-ARCHIVE-STATUS       PIC  X(1).
           12  EX-TAB-28               PIC  X(1).
           12  EX-FINAL-ACT-IND        PIC  X(1).
           12  EX-TAB-29               PIC  X(1).
           12  EX-VA-DISCLOSURE-IND    PIC  X(1).
           12  EX-TAB-30               PIC  X(1).
           12  EX-ENDT-ARCH-NO         PIC  9(8).
           12  EX-TAB-30A              PIC  X(1).           
           12  EX-LAST-MAINT-DATE      PIC  X(10).
           12  EX-TAB-31               PIC  X(1).
           12  EX-LAST-MAINT-TIME      PIC  9(6).
           12  EX-TAB-32               PIC  X(1).
           12  EX-LAST-UPDATED-BY      PIC  X(4).
           12  EX-TAB-33               PIC  X(1).
           12  EX-ARCT-COMMENTS.
               16  EX-ARCT-COMMENT-LINE OCCURS 5 TIMES.
                   20  EX-ARCT-COMMENT     PIC X(69).
                   20  EX-TAB-ARCT1        PIC X(1).
                   20  EX-ARCT-CHG-DATE    PIC X(10).
                   20  EX-TAB-ARCT2        PIC X(1).
                   20  EX-ARCT-CHG-BY      PIC X(4).
                   20  EX-TAB-ARCT3        PIC X(1).
           12  EX-EOR                      PIC X(1).
           12  FILLER                      PIC X(33).
       
       01  WS-MISC.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  WS-ZERO                 PIC S9(1) VALUE +0.
           05  WS-RETURN-CODE          PIC S9(3) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS    PIC X(02) VALUE ZERO.
           05  WS-COMPANY              PIC 99  COMP.
           05  FILLER REDEFINES WS-COMPANY     COMP.
               10 FILLER               PIC 9.
               10 WS-COMPANY-CD        PIC 9.

 
                                     COPY ELCDTECX.

                                     COPY ELCDTEVR.

                                     COPY ELCDATE.
              

       PROCEDURE DIVISION.

       0000-DATE-CARD-READ. COPY ELCDTERX.
           
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0550-START-ERARCH   THRU 0550-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-ERARCH

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' ARCH RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' ARCH RECORDS WRITTEN ' TRLR-RECS-OUT
           GOBACK

           .
       0100-PROCESS-TRLR.

           PERFORM 0200-READ-TRLR THRU 0200-EXIT
           IF END-OF-ERARCH
               GO TO 0100-EXIT
           END-IF.
       
           IF NOT LA-TEMP
               PERFORM 0300-WRITE-TRLR THRU 0300-EXIT
           END-IF.


           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ ERARCH NEXT RECORD

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH READ NEXT ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF

           IF LA-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD
               SET END-OF-ERARCH  TO TRUE
           END-IF
           
           IF NOT END-OF-ERARCH
              ADD 1 TO TRLR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           PERFORM 0750-INIT-EXTRACT THRU 0750-EXIT
           MOVE LA-ARCHIVE-NO  TO EX-ARCHIVE-NO
           IF LA-CERT-NO-A2 EQUAL LOW-VALUES
               MOVE SPACES     TO EX-CERT-NO
           ELSE
              MOVE LA-CERT-NO-A2  TO EX-CERT-NO
           END-IF
           MOVE LA-CARRIER-A2  TO EX-CARRIER
           MOVE LA-GROUPING-A2 TO EX-GROUPING
           MOVE LA-STATE-A2    TO EX-STATE
           MOVE LA-ACCOUNT-A2  TO EX-ACCOUNT
           MOVE LA-EFFECT-DATE-A2 TO DC-BIN-DATE-1.
           MOVE ' '                 TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-EFFECT-DATE
           ELSE
               MOVE SPACES TO EX-EFFECT-DATE
           END-IF
           IF LA-FORM-A3 EQUAL LOW-VALUES
               MOVE SPACES     TO EX-FORM
           ELSE
               MOVE LA-FORM-A3 TO EX-FORM
           END-IF.
           MOVE LA-PROCESSOR-CD TO EX-PROCESSOR-CD
           MOVE LA-QUE-CONTROL-A6 TO EX-QUE-CONTROL
           MOVE LA-NUMBER-LABEL-LINES TO EX-NUMBER-LABEL-LINES
           MOVE LA-CREATION-DATE TO DC-BIN-DATE-1.
           MOVE ' '             TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-CREATION-DATE
           ELSE
               MOVE SPACES TO EX-CREATION-DATE
           END-IF
           MOVE LA-FOLLOW-UP-DATE  TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-FOLLOW-UP-DATE
           ELSE
               MOVE SPACES TO EX-FOLLOW-UP-DATE
           END-IF
           MOVE LA-INITIAL-PRINT-DATE TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-INITIAL-PRINT-DATE
           ELSE
               MOVE SPACES TO EX-INITIAL-PRINT-DATE
           END-IF
           MOVE LA-NO-OF-COPIES  TO EX-NO-OF-COPIES
           MOVE LA-NO-OF-TEXT-RECORDS TO EX-NO-OF-TEXT-RECORDS
           MOVE LA-REPLY-DATE    TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-REPLY-DATE
           ELSE
               MOVE SPACES TO EX-REPLY-DATE
           END-IF 
           MOVE LA-RESEND-DATE   TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-RESEND-DATE
           ELSE
               MOVE SPACES TO EX-RESEND-DATE
           END-IF 
           MOVE LA-SENT-DATE     TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-SENT-DATE
           ELSE
               MOVE SPACES TO EX-SENT-DATE
           END-IF 
           MOVE LA-DATA-SOURCE  TO EX-DATA-SOURCE
           MOVE LA-ADDR-SOURCE  TO EX-ADDR-SOURCE
           MOVE LA-STATUS       TO EX-STATUS
           MOVE LA-LAST-RESENT-PRINT-DATE TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-RESENT-PRINT-DATE
           ELSE
               MOVE SPACES TO EX-LAST-RESENT-PRINT-DATE
           END-IF 
           IF LA-PRINT-RESTRICTION = LOW-VALUES
               MOVE SPACES  TO EX-PRINT-RESTRICTION
           ELSE
               MOVE LA-PRINT-RESTRICTION TO EX-PRINT-RESTRICTION
           END-IF
           MOVE LA-PURGED-DATE   TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-PURGED-DATE
           ELSE
               MOVE SPACES TO EX-PURGED-DATE
           END-IF 
           MOVE LA-VOIDED-DATE   TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-VOIDED-DATE
           ELSE
               MOVE SPACES TO EX-VOIDED-DATE
           END-IF 
           IF LA-RESEND-LETR EQUAL LOW-VALUES
               MOVE SPACES TO EX-RESEND-LETR
           ELSE
               MOVE LA-RESEND-LETR TO EX-RESEND-LETR
           END-IF
           MOVE LA-ARCHIVE-STATUS TO EX-ARCHIVE-STATUS
           MOVE LA-FINAL-ACT-IND TO EX-FINAL-ACT-IND
           MOVE LA-VA-DISCLOSURE-IND TO EX-VA-DISCLOSURE-IND
           IF LA-ENDT-ARCH-NO-X = LOW-VALUES OR SPACES
               MOVE ZERO            TO EX-ENDT-ARCH-NO
           ELSE
               MOVE LA-ENDT-ARCH-NO TO EX-ENDT-ARCH-NO
           END-IF
           MOVE LA-LAST-MAINT-DATE TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DATE
           ELSE
               MOVE SPACES TO EX-LAST-MAINT-DATE
           END-IF
           IF LA-LAST-MAINT-TIME NOT NUMERIC
               MOVE ZEROS           TO EX-LAST-MAINT-TIME
           ELSE 
               MOVE LA-LAST-MAINT-TIME  TO EX-LAST-MAINT-TIME
           END-IF
           IF LA-LAST-UPDATED-BY EQUAL LOW-VALUES
               MOVE SPACES TO EX-LAST-UPDATED-BY
           ELSE
               MOVE LA-LAST-UPDATED-BY  TO EX-LAST-UPDATED-BY
           END-IF
           MOVE 'E' TO EX-EOR.
           
           PERFORM 0600-GET-ERARCT THRU 0600-EXIT.

           WRITE ARCH-RECORD FROM EXTRACT-ARCHIVE.
           ADD 1 TO TRLR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERARCH ERARCT
               OUTPUT ARCH-OUT

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY 'ERARCH open err  ' ERARCH-FILE-STATUS
              MOVE ' ERARCH OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERARCH ERARCT 
                 ARCH-OUT

           .

       0500-EXIT.
           EXIT.
       0550-START-ERARCH.

           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD

           START ERARCH KEY IS NOT < LA-CONTROL-PRIMARY

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH START     ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-GET-ERARCT.

           MOVE LA-COMPANY-CD TO LT-COMPANY-CD
           MOVE LA-ARCHIVE-NO TO LT-ARCHIVE-NO
           MOVE '3' TO LT-RECORD-TYPE
           MOVE +0  TO LT-LINE-SEQ-NO
           
           READ ERARCT
           
           IF ERARCT-FILE-STATUS NOT = '00'
               GO TO 0600-EXIT
           END-IF.
           
           MOVE +0 TO SUB1 SUB2.
           PERFORM VARYING SUB1 FROM +20 BY -1
               UNTIL SUB1 = +0
                 IF LT-COMMENT-LINE (SUB1) > SPACES
                     ADD +1 TO SUB2
                     MOVE LT-COMMENT-LINE (SUB1) TO 
                                       EX-ARCT-COMMENT (SUB2)
                     MOVE LT-COMMENT-CHG-DT (SUB1) TO DC-BIN-DATE-1
                     MOVE ' '               TO DC-OPTION-CODE
                     PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
                     IF NO-CONVERSION-ERROR
                        MOVE DC-GREG-DATE-A-EDIT TO 
                                       EX-ARCT-CHG-DATE (SUB2)
                     ELSE
                        MOVE SPACES TO EX-ARCT-CHG-DATE (SUB2)
                     END-IF
                     MOVE LT-COMMENT-CHG-BY (SUB1) TO 
                                       EX-ARCT-CHG-BY (SUB2)
                 END-IF
           END-PERFORM.
           
       0600-EXIT.
           EXIT.


           
       0750-INIT-EXTRACT.

           MOVE SPACES TO EXTRACT-ARCHIVE.
           MOVE '~'    TO EX-TAB-1
                          EX-TAB-2
                          EX-TAB-3
                          EX-TAB-4
                          EX-TAB-5
                          EX-TAB-6
                          EX-TAB-7
                          EX-TAB-8
                          EX-TAB-9
                          EX-TAB-10
                          EX-TAB-11
                          EX-TAB-12
                          EX-TAB-13
                          EX-TAB-14
                          EX-TAB-15
                          EX-TAB-16
                          EX-TAB-17
                          EX-TAB-18
                          EX-TAB-19
                          EX-TAB-20
                          EX-TAB-21
                          EX-TAB-22
                          EX-TAB-23
                          EX-TAB-24
                          EX-TAB-25
                          EX-TAB-26
                          EX-TAB-27
                          EX-TAB-28
                          EX-TAB-29
                          EX-TAB-30
                          EX-TAB-30A
                          EX-TAB-31
                          EX-TAB-32
                          EX-TAB-33
                          EX-TAB-ARCT1 (1)
                          EX-TAB-ARCT1 (2)
                          EX-TAB-ARCT1 (3)
                          EX-TAB-ARCT1 (4)
                          EX-TAB-ARCT1 (5)
                          EX-TAB-ARCT2 (1)
                          EX-TAB-ARCT2 (2)
                          EX-TAB-ARCT2 (3)
                          EX-TAB-ARCT2 (4)
                          EX-TAB-ARCT2 (5)
                          EX-TAB-ARCT3 (1)
                          EX-TAB-ARCT3 (2)
                          EX-TAB-ARCT3 (3)
                          EX-TAB-ARCT3 (4)
                          EX-TAB-ARCT3 (5).
                                                                            
       0750-EXIT.
           EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       ABEND-PGM.   COPY ELCABEND.
