       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDFIXMA.
       AUTHOR.     AJRA.
       DATE-COMPILED.
      *****************************************************************
      *  THIS PROGRAM WILL REMOVE THE POST CARD INFO IF THE MAIL DATE
      *  IS EQUAL TO 04/11/11  (x'A6EB')
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121511   2011121300002   AJRA  NEW PROGRAM.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MA-CONTROL-PRIMARY
                                   FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTR-OUT         ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERMAIL.

           COPY ERCMAIL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-REC                PIC X(183).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDMAX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-DIS-DATE                 PIC 9(8)  VALUE ZEROS.

           01  WS-MISC.
           05  WS-WORK-DT              PIC 9(8)  VALUE ZEROS.
           05  WS-WORK-DTR REDEFINES WS-WORK-DT.
               10  WS-WORK-DT-CCYY      PIC X(4).
               10  WS-WORK-DT-MM        PIC XX.
               10  WS-WORK-DT-DD        PIC XX.
           05  PGM-SUB                 PIC S999 COMP  VALUE +158.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERMAIL                  VALUE 'Y'.
           05  ERMAIL-RECS-IN          PIC 9(9)   VALUE ZEROS.
           05  EXTR-RECS-OUT           PIC 9(9)   VALUE ZEROS.
           05  S1                      PIC S999   VALUE +0 COMP-3.

072403     05  ERMAIL-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.

030404**** PROGRAM ABEND FIELDS
030404     05  WS-RETURN-CODE          PIC S9(03) VALUE +0.
030404     05  WS-ZERO                 PIC S9(01) VALUE +0.
030404     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
030404     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.


       01  WS-SAVE-EXTR                PIC X(183) VALUE LOW-VALUES.
       01  EXTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-INS-FULL-NAME        PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-MAIL-TYPE1           PIC X(1).
           12  EX-TAB8                 PIC X.
           12  EX-MAIL-STATUS1         PIC X(1).
           12  EX-TAB9                 PIC X.
           12  EX-MAIL-DATE1           PIC X(10).
           12  EX-TAB10                PIC X.
           12  EX-MAIL-TYPE2           PIC X(1).
           12  EX-TAB11                PIC X.
           12  EX-MAIL-STATUS2         PIC X(1).
           12  EX-TAB12                PIC X.
           12  EX-MAIL-DATE2           PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-MAIL-TYPE3           PIC X(1).
           12  EX-TAB14                PIC X.
           12  EX-MAIL-STATUS3         PIC X(1).
           12  EX-TAB15                PIC X.
           12  EX-MAIL-DATE3           PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-MAIL-TYPE4           PIC X(1).
           12  EX-TAB17                PIC X.
           12  EX-MAIL-STATUS4         PIC X(1).
           12  EX-TAB18                PIC X.
           12  EX-MAIL-DATE4           PIC X(10).
           12  EX-TAB19                PIC X.
           12  EX-MAIL-TYPE5           PIC X(1).
           12  EX-TAB20                PIC X.
           12  EX-MAIL-STATUS5         PIC X(1).
           12  EX-TAB21                PIC X.
           12  EX-MAIL-DATE5           PIC X(10).
           12  EX-TAB22                PIC X.
           12  EX-MAIL-TYPE6           PIC X(1).
           12  EX-TAB23                PIC X.
           12  EX-MAIL-STATUS6         PIC X(1).
           12  EX-TAB24                PIC X.
           12  EX-MAIL-DATE6           PIC X(10).
           12  EX-TAB25                PIC X.
           12  EX-MAIL-TYPE7           PIC X(1).
           12  EX-TAB26                PIC X.
           12  EX-MAIL-STATUS7         PIC X(1).
           12  EX-TAB27                PIC X.
           12  EX-MAIL-DATE7           PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-EOR                  PIC X.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

030404 PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ERMAIL THRU 0050-EXIT UNTIL
              (END-OF-ERMAIL)
PEMTST*       OR (ERMAIL-RECS-IN > 10000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' MAIL RECORDS READ    '  ERMAIL-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXTR-RECS-OUT

           GOBACK

           .
       0050-PROCESS-ERMAIL.

           PERFORM 0100-PROCESS-ERMAIL
                                       THRU 0100-EXIT

           PERFORM 0200-READ-ERMAIL    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ERMAIL.

           IF MA-MAIL-DATE (1) = x'A6EB'  OR
              MA-MAIL-DATE (2) = x'A6EB'  OR
              MA-MAIL-DATE (3) = x'A6EB'  OR
              MA-MAIL-DATE (4) = x'A6EB'  OR
              MA-MAIL-DATE (5) = x'A6EB'  OR
              MA-MAIL-DATE (6) = x'A6EB'  OR
              MA-MAIL-DATE (7) = x'A6EB'  
                CONTINUE
           ELSE
               GO TO 0100-EXIT
           END-IF
           
           PERFORM 0150-UPDATE-ERMAIL THRU 0150-EXIT
           
           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD
           MOVE MA-CARRIER             TO EX-CARRIER
           MOVE MA-GROUPING            TO EX-GROUPING
           MOVE MA-STATE               TO EX-STATE
           MOVE MA-ACCOUNT             TO EX-ACCOUNT
           MOVE MA-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE MA-CERT-NO             TO EX-CERT-NO

           IF MA-INSURED-MIDDLE-INIT NOT = SPACES
              STRING MA-INSURED-FIRST-NAME ' ' MA-INSURED-MIDDLE-INIT
                 ' ' MA-INSURED-LAST-NAME DELIMITED BY '  '
                 INTO EX-INS-FULL-NAME
              END-STRING
           ELSE
              STRING MA-INSURED-FIRST-NAME ' ' MA-INSURED-LAST-NAME
                 DELIMITED BY '  ' INTO EX-INS-FULL-NAME
              END-STRING
           END-IF
           
           IF MA-MAIL-TYPE (1) > SPACES
               MOVE MA-MAIL-TYPE (1) TO EX-MAIL-TYPE1
               MOVE MA-MAIL-STATUS (1) TO EX-MAIL-STATUS1
               MOVE MA-MAIL-DATE (1) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE1
               END-IF
           END-IF

           IF MA-MAIL-TYPE (2) > SPACES
               MOVE MA-MAIL-TYPE (2) TO EX-MAIL-TYPE2
               MOVE MA-MAIL-STATUS (2) TO EX-MAIL-STATUS2
               MOVE MA-MAIL-DATE (2) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE2
               END-IF
           END-IF

           IF MA-MAIL-TYPE (3) > SPACES
               MOVE MA-MAIL-TYPE (3) TO EX-MAIL-TYPE3
               MOVE MA-MAIL-STATUS (3) TO EX-MAIL-STATUS3
               MOVE MA-MAIL-DATE (3) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE3
               END-IF
           END-IF

           IF MA-MAIL-TYPE (4) > SPACES
               MOVE MA-MAIL-TYPE (4) TO EX-MAIL-TYPE4
               MOVE MA-MAIL-STATUS (4) TO EX-MAIL-STATUS4
               MOVE MA-MAIL-DATE (4) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE4
               END-IF
           END-IF

           IF MA-MAIL-TYPE (5) > SPACES
               MOVE MA-MAIL-TYPE (5) TO EX-MAIL-TYPE5
               MOVE MA-MAIL-STATUS (5) TO EX-MAIL-STATUS5
               MOVE MA-MAIL-DATE (5) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE5
               END-IF
           END-IF

           IF MA-MAIL-TYPE (6) > SPACES
               MOVE MA-MAIL-TYPE (6) TO EX-MAIL-TYPE6
               MOVE MA-MAIL-STATUS (6) TO EX-MAIL-STATUS6
               MOVE MA-MAIL-DATE (6) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE6
               END-IF
           END-IF

           IF MA-MAIL-TYPE (7) > SPACES
               MOVE MA-MAIL-TYPE (7) TO EX-MAIL-TYPE7
               MOVE MA-MAIL-STATUS (7) TO EX-MAIL-STATUS7
               MOVE MA-MAIL-DATE (7) TO DC-BIN-DATE-1
               MOVE ' '              TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
               IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-A-EDIT TO EX-MAIL-DATE7
               END-IF
           END-IF


           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0150-UPDATE-ERMAIL.
       
           IF MA-MAIL-DATE (1) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (1)
               MOVE SPACES TO MA-MAIL-STATUS (1)
               MOVE LOW-VALUES TO MA-MAIL-DATE (1)
           END-IF
           
           IF MA-MAIL-DATE (2) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (2)
               MOVE SPACES TO MA-MAIL-STATUS (2)
               MOVE LOW-VALUES TO MA-MAIL-DATE (2)
           END-IF
           
           IF MA-MAIL-DATE (3) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (3)
               MOVE SPACES TO MA-MAIL-STATUS (3)
               MOVE LOW-VALUES TO MA-MAIL-DATE (3)
           END-IF
           
           IF MA-MAIL-DATE (4) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (4)
               MOVE SPACES TO MA-MAIL-STATUS (4)
               MOVE LOW-VALUES TO MA-MAIL-DATE (4)
           END-IF
           
           IF MA-MAIL-DATE (5) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (5)
               MOVE SPACES TO MA-MAIL-STATUS (5)
               MOVE LOW-VALUES TO MA-MAIL-DATE (5)
           END-IF
           
           IF MA-MAIL-DATE (6) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (6)
               MOVE SPACES TO MA-MAIL-STATUS (6)
               MOVE LOW-VALUES TO MA-MAIL-DATE (6)
           END-IF
           
           IF MA-MAIL-DATE (7) = x'A6EB'
               MOVE SPACES TO MA-MAIL-TYPE (7)
               MOVE SPACES TO MA-MAIL-STATUS (7)
               MOVE LOW-VALUES TO MA-MAIL-DATE (7)
           END-IF
       
           REWRITE MAILING-DATA    
           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY 'ERMAIL REWRITE ERROR FOR CERT ' MA-CERT-NO
              MOVE ' ERMAIL REWRITE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERMAIL-FILE-STATUS TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
       
           .
       0150-EXIT.
           EXIT.


       0200-READ-ERMAIL.

           READ ERMAIL NEXT RECORD

           IF (ERMAIL-FILE-STATUS = '10' OR '23')
              OR (MA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERMAIL        TO TRUE
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERMAIL - READ NEXT '
                    ERMAIL-FILE-STATUS
                 SET END-OF-ERMAIL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ERMAIL
              ADD 1                    TO ERMAIL-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           INSPECT EXTR-DETAIL-RECORD
              REPLACING ALL ';' BY SPACES
           INSPECT EXTR-DETAIL-RECORD
              REPLACING ALL X'09' BY ';'           

           WRITE EXTR-OUT-REC          FROM EXTR-DETAIL-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

072403     OPEN I-O ERMAIL
               OUTPUT EXTR-OUT

           IF ERMAIL-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERMAIL - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-OUT ERMAIL

           IF ERMAIL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERMAIL - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.

       0550-START-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY

030404     MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD

           START ERMAIL KEY >= MA-CONTROL-PRIMARY

           IF ERMAIL-FILE-STATUS = '10' OR '23'
              SET END-OF-ERMAIL        TO TRUE
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERMAIL - START ' ERMAIL-FILE-STATUS
                 SET END-OF-ERMAIL     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
           MOVE 'E'                    TO EX-EOR

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           PERFORM 0550-START-ERMAIL   THRU 0550-EXIT
           PERFORM 0200-READ-ERMAIL    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

030404 ABEND-PGM. COPY ELCABEND.
