       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL548TX.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.
      
080806******************************************************************
080806*                   C H A N G E   L O G
080806*
080806* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080806*-----------------------------------------------------------------
080806*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080806* EFFECTIVE    NUMBER
080806*-----------------------------------------------------------------
080806* 080806                   PEMA  NEW PROGRAM
080806******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT        ASSIGN TO ERACCT
                                ORGANIZATION IS INDEXED
                                ACCESS IS DYNAMIC
                                RECORD KEY IS AM-CONTROL-PRIMARY
                                FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERCOMP        ASSIGN TO ERCOMP
                                ORGANIZATION IS INDEXED
                                ACCESS IS DYNAMIC
                                RECORD KEY IS CO-CONTROL-PRIMARY
                                FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT ACCT-OUT      ASSIGN TO ACCTOUT
               ORGANIZATION IS LINE SEQUENTIAL.

082603     SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  ERCOMP.

           COPY ERCCOMP.

       FD  ACCT-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

022703 01  ACCT-OUT-REC                PIC X(400).
022703 01  ACCT-HEAD-REC               PIC X(200).

082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   EL548TX WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
022703     88  END-OF-ERACCT             VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  ACT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  ACT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  ACCT-DETAIL-RECORD.
           12  ACCT-CARRIER            PIC X.
           12  ACCT-TAB1               PIC X.
           12  ACCT-STATE              PIC XX.
           12  ACCT-TAB2               PIC X.
           12  ACCT-ACCOUNT            PIC X(10).
           12  ACCT-TAB3               PIC X.
           12  ACCT-NAME               PIC X(30).
           12  ACCT-TAB4               PIC X.
           12  ACCT-EFF-DATE           PIC X(10).
           12  ACCT-TAB5               PIC X.
           12  ACCT-EXP-DATE           PIC X(10).
           12  ACCT-TAB6               PIC X.
           12  ACCT-GA-NO              PIC X(10).
           12  ACCT-TAB6A              PIC X.
           12  ACCT-PRIM-CONT          PIC X(30).
           12  ACCT-TAB6B              PIC X.
           12  ACCT-CONTR-NAME         PIC X(30).
           12  ACCT-TAB6C              PIC X.
           12  ACCT-ACCT-NAME          PIC X(30).
           12  ACCT-TAB6D              PIC X.
           12  ACCT-GA-ADDR1           PIC X(30).
           12  ACCT-TAB6E              PIC X.
           12  ACCT-GA-ADDR2           PIC X(30).
           12  ACCT-TAB7               PIC X.
           12  ACCT-GA-CITY-STATE      PIC X(30).
           12  ACCT-TAB7A              PIC X.
           12  ACCT-GA-ZIP             PIC X(11).
           12  ACCT-TAB8               PIC X.
           12  ACCT-SL-COM             PIC -.99999.
           12  ACCT-TAB9               PIC X.
           12  ACCT-JL-COM             PIC -.99999.
           12  ACCT-TAB10              PIC X.
           12  ACCT-SA-COM             PIC -.99999.
           12  ACCT-TAB11              PIC X.
           12  ACCT-JA-COM             PIC -.99999.
           12  ACCT-TAB12              PIC X.
           12  ACCT-COMMENT4           PIC X(50).
           12  ACCT-TAB13              PIC X.
           12  ACCT-COMMENT5           PIC X(50).
           12  ACCT-TAB14              PIC X.
           12  ACCT-EOR                PIC X.
       01  ACCT-HEADER-RECORD.
           12  HEAD-CARRIER            PIC X(7)  VALUE 'CARRIER'.
           12  HEAD-TAB1               PIC X.
           12  HEAD-STATE              PIC X(5)  VALUE 'STATE'.
           12  HEAD-TAB2               PIC X.
           12  HEAD-ACCOUNT            PIC X(4)  VALUE 'ACCT'.
           12  HEAD-TAB3               PIC X.
           12  HEAD-NAME               PIC X(9)  VALUE 'ACCT NAME'.
           12  HEAD-TAB4               PIC X.
           12  HEAD-EFF-DATE           PIC X(10) VALUE 'EFF DT'.
           12  HEAD-TAB5               PIC X.
           12  HEAD-EXP-DATE           PIC X(6)  VALUE 'EXP DT'.
           12  HEAD-TAB6               PIC X.
           12  HEAD-GA-NO              PIC X(5)  VALUE 'GA NO'.
           12  HEAD-TAB6A              PIC X.
           12  FILLER                  PIC X(9)  VALUE 'PRIM CONT'.
           12  HEAD-TAB6B              PIC X.
           12  FILLER                  PIC X(10) VALUE 'CONTR NAME'.
           12  HEAD-TAB6C              PIC X.
           12  FILLER                  PIC X(12) VALUE 'GA ACCT NAME'.
           12  HEAD-TAB6D              PIC X.
           12  FILLER                  PIC X(5)  VALUE 'ADDR1'.    
           12  HEAD-TAB6E              PIC X.
           12  FILLER                  PIC X(5)  VALUE 'ADDR2'.
           12  HEAD-TAB7               PIC X.
           12  HEAD-GA-CITY-STATE      PIC X(10) VALUE 'GA CITY ST'.
           12  HEAD-TAB7A              PIC X.
           12  FILLER                  PIC X(6)  VALUE 'GA ZIP'.
           12  HEAD-TAB8               PIC X.
           12  HEAD-SL-COM             PIC X(8)  VALUE 'SL COM1 '.
           12  HEAD-TAB9               PIC X.
           12  HEAD-JL-COM             PIC X(8)  VALUE 'JL COM1 '.
           12  HEAD-TAB10              PIC X.
           12  HEAD-SA-COM             PIC X(8)  VALUE 'SA COM1 '.
           12  HEAD-TAB11              PIC X.
           12  HEAD-JA-COM             PIC X(8)  VALUE 'JA COM1 '.
           12  HEAD-TAB12              PIC X.
           12  HEAD-COMMENT4           PIC X(9)  VALUE 'COMMENT 4'.
           12  HEAD-TAB13              PIC X.
           12  HEAD-COMMENT5           PIC X(9)  VALUE 'COMMENT 5'.
           12  HEAD-TAB14              PIC X.
           12  HEAD-EOR                PIC XXX   VALUE 'EOR'.

       01  WS-MISC.
082603     05  PGM-SUB                 PIC S9(4)   VALUE +548.
082603     05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
082603     05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
082603     05  WS-ZERO                 PIC S9      VALUE ZERO.
082603     05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-SAVE-ERACCT          PIC X(400)  VALUE LOW-VALUES.
           05  ERACCT-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  ERCOMP-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)   VALUE ZEROS.

                                       COPY ELCDATE.

082603                                 COPY ELCDTECX.
082603
082603                                 COPY ELCDTEVR.
082603

           EJECT
       PROCEDURE DIVISION.

082603 0000-LOAD-DATE-CARD.            COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ACCT   THRU 0100-EXIT UNTIL
022703         END-OF-ERACCT 

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ACCT RECORDS READ    '  ACT-RECS-IN
           DISPLAY ' ACCT RECORDS WRITTEN '  ACT-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ACCT.

           MOVE WS-SAVE-ERACCT         TO ACCT-DETAIL-RECORD
           MOVE AM-CARRIER             TO ACCT-CARRIER
           MOVE AM-STATE               TO ACCT-STATE
           MOVE AM-ACCOUNT             TO ACCT-ACCOUNT

102902     IF AM-EXPIRATION-DT = HIGH-VALUES
102902        MOVE '99/99/9999'        TO ACCT-EXP-DATE
102902     ELSE
102902        MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
102902        MOVE ' '                 TO DC-OPTION-CODE
102902        PERFORM 8510-DATE-CONVERSION
102902                                 THRU 8590-EXIT
102902        IF NO-CONVERSION-ERROR
102902           MOVE DC-GREG-DATE-A-EDIT
102902                                 TO ACCT-EXP-DATE
102902        END-IF
102902     END-IF

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-EFF-DATE
           END-IF

           MOVE AM-NAME                TO ACCT-NAME
           MOVE AM-COMMENT-LINE (4)    TO ACCT-COMMENT4
           MOVE AM-COMMENT-LINE (5)    TO ACCT-COMMENT5

           PERFORM VARYING SUB1 FROM +2 BY +1 UNTIL
              SUB1 > +8
101305        IF (AM-AGT (SUB1) NOT = SPACES AND ZEROS)
                 AND (AM-COM-TYP (SUB1) = 'P')
053003           IF (AM-L-COM (SUB1) NUMERIC)
053003              AND (AM-L-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                    MOVE AM-L-COM (SUB1)
                                       TO ACCT-SL-COM
                 END-IF
053003           IF (AM-J-COM (SUB1) NUMERIC)
053003              AND (AM-J-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                    MOVE AM-J-COM (SUB1)
                                       TO ACCT-JL-COM
                 END-IF
053003           IF (AM-A-COM (SUB1) NUMERIC)
053003              AND (AM-A-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                    MOVE AM-A-COM (SUB1)
                                       TO ACCT-SA-COM
                                          ACCT-JA-COM
                 END-IF
                 PERFORM 0150-READ-ERCOMP
                                       THRU 0150-EXIT
              END-IF
           END-PERFORM

           PERFORM 0200-READ-ACCT      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0150-READ-ERCOMP.

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE ZEROS                  TO CO-GROUPING
           MOVE AM-AGT (SUB1)          TO CO-RESP-NO
           MOVE LOW-VALUES             TO CO-ACCOUNT
           MOVE 'G'                    TO CO-TYPE
           READ ERCOMP
           IF ERCOMP-FILE-STATUS = '10' OR '23'
              DISPLAY ' ERCOMP NOT FOUND ' CO-CONTROL-PRIMARY (2:17)
           ELSE
              IF ERCOMP-FILE-STATUS = '00'
                 MOVE CO-RESP-NO       TO ACCT-GA-NO
                 MOVE CO-CONTROL-NAME  TO ACCT-PRIM-CONT
                 MOVE CO-MAIL-NAME     TO ACCT-CONTR-NAME
                 MOVE CO-ACCT-NAME     TO ACCT-ACCT-NAME
                 MOVE CO-ADDR-1        TO ACCT-GA-ADDR1
                 MOVE CO-ADDR-2        TO ACCT-GA-ADDR2
                 MOVE CO-ADDR-3        TO ACCT-GA-CITY-STATE
                 MOVE CO-ZIP           TO ACCT-GA-ZIP
                 PERFORM 0300-WRITE-ACCT
                                       THRU 0300-EXIT
              ELSE
                 DISPLAY ' BAD READ ERCOMP ' ERCOMP-FILE-STATUS '  '
                   CO-CONTROL-PRIMARY
              END-IF
           END-IF

           .
       0150-EXIT.
           EXIT.

       0200-READ-ACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
082603        OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-ERACCT        TO TRUE
052804        MOVE SPACES              TO AM-CONTROL-PRIMARY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT READ NEXT ' ERACCT-FILE-STATUS
022703           SET END-OF-ERACCT     TO TRUE
              END-IF
           END-IF

022703     IF NOT END-OF-ERACCT
              ADD 1                    TO ACT-RECS-IN
              IF (AM-REPORT-CODE-1 NOT = 'JM&A')
      *          OR (AM-COMMENT-LINE (4) (1:2) NOT = 'W2')
                 GO TO 0200-READ-ACCT
              END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-ACCT.

           WRITE ACCT-OUT-REC          FROM ACCT-DETAIL-RECORD
           ADD 1                       TO ACT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERACCT ERCOMP
               OUTPUT ACCT-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERACCT ACCT-OUT ERCOMP

           .
       0500-EXIT.
           EXIT.

       0550-START-ACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
082603     MOVE DTE-CLASIC-COMPANY-CD  TO AM-CONTROL-PRIMARY

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY

           IF (ERACCT-FILE-STATUS = '10' OR '23')
082603        OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT START     ' ERACCT-FILE-STATUS
022703           SET END-OF-ERACCT     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ACCT-DETAIL-RECORD
           MOVE ';'                    TO ACCT-TAB1
                                          ACCT-TAB2
                                          ACCT-TAB3
                                          ACCT-TAB4
                                          ACCT-TAB5
                                          ACCT-TAB6
                                          ACCT-TAB6A
                                          ACCT-TAB6B
                                          ACCT-TAB6C
                                          ACCT-TAB6D
                                          ACCT-TAB6E
                                          ACCT-TAB7
                                          ACCT-TAB7A
                                          ACCT-TAB8
                                          ACCT-TAB9
                                          ACCT-TAB10
                                          ACCT-TAB11
                                          ACCT-TAB12
                                          ACCT-TAB13
                                          ACCT-TAB14
           MOVE ZEROS                  TO ACCT-SL-COM
                                          ACCT-JL-COM
                                          ACCT-SA-COM
                                          ACCT-JA-COM
           MOVE ACCT-DETAIL-RECORD     TO WS-SAVE-ERACCT

           MOVE ';'                    TO HEAD-TAB1
                                          HEAD-TAB2
                                          HEAD-TAB3
                                          HEAD-TAB4
                                          HEAD-TAB5
                                          HEAD-TAB6
                                          HEAD-TAB6A
                                          HEAD-TAB6B
                                          HEAD-TAB6C
                                          HEAD-TAB6D
                                          HEAD-TAB6E
                                          HEAD-TAB7
                                          HEAD-TAB7A
                                          HEAD-TAB8
                                          HEAD-TAB9
                                          HEAD-TAB10
                                          HEAD-TAB11
                                          HEAD-TAB12
                                          HEAD-TAB13
                                          HEAD-TAB14

           WRITE ACCT-HEAD-REC         FROM ACCT-HEADER-RECORD

           PERFORM 0550-START-ACCT     THRU 0550-EXIT
           PERFORM 0200-READ-ACCT      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.
082603 ABEND-PGM SECTION.              COPY ELCABEND.

