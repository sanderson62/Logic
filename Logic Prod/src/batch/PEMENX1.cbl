       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMENX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERENDR        ASSIGN TO ERENDR
                                ORGANIZATION IS INDEXED
                                ACCESS IS DYNAMIC
                                RECORD KEY IS EN-CONTROL-PRIMARY
                                FILE STATUS IS ERENDR-FILE-STATUS.

           SELECT ENDR-OUT      ASSIGN TO ENDROUT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERENDR.

                                       COPY ERCENDR.

      /

       FD  ENDR-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

022703 01  ENDR-OUT-REC                PIC X(153).
022703 01  ENDR-HEAD-REC               PIC X(155).

082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE ' PEMENX1 WORKING STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
022703     88  END-OF-FILE                   VALUE 'Y'.
       77  ENDR-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  ENDR-RECS-OUT               PIC 9(9) VALUE ZEROS.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.

       01  ENDR-DETAIL-RECORD.
           12  ENDR-CARRIER            PIC X.
           12  ENDR-TAB1               PIC X.
           12  ENDR-GROUPING           PIC X(6).
           12  ENDR-TAB2               PIC X.
           12  ENDR-STATE              PIC XX.
           12  ENDR-TAB3               PIC X.
           12  ENDR-ACCOUNT            PIC X(10).
           12  ENDR-TAB4               PIC X.
           12  ENDR-EFF-DATE           PIC X(10).
           12  ENDR-TAB5               PIC X.
           12  ENDR-CERT-NO            PIC X(11).
           12  ENDR-TAB6               PIC X.
           12  ENDR-REC-TYPE           PIC X.
           12  ENDR-TAB7               PIC X.
           12  ENDR-LNAME              PIC X(15).
           12  ENDR-TAB8               PIC X.
           12  ENDR-FNAME              PIC X(10).
           12  ENDR-TAB9               PIC X.
           12  ENDR-INPUT-DT           PIC X(10).
           12  ENDR-TAB10              PIC X.
           12  ENDR-PRINT-DT           PIC X(10).
           12  ENDR-TAB11              PIC X.
           12  ENDR-SIG-SW             PIC X.
           12  ENDR-TAB12              PIC X.
           12  ENDR-LF-PREM-ENT        PIC -9(7).99.
           12  ENDR-TAB13              PIC X.
           12  ENDR-LF-PREM-CAL        PIC -9(7).99.
           12  ENDR-TAB14              PIC X.
           12  ENDR-AH-PREM-ENT        PIC -9(7).99.
           12  ENDR-TAB15              PIC X.
           12  ENDR-AH-PREM-CAL        PIC -9(7).99.
           12  ENDR-TAB16              PIC X.
           12  ENDR-MAINT-BY           PIC X(4).
           12  ENDR-TAB17              PIC X.
           12  ENDR-EOR                PIC X.

       01  ENDR-HEADER-RECORD.
           12  HEAD-CARRIER            PIC X(7)  VALUE 'CARRIER'.
           12  HEAD-TAB1               PIC X.
           12  HEAD-GROUPING           PIC X(5)  VALUE 'GROUP'.
           12  HEAD-TAB2               PIC X.
           12  HEAD-STATE              PIC X(5)  VALUE 'STATE'.
           12  HEAD-TAB3               PIC X.
           12  HEAD-ACCOUNT            PIC X(4)  VALUE 'ACCT'.
           12  HEAD-TAB4               PIC X.
           12  HEAD-EXP-DATE           PIC X(6)  VALUE 'EFF DT'.
           12  HEAD-TAB5               PIC X.
           12  HEAD-CERT-NO            PIC X(7)  VALUE 'CERT NO'.
           12  HEAD-TAB6               PIC X.
           12  HEAD-REC-TYPE           PIC X(7)  VALUE 'ISS-CAN'.
           12  HEAD-TAB7               PIC X.
           12  HEAD-LNAME              PIC X(9)  VALUE 'LAST NAME'.
           12  HEAD-TAB8               PIC X.
           12  HEAD-FNAME              PIC X(10) VALUE 'FIRST NAME'.
           12  HEAD-TAB9               PIC X.
           12  HEAD-INPUT-DT           PIC X(8)  VALUE 'INP DATE'.
           12  HEAD-TAB10              PIC X.
           12  HEAD-PRINT-DT           PIC X(8)  VALUE 'PRT DATE'. 
           12  HEAD-TAB11              PIC X.
           12  HEAD-SIG-SW             PIC X(6)  VALUE 'SIG SW'.
           12  HEAD-TAB12              PIC X.
           12  FILLER                  PIC X(11) VALUE 'LF PREM ENT'.
           12  HEAD-TAB13              PIC X.
           12  FILLER                  PIC X(11) VALUE 'LF PREM CAL'.
           12  HEAD-TAB14              PIC X.
           12  FILLER                  PIC X(11) VALUE 'AH PREM ENT'.
           12  HEAD-TAB15              PIC X.
           12  FILLER                  PIC X(11) VALUE 'AH PREM CAL'.
           12  HEAD-TAB16              PIC X.
           12  HEAD-MAINT-BY           PIC X(8)  VALUE 'MAINT BY'.
           12  HEAD-TAB17              PIC X.
           12  HEAD-EOR                PIC XXX   VALUE 'EOR'.    

       01  WS-MISC.
082603     05  PGM-SUB                 PIC S9(4)   VALUE +548.
082603     05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
082603     05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
082603     05  WS-ZERO                 PIC S9      VALUE ZERO.
082603     05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-SAVE-ERENDR          PIC X(153)  VALUE LOW-VALUES.
           05  ERENDR-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)   VALUE ZEROS.

                                       COPY ELCDATE.

082603                                 COPY ELCDTECX.

082603                                 COPY ELCDTEVR.

       PROCEDURE DIVISION.

082603 0000-LOAD-DATE-CARD.            COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
022703           END-OF-FILE

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ENDR RECORDS READ    '  ENDR-RECS-IN
           DISPLAY ' ENDR RECORDS WRITTEN '  ENDR-RECS-OUT
           GOBACK

           .
       0050-PROCESS-FILE.

      *    IF (EN-STATE = 'PA')
      *       IF ((EN-INPUT-DT > X'A1FF')
      *          AND (EN-INPUT-DT < X'A381'))
      *                   OR
      *          ((EN-LAST-MAINT-DT > X'A1FF')
      *          AND (EN-LAST-MAINT-DT < X'A381'))
      *          PERFORM 0100-PROCESS-ENDR THRU 0100-EXIT
      *       END-IF
      *    END-IF

          PERFORM 0100-PROCESS-ENDR THRU 0100-EXIT

           PERFORM 0200-READ-ENDR   THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ENDR.

           MOVE WS-SAVE-ERENDR         TO ENDR-DETAIL-RECORD
           MOVE EN-CARRIER             TO ENDR-CARRIER
           MOVE EN-GROUPING            TO ENDR-GROUPING
           MOVE EN-STATE               TO ENDR-STATE
           MOVE EN-ACCOUNT             TO ENDR-ACCOUNT

102902     MOVE EN-REC-TYPE            TO ENDR-REC-TYPE

           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ENDR-EFF-DATE
           END-IF

           MOVE EN-INPUT-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ENDR-INPUT-DT
           END-IF

           MOVE EN-PRINT-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ENDR-PRINT-DT
           END-IF

           MOVE EN-CERT-PRIME          TO ENDR-CERT-NO (1:10)
           MOVE EN-CERT-SFX            TO ENDR-CERT-NO (11:1)

           MOVE EN-LAST-NAME           TO ENDR-LNAME
           MOVE EN-FIRST-NAME          TO ENDR-FNAME
           MOVE EN-SIG-SW              TO ENDR-SIG-SW
           IF EN-REC-TYPE = '1'
              MOVE EN-LF-PREM-ENT-AMT  TO ENDR-LF-PREM-ENT
              MOVE EN-LF-PREM-CAL-AMT  TO ENDR-LF-PREM-CAL
              MOVE EN-AH-PREM-ENT-AMT  TO ENDR-AH-PREM-ENT
              MOVE EN-AH-PREM-CAL-AMT  TO ENDR-AH-PREM-CAL
           ELSE
              MOVE EN-LF-CANC-ENT-AMT  TO ENDR-LF-PREM-ENT
              MOVE EN-LF-CANC-CAL-AMT  TO ENDR-LF-PREM-CAL
              MOVE EN-AH-CANC-ENT-AMT  TO ENDR-AH-PREM-ENT
              MOVE EN-AH-CANC-CAL-AMT  TO ENDR-AH-PREM-CAL
           END-IF

           MOVE EN-LAST-MAINT-BY       TO ENDR-MAINT-BY
           PERFORM 0300-WRITE-ENDR     THRU 0300-EXIT

PEMTST*    IF ENDR-RECS-IN > 200
PEMTST*       SET END-OF-FILE          TO TRUE
PEMTST*    END-IF

           .
       0100-EXIT.
           EXIT.

       0200-READ-ENDR.

           READ ERENDR NEXT RECORD

           IF (ERENDR-FILE-STATUS = '10' OR '23')
082603        OR (EN-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-FILE          TO TRUE
           ELSE
              IF ERENDR-FILE-STATUS NOT = '00'
                 DISPLAY 'ERENDR READ NEXT ' ERENDR-FILE-STATUS
022703           SET END-OF-FILE       TO TRUE
              END-IF
           END-IF

022703     IF NOT END-OF-FILE
              ADD 1                    TO ENDR-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-ENDR.

           WRITE ENDR-OUT-REC       FROM ENDR-DETAIL-RECORD
           ADD 1                    TO ENDR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERENDR
               OUTPUT ENDR-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERENDR ENDR-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ENDR.

           MOVE LOW-VALUES             TO EN-CONTROL-PRIMARY
082603     MOVE DTE-CLASIC-COMPANY-CD  TO EN-COMPANY-CD

           START ERENDR KEY >= EN-CONTROL-PRIMARY

           IF (ERENDR-FILE-STATUS = '10' OR '23')
082603        OR (EN-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-FILE          TO TRUE
           ELSE
              IF ERENDR-FILE-STATUS NOT = '00'
                 DISPLAY 'ERENDR START     ' ERENDR-FILE-STATUS
022703           SET END-OF-FILE       TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ENDR-DETAIL-RECORD
           MOVE ';'                    TO ENDR-TAB1
                                          ENDR-TAB2
                                          ENDR-TAB3
                                          ENDR-TAB4
                                          ENDR-TAB5
                                          ENDR-TAB6
                                          ENDR-TAB7
                                          ENDR-TAB8
                                          ENDR-TAB9
                                          ENDR-TAB10
                                          ENDR-TAB11
                                          ENDR-TAB12
                                          ENDR-TAB13
                                          ENDR-TAB14
                                          ENDR-TAB15
                                          ENDR-TAB16
                                          ENDR-TAB17
           MOVE 'E'                    TO ENDR-EOR
           MOVE ENDR-DETAIL-RECORD     TO WS-SAVE-ERENDR

           MOVE ';'                    TO HEAD-TAB1
                                          HEAD-TAB2
                                          HEAD-TAB3
                                          HEAD-TAB4
                                          HEAD-TAB5
                                          HEAD-TAB6
                                          HEAD-TAB7
                                          HEAD-TAB8
                                          HEAD-TAB9
                                          HEAD-TAB10
                                          HEAD-TAB11
                                          HEAD-TAB12
                                          HEAD-TAB13
                                          HEAD-TAB14
                                          HEAD-TAB15
                                          HEAD-TAB16
                                          HEAD-TAB17

           WRITE ENDR-HEAD-REC         FROM ENDR-HEADER-RECORD

           PERFORM 0550-START-ENDR     THRU 0550-EXIT
           PERFORM 0200-READ-ENDR      THRU 0200-EXIT
022703     PERFORM 0200-READ-ENDR      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.
082603 ABEND-PGM SECTION.              COPY ELCABEND.

