       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALCERT.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE            ASSIGN TO SORTWK1.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  SAL-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SAL-CERT-RECORD.
           05  FILLER                  PIC X.
           05  SAL-COVERAGE            PIC X(14).
           05  SAL-ACCT-NO             PIC X(10).
           05  SAL-EFF-DT              PIC X(10).
           05  SAL-CERT-NO             PIC X(10).
           05  SAL-INS-ID-NO           PIC X(6).
           05  SAL-INS-LAST-NAME       PIC X(15).
           05  SAL-INS-FIRST-NAME      PIC X(10).
           05  SAL-INS-MID-INIT        PIC X.
           05  SAL-INS-ADDR1           PIC X(30).
           05  SAL-INS-ADDR2           PIC X(40).
           05  SAL-INS-CITY            PIC X(30).
           05  SAL-INS-STATE           PIC XX.
           05  SAL-INS-ZIP             PIC X(10).
           05  SAL-INS-BDT             PIC X(10).
           05  SAL-INS-AGE             PIC 99.
           05  SAL-INS-SEX             PIC X.
           05  SAL-INS-SSN             PIC X(11).
           05  SAL-JNT-ID-NO           PIC X(6).
           05  SAL-JNT-LAST-NAME       PIC X(15).
           05  SAL-JNT-FIRST-NAME      PIC X(10).
           05  SAL-JNT-MID-INIT        PIC X.
           05  SAL-JNT-BDT             PIC X(10).
           05  SAL-JNT-AGE             PIC 99.
           05  SAL-LF-COV-TYPE         PIC X.
           05  SAL-LF-TERM             PIC 999.
           05  SAL-LF-BEN-AMT.
               10  SAL-LF-BEN-DOL      PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-LF-BEN-PEN      PIC 99.
           05  SAL-LF-PRM-AMT.
               10  SAL-LF-PRM-DOL      PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-LF-PRM-PEN      PIC 99.
           05  SAL-LF-BEN-AMT-ALT.
               10  SAL-LF-BEN-DOL-ALT  PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-LF-BEN-PEN-ALT  PIC 99.
           05  SAL-LF-PRM-AMT-ALT.
               10  SAL-LF-PRM-DOL-ALT  PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-LF-PRM-PEN-ALT  PIC 99.
           05  SAL-LF-RFND-AMT.
               10  SAL-LF-RFND-AMT-DOL PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-LF-RFND-AMT-PEN PIC 99.
           05  SAL-LF-EXP-DT           PIC X(10).
           05  SAL-AH-COV-TYPE         PIC XX.
           05  SAL-AH-TERM             PIC 999.
           05  SAL-AH-BEN-AMT.
               10  SAL-AH-BEN-DOL      PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-AH-BEN-PEN      PIC 99.
           05  SAL-AH-PRM-AMT.
               10  SAL-AH-PRM-DOL      PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-AH-PRM-PEN      PIC 99.
           05  SAL-AH-RFND-AMT.
               10  SAL-AH-RFND-AMT-DOL PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-AH-RFND-AMT-PEN PIC 99.
           05  SAL-AH-EXP-DT           PIC X(10).
           05  SAL-AH-CRIT-PER         PIC 999.
           05  SAL-LOAN-OFF            PIC XXX.
           05  SAL-LOAN-APR.
               10  SAL-APR-WHOLE       PIC 999.
               10  FILLER              PIC X.
               10  SAL-APR-DEC         PIC 9(4).
           05  SAL-LOAN-TERM           PIC 999.
           05  SAL-IND-GRP             PIC X.
           05  SAL-FILLER1             PIC XX.
           05  SAL-MORT                PIC X(4).
           05  SAL-1ST-PMT-DT          PIC X(10).
           05  SAL-ENTRY-DT            PIC X(10).
           05  SAL-LF-PAY-OFF-DT       PIC X(10).
           05  SAL-LF-CANC-EXIT        PIC X(10).
           05  SAL-LF-CLM-EXIT         PIC X(10).
           05  SAL-AH-PAY-OFF-DT       PIC X(10).
           05  SAL-AH-CANC-EXIT        PIC X(10).
           05  SAL-AH-CLM-EXIT         PIC X(10).
           05  SAL-PMT-MODE            PIC XX.
           05  SAL-LOAN-NO             PIC X(12).
           05  SAL-COMM-TBL            PIC X.
           05  SAL-LVL1-OW             PIC X(10).
           05  SAL-LVL2-OW             PIC X(10).
           05  SAL-LVL1-OW-PCT.
               10  SAL-LVL1-OW-WHOLE   PIC 999.
               10  FILLER              PIC X.
               10  SAL-LVL1-OW-DEC     PIC 9(5).
           05  SAL-LVL2-OW-PCT.
               10  SAL-LVL2-OW-WHOLE   PIC 999.
               10  FILLER              PIC X.
               10  SAL-LVL2-OW-DEC     PIC 9(5).
           05  SAL-RESCIND             PIC X.
           05  SAL-LF-PCT.     
               10  SAL-LF-PCT-WHOLE    PIC 999.
               10  FILLER              PIC X.
               10  SAL-LF-PCT-DEC      PIC 9(5).
           05  SAL-AH-PCT.     
               10  SAL-AH-PCT-WHOLE    PIC 999.
               10  FILLER              PIC X.
               10  SAL-AH-PCT-DEC      PIC 9(5).
           05  SAL-PMT-AMT.
               10  SAL-PMT-AMT-DOL     PIC 9(8).
               10  FILLER              PIC X.
               10  SAL-PMT-AMT         PIC 99.

           EJECT
       SD  SORT-FILE. 

       01  SORT-RECORD.
           05  FILLER                  PIC XX.
           05  SORT-KEY.
               10  FILLER              PIC X(36).
               10  SORT-CERT-SFX       PIC X.
           05  FILLER                  PIC X(1017).

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(1056).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALCERT  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  BUSNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-INIT-CERT                PIC X(1056).
       77  WS-SAVE-CERT                PIC X(1056).

                                       COPY ECSCRT01.

       01  WS-MISC.
           05  WS-N-SEX-CNT            PIC 9(7)  VALUE ZEROS.
           
           05  WS-HOLD-LFAMT-ALT       PIC S9(9)V99 VALUE +0.
           05  WS-HOLD-LFPRM-ALT       PIC S9(9)V99 VALUE +0.
           05  WS-BALLOON-SPLIT-SW     PIC X     VALUE SPACES.
               88  SPLIT-BALLOONS        VALUE 'Y'.
           05  WS-FULL-CONTROL.
               10  FILLER              PIC X(36) VALUE LOW-VALUES.
               10  WS-FULL-SFX         PIC X     VALUE LOW-VALUES.
           05  WS-SFX-TABLE            PIC X(36)   VALUE
               '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  FILLER REDEFINES WS-SFX-TABLE.
               10  WS-CERT-SFX OCCURS 36
                                       PIC X.
           05  SUBSFX                  PIC S999  COMP-3 VALUE +0.
           05  WS-LF-COV1              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV2              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV3              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV4              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV5              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV6              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COV7              PIC 9(7)  VALUE ZEROS.
           05  WS-LF-COVO              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV1              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV2              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV3              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV4              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV5              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV6              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV7              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV8              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV9              PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV10             PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV11             PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV12             PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COV13             PIC 9(7)  VALUE ZEROS.
           05  WS-AH-COVO              PIC 9(7)  VALUE ZEROS.
           05  WS-WORK-DAYS            PIC 999   VALUE ZEROS.
           05  WS-WORK-DAYS-X REDEFINES WS-WORK-DAYS.
               10  FILLER              PIC X.
               10  WS-WORK-DAYS-C      PIC XX.
           05  WS-WORK-AMT.
               10  WS-AMT1             PIC 9(8).
               10  FILLER              PIC X.
               10  WS-AMT2             PIC 99.
           05  WS-WORK-AMT-X           PIC X(10).
           05  WS-WORK-AMT-N REDEFINES WS-WORK-AMT-X
                                       PIC 9(8)V99.
           05  WS-WORK-APR-X           PIC X(7).
           05  WS-WORK-APR-N REDEFINES WS-WORK-APR-X
                                       PIC 9(3)V9(4).
           05  WS-WORK-COMM-X          PIC X(8).
           05  WS-WORK-COMM-N REDEFINES WS-WORK-COMM-X
                                       PIC 9(3)V9(5).
           05  WS-BIN-EFF-DT           PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-1ST-PMT-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-ENTRY-DT         PIC XX   VALUE LOW-VALUES.
           05  WS-WORK-DATE            PIC X(8).
           05  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                       PIC 9(8).
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.

           EJECT

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
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
           05  PARM-VALUE  DISPLAY PICTURE IS X(100).                   
                                                                        
       PROCEDURE DIVISION USING PARM.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           SORT SORT-FILE ON ASCENDING KEY SORT-KEY
                INPUT PROCEDURE 0002-INPUT
                                       THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT
                                       THRU 0003-EXIT

           GOBACK
             .

       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           MOVE ' '                    TO WS-EOF-SW

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           PERFORM 0075-PROCESS-RECS THRU 0075-EXIT UNTIL
                END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.


       0010-INITIALIZE.

           MOVE SPACES                 TO CERTIFICATE-RECORD
           INITIALIZE                     CERTIFICATE-RECORD
           MOVE 'CR'                   TO CR-RECORD-ID
           MOVE X'04'                  TO CR-COMPANY-CD
           MOVE '9'                    TO CR-CARRIER
           MOVE '000000'               TO CR-GROUPING
           MOVE 'MN'                   TO CR-STATE
           MOVE ZEROS                  TO CR-AGE
                                          CR-LF-DEV-CODE
                                          CR-AH-DEV-CODE
                                          CR-RATING-CLASS
           MOVE '2'                    TO CR-IND-GRP
           MOVE 'L140'                 TO CR-MORT
           MOVE '4'                    TO CR-ENTRY-STATUS

           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE +0                  TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
              MOVE ZEROS               TO CR-COM-AGT (SUB1)
              MOVE ' '                 TO CR-AGT-TYPE (SUB1)
              IF SUB1 < +6
                 MOVE +0               TO CR-INCUR-DISAMT (SUB1)
                                          CR-INCUR-DISEXP (SUB1)
              END-IF
           END-PERFORM

           MOVE CERTIFICATE-RECORD     TO WS-INIT-CERT

           IF PARM-LENGTH > +0
              IF PARM-VALUE = 'SPLIT'
                 SET SPLIT-BALLOONS    TO TRUE
                 DISPLAY '** **************************** **'
                 DISPLAY '** THIS RUN WILL SPLIT BALLOONS **'
                 DISPLAY '** **************************** **'
              ELSE
                 DISPLAY '** ******************************** **'
                 DISPLAY '** THIS RUN WILL NOT SPLIT BALLOONS **'
                 DISPLAY '** ******************************** **'
              END-IF
           END-IF

           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SAL-FILE-IN 
               OUTPUT CID-FILE-OUT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' SALA RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' CERT RECORDS OUTPUT   ' WS-OUTPUT-CNT
      *    DISPLAY ' NOTE RECORDS OUTPUT   ' WS-NOTE-OUTPUT-CNT

           DISPLAY ' LIFE COV 1            ' WS-LF-COV1
           DISPLAY ' LIFE COV 2            ' WS-LF-COV2
           DISPLAY ' LIFE COV 3            ' WS-LF-COV3
           DISPLAY ' LIFE COV 4            ' WS-LF-COV4
           DISPLAY ' LIFE COV 5            ' WS-LF-COV5
           DISPLAY ' LIFE COV 6            ' WS-LF-COV6
           DISPLAY ' LIFE COV 7            ' WS-LF-COV7
           DISPLAY ' LIFE COV O            ' WS-LF-COVO

           DISPLAY '  AH  COV 1            ' WS-AH-COV1
           DISPLAY '  AH  COV 2            ' WS-AH-COV2
           DISPLAY '  AH  COV 3            ' WS-AH-COV3
           DISPLAY '  AH  COV 4            ' WS-AH-COV4
           DISPLAY '  AH  COV 5            ' WS-AH-COV5
           DISPLAY '  AH  COV 6            ' WS-AH-COV6
           DISPLAY '  AH  COV 7            ' WS-AH-COV7
           DISPLAY '  AH  COV 8            ' WS-AH-COV8
           DISPLAY '  AH  COV 9            ' WS-AH-COV9
           DISPLAY '  AH  COV 10           ' WS-AH-COV10
           DISPLAY '  AH  COV 11           ' WS-AH-COV11
           DISPLAY '  AH  COV 12           ' WS-AH-COV12
           DISPLAY '  AH  COV 13           ' WS-AH-COV13
           DISPLAY '  AH  COV O            ' WS-AH-COVO
           DISPLAY '   N SEX CODES NOW M   ' WS-N-SEX-CNT

           CLOSE SAL-FILE-IN CID-FILE-OUT

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           PERFORM 0080-PROCESS-INPUT  THRU 0080-EXIT UNTIL
                 END-OF-INPUT

           .

       0050-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ SAL-FILE-IN AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .

       0060-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-FILE     AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           .

       0070-EXIT.
           EXIT.

       0075-PROCESS-RECS.

           IF SORT-KEY > WS-FULL-CONTROL
              CONTINUE
           ELSE
              DISPLAY ' FOUND DUP ' SORT-KEY
              PERFORM VARYING SUBSFX FROM +1 BY +1 UNTIL
                 (SORT-KEY > WS-FULL-CONTROL)
                 OR (SUBSFX > +36)
                 MOVE WS-CERT-SFX (SUBSFX)
                                       TO SORT-CERT-SFX
              END-PERFORM
              IF SUBSFX > +36
                 DISPLAY ' FOUND TOO MANY DUPS FOR ABOVE CERT '
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           MOVE SORT-KEY               TO WS-FULL-CONTROL
           MOVE SORT-RECORD            TO CID-RECORD-OUT

           PERFORM 0077-BUILD-ERNOTE   THRU 0077-EXIT
           WRITE CID-RECORD-OUT
           ADD 1                       TO WS-OUTPUT-CNT

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.

       0077-BUILD-ERNOTE.
       
      *    MOVE WS-INIT-NOTE           TO NOTE-FILE
      *    
      *    MOVE CR-CONTROL-A           TO NT-CERT-NOTE-KEY
      *    WRITE CID-NOTE-RECORD-OUT
      *    ADD 1                       TO WS-NOTE-OUTPUT-CNT
           
           .
       0077-EXIT.
           EXIT.

       0080-PROCESS-INPUT.


           IF SAL-CERT-NO = '0556634   ' OR '0571640   '
              OR '0000000000'
              DISPLAY ' BYPASSING CERT # ' SAL-CERT-NO
              PERFORM 0060-READ-INPUT  THRU 0060-EXIT
              GO TO 0080-EXIT
           END-IF

           IF SAL-CERT-NO = '0581010   '
              MOVE '08/31/1996'        TO SAL-LF-CANC-EXIT
                                          SAL-AH-CANC-EXIT
           END-IF

           IF SAL-CERT-NO = '0599256   '
              MOVE '07/31/1967'        TO SAL-LF-CANC-EXIT
                                          SAL-AH-CANC-EXIT
           END-IF

           IF SAL-CERT-NO = '0109489   '
              MOVE '09/10/1963'        TO SAL-LF-PAY-OFF-DT
                                          SAL-AH-PAY-OFF-DT
           END-IF
           
           MOVE WS-INIT-CERT           TO CERTIFICATE-RECORD
           
           MOVE '00010'                TO CR-ACCOUNT (1:5)
           MOVE SAL-ACCT-NO (1:3)      TO CR-ACCOUNT (6:3)
           MOVE '00'                   TO CR-ACCOUNT (9:2)

           PERFORM UNTIL SAL-CERT-NO (10:1) NOT = ' '
              MOVE SAL-CERT-NO (1:9)   TO SAL-CERT-NO (2:9)
              MOVE '0'                 TO SAL-CERT-NO (1:1)
           END-PERFORM
           MOVE SAL-CERT-NO            TO CR-CERT

           IF SAL-RESCIND = '1' OR '2'
              MOVE 'D'                 TO CR-ENTRY-STATUS
           END-IF
           MOVE SAL-EFF-DT (7:4)       TO WS-WORK-DATE (1:4)
           MOVE SAL-EFF-DT (1:2)       TO WS-WORK-DATE (5:2)
           MOVE SAL-EFF-DT (4:2)       TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE WS-WORK-DATE-N      TO CR-DT
              MOVE DC-BIN-DATE-1       TO WS-BIN-EFF-DT  
           ELSE
              DISPLAY ' EFF DATE ERROR ON CERT ' CR-CERT
                   '  ' SAL-EFF-DT
              DISPLAY ' BYPASSING CERT # EFF ' SAL-CERT-NO
              PERFORM 0060-READ-INPUT  THRU 0060-EXIT
              GO TO 0080-EXIT
           END-IF
           
           MOVE SAL-INS-LAST-NAME      TO CR-LNAME
           MOVE SAL-INS-FIRST-NAME     TO CR-FNAME
           MOVE SAL-INS-MID-INIT       TO CR-INIT

           MOVE SAL-INS-AGE            TO CR-AGE
           IF CR-AGE = ZEROS
              MOVE 42                  TO CR-AGE
      *       DISPLAY ' USING DEFAULT AGE, 42 FOR CERT ' SAL-CERT-NO
           END-IF

           IF SAL-INS-SEX = 'N'
              MOVE 'M'                 TO SAL-INS-SEX
              ADD 1                    TO WS-N-SEX-CNT
           END-IF
           MOVE SAL-INS-SEX            TO CR-SEX
           IF CR-SEX NOT = 'F' AND 'M'
              MOVE 'M'                 TO CR-SEX
      *       DISPLAY ' USING DEFAULT SEX, M  FOR CERT ' SAL-CERT-NO
           END-IF
           
           MOVE SAL-INS-SSN            TO CR-SOC-SEC
           MOVE SAL-JNT-LAST-NAME      TO CR-JT-LNAME
           MOVE SAL-JNT-FIRST-NAME     TO CR-JT-FNAME
           MOVE SAL-JNT-MID-INIT       TO CR-JT-INIT
           MOVE SAL-JNT-AGE            TO CR-JOINT-AGE
           MOVE '00'                   TO CR-LFTYP

           IF SAL-LF-COV-TYPE = ' '
              MOVE '1'                 TO SAL-LF-COV-TYPE
           END-IF
           IF SAL-LF-COV-TYPE = '1'
              MOVE '51'                TO CR-LFTYP
              ADD 1                    TO WS-LF-COV1
           ELSE
              IF SAL-LF-COV-TYPE = '2'
                 MOVE '2G'             TO CR-LFTYP
                 ADD 1                 TO WS-LF-COV2
              ELSE
                 IF SAL-LF-COV-TYPE = '3'
                    MOVE '59'          TO CR-LFTYP
                    ADD 1              TO WS-LF-COV3
                 ELSE
                    IF SAL-LF-COV-TYPE = '4'
                       MOVE '52'       TO CR-LFTYP
                       ADD 1           TO WS-LF-COV4
                    ELSE
                       IF SAL-LF-COV-TYPE = '5'
                          MOVE '2H'    TO CR-LFTYP
                          ADD 1        TO WS-LF-COV5
                       ELSE
                          IF SAL-LF-COV-TYPE = '6'
                             MOVE '61' TO CR-LFTYP
                             ADD 1     TO WS-LF-COV6
                          ELSE
                             IF SAL-LF-COV-TYPE = '7'
                                ADD 1  TO WS-LF-COV7
                             ELSE
                                ADD 1  TO WS-LF-COVO
                                DISPLAY ' LF COV TYPE ' SAL-LF-COV-TYPE
                                  '   ' SAL-CERT-NO
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
                 
           MOVE '4'                    TO CR-LF-CURRENT-STATUS
           MOVE SAL-LF-TERM            TO CR-LF-TERM
           
           MOVE SAL-LF-BEN-AMT         TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-LFAMT
           MOVE SAL-LF-PRM-AMT         TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-LFPRM
                                          CR-LFPRM-CALC
           
           IF CR-LFPRM > ZEROS
              MOVE +0.02               TO CR-LF-ISS-PREM-TAX 
           END-IF
           MOVE SAL-LF-BEN-AMT-ALT     TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-LFAMT-ALT
           MOVE SAL-LF-PRM-AMT-ALT     TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-LFPRM-ALT
                                          CR-LFPRM-CALC-ALT

           COMPUTE CR-LF-NSP-PRM = CR-LFPRM + CR-LFPRM-ALT
           
           IF CR-LFTYP = '59' OR '61'
              IF (CR-LFAMT-ALT = ZEROS)
                 OR (CR-LFPRM-ALT = ZEROS)
                 DISPLAY ' ALT ZERO FOR BALLOONS ' CR-ACCOUNT '  '
                    CR-CERT-NO
              END-IF
           END-IF

           MOVE SAL-LF-RFND-AMT        TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-LFRFND
                                          CR-LFRFND-CALC
           IF CR-LFRFND > ZEROS
              MOVE +0.02               TO CR-LF-CNC-PREM-TAX
           END-IF

           IF SAL-LF-PAY-OFF-DT NOT = SPACES
              MOVE SAL-LF-PAY-OFF-DT (7:4) TO WS-WORK-DATE (1:4)
              MOVE SAL-LF-PAY-OFF-DT (1:2) TO WS-WORK-DATE (5:2)
              MOVE SAL-LF-PAY-OFF-DT (4:2) TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-WORK-DATE-N   TO CR-LF-CANC-DT
                 MOVE DC-BIN-DATE-1    TO WS-BIN-LF-CANC-DT
              ELSE
                 DISPLAY ' LF CANC DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-LF-PAY-OFF-DT
              END-IF
           END-IF
           
           IF SAL-LF-EXP-DT NOT = SPACES
              PERFORM UNTIL SAL-LF-EXP-DT (10:1) NOT = ' '
                 MOVE SAL-LF-EXP-DT (1:9) TO SAL-LF-EXP-DT (2:9)
                 MOVE '0'                 TO SAL-LF-EXP-DT (1:1)
              END-PERFORM
              MOVE SAL-LF-EXP-DT (7:4) TO WS-WORK-DATE (1:4)
              MOVE SAL-LF-EXP-DT (1:2) TO WS-WORK-DATE (5:2)
              MOVE SAL-LF-EXP-DT (4:2) TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-WORK-DATE-N   TO CR-LF-EXPIRE-DATE
                 MOVE DC-BIN-DATE-1    TO WS-BIN-LF-EXP-DT
              ELSE
      *          DISPLAY ' LF EXP DATE ERROR ON CERT ' CR-CERT
      *               '  ' SAL-LF-EXP-DT
                 MOVE WS-BIN-EFF-DT    TO DC-BIN-DATE-1
                 MOVE CR-LF-TERM       TO DC-ELAPSED-MONTHS
                 MOVE '6'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-2 TO WS-BIN-LF-EXP-DT
                    MOVE DC-GREG-DATE-CYMD
                                       TO CR-LF-EXPIRE-DATE
                 ELSE
                    DISPLAY ' LF EXP DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-LF-EXP-DT
                 END-IF
              END-IF
           END-IF
           
           MOVE '00'                   TO CR-AHTYP
           IF SAL-AH-COV-TYPE = '  '
              MOVE '01'                TO SAL-AH-COV-TYPE
           END-IF

           EVALUATE SAL-AH-COV-TYPE
             WHEN '01'
                MOVE '03'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV1
             WHEN '02'
                MOVE '53'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV2
             WHEN '03'
                MOVE '01'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV3
             WHEN '04'
                MOVE '54'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV4
             WHEN '05'
                MOVE '05'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV5
             WHEN '06'
                MOVE '56'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV6
             WHEN '07'
                MOVE '04'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV7
             WHEN '08'
                MOVE '55'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV8
             WHEN '09'
                MOVE '02'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV9
             WHEN '10'
                MOVE '57'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV10
             WHEN '11'
                MOVE '49'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COV11
                MOVE 24                TO CR-AH-CRIT-PERIOD
             WHEN '12'
                ADD 1                  TO WS-AH-COV12
                MOVE '1E'              TO CR-AHTYP
                MOVE 24                TO CR-AH-CRIT-PERIOD
             WHEN '13'
                ADD 1                  TO WS-AH-COV13
                MOVE '00'              TO CR-AHTYP
             WHEN OTHER
                MOVE '00'              TO CR-AHTYP
                ADD 1                  TO WS-AH-COVO
                DISPLAY ' AH COV TYPE ' SAL-AH-COV-TYPE
                   '   ' SAL-CERT-NO
           END-EVALUATE
                 
           MOVE '4'                    TO CR-AH-CURRENT-STATUS
           MOVE SAL-AH-TERM            TO CR-AH-TERM
           MOVE SAL-AH-BEN-AMT         TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-AHAMT
           MOVE SAL-AH-PRM-AMT         TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-AHPRM
                                          CR-AHPRM-CALC
           IF (CR-AH-TERM = ZEROS)
              AND (CR-AHPRM = ZEROS)
              AND (CR-AHAMT = ZEROS)
              MOVE '00'               TO CR-AHTYP
           END-IF

           IF CR-AHPRM > ZEROS
              MOVE +0.02               TO CR-AH-ISS-PREM-TAX
           END-IF
           MOVE SAL-AH-RFND-AMT        TO WS-WORK-AMT
           PERFORM 0085-CONVERT-AMT    THRU 0085-EXIT
           MOVE WS-WORK-AMT-N          TO CR-AHRFND
                                          CR-AHRFND-CALC
           IF CR-AHRFND > ZEROS
              MOVE +0.02               TO CR-AH-CNC-PREM-TAX
           END-IF

           IF SAL-AH-PAY-OFF-DT NOT = SPACES
              MOVE SAL-AH-PAY-OFF-DT (7:4) TO WS-WORK-DATE (1:4)
              MOVE SAL-AH-PAY-OFF-DT (1:2) TO WS-WORK-DATE (5:2)
              MOVE SAL-AH-PAY-OFF-DT (4:2) TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-WORK-DATE-N   TO CR-AH-CANC-DT
                 MOVE DC-BIN-DATE-1    TO WS-BIN-AH-CANC-DT
              ELSE
                 DISPLAY ' AH CANC DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-AH-PAY-OFF-DT
              END-IF
           END-IF
           
           IF SAL-AH-EXP-DT NOT = SPACES
              PERFORM UNTIL SAL-AH-EXP-DT (10:1) NOT = ' '
                 MOVE SAL-AH-EXP-DT (1:9) TO SAL-AH-EXP-DT (2:9)
                 MOVE '0'                 TO SAL-AH-EXP-DT (1:1)
              END-PERFORM
              MOVE SAL-AH-EXP-DT (7:4) TO WS-WORK-DATE (1:4)
              MOVE SAL-AH-EXP-DT (1:2) TO WS-WORK-DATE (5:2)
              MOVE SAL-AH-EXP-DT (4:2) TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-WORK-DATE-N   TO CR-AH-EXPIRE-DATE
                 MOVE DC-BIN-DATE-1    TO WS-BIN-AH-EXP-DT
              ELSE
      *          DISPLAY ' AH EXP DATE ERROR ON CERT ' CR-CERT
      *             '  ' SAL-AH-EXP-DT
                 MOVE WS-BIN-EFF-DT    TO DC-BIN-DATE-1
                 MOVE CR-AH-TERM       TO DC-ELAPSED-MONTHS
                 MOVE '6'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-2 TO WS-BIN-AH-EXP-DT
                    MOVE DC-GREG-DATE-CYMD
                                       TO CR-AH-EXPIRE-DATE
                 ELSE
                    DISPLAY ' AH EXP DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-AH-EXP-DT
                 END-IF
              END-IF
           END-IF
           
           MOVE SAL-AH-CRIT-PER        TO CR-AH-CRIT-PERIOD
           MOVE SAL-LOAN-OFF           TO CR-LOAN-OFFICER
           MOVE SAL-LOAN-TERM          TO CR-LOAN-TERM
           MOVE '2'                    TO CR-IND-GRP

           IF SAL-1ST-PMT-DT NOT = SPACES
              MOVE SAL-1ST-PMT-DT (9:2)
                                       TO CR-1ST-PMT-YR
              MOVE SAL-1ST-PMT-DT (1:2)
                                       TO CR-1ST-PMT-MO
              MOVE SAL-1ST-PMT-DT (4:2)
                                       TO CR-1ST-PMT-DA
           
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD       
              MOVE '3'                 TO  DC-OPTION-CODE           
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT

              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-BIN-1ST-PMT-DT
              ELSE
                 DISPLAY ' 1ST PMT DT  ERROR ON CERT ' CR-CERT
                    '  ' SAL-1ST-PMT-DT
                 MOVE ZEROS            TO CR-LOAN-1ST-PMT-DT
              END-IF
           END-IF
           
           IF SAL-ENTRY-DT NOT = SPACES
              MOVE SAL-ENTRY-DT (7:4)  TO WS-WORK-DATE (1:4)
              MOVE SAL-ENTRY-DT (1:2)  TO WS-WORK-DATE (5:2)
              MOVE SAL-ENTRY-DT (4:2)  TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-WORK-DATE-N   TO CR-ENTRY-DATE
                 MOVE DC-BIN-DATE-1    TO WS-BIN-ENTRY-DT
              ELSE
                 DISPLAY ' ENTRY  DATE ERROR ON CERT ' CR-CERT
                    '  ' SAL-ENTRY-DT
              END-IF
           END-IF
           
           IF SAL-LF-CANC-EXIT NOT = SPACES
              MOVE SAL-LF-CANC-EXIT (7:4)
                                       TO WS-WORK-DATE (1:4)
              MOVE SAL-LF-CANC-EXIT (1:2)
                                       TO WS-WORK-DATE (5:2)
              MOVE SAL-LF-CANC-EXIT (4:2)
                                       TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-DAYS-IN-MONTH TO WS-WORK-DAYS
                 MOVE WS-WORK-DAYS-C   TO WS-WORK-DATE (7:2)
                 MOVE WS-WORK-DATE-N   TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE WS-WORK-DATE-N
                                       TO CR-LF-CANCEL-EXIT-DATE
                    MOVE DC-BIN-DATE-1 TO WS-BIN-LF-CANC-EXIT-DT
                 ELSE
                    DISPLAY ' LF CANC EXIT DATE ERROR ON CERT '
                      CR-CERT '  ' SAL-LF-CANC-EXIT
                 END-IF
              ELSE
                 DISPLAY ' LF CANC EXIT DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-LF-CANC-EXIT
              END-IF
           END-IF

           IF SAL-LF-CLM-EXIT NOT = SPACES
              MOVE SAL-LF-CLM-EXIT (7:4)
                                       TO WS-WORK-DATE (1:4)
              MOVE SAL-LF-CLM-EXIT (1:2)
                                       TO WS-WORK-DATE (5:2)
              MOVE SAL-LF-CLM-EXIT (4:2)
                                       TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              MOVE '1'                 TO DC-END-OF-MONTH
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-DAYS-IN-MONTH TO WS-WORK-DAYS
                 MOVE WS-WORK-DAYS-C   TO WS-WORK-DATE (7:2)
                 MOVE WS-WORK-DATE-N   TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE WS-WORK-DATE-N TO CR-LF-CLAIM-EXIT-DATE
                    MOVE DC-BIN-DATE-1  TO WS-BIN-LF-CLM-EXIT-DT
                 ELSE
                    DISPLAY ' LF CLM  EXIT DATE ERROR ON CERT '
                       CR-CERT  '  ' SAL-LF-CLM-EXIT
                 END-IF
              ELSE
                 DISPLAY ' LF CLM  EXIT DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-LF-CLM-EXIT 
              END-IF
           END-IF


           IF SAL-AH-CANC-EXIT NOT = SPACES
              MOVE SAL-AH-CANC-EXIT (7:4)
                                       TO WS-WORK-DATE (1:4)
              MOVE SAL-AH-CANC-EXIT (1:2)
                                       TO WS-WORK-DATE (5:2)
              MOVE SAL-AH-CANC-EXIT (4:2)
                                       TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-DAYS-IN-MONTH TO WS-WORK-DAYS
                 MOVE WS-WORK-DAYS-C   TO WS-WORK-DATE (7:2)
                 MOVE WS-WORK-DATE-N   TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE WS-WORK-DATE-N
                                       TO CR-AH-CANCEL-EXIT-DATE
                    MOVE DC-BIN-DATE-1 TO WS-BIN-AH-CANC-EXIT-DT
                 ELSE
                    DISPLAY ' AH CANC EXIT DATE ERROR ON CERT '
                      CR-CERT '  ' SAL-AH-CANC-EXIT
                 END-IF
              ELSE
                 DISPLAY ' AH CANC EXIT DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-AH-CANC-EXIT
              END-IF
           END-IF

           IF SAL-AH-CLM-EXIT NOT = SPACES
              MOVE SAL-AH-CLM-EXIT (7:4)
                                       TO WS-WORK-DATE (1:4)
              MOVE SAL-AH-CLM-EXIT (1:2)
                                       TO WS-WORK-DATE (5:2)
              MOVE SAL-AH-CLM-EXIT (4:2)
                                       TO WS-WORK-DATE (7:2)
              MOVE WS-WORK-DATE-N      TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-DAYS-IN-MONTH TO WS-WORK-DAYS
                 MOVE WS-WORK-DAYS-C   TO WS-WORK-DATE (7:2)
                 MOVE WS-WORK-DATE-N   TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE WS-WORK-DATE-N
                                       TO CR-AH-SETTLEMENT-EXIT-DATE
                    MOVE DC-BIN-DATE-1 TO WS-BIN-AH-CLM-EXIT-DT
                 ELSE
                    DISPLAY ' AH CLM  EXIT DATE ERROR ON CERT '
                      CR-CERT '  ' SAL-AH-CLM-EXIT
                 END-IF
              ELSE
                 DISPLAY ' AH CLM  EXIT DATE ERROR ON CERT ' CR-CERT
                      '  ' SAL-AH-CLM-EXIT
              END-IF
           END-IF

           STRING SAL-APR-WHOLE SAL-APR-DEC DELIMITED BY SIZE INTO
              WS-WORK-APR-X
           END-STRING
           MOVE WS-WORK-APR-N          TO CR-APR
           MOVE +1                     TO SUB1
           MOVE CR-ACCOUNT             TO CR-COM-AGT (SUB1)
           MOVE 'D'                    TO CR-AGT-TYPE (SUB1)

           STRING SAL-LF-PCT-WHOLE SAL-LF-PCT-DEC
              DELIMITED BY SIZE INTO WS-WORK-COMM-X
           END-STRING
           MOVE WS-WORK-COMM-N         TO CR-LCOM-L (SUB1)

           STRING SAL-AH-PCT-WHOLE SAL-AH-PCT-DEC
              DELIMITED BY SIZE INTO WS-WORK-COMM-X
           END-STRING
           MOVE WS-WORK-COMM-N         TO CR-LCOM-AH (SUB1)

           MOVE ' '                    TO SAL-COMM-TBL
           EVALUATE SAL-COMM-TBL
              WHEN 'A'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4750        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3250        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'B'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4750        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'C'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4700        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'D'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4700        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3300        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'E'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4700        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'F'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'G'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'H'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4500        TO CR-LCOM-L (SUB1)
                    MOVE +.4300        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'I'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4400        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'J'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4300        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'K'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4250        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.2500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'L'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4200        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'M'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4000        TO CR-LCOM-L (SUB1)
                    MOVE +.4500        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'N'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4000        TO CR-LCOM-L (SUB1)
                    MOVE +.4300        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'O'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'P'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.4000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.2500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'Q'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.3500        TO CR-LCOM-L (SUB1)
                    MOVE +.4500        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'R'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.3500        TO CR-LCOM-L (SUB1)
                    MOVE +.4000        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'S'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.3500        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
              WHEN 'T'
                 IF (CR-LF-TERM < 61)
                    OR (CR-AH-TERM < 61)
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                    MOVE +.4000        TO CR-LCOM-AH (SUB1)
                 ELSE
                    MOVE +.3000        TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
                 END-IF
           END-EVALUATE


      *    MOVE +.45000                TO CR-LCOM-L (SUB1)
      *                                   CR-LCOM-AH (SUB1)
      *    IF SAL-ACCT-NO (1:3) = '630' OR '536' OR '373' OR '490'
      *       MOVE +.40000             TO CR-LCOM-L (SUB1)
      *                                   CR-LCOM-AH (SUB1)
      *    END-IF
           
           IF SAL-LVL1-OW NOT = SPACES
              ADD +1                   TO SUB1
              MOVE '00010'             TO CR-COM-AGT (SUB1) (1:5)
              MOVE SAL-LVL1-OW (1:3)   TO CR-COM-AGT (SUB1) (6:3)
              MOVE '00'                TO CR-COM-AGT (SUB1) (9:2)
              MOVE 'P'                 TO CR-AGT-TYPE (SUB1)
              STRING SAL-LVL1-OW-WHOLE SAL-LVL1-OW-DEC
                 DELIMITED BY SIZE INTO WS-WORK-COMM-X
              END-STRING
              MOVE WS-WORK-COMM-N      TO CR-LCOM-L (SUB1)
      *       MOVE +.03000             TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
              IF SAL-CERT-NO = '630' OR '536'
                 MOVE +.05000          TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
              END-IF
           END-IF

           IF SAL-LVL2-OW NOT = SPACES
              ADD +1                   TO SUB1
              MOVE '00010'             TO CR-COM-AGT (SUB1) (1:5)
              MOVE SAL-LVL2-OW         TO CR-COM-AGT (SUB1) (6:3)
              MOVE '00'                TO CR-COM-AGT (SUB1) (9:2)
              MOVE 'P'                 TO CR-AGT-TYPE (SUB1)
              STRING SAL-LVL2-OW-WHOLE SAL-LVL2-OW-DEC
                 DELIMITED BY SIZE INTO WS-WORK-COMM-X
              END-STRING
              MOVE WS-WORK-COMM-N      TO CR-LCOM-L (SUB1)
      *       MOVE +.03000             TO CR-LCOM-L (SUB1)
                                          CR-LCOM-AH (SUB1)
           END-IF

           ADD +1                      TO SUB1
           MOVE '0000968600'           TO CR-COM-AGT  (SUB1)
           MOVE 'P'                    TO CR-AGT-TYPE (SUB1)
           MOVE +0                     TO CR-LCOM-L   (SUB1)
                                          CR-LCOM-AH  (SUB1)

           IF (CR-LF-CANCEL-EXIT-DATE = ZEROS)
            AND (CR-LF-CLAIM-EXIT-DATE = ZEROS)
            CONTINUE
           ELSE
            IF (CR-LF-CANCEL-EXIT-DATE NOT = ZEROS)
             AND (CR-LF-CLAIM-EXIT-DATE = ZEROS)
             MOVE '8'                  TO CR-LF-CURRENT-STATUS
             MOVE '4'                  TO CR-LF-STATUS-AT-CANCEL
            ELSE
             IF (CR-LF-CANCEL-EXIT-DATE = ZEROS)
              AND (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
              MOVE '7'                 TO CR-LF-CURRENT-STATUS
              MOVE '4'                 TO CR-LF-STATUS-AT-DEATH
             ELSE
      *       DISPLAY ' CERT LF CANC AND DEATH ' SAL-CERT-NO
      *          '  ' CR-LF-CANCEL-EXIT-DATE '  ' CR-LF-CLAIM-EXIT-DATE
              IF (CR-LF-CANCEL-EXIT-DATE < CR-LF-CLAIM-EXIT-DATE)
               MOVE '7'                TO CR-LF-CURRENT-STATUS
               MOVE '8'                TO CR-LF-STATUS-AT-DEATH
               MOVE '4'                TO CR-LF-STATUS-AT-CANCEL
              ELSE
               MOVE '8'                TO CR-LF-CURRENT-STATUS
               MOVE '7'                TO CR-LF-STATUS-AT-CANCEL
               MOVE '4'                TO CR-LF-STATUS-AT-DEATH
              END-IF
             END-IF
            END-IF
           END-IF

           IF CR-AH-CANCEL-EXIT-DATE = ZEROS
              CONTINUE
           ELSE
              MOVE '8'                 TO CR-AH-CURRENT-STATUS
              MOVE '4'                 TO CR-AH-STATUS-AT-CANCEL
           END-IF

           MOVE CERTIFICATE-RECORD     TO WS-SAVE-CERT

           IF (SPLIT-BALLOONS)
              AND (CR-LFTYP = '59' OR '61')
              PERFORM 0090-SPLIT-BALLOONS
                                       THRU 0090-EXIT
           ELSE
              RELEASE SORT-RECORD      FROM CERTIFICATE-RECORD
           END-IF

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0085-CONVERT-AMT.
       
           INSPECT WS-WORK-AMT REPLACING ALL '-' BY ZERO
           
           STRING WS-AMT1 WS-AMT2 DELIMITED BY SIZE INTO
              WS-WORK-AMT-X
           END-STRING

           .
       0085-EXIT.
           EXIT.

       0090-SPLIT-BALLOONS.
       
           MOVE CR-LFAMT-ALT           TO WS-HOLD-LFAMT-ALT
           MOVE CR-LFPRM-ALT           TO WS-HOLD-LFPRM-ALT
           MOVE +0                     TO CR-LFAMT-ALT
                                          CR-LFPRM-ALT
                                          CR-LFPRM-CALC-ALT
      *    MOVE '51'                   TO CR-LFTYP
           MOVE CR-LFPRM               TO CR-LF-NSP-PRM
           IF CR-LFTYP = '59'
              MOVE '51'                TO CR-LFTYP
           ELSE
              MOVE '52'                TO CR-LFTYP
           END-IF
           RELEASE SORT-RECORD         FROM CERTIFICATE-RECORD
           MOVE 'B'                    TO CR-CERT-SFX
           MOVE WS-HOLD-LFAMT-ALT      TO CR-LFAMT
           MOVE WS-HOLD-LFPRM-ALT      TO CR-LFPRM
                                          CR-LF-NSP-PRM
      *    MOVE '2G'                   TO CR-LFTYP
           MOVE '00'                   TO CR-AHTYP
           MOVE ZEROS                  TO CR-AHPRM
                                          CR-AHRFND
                                          CR-AHAMT
                                          CR-AH-TERM
                                          CR-AHPRM-CALC
                                          CR-AHPRM-RATE
                                          CR-AHRFND
                                          CR-AHRFND-CALC
                                          CR-AH-NSP-PRM
                                          CR-AH-EXPIRE-DATE
                                          CR-AH-CANC-DT
                                          CR-AH-CANCEL-EXIT-DATE
           MOVE SPACES                 TO CR-AH-STATUS-AT-CANCEL
                                          CR-AH-CURRENT-STATUS
           IF CR-LFTYP = '51'
              MOVE '2G'                TO CR-LFTYP
           ELSE
              MOVE '2H'                TO CR-LFTYP
           END-IF

           RELEASE SORT-RECORD         FROM CERTIFICATE-RECORD

           .
       0090-EXIT.
           EXIT.       	

       0110-READ-INPUT.

           READ SAL-FILE-IN AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .
       0110-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
