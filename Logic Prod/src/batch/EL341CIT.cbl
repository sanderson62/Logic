       IDENTIFICATION DIVISION.                                         04/18/98
       PROGRAM-ID.    EL341CI.
      *AUTHOR.     PABLO.
      *            OMAHA, NEBRASKA.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************EL341

      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL341
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *EL341
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.

      *            *****************************************************EL341

      *REMARKS.                                                         EL341
      *    POST CLAIM INTEREST PAYMENTS TO CLAS-IC ONLINE FILE

       ENVIRONMENT DIVISION.                                            EL341
       INPUT-OUTPUT SECTION.                                            EL341
       FILE-CONTROL.                                                    EL341
                                                                        EL341
           SELECT  FILEIN      ASSIGN TO SYS010.
           SELECT  PRNTR       ASSIGN TO SYS008.
           SELECT  DISK-DATE   ASSIGN TO SYS019.
           SELECT  FICH        ASSIGN TO SYS020.

           SELECT  ELTRLR      ASSIGN TO ELTRLR
                               ORGANIZATION IS INDEXED
                               ACCESS IS DYNAMIC
                               RECORD KEY IS AT-CONTROL-PRIMARY
                               FILE STATUS IS ELTRLR-FILE-STATUS.

       DATA DIVISION.                                                   EL341
       FILE SECTION.                                                    EL341

       FD  FILEIN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CLAIM-INT-RECORD.
           05  CI-CARRIER              PIC X.
           05  CI-CLAIM-NO             PIC X(7).
           05  CI-CERT-NO              PIC X(11).
           05  CI-CHECK-NO             PIC X(10).
           05  CI-CHECK-DATE           PIC 9(8).
           05  CI-CHECK-AMOUNT         PIC 9(6)V99.

                                                                        EL341
           EJECT                                                        EL341
       FD  ELTRLR.                                                      EL341
                                                                        EL341
                                   COPY ELCTRLR.                        EL341
           EJECT                                                        EL341
       FD  DISK-DATE COPY ELCDTEFD.                                     EL341
                                                                        EL341
           EJECT                                                        EL341
       FD  PRNTR COPY ELCPRTFD.                                         EL341
                                                                        EL341
       FD  FICH                    COPY ELCFCHFD.                       EL341

       WORKING-STORAGE SECTION.                                         EL341
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL341
       77  FILLER  PIC X(32) VALUE '********************************'.  EL341
       77  FILLER  PIC X(32) VALUE '*   EL341CI WORKING STORAGE    *'.  EL341
       77  FILLER  PIC X(32) VALUE '*****VMOD=2.001*****************'.  EL341
                                                                        EL341
       77  ELM-CTR     PIC 9(6)    VALUE 0.                             EL341
       77  WS-BIN-CHECK-DT             PIC XX VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X.
           88  END-OF-INPUT                  VALUE 'Y'.
       01  DTE-INTERFACE-CODES.                                         EL341
           12  X                       PIC X           VALUE SPACE.     EL341
           12  PGM-SUB                 PIC S9(4)  COMP VALUE +341.      EL341
           12  ABEND-CODE              PIC 9999        VALUE ZERO.      EL341
           12  ABEND-OPTION            PIC X           VALUE SPACE.     EL341
           12  OLC-REPORT-NAME         PIC X(6)        VALUE 'EL341'.   EL341
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACE.     EL341
           12  WS-ABEND-FILE-STATUS    PIC X(4)        VALUE SPACE.     EL341
           12  WS-RETURN-CODE          PIC S9(4)  COMP VALUE ZERO.      EL341
           12  WS-ZERO                 PIC S9   COMP-3 VALUE ZERO.      EL341
                                                                        EL341
       01  ELTRLR-FILE-STATUS          PIC XX  VALUE '00'.              EL341
                                                                        EL341
       01  ERROR-NF-COUNT                  PIC 9999   VALUE ZERO.       EL341
                                                                        EL341
       01  SAVE-POST-DATE                  PIC XX.                      EL341
                                                                        EL341
       01  WS-INITIALS.                                                 EL341
           05  WS-INITIALS1                   PIC X      VALUE SPACE.   EL341
           05  WS-INITIALS2                   PIC X      VALUE SPACE.   EL341
                                                                        EL341
       01  WS-AT-CONTROL-PRIMARY.                                       EL341
               16  WS-AT-COMPANY-CD           PIC X.                    EL341
               16  WS-AT-CARRIER              PIC X.                    EL341
               16  WS-AT-CLAIM-NO             PIC X(7).                 EL341
               16  WS-AT-CERT-NO.                                       EL341
                   20  WS-AT-CERT-PRIME       PIC X(10).                EL341
                   20  WS-AT-CERT-SFX         PIC X.                    EL341
               16  WS-AT-SEQUENCE-NO          PIC S9(4)   COMP VALUE +1.EL341
                                                                        EL341
           EJECT                                                        EL341
       01  HD1.                                                         EL341
           12  FILLER      PIC X(48)   VALUE SPACES.                    EL341
           12  HD1-CARR    PIC X(28)   VALUE                            EL341
            'CLAIMS POSTED FOR CARRIER - '.                             EL341
           12  HD-CARR     PIC X.                                       EL341
           12  FILLER      PIC X(42)   VALUE SPACES.                    EL341
           12  FILLER      PIC X(8)    VALUE 'EL341CI '.                EL341
                                                                        EL341
       01  HD2.                                                         EL341
           12  FILLER      PIC X(47)   VALUE SPACES.                    EL341
           12  HD-CO       PIC X(30).                                   EL341
           12  FILLER      PIC X(42)   VALUE SPACES.                    EL341
           12  HD-RD       PIC X(8).                                    EL341
                                                                        EL341
       01  HD3.                                                         EL341
           12  FILLER      PIC X(53)   VALUE SPACES.                    EL341
           12  HD-DT       PIC X(18).                                   EL341
           12  FILLER      PIC X(48)   VALUE SPACES.                    EL341
           12  FILLER      PIC X(5)    VALUE 'PAGE '.                   EL341
           12  HD-PG       PIC ZZ,ZZ9.                                  EL341
                                                                        EL341
       01  HD4.                                                         EL341
           12  HD4-1   PIC X(24)  VALUE 'CARR CLAIM NO CERT NO   '.
           12  FILLER  PIC X(10)  VALUE 'PAID DATE '.
           12  FILLER  PIC X(21)  VALUE ' CHECK NO     AMOUNT '.
           12  FILLER  PIC X(25)  VALUE '     PAYEE               '.

       01  DTL.
           12  FILLER      PIC X.                                       EL341
           12  P-CARRIER   PIC X.
           12  FILLER      PIC X.                                       EL341
           12  P-CLM-NO    PIC X(7).                                    EL341
           12  FILLER      PIC X.                                       EL341
           12  P-CERT-NO   PIC X(11).                                   EL341
           12  FILLER      PIC X.
           12  P-PAY-DATE  PIC X(10).
           12  FILLER      PIC X.                                       EL341
           12  P-CHECK-NO  PIC X(10).                                   EL341
           12  FILLER      PIC X.                                       EL341
           12  P-AMT       PIC ZZZZ,ZZZ.99-   BLANK WHEN ZERO.          EL341
           12  FILLER      PIC X.                                       EL341
           12  P-NAME      PIC X(30).                                   EL341
           12  FILLER      PIC X.                                       EL341

       01  DTL-RDF.                                                     EL341
           12  FILLER      PIC X(90).                                   EL341
           12  P-MESSAGE   PIC X(27).                                   EL341
           12  FILLER      PIC X(16).                                   EL341
           EJECT                                                        EL341

       01  COMP-3-AREA     COMP-3.                                      EL341
           12  PG-NO       PIC S9(5)       VALUE +0.                    EL341
           12  LN-CT       PIC S9(5)       VALUE +55.
           12  K1          PIC S9(5)       VALUE +1.                    EL341
           12  CNT         PIC S9(6)       VALUE +0.                    EL341
           12  CNT-R       PIC S9(6)       VALUE +0.                    EL341
           12  Z7          PIC S9(7)       VALUE +0.                    EL341
           12  T-DTH       PIC S9(7)V99    VALUE +0.                    EL341
           12  T-DIS       PIC S9(7)V99    VALUE +0.                    EL341
           12  T-DTH-R     PIC S9(7)V99    VALUE +0.                    EL341
           12  T-DIS-R     PIC S9(7)V99    VALUE +0.                    EL341
           12  Z9          PIC S9(7)V99    VALUE +0.                    EL341
           12  G-TOTCNT    PIC S9(6)       VALUE +0.                    EL341
           12  G-TOTDTH    PIC S9(7)V99    VALUE +0.                    EL341
           12  G-TOTDIS    PIC S9(7)V99    VALUE +0.                    EL341
           12  G-TOTCNT-R  PIC S9(7)V99   VALUE ZERO.                   EL341
           12  G-TOTDTH-R  PIC S9(7)V99    VALUE +0.                    EL341
           12  G-TOTDIS-R  PIC S9(7)V99    VALUE +0.                    EL341
           12  A-T-CNT     PIC S9(6)       VALUE +0.                    EL341
           12  A-T-DTH     PIC S9(7)V99    VALUE +0.                    EL341
           12  A-T-DIS     PIC S9(7)V99    VALUE +0.                    EL341
                                                                        EL341
       01  MISC-WORK-AREA.                                              EL341
           12  WK-DT       PIC 9(08)    VALUE 0.                           CL**2
           12  WK-DT-R REDEFINES WK-DT.                                    CL**2
               16  W-CC    PIC 99.                                      EL341
               16  W-YR    PIC 99.                                      EL341
               16  W-MO    PIC 99.                                      EL341
               16  W-DA    PIC 99.                                      EL341
           12  CAR-SW      PIC X        VALUE SPACE.                    EL341
           12  SAV-CARR    PIC X        VALUE SPACES.                   EL341
           12  PREV-ACT    PIC X(6)     VALUE SPACES.                   EL341
           12  SAV-PND-SW  PIC X.                                       EL341
           12  DAT-ERR-SW  PIC X        VALUE SPACE.                    EL341
                                                                        EL341
       01  TL-LN.                                                       EL341
           12  FILLER      PIC X(8)          VALUE SPACES.              EL341
           12  TLC         PIC X(18)         VALUE 'TOTAL FOR CARRIER '.EL341
           12  TL-CARR     PIC X.                                       EL341
           12  FILLER      PIC X(45)         VALUE SPACES.              EL341
           12  TL-CNT      PIC ZZZ,ZZ9.                                 EL341
           12  FILLER      PIC X(12)         VALUE ' CLAIMS FOR '.      EL341
           12  TL-DTH      PIC $(5),$$$.99-.                            EL341
           12  TL-DIS      PIC $(5),$$$.99-.                            EL341
                                                                        EL341
       01  TLR-LN.                                                      EL341
           12  FILLER      PIC X(8)        VALUE SPACES.                EL341
           12  FILLER      PIC X(30)                                    EL341
                                VALUE 'TOTAL REINSURANCE FOR CARRIER '. EL341
           12  TLR-CARR    PIC X.                                       EL341
           12  FILLER      PIC X(33)       VALUE SPACES.                EL341
           12  TLR-CNT     PIC ZZZ,ZZ9.                                 EL341
           12  FILLER      PIC X(12)       VALUE ' CLAIMS FOR '.        EL341
           12  TLR-DTH     PIC $(5),$$$.99-.                            EL341
           12  TLR-DIS     PIC $(5),$$$.99-.                            EL341
                                                                        EL341
       01  G-T-LN.                                                      EL341
           12  FILLER      PIC X(8)          VALUE  SPACE.              EL341
           12  FILLER      PIC X(18)         VALUE  'GRAND TOTALS'.     EL341
           12  FILLER      PIC X(41)         VALUE  SPACE.              EL341
           12  GT-CNT      PIC ZZZZ,ZZ9.                                EL341
           12  FILLNT      PIC X(12)         VALUE  ' CLAIMS FOR '.     EL341
           12  GT-DTH      PIC $(5),$$$.99-.                            EL341
           12  FILLER      PIC X(3)          VALUE  SPACES.             EL341
           12  GT-DIS      PIC $(5),$$$.99-.                            EL341
                                                                        EL341
       01  G-TR-LN.                                                     EL341
           12  FILLER      PIC X(8)          VALUE  SPACE.              EL341
           12  FILLER      PIC X(24)                                    EL341
                                VALUE  'GRAND TOTALS REINSURANCE'.      EL341
           12  FILLER      PIC X(35)         VALUE  SPACE.              EL341
           12  GTR-CNT     PIC ZZZZ,ZZ9.                                EL341
           12  FILLER      PIC X(12)         VALUE  ' CLAIMS FOR '.     EL341
           12  GTR-DTH     PIC $(5),$$$.99-.                            EL341
           12  FILLER      PIC X(3)          VALUE  SPACES.             EL341
           12  GTR-DIS     PIC $(5),$$$.99-.                            EL341
                                                                        EL341
       01  WARN-LN.                                                     EL341
           12  FILLER         PIC X(8)          VALUE  SPACE.           EL341
           12  WARN-LN-COUNT  PIC ZZZZ9.                                EL341
           12  FILLER         PIC X(120)        VALUE  SPACE.           EL341
                                                                        EL341
       01  ERROR-LN.                                                    EL341
           12  FILLER         PIC X(36)      VALUE SPACES.              EL341
           12  ERR-NOTE       PIC X(4).                                 EL341
           12  FILLER         PIC X(12)      VALUE ' ACCEPT DATE'.      EL341
           12  FILLER         PIC X(10)      VALUE SPACES.              EL341
           12  ERR-MO         PIC XX.                                   EL341
           12  FILLER         PIC X          VALUE '-'.                 EL341
           12  ERR-DA         PIC XX.                                   EL341
           12  FILLER         PIC X          VALUE '-'.                 EL341
           12  ERR-YR         PIC XX.                                   EL341
           12  FILLER         PIC X(63)      VALUE SPACES.              EL341
                                                                        EL341
           EJECT                                                        EL341
                                   COPY ELCDTECX.                       EL341
           EJECT                                                        EL341
                                   COPY ELCDTEVR.                       EL341
                                                                        EL341
                                   COPY ELCDATE.                           CL**6
           EJECT                                                        EL341
       PROCEDURE DIVISION.                                              EL341
       CAPTURE-START.                                                   EL341

       0000-SET-START. COPY ELCDTERX.                                   EL341

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0030-PROCESS        THRU 0030-EXIT UNTIL
              END-OF-INPUT

           IF CNT = ZEROS
              MOVE ' NO ACTIVITY TO PROCESS TODAY '
                                       TO DTL
              PERFORM 0170-WRITE-PRINT THRU 0170-EXIT
           END-IF

           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT
           
           GOBACK

           .           
       0010-OPEN-FILES.

           OPEN INPUT FILEIN
PEMTST*               ELTRLR
                OUTPUT PRNTR

PEMTST     OPEN I-O   ELTRLR


           IF ELTRLR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'ERROR OCCURED OPEN - ELTRLR'
                                       TO WS-ABEND-MESSAGE
              MOVE ELTRLR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INIT.


           MOVE COMPANY-NAME           TO HD-CO
           MOVE ALPH-DATE              TO HD-DT
           MOVE WS-CURRENT-DATE        TO HD-RD
           MOVE BIN-RUN-DATE           TO SAVE-POST-DATE
           PERFORM 0050-READ-FILEIN    THRU 0050-EXIT
           PERFORM 0200-PT-HDNG        THRU 0230-ALL-HDNG-E

           .
       0020-EXIT.
           EXIT.

       0030-PROCESS.

           IF SPACES NOT = CI-CARRIER AND CI-CLAIM-NO AND CI-CERT-NO
              PERFORM 0040-PROCESS-PMTS   THRU 0040-EXIT
           END-IF

           PERFORM 0050-READ-FILEIN    THRU 0050-EXIT
              
           .
       0030-EXIT.
           EXIT.

       0040-PROCESS-PMTS.

           IF CI-CARRIER = '6' OR '8' OR '9'
           PERFORM 0140-BUILD-PRINT    THRU 0140-EXIT
           PERFORM 0340-POST-PAYMENT   THRU 0349-EXIT
           PERFORM 0170-WRITE-PRINT    THRU 0170-EXIT
           END-IF

           .
       0040-EXIT.
           EXIT.
           
       0050-READ-FILEIN.

           READ FILEIN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
           
           IF NOT END-OF-INPUT
              ADD +1                   TO ELM-CTR
           END-IF
           
           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
                                       COPY ELCPRTC.

           CLOSE PRNTR
                 FILEIN
                 ELTRLR

           IF ELTRLR-FILE-STATUS NOT = ZERO
              MOVE 'ERROR OCCURED CLOSE - ELTRLR'
                                       TO WS-ABEND-MESSAGE
              MOVE ELTRLR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0127-PRNT-CARR SECTION.                                          EL341
                                                                        EL341
           MOVE ZERO                   TO LN-CT.                        EL341
           MOVE Z9                     TO T-DIS                         EL341
                                          T-DTH                         EL341
                                          G-TOTDTH                      EL341
                                          G-TOTDIS                      EL341
                                          G-TOTCNT                      EL341
                                          T-DIS-R                       EL341
                                          T-DTH-R                       EL341
                                          G-TOTDTH-R                    EL341
                                          G-TOTDIS-R                    EL341
                                          G-TOTCNT-R.                   EL341
                                                                        EL341
       0130-MAIN-LOOP  SECTION.                                         EL341
                                                                        EL341
           IF SAV-CARR = SPACES                                         EL341
               PERFORM 0190-SET-NEW.                                    EL341
                                                                        EL341
           IF  CI-CARRIER NOT = SAV-CARR                                EL341
               PERFORM 0180-PRT-TOTALS                                  EL341
               PERFORM 0190-SET-NEW.                                    EL341
                                                                        EL341
       0140-BUILD-PRINT.

           MOVE SPACES                 TO DTL
                                          DTL-RDF
           MOVE CI-CARRIER             TO P-CARRIER
           MOVE CI-CERT-NO             TO P-CERT-NO
           MOVE CI-CLAIM-NO            TO P-CLM-NO
           MOVE CI-CHECK-NO            TO P-CHECK-NO

           STRING CI-CHECK-DATE (5:2) '/' CI-CHECK-DATE (7:2) '/'
              CI-CHECK-DATE (1:4) DELIMITED BY SIZE INTO P-PAY-DATE
           END-STRING
           MOVE CI-CHECK-AMOUNT        TO P-AMT
           MOVE 'PAYMENT NOT POSTED '  TO P-MESSAGE

           MOVE CI-CHECK-DATE          TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-CHECK-DT
           ELSE
              DISPLAY 'ERROR CONVERTING PAY DATE '
              MOVE 'DATE ERROR, NO POST' TO P-MESSAGE
              GO TO 0140-EXIT
           END-IF

           ADD K1                      TO CNT

           ADD K1                      TO A-T-CNT

           .
       0140-EXIT.
           EXIT.

       0170-WRITE-PRINT.

           IF LN-CT > 52
              PERFORM 0200-PT-HDNG     THRU 0230-ALL-HDNG-E
           END-IF

           MOVE DTL                    TO P-DATA
           MOVE '0'                    TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT

           MOVE DTL-RDF                TO P-DATA
           MOVE ' '                    TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT
           ADD 3                       TO LN-CT

           .
       0170-EXIT.
           EXIT.
                  
       0180-PRT-TOTALS  SECTION.                                        EL341
                                                                        EL341
           MOVE    CNT                 TO  TL-CNT.                      EL341
           MOVE    T-DTH               TO  TL-DTH.                      EL341
           MOVE    T-DIS               TO  TL-DIS.                      EL341
           ADD     T-DTH               TO  G-TOTDTH.                    EL341
           ADD     T-DIS               TO  G-TOTDIS.                    EL341
           ADD     CNT                 TO  G-TOTCNT.                    EL341
           MOVE    TL-LN               TO  P-DATA.                      EL341
           MOVE    '0'                 TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
                                                                        EL341
           MOVE    SPACES              TO  P-DATA.                      EL341
           MOVE    CNT-R               TO  TLR-CNT.                     EL341
           MOVE    T-DTH-R             TO  TLR-DTH.                     EL341
           MOVE    T-DIS-R             TO  TLR-DIS.                     EL341
           ADD     T-DTH-R             TO  G-TOTDTH-R.                  EL341
           ADD     T-DIS-R             TO  G-TOTDIS-R.                  EL341
           ADD     CNT-R               TO  G-TOTCNT-R.                  EL341
           MOVE    TLR-LN              TO  P-DATA.                      EL341
           MOVE    '0'                 TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE    SPACES              TO  SAV-CARR.                    EL341
                                                                        EL341
       0190-SET-NEW   SECTION.                                          EL341
                                                                        EL341
           MOVE CI-CARRIER             TO HD-CARR                       EL341
                                          TL-CARR                       EL341
                                          TLR-CARR                      EL341
                                          SAV-CARR.                     EL341
           MOVE Z7                     TO CNT                           EL341
                                          CNT-R.                        EL341
           MOVE Z9                     TO T-DTH                         EL341
                                          T-DIS                         EL341
                                          T-DTH-R                       EL341
                                          T-DIS-R.                      EL341
                                                                        EL341
       0200-PT-HDNG.                                                    EL341

           ADD K1                      TO PG-NO
           MOVE PG-NO                  TO HD-PG
           MOVE HD1                    TO P-DATA
           MOVE '1'                    TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT
           MOVE HD2                    TO P-DATA
           MOVE SPACE                  TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT
           MOVE HD3                    TO P-DATA
           MOVE SPACE                  TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT

           MOVE HD4                    TO P-DATA
           MOVE '0'                    TO X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT
           MOVE SPACES                 TO P-DATA
                                          X
           PERFORM 0320-PRT-RTN        THRU 0330-PRT-RTN-XIT
           MOVE SPACES                 TO P-DATA
           MOVE 6                      TO LN-CT

           .
       0230-ALL-HDNG-E.                                                 EL341
           EXIT.                                                        EL341
           EJECT                                                        EL341
                                                                        EL341
       0250-PT-WARNING  SECTION.                                        EL341
                                                                        EL341
           MOVE ' ***************************************************'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    '-'                 TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' *** WARNING ... WARNING ... WARNING ... WARNING ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***                                             ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***         THIS  RUN  CONTAINS  ERRORS         ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***                                             ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***    XXXXX CLAIM PAYMENTS WERE NOT POSTED     ***'  EL341
                                       TO  WARN-LN.                     EL341
           MOVE ERROR-NF-COUNT         TO  WARN-LN-COUNT.               EL341
           MOVE WARN-LN                TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***                                             ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' *** WARNING ... WARNING ... WARNING ... WARNING ***'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
           MOVE ' ***************************************************'  EL341
                                       TO  P-DATA.                      EL341
           MOVE    SPACE               TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
                                                                        EL341
       0260-WARNING-EXIT.                                               EL341
           EXIT.                                                        EL341
                                                                        EL341
           EJECT                                                        EL341
       0320-PRT-RTN  SECTION. COPY ELCPRT2.
                                                                        EL341
       0330-PRT-RTN-XIT.                                                EL341
           EXIT.                                                        EL341

       0340-POST-PAYMENT.
                                                                        EL341
           MOVE LOW-VALUES             TO WS-AT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO WS-AT-COMPANY-CD
           MOVE CI-CERT-NO             TO WS-AT-CERT-NO
           MOVE CI-CLAIM-NO            TO WS-AT-CLAIM-NO
           MOVE CI-CARRIER             TO WS-AT-CARRIER
           MOVE +92                    TO WS-AT-SEQUENCE-NO

           MOVE WS-AT-CONTROL-PRIMARY  TO AT-CONTROL-PRIMARY

           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           
           IF ELTRLR-FILE-STATUS = '10' OR '23'
              MOVE '**** PMT NOT FOUND ****S'
                                       TO P-MESSAGE
              GO TO 0349-EXIT
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE 'ERROR OCCURED START - ELTRLR'
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0344-READ-NEXT.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              DISPLAY ' PMT NOT FOUND R ' WS-AT-CLAIM-NO
              MOVE '**** PMT NOT FOUND ****R'
                                       TO P-MESSAGE
              GO TO 0349-EXIT
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE 'ERROR OCCURED READ  - ELTRLR'
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF AT-CONTROL-PRIMARY (2:19)
                          NOT = WS-AT-CONTROL-PRIMARY (2:19)
              DISPLAY ' FINISHED WITH CLM ' WS-AT-CLAIM-NO
              GO TO 0349-EXIT
           END-IF

           IF (AT-CONTROL-PRIMARY (2:19)
                              = WS-AT-CONTROL-PRIMARY (2:19))
              AND (PAYMENT-TR)
              AND (LIFE-INTEREST)
              AND (AT-CHECK-WRITTEN-DT = LOW-VALUES)
              AND (AT-PMT-ACCEPT-DT = LOW-VALUES)
              AND (AT-CHECK-NO = SPACES)
              AND (AT-VOID-DT = LOW-VALUES)
              MOVE AT-PAYEES-NAME      TO P-NAME
              MOVE WS-BIN-CHECK-DT     TO AT-CHECK-WRITTEN-DT
              MOVE CI-CHECK-NO (4:7)   TO AT-CHECK-NO
              MOVE SAVE-POST-DATE      TO AT-PMT-ACCEPT-DT
              MOVE '**** PAYMENT POSTED ****'
                                       TO P-MESSAGE
              DISPLAY ' REWRITE HERE ' AT-CONTROL-PRIMARY (2:19) 
                 '  ' AT-CHECK-NO ' ' 
PEMTST        REWRITE  ACTIVITY-TRAILERS
      *       GO TO 0349-EXIT
           END-IF

           GO TO 0344-READ-NEXT
              
           .
       0349-EXIT.                                                       EL341
           EXIT.                                                        EL341
           EJECT                                                        EL341
                                                                        EL341
       0360-G-TOTALS  SECTION.                                          EL341
                                                                        EL341
           MOVE    'GRAND TOTAL'       TO  HD1-CARR.                    EL341
           MOVE    SPACES              TO  HD-CARR.                     EL341
           PERFORM 0200-PT-HDNG THRU 0230-ALL-HDNG-E.                   EL341
           MOVE    G-TOTCNT            TO  GT-CNT.                      EL341
           MOVE    G-TOTDTH            TO  GT-DTH.                      EL341
           MOVE    G-TOTDIS            TO  GT-DIS.                      EL341
           MOVE    G-T-LN              TO  P-DATA.                      EL341
           MOVE    '0'                 TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
                                                                        EL341
           MOVE    SPACES              TO  P-DATA.                      EL341
           MOVE    G-TOTCNT-R          TO  GTR-CNT.                     EL341
           MOVE    G-TOTDTH-R          TO  GTR-DTH.                     EL341
           MOVE    G-TOTDIS-R          TO  GTR-DIS.                     EL341
           MOVE    G-TR-LN             TO  P-DATA.                      EL341
           MOVE    '0'                 TO  X.                           EL341
           PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
                                                                        EL341
       0360-END-G-TOTAL.                                                EL341
           EXIT.                                                        EL341
                                                                        EL341
       8500-DATE-CONVERSION.                                            EL341
                                                                        EL341
           COPY ELCDCS.                                                 EL341
                                                                        EL341
       ABEND-PGM SECTION. COPY ELCABEND.                                EL341
                                                                        EL341
