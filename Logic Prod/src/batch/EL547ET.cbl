       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL547ET.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM READS THE MONTH END CERT FILE AND CLMS FILE
      *    AND BUILDS AN EXTRACT BY ISSUE YEAR.  THE EXTRACT IS
      *    PRIMARLIY USED BY ACTUARIAL.  THE REFUNDS SELECTED ARE
      *    PLACED IN THE VALUATION YEAR BASED ON THE CANCEL DATE.
      *    THE CLAIMS SELECTED ARE PLACED IN THE VALUATION YEAR
      *    BASED ON INCURRED DATE.  THE LOWEST ISSUE YEAR WILL ALWAYS
      *    BE 1986.  IF YOU NEED TO CHANGE THAT SIMPLY CHANGE THE
      *    HARD CODING WHERE YOU FIND MOVE 1986 TO LO-ISSUE AND
      *    LO-VALUATION.  ANY CHANGES MADE TO THE SPECIAL ECS083
      *    MUST ALSO BE MADE IN THIS PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO
      *       '/data/seqfiles/ZI.XX.CERT.PEMA'.
              SYS010.
           SELECT EXTRACT      ASSIGN TO
      *       '/data/seqfiles/ZI.XX.EXTR547'
              SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CLMS-HIST    ASSIGN TO
      *       '/data/seqfiles/ZI.XX.CLMS.A15860'.
              SYS013.
           SELECT DISK-DATE    ASSIGN TO
      *       '/data/seqfiles/ZI.DD.ER.DATECARD.EL547'.
              SYS019.
           SELECT PRINTX       ASSIGN TO
      *       '/data/seqfiles/sys008'.
              SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  CLMS-HIST
                                       COPY ECSEXTFD.
                                       COPY ECSEXT01.
      /

       EJECT
       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(302).
       01  EXTRACT-RECORD-OUT-HD       PIC X(400).


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     EL547ET  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS  VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS     VALUE 'N'.
       77  IYR                         PIC S999   COMP-3 VALUE +0.
       77  VYR                         PIC S999   COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-CRT-CNT                  PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CLM-CNT                  PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CRT-CNT-TOT              PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CLM-CNT-TOT              PIC S9(9)  COMP-3 VALUE +0.
       77  INTERMED                    PIC S9(9)V9(6)  COMP-3.
       77  WS-REM-AMT                  PIC S9(11)V99 COMP-3 VALUE +0.
       77  LF-REM-TRM1                 PIC S999V99  COMP-3  VALUE +0.
       77  AH-REM-TRM1                 PIC S999V99  COMP-3  VALUE +0.
       77  LF-REM-TRM2                 PIC S999V99  COMP-3  VALUE +0.
       77  AH-REM-TRM2                 PIC S999V99  COMP-3  VALUE +0.
       77  REM-TRM1                    PIC S9(3)V99    COMP-3.
       77  REM-TRM2                    PIC S9(3)V99    COMP-3.
       77  WS-CERT-PREV-ST             PIC S9(7)V99    COMP-3 VALUE +0.
       77  WS-CERT-PREV-R78            PIC S9(7)V99    COMP-3 VALUE +0.
       77  WS-CERT-PREV-PRO            PIC S9(7)V99    COMP-3 VALUE +0.

       01  WS-INIT-EXT-RECORD          PIC X(302)  VALUE LOW-VALUES.

       01  WS-EXTRACT-RECORD.
           05  EXT-ISSUE-CCYYMM        PIC 9(6).
           05  WS-TAB1                 PIC X.
           05  EXT-VALUE-CCYYMM        PIC 9(6).
           05  WS-TAB2                 PIC X.
           05  EXT-LF-INFORCE          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB3                 PIC X.
           05  EXT-LF-WRITTEN          PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB4                 PIC X.
           05  EXT-AH-WRITTEN          PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB5                 PIC X.
           05  EXT-LF-REFUNDS          PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB6                 PIC X.
           05  EXT-AH-REFUNDS          PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB7                 PIC X.
           05  EXT-LF-R78              PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB8                 PIC X.
           05  EXT-LF-PRO              PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB9                 PIC X.
           05  EXT-LF-ST               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB10                PIC X.
           05  EXT-LF-R78-EP           PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB11                PIC X.
           05  EXT-LF-PRO-EP           PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB12                PIC X.
           05  EXT-LF-ST-EP            PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB13                PIC X.
           05  EXT-AH-R78              PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB14                PIC X.
           05  EXT-AH-PRO              PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB15                PIC X.
           05  EXT-AH-ST               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB16                PIC X.
           05  EXT-AH-R78-EP           PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB17                PIC X.
           05  EXT-AH-PRO-EP           PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB18                PIC X.
           05  EXT-AH-ST-EP            PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB19                PIC X.
           05  EXT-LF-CLAIM            PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB20                PIC X.
           05  EXT-AH-CLAIM            PIC ZZZ,ZZZ,ZZ9.99.

       01  WS-HEAD-RECORD.
           05  FILLER                  PIC X(8)    VALUE 'ISS DATE'.
           05  WS-HTAB1                PIC X.
           05  FILLER                  PIC X(8)    VALUE 'VAL DATE'.
           05  WS-HTAB2                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'LF INFORCE'.
           05  WS-HTAB3                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'LF WRITTEN'.
           05  WS-HTAB4                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'AH WRITTEN'.
           05  WS-HTAB5                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'LF REFUNDS'.
           05  WS-HTAB6                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'AH REFUNDS'.
           05  WS-HTAB7                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'LF UEP R78'.
           05  WS-HTAB8                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'LF UEP PRO'.
           05  WS-HTAB9                PIC X.
           05  FILLER                  PIC X(9)    VALUE 'LF UEP ST'.
           05  WS-HTAB10               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'LF EP R78'.
           05  WS-HTAB11               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'LF EP PRO'.
           05  WS-HTAB12               PIC X.
           05  FILLER                  PIC X(8)    VALUE 'LF EP ST'.
           05  WS-HTAB13               PIC X.
           05  FILLER                  PIC X(10)   VALUE 'AH UEP R78'.
           05  WS-HTAB14               PIC X.
           05  FILLER                  PIC X(10)   VALUE 'AH UEP PRO'.
           05  WS-HTAB15               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'AH UEP ST'.
           05  WS-HTAB16               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'AH EP R78'.
           05  WS-HTAB17               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'AH EP PRO'.
           05  WS-HTAB18               PIC X.
           05  FILLER                  PIC X(8)    VALUE 'AH EP ST'.
           05  WS-HTAB19               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'LF CLAIMS'.
           05  WS-HTAB20               PIC X.
           05  FILLER                  PIC X(9)    VALUE 'AH CLAIMS'.



       01  WS-ISSUE-YR-TABLE.
           05  WS-ISSUE         OCCURS 600.
               10  WS-ISSUE-CCYYMM     PIC 9(6).
               10  FILLER              PIC XX.
               10  WS-VALUATION OCCURS 600.
                 12  WS-RECORD.
                   15  WS-VALUATION-CCYYMM PIC 9(6).
                   15  WS-LF-INFORCE   PIC S9(12)V99 COMP-3.
                   15  WS-LF-WRITTEN   PIC S9(9)V99 COMP-3.
                   15  WS-AH-WRITTEN   PIC S9(9)V99 COMP-3.
                   15  WS-LF-REFUNDS   PIC S9(9)V99 COMP-3.
                   15  WS-AH-REFUNDS   PIC S9(9)V99 COMP-3.
                   15  WS-LF-R78       PIC S9(9)V99 COMP-3.
                   15  WS-LF-PRO       PIC S9(9)V99 COMP-3.
                   15  WS-LF-ST        PIC S9(9)V99 COMP-3.
                   15  WS-LF-R78-EP    PIC S9(9)V99 COMP-3.
                   15  WS-LF-PRO-EP    PIC S9(9)V99 COMP-3.
                   15  WS-LF-ST-EP     PIC S9(9)V99 COMP-3.
                   15  WS-AH-R78       PIC S9(9)V99 COMP-3.
                   15  WS-AH-PRO       PIC S9(9)V99 COMP-3.
                   15  WS-AH-ST        PIC S9(9)V99 COMP-3.
                   15  WS-AH-R78-EP    PIC S9(9)V99 COMP-3.
                   15  WS-AH-PRO-EP    PIC S9(9)V99 COMP-3.
                   15  WS-AH-ST-EP     PIC S9(9)V99 COMP-3.
                   15  WS-LF-CLAIM     PIC S9(9)V99 COMP-3.
                   15  WS-AH-CLAIM     PIC S9(9)V99 COMP-3.

       01  FILLER                  PIC X(400)  VALUE LOW-VALUES.

       01  WS-DISPLAY-DATE         PIC ZZZ9(8).
       01  WS-DISPLAY-TERM         PIC Z99.
       01  WS-DISPLAY-RTERM        PIC Z99.99.
       01  WS-DISPLAY-AMT          PIC ZZZZZZZ.99.
       01  WS-DISPLAY-RAMT         PIC ZZZZZZZ.99.
       01  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
       01  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.
       01  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-ENTRY-DT         PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
       01  FILLER. 
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  FILLER              PIC 999.
               10  WS-CCYYMM           PIC 9(6).
               10  FILLER REDEFINES WS-CCYYMM.
                   15  WS-CCYY         PIC 9999.
                   15  WS-MM           PIC 99.
               10  WS-DD               PIC 99.
       01  WS-DEBUG-AREA.
           12  WS-DEBUG-SW             PIC X(01) VALUE ' '.
               88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.
               88  DEBUG-LF-INFORCE-CNT       VALUE '1'.
               88  DEBUG-AH-INFORCE-CNT       VALUE '2'.
               88  DEBUG-LF-STATUTORY         VALUE '3'.
               88  DEBUG-AH-STATUTORY         VALUE '4'.

           EJECT

       01  TEXAS-REGS-WORK-AREAS.
           12  TEX-FACT-1              PIC S9(7)V9(2)    COMP-3.
           12  TEX-FACT-2              PIC S9(3)   COMP-3.
           12  TEX-FACT-3              PIC S9(3)   COMP-3.
           12  TEX-FACT-4              PIC S9(7)   COMP-3.
           12  TEX-FACT-5              PIC S9(3)   COMP-3.
           12  TEX-FACT-6              PIC S9(3)   COMP-3.
           12  TEX-FACT-7              PIC S9(7)   COMP-3.
           12  TEX-FACT-8              PIC S9V9(6) COMP-3.
           12  TEX-FACT-9              PIC S9(4)V9(11)   COMP-3.

      *01  NET-PAY-INTERFACE.
      *    05  NP-APR                  PIC S9(3)V9(4)    COMP-3.
      *    05  NP-ORIG                 PIC S9(3)         COMP-3.
      *    05  NP-REM                  PIC S9(3)         COMP-3.
      *    05  NP-OPT                  PIC X(01).
      *    05  NP-CAP                  PIC S9(3)         COMP-3.
      *    05  NP-FACTOR               PIC S9(4)V9(9)    COMP-3.
      *    05  NP-WORK1                PIC S9(6)V9(9)    COMP-3.
      *    05  NP-WORK2                PIC S9(6)V9(9)    COMP-3.

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

       01  FILLER.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYYMM      PIC 9(6).
               10  FILLER REDEFINES WS-WORK-CCYYMM.
                   15  WS-WORK-CCYY    PIC 9999.
                   15  WS-WORK-MM      PIC 99.
               10  WS-WORK-DD          PIC 99.
       01  FILLER.
           05  WS-EFFECT-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-EFFECT-DATE.
               10  FILLER              PIC 999.
               10  WS-EFFECT-CCYYMM     PIC 9(6).
               10  WS-EFFECT-DD        PIC 99.
       01  FILLER.
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES        WS-ENTRY-DATE.
               10  FILLER              PIC 999.
               10  WS-ENTRY-CCYYMM     PIC 9(6).
               10  WS-ENTRY-DD         PIC 99.
       01  FILLER.
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC 999.
               10  WS-CANCEL-CCYYMM    PIC 9(6).
               10  WS-CANCEL-DD        PIC 99.
       01  FILLER.
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC 999.
               10  WS-CLAIM-CCYYMM     PIC 9(6).
               10  WS-CLAIM-DD         PIC 99.
       01  FILLER.
           05  WS-HI-ISSUE-CCYYMM      PIC 9(6) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYYMM      PIC 9(6) VALUE ZEROS.
       01  FILLER.
           05  WS-HI-VALUATION-CCYYMM  PIC 9(6) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYYMM  PIC 9(6) VALUE ZEROS.
           05  FILLER REDEFINES WS-LO-VALUATION-CCYYMM.
               10  WS-LO-VAL-CCYY      PIC 9999.
               10  WS-LO-VAL-MM        PIC 99.
       01  FILLER.
           05  WS-BIN-VAL-DATES OCCURS 600
                                       PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCCALC.
      /
                                       COPY ELCDATE.
                                       COPY ELCFUNDT.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           MOVE 'Y'                    TO DC-FORCE-EL310-DATE-SW
           MOVE FUNCTION-DATE          TO DC-EL310-DATE


           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT


           GOBACK
           .

       0010-INITIALIZE.

      *    DISPLAY ' MADE IT TO 0010 '
           MOVE RUN-DATE               TO WS-WORK-DATE

           MOVE 200001                 TO WS-LO-ISSUE-CCYYMM
                                          WS-LO-VALUATION-CCYYMM
           MOVE WS-WORK-CCYYMM         TO WS-HI-ISSUE-CCYYMM
           PERFORM VARYING IYR FROM +1 BY +1 UNTIL
                  (IYR > +600) OR
                  (WS-ISSUE-CCYYMM (IYR) > WS-HI-ISSUE-CCYYMM)
              MOVE WS-LO-VALUATION-CCYYMM TO WS-ISSUE-CCYYMM (IYR)
              ADD +1 TO WS-LO-VALUATION-CCYYMM
              IF WS-LO-VAL-MM > 12
                 MOVE 01               TO WS-LO-VAL-MM
                 ADD 1                 TO WS-LO-VAL-CCYY
              END-IF
           END-PERFORM

           MOVE WS-LO-ISSUE-CCYYMM     TO WS-LO-VALUATION-CCYYMM
                                          WS-WORK-CCYYMM

           PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                  VYR > +600
                PERFORM VARYING IYR FROM +1 BY +1 UNTIL
                   IYR > +600
                   MOVE WS-WORK-CCYYMM TO WS-VALUATION-CCYYMM (IYR VYR)
                END-PERFORM
              MOVE WS-WORK-CCYYMM      TO WS-CCYYMM
              
              IF WS-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
                 MOVE 31               TO WS-DD
              ELSE
                 IF WS-MM = 04 OR 06 OR 09 OR 11
                    MOVE 30            TO WS-DD
                 ELSE
                    MOVE 01            TO WS-DD
                    MOVE WS-END-YEAR   TO DC-GREG-DATE-CYMD
                    MOVE 'L'           TO DC-OPTION-CODE
                    PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                    IF NO-CONVERSION-ERROR
      *                DISPLAY ' FEB DAYS ' DC-DAYS-IN-MONTH
                       MOVE DC-DAYS-IN-MONTH
                                       TO WS-DD
                    ELSE
                       DISPLAY ' PROBLEMOS WITH FEB VAL DTE CONV '
                       PERFORM ABEND-PGM
                    END-IF
                 END-IF
              END-IF
              
              MOVE WS-END-YEAR         TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-1       TO WS-BIN-VAL-DATES (VYR)
              ADD 1                    TO WS-WORK-CCYYMM
              IF WS-WORK-MM > 12
                 MOVE 01               TO WS-WORK-MM
                 ADD 1                 TO WS-WORK-CCYY
              END-IF
           END-PERFORM

           SUBTRACT +1 FROM WS-WORK-CCYYMM
           IF WS-WORK-MM = ZEROS
              MOVE 12                  TO WS-WORK-MM
              SUBTRACT 1               FROM WS-WORK-CCYY
           END-IF
           MOVE WS-WORK-CCYYMM         TO WS-HI-VALUATION-CCYYMM

           PERFORM VARYING IYR FROM +1 BY +1 UNTIL
               IYR > +600
               PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                  VYR > +600
                  MOVE +0              TO WS-LF-INFORCE (IYR VYR)
                                          WS-LF-WRITTEN (IYR VYR)
                                          WS-AH-WRITTEN (IYR VYR)
                                          WS-LF-REFUNDS (IYR VYR)
                                          WS-AH-REFUNDS (IYR VYR)
                                          WS-LF-R78     (IYR VYR)
                                          WS-LF-PRO     (IYR VYR)
                                          WS-LF-ST      (IYR VYR)
                                          WS-LF-R78-EP  (IYR VYR)
                                          WS-LF-PRO-EP  (IYR VYR)
                                          WS-LF-ST-EP   (IYR VYR)
                                          WS-AH-R78     (IYR VYR)
                                          WS-AH-PRO     (IYR VYR)
                                          WS-AH-ST      (IYR VYR)
                                          WS-AH-R78-EP  (IYR VYR)
                                          WS-AH-PRO-EP  (IYR VYR)
                                          WS-AH-ST-EP   (IYR VYR)
                                          WS-LF-CLAIM   (IYR VYR)
                                          WS-AH-CLAIM   (IYR VYR)
               END-PERFORM
           END-PERFORM

           MOVE SPACES                 TO WS-EXTRACT-RECORD
           MOVE ';'                    TO WS-TAB1
                                          WS-TAB2
                                          WS-TAB3
                                          WS-TAB4
                                          WS-TAB5
                                          WS-TAB6
                                          WS-TAB7
                                          WS-TAB8
                                          WS-TAB9
                                          WS-TAB10
                                          WS-TAB11
                                          WS-TAB12
                                          WS-TAB13
                                          WS-TAB14
                                          WS-TAB15
                                          WS-TAB16
                                          WS-TAB17
                                          WS-TAB18
                                          WS-TAB19
                                          WS-TAB20
           MOVE WS-EXTRACT-RECORD      TO WS-INIT-EXT-RECORD
           
           MOVE ';'                    TO WS-HTAB1
                                          WS-HTAB2
                                          WS-HTAB3
                                          WS-HTAB4
                                          WS-HTAB5
                                          WS-HTAB6
                                          WS-HTAB7
                                          WS-HTAB8
                                          WS-HTAB9
                                          WS-HTAB10
                                          WS-HTAB11
                                          WS-HTAB12
                                          WS-HTAB13
                                          WS-HTAB14
                                          WS-HTAB15
                                          WS-HTAB16
                                          WS-HTAB17
                                          WS-HTAB18
                                          WS-HTAB19
                                          WS-HTAB20


           DISPLAY '  LO ISSUE  ' WS-LO-ISSUE-CCYYMM
                 '    HI ISSUE  ' WS-HI-ISSUE-CCYYMM
           DISPLAY '  LO VALUE  ' WS-LO-VALUATION-CCYYMM
                 '    HI VALUE  ' WS-HI-VALUATION-CCYYMM

           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           DISPLAY ' MADE IT TO 0020 '
           OPEN INPUT CERTS CLMS-HIST
               OUTPUT EXTRACT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.
           DISPLAY ' MADE IT TO 0030 '

           CLOSE CERTS CLMS-HIST
               EXTRACT

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

      *    DISPLAY ' MADE IT TO 0050 '
           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           DISPLAY ' FINISHED CERTS '

           SET THERE-ARE-MORE-RECORDS TO TRUE

           PERFORM 0070-READ-CLMS      THRU 0070-EXIT UNTIL
               (THERE-ARE-NO-MORE-RECORDS) OR
               ((DE-REIN = SPACES) AND
               (DE-CLAIM))

           DISPLAY ' POSITIONED CLAIMS '
           
           IF NOT THERE-ARE-NO-MORE-RECORDS
              PERFORM 0090-PROCESS-CLMS
                                       THRU 0090-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS
           END-IF

           DISPLAY ' FINISHED CLMS  '

           PERFORM 0500-EMPTY-TABLE    THRU 0500-EXIT

           .

       0050-EXIT.
           EXIT.

       0060-READ-CERT.

      *    DISPLAY ' MADE IT TO 0060 '
           READ CERTS AT END
               SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-READ

           IF NOT THERE-ARE-NO-MORE-RECORDS
              ADD +1                   TO WS-CRT-CNT
                                          WS-CRT-CNT-TOT
              IF WS-CRT-CNT = +10000
                 DISPLAY ' CERTS READ ' WS-CRT-CNT-TOT
                 MOVE +0               TO WS-CRT-CNT
              END-IF
              IF CR-CERT-NO = '0000109489 '
                 DISPLAY ' FOUND BAD CERT '
              END-IF
           END-IF

           .

       0060-EXIT.
           EXIT.

       0070-READ-CLMS.

      *    DISPLAY ' MADE IT TO 0070 '
           READ CLMS-HIST AT END
               SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-READ

           IF NOT THERE-ARE-NO-MORE-RECORDS
              ADD +1                   TO WS-CLM-CNT
                                          WS-CLM-CNT-TOT
              IF WS-CLM-CNT = +10000
                 DISPLAY ' CLMS  READ ' WS-CLM-CNT-TOT
                 MOVE +0               TO WS-CLM-CNT
              END-IF
           END-IF

           .

       0070-EXIT.
           EXIT.

       0080-PROCESS-CERT.

      *    DISPLAY ' MADE IT TO 0080 '
           MOVE CR-DT                  TO WS-EFFECT-DATE
           MOVE CR-ENTRY-DATE          TO WS-ENTRY-DATE

           IF (WS-EFFECT-CCYYMM < WS-LO-ISSUE-CCYYMM) OR
              (WS-EFFECT-CCYYMM > WS-HI-ISSUE-CCYYMM) OR
              (CR-ENTRY-STATUS = '9' OR 'D' OR 'V' OR 'M')
              CONTINUE
           ELSE
              PERFORM VARYING IYR FROM +1 BY +1 UNTIL
                 WS-EFFECT-CCYYMM = WS-ISSUE-CCYYMM (IYR)
              END-PERFORM
              PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                 WS-ENTRY-CCYYMM = WS-VALUATION-CCYYMM (IYR VYR)
              END-PERFORM
              IF CR-ENTRY-STATUS = '5'
                 CONTINUE
              ELSE
                 IF CR-LFTYP NOT = '00' AND '  '
                   COMPUTE WS-LF-WRITTEN (IYR VYR) =
                     WS-LF-WRITTEN (IYR VYR) +
                     CR-LFPRM + CR-LFPRM-ALT
                 END-IF
                 COMPUTE WS-AH-WRITTEN (IYR VYR) =
                     WS-AH-WRITTEN (IYR VYR) + CR-AHPRM
              END-IF
              IF (CR-LFTYP NOT = '00' AND '  ')
                 AND (CR-LF-CANCEL-EXIT-DATE > 19991231)
                 IF CR-LFRFND NOT NUMERIC
                    MOVE +0            TO CR-LFRFND
                 END-IF
                 MOVE CR-LF-CANCEL-EXIT-DATE    TO WS-CANCEL-DATE
                 PERFORM VARYING VYR FROM IYR BY +1 UNTIL
                    WS-CANCEL-CCYYMM = WS-VALUATION-CCYYMM (IYR VYR)
                 END-PERFORM
                 COMPUTE WS-LF-REFUNDS (IYR VYR) =
                       WS-LF-REFUNDS (IYR VYR) + CR-LFRFND
              END-IF
              IF (CR-AHTYP NOT = '00' AND '  ')
                 AND (CR-AH-CANCEL-EXIT-DATE > 19991231)
                 IF CR-AHRFND NOT NUMERIC
                    MOVE +0            TO CR-AHRFND
                 END-IF
                 MOVE CR-AH-CANCEL-EXIT-DATE    TO WS-CANCEL-DATE
                 PERFORM VARYING VYR FROM IYR BY +1 UNTIL
                    WS-CANCEL-CCYYMM = WS-VALUATION-CCYYMM (IYR VYR)
                 END-PERFORM
                 COMPUTE WS-AH-REFUNDS (IYR VYR) =
                       WS-AH-REFUNDS (IYR VYR) + CR-AHRFND
              END-IF
              IF CR-ENTRY-STATUS NOT = '5'
                 PERFORM 0200-CALC-UEP    THRU 0200-EXIT
              END-IF
           END-IF

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .

       0080-EXIT.
           EXIT.

       0090-PROCESS-CLMS.

      *    DISPLAY ' MADE IT TO 0090 '
      *    MOVE DE-EFF                 TO WS-EFFECT-DATE
           MOVE DE-ENTRY-DTE           TO WS-EFFECT-DATE
           MOVE DE-CLM-PROC-DT         TO WS-CLAIM-DATE
      *    MOVE DE-PAY                 TO WS-CLAIM-DATE
      *    MOVE DE-INCUR               TO WS-CLAIM-DATE

           IF (DE-REIN = ' ') AND
              (DE-TRANS = 'X')
           IF (DE-CLM-PROC-DT > RUN-DATE)
              CONTINUE
           ELSE
              IF (WS-EFFECT-CCYYMM < WS-LO-ISSUE-CCYYMM) OR
                 (WS-EFFECT-CCYYMM > WS-HI-ISSUE-CCYYMM)
                 CONTINUE
              ELSE
                 PERFORM VARYING IYR FROM +1 BY +1 UNTIL
                    WS-EFFECT-CCYYMM = WS-ISSUE-CCYYMM (IYR)
                 END-PERFORM
                 PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                    WS-CLAIM-CCYYMM = WS-VALUATION-CCYYMM (IYR VYR)
                 END-PERFORM
                 IF DE-DEATH
                    COMPUTE WS-LF-CLAIM (IYR VYR) =
                           WS-LF-CLAIM (IYR VYR) +
                           DE-CLAIM-AMT
                 ELSE
                    COMPUTE WS-AH-CLAIM (IYR VYR) =
                           WS-AH-CLAIM (IYR VYR) +
                           DE-CLAIM-AMT
                 END-IF
              END-IF
           END-IF
           END-IF

           PERFORM 0070-READ-CLMS      THRU 0070-EXIT

           .

       0090-EXIT.
           EXIT.

       0200-CALC-UEP.

      *    DISPLAY ' MADE IT TO 0200 '
           IF CR-STATE = STATE-SUB (CLAS-INDEXS)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXS FROM +1 BY +1 UNTIL
                  (CLAS-INDEXS > CLAS-MAXS) OR
                  CR-STATE = STATE-SUB (CLAS-INDEXS)
              END-PERFORM
              IF CR-STATE = STATE-SUB (CLAS-INDEXS)
                 CONTINUE
              ELSE
                 DISPLAY 'STATE ' CR-STATE ' NOT IN TABLE'
                 MOVE 0302 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT
                                          WS-BIN-CR-DT

           MOVE CR-ENTRY-DATE          TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-BIN-ENTRY-DT

           IF CR-LOAN-1ST-PMT-DT NOT NUMERIC
              MOVE ZEROS               TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE LOW-VALUES             TO CP-FIRST-PAY-DATE

           IF CR-LOAN-1ST-PMT-DT NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO DC-GREG-DATE-1-YMD
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1   TO CP-FIRST-PAY-DATE
              ELSE
                 MOVE ZEROS           TO CR-LOAN-1ST-PMT-DT
              END-IF
            END-IF

           IF CP-FIRST-PAY-DATE  LESS THAN  CP-CERT-EFF-DT
              MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT
           END-IF

           IF CR-LOAN-1ST-PMT-DT = ZEROS
              MOVE CP-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE +1                  TO DC-ELAPSED-MONTHS
              MOVE ZEROS               TO DC-ELAPSED-DAYS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-2       TO CP-FIRST-PAY-DATE
              MOVE DC-GREG-DATE-1-YMD  TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE CR-LF-EXPIRE-DATE          TO WS-WORK-DATE

           IF (CR-LF-CANCEL-EXIT-DATE NOT = ZEROS) AND
              (CR-LF-CANCEL-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO WS-WORK-DATE
           END-IF

           IF (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS) AND
              (CR-LF-CLAIM-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-LF-CLAIM-EXIT-DATE   TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-BIN-LF-END-DATE

           MOVE CR-AH-EXPIRE-DATE          TO WS-WORK-DATE

           IF (CR-AH-CANCEL-EXIT-DATE NOT = ZEROS) AND
              (CR-AH-CANCEL-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-WORK-DATE
           END-IF

           IF (CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS) AND
              (CR-AH-SETTLEMENT-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-AH-SETTLEMENT-EXIT-DATE
                                           TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-BIN-AH-END-DATE


           MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV
           MOVE '3'                        TO CP-PROCESS-TYPE
           MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD
           MOVE DTE-CLIENT                 TO CP-COMPANY-ID
           MOVE SPACES                     TO CP-ACCT-FLD-5
           MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD

           IF CR-LFTYP = '00' OR '  '
              CONTINUE
           ELSE
              PERFORM 0210-CALC-LIFE-UEP   THRU 0210-EXIT
           END-IF

           IF CR-AHTYP = '00' OR '  '
              CONTINUE
           ELSE
              PERFORM 0410-CALC-AH-UEP   THRU 0410-EXIT
           END-IF

           .

       0200-EXIT.
           EXIT.

       0210-CALC-LIFE-UEP.

      *    DISPLAY ' MADE IT TO 0210 '
           IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXL FROM +1 BY +1 UNTIL
                  (CLAS-INDEXL = CLAS-MAXL) OR
                  (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
              END-PERFORM
              IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
                 CONTINUE
              ELSE
                 DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE'
                 DISPLAY 'RETURN CODE - 0401'
                 MOVE 0401 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           COMPUTE WS-CERT-PREV-R78 = CR-LFPRM + CR-LFPRM-ALT
           MOVE WS-CERT-PREV-R78       TO WS-CERT-PREV-PRO
                                          WS-CERT-PREV-ST
                                          
           PERFORM VARYING VYR FROM +1 BY +1 UNTIL
               (VYR > +600) OR
      *        (WS-BIN-VAL-DATES (VYR) NOT < CP-CERT-EFF-DT)
               (WS-BIN-VAL-DATES (VYR) NOT < WS-BIN-ENTRY-DT)
           END-PERFORM

           IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'
              CONTINUE
           ELSE
             PERFORM VARYING VYR FROM VYR BY +1 UNTIL
               (VYR > +600) OR
               (WS-BIN-VAL-DATES (VYR) NOT < WS-BIN-LF-END-DATE)

              PERFORM 0220-CALC-LF-REM-TERM
                                       THRU 0220-EXIT
              MOVE ZEROS               TO WS-REM-AMT
              IF LF-REM-TRM2 > ZERO
                 PERFORM 2000-CALC-REM-AMT
                                       THRU 2999-CALC-REM-AMT-X
              END-IF
              COMPUTE WS-LF-INFORCE (IYR VYR) =
                  WS-LF-INFORCE (IYR VYR) + WS-REM-AMT
              PERFORM 0230-CALC-LF-UEP THRU 0230-EXIT
             END-PERFORM
           END-IF

           .

       0210-EXIT.
           EXIT.

       0220-CALC-LF-REM-TERM.

      *    DISPLAY ' MADE IT TO 0220 '
           MOVE WS-BIN-VAL-DATES (VYR) TO CP-VALUATION-DT

           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           IF ((CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L') AND
              CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L')
              ADD +1                   TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
           END-IF

           IF CP-TERM-IS-DAYS
              MOVE CR-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS
           ELSE
              MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS
           END-IF

           MOVE DTE-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND
                CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')
      *       MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM
              COMPUTE CP-REMAINING-TERM-1 =
                             CP-REMAINING-TERM-1 - 1
              COMPUTE CP-REMAINING-TERM-2 =
                             CP-REMAINING-TERM-2 - 1
           END-IF

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO LF-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2

           .

       0220-EXIT.
           EXIT.

       0230-CALC-LF-UEP.

      *    DISPLAY ' MADE IT TO 0230 '
           MOVE CR-LFPRM               TO CP-ORIGINAL-PREMIUM
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
           MOVE LF-REM-TRM1            TO CP-REMAINING-TERM
           MOVE CLAS-I-EP (CLAS-INDEXL)
                                       TO CP-EARNING-METHOD
           MOVE CR-LFPRM-ALT           TO CP-ALTERNATE-PREMIUM
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-LFTYP               TO CP-BENEFIT-CD
           MOVE LIFE-OVERRIDE-L1       TO CP-LIFE-OVERRIDE-CODE

           MOVE WS-BIN-CR-DT           TO CP-CERT-EFF-DT

           MOVE WS-BIN-VAL-DATES (VYR) TO CP-VALUATION-DT

           MOVE DTE-R78                TO CP-R78-OPTION
           MOVE SPACES                 TO CP-ACCT-FLD-5
           MOVE DTE-CLIENT             TO CP-COMPANY-ID
           MOVE CR-AGE                 TO CP-ISSUE-AGE
           MOVE CR-APR                 TO CP-LOAN-APR
           MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY
           MOVE CR-LOAN-TERM           TO CP-LOAN-TERM
           MOVE CR-CARRIER             TO CP-CARRIER

           IF CR-RATING-CLASS NOT = SPACE AND ZERO
              MOVE CR-RATING-CLASS     TO CP-CLASS-CODE
           END-IF

           MOVE STATE-SUB (CLAS-INDEXS)
                                       TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)
                                       TO CP-STATE-STD-ABBRV
           MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD
           MOVE CR-PMT-EXTENSION-DAYS  TO CP-TERM-OR-EXT-DAYS

           CALL 'ELUPRMX' USING CALCULATION-PASS-AREA

           COMPUTE WS-LF-ST  (IYR VYR) =
                   WS-LF-ST  (IYR VYR) + CP-STATE-U-PRM

           COMPUTE WS-LF-R78 (IYR VYR) =
                   WS-LF-R78 (IYR VYR) + CP-R78-U-PRM
      *            + CP-R78-U-PRM-ALT

           COMPUTE WS-LF-PRO (IYR VYR) =
                   WS-LF-PRO (IYR VYR) + CP-PRORATA-U-PRM
      *            + CP-PRORATA-U-PRM-ALT

           COMPUTE WS-LF-ST-EP (IYR VYR) =
                   WS-LF-ST-EP (IYR VYR) + (WS-CERT-PREV-ST
                   - CP-STATE-U-PRM)

           COMPUTE WS-LF-R78-EP (IYR VYR) =
                   WS-LF-R78-EP (IYR VYR) + (WS-CERT-PREV-R78
                   - CP-R78-U-PRM)

           COMPUTE WS-LF-PRO-EP (IYR VYR) =
                   WS-LF-PRO-EP (IYR VYR) + (WS-CERT-PREV-PRO
                   - CP-PRORATA-U-PRM)

           MOVE CP-STATE-U-PRM         TO WS-CERT-PREV-ST
           COMPUTE WS-CERT-PREV-R78 = CP-R78-U-PRM
              
           COMPUTE WS-CERT-PREV-PRO = CP-PRORATA-U-PRM

      *    IF (CR-DT > 20020630)
      *       AND (CR-DT < 20020801)
      *       AND (WS-BIN-VAL-DATES (VYR) = X'9BFF')
      *       AND (CR-ACCOUNT = '0000579700')
      *       MOVE CR-DT TO WS-DISPLAY-DATE
      *       MOVE CR-LF-TERM TO WS-DISPLAY-TERM
      *       MOVE LF-REM-TRM2 TO WS-DISPLAY-RTERM
      *       MOVE CR-LFPRM TO WS-DISPLAY-PRM
      *       MOVE WS-REM-AMT   TO WS-DISPLAY-RAMT
      *       MOVE CP-R78-U-PRM TO WS-DISPLAY-UEP
      *       DISPLAY ' LF ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
      *        WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
      *        WS-DISPLAY-PRM '   ' WS-DISPLAY-RAMT '   '
      *        WS-DISPLAY-UEP
      *    END-IF

           .

       0230-EXIT.
           EXIT.

       0410-CALC-AH-UEP.

      *    DISPLAY ' MADE IT TO 0410 '
           IF CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
                  (CLAS-INDEXA = CLAS-MAXA) OR
                  (CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA))
              END-PERFORM
              IF CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA)
                 CONTINUE
              ELSE
                 DISPLAY ' AH  BENEFIT ' CR-AHTYP ' NOT IN TABLE'
                 DISPLAY 'RETURN CODE - 0402'
                 MOVE 0402 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM VARYING VYR FROM +1 BY +1 UNTIL
               (VYR > +600) OR
      *        (WS-BIN-VAL-DATES (VYR) NOT < CP-CERT-EFF-DT)
               (WS-BIN-VAL-DATES (VYR) NOT < WS-BIN-ENTRY-DT)
           END-PERFORM

           MOVE CR-AHPRM            TO WS-CERT-PREV-R78
                                       WS-CERT-PREV-PRO
                                       WS-CERT-PREV-ST

           PERFORM VARYING VYR FROM VYR BY +1 UNTIL
               (VYR > +600) OR
               (WS-BIN-VAL-DATES (VYR) NOT < WS-BIN-AH-END-DATE)

              PERFORM 0420-CALC-AH-REM-TERM THRU 0420-EXIT
              PERFORM 0430-CALC-AH-UEP THRU 0430-EXIT
           END-PERFORM

           .

       0410-EXIT.
           EXIT.

       0420-CALC-AH-REM-TERM.

      *    DISPLAY ' MADE IT TO 0420 '
           MOVE WS-BIN-VAL-DATES (VYR) TO CP-VALUATION-DT

           MOVE CLAS-I-RL-AH (CLAS-INDEXA)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXA)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS

           MOVE DTE-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO AH-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2

           .

       0420-EXIT.
           EXIT.

       0430-CALC-AH-UEP.

      *    DISPLAY ' MADE IT TO 0430 '
           MOVE CR-AHPRM               TO CP-ORIGINAL-PREMIUM
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
           MOVE CR-AHAMT               TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE AH-REM-TRM1            TO CP-REMAINING-TERM
           MOVE CLAS-I-EP (CLAS-INDEXA)
                                       TO CP-EARNING-METHOD
           MOVE ZEROS                  TO CP-ALTERNATE-PREMIUM
           MOVE CLAS-I-RL-AH (CLAS-INDEXA)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-AHTYP               TO CP-BENEFIT-CD
           MOVE AH-OVERRIDE-L1         TO CP-AH-OVERRIDE-CODE

           MOVE WS-BIN-CR-DT           TO CP-CERT-EFF-DT

           MOVE WS-BIN-VAL-DATES (VYR) TO CP-VALUATION-DT

           MOVE DTE-R78                TO CP-R78-OPTION
           MOVE SPACES                 TO CP-ACCT-FLD-5
           MOVE DTE-CLIENT             TO CP-COMPANY-ID
           MOVE CR-AGE                 TO CP-ISSUE-AGE
           MOVE CR-APR                 TO CP-LOAN-APR
           MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY
           MOVE CR-LOAN-TERM           TO CP-LOAN-TERM
           MOVE CR-CARRIER             TO CP-CARRIER

           MOVE STATE-SUB (CLAS-INDEXS)
                                       TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)
                                       TO CP-STATE-STD-ABBRV
           MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD

           CALL 'ELUPRMX' USING CALCULATION-PASS-AREA

           COMPUTE WS-AH-R78 (IYR VYR) =
                   WS-AH-R78 (IYR VYR) + CP-R78-U-PRM
           COMPUTE WS-AH-PRO (IYR VYR) =
                   WS-AH-PRO (IYR VYR) + CP-PRORATA-U-PRM
           COMPUTE WS-AH-ST  (IYR VYR) =
                   WS-AH-ST  (IYR VYR) + CP-STATE-U-PRM

           COMPUTE WS-AH-R78-EP (IYR VYR) =
                   WS-AH-R78-EP (IYR VYR) + (WS-CERT-PREV-R78
                   - CP-R78-U-PRM)
           COMPUTE WS-AH-PRO-EP (IYR VYR) =
                   WS-AH-PRO-EP (IYR VYR) + (WS-CERT-PREV-PRO
                   - CP-PRORATA-U-PRM)
           COMPUTE WS-AH-ST-EP (IYR VYR) =
                   WS-AH-ST-EP (IYR VYR) + (WS-CERT-PREV-ST
                   - CP-STATE-U-PRM)

           MOVE CP-R78-U-PRM           TO WS-CERT-PREV-R78
           MOVE CP-PRORATA-U-PRM       TO WS-CERT-PREV-PRO
           MOVE CP-STATE-U-PRM         TO WS-CERT-PREV-ST

           IF (WS-BIN-VAL-DATES (VYR) = X'9BFF')
              AND (CR-ACCOUNT = '0000579700')
              MOVE CR-DT TO WS-DISPLAY-DATE
              MOVE CR-AH-TERM TO WS-DISPLAY-TERM
              MOVE AH-REM-TRM1 TO WS-DISPLAY-RTERM
              MOVE CR-AHPRM TO WS-DISPLAY-PRM
              MOVE CP-R78-U-PRM TO WS-DISPLAY-UEP
              DISPLAY ' AH ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
               WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
               WS-DISPLAY-PRM '   '
               WS-DISPLAY-UEP
           END-IF

           .

       0430-EXIT.
           EXIT.

       0500-EMPTY-TABLE.

           WRITE EXTRACT-RECORD-OUT-HD FROM WS-HEAD-RECORD
           MOVE WS-INIT-EXT-RECORD     TO WS-EXTRACT-RECORD
           DISPLAY ' MADE IT TO 0500 '
           PERFORM VARYING IYR FROM +1 BY +1 UNTIL
                   IYR > +600
               PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                      VYR > +600
                   MOVE WS-ISSUE-CCYYMM (IYR)
                                       TO EXT-ISSUE-CCYYMM
                   MOVE WS-VALUATION-CCYYMM (IYR VYR)
                                       TO EXT-VALUE-CCYYMM
                   MOVE WS-LF-INFORCE (IYR VYR)
                                       TO EXT-LF-INFORCE
                   MOVE WS-LF-WRITTEN (IYR VYR)
                                       TO EXT-LF-WRITTEN
                   MOVE WS-AH-WRITTEN (IYR VYR)
                                       TO EXT-AH-WRITTEN
                   MOVE WS-LF-REFUNDS (IYR VYR)
                                       TO EXT-LF-REFUNDS
                   MOVE WS-AH-REFUNDS (IYR VYR)
                                       TO EXT-AH-REFUNDS
                   MOVE WS-LF-R78     (IYR VYR)
                                       TO EXT-LF-R78
                   MOVE WS-LF-PRO     (IYR VYR)
                                       TO EXT-LF-PRO
                   MOVE WS-LF-ST      (IYR VYR)
                                       TO EXT-LF-ST
                   MOVE WS-LF-R78-EP  (IYR VYR)
                                       TO EXT-LF-R78-EP
                   MOVE WS-LF-PRO-EP  (IYR VYR)
                                       TO EXT-LF-PRO-EP
                   MOVE WS-LF-ST-EP   (IYR VYR)
                                       TO EXT-LF-ST-EP
                   MOVE WS-AH-R78     (IYR VYR)
                                       TO EXT-AH-R78
                   MOVE WS-AH-PRO     (IYR VYR)
                                       TO EXT-AH-PRO
                   MOVE WS-AH-ST      (IYR VYR)
                                       TO EXT-AH-ST
                   MOVE WS-AH-R78-EP  (IYR VYR)
                                       TO EXT-AH-R78-EP
                   MOVE WS-AH-PRO-EP  (IYR VYR)
                                       TO EXT-AH-PRO-EP
                   MOVE WS-AH-ST-EP   (IYR VYR)
                                       TO EXT-AH-ST-EP
                   MOVE WS-LF-CLAIM   (IYR VYR)
                                       TO EXT-LF-CLAIM
                   MOVE WS-AH-CLAIM   (IYR VYR)
                                       TO EXT-AH-CLAIM
                   IF (EXT-ISSUE-CCYYMM > EXT-VALUE-CCYYMM) OR
                      (ZERO = WS-LF-INFORCE (IYR VYR) AND
                              WS-LF-ST      (IYR VYR) AND
                              WS-AH-ST      (IYR VYR) AND
                              WS-LF-CLAIM   (IYR VYR) AND
                              WS-AH-CLAIM   (IYR VYR) AND
                              WS-LF-REFUNDS (IYR VYR) AND
                              WS-AH-REFUNDS (IYR VYR))
                      CONTINUE
                   ELSE
                      WRITE EXTRACT-RECORD-OUT
                                       FROM WS-EXTRACT-RECORD
                   END-IF
               END-PERFORM
           END-PERFORM

           .

       0500-EXIT.
           EXIT.

       0510-GET-REMAINING-TERM.

      *    DISPLAY ' MADE IT TO 0510 '
           CALL 'ELRTRMX' USING CALCULATION-PASS-AREA

           .

       0510-EXIT.
           EXIT.


       2000-CALC-REM-AMT.

      *    DISPLAY ' MADE IT TO 2000 '
           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
           END-IF

           MOVE +0                     TO WS-REM-AMT
           IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'
              IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
              GO TO 2900-DISPLAY-AMTS
           END-IF

           COMPUTE INTERMED ROUNDED = CR-LFAMT / CR-LF-TERM

           IF LF-REM-TRM2 = CR-LF-TERM
              IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
              GO TO 2900-DISPLAY-AMTS
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
              GO TO 2010-ORDINARY-REM
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'T'
              GO TO 2020-CALC-TEXAS-REM
           END-IF

           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
              MOVE 'N'                 TO
                             CLAS-I-CALC-TYPE (CLAS-INDEXL)
              GO TO 2030-CALC-NET-PAY-REM
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'N'
              GO TO 2030-CALC-NET-PAY-REM
           END-IF

           IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND
              (CR-RATING-CLASS NOT = 'L ')
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT GREATER THAN 19831031) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'MT'
               IF (CR-LF-TERM GREATER THAN +61) AND
                  (CR-DT  GREATER THAN 19830318) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'UT'
               IF (CR-LF-TERM GREATER THAN +62) AND
                  (CR-DT  GREATER THAN 19810831) AND
                  (CR-DT  LESS THAN 19830901) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'RI'
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT  GREATER THAN 19831231) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           .
       2010-ORDINARY-REM.

      *    DISPLAY ' MADE IT TO 2010 '
           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
               COMPUTE WS-REM-AMT ROUNDED =
                        (INTERMED * LF-REM-TRM2) + CR-LFAMT-ALT
           ELSE
               COMPUTE WS-REM-AMT ROUNDED = INTERMED * LF-REM-TRM2
           END-IF

           GO TO 2900-DISPLAY-AMTS

           .
       2020-CALC-TEXAS-REM.

      *    DISPLAY ' MADE IT TO 2020 '

           IF (CR-LF-TERM = ZEROS)
              OR (CR-PMT-FREQ = ZEROS)
              DISPLAY ' ZEROS KEY = ' CR-FULL-CONTROL
              '  PMT FREQ = ' CR-PMT-FREQ
              '  LF TERM  = ' CR-LF-TERM
              MOVE ZEROS               TO WS-REM-AMT
              GO TO 2900-DISPLAY-AMTS
           END-IF
           
           IF (CR-LF-TERM NOT = ZEROS)
              AND (CR-PMT-FREQ) NOT = ZEROS
           DIVIDE CR-LFAMT BY CR-LF-TERM
               GIVING TEX-FACT-1.
           DIVIDE LF-REM-TRM2 BY CR-PMT-FREQ
               GIVING TEX-FACT-2
               REMAINDER TEX-FACT-3.

           IF TEX-FACT-3 NOT = ZERO
               ADD +1                  TO TEX-FACT-2
           END-IF

           IF (TEX-FACT-2 * CR-PMT-FREQ) = CR-LF-TERM
               MOVE CR-LFAMT           TO WS-REM-AMT
           ELSE
               COMPUTE WS-REM-AMT ROUNDED =
                   (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ))
           END-IF

           GO TO 2900-DISPLAY-AMTS

           .
       2030-CALC-NET-PAY-REM.

      *    DISPLAY ' MADE IT TO 2030 '
           MOVE WS-BIN-CR-DT           TO  CP-CERT-EFF-DT

           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
              COMPUTE CP-ORIGINAL-BENEFIT = CR-LFAMT + CR-LFAMT-ALT
           ELSE
              MOVE CR-LFAMT            TO  CP-ORIGINAL-BENEFIT
           END-IF
           MOVE CR-APR                 TO  CP-LOAN-APR
           MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM
           MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM
           MOVE LF-REM-TRM2            TO  CP-REMAINING-TERM
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO  CP-BENEFIT-TYPE
           MOVE CLAS-I-EP (CLAS-INDEXL)
                                       TO  CP-EARNING-METHOD

           CALL 'ELRAMTX' USING CALCULATION-PASS-AREA

           MOVE CP-REMAINING-AMT       TO  WS-REM-AMT

           .
       2900-DISPLAY-AMTS.

      *    DISPLAY ' MADE IT TO 2900 '
      *    IF (CR-DT > 19921231) AND
      *       (CR-DT < 19940101) AND
      *    IF (WS-BIN-VAL-DATES (VYR) = X'95FF')
      *       MOVE CR-DT TO WS-DISPLAY-DATE
      *       MOVE CR-LF-TERM TO WS-DISPLAY-TERM
      *       MOVE LF-REM-TRM2 TO WS-DISPLAY-RTERM
      *       MOVE CR-LFPRM TO WS-DISPLAY-PRM
      *       IF CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L'
      *          ADD CR-LFAMT CR-LFAMT-ALT GIVING WS-DISPLAY-AMT
      *       ELSE
      *          MOVE CR-LFAMT     TO WS-DISPLAY-AMT
      *       END-IF
      *       MOVE WS-REM-AMT   TO WS-DISPLAY-RAMT
      *       DISPLAY '  ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
      *        WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
      *        WS-DISPLAY-AMT '   ' WS-DISPLAY-RAMT
      *    END-IF

           .
       2999-CALC-REM-AMT-X.
           EXIT.
       EJECT
       8510-DATE-CONVERSION.

      *    DISPLAY ' MADE IT TO 8510 '
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
