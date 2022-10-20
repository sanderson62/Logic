       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 ELVADS.
      *AUTHOR.  PABLO.
      *         OMAHA, NEBRASKA.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   HEALTH AND LIFE CO. OF OMAHA                    *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS. TRANSACTION VADS - VA DISCLOSURE PRINT TEST

      *                        C H A N G E   L O G
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 103107    2007092500001   PEMA  NEW PROGRAM
      * 100111    2011022800001   AJRA  NAPERSOFT
033015* 033015  IR2015032400002   PEMA  CORRECT CODE WHEN APR IS ZERO
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC  X(32) VALUE '********************************'.
       77  FILLER  PIC  X(32) VALUE '*   ELVADS WORKING STORAGE     *'.
       77  FILLER  PIC  X(32) VALUE '********* V/M 2.001 ************'.
       77  VA-RATE-SW                  PIC X  VALUE ' '.
           88  FOUND-VA-RATE               VALUE 'F'.
           88  NO-VA-RATE-FOUND            VALUE 'N'.
100111 77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
100111 77  E1                          PIC S999 COMP-3 VALUE +0.
100111 77  ARCH-STATUS                 PIC X.
100111     88  ARCH-OK                        VALUE "P".
100111     88  ARCH-FAIL                      VALUE "F".

       01  WS-CALC-SWITCHES.
           05  WS-TYPE-OF-CERT         PIC X.
               88  GROSS-CERT              VALUE 'G'.
               88  NET-CERT                VALUE 'N'.
               88  LEVEL-CERT              VALUE 'L'.
               88  AH-ONLY                 VALUE 'A'.
           05  WS-CALC-LF-GROSS        PIC X.
               88  CALC-LF-GROSS           VALUE 'Y'.
           05  WS-CALC-LF-NET          PIC X.
               88  CALC-LF-NET             VALUE 'Y'.
           05  WS-CALC-LF-LEVEL        PIC X.
               88  CALC-LF-LEVEL           VALUE 'Y'.
           05  WS-CALC-AH              PIC X.
               88  CALC-AH                 VALUE 'Y'.

       01  WS-CALC-AREA.
           05  WS-NET-PMT-1            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-2            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-3            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-4            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-1          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-2          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-3          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-4          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-1          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-2          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-3          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-4          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-1           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-2           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-3           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-4           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-1         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-2         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-3         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-4         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-1         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-2         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-3         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-4         PIC S9(7)V99 COMP-3 VALUE +0.
           05  ANNUAL-DISCOUNT         PIC S99V999  COMP-3 VALUE +0.
           05  CUR-PMT                 PIC S9(7)V99 COMP-3 VALUE +0.
           05  AMT-FINANCED            PIC S9(9)V99 COMP-3 VALUE +0.
           05  PRINC-AMT               PIC S9(9)V99 COMP-3 VALUE +0.
           05  LFPREM                  PIC S9(7)V99 COMP-3 VALUE +0.
           05  AHPREM                  PIC S9(7)V99 COMP-3 VALUE +0.
           05  LFAMT                   PIC S9(9)V99 COMP-3 VALUE +0.
           05  AHRATE                  PIC S99V9(5) COMP-3 VALUE +0.
           05  LF-RATE                 PIC S99V9(5) COMP-3 VALUE +0.
           05  OBRATE                  PIC S99V9(5) COMP-3 VALUE +0.
033015     05  I                       PIC SV9(10)   COMP-3 VALUE +0.
           05  Z                       PIC S999     COMP-3 VALUE +0.
           05  D                       PIC S9(5)    COMP-3 VALUE +0.
           05  T                       PIC S9(5)    COMP-3 VALUE +0.
           05  DISC                    PIC SV9(9)   COMP-3 VALUE +0.
           05  V-M                     PIC S99V9(9) COMP-3 VALUE +0.
           05  VM-N                    PIC S99V9(9) COMP-3 VALUE +0.
           05  VM                      PIC S99V9(9) COMP-3 VALUE +0.
           05  V                       PIC S99V9(9) COMP-3 VALUE +0.
           05  X                       PIC S99V9(9) COMP-3 VALUE +0.
           05  Y                       PIC S99V9(9) COMP-3 VALUE +0.
           05  N                       PIC S999     COMP-3 VALUE +0.
           05  M                       PIC S999     COMP-3 VALUE +0.
           05  R                       PIC S9(7)V99 COMP-3 VALUE +0.
           05  FILLER PIC X(12) VALUE ' SP IS NEXT '.
           05  SP                      PIC S99V9(9) COMP-3 VALUE +0.
033015     05  ANGLEN                  PIC S9(5)V9(13) COMP-3 VALUE +0.
           05  ANGLENY                 PIC S999V9(9) COMP-3 VALUE +0.
           05  ANGLEN-LEVEL            PIC S999V9(9) COMP-3 VALUE +0.
           05  ANGLENY-LEVEL           PIC S999V9(9) COMP-3 VALUE +0.
           05  LDPN                    PIC S9(5)V99  COMP-3 VALUE +0.
           05  AHPN                    PIC S9(5)V99  COMP-3 VALUE +0.
           05  FILLER PIC X(12) VALUE ' LF IS NEXT '.
           05  LF                      PIC S99V9(11) COMP-3 VALUE +0.
           05  PPY                     PIC S999V9(7) COMP-3 VALUE +0.

       01  WS-HOLD-RATE-EXP.
           05  WS-HOLD-RATE-EXP-AL     PIC X(11).
           05  WS-HOLD-RATE-EXP-DT REDEFINES
               WS-HOLD-RATE-EXP-AL     PIC 9(11).

       01  ERRATE-KEY.
           05  RATE-COMPANY-CD         PIC X       VALUE SPACE.
           05  RATE-STATE-CODE.
               10  RATE-ST-CODE        PIC XX      VALUE SPACES.
               10  RATE-ST-CLASS       PIC XX      VALUE SPACES.
               10  RATE-ST-DEV         PIC XXX     VALUE SPACES.
           05  RATE-L-AH-CODE.         
               10  RATE-L-AH           PIC X       VALUE SPACE.
               10  RATE-LAH-NUM        PIC XX      VALUE ZEROS.
           05  RATE-LIMITS.            
               10  RATE-HIGH-AGE       PIC 99      VALUE ZEROS.
               10  RATE-HIGH-AMT       PIC 9(6)    VALUE ZEROS.
               10  RATE-FUTURE         PIC XX      VALUE SPACES.
               10  RATE-SEX            PIC X       VALUE '9'.
           05  RATE-EXPIRY-DATE        PIC 9(11)   COMP-3.
                                       
       01  SAVE-ERRATE-KEY.            
           05  SVRT-COMPANY-CD         PIC X       VALUE SPACE.
           05  SVRT-STATE-CODE.        
               10  SVRT-ST-CODE        PIC XX      VALUE SPACES.
               10  SVRT-ST-CLASS       PIC XX      VALUE SPACES.
               10  SVRT-ST-DEV         PIC XXX     VALUE SPACES.
           05  SVRT-L-AH-CODE.         
               10  SVRT-L-AH           PIC X       VALUE SPACE.
               10  SVRT-LAH-NUM        PIC XX      VALUE ZEROS.
           05  SVRT-LIMITS.            
               10  SVRT-HIGH-AGE       PIC 99      VALUE ZEROS.
               10  SVRT-HIGH-AMT       PIC 9(6)    VALUE ZEROS.
               10  SVRT-FUTURE         PIC XX      VALUE SPACES.
               10  SVRT-SEX            PIC X       VALUE '9'.
           05  SVRT-EXPIRY-DATE        PIC 9(11)   COMP-3.

100111*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
100111                          COPY ELCZREC.
100111
100111 01  WS-ELLETR-KEY.
100111     05  WS-ELLETR-COMPANY-CD    PIC X.
100111     05  WS-ELLETR-LETTER-ID     PIC X(12).
100111     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
100111 01  WS-ELCNTL-KEY.
100111     05  WS-ELCNTL-COMPANY-ID    PIC XXX.
100111     05  WS-ELCNTL-REC-TYPE      PIC X.
100111     05  WS-ELCNTL-GENL.
100111         10  WS-ELCNTL-STATE     PIC XX  VALUE SPACES.
100111         10  WS-ELCNTL-BEN-CD.
100111             15  F               PIC X.
100111             15  WS-ELCNTL-CARR  PIC X.
100111     05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.
100111 01  WS-ELENCC-KEY.
100111     05  WS-ELENCC-COMPANY-CD    PIC X.
100111     05  WS-ELENCC-REC-TYPE      PIC X.
100111     05  WS-ELENCC-ENC-CODE      PIC X(5).
100111     05  F                       PIC X(9).
100111


       01  WORK-AREA.
           05  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
           05  PGM-NAME                PIC X(8).
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
100111     05  WS-COMPANY-CD           PIC X  VALUE LOW-VALUES.
100111     05  WS-ARCHIVE-NO           PIC S9(8)  COMP VALUE +0.
100111     05  WS-FOLLOW-UP-DT         PIC XX  VALUE LOW-VALUES.
100111     05  WS-RESEND-DT            PIC XX  VALUE LOW-VALUES.
100111     05  WS-AUTO-LAST-SCHED-DT   PIC XX  VALUE LOW-VALUES.
100111     05  WS-LAST-ACT-DT          PIC XX  VALUE LOW-VALUES.
100111     05  WS-ACTIVITY-DT          PIC XX  VALUE LOW-VALUES.
100111     05  WS-FORM                 PIC XXXX  VALUE SPACES.
100111     05  WS-MESSAGE              PIC X(50) VALUE SPACES.
100111
100111 01  VAPRINT-LINE.
100111     05  VA-LETTER               PIC X(06) VALUE SPACES.
100111     05  VA-PROC-ID              PIC X(04) VALUE SPACES.
100111     05  VA-PROC-NAME            PIC X(30) VALUE SPACES.
100111     05  VA-PROC-TITLE           PIC X(30) VALUE SPACES.
100111     05  VA-CSR-CODE             PIC X(4) VALUE SPACES.
100111     05  VA-CSR-NAME             PIC X(30) VALUE SPACES.
100111     05  VA-CSR-TITLE            PIC X(30) VALUE SPACES.
100111     05  VA-CARRIER              PIC X(01) VALUE SPACES.
100111     05  VA-GROUPING             PIC X(06) VALUE SPACES.
100111     05  VA-STATE                PIC X(02) VALUE SPACES.
100111     05  VA-ACCOUNT              PIC X(10) VALUE SPACES.
100111     05  VA-EFFDATE              PIC X(10) VALUE SPACES.
100111     05  VA-CERT-NO              PIC X(11) VALUE SPACES.
100111     05  VA-FIRSTNAME            PIC X(10) VALUE SPACES.
100111     05  VA-MIDINIT              PIC X(1) VALUE SPACES.
100111     05  VA-LASTNAME             PIC X(15) VALUE SPACES.
100111     05  VA-ADDR1                PIC X(30) VALUE SPACES.
100111     05  VA-ADDR2                PIC X(30) VALUE SPACES.
100111     05  VA-ADDR-CITY            PIC X(28) VALUE SPACES.
100111     05  VA-ADDR-ST              PIC X(2) VALUE SPACES.
100111     05  VA-ADDR-ZIP             PIC X(9) VALUE SPACES.
100111     05  VA-JNT-FIRSTNAME        PIC X(10) VALUE SPACES.
100111     05  VA-JNT-MIDINIT          PIC X(1) VALUE SPACES.
100111     05  VA-JNT-LASTNAME         PIC X(15) VALUE SPACES.
100111     05  VA-ARCH-NO              PIC 9(9) VALUE ZEROS.
100111     05  VA-ENC-LINE             PIC X(50) VALUE SPACES.
100111     05  VA-ATTACH               PIC X(50) VALUE SPACES.
100111     05  VA-STACK                PIC X(10) VALUE SPACES.
100111     05  VA-PRINT-NOW            PIC X(01) VALUE SPACES.
100111     05  VA-BORROWER             PIC X(30) VALUE SPACES.
           05  VA-BENE                 PIC X(30) VALUE SPACES.
           05  VA-ACCT-NAME            PIC X(30) VALUE SPACES.
           05  VA-SL                   PIC X VALUE SPACES.
           05  VA-JL                   PIC X VALUE SPACES.
           05  VA-SA                   PIC X VALUE SPACES.
           05  VA-JA                   PIC X VALUE SPACES.
           05  VA-GD                   PIC X VALUE SPACES.
           05  VA-NP                   PIC X VALUE SPACES.
           05  VA-7R                   PIC X VALUE SPACES.
           05  VA-LL                   PIC X VALUE SPACES.
           05  VA-TN                   PIC X VALUE SPACES.
           05  VA-14R                  PIC X VALUE SPACES.
           05  VA-14E                  PIC X VALUE SPACES.
           05  VA-30R                  PIC X VALUE SPACES.
           05  VA-30E                  PIC X VALUE SPACES.
           05  VA-CL-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-NET-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-GRS-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-LEV-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-NET-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-GRS-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-LEV-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-NET-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-GRS-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-LEV-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.

                                       COPY ERCRATE.
100111                                 COPY ELCVADS.
                                       COPY ELCDATE.
100111                                 COPY ELCTEXT.
100111                                 COPY ELCCNTL.
100111                                 COPY ERCARCH.
100111                                 COPY ELCENCC.
100111                                 COPY NSCASEXTR.

           01  WS-PASS-AREA.
100111         05  WS-PASS-AREA-LENGTH PIC S9(4) COMP VALUE +352.
100111         05  WS-COMMAREA.
100111             10  WS-PASS-VADS-REC    PIC X(350).

100111 LINKAGE SECTION.
100111
100111 01  DFHCOMMAREA                     PIC X(1000).

       PROCEDURE DIVISION.
100111
100111     MOVE DFHCOMMAREA          TO  WS-PASS-AREA.
100111
100111     MOVE WS-PASS-VADS-REC       TO VIRGINIA-DISCLOSURE
           MOVE SPACES                 TO WS-CALC-SWITCHES
100111     MOVE VD-COMPANY-CD          TO WS-COMPANY-CD
100111     MOVE VD-LETTER-ID           TO VA-LETTER
100111     MOVE VD-CARRIER             TO VA-CARRIER
100111     MOVE VD-GROUPING            TO VA-GROUPING
100111     MOVE VD-STATE               TO VA-STATE
100111     MOVE VD-ACCOUNT             TO VA-ACCOUNT
100111     MOVE VD-INSURED-LAST-NAME   TO VA-LASTNAME
100111     MOVE VD-INSURED-FIRST-NAME  TO VA-FIRSTNAME
100111     MOVE VD-INSURED-MIDDLE-INIT TO VA-MIDINIT
100111     MOVE VD-INSURED-ADDRESS-1   TO VA-ADDR1
100111     MOVE VD-INSURED-ADDRESS-2   TO VA-ADDR2
100111     MOVE VD-INSURED-CITY        TO VA-ADDR-CITY
100111     MOVE VD-INSURED-STATE       TO VA-ADDR-ST
100111     MOVE VD-INSURED-ZIP-CODE    TO VA-ADDR-ZIP
100111     MOVE VD-JOINT-LAST-NAME     TO VA-JNT-LASTNAME
100111     MOVE VD-JOINT-FIRST-NAME    TO VA-JNT-FIRSTNAME
100111     MOVE VD-JOINT-MIDDLE-INIT   TO VA-JNT-MIDINIT

100111     IF VD-LF-BENEFIT-CD = '01' OR '03'
              SET GROSS-CERT           TO TRUE
              SET CALC-LF-GROSS        TO TRUE
              SET CALC-LF-NET          TO TRUE
           END-IF

100111     IF VD-LF-BENEFIT-CD = '05' OR '06'
              SET NET-CERT             TO TRUE
              SET CALC-LF-NET          TO TRUE
           END-IF

100111     IF VD-LF-BENEFIT-CD = '02' OR '04'
              SET CALC-LF-LEVEL        TO TRUE
              SET LEVEL-CERT           TO TRUE
           END-IF

100111     IF VD-AH-BENEFIT-CD NOT = '00' AND '  '
              SET CALC-AH              TO TRUE
           END-IF

100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              SET AH-ONLY              TO TRUE
           END-IF

100111     MOVE VD-LF-PREMIUM-AMT      TO LFPREM
100111     MOVE VD-AH-PREMIUM-AMT      TO AHPREM
100111     MOVE VD-LF-BENEFIT-AMT      TO LFAMT
100111     IF VD-LF-BENEFIT-CD NOT = '00' AND '  '
100111        MOVE VD-LF-TERM          TO N
           ELSE
100111        MOVE VD-AH-TERM          TO N
           END-IF
100111     MOVE VD-LOAN-TERM           TO M
           IF M = 0
              MOVE N                   TO M
           END-IF
100111     COMPUTE I = VD-LOAN-APR / +1200
033015     if i = zeros
033015        move .0000000001         to i
033015     end-if
           MOVE ZEROS                  TO Z
           MOVE +30                    TO T
           MOVE +3.63                  TO ANNUAL-DISCOUNT
100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
100111     MOVE VD-1ST-PMT-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              COMPUTE D = (DC-ELAPSED-MONTHS * 30) + DC-ODD-DAYS-OVER
           END-IF

100111     IF VD-AH-BENEFIT-CD NOT = '  ' AND '00'
100111        MOVE VD-AH-RATE          TO AHRATE
           END-IF

           IF GROSS-CERT OR LEVEL-CERT
100111        COMPUTE LF-RATE = VD-LF-RATE / (N / 12)
           END-IF

           IF NET-CERT
100111        MOVE VD-LF-RATE          TO OBRATE
           END-IF

           IF GROSS-CERT
              PERFORM 1000-GET-ERRATE  THRU 1000-EXIT
           END-IF

           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE DISC = ANNUAL-DISCOUNT / 1200

           COMPUTE V-M = (1 / (1 + I)) ** N
           COMPUTE VM = (1 / (1 + I)) ** M
           COMPUTE VM-N = (1 / (1 + I)) ** (M - N)
           COMPUTE V = 1 / (1 + I)
           COMPUTE X = 1 / (1 + DISC)
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLENY = ANGLEN / Y

           COMPUTE LF = ((1 + (Z * I)) / I)
              * (((X ** N - 1) / (X - 1))
              - ((X ** N * VM-N - VM) / (X * (1 + I) - 1)))
              * (OBRATE / 1000)

      *    COMPUTE LFA = (1 + (Z * I)) / I
      *    COMPUTE LFB = X ** N - 1
      *    COMPUTE LFC = X - 1
      *    COMPUTE LFD = LFB / LFC
      *    COMPUTE LFE = (((X ** N) * (V ** (M - N))) - (V ** M)) /
      *       ((X * (1 + I)) - 1)
      *    COMPUTE LF = LFA * (LFD - LFE) * (OBRATE / 1000)


           COMPUTE R = LFAMT / (ANGLENY - LF - N * AHRATE / 100)
           COMPUTE SP = (LF * 100) / (ANGLENY * (1 + (Z * I)))
           COMPUTE LDPN = SP * R * ANGLENY * (1 + (Z * I)) / 100
           COMPUTE AHPN = R * N * AHRATE / 100
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLENY = ANGLEN / Y

           IF GROSS-CERT
100111        COMPUTE CUR-PMT = LFAMT / VD-LF-TERM
              COMPUTE AMT-FINANCED = CUR-PMT * ANGLEN
              COMPUTE PRINC-AMT = AMT-FINANCED - LFPREM - AHPREM
           ELSE
              IF NET-CERT
                 COMPUTE PRINC-AMT = LFAMT - LFPREM - AHPREM
                 MOVE LFAMT TO AMT-FINANCED
              END-IF
           END-IF

           MOVE 000000.00              TO VA-CL-NET-AMT
                                          VA-CL-GRS-AMT
                                          VA-CL-LEV-AMT
                                          VA-CD-NET-AMT
                                          VA-CD-GRS-AMT
                                          VA-CD-LEV-AMT
                                          VA-CLD-NET-AMT
                                          VA-CLD-GRS-AMT
                                          VA-CLD-LEV-AMT
                                          VA-NO-NET-AMT
                                          VA-NO-GRS-AMT
                                          VA-NO-LEV-AMT
           MOVE 00000.00               TO VA-CL-NET-PMT
                                          VA-CL-GRS-PMT
                                          VA-CL-LEV-PMT
                                          VA-CD-NET-PMT
                                          VA-CD-GRS-PMT
                                          VA-CD-LEV-PMT
                                          VA-CLD-NET-PMT
                                          VA-CLD-GRS-PMT
                                          VA-CLD-LEV-PMT
                                          VA-NO-NET-PMT
                                          VA-NO-GRS-PMT
                                          VA-NO-LEV-PMT
           MOVE 00000.00               TO VA-CL-NET-PRM
                                          VA-CL-GRS-PRM
                                          VA-CL-LEV-PRM
                                          VA-CD-NET-PRM
                                          VA-CD-GRS-PRM
                                          VA-CD-LEV-PRM
                                          VA-CLD-NET-PRM
                                          VA-CLD-GRS-PRM
                                          VA-CLD-LEV-PRM
                                          VA-NO-NET-PRM
                                          VA-NO-GRS-PRM
                                          VA-NO-LEV-PRM

           IF NET-CERT
              PERFORM 2000-BUILD-NET-AMTS
                                       THRU 2000-EXIT
           END-IF

           IF GROSS-CERT
              PERFORM 2000-BUILD-NET-AMTS
                                       THRU 2000-EXIT
              PERFORM 3000-BUILD-GROSS-AMTS
                                       THRU 3000-EXIT
           END-IF

           IF LEVEL-CERT
              PERFORM 4000-BUILD-LEVEL-AMTS
                                       THRU 4000-EXIT
           END-IF

           IF AH-ONLY
              PERFORM 3500-BUILD-AH-AMTS
                                       THRU 3500-EXIT
           END-IF

100111     STRING VD-INSURED-FIRST-NAME ' '
100111        VD-INSURED-LAST-NAME DELIMITED BY '  '
              INTO VA-BORROWER
           END-STRING
100111     MOVE VD-CERT-NO             TO VA-CERT-NO
100111     MOVE VD-ACCOUNT-NAME        TO VA-ACCT-NAME
100111     MOVE VD-BENEFICIARY-NAME    TO VA-BENE
100111     IF VD-LF-BENEFIT-CD = '03' OR '04' OR '06'
              MOVE 'X'                 TO VA-JL
           ELSE
100111        IF VD-LF-BENEFIT-CD NOT = '00' AND '  '
                 MOVE 'X'              TO VA-SL
              END-IF
           END-IF
           IF GROSS-CERT
              MOVE 'X'                 TO VA-GD
           ELSE
              IF LEVEL-CERT
                 MOVE 'X'              TO VA-LL
              ELSE
                 IF NET-CERT
                    MOVE 'X'           TO VA-NP
                 END-IF
              END-IF
           END-IF

           IF CALC-AH
              MOVE 'X'                 TO VA-SA
              EVALUATE TRUE
100111         WHEN VD-AH-BENEFIT-CD = '53' OR '54' OR
                 '55' OR '56' OR '57'
                 MOVE 'X'              TO VA-JA
                 MOVE ' '              TO VA-SA
100111         WHEN VD-AH-BENEFIT-CD = '03' OR '53'
                 MOVE 'X'              TO VA-7R
100111         WHEN VD-AH-BENEFIT-CD = '01' OR '54'
                 MOVE 'X'              TO VA-14R
100111         WHEN VD-AH-BENEFIT-CD = '05' OR '56'
                 MOVE 'X'              TO VA-14E
100111         WHEN VD-AH-BENEFIT-CD = '04' OR '55'
                 MOVE 'X'              TO VA-30R
100111         WHEN VD-AH-BENEFIT-CD = '02' OR '57'
                 MOVE 'X'              TO VA-30E
              END-EVALUATE
           END-IF

      *    MOVE 'X'                    TO VA-SL VA-JL VA-SA VA-JA VA-GD
      *       VA-NP VA-7R VA-LL VA-TN VA-14R VA-14E VA-30R VA-30E



100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACE                  TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT

           IF NO-CONVERSION-ERROR
              STRING DC-GREG-DATE-CYMD-R (5:2) '/'
                     DC-GREG-DATE-CYMD-R (7:2) '/'
                     DC-GREG-DATE-CYMD-R (1:4) DELIMITED BY SIZE
100111           INTO VA-EFFDATE
              END-STRING
           ELSE
100111        MOVE '00/00/0000'        TO VA-EFFDATE
           END-IF

100111     PERFORM 4100-CREATE-ARCHIVE THRU 4100-EXIT.

           GO TO 8000-RETURN

           .
       1000-GET-ERRATE.

           MOVE ALL '9'                TO ERRATE-KEY
100111     MOVE VD-COMPANY-CD          TO RATE-COMPANY-CD
           MOVE ZEROS                  TO RATE-STATE-CODE
100111     MOVE VD-STATE               TO RATE-ST-CODE
           MOVE 'L'                    TO RATE-L-AH

           IF GROSS-CERT
100111        IF VD-LF-BENEFIT-CD = '01'
                 MOVE '05'             TO RATE-LAH-NUM
              ELSE
                 MOVE '06'             TO RATE-LAH-NUM
              END-IF
           END-IF

           IF NET-CERT
100111        IF VD-LF-BENEFIT-CD = '05'
                 MOVE '01'             TO RATE-LAH-NUM
              ELSE
                 MOVE '03'             TO RATE-LAH-NUM
              END-IF
           END-IF

           MOVE 99                     TO RATE-HIGH-AGE
100111     MOVE VD-LF-BENEFIT-AMT    TO RATE-HIGH-AMT

100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACE                  TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT

           IF NO-CONVERSION-ERROR
              MOVE ZEROS               TO RATE-EXPIRY-DATE
                                          WS-HOLD-RATE-EXP-DT
              MOVE DC-GREG-DATE-CYMD   TO WS-HOLD-RATE-EXP-AL(4:8)
              MOVE WS-HOLD-RATE-EXP-DT TO RATE-EXPIRY-DATE
              MOVE ERRATE-KEY          TO SAVE-ERRATE-KEY
           ELSE
              MOVE 00099999999         TO RATE-EXPIRY-DATE
           END-IF

           PERFORM 1010-STARTBR-ERRATE THRU 1010-EXIT

           IF RESP-NORMAL
             PERFORM UNTIL
              (FOUND-VA-RATE)
              OR (NO-VA-RATE-FOUND)
              OR (NOT RESP-NORMAL)
              PERFORM 1020-READNEXT-ERRATE
                                       THRU 1020-EXIT
              IF (RT-STATE-CODE = SVRT-STATE-CODE)
                 AND (RT-L-AH-CODE = SVRT-L-AH-CODE)
                 IF (RT-HIGH-AMT > SVRT-HIGH-AMT)
                    AND (RT-EXPIRY-DATE > SVRT-EXPIRY-DATE)
                    SET FOUND-VA-RATE     TO TRUE
                    IF SVRT-LAH-NUM = '05' OR '06'
                       MOVE RT-DISCOUNT-OB-RATE
                                       TO OBRATE
                    ELSE
                       COMPUTE LF-RATE = RT-L-RATE (N) / (N / 12)
                    END-IF
                 END-IF
              ELSE
                 SET NO-VA-RATE-FOUND  TO TRUE
              END-IF
             END-PERFORM
           END-IF

           .
       1000-EXIT.
           EXIT.

       1010-STARTBR-ERRATE.

           EXEC CICS STARTBR
              DATASET    ('ERRATE')
              RIDFLD     (ERRATE-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       1010-EXIT.
           EXIT.

       1020-READNEXT-ERRATE.

           EXEC CICS READNEXT
              DATASET    ('ERRATE')
              RIDFLD     (ERRATE-KEY)
              INTO       (RATE-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       1020-EXIT.
           EXIT.

       2000-BUILD-NET-AMTS.

           COMPUTE WS-NET-PMT-1 = PRINC-AMT / (ANGLEN - LF)

           IF AHRATE NOT = ZEROS
              COMPUTE WS-NET-PMT-2 = PRINC-AMT /
               (ANGLEN - (N * AHRATE / 100))
           END-IF

           COMPUTE WS-NET-PMT-3 = PRINC-AMT /
             (ANGLEN - LF - (N * AHRATE / 100))

           COMPUTE WS-NET-PMT-4 = PRINC-AMT / ANGLEN

           COMPUTE WS-NET-PREM-1 = SP * WS-NET-PMT-1 * ANGLEN *
             (1 + (Z * I)) / 100

           COMPUTE WS-NET-PREM-2 = WS-NET-PMT-2 * N * AHRATE / 100

           COMPUTE WS-NET-PREM-3 = (SP * WS-NET-PMT-3 * ANGLEN *
             (1 + (Z * I)) / 100) + (WS-NET-PMT-3 * N * AHRATE / 100)

100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD = '00' OR '  ')
              COMPUTE VA-CL-NET-AMT = PRINC-AMT + WS-NET-PREM-1
              MOVE WS-NET-PMT-1        TO VA-CL-NET-PMT
              MOVE WS-NET-PREM-1       TO VA-CL-NET-PRM
           END-IF

100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              IF WS-NET-PREM-2 NOT = ZEROS
                 COMPUTE VA-CD-NET-AMT = PRINC-AMT + WS-NET-PREM-2
              END-IF
              MOVE WS-NET-PMT-2        TO VA-CD-NET-PMT
              MOVE WS-NET-PREM-2       TO VA-CD-NET-PRM
           END-IF

100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CLD-NET-AMT = PRINC-AMT + WS-NET-PREM-3
              MOVE WS-NET-PMT-3        TO VA-CLD-NET-PMT
              MOVE WS-NET-PREM-3       TO VA-CLD-NET-PRM
           END-IF

           MOVE PRINC-AMT              TO VA-NO-NET-AMT
           MOVE WS-NET-PMT-4           TO VA-NO-NET-PMT
           MOVE WS-NET-PREM-4           TO VA-NO-NET-PRM

           .
       2000-EXIT.
           EXIT.

       3000-BUILD-GROSS-AMTS.

      *    COMPUTE WS-GROSS-PMT-1 = PRINC-AMT /
      *      (ANGLENY - (N * (N + ((D - T) / T)) / 12 * LF-RATE / 100))

      *    COMPUTE WS-GROSS-PMT-1 = AMT-FINANCED / ANGLEN

           COMPUTE WS-GROSS-PMT-1 = PRINC-AMT / (ANGLEN -
             (N * N / 12 * LF-RATE / 100))

      *    COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
      *      (ANGLENY - (N * AHRATE / 100))

           COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
             (ANGLEN - (N * AHRATE / 100))

      *    COMPUTE WS-GROSS-PMT-3 = PRINC-AMT /
      *      (ANGLENY - ((N * (N + ((D - T) / T)) /
      *      12 * LF-RATE / 100) + (N * AHRATE / 100)))

      *    COMPUTE WS-GROSS-PMT-3 = (AMT-FINANCED / ANGLEN)
      *      + (N * AHRATE / 100)

           COMPUTE WS-GROSS-PMT-3 = PRINC-AMT /
              (ANGLEN - ((N * N / 12 * LF-RATE / 100)
             + (N * AHRATE / 100)))

      *    COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLENY
           COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLEN

      *    COMPUTE WS-GROSS-PREM-1 = WS-GROSS-PMT-1 *
      *      N * (N + ((D - T) / T)) / 12 * LF-RATE / 100

           COMPUTE WS-GROSS-PREM-1 = WS-GROSS-PMT-1 *
             N * N / 12 * LF-RATE / 100

           COMPUTE WS-GROSS-PREM-2 = WS-GROSS-PMT-2 * N * AHRATE / 100

      *    COMPUTE WS-GROSS-PREM-3 = (WS-GROSS-PMT-3 * N *
      *      (N + ((D - T) / T)) / 12 * LF-RATE / 100) +
      *      (WS-GROSS-PMT-3 * N * AHRATE / 100)

           COMPUTE WS-GROSS-PREM-3 = (WS-GROSS-PMT-3 * N *
             N / 12 * LF-RATE / 100) +
             (WS-GROSS-PMT-3 * N * AHRATE / 100)

           MOVE ZEROS                  TO WS-GROSS-PREM-4

100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD = '00' OR '  ')
              COMPUTE VA-CL-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-1
              MOVE WS-GROSS-PMT-1      TO VA-CL-GRS-PMT
              MOVE WS-GROSS-PREM-1     TO VA-CL-GRS-PRM
           END-IF
           
100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-2
              MOVE WS-GROSS-PMT-2      TO VA-CD-GRS-PMT
              MOVE WS-GROSS-PREM-2     TO VA-CD-GRS-PRM
           END-IF

100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CLD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-3
              MOVE WS-GROSS-PMT-3      TO VA-CLD-GRS-PMT
              MOVE WS-GROSS-PREM-3     TO VA-CLD-GRS-PRM
           END-IF

           COMPUTE VA-NO-GRS-AMT = PRINC-AMT
           MOVE WS-GROSS-PMT-4           TO VA-NO-GRS-PMT
           MOVE WS-GROSS-PREM-4           TO VA-NO-GRS-PRM

           .
       3000-EXIT.
           EXIT.

       3500-BUILD-AH-AMTS.

100111     MOVE VD-AH-BENEFIT-AMT    TO CUR-PMT
           COMPUTE AMT-FINANCED = CUR-PMT * ANGLEN
           COMPUTE PRINC-AMT = AMT-FINANCED - LFPREM - AHPREM

           COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
             (ANGLEN - (N * AHRATE / 100))

           COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLEN

           COMPUTE WS-GROSS-PREM-2 = WS-GROSS-PMT-2 * N * AHRATE / 100

           MOVE ZEROS                  TO WS-GROSS-PREM-4

           COMPUTE VA-CD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-2
           COMPUTE VA-NO-GRS-AMT = PRINC-AMT

      *    MOVE WS-GROSS-PMT-1           TO VA-CL-GRS-PMT
           MOVE WS-GROSS-PMT-2           TO VA-CD-GRS-PMT
      *    MOVE WS-GROSS-PMT-3           TO VA-CLD-GRS-PMT
           MOVE WS-GROSS-PMT-4           TO VA-NO-GRS-PMT

      *    MOVE WS-GROSS-PREM-1           TO VA-CL-GRS-PRM
           MOVE WS-GROSS-PREM-2           TO VA-CD-GRS-PRM
      *    MOVE WS-GROSS-PREM-3           TO VA-CLD-GRS-PRM
           MOVE WS-GROSS-PREM-4           TO VA-NO-GRS-PRM

           .
       3500-EXIT.
           EXIT.

       4000-BUILD-LEVEL-AMTS.

           MOVE 1                      TO N
           COMPUTE PPY = 1 / M * 12
           COMPUTE T = 360 * M / 12
           COMPUTE D = D + T
100111     COMPUTE I = VD-LOAN-APR / PPY / 100
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** 1) / I
           COMPUTE ANGLENY = ANGLEN / Y
           COMPUTE PRINC-AMT = (LFAMT * ANGLENY) - LFPREM
           COMPUTE WS-LEVEL-PMT-1 = PRINC-AMT / (ANGLENY -
              (1 * ((1 + (D - T) / T) / PPY) * (LF-RATE / 100)))
           COMPUTE WS-LEVEL-PMT-4 = PRINC-AMT / ANGLENY
           COMPUTE WS-LEVEL-PREM-1 = WS-LEVEL-PMT-1 *
              (1 * ((1 + (D - T) / T) / PPY) * (LF-RATE / 100))
           MOVE ZEROS                  TO WS-LEVEL-PREM-4

           MOVE WS-LEVEL-PMT-1           TO VA-CL-LEV-PMT
           MOVE WS-LEVEL-PMT-4           TO VA-NO-LEV-PMT

           MOVE WS-LEVEL-PREM-1           TO VA-CL-LEV-PRM
           MOVE WS-LEVEL-PREM-4           TO VA-NO-LEV-PRM

           COMPUTE VA-CL-LEV-AMT = PRINC-AMT + WS-LEVEL-PREM-1
           MOVE PRINC-AMT              TO VA-NO-LEV-AMT

           .
       4000-EXIT.
           EXIT.
        
100111     
100111 4100-CREATE-ARCHIVE.
100111 
100111     PERFORM 4125-GET-CSR-INFO THRU 4125-EXIT.
100111     PERFORM 4250-GET-ELLETR   THRU 4250-EXIT.
100111     PERFORM 4500-GET-ARCH-NO  THRU 4500-EXIT.
100111     PERFORM 4700-GET-ELENCC   THRU 4700-EXIT.
100111     PERFORM 5700-WRITE-EXTR THRU 5700-EXIT.
100111 
100111 4100-EXIT.
100111     EXIT.
100111
100111 4125-GET-CSR-INFO.
100111
100111     MOVE VD-PROC-ID             TO VA-PROC-ID
100111     MOVE VD-CSR-ID              TO VA-CSR-CODE
100111     MOVE VD-COMP-ID             TO WS-ELCNTL-COMPANY-ID
100111     MOVE '2'                    TO WS-ELCNTL-REC-TYPE
100111     MOVE VD-CSR-ID              TO WS-ELCNTL-GENL
100111     MOVE +0                     TO WS-ELCNTL-SEQ-NO
100111     EXEC CICS READ
100111        INTO    (CONTROL-FILE)
100111        DATASET ('ELCNTL')
100111        RIDFLD  (WS-ELCNTL-KEY)
100111        RESP    (WS-RESPONSE)
100111     END-EXEC
100111     IF RESP-NORMAL           
100111        MOVE CF-PROCESSOR-NAME   TO VA-CSR-NAME
100111                                    VA-PROC-NAME
100111        MOVE VD-CSR-ID           TO VA-CSR-TITLE
100111                                    VA-PROC-TITLE
100111     END-IF
100111
100111     IF VD-PROC-ID NOT = VD-CSR-ID
100111        MOVE VD-COMP-ID          TO WS-ELCNTL-COMPANY-ID
100111        MOVE '2'                 TO WS-ELCNTL-REC-TYPE
100111        MOVE VD-PROC-ID          TO WS-ELCNTL-GENL
100111        MOVE +0                  TO WS-ELCNTL-SEQ-NO
100111        EXEC CICS READ
100111           INTO    (CONTROL-FILE)
100111           DATASET ('ELCNTL')
100111           RIDFLD  (WS-ELCNTL-KEY)
100111           RESP    (WS-RESPONSE)
100111        END-EXEC
100111        IF RESP-NORMAL           
100111           MOVE CF-PROCESSOR-NAME   TO VA-PROC-NAME
100111           MOVE VD-PROC-ID          TO VA-PROC-TITLE
100111        END-IF
100111     END-IF
100111
100111     .
100111 4125-EXIT.
100111     EXIT.
100111     
100111 4250-GET-ELLETR.
100111
100111     MOVE WS-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
100111     MOVE VD-LETTER-ID           TO WS-ELLETR-LETTER-ID
100111     MOVE +0                     TO WS-ELLETR-SEQ-NO
100111     MOVE SPACES                 TO W-Z-CONTROL-DATA
100111
100111     EXEC CICS READ                                               
100111          DATASET    ('ELLETR') 
100111          INTO       (TEXT-FILES)
100111          RIDFLD     (WS-ELLETR-KEY)
100111          RESP       (WS-RESPONSE)
100111          GTEQ
100111     END-EXEC
100111
100111     IF RESP-NORMAL
100111        IF (LETTER-FILE-TEXT)
100111           AND (VD-LETTER-ID = TX-LETTER-NO)
100111           AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
100111           PERFORM 4280-PROCESS-Z-CONTROLS
100111                                 THRU 4280-EXIT
100111        END-IF
100111     END-IF
100111
100111     .
100111 4250-EXIT.
100111     EXIT.
100111
100111
100111 4280-PROCESS-Z-CONTROLS.
100111
100111     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
100111
100111     .
100111 4280-EXIT.
100111     EXIT.
100111
100111     
100111 4500-GET-ARCH-NO.
100111
100111     MOVE VD-COMP-ID             TO WS-ELCNTL-COMPANY-ID
100111     MOVE '1'                    TO WS-ELCNTL-REC-TYPE
100111     MOVE SPACES                 TO WS-ELCNTL-GENL
100111     MOVE +0                     TO WS-ELCNTL-SEQ-NO
100111
100111     EXEC CICS READ
100111        INTO    (CONTROL-FILE)
100111        DATASET ('ELCNTL')
100111        RIDFLD  (WS-ELCNTL-KEY)
100111        UPDATE
100111        RESP    (WS-RESPONSE)
100111     END-EXEC
100111
100111     IF RESP-NORMAL
100111        AND (CF-COMPANY-ID  = VD-COMP-ID)
100111        AND (CF-RECORD-TYPE = '1')
100111        ADD +1                   TO CF-CREDIT-LAST-ARCH-NUM
100111        MOVE CF-CREDIT-LAST-ARCH-NUM
100111                                 TO WS-ARCHIVE-NO
100111        EXEC CICS REWRITE
100111           FROM    (CONTROL-FILE)
100111           DATASET ('ELCNTL')
100111        END-EXEC
100111     ELSE
100111        MOVE +0                  TO WS-ARCHIVE-NO
100111     END-IF
100111
100111*** IF AN ARCHIVE NUMBER IS PASSED TO THIS PGM THEN
100111*** IT IS SAVED IN VA-ARCH-NO.  THE VA DISC LETTER
100111*** WILL CREATE A DIFFERENT ARCH NO.
100111     IF VD-ARCHIVE-NO GREATER THAN ZERO
100111         MOVE VD-ARCHIVE-NO      TO VA-ARCH-NO
100111     ELSE
100111         MOVE WS-ARCHIVE-NO      TO VA-ARCH-NO
100111     END-IF
100111    .
100111 4500-EXIT.
100111     EXIT.
100111
100111
100111 4700-GET-ELENCC.
100111
072312     MOVE SPACES                 TO WS-ELENCC-KEY
100111     MOVE WS-COMPANY-CD          TO WS-ELENCC-COMPANY-CD
100111     MOVE '2'                    TO WS-ELENCC-REC-TYPE
100111     MOVE W-ENCLOSURE-CD         TO WS-ELENCC-ENC-CODE
100111     PERFORM VARYING E1 FROM +1 BY +1 UNTIL
100111        (WS-ELENCC-ENC-CODE (E1:1) = ' ')
100111        OR (E1 > +5)
100111     END-PERFORM
100111
100111     IF E1 < +5
100111        MOVE VD-STATE            TO WS-ELENCC-ENC-CODE (E1:2)
100111     END-IF
100111
100111     EXEC CICS READ
100111         DATASET   ('ELENCC')
100111         INTO      (ENCLOSURE-CODES)
100111         RIDFLD    (WS-ELENCC-KEY)
100111         RESP      (WS-RESPONSE)
100111     END-EXEC
100111
100111     IF NOT RESP-NORMAL
100111        MOVE W-ENCLOSURE-CD    TO WS-ELENCC-ENC-CODE
100111        EXEC CICS READ
100111            DATASET   ('ELENCC')
100111            INTO      (ENCLOSURE-CODES)
100111            RIDFLD    (WS-ELENCC-KEY)
100111            RESP      (WS-RESPONSE)
100111        END-EXEC
100111     END-IF
100111    
100111     IF RESP-NORMAL
100111        MOVE NC-OUTPUT-STACK     TO VA-STACK
100111        MOVE NC-ENCLOSURE-LINE   TO VA-ENC-LINE
100111        MOVE NC-ATTACHMENTS      TO VA-ATTACH
100111        MOVE 'N'                 TO VA-PRINT-NOW
100111     END-IF
100111
100111     .
100111 4700-EXIT.
100111     EXIT.
100111
100111
100111 5000-BUILD-ERARCH.
100111
100111* DATA SOURCE MEANINGS  BL-DATA-SRCE
100111*    1) FROM ACCT MAINT
100111*    2) FROM CERT UPDATE
100111*    3) FROM COMP MAINT
100111*    4) FROM REVIEW AND CORRECTIONS
100111******************************************************************
100111
100111
100111     MOVE 'LA'                   TO LETTER-ARCHIVE
100111
100111     MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO
100111                                    LA-ARCHIVE-NO-A2
100111                                    LA-ARCHIVE-NO-A3
100111                                    LA-ARCHIVE-NO-A4
100111                                    LA-ARCHIVE-NO-A5
100111                                    LA-ARCHIVE-NO-A6
100111
100111     MOVE VD-COMPANY-CD          TO LA-COMPANY-CD
100111                                    LA-COMPANY-CD-A2
100111                                    LA-COMPANY-CD-A3
100111                                    LA-COMPANY-CD-A4
100111                                    LA-COMPANY-CD-A5
100111                                    LA-COMPANY-CD-A6
100111     MOVE VD-CARRIER             TO LA-CARRIER-A2
100111                                    LA-CARRIER-A3
100111                                    LA-CARRIER-A4
100111                                    LA-CARRIER-A5
100111     MOVE VD-GROUPING            TO LA-GROUPING-A2
100111                                    LA-GROUPING-A3
100111                                    LA-GROUPING-A4
100111                                    LA-GROUPING-A5
100111     MOVE VD-ACCOUNT             TO LA-ACCOUNT-A2
100111                                    LA-ACCOUNT-A3
100111                                    LA-ACCOUNT-A4
100111                                    LA-ACCOUNT-A5
100111     MOVE VD-STATE               TO LA-STATE-A2
100111                                    LA-STATE-A3
100111                                    LA-STATE-A4
100111                                    LA-STATE-A5
100111     MOVE VD-CERT-EFF-DT         TO LA-EFFECT-DATE-A2
100111     MOVE VD-CERT-NO             TO LA-CERT-NO-A2
100111
100111     MOVE VD-ENTRY-BATCH         TO LA-ENTRY-A6
100111
100111     MOVE VD-PROC-ID             TO LA-PROCESSOR-CD
100111
100111     MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
100111                                    LA-INITIAL-PRINT-DATE
100111                                    LA-SENT-DATE
100111                                    LA-REPLY-DATE
100111                                    LA-RESEND-DATE
100111                                    LA-FOLLOW-UP-DATE
100111
100111     MOVE 'A'                    TO LA-STATUS
100111     MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
100111     MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
100111     MOVE VD-LETTER-ID           TO LA-FORM-A3
100111     MOVE SPACES                 TO LA-DATA-SOURCE
100111     MOVE 'B'                    TO LA-ARCHIVE-STATUS
100111     MOVE VD-CURRENT-DATE        TO LA-CREATION-DATE
100111     MOVE 'Y'                    TO LA-VA-DISCLOSURE-IND
100111     MOVE VD-CURRENT-DATE        TO LA-LAST-MAINT-DATE
100111     MOVE VD-CURRENT-TIME        TO LA-LAST-MAINT-TIME
100111     MOVE VD-PROC-ID             TO LA-LAST-UPDATED-BY
100111
100111     MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
100111                                    LA-NO-OF-TEXT-RECORDS
100111
100111     EXEC CICS WRITE
100111          DATASET   ('ERARCH')
100111          FROM      (LETTER-ARCHIVE)
100111          RIDFLD    (LA-CONTROL-PRIMARY)
100111          RESP      (WS-RESPONSE)
100111     END-EXEC
100111
100111     IF NOT RESP-NORMAL
100111        SET ARCH-FAIL TO TRUE
100111     END-IF
100111
100111     .
100111 5000-EXIT.
100111     EXIT.
100111
100111
100111 5700-WRITE-EXTR.
100111
100111     PERFORM 5000-BUILD-ERARCH   THRU 5000-EXIT
100111     IF ARCH-FAIL
100111        MOVE ' COULDNT BUILD ERARCH ' TO WS-MESSAGE
100111        GO TO 5700-EXIT
100111     END-IF
100111     MOVE WS-COMPANY-CD          TO NSAS-COMPANY-CD
100111     MOVE WS-ARCHIVE-NO          TO NSAS-ARCHIVE-NO
100111     MOVE +0                     TO NSAS-SEQ-NO
100111     PERFORM 5710-BUILD-NSASEXTR THRU 5710-EXIT
100111     PERFORM 5720-WRITE-NSASEXTR THRU 5720-EXIT
100111     IF ARCH-FAIL
100111        MOVE ' COULDNT BUILD STD NSASEXTR ' TO WS-MESSAGE
100111        GO TO 5700-EXIT
100111     END-IF
100111     IF W-LETTER-TO-ACCT NOT = SPACES
100111        MOVE W-LETTER-TO-ACCT   TO NSAS-LETTER-VARIABLES (5:1)
100111        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
100111        MOVE WS-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
100111        MOVE +1                  TO NSAS-SEQ-NO
100111        PERFORM 5720-WRITE-NSASEXTR
100111                                 THRU 5720-EXIT
100111        IF ARCH-FAIL
100111           MOVE ' COULDNT BUILD ACCT NSASEXTR ' TO WS-MESSAGE
100111           GO TO 5700-EXIT
100111        END-IF
100111     END-IF
100111
100111     IF W-LETTER-TO-BENE NOT = SPACES
100111        MOVE W-LETTER-TO-BENE   TO NSAS-LETTER-VARIABLES (5:1)
100111        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
100111        MOVE WS-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
100111        MOVE +2                  TO NSAS-SEQ-NO
100111        PERFORM 5720-WRITE-NSASEXTR
100111                                 THRU 5720-EXIT
100111        IF ARCH-FAIL
100111           MOVE ' COULDNT BUILD BENE NSASEXTR ' TO WS-MESSAGE
100111        END-IF
100111     END-IF
100111
100111     .
100111 5700-EXIT.
100111     EXIT.
100111
100111 5710-BUILD-NSASEXTR.
100111
100111***  i added the extra space in front of the 1st ~
100111***  only because there may be a letter to acct or bene
100111
100111     STRING
100111         VA-LETTER                ' ~'
100111         VA-PROC-ID               '~'
100111         VA-PROC-NAME             '~'
100111         VA-PROC-TITLE            '~' 
100111         VA-CSR-CODE              '~' 
100111         VA-CSR-NAME              '~' 
100111         VA-CSR-TITLE             '~' 
100111         VA-CARRIER               '~' 
100111         VA-GROUPING              '~' 
100111         VA-STATE                 '~' 
100111         VA-ACCOUNT               '~' 
100111         VA-EFFDATE               '~' 
100111         VA-CERT-NO               '~' 
100111         VA-FIRSTNAME             '~'
100111         VA-MIDINIT               '~'
100111         VA-LASTNAME              '~'
100111         VA-ADDR1                 '~' 
100111         VA-ADDR2                 '~' 
100111         VA-ADDR-CITY             '~' 
100111         VA-ADDR-ST               '~' 
100111         VA-ADDR-ZIP              '~' 
100111         VA-JNT-FIRSTNAME         '~'
100111         VA-JNT-MIDINIT           '~'
100111         VA-JNT-LASTNAME          '~'
100111         VA-ARCH-NO               '~' 
100111         VA-ENC-LINE              '~' 
100111         VA-ATTACH                '~' 
100111         VA-STACK                 '~' 
100111         VA-PRINT-NOW             '~' 
100111         VA-BORROWER              '~' 
100111         VA-BENE                  '~' 
100111         VA-ACCT-NAME             '~' 
100111         VA-SL                    '~' 
100111         VA-JL                    '~' 
100111         VA-SA                    '~' 
100111         VA-JA                    '~' 
100111         VA-GD                    '~' 
100111         VA-NP                    '~' 
100111         VA-7R                    '~' 
100111         VA-LL                    '~' 
100111         VA-TN                    '~' 
100111         VA-14R                   '~' 
100111         VA-14E                   '~' 
100111         VA-30R                   '~' 
100111         VA-30E                   '~' 
100111         VA-CL-NET-AMT            '~' 
100111         VA-CL-GRS-AMT            '~' 
100111         VA-CL-LEV-AMT            '~' 
100111         VA-CL-NET-PMT            '~' 
100111         VA-CL-GRS-PMT            '~' 
100111         VA-CL-LEV-PMT            '~' 
100111         VA-CL-NET-PRM            '~' 
100111         VA-CL-GRS-PRM            '~' 
100111         VA-CL-LEV-PRM            '~' 
100111         VA-CD-NET-AMT            '~' 
100111         VA-CD-GRS-AMT            '~' 
100111         VA-CD-LEV-AMT            '~' 
100111         VA-CD-NET-PMT            '~' 
100111         VA-CD-GRS-PMT            '~' 
100111         VA-CD-LEV-PMT            '~' 
100111         VA-CD-NET-PRM            '~' 
100111         VA-CD-GRS-PRM            '~' 
100111         VA-CD-LEV-PRM            '~' 
100111         VA-CLD-NET-AMT           '~' 
100111         VA-CLD-GRS-AMT           '~' 
100111         VA-CLD-LEV-AMT           '~' 
100111         VA-CLD-NET-PMT           '~' 
100111         VA-CLD-GRS-PMT           '~' 
100111         VA-CLD-LEV-PMT           '~' 
100111         VA-CLD-NET-PRM           '~' 
100111         VA-CLD-GRS-PRM           '~' 
100111         VA-CLD-LEV-PRM           '~' 
100111         VA-NO-NET-AMT            '~' 
100111         VA-NO-GRS-AMT            '~' 
100111         VA-NO-LEV-AMT            '~' 
100111         VA-NO-NET-PMT            '~' 
100111         VA-NO-GRS-PMT            '~' 
100111         VA-NO-LEV-PMT            '~' 
100111         VA-NO-NET-PRM            '~' 
100111         VA-NO-GRS-PRM            '~' 
100111         VA-NO-LEV-PRM            '~' 
100111            DELIMITED BY '  ' INTO NSAS-LETTER-VARIABLES
100111     END-STRING
100111
100111     .
100111 5710-EXIT.
100111     EXIT.
100111
100111 5720-WRITE-NSASEXTR.
100111
100111     EXEC CICS WRITE
100111        DATASET    ('NSASEXTR')
100111        FROM       (NSAS-EXTRACT-RECORD)
100111        RIDFLD     (NSAS-CONTROL-PRIMARY)
100111        RESP       (WS-RESPONSE)
100111     END-EXEC
100111
100111     IF NOT RESP-NORMAL
100111        SET ARCH-FAIL TO TRUE
100111     END-IF
100111
100111     .
100111 5720-EXIT.
100111     EXIT.
100111


       8000-RETURN.

           EXEC CICS RETURN
           END-EXEC

           GOBACK

           .
       9700-DATE-LINK.                                                  

           MOVE LINK-ELDATCV           TO PGM-NAME
           EXEC CICS LINK
              PROGRAM    (PGM-NAME)
              COMMAREA   (DATE-CONVERSION-DATA)
              LENGTH     (DC-COMM-LENGTH)
           END-EXEC

           .
       9700-EXIT.
           EXIT.
