       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6952.
      *              PROGRAM CONVERTED BY
      *              COBOL CONVERSION AID PO 5785-ABJ
      *              CONVERSION DATE 02/12/96 10:03:45.
      *                            VMOD=2.001
      *
      *
      *AUTHOR.           CENTRAL STATES HEALTH AND LIFE.
      *                  OMAHA, NEBRASKA.
      *DATE-COMPILED.
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
      *REMARKS. TRANSACTION EXM9 - ENDORSEMENT PRINT
      *        THIS PROGRAM IS USED TO PRINT THE ENDORSEMENTS
      *        PRINT endorsements      CODE-1 = 1
      *                                CODE-2 = 1
      *        RE PRINT endorsements   CODE-1 = 1
      *                                CODE-2 = 2
      *                        C H A N G E   L O G
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 052302    2002021900005   SMVA  MODIFY GENERAL CHANGE ENDORSEMT
      *                               SPACING FOR RTK SIGNATURE OVERLAY
042605* 042605    2005021100002   PEMA  MODIFY DICKS TITLE AND DATE
      *                                 OF FORM
033006* 033006    2006030800001   PEMA  ADD VA AND NM CHANGES
112906* 112906    2006111600001   PEMA  FIX NCB ON REFUNDS
122706* 122706    2006111300003   PEMA  ADD PROCESSING FOR IH (KY)
      *                                  ALSO, CHANGES FOR LEASES (NJ)
021907* 021907                    PEMA  ADD CONTRACT NAME TO SUM PAGE
042307* 042307                    PEMA  ADD PA PROCESSING PER JJVA
053107* 053107    2007010200001   PEMA  INCREASE COMMENTS TO 75 CHAR,
053107* 053107                      REMOVE GA COPY, ADD BILL CD C & E
081009* 081009    2008101500003   AJRA  FORMAT NAME ON ENDORSEMENT
081209* 081209    2009081100001   AJRA  PRINT COMMENT LINES < 5 CHARACTE
081309* 081309    2009081100002   AJRA  FIX ORIG AMT FOR BAS3
102909* 102909    2009102900002   AJRA  FIX INSERT OF ENDORSED BILL NOTE
110509* 110509    2008100900003   AJRA  UPDATE CERT WHEN NOTE ADDED
113009* 113009    2009112500001   AJRA  FIX INSERT OF ENDORSED WHEN
113009*                                 BLANK LINE IN BILL NOTE RANGE
052110* 052110    2010052000001   PEMA  USE CUR DTE RNG FOR COMM CHGBK
110410* 110410    2007070900001   PEMA  ADD REFORMATIONS AND RESCISSIONS
032112* 032112    2011110900001   AJRA  ADD AHL HEADINGS
061112* 061112    2012061100001   AJRA  FIX ZIP CODE
041320* 041320  CR2020030500002   PEMA  Issue, cancel billing notes
      ******************************************************************
      *
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC  X(32) VALUE '********************************'.
       77  FILLER  PIC  X(32) VALUE '*   EL6952 WORKING STORAGE     *'.
       77  FILLER  PIC  X(32) VALUE '********* V/M 2.001 ************'.
052110 77  WS-CHARGEBACK-L1            PIC 99  VALUE ZEROS.
110410 77  WS-RES-REF-SW               PIC X   VALUE ' '.
110410     88  FOUND-RES                       VALUE '1'.
110410     88  FOUND-REF                       VALUE '2'.
       01  W-PROGRAM-CONSTANTS.
           12  FILLER                  PIC  X(18)
                                       VALUE 'PROGRAM CONSTANTS:'.
           12  W-ZEROS                 PIC S9(04)  COMP VALUE +0.
           12  W-ERENDR-ID             PIC  X(08)  VALUE 'ERENDR'.
           12  W-ERNOTE-ID             PIC  X(08)  VALUE 'ERNOTE'.
           12  W-ELCNTL-ID             PIC  X(08)  VALUE 'ELCNTL'.
           12  W-ERACCT-ID             PIC  X(08)  VALUE 'ERACCT'.
           12  W-ERCOMP-ID             PIC  X(08)  VALUE 'ERCOMP'.
           12  W-ELLETR-ID             PIC  X(08)  VALUE 'ELLETR'.
110509     12  W-ELCERT-ID             PIC  X(08)  VALUE 'ELCERT'.
052110 01  WS-PREV-ERACCT-RANGE        PIC X(2000) VALUE SPACES.
       01  W-PROGRAM-WORK-AREA.
           12  WS-WORK-ZIP             PIC X(10).
           12  WS-WORK-ZIPR REDEFINES
               WS-WORK-ZIP.
               16  WS-ZIP-1-5          PIC X(5).
               16  WS-ZIP-DASH         PIC X.
               16  WS-ZIP-7-10         PIC X(4).
           12  W-WORK-SUB.
               16  W-IN1               PIC  X.
               16  W-IN2               PIC  X.
               16  W-IN3               PIC  X.
           12  W-IN1-3 REDEFINES W-WORK-SUB
                                       PIC 999.
           12  THIS-PGM                PIC  X(8)  VALUE 'EL6952'.
           12  FILLER                  PIC  X(18)
                                       VALUE 'PROGRAM WORK AREA:'.
           12  WS-BLANK-LINES-NEEDED   PIC S9 COMP-3 VALUE +0.
           12  WS-LINES-LEFT           PIC S9(3) COMP-3 VALUE +0.
           12  WS-WORK-TERM            PIC 999.
           12  WS-TERM-ALPH REDEFINES WS-WORK-TERM
                                       PIC XXX.
           12  WS-YEARS                PIC 999.
           12  WS-YEARS-ALPH REDEFINES WS-YEARS
                                       PIC XXX.
           12  WS-MONTHS               PIC 999.
           12  WS-MONTHS-ALPH REDEFINES WS-MONTHS
                                       PIC XXX.
           12  WS-DAYS                 PIC 999.
           12  WS-DAYS-ALPH REDEFINES WS-DAYS
                                       PIC XXX.
           12  W-ASKTIME-CTR           PIC S9(04)  COMP.
           12  W-COPIES                PIC  9.
           12  W-DELAY-INTERVAL        PIC S9(07)  COMP-3 VALUE +2.
           12  W-NDX                   PIC S9(04)  COMP   VALUE +0.
           12  W-NUM-OF-TEXT-RECORDS   PIC S9(04)  COMP   VALUE +0.
           12  W-NUMBER-OF-LINES       PIC S9(04)  COMP.
           12  W-RECORD-COUNT          PIC S9(04)         VALUE +0.
           12  W-SAVE-ARCH-NO          PIC S9(08)  COMP   VALUE +0.
           12  W-SKIP                  PIC  9(02).
           12  W-SUB1                  PIC S9(04)  COMP.
           12  W-SUB2                  PIC S9(04)  COMP.
           12  IN-SUB                  PIC S9(04)  COMP.
           12  OUT-SUB                 PIC S9(04)  COMP.
           12  WS-LAST-CHAR            PIC X VALUE ' '.
           12  WS-WORK-AMT             PIC S9(07)V99 COMP-3 VALUE +0.
           12  W-ASTERISK-LINE1.
               16  FILLER              PIC  X(78)  VALUE ALL '*'.
           12  W-ASTERISK-LINE.
               16  FILLER              PIC  X(01)  VALUE SPACES.
               16  FILLER              PIC  X(78)  VALUE ALL '*'.
052302     12  W-ASTERISK-LINEB.
052302         16  FILLER              PIC  X(01)  VALUE '0'.
052302         16  FILLER              PIC  X(78)  VALUE ALL '*'.
           12  W-CALL-PGM              PIC  X(08).
           12  W-CURRENT-SAVE          PIC  X(02).
           12  W-ERROR-LINE            PIC  X(80).
           12  W-LAST-RESENT-PRINT-DATE
                                       PIC  X(02)  VALUE SPACES.
           12  W-SAVE-CURRENT-DATE     PIC  X(08)  VALUE SPACES.
           12  W-SAVE-CURRENT-BIN-DATE PIC  X(02)  VALUE SPACES.
           12  W-SAVE-LETTER-ARCHIVE   PIC X(250)  VALUE SPACES.
           12  W-TOTAL-LINE.
               20  FILLER              PIC  X(01)  VALUE SPACES.
               20  FILLER              PIC  X(20)
                   VALUE 'PROCESS COMPLETED.  '.
               20  W-TOTAL-LINE-DESC   PIC  X(26)
                   VALUE 'LETTERS PRINTED TOTAL   - '.
               20  W-TOTAL-LETTERS     PIC Z,ZZZ,ZZ9.
           12  W-WORKING-RESEND-DATE   PIC  X(02)  VALUE SPACES.
           12  WS-EOF-SW               PIC X  VALUE ' '.
               88  THERE-ARE-NO-MORE-RECORDS  VALUE 'Y'.
           12  WS-LETR-EOF-SW          PIC X  VALUE ' '.
               88  NO-MORE-LETR               VALUE 'Y'.
           12  WS-PRINT-SW             PIC X  VALUE ' '.
               88  WE-PRINT                   VALUE 'Y'.
           12  WS-REPRINT-SW           PIC X  VALUE ' '.
               88  WE-REPRINT                 VALUE 'Y'.
           12  WS-HIT-SW               PIC X  VALUE ' '.
               88  FOUND-A-HIT                VALUE 'Y'.
081209     12  WS-TEXT-FOUND           PIC X  VALUE 'N'.
081209         88  TEXT-FOUND                 VALUE 'Y'.
081209         88  TEXT-NOT-FOUND             VALUE 'N'.
052110     12  WS-CO-RESPONSE          PIC S9(8)   COMP.
052110         88  CO-RESP-NORMAL               VALUE +00.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  W-EDIT-3-0              PIC  ZZ9.
           12  W-EDIT-9-2              PIC  $$$$,$$$,$$9.99.
           12  FILLER                  PIC  X(11)
                                       VALUE 'TEXT TABLE:'.
           12  W-TX-TABLE                  VALUE SPACES.
               16  W-TX-GRP OCCURS 10 TIMES
                             INDEXED BY W-TG-NDX
                                        W-TG-NDX2.
                   20  W-TX-TEXT.
053107                 24  W-TX-CHAR OCCURS 75 TIMES
                                          INDEXED BY W-TX-NDX
                                                     W-TX-NDX1
                                                     W-TX-NDX2
                                       PIC  X(01).
           12  FILLER                  PIC  X(11)
                                       VALUE 'FILE TABLE:'.
       01  W-PROGRAM-KEYS.
           12  FILLER                  PIC  X(13)
                                       VALUE 'PROGRAM KEYS:'.
           12  WS-ERENDR-HOLD-KEY      PIC X(20) VALUE LOW-VALUES.
           12  W-COMP-KEY.
               16  W-COMP-COMPANY-CD   PIC X.
               16  W-COMP-CARRIER      PIC X.
               16  W-COMP-GROUPING     PIC X(6).
               16  W-COMP-FIN-RESP     PIC X(10).
               16  W-COMP-ACCOUNT      PIC X(10).
               16  W-COMP-REC-TYPE     PIC X.
           12  W-ERNOTE-KEY.
               16  W-NOTE-COMPANY-CD   PIC X.
               16  W-NOTE-CARRIER      PIC X.
               16  W-NOTE-GROUPING     PIC X(6).
               16  W-NOTE-STATE        PIC XX.
               16  W-NOTE-ACCOUNT      PIC X(10).
               16  W-NOTE-CERT-EFF-DT  PIC XX.
               16  W-NOTE-CERT-PRIME   PIC X(10).
               16  W-NOTE-CERT-SFX     PIC X.
041320         16  w-note-record-type  pic x.
           12  W-ERENDR-KEY.
               16  W-ENDR-COMPANY-CD   PIC X.
               16  W-ENDR-CARRIER      PIC X.
               16  W-ENDR-GROUPING     PIC X(6).
               16  W-ENDR-STATE        PIC XX.
               16  W-ENDR-ACCOUNT      PIC X(10).
               16  W-ENDR-CERT-EFF-DT  PIC XX.
               16  W-ENDR-CERT-PRIME   PIC X(10).
               16  W-ENDR-CERT-SFX     PIC X.
               16  W-ENDR-CHG-SEQ-NO   PIC S9(4)  COMP.
               16  W-ENDR-RECORD-TYPE  PIC X.
           12  W-ACCT-KEY.
               16  W-ACCT-COMPANY-CD   PIC  X(01).
               16  W-ACCT-CARRIER      PIC  X(01).
               16  W-ACCT-GROUPING     PIC  X(06).
               16  W-ACCT-STATE        PIC  X(02).
               16  W-ACCT-ACCOUNT      PIC  X(10).
               16  W-ACCT-EXP-DT       PIC  XX.
               16  FILLER              PIC  X(4).
           12  W-CNTL-KEY.
               16  W-CNTL-COMPANY-ID   PIC  X(3).
               16  W-CNTL-RECORD-TYPE  PIC  X.
               16  W-CNTL-PROCESSOR    PIC  X(4).
               16  W-CNTL-SEQ-NO       PIC  S9(4) COMP.
           12  W-LETR-KEY.
               16  W-LETR-COMPANY-CD   PIC  X.
               16  W-LETR-ACCESS-CD    PIC  X(12).
               16  W-LETR-SEQ-NO       PIC  S9(4) COMP.
           12  W-LETR-HOLD-KEY         PIC  X(13).
110509
110509     12  W-ELCERT-KEY.
110509         16  W-CERT-COMPANY-CD   PIC X.
110509         16  W-CERT-CARRIER      PIC X.
110509         16  W-CERT-GROUPING     PIC X(6).
110509         16  W-CERT-STATE        PIC XX.
110509         16  W-CERT-ACCOUNT      PIC X(10).
110509         16  W-CERT-CERT-EFF-DT  PIC XX.
110509         16  W-CERT-CERT-PRIME   PIC X(10).
110509         16  W-CERT-CERT-SFX     PIC X.
110509
       01  W-PROGRAM-SWITCES.
           12  WS-ISSUE-TIMES          pic  9  value zeros.
           12  FILLER                  PIC  X(17)
                                       VALUE 'PROGRAM SWITCHES:'.
           12  WS-ERACCT-BR-SW         PIC  X              VALUE ' '.
               88  ERACCT-BROWSE-STARTED                   VALUE 'Y'.
           12  WS-ENDR-BROWSE-SW       pic  x              value ' '.
               88  ENDR-BROWSE-STARTED                     value 'Y'.
           12  W-ENDBR-SW              PIC  X(01)          VALUE ' '.
               88  W-ENDBR                                 VALUE 'Y'.
           12  WS-LETR-BROWSE-SW       PIC  X              VALUE ' '.
               88  LETR-BROWSE-STARTED                     VALUE 'Y'.
           12  W-FIRST-FORM-SW         PIC  X(01)          VALUE ' '.
               88  W-THIS-IS-FIRST-FORM                    VALUE ' '.
               88  W-THIS-IS-NOT-FIRST-FORM                VALUE 'Y'.
           12  W-PRINT-SW              PIC S9(01) COMP-3   VALUE ZERO.
           12  W-PROCESSING-SW         PIC S9(01) COMP-3   VALUE ZERO.
               88  W-PROCESS-BY-KEY                        VALUE +3.
           12  W-TEXT-BROWSE-STARTED   PIC  X(01)          VALUE 'N'.
           12  W-TOP-FORM-SW           PIC  X(01)          VALUE SPACE.
               88  W-TOP-FORM-SET                          VALUE 'T'.
           12  W-OPTION-CODES          PIC  X(02).
               88  W-PRINT                                 VALUE '11'.
               88  W-REPRINT                               VALUE '12'.
112906     12  WS-NCB-DAYS             PIC S9(5)    COMP-3 VALUE +0.
       01  W-SUPPORTED-VARIABLES.
      *****TERM TERM
           12  FILLER                  PIC  X(03) VALUE '001'.
           12  FILLER                  PIC S9(04) COMP VALUE +3.
           12  FILLER                  PIC  X(30) VALUE ALL '*'.
           12  FILLER                  PIC  X(01) VALUE ALL 'N'.
           12  FILLER                  PIC S9(04) COMP VALUE +08.
      *****CERT EFFECTIVE DATE (ENDR)
           12  FILLER                  PIC  X(03) VALUE '002'.
           12  FILLER                  PIC S9(04) COMP VALUE +08.
           12  FILLER                  PIC  X(30) VALUE ALL '*'.
           12  FILLER                  PIC  X(01) VALUE ALL 'N'.
           12  FILLER                  PIC S9(04) COMP VALUE +08.
      *****BENEFIT ENTERED AMT (ENDR)
           12  FILLER                  PIC  X(03) VALUE '003'.
           12  FILLER                  PIC S9(04) COMP VALUE +15.
           12  FILLER                  PIC  X(30) VALUE ALL '*'.
           12  FILLER                  PIC  X(01) VALUE ALL 'N'.
           12  FILLER                  PIC S9(04) COMP VALUE +09.
      *****INSURED'S NAME (FIRST, INIT, LAST)
           12  FILLER                  PIC  X(03) VALUE  '004'.
           12  FILLER                  PIC S9(04) COMP VALUE +30.
           12  FILLER                  PIC  X(30) VALUE ALL '*'.
PEMTMP     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
           12  FILLER                  PIC S9(04) COMP VALUE +08.
      *****JOINT'S NAME (FIRST, INIT, LAST)
           12  FILLER                  PIC  X(03) VALUE '005'.
           12  FILLER                  PIC S9(04) COMP VALUE +30.
           12  FILLER                  PIC  X(30) VALUE ALL '*'.
           12  FILLER                  PIC  X(01) VALUE ALL 'N'.
           12  FILLER                  PIC S9(04) COMP VALUE +08.
       01  FILLER REDEFINES W-SUPPORTED-VARIABLES.
           12  W-VARIABLE-GRP OCCURS 005 TIMES
                              INDEXED BY W-VG-NDX.
               16  W-VARIABLE-ID         PIC  X(03).
               16  W-VARIABLE-SIZE       PIC S9(04) COMP.
               16  W-VG-TEXT.
                   20  W-VAR-CHAR
                              OCCURS 30 TIMES
                              INDEXED BY W-VC-NDX
                                         PIC  X(01).
               16  W-VARIABLE-UPLOW-IND  PIC  X(01).
                   88  W-USE-UPPER-AND-LOWER-CASE VALUE 'Y'.
               16  W-VARIABLE-SOURCE   PIC S9(04) COMP.
       01  W-VAR-END                   PIC  X(23)
                              VALUE ':VARIABLE WORK AREA END'.
       01  WS-MESSAGES.
           12  WS-COMMENT-LINES1.
               16  WS-TEXT OCCURS 10.
053107             20  WS-CHAR1 OCCURS 75 PIC X.
           12  WS-COMMENT-LINES2.
               16  FILLER   OCCURS 10.
053107             20  WS-CHAR2 OCCURS 75 PIC X.
           12  WS-MESSAGE-TABLE.
               16  FILLER.
                   20  FILLER          PIC X(29) VALUE
                   'BAS1The term of insurance is '.
                   20  BAS1-TERM       PIC ZZZ.
                   20  FILLER          PIC X(42) VALUE
                   ' months.'.
               16  FILLER.
                   20  FILLER          PIC X(45) VALUE
                   'BAS2The effective date of the certificate is '.
                   20  BAS2-EFF-DT     PIC X(10).
                   20  FILLER          PIC X(19) VALUE '.'.
               16  FILLER.
                   20  FILLER          PIC X(40) VALUE
                   'BAS3The original amount of insurance is '.
                   20  BAS3-AMT        PIC $$$,$$$.99.
                   20  FILLER          PIC X(24) VALUE '.'.
               16  FILLER.
                   20  FILLER          PIC X(35) VALUE
                   'EXC1The credit life coverage is on '.
                   20  EXC1-NAME1      PIC X(25).
                   20  FILLER          PIC X(14) VALUE
                   ', instead of'.
               16  FILLER.
                   20  FILLER          PIC X(4) VALUE
                   'EXC1'.
                   20  EXC1-NAME2      PIC X(25).
                   20  FILLER          PIC XX    VALUE
                   '. '.
                   20  EXC1-NAME3      PIC X(25).
                   20  FILLER          PIC X(18) VALUE
                   ' in not covered   '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'EXC1by credit life insurance on this loan.'.
               16  FILLER.
                   20  FILLER          PIC X(4) VALUE
                   'EXC3'.
                   20  EXC3-NAME1      PIC X(25).
                   20  FILLER          PIC X(45) VALUE
                   ' is the primary borrower. '.
               16  FILLER.
                   20  FILLER          PIC X(4) VALUE
                   'EXC3'.
                   20  EXC3-NAME2      PIC X(25).
                   20  FILLER          PIC X(45) VALUE
                   ' is not covered by credit life insurance     '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'EXC3on this loan.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'DIS1The disability coverage is cancelled. The maximu
      -            'm limit of coverage   '.
               16  FILLER.
                   20  FILLER          PIC X(41) VALUE
                   'DIS1allowed was met on a previous policy.'.
                   20  FILLER          PIC X(33).
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'DIS2The monthly disability benefit has been lowered
      -            'to the maximum'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'DIS2allowed.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF1The original amount of life insurance has been l
      -            'owered to the maximum '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF1allowed without an approved health application.
      -            '  '.
               16  FILLER.
                   20  FILLER          PIC X(33) VALUE
                   'LIF2Life insurance is cancelled. '.
                   20  LIF2-NAME       pic x(25).
                   20  FILLER          PIC X(16) VALUE
                   ' does not '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF2qualify due to eligibility.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF3Life insurance coverage is cancelled. The maximu
      -            'm limit of coverage   '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF3allowed was met on a previous policy.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF4The term of insurance has been lowered to the ma
      -            'ximum allowed at      '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF4current age. The coverage is net pay +2 instead
      -            'of gross decreasing   '.
               16  FILLER.
                   20  FILLER          PIC X(46) VALUE
                   'LIF4life. We will provide life coverage until '.
                   20  LIF4-NAME       PIC X(25).
                   20  filler          pic xxx value '''s'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'LIF471st birthday.'.
               16  FILLER.
                   20  FILLER          PIC X(25) VALUE
                   'REC1The Life coverage on '.
                   20  REC1-NAME       PIC X(25).
                   20  FILLER          PIC X(24) VALUE
                   ' is cancelled. The      '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REC1eligibility requirements were not met at time of
      -            ' issue.'.
               16  FILLER.
                   20  FILLER          PIC X(31) VALUE
                   'REC2The disibility coverage on '.
                   20  REC2-NAME       PIC X(25).
                   20  FILLER          PIC X(18) VALUE
                   ' is cancelled.    '.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REC2The eligibility requirements were not met at tim
      -            'e of issue.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF1Incorrect time in force.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF2Incorrect rounding to nearest whole month.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF3Incorrect refund %.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF4Incorrect refund table used.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF5Incorrect original effective date.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF6Incorrect original premium.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF7Death claim paid. Life premium not refundable.'.
               16  FILLER.
                   20  FILLER          PIC X(74) VALUE
                   'REF8Certificate previously cancelled. No refund allo
      -            'wed.'.
           12  FILLER REDEFINES WS-MESSAGE-TABLE OCCURS 35.
               16  WS-ID               PIC X(4).
               16  WS-COMMENT          PIC X(70).
052302 01  RTK-FORMDEF-FLG-NOPRT-HDR.
052302     12  FILLER                  PIC X(01) VALUE ' '.
110410     12  RTK-NOTICE              PIC X(26) VALUE
052302         'GENERAL CHANGE ENDORSEMENT'.
052302     12  FILLER                  PIC X(39) VALUE SPACES.
052302 01  RTK-FORMDEF-FLG-NOPRT-HDR-VANM.
052302     12  FILLER                  PIC X(01) VALUE ' '.
110410     12  RTK-NOTICE-VANM         PIC X(26) VALUE
052302         '  NOTIFICATION OF CHANGE  '.
052302     12  FILLER                  PIC X(39) VALUE SPACES.
052302 01  SIGNATURE-FLG-NOPRT-HDR.
052302     12  FILLER                  PIC X(01) VALUE ' '.
052302     12  FILLER                  PIC X(07) VALUE SPACES.
052302     12  WS-WHO-SIG              PIC X(06) VALUE 'RTKSIG'.
052302     12  FILLER                  PIC X(52) VALUE SPACES.
122706 01  HEADING-1-CARR8.
122706     12  FILLER                  PIC X(19) VALUE ' '.
122706     12  FILLER                  PIC X(41) VALUE
122706     'INVESTORS HERITAGE LIFE INSURANCE COMPANY'.
122706     12  FILLER                  PIC X(06) VALUE SPACES.
122706 01  HEADING-2-CARR8.
122706     12  FILLER                  PIC X(14) VALUE ' '.
122706     12  FILLER                  PIC X(52) VALUE
122706     '          200 CAPITAL AVENUE, P.O. BOX 717          '.
122706 01  HEADING-3-CARR8.
122706     12  FILLER                  PIC X(14) VALUE ' '.
122706     12  FILLER                  PIC X(52) VALUE
122706     '           FRANKFORT, KENTUCKY 40602-0717           '.
       01  HEADING-1.
052302     12  FILLER                  PIC X(19) VALUE ' '.
           12  FILLER                  PIC X(41) VALUE
           'CENTRAL STATES HEALTH & LIFE CO. OF OMAHA'.
052302     12  FILLER                  PIC X(06) VALUE SPACES.
       01  HEADING-2.
052302     12  FILLER                  PIC X(14) VALUE ' '.
           12  FILLER                  PIC X(52) VALUE
           '           PO BOX 34350    96TH & WESTERN           '.
052302 01  HEADING-3.
052302     12  FILLER                  PIC X(14) VALUE ' '.
052302     12  FILLER                  PIC X(52) VALUE
052302     '                OMAHA, NE 68134-0350                '.
       01  HEADING-2-LEASE.
011107     12  FILLER                  PIC X(14) VALUE ' '.
           12  FILLER                  PIC X(52) VALUE
           '                   PO BOX 34350                     '.
032112 01  HEADING-1-CARR8-AHL.
032112     12  FILLER                  PIC X(19) VALUE ' '.
032112     12  FILLER                  PIC X(41) VALUE
032112     'CITIZENS SECURITY LIFE INSURANCE COMPANY '.
032112     12  FILLER                  PIC X(06) VALUE SPACES.
032112
032112 01  HEADING-2-CARR8-AHL.
032112     12  FILLER                  PIC X(09) VALUE ' '.
032112     12  FILLER                  PIC X(57) VALUE
032112     'ADMINISTERED BY CENTRAL STATES HEALTH & LIFE CO. OF OMAHA'.
032112
032112 01  HEADING-3-CARR8-AHL.
032112     12  FILLER                  PIC X(14) VALUE ' '.
032112     12  FILLER                  PIC X(52) VALUE
032112     '     P.O. BOX 34350 OMAHA, NEBRASKA 68134-0350      '.
032112
032112 01  HEADING-1-AHL.
032112     12  FILLER                  PIC X(19) VALUE ' '.
032112     12  FILLER                  PIC X(41) VALUE
032112     'CENTRAL STATES HEALTH & LIFE CO. OF OMAHA'.
032112     12  FILLER                  PIC X(06) VALUE SPACES.
032112
032112 01  HEADING-2-AHL.
032112     12  FILLER                  PIC X(14) VALUE ' '.
032112     12  FILLER                  PIC X(52) VALUE
032112     '        CREDIT INSURANCE ADMINISTRATOR FOR          '.
032112 01  HEADING-3-AHL.
032112     12  FILLER                  PIC X(14) VALUE ' '.
032112     12  FILLER                  PIC X(52) VALUE
032112     '     AMERICAN HERITAGE LIFE INSURANCE COMPANY       '.
032112 01  HEADING-4-AHL.
032112     12  FILLER                  PIC X(14) VALUE ' '.
032112     12  FILLER                  PIC X(52) VALUE
061112     '     P.O. BOX 34350 OMAHA, NEBRASKA 68134-0350      '.
032112
       01  LINE-3.
           12  FILLER                  PIC X VALUE ' '.
           12  LN3-DATE                PIC X(8) VALUE SPACES.
       01  LINE-4.
           12  LN4-ATTN                PIC X(8) VALUE '   ATTN:'.
           12  LN4-CONTACT             PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(13) VALUE SPACES.
           12  FILLER                  PIC X(18) VALUE
           '  ACCOUNT NUMBER: '.
           12  LN4-ACCOUNT             PIC X(10) VALUE SPACES.
       01  LINE-5.
           12  FILLER                  PIC XXX VALUE '   '.
           12  LN5-NAME                PIC X(30) VALUE SPACES.
021907 01  LINE-51A.
           12  FILLER                  PIC XXX VALUE '   '.
           12  LN51A-CONTRACTED-NAME   PIC X(30) VALUE SPACES.
       01  LINE-6.
           12  FILLER                  PIC XXX VALUE '   '.
           12  LN6-ADDRESS             PIC X(30) VALUE SPACES.
       01  LINE-7.
           12  FILLER                  PIC XXX VALUE '   '.
           12  LN7-CITY-STATE-ZIP      PIC X(40) VALUE SPACES.
       01  LINE-8.
           12  FILLER                  PIC X(62) VALUE ' '.
           12  FILLER                  PIC X(18) VALUE
           'Summary of Changes'.
       01  LINE-9.
           12  FILLER                  PIC X VALUE ' '.
           12  FILLER                  PIC X(17) VALUE
           'Corrections Made:'.
       01  LINE-10.
           12  FILLER                  PIC X(44) VALUE ' '.
           12  FILLER                  PIC X(11) VALUE
           'Reported as'.
           12  FILLER                  PIC X(11) VALUE SPACES.
           12  FILLER                  PIC X(10) VALUE
           'Changed To'.
       01  LINE-11.
           12  FILLER                  PIC X(9)  VALUE ' Policy #'.
           12  FILLER                  PIC X(7)  VALUE SPACES.
           12  FILLER                  PIC X(4)  VALUE 'Name'.
081009     12  FILLER                  PIC X(13)  VALUE SPACES.
           12  FILLER                  PIC X(6)  VALUE 'Change'.
081009     12  FILLER                  PIC X(5)  VALUE SPACES.
           12  FILLER                  PIC X(13) VALUE 'Life      A&H'.
           12  FILLER                  PIC X(9)  VALUE SPACES.
           12  FILLER                  PIC X(13) VALUE 'Life      A&H'.
       01  LINE-12.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN12-VALUE              PIC X(80) VALUE SPACES.
       01  DETAIL-1.
           12  FILLER                  PIC X     VALUE ' '.
           12  DT1-CERT-NO             PIC X(11) VALUE SPACES.
           12  DT1-NAME                PIC X(21) VALUE SPACES.
           12  DT1-DESC                PIC X(6)  VALUE SPACES.
           12  FILLER                  PIC XX    VALUE SPACES.
           12  DT1-ENT-LF              PIC ZZ,ZZZ.99- VALUE ZEROS.
           12  DT1-ENT-AH              PIC ZZ,ZZZ.99- VALUE ZEROS.
           12  DT1-CAL-LF              PIC ZZ,ZZZ.99- VALUE ZEROS.
           12  DT1-CAL-AH              PIC ZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-13.
           12  FILLER                  PIC X(51) VALUE
           ' Net Dollar Amount Change'.
           12  FILLER                  PIC X(28) VALUE
           'LIFE        A&H        TOTAL'.
       01  LINE-14.
           12  FILLER                  PIC X(48) VALUE
           ' Gross Premium/Refund changed by CSO'.
           12  LN14-CSO-LIFE           PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN14-CSO-AH             PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN14-CSO-TOTAL          PIC ZZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-15.
           12  FILLER                  PIC X(48) VALUE
           ' Gross Premium/Refund submitted by agent'.
           12  LN15-AGT-LIFE           PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN15-AGT-AH             PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN15-AGT-TOTAL          PIC ZZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-16.
           12  FILLER                  PIC X(48) VALUE
           ' Net                                    '.
           12  LN16-NET-LIFE           PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN16-NET-AH             PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN16-NET-TOTAL          PIC ZZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-17.
           12  FILLER                  PIC X(48) VALUE
           ' Commission Amount                      '.
           12  LN17-COM-LIFE           PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN17-COM-AH             PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN17-COM-TOTAL          PIC ZZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-18.
           12  FILLER                  PIC X(48) VALUE
           ' Net Due                                '.
           12  LN18-DUE-LIFE           PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN18-DUE-AH             PIC ZZZ,ZZZ.99- VALUE ZEROS.
           12  LN18-DUE-TOTAL          PIC ZZZ,ZZZ.99- VALUE ZEROS.
       01  LINE-19.
           12  FILLER                  PIC X(34) VALUE
           '-NET AMOUNT DUE WILL BE REFLECTED '.
           12  FILLER                  PIC X(25) VALUE
           'ON YOUR BILLING STATEMENT'.
       01  LINE-20.
           12  FILLER                  PIC X(16) VALUE
           '-NET AMOUNT DUE '.
           12  LN20-DUE-WHO            PIC X(7) VALUE SPACES.
           12  LN20-DUE-AMT            PIC ZZZ,ZZZ.99  VALUE ZEROS.
       01  LINE-21.
           12  FILLER                  PIC X(44) VALUE
           ' We are enclosing the following corrections:'.
       01  LINE-22.
           12  FILLER                  PIC X(3)  VALUE SPACES.
           12  LN22-CHG-CNT            PIC Z,ZZ9 VALUE ZEROS.
           12  FILLER                  PIC X(21) VALUE
           '  of Gen Change types'.
       01  LINE-23.
           12  FILLER                  PIC X(3)  VALUE SPACES.
           12  LN23-CAN-CNT            PIC Z,ZZ9 VALUE ZEROS.
           12  FILLER                  PIC X(24) VALUE
           '  of Cancel Change types'.
       01  LINE-24.
           12  FILLER                  PIC X(32) VALUE
           ' Thank you for your cooperation '.
           12  FILLER                  PIC X(27) VALUE
           'and assistance in making th'.
           12  FILLER                  PIC X(18) VALUE
           'is change.  If you'.
       01  LINE-25.
           12  FILLER                  PIC X(32) VALUE
           ' have any questions please call '.
           12  LN25-REST               PIC X(49) VALUE SPACES.
       01  LINE-26.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN26-ACCT-NAME          PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(5)  VALUE SPACES.
           12  LN26-WHO-COPY           PIC X(20) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE SPACES.
           12  LN26-PRINT-DATE         PIC X(8)  VALUE SPACES.
       01  LINE-27.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN27-FORM-NUMBER        PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(40) VALUE SPACES.
           12  LN27-FORM-DATE          PIC X(8)  VALUE SPACES.
       01  LINE-3A.
           12  FILLER                  PIC X(10) VALUE
           ' Insured: '.
           12  LN3A-NAME               PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(18) VALUE SPACES.
           12  FILLER                  PIC X(12) VALUE 'Policy No : '.
           12  LN3A-CERT-NO            PIC X(11) VALUE SPACES.
       01  LINE-4A.
           12  FILLER                  PIC X(54) VALUE
           ' In verifying the refund for this policy, we found the'.
           12  FILLER                  PIC X(22) VALUE
           ' following difference:'.
       01  LINE-5A.
           12  FILLER                  PIC X(33) VALUE
           ' Original Term of the policy was '.
           12  LN5A-REST               PIC X(15) VALUE SPACES.
       01  LINE-6A.
           12  FILLER                  PIC X(33) VALUE ' '.
           12  FILLER                  PIC X(13) VALUE
           'You Reported:'.
       01  LINE-7A.
           12  FILLER                  PIC X(50) VALUE ' '.
           12  FILLER                  PIC X(27) VALUE
           'Original            Premium'.
       01  LINE-8A.
           12  FILLER                  PIC X(23) VALUE ' '.
           12  FILLER                  PIC X(27) VALUE
           'Yr. Mo. Da.'.
           12  FILLER                  PIC X(28) VALUE
           'Premium             Refunded'.
       01  LINE-9A.
           12  FILLER                  PIC X(24) VALUE
           ' Date of Cancellation   '.
           12  LN9A-CANC-DATE          PIC X(10) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Life       '.
           12  LN9A-LF-PREM            PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)    VALUE SPACES.
      *    12  LN9A-LF-PCT             PIC ZZZ.ZZ VALUE ZEROS.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN9A-LF-REF             PIC $$$,$$$.99.
       01  LINE-10A.
           12  FILLER                  PIC X(24) VALUE
           ' Date of Policy         '.
           12  LN10A-EFF-DATE          PIC X(10) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Disability '.
           12  LN10A-AH-PREM           PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)  VALUE SPACES.
      *    12  LN10A-AH-PCT            PIC ZZZ.ZZ VALUE ZEROS.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN10A-AH-REF            PIC $$$,$$$.99.
       01  LINE-11A.
           12  FILLER                  PIC X(24) VALUE
           ' Time in Force          '.
           12  LN11A-IN-FORCE.
               16  LN11A-IN-FORCE-YR   PIC 99    VALUE ZEROS.
               16  FILLER              PIC X     VALUE SPACES.
               16  LN11A-IN-FORCE-MO   PIC 99    VALUE ZEROS.
               16  FILLER              PIC X     VALUE SPACES.
               16  LN11A-IN-FORCE-DA   PIC 99    VALUE ZEROS.
           12  FILLER                  PIC XX    VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Total      '.
           12  LN11A-TOT-PREM          PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)  VALUE SPACES.
      *    12  LN11A-TOT-PCT           PIC ZZZ.ZZ.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN11A-TOT-REF           PIC $$$,$$$.99.
       01  LINE-12A.
           12  FILLER                  PIC X(12) VALUE
           ' Rounded to '.
           12  LN12A-ROUNDED           PIC X(20) VALUE SPACES.
       01  LINE-13A.
           12  FILLER                  PIC X(28) VALUE ' '.
           12  LN13A-REF-METHOD        PIC X(18) VALUE SPACES.
           12  FILLER                  PIC X(36) VALUE
           ' refund table used for life premium.'.
       01  LINE-14A.
           12  FILLER                  PIC X(22) VALUE ' '.
           12  LN14A-REF-METHOD        PIC X(18) VALUE SPACES.
           12  FILLER                  PIC X(42) VALUE
           ' refund table used for disability premium.'.
       01  LINE-15A.
           12  FILLER                  PIC X(33) VALUE ' '.
           12  FILLER                  PIC X(14) VALUE
           'We Calculated:'.
       01  LINE-16A.
           12  FILLER                  PIC X(50) VALUE ' '.
           12  FILLER                  PIC X(27) VALUE
           'Original            Premium'.
       01  LINE-17A.
           12  FILLER                  PIC X(23) VALUE ' '.
           12  FILLER                  PIC X(27) VALUE
           'Yr. Mo. Da.'.
           12  FILLER                  PIC X(28) VALUE
           'Premium             Refunded'.
       01  LINE-18A.
           12  FILLER                  PIC X(24) VALUE
           ' Date of Cancellation   '.
           12  LN18A-CANC-DATE         PIC X(10) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Life       '.
           12  LN18A-LF-PREM           PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)  VALUE SPACES.
      *    12  LN18A-LF-PCT            PIC ZZZ.ZZ.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN18A-LF-REF            PIC $$$,$$$.99.
       01  LINE-19A.
           12  FILLER                  PIC X(24) VALUE
           ' Date of Policy         '.
           12  LN19A-EFF-DATE          PIC X(10) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Disability '.
           12  LN19A-AH-PREM           PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)  VALUE SPACES.
      *    12  LN19A-AH-PCT            PIC ZZZ.ZZ.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN19A-AH-REF            PIC $$$,$$$.99.
       01  LINE-20A.
           12  FILLER                  PIC X(24) VALUE
           ' Time in Force          '.
           12  LN20A-IN-FORCE.
               16  LN20A-IN-FORCE-YR   PIC 99    VALUE ZEROS.
               16  FILLER              PIC X     VALUE SPACES.
               16  LN20A-IN-FORCE-MO   PIC 99    VALUE ZEROS.
               16  FILLER              PIC X     VALUE SPACES.
               16  LN20A-IN-FORCE-DA   PIC 99    VALUE ZEROS.
           12  FILLER                  PIC XX    VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE
           '    Total      '.
           12  LN20A-TOT-PREM          PIC $$$,$$$.99.
           12  FILLER                  PIC X(9)  VALUE SPACES.
      *    12  LN20A-TOT-PCT           PIC ZZZ.ZZ.
      *    12  FILLER                  PIC X     VALUE '%'.
           12  LN20A-TOT-REF           PIC $$$,$$$.99.
       01  LINE-21A.
           12  FILLER                  PIC X(12) VALUE
           ' Rounded to '.
           12  LN21A-ROUNDED           PIC X(20) VALUE SPACES.
       01  LINE-22A.
           12  FILLER                  PIC X(28) VALUE ' '.
           12  LN22A-REF-METHOD        PIC X(18) VALUE SPACES.
           12  FILLER                  PIC X(36) VALUE
           ' refund table used for life premium.'.
       01  LINE-23A.
           12  FILLER                  PIC X(22) VALUE ' '.
           12  LN23A-REF-METHOD        PIC X(18) VALUE SPACES.
           12  FILLER                  PIC X(42) VALUE
           ' refund table used for disability premium.'.
       01  LINE-24A.
           12  FILLER                  PIC X(33) VALUE ' '.
           12  LN24A-DESC              PIC X(30) VALUE
           'Overpayment to Insured        '.
           12  LN24A-AMOUNT            PIC $$$,$$$.99.
       01  LINE-25A.
           12  FILLER                  PIC X(24) VALUE
           ' Reasons for difference:'.
       01  LINE-26A.
           12  FILLER                  PIC X     VALUE ' '.
053107     12  LN26A-COMMENT1          PIC X(75) VALUE ' '.
       01  LINE-26A1.
           12  FILLER                  PIC X     VALUE ' '.
053107     12  LN26A1-COMMENT2         PIC X(75) VALUE ' '.
       01  LINE-27A.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN27A-ACCT-NAME         PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(5)  VALUE SPACES.
           12  LN27A-WHO-COPY          PIC X(20) VALUE SPACES.
           12  FILLER                  PIC X(15) VALUE SPACES.
           12  LN27A-PRINT-DATE        PIC X(8)  VALUE SPACES.
       01  LINE-3B.
052302     12  FILLER                  PIC X(27) VALUE '-'.
110410     12  LINE-3B-NOTICE          PIC X(26) VALUE
           'General Change Endorsement'.
033006 01  LINE-3B-VA-NM.
033006     12  FILLER                  PIC X(27) VALUE '-'.
110410     12  LINE-3B-NOTICE-VANM     PIC X(26) VALUE
033006     '  Notification of Change  '.
       01  LINE-4B.
           12  FILLER                  PIC X(54) VALUE
052302     '-This endorsement is to be attached and made a part of'.
           12  FILLER                  PIC X(23) VALUE
           ' the policy/certificate'.
       01  LINE-5B.
           12  FILLER                  PIC X(54) VALUE
           ' listed below.  All policy/certificate provisions not '.
           12  FILLER                  PIC X(23) VALUE
           'in conflict with this  '.
       01  LINE-6B.
           12  FILLER                  PIC X(54) VALUE
           ' endorsement apply.                                   '.
       01  LINE-7B.
           12  FILLER                  PIC X(54) VALUE
           '0The effective date of this endorsement is the same as'.
           12  FILLER                  PIC X(23) VALUE
           ' the policy/certificate'.
       01  LINE-8B.
           12  FILLER                  PIC X(54) VALUE
           ' effective date.  The policy/certificate is amended as'.
           12  FILLER                  PIC X(23) VALUE
           ' follows:              '.
       01  LINE-9B.
           12  FILLER                  PIC X(19) VALUE
           '0Primary Borrower: '.
           12  LN9B-NAME               PIC X(26) VALUE SPACES.
           12  FILLER                  PIC X(24) VALUE
           'Policy/Certificate No.: '.
           12  LN9B-CERT-NO            PIC X(11) VALUE ' '.
       01  LINE-10B.
           12  FILLER                  PIC X(36) VALUE
           ' Policy/Certificate Effective Date: '.
           12  LN10B-EFF-DATE          PIC X(10) VALUE SPACES.
           12  FILLER                  PIC X(09) VALUE SPACES.
           12  FILLER                  PIC X(14) VALUE
           'Agent Number: '.
           12  LN10B-ACCT-NO           PIC X(10) VALUE ' '.
       01  LINE-11B.
           12  FILLER                  PIC X(50) VALUE '0'.
           12  FILLER                  PIC X(7)  VALUE 'Changed'.
       01  LINE-12B.
           12  FILLER                  PIC X(46) VALUE ' '.
           12  FILLER                  PIC X(15) VALUE
           'From         To'.
       01  LINE-13B1.
           12  FILLER                  PIC X(37) VALUE
           ' Original Amount of Life Insurance'.
           12  LN13B1-LF-ENT           PIC $$,$$$,$$$.99.
           12  FILLER                  PIC XX VALUE '  '.
           12  LN13B1-LF-CAL           PIC $$,$$$,$$$.99.
       01  LINE-13B2.
           12  FILLER                  PIC X(37) VALUE
           ' Original Amount of Health Insurance'.
           12  LN13B2-AH-ENT           PIC $$,$$$,$$$.99.
           12  FILLER                  PIC XX VALUE '  '.
           12  LN13B2-AH-CAL           PIC $$,$$$,$$$.99.
       01  LINE-13B.
           12  FILLER                  PIC X(40) VALUE
           ' Life Premium'.
           12  LN13B-LF-ENT            PIC $$$,$$$.99.
           12  FILLER                  PIC X(5) VALUE '  '.
           12  LN13B-LF-CAL            PIC $$$,$$$.99.
       01  LINE-14B.
           12  FILLER                  PIC X(40) VALUE
           ' Disability Premium '.
           12  LN14B-AH-ENT            PIC $$$,$$$.99.
           12  FILLER                  PIC X(5) VALUE '  '.
           12  LN14B-AH-CAL            PIC $$$,$$$.99.
       01  LINE-14B1.
           12  FILLER                  PIC X VALUE ' '.
053107     12  LN14B1-COMMENT1         PIC X(75).
       01  LINE-14B2.
           12  FILLER                  PIC X VALUE ' '.
053107     12  LN14B2-COMMENT2         PIC X(75).
       01  LINE-15B.
           12  FILLER                  PIC X(47) VALUE
           ' Total Premium due Central States Health & Life'.
           12  FILLER                  PIC X(15) VALUE
           ' Co. of Omaha  '.
           12  LN15B-AMT               PIC $$$,$$$.99.
       01  LINE-15B1.
           12  FILLER                  PIC X(28) VALUE
           ' Total Premium due Borrower '.
           12  LN15B1-AMT              PIC $$$,$$$.99.
       01  LINE-16B1.
           12  FILLER                  PIC X(54) VALUE
           '0Signature of Insured : ______________________________'.
           12  FILLER                  PIC X(24) VALUE
           '  Date : _______________'.
       01  LINE-16B2.
           12  FILLER                  PIC X(54) VALUE
           '0Signature of Joint Insured : ________________________'.
           12  FILLER                  PIC X(24) VALUE
           '  Date : _______________'.
122706 01  LINE-16B-CARR8.
122706     12  FILLER                  PIC X(25) VALUE '0'.
122706     12  FILLER                  PIC X(41) VALUE
122706     'Investors Heritage Life Insurance Company'.
122706     12  FILLER                  PIC X(15) VALUE SPACES.
       01  LINE-16B.
052302     12  FILLER                  PIC X(25) VALUE '0'.
052302     12  FILLER                  PIC X(41) VALUE
           'Central States Health & Life Co. of Omaha'.
052302     12  FILLER                  PIC X(15) VALUE SPACES.
122706 01  LINE-17B-CARR8.
122706     12  FILLER                  PIC X(29) VALUE ' '.
122706     12  FILLER                  PIC X(33) VALUE
122706     '            President            '.
122706     12  FILLER                  PIC X(11) VALUE SPACES.
       01  LINE-17B.
           12  FILLER                  PIC X(29) VALUE ' '.
052302     12  FILLER                  PIC X(33) VALUE
042605*    'Chairman, Chief Executive Officer'.
042605     '             Chairman            '.
052302     12  FILLER                  PIC X(11) VALUE SPACES.
       01  LINE-17B1.
           12  FILLER                  PIC X     VALUE ' '.
           12  FILLER                  PIC X(51) VALUE
           '*   Please return a signed copy of this endorsement'.
           12  FILLER                  PIC X(27) VALUE
           ' to Central States.       *'.
033006 01  LINE-17B1-VA-NM.
033006     12  FILLER                  PIC X     VALUE ' '.
033006     12  FILLER                  PIC X(46) VALUE
033006     '*   Please return a signed copy of this notice'.
033006     12  FILLER                  PIC X(32) VALUE
033006     ' to Central States.            *'.
       01  LINE-18B.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN18B-ACCT-NAME         PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(5)  VALUE SPACES.
           12  LN18B-WHO-COPY          PIC X(20) VALUE SPACES.
           12  FILLER                  PIC X(14) VALUE SPACES.
           12  LN18B-PRINT-DATE        PIC X(10) VALUE SPACES.
       01  LINE-19B.
           12  FILLER                  PIC X     VALUE ' '.
           12  LN19B-FORM-NUMBER       PIC X(30) VALUE SPACES.
           12  FILLER                  PIC X(40) VALUE SPACES.
           12  LN19B-FORM-DATE         PIC X(8)  VALUE SPACES.
122706 01  LINE-20B.
122706     12  FILLER                  PIC X(8)  VALUE ' '.
122706     12  LN20B-LEASE-COMMENT     PIC X(30) VALUE SPACES.
       01  WS-SUMMARY-AREA.
           12  FILLER OCCURS 200.
               16  WS-DET-CERT-NO      PIC X(11).
               16  WS-DET-NAME         PIC X(30).
               16  WS-DET-TYPE         PIC X(6).
               16  WS-DET-LF-ENT       PIC S9(7)V99 COMP-3.
               16  WS-DET-AH-ENT       PIC S9(7)V99 COMP-3.
               16  WS-DET-LF-CAL       PIC S9(7)V99 COMP-3.
               16  WS-DET-AH-CAL       PIC S9(7)V99 COMP-3.
               16  WS-DET-LF-COMM      PIC S9(5)V99 COMP-3.
               16  WS-DET-AH-COMM      PIC S9(5)V99 COMP-3.
           12  SUB1                    PIC S9(3)    COMP-3.
           12  NOTE-SUB                PIC S9(3)    COMP-3.
           12  WS-TOT-LF-CAL           PIC S9(9)V99 COMP-3.
           12  WS-TOT-AH-CAL           PIC S9(9)V99 COMP-3.
           12  WS-TOT-CAL              PIC S9(9)V99 COMP-3.
           12  WS-TOT-LF-ENT           PIC S9(9)V99 COMP-3.
           12  WS-TOT-AH-ENT           PIC S9(9)V99 COMP-3.
           12  WS-TOT-ENT              PIC S9(9)V99 COMP-3.
           12  WS-TOT-LF-COMM          PIC S9(7)V99 COMP-3.
           12  WS-TOT-AH-COMM          PIC S9(7)V99 COMP-3.
           12  WS-TOT-COMM             PIC S9(7)V99 COMP-3.
           12  WS-TOT-NET              PIC S9(9)V99 COMP-3.
           12  WS-TOT-NET-DUE          PIC S9(9)V99 COMP-3.
           12  WS-TOT-ISSUES           PIC S9(5)    COMP-3.
           12  WS-TOT-CANCELS          PIC S9(5)    COMP-3.
           12  WS-BILL-SW              PIC X.
           12  WS-CSR-TO-USE           PIC X(30).
       01  FILLER                      PIC  X(25)
                                   VALUE 'PROGRAM INTERFACE STARTS:'.
      *                                COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
           12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
      **********************************************************
               16  PI-695-PRINT-DATE   PIC  X(08).
               16  PI-695-PRINT-DATE-BIN
                                       PIC  X(02).
               16  PI-695-PRINT-ID     PIC  X(04).
               16  PI-695-PRINT-KEY.
                   20  PI-695-PRINT-CARRIER
                                       PIC  X(01).
                   20  PI-695-PRINT-GROUPING
                                       PIC  X(06).
                   20  PI-695-PRINT-STATE
                                       PIC  X(02).
                   20  PI-695-PRINT-ACCOUNT
                                       PIC  X(10).
               16  PI-695-PRINT-PROCESSOR
                                       PIC  X(04).
               16  PI-695-ISSREF-TYPE  PIC  X(01).
               16  FILLER              PIC  X(602).
                                       EJECT
      *    COPY ELPRTCVD.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELPRTCVD.                          *
00004 *                            VMOD=2.001                         *
00005 *****************************************************************.
00006
00007 ******************************************************************
00008 ***   WORK AREAS  FOR TERMINAL ONLINE PRINT ROUTINE
00009 ***                 -ELPRTCVD-
00010 ***   TO BE USED WITH PROCEDURE COPY MEMBER -ELPRTCVP-
00011 ******************************************************************
00012
00013  01  S-WORK-AREA                     SYNC.
00014      12  WS-LINE-LEN                 PIC S9(4)       VALUE +80
00015                                      COMP.
00016
00017      12  WS-LINE-LENGTH              PIC S9(4)       VALUE ZERO
00018                                      COMP.
00019
00020      12  WS-BUFFER-SIZE              PIC S9(4)       VALUE +1916
00021                                      COMP.
00022
00023      12  WS-BUFFER-LENGTH            PIC S9(4)       VALUE ZERO
00024                                      COMP.
00025
00026      12  WS-PROG-END                 PIC X           VALUE SPACES.
00027
00028      12  WS-PRINT-AREA.
00029          16  WS-PASSED-CNTL-CHAR     PIC X           VALUE SPACES.
00030            88  SINGLE-SPACE                          VALUE ' '.
00031            88  DOUBLE-SPACE                          VALUE '0'.
00032            88  TRIPLE-SPACE                          VALUE '-'.
00033            88  TOP-PAGE                              VALUE '1'.
00034
00035          16  WS-PASSED-DATA.
00036              20  WS-PRINT-BYTE       PIC X
00037                  OCCURS 132 TIMES    INDEXED BY PRT-INDEX.
00038
00039      12  WS-LINE-CNT                 PIC S9(3)        VALUE ZERO
00040                                      COMP-3.
00041      12  WS-WCC-CNTL                 PIC X(1)         VALUE 'H'.
00042
00043      12  WS-EM                       PIC S9(4)        VALUE +25
00044                                      COMP.
00045      12  FILLER   REDEFINES WS-EM.
00046          16  FILLER                  PIC X.
00047          16  T-EM                    PIC X.
00048
00049 *    12  WS-SS                       PIC S9(4)        VALUE +21
00049      12  WS-SS                       PIC S9(4)        VALUE +10
00050                                      COMP.
00051      12  FILLER   REDEFINES WS-SS.
00052          16  FILLER                  PIC X.
00053          16  T-SS                    PIC X.
00054
00055      12  WS-TP                       PIC S9(4)      VALUE +12
00056                                      COMP.
00057      12  FILLER   REDEFINES WS-TP.
00058          16  FILLER                  PIC X.
00059          16  T-TP                    PIC X.
00060
00061      12  WS-FIRST-TIME-SW            PIC X           VALUE '1'.
00062          88  FIRST-TIME                              VALUE '1'.
00063          88  FIRST-LINE-NEXT-BUFFER                  VALUE '2'.
00064
00065      12  WS-BUFFER-AREA.
00066          16  WS-BUFFER-BYTE          PIC X
00067              OCCURS 1920 TIMES       INDEXED BY BUFFER-INDEX
00068                                                 BUFFER-INDEX2.
00069
00070 ******************************************************************
       01  FILLER.
           16  FILLER                  PIC  X(200)
               VALUE 'THIS IS PART OF THE BUFFER ZONE'.
                                       EJECT
       01  FILLER                      PIC  X(18)
                                   VALUE 'WORK TABLE STARTS:'.
       01  W-ADJUST-AREA.
           12  FILLER                  PIC  X(07).
           12  W-AD-PRINT-AREA         PIC  X(70).
           12  FILLER                  PIC  X(03).
       01  W-WORK-TABLE.
           12  W-WORK-LINE OCCURS 300 TIMES
                                INDEXED BY W-WK-NDX.
               16  W-TEXT-LINE         PIC  X(70).
               16  W-SKIP-CONTROL      PIC  X(02).
                   88  W-NO-LINES-SKIPPED            VALUE SPACES.
                   88  W-SKIP-TO-NEXT-PAGE           VALUE '99'.
                                       EJECT
       01  FILLER                      PIC  X(16)
                                   VALUE 'WORK TABLE ENDS:'.
                                       EJECT
      *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
                                       EJECT
      *    COPY ERCENDR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCENDR.                            *
      *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ENDORSEMENT SUMMARY BY ACCOUNT            *
      *                                                                *
      *                                                                *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
PEMMOD*   RECORD SIZE = 513  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERENDR                         RKP=2,LEN=36   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
052307* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
052307*                                WITH OPEN CLAIMS
052307*-----------------------------------------------------------------
       01  ENDORSEMENT-RECORD.
           12  EN-RECORD-ID            PIC XX.
               88  VALID-EN-ID             VALUE 'EN'.
           12  EN-CONTROL-PRIMARY.
               16  EN-COMPANY-CD       PIC X.
               16  EN-CARRIER          PIC X.
               16  EN-GROUPING         PIC X(6).
               16  EN-STATE            PIC XX.
               16  EN-ACCOUNT          PIC X(10).
               16  EN-CERT-EFF-DT      PIC XX.
               16  EN-CERT-NO.
                   20  EN-CERT-PRIME   PIC X(10).
                   20  EN-CERT-SFX     PIC X.
               16  EN-SEQ-NO           PIC S9(04) COMP.
               16  EN-REC-TYPE         PIC X.
                   88  EN-ISSUE             VALUE '1'.
                   88  EN-CANCELLATION      VALUE '2'.
           12  EN-LAST-MAINT-DT        PIC XX.
           12  EN-LAST-MAINT-BY        PIC X(4).
           12  EN-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
           12  EN-LAST-NAME            PIC X(15).
           12  EN-FIRST-NAME           PIC X(10).
           12  EN-MIDDLE-INIT          PIC X.
           12  EN-JOINT-LAST-NAME      PIC X(15).
           12  EN-JOINT-FIRST-NAME     PIC X(10).
           12  EN-JOINT-MIDDLE-INIT    PIC X.
           12  EN-LF-TERM              PIC S999      COMP-3.
           12  EN-AH-TERM              PIC S999      COMP-3.
           12  EN-LF-BEN-ENT-AMT       PIC S9(9)V99  COMP-3.
           12  EN-AH-BEN-ENT-AMT       PIC S9(7)V99  COMP-3.
           12  EN-LF-BEN-CAL-AMT       PIC S9(9)V99  COMP-3.
           12  EN-AH-BEN-CAL-AMT       PIC S9(7)V99  COMP-3.
           12  EN-LF-PREM-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-PREM-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-CANC-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-PREM-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-PREM-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-CANC-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-DT           PIC XX.
           12  EN-AH-CANC-DT           PIC XX.
           12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-LF-BEN-CD            PIC XX.
           12  EN-AH-BEN-CD            PIC XX.
052307*           12  FILLER                  PIC X(18).
052307     12  FILLER                  PIC X(17).
052307     12  EN-FLAG-CERT            PIC X.
           12  EN-INPUT-DT             PIC XX.
           12  EN-PRINT-DT             PIC XX.
           12  EN-SIG-SW               PIC X.
           12  EN-COMMENTS1            PIC X(70).
           12  EN-COMMENTS2            PIC X(70).
PEMMOD     12  EN-COMMENTS3            PIC X(70).
PEMMOD     12  EN-COMMENTS4            PIC X(70).
           12  FILLER                  PIC X(30).
      ******************************************************************
                                       EJECT
      *    COPY ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
041320         16  CN-RECORD-TYPE          PIC X.
041320             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
041320             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
041320     12  FILLER                      PIC X(5).
00048 ******************************************************************
                                       EJECT
      *    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
                                       EJECT
      *    COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
                                       EJECT
      *    COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
                                       EJECT
      *    COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
                                       EJECT
      *    COPY ELCDMD34.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMD34.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DMD DLO034 PARAMETER AREA                 *
00007 *                                                                *
00008 *    LENGTH = 272    RECFRM = FIXED                              *
00009 *                                                                *
00010 ******************************************************************
00011  01  DLO034-COMMUNICATION-AREA.
00012      12  DL34-PROCESS-TYPE             PIC X.
00013      12  DL34-COMPANY-ID               PIC XXX.
00014      12  DL34-PRINT-PROGRAM-ID         PIC X(8).
00015      12  DL34-USERID                   PIC X(4).
00016      12  DL34-PRINT-LINE               PIC X(250).
00017      12  DL34-OVERRIDE-PRINTER-ID      PIC X(4).
00018      12  DL34-RETURN-CODE              PIC XX.
00019  01  DLO034-REC-LENGTH                 PIC S9(4) COMP VALUE +272.
110509
110509*    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
      *01 PARMLIST .
      *    02  FILLER                  PIC S9(08) COMP.
      *    02  L-ARCH-POINTER          PIC S9(08) COMP.
      *    02  L-ARCT-POINTER          PIC S9(08) COMP.
                                       EJECT
       01  DFHCOMMAREA       PIC X(01).
       01  L-LETTER-ARCHIVE            PIC X(250).
                                       EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA L-LETTER-ARCHIVE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6952' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-CURRENT-DATE
           MOVE DC-BIN-DATE-1          TO W-SAVE-CURRENT-BIN-DATE
           MOVE SPACES                 TO DL34-PROCESS-TYPE
           .
       0100-RETRIEVE-LOOP.
           
      * EXEC CICS HANDLE CONDITION
      *         ENDDATA (0200-END-DATA)
      *         NOTFND  (0300-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$&I                  ! " #00004691' TO DFHEIV0
           MOVE X'222426492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS RETRIEVE
      *         INTO    (PROGRAM-INTERFACE-BLOCK)
      *         LENGTH  (PI-COMM-LENGTH)
      *    END-EXEC
      *    MOVE '0*I L                 &   #00004695' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
               IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
                   MOVE 'O'                TO DL34-PROCESS-TYPE
                   MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
                   MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
                   MOVE PI-PROCESSOR-ID    TO DL34-USERID
                   MOVE SPACES             TO DL34-PRINT-LINE
                   MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
                   
      * EXEC CICS LINK
      *                PROGRAM    ('DLO034')
      *                COMMAREA   (DLO034-COMMUNICATION-AREA)
      *                LENGTH     (DLO034-REC-LENGTH)
      *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00004708' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                   IF DL34-RETURN-CODE NOT = 'OK'
                       MOVE  '**DLO034 OPEN ERROR - ABORT**'
                                           TO W-ERROR-LINE
                       PERFORM 0400-SEND-TEXT
                       
      * EXEC CICS RETURN
      *                END-EXEC.
      *    MOVE '.(                    ''   #00004717' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-ERENDR THRU 2000-EXIT.
       0200-END-DATA.
           MOVE '1'                    TO WS-PRINT-AREA.
           MOVE W-ASTERISK-LINE1       TO WS-PASSED-DATA.
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
           MOVE SPACES                 TO WS-PRINT-AREA.
           MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
           MOVE '0'                    TO WS-PRINT-AREA
           MOVE '          End of Endorsement Print '
                                       TO WS-PASSED-DATA
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
           MOVE '0'                    TO WS-PRINT-AREA.
           MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
           MOVE SPACES                 TO WS-PRINT-AREA.
           MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
      *    MOVE '1'                    TO WS-PRINT-AREA.
      *    MOVE SPACES                 TO WS-PASSED-DATA.
      *    PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
           MOVE 'X'                    TO WS-PROG-END.
           PERFORM ELPRTCVP            THRU ELPRTCVP-EXIT.
      * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
               MOVE 'C'                TO DL34-PROCESS-TYPE
               MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
               MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
               MOVE PI-PROCESSOR-ID    TO DL34-USERID
               MOVE SPACES             TO DL34-PRINT-LINE
                                          DL34-OVERRIDE-PRINTER-ID
               
      * EXEC CICS LINK
      *            PROGRAM    ('DLO034')
      *            COMMAREA   (DLO034-COMMUNICATION-AREA)
      *            LENGTH     (DLO034-REC-LENGTH)
      *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00004751' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               IF DL34-RETURN-CODE NOT = 'OK'
                   MOVE  '**DLO034 CLOSE ERROR - ABORT**'
                                       TO W-ERROR-LINE
                   PERFORM 0400-SEND-TEXT.
           
      * EXEC CICS RETURN
      *    END-EXEC
      *    MOVE '.(                    ''   #00004760' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0300-NOT-FOUND.
           MOVE 'NO COMMUNICATION AREA FOUND'
                                       TO W-ERROR-LINE.
           PERFORM 0400-SEND-TEXT.
           GO TO 0200-END-DATA.
       0400-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM   (W-ERROR-LINE)
      *        LENGTH (70)
      *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00004769' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERROR-LINE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
                                       EJECT
       1000-INITIALIZE.
           MOVE SPACES                 TO W-ADJUST-AREA
           MOVE W-SAVE-CURRENT-BIN-DATE
                                       TO W-CURRENT-SAVE
           MOVE PI-ENTRY-CODES         TO W-OPTION-CODES
           PERFORM 1010-INIT-TABLE     THRU 1010-EXIT
           .
       1000-EXIT.
           EXIT.
                                       EJECT
       1010-INIT-TABLE.
           MOVE SPACES                 TO WS-SUMMARY-AREA
           MOVE +0                     TO WS-TOT-LF-CAL
                                          WS-TOT-AH-CAL
                                          WS-TOT-LF-ENT
                                          WS-TOT-AH-ENT
                                          WS-TOT-LF-COMM
                                          WS-TOT-AH-COMM
                                          WS-TOT-ISSUES
                                          WS-TOT-CANCELS
                                          WS-TOT-CAL
                                          WS-TOT-ENT
                                          WS-TOT-NET
                                          WS-TOT-NET-DUE
                                          WS-TOT-COMM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
               (SUB1 > +200)
               MOVE +0                 TO WS-DET-LF-ENT (SUB1)
                                          WS-DET-AH-ENT (SUB1)
                                          WS-DET-LF-CAL (SUB1)
                                          WS-DET-AH-CAL (SUB1)
                                          WS-DET-LF-COMM (SUB1)
                                          WS-DET-AH-COMM (SUB1)
           END-PERFORM
           MOVE +0                     TO SUB1
           .
       1010-EXIT.
           EXIT.
       2000-PROCESS-ERENDR.
           MOVE LOW-VALUES             TO W-ERENDR-KEY
           MOVE PI-COMPANY-CD          TO W-ENDR-COMPANY-CD
           PERFORM 2010-ERENDR-STARTBR THRU 2010-EXIT
           IF NOT RESP-NORMAL
              GO TO 2000-EXIT
           END-IF
           PERFORM 2020-ERENDR-READNEXT THRU 2020-EXIT
           IF RESP-NORMAL
              MOVE EN-CONTROL-PRIMARY (1:20)
                                       TO WS-ERENDR-HOLD-KEY
           END-IF
           IF NOT RESP-NORMAL
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-IF
           PERFORM 2030-PROCESS-ERENDR THRU 2030-EXIT UNTIL
              THERE-ARE-NO-MORE-RECORDS
           PERFORM 2070-PRINT-SUMMARY  THRU 2070-EXIT
           .
       2000-EXIT.
           EXIT.
       2010-ERENDR-STARTBR.
           
      * EXEC CICS STARTBR
      *       DATASET    (W-ERENDR-ID)
      *       RIDFLD     (W-ERENDR-KEY)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004834' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERENDR-ID, 
                 W-ERENDR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              SET ENDR-BROWSE-STARTED TO TRUE
           END-IF
           .
       2010-EXIT.
           EXIT.
       2012-ERENDR-ENDBR.
           
      * EXEC CICS ENDBR
      *       DATASET    (W-ERENDR-ID)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $  N#00004846' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERENDR-ID, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              MOVE ' '                 TO WS-ENDR-BROWSE-SW
           END-IF
           .
       2012-EXIT.
           EXIT.
       2015-ERENDR-REWRITE.
           
      * EXEC CICS REWRITE
      *       DATASET    (W-ERENDR-ID)
      *       FROM       (ENDORSEMENT-RECORD)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00004857' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERENDR-ID, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       2015-EXIT.
           EXIT.
       2020-ERENDR-READNEXT.
           
      * EXEC CICS READNEXT
      *       DATASET    (W-ERENDR-ID)
      *       RIDFLD     (W-ERENDR-KEY)
      *       INTO       (ENDORSEMENT-RECORD)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004866' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERENDR-ID, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV12, 
                 W-ERENDR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       2020-EXIT.
           EXIT.
       2025-ERENDR-READ-UPDATE.
           
      * EXEC CICS READ
      *       UPDATE
      *       DATASET    (W-ERENDR-ID)
      *       RIDFLD     (W-ERENDR-KEY)
      *       INTO       (ENDORSEMENT-RECORD)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00004876' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERENDR-ID, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 W-ERENDR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           ADD +1                      TO W-RECORD-COUNT
           IF W-RECORD-COUNT > +100
              MOVE +0                  TO W-RECORD-COUNT
              
      * EXEC CICS DELAY
      *            INTERVAL (W-DELAY-INTERVAL)
      *       END-EXEC
      *    MOVE '0$I                   &   #00004886' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       2025-EXIT.
           EXIT.
       2030-PROCESS-ERENDR.
           IF W-REPRINT
              MOVE ' '                 TO WS-REPRINT-SW
              PERFORM 2040-DO-WE-REPRINT
                                       THRU 2040-EXIT
              IF WE-REPRINT
                 IF EN-ISSUE
                    PERFORM 2050-PRINT-ISSUE
                                       THRU 2050-EXIT
                 ELSE
                    PERFORM 2060-PRINT-CANCEL
                                       THRU 2060-EXIT
                 END-IF
                 PERFORM 2075-ERENDR-UPDATE
                                       THRU 2075-EXIT
              END-IF
           ELSE
              MOVE ' '                 TO WS-PRINT-SW
              PERFORM 2045-DO-WE-PRINT THRU 2045-EXIT
              IF WE-PRINT
                 IF EN-ISSUE
                    PERFORM 2050-PRINT-ISSUE
                                       THRU 2050-EXIT
                 ELSE
                    PERFORM 2060-PRINT-CANCEL
                                       THRU 2060-EXIT
                 END-IF
                 PERFORM 2075-ERENDR-UPDATE
                                       THRU 2075-EXIT
              END-IF
           END-IF
           PERFORM 2020-ERENDR-READNEXT THRU 2020-EXIT
           IF (NOT RESP-NORMAL) OR
              (PI-COMPANY-CD NOT = EN-COMPANY-CD)
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-IF
           .
       2030-EXIT.
           EXIT.
       2040-DO-WE-REPRINT.
           IF EN-PRINT-DT = PI-695-PRINT-DATE-BIN
              IF (PI-695-PRINT-CARRIER = SPACES OR EN-CARRIER)
                         AND
                 (PI-695-PRINT-GROUPING = SPACES OR EN-GROUPING)
                         AND
                 (PI-695-PRINT-STATE = SPACES OR EN-STATE)
                         AND
                 (PI-695-PRINT-ACCOUNT = SPACES OR EN-ACCOUNT)
                         AND
                 (PI-695-PRINT-PROCESSOR = SPACES OR EN-LAST-MAINT-BY)
                         AND
                 (PI-695-ISSREF-TYPE = SPACES OR EN-REC-TYPE)
                 SET WE-REPRINT TO TRUE
              END-IF
           END-IF
           .
       2040-EXIT.
           EXIT.
       2045-DO-WE-PRINT.
           IF EN-PRINT-DT = PI-695-PRINT-DATE-BIN
              IF (PI-695-PRINT-CARRIER = SPACES OR EN-CARRIER)
                         AND
                 (PI-695-PRINT-GROUPING = SPACES OR EN-GROUPING)
                         AND
                 (PI-695-PRINT-STATE = SPACES OR EN-STATE)
                         AND
                 (PI-695-PRINT-ACCOUNT = SPACES OR EN-ACCOUNT)
                         AND
                 (PI-695-PRINT-PROCESSOR = SPACES OR EN-LAST-MAINT-BY)
                         AND
                 (PI-695-ISSREF-TYPE = SPACES OR EN-REC-TYPE)
                 SET WE-PRINT TO TRUE
              END-IF
           END-IF
           .
       2045-EXIT.
           EXIT.
       2050-PRINT-ISSUE.
           IF WS-ERENDR-HOLD-KEY NOT = EN-CONTROL-PRIMARY (1:20)
              PERFORM 2070-PRINT-SUMMARY THRU 2070-EXIT
              PERFORM 1010-INIT-TABLE THRU 1010-EXIT
              MOVE EN-CONTROL-PRIMARY (1:20) TO WS-ERENDR-HOLD-KEY
           END-IF
           PERFORM 3000-MATCH-TO-ACCT  THRU 3000-EXIT
           PERFORM 3050-MATCH-TO-CSR   THRU 3050-EXIT
           MOVE SPACES                 TO LN9B-NAME
081009     IF EN-MIDDLE-INIT > SPACES
081009         STRING EN-FIRST-NAME DELIMITED BY '  '
081009             ' ' EN-MIDDLE-INIT ' ' EN-LAST-NAME
081009             DELIMITED BY '   ' INTO LN9B-NAME
081009         END-STRING
081009     ELSE
081009         STRING EN-FIRST-NAME DELIMITED BY '  '
081009         ' ' EN-LAST-NAME
081009         DELIMITED BY '   ' INTO LN9B-NAME
081009         END-STRING
081009     END-IF
           MOVE EN-CERT-PRIME          TO LN9B-CERT-NO (1:10)
           MOVE EN-CERT-SFX            TO LN9B-CERT-NO (11:1)
           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LN10B-EFF-DATE
           END-IF
           MOVE EN-ACCOUNT             TO LN10B-ACCT-NO
           MOVE EN-LF-PREM-ENT-AMT     TO LN13B-LF-ENT
           MOVE EN-LF-PREM-CAL-AMT     TO LN13B-LF-CAL
           MOVE EN-LF-BEN-ENT-AMT      TO LN13B1-LF-ENT
           MOVE EN-LF-BEN-CAL-AMT      TO LN13B1-LF-CAL
           MOVE EN-AH-BEN-ENT-AMT      TO LN13B2-AH-ENT
           MOVE EN-AH-BEN-CAL-AMT      TO LN13B2-AH-CAL
           IF EN-LF-TERM NOT = ZEROS
              MOVE EN-LF-TERM          TO W-EDIT-3-0
              MOVE W-EDIT-3-0          TO W-VG-TEXT (1)
           ELSE
              MOVE EN-AH-TERM          TO W-EDIT-3-0
              MOVE W-EDIT-3-0          TO W-VG-TEXT (1)
           END-IF
           MOVE LN10B-EFF-DATE         TO W-VG-TEXT (2)
081309     MOVE EN-LF-BEN-CAL-AMT      TO W-EDIT-9-2
           MOVE W-EDIT-9-2             TO W-VG-TEXT (3)
           MOVE LN9B-NAME              TO W-VG-TEXT (4)
           MOVE SPACES                 TO W-VG-TEXT (5)
           STRING EN-JOINT-FIRST-NAME ' '
                  EN-JOINT-MIDDLE-INIT ' '
                  EN-JOINT-LAST-NAME
                  DELIMITED BY '   '  INTO W-VG-TEXT (5)
           END-STRING
           MOVE SPACES                 TO WS-COMMENT-LINES1
                                          WS-COMMENT-LINES2
110410                                    WS-RES-REF-SW
           MOVE +1                     TO W-SUB2
           IF (EN-COMMENTS1 (1:4) NOT = SPACES) AND
              (EN-COMMENTS1 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS1 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS1    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS1        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS2 (1:4) NOT = SPACES) AND
              (EN-COMMENTS2 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS2 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS2    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS2        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS3 (1:4) NOT = SPACES) AND
              (EN-COMMENTS3 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS3 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS3    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS3        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS4 (1:4) NOT = SPACES) AND
              (EN-COMMENTS4 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS4 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS4    TO WS-TEXT (W-SUB2)
081209        END-IF
           ELSE
              MOVE EN-COMMENTS4        TO WS-TEXT (W-SUB2)
           END-IF
           PERFORM 5040-RES-VAR        THRU 5040-EXIT
           MOVE WS-COMMENT-LINES2      TO WS-COMMENT-LINES1
           MOVE SPACES                 TO WS-COMMENT-LINES2
                                          WS-LAST-CHAR
           PERFORM VARYING W-SUB1 FROM +1 BY +1 UNTIL
              (W-SUB1 > +10)
              MOVE +1                  TO OUT-SUB
              PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
053107           (IN-SUB > +75)
                 IF (WS-CHAR1 (W-SUB1 IN-SUB) = ' ') AND
                    (WS-LAST-CHAR = ' ')
                    CONTINUE
                 ELSE
                    MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO WS-CHAR2 (W-SUB1 OUT-SUB)
                                          WS-LAST-CHAR
                    ADD +1             TO OUT-SUB
                 END-IF
              END-PERFORM
           END-PERFORM
           MOVE WS-COMMENT-LINES2      TO WS-COMMENT-LINES1
           MOVE EN-AH-PREM-ENT-AMT     TO LN14B-AH-ENT
           MOVE EN-AH-PREM-CAL-AMT     TO LN14B-AH-CAL
           COMPUTE WS-WORK-AMT =
             (EN-LF-PREM-ENT-AMT + EN-AH-PREM-ENT-AMT) -
             (EN-LF-PREM-CAL-AMT + EN-AH-PREM-CAL-AMT)
           MOVE WS-WORK-AMT            TO LN15B-AMT
                                          LN15B1-AMT
           MOVE AM-NAME                TO LN18B-ACCT-NAME
           MOVE W-SAVE-CURRENT-DATE    TO LN18B-PRINT-DATE
           EVALUATE TRUE
122706*       WHEN EN-CARRIER = '8'
122706*          MOVE ' Form 20429E-PC KY'
122706*                                TO LN19B-FORM-NUMBER
              WHEN EN-STATE = 'TX'
                 MOVE ' Form 20421E-PC TX (3.53)'
                                       TO LN19B-FORM-NUMBER
042307        WHEN (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (EN-CARRIER = '8')
033006           MOVE SPACES           TO LN19B-FORM-NUMBER
              WHEN (EN-STATE = 'NJ' OR 'NC')
                 AND (EN-LF-BEN-CD = '55' OR '56')
                 MOVE '  Form 20430E-PC'
                                       TO LN19B-FORM-NUMBER
033006        WHEN OTHER
                 MOVE '  Form 20421E-PC'
                                       TO LN19B-FORM-NUMBER
           END-EVALUATE
042605*    MOVE '03/1995'              TO LN19B-FORM-DATE
           EVALUATE TRUE
122706*       WHEN EN-CARRIER = '8'
122706*          MOVE '12-06'          TO LN19B-FORM-DATE
042307        WHEN (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (EN-CARRIER = '8')
033006           MOVE SPACES           TO LN19B-FORM-DATE
              WHEN (EN-STATE = 'NJ' OR 'NC')
                 AND (EN-LF-BEN-CD = '55' OR '56')
                 MOVE '1-07'           TO LN19B-FORM-DATE
                 MOVE '(Lease)'        TO LN20B-LEASE-COMMENT
042605        WHEN OTHER
                 MOVE '04/2005'        TO LN19B-FORM-DATE
           END-EVALUATE
           MOVE 'Account copy'         TO LN18B-WHO-COPY
           IF EN-SIG-SW = 'Y'
              MOVE 4                   TO WS-ISSUE-TIMES
           ELSE
              MOVE 3                   TO WS-ISSUE-TIMES
           END-IF
052302***** GENERAL CHANGE ENDORSEMENTS
           PERFORM WS-ISSUE-TIMES TIMES
052302        MOVE SPACES              TO WS-PRINT-AREA
052302        MOVE '1'                 TO WS-PASSED-CNTL-CHAR
052302        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        IF PI-COMPANY-ID = 'AHL'
032112           MOVE SPACES TO WS-PRINT-AREA
032112        ELSE
122706           IF EN-CARRIER = '8'
122706              MOVE 'IHPSIG'         TO WS-WHO-SIG
122706           ELSE
122706              MOVE 'RTKSIG'         TO WS-WHO-SIG
122706           END-IF
052302           MOVE SIGNATURE-FLG-NOPRT-HDR TO WS-PRINT-AREA
032112        END-IF
052302        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
122706        IF EN-CARRIER = '8'
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
122706              MOVE HEADING-1-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-2-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-3-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        ELSE
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-4-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
                    MOVE HEADING-1        TO WS-PRINT-AREA
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                    MOVE HEADING-2        TO WS-PRINT-AREA
                    IF (EN-STATE = 'NJ' OR 'NC')
                       AND (EN-LF-BEN-CD = '55' OR '56')
                       MOVE HEADING-2-LEASE
                                       TO WS-PRINT-AREA
                    END-IF
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
052302              MOVE HEADING-3        TO WS-PRINT-AREA
052302              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        END-IF
042307        IF (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (EN-CARRIER = '8')
110410           IF FOUND-RES
110410              MOVE '   Notice of Rescission   '
110410                                 TO LINE-3B-NOTICE-VANM
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  Notice of Reformation   '
110410                                 TO LINE-3B-NOTICE-VANM
110410              ELSE
110410                 MOVE '  Notification of Change  '
110410                                 TO LINE-3B-NOTICE-VANM
110410              END-IF
110410           END-IF
033006           MOVE LINE-3B-VA-NM    TO WS-PRINT-AREA
033006        ELSE
110410           IF FOUND-RES
110410              MOVE '   Notice of Rescission   '
110410                                 TO LINE-3B-NOTICE
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  Notice of Reformation   '
110410                                 TO LINE-3B-NOTICE
110410              ELSE
110410                 MOVE 'General Change Endorsement'
110410                                 TO LINE-3B-NOTICE
110410              END-IF
110410           END-IF
                 MOVE LINE-3B          TO WS-PRINT-AREA
033006        END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
042307        IF (EN-STATE NOT = 'VA' AND 'NM' AND 'PA')
042307           AND (EN-CARRIER NOT = '8')
                 MOVE LINE-4B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE LINE-5B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE LINE-6B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE LINE-7B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE LINE-8B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE '0'              TO LINE-9B (1:1)
                 IF EN-LF-BEN-CD = '55' OR '56'
                    MOVE 'Lessee :'    TO LINE-9B (10:10)
                 END-IF
                 MOVE LINE-9B          TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
033006        ELSE
033006           MOVE '-'              TO LINE-9B (1:1)
033006           MOVE LINE-9B          TO WS-PRINT-AREA
033006           PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
033006        END-IF
              MOVE LINE-10B            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-11B            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-12B            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE +0                  TO WS-BLANK-LINES-NEEDED
              IF (EN-LF-BEN-ENT-AMT - EN-LF-BEN-CAL-AMT) = ZEROS
                 ADD +1                TO WS-BLANK-LINES-NEEDED
              ELSE
                 MOVE LINE-13B1        TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
              IF (EN-AH-BEN-ENT-AMT - EN-AH-BEN-CAL-AMT) = ZEROS
                 ADD +1                TO WS-BLANK-LINES-NEEDED
              ELSE
                 MOVE LINE-13B2        TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
              IF (EN-LF-PREM-ENT-AMT = +0) AND
                 (EN-LF-PREM-CAL-AMT = +0)
                 ADD +1                TO WS-BLANK-LINES-NEEDED
              ELSE
                 MOVE LINE-13B         TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
              IF (EN-AH-PREM-ENT-AMT = +0) AND
                 (EN-AH-PREM-CAL-AMT = +0)
                 ADD +1                TO WS-BLANK-LINES-NEEDED
              ELSE
                 MOVE LINE-14B         TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
              PERFORM WS-BLANK-LINES-NEEDED TIMES
                 MOVE SPACES           TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-PERFORM
052302        MOVE ' '                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
052302        MOVE +9                  TO WS-LINES-LEFT
              PERFORM VARYING W-SUB2 FROM +1 BY +1 UNTIL
052302           (W-SUB2 > +8) OR
                 (WS-TEXT (W-SUB2) = SPACES)
                 MOVE WS-TEXT (W-SUB2) TO LN14B1-COMMENT1
                 MOVE LINE-14B1        TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 SUBTRACT +1           FROM WS-LINES-LEFT
              END-PERFORM
      *       MOVE LINE-14B1           TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-14B2           TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              PERFORM WS-LINES-LEFT TIMES
                 MOVE SPACES           TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-PERFORM
033006***  I ADDED THE NEXT LINES OF CODE TO MAKE UP FOR LINE 5 THRU 8
042307        IF (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (EN-CARRIER = '8')
033006           MOVE '-'              TO WS-PRINT-AREA
033006           PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
033006           MOVE '-'              TO WS-PRINT-AREA
033006           PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
033006        END-IF
              IF WS-WORK-AMT < +0
                 MOVE LINE-15B         TO WS-PRINT-AREA
              ELSE
                 MOVE LINE-15B1        TO WS-PRINT-AREA
              END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
052302*       MOVE ' '                 to WS-PRINT-AREA
052302*       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              IF EN-SIG-SW = 'Y'
                 MOVE LINE-16B1        TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE LINE-16B2        TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              ELSE
                 MOVE '0'              TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE '0'              TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
032112        IF PI-COMPANY-ID EQUAL 'AHL'
032112            MOVE SPACES          TO WS-PRINT-AREA
032112        ELSE
122706           IF EN-CARRIER = '8'
122706              MOVE LINE-16B-CARR8   TO WS-PRINT-AREA
122706           ELSE
                    MOVE LINE-16B         TO WS-PRINT-AREA
122706           END-IF
032112        END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
052302        MOVE '0'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        IF PI-COMPANY-ID EQUAL 'AHL'
032112            MOVE SPACES          TO WS-PRINT-AREA
032112        ELSE
122706           IF EN-CARRIER = '8'
122706              MOVE LINE-17B-CARR8   TO WS-PRINT-AREA
122706           ELSE
                    MOVE LINE-17B         TO WS-PRINT-AREA
122706           END-IF
032112        END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              IF EN-SIG-SW = 'Y'
                 MOVE W-ASTERISK-LINEB TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
042307           IF (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307              OR (EN-CARRIER = '8')
033006              MOVE LINE-17B1-VA-NM
033006                                 TO WS-PRINT-AREA
033006              PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT
033006           ELSE
                    MOVE LINE-17B1     TO WS-PRINT-AREA
                    PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT
033006           END-IF
                 MOVE W-ASTERISK-LINE  TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              ELSE
052302           MOVE '0'              TO WS-PASSED-CNTL-CHAR
052302           MOVE SPACES           TO WS-PASSED-DATA
052302*          MOVE ' '              TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
052302           MOVE '0'              TO WS-PASSED-CNTL-CHAR
052302           MOVE SPACES           TO WS-PASSED-DATA
052302*          MOVE ' '              TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
052302*          MOVE ' '              TO WS-PRINT-AREA
052302*          PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-IF
              MOVE '0'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-18B            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        IF PI-COMPANY-ID NOT EQUAL 'AHL'
                 MOVE LINE-19B            TO WS-PRINT-AREA
                 PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
                 MOVE LINE-20B            TO WS-PRINT-AREA
                 PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        END-IF
              IF LN18B-WHO-COPY (1:1) = 'A'
                 MOVE 'Insured copy'   TO LN18B-WHO-COPY
              ELSE
                 IF LN18B-WHO-COPY (1:1) = 'I'
                    MOVE 'Home office copy'
                                       TO LN18B-WHO-COPY
                 ELSE
                    MOVE 'Return copy' TO LN18B-WHO-COPY
                 END-IF
              END-IF
           END-PERFORM
           ADD +1 TO SUB1
           MOVE EN-CERT-PRIME          TO WS-DET-CERT-NO (SUB1) (1:10)
           MOVE EN-CERT-SFX            TO WS-DET-CERT-NO (SUB1) (11:1)
           MOVE SPACES                 TO WS-DET-NAME (SUB1)
081009*     STRING EN-FIRST-NAME ' ' EN-MIDDLE-INIT ' '
081009     STRING EN-FIRST-NAME DELIMITED BY '  '  ' '
                  EN-LAST-NAME DELIMITED BY '   '
                  INTO WS-DET-NAME (SUB1)
           END-STRING
           MOVE ' GEN'                 TO WS-DET-TYPE (SUB1)
           MOVE EN-LF-PREM-ENT-AMT     TO WS-DET-LF-ENT (SUB1)
           COMPUTE WS-TOT-LF-ENT = WS-TOT-LF-ENT +
                EN-LF-PREM-ENT-AMT
           MOVE EN-AH-PREM-ENT-AMT     TO WS-DET-AH-ENT (SUB1)
           COMPUTE WS-TOT-AH-ENT = WS-TOT-AH-ENT +
                EN-AH-PREM-ENT-AMT
           MOVE EN-LF-PREM-CAL-AMT     TO WS-DET-LF-CAL (SUB1)
           COMPUTE WS-TOT-LF-CAL = WS-TOT-LF-CAL +
                EN-LF-PREM-CAL-AMT
           MOVE EN-AH-PREM-CAL-AMT     TO WS-DET-AH-CAL (SUB1)
           COMPUTE WS-TOT-AH-CAL = WS-TOT-AH-CAL +
                EN-AH-PREM-CAL-AMT
           IF EN-LF-COMMISSION NOT NUMERIC
              MOVE +0                  TO EN-LF-COMMISSION
           END-IF
           IF EN-AH-COMMISSION NOT NUMERIC
              MOVE +0                  TO EN-AH-COMMISSION
           END-IF
           COMPUTE WS-DET-LF-COMM (SUB1) = (EN-LF-PREM-CAL-AMT -
                EN-LF-PREM-ENT-AMT) * EN-LF-COMMISSION
           COMPUTE WS-TOT-LF-COMM = WS-TOT-LF-COMM +
                WS-DET-LF-COMM (SUB1)
           COMPUTE WS-DET-AH-COMM (SUB1) = (EN-AH-PREM-CAL-AMT -
                EN-AH-PREM-ENT-AMT) * EN-AH-COMMISSION
           COMPUTE WS-TOT-AH-COMM = WS-TOT-AH-COMM +
                WS-DET-AH-COMM (SUB1)
           ADD +1                      TO WS-TOT-ISSUES
           .
       2050-EXIT.
           EXIT.
       2060-PRINT-CANCEL.
           IF WS-ERENDR-HOLD-KEY NOT = EN-CONTROL-PRIMARY (1:20)
              PERFORM 2070-PRINT-SUMMARY THRU 2070-EXIT
              PERFORM 1010-INIT-TABLE THRU 1010-EXIT
              MOVE EN-CONTROL-PRIMARY (1:20) TO WS-ERENDR-HOLD-KEY
           END-IF
           PERFORM 3000-MATCH-TO-ACCT  THRU 3000-EXIT
           PERFORM 3050-MATCH-TO-CSR   THRU 3050-EXIT
           MOVE SPACES                 TO LN3A-NAME
081009     IF EN-MIDDLE-INIT > SPACES
081009         STRING EN-FIRST-NAME DELIMITED BY '  '
081009             ' ' EN-MIDDLE-INIT ' ' EN-LAST-NAME
081009             DELIMITED BY '   ' INTO LN3A-NAME
081009         END-STRING
081009     ELSE
081009         STRING EN-FIRST-NAME DELIMITED BY '  '
081009         ' ' EN-LAST-NAME
081009         DELIMITED BY '   ' INTO LN3A-NAME
081009         END-STRING
081009     END-IF
           MOVE EN-CERT-PRIME          TO LN3A-CERT-NO (1:10)
           MOVE EN-CERT-SFX            TO LN3A-CERT-NO (11:1)
           IF EN-LF-TERM NOT = +0
              MOVE EN-LF-TERM          TO WS-WORK-TERM
           ELSE
              MOVE EN-AH-TERM          TO WS-WORK-TERM
           END-IF
           MOVE SPACES                 TO LN5A-REST
           STRING WS-TERM-ALPH ' MONTHS. ' DELIMITED BY ' '
                 INTO LN5A-REST
           END-STRING
           IF EN-LF-CANC-DT NOT = LOW-VALUES
              MOVE EN-LF-CANC-DT       TO DC-BIN-DATE-1
           ELSE
              MOVE EN-AH-CANC-DT       TO DC-BIN-DATE-1
           END-IF
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE SPACES                 TO LN9A-CANC-DATE
           IF NO-CONVERSION-ERROR
              STRING DC-EDIT1-YEAR ' ' DC-EDIT1-MONTH ' '
                     DC-EDIT1-DAY DELIMITED BY SIZE INTO
                            LN9A-CANC-DATE
              END-STRING
              MOVE LN9A-CANC-DATE      TO LN18A-CANC-DATE
      *       MOVE DC-GREG-DATE-1-EDIT TO LN9A-CANC-DATE
      *                                   LN18A-CANC-DATE
           END-IF
           MOVE EN-LF-PREM-ENT-AMT     TO LN9A-LF-PREM
                                          LN18A-LF-PREM
           MOVE EN-LF-CANC-ENT-AMT     TO LN9A-LF-REF
      *    MOVE ZEROS                  TO LN9A-LF-PCT
      *    IF EN-LF-PREM-ENT-AMT NOT = +0
      *       COMPUTE LN9A-LF-PCT = (EN-LF-CANC-ENT-AMT /
      *             EN-LF-PREM-ENT-AMT) * +100
      *    END-IF
           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE SPACES                 TO LN10A-EFF-DATE
           IF NO-CONVERSION-ERROR
              STRING DC-EDIT1-YEAR ' ' DC-EDIT1-MONTH ' '
                     DC-EDIT1-DAY DELIMITED BY SIZE INTO
                            LN10A-EFF-DATE
              END-STRING
              MOVE LN10A-EFF-DATE      TO LN19A-EFF-DATE
      *       MOVE DC-GREG-DATE-1-EDIT TO LN10A-EFF-DATE
      *                                   LN19A-EFF-DATE
           END-IF
           MOVE EN-AH-PREM-ENT-AMT     TO LN10A-AH-PREM
                                          LN19A-AH-PREM
           MOVE EN-AH-CANC-ENT-AMT     TO LN10A-AH-REF
      *    MOVE ZEROS                  TO LN10A-AH-PCT
      *    IF EN-AH-PREM-ENT-AMT NOT = +0
      *       COMPUTE LN10A-AH-PCT = (EN-AH-CANC-ENT-AMT /
      *             EN-AH-PREM-ENT-AMT) * +100
      *    END-IF
           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           IF EN-LF-CANC-DT NOT = LOW-VALUES
              MOVE EN-LF-CANC-DT       TO DC-BIN-DATE-2
           ELSE
              MOVE EN-AH-CANC-DT       TO DC-BIN-DATE-2
           END-IF
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE +0 TO WS-YEARS
112906        MOVE DC-ELAPSED-DAYS     TO WS-NCB-DAYS
              IF DC-ELAPSED-MONTHS > +11
                 COMPUTE WS-YEARS = DC-ELAPSED-MONTHS / +12
                 COMPUTE DC-ELAPSED-MONTHS =
                    DC-ELAPSED-MONTHS - (WS-YEARS * +12)
              END-IF
              MOVE WS-YEARS            TO LN11A-IN-FORCE-YR
              MOVE DC-ELAPSED-MONTHS   TO LN11A-IN-FORCE-MO
              MOVE DC-ODD-DAYS-OVER    TO LN11A-IN-FORCE-DA
      *       MOVE DC-ELAPSED-MONTHS   TO WS-MONTHS
      *       MOVE DC-ODD-DAYS-OVER    TO WS-DAYS
      *       STRING WS-YEARS-ALPH ' YEARS ' WS-MONTHS-ALPH
      *         WS-DAYS-ALPH ' DAYS ' DELIMITED BY SIZE
      *         INTO LN11A-IN-FORCE
      *       END-STRING
              MOVE LN11A-IN-FORCE      TO LN20A-IN-FORCE
           END-IF
           COMPUTE WS-WORK-AMT = EN-LF-PREM-ENT-AMT +
                  EN-AH-PREM-ENT-AMT
           MOVE WS-WORK-AMT            TO LN11A-TOT-PREM
                                          LN20A-TOT-PREM
           COMPUTE WS-WORK-AMT = EN-LF-CANC-ENT-AMT +
                  EN-AH-CANC-ENT-AMT
           MOVE WS-WORK-AMT            TO LN11A-TOT-REF
      *    MOVE ZEROS                  TO LN11A-TOT-PCT
      *    IF (EN-LF-PREM-ENT-AMT + EN-AH-PREM-ENT-AMT) NOT = +0
      *       COMPUTE LN11A-TOT-PCT = (EN-LF-CANC-ENT-AMT +
      *           EN-AH-CANC-ENT-AMT) /
      *           (EN-LF-PREM-ENT-AMT + EN-AH-PREM-ENT-AMT) * +100
      *    END-IF
           MOVE EN-LF-CANC-CAL-AMT     TO LN18A-LF-REF
      *    MOVE ZEROS                  TO LN18A-LF-PCT
      *    IF EN-LF-PREM-ENT-AMT NOT = +0
      *       COMPUTE LN18A-LF-PCT = (EN-LF-CANC-CAL-AMT /
      *          EN-LF-PREM-ENT-AMT) * +100
      *    END-IF
           MOVE EN-AH-CANC-CAL-AMT     TO LN19A-AH-REF
      *    MOVE ZEROS                  TO LN19A-AH-PCT
      *    IF EN-AH-PREM-ENT-AMT NOT = +0
      *       COMPUTE LN19A-AH-PCT = (EN-AH-CANC-CAL-AMT /
      *          EN-AH-PREM-ENT-AMT) * +100
      *    END-IF
           COMPUTE LN20A-TOT-REF = EN-LF-CANC-CAL-AMT +
                EN-AH-CANC-CAL-AMT
      *    MOVE ZEROS                  TO LN20A-TOT-PCT
      *    IF (EN-LF-PREM-ENT-AMT + EN-AH-PREM-ENT-AMT) NOT = +0
      *       COMPUTE LN20A-TOT-PCT = (EN-LF-CANC-CAL-AMT +
      *          EN-AH-CANC-CAL-AMT) /
      *          (EN-LF-PREM-ENT-AMT + EN-AH-PREM-ENT-AMT) * +100
      *    END-IF
           COMPUTE LN24A-AMOUNT = (EN-LF-CANC-ENT-AMT +
              EN-AH-CANC-ENT-AMT) -
              (EN-LF-CANC-CAL-AMT + EN-AH-CANC-CAL-AMT)
           IF (EN-LF-CANC-ENT-AMT + EN-AH-CANC-ENT-AMT) NOT <
              (EN-LF-CANC-CAL-AMT + EN-AH-CANC-CAL-AMT)
              MOVE 'Overpayment to Insured '
                                       TO LN24A-DESC
           ELSE
              MOVE 'Additional Refund Due Insured '
                                       TO LN24A-DESC
           END-IF
           IF EN-LF-TERM NOT = ZEROS
              MOVE EN-LF-TERM          TO W-EDIT-3-0
              MOVE W-EDIT-3-0          TO W-VG-TEXT (1)
           ELSE
              MOVE EN-AH-TERM          TO W-EDIT-3-0
              MOVE W-EDIT-3-0          TO W-VG-TEXT (1)
           END-IF
           MOVE LN10A-EFF-DATE         TO W-VG-TEXT (2)
           MOVE EN-LF-BEN-ENT-AMT      TO W-EDIT-9-2
           MOVE W-EDIT-9-2             TO W-VG-TEXT (3)
           MOVE LN3A-NAME              TO W-VG-TEXT (4)
           MOVE SPACES                 TO W-VG-TEXT (5)
           STRING EN-JOINT-FIRST-NAME ' '
                  EN-JOINT-MIDDLE-INIT ' '
                  EN-JOINT-LAST-NAME
                  DELIMITED BY '   '  INTO W-VG-TEXT (5)
           END-STRING
           MOVE +1                     TO W-SUB2
           MOVE SPACES                 TO WS-COMMENT-LINES1
                                          WS-COMMENT-LINES2
110410                                    WS-RES-REF-SW
           IF (EN-COMMENTS1 (1:4) NOT = SPACES) AND
              (EN-COMMENTS1 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS1 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS1    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS1        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS2 (1:4) NOT = SPACES) AND
              (EN-COMMENTS2 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS2 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS2    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS2        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS3 (1:4) NOT = SPACES) AND
              (EN-COMMENTS3 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS3 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS3    TO WS-TEXT (W-SUB2)
081209            ADD +1               TO W-SUB2
081209        END-IF
           ELSE
              MOVE EN-COMMENTS3        TO WS-TEXT (W-SUB2)
              ADD +1                   TO W-SUB2
           END-IF
           IF (EN-COMMENTS4 (1:4) NOT = SPACES) AND
              (EN-COMMENTS4 (5:20) = SPACES)
              MOVE PI-COMPANY-CD       TO W-LETR-COMPANY-CD
              MOVE EN-COMMENTS4 (1:4)  TO W-LETR-ACCESS-CD
              MOVE +0                  TO W-LETR-SEQ-NO
              MOVE W-LETR-KEY          TO W-LETR-HOLD-KEY
081209        MOVE 'N'                 TO WS-TEXT-FOUND
110410        IF W-LETR-ACCESS-CD = 'RS01' OR 'RS02' OR 'RS03'
110410           OR 'RS04'
110410           SET FOUND-RES         TO TRUE
110410        ELSE
110410           IF W-LETR-ACCESS-CD = 'RS05' OR 'RS06' OR 'RS07'
110410              OR 'RS08'
110410              SET FOUND-REF      TO TRUE
110410           END-IF
110410        END-IF
              PERFORM 5000-GET-TEXT    THRU 5000-EXIT
081209        IF TEXT-NOT-FOUND
081209            MOVE EN-COMMENTS4    TO WS-TEXT (W-SUB2)
081209        END-IF
           ELSE
              MOVE EN-COMMENTS4        TO WS-TEXT (W-SUB2)
           END-IF
           PERFORM 5040-RES-VAR        THRU 5040-EXIT
           MOVE WS-COMMENT-LINES2      TO WS-COMMENT-LINES1
           MOVE SPACES                 TO WS-COMMENT-LINES2
                                          WS-LAST-CHAR
           PERFORM VARYING W-SUB1 FROM +1 BY +1 UNTIL
              (W-SUB1 > +10)
              MOVE +1                  TO OUT-SUB
              PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
053107           (IN-SUB > +75)
                 IF (WS-CHAR1 (W-SUB1 IN-SUB) = ' ') AND
                    (WS-LAST-CHAR = ' ')
                    CONTINUE
                 ELSE
                    MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO WS-CHAR2 (W-SUB1 OUT-SUB)
                                          WS-LAST-CHAR
                    ADD +1             TO OUT-SUB
                 END-IF
              END-PERFORM
           END-PERFORM
           MOVE WS-COMMENT-LINES2      TO WS-COMMENT-LINES1
           MOVE AM-NAME                TO LN27A-ACCT-NAME
           MOVE W-SAVE-CURRENT-DATE    TO LN27A-PRINT-DATE
           MOVE 'Account copy'         TO LN27A-WHO-COPY
           PERFORM 3 TIMES
052302        MOVE SPACES              TO WS-PRINT-AREA
052302        MOVE '1'                 TO WS-PASSED-CNTL-CHAR
052302        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
042307        IF (EN-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (EN-CARRIER = '8')
110410           IF FOUND-RES
110410              MOVE '   NOTICE OF RESCISSION   '
110410                                 TO RTK-NOTICE-VANM
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  NOTICE OF REFORMATION   '
110410                                 TO RTK-NOTICE-VANM
110410              ELSE
110410                 MOVE '  NOTIFICATION OF CHANGE  '
110410                                 TO RTK-NOTICE-VANM
110410              END-IF
110410           END-IF
033006           MOVE RTK-FORMDEF-FLG-NOPRT-HDR-VANM
033006                                 TO WS-PRINT-AREA
033006        ELSE
110410           IF FOUND-RES
110410              MOVE '   NOTICE OF RESCISSION   '
110410                                 TO RTK-NOTICE
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  NOTICE OF REFORMATION   '
110410                                 TO RTK-NOTICE
110410              ELSE
110410                 MOVE 'GENERAL CHANGE ENDORSEMENT'
110410                                 TO RTK-NOTICE
110410              END-IF
110410           END-IF
052302           MOVE RTK-FORMDEF-FLG-NOPRT-HDR
                                       TO WS-PRINT-AREA
033006        END-IF
033006        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
122706        IF EN-CARRIER = '8'
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
122706              MOVE HEADING-1-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-2-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-3-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        ELSE
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-4-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
                    MOVE HEADING-1        TO WS-PRINT-AREA
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                    MOVE HEADING-2        TO WS-PRINT-AREA
                    IF (EN-STATE = 'NJ' OR 'NC')
                       AND (EN-LF-BEN-CD = '55' OR '56')
                       MOVE HEADING-2-LEASE
                                       TO WS-PRINT-AREA
                    END-IF
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
052302              MOVE HEADING-3        TO WS-PRINT-AREA
052302              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        END-IF
052302*       MOVE '-'                 TO WS-PRINT-AREA
052302*       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '0'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-3A             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-4A             TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-5A             TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
052302        MOVE ' '                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE ' '                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-6A             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-7A             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-8A             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-9A             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-10A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-11A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-12A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '0'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-13A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-14A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-15A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-16A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-17A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-18A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-19A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-20A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-21A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '0'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-22A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
      *       MOVE LINE-23A            TO WS-PRINT-AREA
      *       PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-24A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-25A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE +20                 TO WS-LINES-LEFT
              PERFORM VARYING W-SUB2 FROM +1 BY +1 UNTIL
                 (W-SUB2 > +10) OR
                 (WS-TEXT (W-SUB2) = SPACES)
                 MOVE WS-TEXT (W-SUB2) TO LN26A-COMMENT1
                 MOVE LINE-26A         TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 SUBTRACT +1           FROM WS-LINES-LEFT
              END-PERFORM
              PERFORM WS-LINES-LEFT TIMES
                 MOVE SPACES           TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
              END-PERFORM
              MOVE LINE-27A            TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              IF LN27A-WHO-COPY (1:1) = 'A'
                 MOVE 'Insured copy '  TO LN27A-WHO-COPY
              ELSE
                 MOVE 'Home office copy '
                                       TO LN27A-WHO-COPY
              END-IF
           END-PERFORM
           ADD +1 TO SUB1
           MOVE EN-CERT-PRIME          TO WS-DET-CERT-NO (SUB1) (1:10)
           MOVE EN-CERT-SFX            TO WS-DET-CERT-NO (SUB1) (11:1)
           MOVE SPACES                 TO WS-DET-NAME (SUB1)
081009*     STRING EN-FIRST-NAME ' ' EN-MIDDLE-INIT ' '
081009     STRING EN-FIRST-NAME DELIMITED BY '  ' ' '
                  EN-LAST-NAME DELIMITED BY '   '
                  INTO WS-DET-NAME (SUB1)
           END-STRING
           MOVE 'CANCEL'               TO WS-DET-TYPE (SUB1)
           COMPUTE WS-DET-LF-ENT (SUB1) =
               EN-LF-CANC-ENT-AMT * -1
           COMPUTE WS-DET-AH-ENT (SUB1) =
               EN-AH-CANC-ENT-AMT * -1
           COMPUTE WS-DET-LF-CAL (SUB1) =
               EN-LF-CANC-CAL-AMT * -1
           COMPUTE WS-DET-AH-CAL (SUB1) =
               EN-AH-CANC-CAL-AMT * -1
      *    MOVE EN-LF-CANC-ENT-AMT     TO WS-DET-LF-ENT (SUB1)
      *    MOVE EN-AH-CANC-ENT-AMT     TO WS-DET-AH-ENT (SUB1)
      *    MOVE EN-LF-CANC-CAL-AMT     TO WS-DET-LF-CAL (SUB1)
      *    MOVE EN-AH-CANC-CAL-AMT     TO WS-DET-AH-CAL (SUB1)
           COMPUTE WS-TOT-LF-ENT = WS-TOT-LF-ENT +
              (EN-LF-CANC-ENT-AMT * -1)
           COMPUTE WS-TOT-AH-ENT = WS-TOT-AH-ENT +
              (EN-AH-CANC-ENT-AMT * -1)
           COMPUTE WS-TOT-LF-CAL = WS-TOT-LF-CAL +
              (EN-LF-CANC-CAL-AMT * -1)
           COMPUTE WS-TOT-AH-CAL = WS-TOT-AH-CAL +
              (EN-AH-CANC-CAL-AMT * -1)
           IF EN-LF-COMMISSION NOT NUMERIC
              MOVE +0                  TO EN-LF-COMMISSION
           END-IF
           IF EN-AH-COMMISSION NOT NUMERIC
              MOVE +0                  TO EN-AH-COMMISSION
           END-IF
052110     IF WS-CHARGEBACK-L1 NOT NUMERIC
052110        MOVE ZEROS               TO WS-CHARGEBACK-L1
052110     END-IF
052110
052110     IF (WS-CHARGEBACK-L1 = 99)
052110                   OR
052110        ((WS-CHARGEBACK-L1 NOT = ZEROS)
052110        AND (WS-NCB-DAYS > (WS-CHARGEBACK-L1 * 30)))
052110        MOVE +0                  TO EN-LF-COMMISSION
052110                                    EN-AH-COMMISSION
052110     END-IF
112906*    IF AM-COMM-CHARGEBACK (1) NOT NUMERIC
112906*       MOVE ZEROS               TO AM-COMM-CHARGEBACK (1)
112906*    END-IF
      *
112906*    IF (AM-COMM-CHARGEBACK (1) = 99)
112906*                  OR
112906*       ((AM-COMM-CHARGEBACK (1) NOT = ZEROS)
112906*       AND (WS-NCB-DAYS > (AM-COMM-CHARGEBACK (1) * 30)))
112906*       MOVE +0                  TO EN-LF-COMMISSION
112906*                                   EN-AH-COMMISSION
112906*    END-IF
           COMPUTE WS-DET-LF-COMM (SUB1) = ((EN-LF-CANC-CAL-AMT * -1) -
                (EN-LF-CANC-ENT-AMT * -1)) * EN-LF-COMMISSION
           COMPUTE WS-DET-AH-COMM (SUB1) = ((EN-AH-CANC-CAL-AMT * -1) -
                (EN-AH-CANC-ENT-AMT * -1)) * EN-AH-COMMISSION
           COMPUTE WS-TOT-LF-COMM = WS-TOT-LF-COMM +
             WS-DET-LF-COMM (SUB1)
           COMPUTE WS-TOT-AH-COMM = WS-TOT-AH-COMM +
             WS-DET-AH-COMM (SUB1)
           ADD +1                      TO WS-TOT-CANCELS
           .
       2060-EXIT.
           EXIT.
       2070-PRINT-SUMMARY.
           MOVE W-SAVE-CURRENT-DATE    TO LN3-DATE
           IF AM-PERSON (1:5) = 'ATTN:' OR 'ATTN '
              MOVE AM-PERSON (6:25)    TO AM-PERSON
           END-IF
PEMMOD     IF AM-CONTROL-NAME (1:1) = SPACES OR LOW-VALUES
PEMMOD        MOVE SPACES              TO LN4-ATTN
PEMMOD                                    LN4-CONTACT
PEMMOD     ELSE
PEMMOD        MOVE AM-CONTROL-NAME     TO LN4-CONTACT
PEMMOD        MOVE '   ATTN:'          TO LN4-ATTN
PEMMOD     END-IF
PEMMOD
           MOVE AM-ACCOUNT             TO LN4-ACCOUNT
           MOVE AM-NAME                TO LN5-NAME
021907     MOVE AM-PERSON              TO LN51A-CONTRACTED-NAME
           MOVE AM-ADDRS               TO LN6-ADDRESS
           MOVE SPACES                 TO LN7-CITY-STATE-ZIP
                                          WS-WORK-ZIP
           MOVE AM-ZIP-PRIME           TO WS-ZIP-1-5
           IF AM-ZIP-PLUS4 = SPACES OR ZEROS
              CONTINUE
           ELSE
              MOVE AM-ZIP-PLUS4        TO WS-ZIP-7-10
              MOVE '-'                 TO WS-ZIP-DASH
           END-IF
           MOVE SPACES                 TO LN7-CITY-STATE-ZIP
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE ' ' WS-WORK-ZIP
              DELIMITED BY '     ' INTO LN7-CITY-STATE-ZIP
           END-STRING
           MOVE ALL '-'                TO LN12-VALUE
           MOVE WS-TOT-LF-CAL          TO LN14-CSO-LIFE
           MOVE WS-TOT-AH-CAL          TO LN14-CSO-AH
           COMPUTE WS-TOT-CAL = WS-TOT-LF-CAL + WS-TOT-AH-CAL
           MOVE WS-TOT-CAL             TO LN14-CSO-TOTAL
           MOVE WS-TOT-LF-ENT          TO LN15-AGT-LIFE
           MOVE WS-TOT-AH-ENT          TO LN15-AGT-AH
           COMPUTE WS-TOT-ENT = WS-TOT-LF-ENT + WS-TOT-AH-ENT
           MOVE WS-TOT-ENT             TO LN15-AGT-TOTAL
           COMPUTE LN16-NET-LIFE = WS-TOT-LF-CAL - WS-TOT-LF-ENT
           COMPUTE LN16-NET-AH   = WS-TOT-AH-CAL - WS-TOT-AH-ENT
           COMPUTE WS-TOT-NET = (WS-TOT-LF-CAL + WS-TOT-AH-CAL) -
              (WS-TOT-LF-ENT + WS-TOT-AH-ENT)
           MOVE WS-TOT-NET             TO LN16-NET-TOTAL
           MOVE WS-TOT-LF-COMM         TO LN17-COM-LIFE
           MOVE WS-TOT-AH-COMM         TO LN17-COM-AH
           COMPUTE WS-TOT-COMM = WS-TOT-LF-COMM + WS-TOT-AH-COMM
           MOVE WS-TOT-COMM            TO LN17-COM-TOTAL
           COMPUTE LN18-DUE-LIFE = (WS-TOT-LF-CAL - WS-TOT-LF-ENT)
              - WS-TOT-LF-COMM
           COMPUTE LN18-DUE-AH = (WS-TOT-AH-CAL - WS-TOT-AH-ENT)
              - WS-TOT-AH-COMM
           COMPUTE WS-TOT-NET-DUE = WS-TOT-NET - WS-TOT-COMM
           MOVE WS-TOT-NET-DUE         TO LN18-DUE-TOTAL
           IF WS-TOT-NET-DUE < +0
              MOVE ' AGENT '           TO LN20-DUE-WHO
           ELSE
              MOVE '  CSO  '           TO LN20-DUE-WHO
           END-IF
           MOVE WS-TOT-NET-DUE         TO LN20-DUE-AMT
           MOVE WS-TOT-ISSUES          TO LN22-CHG-CNT
           MOVE WS-TOT-CANCELS         TO LN23-CAN-CNT
           MOVE SPACES                 TO LN25-REST
           STRING CF-PROCESSOR-NAME ' AT (800) 826-6587.'
               DELIMITED BY '   ' INTO LN25-REST
           END-STRING
           MOVE AM-NAME                TO LN26-ACCT-NAME
           MOVE 'Account copy'         TO LN26-WHO-COPY
           MOVE W-SAVE-CURRENT-DATE    TO LN26-PRINT-DATE
033006*    IF EN-STATE = 'TX'
           IF AM-STATE = 'TX'
              MOVE ' Form 20421E-PC TX (3.53)'
                                       to LN27-FORM-NUMBER
033006     ELSE
042307        IF (AM-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (AM-CARRIER = '8')
033006           MOVE SPACES           TO LN27-FORM-NUMBER
033006        ELSE
                 MOVE '  Form 20421E-PC'
                                       TO LN27-FORM-NUMBER
033006        END-IF
           END-IF
042605*    MOVE '03/1995'              TO LN27-FORM-DATE
042307     IF (AM-STATE = 'VA' OR 'NM' OR 'PA')
042307        OR (AM-CARRIER = '8')
033006        MOVE SPACES              TO LN27-FORM-DATE
033006     ELSE
042605        MOVE '04/2005'           TO LN27-FORM-DATE
033006     END-IF
           IF WS-DET-CERT-NO (1) NOT = SPACES
053107*      PERFORM 3 TIMES
053107       PERFORM 2 TIMES
052302        MOVE SPACES              TO WS-PRINT-AREA
052302        MOVE '1'                 TO WS-PASSED-CNTL-CHAR
052302        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
042307        IF (AM-STATE = 'VA' OR 'NM' OR 'PA')
042307           OR (AM-CARRIER = '8')
110410           IF FOUND-RES
110410              MOVE '   NOTICE OF RESCISSION   '
110410                                 TO RTK-NOTICE-VANM
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  NOTICE OF REFORMATION   '
110410                                 TO RTK-NOTICE-VANM
110410              ELSE
110410                 MOVE '  NOTIFICATION OF CHANGE  '
110410                                 TO RTK-NOTICE-VANM
110410              END-IF
110410           END-IF
033006           MOVE RTK-FORMDEF-FLG-NOPRT-HDR-VANM
033006                                 TO WS-PRINT-AREA
033006        ELSE
110410           IF FOUND-RES
110410              MOVE '   NOTICE OF RESCISSION   '
110410                                 TO RTK-NOTICE
110410           ELSE
110410              IF FOUND-REF
110410                 MOVE '  NOTICE OF REFORMATION   '
110410                                 TO RTK-NOTICE
110410              ELSE
110410                 MOVE 'GENERAL CHANGE ENDORSEMENT'
110410                                 TO RTK-NOTICE
110410              END-IF
110410           END-IF
052302           MOVE RTK-FORMDEF-FLG-NOPRT-HDR
                                       TO WS-PRINT-AREA
033006        END-IF
033006        PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
122706        IF AM-CARRIER = '8'
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-CARR8-AHL  TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
122706              MOVE HEADING-1-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-2-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
122706              MOVE HEADING-3-CARR8  TO WS-PRINT-AREA
122706              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        ELSE
032112           IF PI-COMPANY-ID = 'AHL'
032112              MOVE HEADING-1-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-2-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-3-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112              MOVE HEADING-4-AHL    TO WS-PRINT-AREA
032112              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           ELSE
                    MOVE HEADING-1        TO WS-PRINT-AREA
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                    MOVE HEADING-2        TO WS-PRINT-AREA
                    PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
052302              MOVE HEADING-3        TO WS-PRINT-AREA
052302              PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
032112           END-IF
122706        END-IF
              MOVE LINE-3              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE ' '                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-4              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-5              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
021907        IF LINE-51A NOT = SPACES
                 MOVE LINE-51A         TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 MOVE +20              TO WS-LINE-CNT
              ELSE
                 MOVE +19              TO WS-LINE-CNT
              END-IF
              MOVE LINE-6              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-7              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-8              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-9              TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-10             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-11             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-12             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
021907*       MOVE +19                 TO WS-LINE-CNT
              PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
                             (SUB1 > +200) OR
                             (WS-DET-CERT-NO (SUB1) = SPACES)
                 MOVE WS-DET-CERT-NO (SUB1)
                                       TO DT1-CERT-NO
                 MOVE WS-DET-NAME (SUB1)  TO DT1-NAME
                 MOVE WS-DET-TYPE (SUB1)  TO DT1-DESC
                 MOVE WS-DET-LF-ENT (SUB1)
                                       TO DT1-ENT-LF
                 MOVE WS-DET-AH-ENT (SUB1)
                                       TO DT1-ENT-AH
                 MOVE WS-DET-LF-CAL (SUB1)
                                       TO DT1-CAL-LF
                 MOVE WS-DET-AH-CAL (SUB1)
                                       TO DT1-CAL-AH
                 MOVE DETAIL-1         TO WS-PRINT-AREA
                 PERFORM ELPRTCVP      THRU ELPRTCVP-EXIT
                 ADD +1                TO WS-LINE-CNT
              END-PERFORM
              IF WS-LINE-CNT < +39
                 PERFORM UNTIL WS-LINE-CNT NOT < +39
                    MOVE SPACES        TO WS-PRINT-AREA
                    PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT
                    ADD +1             TO WS-LINE-CNT
                 END-PERFORM
              END-IF
              MOVE LINE-13             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-14             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-15             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-16             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-17             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-18             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
053107        IF WS-BILL-SW = 'B' OR 'C' OR 'E'
                 MOVE LINE-19          TO WS-PRINT-AREA
              ELSE
                 MOVE LINE-20          TO WS-PRINT-AREA
              END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-21             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              IF WS-TOT-ISSUES > ZEROS
                 MOVE LINE-22          TO WS-PRINT-AREA
              ELSE
                 MOVE SPACES           TO WS-PRINT-AREA
              END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              IF WS-TOT-CANCELS > ZEROS
                 MOVE LINE-23          TO WS-PRINT-AREA
              ELSE
                 MOVE SPACES           TO WS-PRINT-AREA
              END-IF
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE '-'                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-24             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-25             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE ' '                 TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
              MOVE LINE-26             TO WS-PRINT-AREA
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        IF PI-COMPANY-ID NOT EQUAL 'AHL'
                 MOVE LINE-27             TO WS-PRINT-AREA
                 PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
032112        END-IF
053107        MOVE 'Home Office copy'  TO LN26-WHO-COPY
053107*       IF LN26-WHO-COPY (1:1) = 'A'
053107*          MOVE 'G/A copy'       TO LN26-WHO-COPY
053107*       ELSE
053107*          MOVE 'Home Office copy'
053107*                                TO LN26-WHO-COPY
053107*       END-IF
             END-PERFORM
           END-IF
           .
       2070-EXIT.
           EXIT.
       2075-ERENDR-UPDATE.
           IF ENDR-BROWSE-STARTED
              PERFORM 2012-ERENDR-ENDBR
                                       THRU 2012-EXIT
           END-IF
           PERFORM 2025-ERENDR-READ-UPDATE
                                       THRU 2025-EXIT
           IF RESP-NORMAL
              MOVE W-SAVE-CURRENT-BIN-DATE
                                       TO EN-PRINT-DT
              PERFORM 2015-ERENDR-REWRITE
                                       THRU 2015-EXIT
              PERFORM 2100-ERNOTE-UPDATE
                                       THRU 2100-EXIT
           END-IF
           PERFORM 2010-ERENDR-STARTBR THRU 2010-EXIT
           PERFORM 2020-ERENDR-READNEXT THRU 2020-EXIT
           .
       2075-EXIT.
           EXIT.
       2100-ERNOTE-UPDATE.
           MOVE W-ERENDR-KEY           TO W-ERNOTE-KEY
041320     move '1'                    to w-note-record-type
           
      * EXEC CICS READ
      *       DATASET    (W-ERNOTE-ID)
      *       RIDFLD     (W-ERNOTE-KEY)
      *       INTO       (CERTIFICATE-NOTE)
      *       RESP       (WS-RESPONSE)
      *       UPDATE
      *    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00006325' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERNOTE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 W-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
102909       IF CN-BILLING-START-LINE-NO NOT NUMERIC
102909          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
102909       END-IF
102909       IF CN-BILLING-END-LINE-NO NOT NUMERIC
102909          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
102909       END-IF
103009       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
                 (NOTE-SUB > +10) OR
CIDMOD           (CN-LINE (NOTE-SUB) (1:8) = 'ENDORSED')
             END-PERFORM
             IF CN-LINE (NOTE-SUB) (1:8) = 'ENDORSED'
CIDMOD         
      * EXEC CICS UNLOCK
CIDMOD*           DATASET    (W-ERNOTE-ID)
CIDMOD*        END-EXEC
      *    MOVE '&*                    #   #00006344' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERNOTE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
             ELSE
113009         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
113009           (NOTE-SUB > +10) OR
113009           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES)
113009         END-PERFORM
113009         IF (NOTE-SUB < +11)
113009           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
113009              NOTE-SUB <= CN-BILLING-END-LINE-NO
113009                MOVE 'ENDORSED'   TO CN-LINE (NOTE-SUB)
113009           ELSE
113009             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
113009              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
113009                MOVE 'ENDORSED'   TO CN-LINE (NOTE-SUB)
113009                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
113009             ELSE
113009               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
113009                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
113009                     MOVE 'ENDORSED' TO CN-LINE (NOTE-SUB)
113009                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
113009               ELSE
113009                 IF (CN-BILLING-END-LINE-NO = ZEROS)
113009                   MOVE 'ENDORSED'  TO CN-LINE (NOTE-SUB)
113009                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
113009                                       CN-BILLING-START-LINE-NO
113009                 ELSE
113009                    PERFORM 2120-SQUEEZE-IT-IN
113009                                        THRU 2120-EXIT
113009                 END-IF
113009               END-IF
113009             END-IF
113009           END-IF
113009           MOVE 'E695'              TO CN-LAST-MAINT-USER
113009           MOVE W-SAVE-CURRENT-BIN-DATE
113009                                    TO CN-LAST-MAINT-DT
113009           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
113009           
      * EXEC CICS REWRITE
113009*             DATASET    (W-ERNOTE-ID)
113009*             FROM       (CERTIFICATE-NOTE)
113009*             RESP       (WS-RESPONSE)
113009*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00006382' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERNOTE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
113009           PERFORM 2200-CERTIFICATE-UPDATE THRU 2200-EXIT
113009         END-IF
113009       END-IF
           ELSE
              MOVE SPACES              TO CERTIFICATE-NOTE
              MOVE 'CN'                TO CN-RECORD-ID
              MOVE W-ERENDR-KEY        TO CN-CONTROL-PRIMARY
                                          W-ERNOTE-KEY
041320        move '1'                 to cn-record-type
041320                                    w-note-record-type
              MOVE 01                  TO CN-BILLING-START-LINE-NO
                                          CN-BILLING-END-LINE-NO
              MOVE 'ENDORSED'
                                       TO CN-LINE (01)
              MOVE 'E695'              TO CN-LAST-MAINT-USER
              MOVE W-SAVE-CURRENT-BIN-DATE
                                       TO CN-LAST-MAINT-DT
              MOVE EIBTIME              TO CN-LAST-MAINT-HHMMSS
              
      * EXEC CICS WRITE
      *          DATASET    (W-ERNOTE-ID)
      *          FROM       (CERTIFICATE-NOTE)
041320*          RIDFLD     (cn-control-primary)
      *          RESP       (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00006405' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERNOTE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 cn-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110509        PERFORM 2200-CERTIFICATE-UPDATE THRU 2200-EXIT
           END-IF
           .
       2100-EXIT.
           EXIT.
       2120-SQUEEZE-IT-IN.
113009     IF NOTE-SUB < CN-BILLING-START-LINE-NO
113009        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
113009           NOTE-SUB = +10
113009           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
113009           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
113009             MOVE 'ENDORSED' TO CN-LINE (NOTE-SUB + 1)
113009             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
113009             MOVE +9 TO NOTE-SUB
113009           END-IF
113009        END-PERFORM
113009     ELSE
113009        IF NOTE-SUB > CN-BILLING-END-LINE-NO
113009           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1
113009             UNTIL NOTE-SUB = +1
113009             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
113009             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
113009                MOVE 'ENDORSED'  TO CN-LINE (NOTE-SUB - 1)
113009                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
113009                MOVE +2          TO NOTE-SUB
113009             END-IF
113009           END-PERFORM
113009        END-IF
113009     END-IF
           .
       2120-EXIT.
           EXIT.
110509
110509 2200-CERTIFICATE-UPDATE.
110509     MOVE W-ERNOTE-KEY           TO W-ELCERT-KEY
110509
110509     
      * EXEC CICS HANDLE CONDITION
110509*        NOTFND   (2200-EXIT)
110509*    END-EXEC.
      *    MOVE '"$I                   ! # #00006447' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
110509
110509     
      * EXEC CICS READ
110509*        DATASET   (W-ELCERT-ID)
110509*        INTO      (CERTIFICATE-MASTER)
110509*        RIDFLD    (W-ELCERT-KEY)
110509*        RESP      (WS-RESPONSE)
110509*        UPDATE
110509*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00006451' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 W-ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
110509
110509     IF RESP-NORMAL
110509         IF CM-NOTE-SW = ' '
110509             MOVE '2'                TO  CM-NOTE-SW
110509         ELSE
110509           IF CM-NOTE-SW = '1'
110509              MOVE '3'               TO  CM-NOTE-SW
110509           ELSE
110509             IF CM-NOTE-SW = '4'
110509                MOVE '6'             TO  CM-NOTE-SW
110509             ELSE
110509               IF CM-NOTE-SW = '5'
110509                  MOVE '7'           TO  CM-NOTE-SW
110509               END-IF
110509             END-IF
110509           END-IF
110509         END-IF
110509         
      * EXEC CICS REWRITE
110509*            FROM      (CERTIFICATE-MASTER)
110509*            DATASET   (W-ELCERT-ID)
110509*        END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006475' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110509     END-IF.
110509 2200-EXIT.
110509     EXIT.
       3000-MATCH-TO-ACCT.
052110     MOVE ZEROS                  TO WS-CHARGEBACK-L1
052110     MOVE ' '                    TO WS-BILL-SW
052110     PERFORM 3010-ERACCT-STARTBR THRU 3010-EXIT
052110     PERFORM 3020-ERACCT-READNEXT
052110                                 THRU 3020-EXIT
052110     IF RESP-NORMAL
052110        PERFORM WITH TEST AFTER UNTIL
052110           NOT (RESP-NORMAL)
052110           OR (EN-CONTROL-PRIMARY (1:20) NOT =
052110                        AM-CONTROL-PRIMARY (1:20))
052110           IF (EN-CONTROL-PRIMARY (1:20)
052110              = AM-CONTROL-PRIMARY (1:20))
052110              MOVE ACCOUNT-MASTER TO WS-PREV-ERACCT-RANGE
052110              IF (EN-CERT-EFF-DT >= AM-EFFECTIVE-DT)
052110                 AND (EN-CERT-EFF-DT < AM-EXPIRATION-DT)
052110                 MOVE AM-COMM-CHARGEBACK (1)
052110                                 TO WS-CHARGEBACK-L1
052110                 PERFORM 4000-ERCOMP-READ THRU 4000-EXIT
052110                 IF CO-RESP-NORMAL
052110                    MOVE CO-BILL-SW TO  WS-BILL-SW
052110                 END-IF
052110              END-IF
052110              PERFORM 3020-ERACCT-READNEXT
052110                                 THRU 3020-EXIT
052110           END-IF
052110        END-PERFORM
052110        MOVE WS-PREV-ERACCT-RANGE TO ACCOUNT-MASTER
           ELSE
              MOVE 'UNKNOWN'           TO AM-NAME
                                          AM-ADDRS
                                          AM-CITY
                                          AM-ZIP
                                          AM-PERSON
           END-IF
      *     IF (EN-CONTROL-PRIMARY (1:20) = AM-CONTROL-PRIMARY (1:20))
      *        AND (EN-CERT-EFF-DT >= AM-EFFECTIVE-DT)
      *        AND (EN-CERT-EFF-DT < AM-EXPIRATION-DT)
      *        MOVE AM-COMM-CHARGEBACK (1)
      *                                 TO WS-CHARGEBACK-L1
      *        MOVE ACCOUNT-MASTER      TO WS-PREV-ERACCT-RANGE
      *     ELSE
      *        PERFORM 3010-ERACCT-STARTBR
      *                                 THRU 3010-EXIT
      *        PERFORM 3020-ERACCT-READNEXT
      *                                 THRU 3020-EXIT
      *        PERFORM 3020-ERACCT-READNEXT
      *                                 THRU 3020-EXIT UNTIL
      *           (NOT RESP-NORMAL) OR
      *           (EN-CONTROL-PRIMARY (1:20) NOT =
      *                            AM-CONTROL-PRIMARY (1:20)) OR
      *           ((EN-CONTROL-PRIMARY (1:20) =
      *                            AM-CONTROL-PRIMARY (1:20)) AND
      **          (EN-CERT-EFF-DT < AM-EXPIRATION-DT))
      *           (W-SAVE-CURRENT-BIN-DATE < AM-EXPIRATION-DT))
      *        IF (EN-CONTROL-PRIMARY (1:20) =
      *                            AM-CONTROL-PRIMARY (1:20)) AND
      **          (EN-CERT-EFF-DT < AM-EXPIRATION-DT)
      *           (W-SAVE-CURRENT-BIN-DATE < AM-EXPIRATION-DT)
      *           PERFORM 4000-ERCOMP-READ THRU 4000-EXIT
      *           IF RESP-NORMAL
      *              MOVE CO-BILL-SW TO  WS-BILL-SW
      *           END-IF
      *        ELSE
      **          MOVE 'UNKNOWN '    TO AM-NAME
      *           PERFORM 3030-ERACCT-READPREV
      *                                 THRU 3030-EXIT
      *           PERFORM 3030-ERACCT-READPREV
      *                                 THRU 3030-EXIT
      *           IF (EN-CONTROL-PRIMARY (1:20) =
      *                                 AM-CONTROL-PRIMARY (1:20))
      *               CONTINUE
      *           ELSE
      *              MOVE 'UNKNOWN'     TO AM-NAME
      *                                    AM-ADDRS
      *                                    AM-CITY
      *                                    AM-ZIP
      *                                    AM-PERSON
      *           END-IF
      *        END-IF
      *     END-IF
           .
       3000-EXIT.
           EXIT.
       3010-ERACCT-STARTBR.
           MOVE LOW-VALUES             TO W-ACCT-KEY
           MOVE PI-COMPANY-CD          TO W-ACCT-COMPANY-CD
           MOVE EN-CARRIER             TO W-ACCT-CARRIER
           MOVE EN-GROUPING            TO W-ACCT-GROUPING
           MOVE EN-STATE               TO W-ACCT-STATE
           MOVE EN-ACCOUNT             TO W-ACCT-ACCOUNT
           MOVE EN-CERT-EFF-DT         TO W-ACCT-EXP-DT
      *    MOVE W-SAVE-CURRENT-BIN-DATE
      *                                TO W-ACCT-EXP-DT
052110     IF ERACCT-BROWSE-STARTED
052110        
      * EXEC CICS ENDBR
052110*           DATASET     (W-ERACCT-ID)
052110*       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006577' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052110     END-IF
           
      * EXEC CICS STARTBR
      *       DATASET    (W-ERACCT-ID)
      *       RIDFLD     (W-ACCT-KEY)
      *       GTEQ
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006581' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERACCT-ID, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       3010-EXIT.
           EXIT.
       3020-ERACCT-READNEXT.
           
      * EXEC CICS READNEXT
      *       DATASET    (W-ERACCT-ID)
      *       RIDFLD     (W-ACCT-KEY)
      *       INTO       (ACCOUNT-MASTER)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006591' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       3020-EXIT.
           EXIT.
       3030-ERACCT-READPREV.
           
      * EXEC CICS READPREV
      *       DATASET    (W-ERACCT-ID)
      *       RIDFLD     (W-ACCT-KEY)
      *       INTO       (ACCOUNT-MASTER)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )  N#00006601' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       3030-EXIT.
           EXIT.
       3050-MATCH-TO-CSR.
           IF EN-LAST-MAINT-BY = CF-PROCESSOR
              CONTINUE
           ELSE
              PERFORM 3060-ELCNTL-READ THRU 3060-EXIT
              IF (RESP-NORMAL) AND
                 (EN-LAST-MAINT-BY = CF-PROCESSOR)
                 CONTINUE
              ELSE
                 MOVE 'ANN WENZL ' TO CF-PROCESSOR-NAME
              END-IF
           END-IF
           .
       3050-EXIT.
           EXIT.
       3060-ELCNTL-READ.
           MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID
           MOVE '2'                    TO W-CNTL-RECORD-TYPE
           MOVE EN-LAST-MAINT-BY       TO W-CNTL-PROCESSOR
           MOVE +0                     TO W-CNTL-SEQ-NO
           
      * EXEC CICS READ
      *       DATASET    (W-ELCNTL-ID)
      *       RIDFLD     (W-CNTL-KEY)
      *       INTO       (CONTROL-FILE)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00006630' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELCNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       3060-EXIT.
           EXIT.
       4000-ERCOMP-READ.
           IF AM-REMIT-TO NOT NUMERIC
              MOVE 1                   TO AM-REMIT-TO
           END-IF
           MOVE LOW-VALUES             TO W-COMP-KEY
           MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD
           MOVE AM-CARRIER             TO W-COMP-CARRIER
           MOVE AM-GROUPING            TO W-COMP-GROUPING
           MOVE AM-AGT (AM-REMIT-TO)   TO W-COMP-FIN-RESP
           MOVE AM-AGT (1)             TO W-COMP-ACCOUNT
           MOVE 'A'                    TO W-COMP-REC-TYPE
           
      * EXEC CICS READ
      *       DATASET    (W-ERCOMP-ID)
      *       RIDFLD     (W-COMP-KEY)
      *       INTO       (COMPENSATION-MASTER)
052110*       RESP       (WS-CO-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00006650' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERCOMP-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-CO-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4000-EXIT.
           EXIT.
       5000-GET-TEXT.
           MOVE ' '                    TO WS-LETR-BROWSE-SW
                                          WS-LETR-EOF-SW
           PERFORM 5010-ELLETR-STARTBR THRU 5010-EXIT
           IF RESP-NORMAL
              PERFORM 5020-ELLETR-READNEXT THRU 5020-EXIT
           END-IF
           IF RESP-NORMAL
              IF TX-CONTROL-PRIMARY (1:13) = W-LETR-HOLD-KEY
081209           MOVE 'Y' TO WS-TEXT-FOUND
                 PERFORM 5030-PROCESS-LETR THRU 5030-EXIT UNTIL
                   NO-MORE-LETR
              END-IF
           END-IF
           IF LETR-BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *           DATASET     (W-ELLETR-ID)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006674' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELLETR-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       5000-EXIT.
           EXIT.
       5010-ELLETR-STARTBR.
           
      * EXEC CICS STARTBR
      *         DATASET    (W-ELLETR-ID)
      *         RIDFLD     (W-LETR-KEY)
      *         GTEQ
      *         RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006682' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELLETR-ID, 
                 W-LETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              SET LETR-BROWSE-STARTED TO TRUE
           END-IF
           .
       5010-EXIT.
           EXIT.
       5020-ELLETR-READNEXT.
           
      * EXEC CICS READNEXT
      *         DATASET    (W-ELLETR-ID)
      *         INTO       (TEXT-FILES)
      *         RIDFLD     (W-LETR-KEY)
      *         RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006695' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ELLETR-ID, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 W-LETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       5020-EXIT.
           EXIT.
       5030-PROCESS-LETR.
      *    MOVE TX-TEXT-LINE           TO W-TX-TEXT (W-TG-NDX)
      *    SET W-TG-NDX                UP BY +1
           MOVE TX-TEXT-LINE           TO WS-TEXT (W-SUB2)
           ADD +1                      TO W-SUB2
           PERFORM 5020-ELLETR-READNEXT
                                       THRU 5020-EXIT
           IF (NOT RESP-NORMAL) OR
              (TX-CONTROL-PRIMARY (1:13) NOT = W-LETR-HOLD-KEY)
              SET NO-MORE-LETR TO TRUE
           END-IF
           .
       5030-EXIT.
           EXIT.
       5040-RES-VAR.
           PERFORM VARYING W-SUB1 FROM +1 BY +1 UNTIL
              (W-SUB1 > +10) OR
              (WS-TEXT (W-SUB1) = SPACES)
              MOVE +1                  TO OUT-SUB
              MOVE ' '                 TO WS-LAST-CHAR
              PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
053107           (IN-SUB > +75)
                 IF WS-CHAR1 (W-SUB1 IN-SUB) = '@'
                    IF WS-LAST-CHAR = '@'
                       PERFORM 5050-ADD-VAR THRU 5050-EXIT
                       ADD +1          TO OUT-SUB
                    ELSE
                       MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO WS-LAST-CHAR
                    END-IF
                 ELSE
                    MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO WS-CHAR2 (W-SUB1 OUT-SUB)
                                          WS-LAST-CHAR
                    ADD +1             TO OUT-SUB
                 END-IF
              END-PERFORM
           END-PERFORM
           .
       5040-EXIT.
           EXIT.
       5050-ADD-VAR.
           ADD +1                      TO IN-SUB
                                          OUT-SUB
           MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO W-IN1
           ADD +1                      TO IN-SUB
           MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO W-IN2
           ADD +1                      TO IN-SUB
           MOVE WS-CHAR1 (W-SUB1 IN-SUB)
                                       TO W-IN3
           IF (W-IN1-3 NUMERIC)
033006        AND (W-IN1-3 < 6)
              SET W-VG-NDX             TO W-IN1-3
              PERFORM VARYING W-VC-NDX FROM +1 BY +1 UNTIL
                 (W-VC-NDX > W-VARIABLE-SIZE (W-VG-NDX))
                 MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
                                       TO WS-CHAR2 (W-SUB1 OUT-SUB)
                 ADD +1                TO OUT-SUB
      *                                   IN-SUB
              END-PERFORM
           END-IF
           .
       5050-EXIT.
           EXIT.
       9700-DATE-LINK.
           
      * EXEC CICS LINK
      *         PROGRAM  ('ELDATCV')
      *         COMMAREA (DATE-CONVERSION-DATA)
      *         LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006771' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
            EXIT.
       9800-PRINT-ROUTINE.             
      *                                COPY ELPRTCVP.
00001 ******************************************************************
00002 ***                                                              *
00003 ***                          ELPRTCVP.                           *
00004 ***                          VMOD=2.003                          *
00005 ***                                                              *
00006 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
00007 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
00008 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
00009 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
00010 ***                                                              *
00011 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
00012 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
00013 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
00014 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
00015 ***      SUCCESSFUL.                                             *
00016 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
00017 ***                         LENGTH OF THE LINE TO BE PRINTED     *
00018 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
00019 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
00020 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
00021 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
00022 ***                         PROGRAM END SWITCH. INITIALIZED      *
00023 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
00024 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
00025 ***      05  WS-PRINT-AREA.                                      *
00026 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
00027 ***          10  WS-PASSED-DATA          PIC X(132).             *
00028 ***                         USE THE DATA TO BE PRINTED IN THE    *
00029 ***                         WS-PASSED-DATA.                      *
00030 ***                         USE THE STANDARD CARRIAGE CONTROL    *
00031 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
00032 ***                           SINGLE-SPACE            VALUE ' '  *
00033 ***                           DOUBLE-SPACE            VALUE '0'  *
00034 ***                           TRIPLE-SPACE            VALUE '-'  *
00035 ***                           TOP-PAGE                VALUE '1'  *
00036 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
00037 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
00038 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
00039 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
00040 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
00041 ***            CONDITION.                                        *
00042 ***                                                              *
00043 ******************************************************************
00044
00045  ELPRTCVP.
00046
pemuni*    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'
pemuni     IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'XXX'
00048          MOVE 'P'                TO DL34-PROCESS-TYPE
00049          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00050          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00051          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00052          MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE
00053          MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00054
00055          
      * EXEC CICS LINK
00056 *            PROGRAM    ('DLO034')
00057 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00058 *            LENGTH     (DLO034-REC-LENGTH)
00059 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00006835' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00060
00061             IF DL34-RETURN-CODE = 'OK'
00062                 GO TO ELPRTCVP-EXIT
00063             ELSE
00064 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
00065                 GO TO ELPRTCVP-EXIT.
00066
00067      IF NOT FIRST-TIME
00068          GO TO ELPRTCVP-020.
00069
00070      IF WS-LINE-LEN NOT GREATER ZERO
00071          GO TO ELPRTCVP-EXIT.
00072
00073      MOVE '2'                    TO WS-FIRST-TIME-SW.
00074      MOVE LOW-VALUES             TO WS-BUFFER-AREA.
00075
00076      SET BUFFER-INDEX TO +1
00077
00078      IF EIBTRMID IS EQUAL TO 'AFLP'
00079          NEXT SENTENCE
00080      ELSE
00081          IF NOT TOP-PAGE
00082              MOVE T-TP           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00083              SET BUFFER-INDEX UP BY +1.
00084
00085  ELPRTCVP-020.
00086      IF WS-PROG-END = SPACES
00087          GO TO ELPRTCVP-030.
00088
00089      MOVE SPACES                 TO WS-PROG-END.
00090
00091      IF BUFFER-INDEX GREATER +1
00092          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00093
00094      MOVE '1'                    TO WS-FIRST-TIME-SW.
00095
00096      GO TO ELPRTCVP-EXIT.
00097
00098  ELPRTCVP-030.
00099      IF WS-PASSED-DATA = SPACES
00100          SET PRT-INDEX TO +1
00101          GO TO ELPRTCVP-050.
00102
00103      SET PRT-INDEX TO WS-LINE-LEN.
00104
00105  ELPRTCVP-040.
00106      IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES
00107          GO TO ELPRTCVP-050.
00108
00109      IF PRT-INDEX GREATER +1
00110          SET PRT-INDEX DOWN BY +1
00111          GO TO ELPRTCVP-040.
00112
00113  ELPRTCVP-050.
00114      SET WS-LINE-LENGTH TO PRT-INDEX.
00115      SET BUFFER-INDEX2 TO BUFFER-INDEX.
00116      SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.
00117
00118      IF BUFFER-INDEX2 NOT LESS WS-BUFFER-SIZE
00119          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00120
00121      IF TRIPLE-SPACE
00122           ADD +2  TO  WS-LINE-CNT
00123           MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00124                                  WS-BUFFER-BYTE (BUFFER-INDEX + 1)
00125           SET BUFFER-INDEX UP BY +2.
00126
00127      IF DOUBLE-SPACE
00128           ADD +1  TO  WS-LINE-CNT
00129           MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)
00130           SET BUFFER-INDEX UP BY +1.
00131
00132      ADD +1 TO WS-LINE-CNT
00133 ************************************************************
00134 *     BYPASS NEW LINE SYMBOL                               *
00135 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *
00136 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *
00137 ************************************************************
00138
00139      IF (BUFFER-INDEX GREATER +1 AND
00140          WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)  OR
00141          FIRST-LINE-NEXT-BUFFER
00142          MOVE ZERO               TO WS-FIRST-TIME-SW
00143      ELSE
00144          MOVE T-SS               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00145          SET BUFFER-INDEX UP BY +1.
00146
00147 **   NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR
00148
00149      IF TOP-PAGE
00150          MOVE +1                TO WS-LINE-CNT
00151          MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)
00152          SET BUFFER-INDEX UP BY +1.
00153
00154      SET PRT-INDEX TO +1.
00155
00156  ELPRTCVP-060.
00157      MOVE WS-PRINT-BYTE (PRT-INDEX)
00158                                  TO WS-BUFFER-BYTE (BUFFER-INDEX).
00159      SET BUFFER-INDEX UP BY +1.
00160
00161      IF PRT-INDEX LESS WS-LINE-LENGTH
00162          SET PRT-INDEX UP BY +1
00163          GO TO ELPRTCVP-060.
00164
00165  ELPRTCVP-EXIT.
00166      EXIT.
00167
00168  ELPRTCVP-PRINT-BUFFER.
00169      IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS
00170         MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00171         SET BUFFER-INDEX UP BY 1.
00172
00173      MOVE  T-EM                  TO  WS-BUFFER-BYTE (BUFFER-INDEX)
00174      SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
00175
00176      
      * EXEC CICS SEND
00177 *        FROM    (WS-BUFFER-AREA)
00178 *        LENGTH  (WS-BUFFER-LENGTH)
00179 *        CTLCHAR (WS-WCC-CNTL)
00180 *        ERASE
00181 *    END-EXEC.
      *    MOVE '$$    C E         L F ,   #00006956' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BUFFER-AREA, 
                 WS-BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WS-WCC-CNTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00182
00183      SET BUFFER-INDEX TO +1.
00184      MOVE '2'                    TO WS-FIRST-TIME-SW.
00185
00186  ELPRTCVP-PRINT-EXIT.
00187      EXIT.
00188
       9999-GOBACK.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6952' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6952' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0200-END-DATA,
                     0300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2200-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6952' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
