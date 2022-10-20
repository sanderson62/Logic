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
081209* 081209    2009081100001   AJRA  PRINT COMMENT LINES < 5 CHARACTERS
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
                                       COPY ELCINTF.
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
           COPY ELPRTCVD.
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
           COPY ELCDATE.
                                       EJECT
           COPY ERCENDR.
                                       EJECT
           COPY ERCNOTE.
                                       EJECT
           COPY ELCTEXT.
                                       EJECT
           COPY ELCCNTL.
                                       EJECT
           COPY ERCACCT.
                                       EJECT
           COPY ERCCOMP.
                                       EJECT
           COPY ELCDMD34.
110509
110509     COPY ELCCERT.

       LINKAGE SECTION.
      *01 PARMLIST .
      *    02  FILLER                  PIC S9(08) COMP.
      *    02  L-ARCH-POINTER          PIC S9(08) COMP.
      *    02  L-ARCT-POINTER          PIC S9(08) COMP.
                                       EJECT
       01  L-LETTER-ARCHIVE            PIC X(250).
                                       EJECT
       PROCEDURE DIVISION.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-CURRENT-DATE
           MOVE DC-BIN-DATE-1          TO W-SAVE-CURRENT-BIN-DATE
           MOVE SPACES                 TO DL34-PROCESS-TYPE

           .
       0100-RETRIEVE-LOOP.

           EXEC CICS HANDLE CONDITION
                ENDDATA (0200-END-DATA)
                NOTFND  (0300-NOT-FOUND)
           END-EXEC

           EXEC CICS RETRIEVE
                INTO    (PROGRAM-INTERFACE-BLOCK)
                LENGTH  (PI-COMM-LENGTH)
           END-EXEC


      * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
               IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
                   MOVE 'O'                TO DL34-PROCESS-TYPE
                   MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
                   MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
                   MOVE PI-PROCESSOR-ID    TO DL34-USERID
                   MOVE SPACES             TO DL34-PRINT-LINE
                   MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
                   EXEC CICS LINK
                       PROGRAM    ('DLO034')
                       COMMAREA   (DLO034-COMMUNICATION-AREA)
                       LENGTH     (DLO034-REC-LENGTH)
                   END-EXEC
                   IF DL34-RETURN-CODE NOT = 'OK'
                       MOVE  '**DLO034 OPEN ERROR - ABORT**'
                                           TO W-ERROR-LINE
                       PERFORM 0400-SEND-TEXT
                       EXEC CICS RETURN
                       END-EXEC.

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
               EXEC CICS LINK
                   PROGRAM    ('DLO034')
                   COMMAREA   (DLO034-COMMUNICATION-AREA)
                   LENGTH     (DLO034-REC-LENGTH)
               END-EXEC
               IF DL34-RETURN-CODE NOT = 'OK'
                   MOVE  '**DLO034 CLOSE ERROR - ABORT**'
                                       TO W-ERROR-LINE
                   PERFORM 0400-SEND-TEXT.

           EXEC CICS RETURN
           END-EXEC

           .
       0300-NOT-FOUND.

           MOVE 'NO COMMUNICATION AREA FOUND'
                                       TO W-ERROR-LINE.
           PERFORM 0400-SEND-TEXT.
           GO TO 0200-END-DATA.

       0400-SEND-TEXT.

           EXEC CICS SEND TEXT
               FROM   (W-ERROR-LINE)
               LENGTH (70)
           END-EXEC.
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

           EXEC CICS STARTBR
              DATASET    (W-ERENDR-ID)
              RIDFLD     (W-ERENDR-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET ENDR-BROWSE-STARTED TO TRUE
           END-IF
           .
       2010-EXIT.
           EXIT.

       2012-ERENDR-ENDBR.

           EXEC CICS ENDBR
              DATASET    (W-ERENDR-ID)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE ' '                 TO WS-ENDR-BROWSE-SW
           END-IF

           .
       2012-EXIT.
           EXIT.

       2015-ERENDR-REWRITE.

           EXEC CICS REWRITE
              DATASET    (W-ERENDR-ID)
              FROM       (ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       2015-EXIT.
           EXIT.

       2020-ERENDR-READNEXT.

           EXEC CICS READNEXT
              DATASET    (W-ERENDR-ID)
              RIDFLD     (W-ERENDR-KEY)
              INTO       (ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       2020-EXIT.
           EXIT.

       2025-ERENDR-READ-UPDATE.

           EXEC CICS READ
              UPDATE
              DATASET    (W-ERENDR-ID)
              RIDFLD     (W-ERENDR-KEY)
              INTO       (ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           ADD +1                      TO W-RECORD-COUNT

           IF W-RECORD-COUNT > +100
              MOVE +0                  TO W-RECORD-COUNT
              EXEC CICS DELAY
                   INTERVAL (W-DELAY-INTERVAL)
              END-EXEC
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
      *                     ' MONTHS '
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

           EXEC CICS READ
              DATASET    (W-ERNOTE-ID)
              RIDFLD     (W-ERNOTE-KEY)
              INTO       (CERTIFICATE-NOTE)
              RESP       (WS-RESPONSE)
              UPDATE
           END-EXEC

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
CIDMOD         EXEC CICS UNLOCK
CIDMOD            DATASET    (W-ERNOTE-ID)
CIDMOD         END-EXEC
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
113009           EXEC CICS REWRITE
113009              DATASET    (W-ERNOTE-ID)
113009              FROM       (CERTIFICATE-NOTE)
113009              RESP       (WS-RESPONSE)
113009           END-EXEC
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
              EXEC CICS WRITE
                 DATASET    (W-ERNOTE-ID)
                 FROM       (CERTIFICATE-NOTE)
041320           RIDFLD     (cn-control-primary)
                 RESP       (WS-RESPONSE)
              END-EXEC
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
110509     EXEC CICS HANDLE CONDITION
110509         NOTFND   (2200-EXIT)
110509     END-EXEC.               
110509                             
110509     EXEC CICS READ          
110509         DATASET   (W-ELCERT-ID)
110509         INTO      (CERTIFICATE-MASTER) 
110509         RIDFLD    (W-ELCERT-KEY)
110509         RESP      (WS-RESPONSE)
110509         UPDATE                           
110509     END-EXEC.                            
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
110509         EXEC CICS REWRITE
110509             FROM      (CERTIFICATE-MASTER)
110509             DATASET   (W-ELCERT-ID)
110509         END-EXEC
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
052110        EXEC CICS ENDBR
052110            DATASET     (W-ERACCT-ID)
052110        END-EXEC
052110     END-IF

           EXEC CICS STARTBR
              DATASET    (W-ERACCT-ID)
              RIDFLD     (W-ACCT-KEY)
              GTEQ
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       3010-EXIT.
           EXIT.

       3020-ERACCT-READNEXT.

           EXEC CICS READNEXT
              DATASET    (W-ERACCT-ID)
              RIDFLD     (W-ACCT-KEY)
              INTO       (ACCOUNT-MASTER)
              RESP       (WS-RESPONSE)
           END-EXEC

           .
       3020-EXIT.
           EXIT.

       3030-ERACCT-READPREV.

           EXEC CICS READPREV
              DATASET    (W-ERACCT-ID)
              RIDFLD     (W-ACCT-KEY)
              INTO       (ACCOUNT-MASTER)
              RESP       (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS READ
              DATASET    (W-ELCNTL-ID)
              RIDFLD     (W-CNTL-KEY)
              INTO       (CONTROL-FILE)
              RESP       (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS READ
              DATASET    (W-ERCOMP-ID)
              RIDFLD     (W-COMP-KEY)
              INTO       (COMPENSATION-MASTER)
052110        RESP       (WS-CO-RESPONSE)
           END-EXEC

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
              EXEC CICS ENDBR
                  DATASET     (W-ELLETR-ID)
              END-EXEC
           END-IF

           .
       5000-EXIT.
           EXIT.

       5010-ELLETR-STARTBR.

           EXEC CICS STARTBR
                DATASET    (W-ELLETR-ID)
                RIDFLD     (W-LETR-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET LETR-BROWSE-STARTED TO TRUE
           END-IF

           .
       5010-EXIT.
           EXIT.

       5020-ELLETR-READNEXT.

           EXEC CICS READNEXT
                DATASET    (W-ELLETR-ID)
                INTO       (TEXT-FILES)
                RIDFLD     (W-LETR-KEY)
                RESP       (WS-RESPONSE)
           END-EXEC

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

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

       9800-PRINT-ROUTINE.             COPY ELPRTCVP.

       9999-GOBACK.

           GOBACK.

       9999-EXIT.
           EXIT.

