      *((program: EL1278.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1278.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 04/21/94 14:09:26.
000007*                            VMOD=2.030.
000008*
000009*
000010*AUTHOR.        LOGIC,INC.
000011*               DALLAS, TEXAS.
000012
000013*DATE-COMPILED.
000014
000015*SECURITY.   *****************************************************
000016*            *                                                   *
000017*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000018*            *                                                   *
000019*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000020*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000021*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
000022*            *                                                   *
000023*            *****************************************************
000024*
000025*REMARKS.
000026*        TRANSACTION - EXXA - CANCELLATION QUOTE.
000027*
000028******************************************************************
000029*                   C H A N G E   L O G
000030*
000031* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000032*-----------------------------------------------------------------
000033*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000034* EFFECTIVE    NUMBER
000035*-----------------------------------------------------------------
000036* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000037* 070105    2005063000003  PEMA  COMMENT OUT CODE THAT ADDS 1
000038*         TO THE TERM AND REMAINING TERM
000039* 092705  CR2005050300006  PEMA  ADD SPP LEASES
000040* 081606  CR2006051800002  PEMA  ADD POST CARD PROCESSING
000041* 111907  CR2004020600011  PEMA  FORCE FP TO REFUND AS FP
000042* 022608  CR2008010200006  PEMA  CHANGE MO RTERM MTHOD
000043* 050508  CR2008010200004  PEMA  FIX MO REFUNDING
000044* 090408  CR2008081900002  PEMA  UPDATE MO REFUNDING
000045* 021710  IR2010021000002  PEMA  ADD CANCEL DATE EDIT
000046* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000047* 042011  CR2010030900001  PEMA ADD CLM PROCESSING ON QUOTES
000048* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
000049* 010412  CR2011022800001  AJRA  NAPERSOFT CANCEL PROCESSING
000050* 032912  CR2011110200001  PEMA  AHL CHANGES
000051* 062712  CR2011022800001  AJRA NAPERSOFT
000052* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
000053* 102212  CR2012013100001  PEMA  ALLOW MN OVERRIDE RF
000054* 112612  CR2012101700002  AJRA  FIX CANCEL WHEN DEATH CLAIM
000055* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000056* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
000057* 050713  CR2011062300001  PEMA  REM CHG FOR EXT DYS FOR ROA REF
000058* 041514  CR2012110200003  AJRA  REMOVE PF11 FROM SCREEN,
000059*                                CANCEL QUOTE SCRIPT WILL DO PF11
000060* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
000061* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000062* 072616  CR2016020400001  PEMA  ADD PF10 CONFIRMATION
000063* 032117  CR2017030300002  PEMA  ADD COMM % AND COMM AMT
000064* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000065* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000066* 070622  CR2020061200002  TANA  Add cancel reason logic
000067******************************************************************
000068
000069 EJECT
000070 ENVIRONMENT DIVISION.
000071 DATA DIVISION.
000072 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000073 77  FILLER  PIC  X(32) VALUE '********************************'.
000074 77  FILLER  PIC  X(32) VALUE '*    EL1278 WORKING STORAGE    *'.
000075 77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.030 *********'.
000076 77  S1                          PIC S999 COMP-3 VALUE +0.
000077 77  P1                          PIC S999 COMP-3 VALUE +0.
000078 77  P2                          PIC S999 COMP-3 VALUE +0.
000079 77  C0                          PIC S999 COMP-3 VALUE +0.
000080 77  C1                          PIC S999 COMP-3 VALUE +0.
000081 77  C2                          PIC S999 COMP-3 VALUE +0.
000082 77  C3                          PIC S999 COMP-3 VALUE +0.
000083 77  WS-STOP-SW                  PIC X  VALUE ' '.
000084     88  I-SAY-TO-STOP      VALUE 'Y'.
000085 77  WS-OPEN-LF-CLAIM            PIC X  VALUE ' '.
000086     88  OPEN-LF-CLAIM              VALUE 'Y'.
000087 77  WS-CLOSED-LF-CLAIM          PIC X  VALUE ' '.
000088     88  CLOSED-LF-CLAIM            VALUE 'Y'.
000089 77  WS-OPEN-AH-CLAIM            PIC X  VALUE ' '.
000090     88  OPEN-AH-CLAIM              VALUE 'Y'.
000091 77  WS-CLOSED-AH-CLAIM          PIC X  VALUE ' '.
000092     88  CLOSED-AH-CLAIM            VALUE 'Y'.
000093 77  WS-FOUND-A-CLAIM            PIC X  VALUE ' '.
000094     88  FOUND-A-CLAIM              VALUE 'Y'.
000095 77  WS-CLAIM-TYPE               PIC X  VALUE ' '.
000096     88  LIFE-CLAIM                 VALUE 'L'.
000097     88  OTHER-CLAIM                VALUE 'O'.
000098 77  WS-CLAIM-LIT                PIC X(06)  VALUE SPACES.
000099
000100 77  WS-ST-REF-IND               PIC X.
000101 77  WS-LF-INCUR-DT              PIC XX  VALUE LOW-VALUES.
000102 77  WS-AH-INCUR-DT              PIC XX  VALUE LOW-VALUES.
000103 77  WS-AH-PAID-THRU-DT          PIC XX  VALUE LOW-VALUES.
000104 77  WS-LF-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
000105 77  WS-AH-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
000106 77  DD-IU-SW                    PIC X   VALUE ' '.
000107     88  DD-IU-PRESENT                 VALUE 'Y'.
000108 77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
000109 77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
000110 77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
000111 77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
000112 77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
000113 77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
000114 77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
000115 77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
000116 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
000117     88  NO-CERT-RW                 VALUE 'N'.
000118     88  CERT-RW                    VALUE 'Y'.
000119 77  WS-ERMAIL-SW                PIC X  VALUE ' '.
000120     88  ERMAIL-FOUND                 VALUE 'Y'.
000121 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE ZERO.
000122 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000123 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
000124
000127 EXEC SQL
          INCLUDE SQLDA
       END-EXEC
000128
000131*EXEC SQL
      *   INCLUDE SQLCA
      *END-EXEC
      *>>((file: SQLCA))
000001****************************************************************<*
000002* Copyright (c) 2016-2021 NTT DATA, Inc. All rights reserved.  *<*
000003* Users of NTT DATA Enterprise COBOL may freely                *<*
000004* redistribute this copybook.                                  *<*
000005****************************************************************<*
000006
000007 01  SQLCA GLOBAL.
000008     05  SQLCAID                PIC X(8).
000009     05  SQLCABC                PIC S9(9) COMP-5.
000010     05  SQLCODE                PIC S9(9) COMP-5.
000011     05  SQLERRM.
000012         49  SQLERRML           PIC S9(4) COMP-5.
000013         49  SQLERRMC           PIC X(254).
000014     05  SQLERRP                PIC X(8).
000015     05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
000016     05  SQLWARN.
000017         10 SQLWARN0            PIC X(1).
000018         10 SQLWARN1            PIC X(1).
000019         10 SQLWARN2            PIC X(1).
000020         10 SQLWARN3            PIC X(1).
000021         10 SQLWARN4            PIC X(1).
000022         10 SQLWARN5            PIC X(1).
000023         10 SQLWARN6            PIC X(1).
000024         10 SQLWARN7            PIC X(1).
000025     05  SQLSTATE               PIC X(5).
000026     05  SQLEXT                 PIC S9(5) COMP-3 VALUE 1.
      *<<((file: SQLCA))
000132
000134 EXEC SQL
          BEGIN DECLARE SECTION
000135 END-EXEC
000136
000137 01  SQLCMD                      PIC X(1024).
000138 01  SVR                         PIC X(32).
000139 01  USR                         PIC X(32).
000140 01  PASS                        PIC X(32).
000141 01  USR-PASS                    PIC X(64).
000142
000143 01  WS-SQL-DATA.
000144     05  WS-CYCLE-DATE           PIC X(10).
000145     05  WS-NEXT-BUS-DT          PIC X(10).
000146     05  WS-LOOKUPID             PIC X(4).
000147     05  WS-LOOKUPNAME           PIC X(4).
000148     05  WS-LOOKUP-VALUE         PIC X(100).
000149     05  WS-CARRIER              PIC X.
000150     05  WS-GROUP                PIC X(6).
000151     05  WS-STATE                PIC XX.
000152     05  WS-ACCOUNT              PIC X(10).
000153     05  WS-EFF-DT               PIC XX.
000154     05  WS-CERT-NO              PIC X(10).
000155     05  WS-CERT-NO-SUF          PIC X(01).
000156
000158 EXEC SQL
          END DECLARE SECTION
000159 END-EXEC
000160 01  P pointer.
000161 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000162 01  KIXHOST             pic x(9) value Z"HOSTNAME".
000163 01  var-ptr pointer.
000164 01  env-var-len                 pic 9(4)  binary.
000165 01  rc                          pic 9(9)  binary.
000166
000167 01  WS-KIXHOST                  PIC X(10).
000168 01  WS-KIXSYS.
000169     05  WS-KIX-FIL1             PIC X(10).
000170     05  WS-KIX-APPS             PIC X(10).
000171     05  WS-KIX-ENV              PIC X(10).
000172     05  WS-KIX-MYENV            PIC X(10).
000173     05  WS-KIX-SYS              PIC X(10).
000174
000175
000176*                            COPY ELCSCTM.
      *>>((file: ELCSCTM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCTM                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
000008*                                                                *
000009******************************************************************
000010 01  SECURITY-MESSAGE.
000011     12  FILLER                          PIC X(30)
000012            VALUE '** LOGIC SECURITY VIOLATION -'.
000013     12  SM-READ                         PIC X(6).
000014     12  FILLER                          PIC X(5)
000015            VALUE ' PGM='.
000016     12  SM-PGM                          PIC X(6).
000017     12  FILLER                          PIC X(5)
000018            VALUE ' OPR='.
000019     12  SM-PROCESSOR-ID                 PIC X(4).
000020     12  FILLER                          PIC X(6)
000021            VALUE ' TERM='.
000022     12  SM-TERMID                       PIC X(4).
000023     12  FILLER                          PIC XX   VALUE SPACE.
000024     12  SM-JUL-DATE                     PIC 9(5).
000025     12  FILLER                          PIC X    VALUE SPACE.
000026     12  SM-TIME                         PIC 99.99.
000027
      *<<((file: ELCSCTM))
000177
000178*                            COPY ELCSCRTY.
      *>>((file: ELCSCRTY))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCRTY                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
000008*        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
000009*        SAVED IN PI-SECURITY-ADDRESS.                           *
000010*                                                                *
000011******************************************************************
000012 01  SECURITY-CONTROL.
000013     12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
000014     12  FILLER                       PIC XX    VALUE 'SC'.
000015     12  SC-CREDIT-CODES.
000016         16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
000017             20  SC-CREDIT-DISPLAY    PIC X.
000018             20  SC-CREDIT-UPDATE     PIC X.
000019     12  SC-CLAIMS-CODES.
000020         16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
000021             20  SC-CLAIMS-DISPLAY    PIC X.
000022             20  SC-CLAIMS-UPDATE     PIC X.
      *<<((file: ELCSCRTY))
000179
000180 01  FILLER          COMP-3.
000181     12  WS-BALLOON-RTRM         PIC S9(03)      VALUE ZERO.
000182     12  WS-ELAPSED-MONTHS       PIC S9(03)      VALUE ZERO.
000183     12  WS-TOT-AH-PREM          PIC S9(9)V99    VALUE ZERO.
000184     12  WS-TOT-LF-PREM          PIC S9(9)V99    VALUE ZERO.
000185     12  WS-TOT-AH-RFND          PIC S9(9)V99    VALUE ZERO.
000186     12  WS-TOT-LF-RFND          PIC S9(9)V99    VALUE ZERO.
000187     12  WS-TOT-PREM             PIC S9(9)V99    VALUE ZERO.
000188     12  WS-TOT-RFND             PIC S9(9)V99    VALUE ZERO.
000189     12  WS-TOT-ITDR             PIC S9(9)V99    VALUE ZERO.
000190
000191 01  WS-DATE-AREA.
000192     12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
000193     12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
000194     12  WS-LF-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
000195     12  WS-AH-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
000196     12  WS-LF-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
000197     12  WS-AH-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
000198
000199 01  ERPDEF-KEY-SAVE             PIC X(18).
000200 01  ERPDEF-KEY.
000201     12  ERPDEF-COMPANY-CD       PIC X.
000202     12  ERPDEF-STATE            PIC XX.
000203     12  ERPDEF-PROD-CD          PIC XXX.
000204     12  F                       PIC X(7).
000205     12  ERPDEF-BEN-TYPE         PIC X.
000206     12  ERPDEF-BEN-CODE         PIC XX.
000207     12  ERPDEF-EXP-DT           PIC XX.
000208
000209 01  ELMSTR-KEY.
000210     12  ELMSTR-COMP-CD          PIC X.
000211     12  ELMSTR-CERT-NO          PIC X(11).
000212
000213 01  ELCRTT-KEY.
000214     12  ELCRTT-PRIMARY          PIC X(33).
000215     12  ELCRTT-REC-TYPE         PIC X(1).
000216
000217 01  ELCRTO-KEY.
000218     05  ELCRTO-COMPANY-CD       PIC X.
000219     05  ELCRTO-CARRIER          PIC X.
000220     05  ELCRTO-GROUPING         PIC X(6).
000221     05  ELCRTO-STATE            PIC XX.
000222     05  ELCRTO-ACCOUNT          PIC X(10).
000223     05  ELCRTO-CERT-EFF-DT      PIC XX.
000224     05  ELCRTO-CERT-NO.
000225         10  ELCRTO-CERT-PRIME   PIC X(10).
000226         10  ELCRTO-CERT-SFX     PIC X.
000227     05  ELCRTO-RECORD-TYPE      PIC X.
000228     05  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.
000229
000230 01  STANDARD-AREAS.
000231     12  SC-ITEM                 PIC S9(04) COMP VALUE +0001.
000232     12  SUB3                    PIC S9(04) COMP VALUE +0.
000233     12  SUB4                    PIC S9(04) COMP VALUE +0.
000234     12  GETMAIN-SPACE           PIC  X(01)      VALUE SPACE.
000235     12  MAP-NAME                PIC  X(08)      VALUE 'EL127H'.
000236     12  MAPSET-NAME             PIC  X(08)      VALUE 'EL1278S'.
000237     12  SCREEN-NUMBER           PIC  X(04)      VALUE '127H'.
000238     12  TRANS-ID                PIC  X(04)      VALUE 'EXXA'.
000239     12  THIS-PGM                PIC  X(08)      VALUE 'EL1278'.
000240     12  PGM-NAME                PIC  X(08).
000241     12  TIME-IN                 PIC S9(07).
000242     12  TIME-OUT-R  REDEFINES  TIME-IN.
000243         16  FILLER              PIC  X(01).
000244         16  TIME-OUT            PIC  9(02)V99.
000245         16  FILLER              PIC  X(02).
000246     12  XCTL-005                PIC  X(08)      VALUE 'EL005'.
000247     12  XCTL-010                PIC  X(08)      VALUE 'EL010'.
000248     12  XCTL-126                PIC  X(08)      VALUE 'EL126'.
000249     12  LINK-001                PIC  X(08)      VALUE 'EL001'.
000250     12  LINK-004                PIC  X(08)      VALUE 'EL004'.
000251     12  LINK-ELDATCV            PIC  X(08)      VALUE 'ELDATCV'.
000252     12  ELRTRM-ID               PIC  X(08)      VALUE 'ELRTRM'.
000253     12  ELRAMT-ID               PIC  X(08)      VALUE 'ELRAMT'.
000254     12  ELRFND-ID               PIC  X(08)      VALUE 'ELRFND'.
000255     12  ELCNTL-ID               PIC  X(08)      VALUE 'ELCNTL'.
000256     12  ERACCT-ID               PIC  X(08)      VALUE 'ERACCT'.
000257     12  ELCERT-ID               PIC  X(08)      VALUE 'ELCERT'.
000258     12  ERMAIL-ID               PIC  X(08)      VALUE 'ERMAIL'.
000259     12  ERFORM-ID               PIC  X(08)      VALUE 'ERFORM'.
000260     12  ERNOTE-ID               PIC  X(08)      VALUE 'ERCNOT'.
000261     12  ELCRTT-ID               PIC  X(08)      VALUE 'ELCRTT'.
000262     12  DATE-RANGE-SW           PIC  X(01)      VALUE SPACE.
000263     12  WS-REFUND-SEARCH-SW     PIC  X(01)      VALUE ' '.
000264         88  LIFE-REFUND-SEARCH                  VALUE 'L'.
000265     12  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
000266         88  BROWSE-STARTED                      VALUE 'Y'.
000267     12  WS-FORM-RECORD-SW       PIC  X(01)      VALUE SPACE.
000268         88  FORM-FOUND                          VALUE 'Y'.
000269     12  BEN-SEARCH-SW           PIC  X(01)      VALUE SPACE.
000270         88  NO-BENEFIT-FOUND                    VALUE 'N'.
000271     12  WS-CF-LF-COVERAGE-TYPE  PIC  X(01)      VALUE SPACE.
000272         88  WS-REDUCING                         VALUE 'R'.
000273         88  WS-LEVEL                            VALUE 'L'  'P'.
000274     12  WS-NEW-LF-CANCEL-SW     PIC  X(01)      VALUE 'N'.
000275         88  WS-NEW-LF-CANCEL                    VALUE 'Y'.
000276     12  WS-NEW-AH-CANCEL-SW     PIC  X(01)      VALUE 'N'.
000277         88  WS-NEW-AH-CANCEL                    VALUE 'Y'.
000278     12  WS-BENEFIT-DESCRIP      PIC  X(10)      VALUE SPACES.
000279     12  WS-PMT-DESCRIP          PIC  X(10)      VALUE
000280         'CLAIM PMTS'.
000281     12  WS-REFUND-DESCRIP       PIC  X(10)      VALUE
000282         'ITD REFUND'.
000283     12  WS-ACCESS.
000284         16  FILLER              PIC  X(02).
000285         16  WS-BEN-CD           PIC  X(02).
000286     12  WS-KIND                 PIC  X(03)      VALUE SPACES.
000287     12  WS-LF-KIND              PIC  X(03)      VALUE SPACES.
000288     12  WS-AH-KIND              PIC  X(03)      VALUE SPACES.
000289     12  WS-LOOKUP-TYPE          PIC  X(01).
000290     12  WS-BENEFIT-NO           PIC  X(02)      VALUE ZERO.
000291     12  WS-STATE-ABBREVIATION   PIC  X(02)      VALUE SPACES.
000292     12  WS-CALC-CD              PIC  X(01).
000293     12  DEEDIT-FIELD            PIC  X(15).
000294     12  DEEDIT-FIELD-V0  REDEFINES
000295         DEEDIT-FIELD            PIC S9(15).
000296     12  WS-CURRENT-DATE         PIC  X(02)  VALUE LOW-VALUES.
000297     12  RETURN-FROM             PIC  X(08).
000298     12  QID.
000299         16  QID-TERM            PIC  X(04).
000300         16  FILLER              PIC  X(04)      VALUE '127H'.
000301     12  WS-FIRST-ENTRY-SW       PIC  X(01)      VALUE SPACE.
000302         88  FIRST-ENTRY                         VALUE 'Y'.
000303     12  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
000304         88  PDEF-FOUND                          VALUE 'Y'.
000305     12  WS-CERT-RECORD-SW       PIC  X(01)      VALUE SPACE.
000306         88  CERT-FOUND                          VALUE 'Y'.
000307         88  CERT-NOT-FOUND                      VALUE 'N'.
000308     12  WS-CNTL-RECORD-SW       PIC  X(01)      VALUE SPACE.
000309         88  CNTL-FOUND                          VALUE 'Y'.
000310         88  CNTL-NOT-FOUND                      VALUE 'N'.
000311     12  WS-BEN-RECORD-SW        PIC  X(01)      VALUE SPACE.
000312         88  BEN-FOUND                           VALUE 'Y'.
000313     12  WS-STATE-RECORD-SW      PIC  X(01)      VALUE SPACE.
000314         88  STATE-FOUND                         VALUE 'Y'.
000315     12  WS-ST-BEN-RECORD-SW     PIC  X(01)      VALUE SPACE.
000316         88  ST-BEN-FOUND                        VALUE 'Y'.
000317     12  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
000318         88  ACCT-FOUND                          VALUE 'Y'.
000319     12  WS-DUPREC-SW            PIC  X(01)      VALUE SPACE.
000320         88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
000321     12  WS-NOT-FOUND            PIC S9(01) COMP-3
000322                                                 VALUE ZERO.
000323         88  BENEFIT-FOUND                       VALUE +1.
000324     12  WS-CALC-REFUND          PIC S9(07)V99 COMP-3
000325                                                 VALUE ZERO.
000326     12  WS-LIFE-PREMIUM         PIC S9(07)V99 COMP-3
000327                                                 VALUE ZERO.
000328     12  WS-INDEX                PIC S9(04) COMP VALUE ZERO.
000329     12  WS-SUB1                 PIC S99    COMP.
000330     12  WS-ACCT-USER-FLD-5      PIC  X(02).
000331     12  WS-CF-DEFAULT-APR       PIC  S9(03)V9(04) COMP-3.
000332     12  WS-CF-CR-R78-METHOD     PIC  X(01)      VALUE SPACE.
000333     12  WS-CF-CR-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000334     12  WS-AH-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
000335     12  WS-AH-BEN-CATEGORY      PIC  X          VALUE SPACE.
000336     12  WS-LF-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
000337     12  WS-AH-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000338     12  WS-LF-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000339     12  WS-AH-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
000340     12  WS-LF-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
000341     12  WS-AH-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000342     12  WS-LF-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000343     12  WS-AH-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000344     12  WS-LF-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000345     12  WS-AH-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000346     12  WS-LF-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
000347     12  WS-AH-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000348     12  WS-LF-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000349     12  WS-AH-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000350     12  WS-LF-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
000351     12  WS-AM-EARN-METHOD-A     PIC  X(01)      VALUE SPACE.
000352     12  WS-AM-EARN-METHOD-L     PIC  X(01)      VALUE SPACE.
000353     12  WS-AM-EARN-METHOD-R     PIC  X(01)      VALUE SPACE.
000354     12  ERROR-MESSAGES.
000355         16  ER-0000             PIC  X(04)      VALUE '0000'.
000356         16  ER-0004             PIC  X(04)      VALUE '0004'.
000357         16  ER-0008             PIC  X(04)      VALUE '0008'.
000358         16  ER-0028             PIC  X(04)      VALUE '0028'.
000359         16  ER-0029             PIC  X(04)      VALUE '0029'.
000360         16  ER-0070             PIC  X(04)      VALUE '0070'.
000361         16  ER-0142             PIC  X(04)      VALUE '0142'.
000362         16  ER-0214             PIC  X(04)      VALUE '0214'.
000363         16  ER-0681             PIC  X(04)      VALUE '0681'.
000364         16  ER-0682             PIC  X(04)      VALUE '0682'.
000365         16  ER-0683             PIC  X(04)      VALUE '0683'.
000366         16  ER-0684             PIC  X(04)      VALUE '0684'.
000367         16  ER-1590             PIC  X(04)      VALUE '1590'.
000368         16  ER-2227             PIC  X(04)      VALUE '2227'.
000369         16  ER-2601             PIC  X(04)      VALUE '2601'.
000370         16  ER-2616             PIC  X(04)      VALUE '2616'.
000371         16  ER-2617             PIC  X(04)      VALUE '2617'.
000372         16  ER-2619             PIC  X(04)      VALUE '2619'.
000373         16  ER-2740             PIC  X(04)      VALUE '2740'.
000374         16  ER-2756             PIC  X(04)      VALUE '2756'.
000375         16  ER-2768             PIC  X(04)      VALUE '2768'.
000376         16  ER-2774             PIC  X(04)      VALUE '2774'.
000377         16  ER-2775             PIC  X(04)      VALUE '2775'.
000378         16  ER-2965             PIC  X(04)      VALUE '2965'.
000379         16  ER-2966             PIC  X(04)      VALUE '2966'.
000380         16  ER-2967             PIC  X(04)      VALUE '2967'.
000381         16  ER-2968             PIC  X(04)      VALUE '2968'.
000382         16  ER-2969             PIC  X(04)      VALUE '2969'.
000383         16  ER-2998             PIC  X(04)      VALUE '2998'.
000384         16  ER-3037             PIC  X(04)      VALUE '3037'.
000385         16  ER-3038             PIC  X(04)      VALUE '3038'.
000386         16  ER-3039             PIC  X(04)      VALUE '3039'.
000387         16  ER-3780             PIC  X(04)      VALUE '3780'.
000388         16  ER-3781             PIC  X(04)      VALUE '3781'.
000389         16  ER-3830             PIC  X(04)      VALUE '3830'.
000390         16  er-3847             pic  x(04)      value '3847'.
000391         16  ER-8160             PIC  X(04)      VALUE '8160'.
000392         16  ER-8161             PIC  X(04)      VALUE '8161'.
000393         16  ER-9999             PIC  X(04)      VALUE '9999'.
000394     12  WS-CF-LIFE-OVERRIDE-L1  PIC  X(01).
000395     12  WS-CF-LIFE-OVERRIDE-L2  PIC  X(02).
000396     12  WS-CF-AH-OVERRIDE-L1    PIC  X(01).
000397     12  WS-CF-AH-OVERRIDE-L2    PIC  X(02).
000398
000399     12  WS-RESPONSE             PIC S9(8)       COMP.
000400         88  WS-RESP-NORMAL                      VALUE +00.
000401         88  WS-RESP-NOTFND                      VALUE +13.
000402         88  WS-RESP-DUPREC                      VALUE +14.
000403         88  WS-RESP-DUPKEY                      VALUE +15.
000404         88  WS-RESP-NOTOPEN                     VALUE +19.
000405         88  WS-RESP-ENDFILE                     VALUE +20.
000406
000407     12  WS-01-CANCEL-ERR        PIC X(25)
000408         VALUE 'CANCEL DATE ERROR'.
000409     12  WS-02-CANCEL-ERR        PIC X(25)
000410         VALUE 'CERT NOT FOUND'.
000411     12  WS-04-CANCEL-ERR        PIC X(25)
000412         VALUE 'CANCEL AMOUNT ERROR'.
000413     12  WS-05-CANCEL-ERR        PIC X(25)
000414         VALUE 'CANCEL OPTION ERROR'.
000415     12  WS-06-CANCEL-ERR        PIC X(25)
000416         VALUE 'PREVIOUSLY CANCELLED'.
000417     12  WS-07-CANCEL-ERR        PIC X(25)
000418         VALUE 'INVALID DATA'.
000419     12  WS-08-CANCEL-ERR        PIC X(25)
000420         VALUE 'NO ACCOUNT MASTER'.
000421     12  WS-09-CANCEL-ERR        PIC X(25)
000422         VALUE 'SUFFIX ALREADY EXISTS'.
000423     12  WS-99-CANCEL-ERR        PIC X(25)
000424         VALUE 'MISC CANCEL ERROR'.
000425
000426 01  WS-SAVE-ALLOWABLE-BENEFIT.
000427     16  WS-SAVE-BENEFIT-CODE      PIC XX.
000428     16  WS-SAVE-BENEFIT-TYPE      PIC X.
000429     16  WS-SAVE-BENEFIT-REVISION  PIC XXX.
000430     16  WS-SAVE-BENEFIT-REM-TERM  PIC X.
000431     16  WS-SAVE-BENEFIT-RETRO-Y-N PIC X.
000432     16  FILLER                    PIC XX.
000433
000434 01  CTBL-KEY-SAVE               PIC X(5).
000435 01  CTBL-KEY.
000436     05  CTBL-COMPANY-CD         PIC X.
000437     05  CTBL-TABLE              PIC XXX.
000438     05  CTBL-BEN-TYPE           PIC X.
000439     05  CTBL-BEN-CODE           PIC XX.
000440
000441 01  CANCEL-GEN-PASS-AREA.
000442     05  CG-OPTION-CODE          PIC X.
000443         88  CG-VALID-OPTION       VALUE '1' '2' '3'.
000444         88  CG-FLAT-CANCEL        VALUE '1'.
000445         88  CG-CANCEL             VALUE '2'.
000446         88  CG-CANCEL-REISSUE     VALUE '3'.
000447     05  CG-ERROR-CODE           PIC 99.
000448         88  CG-SUCCESS            VALUE 00.
000449         88  CG-DATE-ERROR         VALUE 01.
000450         88  CG-CERT-NOT-FOUND     VALUE 02.
000451         88  CG-AMOUNT-ERROR       VALUE 04.
000452         88  CG-OPTION-ERROR       VALUE 05.
000453         88  CG-PREV-CAN           VALUE 06.
000454         88  CG-INVALID-DATA       VALUE 07.
000455         88  CG-NO-ACCT-MSTR       VALUE 08.
000456         88  CG-SFX-A-EXIST        VALUE 09.
000457         88  CG-MISC-ERROR         VALUE 99.
000458     05  CG-COMPANY-ID           PIC XXX.
000459     05  CG-PROC-ID              PIC XXXX.
000460     05  CG-CURRENT-DT           PIC XX.
000461     05  CG-MONTH-END-DT         PIC XX.
000462     05  CG-CERT-KEY.
000463         10  CG-CERT-COMPANY-CD  PIC X.
000464         10  CG-CERT-CARRIER     PIC X.
000465         10  CG-CERT-GROUP       PIC X(6).
000466         10  CG-CERT-STATE       PIC XX.
000467         10  CG-CERT-ACCOUNT     PIC X(10).
000468         10  CG-CERT-EFF-DT      PIC XX.
000469         10  CG-CERT-CERT-NO     PIC X(11).
000470     05  CG-LF-CAN-DATA.
000471         10  CG-LF-CAN-DT        PIC XX.
000472         10  CG-LF-CAN-AMT       PIC S9(7)V99 COMP-3.
000473     05  CG-AH-CAN-DATA.
000474         10  CG-AH-CAN-DT        PIC XX.
000475         10  CG-AH-CAN-AMT       PIC S9(7)V99 COMP-3.
000476     05  CG-CERT-PROFILE-DATA.
000477         10  CG-INS-LNAME        PIC X(15).
000478         10  CG-INS-FNAME        PIC X(10).
000479         10  CG-INS-MID-INIT     PIC X.
000480         10  CG-INS-AGE          PIC 99.
000481         10  CG-JNT-LNAME        PIC X(15).
000482         10  CG-JNT-FNAME        PIC X(10).
000483         10  CG-JNT-MID-INIT     PIC X.
000484         10  CG-JNT-AGE          PIC 99.
000485     05  CG-LF-ISS-DATA.
000486         10  CG-LF-BENCD         PIC XX.
000487         10  CG-LF-PREM-AMT      PIC S9(7)V99 COMP-3.
000488         10  CG-LF-ALT-PREM-AMT  PIC S9(7)V99 COMP-3.
000489     05  CG-AH-ISS-DATA.
000490         10  CG-AH-BENCD         PIC XX.
000491         10  CG-AH-PREM-AMT      PIC S9(7)V99 COMP-3.
000492     05  CG-BATCH-NO             PIC X(6).
000493     05  CG-BATCH-SEQ-NO         PIC 9(4)  COMP.
000494     05  CG-DCC-REASON-CD        PIC X.
000495     05  CG-COMM-PCT-ZERO        PIC X.
000496     05  cg-vin                  pic x(17).
000497     05  cg-from-where           pic x(6).
000498     05  FILLER                  PIC X(281).
000499
000500
000501 01  WS-CAN-QUOTE-NOTE.
000502     12  WS-CN-CANCEL-DT         PIC  X(09).
000503     12  FILLER                  PIC  X(10)      VALUE
000504         'RF QUOTE: '.
000505     12  WS-CN-LIFE-AMT-RF       PIC  Z,ZZ9.99.
000506     12  FILLER                  PIC  X(4)       VALUE
000507         ' LF '.
000508     12  WS-CN-LF-REF-METH       PIC  X.
000509     12  FILLER                  PIC  X          VALUE ','.
000510     12  WS-CN-AH-AMT-RF         PIC  Z,ZZ9.99.
000511     12  FILLER                  PIC  X(5)       VALUE
000512         ' AH '.
000513     12  WS-CN-AH-REF-METH       PIC  X.
000514     12  FILLER                  PIC  XX         VALUE ','.
000515     12  FILLER                  PIC  X(6)       VALUE
000516         'TOTAL '.
000517     12  WS-CN-TOTAL-RF          PIC  Z,ZZ9.99.
000518     12  FILLER                  PIC  X(01)      VALUE ' '.
000519     12  WS-CN-DT-QUOTED         PIC  X(08).
000520     12  FILLER                  PIC  X(01)      VALUE ';'.
000521     12  WS-CN-PROCESSOR-ID      PIC  X(04).
000522
000523 01  WS-CM-CONTROL-PRIMARY.
000524     12  WS-CM-COMPANY-CD        PIC  X(01).
000525     12  WS-CM-CARRIER           PIC  X(01).
000526     12  WS-CM-GROUPING          PIC  X(06).
000527     12  WS-CM-STATE             PIC  X(02).
000528     12  WS-CM-ACCOUNT           PIC  X(10).
000529     12  WS-CM-CERT-EFF-DT       PIC  X(02).
000530     12  WS-CM-CERT-NO.
000531         16  WS-CM-CERT-PRIME    PIC  X(10).
000532         16  WS-CM-CERT-SFX      PIC  X(01).
000533
000534 01  WS-MA-CONTROL-PRIMARY.
000535     12  WS-MA-COMPANY-CD        PIC  X(01).
000536     12  WS-MA-CARRIER           PIC  X(01).
000537     12  WS-MA-GROUPING          PIC  X(06).
000538     12  WS-MA-STATE             PIC  X(02).
000539     12  WS-MA-ACCOUNT           PIC  X(10).
000540     12  WS-MA-CERT-EFF-DT       PIC  X(02).
000541     12  WS-MA-CERT-NO.
000542         16  WS-MA-CERT-PRIME    PIC  X(10).
000543         16  WS-MA-CERT-SFX      PIC  X(01).
000544
000545 01  WS-CF-CONTROL-PRIMARY.
000546     12  WS-CF-COMPANY-ID        PIC  X(03)      VALUE SPACES.
000547     12  WS-CF-RECORD-TYPE       PIC  X(01)      VALUE ZERO.
000548*        88  COMPANY-MASTER                      VALUE '1'.
000549*        88  STATE-MASTER                        VALUE '3'.
000550*        88  LF-BENEFIT-MASTER                   VALUE '4'.
000551*        88  AH-BENEFIT-MASTER                   VALUE '5'.
000552     12  WS-CF-ACCESS.
000553         16  WS-CF-STATE         PIC  X(02)      VALUE SPACES.
000554         16  WS-CF-BENEFIT-NO                    VALUE SPACES.
000555             20  FILLER          PIC  X(01).
000556             20  WS-CF-CARRIER   PIC  X(01).
000557     12  WS-CF-SEQUENCE-NO       PIC S9(04) COMP VALUE ZERO.
000558
000559 01  WS-AM-CONTROL-PRIMARY.
000560     12  WS-AM-COMPANY-CD        PIC  X(01).
000561     12  WS-AM-CARRIER           PIC  X(01).
000562     12  WS-AM-GROUPING          PIC  X(06).
000563     12  WS-AM-STATE             PIC  X(02).
000564     12  WS-AM-ACCOUNT           PIC  X(10).
000565     12  WS-AM-EXPIRATION-DT     PIC  X(02).
000566     12  WS-AM-FILLER            PIC  X(04).
000567
000568 01  WS-CN-CONTROL-PRIMARY.
000569     12  WS-CN-COMPANY-CD        PIC  X(01).
000570     12  WS-CN-CARRIER           PIC  X(01).
000571     12  WS-CN-GROUPING          PIC  X(06).
000572     12  WS-CN-STATE             PIC  X(02).
000573     12  WS-CN-ACCOUNT           PIC  X(10).
000574     12  WS-CN-CERT-EFF-DT       PIC  X(02).
000575     12  WS-CN-CERT-NO.
000576         16  WS-CN-CERT-PRIME    PIC  X(10).
000577         16  WS-CN-CERT-SFX      PIC  X.
000578     12  WS-CN-REC-TYPE          PIC X.
000579     12  WS-CN-SEQ-NO            PIC 9(4) BINARY.
000580
000581 01  WS-FO-CONTROL-PRIMARY.
000582     12  WS-FO-COMPANY-CD        PIC  X(01).
000583     12  WS-FO-STATE             PIC  X(02).
000584     12  WS-FO-FORM-ID           PIC  X(12).
000585     12  WS-FO-EXP-DT            PIC  X(02).
000586 EJECT
000587
000588
000589*                            COPY ERCCNOT.
      *>>((file: ERCCNOT))
000001******************************************************************
000002*                                                                *
000003*                            ERCCNOT                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*        FILE DESCRIPTION = CERTIFICATE NOTES                    *
000008*                                                                *
000009*        FILE TYPE= VSAM,KSDS                                    *
000010*        RECORD SIZE = 150    RECFORM = FIXED                    *
000011*                                                                *
000012*        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
000013*                                                                *
000014*        LOG = YES                                               *
000015*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000016*                                                                *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
000026******************************************************************
000027
000028 01  CERT-NOTE-FILE.
000029     12  CZ-RECORD-ID                PIC  XX.
000030         88  VALID-CZ-ID                  VALUE 'CZ'.
000031
000032     12  CZ-CONTROL-PRIMARY.
000033         16  CZ-COMPANY-CD           PIC X.
000034         16  CZ-CARRIER              PIC X.
000035         16  CZ-GROUPING.
000036             20 CZ-GROUPING-PREFIX   PIC XXX.
000037             20 CZ-GROUPING-PRIME    PIC XXX.
000038         16  CZ-STATE                PIC XX.
000039         16  CZ-ACCOUNT.
000040             20 CZ-ACCOUNT-PREFIX    PIC X(4).
000041             20 CZ-ACCOUNT-PRIME     PIC X(6).
000042         16  CZ-CERT-EFF-DT          PIC XX.
000043         16  CZ-CERT-NO.
000044             20  CZ-CERT-PRIME       PIC X(10).
000045             20  CZ-CERT-SFX         PIC X.
000046         16  CZ-RECORD-TYPE          PIC X.
000047             88  CERT-NOTE           VALUE '1'.
000048             88  CLAIM-CERT-NOTE     VALUE '2'.
000049         16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
000050
000051     12  CZ-LAST-MAINT-DT            PIC XX.
000052     12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000053     12  CZ-LAST-MAINT-USER          PIC X(4).
000054
000055     12  CZ-NOTE-INFORMATION.
000056         16  CZ-NOTE                 PIC X(63).
000057         16  FILLER                  PIC X(39).
000058******************************************************************
      *<<((file: ERCCNOT))
000590
000591*                            COPY ELCDATE.
      *>>((file: ELCDATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDATE.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*                                                                *
000009*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
000010*                 LENGTH = 200                                   *
000011******************************************************************
000012
000013 01  DATE-CONVERSION-DATA.
000014     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
000015     12  DC-OPTION-CODE                PIC X.
000016         88  BIN-TO-GREG                VALUE ' '.
000017         88  ELAPSED-BETWEEN-BIN        VALUE '1'.
000018         88  EDIT-GREG-TO-BIN           VALUE '2'.
000019         88  YMD-GREG-TO-BIN            VALUE '3'.
000020         88  MDY-GREG-TO-BIN            VALUE '4'.
000021         88  JULIAN-TO-BIN              VALUE '5'.
000022         88  BIN-PLUS-ELAPSED           VALUE '6'.
000023         88  FIND-CENTURY               VALUE '7'.
000024         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
000025         88  EDIT-GREG-TO-BIN-3         VALUE '9'.
000026         88  YMD-GREG-TO-BIN-3          VALUE 'A'.
000027         88  MDY-GREG-TO-BIN-3          VALUE 'B'.
000028         88  JULIAN-TO-BIN-3            VALUE 'C'.
000029         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
000030         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
000031         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
000032         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
000033         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
000034         88  CHECK-LEAP-YEAR            VALUE 'H'.
000035         88  BIN-3-TO-GREG              VALUE 'I'.
000036         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
000037         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
000038         88  CYMD-GREG-TO-BIN           VALUE 'L'.
000039         88  MDCY-GREG-TO-BIN           VALUE 'M'.
000040         88  MDY-GREG-TO-JULIAN         VALUE 'N'.
000041         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
000042         88  YMD-GREG-TO-JULIAN         VALUE 'P'.
000043         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
000044         88  THREE-CHARACTER-BIN
000045                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
000046         88  GREGORIAN-TO-BIN
000047                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
000048         88  BIN-TO-GREGORIAN
000049                  VALUES ' ' '1' 'I' '8' 'G'.
000050         88  JULIAN-TO-BINARY
000051                  VALUES '5' 'C' 'E' 'F'.
000052     12  DC-ERROR-CODE                 PIC X.
000053         88  NO-CONVERSION-ERROR        VALUE ' '.
000054         88  DATE-CONVERSION-ERROR
000055                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
000056         88  DATE-IS-ZERO               VALUE '1'.
000057         88  DATE-IS-NON-NUMERIC        VALUE '2'.
000058         88  DATE-IS-INVALID            VALUE '3'.
000059         88  DATE1-GREATER-DATE2        VALUE '4'.
000060         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
000061         88  DATE-INVALID-OPTION        VALUE '9'.
000062         88  INVALID-CENTURY            VALUE 'A'.
000063         88  ONLY-CENTURY               VALUE 'B'.
000064         88  ONLY-LEAP-YEAR             VALUE 'C'.
000065         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
000066     12  DC-END-OF-MONTH               PIC X.
000067         88  CALCULATE-END-OF-MONTH     VALUE '1'.
000068     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
000069         88  USE-NORMAL-PROCESS         VALUE ' '.
000070         88  ADJUST-DOWN-100-YRS        VALUE '1'.
000071         88  ADJUST-UP-100-YRS          VALUE '2'.
000072     12  FILLER                        PIC X.
000073     12  DC-CONVERSION-DATES.
000074         16  DC-BIN-DATE-1             PIC XX.
000075         16  DC-BIN-DATE-2             PIC XX.
000076         16  DC-GREG-DATE-1-EDIT       PIC X(08).
000077         16  DC-GREG-DATE-1-EDIT-R REDEFINES
000078                       DC-GREG-DATE-1-EDIT.
000079             20  DC-EDIT1-MONTH        PIC 99.
000080             20  SLASH1-1              PIC X.
000081             20  DC-EDIT1-DAY          PIC 99.
000082             20  SLASH1-2              PIC X.
000083             20  DC-EDIT1-YEAR         PIC 99.
000084         16  DC-GREG-DATE-2-EDIT       PIC X(08).
000085         16  DC-GREG-DATE-2-EDIT-R REDEFINES
000086                     DC-GREG-DATE-2-EDIT.
000087             20  DC-EDIT2-MONTH        PIC 99.
000088             20  SLASH2-1              PIC X.
000089             20  DC-EDIT2-DAY          PIC 99.
000090             20  SLASH2-2              PIC X.
000091             20  DC-EDIT2-YEAR         PIC 99.
000092         16  DC-GREG-DATE-1-YMD        PIC 9(06).
000093         16  DC-GREG-DATE-1-YMD-R  REDEFINES
000094                     DC-GREG-DATE-1-YMD.
000095             20  DC-YMD-YEAR           PIC 99.
000096             20  DC-YMD-MONTH          PIC 99.
000097             20  DC-YMD-DAY            PIC 99.
000098         16  DC-GREG-DATE-1-MDY        PIC 9(06).
000099         16  DC-GREG-DATE-1-MDY-R REDEFINES
000100                      DC-GREG-DATE-1-MDY.
000101             20  DC-MDY-MONTH          PIC 99.
000102             20  DC-MDY-DAY            PIC 99.
000103             20  DC-MDY-YEAR           PIC 99.
000104         16  DC-GREG-DATE-1-ALPHA.
000105             20  DC-ALPHA-MONTH        PIC X(10).
000106             20  DC-ALPHA-DAY          PIC 99.
000107             20  FILLER                PIC XX.
000108             20  DC-ALPHA-CENTURY.
000109                 24 DC-ALPHA-CEN-N     PIC 99.
000110             20  DC-ALPHA-YEAR         PIC 99.
000111         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
000112         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
000113         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
000114         16  DC-JULIAN-DATE            PIC 9(05).
000115         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
000116                                       PIC 9(05).
000117         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
000118             20  DC-JULIAN-YEAR        PIC 99.
000119             20  DC-JULIAN-DAYS        PIC 999.
000120         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
000121         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
000122         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
000123     12  DATE-CONVERSION-VARIBLES.
000124         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
000125         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
000126             20  FILLER                PIC 9(3).
000127             20  HOLD-CEN-1-CCYY.
000128                 24  HOLD-CEN-1-CC     PIC 99.
000129                 24  HOLD-CEN-1-YY     PIC 99.
000130             20  HOLD-CEN-1-MO         PIC 99.
000131             20  HOLD-CEN-1-DA         PIC 99.
000132         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
000133             20  HOLD-CEN-1-R-MO       PIC 99.
000134             20  HOLD-CEN-1-R-DA       PIC 99.
000135             20  HOLD-CEN-1-R-CCYY.
000136                 24  HOLD-CEN-1-R-CC   PIC 99.
000137                 24  HOLD-CEN-1-R-YY   PIC 99.
000138             20  FILLER                PIC 9(3).
000139         16  HOLD-CENTURY-1-X.
000140             20  FILLER                PIC X(3)  VALUE SPACES.
000141             20  HOLD-CEN-1-X-CCYY.
000142                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
000143                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
000144             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
000145             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
000146         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
000147             20  HOLD-CEN-1-R-X-MO     PIC XX.
000148             20  HOLD-CEN-1-R-X-DA     PIC XX.
000149             20  HOLD-CEN-1-R-X-CCYY.
000150                 24  HOLD-CEN-1-R-X-CC PIC XX.
000151                 24  HOLD-CEN-1-R-X-YY PIC XX.
000152             20  FILLER                PIC XXX.
000153         16  DC-BIN-DATE-EXPAND-1      PIC XXX.
000154         16  DC-BIN-DATE-EXPAND-2      PIC XXX.
000155         16  DC-JULIAN-DATE-1          PIC 9(07).
000156         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
000157             20  DC-JULIAN-1-CCYY.
000158                 24  DC-JULIAN-1-CC    PIC 99.
000159                 24  DC-JULIAN-1-YR    PIC 99.
000160             20  DC-JULIAN-DA-1        PIC 999.
000161         16  DC-JULIAN-DATE-2          PIC 9(07).
000162         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
000163             20  DC-JULIAN-2-CCYY.
000164                 24  DC-JULIAN-2-CC    PIC 99.
000165                 24  DC-JULIAN-2-YR    PIC 99.
000166             20  DC-JULIAN-DA-2        PIC 999.
000167         16  DC-GREG-DATE-A-EDIT.
000168             20  DC-EDITA-MONTH        PIC 99.
000169             20  SLASHA-1              PIC X VALUE '/'.
000170             20  DC-EDITA-DAY          PIC 99.
000171             20  SLASHA-2              PIC X VALUE '/'.
000172             20  DC-EDITA-CCYY.
000173                 24  DC-EDITA-CENT     PIC 99.
000174                 24  DC-EDITA-YEAR     PIC 99.
000175         16  DC-GREG-DATE-B-EDIT.
000176             20  DC-EDITB-MONTH        PIC 99.
000177             20  SLASHB-1              PIC X VALUE '/'.
000178             20  DC-EDITB-DAY          PIC 99.
000179             20  SLASHB-2              PIC X VALUE '/'.
000180             20  DC-EDITB-CCYY.
000181                 24  DC-EDITB-CENT     PIC 99.
000182                 24  DC-EDITB-YEAR     PIC 99.
000183         16  DC-GREG-DATE-CYMD         PIC 9(08).
000184         16  DC-GREG-DATE-CYMD-R REDEFINES
000185                              DC-GREG-DATE-CYMD.
000186             20  DC-CYMD-CEN           PIC 99.
000187             20  DC-CYMD-YEAR          PIC 99.
000188             20  DC-CYMD-MONTH         PIC 99.
000189             20  DC-CYMD-DAY           PIC 99.
000190         16  DC-GREG-DATE-MDCY         PIC 9(08).
000191         16  DC-GREG-DATE-MDCY-R REDEFINES
000192                              DC-GREG-DATE-MDCY.
000193             20  DC-MDCY-MONTH         PIC 99.
000194             20  DC-MDCY-DAY           PIC 99.
000195             20  DC-MDCY-CEN           PIC 99.
000196             20  DC-MDCY-YEAR          PIC 99.
000197    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
000198        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
000199    12  DC-EL310-DATE                  PIC X(21).
000200    12  FILLER                         PIC X(28).
      *<<((file: ELCDATE))
000592 EJECT
000593*                            COPY ELCLOGOF.
      *>>((file: ELCLOGOF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCLOGOF.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
000008*                                                                *
000009******************************************************************
000010 01  CLASIC-LOGOFF.
000011     12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000012     12  LOGOFF-TEXT.
000013         16  FILLER          PIC X(5)    VALUE SPACES.
000014         16  LOGOFF-MSG.
000015             20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
000016             20  FILLER      PIC X       VALUE SPACES.
000017             20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
000018         16  FILLER          PIC X(80)
000019           VALUE '* YOU ARE NOW LOGGED OFF'.
000020         16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000021         16  FILLER          PIC X       VALUE QUOTE.
000022         16  LOGOFF-SYS-MSG  PIC X(17)
000023           VALUE 'S CLAS-IC SYSTEM '.
000024     12  TEXT-MESSAGES.
000025         16  UNACCESS-MSG    PIC X(29)
000026             VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
000027         16  PGMIDERR-MSG    PIC X(17)
000028             VALUE 'PROGRAM NOT FOUND'.
      *<<((file: ELCLOGOF))
000594 EJECT
000595*                            COPY ELCATTR.
      *>>((file: ELCATTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCATTR.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*             LIST OF STANDARD ATTRIBUTE VALUES                  *
000007*                                                                *
000008*   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
000009*                                                                *
000010*                   POS 1   P=PROTECTED                          *
000011*                           U=UNPROTECTED                        *
000012*                           S=ASKIP                              *
000013*                   POS 2   A=ALPHA/NUMERIC                      *
000014*                           N=NUMERIC                            *
000015*                   POS 3   N=NORMAL                             *
000016*                           B=BRIGHT                             *
000017*                           D=DARK                               *
000018*                   POS 4-5 ON=MODIFIED DATA TAG ON              *
000019*                           OF=MODIFIED DATA TAG OFF             *
000020*                                                                *
000021*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
000022******************************************************************
000023 01  ATTRIBUTE-LIST.
000024     12  AL-PABOF            PIC X       VALUE 'Y'.
000025     12  AL-PABON            PIC X       VALUE 'Z'.
000026     12  AL-PADOF            PIC X       VALUE '%'.
000027     12  AL-PADON            PIC X       VALUE '_'.
000028     12  AL-PANOF            PIC X       VALUE '-'.
000029     12  AL-PANON            PIC X       VALUE '/'.
000030     12  AL-SABOF            PIC X       VALUE '8'.
000031     12  AL-SABON            PIC X       VALUE '9'.
000032     12  AL-SADOF            PIC X       VALUE '@'.
000033     12  AL-SADON            PIC X       VALUE QUOTE.
000034     12  AL-SANOF            PIC X       VALUE '0'.
000035     12  AL-SANON            PIC X       VALUE '1'.
000036     12  AL-UABOF            PIC X       VALUE 'H'.
000037     12  AL-UABON            PIC X       VALUE 'I'.
000038     12  AL-UADOF            PIC X       VALUE '<'.
000039     12  AL-UADON            PIC X       VALUE '('.
000040     12  AL-UANOF            PIC X       VALUE ' '.
000041     12  AL-UANON            PIC X       VALUE 'A'.
000042     12  AL-UNBOF            PIC X       VALUE 'Q'.
000043     12  AL-UNBON            PIC X       VALUE 'R'.
000044     12  AL-UNDOF            PIC X       VALUE '*'.
000045     12  AL-UNDON            PIC X       VALUE ')'.
000046     12  AL-UNNOF            PIC X       VALUE '&'.
000047     12  AL-UNNON            PIC X       VALUE 'J'.
      *<<((file: ELCATTR))
000596 EJECT
000597*                            COPY ELCEMIB.
      *>>((file: ELCEMIB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEMIB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
000009*                                                                *
000010******************************************************************
000011 01  ERROR-MESSAGE-INTERFACE-BLOCK.
000012     12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
000013     12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
000014     12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
000015     12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
000016     12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
000017     12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
000018     12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
000019     12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
000020     12  EMI-SWITCH1             PIC X        VALUE '1'.
000021         88  EMI-NO-ERRORS                    VALUE '1'.
000022         88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
000023         88  EMI-ERRORS-COMPLETE              VALUE '3'.
000024     12  EMI-SWITCH2             PIC X        VALUE '1'.
000025         88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
000026     12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
000027         88  EMI-AREA1-EMPTY                  VALUE '1'.
000028         88  EMI-AREA1-FULL                   VALUE '2'.
000029     12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
000030         88  EMI-AREA2-EMPTY                  VALUE '1'.
000031         88  EMI-AREA2-FULL                   VALUE '2'.
000032     12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
000033         88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
000034         88  EMI-BYPASS-NOTES                 VALUE 'N'.
000035         88  EMI-BYPASS-WARNINGS              VALUE 'W'.
000036         88  EMI-BYPASS-FORCABLES             VALUE 'F'.
000037         88  EMI-BYPASS-FATALS                VALUE 'X'.
000038     12  EMI-ERROR-LINES.
000039         16  EMI-LINE1           PIC X(72)   VALUE SPACES.
000040         16  EMI-LINE2           PIC X(72)   VALUE SPACES.
000041         16  EMI-LINE3           PIC X(72)   VALUE SPACES.
000042         16  EMI-CODE-LINE REDEFINES EMI-LINE3.
000043             20  EMI-ERR-CODES OCCURS 10 TIMES.
000044                 24  EMI-ERR-NUM         PIC X(4).
000045                 24  EMI-FILLER          PIC X.
000046                 24  EMI-SEV             PIC X.
000047                 24  FILLER              PIC X.
000048             20  FILLER                  PIC X(02).
000049     12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
000050         16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
000051             20  EMI-ERROR-NUMBER    PIC X(4).
000052             20  EMI-FILL            PIC X.
000053             20  EMI-SEVERITY        PIC X.
000054             20  FILLER              PIC X.
000055             20  EMI-ERROR-TEXT.
000056                 24  EMI-TEXT-VARIABLE   PIC X(10).
000057                 24  FILLER          PIC X(55).
000058     12  EMI-SEVERITY-SAVE           PIC X.
000059         88  EMI-NOTE                    VALUE 'N'.
000060         88  EMI-WARNING                 VALUE 'W'.
000061         88  EMI-FORCABLE                VALUE 'F'.
000062         88  EMI-FATAL                   VALUE 'X'.
000063     12  EMI-MESSAGE-FLAG            PIC X.
000064         88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
000065         88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
000066     12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
000067     12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
000068         88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
000069         88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
000070         88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
000071     12  emi-claim-no                pic x(7).
000072     12  emi-claim-type              pic x(6).
000073     12  FILLER                      PIC X(124)  VALUE SPACES.
000074     12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
000075     12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
000076     12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
000077     12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *<<((file: ELCEMIB))
000598 EJECT
000599 01  WS-PASS-631.
000600     12  WS-PASS-WORK-AREA         PIC X(384).
000601     12  WS-PASS-PROGRAM-WORK-AREA PIC X(640).
000602     12  FILLER REDEFINES WS-PASS-PROGRAM-WORK-AREA.
000603*        COPY ELC631PI.
      *>>((file: ELC631PI))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELC631PI                            *
000005*                            VMOD=2.012                          *
000006*                                                                *
000007*    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
000008*    REVIEW AND CORRRECTION SUB-SYSTEM.  ANY CHANGES WILL        *
000009*    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
000010*                                                                *
000011*    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
000012*    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
000013*    BETWEEN PROGRAMS.                                           *
000014*                                                                *
000015*    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
000016*                                                                *
000017*               EL631 - EL6311 - EL6312 - EL6313                 *
000018*                                                                *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 021414    2003053000001  PEMA  changes for auto chk request
000028*         CR2020061200002  TANA  ADD Cancel Reason Code
000029******************************************************************
000030
000031         16  PI-631-DATA.
000032             20  PI-ERPNDB-KEY.
000033                 24  PI-PB-COMPANY-CD     PIC X.
000034                 24  PI-PB-ENTRY-BATCH    PIC X(6).
000035                 24  PI-PB-BATCH-SEQ-NO   PIC S9(4) COMP.
000036                 24  PI-PB-BATCH-CHG-SEQ-NO PIC S9(4) COMP.
000037
000038             20  PI-ERPNDB-ALT-KEY.
000039                 24  PI-PB-COMPANY-CD-A1  PIC X.
000040                 24  PI-PB-CARRIER        PIC X.
000041                 24  PI-PB-GROUPING       PIC X(6).
000042                 24  PI-PB-STATE          PIC XX.
000043                 24  PI-PB-ACCOUNT        PIC X(10).
000044                 24  PI-PB-CERT-EFF-DT    PIC XX.
000045                 24  PI-PB-CERT-NO.
000046                     28  PI-PB-CERT-PRIME PIC X(10).
000047                     28  PI-PB-CERT-SFX   PIC X.
000048                 24  PI-PB-ALT-CHG-SEQ-NO PIC S9(4) COMP.
000049                 24  PI-PB-RECORD-TYPE    PIC X.
000050
000051             20  PI-ERPNDB-CSR-KEY.
000052                 24  PI-PB-CSR-COMPANY-CD-A2  PIC X.
000053                 24  PI-PB-CSR-ID             PIC X(4).
000054                 24  PI-PB-CSR-ENTRY-BATCH    PIC X(6).
000055                 24  PI-PB-CSR-BTCH-SEQ-NO    PIC S9(4) COMP.
000056                 24  PI-PB-CSR-BTCH-CHG-SEQ-NO PIC S9(4) COMP.
000057
000058             20  PI-BROWSE-TYPE               PIC X.
000059                 88  PI-FILE-BROWSE             VALUE ' '.
000060                 88  PI-PRIMARY-BROWSE          VALUE '1'.
000061                 88  PI-ALTERNATE-BROWSE        VALUE '2'.
000062                 88  PI-PRIMARY-WITH-SELECT     VALUE '3'.
000063                 88  PI-CSR-BROWSE              VALUE '4'.
000064
000065             20  PI-MAINT-FUNCTION            PIC X.
000066                 88  PI-ADD-FUNCTION            VALUE 'A'.
000067                 88  PI-BROWSE-FUNCTION         VALUE 'B'.
000068                 88  PI-CHANGE-FUNCTION         VALUE 'C'.
000069                 88  PI-DELETE-FUNCTION         VALUE 'D'.
000070                 88  PI-SHOW-FUNCTION           VALUE 'S'.
000071                 88  PI-PF5-FUNCTION            VALUE '5'.
000072                 88  PI-PF6-FUNCTION            VALUE '6'.
000073
000074             20  PI-FILE-SWITCHES.
000075                 24  PI-ALL-ISSUES-SW         PIC X.
000076                     88  ALL-ISSUES             VALUE 'Y'.
000077                 24  PI-ALL-CANCELS-SW        PIC X.
000078                     88  ALL-CANCELS            VALUE 'Y'.
000079                 24  PI-ISSUES-IN-ERROR-SW    PIC X.
000080                     88  ISSUES-IN-ERROR        VALUE 'Y'.
000081                 24  PI-CANCELS-IN-ERROR-SW   PIC X.
000082                     88  CANCEL-IN-ERROR        VALUE 'Y'.
000083                 24  PI-ONLY-BATCH-HEADERS-SW PIC X.
000084                     88  ONLY-BATCH-HEADERS     VALUE 'Y'.
000085                 24  PI-ALL-OUT-OF-BAL-SW     PIC X.
000086                     88  ALL-OUT-OF-BAL         VALUE 'Y'.
000087                 24  PI-HOLD-REC-SW           PIC X.
000088                     88  DISPLAY-HOLD-RECORDS   VALUE 'Y'.
000089                 24  PI-CHANGE-REC-SW         PIC X.
000090                     88  DISPLAY-CHANGE-RECORDS VALUE 'Y'.
000091                 24  PI-CHK-REQ-REC-SW        PIC X.
000092                     88  DISPLAY-CHK-REQ-RECORDS VALUE 'Y'.
000093                 24  PI-ISSUE-WARNING-SW      PIC X.
000094                     88  ISSUE-WITH-WARNING     VALUE 'Y'.
000095                 24  PI-CANCEL-WARNING-SW     PIC X.
000096                     88  CANCEL-WITH-WARNING    VALUE 'Y'.
000097             20  PI-DISPLAY-SCREEN-SW         PIC X.
000098                     88  PI-DISPLAY-SCREEN      VALUE 'Y'.
000099             20  PI-ORIGINAL-BATCH-SW         PIC X.
000100                     88  PI-DISPLAY-ORIGINAL-BATCH VALUE 'Y'.
000101
000102             20  PI-MAP-NAME                  PIC X(8).
000103
000104             20  PI-CURSOR                    PIC S9(4) COMP.
000105
000106             20  PI-PREV-ALT-KEY              PIC X(36).
000107             20  PI-PREV-CSR-KEY              PIC X(15).
000108             20  PI-PREV-KEY.
000109                 24  PI-PREV-COMPANY-CD       PIC X.
000110                 24  PI-PREV-BATCH            PIC X(6).
000111                 24  PI-PREV-SEQ-NO           PIC S9(4) COMP.
000112                 24  PI-PREV-CHG-SEQ-NO       PIC S9(4) COMP.
000113             20  PI-PREV-CONTROL-PRIMARY      PIC X(11).
000114             20  PI-BROWSE-SW                 PIC X.
000115                 88  PI-GOOD-BROWSE             VALUE 'Y'.
000116                 88  PI-NO-PB-RECS-FOUND        VALUE '9'.
000117             20  PI-SV-CARRIER                PIC X.
000118             20  PI-SV-GROUPING               PIC X(6).
000119             20  PI-SV-STATE                  PIC XX.
000120             20  PI-EDIT-SW                   PIC X.
000121             20  PI-DISPLAY-SW                PIC XX.
000122                 88 PI-DISPLAY-LIFE        VALUE 'LF'.
000123                 88 PI-DISPLAY-AH          VALUE 'AH'.
000124             20  PI-CRITERIA-DATA             PIC X(350).
000125             20  PI-BMODE                     PIC X.
000126             20  PI-BPMTAMT                   PIC S9(7)V99 COMP-3.
000127             20  PI-BPMTS                     PIC S999     COMP-3.
000128             20  PI-BTYPE                     PIC XXX OCCURS 2.
000129             20  PI-HIGH-SEQ-NO               PIC S9(4) COMP.
000130             20  PI-CSR-SESSION-SW            PIC X.
000131                 88  CSR-EDIT-SESSION           VALUE 'Y'.
000132             20  PI-ERRORS-SW                 PIC X.
000133                 88  FATAL-ERRORS               VALUE 'X'.
000134*                88  FATAL-OR-UNFORCED          VALUE 'X'.
000135             20  pi-unforced-sw               pic x.
000136                 88  unforced-errors            value 'X'.
000137             20  PI-CANCEL-REASON-631         PIC X.
000138             20  FILLER                       PIC X(3).
000139
      *<<((file: ELC631PI))
000604         16  FILLER                PIC X(94).
000605
000606
000607*                            COPY ELCINTF.
      *>>((file: ELCINTF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCINTF.                            *
000005*                            VMOD=2.017                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
000008*                                                                *
000009*       LENGTH = 1024                                            *
000010*                                                                *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000020******************************************************************
000021 01  PROGRAM-INTERFACE-BLOCK.
000022     12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
000023     12  PI-CALLING-PROGRAM              PIC X(8).
000024     12  PI-SAVED-PROGRAM-1              PIC X(8).
000025     12  PI-SAVED-PROGRAM-2              PIC X(8).
000026     12  PI-SAVED-PROGRAM-3              PIC X(8).
000027     12  PI-SAVED-PROGRAM-4              PIC X(8).
000028     12  PI-SAVED-PROGRAM-5              PIC X(8).
000029     12  PI-SAVED-PROGRAM-6              PIC X(8).
000030     12  PI-RETURN-TO-PROGRAM            PIC X(8).
000031     12  PI-COMPANY-ID                   PIC XXX.
000032     12  PI-COMPANY-CD                   PIC X.
000033
000034     12  PI-COMPANY-PASSWORD             PIC X(8).
000035
000036     12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
000037
000038     12  PI-CONTROL-IN-PROGRESS.
000039         16  PI-CARRIER                  PIC X.
000040         16  PI-GROUPING                 PIC X(6).
000041         16  PI-STATE                    PIC XX.
000042         16  PI-ACCOUNT                  PIC X(10).
000043         16  PI-PRODUCER REDEFINES PI-ACCOUNT
000044                                         PIC X(10).
000045         16  PI-CLAIM-CERT-GRP.
000046             20  PI-CLAIM-NO             PIC X(7).
000047             20  PI-CERT-NO.
000048                 25  PI-CERT-PRIME       PIC X(10).
000049                 25  PI-CERT-SFX         PIC X.
000050             20  PI-CERT-EFF-DT          PIC XX.
000051         16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
000052             20  PI-PLAN-CODE            PIC X(2).
000053             20  PI-REVISION-NUMBER      PIC X(3).
000054             20  PI-PLAN-EFF-DT          PIC X(2).
000055             20  PI-PLAN-EXP-DT          PIC X(2).
000056             20  FILLER                  PIC X(11).
000057         16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
000058             20  PI-OE-REFERENCE-1.
000059                 25  PI-OE-REF-1-PRIME   PIC X(18).
000060                 25  PI-OE-REF-1-SUFF    PIC XX.
000061
000062     12  PI-SESSION-IN-PROGRESS          PIC X.
000063         88  CLAIM-SESSION                   VALUE '1'.
000064         88  CREDIT-SESSION                  VALUE '2'.
000065         88  WARRANTY-SESSION                VALUE '3'.
000066         88  MORTGAGE-SESSION                VALUE '4'.
000067         88  GENERAL-LEDGER-SESSION          VALUE '5'.
000068
000069
000070*THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
000071
000072     12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
000073     12  PI-ORIGINAL-COMPANY-CD          PIC X.
000074
000075     12  PI-CREDIT-USER                  PIC X.
000076         88  PI-NOT-CREDIT-USER              VALUE 'N'.
000077         88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
000078
000079     12  PI-CLAIM-USER                   PIC X.
000080         88  PI-NOT-CLAIM-USER               VALUE 'N'.
000081         88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
000082
000083     12  PI-PROCESSOR-SYS-ACCESS         PIC X.
000084         88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
000085         88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
000086         88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
000087         88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
000088         88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
000089
000090     12  PI-PROCESSOR-ID                 PIC X(4).
000091
000092     12  PI-PROCESSOR-PASSWORD           PIC X(11).
000093
000094     12  PI-MEMBER-CAPTION               PIC X(10).
000095
000096     12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
000097         88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
000098
000099     12  PI-LIFE-OVERRIDE-L1             PIC X.
000100     12  PI-LIFE-OVERRIDE-L2             PIC XX.
000101     12  PI-LIFE-OVERRIDE-L6             PIC X(6).
000102     12  PI-LIFE-OVERRIDE-L12            PIC X(12).
000103
000104     12  PI-AH-OVERRIDE-L1               PIC X.
000105     12  PI-AH-OVERRIDE-L2               PIC XX.
000106     12  PI-AH-OVERRIDE-L6               PIC X(6).
000107     12  PI-AH-OVERRIDE-L12              PIC X(12).
000108
000109     12  PI-NEW-SYSTEM                   PIC X(2).
000110
000111     12  PI-PRIMARY-CERT-NO              PIC X(11).
000112     12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
000113         88  PI-USES-PAID-TO                 VALUE '1'.
000114     12  PI-CRDTCRD-SYSTEM.
000115         16  PI-CRDTCRD-USER             PIC X.
000116             88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
000117             88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
000118         16  PI-CC-MONTH-END-DT          PIC XX.
000119     12  PI-PROCESSOR-PRINTER            PIC X(4).
000120
000121     12  PI-OE-REFERENCE-2.
000122         16  PI-OE-REF-2-PRIME           PIC X(10).
000123         16  PI-OE-REF-2-SUFF            PIC X.
000124
000125     12  PI-REM-TRM-CALC-OPTION          PIC X.
000126
000127     12  PI-LANGUAGE-TYPE                PIC X.
000128             88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
000129             88  PI-LANGUAGE-IS-FR           VALUE 'F'.
000130             88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
000131
000132     12  PI-POLICY-LINKAGE-IND           PIC X.
000133         88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
000134         88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
000135                                                   LOW-VALUES.
000136
000137     12  PI-ALT-DMD-PRT-ID               PIC X(4).
000138     12  PI-CLAIM-PW-SESSION             PIC X(1).
000139         88  PI-CLAIM-CREDIT                 VALUE '1'.
000140         88  PI-CLAIM-CONVEN                 VALUE '2'.
000141
000142     12  PI-PROCESSOR-CSR-IND            PIC X.
000143         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
000144         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
000145
000146     12  FILLER                          PIC X(3).
000147
000148     12  PI-SYSTEM-LEVEL                 PIC X(145).
000149
000150     12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
000151         PI-SYSTEM-LEVEL.
000152
000153         16  PI-ENTRY-CODES.
000154             20  PI-ENTRY-CD-1           PIC X.
000155             20  PI-ENTRY-CD-2           PIC X.
000156
000157         16  PI-RETURN-CODES.
000158             20  PI-RETURN-CD-1          PIC X.
000159             20  PI-RETURN-CD-2          PIC X.
000160
000161         16  PI-UPDATE-STATUS-SAVE.
000162             20  PI-UPDATE-BY            PIC X(4).
000163             20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
000164
000165         16  PI-LOWER-CASE-LETTERS       PIC X.
000166             88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
000167
000168*        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
000169*            88  CLAIM-NO-UNIQUE             VALUE '1'.
000170*            88  CARRIER-CLM-CNTL            VALUE '2'.
000171
000172         16  PI-CERT-ACCESS-CONTROL      PIC X.
000173             88  ST-ACCNT-CNTL               VALUE ' '.
000174             88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000175             88  CARR-ST-ACCNT-CNTL          VALUE '2'.
000176             88  ACCNT-CNTL                  VALUE '3'.
000177             88  CARR-ACCNT-CNTL             VALUE '4'.
000178
000179         16  PI-PROCESSOR-CAP-LIST.
000180             20  PI-SYSTEM-CONTROLS.
000181                24 PI-SYSTEM-DISPLAY     PIC X.
000182                 88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
000183                24 PI-SYSTEM-MODIFY      PIC X.
000184                 88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
000185             20  FILLER                  PIC XX.
000186             20  PI-DISPLAY-CAP          PIC X.
000187                 88  DISPLAY-CAP             VALUE 'Y'.
000188             20  PI-MODIFY-CAP           PIC X.
000189                 88  MODIFY-CAP              VALUE 'Y'.
000190             20  PI-MSG-AT-LOGON-CAP     PIC X.
000191                 88  MSG-AT-LOGON-CAP        VALUE 'Y'.
000192             20  PI-FORCE-CAP            PIC X.
000193                 88  FORCE-CAP               VALUE 'Y'.
000194
000195         16  PI-PROGRAM-CONTROLS.
000196             20  PI-PGM-PRINT-OPT        PIC X.
000197             20  PI-PGM-FORMAT-OPT       PIC X.
000198             20  PI-PGM-PROCESS-OPT      PIC X.
000199             20  PI-PGM-TOTALS-OPT       PIC X.
000200
000201         16  PI-HELP-INTERFACE.
000202             20  PI-LAST-ERROR-NO        PIC X(4).
000203             20  PI-CURRENT-SCREEN-NO    PIC X(4).
000204
000205         16  PI-CARRIER-CONTROL-LEVEL    PIC X.
000206             88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
000207
000208         16  PI-CR-CONTROL-IN-PROGRESS.
000209             20  PI-CR-CARRIER           PIC X.
000210             20  PI-CR-GROUPING          PIC X(6).
000211             20  PI-CR-STATE             PIC XX.
000212             20  PI-CR-ACCOUNT           PIC X(10).
000213             20  PI-CR-FIN-RESP          PIC X(10).
000214             20  PI-CR-TYPE              PIC X.
000215
000216         16  PI-CR-BATCH-NUMBER          PIC X(6).
000217
000218         16  PI-CR-MONTH-END-DT          PIC XX.
000219
000220         16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
000221             88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
000222             88  PI-ZERO-CARRIER             VALUE '1'.
000223             88  PI-ZERO-GROUPING            VALUE '2'.
000224             88  PI-ZERO-CAR-GROUP           VALUE '3'.
000225
000226         16  PI-CARRIER-SECURITY         PIC X.
000227             88  PI-NO-CARRIER-SECURITY      VALUE ' '.
000228
000229         16  PI-ACCOUNT-SECURITY         PIC X(10).
000230             88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
000231             88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
000232
000233         16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
000234             20  PI-ACCESS-CODE          OCCURS 10 TIMES
000235                                         INDEXED BY PI-ACCESS-NDX
000236                                         PIC X.
000237
000238         16  PI-GA-BILLING-CONTROL       PIC X.
000239             88  PI-GA-BILLING               VALUE '1'.
000240
000241         16  PI-MAIL-PROCESSING          PIC X.
000242             88  PI-MAIL-YES                 VALUE 'Y'.
000243
000244         16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
000245
000246         16  PI-AR-SYSTEM.
000247             20  PI-AR-PROCESSING-CNTL   PIC X.
000248                 88  PI-AR-PROCESSING        VALUE 'Y'.
000249             20  PI-AR-SUMMARY-CODE      PIC X(6).
000250             20  PI-AR-MONTH-END-DT      PIC XX.
000251
000252         16  PI-MP-SYSTEM.
000253             20  PI-MORTGAGE-USER            PIC X.
000254                 88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
000255                 88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
000256             20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
000257                 88  PI-MP-ST-PROD-CNTL              VALUE ' '.
000258                 88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
000259                 88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
000260                 88  PI-MP-PROD-CNTL                 VALUE '3'.
000261                 88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
000262             20  PI-MP-MONTH-END-DT          PIC XX.
000263             20  PI-MP-REFERENCE-NO.
000264                 24  PI-MP-REFERENCE-PRIME   PIC X(18).
000265                 24  PI-MP-REFERENCE-SFX     PIC XX.
000266
000267         16  PI-LABEL-CONTROL            PIC X(01).
000268             88  PI-CREATE-LABELS                    VALUE 'Y'.
000269             88  PI-BYPASS-LABELS                    VALUE 'N'.
000270
000271         16  PI-BILL-GROUPING-CODE       PIC X(01).
000272             88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
000273
000274         16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
000275             88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
000276             88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
000277
000278         16  FILLER                      PIC X(14).
000279
000280     12  PI-PROGRAM-WORK-AREA            PIC X(640).
000281******************************************************************
      *<<((file: ELCINTF))
000608     12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
000609         16  FILLER                PIC  X(500).
000610         16  PI-PEND-SW            PIC  X(01).
000611         16  PI-LF-CANCEL-DATE     PIC  X(02).
000612         16  PI-AH-CANCEL-DATE     PIC  X(02).
000613         16  PI-LF-CANCEL-DATE-ED  PIC  X(08).
000614         16  PI-AH-CANCEL-DATE-ED  PIC  X(08).
000615         16  PI-EARNING-METHOD-LF  PIC  X.
000616         16  PI-EARNING-METHOD-AH  PIC  X.
000617         16  PI-LF-REFUND-AMT      PIC  S9(7)V99 COMP-3.
000618         16  PI-LF-REFUND-METH     PIC  X.
000619         16  PI-AH-REFUND-AMT      PIC  S9(7)V99 COMP-3.
000620         16  PI-AH-REFUND-METH     PIC  X.
000621         16  PI-TOTAL-REFUND-AMT   PIC  S9(7)V99 COMP-3.
000622         16  PI-MODIFY-CERT-CAP    PIC  X.
000623             88 MODIFY-CERT-CAP    VALUE 'Y'.
000624         16  PI-CLP-YN             PIC X.
000625         16  PI-CANCEL-REASON      PIC X.
000626         16  PI-PF10-OK            PIC X.
000627         16  FILLER                PIC  X(96).
000628 EJECT
000629*                            COPY ELCAID.
      *>>((file: ELCAID))
000001******************************************************************
000002*                                                                *
000003*                            ELCAID.                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
000007*                                                                *
000008*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
000009*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
000010******************************************************************
000011
000012 01  DFHAID.
000013   02  DFHNULL   PIC  X  VALUE  ' '.
000014   02  DFHENTER  PIC  X  VALUE  QUOTE.
000015   02  DFHCLEAR  PIC  X  VALUE  '_'.
000016   02  DFHPEN    PIC  X  VALUE  '='.
000017   02  DFHOPID   PIC  X  VALUE  'W'.
000018   02  DFHPA1    PIC  X  VALUE  '%'.
000019   02  DFHPA2    PIC  X  VALUE  '>'.
000020   02  DFHPA3    PIC  X  VALUE  ','.
000021   02  DFHPF1    PIC  X  VALUE  '1'.
000022   02  DFHPF2    PIC  X  VALUE  '2'.
000023   02  DFHPF3    PIC  X  VALUE  '3'.
000024   02  DFHPF4    PIC  X  VALUE  '4'.
000025   02  DFHPF5    PIC  X  VALUE  '5'.
000026   02  DFHPF6    PIC  X  VALUE  '6'.
000027   02  DFHPF7    PIC  X  VALUE  '7'.
000028   02  DFHPF8    PIC  X  VALUE  '8'.
000029   02  DFHPF9    PIC  X  VALUE  '9'.
000030   02  DFHPF10   PIC  X  VALUE  ':'.
000031   02  DFHPF11   PIC  X  VALUE  '#'.
000032   02  DFHPF12   PIC  X  VALUE  '@'.
000033   02  DFHPF13   PIC  X  VALUE  'A'.
000034   02  DFHPF14   PIC  X  VALUE  'B'.
000035   02  DFHPF15   PIC  X  VALUE  'C'.
000036   02  DFHPF16   PIC  X  VALUE  'D'.
000037   02  DFHPF17   PIC  X  VALUE  'E'.
000038   02  DFHPF18   PIC  X  VALUE  'F'.
000039   02  DFHPF19   PIC  X  VALUE  'G'.
000040   02  DFHPF20   PIC  X  VALUE  'H'.
000041   02  DFHPF21   PIC  X  VALUE  'I'.
000042*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
000043   02  DFHPF22   PIC  X  VALUE  '['.
000044   02  DFHPF23   PIC  X  VALUE  '.'.
000045   02  DFHPF24   PIC  X  VALUE  '<'.
000046   02  DFHMSRE   PIC  X  VALUE  'X'.
000047   02  DFHSTRF   PIC  X  VALUE  'h'.
000048   02  DFHTRIG   PIC  X  VALUE  '"'.
      *<<((file: ELCAID))
000630
000631 01  FILLER  REDEFINES  DFHAID.
000632     12  FILLER                  PIC  X(08).
000633     12  PF-VALUES               PIC  X(01)      OCCURS 24 TIMES.
000634 EJECT
000635*                            COPY EL1278S.
      *>>((file: EL1278S))
000001 01  EL127HI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  HDATEL PIC S9(0004) COMP.
000005     05  HDATEF PIC  X(0001).
000006     05  FILLER REDEFINES HDATEF.
000007         10  HDATEA PIC  X(0001).
000008     05  HDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  HTIMEL PIC S9(0004) COMP.
000011     05  HTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES HTIMEF.
000013         10  HTIMEA PIC  X(0001).
000014     05  HTIMEI PIC  999V99.
000015*    -------------------------------
000016     05  CMPNYIDL PIC S9(0004) COMP.
000017     05  CMPNYIDF PIC  X(0001).
000018     05  FILLER REDEFINES CMPNYIDF.
000019         10  CMPNYIDA PIC  X(0001).
000020     05  CMPNYIDI PIC  X(0003).
000021*    -------------------------------
000022     05  USERIDL PIC S9(0004) COMP.
000023     05  USERIDF PIC  X(0001).
000024     05  FILLER REDEFINES USERIDF.
000025         10  USERIDA PIC  X(0001).
000026     05  USERIDI PIC  X(0004).
000027*    -------------------------------
000028     05  HCARIERL PIC S9(0004) COMP.
000029     05  HCARIERF PIC  X(0001).
000030     05  FILLER REDEFINES HCARIERF.
000031         10  HCARIERA PIC  X(0001).
000032     05  HCARIERI PIC  X(0001).
000033*    -------------------------------
000034     05  HGROUPL PIC S9(0004) COMP.
000035     05  HGROUPF PIC  X(0001).
000036     05  FILLER REDEFINES HGROUPF.
000037         10  HGROUPA PIC  X(0001).
000038     05  HGROUPI PIC  X(0006).
000039*    -------------------------------
000040     05  HSTATEL PIC S9(0004) COMP.
000041     05  HSTATEF PIC  X(0001).
000042     05  FILLER REDEFINES HSTATEF.
000043         10  HSTATEA PIC  X(0001).
000044     05  HSTATEI PIC  X(0002).
000045*    -------------------------------
000046     05  HACCTNOL PIC S9(0004) COMP.
000047     05  HACCTNOF PIC  X(0001).
000048     05  FILLER REDEFINES HACCTNOF.
000049         10  HACCTNOA PIC  X(0001).
000050     05  HACCTNOI PIC  X(0010).
000051*    -------------------------------
000052     05  HEFFDTL PIC S9(0004) COMP.
000053     05  HEFFDTF PIC  X(0001).
000054     05  FILLER REDEFINES HEFFDTF.
000055         10  HEFFDTA PIC  X(0001).
000056     05  HEFFDTI PIC  X(0008).
000057*    -------------------------------
000058     05  HCERTNOL PIC S9(0004) COMP.
000059     05  HCERTNOF PIC  X(0001).
000060     05  FILLER REDEFINES HCERTNOF.
000061         10  HCERTNOA PIC  X(0001).
000062     05  HCERTNOI PIC  X(0010).
000063*    -------------------------------
000064     05  HCRTSFXL PIC S9(0004) COMP.
000065     05  HCRTSFXF PIC  X(0001).
000066     05  FILLER REDEFINES HCRTSFXF.
000067         10  HCRTSFXA PIC  X(0001).
000068     05  HCRTSFXI PIC  X(0001).
000069*    -------------------------------
000070     05  HLNAMEL PIC S9(0004) COMP.
000071     05  HLNAMEF PIC  X(0001).
000072     05  FILLER REDEFINES HLNAMEF.
000073         10  HLNAMEA PIC  X(0001).
000074     05  HLNAMEI PIC  X(0015).
000075*    -------------------------------
000076     05  HFNAMEL PIC S9(0004) COMP.
000077     05  HFNAMEF PIC  X(0001).
000078     05  FILLER REDEFINES HFNAMEF.
000079         10  HFNAMEA PIC  X(0001).
000080     05  HFNAMEI PIC  X(0010).
000081*    -------------------------------
000082     05  HINITL PIC S9(0004) COMP.
000083     05  HINITF PIC  X(0001).
000084     05  FILLER REDEFINES HINITF.
000085         10  HINITA PIC  X(0001).
000086     05  HINITI PIC  X(0001).
000087*    -------------------------------
000088     05  LCLMDTHL PIC S9(0004) COMP.
000089     05  LCLMDTHF PIC  X(0001).
000090     05  FILLER REDEFINES LCLMDTHF.
000091         10  LCLMDTHA PIC  X(0001).
000092     05  LCLMDTHI PIC  X(0022).
000093*    -------------------------------
000094     05  LCLMDTL PIC S9(0004) COMP.
000095     05  LCLMDTF PIC  X(0001).
000096     05  FILLER REDEFINES LCLMDTF.
000097         10  LCLMDTA PIC  X(0001).
000098     05  LCLMDTI PIC  X(0008).
000099*    -------------------------------
000100     05  HPCYNL PIC S9(0004) COMP.
000101     05  HPCYNF PIC  X(0001).
000102     05  FILLER REDEFINES HPCYNF.
000103         10  HPCYNA PIC  X(0001).
000104     05  HPCYNI PIC  X(0001).
000105*    -------------------------------
000106     05  ACLMDTHL PIC S9(0004) COMP.
000107     05  ACLMDTHF PIC  X(0001).
000108     05  FILLER REDEFINES ACLMDTHF.
000109         10  ACLMDTHA PIC  X(0001).
000110     05  ACLMDTHI PIC  X(0023).
000111*    -------------------------------
000112     05  ACLMDTL PIC S9(0004) COMP.
000113     05  ACLMDTF PIC  X(0001).
000114     05  FILLER REDEFINES ACLMDTF.
000115         10  ACLMDTA PIC  X(0001).
000116     05  ACLMDTI PIC  X(0008).
000117*    -------------------------------
000118     05  PMTHDL PIC S9(0004) COMP.
000119     05  PMTHDF PIC  X(0001).
000120     05  FILLER REDEFINES PMTHDF.
000121         10  PMTHDA PIC  X(0001).
000122     05  PMTHDI PIC  X(0010).
000123*    -------------------------------
000124     05  CNCDTHDL PIC S9(0004) COMP.
000125     05  CNCDTHDF PIC  X(0001).
000126     05  FILLER REDEFINES CNCDTHDF.
000127         10  CNCDTHDA PIC  X(0001).
000128     05  CNCDTHDI PIC  X(0007).
000129*    -------------------------------
000130     05  HLKINDL PIC S9(0004) COMP.
000131     05  HLKINDF PIC  X(0001).
000132     05  FILLER REDEFINES HLKINDF.
000133         10  HLKINDA PIC  X(0001).
000134     05  HLKINDI PIC  X(0002).
000135*    -------------------------------
000136     05  HLCDL PIC S9(0004) COMP.
000137     05  HLCDF PIC  X(0001).
000138     05  FILLER REDEFINES HLCDF.
000139         10  HLCDA PIC  X(0001).
000140     05  HLCDI PIC  X(0002).
000141*    -------------------------------
000142     05  HLEDESCL PIC S9(0004) COMP.
000143     05  HLEDESCF PIC  X(0001).
000144     05  FILLER REDEFINES HLEDESCF.
000145         10  HLEDESCA PIC  X(0001).
000146     05  HLEDESCI PIC  X(0003).
000147*    -------------------------------
000148     05  HLTERML PIC S9(0004) COMP.
000149     05  HLTERMF PIC  X(0001).
000150     05  FILLER REDEFINES HLTERMF.
000151         10  HLTERMA PIC  X(0001).
000152     05  HLTERMI PIC  9(3).
000153*    -------------------------------
000154     05  HLREML PIC S9(0004) COMP.
000155     05  HLREMF PIC  X(0001).
000156     05  FILLER REDEFINES HLREMF.
000157         10  HLREMA PIC  X(0001).
000158     05  HLREMI PIC  X(0003).
000159*    -------------------------------
000160     05  HLPREML PIC S9(0004) COMP.
000161     05  HLPREMF PIC  X(0001).
000162     05  FILLER REDEFINES HLPREMF.
000163         10  HLPREMA PIC  X(0001).
000164     05  HLPREMI PIC  X(0011).
000165*    -------------------------------
000166     05  HLITDRL PIC S9(0004) COMP.
000167     05  HLITDRF PIC  X(0001).
000168     05  FILLER REDEFINES HLITDRF.
000169         10  HLITDRA PIC  X(0001).
000170     05  HLITDRI PIC  X(0011).
000171*    -------------------------------
000172     05  HLREFNDL PIC S9(0004) COMP.
000173     05  HLREFNDF PIC  X(0001).
000174     05  FILLER REDEFINES HLREFNDF.
000175         10  HLREFNDA PIC  X(0001).
000176     05  HLREFNDI PIC  X(0011).
000177*    -------------------------------
000178     05  HLCANCL PIC S9(0004) COMP.
000179     05  HLCANCF PIC  X(0001).
000180     05  FILLER REDEFINES HLCANCF.
000181         10  HLCANCA PIC  X(0001).
000182     05  HLCANCI PIC  X(0008).
000183*    -------------------------------
000184     05  HLCAL1L PIC S9(0004) COMP.
000185     05  HLCAL1F PIC  X(0001).
000186     05  FILLER REDEFINES HLCAL1F.
000187         10  HLCAL1A PIC  X(0001).
000188     05  HLCAL1I PIC  X(0001).
000189*    -------------------------------
000190     05  HLCALCL PIC S9(0004) COMP.
000191     05  HLCALCF PIC  X(0001).
000192     05  FILLER REDEFINES HLCALCF.
000193         10  HLCALCA PIC  X(0001).
000194     05  HLCALCI PIC  X(0012).
000195*    -------------------------------
000196     05  HLCPCTL PIC S9(0004) COMP.
000197     05  HLCPCTF PIC  X(0001).
000198     05  FILLER REDEFINES HLCPCTF.
000199         10  HLCPCTA PIC  X(0001).
000200     05  HLCPCTI PIC  X(0006).
000201*    -------------------------------
000202     05  HLCAMTL PIC S9(0004) COMP.
000203     05  HLCAMTF PIC  X(0001).
000204     05  FILLER REDEFINES HLCAMTF.
000205         10  HLCAMTA PIC  X(0001).
000206     05  HLCAMTI PIC  X(0009).
000207*    -------------------------------
000208     05  HAKINDL PIC S9(0004) COMP.
000209     05  HAKINDF PIC  X(0001).
000210     05  FILLER REDEFINES HAKINDF.
000211         10  HAKINDA PIC  X(0001).
000212     05  HAKINDI PIC  X(0002).
000213*    -------------------------------
000214     05  HACDL PIC S9(0004) COMP.
000215     05  HACDF PIC  X(0001).
000216     05  FILLER REDEFINES HACDF.
000217         10  HACDA PIC  X(0001).
000218     05  HACDI PIC  X(0002).
000219*    -------------------------------
000220     05  HAEDESCL PIC S9(0004) COMP.
000221     05  HAEDESCF PIC  X(0001).
000222     05  FILLER REDEFINES HAEDESCF.
000223         10  HAEDESCA PIC  X(0001).
000224     05  HAEDESCI PIC  X(0003).
000225*    -------------------------------
000226     05  HATERML PIC S9(0004) COMP.
000227     05  HATERMF PIC  X(0001).
000228     05  FILLER REDEFINES HATERMF.
000229         10  HATERMA PIC  X(0001).
000230     05  HATERMI PIC  9(3).
000231*    -------------------------------
000232     05  HAREML PIC S9(0004) COMP.
000233     05  HAREMF PIC  X(0001).
000234     05  FILLER REDEFINES HAREMF.
000235         10  HAREMA PIC  X(0001).
000236     05  HAREMI PIC  X(0003).
000237*    -------------------------------
000238     05  HAPREML PIC S9(0004) COMP.
000239     05  HAPREMF PIC  X(0001).
000240     05  FILLER REDEFINES HAPREMF.
000241         10  HAPREMA PIC  X(0001).
000242     05  HAPREMI PIC  X(0011).
000243*    -------------------------------
000244     05  HAITDRL PIC S9(0004) COMP.
000245     05  HAITDRF PIC  X(0001).
000246     05  FILLER REDEFINES HAITDRF.
000247         10  HAITDRA PIC  X(0001).
000248     05  HAITDRI PIC  X(0011).
000249*    -------------------------------
000250     05  HAREFNDL PIC S9(0004) COMP.
000251     05  HAREFNDF PIC  X(0001).
000252     05  FILLER REDEFINES HAREFNDF.
000253         10  HAREFNDA PIC  X(0001).
000254     05  HAREFNDI PIC  X(0011).
000255*    -------------------------------
000256     05  HACANCL PIC S9(0004) COMP.
000257     05  HACANCF PIC  X(0001).
000258     05  FILLER REDEFINES HACANCF.
000259         10  HACANCA PIC  X(0001).
000260     05  HACANCI PIC  X(0008).
000261*    -------------------------------
000262     05  HACAL1L PIC S9(0004) COMP.
000263     05  HACAL1F PIC  X(0001).
000264     05  FILLER REDEFINES HACAL1F.
000265         10  HACAL1A PIC  X(0001).
000266     05  HACAL1I PIC  X(0001).
000267*    -------------------------------
000268     05  HACALCL PIC S9(0004) COMP.
000269     05  HACALCF PIC  X(0001).
000270     05  FILLER REDEFINES HACALCF.
000271         10  HACALCA PIC  X(0001).
000272     05  HACALCI PIC  X(0012).
000273*    -------------------------------
000274     05  HACPCTL PIC S9(0004) COMP.
000275     05  HACPCTF PIC  X(0001).
000276     05  FILLER REDEFINES HACPCTF.
000277         10  HACPCTA PIC  X(0001).
000278     05  HACPCTI PIC  X(0006).
000279*    -------------------------------
000280     05  HACAMTL PIC S9(0004) COMP.
000281     05  HACAMTF PIC  X(0001).
000282     05  FILLER REDEFINES HACAMTF.
000283         10  HACAMTA PIC  X(0001).
000284     05  HACAMTI PIC  X(0009).
000285*    -------------------------------
000286     05  CANFEEHL PIC S9(0004) COMP.
000287     05  CANFEEHF PIC  X(0001).
000288     05  FILLER REDEFINES CANFEEHF.
000289         10  CANFEEHA PIC  X(0001).
000290     05  CANFEEHI PIC  X(0012).
000291*    -------------------------------
000292     05  CANFEEL PIC S9(0004) COMP.
000293     05  CANFEEF PIC  X(0001).
000294     05  FILLER REDEFINES CANFEEF.
000295         10  CANFEEA PIC  X(0001).
000296     05  CANFEEI PIC  X(0006).
000297*    -------------------------------
000298     05  TOPREML PIC S9(0004) COMP.
000299     05  TOPREMF PIC  X(0001).
000300     05  FILLER REDEFINES TOPREMF.
000301         10  TOPREMA PIC  X(0001).
000302     05  TOPREMI PIC  X(0011).
000303*    -------------------------------
000304     05  TOITDRL PIC S9(0004) COMP.
000305     05  TOITDRF PIC  X(0001).
000306     05  FILLER REDEFINES TOITDRF.
000307         10  TOITDRA PIC  X(0001).
000308     05  TOITDRI PIC  X(0011).
000309*    -------------------------------
000310     05  TORFNDL PIC S9(0004) COMP.
000311     05  TORFNDF PIC  X(0001).
000312     05  FILLER REDEFINES TORFNDF.
000313         10  TORFNDA PIC  X(0001).
000314     05  TORFNDI PIC  X(0011).
000315*    -------------------------------
000316     05  CANREAHL PIC S9(0004) COMP.
000317     05  CANREAHF PIC  X(0001).
000318     05  FILLER REDEFINES CANREAHF.
000319         10  CANREAHA PIC  X(0001).
000320     05  CANREAHI PIC  X(0015).
000321*    -------------------------------
000322     05  CANREAL PIC S9(0004) COMP.
000323     05  CANREAF PIC  X(0001).
000324     05  FILLER REDEFINES CANREAF.
000325         10  CANREAA PIC  X(0001).
000326     05  CANREAI PIC  X(0001).
000327*    -------------------------------
000328     05  CLPYNHL PIC S9(0004) COMP.
000329     05  CLPYNHF PIC  X(0001).
000330     05  FILLER REDEFINES CLPYNHF.
000331         10  CLPYNHA PIC  X(0001).
000332     05  CLPYNHI PIC  X(0005).
000333*    -------------------------------
000334     05  CLPYNL PIC S9(0004) COMP.
000335     05  CLPYNF PIC  X(0001).
000336     05  FILLER REDEFINES CLPYNF.
000337         10  CLPYNA PIC  X(0001).
000338     05  CLPYNI PIC  X(0001).
000339*    -------------------------------
000340     05  REFDUEHL PIC S9(0004) COMP.
000341     05  REFDUEHF PIC  X(0001).
000342     05  FILLER REDEFINES REFDUEHF.
000343         10  REFDUEHA PIC  X(0001).
000344     05  REFDUEHI PIC  X(0018).
000345*    -------------------------------
000346     05  REFDUEL PIC S9(0004) COMP.
000347     05  REFDUEF PIC  X(0001).
000348     05  FILLER REDEFINES REFDUEF.
000349         10  REFDUEA PIC  X(0001).
000350     05  REFDUEI PIC  X(0010).
000351*    -------------------------------
000352     05  HLREDL PIC S9(0004) COMP.
000353     05  HLREDF PIC  X(0001).
000354     05  FILLER REDEFINES HLREDF.
000355         10  HLREDA PIC  X(0001).
000356     05  HLREDI PIC  X(0014).
000357*    -------------------------------
000358     05  FREMTRML PIC S9(0004) COMP.
000359     05  FREMTRMF PIC  X(0001).
000360     05  FILLER REDEFINES FREMTRMF.
000361         10  FREMTRMA PIC  X(0001).
000362     05  FREMTRMI PIC  X(0003).
000363*    -------------------------------
000364     05  HERMSG1L PIC S9(0004) COMP.
000365     05  HERMSG1F PIC  X(0001).
000366     05  FILLER REDEFINES HERMSG1F.
000367         10  HERMSG1A PIC  X(0001).
000368     05  HERMSG1I PIC  X(0079).
000369*    -------------------------------
000370     05  HERMSG2L PIC S9(0004) COMP.
000371     05  HERMSG2F PIC  X(0001).
000372     05  FILLER REDEFINES HERMSG2F.
000373         10  HERMSG2A PIC  X(0001).
000374     05  HERMSG2I PIC  X(0079).
000375*    -------------------------------
000376     05  HPFKEYL PIC S9(0004) COMP.
000377     05  HPFKEYF PIC  X(0001).
000378     05  FILLER REDEFINES HPFKEYF.
000379         10  HPFKEYA PIC  X(0001).
000380     05  HPFKEYI PIC  99.
000381*    -------------------------------
000382     05  PFKEY11L PIC S9(0004) COMP.
000383     05  PFKEY11F PIC  X(0001).
000384     05  FILLER REDEFINES PFKEY11F.
000385         10  PFKEY11A PIC  X(0001).
000386     05  PFKEY11I PIC  X(0014).
000387 01  EL127HO REDEFINES EL127HI.
000388     05  FILLER            PIC  X(0012).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  HDATEO PIC  X(0008).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  HTIMEO PIC  99.99.
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  CMPNYIDO PIC  X(0003).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  USERIDO PIC  X(0004).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  HCARIERO PIC  X(0001).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  HGROUPO PIC  X(0006).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  HSTATEO PIC  X(0002).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  HACCTNOO PIC  X(0010).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  HEFFDTO PIC  X(0008).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  HCERTNOO PIC  X(0010).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  HCRTSFXO PIC  X(0001).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  HLNAMEO PIC  X(0015).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  HFNAMEO PIC  X(0010).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  HINITO PIC  X(0001).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  LCLMDTHO PIC  X(0022).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  LCLMDTO PIC  X(0008).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  HPCYNO PIC  X(0001).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  ACLMDTHO PIC  X(0023).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  ACLMDTO PIC  X(0008).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  PMTHDO PIC  X(0010).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  CNCDTHDO PIC  X(0007).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  HLKINDO PIC  X(0002).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  HLCDO PIC  X(0002).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  HLEDESCO PIC  X(0003).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  HLTERMO PIC  ZZ9.
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  HLREMO PIC  ZZ9.
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  HLPREMO PIC  ZZZZ,ZZ9.99.
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  HLITDRO PIC  ZZZZ,ZZ9.99.
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  HLREFNDO PIC  ZZZZ,ZZ9.99.
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  HLCANCO PIC  X(0008).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  HLCAL1O PIC  X(0001).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  HLCALCO PIC  X(0012).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  HLCPCTO PIC  .99999.
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  HLCAMTO PIC  ZZ,ZZ9.99.
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  HAKINDO PIC  X(0002).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  HACDO PIC  X(0002).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  HAEDESCO PIC  X(0003).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  HATERMO PIC  ZZ9.
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  HAREMO PIC  ZZ9.
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  HAPREMO PIC  ZZZZ,ZZ9.99.
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  HAITDRO PIC  ZZZZ,ZZ9.99.
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  HAREFNDO PIC  ZZZZ,ZZ9.99.
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  HACANCO PIC  X(0008).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  HACAL1O PIC  X(0001).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  HACALCO PIC  X(0012).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  HACPCTO PIC  .99999.
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  HACAMTO PIC  ZZ,ZZ9.99.
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  CANFEEHO PIC  X(0012).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  CANFEEO PIC  ZZ9.99.
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  TOPREMO PIC  ZZZZ,ZZ9.99.
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  TOITDRO PIC  ZZZZ,ZZ9.99.
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  TORFNDO PIC  ZZZZ,ZZ9.99.
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  CANREAHO PIC  X(0015).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  CANREAO PIC  X(0001).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  CLPYNHO PIC  X(0005).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  CLPYNO PIC  X(0001).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  REFDUEHO PIC  X(0018).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  REFDUEO PIC  ZZZ,ZZ9.99.
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  HLREDO PIC  ZZZ,ZZZ,ZZ9.99.
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  FREMTRMO PIC  ZZ9.
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  HERMSG1O PIC  X(0079).
000572*    -------------------------------
000573     05  FILLER            PIC  X(0003).
000574     05  HERMSG2O PIC  X(0079).
000575*    -------------------------------
000576     05  FILLER            PIC  X(0003).
000577     05  HPFKEYO PIC  99.
000578*    -------------------------------
000579     05  FILLER            PIC  X(0003).
000580     05  PFKEY11O PIC  X(0014).
000581*    -------------------------------
      *<<((file: EL1278S))
000636 EJECT
000637*                            COPY ELCCALC.
      *>>((file: ELCCALC))
000001******************************************************************
000002*                                                                *
000003*                           ELCCALC.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.025                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
000008*                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
000009*                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
000010*                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
000011*                                                                *
000012*  PASSED TO ELRTRM                                              *
000013*  -----------------                                             *
000014*  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
000015*  ORIGINAL TERM                                                 *
000016*  BEGINNING DATE                                                *
000017*  ENDING DATE                                                   *
000018*  COMPANY I.D.                                                  *
000019*  ACCOUNT MASTER USER FIELD                                     *
000020*  PROCESS SWITCH (CANCEL, CLAIM)                                *
000021*  FREE LOOK DAYS                                                *
000022*                                                                *
000023*  RETURNED FROM ELRTRM                                          *
000024*  ---------------------                                         *
000025*  REMAINING TERM 1 - USED FOR EARNINGS                          *
000026*  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
000027*  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
000028*  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
000029*----------------------------------------------------------------*
000030*  PASSED TO ELRAMT                                              *
000031*  ----------------                                              *
000032*  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
000033*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000034*  ORIGINAL AMOUNT                                               *
000035*  ALTERNATE BENEFIT (BALLON)                                    *
000036*  A.P.R. - NET PAY ONLY                                         *
000037*  METHOD
000038*  PAYMENT FREQUENCY - FOR FARM PLAN                             *
000039*  COMPANY I.D.                                                  *
000040*  BENEFIT TYPE                                                  *
000041*                                                                *
000042*  RETURNED FROM ELRAMT                                          *
000043*  --------------------                                          *
000044*  REMAINING AMOUNT 1 - CURRENT                                  *
000045*  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
000046*  REMAINING AMOUNT FACTOR
000047*----------------------------------------------------------------*
000048*  PASSED TO ELRESV                                              *
000049*  -----------------                                             *
000050*  CERTIFICATE EFFECTIVE DATE                                    *
000051*  VALUATION DATE                                                *
000052*  PAID THRU DATE                                                *
000053*  BENEFIT                                                       *
000054*  INCURRED DATE                                                 *
000055*  REPORTED DATE                                                 *
000056*  ISSUE AGE                                                     *
000057*  TERM                                                          *
000058*  CDT PERCENT                                                   *
000059*  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
000060* *CLAIM TYPE (LIFE, A/H)                                        *
000061* *REMAINING BENEFIT (FROM ELRAMT)                               *
000062* *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
000063*                                                                *
000064*  RETURNED FROM ELRESV                                          *
000065*  --------------------                                          *
000066*  CDT TABLE USED                                                *
000067*  CDT FACTOR USED                                               *
000068*  PAY TO CURRENT RESERVE                                        *
000069*  I.B.N.R. - A/H ONLY                                           *
000070*  FUTURE (ACCRUED) AH ONLY                                      *
000071*----------------------------------------------------------------*
000072*  PASSED TO ELRATE                                              *
000073*  ----------------                                              *
000074*  CERT ISSUE DATE                                               *
000075*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000076*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000077*  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
000078*  STATE CODE (CLIENT DEFINED)                                   *
000079*  STATE CODE (STANDARD P.O. ABBRV)                              *
000080*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000081*  DEVIATION CODE                                                *
000082*  ISSUE AGE                                                     *
000083*  ORIGINAL BENEFIT AMOUNT                                       *
000084*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000085*  PROCESS TYPE (ISSUE OR CANCEL)                                *
000086*  BENEFIT KIND (LIFE OR A/H)                                    *
000087*  A.P.R.                                                        *
000088*  METHOD
000089*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000090*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000091*  COMPANY I.D. (3 CHARACTER)                                    *
000092*  BENEFIT CODE                                                  *
000093*  BENEFIT OVERRIDE CODE                                         *
000094*  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
000095*  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
000096*  JOINT INDICATOR (CSL ONLY)                                    *
000097*  FIRST PAYMENT DATE (CSL ONLY)                                 *
000098*  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
000099*                                                                *
000100*  RETURNED FROM ELRATE                                          *
000101*  --------------------                                          *
000102*  CALCULATED PREMIUM                                            *
000103*  PREMIUM RATE                                                  *
000104*  MORTALITY CODE                                                *
000105*  MAX ATTAINED AGE                                              *
000106*  MAX AGE                                                       *
000107*  MAX TERM                                                      *
000108*  MAX MONTHLY BENEFIT                                           *
000109*  MAX TOTAL BENIFIT                                             *
000110*  COMPOSITE RATE (OPEN-END ONLY)                                *
000111*----------------------------------------------------------------*
000112*  PASSED TO ELRFND                                              *
000113*  ----------------                                              *
000114*  CERT ISSUE DATE                                               *
000115*  REFUND DATE                                                   *
000116*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000117*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000118*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000119*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000120*  STATE CODE (CLIENT DEFINED)                                   *
000121*  STATE CODE (STANDARD P.O. ABBRV)                              *
000122*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000123*  DEVIATION CODE                                                *
000124*  ISSUE AGE                                                     *
000125*  ORIGINAL BENEFIT AMOUNT                                       *
000126*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000127*  PROCESS TYPE (CANCEL)                                         *
000128*  BENEFIT KIND (LIFE OR A/H)                                    *
000129*  A.P.R.                                                        *
000130*  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
000131*  RATING METHOD -  (CODE FROM BENEFIT)                          *
000132*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000133*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000134*  COMPANY I.D. (3 CHARACTER)                                    *
000135*  BENEFIT CODE                                                  *
000136*  BENEFIT OVERRIDE CODE                                         *
000137*                                                                *
000138*  RETURNED FROM ELRFND                                          *
000139*  --------------------                                          *
000140*  CALCULATED REFUND                                             *
000141*----------------------------------------------------------------*
000142*  PASSED TO ELEARN                                              *
000143*  ----------------                                              *
000144*  CERT ISSUE DATE                                               *
000145*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000146*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000147*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000148*  STATE CODE (CLIENT DEFINED)                                   *
000149*  STATE CODE (STANDARD P.O. ABBRV)                              *
000150*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000151*  DEVIATION CODE                                                *
000152*  ISSUE AGE                                                     *
000153*  ORIGINAL BENEFIT AMOUNT                                       *
000154*  BENEFIT KIND (LIFE OR A/H)                                    *
000155*  A.P.R.                                                        *
000156*  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
000157*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000158*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000159*  COMPANY I.D. (3 CHARACTER)                                    *
000160*  BENEFIT CODE                                                  *
000161*  BENEFIT OVERRIDE CODE                                         *
000162*                                                                *
000163*  RETURNED FROM ELEARN                                          *
000164*  --------------------                                          *
000165*  INDICATED  EARNINGS                                           *
000166*----------------------------------------------------------------*
000167*                 LENGTH = 450                                   *
000168*                                                                *
000169******************************************************************
000170******************************************************************
000171*                   C H A N G E   L O G
000172*
000173* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000174*-----------------------------------------------------------------
000175*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000176* EFFECTIVE    NUMBER
000177*-----------------------------------------------------------------
000178* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
000179* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000180* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
000181* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
000182* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
000183* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000184* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
000185* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000186* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000187* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
000188* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000189* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000190******************************************************************
000191
000192 01  CALCULATION-PASS-AREA.
000193     12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
000194                                     COMP.
000195
000196     12  CP-RETURN-CODE            PIC X             VALUE ZERO.
000197       88  NO-CP-ERROR                             VALUE ZERO.
000198       88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
000199                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
000200       88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
000201       88  CP-ERROR-IN-DATES                       VALUE '2'.
000202       88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
000203       88  CP-ERROR-IN-TERMS                       VALUE '4'.
000204       88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
000205       88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
000206       88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
000207       88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
000208       88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
000209       88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
000210       88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
000211       88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
000212       88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
000213       88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
000214       88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
000215       88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
000216       88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
000217       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
000218
000219     12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
000220       88  NO-CP-ERROR-2                           VALUE ZERO.
000221***********************  INPUT AREAS ****************************
000222
000223     12  CP-CALCULATION-AREA.
000224         16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
000225         16  CP-CERT-EFF-DT        PIC XX.
000226         16  CP-VALUATION-DT       PIC XX.
000227         16  CP-PAID-THRU-DT       PIC XX.
000228         16  CP-BENEFIT-TYPE       PIC X.
000229           88  CP-AH                               VALUE 'A' 'D'
000230                                                   'I' 'U'.
000231           88  CP-REDUCING-LIFE                    VALUE 'R'.
000232           88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
000233         16  CP-INCURRED-DT        PIC XX.
000234         16  CP-REPORTED-DT        PIC XX.
000235         16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
000236         16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
000237         16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
000238                                     COMP-3.
000239         16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
000240                                     COMP-3.
000241         16  CP-CDT-METHOD         PIC X.
000242           88  CP-CDT-ROUND-NEAR                   VALUE '1'.
000243           88  CP-CDT-ROUND-HIGH                   VALUE '2'.
000244           88  CP-CDT-INTERPOLATED                 VALUE '3'.
000245         16  CP-CLAIM-TYPE         PIC X.
000246           88  CP-AH-CLAIM                         VALUE 'A'.
000247           88  CP-LIFE-CLAIM                       VALUE 'L'.
000248         16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
000249                                     COMP-3.
000250         16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
000251                                     COMP-3.
000252         16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
000253                                     COMP-3.
000254         16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
000255                                     COMP-3.
000256         16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
000257                                     COMP-3.
000258         16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
000259                                     COMP-3.
000260         16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
000261                                     COMP-3.
000262         16  CP-REM-TERM-METHOD    PIC X.
000263           88  CP-EARN-AFTER-15TH                  VALUE '1'.
000264           88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
000265           88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
000266           88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
000267           88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
000268           88  CP-EARN-AFTER-14TH                  VALUE '6'.
000269           88  CP-EARN-AFTER-16TH                  VALUE '7'.
000270         16  CP-EARNING-METHOD     PIC X.
000271           88  CP-EARN-BY-R78                      VALUE '1' 'R'.
000272           88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
000273           88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
000274           88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
000275           88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
000276           88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
000277           88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
000278           88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
000279           88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
000280           88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
000281           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
000282           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
000283           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
000284           88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
000285         16  CP-PROCESS-TYPE       PIC X.
000286           88  CP-CLAIM                            VALUE '1'.
000287           88  CP-CANCEL                           VALUE '2'.
000288           88  CP-ISSUE                            VALUE '3'.
000289         16  CP-SPECIAL-CALC-CD    PIC X.
000290           88  CP-OUTSTANDING-BAL              VALUE 'O'.
000291           88  CP-1-MTH-INTEREST               VALUE ' '.
000292           88  CP-0-MTH-INTEREST               VALUE 'A'.
000293           88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
000294           88  CP-CRITICAL-PERIOD              VALUE 'C'.
000295           88  CP-TERM-IS-DAYS                 VALUE 'D'.
000296           88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
000297           88  CP-FARM-PLAN                    VALUE 'F'.
000298           88  CP-RATE-AS-STANDARD             VALUE 'G'.
000299           88  CP-2-MTH-INTEREST               VALUE 'I'.
000300           88  CP-3-MTH-INTEREST               VALUE 'J'.
000301           88  CP-4-MTH-INTEREST               VALUE 'K'.
000302           88  CP-BALLOON-LAST-PMT             VALUE 'L'.
000303           88  CP-MORTGAGE-REC                 VALUE 'M'.
000304           88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
000305           88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
000306           88  CP-NET-PAY-SIMPLE               VALUE 'S'.
000307           88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
000308                                                     'W' 'X'.
000309           88  CP-TRUNCATE-0-MTH               VALUE 'T'.
000310           88  CP-TRUNCATE-1-MTH               VALUE 'U'.
000311           88  CP-TRUNCATE-2-MTH               VALUE 'V'.
000312           88  CP-TRUNCATE-3-MTH               VALUE 'W'.
000313           88  CP-TRUNCATE-4-MTH               VALUE 'X'.
000314           88  CP-SUMMARY-REC                  VALUE 'Z'.
000315           88  CP-PROPERTY-BENEFIT             VALUE '2'.
000316           88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
000317           88  CP-AD-D-BENEFIT                 VALUE '4'.
000318           88  CP-CSL-METH-1                   VALUE '5'.
000319           88  CP-CSL-METH-2                   VALUE '6'.
000320           88  CP-CSL-METH-3                   VALUE '7'.
000321           88  CP-CSL-METH-4                   VALUE '8'.
000322
000323         16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
000324                                     COMP-3.
000325         16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
000326         16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
000327         16  CP-STATE              PIC XX          VALUE SPACE.
000328         16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
000329         16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
000330           88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
000331               '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
000332         16  CP-R78-OPTION         PIC X.
000333           88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
000334           88  CP-TERM-TIMES-TERM                  VALUE '1'.
000335
000336         16  CP-COMPANY-CD         PIC X             VALUE SPACE.
000337         16  CP-IBNR-RESERVE-SW    PIC X.
000338         16  CP-CLAIM-STATUS       PIC X.
000339         16  CP-RATE-FILE          PIC X.
000340         16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
000341                                     COMP-3.
000342
000343         16  CP-LIFE-OVERRIDE-CODE PIC X.
000344         16  CP-AH-OVERRIDE-CODE   PIC X.
000345
000346         16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
000347                                     COMP-3.
000348         16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
000349                                   PIC S9(5)V99 COMP-3.
000350         16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
000351                                     COMP-3.
000352         16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
000353                                     COMP-3.
000354         16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
000355                                     COMP-3.
000356         16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
000357                                  PIC S9(7)V99 COMP-3.
000358
000359         16  CP-PAID-FROM-DATE     PIC X(02).
000360         16  CP-CLAIM-CALC-METHOD  PIC X(01).
000361         16  CP-EXT-DAYS-CALC      PIC X.
000362           88  CP-EXT-NO-CHG                   VALUE ' '.
000363           88  CP-EXT-CHG-LF                   VALUE '1'.
000364           88  CP-EXT-CHG-AH                   VALUE '2'.
000365           88  CP-EXT-CHG-LF-AH                VALUE '3'.
000366         16  CP-DOMICILE-STATE     PIC XX.
000367         16  CP-CARRIER            PIC X.
000368         16  CP-REIN-FLAG          PIC X.
000369         16  CP-REM-TRM-CALC-OPTION PIC X.
000370           88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
000371                      '2' '3' '4' '5'.
000372           88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
000373           88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
000374           88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
000375           88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
000376           88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
000377           88  CP-EXT-30-DAY-MONTH          VALUE '3'.
000378           88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
000379           88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
000380         16  CP-SIG-SWITCH         PIC X.
000381         16  CP-RATING-METHOD      PIC X.
000382           88  CP-RATE-AS-R78                      VALUE '1' 'R'.
000383           88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
000384           88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
000385           88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
000386           88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
000387           88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
000388           88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
000389           88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
000390           88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
000391         16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
000392                                     COMP-3.
000393         16  CP-BEN-CATEGORY       PIC X.
000394         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000395         16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
000396                                   PIC S99V9(5) COMP-3.
000397         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000398         16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
000399                                   PIC S99V9(5) COMP-3.
000400         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
000401         16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
000402         16  CP-EXPIRE-DT          PIC XX.
000403         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
000404         16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
000405         16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
000406         16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
000407         16  CP-DDF-SPEC-CALC      PIC X.
000408             88  CP-CALC-GROSS-FEE        VALUE 'G'.
000409             88  CP-CALC-CLP              VALUE 'C'.
000410         16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
000411         16  CP-CANCEL-REASON      PIC X.
000412         16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
000413         16  CP-PMT-MODE           PIC X.
000414         16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
000415         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
000416         16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
000417         16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
000418         16  FILLER                PIC X.
000419
000420***************    OUTPUT FROM ELRESV   ************************
000421
000422         16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
000423
000424         16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
000425                                     COMP-3.
000426         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
000427                                     COMP-3.
000428         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
000429                                     COMP-3.
000430         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
000431                                     COMP-3.
000432         16  FILLER                PIC X(09).
000433***************    OUTPUT FROM ELRTRM   *************************
000434
000435         16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
000436                                     COMP-3.
000437         16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
000438                                     COMP-3.
000439         16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
000440                                     COMP-3.
000441         16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
000442                                     COMP-3.
000443         16  FILLER                PIC X(12).
000444
000445***************    OUTPUT FROM ELRAMT   *************************
000446
000447         16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
000448                                     COMP-3.
000449         16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
000450                                     COMP-3.
000451         16  FILLER                PIC X(12).
000452
000453***************    OUTPUT FROM ELRATE   *************************
000454
000455         16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
000456                                     COMP-3.
000457         16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
000458                                     COMP-3.
000459         16  CP-MORTALITY-CODE     PIC X(4).
000460         16  CP-RATE-EDIT-FLAG     PIC X.
000461             88  CP-RATES-NEED-APR                  VALUE '1'.
000462         16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
000463                                     COMP-3.
000464         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
000465         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
000466         16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
000467                                   PIC S9(7)V99 COMP-3.
000468         16  FILLER                PIC X(07).
000469
000470***************    OUTPUT FROM ELRFND   *************************
000471
000472         16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
000473                                     COMP-3.
000474         16  CP-REFUND-TYPE-USED   PIC X.
000475           88  CP-R-AS-R78                         VALUE '1'.
000476           88  CP-R-AS-PRORATA                     VALUE '2'.
000477           88  CP-R-AS-CALIF                       VALUE '3'.
000478           88  CP-R-AS-TEXAS                       VALUE '4'.
000479           88  CP-R-AS-FARM-PLAN                   VALUE '4'.
000480           88  CP-R-AS-NET-PAY                     VALUE '5'.
000481           88  CP-R-AS-ANTICIPATION                VALUE '6'.
000482           88  CP-R-AS-MEAN                        VALUE '8'.
000483           88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
000484           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
000485           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
000486           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
000487           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
000488           88  CP-R-AS-REPOSSESSION                VALUE 'R'.
000489         16  FILLER                PIC X(12).
000490
000491***************    OUTPUT FROM ELEARN   *************************
000492
000493         16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
000494                                     COMP-3.
000495         16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
000496                                     COMP-3.
000497         16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
000498                                     COMP-3.
000499         16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
000500                                     COMP-3.
000501         16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
000502                                     COMP-3.
000503         16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
000504                                     COMP-3.
000505         16  CP-EARNING-TYPE-USED  PIC X.
000506           88  CP-E-AS-SPECIAL                     VALUE 'S'.
000507           88  CP-E-AS-R78                         VALUE '1'.
000508           88  CP-E-AS-PRORATA                     VALUE '2'.
000509           88  CP-E-AS-TEXAS                       VALUE '4'.
000510           88  CP-E-AS-FARM-PLAN                   VALUE '4'.
000511           88  CP-E-AS-NET-PAY                     VALUE '5'.
000512           88  CP-E-AS-ANTICIPATION                VALUE '6'.
000513           88  CP-E-AS-MEAN                        VALUE '8'.
000514           88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
000515         16  FILLER                PIC X(12).
000516
000517***************    OUTPUT FROM ELPMNT   *************************
000518
000519         16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
000520                                     COMP-3.
000521         16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
000522                                     COMP-3.
000523         16  FILLER                PIC X(12).
000524
000525***************   MISC WORK AREAS    *****************************
000526         16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
000527                                     COMP-3.
000528         16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
000529                                     COMP-3.
000530         16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
000531                                     COMP-3.
000532         16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
000533                                     COMP-3.
000534         16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
000535                                     COMP-3.
000536         16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
000537                                     COMP-3.
000538         16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
000539             88  OPEN-RATE-FILE                   VALUE 'O'.
000540             88  CLOSE-RATE-FILE                  VALUE 'C'.
000541             88  IO-ERROR                         VALUE 'E'.
000542
000543         16  CP-FIRST-PAY-DATE     PIC XX.
000544
000545         16  CP-JOINT-INDICATOR    PIC X.
000546
000547         16  CP-RESERVE-REMAINING-TERM
000548                                   PIC S9(4)V9    VALUE ZERO
000549                                     COMP-3.
000550
000551         16  CP-INSURED-BIRTH-DT   PIC XX.
000552
000553         16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
000554                                     COMP-3.
000555
000556         16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
000557                                     COMP-3.
000558
000559         16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
000560                                     COMP-3.
000561
000562         16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
000563                                     COMP-3.
000564
000565         16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
000566                                     COMP-3.
000567
000568         16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
000569                                     COMP-3.
000570
000571         16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
000572                                     COMP-3.
000573
000574         16  CP-ROA-REFUND         PIC X          VALUE 'N'.
000575             88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
000576
000577         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
000578                                     COMP-3.
000579         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
000580                                     COMP-3.
000581         16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
000582         16  cp-extra-periods      pic 9 value zeros.
000583         16  cp-net-only-state     pic x value spaces.
000584         16  FILLER                PIC X(13).
000585******************************************************************
      *<<((file: ELCCALC))
000638 EJECT
000639*                            COPY ERCCTBL.
      *>>((file: ERCCTBL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCTBL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION TABLE                        *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 200   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021*                                                                *
000022******************************************************************
000023
000024 01  COMM-TABLE-RECORD.
000025     12  CT-RECORD-ID                      PIC XX.
000026         88  VALID-CT-ID                      VALUE 'CT'.
000027
000028     12  CT-CONTROL-PRIMARY.
000029         16  CT-COMPANY-CD                 PIC X.
000030         16  CT-TABLE                      PIC XXX.
000031         16  CT-CNTRL-2.
000032             20  CT-BEN-TYPE               PIC X.
000033             20  CT-BEN-CODE               PIC XX.
000034
000035     12  CT-MAINT-INFORMATION.
000036         16  CT-LAST-MAINT-DT              PIC XX.
000037         16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000038         16  CT-LAST-MAINT-USER            PIC X(4).
000039         16  FILLER                        PIC X(31).
000040
000041     12  CT-LIMITS.
000042         16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
000043
000044         16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
000045
000046         16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
000047
000048     12  CT-RATES.
000049         16  CT-RTX          OCCURS 27 TIMES.
000050             20  CT-RT                     PIC SV9(5)     COMP-3.
000051             20  CT-RT-R   REDEFINES
000052                 CT-RT                     PIC XXX.
000053
000054     12  FILLER                            PIC  X(42).
000055
000056******************************************************************
      *<<((file: ERCCTBL))
000640
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
000642
000643 01  DFHCOMMAREA                 PIC  X(1024).
000644
000645*01 PARMLIST .
000646*    12  FILLER                  PIC S9(08)      COMP.
000647*    12  ELCNTL-POINTER          PIC S9(08)      COMP.
000648*    12  ELACCT-POINTER          PIC S9(08)      COMP.
000649*    12  ELCERT-POINTER          PIC S9(08)      COMP.
000650
000651*                            COPY ELCCNTL.
      *>>((file: ELCCNTL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCNTL.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.059                          *
000007*                                                                *
000008*   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 750  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 082503                   PEMA  ADD BENEFIT GROUP
000027* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000028* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000029* 092705    2005050300006  PEMA  ADD SPP LEASES
000030* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000031* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
000032* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
000033* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
000034* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
000035* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000036* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
000037* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
000041******************************************************************
000042*
000043 01  CONTROL-FILE.
000044     12  CF-RECORD-ID                       PIC XX.
000045         88  VALID-CF-ID                        VALUE 'CF'.
000046
000047     12  CF-CONTROL-PRIMARY.
000048         16  CF-COMPANY-ID                  PIC XXX.
000049         16  CF-RECORD-TYPE                 PIC X.
000050             88  CF-COMPANY-MASTER              VALUE '1'.
000051             88  CF-PROCESSOR-MASTER            VALUE '2'.
000052             88  CF-STATE-MASTER                VALUE '3'.
000053             88  CF-LF-BENEFIT-MASTER           VALUE '4'.
000054             88  CF-AH-BENEFIT-MASTER           VALUE '5'.
000055             88  CF-CARRIER-MASTER              VALUE '6'.
000056             88  CF-MORTALITY-MASTER            VALUE '7'.
000057             88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
000058             88  CF-TERMINAL-MASTER             VALUE '9'.
000059             88  CF-AH-EDIT-MASTER              VALUE 'A'.
000060             88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
000061             88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
000062             88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
000063             88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
000064             88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
000065             88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
000066             88  CF-REMINDERS-MASTER            VALUE 'R'.
000067             88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
000068         16  CF-ACCESS-CD-GENL              PIC X(4).
000069         16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
000070             20  CF-PROCESSOR               PIC X(4).
000071         16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
000072             20  CF-STATE-CODE              PIC XX.
000073             20  FILLER                     PIC XX.
000074         16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
000075             20  FILLER                     PIC XX.
000076             20  CF-HI-BEN-IN-REC           PIC XX.
000077         16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
000078             20  FILLER                     PIC XXX.
000079             20  CF-CARRIER-CNTL            PIC X.
000080         16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
000081             20  FILLER                     PIC XX.
000082             20  CF-HI-TYPE-IN-REC          PIC 99.
000083         16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
000084             20  CF-CRDB-TABLE-INDICATOR    PIC X.
000085                 88  CF-CRDB-NAIC-TABLE         VALUE '9'.
000086             20  CF-CRDB-BENEFIT-TYPE       PIC X.
000087             20  CF-CRDB-WAITING-PERIOD     PIC XX.
000088         16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
000089             20  FILLER                     PIC X.
000090             20  CF-CUSTOM-REPORT-NO        PIC 999.
000091         16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
000092             20  FILLER                     PIC XX.
000093             20  CF-MORTGAGE-PLAN           PIC XX.
000094         16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
000095
000096     12  CF-LAST-MAINT-DT                   PIC XX.
000097     12  CF-LAST-MAINT-BY                   PIC X(4).
000098     12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
000099
000100     12  CF-RECORD-BODY                     PIC X(728).
000101
000102
000103****************************************************************
000104*             COMPANY MASTER RECORD                            *
000105****************************************************************
000106
000107     12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000108         16  CF-COMPANY-ADDRESS.
000109             20  CF-CL-MAIL-TO-NAME         PIC X(30).
000110             20  CF-CL-IN-CARE-OF           PIC X(30).
000111             20  CF-CL-ADDR-LINE-1          PIC X(30).
000112             20  CF-CL-ADDR-LINE-2          PIC X(30).
000113             20  CF-CL-CITY-STATE           PIC X(30).
000114             20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
000115             20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
000116         16  CF-COMPANY-CD                  PIC X.
000117         16  CF-COMPANY-PASSWORD            PIC X(8).
000118         16  CF-SECURITY-OPTION             PIC X.
000119             88  ALL-SECURITY                   VALUE '1'.
000120             88  COMPANY-VERIFY                 VALUE '2'.
000121             88  PROCESSOR-VERIFY               VALUE '3'.
000122             88  NO-SECURITY                    VALUE '4'.
000123             88  ALL-BUT-TERM                   VALUE '5'.
000124         16  CF-CARRIER-CONTROL-LEVEL       PIC X.
000125             88  USE-ACTUAL-CARRIER             VALUE SPACE.
000126         16  CF-LGX-INTERFACE-CNTL          PIC X.
000127             88  LGX-TIME-SHR-COMPANY           VALUE '1'.
000128         16  CF-INFORCE-LOCATION            PIC X.
000129             88  CERTS-ARE-ONLINE               VALUE '1'.
000130             88  CERTS-ARE-OFFLINE              VALUE '2'.
000131             88  NO-CERTS-AVAILABLE             VALUE '3'.
000132         16  CF-LOWER-CASE-LETTERS          PIC X.
000133         16  CF-CERT-ACCESS-CONTROL         PIC X.
000134             88  CF-ST-ACCNT-CNTL               VALUE ' '.
000135             88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000136             88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
000137             88  CF-ACCNT-CNTL                  VALUE '3'.
000138             88  CF-CARR-ACCNT-CNTL             VALUE '4'.
000139
000140         16  CF-FORMS-PRINTER-ID            PIC X(4).
000141         16  CF-CHECK-PRINTER-ID            PIC X(4).
000142
000143         16  CF-LGX-CREDIT-USER             PIC X.
000144             88  CO-IS-NOT-USER                 VALUE 'N'.
000145             88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
000146
000147         16 CF-CREDIT-CALC-CODES.
000148             20  CF-CR-REM-TERM-CALC PIC X.
000149               88  CR-EARN-AFTER-15TH           VALUE '1'.
000150               88  CR-EARN-ON-HALF-MO           VALUE '2'.
000151               88  CR-EARN-ON-1ST-DAY           VALUE '3'.
000152               88  CR-EARN-ON-FULL-MO           VALUE '4'.
000153               88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
000154               88  CR-EARN-AFTER-14TH           VALUE '6'.
000155               88  CR-EARN-AFTER-16TH           VALUE '7'.
000156             20  CF-CR-R78-METHOD           PIC X.
000157               88  USE-TERM-PLUS-ONE            VALUE SPACE.
000158               88  DONT-USE-PLUS-ONE            VALUE '1'.
000159
000160         16  CF-CLAIM-CONTROL-COUNTS.
000161             20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
000162                 88  CO-CLM-COUNT-RESET         VALUE +99999.
000163
000164             20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
000165                 88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
000166
000167             20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
000168                 88  CO-CHECK-COUNT-RESET       VALUE +9999999.
000169
000170             20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
000171                 88  CO-QUE-COUNT-RESET         VALUE +9999999.
000172
000173         16  CF-CURRENT-MONTH-END           PIC XX.
000174
000175         16  CF-CO-CALC-QUOTE-TOLERANCE.
000176             20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
000177             20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
000178             20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
000179             20  CF-CO-CLAIM-REJECT-SW      PIC X.
000180                 88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
000181                 88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
000182             20  CF-CO-PREM-REJECT-SW       PIC X.
000183                 88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
000184                 88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
000185             20  CF-CO-REF-REJECT-SW        PIC X.
000186                 88 CO-WARN-IF-REF-OUT          VALUE SPACE.
000187                 88 CO-FORCE-IF-REF-OUT         VALUE '1'.
000188
000189         16  CF-CO-REPORTING-DT             PIC XX.
000190         16  CF-CO-REPORTING-MONTH-DT       PIC XX.
000191         16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
000192           88  CF-CO-NOT-MONTH-END              VALUE SPACES.
000193           88  CF-CO-MONTH-END                  VALUE '1'.
000194
000195         16  CF-LGX-CLAIM-USER              PIC X.
000196             88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
000197             88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
000198
000199         16  CF-CREDIT-EDIT-CONTROLS.
000200             20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
000201             20  CF-MIN-AGE                 PIC 99.
000202             20  CF-DEFAULT-AGE             PIC 99.
000203             20  CF-MIN-TERM                PIC S999      COMP-3.
000204             20  CF-MAX-TERM                PIC S999      COMP-3.
000205             20  CF-DEFAULT-SEX             PIC X.
000206             20  CF-JOINT-AGE-INPUT         PIC X.
000207                 88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
000208             20  CF-BIRTH-DATE-INPUT        PIC X.
000209                 88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
000210             20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
000211                 88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
000212                 88  CF-ZERO-CARRIER            VALUE '1'.
000213                 88  CF-ZERO-GROUPING           VALUE '2'.
000214                 88  CF-ZERO-CAR-GROUP          VALUE '3'.
000215             20  CF-EDIT-SW                 PIC X.
000216                 88  CF-START-EDIT-TONIGHT      VALUE '1'.
000217             20  CF-EDIT-RESTART-BATCH      PIC X(6).
000218             20  CF-CR-PR-METHOD            PIC X.
000219               88  USE-NORMAL-PR-METHOD         VALUE SPACE.
000220               88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
000221             20  FILLER                     PIC X.
000222
000223         16  CF-CREDIT-MISC-CONTROLS.
000224             20  CF-REIN-TABLE-SW           PIC X.
000225                 88 REIN-TABLES-ARE-USED        VALUE '1'.
000226             20  CF-COMP-TABLE-SW           PIC X.
000227                 88 COMP-TABLES-ARE-USED        VALUE '1'.
000228             20  CF-EXPERIENCE-RETENTION-AGE
000229                                            PIC S9        COMP-3.
000230             20  CF-CONVERSION-DT           PIC XX.
000231             20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
000232             20  CF-RUN-FREQUENCY-SW        PIC X.
000233                 88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
000234                 88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
000235
000236             20  CF-CR-CHECK-NO-CONTROL.
000237                 24  CF-CR-CHECK-NO-METHOD    PIC X.
000238                     88  CR-CHECK-NO-MANUAL       VALUE '1'.
000239                     88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
000240                     88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
000241                 24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
000242                     88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
000243
000244                 24  CF-CR-CHECK-COUNT       REDEFINES
000245                     CF-CR-CHECK-COUNTER      PIC X(4).
000246
000247                 24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
000248                     88  CR-QUE-COUNT-RESET      VALUE +9999999.
000249
000250                 24  CF-CR-CHECK-QUE-COUNT   REDEFINES
000251                     CF-CR-CHECK-QUE-COUNTER  PIC X(4).
000252                 24  CF-MAIL-PROCESSING       PIC X.
000253                     88  MAIL-PROCESSING          VALUE 'Y'.
000254
000255         16  CF-MISC-SYSTEM-CONTROL.
000256             20  CF-SYSTEM-C                 PIC X.
000257                 88  CONFIRMATION-SYS-USED       VALUE '1'.
000258             20  CF-SYSTEM-D                 PIC X.
000259                 88  DAILY-BILL-SYS-USED         VALUE '1'.
000260             20  CF-SOC-SEC-NO-SW            PIC X.
000261                 88  SOC-SEC-NO-USED             VALUE '1'.
000262             20  CF-MEMBER-NO-SW             PIC X.
000263                 88  MEMBER-NO-USED              VALUE '1'.
000264             20  CF-TAX-ID-NUMBER            PIC X(11).
000265             20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
000266             20  CF-PAYMENT-APPROVAL-SW      PIC X.
000267                 88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
000268                 88  CF-NO-APPROVAL              VALUE ' ' 'N'.
000269                 88  CF-ALL-APPROVED             VALUE 'Y'.
000270                 88  CF-GRADUATED-APPROVAL       VALUE 'G'.
000271             20  CF-SYSTEM-E                 PIC X.
000272                 88  CF-AR-SYSTEM-USED           VALUE 'Y'.
000273
000274         16  CF-LGX-LIFE-USER               PIC X.
000275             88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
000276             88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
000277
000278         16  CF-CR-MONTH-END-DT             PIC XX.
000279
000280         16  CF-FILE-MAINT-DATES.
000281             20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
000282                 88  CF-LAST-BATCH-RESET        VALUE +999999.
000283             20  CF-LAST-BATCH       REDEFINES
000284                 CF-LAST-BATCH-NO               PIC X(4).
000285             20  CF-RATES-FILE-MAINT-DT         PIC XX.
000286             20  CF-RATES-FILE-CREATE-DT        PIC XX.
000287             20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
000288             20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
000289             20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
000290             20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
000291             20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
000292             20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
000293             20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
000294             20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
000295
000296         16  CF-NEXT-COMPANY-ID             PIC XXX.
000297         16  FILLER                         PIC X.
000298
000299         16  CF-ALT-MORT-CODE               PIC X(4).
000300         16  CF-MEMBER-CAPTION              PIC X(10).
000301
000302         16  CF-LIFE-ACCESS-CONTROL         PIC X.
000303             88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
000304             88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
000305             88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
000306             88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
000307             88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
000308
000309         16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
000310
000311         16  CF-LIFE-OVERRIDE-L1            PIC X.
000312         16  CF-LIFE-OVERRIDE-L2            PIC XX.
000313         16  CF-LIFE-OVERRIDE-L6            PIC X(6).
000314         16  CF-LIFE-OVERRIDE-L12           PIC X(12).
000315
000316         16  CF-AH-OVERRIDE-L1              PIC X.
000317         16  CF-AH-OVERRIDE-L2              PIC XX.
000318         16  CF-AH-OVERRIDE-L6              PIC X(6).
000319         16  CF-AH-OVERRIDE-L12             PIC X(12).
000320
000321         16  CF-REPORT-CD1-CAPTION          PIC X(10).
000322         16  CF-REPORT-CD2-CAPTION          PIC X(10).
000323
000324         16  CF-CLAIM-CUTOFF-DATE           PIC XX.
000325         16  CF-AR-LAST-EL860-DT            PIC XX.
000326         16  CF-MP-MONTH-END-DT             PIC XX.
000327
000328         16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
000329         16  CF-CLAIM-PAID-THRU-TO          PIC X.
000330             88  CF-CLAIM-PAID-TO               VALUE '1'.
000331
000332         16  CF-AR-MONTH-END-DT             PIC XX.
000333
000334         16  CF-CRDTCRD-USER                PIC X.
000335             88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
000336             88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
000337
000338         16  CF-CC-MONTH-END-DT             PIC XX.
000339
000340         16  CF-PRINT-ADDRESS-LABELS        PIC X.
000341
000342         16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
000343             88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
000344             88  CF-USE-ALL-AGE-LAST            VALUE '2'.
000345             88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
000346         16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
000347         16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
000348         16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
000349         16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
000350             88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
000351             88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
000352         16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
000353         16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
000354         16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
000355             88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
000356         16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
000357
000358         16  CF-CL-ZIP-CODE.
000359             20  CF-CL-ZIP-PRIME.
000360                 24  CF-CL-ZIP-1ST          PIC X.
000361                     88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
000362                 24  FILLER                 PIC X(4).
000363             20  CF-CL-ZIP-PLUS4            PIC X(4).
000364         16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
000365             20  CF-CL-CAN-POSTAL-1         PIC XXX.
000366             20  CF-CL-CAN-POSTAL-2         PIC XXX.
000367             20  FILLER                     PIC XXX.
000368
000369         16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
000370         16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
000371         16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
000372         16  CF-CO-OPTION-START-DATE        PIC XX.
000373         16  CF-REM-TRM-CALC-OPTION         PIC X.
000374           88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
000375                                                      '3' '4'.
000376           88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
000377           88  CF-30-DAY-MONTH                  VALUE '1' '3'.
000378           88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
000379           88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
000380           88  CF-EXT-30-DAY-MONTH              VALUE '3'.
000381           88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
000382
000383         16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
000384
000385         16  CF-PAYMENT-APPROVAL-LEVELS.
000386             20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
000387             20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
000388             20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
000389             20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
000390             20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
000391             20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
000392
000393         16  CF-END-USER-REPORTING-USER     PIC X.
000394             88  CO-NO-END-USER-REPORTING       VALUE 'N'.
000395             88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
000396
000397         16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
000398             88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
000399             88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
000400
000401         16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
000402
000403         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
000404         16  FILLER                         PIC X.
000405
000406         16  CF-CREDIT-ARCHIVE-CNTL.
000407             20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
000408             20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
000409             20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
000410
000411         16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
000412
000413         16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
000414             88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
000415             88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
000416
000417         16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
000418             88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
000419             88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
000420
000421         16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
000422
000423         16  CF-CO-ACH-ID-CODE              PIC  X.
000424             88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
000425             88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
000426             88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
000427         16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
000428         16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
000429         16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
000430         16  CF-CO-ACH-ADMIN-NO             PIC X(09).
000431         16  CF-CO-ACH-RECV-NAME            PIC X(23).
000432         16  CF-CO-ACH-RECV-NO              PIC X(08).
000433         16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
000434         16  CF-CO-ACH-COMPANY-ID           PIC X(09).
000435         16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
000436                 88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
000437         16  CF-CO-ACH-TRACE-SPACE REDEFINES
000438                 CF-CO-ACH-TRACE-NO         PIC X(4).
000439
000440         16  CF-CO-OVER-SHORT.
000441             20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
000442             20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000443
000444*         16  FILLER                         PIC X(102).
000445         16  CF-PAYMENT-APPROVAL-LEVELS-2.
000446             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
000447             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
000448
000449         16  CF-AH-APPROVAL-DAYS.
000450             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
000451             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
000452             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
000453             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
000454
000455         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
000456
000457         16  CF-APPROV-LEV-5.
000458             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
000459             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
000460             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
000461
000462         16  FILLER                         PIC X(68).
000463****************************************************************
000464*             PROCESSOR/USER RECORD                            *
000465****************************************************************
000466
000467     12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000468         16  CF-PROCESSOR-NAME              PIC X(30).
000469         16  CF-PROCESSOR-PASSWORD          PIC X(11).
000470         16  CF-PROCESSOR-TITLE             PIC X(26).
000471         16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
000472                 88  MESSAGE-YES                VALUE 'Y'.
000473                 88  MESSAGE-NO                 VALUE ' ' 'N'.
000474
000475*****************************************************
000476****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
000477****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
000478****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
000479****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
000480*****************************************************
000481
000482         16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
000483             20  CF-ADMINISTRATION-CONTROLS PIC XX.
000484             20  CF-APPLICATION-FORCE       PIC X.
000485             20  CF-INDIVIDUAL-APP.
000486                 24  CF-APP-SWITCHES  OCCURS  44 TIMES.
000487                     28  CF-BROWSE-APP      PIC X.
000488                     28  CF-UPDATE-APP      PIC X.
000489
000490         16  CF-CURRENT-TERM-ON             PIC X(4).
000491         16  CF-PROCESSOR-LIMITS-CLAIMS.
000492             20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
000493             20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
000494             20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
000495             20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
000496             20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
000497             20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
000498             20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
000499         16  CF-PROCESSOR-CARRIER           PIC X.
000500             88  NO-CARRIER-SECURITY            VALUE ' '.
000501         16  CF-PROCESSOR-ACCOUNT           PIC X(10).
000502             88  NO-ACCOUNT-SECURITY            VALUE SPACES.
000503         16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
000504             88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
000505         16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
000506             88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
000507
000508         16  CF-PROC-SYS-ACCESS-SW.
000509             20  CF-PROC-CREDIT-CLAIMS-SW.
000510                 24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
000511                     88  ACCESS-TO-CREDIT           VALUE 'Y'.
000512                 24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
000513                     88  ACCESS-TO-CLAIMS           VALUE 'Y'.
000514             20  CF-PROC-CREDIT-CLAIMS   REDEFINES
000515                 CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
000516                 88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
000517             20  CF-PROC-LIFE-GNRLDGR-SW.
000518                 24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
000519                     88  ACCESS-TO-LIFE             VALUE 'Y'.
000520                 24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
000521                     88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
000522             20  CF-PROC-LIFE-GNRLDGR    REDEFINES
000523                 CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
000524                 88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
000525         16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
000526             CF-PROC-SYS-ACCESS-SW              PIC X(4).
000527             88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
000528         16  CF-PROCESSOR-PRINTER               PIC X(4).
000529
000530         16  CF-APPROVAL-LEVEL                  PIC X.
000531             88  APPROVAL-LEVEL-1                   VALUE '1'.
000532             88  APPROVAL-LEVEL-2                   VALUE '2'.
000533             88  APPROVAL-LEVEL-3                   VALUE '3'.
000534             88  APPROVAL-LEVEL-4                   VALUE '4'.
000535             88  APPROVAL-LEVEL-5                   VALUE '5'.
000536
000537         16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
000538
000539         16  CF-LANGUAGE-TYPE                   PIC X.
000540             88  CF-LANG-IS-ENG                     VALUE 'E'.
000541             88  CF-LANG-IS-FR                      VALUE 'F'.
000542
000543         16  CF-CSR-IND                         PIC X.
000544         16  FILLER                             PIC X(239).
000545
000546****************************************************************
000547*             PROCESSOR/REMINDERS RECORD                       *
000548****************************************************************
000549
000550     12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
000551         16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
000552             20  CF-START-REMIND-DT         PIC XX.
000553             20  CF-END-REMIND-DT           PIC XX.
000554             20  CF-REMINDER-TEXT           PIC X(50).
000555         16  FILLER                         PIC X(296).
000556
000557
000558****************************************************************
000559*             STATE MASTER RECORD                              *
000560****************************************************************
000561
000562     12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000563         16  CF-STATE-ABBREVIATION          PIC XX.
000564         16  CF-STATE-NAME                  PIC X(25).
000565         16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
000566         16  CF-ST-CALC-QUOTE-TOLERANCE.
000567             20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
000568             20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
000569             20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
000570             20  CF-ST-CLAIM-REJECT-SW      PIC X.
000571                 88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
000572                 88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
000573             20  CF-ST-PREM-REJECT-SW       PIC X.
000574                 88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
000575                 88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
000576             20  CF-ST-REF-REJECT-SW        PIC X.
000577                 88 ST-WARN-IF-REF-OUT          VALUE SPACE.
000578                 88 ST-FORCE-IF-REF-OUT         VALUE '1'.
000579         16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
000580         16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
000581         16  CF-ST-REFUND-RULES.
000582             20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
000583             20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
000584             20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
000585         16  CF-ST-FST-PMT-EXTENSION.
000586             20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
000587             20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
000588                 88  CF-ST-EXT-NO-CHG           VALUE ' '.
000589                 88  CF-ST-EXT-CHG-LF           VALUE '1'.
000590                 88  CF-ST-EXT-CHG-AH           VALUE '2'.
000591                 88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
000592         16  CF-ST-STATE-CALL.
000593             20  CF-ST-CALL-UNEARNED        PIC X.
000594             20  CF-ST-CALL-RPT-CNTL        PIC X.
000595             20  CF-ST-CALL-RATE-DEV        PIC XXX.
000596         16  CF-REPLACEMENT-LAW-SW          PIC X.
000597             88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
000598             88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
000599         16  CF-REPLACEMENT-LETTER          PIC X(4).
000600         16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
000601         16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
000602         16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
000603         16  CF-ST-SPLIT-PAYMENT            PIC X.
000604         16  FILLER                         PIC X.
000605         16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
000606             20  CF-ST-BENEFIT-CD           PIC XX.
000607             20  CF-ST-BENEFIT-KIND         PIC X.
000608                 88  CF-ST-LIFE-KIND            VALUE 'L'.
000609                 88  CF-ST-AH-KIND              VALUE 'A'.
000610             20  CF-ST-REM-TERM-CALC        PIC X.
000611                 88  ST-REM-TERM-NOT-USED       VALUE SPACE.
000612                 88  ST-EARN-AFTER-15TH         VALUE '1'.
000613                 88  ST-EARN-ON-HALF-MO         VALUE '2'.
000614                 88  ST-EARN-ON-1ST-DAY         VALUE '3'.
000615                 88  ST-EARN-ON-FULL-MO         VALUE '4'.
000616                 88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
000617                 88  ST-EARN-AFTER-14TH         VALUE '6'.
000618                 88  ST-EARN-AFTER-16TH         VALUE '7'.
000619
000620             20  CF-ST-REFUND-CALC          PIC X.
000621                 88  ST-REFUND-NOT-USED         VALUE SPACE.
000622                 88  ST-REFD-BY-R78             VALUE '1'.
000623                 88  ST-REFD-BY-PRO-RATA        VALUE '2'.
000624                 88  ST-REFD-AS-CALIF           VALUE '3'.
000625                 88  ST-REFD-AS-TEXAS           VALUE '4'.
000626                 88  ST-REFD-IS-NET-PAY         VALUE '5'.
000627                 88  ST-REFD-ANTICIPATION       VALUE '6'.
000628                 88  ST-REFD-UTAH               VALUE '7'.
000629                 88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
000630                 88  ST-REFD-REG-BALLOON        VALUE 'B'.
000631                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
000632
000633             20  CF-ST-EARNING-CALC         PIC X.
000634                 88  ST-EARNING-NOT-USED        VALUE SPACE.
000635                 88  ST-EARN-BY-R78             VALUE '1'.
000636                 88  ST-EARN-BY-PRO-RATA        VALUE '2'.
000637                 88  ST-EARN-AS-CALIF           VALUE '3'.
000638                 88  ST-EARN-AS-TEXAS           VALUE '4'.
000639                 88  ST-EARN-IS-NET-PAY         VALUE '5'.
000640                 88  ST-EARN-ANTICIPATION       VALUE '6'.
000641                 88  ST-EARN-MEAN               VALUE '8'.
000642                 88  ST-EARN-REG-BALLOON        VALUE 'B'.
000643
000644             20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
000645                 88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
000646                 88  ST-OVRD-BY-R78             VALUE '1'.
000647                 88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
000648                 88  ST-OVRD-AS-CALIF           VALUE '3'.
000649                 88  ST-OVRD-AS-TEXAS           VALUE '4'.
000650                 88  ST-OVRD-IS-NET-PAY         VALUE '5'.
000651                 88  ST-OVRD-ANTICIPATION       VALUE '6'.
000652                 88  ST-OVRD-MEAN               VALUE '8'.
000653                 88  ST-OVRD-REG-BALLOON        VALUE 'B'.
000654             20  cf-st-extra-periods        pic 9.
000655*            20  FILLER                     PIC X.
000656
000657         16  CF-ST-COMMISSION-CAPS.
000658             20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
000659             20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
000660             20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
000661             20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
000662         16  CF-COMM-CAP-LIMIT-TO           PIC X.
000663                 88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
000664                 88  ST-LIMIT-TO-GA             VALUE 'G'.
000665                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
000666
000667         16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
000668
000669         16  CF-ST-STATUTORY-INTEREST.
000670             20  CF-ST-STAT-DATE-FROM       PIC X.
000671                 88  ST-STAT-FROM-INCURRED      VALUE 'I'.
000672                 88  ST-STAT-FROM-REPORTED      VALUE 'R'.
000673             20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
000674             20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
000675             20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
000676             20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
000677             20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
000678
000679         16  CF-ST-OVER-SHORT.
000680             20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
000681             20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
000682
000683         16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
000684
000685         16  CF-ST-RT-CALC                  PIC X.
000686
000687         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
000688         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
000689         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
000690         16  CF-ST-RF-LR-CALC               PIC X.
000691         16  CF-ST-RF-LL-CALC               PIC X.
000692         16  CF-ST-RF-LN-CALC               PIC X.
000693         16  CF-ST-RF-AH-CALC               PIC X.
000694         16  CF-ST-RF-CP-CALC               PIC X.
000695*        16  FILLER                         PIC X(206).
000696*CIDMOD         16  FILLER                         PIC X(192).
000697         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
000698             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
000699         16  CF-ST-REF-AH-DEATH-IND         PIC X.
000700         16  CF-ST-VFY-2ND-BENE             PIC X.
000701         16  CF-ST-CAUSAL-STATE             PIC X.
000702         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
000703         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
000704         16  CF-ST-AGENT-SIG-EDIT           PIC X.
000705             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
000706         16  CF-ST-NET-ONLY-STATE           PIC X.
000707             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
000708         16  cf-commission-cap-required     pic x.
000709         16  CF-ST-GA-COMMISSION-CAPS.
000710             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
000711             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
000712             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
000713             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
000714         16  CF-ST-TOT-COMMISSION-CAPS.
000715             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
000716             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
000717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
000718             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
000719         16  FILLER                         PIC X(156).
000720
000721****************************************************************
000722*             BENEFIT MASTER RECORD                            *
000723****************************************************************
000724
000725     12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000726         16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
000727             20  CF-BENEFIT-CODE            PIC XX.
000728             20  CF-BENEFIT-NUMERIC  REDEFINES
000729                 CF-BENEFIT-CODE            PIC XX.
000730             20  CF-BENEFIT-ALPHA           PIC XXX.
000731             20  CF-BENEFIT-DESCRIP         PIC X(10).
000732             20  CF-BENEFIT-COMMENT         PIC X(10).
000733
000734             20  CF-LF-COVERAGE-TYPE        PIC X.
000735                 88  CF-REDUCING                VALUE 'R'.
000736                 88  CF-LEVEL                   VALUE 'L' 'P'.
000737
000738             20  CF-SPECIAL-CALC-CD         PIC X.
000739                 88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
000740                 88  CF-NP-0-MO-INT             VALUE 'A'.
000741                 88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
000742                 88  CF-CRITICAL-PERIOD         VALUE 'C'.
000743                 88  CF-TERM-IN-DAYS            VALUE 'D'.
000744                 88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
000745                 88  CF-FARM-PLAN               VALUE 'F'.
000746                 88  CF-RATE-AS-STANDARD        VALUE 'G'.
000747                 88  CF-2-MTH-INTEREST          VALUE 'I'.
000748                 88  CF-3-MTH-INTEREST          VALUE 'J'.
000749                 88  CF-4-MTH-INTEREST          VALUE 'K'.
000750                 88  CF-BALLOON-LAST-PMT        VALUE 'L'.
000751                 88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
000752                 88  CF-PRUDENTIAL              VALUE 'P'.
000753                 88  CF-OUTSTANDING-BAL         VALUE 'O'.
000754                 88  CF-TRUNCATED-LIFE          VALUE 'T'.
000755                 88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
000756                 88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
000757                 88  CF-NET-PAY-SIMPLE          VALUE 'S'.
000758                 88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
000759
000760             20  CF-JOINT-INDICATOR         PIC X.
000761                 88  CF-JOINT-COVERAGE          VALUE 'J'.
000762
000763*            20  FILLER                     PIC X(12).
000764             20  cf-maximum-benefits        pic s999 comp-3.
000765             20  FILLER                     PIC X(09).
000766             20  CF-BENEFIT-CATEGORY        PIC X.
000767             20  CF-LOAN-TYPE               PIC X(8).
000768
000769             20  CF-CO-REM-TERM-CALC        PIC X.
000770                 88  CO-EARN-AFTER-15TH         VALUE '1'.
000771                 88  CO-EARN-ON-HALF-MO         VALUE '2'.
000772                 88  CO-EARN-ON-1ST-DAY         VALUE '3'.
000773                 88  CO-EARN-ON-FULL-MO         VALUE '4'.
000774                 88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
000775
000776             20  CF-CO-EARNINGS-CALC        PIC X.
000777                 88  CO-EARN-BY-R78             VALUE '1'.
000778                 88  CO-EARN-BY-PRO-RATA        VALUE '2'.
000779                 88  CO-EARN-AS-CALIF           VALUE '3'.
000780                 88  CO-EARN-AS-TEXAS           VALUE '4'.
000781                 88  CO-EARN-IS-NET-PAY         VALUE '5'.
000782                 88  CO-EARN-ANTICIPATION       VALUE '6'.
000783                 88  CO-EARN-AS-MEAN            VALUE '8'.
000784                 88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
000785
000786             20  CF-CO-REFUND-CALC          PIC X.
000787                 88  CO-REFUND-NOT-USED         VALUE SPACE.
000788                 88  CO-REFD-BY-R78             VALUE '1'.
000789                 88  CO-REFD-BY-PRO-RATA        VALUE '2'.
000790                 88  CO-REFD-AS-CALIF           VALUE '3'.
000791                 88  CO-REFD-AS-TEXAS           VALUE '4'.
000792                 88  CO-REFD-IS-NET-PAY         VALUE '5'.
000793                 88  CO-REFD-ANTICIPATION       VALUE '6'.
000794                 88  CO-REFD-MEAN               VALUE '8'.
000795                 88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
000796                 88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
000797                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
000798
000799             20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
000800                 88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
000801                 88  CO-OVRD-BY-R78             VALUE '1'.
000802                 88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
000803                 88  CO-OVRD-AS-CALIF           VALUE '3'.
000804                 88  CO-OVRD-AS-TEXAS           VALUE '4'.
000805                 88  CO-OVRD-IS-NET-PAY         VALUE '5'.
000806                 88  CO-OVRD-ANTICIPATION       VALUE '6'.
000807                 88  CO-OVRD-MEAN               VALUE '8'.
000808                 88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
000809
000810             20  CF-CO-BEN-I-G-CD           PIC X.
000811                 88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
000812                 88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
000813                 88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
000814
000815         16  FILLER                         PIC X(304).
000816
000817
000818****************************************************************
000819*             CARRIER MASTER RECORD                            *
000820****************************************************************
000821
000822     12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000823         16  CF-ADDRESS-DATA.
000824             20  CF-MAIL-TO-NAME            PIC X(30).
000825             20  CF-IN-CARE-OF              PIC X(30).
000826             20  CF-ADDRESS-LINE-1          PIC X(30).
000827             20  CF-ADDRESS-LINE-2          PIC X(30).
000828             20  CF-CITY-STATE              PIC X(30).
000829             20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
000830             20  CF-PHONE-NO                PIC 9(11)     COMP-3.
000831
000832         16  CF-CLAIM-NO-CONTROL.
000833             20  CF-CLAIM-NO-METHOD         PIC X.
000834                 88  CLAIM-NO-MANUAL            VALUE '1'.
000835                 88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
000836                 88  CLAIM-NO-SEQ               VALUE '3'.
000837                 88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
000838             20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
000839                 88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
000840                 88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
000841                 88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
000842
000843         16  CF-CHECK-NO-CONTROL.
000844             20  CF-CHECK-NO-METHOD         PIC X.
000845                 88  CHECK-NO-MANUAL            VALUE '1'.
000846                 88  CHECK-NO-AUTO-SEQ          VALUE '2'.
000847                 88  CHECK-NO-CARR-SEQ          VALUE '3'.
000848                 88  CHECK-NO-AT-PRINT          VALUE '4'.
000849             20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
000850                 88  CHECK-CNT-RESET-VALUE      VALUE +999999.
000851
000852         16  CF-DOMICILE-STATE              PIC XX.
000853
000854         16  CF-EXPENSE-CONTROLS.
000855             20  CF-EXPENSE-METHOD          PIC X.
000856                 88  EXPENSE-CALC-MANUAL        VALUE '1'.
000857                 88  DOLLARS-PER-PMT            VALUE '2'.
000858                 88  PERCENT-OF-PAYMENT         VALUE '3'.
000859                 88  DOLLARS-PER-MONTH          VALUE '4'.
000860             20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
000861             20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
000862
000863         16  CF-CORRESPONDENCE-CONTROL.
000864             20  CF-LETTER-RESEND-OPT       PIC X.
000865                 88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
000866                 88  LETTERS-ARE-ARCHIVED       VALUE '1'.
000867             20  FILLER                     PIC X(4).
000868
000869         16  CF-RESERVE-CONTROLS.
000870             20  CF-MANUAL-SW               PIC X.
000871                 88  CF-MANUAL-RESERVES-USED    VALUE '1'.
000872             20  CF-FUTURE-SW               PIC X.
000873                 88  CF-FUTURE-RESERVES-USED    VALUE '1'.
000874             20  CF-PTC-SW                  PIC X.
000875                 88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
000876             20  CF-IBNR-SW                 PIC X.
000877                 88  CF-IBNR-RESERVES-USED      VALUE '1'.
000878             20  CF-PTC-LF-SW               PIC X.
000879                 88  CF-LF-PTC-USED             VALUE '1'.
000880             20  CF-CDT-ACCESS-METHOD       PIC X.
000881                 88  CF-CDT-ROUND-NEAR          VALUE '1'.
000882                 88  CF-CDT-ROUND-HIGH          VALUE '2'.
000883                 88  CF-CDT-INTERPOLATED        VALUE '3'.
000884             20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
000885
000886         16  CF-CLAIM-CALC-METHOD           PIC X.
000887             88  360-PLUS-MONTHS                VALUE '1'.
000888             88  365-PLUS-MONTHS                VALUE '2'.
000889             88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
000890             88  360-DAILY                      VALUE '4'.
000891             88  365-DAILY                      VALUE '5'.
000892
000893         16  CF-LAST-ALPHA-CHARACTER        PIC X.
000894         16  FILLER                         PIC X(11).
000895
000896         16  CF-LIMIT-AMOUNTS.
000897             20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
000898             20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
000899             20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
000900             20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
000901             20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
000902             20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
000903             20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
000904             20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
000905             20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
000906             20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
000907
000908         16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
000909         16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
000910         16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
000911
000912         16  CF-ZIP-CODE.
000913             20  CF-ZIP-PRIME.
000914                 24  CF-ZIP-1ST             PIC X.
000915                     88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000916                 24  FILLER                 PIC X(4).
000917             20  CF-ZIP-PLUS4               PIC X(4).
000918         16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
000919             20  CF-CAN-POSTAL-1            PIC XXX.
000920             20  CF-CAN-POSTAL-2            PIC XXX.
000921             20  FILLER                     PIC XXX.
000922
000923         16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
000924         16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
000925         16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
000926
000927         16  CF-RATING-SWITCH               PIC X.
000928             88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
000929             88  CF-NO-RATING                   VALUE 'N'.
000930
000931         16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
000932
000933         16  CF-CARRIER-OVER-SHORT.
000934             20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
000935             20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000936
000937         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
000938         16  CF-SECPAY-SWITCH               PIC X.
000939             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
000940             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
000941         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
000942         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
000943         16  FILLER                         PIC X(444).
000944*        16  FILLER                         PIC X(452).
000945
000946
000947****************************************************************
000948*             MORTALITY MASTER RECORD                          *
000949****************************************************************
000950
000951     12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
000952         16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
000953                                INDEXED BY CF-MORT-NDX.
000954             20  CF-MORT-TABLE              PIC X(5).
000955             20  CF-MORT-TABLE-TYPE         PIC X.
000956                 88  CF-MORT-JOINT              VALUE 'J'.
000957                 88  CF-MORT-SINGLE             VALUE 'S'.
000958                 88  CF-MORT-COMBINED           VALUE 'C'.
000959                 88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
000960                 88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
000961             20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
000962             20  CF-MORT-AGE-METHOD         PIC XX.
000963                 88  CF-AGE-LAST                VALUE 'AL'.
000964                 88  CF-AGE-NEAR                VALUE 'AN'.
000965             20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
000966             20  CF-MORT-ADJUSTMENT-DIRECTION
000967                                            PIC X.
000968                 88  CF-MINUS                   VALUE '-'.
000969                 88  CF-PLUS                    VALUE '+'.
000970             20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
000971             20  CF-MORT-JOINT-CODE         PIC X.
000972                 88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
000973             20  CF-MORT-PC-Q               PIC X.
000974                 88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
000975             20  CF-MORT-TABLE-CODE         PIC X(4).
000976             20  CF-MORT-COMMENTS           PIC X(15).
000977             20  FILLER                     PIC X(14).
000978
000979         16  FILLER                         PIC X(251).
000980
000981
000982****************************************************************
000983*             BUSSINESS TYPE MASTER RECORD                     *
000984****************************************************************
000985
000986     12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
000987* FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
000988* RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
000989* AND RECORD 05 IS TYPES 81-99
000990         16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
000991             20  CF-BUSINESS-TITLE          PIC  X(19).
000992             20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
000993                                            PIC S9V9(4) COMP-3.
000994             20  CF-BUS-EXCL-ST-CALL        PIC  X.
000995             20  FILLER                     PIC  X.
000996         16  FILLER                         PIC  X(248).
000997
000998
000999****************************************************************
001000*             TERMINAL MASTER RECORD                           *
001001****************************************************************
001002
001003     12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001004
001005         16  CF-COMPANY-TERMINALS.
001006             20  CF-TERMINAL-ID  OCCURS 120 TIMES
001007                                  PIC X(4).
001008         16  FILLER               PIC X(248).
001009
001010
001011****************************************************************
001012*             LIFE EDIT MASTER RECORD                          *
001013****************************************************************
001014
001015     12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001016         16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
001017             20  CF-LIFE-CODE-IN            PIC XX.
001018             20  CF-LIFE-CODE-OUT           PIC XX.
001019         16  FILLER                         PIC X(248).
001020
001021
001022****************************************************************
001023*             AH EDIT MASTER RECORD                            *
001024****************************************************************
001025
001026     12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001027         16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
001028             20  CF-AH-CODE-IN              PIC XXX.
001029             20  CF-AH-CODE-OUT             PIC XX.
001030         16  FILLER                         PIC X(248).
001031
001032
001033****************************************************************
001034*             CREDIBILITY TABLES                               *
001035****************************************************************
001036
001037     12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
001038         16  CF-CRDB-ENTRY   OCCURS 36 TIMES
001039                             INDEXED BY CF-CRDB-NDX.
001040             20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
001041             20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
001042             20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
001043         16  FILLER                         PIC  X(332).
001044
001045
001046****************************************************************
001047*             REPORT CUSTOMIZATION RECORD                      *
001048****************************************************************
001049
001050     12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
001051         16  CF-ACCOUNT-MASTER-STATUS       PIC X.
001052             88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
001053             88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
001054             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
001055**** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
001056****       A T-TRANSFER.                                   ****
001057             88  CF-ALL-ACCOUNTS                VALUE 'B'.
001058
001059         16  FILLER                         PIC XX.
001060
001061         16  CF-CARRIER-CNTL-OPT.
001062             20  CF-CARRIER-OPT-SEQ         PIC 9.
001063                 88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
001064                 88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
001065             20  CF-CARRIER-SELECT OCCURS 3 TIMES
001066                                            PIC X.
001067         16  CF-GROUP-CNTL-OPT.
001068             20  CF-GROUP-OPT-SEQ           PIC 9.
001069                 88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
001070                 88  CF-GROUP-OPT-NOT-USED      VALUE 0.
001071             20  CF-GROUP-SELECT OCCURS 3 TIMES
001072                                            PIC X(6).
001073         16  CF-STATE-CNTL-OPT.
001074             20  CF-STATE-OPT-SEQ           PIC 9.
001075                 88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
001076                 88  CF-STATE-OPT-NOT-USED      VALUE 0.
001077             20  CF-STATE-SELECT OCCURS 3 TIMES
001078                                            PIC XX.
001079         16  CF-ACCOUNT-CNTL-OPT.
001080             20  CF-ACCOUNT-OPT-SEQ         PIC 9.
001081                 88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
001082                 88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
001083             20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
001084                                            PIC X(10).
001085         16  CF-BUS-TYP-CNTL-OPT.
001086             20  CF-BUS-TYP-OPT-SEQ         PIC 9.
001087                 88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
001088                 88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
001089             20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
001090                                            PIC XX.
001091         16  CF-LF-TYP-CNTL-OPT.
001092             20  CF-LF-TYP-OPT-SEQ          PIC 9.
001093                 88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
001094                 88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
001095             20  CF-BUS-LF-SELECT OCCURS 3 TIMES
001096                                            PIC XX.
001097         16  CF-AH-TYP-CNTL-OPT.
001098             20  CF-AH-TYP-OPT-SEQ          PIC 9.
001099                 88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
001100                 88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
001101             20  CF-BUS-AH-SELECT OCCURS 3 TIMES
001102                                            PIC XX.
001103         16  CF-REPTCD1-CNTL-OPT.
001104             20  CF-REPTCD1-OPT-SEQ         PIC 9.
001105                 88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
001106                 88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
001107             20  CF-REPTCD1-SELECT OCCURS 3 TIMES
001108                                            PIC X(10).
001109         16  CF-REPTCD2-CNTL-OPT.
001110             20  CF-REPTCD2-OPT-SEQ         PIC 9.
001111                 88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
001112                 88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
001113             20  CF-REPTCD2-SELECT OCCURS 3 TIMES
001114                                            PIC X(10).
001115         16  CF-USER1-CNTL-OPT.
001116             20  CF-USER1-OPT-SEQ           PIC 9.
001117                 88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
001118                 88  CF-USER1-OPT-NOT-USED      VALUE 0.
001119             20  CF-USER1-SELECT OCCURS 3 TIMES
001120                                            PIC X(10).
001121         16  CF-USER2-CNTL-OPT.
001122             20  CF-USER2-OPT-SEQ           PIC 9.
001123                 88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
001124                 88  CF-USER2-OPT-NOT-USED      VALUE 0.
001125             20  CF-USER2-SELECT OCCURS 3 TIMES
001126                                            PIC X(10).
001127         16  CF-USER3-CNTL-OPT.
001128             20  CF-USER3-OPT-SEQ           PIC 9.
001129                 88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
001130                 88  CF-USER3-OPT-NOT-USED      VALUE 0.
001131             20  CF-USER3-SELECT OCCURS 3 TIMES
001132                                            PIC X(10).
001133         16  CF-USER4-CNTL-OPT.
001134             20  CF-USER4-OPT-SEQ           PIC 9.
001135                 88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
001136                 88  CF-USER4-OPT-NOT-USED      VALUE 0.
001137             20  CF-USER4-SELECT OCCURS 3 TIMES
001138                                            PIC X(10).
001139         16  CF-USER5-CNTL-OPT.
001140             20  CF-USER5-OPT-SEQ           PIC 9.
001141                 88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
001142                 88  CF-USER5-OPT-NOT-USED      VALUE 0.
001143             20  CF-USER5-SELECT OCCURS 3 TIMES
001144                                            PIC X(10).
001145         16  CF-REINS-CNTL-OPT.
001146             20  CF-REINS-OPT-SEQ           PIC 9.
001147                 88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
001148                 88  CF-REINS-OPT-NOT-USED      VALUE 0.
001149             20  CF-REINS-SELECT OCCURS 3 TIMES.
001150                 24  CF-REINS-PRIME         PIC XXX.
001151                 24  CF-REINS-SUB           PIC XXX.
001152
001153         16  CF-AGENT-CNTL-OPT.
001154             20  CF-AGENT-OPT-SEQ           PIC 9.
001155                 88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
001156                 88  CF-AGENT-OPT-NOT-USED      VALUE 0.
001157             20  CF-AGENT-SELECT OCCURS 3 TIMES
001158                                            PIC X(10).
001159
001160         16  FILLER                         PIC X(43).
001161
001162         16  CF-LOSS-RATIO-SELECT.
001163             20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
001164             20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
001165         16  CF-ENTRY-DATE-SELECT.
001166             20  CF-SEL-LO-ENTRY-DATE       PIC XX.
001167             20  CF-SEL-HI-ENTRY-DATE       PIC XX.
001168         16  CF-EFFECTIVE-DATE-SELECT.
001169             20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
001170             20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
001171
001172         16  CF-EXCEPTION-LIST-IND          PIC X.
001173             88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
001174
001175         16  FILLER                         PIC X(318).
001176
001177****************************************************************
001178*                  EXCEPTION REPORTING RECORD                  *
001179****************************************************************
001180
001181     12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
001182         16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
001183             88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
001184
001185         16  CF-COMBINED-LIFE-AH-OPT.
001186             20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
001187             20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
001188             20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
001189             20  CF-CANCELLATION-RATIO      PIC S9(02).
001190
001191         16  CF-LIFE-OPT.
001192             20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001193             20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001194             20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001195             20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001196             20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001197             20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001198             20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001199             20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001200             20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001201             20  CF-LF-AVG-AGE-MAX          PIC S9(02).
001202
001203         16  CF-AH-OPT.
001204             20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001205             20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001206             20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001207             20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001208             20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001209             20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001210             20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001211             20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001212             20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001213             20  CF-AH-AVG-AGE-MAX          PIC S9(02).
001214
001215         16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
001216             88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
001217             88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
001218             88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
001219
001220         16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
001221
001222         16  FILLER                         PIC X(673).
001223
001224
001225****************************************************************
001226*             MORTGAGE SYSTEM PLAN RECORD                      *
001227****************************************************************
001228
001229     12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
001230         16  CF-PLAN-TYPE                   PIC X.
001231             88  CF-LIFE-MORT-PLAN             VALUE 'L'.
001232             88  CF-DISAB-MORT-PLAN            VALUE 'D'.
001233             88  CF-AD-D-MORT-PLAN             VALUE 'A'.
001234         16  CF-PLAN-ABBREV                 PIC XXX.
001235         16  CF-PLAN-DESCRIPT               PIC X(10).
001236         16  CF-PLAN-NOTES                  PIC X(20).
001237         16  CF-PLAN-ESTABLISH-DATE         PIC XX.
001238         16  CF-PLAN-UNDERWRITING.
001239             20  CF-PLAN-TERM-DATA.
001240                 24  CF-MINIMUM-TERM        PIC S999      COMP-3.
001241                 24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
001242             20  CF-PLAN-AGE-DATA.
001243                 24  CF-MINIMUM-AGE         PIC S999      COMP-3.
001244                 24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
001245                 24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
001246             20  CF-PLAN-BENEFIT-DATA.
001247                 24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001248                 24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001249                 24  CF-MAXIMUM-MONTHLY-BENEFIT
001250                                            PIC S9(7)V99  COMP-3.
001251         16  CF-PLAN-POLICY-FORMS.
001252             20  CF-POLICY-FORM             PIC X(12).
001253             20  CF-MASTER-APPLICATION      PIC X(12).
001254             20  CF-MASTER-POLICY           PIC X(12).
001255         16  CF-PLAN-RATING.
001256             20  CF-RATE-CODE               PIC X(5).
001257             20  CF-SEX-RATING              PIC X.
001258                 88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
001259                 88  CF-PLAN-SEX-RATED         VALUE '2'.
001260             20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
001261             20  CF-SUB-STD-TYPE            PIC X.
001262                 88  CF-PCT-OF-PREM            VALUE '1'.
001263                 88  CF-PCT-OF-BENE            VALUE '2'.
001264         16  CF-PLAN-PREM-TOLERANCES.
001265             20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
001266             20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
001267         16  CF-PLAN-PYMT-TOLERANCES.
001268             20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
001269             20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
001270         16  CF-PLAN-MISC-DATA.
001271             20  FILLER                     PIC X.
001272             20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
001273             20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
001274         16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
001275         16  CF-PLAN-IND-GRP                PIC X.
001276             88  CF-MORT-INDIV-PLAN            VALUE 'I'
001277                                                     '1'.
001278             88  CF-MORT-GROUP-PLAN            VALUE 'G'
001279                                                     '2'.
001280         16  CF-MIB-SEARCH-SW               PIC X.
001281             88  CF-MIB-SEARCH-ALL             VALUE '1'.
001282             88  CF-MIB-SEARCH-NONE            VALUE '2'.
001283             88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
001284             88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
001285         16  CF-ALPHA-SEARCH-SW             PIC X.
001286             88  CF-MIB-ALPHA-ALL              VALUE '1'.
001287             88  CF-MIB-ALPHA-NONE             VALUE '2'.
001288             88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
001289             88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
001290             88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
001291             88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
001292             88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
001293             88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
001294             88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
001295             88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
001296                                                     'A' 'B' 'C'
001297                                                     'X' 'Y' 'Z'.
001298         16  CF-EFF-DT-RULE-SW              PIC X.
001299             88  CF-EFF-DT-ENTER               VALUE 'E'.
001300             88  CF-EFF-DT-MONTH               VALUE 'M'.
001301             88  CF-EFF-DT-QTR                 VALUE 'Q'.
001302             88  CF-EFF-DT-SEMI                VALUE 'S'.
001303             88  CF-EFF-DT-ANN                 VALUE 'A'.
001304         16  FILLER                         PIC X(4).
001305         16  CF-HEALTH-QUESTIONS            PIC X.
001306             88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
001307         16  CF-GRACE-PERIOD                PIC S999      COMP-3.
001308         16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
001309         16  CF-PLAN-SNGL-JNT               PIC X.
001310             88  CF-COMBINED-PLAN              VALUE 'C'.
001311             88  CF-JNT-PLAN                   VALUE 'J'.
001312             88  CF-SNGL-PLAN                  VALUE 'S'.
001313         16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
001314         16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
001315         16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
001316         16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
001317         16  CF-RERATE-CNTL                 PIC  X.
001318             88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
001319             88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
001320             88  CF-DO-NOT-RERATE               VALUE '3' ' '.
001321             88  CF-AUTO-RECALC                 VALUE '4'.
001322         16  CF-BENEFIT-TYPE                PIC  X.
001323             88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
001324             88  CF-BENEFIT-REDUCES             VALUE '2'.
001325         16  CF-POLICY-FEE                  PIC S999V99
001326                                                    COMP-3.
001327         16  CF-1ST-NOTICE-FORM             PIC  X(04).
001328         16  CF-2ND-NOTICE-FORM             PIC  X(04).
001329         16  CF-3RD-NOTICE-FORM             PIC  X(04).
001330         16  CF-4TH-NOTICE-FORM             PIC  X(04).
001331         16  FILLER                         PIC  X(32).
001332         16  CF-TERMINATION-FORM            PIC  X(04).
001333         16  FILLER                         PIC  X(08).
001334         16  CF-CLAIM-CAP                   PIC S9(7)V99
001335                                                       COMP-3.
001336         16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
001337         16  CF-ISSUE-LETTER                PIC  X(4).
001338         16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
001339         16  CF-DEPENDENT-COVERAGE          PIC  X.
001340             88  CF-YES-DEP-COV                 VALUE 'Y'.
001341             88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
001342         16  CF-MP-REFUND-CALC              PIC X.
001343             88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
001344             88  CF-MP-REFD-BY-R78              VALUE '1'.
001345             88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
001346             88  CF-MP-REFD-AS-CALIF            VALUE '3'.
001347             88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
001348             88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
001349             88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
001350             88  CF-MP-REFD-MEAN                VALUE '8'.
001351         16  CF-ALT-RATE-CODE               PIC  X(5).
001352
001353
001354         16  FILLER                         PIC X(498).
001355****************************************************************
001356*             MORTGAGE COMPANY MASTER RECORD                   *
001357****************************************************************
001358
001359     12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001360         16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
001361         16  CF-MORTG-ACCESS-CONTROL        PIC X.
001362             88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
001363             88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
001364             88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
001365             88  CF-MORT-PROD-CNTL                   VALUE '3'.
001366             88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
001367
001368         16  CF-MORTG-CONVERSION-DATE       PIC XX.
001369         16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
001370         16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
001371         16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
001372         16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
001373
001374         16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
001375             88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
001376         16  CF-MP-RECON-USE-IND            PIC X(1).
001377             88  CF-MP-USE-RECON             VALUE 'Y'.
001378         16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
001379             88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
001380         16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
001381             88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
001382             88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
001383         16  FILLER                         PIC X(1).
001384         16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
001385             88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
001386         16  CF-MORTG-MIB-VERSION           PIC X.
001387             88  CF-MORTG-MIB-BATCH         VALUE '1'.
001388             88  CF-MORTG-MIB-ONLINE        VALUE '2'.
001389             88  CF-MORTG-MIB-BOTH          VALUE '3'.
001390         16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
001391             20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
001392                 88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
001393             20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
001394                 88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
001395             20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
001396                 88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
001397             20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
001398                 88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
001399             20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
001400                 88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
001401             20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
001402                 88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
001403         16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
001404         16  FILLER                         PIC X(7).
001405         16  CF-MORTG-DESTINATION-SYMBOL.
001406             20  CF-MORTG-MIB-COMM          PIC X(5).
001407             20  CF-MORTG-MIB-TERM          PIC X(5).
001408         16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
001409             88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
001410         16  FILLER                         PIC X(03).
001411         16  CF-MP-CHECK-NO-CONTROL.
001412             20  CF-MP-CHECK-NO-METHOD      PIC X(01).
001413                 88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
001414                 88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
001415                                                ' ' LOW-VALUES.
001416                 88  CF-MP-CHECK-NO-PRE-PRINTED
001417                                               VALUE '3'.
001418         16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
001419         16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
001420         16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
001421             20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
001422                 88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
001423             20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
001424                 88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
001425             20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
001426                 88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
001427             20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
001428                 88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
001429             20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
001430                 88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
001431             20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
001432                 88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
001433         16  CF-MORTG-BILLING-AREA.
001434             20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
001435                                            PIC X.
001436         16  CF-MORTG-MONTH-END-DT          PIC XX.
001437         16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
001438         16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
001439         16  CF-MORTG-MIB-DEST-SW           PIC X.
001440             88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
001441             88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
001442         16  FILLER                         PIC X.
001443         16  CF-MORTG-LABEL-CONTROL         PIC X.
001444             88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
001445             88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
001446         16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
001447         16  FILLER                         PIC X(8).
001448         16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
001449         16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
001450         16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
001451         16  CF-ACH-COMPANY-ID.
001452             20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
001453                 88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
001454                 88  CF-ACH-ICD-DUNS                VALUE '3'.
001455                 88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
001456             20  CF-ACH-COMPANY-ID-NO       PIC X(9).
001457         16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
001458             88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
001459         16  CF-RATE-DEV-AUTHORIZATION      PIC X.
001460             88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
001461             88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
001462         16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
001463         16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
001464         16  FILLER                         PIC X(536).
001465
001466****************************************************************
001467*             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
001468****************************************************************
001469
001470     12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
001471         16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
001472             20  CF-FEMALE-HEIGHT.
001473                 24  CF-FEMALE-FT           PIC 99.
001474                 24  CF-FEMALE-IN           PIC 99.
001475             20  CF-FEMALE-MIN-WT           PIC 999.
001476             20  CF-FEMALE-MAX-WT           PIC 999.
001477         16  FILLER                         PIC X(428).
001478
001479     12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
001480         16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
001481             20  CF-MALE-HEIGHT.
001482                 24  CF-MALE-FT             PIC 99.
001483                 24  CF-MALE-IN             PIC 99.
001484             20  CF-MALE-MIN-WT             PIC 999.
001485             20  CF-MALE-MAX-WT             PIC 999.
001486         16  FILLER                         PIC X(428).
001487******************************************************************
001488*             AUTOMATIC ACTIVITY RECORD                          *
001489******************************************************************
001490     12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
001491         16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
001492             20  CF-SYS-ACTIVE-SW           PIC X(01).
001493             20  CF-SYS-LETTER-ID           PIC X(04).
001494             20  CF-SYS-RESEND-DAYS         PIC 9(03).
001495             20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
001496             20  CF-SYS-RESET-SW            PIC X(01).
001497             20  CF-SYS-REPORT-DAYS         PIC 9(03).
001498             20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
001499
001500         16  FILLER                         PIC X(50).
001501
001502         16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
001503             20  CF-USER-ACTIVE-SW          PIC X(01).
001504             20  CF-USER-LETTER-ID          PIC X(04).
001505             20  CF-USER-RESEND-DAYS        PIC 9(03).
001506             20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
001507             20  CF-USER-RESET-SW           PIC X(01).
001508             20  CF-USER-REPORT-DAYS        PIC 9(03).
001509             20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
001510             20  CF-USER-ACTIVITY-DESC      PIC X(20).
001511
001512         16  FILLER                         PIC X(246).
      *<<((file: ELCCNTL))
000652
000653*                            COPY ERCACCT.
      *>>((file: ERCACCT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACCT                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.031                          *
000007*                                                                *
000008*   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
000009*                                                                *
000010*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000011*   VSAM ACCOUNT MASTER FILES.                                   *
000012*                                                                *
000013*   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
000014*                                                                *
000015*   FILE TYPE = VSAM,KSDS                                        *
000016*   RECORD SIZE = 2000  RECFORM = FIX                            *
000017*                                                                *
000018*   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
000019*       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
000020*                                                                *
000021*   LOG = NO                                                     *
000022*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000023*                                                                *
000024*                                                                *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
000034* 092705    2005050300006  PEMA  ADD SPP LEASES
000035* 022808    2007083100002  PEMA  ADD FREEZE STATUS
000036* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000037* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
000038* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
000039* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
000040* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
000041******************************************************************
000042
000043 01  ACCOUNT-MASTER.
000044     12  AM-RECORD-ID                      PIC XX.
000045         88  VALID-AM-ID                      VALUE 'AM'.
000046
000047     12  AM-CONTROL-PRIMARY.
000048         16  AM-COMPANY-CD                 PIC X.
000049         16  AM-MSTR-CNTRL.
000050             20  AM-CONTROL-A.
000051                 24  AM-CARRIER            PIC X.
000052                 24  AM-GROUPING.
000053                     28 AM-GROUPING-PREFIX PIC XXX.
000054                     28 AM-GROUPING-PRIME  PIC XXX.
000055                 24  AM-STATE              PIC XX.
000056                 24  AM-ACCOUNT.
000057                     28  AM-ACCOUNT-PREFIX PIC X(4).
000058                     28  AM-ACCOUNT-PRIME  PIC X(6).
000059             20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
000060                                           PIC X(19).
000061             20  AM-CNTRL-B.
000062                 24  AM-EXPIRATION-DT      PIC XX.
000063                 24  FILLER                PIC X(4).
000064             20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
000065                 24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
000066
000067     12  AM-CONTROL-BY-VAR-GRP.
000068         16  AM-COMPANY-CD-A1              PIC X.
000069         16  AM-VG-CARRIER                 PIC X.
000070         16  AM-VG-GROUPING                PIC X(6).
000071         16  AM-VG-STATE                   PIC XX.
000072         16  AM-VG-ACCOUNT                 PIC X(10).
000073         16  AM-VG-DATE.
000074             20  AM-VG-EXPIRATION-DT       PIC XX.
000075             20  FILLER                    PIC X(4).
000076         16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
000077                                           PIC 9(11)      COMP-3.
000078     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
000079         16  FILLER                        PIC X(10).
000080         16  AM-VG-KEY3.
000081             20  AM-VG3-ACCOUNT            PIC X(10).
000082             20  AM-VG3-EXP-DT             PIC XX.
000083         16  FILLER                        PIC X(4).
000084     12  AM-MAINT-INFORMATION.
000085         16  AM-LAST-MAINT-DT              PIC XX.
000086         16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000087         16  AM-LAST-MAINT-USER            PIC X(4).
000088         16  FILLER                        PIC XX.
000089
000090     12  AM-EFFECTIVE-DT                   PIC XX.
000091     12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
000092
000093     12  AM-PREV-DATES  COMP-3.
000094         16  AM-PREV-EXP-DT                PIC 9(11).
000095         16  AM-PREV-EFF-DT                PIC 9(11).
000096
000097     12  AM-REPORT-CODE-1                  PIC X(10).
000098     12  AM-REPORT-CODE-2                  PIC X(10).
000099
000100     12  AM-CITY-CODE                      PIC X(4).
000101     12  AM-COUNTY-PARISH                  PIC X(6).
000102
000103     12  AM-NAME                           PIC X(30).
000104     12  AM-PERSON                         PIC X(30).
000105     12  AM-ADDRS                          PIC X(30).
000106     12  AM-CITY.
000107         16  AM-ADDR-CITY                  PIC X(28).
000108         16  AM-ADDR-STATE                 PIC XX.
000109     12  AM-ZIP.
000110         16  AM-ZIP-PRIME.
000111             20  AM-ZIP-PRI-1ST            PIC X.
000112                 88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000113             20  FILLER                    PIC X(4).
000114         16  AM-ZIP-PLUS4                  PIC X(4).
000115     12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
000116         16  AM-CAN-POSTAL-1               PIC XXX.
000117         16  AM-CAN-POSTAL-2               PIC XXX.
000118         16  FILLER                        PIC XXX.
000119     12  AM-TEL-NO.
000120         16  AM-AREA-CODE                  PIC 999.
000121         16  AM-TEL-PRE                    PIC 999.
000122         16  AM-TEL-NBR                    PIC 9(4).
000123     12  AM-TEL-LOC                        PIC X.
000124         88  AM-TEL-AT-HOME                   VALUE 'H'.
000125         88  AM-TEL-AT-BUSINESS               VALUE 'B'.
000126
000127     12  AM-COMM-STRUCTURE.
000128         16  AM-DEFN-1.
000129             20  AM-AGT-COMMS       OCCURS 10 TIMES.
000130                 24  AM-AGT.
000131                     28  AM-AGT-PREFIX     PIC X(4).
000132                     28  AM-AGT-PRIME      PIC X(6).
000133                 24  AM-COM-TYP            PIC X.
000134                 24  AM-L-COM              PIC SV9(5)     COMP-3.
000135                 24  AM-J-COM              PIC SV9(5)     COMP-3.
000136                 24  AM-A-COM              PIC SV9(5)     COMP-3.
000137                 24  AM-RECALC-LV-INDIC    PIC X.
000138                 24  AM-RETRO-LV-INDIC     PIC X.
000139                 24  AM-GL-CODES           PIC X.
000140                 24  AM-COMM-CHARGEBACK    PIC 9(02).
000141                 24  FILLER                PIC X(01).
000142         16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
000143             20  AM-COM-TBLS        OCCURS 10 TIMES.
000144                 24  FILLER                PIC X(11).
000145                 24  AM-L-COMA             PIC XXX.
000146                 24  AM-J-COMA             PIC XXX.
000147                 24  AM-A-COMA             PIC XXX.
000148                 24  FILLER                PIC X(6).
000149
000150     12  AM-COMM-CHANGE-STATUS             PIC X.
000151         88  AM-COMMISSIONS-CHANGED           VALUE '*'.
000152
000153     12  AM-CSR-CODE                       PIC X(4).
000154
000155     12  AM-BILLING-STATUS                 PIC X.
000156         88  AM-ACCOUNT-BILLED                VALUE 'B'.
000157         88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
000158     12  AM-AUTO-REFUND-SW                 PIC X.
000159         88  AUTO-REFUNDS-USED                VALUE 'Y'.
000160         88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
000161     12  AM-GPCD                           PIC 99.
000162     12  AM-IG                             PIC X.
000163         88  AM-HAS-INDIVIDUAL                VALUE '1'.
000164         88  AM-HAS-GROUP                     VALUE '2'.
000165     12  AM-STATUS                         PIC X.
000166         88  AM-ACCOUNT-ACTIVE                VALUE '0'.
000167         88  AM-ACCOUNT-INACTIVE              VALUE '1'.
000168         88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
000169         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
000170         88  AM-ACCOUNT-FROZEN                VALUE '4'.
000171         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
000172         88  AM-ACCOUNT-DROPPED               VALUE '6'.
000173         88  AM-ACCOUNT-LAPSED                VALUE '7'.
000174         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
000175         88  AM-ACCOUNT-PENDING               VALUE '9'.
000176     12  AM-REMIT-TO                       PIC 99.
000177     12  AM-ID-NO                          PIC X(11).
000178
000179     12  AM-CAL-TABLE                      PIC XX.
000180     12  AM-LF-DEVIATION                   PIC XXX.
000181     12  AM-AH-DEVIATION                   PIC XXX.
000182     12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000183     12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000184     12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
000185     12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
000186     12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000187     12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000188
000189     12  AM-USER-FIELDS.
000190         16  AM-FLD-1                      PIC XX.
000191         16  AM-FLD-2                      PIC XX.
000192         16  AM-FLD-3                      PIC XX.
000193         16  AM-FLD-4                      PIC XX.
000194         16  AM-FLD-5                      PIC XX.
000195
000196     12  AM-1ST-PROD-DATE.
000197         16  AM-1ST-PROD-YR                PIC XX.
000198         16  AM-1ST-PROD-MO                PIC XX.
000199         16  AM-1ST-PROD-DA                PIC XX.
000200     12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
000201     12  AM-CERTS-PURGED-DATE.
000202         16  AM-PUR-YR                     PIC XX.
000203         16  AM-PUR-MO                     PIC XX.
000204         16  AM-PUR-DA                     PIC XX.
000205     12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
000206     12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
000207     12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
000208     12  AM-INACTIVE-DATE.
000209         16  AM-INA-MO                     PIC 99.
000210         16  AM-INA-DA                     PIC 99.
000211         16  AM-INA-YR                     PIC 99.
000212     12  AM-AR-HI-CERT-DATE                PIC XX.
000213
000214     12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000215     12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000216
000217     12  AM-OB-PAYMENT-MODE                PIC X.
000218         88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
000219         88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
000220         88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
000221         88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
000222
000223     12  AM-AH-ONLY-INDICATOR              PIC X.
000224         88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
000225         88  AM-NO-AH-ONLY                    VALUE 'N'.
000226
000227     12  AM-EDIT-LOAN-OFC                  PIC X(01).
000228
000229     12  AM-OVER-SHORT.
000230         16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
000231         16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
000232
000233     12  AM-DCC-PRODUCT-CODE               PIC XXX.
000234     12  AM-DCC-CLP-STATE                  PIC XX.
000235
000236     12  AM-RECALC-COMM                    PIC X.
000237     12  AM-RECALC-REIN                    PIC X.
000238
000239     12  AM-REI-TABLE                      PIC XXX.
000240     12  AM-REI-ET-LF                      PIC X.
000241     12  AM-REI-ET-AH                      PIC X.
000242     12  AM-REI-PE-LF                      PIC X.
000243     12  AM-REI-PE-AH                      PIC X.
000244     12  AM-REI-PRT-ST                     PIC X.
000245     12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
000246     12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
000247     12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
000248     12  AM-REI-GROUP-A                    PIC X(6).
000249     12  AM-REI-MORT                       PIC X(4).
000250     12  AM-REI-PRT-OW                     PIC X.
000251     12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
000252     12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
000253     12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
000254     12  AM-REI-GROUP-B                    PIC X(6).
000255
000256     12  AM-TRUST-TYPE                     PIC X(2).
000257
000258     12  AM-EMPLOYER-STMT-USED             PIC X.
000259     12  AM-GROUPED-CHECKS-Y-N             PIC X.
000260
000261     12  AM-STD-AH-TYPE                    PIC XX.
000262     12  AM-EARN-METHODS.
000263         16  AM-EARN-METHOD-R              PIC X.
000264             88 AM-REF-RL-R78                 VALUE 'R'.
000265             88 AM-REF-RL-PR                  VALUE 'P'.
000266             88 AM-REF-RL-MEAN                VALUE 'M'.
000267             88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
000268         16  AM-EARN-METHOD-L              PIC X.
000269             88 AM-REF-LL-R78                 VALUE 'R'.
000270             88 AM-REF-LL-PR                  VALUE 'P'.
000271             88 AM-REF-LL-MEAN                VALUE 'M'.
000272             88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
000273         16  AM-EARN-METHOD-A              PIC X.
000274             88 AM-REF-AH-R78                 VALUE 'R'.
000275             88 AM-REF-AH-PR                  VALUE 'P'.
000276             88 AM-REF-AH-MEAN                VALUE 'M'.
000277             88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
000278             88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
000279             88 AM-REF-AH-NET                 VALUE 'N'.
000280
000281     12  AM-TOL-PREM                       PIC S999V99    COMP-3.
000282     12  AM-TOL-REF                        PIC S999V99    COMP-3.
000283     12  AM-TOL-CLM                        PIC S999V99    COMP-3.
000284
000285     12  AM-RET-Y-N                        PIC X.
000286     12  AM-RET-P-E                        PIC X.
000287     12  AM-LF-RET                         PIC S9V9999    COMP-3.
000288     12  AM-AH-RET                         PIC S9V9999    COMP-3.
000289     12  AM-RET-GRP                        PIC X(6).
000290     12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
000291         16  AM-POOL-PRIME                 PIC XXX.
000292         16  AM-POOL-SUB                   PIC XXX.
000293     12  AM-RETRO-EARNINGS.
000294         16  AM-RET-EARN-R                 PIC X.
000295         16  AM-RET-EARN-L                 PIC X.
000296         16  AM-RET-EARN-A                 PIC X.
000297     12  AM-RET-ST-TAX-USE                 PIC X.
000298         88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
000299         88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
000300     12  AM-RETRO-BEG-EARNINGS.
000301         16  AM-RET-BEG-EARN-R             PIC X.
000302         16  AM-RET-BEG-EARN-L             PIC X.
000303         16  AM-RET-BEG-EARN-A             PIC X.
000304     12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
000305     12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
000306
000307     12  AM-USER-SELECT-OPTIONS.
000308         16  AM-USER-SELECT-1              PIC X(10).
000309         16  AM-USER-SELECT-2              PIC X(10).
000310         16  AM-USER-SELECT-3              PIC X(10).
000311         16  AM-USER-SELECT-4              PIC X(10).
000312         16  AM-USER-SELECT-5              PIC X(10).
000313
000314     12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000315
000316     12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000317
000318     12  AM-RPT045A-SWITCH                 PIC X.
000319         88  RPT045A-OFF                   VALUE 'N'.
000320
000321     12  AM-INSURANCE-LIMITS.
000322         16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
000323         16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
000324
000325     12  AM-PROFILE-CHANGE-SWITCH          PIC X.
000326         88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
000327
000328     12  AM-DISMBR-COVERAGE-SW             PIC X.
000329         88  AM-DISMBR-COVERAGE               VALUE 'Y'.
000330         88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
000331
000332     12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
000333
000334     12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
000335     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
000336     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
000337     12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
000338     12  AM-DCC-UEF-STATE                  PIC XX.
000339     12  FILLER                            PIC XXX.
000340     12  AM-REPORT-CODE-3                  PIC X(10).
000341*    12  FILLER                            PIC X(22).
000342
000343     12  AM-RESERVE-DATE.
000344         16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
000345         16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
000346         16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
000347
000348     12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
000349     12  AM-NOTIFICATION-TYPES.
000350         16  AM-NOTIF-OF-LETTERS           PIC X.
000351         16  AM-NOTIF-OF-PAYMENTS          PIC X.
000352         16  AM-NOTIF-OF-REPORTS           PIC X.
000353         16  AM-NOTIF-OF-STATUS            PIC X.
000354
000355     12  AM-BENEFIT-TABLE-USAGE            PIC X.
000356         88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
000357         88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
000358         88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
000359         88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
000360
000361     12  AM-BENEFIT-CONTROLS.
000362         16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
000363             20  AM-BENEFIT-CODE           PIC XX.
000364             20  AM-BENEFIT-TYPE           PIC X.
000365             20  AM-BENEFIT-REVISION       PIC XXX.
000366             20  AM-BENEFIT-REM-TERM       PIC X.
000367             20  AM-BENEFIT-RETRO-Y-N      PIC X.
000368             20  FILLER                    PIC XX.
000369         16  FILLER                        PIC X(80).
000370
000371     12  AM-TRANSFER-DATA.
000372         16  AM-TRANSFERRED-FROM.
000373             20  AM-TRNFROM-CARRIER        PIC X.
000374             20  AM-TRNFROM-GROUPING.
000375                 24  AM-TRNFROM-GRP-PREFIX PIC XXX.
000376                 24  AM-TRNFROM-GRP-PRIME  PIC XXX.
000377             20  AM-TRNFROM-STATE          PIC XX.
000378             20  AM-TRNFROM-ACCOUNT.
000379                 24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
000380                 24  AM-TRNFROM-ACCT-PRIME PIC X(6).
000381             20  AM-TRNFROM-DTE            PIC XX.
000382         16  AM-TRANSFERRED-TO.
000383             20  AM-TRNTO-CARRIER          PIC X.
000384             20  AM-TRNTO-GROUPING.
000385                 24  AM-TRNTO-GRP-PREFIX   PIC XXX.
000386                 24  AM-TRNTO-GRP-PRIME    PIC XXX.
000387             20  AM-TRNTO-STATE            PIC XX.
000388             20  AM-TRNTO-ACCOUNT.
000389                 24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
000390                 24  AM-TRNTO-ACCT-PRIME   PIC X(6).
000391             20  AM-TRNTO-DTE              PIC XX.
000392         16  FILLER                        PIC X(10).
000393
000394     12  AM-SAVED-REMIT-TO                 PIC 99.
000395
000396     12  AM-COMM-STRUCTURE-SAVED.
000397         16  AM-DEFN-1-SAVED.
000398             20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
000399                 24  AM-AGT-SV             PIC X(10).
000400                 24  AM-COM-TYP-SV         PIC X.
000401                 24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
000402                 24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
000403                 24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
000404                 24  AM-RECALC-LV-INDIC-SV PIC X.
000405                 24  FILLER                PIC X.
000406                 24  AM-GL-CODES-SV        PIC X.
000407                 24  AM-COM-CHARGEBACK-SV  PIC 99.
000408                 24  FILLER                PIC X.
000409         16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
000410             20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
000411                 24  FILLER                PIC X(11).
000412                 24  AM-L-COMA-SV          PIC XXX.
000413                 24  AM-J-COMA-SV          PIC XXX.
000414                 24  AM-A-COMA-SV          PIC XXX.
000415                 24  FILLER                PIC X(6).
000416
000417     12  AM-FLC-NET-PREMIUM-ALLOWANCE.
000418         16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
000419            20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
000420            20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
000421            20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
000422
000423     12  AM-ORIG-DEALER-NO                 PIC X(10).
000424     12  FILLER                            PIC X(120).
000425
000426     12  AM-ACCOUNT-EXECUTIVE-DATA.
000427         16  AM-CONTROL-NAME               PIC X(30).
000428         16  AM-EXECUTIVE-ONE.
000429             20  AM-EXEC1-NAME             PIC X(15).
000430             20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
000431                                                          COMP-3.
000432             20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
000433                                                          COMP-3.
000434         16  AM-EXECUTIVE-TWO.
000435             20  AM-EXEC2-NAME             PIC X(15).
000436             20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
000437                                                          COMP-3.
000438             20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
000439                                                          COMP-3.
000440
000441     12  AM-RETRO-ADDITIONAL-DATA.
000442         16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
000443         16  AM-RETRO-PREM-P-E             PIC X.
000444         16  AM-RETRO-CLMS-P-I             PIC X.
000445         16  AM-RETRO-RET-BRACKET-LF.
000446             20  AM-RETRO-RET-METHOD-LF    PIC X.
000447                 88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
000448                 88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
000449             20  AM-RETRO-RET-BASIS-LF     PIC X.
000450                 88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
000451                 88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
000452             20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
000453                 24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
000454                 24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
000455         16  AM-RETRO-RET-BRACKET-AH.
000456             20  AM-RETRO-RET-METHOD-AH    PIC X.
000457                 88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
000458                 88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
000459                 88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
000460             20  AM-RETRO-RET-BASIS-AH     PIC X.
000461                 88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
000462                 88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
000463             20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
000464                 24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
000465                 24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
000466
000467     12  AM-COMMENTS.
000468         16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
000469
000470     12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
000471         16  AM-FLI-RETRO-SHARE-CODE       PIC X.
000472         16  AM-FLI-BILLING-CODE           PIC X.
000473         16  AM-FLI-ALT-STATE-CODE         PIC XX.
000474         16  AM-FLI-UNITED-IDENT           PIC X.
000475         16  AM-FLI-INTEREST-LOST-DATA.
000476             20  AM-FLI-BANK-NO            PIC X(5).
000477             20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
000478             20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
000479             20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
000480         16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
000481             20  AM-FLI-AGT                PIC X(9).
000482             20  AM-FLI-AGT-COMM-ACC       PIC X.
000483             20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
000484         16  FILLER                        PIC X(102).
000485
000486     12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
000487         16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
000488             20  AM-BENEFIT-DMD-CODE         PIC XX.
000489             20  AM-BENEFIT-DMD-TYPE         PIC X.
000490             20  AM-BENEFIT-DMD-REVISION     PIC XXX.
000491             20  AM-BENEFIT-DMD-REM-TERM     PIC X.
000492             20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
000493         16  FILLER                          PIC X(10).
000494******************************************************************
      *<<((file: ERCACCT))
000654
000655*                            COPY ELCCERT.
      *>>((file: ELCCERT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCERT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CERTIFICATE MASTER                        *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 450  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
000013*       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
000014*       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
000015*       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
000016*       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
000017*                                                                *
000018*   LOG = YES                                                    *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000029* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000030* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
000031* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000032* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000033* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
000034* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000035* 032612  CR2011110200001  PEMA  AHL CHANGES
000036* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
000037* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000038* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000039******************************************************************
000040
000041 01  CERTIFICATE-MASTER.
000042     12  CM-RECORD-ID                      PIC XX.
000043         88  VALID-CM-ID                      VALUE 'CM'.
000044
000045     12  CM-CONTROL-PRIMARY.
000046         16  CM-COMPANY-CD                 PIC X.
000047         16  CM-CARRIER                    PIC X.
000048         16  CM-GROUPING.
000049             20  CM-GROUPING-PREFIX        PIC X(3).
000050             20  CM-GROUPING-PRIME         PIC X(3).
000051         16  CM-STATE                      PIC XX.
000052         16  CM-ACCOUNT.
000053             20  CM-ACCOUNT-PREFIX         PIC X(4).
000054             20  CM-ACCOUNT-PRIME          PIC X(6).
000055         16  CM-CERT-EFF-DT                PIC XX.
000056         16  CM-CERT-NO.
000057             20  CM-CERT-PRIME             PIC X(10).
000058             20  CM-CERT-SFX               PIC X.
000059
000060     12  CM-CONTROL-BY-NAME.
000061         16  CM-COMPANY-CD-A1              PIC X.
000062         16  CM-INSURED-LAST-NAME          PIC X(15).
000063         16  CM-INSURED-INITIALS.
000064             20  CM-INSURED-INITIAL1       PIC X.
000065             20  CM-INSURED-INITIAL2       PIC X.
000066
000067     12  CM-CONTROL-BY-SSN.
000068         16  CM-COMPANY-CD-A2              PIC X.
000069         16  CM-SOC-SEC-NO.
000070             20  CM-SSN-STATE              PIC XX.
000071             20  CM-SSN-ACCOUNT            PIC X(6).
000072             20  CM-SSN-LN3.
000073                 25  CM-INSURED-INITIALS-A2.
000074                     30 CM-INSURED-INITIAL1-A2   PIC X.
000075                     30 CM-INSURED-INITIAL2-A2   PIC X.
000076                 25 CM-PART-LAST-NAME-A2         PIC X.
000077
000078     12  CM-CONTROL-BY-CERT-NO.
000079         16  CM-COMPANY-CD-A4              PIC X.
000080         16  CM-CERT-NO-A4                 PIC X(11).
000081
000082     12  CM-CONTROL-BY-MEMB.
000083         16  CM-COMPANY-CD-A5              PIC X.
000084         16  CM-MEMBER-NO.
000085             20  CM-MEMB-STATE             PIC XX.
000086             20  CM-MEMB-ACCOUNT           PIC X(6).
000087             20  CM-MEMB-LN4.
000088                 25  CM-INSURED-INITIALS-A5.
000089                     30 CM-INSURED-INITIAL1-A5   PIC X.
000090                     30 CM-INSURED-INITIAL2-A5   PIC X.
000091                 25 CM-PART-LAST-NAME-A5         PIC XX.
000092
000093     12  CM-INSURED-PROFILE-DATA.
000094         16  CM-INSURED-FIRST-NAME.
000095             20  CM-INSURED-1ST-INIT       PIC X.
000096             20  FILLER                    PIC X(9).
000097         16  CM-INSURED-ISSUE-AGE          PIC 99.
000098         16  CM-INSURED-SEX                PIC X.
000099             88  CM-SEX-MALE                  VALUE 'M'.
000100             88  CM-SEX-FEMAL                 VALUE 'F'.
000101         16  CM-INSURED-JOINT-AGE          PIC 99.
000102         16  CM-JOINT-INSURED-NAME.
000103             20  CM-JT-LAST-NAME           PIC X(15).
000104             20  CM-JT-FIRST-NAME.
000105                 24  CM-JT-1ST-INIT        PIC X.
000106                 24  FILLER                PIC X(9).
000107             20  CM-JT-INITIAL             PIC X.
000108
000109     12  CM-LIFE-DATA.
000110         16  CM-LF-BENEFIT-CD              PIC XX.
000111         16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
000112         16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
000113         16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
000114         16  CM-LF-DEV-CODE                PIC XXX.
000115         16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
000116         16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
000117         16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000118         16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
000119         16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000120         16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000121         16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
000122         16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000123         16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
000124         16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000125         16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
000126         16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
000127         16  cm-temp-epiq                  pic xx.
000128             88  EPIQ-CLASS                  value 'EQ'.
000129*        16  FILLER                        PIC XX.
000130
000131     12  CM-AH-DATA.
000132         16  CM-AH-BENEFIT-CD              PIC XX.
000133         16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
000134         16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
000135         16  CM-AH-DEV-CODE                PIC XXX.
000136         16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
000137         16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
000138         16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000139         16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000140         16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000141         16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
000142         16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
000143         16  CM-AH-PAID-THRU-DT            PIC XX.
000144             88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
000145         16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000146         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
000147         16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
000148         16  FILLER                        PIC X.
000149
000150     12  CM-LOAN-INFORMATION.
000151         16  CM-LIVES                      PIC S9(7)     COMP-3.
000152         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
000153                                           PIC S9(5)V99  COMP-3.
000154         16  CM-BILLED                     PIC S9(7)     COMP-3.
000155         16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
000156         16  CM-PAY-FREQUENCY              PIC S99.
000157         16  CM-LOAN-TERM                  PIC S999      COMP-3.
000158         16  CM-RATE-CLASS                 PIC XX.
000159         16  CM-BENEFICIARY                PIC X(25).
000160         16  CM-POLICY-FORM-NO             PIC X(12).
000161         16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
000162         16  CM-LAST-ADD-ON-DT             PIC XX.
000163         16  CM-DEDUCTIBLE-AMOUNTS.
000164             20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
000165             20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
000166         16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000167             20  CM-RESIDENT-STATE         PIC XX.
000168             20  CM-RATE-CODE              PIC X(4).
000169             20  FILLER                    PIC XX.
000170         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000171             20  CM-LOAN-OFFICER           PIC X(5).
000172             20  FILLER                    PIC XXX.
000173         16  CM-CSR-CODE                   PIC XXX.
000174         16  CM-UNDERWRITING-CODE          PIC X.
000175             88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
000176         16  CM-POST-CARD-IND              PIC X.
000177         16  CM-REF-INTERFACE-SW           PIC X.
000178         16  CM-PREMIUM-TYPE               PIC X.
000179             88  CM-SING-PRM                  VALUE '1'.
000180             88  CM-O-B-COVERAGE              VALUE '2'.
000181             88  CM-OPEN-END                  VALUE '3'.
000182         16  CM-IND-GRP-TYPE               PIC X.
000183             88  CM-INDIVIDUAL                VALUE 'I'.
000184             88  CM-GROUP                     VALUE 'G'.
000185         16  CM-SKIP-CODE                  PIC X.
000186             88  NO-MONTHS-SKIPPED            VALUE SPACE.
000187             88  SKIP-JULY                    VALUE '1'.
000188             88  SKIP-AUGUST                  VALUE '2'.
000189             88  SKIP-SEPTEMBER               VALUE '3'.
000190             88  SKIP-JULY-AUG                VALUE '4'.
000191             88  SKIP-AUG-SEPT                VALUE '5'.
000192             88  SKIP-JULY-AUG-SEPT           VALUE '6'.
000193             88  SKIP-JUNE-JULY-AUG           VALUE '7'.
000194             88  SKIP-JUNE                    VALUE '8'.
000195             88  SKIP-JUNE-JULY               VALUE '9'.
000196             88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
000197             88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
000198         16  CM-PAYMENT-MODE               PIC X.
000199             88  PAY-MONTHLY                  VALUE SPACE.
000200             88  PAY-WEEKLY                   VALUE '1'.
000201             88  PAY-SEMI-MONTHLY             VALUE '2'.
000202             88  PAY-BI-WEEKLY                VALUE '3'.
000203             88  PAY-SEMI-ANUALLY             VALUE '4'.
000204         16  CM-LOAN-NUMBER                PIC X(8).
000205         16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
000206         16  CM-OLD-LOF                    PIC XXX.
000207*        16  CM-LOAN-OFFICER               PIC XXX.
000208         16  CM-REIN-TABLE                 PIC XXX.
000209         16  CM-SPECIAL-REIN-CODE          PIC X.
000210         16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
000211         16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
000212         16  CM-LOAN-1ST-PMT-DT            PIC XX.
000213
000214     12  CM-STATUS-DATA.
000215         16  CM-ENTRY-STATUS               PIC X.
000216         16  CM-ENTRY-DT                   PIC XX.
000217
000218         16  CM-LF-STATUS-AT-CANCEL        PIC X.
000219         16  CM-LF-CANCEL-DT               PIC XX.
000220         16  CM-LF-CANCEL-EXIT-DT          PIC XX.
000221
000222         16  CM-LF-STATUS-AT-DEATH         PIC X.
000223         16  CM-LF-DEATH-DT                PIC XX.
000224         16  CM-LF-DEATH-EXIT-DT           PIC XX.
000225
000226         16  CM-LF-CURRENT-STATUS          PIC X.
000227             88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000228                                                'M' '4' '5' '9'.
000229             88  CM-LF-NORMAL-ENTRY           VALUE '1'.
000230             88  CM-LF-POLICY-PENDING         VALUE '2'.
000231             88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
000232             88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
000233             88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
000234             88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
000235             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000236             88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
000237             88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000238             88  CM-LF-CANCEL-APPLIED         VALUE '8'.
000239             88  CM-LF-IS-REIN-ONLY           VALUE '9'.
000240             88  CM-LF-DECLINED               VALUE 'D'.
000241             88  CM-LF-VOIDED                 VALUE 'V'.
000242
000243         16  CM-AH-STATUS-AT-CANCEL        PIC X.
000244         16  CM-AH-CANCEL-DT               PIC XX.
000245         16  CM-AH-CANCEL-EXIT-DT          PIC XX.
000246
000247         16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
000248         16  CM-AH-SETTLEMENT-DT           PIC XX.
000249         16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
000250
000251         16  CM-AH-CURRENT-STATUS          PIC X.
000252             88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000253                                                'M' '4' '5' '9'.
000254             88  CM-AH-NORMAL-ENTRY           VALUE '1'.
000255             88  CM-AH-POLICY-PENDING         VALUE '2'.
000256             88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
000257             88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
000258             88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
000259             88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
000260             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000261             88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
000262             88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000263             88  CM-AH-CANCEL-APPLIED         VALUE '8'.
000264             88  CM-AH-IS-REIN-ONLY           VALUE '9'.
000265             88  CM-AH-DECLINED               VALUE 'D'.
000266             88  CM-AH-VOIDED                 VALUE 'V'.
000267
000268         16  CM-CLAIM-INTERFACE-SW         PIC X.
000269             88  NO-CLAIM-ATTACHED            VALUE SPACE.
000270             88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
000271             88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
000272         16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
000273
000274         16  CM-ENTRY-BATCH                PIC X(6).
000275         16  CM-LF-EXIT-BATCH              PIC X(6).
000276         16  CM-AH-EXIT-BATCH              PIC X(6).
000277         16  CM-LAST-MONTH-END             PIC XX.
000278
000279     12  CM-NOTE-SW                        PIC X.
000280         88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
000281         88  CERT-NOTES-PRESENT               VALUE '1'.
000282         88  BILLING-NOTES-PRESENT            VALUE '2'.
000283         88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
000284         88  CLAIM-NOTES-PRESENT              VALUE '4'.
000285         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
000286         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
000287         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
000288     12  CM-COMP-EXCP-SW                   PIC X.
000289         88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
000290         88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
000291     12  CM-INSURED-ADDRESS-SW             PIC X.
000292         88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
000293         88  INSURED-ADDR-PRESENT             VALUE '1'.
000294
000295*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
000296     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
000297     12  FILLER                            PIC X.
000298
000299*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
000300     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
000301     12  FILLER                            PIC X.
000302*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
000303     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
000304
000305     12  CM-CREDIT-INTERFACE-SW-1          PIC X.
000306         88  CERT-ADDED-BATCH                 VALUE ' '.
000307         88  CERT-ADDED-ONLINE                VALUE '1'.
000308         88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
000309         88  CERT-PURGED-OFFLINE              VALUE '3'.
000310         88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
000311     12  CM-CREDIT-INTERFACE-SW-2          PIC X.
000312         88  CERT-AS-LOADED                   VALUE ' '.
000313         88  CERT-CANCELLED-ONLINE            VALUE '1'.
000314         88  CERT-CLAIM-ONLINE                VALUE '2'.
000315         88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
000316         88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
000317         88  CERT-PEND-CANCEL-VOID            VALUE '5'.
000318         88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
000319         88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
000320
000321     12  CM-ACCOUNT-COMM-PCTS.
000322         16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
000323         16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000324
000325     12  CM-USER-FIELD                     PIC X.
000326     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
000327     12  CM-CLP-STATE                      PIC XX.
000328     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
000329     12  CM-USER-RESERVED                  PIC XXX.
000330     12  FILLER REDEFINES CM-USER-RESERVED.
000331         16  CM-AH-CLASS-CD                PIC XX.
000332         16  F                             PIC X.
000333******************************************************************
      *<<((file: ELCCERT))
000656
000657*                            COPY ERCMAIL.
      *>>((file: ERCMAIL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCMAIL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
000014*   ALTERNATE PATH    = NOT USED                                 *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
000027* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000028* 111108                   PEMA  ADD CRED BENE ADDR2
000029******************************************************************
000030
000031 01  MAILING-DATA.
000032     12  MA-RECORD-ID                      PIC XX.
000033         88  VALID-MA-ID                       VALUE 'MA'.
000034
000035     12  MA-CONTROL-PRIMARY.
000036         16  MA-COMPANY-CD                 PIC X.
000037         16  MA-CARRIER                    PIC X.
000038         16  MA-GROUPING.
000039             20  MA-GROUPING-PREFIX        PIC XXX.
000040             20  MA-GROUPING-PRIME         PIC XXX.
000041         16  MA-STATE                      PIC XX.
000042         16  MA-ACCOUNT.
000043             20  MA-ACCOUNT-PREFIX         PIC X(4).
000044             20  MA-ACCOUNT-PRIME          PIC X(6).
000045         16  MA-CERT-EFF-DT                PIC XX.
000046         16  MA-CERT-NO.
000047             20  MA-CERT-PRIME             PIC X(10).
000048             20  MA-CERT-SFX               PIC X.
000049
000050     12  FILLER                            PIC XX.
000051
000052     12  MA-ACCESS-CONTROL.
000053         16  MA-SOURCE-SYSTEM              PIC XX.
000054             88  MA-FROM-CREDIT                VALUE 'CR'.
000055             88  MA-FROM-VSI                   VALUE 'VS'.
000056             88  MA-FROM-WARRANTY              VALUE 'WA'.
000057             88  MA-FROM-OTHER                 VALUE 'OT'.
000058         16  MA-RECORD-ADD-DT              PIC XX.
000059         16  MA-RECORD-ADDED-BY            PIC XXXX.
000060         16  MA-LAST-MAINT-DT              PIC XX.
000061         16  MA-LAST-MAINT-BY              PIC XXXX.
000062         16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000063
000064     12  MA-PROFILE-INFO.
000065         16  MA-QUALIFY-CODE-1             PIC XX.
000066         16  MA-QUALIFY-CODE-2             PIC XX.
000067         16  MA-QUALIFY-CODE-3             PIC XX.
000068         16  MA-QUALIFY-CODE-4             PIC XX.
000069         16  MA-QUALIFY-CODE-5             PIC XX.
000070
000071         16  MA-INSURED-LAST-NAME          PIC X(15).
000072         16  MA-INSURED-FIRST-NAME         PIC X(10).
000073         16  MA-INSURED-MIDDLE-INIT        PIC X.
000074         16  MA-INSURED-ISSUE-AGE          PIC 99.
000075         16  MA-INSURED-BIRTH-DT           PIC XX.
000076         16  MA-INSURED-SEX                PIC X.
000077             88  MA-SEX-MALE                   VALUE 'M'.
000078             88  MA-SEX-FEMALE                 VALUE 'F'.
000079         16  MA-INSURED-SOC-SEC-NO         PIC X(11).
000080
000081         16  MA-ADDRESS-CORRECTED          PIC X.
000082         16  MA-JOINT-BIRTH-DT             PIC XX.
000083*        16  FILLER                        PIC X(12).
000084
000085         16  MA-ADDRESS-LINE-1             PIC X(30).
000086         16  MA-ADDRESS-LINE-2             PIC X(30).
000087         16  MA-CITY-STATE.
000088             20  MA-CITY                   PIC X(28).
000089             20  MA-ADDR-STATE             PIC XX.
000090         16  MA-ZIP.
000091             20  MA-ZIP-CODE.
000092                 24  MA-ZIP-CODE-1ST       PIC X(1).
000093                     88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000094                 24  FILLER                PIC X(4).
000095             20  MA-ZIP-PLUS4              PIC X(4).
000096         16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
000097             20  MA-CAN-POSTAL-CODE-1      PIC X(3).
000098             20  MA-CAN-POSTAL-CODE-2      PIC X(3).
000099             20  FILLER                    PIC X(3).
000100
000101         16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
000102
000103         16  FILLER                        PIC XXX.
000104*        16  FILLER                        PIC X(10).
000105
000106
000107     12  MA-CRED-BENE-INFO.
000108         16  MA-CRED-BENE-NAME                 PIC X(25).
000109         16  MA-CRED-BENE-ADDR                 PIC X(30).
000110         16  MA-CRED-BENE-ADDR2                PIC X(30).
000111         16  MA-CRED-BENE-CTYST.
000112             20  MA-CRED-BENE-CITY             PIC X(28).
000113             20  MA-CRED-BENE-STATE            PIC XX.
000114         16  MA-CRED-BENE-ZIP.
000115             20  MA-CB-ZIP-CODE.
000116                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
000117                     88  MA-CB-CANADIAN-POST-CODE
000118                                           VALUE 'A' THRU 'Z'.
000119                 24  FILLER                    PIC X(4).
000120             20  MA-CB-ZIP-PLUS4               PIC X(4).
000121         16  MA-CB-CANADIAN-POSTAL-CODE
000122                            REDEFINES MA-CRED-BENE-ZIP.
000123             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
000124             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
000125             20  FILLER                        PIC X(3).
000126     12  MA-POST-CARD-MAIL-DATA.
000127         16  MA-MAIL-DATA OCCURS 7.
000128             20  MA-MAIL-TYPE              PIC X.
000129                 88  MA-12MO-MAILING           VALUE '1'.
000130                 88  MA-EXP-MAILING            VALUE '2'.
000131             20  MA-MAIL-STATUS            PIC X.
000132                 88  MA-MAIL-ST-MAILED         VALUE '1'.
000133                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
000134                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
000135             20  MA-MAIL-DATE              PIC XX.
000136     12  FILLER                            PIC XX.
000137     12  FILLER                            PIC XX.
000138*    12  FILLER                            PIC X(30).
000139******************************************************************
      *<<((file: ERCMAIL))
000658*                            COPY ELCCRTO.
      *>>((file: ELCCRTO))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTO.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = ORIGINAL CERTIFICATE INFORMATION          *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 524  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTO                         RKP=2,LEN=36   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 061011  2011022800001    PEMA  NEW FILE TO SAVE ORIG CERT INFO
000023* 062712  2011022800001    AJRA  REDEFINE ORIG DATA
000024* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
000025* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
000027******************************************************************
000028
000029 01  ORIGINAL-CERTIFICATE.
000030     12  OC-RECORD-ID                      PIC XX.
000031         88  VALID-OC-ID                      VALUE 'OC'.
000032
000033     12  OC-CONTROL-PRIMARY.
000034         16  OC-COMPANY-CD                 PIC X.
000035         16  OC-CARRIER                    PIC X.
000036         16  OC-GROUPING                   PIC X(6).
000037         16  OC-STATE                      PIC XX.
000038         16  OC-ACCOUNT                    PIC X(10).
000039         16  OC-CERT-EFF-DT                PIC XX.
000040         16  OC-CERT-NO.
000041             20  OC-CERT-PRIME             PIC X(10).
000042             20  OC-CERT-SFX               PIC X.
000043         16  OC-RECORD-TYPE                PIC X.
000044         16  OC-KEY-SEQ-NO                 PIC 9(4) BINARY.
000045
000046     12  OC-LAST-MAINT-DT                  PIC XX.
000047     12  OC-LAST-MAINT-BY                  PIC X(4).
000048     12  OC-LAST-MAINT-HHMMSS              PIC S9(6)   COMP-3.
000049
000050     12  OC-ENDORSEMENT-PROCESSED-DT       PIC XX.
000051     12  FILLER                            PIC X(49).
000052
000053     12  OC-ORIG-REC.
000054         16  OC-INS-LAST-NAME              PIC X(15).
000055         16  OC-INS-FIRST-NAME             PIC X(10).
000056         16  OC-INS-MIDDLE-INIT            PIC X.
000057         16  OC-INS-AGE                    PIC S999     COMP-3.
000058         16  OC-JNT-LAST-NAME              PIC X(15).
000059         16  OC-JNT-FIRST-NAME             PIC X(10).
000060         16  OC-JNT-MIDDLE-INIT            PIC X.
000061         16  OC-JNT-AGE                    PIC S999     COMP-3.
000062         16  OC-LF-BENCD                   PIC XX.
000063         16  OC-LF-TERM                    PIC S999      COMP-3.
000064         16  OC-LF-BEN-AMT                 PIC S9(9)V99  COMP-3.
000065         16  OC-LF-PRM-AMT                 PIC S9(7)V99  COMP-3.
000066         16  OC-LF-ALT-BEN-AMT             PIC S9(9)V99  COMP-3.
000067         16  OC-LF-ALT-PRM-AMT             PIC S9(7)V99  COMP-3.
000068         16  OC-LF-EXP-DT                  PIC XX.
000069         16  OC-LF-COMM-PCT                PIC SV9(5)    COMP-3.
000070         16  OC-LF-CANCEL-DT               PIC XX.
000071         16  OC-LF-CANCEL-AMT              PIC S9(7)V99  COMP-3.
000072         16  OC-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000073         16  OC-AH-BENCD                   PIC XX.
000074         16  OC-AH-TERM                    PIC S999      COMP-3.
000075         16  OC-AH-BEN-AMT                 PIC S9(9)V99  COMP-3.
000076         16  OC-AH-PRM-AMT                 PIC S9(7)V99  COMP-3.
000077         16  OC-AH-EXP-DT                  PIC XX.
000078         16  OC-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000079         16  OC-AH-CP                      PIC 99.
000080         16  OC-AH-CANCEL-DT               PIC XX.
000081         16  OC-AH-CANCEL-AMT              PIC S9(7)V99  COMP-3.
000082         16  OC-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000083         16  OC-CRED-BENE-NAME             PIC X(25).
000084         16  OC-1ST-PMT-DT                 PIC XX.
000085         16  OC-INS-AGE-DEFAULT-FLAG       PIC X.
000086         16  OC-JNT-AGE-DEFAULT-FLAG       PIC X.
000087         16  OC-ISSUE-TRAN-IND             PIC X.
000088         16  OC-CANCEL-TRAN-IND            PIC X.
000089         16  FILLER                        PIC X(211).
000090
000091     12  FILLER                            PIC X(50).
000092
000093******************************************************************
      *<<((file: ELCCRTO))
000659
000660*                            COPY ERCFORM.
      *>>((file: ERCFORM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCFORM.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*    FILE DESCRIPTION = POLICY FORM MASTER FILE                  *
000009*                                                                *
000010*    FILE TYPE = VSAM,KSDS                                       *
000011*    RECORD SIZE = 500  RECFORM = FIXED                          *
000012*                                                                *
000013*    BASE CLUSTER = ERFORM                      RKP=02,LEN=20    *
000014*                                                                *
000015*    LOG = YES                                                   *
000016*    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018 01  FORM-MASTER.
000019    12  FO-RECORD-ID                 PIC X(02).
000020        88  VALID-FO-ID                  VALUE 'FO'.
000021
000022    12  FO-CONTROL-PRIMARY.
000023        16  FO-COMPANY-CD            PIC X(01).
000024        16  FO-STATE                 PIC X(02).
000025        16  FO-FORM-ID               PIC X(12).
000026        16  FO-FORM-EXP-DT           PIC X(02).
000027
000028    12  FO-POLICY-FORM-DATA.
000029        16  FO-IND-GRP-CD            PIC X(01).
000030        16  FO-MAX-ATT-AGE           PIC S9(03)      COMP-3.
000031        16  FO-LF-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
000032        16  FO-AH-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
000033        16  FO-LF-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
000034        16  FO-AH-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
000035        16  FO-LF-MAX-TERM           PIC S9(03)      COMP-3.
000036        16  FO-AH-MAX-TERM           PIC S9(03)      COMP-3.
000037        16  FO-MAX-LF-AMT            PIC S9(07)V99   COMP-3.
000038        16  FO-MAX-AH-AMT            PIC S9(05)V99   COMP-3.
000039        16  FO-SUICIDE-EXCL-TYPE     PIC 9(02).
000040        16  FO-LF-PRE-EXIST-EXCL-TYPE    PIC 9(02).
000041        16  FO-AH-PRE-EXIST-EXCL-TYPE    PIC 9(02).
000042        16  FO-DIS-DEF-TYPE          PIC 9(02).
000043        16  FO-DISMEMBERMENT-CD      PIC X(01).
000044        16  FO-APP-CERT-USE-CD       PIC X(01).
000045
000046    12  FILLER                       PIC X(29).
000047
000048    12  FO-FORM-PLAN-TABLE.
000049        16  FO-FORM-PLANS     OCCURS 40 TIMES.
000050            20  FO-PLAN-TYPE         PIC X(01).
000051            20  FO-PLAN-ID           PIC X(02).
000052            20  FO-PLAN-TERM         PIC 9(03).
000053            20  FO-PLAN-REFUND-METHOD
000054                                     PIC X.
000055
000056    12  FILLER                       PIC X(30).
000057
000058    12  FO-COMMENTS-AREA.
000059        16  FO-COMMENT-LINE-1        PIC X(78).
000060
000061    12  FILLER                       PIC X(20).
000062
000063    12  FO-MAINT-INFORMATION.
000064        16  FO-LAST-MAINT-DT         PIC X(02).
000065        16  FO-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
000066        16  FO-LAST-MAINT-BY         PIC X(04).
      *<<((file: ERCFORM))
000661
000662*                            COPY ELCMSTR.
      *>>((file: ELCMSTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCMSTR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.012                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CLAIM MASTER FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 350  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
000013*       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
000014*       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000015*       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
000016*       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
000017*                                                 RKP=75,LEN=21  *
000018*                                                                *
000019*   **** NOTE ****                                               *
000020*             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
000021*             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
000022*                                                                *
000023*   LOG = YES                                                    *
000024*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000034* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
000035* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
000036* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000037* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000038* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
000039* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000040******************************************************************
000041 01  CLAIM-MASTER.
000042     12  CL-RECORD-ID                PIC XX.
000043         88  VALID-CL-ID         VALUE 'CL'.
000044
000045     12  CL-CONTROL-PRIMARY.
000046         16  CL-COMPANY-CD           PIC X.
000047         16  CL-CARRIER              PIC X.
000048         16  CL-CLAIM-NO             PIC X(7).
000049         16  CL-CERT-NO.
000050             20  CL-CERT-PRIME       PIC X(10).
000051             20  CL-CERT-SFX         PIC X.
000052
000053     12  CL-CONTROL-BY-NAME.
000054         16  CL-COMPANY-CD-A1        PIC X.
000055         16  CL-INSURED-LAST-NAME    PIC X(15).
000056         16  CL-INSURED-NAME.
000057             20  CL-INSURED-1ST-NAME PIC X(12).
000058             20  CL-INSURED-MID-INIT PIC X.
000059
000060     12  CL-CONTROL-BY-SSN.
000061         16  CL-COMPANY-CD-A2        PIC X.
000062         16  CL-SOC-SEC-NO.
000063             20  CL-SSN-STATE        PIC XX.
000064             20  CL-SSN-ACCOUNT      PIC X(6).
000065             20  CL-SSN-LN3          PIC X(3).
000066
000067     12  CL-CONTROL-BY-CERT-NO.
000068         16  CL-COMPANY-CD-A4        PIC X.
000069         16  CL-CERT-NO-A4.
000070             20  CL-CERT-A4-PRIME    PIC X(10).
000071             20  CL-CERT-A4-SFX      PIC X.
000072
000073     12  CL-CONTROL-BY-CCN.
000074         16  CL-COMPANY-CD-A5        PIC X.
000075         16  CL-CCN-A5.
000076             20  CL-CCN.
000077                 24  CL-CCN-PREFIX-A5 PIC X(4).
000078                 24  CL-CCN-PRIME-A5 PIC X(12).
000079             20  CL-CCN-FILLER-A5    PIC X(4).
000080
000081     12  CL-INSURED-PROFILE-DATA.
000082         16  CL-INSURED-BIRTH-DT     PIC XX.
000083         16  CL-INSURED-SEX-CD       PIC X.
000084             88  INSURED-IS-MALE        VALUE 'M'.
000085             88  INSURED-IS-FEMALE      VALUE 'F'.
000086             88  INSURED-SEX-UNKNOWN    VALUE ' '.
000087         16  CL-INSURED-OCC-CD       PIC X(6).
000088         16  FILLER                  PIC X(5).
000089
000090     12  CL-PROCESSING-INFO.
000091         16  CL-PROCESSOR-ID         PIC X(4).
000092         16  CL-CLAIM-STATUS         PIC X.
000093             88  CLAIM-IS-OPEN          VALUE 'O'.
000094             88  CLAIM-IS-CLOSED        VALUE 'C'.
000095         16  CL-CLAIM-TYPE           PIC X.
000096*            88  AH-CLAIM               VALUE 'A'.
000097*            88  LIFE-CLAIM             VALUE 'L'.
000098*            88  PROPERTY-CLAIM         VALUE 'P'.
000099*            88  IUI-CLAIM              VALUE 'I'.
000100*            88  GAP-CLAIM              VALUE 'G'.
000101*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
000102*            88  OTHER-CLAIM            VALUE 'O'.
000103         16  CL-CLAIM-PREM-TYPE      PIC X.
000104             88  SINGLE-PREMIUM         VALUE '1'.
000105             88  O-B-COVERAGE           VALUE '2'.
000106             88  OPEN-END-COVERAGE      VALUE '3'.
000107         16  CL-INCURRED-DT          PIC XX.
000108         16  CL-REPORTED-DT          PIC XX.
000109         16  CL-FILE-ESTABLISH-DT    PIC XX.
000110         16  CL-EST-END-OF-DISAB-DT  PIC XX.
000111         16  CL-LAST-PMT-DT          PIC XX.
000112         16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000113         16  CL-PAID-THRU-DT         PIC XX.
000114         16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000115         16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000116         16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000117         16  CL-PMT-CALC-METHOD      PIC X.
000118             88  CL-360-DAY-YR          VALUE '1'.
000119             88  CL-365-DAY-YR          VALUE '2'.
000120             88  CL-FULL-MONTHS         VALUE '3'.
000121         16  CL-CAUSE-CD             PIC X(6).
000122
000123         16  CL-PRIME-CERT-NO.
000124             20  CL-PRIME-CERT-PRIME PIC X(10).
000125             20  CL-PRIME-CERT-SFX   PIC X.
000126
000127         16  CL-SYSTEM-IDENTIFIER    PIC XX.
000128             88  CL-CREDIT-CLAIM        VALUE 'CR'.
000129             88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
000130
000131         16  CL-MICROFILM-NO         PIC X(10).
000132         16  FILLER REDEFINES CL-MICROFILM-NO.
000133             20  CL-BENEFIT-PERIOD   PIC 99.
000134             20  FILLER              PIC X(8).
000135         16  CL-PROG-FORM-TYPE       PIC X.
000136         16  CL-LAST-ADD-ON-DT       PIC XX.
000137
000138         16  CL-LAST-REOPEN-DT       PIC XX.
000139         16  CL-LAST-CLOSE-DT        PIC XX.
000140         16  CL-LAST-CLOSE-REASON    PIC X(01).
000141             88  FINAL-PAID             VALUE '1'.
000142             88  CLAIM-DENIED           VALUE '2'.
000143             88  AUTO-CLOSE             VALUE '3'.
000144             88  MANUAL-CLOSE           VALUE '4'.
000145             88  BENEFITS-CHANGED       VALUE 'C'.
000146             88  SETUP-ERRORS           VALUE 'E'.
000147         16  CL-ASSOC-CERT-SEQU      PIC S99.
000148         16  CL-ASSOC-CERT-TOTAL     PIC S99.
000149         16  CL-CLAIM-PAYMENT-STATUS PIC 9.
000150             88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
000151         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
000152         16  FILLER                  PIC X.
000153
000154     12  CL-CERTIFICATE-DATA.
000155         16  CL-CERT-ORIGIN          PIC X.
000156             88  CERT-WAS-ONLINE        VALUE '1'.
000157             88  CERT-WAS-CREATED       VALUE '2'.
000158             88  COVERAGE-WAS-ADDED     VALUE '3'.
000159         16  CL-CERT-KEY-DATA.
000160             20  CL-CERT-CARRIER     PIC X.
000161             20  CL-CERT-GROUPING    PIC X(6).
000162             20  CL-CERT-STATE       PIC XX.
000163             20  CL-CERT-ACCOUNT.
000164                 24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
000165                 24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
000166             20  CL-CERT-EFF-DT      PIC XX.
000167
000168     12  CL-STATUS-CONTROLS.
000169         16  CL-PRIORITY-CD          PIC X.
000170             88  CONFIDENTIAL-DATA      VALUE '8'.
000171             88  HIGHEST-PRIORITY       VALUE '9'.
000172         16  CL-SUPV-ATTN-CD         PIC X.
000173             88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
000174             88  SUPV-IS-REQUIRED       VALUE 'Y'.
000175         16  CL-PURGED-DT            PIC XX.
000176         16  CL-RESTORED-DT          PIC XX.
000177         16  CL-NEXT-AUTO-PAY-DT     PIC XX.
000178         16  CL-NEXT-RESEND-DT       PIC XX.
000179         16  CL-NEXT-FOLLOWUP-DT     PIC XX.
000180         16  CL-CRITICAL-PERIOD      PIC 99.
000181*        16  FILLER                  PIC XX.
000182         16  CL-LAST-MAINT-DT        PIC XX.
000183         16  CL-LAST-MAINT-USER      PIC X(4).
000184         16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000185         16  CL-LAST-MAINT-TYPE      PIC X.
000186             88  CLAIM-SET-UP           VALUE ' '.
000187             88  PAYMENT-MADE           VALUE '1'.
000188             88  LETTER-SENT            VALUE '2'.
000189             88  MASTER-WAS-ALTERED     VALUE '3'.
000190             88  MASTER-WAS-RESTORED    VALUE '4'.
000191             88  INCURRED-DATE-CHANGED  VALUE '5'.
000192             88  FILE-CONVERTED         VALUE '6'.
000193             88  CHANGE-OF-BENEFITS     VALUE 'C'.
000194             88  ERROR-CORRECTION       VALUE 'E'.
000195         16  CL-RELATED-CLAIM-NO     PIC X(7).
000196         16  CL-HISTORY-ARCHIVE-DT   PIC XX.
000197         16  CL-BENEFICIARY          PIC X(10).
000198         16  CL-FILE-ESTABLISHED-BY  PIC X(4).
000199         16  CL-DENIAL-TYPE          PIC X.
000200             88  CL-TYPE-DENIAL          VALUE '1'.
000201             88  CL-TYPE-RESCISSION      VALUE '2'.
000202             88  CL-TYPE-REFORMATION     VALUE '3'.
000203             88  CL-TYPE-REF-TO-RES      VALUE '4'.
000204             88  CL-TYPE-RECONSIDERED    VALUE '5'.
000205         16  CL-NO-OF-EXTENSIONS     PIC 99.
000206
000207         16  filler                  pic x(3).
000208*        16  CL-CRIT-PER-RECURRENT   PIC X.
000209*        16  CL-CRIT-PER-RTW-MOS     PIC 99.
000210*        16  CL-RTW-DT               PIC XX.
000211
000212     12  CL-TRAILER-CONTROLS.
000213         16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000214             88  CL-1ST-TRL-AVAIL       VALUE +4095.
000215             88  CL-LAST-TRL-AVAIL      VALUE +100.
000216             88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
000217         16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000218         16  FILLER                  PIC XX.
000219         16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000220         16  CL-ADDRESS-TRAILER-CNT.
000221             20  CL-INSURED-ADDR-CNT  PIC S9(1).
000222                 88  NO-INSURED-AVAILABLE    VALUE ZERO.
000223             20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
000224                 88  ACCOUNT-IS-ONLINE       VALUE ZERO.
000225             20  CL-BENIF-ADDR-CNT    PIC S9(1).
000226                 88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
000227             20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
000228                 88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
000229             20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
000230                 88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
000231             20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
000232                 88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
000233             20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
000234                 88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
000235
000236     12  CL-CV-REFERENCE-NO.
000237         16  CL-CV-REFNO-PRIME       PIC X(18).
000238         16  CL-CV-REFNO-SFX         PIC XX.
000239
000240     12  CL-FILE-LOCATION            PIC X(4).
000241
000242     12  CL-PROCESS-ERRORS.
000243         16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000244             88  NO-FATAL-ERRORS        VALUE ZERO.
000245         16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000246             88  NO-FORCABLE-ERRORS     VALUE ZERO.
000247
000248     12  CL-PRODUCT-CD               PIC X.
000249
000250     12  CL-CURRENT-KEY-DATA.
000251         16  CL-CURRENT-CARRIER      PIC X.
000252         16  CL-CURRENT-GROUPING     PIC X(6).
000253         16  CL-CURRENT-STATE        PIC XX.
000254         16  CL-CURRENT-ACCOUNT      PIC X(10).
000255
000256     12  CL-ASSOCIATES               PIC X.
000257         88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
000258         88  CL-ASSOC-INTERFACE         VALUE 'I'.
000259         88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000260         88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
000261
000262     12  CL-ACTIVITY-CODE            PIC 99.
000263     12  CL-ACTIVITY-MAINT-DT        PIC XX.
000264     12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
000265
000266     12  CL-LAPSE-REPORT-CODE        PIC 9.
000267     12  CL-LAG-REPORT-CODE          PIC 9.
000268     12  CL-LOAN-TYPE                PIC XX.
000269     12  CL-LEGAL-STATE              PIC XX.
000270
000271     12  CL-YESNOSW                  PIC X.
000272     12  CL-ACCIDENT-CLAIM-SW        PIC X.
000273         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
000274         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
000275         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
000276     12  cl-insured-type             pic x.
000277         88  cl-claim-on-primary         value 'P'.
000278         88  cl-claim-on-co-borrower     value 'C'.
000279     12  cl-benefit-expiration-dt    PIC XX.
      *<<((file: ELCMSTR))
000663*                            COPY ERCPDEF.
      *>>((file: ERCPDEF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPDEF.                            *
000005*                                                                *
000006*    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
000007*                                                                *
000008*    FILE TYPE = VSAM,KSDS                                       *
000009*    RECORD SIZE = 1319 RECFORM = FIXED                          *
000010*                                                                *
000011*    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
000012*                                                                *
000013*    LOG = YES                                                   *
000014*    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000024* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000025* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
000026* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000027******************************************************************
000028 01  PRODUCT-MASTER.
000029    12  PD-RECORD-ID                 PIC X(02).
000030        88  VALID-PD-ID                  VALUE 'PD'.
000031
000032    12  PD-CONTROL-PRIMARY.
000033        16  PD-COMPANY-CD            PIC X.
000034        16  PD-STATE                 PIC XX.
000035        16  PD-PRODUCT-CD            PIC XXX.
000036        16  PD-FILLER                PIC X(7).
000037        16  PD-BEN-TYPE              PIC X.
000038        16  PD-BEN-CODE              PIC XX.
000039        16  PD-PROD-EXP-DT           PIC XX.
000040
000041    12  FILLER                       PIC X(50).
000042
000043    12  PD-PRODUCT-DATA OCCURS 8.
000044        16  PD-PROD-CODE             PIC X.
000045            88  PD-PROD-LIFE           VALUE 'L'.
000046            88  PD-PROD-PROP           VALUE 'P'.
000047            88  PD-PROD-AH             VALUE 'A'.
000048            88  PD-PROD-IU             VALUE 'I'.
000049            88  PD-PROD-GAP            VALUE 'G'.
000050            88  PD-PROD-FAML           VALUE 'F'.
000051            88  PD-PROD-OTH            VALUE 'O'.
000052        16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
000053        16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
000054        16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
000055        16  PD-MAX-TERM              PIC S999        COMP-3.
000056        16  PD-MAX-AMT               PIC S9(07)      COMP-3.
000057        16  FILLER                   PIC X.
000058        16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
000059        16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
000060        16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
000061        16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
000062        16  PD-CRIT-PERIOD           PIC S999        COMP-3.
000063        16  PD-REC-CRIT-PERIOD       PIC 99.
000064        16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
000065            20  PD-RECURRING-YN      PIC X.
000066            20  FILLER               PIC X.
000067        16  PD-RTW-MOS               PIC 99.
000068        16  PD-MAX-EXTENSION         PIC 99.
000069        16  pd-ben-pct               pic sv999 comp-3.
000070*       16  FILLER                   PIC XX.
000071
000072    12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
000073
000074    12  PD-TERM-LIMITS OCCURS 15.
000075        16  PD-LOW-TERM              PIC S999        COMP-3.
000076        16  PD-HI-TERM               PIC S999        COMP-3.
000077
000078*  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
000079    12  PD-LOAN-AMT-LIMITS OCCURS 15.
000080        16  PD-LOW-AMT               PIC S9(5)       COMP-3.
000081        16  PD-HI-AMT                PIC S9(7)       COMP-3.
000082
000083    12  PD-EARN-FACTORS.
000084        16  FILLER OCCURS 15.
000085            20  FILLER OCCURS 15.
000086                24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
000087
000088    12  PD-PRODUCT-DESC              PIC X(80).
000089    12  PD-TRUNCATED                 PIC X.
000090    12  FILLER                       PIC X(59).
000091
000092    12  PD-MAINT-INFORMATION.
000093        16  PD-LAST-MAINT-DT         PIC X(02).
000094        16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
000095        16  PD-LAST-MAINT-BY         PIC X(04).
      *<<((file: ERCPDEF))
000664*                            COPY ELCCRTT.
      *>>((file: ELCCRTT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTT.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 552  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
000023* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
000024* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
000025* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000027* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000028* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
000030* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000031* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
000032* 091318  CR2018073000001  PEMA  ADD Refund methods
000033* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000034******************************************************************
000035
000036 01  CERTIFICATE-TRAILERS.
000037     12  CS-RECORD-ID                      PIC XX.
000038         88  VALID-CS-ID                      VALUE 'CS'.
000039
000040     12  CS-CONTROL-PRIMARY.
000041         16  CS-COMPANY-CD                 PIC X.
000042         16  CS-CARRIER                    PIC X.
000043         16  CS-GROUPING                   PIC X(6).
000044         16  CS-STATE                      PIC XX.
000045         16  CS-ACCOUNT                    PIC X(10).
000046         16  CS-CERT-EFF-DT                PIC XX.
000047         16  CS-CERT-NO.
000048             20  CS-CERT-PRIME             PIC X(10).
000049             20  CS-CERT-SFX               PIC X.
000050         16  CS-TRAILER-TYPE               PIC X.
000051             88  COMM-TRLR           VALUE 'A'.
000052             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
000053             88  CERT-DATA-TRLR      VALUE 'C'.
000054
000055     12  CS-DATA-AREA                      PIC X(516).
000056
000057     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
000058         16  CS-BANK-COMMISSION-AREA.
000059             20  CS-BANK-COMMS       OCCURS 10.
000060                 24  CS-AGT                PIC X(10).
000061                 24  CS-COM-TYP            PIC X.
000062                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
000063                 24  CS-RECALC-LV-INDIC    PIC X.
000064                 24  FILLER                PIC X(10).
000065
000066         16  FILLER                        PIC X(256).
000067
000068     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
000069****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
000070
000071         16  CS-MB-CLAIM-DATA OCCURS 24.
000072             20  CS-CLAIM-NO               PIC X(7).
000073             20  CS-CLAIM-TYPE             PIC X.
000074                 88  CS-AH-CLM               VALUE 'A'.
000075                 88  CS-IU-CLM               VALUE 'I'.
000076                 88  CS-GP-CLM               VALUE 'G'.
000077                 88  CS-LF-CLM               VALUE 'L'.
000078                 88  CS-PR-CLM               VALUE 'P'.
000079                 88  CS-FL-CLM               VALUE 'F'.
000080                 88  CS-OT-CLM               VALUE 'O'.
000081             20  CS-INSURED-TYPE           PIC X.
000082                 88  CS-PRIM-INSURED          VALUE 'P'.
000083                 88  CS-CO-BORROWER           VALUE 'C'.
000084             20  CS-BENEFIT-PERIOD         PIC 99.
000085             20  CS-DAYS-PAID              PIC S9(5) COMP-3.
000086             20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
000087             20  CS-REMAINING-BENS         PIC S999 COMP-3.
000088         16  FILLER                        PIC X(12).
000089
000090     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
000091         16  CS-VIN-NUMBER                 PIC X(17).
000092         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
000093         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
000094         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
000095         16  cs-agent-name.
000096             20  cs-agent-fname            pic x(20).
000097             20  cs-agent-mi               pic x.
000098             20  cs-agent-lname            pic x(25).
000099         16  cs-license-no                 pic x(15).
000100         16  cs-npn-number                 pic x(10).
000101         16  cs-agent-edit-status          pic x.
000102             88  cs-ae-refer-to-manager      value 'M'.
000103             88  cs-ae-cover-sheet           value 'C'.
000104             88  cs-ae-sig-form              value 'S'.
000105             88  cs-ae-verified              value 'V'.
000106             88  cs-unidentified-signature   value 'U'.
000107             88  cs-cert-returned            value 'R'.
000108             88  cs-accept-no-commission     value 'N'.
000109         16  cs-year                       pic 9999.
000110         16  cs-make                       pic x(20).
000111         16  cs-model                      pic x(20).
000112         16  cs-future                     pic x(20).
000113         16  cs-vehicle-odometer           pic s9(7) comp-3.
000114         16  cs-claim-verification-status  pic x.
000115             88  cs-clm-ver-eligible         value 'A'.
000116             88  cs-clm-ver-partial-elig     value 'B'.
000117             88  cs-clm-ver-not-eligible     value 'C'.
000118             88  cs-clm-ver-not-elig-opn-clm value 'D'.
000119             88  cs-clm-ver-not-part-elig-rw value 'E'.
000120             88  cs-clm-ver-ND-CERT          value 'F'.
000121             88  cs-clm-ver-spec-other       value 'G'.
000122             88  cs-clam-ver-pratial-corrected
000123                                             value 'H'.
000124             88  cs-clm-ver-no-matches       value 'I'.
000125             88  cs-clm-ver-not-elig-corrected
000126                                             value 'J'.
000127             88  cs-clm-ver-needs-review     value 'R'.
000128             88  cs-clm-ver-sent-to-claims   value 'W'.
000129         16  CS-LF-REFUND-METHOD           PIC X.
000130         16  CS-AH-REFUND-METHOD           PIC X.
000131         16  FILLER                        PIC X(353). *> was 420
000132*        16  FILLER                        PIC X(496).
      *<<((file: ELCCRTT))
000665 01  var  pic x(30).
000666
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1278' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000668*    SERVICE RELOAD PARMLIST.
000667 VCOBOL-DUMMY-PROCEDURE.
000669
000670     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000671     MOVE '5'                    TO  DC-OPTION-CODE.
000672
000673     PERFORM 9600-DATE-LINK.
000674
000675     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000676     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000677     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000678     MOVE 2                      TO  EMI-NUMBER-OF-LINES.
000679
000680     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
000681     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
000682
000683     MOVE EIBTRMID               TO  QID-TERM.
000684
000685     IF EIBCALEN  IS EQUAL TO  ZERO
000686         GO TO 8700-UNAUTHORIZED-ACCESS.
000687
000688     IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM
000689         IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
000690             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
000691             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
000692             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
000693             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
000694             MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
000695             MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
000696             MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
000697             MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
000698         ELSE
000699             MOVE PI-CALLING-PROGRAM    TO  RETURN-FROM
000700             MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
000701             MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
000702             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
000703             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
000704             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
000705             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
000706             MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
000707             MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
000708
000709     IF EIBAID  IS EQUAL TO  DFHCLEAR
000710         GO TO 9300-CLEAR.
000711
000712     IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
000713         NEXT SENTENCE
000714     ELSE
000715         
      * EXEC CICS READQ TS
000716*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000717*            INTO    (SECURITY-CONTROL)
000718*            LENGTH  (SC-COMM-LENGTH)
000719*            ITEM    (SC-ITEM)
000720*        END-EXEC
      *    MOVE '*$II   L              ''   #00006179' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036313739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000721         MOVE SC-CREDIT-DISPLAY (33)
000722                                 TO  PI-DISPLAY-CAP
000723         MOVE SC-CREDIT-UPDATE  (33)
000724                                 TO  PI-MODIFY-CAP
000725         MOVE SC-CREDIT-UPDATE  (32)
000726                                 TO  PI-MODIFY-CERT-CAP
000727         IF NOT  DISPLAY-CAP
000728             MOVE 'READ'         TO  SM-READ
000729             PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
000730             MOVE ER-0070        TO  EMI-ERROR
000731             PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
000732             GO TO 8100-SEND-INITIAL-MAP.
000733
000734     IF EIBTRNID  IS NOT EQUAL TO  TRANS-ID
000735         MOVE 'Y' TO WS-FIRST-ENTRY-SW
000736         MOVE LOW-VALUES         TO  EL127HI
000737         MOVE SAVE-BIN-DATE      TO  WS-LF-CANCEL-DATE
000738                                     PI-LF-CANCEL-DATE
000739                                     WS-AH-CANCEL-DATE
000740                                     PI-AH-CANCEL-DATE
000741         MOVE SAVE-DATE          TO  WS-LF-CANCEL-DATE-ED
000742                                     PI-LF-CANCEL-DATE-ED
000743                                     WS-AH-CANCEL-DATE-ED
000744                                     PI-AH-CANCEL-DATE-ED
000745         MOVE SPACES             TO  PI-EARNING-METHOD-LF
000746                                     PI-EARNING-METHOD-AH
000747         PERFORM 0500-FORMAT-SCREEN
000748         GO TO 8100-SEND-INITIAL-MAP.
000749
000750     IF NOT  MODIFY-CAP
000751         MOVE 'UPDATE'           TO  SM-READ
000752         PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
000753         MOVE ER-0070            TO  EMI-ERROR
000754         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
000755         GO TO 8100-SEND-INITIAL-MAP.
000756
000757     
      * EXEC CICS HANDLE CONDITION
000758*        PGMIDERR  (9500-PGMID-ERROR)
000759*        ERROR     (9900-ABEND)
000760*    END-EXEC.
      *    MOVE '"$L.                  ! " #00006221' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303036323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000761
000762     set P to address of KIXSYS
000763     CALL "getenv" using by value P returning var-ptr
000764     if var-ptr = null then
000765        display ' kixsys not set '
000766     else
000767        set address of var to var-ptr
000768        move 0 to env-var-len
000769        inspect var tallying env-var-len
000770          for characters before X'00'
000771        unstring var (1:env-var-len) delimited by '/'
000772        into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000773              WS-KIX-SYS
000774        end-unstring
000775     end-if
000776
000777     set P to address of KIXHOST
000778     CALL "getenv" using by value P returning var-ptr
000779     if var-ptr = null then
000780        display ' kixhost not set '
000781     else
000782        set address of var to var-ptr
000783        move 0 to env-var-len
000784        inspect var tallying env-var-len
000785          for characters before X'00'
000786        MOVE var(1:env-var-len)  to ws-kixhost
000787        DISPLAY ' WS KIX HOST ' WS-KIXHOST
000788     end-if.
000789
000790 EJECT
000791 0200-RECEIVE.
000792     IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
000793         MOVE ER-0008            TO  EMI-ERROR
000794         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
000795         MOVE -1                 TO  HLCANCL
000796         GO TO 8200-SEND-DATAONLY.
000797
000798     
      * EXEC CICS RECEIVE
000799*        MAP     (MAP-NAME)
000800*        MAPSET  (MAPSET-NAME)
000801*        INTO    (EL127HI)
000802*    END-EXEC.
           MOVE LENGTH OF
            EL127HI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006262' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036323632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000803
000804     IF HPFKEYL  IS EQUAL TO  ZERO
000805         GO TO 0300-CHECK-PFKEYS.
000806
000807     IF EIBAID  IS NOT EQUAL TO  DFHENTER
000808         MOVE ER-0004            TO  EMI-ERROR
000809         GO TO 0320-INPUT-ERROR.
000810
000811     IF HPFKEYI NUMERIC
000812         IF HPFKEYI  IS EQUAL TO  '12'  OR  '23'  OR  '24' OR '11'
000813             MOVE PF-VALUES (HPFKEYI)
000814                                 TO  EIBAID
000815         ELSE
000816             MOVE ER-0029        TO  EMI-ERROR
000817             GO TO 0320-INPUT-ERROR.
000818
000819 0300-CHECK-PFKEYS.
000820     IF EIBAID  IS EQUAL TO  DFHPF23
000821         GO TO 8800-PF23.
000822
000823     IF EIBAID  IS EQUAL TO  DFHPF24
000824         GO TO 9100-RETURN-MAIN-MENU.
000825
000826     if eibaid not = dfhpf10
000827        move 'N'                 to pi-pf10-ok
000828     end-if
000829
000830     IF EIBAID = DFHPF11
000831        GO TO 1700-WRITE-CERT-NOTE
000832     END-IF
000833
000834     IF EIBAID  IS EQUAL TO  DFHPF12
000835         GO TO 9400-PF12.
000836
000837     IF EIBAID = DFHENTER OR DFHPF10
000838        GO TO 0400-EDIT-INPUT-DATA
000839     END-IF
000840
000841     MOVE ER-0029                TO  EMI-ERROR.
000842
000843 0320-INPUT-ERROR.
000844     PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
000845
000846     MOVE AL-UNBON               TO  HPFKEYA.
000847
000848     IF HPFKEYL  IS EQUAL TO  ZERO
000849         MOVE -1                 TO  HLCANCL
000850     ELSE
000851         MOVE -1                 TO  HPFKEYL.
000852
000853     GO TO 8200-SEND-DATAONLY.
000854 EJECT
000855 0400-EDIT-INPUT-DATA.
000856     MOVE 'N'                    TO  WS-NEW-AH-CANCEL-SW
000857                                     WS-NEW-LF-CANCEL-SW.
000858
000859
000860     IF (HPCYNL > 0)
000861        AND (HPCYNI = 'Y' OR 'N')
000862        PERFORM 1800-READ-ELCERT-UPDATE
000863                                 THRU 1800-EXIT
000864        IF WS-RESP-NORMAL
000865           MOVE HPCYNI           TO CM-POST-CARD-IND
000866           PERFORM 1810-REWRITE-ELCERT
000867                                 THRU 1810-EXIT
000868        END-IF
000869     END-IF
000870
000871     IF HLCAL1L GREATER ZERO
000872         MOVE HLCAL1I              TO  PI-EARNING-METHOD-LF.
000873     IF CLPYNI = 'Y'
000874        MOVE 'Y'                   TO PI-CLP-YN
000875        MOVE AL-UANON              TO CLPYNA
000876     ELSE
000877        MOVE ' '                   TO PI-CLP-YN
000878        MOVE AL-UANON              TO CLPYNA
000879     END-IF
000880     IF CANREAI = 'R'
000881        MOVE 'R'                 TO PI-CANCEL-REASON
000882        MOVE AL-UANON            TO CANREAA
000883     ELSE
000884        IF CANREAI > SPACES
000885           MOVE CANREAI             TO PI-CANCEL-REASON
000886           MOVE AL-UANON            TO CANREAA
000887        END-IF
000888     END-IF
000889
000890     PERFORM 0450-CHECK-CANCEL-REASON THRU 0450-EXIT.
000891
000892     IF HLCANCL  IS EQUAL TO  ZERO
000893       AND HACANCL  IS EQUAL TO  ZERO
000894         MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
000895         MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
000896         MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
000897         MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
000898         GO TO 0420-CHECK-ERRORS.
000899
000900     IF HLCANCL  IS EQUAL TO  ZERO
000901         MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
000902         MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
000903         GO TO 0410-CHECK-AH-DATE.
000904
000905     IF HLCANCI = SPACES
000906        MOVE LOW-VALUES            TO PI-LF-CANCEL-DATE
000907                                      WS-LF-CANCEL-DATE
000908        MOVE SPACES                TO PI-LF-CANCEL-DATE-ED
000909                                      WS-LF-CANCEL-DATE-ED
000910                                      PI-LF-REFUND-METH
000911        MOVE +0                    TO PI-LF-REFUND-AMT
000912        GO TO 0410-CHECK-AH-DATE
000913     END-IF
000914
000915     MOVE AL-UNNON               TO  HLCANCA.
000916     MOVE HLCANCI                TO  DEEDIT-FIELD.
000917     MOVE 'Y'                    TO  WS-NEW-LF-CANCEL-SW.
000918
000919     PERFORM 8600-DEEDIT.
000920
000921     MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
000922     MOVE '4'                    TO  DC-OPTION-CODE.
000923
000924     PERFORM 9600-DATE-LINK.
000925
000926     IF DATE-CONVERSION-ERROR
000927         MOVE -1                 TO  HLCANCL
000928         MOVE AL-UABON           TO  HLCANCA
000929         MOVE ER-2227            TO  EMI-ERROR
000930         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
000931     ELSE
000932        IF PI-CERT-EFF-DT > DC-BIN-DATE-1
000933           MOVE -1               TO  HACANCL
000934           MOVE AL-UABON         TO  HACANCA
000935           MOVE ER-2774          TO  EMI-ERROR
000936           PERFORM 9700-ERROR-FORMAT
000937                                 THRU  9799-EXIT
000938        ELSE
000939           MOVE DC-BIN-DATE-1    TO WS-LF-CANCEL-DATE
000940                                    PI-LF-CANCEL-DATE
000941           MOVE DC-GREG-DATE-1-EDIT
000942                                 TO HLCANCO
000943                                    WS-LF-CANCEL-DATE-ED
000944                                    PI-LF-CANCEL-DATE-ED
000945        END-IF
000946     END-IF
000947
000948     .
000949 0410-CHECK-AH-DATE.
000950     IF HACAL1L GREATER ZERO
000951         MOVE HACAL1I              TO  PI-EARNING-METHOD-AH.
000952
000953     IF HACANCL  IS EQUAL TO  ZERO
000954         MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
000955         MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
000956         GO TO 0420-CHECK-ERRORS.
000957
000958     IF HACANCI = SPACES
000959        MOVE LOW-VALUES            TO PI-AH-CANCEL-DATE
000960                                      WS-AH-CANCEL-DATE
000961        MOVE SPACES                TO PI-AH-CANCEL-DATE-ED
000962                                      WS-AH-CANCEL-DATE-ED
000963                                      PI-AH-REFUND-METH
000964        MOVE +0                    TO PI-AH-REFUND-AMT
000965        GO TO 0420-CHECK-ERRORS
000966     END-IF
000967
000968
000969     MOVE AL-UNNON               TO  HACANCA.
000970     MOVE HACANCI                TO  DEEDIT-FIELD.
000971     MOVE 'Y'                    TO  WS-NEW-AH-CANCEL-SW.
000972
000973     PERFORM 8600-DEEDIT.
000974
000975     MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
000976     MOVE '4'                    TO  DC-OPTION-CODE.
000977
000978     PERFORM 9600-DATE-LINK.
000979
000980     IF DATE-CONVERSION-ERROR
000981         MOVE -1                 TO  HACANCL
000982         MOVE AL-UABON           TO  HACANCA
000983         MOVE ER-2227            TO  EMI-ERROR
000984         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
000985     ELSE
000986        IF PI-CERT-EFF-DT > DC-BIN-DATE-1
000987           MOVE -1               TO  HACANCL
000988           MOVE AL-UABON         TO  HACANCA
000989           MOVE ER-2775          TO  EMI-ERROR
000990           PERFORM 9700-ERROR-FORMAT
000991                                 THRU  9799-EXIT
000992        ELSE
000993           MOVE DC-BIN-DATE-1    TO WS-AH-CANCEL-DATE
000994                                    PI-AH-CANCEL-DATE
000995           MOVE DC-GREG-DATE-1-EDIT
000996                                 TO HACANCO
000997                                    WS-AH-CANCEL-DATE-ED
000998                                    PI-AH-CANCEL-DATE-ED
000999        END-IF
001000     END-IF
001001
001002     .
001003 0420-CHECK-ERRORS.
001004
001005     IF EMI-ERROR  IS EQUAL TO  ZEROS
001006         NEXT SENTENCE
001007     ELSE
001008         GO TO 8200-SEND-DATAONLY.
001009
001010
001011
001012     PERFORM 0500-FORMAT-SCREEN.
001013
001014     if eibaid = dfhpf10
001015        if pi-pf10-ok not = 'Y'
001016           move 'Y'              to pi-pf10-ok
001017           move er-3847          to emi-error
001018           move -1               to hlcancl
001019           perform 9700-error-format
001020                                 thru 9799-exit
001021           go to 8200-send-dataonly
001022        end-if
001023     end-if
001024
001025     IF EIBAID = DFHPF10
001026        perform 4000-GEN-CANCEL-TRANS
001027                                 thru 4000-exit
001028        MOVE PROGRAM-INTERFACE-BLOCK TO WS-PASS-631
001029        MOVE LOW-VALUES     TO WS-PASS-PROGRAM-WORK-AREA
001030        MOVE PI-COMPANY-CD  TO PI-PB-COMPANY-CD
001031        MOVE CG-BATCH-NO    TO PI-PB-ENTRY-BATCH
001032        MOVE 1              TO PI-PB-BATCH-SEQ-NO
001033        MOVE 'Y'            TO PI-ALL-ISSUES-SW
001034                               PI-ALL-CANCELS-SW
001035        IF PI-PROCESSOR-IS-CSR
001036            MOVE 'Y'        TO PI-CSR-SESSION-SW
001037        ELSE
001038            MOVE ' '        TO PI-CSR-SESSION-SW
001039        END-IF
001040        MOVE 'N'            TO PI-ISSUES-IN-ERROR-SW
001041                               PI-CANCELS-IN-ERROR-SW
001042                               PI-ONLY-BATCH-HEADERS-SW
001043                               PI-ALL-OUT-OF-BAL-SW
001044                               PI-HOLD-REC-SW
001045                               PI-CHANGE-REC-SW
001046                               PI-CHK-REQ-REC-SW
001047                               PI-ISSUE-WARNING-SW
001048                               PI-CANCEL-WARNING-SW
001049        MOVE '1'            TO PI-BROWSE-TYPE
001050        MOVE DFHENTER       TO  EIBAID
001051        MOVE PI-CANCEL-REASON TO PI-CANCEL-REASON-631
001052
001053        
      * EXEC CICS XCTL
001054*           PROGRAM    ('EL6311')
001055*           COMMAREA   (WS-PASS-631)
001056*           LENGTH     (1300)
001057*       END-EXEC
           MOVE 'EL6311' TO DFHEIV1
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   %   #00006517' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036353137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-PASS-631, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001058
001059*        PERFORM 0500-FORMAT-SCREEN
001060     END-IF
001061
001062     GO TO 8100-SEND-INITIAL-MAP.
001063 0450-CHECK-CANCEL-REASON.
001064
001065*      Read Napersoft CancelReasons Lookup Table
001066     MOVE 'naperadmin'           TO USR
001067     MOVE 'cCm8naper'            TO PASS
001068     IF WS-KIXHOST = 'logictest'
001069        MOVE 'HOVTSTDB01_NaperRepo'
001070                                 TO SVR
001071        MOVE '1029'              TO WS-LOOKUPID
001072     ELSE
001073        MOVE 'SDVDB01_NaperRepo' TO SVR
001074        MOVE '1029'              TO WS-LOOKUPID
001075     END-IF
001076
001077     MOVE SPACES                 TO WS-LOOKUP-VALUE
001078
001079     PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
001080     MOVE PI-CANCEL-REASON TO WS-LOOKUPNAME
001081
001083     EXEC SQL
                 SELECT LOOKUPVALUE
001084             INTO :WS-LOOKUP-VALUE
001085             FROM LOOKUPVALUES
001086               WHERE LOOKUPID = :WS-LOOKUPID
001087                 AND LOOKUPNAME = :WS-LOOKUPNAME
001088     END-EXEC
001089
001090     IF SQLCODE = 0
001091        CONTINUE
001092     ELSE
001093     IF SQLCODE = 100
001094         MOVE -1                    TO CANREAL
001095         MOVE AL-UANON              TO CANREAA
001096         MOVE ER-1590               TO EMI-ERROR
001097         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
001098     ELSE
001099        DISPLAY "ERROR: INVALID CancelReasons Lookup Table SELECT"
001100        DISPLAY ' SQL RETURN CODE ' SQLCODE
001101        DISPLAY ' SQL ERR MESS    ' SQLERRMC
001102     END-IF.
001103
001104     PERFORM 4300-DISCONNECT THRU 4300-EXIT.
001105
001106 0450-EXIT.
001107     EXIT.
001108
001109 EJECT
001110 0500-FORMAT-SCREEN  SECTION.
001111     MOVE LOW-VALUES             TO  EL127HO.
001112     MOVE PI-CARRIER             TO  HCARIERO.
001113     MOVE PI-GROUPING            TO  HGROUPO.
001114     MOVE PI-STATE               TO  HSTATEO.
001115     MOVE PI-ACCOUNT             TO  HACCTNOO.
001116     MOVE ' '                    TO  DC-OPTION-CODE.
001117     MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.
001118
001119     PERFORM 9600-DATE-LINK.
001120
001121     MOVE DC-GREG-DATE-1-EDIT    TO  HEFFDTO.
001122     MOVE PI-CERT-PRIME          TO  HCERTNOO.
001123     MOVE PI-CERT-SFX            TO  HCRTSFXO.
001124     MOVE PI-CLP-YN              TO  CLPYNO
001125     MOVE PI-CANCEL-REASON       TO  CANREAO
001126
001127     .
001128 0510-GET-CERT.
001129     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
001130     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
001131     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
001132     MOVE PI-STATE               TO  WS-CM-STATE.
001133     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
001134     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
001135     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
001136     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
001137
001138     PERFORM 0900-READ-CERT-FILE.
001139
001140     IF CERT-NOT-FOUND
001141         MOVE -1                 TO  HLCANCL
001142         MOVE AL-SANON           TO  HLCANCA
001143         MOVE ER-0214            TO  EMI-ERROR
001144         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
001145         GO TO 8100-SEND-INITIAL-MAP.
001146
001147     IF CM-CLAIM-ATTACHED-COUNT = +0
001148        GO TO 0510-SKIP-CLAIM-CHECK
001149     END-IF
001150
001151*    DISPLAY ' STARTING CLAIM TEST '
001152
001153     MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
001154     MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
001155     
      * EXEC CICS STARTBR
001156*       DATASET     ('ELMSTR5')
001157*       RIDFLD      (ELMSTR-KEY)
001158*       RESP        (WS-RESPONSE)
001159*    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006619' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303036363139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001160
001161     IF WS-RESP-NORMAL
001162
001163     PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
001164
001165        
      * EXEC CICS READNEXT
001166*          DATASET   ('ELMSTR5')
001167*          RIDFLD    (ELMSTR-KEY)
001168*          SET       (ADDRESS OF CLAIM-MASTER)
001169*          RESP      (WS-RESPONSE)
001170*       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00006629' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303036363239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001171        IF (WS-RESP-NORMAL OR WS-RESP-DUPKEY)
001172           AND (CM-COMPANY-CD = CL-COMPANY-CD)
001173           AND (CM-CERT-NO = CL-CERT-NO)
001174           IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
001175              AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
001176              SET FOUND-A-CLAIM  TO TRUE
001177              MOVE CL-CLAIM-TYPE TO WS-CLAIM-TYPE
001178              IF CL-CLAIM-TYPE = 'L' OR 'O'
001179                 IF CL-INCURRED-DT > WS-LF-INCUR-DT
001180                    MOVE CL-INCURRED-DT TO WS-LF-INCUR-DT
001181                 END-IF
001182                 IF CLAIM-IS-OPEN
001183                    SET OPEN-LF-CLAIM TO TRUE
001184                 ELSE
001185                    IF CL-LAST-CLOSE-DT > WS-LF-LAST-CLOSE-DT
001186                       MOVE CL-LAST-CLOSE-DT
001187                                 TO WS-LF-LAST-CLOSE-DT
001188                    END-IF
001189                    SET CLOSED-LF-CLAIM TO TRUE
001190                 END-IF
001191              ELSE
001192                 IF CLAIM-IS-OPEN
001193                    MOVE CL-PAID-THRU-DT
001194                                 TO WS-AH-PAID-THRU-DT
001195                    SET OPEN-AH-CLAIM TO TRUE
001196                 ELSE
001197                    IF CL-PAID-THRU-DT > WS-AH-PAID-THRU-DT
001198                       MOVE CL-PAID-THRU-DT
001199                                 TO WS-AH-PAID-THRU-DT
001200                    END-IF
001201                    IF CL-LAST-CLOSE-DT > WS-AH-LAST-CLOSE-DT
001202                       MOVE CL-LAST-CLOSE-DT
001203                                 TO WS-AH-LAST-CLOSE-DT
001204                    END-IF
001205                    SET CLOSED-AH-CLAIM TO TRUE
001206                 END-IF
001207              END-IF
001208           END-IF
001209        ELSE
001210           SET I-SAY-TO-STOP TO TRUE
001211        END-IF
001212     END-PERFORM
001213
001214     END-IF
001215
001216     IF FOUND-A-CLAIM
001217        IF WS-LF-INCUR-DT NOT = LOW-VALUES
001218           MOVE WS-LF-INCUR-DT   TO DC-BIN-DATE-1
001219           MOVE ' '              TO DC-OPTION-CODE
001220           PERFORM 9600-DATE-LINK
001221           IF NO-CONVERSION-ERROR
001222              MOVE DC-GREG-DATE-1-EDIT TO LCLMDTO
001223              MOVE AL-SANOF      TO LCLMDTHA
001224           END-IF
001225        END-IF
001226        IF WS-CLAIM-TYPE = 'O'
001227           MOVE 'CLAIM TYPE O INCURRED' TO LCLMDTHO
001228           MOVE 'OTHER' TO WS-CLAIM-LIT
001229        ELSE
001230           MOVE 'LIFE ' TO WS-CLAIM-LIT
001231        END-IF
001232
001233        IF WS-AH-PAID-THRU-DT NOT = LOW-VALUES
001234           MOVE WS-AH-PAID-THRU-DT TO DC-BIN-DATE-1
001235           MOVE ' '              TO DC-OPTION-CODE
001236           PERFORM 9600-DATE-LINK
001237           IF NO-CONVERSION-ERROR
001238              MOVE DC-GREG-DATE-1-EDIT TO ACLMDTO
001239              MOVE AL-SANOF      TO ACLMDTHA
001240           END-IF
001241        END-IF
001242        MOVE PI-COMPANY-ID       TO WS-CF-COMPANY-ID
001243        MOVE '3'                 TO WS-CF-RECORD-TYPE
001244        MOVE CM-STATE            TO WS-CF-ACCESS
001245        MOVE +0                  TO WS-CF-SEQUENCE-NO
001246
001247        
      * EXEC CICS READ
001248*          DATASET  (ELCNTL-ID)
001249*          SET      (ADDRESS OF CONTROL-FILE)
001250*          RIDFLD   (WS-CF-CONTROL-PRIMARY)
001251*          RESP     (WS-RESPONSE)
001252*       END-EXEC
      *    MOVE '&"S        E          (  N#00006711' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001253
001254        IF WS-RESP-NORMAL
001255           MOVE CF-ST-REF-AH-DEATH-IND
001256                                 TO WS-ST-REF-IND
001257        END-IF
001258     END-IF
001259
001260     .
001261 0510-SKIP-CLAIM-CHECK.
001262
001263     MOVE CM-INSURED-LAST-NAME   TO  HLNAMEO.
001264     MOVE CM-INSURED-FIRST-NAME  TO  HFNAMEO.
001265     MOVE CM-INSURED-INITIAL2    TO  HINITO.
001266
001267     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY
001268
001269     
      * EXEC CICS READ
001270*        DATASET  (ERMAIL-ID)
001271*        RIDFLD   (WS-MA-CONTROL-PRIMARY)
001272*        SET      (ADDRESS OF MAILING-DATA)
001273*        RESP     (WS-RESPONSE)
001274*    END-EXEC.
      *    MOVE '&"S        E          (  N#00006733' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036373333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001275
001276     IF WS-RESP-NORMAL
001277        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001278           (S1 > +7)
001279           OR (MA-MAIL-STATUS (S1) = '1')
001280        END-PERFORM
001281        IF S1 > +7
001282           MOVE AL-SANOF         TO HPCYNA
001283        END-IF
001284     END-IF
001285
001286     IF CM-POST-CARD-IND = 'Y'
001287        MOVE 'Y'                 TO HPCYNO
001288     ELSE
001289        MOVE 'N'                 TO HPCYNO
001290     END-IF
001291     IF ('8' NOT = CM-LF-CURRENT-STATUS AND CM-AH-CURRENT-STATUS)
001292        OR (NOT CERT-AS-LOADED)
001293        MOVE AL-SANOF            TO HPCYNA
001294     END-IF
001295
001296*    IF (CERT-ADDED-BATCH)
001297*       CONTINUE
001298*    ELSE
001299*       MOVE AL-SANOF            TO HPCYNA
001300*    END-IF
001301     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001302     MOVE '5'                    TO  DC-OPTION-CODE.
001303
001304     PERFORM 9600-DATE-LINK.
001305
001306     MOVE DC-BIN-DATE-1          TO  DC-BIN-DATE-2.
001307     MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
001308     MOVE '1'                    TO  DC-OPTION-CODE.
001309
001310     PERFORM 9600-DATE-LINK.
001311
001312     MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.
001313
001314     MOVE ZEROS TO WS-TOT-ITDR.
001315     MOVE WS-REFUND-DESCRIP TO PMTHDO.
001316
001317 0510-CHECK-LF-STATUS.
001318
001319     IF (CM-LF-BENEFIT-CD IS EQUAL TO '00' OR '  ')
001320         MOVE LOW-VALUES         TO WS-LF-CANCEL-DATE
001321                                    PI-LF-CANCEL-DATE
001322         MOVE SPACES             TO WS-LF-CANCEL-DATE-ED
001323                                    PI-LF-CANCEL-DATE-ED
001324                                    PI-LF-REFUND-METH
001325         MOVE ZEROS              TO PI-LF-REFUND-AMT
001326         GO TO 0510-CHECK-AH-STATUS.
001327
001328     IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '8') AND
001329        (CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
001330          MOVE CM-LF-ITD-CANCEL-AMT TO HLITDRO
001331          ADD  CM-LF-ITD-CANCEL-AMT TO WS-TOT-ITDR
001332          IF FIRST-ENTRY
001333              MOVE CM-LF-CANCEL-DT      TO DC-BIN-DATE-1
001334              MOVE SPACES               TO DC-OPTION-CODE
001335              PERFORM 9600-DATE-LINK
001336              IF NOT DATE-CONVERSION-ERROR
001337                   MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
001338                                            PI-LF-CANCEL-DATE-ED
001339                                            WS-LF-CANCEL-DATE-ED
001340                   MOVE CM-LF-CANCEL-DT TO  PI-LF-CANCEL-DATE
001341                                            WS-LF-CANCEL-DATE.
001342
001343     IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '7')  AND
001344        (CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES)
001345          MOVE WS-PMT-DESCRIP       TO PMTHDO
001346          MOVE CM-LF-ITD-DEATH-AMT  TO HLITDRO
001347          ADD  CM-LF-ITD-DEATH-AMT TO WS-TOT-ITDR
001348        IF FIRST-ENTRY
001349            MOVE CM-LF-DEATH-DT       TO DC-BIN-DATE-1
001350            MOVE SPACES               TO DC-OPTION-CODE
001351            PERFORM 9600-DATE-LINK
001352            IF NOT DATE-CONVERSION-ERROR
001353                 MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
001354                                          PI-LF-CANCEL-DATE-ED
001355                                          WS-LF-CANCEL-DATE-ED
001356                 MOVE CM-LF-DEATH-DT  TO  PI-LF-CANCEL-DATE
001357                                          WS-LF-CANCEL-DATE.
001358
001359 0510-CHECK-AH-STATUS.
001360
001361     IF (CM-AH-BENEFIT-CD IS EQUAL TO '00' OR '  ')
001362         MOVE LOW-VALUES         TO WS-AH-CANCEL-DATE
001363                                    PI-AH-CANCEL-DATE
001364         MOVE SPACES             TO WS-AH-CANCEL-DATE-ED
001365                                    PI-AH-CANCEL-DATE-ED
001366                                    PI-AH-REFUND-METH
001367         MOVE ZEROS              TO PI-AH-REFUND-AMT
001368         GO TO 0510-CONT.
001369
001370     IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '8')  AND
001371        (CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
001372          MOVE CM-AH-ITD-CANCEL-AMT TO HAITDRO
001373          ADD  CM-AH-ITD-CANCEL-AMT TO WS-TOT-ITDR
001374        IF FIRST-ENTRY
001375            MOVE CM-AH-CANCEL-DT      TO DC-BIN-DATE-1
001376            MOVE SPACES               TO DC-OPTION-CODE
001377            PERFORM 9600-DATE-LINK
001378            IF NOT DATE-CONVERSION-ERROR
001379                MOVE DC-GREG-DATE-1-EDIT TO HACANCO
001380                                            PI-AH-CANCEL-DATE-ED
001381                                            WS-AH-CANCEL-DATE-ED
001382                 MOVE CM-AH-CANCEL-DT  TO   PI-AH-CANCEL-DATE
001383                                            WS-AH-CANCEL-DATE.
001384
001385     IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '6' OR '7')   AND
001386        (CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES)
001387          MOVE WS-PMT-DESCRIP       TO PMTHDO
001388          MOVE CM-AH-ITD-LUMP-PMT   TO HAITDRO
001389          ADD  CM-AH-ITD-LUMP-PMT   TO WS-TOT-ITDR
001390         IF FIRST-ENTRY
001391             MOVE CM-AH-SETTLEMENT-DT  TO DC-BIN-DATE-1
001392             MOVE SPACES               TO DC-OPTION-CODE
001393             PERFORM 9600-DATE-LINK
001394             IF NOT DATE-CONVERSION-ERROR
001395                MOVE DC-GREG-DATE-1-EDIT TO HACANCO
001396                                            PI-AH-CANCEL-DATE-ED
001397                                            WS-AH-CANCEL-DATE-ED
001398                MOVE CM-AH-SETTLEMENT-DT TO PI-AH-CANCEL-DATE
001399                                            WS-AH-CANCEL-DATE.
001400
001401     IF CM-AH-CURRENT-STATUS IS EQUAL TO '8'
001402         MOVE 'CANC DT'              TO  CNCDTHDO
001403         GO TO 0510-CONT.
001404
001405     IF CM-LF-CURRENT-STATUS IS EQUAL TO '7'
001406         MOVE 'DTH DT '              TO  CNCDTHDO
001407         IF FIRST-ENTRY
001408             MOVE CM-LF-DEATH-DT     TO  DC-BIN-DATE-1
001409             MOVE ' '                TO  DC-OPTION-CODE
001410             PERFORM 9600-DATE-LINK
001411             IF NOT DATE-CONVERSION-ERROR
001412                 MOVE DC-GREG-DATE-1-EDIT TO HACANCO
001413                                             PI-AH-CANCEL-DATE-ED
001414                                             WS-AH-CANCEL-DATE-ED
001415                 MOVE CM-LF-DEATH-DT      TO PI-AH-CANCEL-DATE
001416                                             WS-AH-CANCEL-DATE.
001417
001418 0510-CONT.
001419
001420     IF WS-TOT-ITDR GREATER THAN ZEROS
001421         MOVE WS-TOT-ITDR TO TOITDRO.
001422
001423 0520-GET-COMPANY-RECORD.
001424     MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
001425     MOVE '1'                    TO  WS-CF-RECORD-TYPE.
001426     MOVE SPACES                 TO  WS-CF-ACCESS.
001427     MOVE +0                     TO  WS-CF-SEQUENCE-NO.
001428
001429     PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
001430
001431     IF CNTL-NOT-FOUND
001432         MOVE -1                 TO  HLCANCL
001433         MOVE AL-SANON           TO  HLCANCA
001434         MOVE ER-2616            TO  EMI-ERROR
001435         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
001436         GO TO 8100-SEND-INITIAL-MAP.
001437
001438     IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
001439       AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
001440         MOVE CF-LIFE-OVERRIDE-L1
001441                                 TO  WS-CF-LIFE-OVERRIDE-L1
001442         MOVE CF-LIFE-OVERRIDE-L2
001443                                 TO  WS-CF-LIFE-OVERRIDE-L2
001444         MOVE CF-AH-OVERRIDE-L1  TO  WS-CF-AH-OVERRIDE-L1
001445         MOVE CF-AH-OVERRIDE-L2  TO  WS-CF-AH-OVERRIDE-L2
001446         MOVE CF-CR-REM-TERM-CALC
001447                                 TO  WS-CF-CR-REM-TERM-CALC
001448         MOVE CF-CR-R78-METHOD   TO  WS-CF-CR-R78-METHOD
001449         IF CF-DEFAULT-APR NUMERIC
001450             MOVE CF-DEFAULT-APR     TO  WS-CF-DEFAULT-APR
001451             GO TO 0525-GET-LF-BENEFIT
001452         ELSE
001453             MOVE ZEROS              TO  WS-CF-DEFAULT-APR
001454             GO TO 0525-GET-LF-BENEFIT.
001455
001456     MOVE -1                     TO  HLCANCL.
001457     MOVE AL-SANON               TO  HLCANCA.
001458     MOVE ER-2616                TO  EMI-ERROR.
001459
001460     PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
001461
001462     GO TO 8100-SEND-INITIAL-MAP.
001463
001464 0525-GET-LF-BENEFIT.
001465
001466     IF CM-LF-BENEFIT-CD = '  ' OR '00'
001467        GO TO 0527-GET-AH-BENEFIT
001468     END-IF
001469
001470
001471
001472     MOVE '4'                    TO  WS-CF-RECORD-TYPE.
001473     MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO
001474                                     HLCDO.
001475     MOVE WS-CF-LIFE-OVERRIDE-L2
001476                                 TO  HLKINDO.
001477
001478     PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
001479
001480     IF BENEFIT-FOUND
001481         NEXT SENTENCE
001482     ELSE
001483         MOVE -1                 TO  HLCDL
001484         MOVE AL-SANON           TO  HLCDA
001485         MOVE ER-0028            TO  EMI-ERROR
001486         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
001487         GO TO 8100-SEND-INITIAL-MAP.
001488
001489     IF CF-SUMMARY-PROCESSING (WS-INDEX)
001490         MOVE AL-UNNOF           TO  HLTERMA.
001491
001492*****MOVE WS-BENEFIT-DESCRIP     TO  HLDESCO.
001493     MOVE WS-KIND                TO WS-LF-KIND
001494     MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)
001495                                 TO  WS-CF-LF-COVERAGE-TYPE.
001496
001497     IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
001498         MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
001499                                 TO  WS-LF-CO-REM-TERM-CALC.
001500
001501     IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
001502         MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
001503                                 TO  WS-LF-CO-EARNINGS-CALC
001504                                     WS-LF-CO-REFUND-CALC.
001505
001506     IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
001507         MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
001508                                 TO  WS-LF-SPECIAL-CALC-CD.
001509
001510     IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
001511         MOVE CF-CO-REFUND-CALC (WS-INDEX)
001512                                 TO  WS-LF-CO-REFUND-CALC.
001513
001514 0527-GET-AH-BENEFIT.
001515
001516     IF CM-AH-BENEFIT-CD = '00' OR '  '
001517        GO TO 0530-GET-STATE-RECORD
001518     END-IF
001519
001520     MOVE '5'                    TO  WS-CF-RECORD-TYPE.
001521     MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO
001522                                     HACDO.
001523     MOVE WS-CF-AH-OVERRIDE-L2   TO  HAKINDO.
001524
001525     PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
001526
001527     IF BENEFIT-FOUND
001528         NEXT SENTENCE
001529     ELSE
001530         MOVE -1                 TO  HACDL
001531         MOVE AL-SANON           TO  HACDA
001532         MOVE ER-0028            TO  EMI-ERROR
001533         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
001534         GO TO 8100-SEND-INITIAL-MAP.
001535
001536     IF CF-SUMMARY-PROCESSING (WS-INDEX)
001537         MOVE AL-UNNOF           TO  HATERMA.
001538
001539*****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
001540     MOVE WS-KIND                TO  WS-AH-KIND
001541
001542     IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
001543         MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
001544                                 TO  WS-AH-CO-REM-TERM-CALC.
001545
001546     IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
001547         MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
001548                                 TO  WS-AH-CO-EARNINGS-CALC
001549                                     WS-AH-CO-REFUND-CALC.
001550
001551     IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
001552         MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
001553                                 TO  WS-AH-SPECIAL-CALC-CD.
001554
001555     IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
001556         MOVE CF-CO-REFUND-CALC (WS-INDEX)
001557                                 TO  WS-AH-CO-REFUND-CALC.
001558
001559     IF CF-BENEFIT-CATEGORY (WS-INDEX) > ' '
001560        MOVE CF-BENEFIT-CATEGORY (WS-INDEX)
001561                                 TO WS-AH-BEN-CATEGORY
001562     END-IF
001563
001564     .
001565 0530-GET-STATE-RECORD.
001566     MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
001567     MOVE '3'                    TO  WS-CF-RECORD-TYPE.
001568     MOVE CM-STATE               TO  WS-CF-ACCESS.
001569     MOVE +0                     TO  WS-CF-SEQUENCE-NO.
001570
001571     PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
001572
001573     IF CNTL-NOT-FOUND
001574         MOVE '0'                TO  WS-LF-ST-REFUND-CALC
001575                                     WS-LF-ST-REM-TERM-CALC
001576         MOVE 'N'                TO  WS-STATE-RECORD-SW
001577         GO TO 0560-FIND-ACCOUNT.
001578
001579     IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
001580       AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
001581       AND CM-STATE  IS EQUAL TO  CF-STATE-CODE
001582         MOVE 'Y'                TO  WS-STATE-RECORD-SW
001583     ELSE
001584         MOVE '0'                TO  WS-LF-ST-REFUND-CALC
001585                                     WS-LF-ST-REM-TERM-CALC
001586         MOVE 'N'                TO  WS-STATE-RECORD-SW
001587         GO TO 0560-FIND-ACCOUNT.
001588
001589     MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREVIATION.
001590     MOVE CF-ST-FST-PMT-DAYS-CHG TO  WS-STATE-EXT-DAYS-CHG
001591
001592     IF CM-LF-BENEFIT-CD  = '00'
001593         GO TO 0550-GET-AH-BEN-IN-STATE.
001594
001595 0540-GET-LF-BEN-IN-STATE.
001596     MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
001597     MOVE PI-LIFE-OVERRIDE-L1    TO  WS-LOOKUP-TYPE.
001598
001599     MOVE '0'                    TO WS-LF-ST-REM-TERM-CALC
001600     IF CF-ST-RT-CALC NOT = SPACES
001601        MOVE CF-ST-RT-CALC       TO WS-LF-ST-REM-TERM-CALC
001602     END-IF
001603
001604     IF WS-CF-LF-COVERAGE-TYPE = 'R'
001605        IF CF-ST-RF-LR-CALC > '0'
001606           MOVE CF-ST-RF-LR-CALC TO WS-LF-ST-REFUND-CALC
001607        END-IF
001608        IF WS-LF-CO-EARNINGS-CALC = 'N' OR '5'
001609           IF CF-ST-RF-LN-CALC > '0'
001610              MOVE CF-ST-RF-LN-CALC
001611                                 TO WS-LF-ST-REFUND-CALC
001612           END-IF
001613        END-IF
001614     ELSE
001615        IF CF-ST-RF-LL-CALC > '0'
001616           MOVE CF-ST-RF-LL-CALC TO WS-LF-ST-REFUND-CALC
001617        END-IF
001618     END-IF
001619
001620     PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
001621
001622     IF NO-BENEFIT-FOUND
001623*        MOVE '0'                TO  WS-LF-ST-REFUND-CALC
001624         GO TO 0550-GET-AH-BEN-IN-STATE.
001625
001626     IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
001627         MOVE CF-ST-REM-TERM-CALC (SUB3)
001628                                 TO  WS-LF-ST-REM-TERM-CALC.
001629
001630     IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
001631         MOVE CF-ST-REFUND-CALC (SUB3)
001632                                 TO  WS-LF-ST-REFUND-CALC.
001633
001634 0550-GET-AH-BEN-IN-STATE.
001635     IF CM-AH-BENEFIT-CD  = '00'
001636         GO TO 0560-FIND-ACCOUNT.
001637
001638     MOVE '0'                    TO WS-AH-ST-REM-TERM-CALC
001639     IF CF-ST-RT-CALC NOT = SPACES
001640        MOVE CF-ST-RT-CALC       TO WS-AH-ST-REM-TERM-CALC
001641     END-IF
001642
001643     IF CF-ST-RF-AH-CALC > '0'
001644        MOVE CF-ST-RF-AH-CALC    TO WS-AH-ST-REFUND-CALC
001645     END-IF
001646     IF WS-AH-SPECIAL-CALC-CD = 'C'
001647        IF CF-ST-RF-CP-CALC > '0'
001648           MOVE CF-ST-RF-CP-CALC TO WS-AH-ST-REFUND-CALC
001649        END-IF
001650     END-IF
001651
001652     MOVE CM-AH-BENEFIT-CD       TO  WS-BEN-CD.
001653     MOVE PI-AH-OVERRIDE-L1      TO  WS-LOOKUP-TYPE.
001654
001655     PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
001656
001657     IF NO-BENEFIT-FOUND
001658*        MOVE '0'                TO  WS-AH-ST-REFUND-CALC
001659         GO TO 0560-FIND-ACCOUNT.
001660
001661     IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
001662         MOVE CF-ST-REM-TERM-CALC (SUB3)
001663                                 TO  WS-AH-ST-REM-TERM-CALC.
001664
001665     IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
001666         MOVE CF-ST-REFUND-CALC (SUB3)
001667                                 TO  WS-AH-ST-REFUND-CALC.
001668
001669 0560-FIND-ACCOUNT.
001670     MOVE CM-COMPANY-CD          TO  WS-AM-COMPANY-CD.
001671     MOVE CM-CARRIER             TO  WS-AM-CARRIER.
001672     MOVE CM-GROUPING            TO  WS-AM-GROUPING.
001673     MOVE CM-STATE               TO  WS-AM-STATE.
001674     MOVE CM-ACCOUNT             TO  WS-AM-ACCOUNT.
001675     MOVE CM-CERT-EFF-DT         TO  WS-AM-EXPIRATION-DT.
001676     MOVE LOW-VALUES             TO  WS-AM-FILLER.
001677
001678     PERFORM 1200-START-ACCOUNT-MASTER  THRU  1299-EXIT.
001679
001680     IF ACCT-FOUND
001681         NEXT SENTENCE
001682     ELSE
001683         GO TO 0585-NO-ACCT.
001684
001685 0570-READ-NEXT-RANGE.
001686     PERFORM 1300-READ-ACCOUNT-MASTER  THRU  1399-EXIT.
001687
001688     IF ACCT-FOUND
001689         NEXT SENTENCE
001690     ELSE
001691         GO TO 0580-ACCT-NOT-FOUND.
001692
001693     IF CM-COMPANY-CD  IS EQUAL TO  AM-COMPANY-CD
001694       AND CM-CARRIER  IS EQUAL TO  AM-CARRIER
001695       AND CM-GROUPING  IS EQUAL TO  AM-GROUPING
001696       AND CM-STATE  IS EQUAL TO  AM-STATE
001697       AND CM-ACCOUNT  IS EQUAL TO  AM-ACCOUNT
001698         NEXT SENTENCE
001699     ELSE
001700         GO TO 0580-ACCT-NOT-FOUND.
001701
001702     MOVE 'X'                    TO  DATE-RANGE-SW.
001703
001704     IF CM-CERT-EFF-DT  IS LESS THAN  AM-EFFECTIVE-DT
001705         MOVE 'X'                TO  DATE-RANGE-SW
001706         GO TO 0580-ACCT-NOT-FOUND.
001707
001708     IF CM-CERT-EFF-DT  IS NOT LESS THAN  AM-EXPIRATION-DT
001709         GO TO 0570-READ-NEXT-RANGE.
001710
001711     IF WS-BROWSE-STARTED-SW = 'Y'
001712        
      * EXEC CICS ENDBR
001713*          DATASET    ('ERACCT')
001714*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007176' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037313736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001715        MOVE SPACES             TO  WS-BROWSE-STARTED-SW
001716     END-IF
001717
001718     MOVE ' '                    TO  DATE-RANGE-SW.
001719     MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
001720
001721     GO TO 0590-SET-ACCOUNT.
001722
001723 0580-ACCT-NOT-FOUND.
001724     IF DATE-RANGE-SW  IS NOT EQUAL TO  SPACES
001725         MOVE ER-2601            TO  EMI-ERROR
001726     ELSE
001727         MOVE ER-2619            TO  EMI-ERROR.
001728
001729     MOVE 'N'                    TO  WS-ACCT-RECORD-SW.
001730
001731     PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
001732
001733 0585-NO-ACCT.
001734     MOVE SPACES                 TO  WS-ACCT-USER-FLD-5.
001735     MOVE '0'                    TO  WS-AM-EARN-METHOD-L
001736                                     WS-AM-EARN-METHOD-R
001737                                     WS-AM-EARN-METHOD-A
001738                                     WS-LF-AM-REM-TERM-CALC
001739                                     WS-AH-AM-REM-TERM-CALC.
001740
001741     GO TO 0600-CHECK-FOR-LIFE.
001742
001743 0590-SET-ACCOUNT.
001744******************************************************************
001745******* THIS CODE ALLOWS THE FORM MASTER BENEFIT CODES TERM
001746******* AND REFUND METHOD TO OVERRIDE COMPANY AND STATE REFUND
001747******* METHODS.
001748******************************************************************
001749     MOVE SPACES              TO WS-LF-FO-REFUND-CALC.
001750     MOVE SPACES              TO WS-AH-FO-REFUND-CALC.
001751     IF PI-COMPANY-ID EQUAL TO 'NCL'
001752          IF ((CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES)
001753                 OR (CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES))
001754                  PERFORM 7000-GET-ERFORM THRU 7000-EXIT
001755                  IF FORM-FOUND
001756                      PERFORM 7100-BENEFIT-SEARCH THRU 7100-EXIT.
001757******************************************************************
001758
001759     MOVE AM-FLD-5               TO  WS-ACCT-USER-FLD-5.
001760     MOVE '0'                    TO  WS-AM-EARN-METHOD-L
001761                                     WS-AM-EARN-METHOD-R
001762                                     WS-AM-EARN-METHOD-A.
001763     IF AM-GPCD > 1
001764       AND AM-GPCD < 6
001765       AND PI-CANCEL-REASON NOT > SPACES
001766        MOVE 'Y'                 TO PI-CANCEL-REASON
001767     END-IF.
001768     MOVE PI-CANCEL-REASON       TO  CANREAO
001769
001770*    EVALUATE AM-EARN-METHOD-R
001771*       WHEN 'R'
001772*           MOVE '1'                TO  WS-AM-EARN-METHOD-R
001773*       WHEN 'P'
001774*           MOVE '2'                TO  WS-AM-EARN-METHOD-R
001775*       WHEN 'C'
001776*           MOVE '3'                TO  WS-AM-EARN-METHOD-R
001777*       WHEN 'A'
001778*           MOVE '6'                TO  WS-AM-EARN-METHOD-R
001779*       WHEN 'M'
001780*           MOVE '8'                TO  WS-AM-EARN-METHOD-R
001781*       WHEN 'S'
001782*           MOVE '9'                TO  WS-AM-EARN-METHOD-R
001783*    END-EVALUATE.
001784
001785*    EVALUATE AM-EARN-METHOD-L
001786*       WHEN 'R'
001787*           MOVE '1'                TO  WS-AM-EARN-METHOD-L
001788*       WHEN 'P'
001789*           MOVE '2'                TO  WS-AM-EARN-METHOD-L
001790*       WHEN 'C'
001791*           MOVE '3'                TO  WS-AM-EARN-METHOD-L
001792*       WHEN 'A'
001793*           MOVE '6'                TO  WS-AM-EARN-METHOD-L
001794*       WHEN 'M'
001795*           MOVE '8'                 TO  WS-AM-EARN-METHOD-L
001796*       WHEN 'S'
001797*           MOVE '9'                 TO  WS-AM-EARN-METHOD-L
001798*    END-EVALUATE.
001799
001800*    EVALUATE AM-EARN-METHOD-A
001801*       WHEN 'R'
001802*           MOVE '1'                TO  WS-AM-EARN-METHOD-A
001803*       WHEN 'P'
001804*           MOVE '2'                TO  WS-AM-EARN-METHOD-A
001805*       WHEN 'C'
001806*           MOVE '3'                TO  WS-AM-EARN-METHOD-A
001807*       WHEN 'A'
001808*           MOVE '6'                TO  WS-AM-EARN-METHOD-A
001809*       WHEN 'M'
001810*           MOVE '8'                TO  WS-AM-EARN-METHOD-A
001811*       WHEN 'N'
001812*           MOVE '5'                TO  WS-AM-EARN-METHOD-A
001813*       WHEN 'S'
001814*           MOVE '9'                TO  WS-AM-EARN-METHOD-A
001815*    END-EVALUATE.
001816
001817     MOVE ZEROS                  TO  WS-TOT-AH-PREM
001818                                     WS-TOT-LF-PREM
001819                                     WS-TOT-AH-RFND
001820                                     WS-TOT-LF-RFND.
001821
001822 0600-CHECK-FOR-LIFE.
001823     IF CM-LF-BENEFIT-CD  = '00'
001824         GO TO 0640-AH-BENEFIT.
001825
001826 0610-GET-LIFE-BENEFIT.
001827
001828     IF ACCT-FOUND
001829         MOVE +1                 TO SUB3
001830         PERFORM 5000-GET-LF-AM-REM-TERM THRU 5900-EXIT.
001831
001832     MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
001833     MOVE WS-LF-KIND             TO  HLEDESCO.
001834     MOVE CM-LF-ORIG-TERM        TO  HLTERMO
001835     IF WS-LF-CO-EARNINGS-CALC = 'B'
001836        COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
001837                             + CM-LF-ALT-PREMIUM-AMT
001838     ELSE
001839        MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
001840     END-IF
001841
001842     MOVE WS-LIFE-PREMIUM        TO  HLPREMO
001843
001844
001845     EVALUATE TRUE
001846        WHEN ((OPEN-LF-CLAIM)
001847           OR (CLOSED-LF-CLAIM))
001848           AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
001849           MOVE ZEROS            TO HLREFNDO  WS-CALC-REFUND
001850                                    WS-TOT-LF-RFND
001851                                    PI-LF-REFUND-AMT
001852           IF CM-AH-BENEFIT-CD NOT = '00'
001853              MOVE ZEROS         TO HAREFNDO
001854                                    WS-TOT-AH-RFND
001855                                    PI-AH-REFUND-AMT
001856              MOVE SPACES        TO PI-AH-REFUND-METH
001857           END-IF
001858           IF OTHER-CLAIM
001859              MOVE ER-2998          TO EMI-ERROR
001860           ELSE
001861              MOVE ER-2965          TO EMI-ERROR
001862           END-IF
001863           PERFORM 9700-ERROR-FORMAT
001864                                 THRU  9799-EXIT
001865           GO TO 0640-AH-BENEFIT
001866        WHEN OPEN-LF-CLAIM
001867           AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
001868           AND WS-ST-REF-IND = '4'
001869           MOVE ER-2966          TO EMI-ERROR
001870           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
001871           PERFORM 9700-ERROR-FORMAT
001872                                 THRU  9799-EXIT
001873        WHEN OPEN-LF-CLAIM
001874           AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
001875           AND WS-ST-REF-IND NOT = '4'
001876           MOVE ER-2967          TO EMI-ERROR
001877           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
001878           PERFORM 9700-ERROR-FORMAT
001879                                 THRU  9799-EXIT
001880           GO TO 0640-AH-BENEFIT
001881        WHEN CLOSED-LF-CLAIM
001882           AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
001883           AND WS-ST-REF-IND = '4'
001884           MOVE ER-2968          TO EMI-ERROR
001885           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
001886           PERFORM 9700-ERROR-FORMAT
001887                                 THRU  9799-EXIT
001888        WHEN CLOSED-LF-CLAIM
001889           AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
001890           AND WS-ST-REF-IND NOT = '4'
001891           MOVE ER-2969          TO EMI-ERROR
001892           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
001893           PERFORM 9700-ERROR-FORMAT
001894                                 THRU  9799-EXIT
001895           GO TO 0640-AH-BENEFIT
001896        WHEN OTHER
001897           CONTINUE
001898     END-EVALUATE
001899
001900     .
001901 0620-GET-REM-TERM.
001902
001903     MOVE WS-CF-LIFE-OVERRIDE-L1
001904                                 TO  CP-LIFE-OVERRIDE-CODE.
001905     MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
001906     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
001907     MOVE WS-LF-CANCEL-DATE      TO  CP-VALUATION-DT.
001908     MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
001909     MOVE CM-STATE               TO  CP-STATE.
001910     MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
001911     MOVE WS-CF-LF-COVERAGE-TYPE
001912                                 TO  CP-BENEFIT-TYPE.
001913     MOVE WS-LF-KIND             TO  HLEDESCO.
001914     MOVE WS-LF-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
001915     MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
001916     MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
001917     MOVE '2'                    TO  CP-PROCESS-TYPE.
001918     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
001919     MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
001920     MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
001921     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
001922     MOVE WS-CF-CR-REM-TERM-CALC
001923                                 TO  CP-REM-TERM-METHOD.
001924
001925     IF WS-LF-CO-REM-TERM-CALC  IS GREATER THAN  '0'
001926         MOVE WS-LF-CO-REM-TERM-CALC
001927                                 TO  CP-REM-TERM-METHOD.
001928
001929     IF WS-LF-ST-REM-TERM-CALC  IS GREATER THAN  '0'
001930         MOVE WS-LF-ST-REM-TERM-CALC
001931                                 TO  CP-REM-TERM-METHOD.
001932
001933     IF WS-LF-AM-REM-TERM-CALC  IS GREATER THAN  '0'
001934         MOVE WS-LF-AM-REM-TERM-CALC
001935                                 TO  CP-REM-TERM-METHOD.
001936
001937     IF (PI-COMPANY-ID = 'CID')
001938        AND (CP-STATE-STD-ABBRV = 'WI')
001939        MOVE '7'                 TO CP-REM-TERM-METHOD
001940        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
001941     END-IF
001942
001943     IF (PI-COMPANY-ID = 'CID')
001944        AND (CP-STATE-STD-ABBRV = 'MO')
001945        AND (CM-CERT-EFF-DT >= X'9B41')
001946        AND (CM-CERT-EFF-DT <= X'A2FB')
001947        MOVE '7'                 TO CP-REM-TERM-METHOD
001948        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
001949     END-IF
001950
001951     MOVE CM-LF-ORIG-TERM        TO  HLTERMO
001952                                     CP-ORIGINAL-TERM
001953                                     CP-LOAN-TERM.
001954
001955     IF CP-TRUNCATED-LIFE
001956         MOVE CM-LOAN-TERM       TO  CP-LOAN-TERM.
001957
001958     IF CP-TERM-IS-DAYS
001959         IF CM-LF-TERM-IN-DAYS  IS NUMERIC
001960             MOVE CM-LF-TERM-IN-DAYS
001961                                 TO  CP-TERM-OR-EXT-DAYS
001962         ELSE
001963             MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS
001964     ELSE
001965         IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
001966             MOVE CM-PMT-EXTENSION-DAYS
001967                                 TO  CP-TERM-OR-EXT-DAYS
001968         ELSE
001969             MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
001970
001971*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
001972     MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
001973     MOVE '3'                    TO  WS-CF-RECORD-TYPE.
001974     MOVE CM-STATE               TO  WS-CF-ACCESS.
001975     MOVE +0                     TO  WS-CF-SEQUENCE-NO.
001976
001977     
      * EXEC CICS READ
001978*        DATASET  (ELCNTL-ID)
001979*        SET      (ADDRESS OF CONTROL-FILE)
001980*        RIDFLD   (WS-CF-CONTROL-PRIMARY)
001981*        RESP     (WS-RESPONSE)
001982*    END-EXEC.
      *    MOVE '&"S        E          (  N#00007441' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037343431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001983
001984     IF WS-RESP-NOTOPEN
001985         MOVE ER-2617            TO  EMI-ERROR
001986         GO TO 0320-INPUT-ERROR.
001987
001988     IF WS-RESP-NOTFND
001989         MOVE ZERO               TO  CP-FREE-LOOK
001990     ELSE
001991         MOVE CF-ST-FREE-LOOK-PERIOD
001992                                 TO CP-FREE-LOOK.
001993
001994     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
001995        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
001996        ADD +1   TO CP-ORIGINAL-TERM CP-LOAN-TERM
001997     END-IF
001998
001999     PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
002000
002001     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
002002        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
002003        MOVE CP-REMAINING-TERM-1   TO WS-BALLOON-RTRM
002004        COMPUTE CP-REMAINING-TERM-1 =
002005                CP-REMAINING-TERM-1 - +1
002006        COMPUTE CP-REMAINING-TERM-2 =
002007                CP-REMAINING-TERM-2 - +1
002008     END-IF
002009
002010     IF CP-REMAINING-TERM-1 NEGATIVE
002011        MOVE ZEROS               TO CP-REMAINING-TERM-1
002012     END-IF
002013     IF CP-REMAINING-TERM-2 NEGATIVE
002014        MOVE ZEROS               TO CP-REMAINING-TERM-2
002015     END-IF
002016     MOVE CP-REMAINING-TERM-1    TO  HLREMO.
002017
002018     IF PI-LF-CANCEL-DATE = LOW-VALUES
002019         GO TO 0640-AH-BENEFIT.
002020
002021*    IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-LF-ORIG-TERM
002022*        MOVE CM-LF-ORIG-TERM    TO  HLREMO.
002023
002024     IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '8'
002025         IF CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
002026             MOVE ER-0681        TO  EMI-ERROR
002027             PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002028
002029     IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '7'
002030         IF CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES
002031             MOVE ER-0682        TO  EMI-ERROR
002032             PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002033
002034 0630-CONTINUE-LIFE.
002035
002036     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
002037        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
002038        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
002039                                    FREMTRMO
002040     ELSE
002041        MOVE CP-REMAINING-TERM-2 TO  CP-REMAINING-TERM
002042                                     FREMTRMO
002043     END-IF
002044
002045     display ' remain terms ' cp-remaining-term-1 ' '
002046        cp-remaining-term-2
002047
002048     MOVE WS-LF-CO-EARNINGS-CALC TO  CP-EARNING-METHOD.
002049     MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT.
002050     if pi-company-id = 'AHL'
002051        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
002052        if cp-class-code = spaces
002053           move zeros            to cp-class-code
002054        end-if
002055     ELSE
002056        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
002057     END-IF
002058
002059     IF (PI-COMPANY-ID = 'DCC')
002060        AND (AM-DCC-PRODUCT-CODE = 'DDF')
002061        MOVE ' '                 TO WS-PDEF-RECORD-SW
002062        PERFORM 0730-GET-DDF-FACTORS
002063                                 THRU 0730-EXIT
002064        IF PDEF-FOUND
002065           IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
002066               MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
002067           ELSE
002068               MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
002069           END-IF
002070           COMPUTE WS-ATT-AGE = WS-EDIT-AGE
002071              + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
002072           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
002073              (P1 > +11)
002074              OR ((PD-PROD-CODE (P1) = 'L' OR 'O')
002075                AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
002076           END-PERFORM
002077           IF P1 < +12
002078              MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
002079           END-IF
002080        END-IF
002081     END-IF
002082
002083     PERFORM 0710-LINK-REM-AMOUNT THRU 0710-EXIT.
002084
002085     MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT.
002086
002087     display ' rem amt ' cp-remaining-amt
002088
002089     IF WS-LF-CO-EARNINGS-CALC = 'B'
002090        COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
002091                             + CM-LF-ALT-PREMIUM-AMT
002092     ELSE
002093        MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
002094     END-IF
002095
002096     MOVE WS-LIFE-PREMIUM        TO  HLPREMO
002097                                     WS-TOT-LF-PREM.
002098
002099     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
002100        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
002101        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
002102     ELSE
002103        MOVE CP-REMAINING-TERM-1 TO  CP-REMAINING-TERM
002104     END-IF
002105
002106     MOVE WS-LF-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
002107     MOVE WS-LF-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
002108
002109     IF WS-LF-ST-REFUND-CALC  IS GREATER THAN  ZERO
002110         MOVE WS-LF-ST-REFUND-CALC
002111                                 TO  CP-EARNING-METHOD.
002112
002113     IF WS-LF-FO-REFUND-CALC  IS GREATER THAN  ZERO
002114         MOVE WS-LF-FO-REFUND-CALC
002115                                 TO  CP-EARNING-METHOD.
002116
002117     if (pi-company-id = 'CID')
002118        and (cp-state-std-abbrv = 'MN' OR 'WA')
002119        continue
002120     else
002121        IF CP-RATING-METHOD = '4'
002122           MOVE '4'              TO CP-EARNING-METHOD
002123        END-IF
002124     end-if
002125
002126*    IF WS-CF-LF-COVERAGE-TYPE = 'R'
002127*        IF WS-AM-EARN-METHOD-R  IS GREATER THAN  ZERO
002128*            MOVE WS-AM-EARN-METHOD-R
002129*                                TO  CP-EARNING-METHOD
002130*        END-IF
002131*    ELSE
002132*        IF WS-AM-EARN-METHOD-L  IS GREATER THAN  ZERO
002133*            MOVE WS-AM-EARN-METHOD-L
002134*                                TO  CP-EARNING-METHOD.
002135
002136     if pi-company-id = 'AHL'
002137        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
002138        if cp-class-code = spaces
002139           move zeros            to cp-class-code
002140        end-if
002141     ELSE
002142        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
002143     END-IF
002144     MOVE CM-LF-BENEFIT-CD       TO  CP-BENEFIT-CD.
002145     MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
002146                                     CP-RATING-BENEFIT-AMT.
002147     IF CP-STATE-STD-ABBRV = 'OR'
002148         COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
002149                                         CM-LF-ALT-BENEFIT-AMT.
002150****   N O T E   ****
002151*      CID DOES NOT WANT THE REFUND METHOD TO OVERRIDE
002152*      THE OH HARD CODING IN ELCRFNDP
002153
002154     IF PI-COMPANY-ID = 'CID'
002155        IF CP-STATE-STD-ABBRV = 'OH'
002156           MOVE WS-LF-CO-EARNINGS-CALC
002157                                 TO CP-EARNING-METHOD
002158        END-IF
002159     END-IF
002160
002161****   N O T E   ****
002162
002163****   MN   NOTE  ****  CID HAS THE STATE REFUND METHOD SET AS
002164****      R78 BUT THEY WANT THE BALLOONS REFUNDED WITH A
002165****      CUSTOM FORMULA THAT IS IN ELRFND SO I AM SETTING
002166****     THE APPROPRIATE OPTIONS HERE
002167
002168*    IF PI-COMPANY-ID = 'CID'
002169*       IF (CP-STATE-STD-ABBRV = 'MN')
002170*          AND (CM-CERT-EFF-DT > X'A4FF')
002171*          AND (WS-LF-CO-EARNINGS-CALC = 'B')
002172*          MOVE '5'              TO CP-EARNING-METHOD
002173*       END-IF
002174*    END-IF
002175
002176
002177     MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
002178     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
002179     MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
002180     MOVE CM-LF-DEV-PCT          TO  CP-RATE-DEV-PCT.
002181     MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
002182     MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON
002183
002184*    IF CP-EARN-AS-NET-PAY
002185*        IF CP-LOAN-APR NOT GREATER THAN ZEROS
002186*           IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
002187*               MOVE WS-CF-DEFAULT-APR
002188*                                TO  CP-LOAN-APR
002189*               MOVE ER-3780     TO  EMI-ERROR
002190*               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
002191*           ELSE
002192*               MOVE ER-3781     TO  EMI-ERROR
002193*               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002194
002195
002196*    DISPLAY ' 1278 REM AMT      ' CP-REMAINING-BENEFIT
002197*    DISPLAY ' 1278 REM TRM      ' CP-REMAINING-TERM
002198*    DISPLAY ' 1278 EARN METH    ' CP-EARNING-METHOD
002199*    DISPLAY ' 1278 RATE METH    ' CP-RATING-METHOD
002200*    DISPLAY ' 1278 CLASS        ' CP-CLASS-CODE
002201*    DISPLAY ' 1278 BENE CODE    ' CP-BENEFIT-CD
002202*    DISPLAY ' 1278 ORIG BENE    ' CP-ORIGINAL-BENEFIT
002203*    DISPLAY ' 1278 RATE BENE    ' CP-RATING-BENEFIT-AMT
002204*    DISPLAY ' 1278 ORIG PREM    ' CP-ORIGINAL-PREMIUM
002205*    DISPLAY ' 1278 ISS AGE      ' CP-ISSUE-AGE
002206*    DISPLAY ' 1278 DEV CODE     ' CP-DEVIATION-CODE
002207*    DISPLAY ' 1278 DEV PCT      ' CP-RATE-DEV-PCT
002208*    DISPLAY ' 1278 APR          ' CP-LOAN-APR
002209*    DISPLAY ' 1278 EXT DAYS     ' CP-TERM-OR-EXT-DAYS
002210*    DISPLAY ' 1278 LOAN TERM    ' CP-LOAN-TERM
002211*    DISPLAY ' 1278 SPEC CALC    ' CP-SPECIAL-CALC-CD
002212*    DISPLAY ' 1278 ST STD ABB   ' CP-STATE-STD-ABBRV
002213
002214*    MOVE WS-STATE-EXT-DAYS-CHG  TO CP-EXT-DAYS-CALC
002215
002216     PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
002217
002218     display ' back from link rfnd ' cp-calc-refund
002219
002220     MOVE 'L'                    TO  WS-REFUND-SEARCH-SW.
002221
002222     PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
002223
002224     IF CP-ERROR-RATE-IS-ZERO
002225       OR CP-ERROR-RATE-NOT-FOUND
002226         MOVE ER-2740            TO  EMI-ERROR
002227         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002228
002229     IF CP-ERROR-RATE-FILE-NOTOPEN
002230         MOVE ER-2617            TO  EMI-ERROR
002231         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002232
002233     MOVE ZEROS                  TO  WS-CALC-REFUND.
002234     MOVE CP-CALC-REFUND         TO  HLREFNDO  WS-CALC-REFUND
002235                                     WS-TOT-LF-RFND
002236                                     PI-LF-REFUND-AMT.
002237     MOVE CP-REMAINING-AMT       TO  HLREDO.
002238
002239     MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.
002240
002241     move cm-life-comm-pct       to hlcpcto
002242     compute hlcamto rounded =
002243        cp-calc-refund * cm-life-comm-pct
002244
002245     IF WS-LF-CO-EARNINGS-CALC  IS NOT EQUAL TO  'B'
002246         GO TO 0640-AH-BENEFIT.
002247
002248*    IF WS-LF-SPECIAL-CALC-CD = 'L'
002249*        ADD +1                  TO  CP-REMAINING-TERM
002250*                                    CP-ORIGINAL-TERM.
002251
002252     MOVE 'L'                    TO  CP-BENEFIT-TYPE.
002253     MOVE '2'                    TO  CP-EARNING-METHOD
002254                                     CP-RATING-METHOD.
002255
002256     IF PI-COMPANY-ID = 'CID'
002257        IF (CP-STATE-STD-ABBRV = 'MN')
002258           AND (CM-CERT-EFF-DT > X'A4FF')
002259           AND (WS-LF-CO-EARNINGS-CALC = 'B')
002260           MOVE '5'              TO CP-EARNING-METHOD
002261           MOVE WS-LF-CO-EARNINGS-CALC TO CP-RATING-METHOD
002262        END-IF
002263     END-IF
002264
002265     MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ORIGINAL-BENEFIT
002266                                     CP-REMAINING-BENEFIT
002267                                     CP-RATING-BENEFIT-AMT.
002268     IF CP-STATE-STD-ABBRV = 'OR'
002269         COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
002270                                         CM-LF-ALT-BENEFIT-AMT.
002271     MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ORIGINAL-PREMIUM.
002272     MOVE 'LEV'                  TO  CP-DEVIATION-CODE.
002273
002274     PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
002275
002276     IF CP-ERROR-RATE-IS-ZERO
002277       OR CP-ERROR-RATE-NOT-FOUND
002278         MOVE ER-2740            TO  EMI-ERROR
002279         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002280
002281     IF CP-ERROR-RATE-FILE-NOTOPEN
002282         MOVE ER-2617            TO  EMI-ERROR
002283         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002284
002285     ADD CP-CALC-REFUND          TO  WS-CALC-REFUND.
002286
002287     MOVE WS-CALC-REFUND         TO  HLREFNDO
002288                                     WS-TOT-LF-RFND
002289                                     PI-LF-REFUND-AMT.
002290
002291     MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.
002292
002293 0640-AH-BENEFIT.
002294     IF CM-AH-BENEFIT-CD = '00'
002295         GO TO 0680-CONTINUE.
002296
002297 0650-GET-AH-BENEFIT.
002298     IF ACCT-FOUND
002299         MOVE +1                 TO SUB3
002300         PERFORM 6000-GET-AH-AM-REM-TERM THRU 6900-EXIT.
002301
002302     MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
002303     MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
002304     MOVE CM-AH-ORIG-TERM        TO  HATERMO
002305
002306     MOVE WS-AH-KIND             TO  HAEDESCO.
002307
002308     EVALUATE TRUE
002309
002310        WHEN ((OPEN-LF-CLAIM)
002311           OR (CLOSED-LF-CLAIM))
002312           AND (WS-AH-CANCEL-DATE NOT = WS-LF-INCUR-DT)
002313           MOVE ZEROS            TO HAREFNDO  WS-CALC-REFUND
002314                                    WS-TOT-AH-RFND
002315                                    PI-AH-REFUND-AMT
002316           MOVE SPACES           TO PI-AH-REFUND-METH
002317           IF OTHER-CLAIM
002318              MOVE ER-2998          TO EMI-ERROR
002319           ELSE
002320              MOVE ER-2965          TO EMI-ERROR
002321           END-IF
002322
002323           PERFORM 9700-ERROR-FORMAT
002324                                 THRU  9799-EXIT
002325           GO TO 0680-CONTINUE
002326
002327
002328        WHEN (CM-LF-BENEFIT-CD NOT = '00')
002329           AND ((OPEN-LF-CLAIM) OR (CLOSED-LF-CLAIM))
002330           AND ((EMI-ERROR = ER-2965) OR (EMI-ERROR = ER-2998))
002331           AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
002332           MOVE ZEROS            TO HAREFNDO
002333                                    WS-TOT-AH-RFND
002334                                    PI-AH-REFUND-AMT
002335           MOVE SPACES           TO PI-AH-REFUND-METH
002336           GO TO 0680-CONTINUE
002337        WHEN (OPEN-AH-CLAIM)
002338           AND (WS-ST-REF-IND = '1')
002339           MOVE ZEROS            TO HAREFNDO
002340                                    WS-TOT-AH-RFND
002341                                    PI-AH-REFUND-AMT
002342           MOVE ER-3039          TO EMI-ERROR
002343           PERFORM 9700-ERROR-FORMAT
002344                                 THRU  9799-EXIT
002345           GO TO 0680-CONTINUE
002346        WHEN (OPEN-AH-CLAIM)
002347           AND (WS-ST-REF-IND = '2' OR '3' OR '4')
002348           MOVE ER-3037          TO EMI-ERROR
002349           PERFORM 9700-ERROR-FORMAT
002350                                 THRU  9799-EXIT
002351*          CONTINUE
002352        WHEN (OPEN-AH-CLAIM)
002353           AND (WS-ST-REF-IND = '5')
002354           MOVE ER-3038          TO EMI-ERROR
002355           PERFORM 9700-ERROR-FORMAT
002356                                 THRU  9799-EXIT
002357*          CONTINUE
002358        WHEN (OPEN-AH-CLAIM)
002359           AND (WS-ST-REF-IND = ' ')
002360           MOVE ER-2768          TO EMI-ERROR
002361           PERFORM 9700-ERROR-FORMAT
002362                                 THRU  9799-EXIT
002363        WHEN (CLOSED-AH-CLAIM)
002364           AND (WS-AH-CANCEL-DATE <= WS-AH-PAID-THRU-DT)
002365           MOVE ZEROS            TO HAREFNDO
002366                                    WS-TOT-AH-RFND
002367                                    PI-AH-REFUND-AMT
002368           MOVE ER-2756          TO EMI-ERROR
002369           PERFORM 9700-ERROR-FORMAT
002370                                 THRU  9799-EXIT
002371           IF WS-TOT-LF-RFND > 0
002372              MOVE ZEROS         TO HLREFNDO
002373                                    WS-TOT-LF-RFND
002374                                    PI-LF-REFUND-AMT
002375              MOVE SPACES        TO PI-LF-REFUND-METH
002376           END-IF
002377           GO TO 0680-CONTINUE
002378*       WHEN OTHER
002379*          CONTINUE
002380     END-EVALUATE
002381
002382     .
002383 0660-GET-REM-TERM.
002384
002385     MOVE WS-CF-AH-OVERRIDE-L1   TO  CP-AH-OVERRIDE-CODE.
002386     MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
002387     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
002388     MOVE WS-AH-CANCEL-DATE      TO  CP-VALUATION-DT.
002389     MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
002390*****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
002391     MOVE WS-AH-KIND             TO  HAEDESCO.
002392     MOVE CM-STATE               TO  CP-STATE.
002393     MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
002394     MOVE 'A'                    TO  CP-BENEFIT-TYPE.
002395     MOVE WS-AH-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
002396     MOVE '2'                    TO  CP-PROCESS-TYPE.
002397     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
002398     MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
002399     MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
002400     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
002401     MOVE WS-CF-CR-REM-TERM-CALC
002402                                 TO  CP-REM-TERM-METHOD.
002403
002404     IF WS-AH-CO-REM-TERM-CALC  IS GREATER THAN  '0'
002405         MOVE WS-AH-CO-REM-TERM-CALC
002406                                 TO  CP-REM-TERM-METHOD.
002407
002408     IF WS-AH-ST-REM-TERM-CALC  IS GREATER THAN  '0'
002409         MOVE WS-AH-ST-REM-TERM-CALC
002410                                 TO  CP-REM-TERM-METHOD.
002411
002412     IF WS-AH-AM-REM-TERM-CALC  IS GREATER THAN  '0'
002413         MOVE WS-AH-AM-REM-TERM-CALC
002414                                 TO  CP-REM-TERM-METHOD.
002415     IF (PI-COMPANY-ID = 'CID')
002416        AND (CP-STATE-STD-ABBRV = 'WI')
002417        MOVE '7'                 TO CP-REM-TERM-METHOD
002418        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
002419     END-IF
002420
002421     IF (PI-COMPANY-ID = 'CID')
002422        AND (CP-STATE-STD-ABBRV = 'MO')
002423        AND (CM-CERT-EFF-DT >= X'9B41')
002424        AND (CM-CERT-EFF-DT <= X'A2FB')
002425        MOVE '7'                 TO CP-REM-TERM-METHOD
002426        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
002427     END-IF
002428
002429     MOVE CM-AH-ORIG-TERM        TO  HATERMO
002430                                     CP-ORIGINAL-TERM
002431                                     CP-LOAN-TERM.
002432
002433     IF  NOT  CP-TERM-IS-DAYS
002434         IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
002435             MOVE CM-PMT-EXTENSION-DAYS
002436                                 TO  CP-TERM-OR-EXT-DAYS
002437         ELSE
002438             MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
002439
002440*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
002441     MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
002442     MOVE '3'                    TO  WS-CF-RECORD-TYPE.
002443     MOVE CM-STATE               TO  WS-CF-ACCESS.
002444     MOVE +0                     TO  WS-CF-SEQUENCE-NO.
002445
002446     
      * EXEC CICS READ
002447*        DATASET  (ELCNTL-ID)
002448*        SET      (ADDRESS OF CONTROL-FILE)
002449*        RIDFLD   (WS-CF-CONTROL-PRIMARY)
002450*        RESP     (WS-RESPONSE)
002451*    END-EXEC.
      *    MOVE '&"S        E          (  N#00007910' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037393130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002452
002453     IF WS-RESP-NOTOPEN
002454         MOVE ER-2617            TO  EMI-ERROR
002455         GO TO 0320-INPUT-ERROR.
002456
002457     IF WS-RESP-NOTFND
002458         MOVE ZERO               TO CP-FREE-LOOK
002459     ELSE
002460         MOVE CF-ST-FREE-LOOK-PERIOD
002461                                 TO CP-FREE-LOOK.
002462
002463     PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
002464
002465     MOVE CP-REMAINING-TERM-1    TO  HAREMO.
002466
002467     IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '8'
002468         IF CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
002469             MOVE ER-0683        TO  EMI-ERROR
002470             PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002471
002472     IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '6'  OR  '7'
002473         IF CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES
002474             MOVE ER-0684        TO  EMI-ERROR
002475             PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002476
002477 0670-CONTINUE-AH.
002478     MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
002479                                     WS-TOT-AH-PREM
002480     MOVE CP-REMAINING-TERM-1    TO  HAREMO.
002481
002482     IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-AH-ORIG-TERM
002483         MOVE CM-AH-ORIG-TERM    TO  HAREMO.
002484
002485
002486     IF PI-AH-CANCEL-DATE = LOW-VALUES
002487        GO TO 0680-CONTINUE.
002488
002489
002490     MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
002491     MOVE WS-AH-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
002492     MOVE WS-AH-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
002493
002494     IF WS-AH-ST-REFUND-CALC  IS GREATER THAN  ZERO
002495         MOVE WS-AH-ST-REFUND-CALC
002496                                 TO  CP-EARNING-METHOD.
002497
002498     IF WS-AH-FO-REFUND-CALC  IS GREATER THAN  ZERO
002499         MOVE WS-AH-FO-REFUND-CALC
002500                                 TO  CP-EARNING-METHOD.
002501
002502*    IF WS-AM-EARN-METHOD-A  IS GREATER THAN  ZERO
002503*        MOVE WS-AM-EARN-METHOD-A
002504*                                TO  CP-EARNING-METHOD.
002505
002506     if pi-company-id = 'AHL'
002507        MOVE CM-AH-CLASS-CD      TO CP-CLASS-CODE
002508        if cp-class-code = spaces
002509           move zeros            to cp-class-code
002510        end-if
002511     ELSE
002512        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
002513     END-IF
002514     MOVE CM-AH-BENEFIT-CD       TO  CP-BENEFIT-CD.
002515     MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
002516                                     CP-RATING-BENEFIT-AMT.
002517     IF CP-STATE-STD-ABBRV = 'OR'
002518         COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
002519                                         CM-AH-ORIG-TERM.
002520     MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
002521     MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
002522     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
002523     MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
002524     MOVE CM-AH-DEV-PCT          TO  CP-RATE-DEV-PCT.
002525     MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
002526
002527     MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
002528     MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
002529     MOVE ' '                    TO  DC-OPTION-CODE.
002530
002531     PERFORM 9600-DATE-LINK.
002532
002533     IF CP-STATE-STD-ABBRV = 'OH'
002534        IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
002535         IF (CP-ORIGINAL-TERM  IS GREATER THAN  60)
002536           AND (DC-GREG-DATE-1-YMD  IS GREATER THAN  '831101')
002537           AND (CM-LF-BENEFIT-CD  IS NOT EQUAL TO  ZERO)
002538             MOVE '6'            TO  CP-EARNING-METHOD
002539         END-IF
002540        END-IF
002541        IF PI-COMPANY-ID = 'CID'
002542           IF CM-LF-BENEFIT-CD = (SPACES OR ZEROS OR
002543                           LOW-VALUES)
002544              IF CP-CRITICAL-PERIOD
002545                 MOVE '2'       TO CP-EARNING-METHOD
002546              ELSE
002547                 MOVE '6'       TO CP-EARNING-METHOD
002548              END-IF
002549           ELSE
002550              IF WS-CF-LF-COVERAGE-TYPE = 'L'
002551                 MOVE '2'       TO CP-EARNING-METHOD
002552              ELSE
002553                 IF ((CM-LF-ORIG-TERM > 60) AND
002554                    (CM-RATE-CLASS NOT = 'L '))
002555                               OR
002556                    (WS-LF-CO-EARNINGS-CALC = '5')
002557                    IF CP-CRITICAL-PERIOD
002558                       MOVE '2'  TO CP-EARNING-METHOD
002559                    ELSE
002560                       MOVE '6'  TO CP-EARNING-METHOD
002561                    END-IF
002562                 ELSE
002563                    MOVE '1'     TO CP-EARNING-METHOD
002564                 END-IF
002565              END-IF
002566           END-IF
002567        END-IF
002568     END-IF
002569
002570     IF CP-STATE-STD-ABBRV = 'VA'
002571       IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
002572         IF DC-GREG-DATE-1-YMD  IS GREATER THAN  '921231'
002573            IF CP-ORIGINAL-TERM  IS GREATER THAN  61
002574                MOVE '6'            TO  CP-EARNING-METHOD
002575            ELSE
002576                MOVE '1'            TO  CP-EARNING-METHOD.
002577
002578     IF CP-EARN-AS-NET-PAY
002579         IF CP-LOAN-APR NOT GREATER THAN ZEROS
002580            IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
002581                MOVE WS-CF-DEFAULT-APR
002582                                 TO  CP-LOAN-APR
002583                MOVE ER-3780     TO  EMI-ERROR
002584                PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
002585            ELSE
002586                MOVE ER-3781     TO  EMI-ERROR
002587                PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002588     IF PI-COMPANY-ID = 'DCC'
002589        IF (WS-AH-BEN-CATEGORY = 'G' OR 'L')
002590           AND (CP-EARNING-METHOD NOT = 'G' AND 'D')
002591           MOVE 'S'              TO CP-EARNING-METHOD
002592        END-IF
002593     END-IF
002594
002595     MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON
002596
002597     IF (PI-COMPANY-ID = 'DCC')
002598        AND (CP-EARNING-METHOD = 'D')
002599        MOVE +0                  TO WS-DDF-ADMIN-FEES
002600                                    WS-DDF-CSO-ADMIN-FEE
002601                                    WS-DDF-1ST-YR-TOT-EXP
002602                                    WS-DDF-COMM-AND-MFEE
002603        PERFORM VARYING S1 FROM +2 BY +1 UNTIL
002604           S1 > +10
002605           IF AM-COM-TYP (S1) = 'L' OR 'N' OR 'J' OR 'I'
002606              IF (AM-A-COM (S1) NUMERIC)
002607                 AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M')
002608                 COMPUTE WS-COMM-PCT = (AM-A-COM (S1) * +1000)
002609              ELSE
002610                 MOVE +0         TO WS-COMM-PCT C0
002611                 PERFORM 0740-GET-ERCTBL THRU 0740-EXIT
002612                 COMPUTE WS-COMM-PCT = WS-COMM-PCT * +1000
002613              END-IF
002614              IF AM-COM-TYP (S1) = 'L' OR 'N'
002615                 COMPUTE WS-DDF-ADMIN-FEES = WS-DDF-ADMIN-FEES
002616                    + WS-COMM-PCT
002617              END-IF
002618              IF AM-COM-TYP (S1) = 'N'
002619                 COMPUTE WS-DDF-CSO-ADMIN-FEE =
002620                    WS-DDF-CSO-ADMIN-FEE + WS-COMM-PCT
002621              END-IF
002622              IF AM-COM-TYP (S1) = 'J' OR 'L'
002623                 COMPUTE WS-DDF-1ST-YR-TOT-EXP
002624                    = WS-DDF-1ST-YR-TOT-EXP + WS-COMM-PCT
002625              END-IF
002626              IF AM-COM-TYP (S1) = 'I'
002627                 COMPUTE WS-DDF-COMM-AND-MFEE
002628                    = WS-DDF-COMM-AND-MFEE + WS-COMM-PCT
002629              END-IF
002630           END-IF
002631        END-PERFORM
002632        MOVE WS-DDF-CSO-ADMIN-FEE TO CP-DDF-CSO-ADMIN-FEE
002633        MOVE WS-DDF-ADMIN-FEES   TO CP-DDF-ADMIN-FEES
002634        COMPUTE WS-DDF-COMM-AND-MFEE = WS-DDF-COMM-AND-MFEE +
002635           (CM-AH-PREMIUM-AMT - CM-AH-CLP - CM-ADDL-CLP)
002636     END-IF
002637
002638     IF (PI-COMPANY-ID = 'DCC')
002639        AND (CP-EARNING-METHOD = 'D')
002640        AND (PI-CANCEL-REASON NOT = 'R')
002641        PERFORM 0730-GET-DDF-FACTORS
002642                                 THRU 0730-EXIT
002643        IF NOT PDEF-FOUND
002644           MOVE ER-9999          TO EMI-ERROR
002645           PERFORM 9700-ERROR-FORMAT
002646                                 THRU 9799-EXIT
002647           GO TO 8200-SEND-DATAONLY
002648        END-IF
002649
002650        MOVE PD-UEP-FACTOR (P1 P2 + 1)
002651                                 TO CP-DDF-LO-FACT
002652        MOVE PD-UEP-FACTOR (P1 P2)
002653                                 TO CP-DDF-HI-FACT
002654        MOVE WS-DDF-COMM-AND-MFEE TO CP-DDF-COMM-AND-MFEE
002655        MOVE CM-AH-CLP           TO CP-DDF-CLP
002656        MOVE PD-1ST-YR-ADMIN-ALLOW TO CP-DDF-YR1AF
002657        COMPUTE CP-1ST-YR-ALLOW = WS-DDF-1ST-YR-TOT-EXP
002658           + PD-1ST-YR-ADMIN-ALLOW
002659
002660        MOVE 'G'                 TO CP-DDF-SPEC-CALC
002661        IF PI-CLP-YN = 'Y'
002662           MOVE 'C'              TO CP-DDF-SPEC-CALC
002663           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
002664                                    CP-DDF-CLP
002665           MOVE ZEROS            TO CP-1ST-YR-ALLOW
002666        END-IF
002667
002668        IF DD-IU-PRESENT
002669           MOVE 'I'              TO CP-EARNING-METHOD
002670        END-IF
002671
002672        MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
002673                                    CP-CLP-RATE-UP
002674
002675        IF (CP-CALC-GROSS-FEE)
002676           AND (CP-IU-RATE-UP NOT = ZEROS)
002677           COMPUTE TEX-FACT-8 = 1 - ((CM-ADDL-CLP + CM-AH-CLP)
002678              / CP-ORIGINAL-PREMIUM)
002679           COMPUTE CP-IU-RATE-UP ROUNDED = CP-IU-RATE-UP
002680              / (1 - TEX-FACT-8)
002681        END-IF
002682
002683     END-IF
002684
002685     if pi-company-id = 'VPP'
002686        IF PI-CLP-YN = 'Y'
002687           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
002688                                    hapremo
002689                                    CP-DDF-CLP
002690        END-IF
002691     end-if
002692
002693     PERFORM 0800-LINK-REFUND  THRU  0899-EXIT
002694
002695     MOVE ' '                    TO  WS-REFUND-SEARCH-SW.
002696
002697     PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
002698
002699     IF CP-ERROR-RATE-IS-ZERO
002700       OR CP-ERROR-RATE-NOT-FOUND
002701         MOVE ER-2740            TO  EMI-ERROR
002702         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002703
002704     IF CP-ERROR-RATE-FILE-NOTOPEN
002705         MOVE ER-2617            TO  EMI-ERROR
002706         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
002707
002708     MOVE ZEROS                  TO  WS-CALC-REFUND.
002709     MOVE CP-CALC-REFUND         TO  HAREFNDO  WS-CALC-REFUND
002710                                     WS-TOT-AH-RFND
002711                                     PI-AH-REFUND-AMT.
002712
002713     move cm-ah-comm-pct         to hacpcto
002714     compute hacamto rounded =
002715        cp-calc-refund * cm-ah-comm-pct
002716
002717     if pi-company-id = 'VPP'
002718        if cp-calc-refund < cm-cancel-fee
002719           move cp-calc-refund   to cm-cancel-fee
002720        end-if
002721        if cp-calc-refund = cm-ah-premium-amt
002722           move zeros            to cm-cancel-fee
002723        end-if
002724        move cm-cancel-fee       to canfeeo
002725        compute refdueo =
002726           cp-calc-refund - cm-cancel-fee
002727     end-if
002728
002729
002730     MOVE CP-REFUND-TYPE-USED    TO  PI-AH-REFUND-METH.
002731
002732     MOVE AL-UANON               TO  HACANCA.
002733
002734 0680-CONTINUE.
002735     IF BROWSE-STARTED
002736         PERFORM 1500-END-BROWSE  THRU  1599-EXIT
002737         MOVE SPACES             TO  WS-BROWSE-STARTED-SW.
002738
002739     IF CM-LF-BENEFIT-CD = '00'
002740         MOVE -1                 TO  HACANCL
002741     ELSE
002742         MOVE -1                 TO  HLCANCL.
002743
002744     ADD WS-TOT-LF-PREM    WS-TOT-AH-PREM GIVING
002745                           WS-TOT-PREM.
002746     ADD WS-TOT-LF-RFND    WS-TOT-AH-RFND GIVING
002747                           WS-TOT-RFND.
002748
002749     MOVE WS-TOT-PREM            TO  TOPREMO.
002750     MOVE WS-TOT-RFND            TO  TORFNDO.
002751
002752 0680-EXIT.
002753     EXIT.
002754 EJECT
002755 0700-LINK-REM-TERM SECTION.
002756     
      * EXEC CICS LINK
002757*        PROGRAM   (ELRTRM-ID)
002758*        COMMAREA  (CALCULATION-PASS-AREA)
002759*        LENGTH    (CP-COMM-LENGTH)
002760*    END-EXEC.
      *    MOVE '."C                   (   #00008220' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038323230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002761
002762 0700-EXIT.
002763     EXIT.
002764 EJECT
002765 0710-LINK-REM-AMOUNT SECTION.
002766     
      * EXEC CICS LINK
002767*        PROGRAM   (ELRAMT-ID)
002768*        COMMAREA  (CALCULATION-PASS-AREA)
002769*        LENGTH    (CP-COMM-LENGTH)
002770*    END-EXEC.
      *    MOVE '."C                   (   #00008230' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038323330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRAMT-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002771
002772 0710-EXIT.
002773     EXIT.
002774
002775 0730-GET-DDF-FACTORS.
002776
002777     MOVE ' '                    TO WS-PDEF-RECORD-SW
002778
002779     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
002780     MOVE CM-STATE               TO ERPDEF-STATE
002781     if cm-clp-state <> cm-state and spaces
002782                  and low-values and zeros
002783        move cm-clp-state        to erpdef-state
002784     end-if
002785     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
002786     MOVE 'A'                    TO ERPDEF-BEN-TYPE
002787     MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
002788     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
002789     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
002790
002791     
      * EXEC CICS STARTBR
002792*        DATASET  ('ERPDEF')
002793*        RIDFLD   (ERPDEF-KEY)
002794*        GTEQ
002795*        RESP     (WS-RESPONSE)
002796*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00008255' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303038323535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002797
002798     IF NOT WS-RESP-NORMAL
002799        GO TO 0730-EXIT
002800     END-IF
002801
002802     .
002803 0730-READNEXT.
002804
002805     
      * EXEC CICS READNEXT
002806*       DATASET  ('ERPDEF')
002807*       SET      (ADDRESS OF PRODUCT-MASTER)
002808*       RIDFLD   (ERPDEF-KEY)
002809*       RESP     (WS-RESPONSE)
002810*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00008269' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303038323639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002811
002812     IF NOT WS-RESP-NORMAL
002813        GO TO 0730-ENDBR
002814     END-IF
002815
002816     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
002817        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
002818           MOVE 'Y'              TO WS-PDEF-RECORD-SW
002819        ELSE
002820           GO TO 0730-READNEXT
002821        END-IF
002822     ELSE
002823        GO TO 0730-ENDBR
002824     END-IF
002825
002826     PERFORM VARYING P1 FROM +1 BY +1 UNTIL
002827        (P1 > +8)
002828        OR (PD-PROD-CODE (P1) = 'I')
002829     END-PERFORM
002830     IF P1 < +9
002831        SET DD-IU-PRESENT        TO TRUE
002832     END-IF
002833
002834     IF CM-LOAN-TERM = ZEROS
002835        MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
002836     END-IF
002837
002838     IF PD-TRUNCATED = 'Y'
002839        MOVE CM-LOAN-TERM        TO WS-TERM
002840     ELSE
002841        MOVE CP-ORIGINAL-TERM    TO WS-TERM
002842     END-IF
002843
002844     EVALUATE TRUE
002845        WHEN WS-TERM > +168
002846           MOVE 15               TO P1
002847        WHEN WS-TERM > +156
002848           MOVE 14               TO P1
002849        WHEN WS-TERM > +144
002850           MOVE 13               TO P1
002851        WHEN WS-TERM > +132
002852           MOVE 12               TO P1
002853        WHEN WS-TERM > +120
002854           MOVE 11               TO P1
002855        WHEN WS-TERM > +108
002856           MOVE 10               TO P1
002857        WHEN WS-TERM > +96
002858           MOVE 9                TO P1
002859        WHEN WS-TERM > +84
002860           MOVE 8                TO P1
002861        WHEN WS-TERM > +72
002862           MOVE 7                TO P1
002863        WHEN WS-TERM > +60
002864           MOVE 6                TO P1
002865        WHEN WS-TERM > +48
002866           MOVE 5                TO P1
002867        WHEN WS-TERM > +36
002868           MOVE 4                TO P1
002869        WHEN WS-TERM > +24
002870           MOVE 3                TO P1
002871        WHEN WS-TERM > +12
002872           MOVE 2                TO P1
002873        WHEN OTHER
002874           MOVE 1                TO P1
002875     END-EVALUATE
002876
002877     EVALUATE TRUE
002878*       WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13)
002879*          AND (DD-IU-PRESENT)
002880*          MOVE 2                TO P2
002881*       WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13
002882*          MOVE 1                TO P2
002883        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +25
002884           MOVE 2                TO P2
002885        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +37
002886           MOVE 3                TO P2
002887        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +49
002888           MOVE 4                TO P2
002889        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +61
002890           MOVE 5                TO P2
002891        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +73
002892           MOVE 6                TO P2
002893        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +85
002894           MOVE 7                TO P2
002895        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +97
002896           MOVE 8                TO P2
002897        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +109
002898           MOVE 9                TO P2
002899        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +121
002900           MOVE 10               TO P2
002901        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +133
002902           MOVE 11               TO P2
002903        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +145
002904           MOVE 12               TO P2
002905        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +157
002906           MOVE 13               TO P2
002907        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +169
002908           MOVE 14               TO P2
002909        WHEN OTHER
002910           MOVE 15               TO P2
002911     END-EVALUATE
002912
002913     .
002914 0730-ENDBR.
002915
002916     
      * EXEC CICS ENDBR
002917*       DATASET  ('ERPDEF')
002918*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008380' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038333830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002919
002920     .
002921 0730-EXIT.
002922     EXIT.
002923
002924 0740-GET-ERCTBL.
002925
002926     MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
002927     MOVE AM-A-COMA (S1)         TO CTBL-TABLE
002928     MOVE 'A'                    TO CTBL-BEN-TYPE
002929     MOVE CM-AH-BENEFIT-CD       TO CTBL-BEN-CODE
002930     MOVE CTBL-KEY               TO CTBL-KEY-SAVE
002931
002932     PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
002933     IF WS-RESP-NORMAL
002934        PERFORM 0760-FIND-COMM   THRU 0760-EXIT
002935     ELSE
002936        MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
002937        MOVE AM-A-COMA (S1)         TO CTBL-TABLE
002938        MOVE 'A'                    TO CTBL-BEN-TYPE
002939        MOVE 'AA'                   TO CTBL-BEN-CODE
002940        MOVE CTBL-KEY               TO CTBL-KEY-SAVE
002941
002942        PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
002943        IF WS-RESP-NORMAL
002944           PERFORM 0760-FIND-COMM   THRU 0760-EXIT
002945        END-IF
002946     END-IF
002947
002948     .
002949 0740-EXIT.
002950     EXIT.
002951
002952 0750-READ-ERCTBL.
002953
002954     
      * EXEC CICS READ
002955*         INTO    (COMM-TABLE-RECORD)
002956*         DATASET ('ERCTBL')
002957*         RIDFLD  (CTBL-KEY)
002958*         RESP    (WS-RESPONSE)
002959*    END-EXEC
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
           MOVE 'ERCTBL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008418' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038343138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMM-TABLE-RECORD, 
                 DFHEIV11, 
                 CTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002960
002961     .
002962 0750-EXIT.
002963     EXIT.
002964
002965 0760-FIND-COMM.
002966
002967     PERFORM VARYING C1 FROM +1 BY +1 UNTIL
002968        ((CM-AH-BENEFIT-AMT * CM-AH-ORIG-TERM) <= CT-TBF (C1))
002969        OR (C1 > +3)
002970     END-PERFORM
002971
002972     PERFORM VARYING C2 FROM +1 BY +1 UNTIL
002973        (CM-INSURED-ISSUE-AGE <= CT-AGE (C2))
002974        OR (C2 > +3)
002975     END-PERFORM
002976
002977     PERFORM VARYING C3 FROM +1 BY +1 UNTIL
002978        (CM-AH-ORIG-TERM <= CT-TRM (C3))
002979        OR (C3 > +3)
002980     END-PERFORM
002981
002982     IF C1 > +3
002983        MOVE +1                  TO C1
002984     END-IF
002985     IF C2 > +3
002986        MOVE +1                  TO C2
002987     END-IF
002988     IF C3 > +3
002989        MOVE +1                  TO C3
002990     END-IF
002991
002992     IF C1 = +3
002993        MOVE +18                 TO C0
002994     ELSE
002995        IF C1 = +2
002996           MOVE +9               TO C0
002997        END-IF
002998     END-IF
002999
003000     IF C2 = +3
003001        ADD +6                   TO C0
003002     ELSE
003003        IF C2 = +2
003004           ADD +3                TO C0
003005        END-IF
003006     END-IF
003007
003008     ADD C3                      TO C0
003009
003010     MOVE CT-RT (C0)             TO WS-COMM-PCT
003011
003012     .
003013 0760-EXIT.
003014     EXIT.
003015
003016 0800-LINK-REFUND SECTION.
003017     IF CP-AH
003018       IF PI-EARNING-METHOD-AH NOT = SPACES
003019         MOVE PI-EARNING-METHOD-AH   TO CP-EARNING-METHOD
003020        ELSE
003021         NEXT SENTENCE
003022      ELSE
003023       IF PI-EARNING-METHOD-LF NOT = SPACES
003024         MOVE PI-EARNING-METHOD-LF   TO CP-EARNING-METHOD.
003025
003026     
      * EXEC CICS LINK
003027*        PROGRAM   (ELRFND-ID)
003028*        COMMAREA  (CALCULATION-PASS-AREA)
003029*        LENGTH    (CP-COMM-LENGTH)
003030*    END-EXEC.
      *    MOVE '."C                   (   #00008490' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038343930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRFND-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003031
003032 0899-EXIT.
003033     EXIT.
003034 EJECT
003035 0900-READ-CERT-FILE  SECTION.
003036     
      * EXEC CICS HANDLE CONDITION
003037*        NOTFND  (0910-NOT-FOUND)
003038*    END-EXEC.
      *    MOVE '"$I                   ! # #00008500' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303038353030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003039
003040     
      * EXEC CICS READ
003041*        DATASET  (ELCERT-ID)
003042*        RIDFLD   (WS-CM-CONTROL-PRIMARY)
003043*        SET      (ADDRESS OF CERTIFICATE-MASTER)
003044*    END-EXEC.
      *    MOVE '&"S        E          (   #00008504' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038353034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003045
003046*    SERVICE RELOAD CERTIFICATE-MASTER.
003047
003048     GO TO 0999-EXIT.
003049
003050 0910-NOT-FOUND.
003051     MOVE 'N'                    TO  WS-CERT-RECORD-SW.
003052
003053 0999-EXIT.
003054     EXIT.
003055 EJECT
003056 1000-READ-CONTROL  SECTION.
003057     
      * EXEC CICS HANDLE CONDITION
003058*        NOTFND   (1010-NOTFND)
003059*        NOTOPEN  (1020-NOTOPEN)
003060*    END-EXEC.
      *    MOVE '"$IJ                  ! $ #00008521' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303038353231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003061
003062     
      * EXEC CICS READ
003063*        DATASET  (ELCNTL-ID)
003064*        SET      (ADDRESS OF CONTROL-FILE)
003065*        RIDFLD   (WS-CF-CONTROL-PRIMARY)
003066*        GTEQ
003067*    END-EXEC.
      *    MOVE '&"S        G          (   #00008526' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303038353236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003068
003069*    SERVICE RELOAD CONTROL-FILE.
003070
003071     GO TO 1099-EXIT.
003072
003073 1010-NOTFND.
003074     MOVE 'N'                    TO  WS-CNTL-RECORD-SW.
003075
003076     GO TO 1099-EXIT.
003077
003078 1020-NOTOPEN.
003079     MOVE ER-2617                TO  EMI-ERROR.
003080
003081     GO TO 0320-INPUT-ERROR.
003082
003083 1099-EXIT.
003084     EXIT.
003085 EJECT
003086 1100-FIND-BENEFIT-IN-STATE  SECTION.
003087     MOVE 'N'                    TO  BEN-SEARCH-SW.
003088
003089     PERFORM 1110-BENEFIT-DUMMY  THRU  1119-EXIT
003090         VARYING  SUB3  FROM  1  BY  1
003091             UNTIL  ((SUB3  IS GREATER THAN  50)
003092               OR ((CF-ST-BENEFIT-CD (SUB3)
003093                      IS EQUAL TO  WS-BEN-CD)
003094               AND (WS-LOOKUP-TYPE
003095                      IS EQUAL TO  CF-ST-BENEFIT-KIND (SUB3)))).
003096
003097     IF SUB3  IS NOT EQUAL TO  51
003098         MOVE 'Y'                TO  BEN-SEARCH-SW.
003099
003100     GO TO 1199-EXIT.
003101
003102 1110-BENEFIT-DUMMY.
003103
003104 1119-EXIT.
003105     EXIT.
003106
003107 1199-EXIT.
003108     EXIT.
003109 EJECT
003110 1200-START-ACCOUNT-MASTER  SECTION.
003111     
      * EXEC CICS HANDLE CONDITION
003112*        NOTFND   (1290-ACCT-NOT-FOUND)
003113*        ENDFILE  (1290-ACCT-NOT-FOUND)
003114*    END-EXEC.
      *    MOVE '"$I''                  ! % #00008575' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303038353735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003115
003116     MOVE ' '                    TO  WS-ACCT-RECORD-SW.
003117
003118     
      * EXEC CICS STARTBR
003119*        DATASET  (ERACCT-ID)
003120*        RIDFLD   (WS-AM-CONTROL-PRIMARY)
003121*        GTEQ
003122*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008582' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038353832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 WS-AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003123
003124     MOVE 'Y'                    TO  WS-BROWSE-STARTED-SW.
003125     MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
003126
003127     GO TO 1299-EXIT.
003128
003129 1290-ACCT-NOT-FOUND.
003130     MOVE ' '                    TO  WS-ACCT-RECORD-SW.
003131
003132 1299-EXIT.
003133     EXIT.
003134 EJECT
003135 1300-READ-ACCOUNT-MASTER  SECTION.
003136     
      * EXEC CICS HANDLE CONDITION
003137*        NOTFND   (0580-ACCT-NOT-FOUND)
003138*        ENDFILE  (0580-ACCT-NOT-FOUND)
003139*    END-EXEC.
      *    MOVE '"$I''                  ! & #00008600' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303038363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003140
003141     
      * EXEC CICS READNEXT
003142*        DATASET  (ERACCT-ID)
003143*        SET      (ADDRESS OF ACCOUNT-MASTER)
003144*        RIDFLD   (WS-AM-CONTROL-PRIMARY)
003145*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008605' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038363035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003146
003147*    SERVICE RELOAD ACCOUNT-MASTER.
003148
003149 1399-EXIT.
003150     EXIT.
003151 EJECT
003152 1400-GET-REFUND-TYPE  SECTION.
003153     IF LIFE-REFUND-SEARCH
003154         NEXT SENTENCE
003155     ELSE
003156         GO TO 1410-AH-REFUND-TYPE.
003157
003158     IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
003159         MOVE SPACES             TO  HLCALCO
003160                                     HLCAL1O
003161         GO TO 1499-EXIT.
003162
003163     MOVE CP-REFUND-TYPE-USED    TO  HLCAL1O.
003164
003165     EVALUATE TRUE
003166        WHEN CP-R-AS-REPOSSESSION
003167           MOVE 'REPOSSESSION'     TO  HLCALCO
003168        WHEN CP-R-AS-R78
003169           MOVE 'RULE 78'          TO  HLCALCO
003170        WHEN  CP-R-AS-PRORATA
003171           MOVE 'PRO RATA'         TO  HLCALCO
003172        WHEN CP-R-AS-CALIF
003173           MOVE 'CALIF'            TO  HLCALCO
003174        WHEN CP-R-AS-TEXAS
003175           MOVE 'IRREG'            TO  HLCALCO
003176        WHEN CP-REFUND-TYPE-USED IS EQUAL TO 'S'
003177           MOVE 'UTAH'             TO  HLCALCO
003178        WHEN CP-R-AS-FARM-PLAN
003179           MOVE 'FARM PLAN'        TO  HLCALCO
003180        WHEN CP-R-AS-NET-PAY
003181           MOVE 'NET PAY'          TO  HLCALCO
003182        WHEN CP-R-AS-ANTICIPATION
003183           MOVE 'ANTICIPATION'     TO  HLCALCO
003184        WHEN CP-R-AS-MEAN
003185           MOVE 'MEAN'             TO  HLCALCO
003186        WHEN CP-R-AS-SUM-OF-DIGITS
003187           MOVE 'SUM OF DIGIT'     TO  HLCALCO
003188        WHEN OTHER
003189           MOVE 'UNDEFINED'        TO  HLCALCO
003190     END-EVALUATE.
003191
003192     GO TO 1499-EXIT.
003193
003194 1410-AH-REFUND-TYPE.
003195     IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
003196         MOVE SPACES             TO  HACALCO
003197                                     HACAL1O
003198         GO TO 1499-EXIT.
003199
003200     MOVE CP-REFUND-TYPE-USED    TO  HACAL1O.
003201
003202     EVALUATE TRUE
003203        WHEN CP-R-AS-REPOSSESSION
003204           MOVE 'REPOSSESSION'     TO HACALCO
003205        WHEN CP-R-AS-R78
003206           MOVE 'RULE 78'          TO HACALCO
003207        WHEN CP-R-AS-PRORATA
003208           MOVE 'PRO RATA'         TO HACALCO
003209        WHEN CP-REFUND-TYPE-USED = '3'
003210           MOVE 'CALIF'            TO HACALCO
003211        WHEN CP-R-AS-TEXAS
003212           MOVE 'IRREG'            TO HACALCO
003213        WHEN CP-R-AS-FARM-PLAN
003214           MOVE 'FARM PLAN'        TO HACALCO
003215        WHEN CP-R-AS-NET-PAY
003216           MOVE 'NET PAY'          TO HACALCO
003217        WHEN CP-R-AS-ANTICIPATION
003218           MOVE 'ANTICIPATION'     TO HACALCO
003219        WHEN CP-R-AS-MEAN
003220           MOVE 'MEAN'             TO HACALCO
003221        WHEN CP-R-AS-SUM-OF-DIGITS
003222           MOVE 'SUM OF DIGIT'     TO HACALCO
003223        WHEN CP-GAP-ACTUARIAL
003224           MOVE 'SP ACTUARIAL'     TO HACALCO
003225        WHEN CP-R-AS-SPP-DDF
003226           MOVE 'SPEC DDF'         TO HACALCO
003227        WHEN CP-R-AS-SPP-DDF-IU
003228           MOVE 'SPEC DDF IU'      TO HACALCO
003229        WHEN OTHER
003230           MOVE 'UNKNOWN'          TO HLCALCO
003231     END-EVALUATE
003232
003233     .
003234 1499-EXIT.
003235     EXIT.
003236 EJECT
003237 1500-END-BROWSE  SECTION.
003238     IF BROWSE-STARTED
003239         
      * EXEC CICS ENDBR
003240*            DATASET  (ERACCT-ID)
003241*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008703' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038373033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003242
003243 1599-EXIT.
003244     EXIT.
003245 EJECT
003246 1600-LOCATE-BENEFIT  SECTION.
003247     
      * EXEC CICS HANDLE CONDITION
003248*        NOTFND  (1699-EXIT)
003249*    END-EXEC.
      *    MOVE '"$I                   ! '' #00008711' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303038373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003250
003251     MOVE SPACES                 TO  WS-KIND.
003252     MOVE ZERO                   TO  WS-NOT-FOUND.
003253     MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
003254     MOVE SPACES                 TO  WS-CF-STATE.
003255     MOVE WS-BENEFIT-NO          TO  WS-CF-BENEFIT-NO.
003256
003257     
      * EXEC CICS READ
003258*        DATASET  (ELCNTL-ID)
003259*        RIDFLD   (WS-CF-CONTROL-PRIMARY)
003260*        SET      (ADDRESS OF CONTROL-FILE)
003261*        GTEQ
003262*    END-EXEC.
      *    MOVE '&"S        G          (   #00008721' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303038373231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003263
003264*    SERVICE RELOAD CONTROL-FILE.
003265
003266     IF WS-CF-COMPANY-ID  IS NOT EQUAL TO  CF-COMPANY-ID
003267       OR WS-CF-RECORD-TYPE  IS NOT EQUAL TO  CF-RECORD-TYPE
003268         GO TO 1699-EXIT.
003269
003270     MOVE +1                     TO  WS-INDEX.
003271
003272 1610-LOOKUP-BENEFIT.
003273     IF WS-BENEFIT-NO  IS EQUAL TO  CF-BENEFIT-CODE (WS-INDEX)
003274         MOVE CF-BENEFIT-ALPHA (WS-INDEX)
003275                                 TO  WS-KIND
003276         MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
003277                                 TO  WS-CALC-CD
003278         MOVE CF-BENEFIT-DESCRIP (WS-INDEX)
003279                                 TO  WS-BENEFIT-DESCRIP
003280         MOVE +1                 TO  WS-NOT-FOUND
003281         GO TO 1699-EXIT.
003282
003283     IF CF-BENEFIT-CODE (WS-INDEX)
003284             IS NOT LESS THAN  CF-HI-BEN-IN-REC
003285         GO TO 1699-EXIT.
003286
003287     IF WS-INDEX  IS LESS THAN  +8
003288         ADD +1                  TO  WS-INDEX
003289         GO TO 1610-LOOKUP-BENEFIT.
003290
003291 1699-EXIT.
003292     EXIT.
003293 EJECT
003294 1700-WRITE-CERT-NOTE      SECTION.
003295
003296     IF NOT MODIFY-CERT-CAP
003297         MOVE 'UPDATE'           TO  SM-READ
003298         PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
003299         MOVE ER-0070            TO  EMI-ERROR
003300         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
003301         GO TO 8200-SEND-DATAONLY.
003302
003303**MOVE CANCEL QUOTE NOTE INFO
003304     IF PI-LF-REFUND-AMT NUMERIC
003305        MOVE PI-LF-REFUND-AMT    TO WS-CN-LIFE-AMT-RF
003306        MOVE PI-LF-REFUND-METH   TO WS-CN-LF-REF-METH
003307     ELSE
003308        MOVE ZERO                TO WS-CN-LIFE-AMT-RF
003309                                    PI-LF-REFUND-AMT
003310        MOVE SPACE               TO WS-CN-LF-REF-METH
003311                                    PI-LF-REFUND-METH.
003312
003313     IF PI-AH-REFUND-AMT NUMERIC
003314        MOVE PI-AH-REFUND-AMT    TO WS-CN-AH-AMT-RF
003315        MOVE PI-AH-REFUND-METH   TO WS-CN-AH-REF-METH
003316     ELSE
003317        MOVE ZERO                TO WS-CN-AH-AMT-RF
003318                                    PI-AH-REFUND-AMT
003319        MOVE SPACE               TO WS-CN-AH-REF-METH
003320                                    PI-AH-REFUND-METH.
003321
003322     COMPUTE PI-TOTAL-REFUND-AMT = PI-LF-REFUND-AMT +
003323                                   PI-AH-REFUND-AMT.
003324
003325     MOVE PI-TOTAL-REFUND-AMT    TO WS-CN-TOTAL-RF.
003326
003327     MOVE SAVE-DATE              TO WS-CN-DT-QUOTED.
003328     MOVE PI-PROCESSOR-ID        TO WS-CN-PROCESSOR-ID.
003329     IF PI-LF-CANCEL-DATE-ED NOT EQUAL SPACE
003330        MOVE PI-LF-CANCEL-DATE-ED
003331                                 TO WS-CN-CANCEL-DT
003332     ELSE
003333        MOVE PI-AH-CANCEL-DATE-ED
003334                                 TO WS-CN-CANCEL-DT
003335     END-IF
003336
003337     MOVE 'CZ'                   TO CERT-NOTE-FILE
003338     MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD
003339     MOVE PI-CARRIER             TO CZ-CARRIER
003340     MOVE PI-GROUPING            TO CZ-GROUPING
003341     MOVE PI-STATE               TO CZ-STATE
003342     MOVE PI-ACCOUNT             TO CZ-ACCOUNT
003343     MOVE PI-CERT-EFF-DT         TO CZ-CERT-EFF-DT
003344     MOVE PI-CERT-PRIME          TO CZ-CERT-PRIME
003345     MOVE PI-CERT-SFX            TO CZ-CERT-SFX
003346     MOVE '1'                    TO CZ-RECORD-TYPE
003347     MOVE +1                     TO CZ-NOTE-SEQUENCE
003348
003349     MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER
003350     MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS
003351     MOVE SAVE-BIN-DATE          TO CZ-LAST-MAINT-DT
003352     MOVE WS-CAN-QUOTE-NOTE      TO CZ-NOTE-INFORMATION
003353
003354     MOVE ' '                    TO WS-STOP-SW
003355     PERFORM UNTIL I-SAY-TO-STOP
003356        
      * EXEC CICS WRITE
003357*           FROM      (CERT-NOTE-FILE)
003358*           DATASET   (ERNOTE-ID)
003359*           RIDFLD    (CZ-CONTROL-PRIMARY)
003360*           RESP      (WS-RESPONSE)
003361*       END-EXEC
           MOVE LENGTH OF
            CERT-NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00008820' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303038383230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 CERT-NOTE-FILE, 
                 DFHEIV11, 
                 CZ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003362        IF WS-RESP-DUPREC
003363           ADD +1                TO CZ-NOTE-SEQUENCE
003364        ELSE
003365           SET I-SAY-TO-STOP     TO TRUE
003366        END-IF
003367     END-PERFORM
003368
003369     PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
003370     IF WS-RESP-NORMAL
003371        EVALUATE CM-NOTE-SW
003372           WHEN '1'
003373           WHEN '3'
003374           WHEN '5'
003375           WHEN '7'
003376              SET NO-CERT-RW     TO TRUE
003377           WHEN ' '
003378              MOVE '1'           TO CM-NOTE-SW
003379           WHEN '2'
003380              MOVE '3'           TO CM-NOTE-SW
003381           WHEN '4'
003382              MOVE '5'           TO CM-NOTE-SW
003383           WHEN '6'
003384              MOVE '7'           TO CM-NOTE-SW
003385        END-EVALUATE
003386     END-IF
003387     IF NOT NO-CERT-RW
003388        PERFORM 1810-REWRITE-ELCERT
003389                                 THRU 1810-EXIT
003390     ELSE
003391        
      * EXEC CICS UNLOCK
003392*          DATASET    (ELCERT-ID)
003393*       END-EXEC
      *    MOVE '&*                    #   #00008855' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038383535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003394     END-IF
003395
003396     MOVE ER-8160                TO EMI-ERROR.
003397     PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
003398     GO TO 8200-SEND-DATAONLY.
003399
003400 1799-EXIT.
003401     EXIT.
003402
003403 1800-READ-ELCERT-UPDATE.
003404
003405     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
003406     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
003407     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
003408     MOVE PI-STATE               TO  WS-CM-STATE.
003409     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
003410     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
003411     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
003412     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
003413
003414     
      * EXEC CICS READ
003415*        UPDATE
003416*        DATASET  (ELCERT-ID)
003417*        RIDFLD   (WS-CM-CONTROL-PRIMARY)
003418*        SET      (ADDRESS OF CERTIFICATE-MASTER)
003419*        RESP     (WS-RESPONSE)
003420*    END-EXEC
      *    MOVE '&"S        EU         (  N#00008878' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303038383738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003421
003422     .
003423 1800-EXIT.
003424     EXIT.
003425
003426 1810-REWRITE-ELCERT.
003427
003428     
      * EXEC CICS REWRITE
003429*        FROM      (CERTIFICATE-MASTER)
003430*        DATASET   (ELCERT-ID)
003431*        RESP     (WS-RESPONSE)
003432*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00008892' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303038383932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003433
003434     .
003435 1810-EXIT.
003436     EXIT.
003437
003438
003439 4000-GEN-CANCEL-TRANS.
003440
003441     IF PI-LF-REFUND-AMT NOT NUMERIC
003442         MOVE +0 TO PI-LF-REFUND-AMT
003443     END-IF
003444     IF PI-AH-REFUND-AMT NOT NUMERIC
003445         MOVE +0 TO PI-AH-REFUND-AMT
003446     END-IF
003447
003448     PERFORM 4400-ADD-ORIG-REC THRU 4400-EXIT
003449
003450     MOVE '2'                    TO CG-OPTION-CODE
003451     MOVE PI-COMPANY-ID          TO CG-COMPANY-ID
003452     MOVE PI-PROCESSOR-ID        TO CG-PROC-ID
003453     MOVE PI-CR-MONTH-END-DT     TO CG-MONTH-END-DT
003454     MOVE PI-COMPANY-CD          TO CG-CERT-COMPANY-CD
003455     MOVE PI-CARRIER             TO CG-CERT-CARRIER
003456     MOVE PI-GROUPING            TO CG-CERT-GROUP
003457     MOVE PI-STATE               TO CG-CERT-STATE
003458     MOVE PI-ACCOUNT             TO CG-CERT-ACCOUNT
003459     MOVE PI-CERT-EFF-DT         TO CG-CERT-EFF-DT
003460     MOVE PI-CERT-NO             TO CG-CERT-CERT-NO
003461     MOVE SAVE-BIN-DATE          TO CG-CURRENT-DT
003462
003463     MOVE ZEROS                  TO CG-LF-BENCD
003464                                    CG-AH-BENCD
003465                                    CG-LF-CAN-AMT
003466                                    CG-AH-CAN-AMT
003467     MOVE LOW-VALUES             TO CG-LF-CAN-DT
003468                                    CG-AH-CAN-DT
003469     MOVE SPACES                 TO CG-BATCH-NO
003470     MOVE PI-CANCEL-REASON       TO CG-DCC-REASON-CD
003471
003472     IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
003473       AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
003474        IF (PI-LF-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
003475           MOVE CM-LF-BENEFIT-CD  TO CG-LF-BENCD
003476           MOVE PI-LF-REFUND-AMT  TO CG-LF-CAN-AMT
003477           MOVE PI-LF-CANCEL-DATE TO CG-LF-CAN-DT
003478        END-IF
003479     END-IF
003480     IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
003481       AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
003482        IF (PI-AH-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
003483           MOVE CM-AH-BENEFIT-CD  TO CG-AH-BENCD
003484           MOVE PI-AH-REFUND-AMT  TO CG-AH-CAN-AMT
003485           MOVE PI-AH-CANCEL-DATE TO CG-AH-CAN-DT
003486        END-IF
003487     END-IF
003488
003489     
      * EXEC CICS LINK
003490*        PROGRAM  ('ELCANC')
003491*        COMMAREA (CANCEL-GEN-PASS-AREA)
003492*    END-EXEC
           MOVE LENGTH OF
            CANCEL-GEN-PASS-AREA
             TO DFHEIV11
           MOVE 'ELCANC' TO DFHEIV1
      *    MOVE '."C                   (   #00008953' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CANCEL-GEN-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003493     IF CG-SUCCESS
003494        CONTINUE
003495     ELSE
003496        MOVE SPACES              TO EMI-MESSAGE-AREA (1)
003497        move '3'                 to emi-switch1
003498        MOVE CG-ERROR-CODE       TO EMI-TEXT-VARIABLE (1)
003499        EVALUATE TRUE
003500           WHEN CG-DATE-ERROR
003501             MOVE WS-01-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003502           WHEN CG-CERT-NOT-FOUND
003503             MOVE WS-02-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003504           WHEN CG-AMOUNT-ERROR
003505             MOVE WS-04-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003506           WHEN CG-OPTION-ERROR
003507             MOVE WS-05-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003508           WHEN CG-PREV-CAN
003509             MOVE WS-06-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003510           WHEN CG-INVALID-DATA
003511             MOVE WS-07-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003512           WHEN CG-NO-ACCT-MSTR
003513             MOVE WS-08-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003514           WHEN CG-SFX-A-EXIST
003515             MOVE WS-09-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003516           WHEN CG-MISC-ERROR
003517             MOVE WS-99-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
003518           WHEN OTHER
003519             move ' error - elcanc - return '
003520                                 to emi-error-text (1) (12:25)
003521        END-EVALUATE
003522        go to 8200-send-dataonly
003523     end-if
003524
003525     .
003526 4000-exit.
003527     exit.
003528 4100-CONNECT-TO-DB.
003529
003530     IF SVR > SPACES
003531        CONTINUE
003532     ELSE
003533        MOVE 'NTCSO2_LOGIC'         TO SVR
003534        MOVE 'sa'                   TO USR
003535        MOVE 'ntcso2'               TO PASS
003536     END-IF
003537
003538     STRING
003539         USR DELIMITED SPACE
003540         "." DELIMITED SIZE
003541         PASS DELIMITED SPACE INTO USR-PASS
003542     END-STRING
003543
003545     EXEC SQL
              CONNECT TO :SVR USER :USR-PASS
003546     END-EXEC
003547
003548     IF SQLCODE NOT = 0
003549        DISPLAY "ERROR: CANNOT CONNECT "
003550        DISPLAY SQLCODE
003551        DISPLAY SQLERRMC
003552        GO TO 4100-EXIT
003553     END-IF
003554
003555     .
003556 4100-EXIT.
003557     EXIT.
003558 4300-DISCONNECT.
003559
003561     EXEC SQL
              DISCONNECT
003562     END-EXEC
003563     .
003564 4300-EXIT.
003565     EXIT.
003566
003567
003568
003569 4400-ADD-ORIG-REC.
003570
003571     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
003572     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
003573     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
003574     MOVE PI-STATE               TO  WS-CM-STATE.
003575     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
003576     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
003577     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
003578     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
003579
003580     
      * EXEC CICS READ
003581*        DATASET  (ELCERT-ID)
003582*        RIDFLD   (WS-CM-CONTROL-PRIMARY)
003583*        SET      (ADDRESS OF CERTIFICATE-MASTER)
003584*        RESP     (WS-RESPONSE)
003585*    END-EXEC
      *    MOVE '&"S        E          (  N#00009044' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039303434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003586     IF WS-RESP-NORMAL
003587         CONTINUE
003588     ELSE
003589        MOVE ER-0142                TO EMI-ERROR
003590        PERFORM 9700-ERROR-FORMAT THRU 9799-EXIT
003591        GO TO 8100-SEND-INITIAL-MAP
003592     END-IF
003593
003594******************************************************************
003595*            A D D   O R I G   C E R T   I N F O                 *
003596******************************************************************
003597
003598     display ' made it to add orig cert ' WS-CM-CONTROL-PRIMARY
003599
003600     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY
003601     MOVE ' '                    TO WS-ERMAIL-SW
003602
003603     
      * EXEC CICS READ
003604*       DATASET   ('ERMAIL')
003605*       SET       (ADDRESS OF MAILING-DATA)
003606*       RIDFLD    (WS-MA-CONTROL-PRIMARY)
003607*       RESP      (WS-RESPONSE)
003608*    END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009067' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039303637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003609
003610     IF WS-RESP-NORMAL
003611        SET ERMAIL-FOUND TO TRUE
003612     END-IF
003613
003614     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTT-PRIMARY
003615     MOVE 'C'                   TO ELCRTT-REC-TYPE
003616     MOVE +0                    TO WS-CERT-TRL-REC-NOT-FOUND
003617
003618     
      * EXEC CICS READ
003619*         DATASET  (ELCRTT-ID)
003620*         RIDFLD   (ELCRTT-KEY)
003621*         SET      (ADDRESS OF CERTIFICATE-TRAILERS)
003622*         RESP     (WS-RESPONSE)
003623*    END-EXEC
      *    MOVE '&"S        E          (  N#00009082' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039303832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003624
003625     IF NOT WS-RESP-NORMAL
003626        MOVE +1               TO WS-CERT-TRL-REC-NOT-FOUND
003627     END-IF
003628
003629     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTO-KEY (1:33)
003630     MOVE 'I'                 TO ELCRTO-RECORD-TYPE
003631     MOVE +0                  TO ELCRTO-SEQ-NO
003632
003633     
      * EXEC CICS READ
003634*       DATASET   ('ELCRTO')
003635*       SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
003636*       RIDFLD    (ELCRTO-KEY)
003637*       GTEQ
003638*       RESP      (WS-RESPONSE)
003639*    END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00009097' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303039303937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003640
003641     IF WS-RESP-NORMAL
003642        AND (OC-CONTROL-PRIMARY (1:33) =
003643                 WS-CM-CONTROL-PRIMARY)
003644        AND (OC-RECORD-TYPE = 'I')
003645        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
003646           
      * EXEC CICS READ
003647*             DATASET   ('ELCRTO')
003648*             SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
003649*             RIDFLD    (OC-CONTROL-PRIMARY)
003650*             UPDATE
003651*             RESP      (WS-RESPONSE)
003652*          END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00009110' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303039313130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003653           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME
003654           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME
003655           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT
003656           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE
003657           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME
003658           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME
003659           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT
003660           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE
003661           IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
003662            (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES))
003663               MOVE SPACES             TO OC-LF-BENCD
003664               MOVE ZEROS              TO OC-LF-TERM
003665                                          OC-LF-BEN-AMT
003666                                          OC-LF-PRM-AMT
003667                                          OC-LF-ALT-BEN-AMT
003668                                          OC-LF-ALT-PRM-AMT
003669                                          OC-LF-COMM-PCT
003670                                          OC-LF-CANCEL-AMT
003671               MOVE LOW-VALUES         TO OC-LF-EXP-DT
003672               MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
003673           ELSE
003674               MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD
003675               MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM
003676               MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT
003677               MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT
003678               MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT
003679               MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT
003680               MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT
003681               MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT
003682               MOVE PI-LF-REFUND-AMT       TO OC-LF-CANCEL-AMT
003683               MOVE PI-LF-CANCEL-DATE      TO OC-LF-CANCEL-DT
003684           END-IF
003685           MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
003686           IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
003687            (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES))
003688               MOVE SPACES             TO OC-AH-BENCD
003689               MOVE ZEROS              TO OC-AH-TERM
003690                                          OC-AH-BEN-AMT
003691                                          OC-AH-PRM-AMT
003692                                          OC-AH-COMM-PCT
003693                                          OC-AH-CANCEL-AMT
003694                                          OC-AH-CP
003695               MOVE LOW-VALUES         TO OC-AH-EXP-DT
003696               MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
003697           ELSE
003698               MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD
003699               MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM
003700               MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT
003701               MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT
003702               MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT
003703               MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT
003704               MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
003705               MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
003706               MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
003707           END-IF
003708           MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
003709           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
003710           MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
003711           MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
003712           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
003713           MOVE EIBDATE                TO DC-JULIAN-YYDDD
003714           MOVE '5'                    TO DC-OPTION-CODE
003715           PERFORM 9600-DATE-LINK
003716           MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
003717           IF ERMAIL-FOUND
003718               MOVE MA-CRED-BENE-NAME
003719                           TO OC-CRED-BENE-NAME
003720           END-IF
003721           IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
003722               MOVE CS-INS-AGE-DEFAULT-FLAG TO
003723                                  OC-INS-AGE-DEFAULT-FLAG
003724               MOVE CS-JNT-AGE-DEFAULT-FLAG TO
003725                                  OC-JNT-AGE-DEFAULT-FLAG
003726           END-IF
003727
003728           
      * EXEC CICS REWRITE
003729*             DATASET   ('ELCRTO')
003730*             FROM      (ORIGINAL-CERTIFICATE)
003731*             RESP      (WS-RESPONSE)
003732*          END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&& L                  %  N#00009192' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303039313932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003733           IF NOT WS-RESP-NORMAL
003734              MOVE ER-3830          TO EMI-ERROR
003735              PERFORM 9700-ERROR-FORMAT
003736                                 THRU 9799-EXIT
003737              GO TO 8200-SEND-DATAONLY
003738           END-IF
003739
003740           GO TO 4400-EXIT
003741
003742        ELSE
003743           SUBTRACT +1 FROM OC-KEY-SEQ-NO
003744        END-IF
003745     ELSE
003746        MOVE SPACES              TO ORIGINAL-CERTIFICATE
003747        MOVE 'OC'                TO OC-RECORD-ID
003748        MOVE WS-CM-CONTROL-PRIMARY TO OC-CONTROL-PRIMARY (1:33)
003749        MOVE 'I'                 TO OC-RECORD-TYPE
003750        MOVE +4096               TO OC-KEY-SEQ-NO
003751     END-IF
003752
003753     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME
003754     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME
003755     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT
003756     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE
003757     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME
003758     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME
003759     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT
003760     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE
003761     IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
003762      (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES))
003763         MOVE SPACES             TO OC-LF-BENCD
003764         MOVE ZEROS              TO OC-LF-TERM
003765                                    OC-LF-BEN-AMT
003766                                    OC-LF-PRM-AMT
003767                                    OC-LF-ALT-BEN-AMT
003768                                    OC-LF-ALT-PRM-AMT
003769                                    OC-LF-COMM-PCT
003770                                    OC-LF-CANCEL-AMT
003771         MOVE LOW-VALUES         TO OC-LF-EXP-DT
003772         MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
003773     ELSE
003774         MOVE CM-LF-BENEFIT-CD   TO OC-LF-BENCD
003775         MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM
003776         MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT
003777         MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT
003778         MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
003779         MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
003780         MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT
003781         MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT
003782         MOVE PI-LF-REFUND-AMT   TO OC-LF-CANCEL-AMT
003783         MOVE PI-LF-CANCEL-DATE  TO OC-LF-CANCEL-DT
003784     END-IF
003785     MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
003786     IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
003787      (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES))
003788         MOVE SPACES             TO OC-AH-BENCD
003789         MOVE ZEROS              TO OC-AH-TERM
003790                                    OC-AH-BEN-AMT
003791                                    OC-AH-PRM-AMT
003792                                    OC-AH-COMM-PCT
003793                                    OC-AH-CANCEL-AMT
003794                                    OC-AH-CP
003795         MOVE LOW-VALUES         TO OC-AH-EXP-DT
003796         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
003797     ELSE
003798         MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD
003799         MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM
003800         MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT
003801         MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT
003802         MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT
003803         MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT
003804         MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
003805         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
003806         MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
003807         MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
003808     END-IF
003809     MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
003810     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
003811     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
003812     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
003813     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
003814     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
003815     MOVE EIBDATE                TO DC-JULIAN-YYDDD
003816     MOVE '5'                    TO DC-OPTION-CODE
003817     PERFORM 9600-DATE-LINK
003818     MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
003819     IF ERMAIL-FOUND
003820         MOVE MA-CRED-BENE-NAME
003821                     TO OC-CRED-BENE-NAME
003822     END-IF
003823     IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
003824         MOVE CS-INS-AGE-DEFAULT-FLAG TO
003825                            OC-INS-AGE-DEFAULT-FLAG
003826         MOVE CS-JNT-AGE-DEFAULT-FLAG TO
003827                            OC-JNT-AGE-DEFAULT-FLAG
003828     END-IF
003829
003830     MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
003831     .
003832 4400-WRITE-ELCRTO.
003833
003834     
      * EXEC CICS WRITE
003835*       DATASET   ('ELCRTO')
003836*       FROM      (ORIGINAL-CERTIFICATE)
003837*       RIDFLD    (OC-CONTROL-PRIMARY)
003838*       RESP      (WS-RESPONSE)
003839*    END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00009298' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303039323938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003840
003841    IF WS-RESP-DUPKEY OR WS-RESP-DUPREC
003842        SUBTRACT +1    FROM OC-KEY-SEQ-NO
003843        GO TO 4400-WRITE-ELCRTO
003844    ELSE
003845        IF NOT WS-RESP-NORMAL
003846           MOVE ER-3830          TO EMI-ERROR
003847           PERFORM 9700-ERROR-FORMAT
003848                                 THRU 9799-EXIT
003849           GO TO 8200-SEND-DATAONLY
003850        END-IF
003851    END-IF
003852
003853     .
003854 4400-EXIT.
003855    EXIT.
003856
003857
003858
003859 5000-GET-LF-AM-REM-TERM.
003860     MOVE +1                     TO SUB3.
003861
003862 5100-GET-LF-AM-REM-TERM.
003863     IF AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
003864        IF CM-LF-BENEFIT-CD = AM-BENEFIT-CODE (SUB3)
003865           MOVE AM-ALLOWABLE-BENEFITS (SUB3)
003866                         TO WS-SAVE-ALLOWABLE-BENEFIT
003867           GO TO 5300-MOVE-LF-REM-TERM.
003868
003869     IF SUB3 LESS THAN +20
003870        ADD +1                   TO SUB3
003871        GO TO 5100-GET-LF-AM-REM-TERM.
003872
003873     MOVE +1                     TO SUB3.
003874
003875     IF PI-COMPANY-ID NOT EQUAL 'DMD'
003876         GO TO 5200-EDIT-FOR-NINETY-LF.
003877
003878 5100-GET-LF-DMD-AM-REM-TERM.
003879
003880     IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
003881        IF CM-LF-BENEFIT-CD = AM-BENEFIT-DMD-CODE (SUB3)
003882           MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
003883                         TO WS-SAVE-ALLOWABLE-BENEFIT
003884           GO TO 5300-MOVE-LF-REM-TERM.
003885
003886     IF SUB3 LESS THAN +30
003887        ADD +1                   TO SUB3
003888        GO TO 5100-GET-LF-DMD-AM-REM-TERM.
003889
003890     MOVE +1                     TO SUB3.
003891
003892 5200-EDIT-FOR-NINETY-LF.
003893
003894***************************************************************
003895*                                                             *
003896*    NINETY BENEFIT PLAN TYPES:                               *
003897*                                                             *
003898*      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
003899*      92 = ALL GROUP REDUCING BENEFITS                       *
003900*      93 = ALL INDIVIDUAL LEVEL BENEFITS                     *
003901*      94 = ALL GROUP LEVEL BENEFITS                          *
003902*      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
003903*           BENEFIT CONTROLS.                                 *
003904*      99 = ALL BENEFITS                                      *
003905*                                                             *
003906***************************************************************
003907
003908     MOVE AM-ALLOWABLE-BENEFITS (SUB3)
003909                         TO WS-SAVE-ALLOWABLE-BENEFIT.
003910
003911     IF (AM-BENEFIT-CODE (SUB3) = '99')
003912       AND
003913        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003914           GO TO 5300-MOVE-LF-REM-TERM.
003915
003916     IF (AM-BENEFIT-CODE (SUB3) = '91')
003917       AND
003918        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003919        IF CF-REDUCING (WS-INDEX)
003920           IF CM-INDIVIDUAL
003921               GO TO 5300-MOVE-LF-REM-TERM.
003922
003923     IF (AM-BENEFIT-CODE (SUB3) = '92')
003924       AND
003925        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003926        IF CF-REDUCING (WS-INDEX)
003927           IF CM-GROUP
003928               GO TO 5300-MOVE-LF-REM-TERM.
003929
003930     IF (AM-BENEFIT-CODE (SUB3) = '93')
003931       AND
003932        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003933        IF CF-LEVEL (WS-INDEX)
003934           IF CM-INDIVIDUAL
003935               GO TO 5300-MOVE-LF-REM-TERM.
003936
003937     IF (AM-BENEFIT-CODE (SUB3) = '94')
003938       AND
003939        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003940        IF CF-LEVEL (WS-INDEX)
003941           IF CM-GROUP
003942               GO TO 5300-MOVE-LF-REM-TERM.
003943
003944     IF (AM-BENEFIT-CODE (SUB3) = '98')
003945       AND
003946        (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003947            GO TO 5300-MOVE-LF-REM-TERM.
003948
003949     IF SUB3 LESS THAN +20
003950        ADD +1           TO SUB3
003951          GO TO 5200-EDIT-FOR-NINETY-LF.
003952
003953     MOVE SPACES         TO WS-SAVE-ALLOWABLE-BENEFIT.
003954
003955     IF PI-COMPANY-ID NOT EQUAL 'DMD'
003956         MOVE '0'                 TO WS-LF-AM-REM-TERM-CALC
003957         GO TO 5900-EXIT.
003958
003959     MOVE 1                       TO SUB3.
003960
003961 5200-EDIT-FOR-DMD-NINETY-LF.
003962
003963     MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
003964                         TO WS-SAVE-ALLOWABLE-BENEFIT.
003965
003966     IF (AM-BENEFIT-DMD-CODE (SUB3) = '99')
003967       AND
003968        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003969           GO TO 5300-MOVE-LF-REM-TERM.
003970
003971     IF (AM-BENEFIT-DMD-CODE (SUB3) = '91')
003972       AND
003973        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003974        IF CF-REDUCING (WS-INDEX)
003975           IF CM-INDIVIDUAL
003976               GO TO 5300-MOVE-LF-REM-TERM.
003977
003978     IF (AM-BENEFIT-DMD-CODE (SUB3) = '92')
003979       AND
003980        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003981        IF CF-REDUCING (WS-INDEX)
003982           IF CM-GROUP
003983               GO TO 5300-MOVE-LF-REM-TERM.
003984
003985     IF (AM-BENEFIT-DMD-CODE (SUB3) = '93')
003986       AND
003987        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003988        IF CF-LEVEL (WS-INDEX)
003989           IF CM-INDIVIDUAL
003990               GO TO 5300-MOVE-LF-REM-TERM.
003991
003992     IF (AM-BENEFIT-DMD-CODE (SUB3) = '94')
003993       AND
003994        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
003995        IF CF-LEVEL (WS-INDEX)
003996           IF CM-GROUP
003997               GO TO 5300-MOVE-LF-REM-TERM.
003998
003999     IF (AM-BENEFIT-DMD-CODE (SUB3) = '98')
004000       AND
004001        (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
004002            GO TO 5300-MOVE-LF-REM-TERM.
004003
004004     IF SUB3 LESS THAN +30
004005        ADD +1                   TO SUB3
004006          GO TO 5200-EDIT-FOR-DMD-NINETY-LF.
004007
004008     MOVE '0'                    TO WS-LF-AM-REM-TERM-CALC.
004009
004010     GO TO 5900-EXIT.
004011
004012 5300-MOVE-LF-REM-TERM.
004013     MOVE WS-SAVE-BENEFIT-REM-TERM
004014                                 TO WS-LF-AM-REM-TERM-CALC.
004015
004016 5900-EXIT.
004017      EXIT.
004018
004019 EJECT
004020
004021 6000-GET-AH-AM-REM-TERM.
004022     MOVE +1                     TO SUB3.
004023
004024 6100-GET-AH-AM-REM-TERM.
004025
004026     IF AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004027        IF WS-BEN-CD = AM-BENEFIT-CODE (SUB3)
004028           MOVE AM-ALLOWABLE-BENEFITS (SUB3)
004029                         TO WS-SAVE-ALLOWABLE-BENEFIT
004030           GO TO 6300-MOVE-AH-REM-TERM.
004031
004032     IF SUB3 LESS THAN +20
004033        ADD +1                   TO SUB3
004034        GO TO 6100-GET-AH-AM-REM-TERM.
004035
004036     MOVE +1                     TO SUB3.
004037
004038     IF PI-COMPANY-ID NOT EQUAL 'DMD'
004039         GO TO 6200-EDIT-FOR-NINETY-AH.
004040
004041 6100-GET-AH-AM-DMD-REM-TERM.
004042     IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004043        IF WS-BEN-CD = AM-BENEFIT-DMD-CODE (SUB3)
004044           MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
004045                         TO WS-SAVE-ALLOWABLE-BENEFIT
004046           GO TO 6300-MOVE-AH-REM-TERM.
004047
004048     IF SUB3 LESS THAN +30
004049        ADD +1                   TO SUB3
004050        GO TO 6100-GET-AH-AM-DMD-REM-TERM.
004051
004052     MOVE +1                     TO SUB3.
004053
004054 6200-EDIT-FOR-NINETY-AH.
004055
004056***************************************************************
004057*                                                             *
004058*    NINETY BENEFIT PLAN TYPES:                               *
004059*                                                             *
004060*      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
004061*      92 = ALL GROUP REDUCING BENEFITS.                      *
004062*      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
004063*           BENEFIT CONTROLS.                                 *
004064*      99 = ALL BENEFITS.                                     *
004065*                                                             *
004066***************************************************************
004067
004068     MOVE AM-ALLOWABLE-BENEFITS (SUB3)
004069                         TO WS-SAVE-ALLOWABLE-BENEFIT.
004070
004071     IF AM-BENEFIT-CODE (SUB3) = '99'
004072       AND
004073        AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004074           GO TO 6300-MOVE-AH-REM-TERM.
004075
004076     IF AM-BENEFIT-CODE (SUB3) = '91'
004077       AND
004078        AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004079           IF CM-INDIVIDUAL
004080               GO TO 6300-MOVE-AH-REM-TERM.
004081
004082     IF AM-BENEFIT-CODE (SUB3) = '92'
004083       AND
004084        AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004085           IF CM-GROUP
004086               GO TO 6300-MOVE-AH-REM-TERM.
004087
004088     IF AM-BENEFIT-CODE (SUB3) = '98'
004089       AND
004090        AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004091           GO TO 6300-MOVE-AH-REM-TERM.
004092
004093     IF SUB3 LESS THAN +20
004094        ADD +1                    TO SUB3
004095        GO TO 6200-EDIT-FOR-NINETY-AH.
004096
004097     MOVE +1                      TO SUB3.
004098
004099     IF PI-COMPANY-ID NOT EQUAL 'DMD'
004100         MOVE '0'                 TO WS-AH-AM-REM-TERM-CALC
004101         GO TO 6900-EXIT.
004102
004103 6200-EDIT-FOR-DMD-NINETY-AH.
004104
004105     MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
004106                         TO WS-SAVE-ALLOWABLE-BENEFIT.
004107
004108     IF AM-BENEFIT-DMD-CODE (SUB3) = '99'
004109       AND
004110        AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004111           GO TO 6300-MOVE-AH-REM-TERM.
004112
004113     IF AM-BENEFIT-DMD-CODE (SUB3) = '91'
004114       AND
004115        AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004116           IF CM-INDIVIDUAL
004117               GO TO 6300-MOVE-AH-REM-TERM.
004118
004119     IF AM-BENEFIT-DMD-CODE (SUB3) = '92'
004120       AND
004121        AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004122           IF CM-GROUP
004123               GO TO 6300-MOVE-AH-REM-TERM.
004124
004125     IF AM-BENEFIT-DMD-CODE (SUB3) = '98'
004126       AND
004127        AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
004128           GO TO 6300-MOVE-AH-REM-TERM.
004129
004130     IF SUB3 LESS THAN +30
004131        ADD +1                   TO SUB3
004132        GO TO 6200-EDIT-FOR-DMD-NINETY-AH.
004133
004134     MOVE '0'                    TO WS-AH-AM-REM-TERM-CALC.
004135
004136     GO TO 6900-EXIT.
004137
004138 6300-MOVE-AH-REM-TERM.
004139
004140     MOVE WS-SAVE-BENEFIT-REM-TERM
004141                                 TO WS-AH-AM-REM-TERM-CALC.
004142
004143 6900-EXIT.
004144      EXIT.
004145
004146 EJECT
004147 7000-GET-ERFORM     SECTION.
004148     
      * EXEC CICS HANDLE CONDITION
004149*        NOTFND  (7000-EXIT)
004150*    END-EXEC.
      *    MOVE '"$I                   ! ( #00009612' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303039363132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004151
004152     MOVE ' '                    TO  WS-FORM-RECORD-SW.
004153     MOVE PI-COMPANY-CD          TO  WS-FO-COMPANY-CD.
004154     MOVE PI-STATE               TO  WS-FO-STATE.
004155     MOVE CM-POLICY-FORM-NO      TO  WS-FO-FORM-ID.
004156     MOVE CM-CERT-EFF-DT         TO  WS-FO-EXP-DT.
004157
004158     
      * EXEC CICS READ
004159*        DATASET  (ERFORM-ID)
004160*        RIDFLD   (WS-FO-CONTROL-PRIMARY)
004161*        SET      (ADDRESS OF FORM-MASTER)
004162*        GTEQ
004163*    END-EXEC.
      *    MOVE '&"S        G          (   #00009622' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039363232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-FO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004164
004165     IF PI-COMPANY-CD     EQUAL FO-COMPANY-CD AND
004166        PI-STATE          EQUAL FO-STATE      AND
004167        CM-POLICY-FORM-NO EQUAL FO-FORM-ID
004168         MOVE 'Y'  TO WS-FORM-RECORD-SW
004169         GO TO 7000-EXIT.
004170 7000-EXIT.
004171     EXIT.
004172 7100-BENEFIT-SEARCH SECTION.
004173
004174     IF CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES
004175        MOVE +1                  TO  SUB4
004176        PERFORM 7150-BENEFIT-LIFE-SEARCH THRU 7150-EXIT.
004177
004178     IF CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES
004179        MOVE +1                  TO  SUB4
004180        PERFORM 7250-BENEFIT-AH-SEARCH THRU 7250-EXIT.
004181 7100-EXIT.
004182     EXIT.
004183 7150-BENEFIT-LIFE-SEARCH.
004184     IF PI-LIFE-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
004185       IF CM-LF-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
004186         IF (CM-LF-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
004187          OR CM-LF-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
004188            IF FO-PLAN-REFUND-METHOD (SUB4)
004189                    IS GREATER THAN SPACES
004190               MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
004191                                    WS-LF-FO-REFUND-CALC
004192               GO TO 7150-EXIT.
004193
004194     IF SUB4 LESS THAN +40
004195         ADD +1              TO SUB4
004196         GO TO 7150-BENEFIT-LIFE-SEARCH.
004197
004198 7150-EXIT.
004199     EXIT.
004200 7250-BENEFIT-AH-SEARCH.
004201     IF PI-AH-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
004202       IF CM-AH-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
004203         IF (CM-AH-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
004204          OR CM-AH-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
004205            IF FO-PLAN-REFUND-METHOD (SUB4)
004206                    IS GREATER THAN SPACES
004207               MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
004208                                    WS-AH-FO-REFUND-CALC
004209               GO TO 7250-EXIT.
004210
004211     IF SUB4 LESS THAN +40
004212         ADD +1              TO SUB4
004213         GO TO 7250-BENEFIT-AH-SEARCH.
004214
004215 7250-EXIT.
004216     EXIT.
004217 EJECT
004218
004219 8100-SEND-INITIAL-MAP  SECTION.
004220     MOVE SAVE-DATE              TO  HDATEO.
004221     MOVE EIBTIME                TO  TIME-IN.
004222     MOVE TIME-OUT               TO  HTIMEO.
004223     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
004224     MOVE PI-PROCESSOR-ID        TO  USERIDO.
004225     MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
004226     MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
004227
004228     IF CM-LF-BENEFIT-CD = '00'
004229         MOVE -1                 TO  HACANCL
004230     ELSE
004231         MOVE -1                 TO  HLCANCL.
004232
004233
004234     if pi-company-id = 'VPP'
004235        move al-sanof            to refdueha
004236                                    canfeeha
004237        move al-uanof            to refduea
004238                                    canfeea
004239     end-if
004240
004241     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
004242*       MOVE AL-SADOF            TO CANREAHA
004243        MOVE AL-SADOF            TO CLPYNHA
004244*                                   CANREAA
004245                                    CLPYNA
004246     END-IF
004247
004248     
      * EXEC CICS SEND
004249*        FROM    (EL127HO)
004250*        MAPSET  (MAPSET-NAME)
004251*        MAP     (MAP-NAME)
004252*        CURSOR
004253*        ERASE
004254*    END-EXEC.
           MOVE LENGTH OF
            EL127HO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009712' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303039373132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004255
004256     GO TO 9000-RETURN-TRAN.
004257
004258 8200-SEND-DATAONLY  SECTION.
004259     IF FIRST-ENTRY
004260         GO TO 8100-SEND-INITIAL-MAP.
004261
004262     MOVE SAVE-DATE              TO  HDATEO.
004263     MOVE EIBTIME                TO  TIME-IN.
004264     MOVE TIME-OUT               TO  HTIMEO.
004265     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
004266     MOVE PI-PROCESSOR-ID        TO  USERIDO.
004267     MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
004268     MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
004269     MOVE -1                     TO  HLCANCL.
004270
004271     
      * EXEC CICS SEND DATAONLY
004272*        FROM    (EL127HO)
004273*        MAPSET  (MAPSET-NAME)
004274*        MAP     (MAP-NAME)
004275*        CURSOR
004276*    END-EXEC.
           MOVE LENGTH OF
            EL127HO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009735' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303039373335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004277
004278     GO TO 9000-RETURN-TRAN.
004279 EJECT
004280 8300-SEND-TEXT  SECTION.
004281     
      * EXEC CICS SEND TEXT
004282*        FROM    (LOGOFF-TEXT)
004283*        LENGTH  (LOGOFF-LENGTH)
004284*        ERASE
004285*        FREEKB
004286*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009745' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303039373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
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
           
004287
004288     
      * EXEC CICS RETURN
004289*    END-EXEC.
      *    MOVE '.(                    ''   #00009752' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039373532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004290 EJECT
004291 8600-DEEDIT  SECTION.
004292     
      * EXEC CICS BIF DEEDIT
004293*        FIELD   (DEEDIT-FIELD)
004294*        LENGTH  (15)
004295*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009756' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039373536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004296 EJECT
004297 8700-UNAUTHORIZED-ACCESS  SECTION.
004298     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
004299
004300     GO TO 8300-SEND-TEXT.
004301
004302 8800-PF23  SECTION.
004303     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
004304     MOVE XCTL-005               TO  PGM-NAME.
004305
004306     GO TO 9200-XCTL.
004307
004308 8900-RETURN-CICS  SECTION.
004309     
      * EXEC CICS RETURN
004310*    END-EXEC.
      *    MOVE '.(                    ''   #00009773' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039373733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004311
004312 9000-RETURN-TRAN  SECTION.
004313     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
004314     MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
004315
004316     
      * EXEC CICS RETURN
004317*        TRANSID   (TRANS-ID)
004318*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
004319*        LENGTH    (PI-COMM-LENGTH)
004320*    END-EXEC.
      *    MOVE '.(CT                  ''   #00009780' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039373830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004321
004322 9100-RETURN-MAIN-MENU  SECTION.
004323     MOVE XCTL-126               TO  PGM-NAME.
004324
004325     GO TO 9200-XCTL.
004326
004327 9200-XCTL  SECTION.
004328     
      * EXEC CICS XCTL
004329*        PROGRAM   (PGM-NAME)
004330*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
004331*        LENGTH    (PI-COMM-LENGTH)
004332*    END-EXEC.
      *    MOVE '.$C                   %   #00009792' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039373932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004333
004334 9300-CLEAR  SECTION.
004335     MOVE ' '                    TO  PI-PEND-SW.
004336     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
004337
004338     GO TO 9200-XCTL.
004339
004340 9400-PF12  SECTION.
004341     MOVE XCTL-010               TO  PGM-NAME.
004342
004343     GO TO 9200-XCTL.
004344
004345 9500-PGMID-ERROR  SECTION.
004346     
      * EXEC CICS HANDLE CONDITION
004347*        PGMIDERR  (8300-SEND-TEXT)
004348*    END-EXEC.
      *    MOVE '"$L                   ! ) #00009810' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303039383130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004349
004350     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
004351     MOVE ' '                    TO  PI-ENTRY-CD-1.
004352     MOVE XCTL-005               TO  PGM-NAME.
004353     MOVE PGM-NAME               TO  LOGOFF-PGM.
004354     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
004355
004356     GO TO 9200-XCTL.
004357
004358 9600-DATE-LINK  SECTION.
004359     MOVE LINK-ELDATCV           TO  PGM-NAME
004360
004361     
      * EXEC CICS LINK
004362*        PROGRAM   (PGM-NAME)
004363*        COMMAREA  (DATE-CONVERSION-DATA)
004364*        LENGTH    (DC-COMM-LENGTH)
004365*    END-EXEC.
      *    MOVE '."C                   (   #00009825' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004366
004367
004368 9700-ERROR-FORMAT  SECTION.
004369     IF NOT EMI-ERRORS-COMPLETE
004370         MOVE LINK-001               TO  PGM-NAME
004371         
      * EXEC CICS LINK
004372*            PROGRAM   (PGM-NAME)
004373*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
004374*            LENGTH    (EMI-COMM-LENGTH)
004375*        END-EXEC.
      *    MOVE '."C                   (   #00009835' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004376
004377 9799-EXIT.
004378     EXIT.
004379
004380 9800-SECURITY-VIOLATION  SECTION.
004381*                            COPY ELCSCTP.
      *>>((file: ELCSCTP))
000001******************************************************************
000002*                                                                *
000003*                            ELCSCTP                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
000007******************************************************************
000008
000009
000010     MOVE EIBDATE          TO SM-JUL-DATE.
000011     MOVE EIBTRMID         TO SM-TERMID.
000012     MOVE THIS-PGM         TO SM-PGM.
000013     MOVE EIBTIME          TO TIME-IN.
000014     MOVE TIME-OUT         TO SM-TIME.
000015     MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
000016
000017     
      * EXEC CICS LINK
000018*         PROGRAM  ('EL003')
000019*         COMMAREA (SECURITY-MESSAGE)
000020*         LENGTH   (80)
000021*    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00009863' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000022
000023******************************************************************
000024
      *<<((file: ELCSCTP))
004382
004383 9899-EXIT.
004384     EXIT.
004385
004386 9900-ABEND  SECTION.
004387     MOVE LINK-004               TO  PGM-NAME.
004388     MOVE DFHEIBLK               TO  EMI-LINE1.
004389
004390     
      * EXEC CICS LINK
004391*        PROGRAM   (PGM-NAME)
004392*        COMMAREA  (EMI-LINE1)
004393*        LENGTH    (72)
004394*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009880' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004395
004396     MOVE -1                     TO  HLCANCL.
004397
004398     GO TO 8200-SEND-DATAONLY.
004399
004400 9999-LAST-PARAGRAPH.
004401     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9500-PGMID-ERROR,
                     9900-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0910-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1010-NOTFND,
                     1020-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1290-ACCT-NOT-FOUND,
                     1290-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0580-ACCT-NOT-FOUND,
                     0580-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
