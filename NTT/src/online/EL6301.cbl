      *((program: EL6301.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL6301.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 04/21/94 14:29:04.
000007*                            VMOD=2.075
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*DATE-COMPILED.
000013
000014*SECURITY.   *****************************************************
000015*            *                                                   *
000016*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000017*            *                                                   *
000018*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000019*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000020*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000021*            *                                                   *
000022*            *****************************************************
000023
000024*REMARKS.
000025*         TRANSACTION - EXA6 - NEW BUSINESS - DATA ENTRY (ISSUES).
000026******************************************************************
000027*                   C H A N G E   L O G
000028*
000029* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000030*-----------------------------------------------------------------
000031*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000032* EFFECTIVE    NUMBER
000033*-----------------------------------------------------------------
000034* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000035* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
000036* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000037* 081606  CR2006080800002  PEMA  ADD VIN NUMBER
000038* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000039* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
000040* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000041* 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
000042* 020210  IR2010011100002  PEMA  CORRECT ATTRB ON BCZIPCD
000043* 030310  IR2010022400001  PEMA  CORRECT PF5 LOGIC, ADD APR PROC
000044* 030310  CR2009031200002  PEMA  CHECK LOAN OFF EDIT SWITCH
000045* 060211  CR2011051600002  PEMA  OPEN CP FIELD
000046* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000047* 050713  CR2008042200001  PEMA  ADD ZERO PCT APR PROCESSING
000048* 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
000049* 020514  IR2014012400001  PEMA  DARK OUT AH ALT BEN FOR CID&AHL
000050* 042114  CR2014032000001  PEMA  rearrange dob,jntdob&ssn
000051* 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
000052* 101615  CR2015080300002  PEMA  ALLOW VIN FOR CID
000053* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
000054* 041320  CR2020040200001  PEMA  PENDING BUSINESS JOURNALING
000055* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000056* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000057* 070622  CR2020061200002  TANA  Add cancel reason logic
000058******************************************************************
000059
000060 ENVIRONMENT DIVISION.
000061 DATA DIVISION.
000062 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000063 77  FILLER  PIC X(32)  VALUE '********************************'.
000064 77  FILLER  PIC X(32)  VALUE '*    EL6301 WORKING STORAGE    *'.
000065 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.075 *********'.
000066 77  WS-BEG                      PIC S999 COMP-3 VALUE +0.
000067 77  A1                          PIC S999 COMP-3 VALUE +0.
000068 77  WS-END                      PIC S999 COMP-3 VALUE +0.
000069
000070*    COPY ELCSCTM.
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
000071
000072*    COPY ELCSCRTY.
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
000073
000074 01  WS-NUM-TABLE                PIC X(26)  VALUE
000075     '12345678012345070923456789'.
000076 01  FILLER REDEFINES WS-NUM-TABLE.
000077     05  WS-NUM OCCURS 26        PIC 9.
000078 01  V1                          PIC S999 COMP-3.
000079 01  V2                          PIC S999 COMP-3.
000080 01  V3                          PIC S999 COMP-3.
000081 01  WS-WORK-VIN                 PIC X(17)  VALUE SPACES.
000082 01  FILLER REDEFINES WS-WORK-VIN.
000083     05  WS-WORK-VIN-N OCCURS 17        PIC 9.
000084 01  WS-VIN-TOTAL                PIC S9(9)  VALUE +0.
000085 01  WS-VIN-FINAL                PIC S9(7)  VALUE +0.
000086 01  WS-VIN-REMAINDER            PIC S999   VALUE +0.
000087 01  WS-HEX-WORK.
000088     05  FILLER                  PIC X  VALUE LOW-VALUES.
000089     05  WS-HEX-BYTE             PIC X.
000090 01  WS-CHARCD REDEFINES WS-HEX-WORK PIC S9(4)  COMP.
000091*EXEC SQL
000092*   INCLUDE SQLDA
000093*END-EXEC
000094
000097*EXEC SQL
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
000098
000100 EXEC SQL
          BEGIN DECLARE SECTION
000101 END-EXEC
000102
000103 01  SQLCMD                      PIC X(1024).
000104 01  SVR                         PIC X(32).
000105 01  USR                         PIC X(32).
000106 01  PASS                        PIC X(32).
000107 01  USR-PASS                    PIC X(64).
000108 01  WS-CHARCD-A                 PIC S9(4)   COMP VALUE +65.
000109
000110 01  WS-SQL-DATA.
000111     05  WS-CYCLE-DATE           PIC X(10).
000112     05  WS-NEXT-BUS-DT          PIC X(10).
000113     05  WS-LOOKUPID             PIC X(4).
000114     05  WS-LOOKUPNAME           PIC X(4).
000115     05  WS-LOOKUP-VALUE         PIC X(100).
000116     05  WS-CARRIER              PIC X.
000117     05  WS-GROUP                PIC X(6).
000118     05  WS-STATE                PIC XX.
000119     05  WS-ACCOUNT              PIC X(10).
000120     05  WS-EFF-DT               PIC XX.
000121     05  WS-CERT-NO              PIC X(10).
000122     05  WS-CERT-NO-SUF          PIC X(01).
000123
000125 EXEC SQL
          END DECLARE SECTION
000126 END-EXEC
000127
000128 01  P pointer.
000129 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000130 01  KIXHOST                     pic x(9) value Z"HOSTNAME".
000131 01  var-ptr pointer.
000132 01  env-var-len                 pic 9(4)  binary.
000133 01  rc                          pic 9(9)  binary.
000134
000135 01  WS-KIXSYS.
000136     05  WS-KIX-FIL1             PIC X(10).
000137     05  WS-KIX-APPS             PIC X(10).
000138     05  WS-KIX-ENV              PIC X(10).
000139     05  WS-KIX-MYENV            PIC X(10).
000140     05  WS-KIX-SYS              PIC X(10).
000141 01  WS-KIXHOST                  PIC X(10).
000142
000143 01  WS-COMM-LENGTH          PIC S9(4) COMP VALUE +1900.
000144
000145 01  STANDARD-AREAS.
000146     12  WS-RESPONSE             PIC S9(8)   COMP.
000147         88  RESP-NORMAL              VALUE +00.
000148         88  RESP-ERROR               VALUE +01.
000149         88  RESP-NOTFND              VALUE +13.
000150         88  RESP-NOTOPEN             VALUE +19.
000151         88  RESP-ENDFILE             VALUE +20.
000152     12  GETMAIN-SPACE       PIC X       VALUE SPACE.
000153     12  EL630B              PIC X(8)    VALUE 'EL630B'.
000154     12  EL630C              PIC X(8)    VALUE 'EL630C'.
000155     12  MAPSET-EL6301S      PIC X(8)    VALUE 'EL6301S'.
000156     12  TRANS-EXA6          PIC X(4)    VALUE 'EXA6'.
000157     12  THIS-PGM            PIC X(8)    VALUE 'EL6301'.
000158     12  PGM-NAME            PIC X(8).
000159     12  TIME-IN             PIC S9(7).
000160     12  TIME-OUT-R  REDEFINES TIME-IN.
000161         16  FILLER          PIC X.
000162         16  TIME-OUT        PIC 99V99.
000163         16  FILLER          PIC XX.
000164     12  LINK-EL001          PIC X(8)    VALUE 'EL001'.
000165     12  LINK-EL004          PIC X(8)    VALUE 'EL004'.
000166     12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.
000167     12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.
000168     12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
000169     12  XCTL-EL630          PIC X(8)    VALUE 'EL630'.
000170     12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
000171     12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.
000172     12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.
000173     12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.
000174     12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.
000175     12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.
000176     12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.
000177     12  WS-TERM-IN-DAYS-SW  PIC X.
000178         88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.
000179
000180     EJECT
000181 01  ERROR-MESSAGES.
000182     12  ER-0000                 PIC X(4)  VALUE '0000'.
000183     12  ER-0004                 PIC X(4)  VALUE '0004'.
000184     12  ER-0008                 PIC X(4)  VALUE '0008'.
000185     12  ER-0029                 PIC X(4)  VALUE '0029'.
000186     12  ER-0070                 PIC X(4)  VALUE '0070'.
000187     12  ER-0582                 PIC X(4)  VALUE '0582'.
000188     12  ER-1923                 PIC X(4)  VALUE '1923'.
000189     12  ER-2049                 PIC X(4)  VALUE '2049'.
000190     12  ER-2119                 PIC X(4)  VALUE '2119'.
000191     12  ER-2212                 PIC X(4)  VALUE '2212'.
000192     12  ER-2217                 PIC X(4)  VALUE '2217'.
000193     12  ER-2218                 PIC X(4)  VALUE '2218'.
000194     12  ER-2200                 PIC X(4)  VALUE '2200'.
000195     12  ER-2209                 PIC X(4)  VALUE '2209'.
000196     12  ER-2220                 PIC X(4)  VALUE '2220'.
000197     12  ER-2222                 PIC X(4)  VALUE '2222'.
000198     12  ER-2223                 PIC X(4)  VALUE '2223'.
000199     12  ER-2224                 PIC X(4)  VALUE '2224'.
000200     12  ER-2226                 PIC X(4)  VALUE '2226'.
000201     12  ER-2227                 PIC X(4)  VALUE '2227'.
000202     12  ER-2228                 PIC X(4)  VALUE '2228'.
000203     12  ER-2240                 PIC X(4)  VALUE '2240'.
000204     12  ER-2241                 PIC X(4)  VALUE '2241'.
000205     12  ER-2247                 PIC X(4)  VALUE '2247'.
000206     12  ER-2423                 PIC X(4)  VALUE '2423'.
000207     12  ER-2424                 PIC X(4)  VALUE '2424'.
000208     12  ER-2425                 PIC X(4)  VALUE '2425'.
000209     12  ER-2426                 PIC X(4)  VALUE '2426'.
000210     12  ER-2427                 PIC X(4)  VALUE '2427'.
000211     12  ER-2428                 PIC X(4)  VALUE '2428'.
000212     12  ER-2431                 PIC X(4)  VALUE '2431'.
000213     12  ER-2433                 PIC X(4)  VALUE '2433'.
000214     12  ER-2437                 PIC X(4)  VALUE '2437'.
000215     12  ER-2429                 PIC X(4)  VALUE '2429'.
000216     12  ER-2442                 PIC X(4)  VALUE '2442'.
000217     12  ER-2471                 PIC X(4)  VALUE '2471'.
000218     12  ER-2526                 PIC X(4)  VALUE '2526'.
000219     12  ER-2529                 PIC X(4)  VALUE '2529'.
000220     12  ER-2531                 PIC X(4)  VALUE '2531'.
000221     12  ER-2532                 PIC X(4)  VALUE '2532'.
000222     12  ER-2541                 PIC X(4)  VALUE '2541'.
000223     12  ER-2542                 PIC X(4)  VALUE '2542'.
000224     12  ER-2589                 PIC X(4)  VALUE '2589'.
000225     12  ER-2591                 PIC X(4)  VALUE '2591'.
000226     12  ER-2592                 PIC X(4)  VALUE '2592'.
000227     12  ER-2593                 PIC X(4)  VALUE '2593'.
000228     12  ER-2594                 PIC X(4)  VALUE '2594'.
000229     12  ER-2629                 PIC X(4)  VALUE '2629'.
000230     12  ER-2630                 PIC X(4)  VALUE '2630'.
000231     12  ER-2635                 PIC X(4)  VALUE '2635'.
000232     12  ER-2636                 PIC X(4)  VALUE '2636'.
000233     12  ER-2651                 PIC X(4)  VALUE '2651'.
000234     12  ER-2670                 PIC X(4)  VALUE '2670'.
000235     12  ER-2683                 PIC X(4)  VALUE '2683'.
000236     12  ER-2700                 PIC X(4)  VALUE '2700'.
000237     12  ER-2701                 PIC X(4)  VALUE '2701'.
000238     12  ER-2702                 PIC X(4)  VALUE '2702'.
000239     12  ER-2901                 PIC X(4)  VALUE '2901'.
000240     12  ER-2963                 PIC X(4)  VALUE '2963'.
000241     12  ER-2964                 PIC X(4)  VALUE '2964'.
000242     12  ER-3166                 PIC X(4)  VALUE '3166'.
000243     12  ER-3825                 PIC X(4)  VALUE '3825'.
000244     12  ER-3826                 PIC X(4)  VALUE '3826'.
000245     12  ER-7400                 PIC X(4)  VALUE '7400'.
000246     12  ER-7403                 PIC X(4)  VALUE '7403'.
000247     12  ER-7404                 PIC X(4)  VALUE '7404'.
000248     12  ER-7405                 PIC X(4)  VALUE '7405'.
000249     12  ER-7423                 PIC X(4)  VALUE '7423'.
000250     12  ER-7424                 PIC X(4)  VALUE '7424'.
000251     12  ER-7530                 PIC X(4)  VALUE '7530'.
000252     12  ER-7632                 PIC X(4)  VALUE '7632'.
000253     12  ER-7630                 PIC X(4)  VALUE '7630'.
000254     12  ER-7631                 PIC X(4)  VALUE '7631'.
000255     12  ER-7633                 PIC X(4)  VALUE '7633'.
000256     12  ER-7997                 PIC X(4)  VALUE '7997'.
000257     12  ER-7998                 PIC X(4)  VALUE '7998'.
000258     12  ER-9841                 PIC X(4)  VALUE '9841'.
000259     12  ER-9999                 PIC X(4)  VALUE '9999'.
000260
000261     EJECT
000262
000263 01  ACCESS-KEYS.
000264     12  ERPNDB-KEY.
000265         16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.
000266         16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
000267         16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
000268         16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
000269
000270     12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.
000271     12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.
000272
000273     12  ELCNTL-KEY.
000274         16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.
000275         16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.
000276         16  ELCNTL-ACCESS.
000277             20  FILLER              PIC XX.
000278             20  ELCNTL-HI-BEN       PIC XX.
000279         16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.
000280
000281     12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.
000282     12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.
000283     12  ERACCT-KEY.
000284         16  ERACCT-COMPANY-CD          PIC  X.
000285         16  ERACCT-CARRIER             PIC  X.
000286         16  ERACCT-GROUPING            PIC  X(6).
000287         16  ERACCT-STATE               PIC  XX.
000288         16  ERACCT-ACCOUNT             PIC  X(10).
000289         16  ERACCT-EXP-DT.
000290             20  ERACCT-DT              PIC  XX.
000291             20  ERACCT-FILL            PIC  X(4).
000292
000293     12  ELCERT-KEY.
000294         16  ELCERT-COMPANY-CD       PIC X.
000295         16  ELCERT-CARRIER          PIC X.
000296         16  ELCERT-GROUPING         PIC X(6).
000297         16  ELCERT-STATE            PIC XX.
000298         16  ELCERT-ACCOUNT          PIC X(10).
000299         16  ELCERT-CERT-EFF-DT      PIC XX.
000300         16  ELCERT-CERT-NO.
000301             20  ELCERT-CERT-PRIME   PIC X(10).
000302             20  ELCERT-CERT-SFX     PIC X.
000303
000304     12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.
000305     12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.
000306
000307     12  ERPNDM-KEY.
000308         16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.
000309         16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
000310         16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
000311         16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
000312
000313*    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.
000314     12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.
000315*    12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.
000316     12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +397.
000317
000318     EJECT
000319 01  WORK-AREA.
000320     12  DEEDIT-FIELD            PIC X(15).
000321     12  FILLER REDEFINES DEEDIT-FIELD.
000322         16  FILLER              PIC X(4).
000323         16  DEEDIT-FIELD-X11    PIC X(11).
000324     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).
000325     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.
000326     12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).
000327     12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
000328     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
000329
000330     12  WS-SUB                  PIC S9(4) VALUE +0  COMP.
000331     12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.
000332     12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.
000333     12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.
000334     12  WS-ACCT-SUB             PIC S9(4) VALUE +0  COMP.
000335     12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.
000336     12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.
000337     12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.
000338     12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.
000339     12  FILLER REDEFINES WS-WORK-BIN-RED.
000340         16  FILLER              PIC X(02).
000341         16  WS-WORK-BIN-DT      PIC X(02).
000342
000343     12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.
000344     12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.
000345         16  WS-CALC-TERM-WHOLE  PIC S999.
000346         16  WS-CALC-TERM-REMAIN PIC SV9(5).
000347
000348     12  ERROR-SW                PIC X     VALUE SPACE.
000349         88  NO-ERROR                VALUE SPACE.
000350         88  ERRORS                  VALUE 'Y'.
000351         88  WS-COVERAGE-PRESENT     VALUE 'Y'.
000352
000353     12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.
000354         88  WS-DATA-NOT-KEYED       VALUE SPACE.
000355         88  WS-DATA-KEYED           VALUE 'Y'.
000356
000357     12  WS-EDITED-LF-CODE       PIC XX   VALUE SPACES.
000358     12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.
000359     12  ws-lf-earnings-calc     pic x    value spaces.
000360
000361     12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.
000362     12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.
000363
000364     12  WS-BEN-CD               PIC XX   VALUE SPACES.
000365
000366     12  WS-ENTRY-CODE           PIC X     VALUE SPACE.
000367         88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'
000368                                         'M' 'D' 'V' 'U'.
000369
000370     12  WS-FORCE-CODE           PIC X     VALUE SPACE.
000371         88  WS-FORCE-CODE-VALID   VALUE ' ' 'A' 'D'.
000372
000373     12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.
000374
000375     12  WS-MODE-CODE            PIC X     VALUE SPACE.
000376         88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B' 'T'.
000377
000378     12  WS-SKIP-CODE            PIC X     VALUE SPACE.
000379         88 WS-SKIP-CODE-VALID     VALUE ' ' 'A' 'X' '0' THRU '9'.
000380
000381     12  WS-KIND                 PIC XX    VALUE SPACE.
000382         88 WS-KIND-LF             VALUE 'LF'.
000383         88 WS-KIND-AH             VALUE 'AH'.
000384         88 WS-KIND-PR             VALUE 'PR'.
000385         88 WS-KIND-UE             VALUE 'UE'.
000386         88 WS-KIND-DI             VALUE 'DI'.
000387         88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.
000388
000389     12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.
000390
000391     12  WS-EDIT-CODE               PIC X(4)  VALUE SPACES.
000392
000393     12  WS-SAVE-INPUT-FIELDS.
000394         16  WS-BAGE                 PIC 99       VALUE ZERO.
000395         16  WS-BJNT-AGE             PIC 99       VALUE ZERO.
000396         16  WS-BDAYS                PIC 999      VALUE ZERO.
000397         16  WS-BLN-TERM             PIC 999      VALUE ZERO.
000398         16  WS-BFREQ                PIC 99       VALUE ZERO.
000399         16  WS-BPHONE               PIC 9(12)    VALUE  0 COMP-3.
000400         16  WS-BAPR                 PIC 99V9(4) VALUE ZEROS.
000401         16  FILLER REDEFINES WS-BAPR.
000402             20  WS-APR-WHOLE-NUM    PIC 99.
000403             20  WS-APR-DEC          PIC 9999.
000404         16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.
000405         16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.
000406         16  WS-BLIVES               PIC 9(7)      COMP-3.
000407
000408         16  WS-B-COVERAGE.
000409             20  WS-BTERM1            PIC 999       COMP-3.
000410             20  WS-BBEN1             PIC S9(10)V99 COMP-3.
000411             20  WS-BALT-BEN1     PIC S9(10)V99 COMP-3.
000412             20  WS-BPREM1        PIC S9(10)V99 COMP-3.
000413             20  WS-BALT-PREM1    PIC S9(7)V99 COMP-3.
000414             20  WS-BTERM2            PIC 999       COMP-3.
000415             20  WS-BCRIT-PERD2       PIC 99        COMP-3.
000416             20  WS-BBEN2             PIC S9(10)V99 COMP-3.
000417             20  WS-BPREM2        PIC S9(10)V99 COMP-3.
000418             20  WS-BALT-BEN2     PIC S9(10)V99 COMP-3.
000419             20  WS-BALT-PREM2    PIC S9(7)V99 COMP-3.
000420
000421         16  WS-C-FIELDS   OCCURS 4 TIMES.
000422             20  WS-CLIVES      PIC 9(3)          COMP-3.
000423             20  WS-CREFUND1    PIC S9(7)V99      COMP-3.
000424             20  WS-CREFUND2    PIC S9(7)V99      COMP-3.
000425             20  WS-CAN-REA     PIC X.
000426
000427     12  WS-CONVERTED-BIRTH      OCCURS 2 PIC XX.
000428     12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.
000429     12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.
000430     12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.
000431     12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.
000432         16  WS-CONVERTED-CANDT1 PIC XX.
000433         16  WS-CONVERTED-CANDT2 PIC XX.
000434     12  WS-CONVERTED-CAN-EFF-DATES OCCURS 4 TIMES.
000435         16  WS-CONVERTED-CAN-EFF-DT PIC XX.
000436
000437     12  WS-FIRST-NAME.
000438         16  WS-1ST-INIT         PIC X.
000439         16  FILLER              PIC X(9).
000440
000441     12  WS-INITIALS.
000442         16  WS-INITIAL-1        PIC X.
000443         16  WS-INITIAL-2        PIC X.
000444
000445     12  WS-ZIP-CODE.
000446         16  WS-ZIP-1            PIC X.
000447             88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
000448         16  WS-ZIP-2-3          PIC XX.
000449         16  WS-ZIP-4            PIC X.
000450         16  WS-ZIP-5            PIC X.
000451         16  WS-ZIP-6            PIC X.
000452         16  FILLER              PIC X(4).
000453     12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
000454         16  WS-ZIP-AM-1-CODE    PIC X(5).
000455         16  WS-ZIP-AM-1-PLUS4   PIC X(4).
000456         16  FILLER              PIC X.
000457     12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
000458         16  WS-ZIP-AM-2-CODE    PIC X(5).
000459         16  WS-ZIP-AM-2-DASH    PIC X.
000460         16  WS-ZIP-AM-2-PLUS4   PIC X(4).
000461     12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
000462         16  WS-ZIP-CAN-1-POST1  PIC XXX.
000463         16  WS-ZIP-CAN-1-POST2  PIC XXX.
000464         16  FILLER              PIC X(4).
000465     12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
000466         16  WS-ZIP-CAN-2-POST1  PIC XXX.
000467         16  FILLER              PIC X.
000468         16  WS-ZIP-CAN-2-POST2  PIC XXX.
000469         16  FILLER              PIC XXX.
000470     12  WS-MEMBER-NO            PIC X(12).
000471     12  FILLER  REDEFINES  WS-MEMBER-NO.
000472         16  WS-MEMBER-NO-1-8    PIC 9(8).
000473         16  FILLER              PIC X(4).
000474     12  WS-I-MICRO-NO           PIC S9(9)        COMP-3.
000475
000476     EJECT
000477
000478 01  WS-DATE-AREA.
000479     12  WS-COMPARE-CURRENT-DT.
000480         16  FILLER              PIC X(4)    VALUE SPACES.
000481         16  WS-COMPARE-CURR-YR  PIC X(2)    VALUE SPACES.
000482     12  WS-SAVE-BIRTH-DATE.
000483         16  FILLER              PIC X(4)   VALUE SPACES.
000484         16  WS-SAVE-BIRTH-YR    PIC X(2)   VALUE SPACES.
000485
000486 01  CLASIC-WARNING.
000487     12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.
000488     12  WARNING-TEXT.
000489         16  FILLER                  PIC X(80)  VALUE
000490             'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.
000491         16  FILLER                  PIC X(44)  VALUE
000492             'CONTACT PAUL @ CSO FOR FURTHER INFORMATION'.
000493
000494     EJECT
000495
000496*    COPY ELCDATE.
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
000497
000498     EJECT
000499*    COPY ELCLOGOF.
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
000500
000501     EJECT
000502*    COPY ELCATTR.
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
000503
000504     EJECT
000505*    COPY ELCEMIB.
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
000506
000507     EJECT
000508*    COPY ELCINTF.
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
000509*    COPY ELC630PI.
      *>>((file: ELC630PI))
000001******************************************************************
000002*                                                                *
000003*                            ELC630PI                            *
000004*                            VMOD=2.014                          *
000005*                                                                *
000006* - PI-PROGRAM-WORK-AREA FOR THE DATA-ENTRY SUB-SYSTEM -         *
000007*                                                                *
000008*    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
000009*                                                                *
000010*               EL630 - EL6301 - EL6302                          *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
000020* 030310  CR2009031200002  PEMA  OPEN LOAN OFFICER FIELD
000021* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000022******************************************************************
000023
000024     12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
000025         16  PI-AM-NAME                  PIC X(30).
000026         16  PI-MAP-NAME                 PIC X(8).
000027         16  PI-BATCH-AMOUNTS    COMP-3.
000028             20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
000029             20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
000030             20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
000031             20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
000032             20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
000033             20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
000034             20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
000035             20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
000036             20  PI-ISS-CNT-REMITTED     PIC S9(5).
000037             20  PI-ISS-CNT-ENTERED      PIC S9(5).
000038             20  PI-CAN-CNT-REMITTED     PIC S9(5).
000039             20  PI-CAN-CNT-ENTERED      PIC S9(5).
000040         16  PI-MAINT-FUNC               PIC X.
000041         16  PI-ERROR-SW                 PIC X.
000042             88  PI-DATA-ERRORS              VALUE 'Y'.
000043         16  PI-UPDATE-SW                PIC X.
000044             88  PI-DATA-UPDATED             VALUE 'Y'.
000045         16  PI-DISPLAY-SW               PIC X.
000046             88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
000047         16  PI-SAVE-CALLING-PGM         PIC X(8).
000048         16  PI-LAST-SEQ-NO-ADDED        PIC S9(4) COMP.
000049         16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4) COMP.
000050         16  PI-SAV-CARRIER              PIC X.
000051         16  PI-SAV-GROUPING             PIC X(6).
000052         16  PI-SAV-STATE                PIC XX.
000053         16  PI-SAV-ACCOUNT              PIC X(10).
000054         16  PI-SAV-CERT-EFF-DT          PIC XX.
000055         16  PI-SAV-CERT-NO.
000056             20  PI-SAV-CERT-PRIME       PIC X(14).
000057             20  PI-SAV-CERT-SFX         PIC X.
000058         16  PI-PYAJ-REFERENCE REDEFINES PI-SAV-CERT-NO.
000059             20  PI-SAV-PYAJ-REFERENCE   PIC X(12).
000060             20  FILLER                  PIC X(3).
000061         16  PI-SAV-ENDING-ERPNDB-KEY.
000062             20  PI-SAV-COMP-CD          PIC X.
000063             20  PI-SAV-ENTRY-BATCH      PIC X(6).
000064             20  PI-SAV-BATCH-SEQ        PIC S9(4) COMP.
000065             20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4) COMP.
000066         16  PI-SAV-REFERENCE            PIC X(12).
000067         16  PI-SAV-FULL-CONTROL.
000068             20  PI-SAV-FC-CARRIER       PIC X.
000069             20  PI-SAV-FC-GROUPING      PIC X(6).
000070             20  PI-SAV-FC-STATE         PIC XX.
000071         16  PI-VERIFY-DELETE-SW         PIC X.
000072             88  PI-DELETE-IS-OK             VALUE 'Y'.
000073         16  PI-EL630-FIRST-TIME-SW      PIC X.
000074             88  PI-EL630-FIRST-TIME         VALUE SPACE.
000075         16  PI-CREDIT-EDIT-CONTROLS.
000076             20  PI-MIN-PREMIUM          PIC S9(3)V99  COMP-3.
000077             20  PI-MIN-AGE              PIC 99.
000078             20  PI-DEFAULT-AGE          PIC 99.
000079             20  PI-MIN-TERM             PIC S9(3)     COMP-3.
000080             20  PI-MAX-TERM             PIC S9(3)     COMP-3.
000081             20  PI-DEFAULT-SEX          PIC X.
000082             20  PI-JOINT-AGE-INPUT      PIC X.
000083                 88 PI-JOINT-AGE-IS-INPUT       VALUE '1'.
000084             20  PI-BIRTH-DATE-INPUT     PIC X.
000085                 88 PI-BIRTH-DATE-IS-INPUT      VALUE '1'.
000086         16  PI-KEYED-SWITCHES.
000087             20  PI-ISS-SUFFIX-KEYED-SW  PIC X.
000088                 88  PI-ISS-SUFFIX-KEYED     VALUE 'Y'.
000089             20  PI-CAN-SUFFIX-KEYED-SW  PIC X.
000090                 88  PI-CAN-SUFFIX-KEYED     VALUE 'Y'.
000091             20  PI-IG-KEYED-SW          PIC X.
000092                 88  PI-IG-KEYED             VALUE 'Y'.
000093             20  PI-APR-KEYED-SW         PIC X.
000094                 88  PI-APR-KEYED            VALUE 'Y'.
000095*            20  PI-FREQ-KEYED-SW        PIC X.
000096*                88  PI-FREQ-KEYED           VALUE 'Y'.
000097             20  PI-VIN-KEYED-SW         PIC X.
000098                 88  PI-VIN-KEYED            VALUE 'Y'.
000099             20  PI-SIG-KEYED-SW         PIC X.
000100                 88  PI-SIG-KEYED            VALUE 'Y'.
000101             20  PI-LFRT-KEYED-SW        PIC X.
000102                 88  PI-LFRT-KEYED           VALUE 'Y'.
000103             20  PI-AHRT-KEYED-SW        PIC X.
000104                 88  PI-AHRT-KEYED           VALUE 'Y'.
000105             20  PI-SSNUM-KEYED-SW       PIC X.
000106                 88  PI-SSNUM-KEYED          VALUE 'Y'.
000107             20  PI-JNT-SSNUM-KEYED-SW   PIC X.
000108                 88  PI-JNT-SSNUM-KEYED      VALUE 'Y'.
000109             20  PI-MEMBER-KEYED-SW      PIC X.
000110                 88  PI-MEMBER-KEYED         VALUE 'Y'.
000111             20  PI-MODE-KEYED-SW        PIC X.
000112                 88  PI-MODE-KEYED           VALUE 'Y'.
000113             20  PI-PMTS-KEYED-SW        PIC X.
000114                 88  PI-PMTS-KEYED           VALUE 'Y'.
000115             20  PI-LN-OFFICER-KEYED-SW  PIC X.
000116                 88  PI-LN-OFFICER-KEYED     VALUE 'Y'.
000117             20  PI-ENTRY-KEYED-SW       PIC X.
000118                 88  PI-ENTRY-KEYED          VALUE 'Y'.
000119             20  PI-FORCE-KEYED-SW       PIC X.
000120                 88  PI-FORCE-KEYED          VALUE 'Y'.
000121             20  PI-RINCD-KEYED-SW       PIC X.
000122                 88  PI-RINCD-KEYED          VALUE 'Y'.
000123             20  PI-BILLCD-KEYED-SW      PIC X.
000124                 88  PI-BILLCD-KEYED         VALUE 'Y'.
000125             20  PI-RTCLS-KEYED-SW       PIC X.
000126                 88  PI-RTCLS-KEYED          VALUE 'Y'.
000127             20  PI-LNTRM-KEYED-SW       PIC X.
000128                 88  PI-LNTRM-KEYED          VALUE 'Y'.
000129             20  PI-EXPIR-KEYED-SW       PIC X.
000130                 88  PI-EXPIR-KEYED          VALUE 'Y'.
000131             20  PI-PMT-KEYED-SW         PIC X.
000132                 88  PI-PMT-KEYED            VALUE 'Y'.
000133             20  PI-1ST-PMT-KEYED-SW     PIC X.
000134                 88  PI-1ST-PMT-KEYED        VALUE 'Y'.
000135             20  PI-DAYS-KEYED-SW        PIC X.
000136                 88  PI-DAYS-KEYED           VALUE 'Y'.
000137             20  PI-SKPCD-KEYED-SW       PIC X.
000138                 88  PI-SKPCD-KEYED          VALUE 'Y'.
000139             20  PI-JNT-AGE-KEYED-SW     PIC X.
000140                 88  PI-JNT-AGE-KEYED        VALUE 'Y'.
000141             20  PI-JNT-NAME-KEYED-SW    PIC X.
000142                 88  PI-JNT-NAME-KEYED       VALUE 'Y'.
000143             20  PI-ISS-LIVES-KEYED-SW   PIC X.
000144                 88  PI-ISS-LIVES-KEYED      VALUE 'Y'.
000145             20  PI-CAN-LIVES-KEYED-SW   PIC X.
000146                 88  PI-CAN-LIVES-KEYED      VALUE 'Y'.
000147             20  PI-PAYEE-KEYED-SW       PIC X.
000148                 88  PI-PAYEE-KEYED          VALUE 'Y'.
000149             20  PI-CHK-REQ-KEYED-SW     PIC X.
000150                 88  PI-CHK-REQ-KEYED        VALUE 'Y'.
000151             20  PI-ZIP4-KEYED-SW        PIC X.
000152                 88  PI-ZIP4-KEYED           VALUE 'Y'.
000153             20  PI-POLICY-KEYED-SW      PIC X.
000154                 88  PI-POLICY-KEYED         VALUE 'Y'.
000155             20  PI-EXPIRE-KEYED-SW      PIC X.
000156                 88  PI-EXPIRE-KEYED         VALUE 'Y'.
000157             20  PI-CRIT-PERD-KEYED-SW    PIC X.
000158                 88  PI-CRIT-PERD-KEYED      VALUE 'Y'.
000159             20  PI-BENEFICIARY-KEYED-SW PIC X.
000160                 88  PI-BENEFICIARY-KEYED    VALUE 'Y'.
000161             20  PI-PHONE-KEYED-SW       PIC X.
000162                 88  PI-PHONE-KEYED          VALUE 'Y'.
000163             20  PI-ALT-BEN-KEYED-SW     PIC X.
000164                 88  PI-ALT-BEN-KEYED        VALUE 'Y'.
000165             20  PI-ALT-PREM-KEYED-SW    PIC X.
000166                 88  PI-ALT-PREM-KEYED       VALUE 'Y'.
000167             20  PI-REFUND-MTHD-KEYED-SW PIC X.
000168                 88  PI-REFUND-MTHD-KEYED    VALUE 'Y'.
000169         16  PI-ACCT-LOW-EFF-DT          PIC XX.
000170         16  PI-ACCT-HIGH-EXP-DT         PIC XX.
000171         16  PI-BATCH-EOF-SW             PIC X.
000172             88  PI-BATCH-EOF                VALUE 'Y'.
000173         16  PI-NB-MONTH-END-DT          PIC XX.
000174         16  PI-ISSUE-ADDED-SW           PIC X.
000175             88  PI-ISSUE-ADDED              VALUE 'Y'.
000176         16  PI-BROWSE-SW                PIC X.
000177             88  PI-BROWSE                   VALUE 'Y'.
000178         16  PI-ACCT-AGENT-ERROR-SW      PIC X.
000179             88  PI-ACCT-AGENT-ERROR         VALUE 'Y'.
000180         16  PI-FIN-RESP-ERROR-SW        PIC X.
000181             88  PI-FIN-RESP-ERROR           VALUE 'Y'.
000182         16  PI-CAN-REA-KEYED-SW         PIC X.
000183             88  PI-CAN-REA-KEYED            VALUE 'Y'.
000184         16  PI-AM-EDIT-LOAN-OFC         PIC X.
000185         16  PI-AM-ADDR1                 PIC X(30).
000186         16  PI-AM-ADDR2                 PIC X(30).
000187         16  PI-AM-CITYST.
000188             20  PI-AM-CITY              PIC X(28).
000189             20  PI-AM-STATE             PIC XX.
000190         16  PI-AM-ZIP                   PIC X(9).
000191         16  FILLER                      PIC X(290).
000192*        16  FILLER                      PIC X(390).
000193     12  PI-MISC.
000194         16  PI-ACCT-DATE-RANGES OCCURS 32 TIMES.
000195             20  PI-ACCT-EFF-DT          PIC XX.
000196             20  PI-ACCT-EXP-DT          PIC XX.
000197             20  PI-REMIT-AGENT          PIC X(10).
000198             20  PI-ACCT-AGENT           PIC X(10).
000199         16  PI-ACCOUNT-AGENT            PIC X(10).
000200         16  PI-FIN-RESP                 PIC X(10).
000201         16  PI-SUMMARY-CODE             PIC X(6).
000202         16  PI-SUB                      PIC S9(4) COMP.
000203         16  PI-COMP-CARRIER             PIC X.
000204         16  PI-COMP-GROUPING            PIC X(6).
000205         16  PI-ACCT-AGENT-PROCESSED-SW  PIC X.
000206             88  PI-ACCT-AGENT-PROCESSED     VALUE 'Y'.
000207         16  PI-CLEAR-ERROR-SW           PIC X.
000208             88  PI-CLEAR-ERROR              VALUE 'Y'.
000209         16  PI-AGE-KEYED-SW             PIC X.
000210             88  PI-AGE-KEYED                VALUE 'Y'.
000211         16  PI-BIRTHDT-KEYED-SW         PIC X.
000212             88  PI-BIRTHDT-KEYED            VALUE 'Y'.
000213         16  PI-RECEIVED-DT              PIC XX.
000214         16  PI-CSR-ID                   PIC X(4).
000215
000216     EJECT
      *<<((file: ELC630PI))
000510     EJECT
000511*    COPY ELCJPFX.
      *>>((file: ELCJPFX))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCJPFX.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
000009*                                                                *
000010*     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
000011*     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
000012*        ELCNTL - CONTROL FILE                                   *
000013*        ELMSTR - CLAIM MASTERS                                  *
000014*        ELTRLR - ACTIVITY TRAILERS                              *
000015*        ELCHKQ - CHECK QUE                                      *
000016******************************************************************
000017 01  JOURNAL-RECORD.
000018     12  jp-date                     pic s9(5) comp-3.
000019     12  jp-time                     pic s9(7) comp-3.
000020     12  JP-USER-ID                  PIC X(4).
000021     12  JP-FILE-ID                  PIC X(8).
000022     12  JP-PROGRAM-ID               PIC X(8).
000023     12  JP-RECORD-TYPE              PIC X.
000024         88 JP-ADD              VALUE 'A'.
000025         88 JP-BEFORE-CHANGE    VALUE 'B'.
000026         88 JP-AFTER-CHANGE     VALUE 'C'.
000027         88 JP-DELETE           VALUE 'D'.
000028         88 JP-GENERIC-DELETE   VALUE 'G'.
000029         88 JP-KEY-CHG-DELETE   VALUE 'K'.
000030         88 JP-KEY-CHG-ADD      VALUE 'N'.
000031     12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
000032     12  JP-RECORD-AREA
000033
000034
      *<<((file: ELCJPFX))
000512                             PIC X(608).
000513
000514     EJECT
000515*    COPY ELCAID.
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
000516
000517 01  FILLER    REDEFINES DFHAID.
000518     12  FILLER              PIC X(8).
000519     12  PF-VALUES           PIC X       OCCURS 2.
000520
000521     EJECT
000522*    COPY EL6301S.
      *>>((file: EL6301S))
000001 01  EL630BI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  BDATEL PIC S9(0004) COMP.
000005     05  BDATEF PIC  X(0001).
000006     05  FILLER REDEFINES BDATEF.
000007         10  BDATEA PIC  X(0001).
000008     05  BDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  BTIMEL PIC S9(0004) COMP.
000011     05  BTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES BTIMEF.
000013         10  BTIMEA PIC  X(0001).
000014     05  BTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  BBATCHL PIC S9(0004) COMP.
000017     05  BBATCHF PIC  X(0001).
000018     05  FILLER REDEFINES BBATCHF.
000019         10  BBATCHA PIC  X(0001).
000020     05  BBATCHI PIC  X(0008).
000021*    -------------------------------
000022     05  BSEQL PIC S9(0004) COMP.
000023     05  BSEQF PIC  X(0001).
000024     05  FILLER REDEFINES BSEQF.
000025         10  BSEQA PIC  X(0001).
000026     05  BSEQI PIC  X(0004).
000027*    -------------------------------
000028     05  BMOENDL PIC S9(0004) COMP.
000029     05  BMOENDF PIC  X(0001).
000030     05  FILLER REDEFINES BMOENDF.
000031         10  BMOENDA PIC  X(0001).
000032     05  BMOENDI PIC  X(0008).
000033*    -------------------------------
000034     05  BACCTNML PIC S9(0004) COMP.
000035     05  BACCTNMF PIC  X(0001).
000036     05  FILLER REDEFINES BACCTNMF.
000037         10  BACCTNMA PIC  X(0001).
000038     05  BACCTNMI PIC  X(0030).
000039*    -------------------------------
000040     05  BCERTL PIC S9(0004) COMP.
000041     05  BCERTF PIC  X(0001).
000042     05  FILLER REDEFINES BCERTF.
000043         10  BCERTA PIC  X(0001).
000044     05  BCERTI PIC  X(0010).
000045*    -------------------------------
000046     05  BSFXL PIC S9(0004) COMP.
000047     05  BSFXF PIC  X(0001).
000048     05  FILLER REDEFINES BSFXF.
000049         10  BSFXA PIC  X(0001).
000050     05  BSFXI PIC  X(0001).
000051*    -------------------------------
000052     05  BVINHDL PIC S9(0004) COMP.
000053     05  BVINHDF PIC  X(0001).
000054     05  FILLER REDEFINES BVINHDF.
000055         10  BVINHDA PIC  X(0001).
000056     05  BVINHDI PIC  X(0005).
000057*    -------------------------------
000058     05  BVINL PIC S9(0004) COMP.
000059     05  BVINF PIC  X(0001).
000060     05  FILLER REDEFINES BVINF.
000061         10  BVINA PIC  X(0001).
000062     05  BVINI PIC  X(0017).
000063*    -------------------------------
000064     05  BLONOFCL PIC S9(0004) COMP.
000065     05  BLONOFCF PIC  X(0001).
000066     05  FILLER REDEFINES BLONOFCF.
000067         10  BLONOFCA PIC  X(0001).
000068     05  BLONOFCI PIC  X(0005).
000069*    -------------------------------
000070     05  B1STNML PIC S9(0004) COMP.
000071     05  B1STNMF PIC  X(0001).
000072     05  FILLER REDEFINES B1STNMF.
000073         10  B1STNMA PIC  X(0001).
000074     05  B1STNMI PIC  X(0010).
000075*    -------------------------------
000076     05  BINTL PIC S9(0004) COMP.
000077     05  BINTF PIC  X(0001).
000078     05  FILLER REDEFINES BINTF.
000079         10  BINTA PIC  X(0001).
000080     05  BINTI PIC  X(0001).
000081*    -------------------------------
000082     05  BLASTNML PIC S9(0004) COMP.
000083     05  BLASTNMF PIC  X(0001).
000084     05  FILLER REDEFINES BLASTNMF.
000085         10  BLASTNMA PIC  X(0001).
000086     05  BLASTNMI PIC  X(0015).
000087*    -------------------------------
000088     05  BAGEL PIC S9(0004) COMP.
000089     05  BAGEF PIC  X(0001).
000090     05  FILLER REDEFINES BAGEF.
000091         10  BAGEA PIC  X(0001).
000092     05  BAGEI PIC  X(0002).
000093*    -------------------------------
000094     05  BBIRTHL PIC S9(0004) COMP.
000095     05  BBIRTHF PIC  X(0001).
000096     05  FILLER REDEFINES BBIRTHF.
000097         10  BBIRTHA PIC  X(0001).
000098     05  BBIRTHI PIC  X(0006).
000099*    -------------------------------
000100     05  BJNT1STL PIC S9(0004) COMP.
000101     05  BJNT1STF PIC  X(0001).
000102     05  FILLER REDEFINES BJNT1STF.
000103         10  BJNT1STA PIC  X(0001).
000104     05  BJNT1STI PIC  X(0010).
000105*    -------------------------------
000106     05  BJNTINTL PIC S9(0004) COMP.
000107     05  BJNTINTF PIC  X(0001).
000108     05  FILLER REDEFINES BJNTINTF.
000109         10  BJNTINTA PIC  X(0001).
000110     05  BJNTINTI PIC  X(0001).
000111*    -------------------------------
000112     05  BJNTNAML PIC S9(0004) COMP.
000113     05  BJNTNAMF PIC  X(0001).
000114     05  FILLER REDEFINES BJNTNAMF.
000115         10  BJNTNAMA PIC  X(0001).
000116     05  BJNTNAMI PIC  X(0015).
000117*    -------------------------------
000118     05  BJNTAGEL PIC S9(0004) COMP.
000119     05  BJNTAGEF PIC  X(0001).
000120     05  FILLER REDEFINES BJNTAGEF.
000121         10  BJNTAGEA PIC  X(0001).
000122     05  BJNTAGEI PIC  X(0002).
000123*    -------------------------------
000124     05  BJNTDOBL PIC S9(0004) COMP.
000125     05  BJNTDOBF PIC  X(0001).
000126     05  FILLER REDEFINES BJNTDOBF.
000127         10  BJNTDOBA PIC  X(0001).
000128     05  BJNTDOBI PIC  X(0006).
000129*    -------------------------------
000130     05  BADDRS1L PIC S9(0004) COMP.
000131     05  BADDRS1F PIC  X(0001).
000132     05  FILLER REDEFINES BADDRS1F.
000133         10  BADDRS1A PIC  X(0001).
000134     05  BADDRS1I PIC  X(0030).
000135*    -------------------------------
000136     05  BADDRS2L PIC S9(0004) COMP.
000137     05  BADDRS2F PIC  X(0001).
000138     05  FILLER REDEFINES BADDRS2F.
000139         10  BADDRS2A PIC  X(0001).
000140     05  BADDRS2I PIC  X(0030).
000141*    -------------------------------
000142     05  BCITYL PIC S9(0004) COMP.
000143     05  BCITYF PIC  X(0001).
000144     05  FILLER REDEFINES BCITYF.
000145         10  BCITYA PIC  X(0001).
000146     05  BCITYI PIC  X(0028).
000147*    -------------------------------
000148     05  BSTATEL PIC S9(0004) COMP.
000149     05  BSTATEF PIC  X(0001).
000150     05  FILLER REDEFINES BSTATEF.
000151         10  BSTATEA PIC  X(0001).
000152     05  BSTATEI PIC  X(0002).
000153*    -------------------------------
000154     05  BZIPCDEL PIC S9(0004) COMP.
000155     05  BZIPCDEF PIC  X(0001).
000156     05  FILLER REDEFINES BZIPCDEF.
000157         10  BZIPCDEA PIC  X(0001).
000158     05  BZIPCDEI PIC  X(0010).
000159*    -------------------------------
000160     05  BNFICRYL PIC S9(0004) COMP.
000161     05  BNFICRYF PIC  X(0001).
000162     05  FILLER REDEFINES BNFICRYF.
000163         10  BNFICRYA PIC  X(0001).
000164     05  BNFICRYI PIC  X(0025).
000165*    -------------------------------
000166     05  BCADDR1L PIC S9(0004) COMP.
000167     05  BCADDR1F PIC  X(0001).
000168     05  FILLER REDEFINES BCADDR1F.
000169         10  BCADDR1A PIC  X(0001).
000170     05  BCADDR1I PIC  X(0030).
000171*    -------------------------------
000172     05  BCADDR2L PIC S9(0004) COMP.
000173     05  BCADDR2F PIC  X(0001).
000174     05  FILLER REDEFINES BCADDR2F.
000175         10  BCADDR2A PIC  X(0001).
000176     05  BCADDR2I PIC  X(0030).
000177*    -------------------------------
000178     05  BCCITYL PIC S9(0004) COMP.
000179     05  BCCITYF PIC  X(0001).
000180     05  FILLER REDEFINES BCCITYF.
000181         10  BCCITYA PIC  X(0001).
000182     05  BCCITYI PIC  X(0028).
000183*    -------------------------------
000184     05  BCSTATEL PIC S9(0004) COMP.
000185     05  BCSTATEF PIC  X(0001).
000186     05  FILLER REDEFINES BCSTATEF.
000187         10  BCSTATEA PIC  X(0001).
000188     05  BCSTATEI PIC  X(0002).
000189*    -------------------------------
000190     05  BCZIPCDL PIC S9(0004) COMP.
000191     05  BCZIPCDF PIC  X(0001).
000192     05  FILLER REDEFINES BCZIPCDF.
000193         10  BCZIPCDA PIC  X(0001).
000194     05  BCZIPCDI PIC  X(0010).
000195*    -------------------------------
000196     05  BEFFDTL PIC S9(0004) COMP.
000197     05  BEFFDTF PIC  X(0001).
000198     05  FILLER REDEFINES BEFFDTF.
000199         10  BEFFDTA PIC  X(0001).
000200     05  BEFFDTI PIC  X(0006).
000201*    -------------------------------
000202     05  B1STPMTL PIC S9(0004) COMP.
000203     05  B1STPMTF PIC  X(0001).
000204     05  FILLER REDEFINES B1STPMTF.
000205         10  B1STPMTA PIC  X(0001).
000206     05  B1STPMTI PIC  X(0006).
000207*    -------------------------------
000208     05  BAPRL PIC S9(0004) COMP.
000209     05  BAPRF PIC  X(0001).
000210     05  FILLER REDEFINES BAPRF.
000211         10  BAPRA PIC  X(0001).
000212     05  BAPRI PIC  X(0007).
000213*    -------------------------------
000214     05  BLNTRML PIC S9(0004) COMP.
000215     05  BLNTRMF PIC  X(0001).
000216     05  FILLER REDEFINES BLNTRMF.
000217         10  BLNTRMA PIC  X(0001).
000218     05  BLNTRMI PIC  X(0003).
000219*    -------------------------------
000220     05  BKIND1L PIC S9(0004) COMP.
000221     05  BKIND1F PIC  X(0001).
000222     05  FILLER REDEFINES BKIND1F.
000223         10  BKIND1A PIC  X(0001).
000224     05  BKIND1I PIC  X(0002).
000225*    -------------------------------
000226     05  BTYPE1L PIC S9(0004) COMP.
000227     05  BTYPE1F PIC  X(0001).
000228     05  FILLER REDEFINES BTYPE1F.
000229         10  BTYPE1A PIC  X(0001).
000230     05  BTYPE1I PIC  X(0003).
000231*    -------------------------------
000232     05  BTRM1L PIC S9(0004) COMP.
000233     05  BTRM1F PIC  X(0001).
000234     05  FILLER REDEFINES BTRM1F.
000235         10  BTRM1A PIC  X(0001).
000236     05  BTRM1I PIC  X(0003).
000237*    -------------------------------
000238     05  BBEN1L PIC S9(0004) COMP.
000239     05  BBEN1F PIC  X(0001).
000240     05  FILLER REDEFINES BBEN1F.
000241         10  BBEN1A PIC  X(0001).
000242     05  BBEN1I PIC  X(0012).
000243*    -------------------------------
000244     05  BPRM1L PIC S9(0004) COMP.
000245     05  BPRM1F PIC  X(0001).
000246     05  FILLER REDEFINES BPRM1F.
000247         10  BPRM1A PIC  X(0001).
000248     05  BPRM1I PIC  X(0011).
000249*    -------------------------------
000250     05  BALTBN1L PIC S9(0004) COMP.
000251     05  BALTBN1F PIC  X(0001).
000252     05  FILLER REDEFINES BALTBN1F.
000253         10  BALTBN1A PIC  X(0001).
000254     05  BALTBN1I PIC  X(0012).
000255*    -------------------------------
000256     05  BALTPM1L PIC S9(0004) COMP.
000257     05  BALTPM1F PIC  X(0001).
000258     05  FILLER REDEFINES BALTPM1F.
000259         10  BALTPM1A PIC  X(0001).
000260     05  BALTPM1I PIC  X(0009).
000261*    -------------------------------
000262     05  BKIND2L PIC S9(0004) COMP.
000263     05  BKIND2F PIC  X(0001).
000264     05  FILLER REDEFINES BKIND2F.
000265         10  BKIND2A PIC  X(0001).
000266     05  BKIND2I PIC  X(0002).
000267*    -------------------------------
000268     05  BTYPE2L PIC S9(0004) COMP.
000269     05  BTYPE2F PIC  X(0001).
000270     05  FILLER REDEFINES BTYPE2F.
000271         10  BTYPE2A PIC  X(0001).
000272     05  BTYPE2I PIC  X(0003).
000273*    -------------------------------
000274     05  BTRM2L PIC S9(0004) COMP.
000275     05  BTRM2F PIC  X(0001).
000276     05  FILLER REDEFINES BTRM2F.
000277         10  BTRM2A PIC  X(0001).
000278     05  BTRM2I PIC  X(0003).
000279*    -------------------------------
000280     05  BBEN2L PIC S9(0004) COMP.
000281     05  BBEN2F PIC  X(0001).
000282     05  FILLER REDEFINES BBEN2F.
000283         10  BBEN2A PIC  X(0001).
000284     05  BBEN2I PIC  X(0012).
000285*    -------------------------------
000286     05  BPRM2L PIC S9(0004) COMP.
000287     05  BPRM2F PIC  X(0001).
000288     05  FILLER REDEFINES BPRM2F.
000289         10  BPRM2A PIC  X(0001).
000290     05  BPRM2I PIC  X(0011).
000291*    -------------------------------
000292     05  BCP2L PIC S9(0004) COMP.
000293     05  BCP2F PIC  X(0001).
000294     05  FILLER REDEFINES BCP2F.
000295         10  BCP2A PIC  X(0001).
000296     05  BCP2I PIC  X(0002).
000297*    -------------------------------
000298     05  BALTBN2L PIC S9(0004) COMP.
000299     05  BALTBN2F PIC  X(0001).
000300     05  FILLER REDEFINES BALTBN2F.
000301         10  BALTBN2A PIC  X(0001).
000302     05  BALTBN2I PIC  X(0012).
000303*    -------------------------------
000304     05  BALTPM2L PIC S9(0004) COMP.
000305     05  BALTPM2F PIC  X(0001).
000306     05  FILLER REDEFINES BALTPM2F.
000307         10  BALTPM2A PIC  X(0001).
000308     05  BALTPM2I PIC  X(0009).
000309*    -------------------------------
000310     05  BERMSG1L PIC S9(0004) COMP.
000311     05  BERMSG1F PIC  X(0001).
000312     05  FILLER REDEFINES BERMSG1F.
000313         10  BERMSG1A PIC  X(0001).
000314     05  BERMSG1I PIC  X(0079).
000315*    -------------------------------
000316     05  BERMSG2L PIC S9(0004) COMP.
000317     05  BERMSG2F PIC  X(0001).
000318     05  FILLER REDEFINES BERMSG2F.
000319         10  BERMSG2A PIC  X(0001).
000320     05  BERMSG2I PIC  X(0079).
000321*    -------------------------------
000322     05  BPFENTRL PIC S9(0004) COMP.
000323     05  BPFENTRF PIC  X(0001).
000324     05  FILLER REDEFINES BPFENTRF.
000325         10  BPFENTRA PIC  X(0001).
000326     05  BPFENTRI PIC  9(2).
000327*    -------------------------------
000328     05  BDELHDGL PIC S9(0004) COMP.
000329     05  BDELHDGF PIC  X(0001).
000330     05  FILLER REDEFINES BDELHDGF.
000331         10  BDELHDGA PIC  X(0001).
000332     05  BDELHDGI PIC  X(0017).
000333 01  EL630BO REDEFINES EL630BI.
000334     05  FILLER            PIC  X(0012).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  BDATEO PIC  X(0008).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  BTIMEO PIC  99.99.
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  BBATCHO PIC  X(0008).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  BSEQO PIC  X(0004).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  BMOENDO PIC  X(0008).
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  BACCTNMO PIC  X(0030).
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  BCERTO PIC  X(0010).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  BSFXO PIC  X(0001).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  BVINHDO PIC  X(0005).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  BVINO PIC  X(0017).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  BLONOFCO PIC  X(0005).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  B1STNMO PIC  X(0010).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  BINTO PIC  X(0001).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  BLASTNMO PIC  X(0015).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  BAGEO PIC  X(0002).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  BBIRTHO PIC  X(0006).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  BJNT1STO PIC  X(0010).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  BJNTINTO PIC  X(0001).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  BJNTNAMO PIC  X(0015).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  BJNTAGEO PIC  X(0002).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  BJNTDOBO PIC  X(0006).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  BADDRS1O PIC  X(0030).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  BADDRS2O PIC  X(0030).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  BCITYO PIC  X(0028).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  BSTATEO PIC  X(0002).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  BZIPCDEO PIC  X(0010).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  BNFICRYO PIC  X(0025).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  BCADDR1O PIC  X(0030).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  BCADDR2O PIC  X(0030).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  BCCITYO PIC  X(0028).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  BCSTATEO PIC  X(0002).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  BCZIPCDO PIC  X(0010).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  BEFFDTO PIC  X(0006).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  B1STPMTO PIC  X(0006).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  BAPRO PIC  X(0007).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  BLNTRMO PIC  X(0003).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  BKIND1O PIC  X(0002).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  BTYPE1O PIC  X(0003).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  BTRM1O PIC  X(0003).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  BBEN1O PIC  X(0012).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  BPRM1O PIC  X(0011).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  BALTBN1O PIC  X(0012).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  BALTPM1O PIC  X(0009).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  BKIND2O PIC  X(0002).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  BTYPE2O PIC  X(0003).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  BTRM2O PIC  X(0003).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  BBEN2O PIC  X(0012).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  BPRM2O PIC  X(0011).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  BCP2O PIC  X(0002).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  BALTBN2O PIC  X(0012).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  BALTPM2O PIC  X(0009).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  BERMSG1O PIC  X(0079).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  BERMSG2O PIC  X(0079).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  BPFENTRO PIC  99.
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  BDELHDGO PIC  X(0017).
000500*    -------------------------------
000501 01  EL630CI REDEFINES EL630BI.
000502     05  FILLER            PIC  X(0012).
000503*    -------------------------------
000504     05  CDATEL PIC S9(0004) COMP.
000505     05  CDATEF PIC  X(0001).
000506     05  FILLER REDEFINES CDATEF.
000507         10  CDATEA PIC  X(0001).
000508     05  CDATEI PIC  X(0008).
000509*    -------------------------------
000510     05  CTIMEL PIC S9(0004) COMP.
000511     05  CTIMEF PIC  X(0001).
000512     05  FILLER REDEFINES CTIMEF.
000513         10  CTIMEA PIC  X(0001).
000514     05  CTIMEI PIC  X(0005).
000515*    -------------------------------
000516     05  CBATCHL PIC S9(0004) COMP.
000517     05  CBATCHF PIC  X(0001).
000518     05  FILLER REDEFINES CBATCHF.
000519         10  CBATCHA PIC  X(0001).
000520     05  CBATCHI PIC  X(0008).
000521*    -------------------------------
000522     05  CMOENDL PIC S9(0004) COMP.
000523     05  CMOENDF PIC  X(0001).
000524     05  FILLER REDEFINES CMOENDF.
000525         10  CMOENDA PIC  X(0001).
000526     05  CMOENDI PIC  X(0008).
000527*    -------------------------------
000528     05  CACCTNML PIC S9(0004) COMP.
000529     05  CACCTNMF PIC  X(0001).
000530     05  FILLER REDEFINES CACCTNMF.
000531         10  CACCTNMA PIC  X(0001).
000532     05  CACCTNMI PIC  X(0030).
000533*    -------------------------------
000534     05  CSEQ1L PIC S9(0004) COMP.
000535     05  CSEQ1F PIC  X(0001).
000536     05  FILLER REDEFINES CSEQ1F.
000537         10  CSEQ1A PIC  X(0001).
000538     05  CSEQ1I PIC  X(0004).
000539*    -------------------------------
000540     05  CCERT1L PIC S9(0004) COMP.
000541     05  CCERT1F PIC  X(0001).
000542     05  FILLER REDEFINES CCERT1F.
000543         10  CCERT1A PIC  X(0001).
000544     05  CCERT1I PIC  X(0010).
000545*    -------------------------------
000546     05  CSFX1L PIC S9(0004) COMP.
000547     05  CSFX1F PIC  X(0001).
000548     05  FILLER REDEFINES CSFX1F.
000549         10  CSFX1A PIC  X(0001).
000550     05  CSFX1I PIC  X(0001).
000551*    -------------------------------
000552     05  CEFFDT1L PIC S9(0004) COMP.
000553     05  CEFFDT1F PIC  X(0001).
000554     05  FILLER REDEFINES CEFFDT1F.
000555         10  CEFFDT1A PIC  X(0001).
000556     05  CEFFDT1I PIC  9(6).
000557*    -------------------------------
000558     05  CLSTNM1L PIC S9(0004) COMP.
000559     05  CLSTNM1F PIC  X(0001).
000560     05  FILLER REDEFINES CLSTNM1F.
000561         10  CLSTNM1A PIC  X(0001).
000562     05  CLSTNM1I PIC  X(0015).
000563*    -------------------------------
000564     05  CKIND1L PIC S9(0004) COMP.
000565     05  CKIND1F PIC  X(0001).
000566     05  FILLER REDEFINES CKIND1F.
000567         10  CKIND1A PIC  X(0001).
000568     05  CKIND1I PIC  X(0002).
000569*    -------------------------------
000570     05  CCANDT1L PIC S9(0004) COMP.
000571     05  CCANDT1F PIC  X(0001).
000572     05  FILLER REDEFINES CCANDT1F.
000573         10  CCANDT1A PIC  X(0001).
000574     05  CCANDT1I PIC  9(6).
000575*    -------------------------------
000576     05  CRFUND1L PIC S9(0004) COMP.
000577     05  CRFUND1F PIC  X(0001).
000578     05  FILLER REDEFINES CRFUND1F.
000579         10  CRFUND1A PIC  X(0001).
000580     05  CRFUND1I PIC  S9(9)V9(2).
000581*    -------------------------------
000582     05  CMTHD1L PIC S9(0004) COMP.
000583     05  CMTHD1F PIC  X(0001).
000584     05  FILLER REDEFINES CMTHD1F.
000585         10  CMTHD1A PIC  X(0001).
000586     05  CMTHD1I PIC  X(0001).
000587*    -------------------------------
000588     05  CKIND2L PIC S9(0004) COMP.
000589     05  CKIND2F PIC  X(0001).
000590     05  FILLER REDEFINES CKIND2F.
000591         10  CKIND2A PIC  X(0001).
000592     05  CKIND2I PIC  X(0002).
000593*    -------------------------------
000594     05  CCANDT2L PIC S9(0004) COMP.
000595     05  CCANDT2F PIC  X(0001).
000596     05  FILLER REDEFINES CCANDT2F.
000597         10  CCANDT2A PIC  X(0001).
000598     05  CCANDT2I PIC  9(6).
000599*    -------------------------------
000600     05  CRFUND2L PIC S9(0004) COMP.
000601     05  CRFUND2F PIC  X(0001).
000602     05  FILLER REDEFINES CRFUND2F.
000603         10  CRFUND2A PIC  X(0001).
000604     05  CRFUND2I PIC  S9(9)V9(2).
000605*    -------------------------------
000606     05  CMTHD2L PIC S9(0004) COMP.
000607     05  CMTHD2F PIC  X(0001).
000608     05  FILLER REDEFINES CMTHD2F.
000609         10  CMTHD2A PIC  X(0001).
000610     05  CMTHD2I PIC  X(0001).
000611*    -------------------------------
000612     05  CCHK1L PIC S9(0004) COMP.
000613     05  CCHK1F PIC  X(0001).
000614     05  FILLER REDEFINES CCHK1F.
000615         10  CCHK1A PIC  X(0001).
000616     05  CCHK1I PIC  X(0001).
000617*    -------------------------------
000618     05  CPAYEE1L PIC S9(0004) COMP.
000619     05  CPAYEE1F PIC  X(0001).
000620     05  FILLER REDEFINES CPAYEE1F.
000621         10  CPAYEE1A PIC  X(0001).
000622     05  CPAYEE1I PIC  X(0006).
000623*    -------------------------------
000624     05  CLIVES1L PIC S9(0004) COMP.
000625     05  CLIVES1F PIC  X(0001).
000626     05  FILLER REDEFINES CLIVES1F.
000627         10  CLIVES1A PIC  X(0001).
000628     05  CLIVES1I PIC  9(3).
000629*    -------------------------------
000630     05  CCANRN1L PIC S9(0004) COMP.
000631     05  CCANRN1F PIC  X(0001).
000632     05  FILLER REDEFINES CCANRN1F.
000633         10  CCANRN1A PIC  X(0001).
000634     05  CCANRN1I PIC  X(0001).
000635*    -------------------------------
000636     05  CSEQ2L PIC S9(0004) COMP.
000637     05  CSEQ2F PIC  X(0001).
000638     05  FILLER REDEFINES CSEQ2F.
000639         10  CSEQ2A PIC  X(0001).
000640     05  CSEQ2I PIC  X(0004).
000641*    -------------------------------
000642     05  CCERT2L PIC S9(0004) COMP.
000643     05  CCERT2F PIC  X(0001).
000644     05  FILLER REDEFINES CCERT2F.
000645         10  CCERT2A PIC  X(0001).
000646     05  CCERT2I PIC  X(0010).
000647*    -------------------------------
000648     05  CSFX2L PIC S9(0004) COMP.
000649     05  CSFX2F PIC  X(0001).
000650     05  FILLER REDEFINES CSFX2F.
000651         10  CSFX2A PIC  X(0001).
000652     05  CSFX2I PIC  X(0001).
000653*    -------------------------------
000654     05  CEFFDT2L PIC S9(0004) COMP.
000655     05  CEFFDT2F PIC  X(0001).
000656     05  FILLER REDEFINES CEFFDT2F.
000657         10  CEFFDT2A PIC  X(0001).
000658     05  CEFFDT2I PIC  9(6).
000659*    -------------------------------
000660     05  CLSTNM2L PIC S9(0004) COMP.
000661     05  CLSTNM2F PIC  X(0001).
000662     05  FILLER REDEFINES CLSTNM2F.
000663         10  CLSTNM2A PIC  X(0001).
000664     05  CLSTNM2I PIC  X(0015).
000665*    -------------------------------
000666     05  CKIND3L PIC S9(0004) COMP.
000667     05  CKIND3F PIC  X(0001).
000668     05  FILLER REDEFINES CKIND3F.
000669         10  CKIND3A PIC  X(0001).
000670     05  CKIND3I PIC  X(0002).
000671*    -------------------------------
000672     05  CCANDT3L PIC S9(0004) COMP.
000673     05  CCANDT3F PIC  X(0001).
000674     05  FILLER REDEFINES CCANDT3F.
000675         10  CCANDT3A PIC  X(0001).
000676     05  CCANDT3I PIC  9(6).
000677*    -------------------------------
000678     05  CRFUND3L PIC S9(0004) COMP.
000679     05  CRFUND3F PIC  X(0001).
000680     05  FILLER REDEFINES CRFUND3F.
000681         10  CRFUND3A PIC  X(0001).
000682     05  CRFUND3I PIC  S9(9)V9(2).
000683*    -------------------------------
000684     05  CMTHD3L PIC S9(0004) COMP.
000685     05  CMTHD3F PIC  X(0001).
000686     05  FILLER REDEFINES CMTHD3F.
000687         10  CMTHD3A PIC  X(0001).
000688     05  CMTHD3I PIC  X(0001).
000689*    -------------------------------
000690     05  CKIND4L PIC S9(0004) COMP.
000691     05  CKIND4F PIC  X(0001).
000692     05  FILLER REDEFINES CKIND4F.
000693         10  CKIND4A PIC  X(0001).
000694     05  CKIND4I PIC  X(0002).
000695*    -------------------------------
000696     05  CCANDT4L PIC S9(0004) COMP.
000697     05  CCANDT4F PIC  X(0001).
000698     05  FILLER REDEFINES CCANDT4F.
000699         10  CCANDT4A PIC  X(0001).
000700     05  CCANDT4I PIC  9(6).
000701*    -------------------------------
000702     05  CRFUND4L PIC S9(0004) COMP.
000703     05  CRFUND4F PIC  X(0001).
000704     05  FILLER REDEFINES CRFUND4F.
000705         10  CRFUND4A PIC  X(0001).
000706     05  CRFUND4I PIC  S9(9)V9(2).
000707*    -------------------------------
000708     05  CMTHD4L PIC S9(0004) COMP.
000709     05  CMTHD4F PIC  X(0001).
000710     05  FILLER REDEFINES CMTHD4F.
000711         10  CMTHD4A PIC  X(0001).
000712     05  CMTHD4I PIC  X(0001).
000713*    -------------------------------
000714     05  CCHK2L PIC S9(0004) COMP.
000715     05  CCHK2F PIC  X(0001).
000716     05  FILLER REDEFINES CCHK2F.
000717         10  CCHK2A PIC  X(0001).
000718     05  CCHK2I PIC  X(0001).
000719*    -------------------------------
000720     05  CPAYEE2L PIC S9(0004) COMP.
000721     05  CPAYEE2F PIC  X(0001).
000722     05  FILLER REDEFINES CPAYEE2F.
000723         10  CPAYEE2A PIC  X(0001).
000724     05  CPAYEE2I PIC  X(0006).
000725*    -------------------------------
000726     05  CLIVES2L PIC S9(0004) COMP.
000727     05  CLIVES2F PIC  X(0001).
000728     05  FILLER REDEFINES CLIVES2F.
000729         10  CLIVES2A PIC  X(0001).
000730     05  CLIVES2I PIC  9(3).
000731*    -------------------------------
000732     05  CCANRN2L PIC S9(0004) COMP.
000733     05  CCANRN2F PIC  X(0001).
000734     05  FILLER REDEFINES CCANRN2F.
000735         10  CCANRN2A PIC  X(0001).
000736     05  CCANRN2I PIC  X(0001).
000737*    -------------------------------
000738     05  CSEQ3L PIC S9(0004) COMP.
000739     05  CSEQ3F PIC  X(0001).
000740     05  FILLER REDEFINES CSEQ3F.
000741         10  CSEQ3A PIC  X(0001).
000742     05  CSEQ3I PIC  X(0004).
000743*    -------------------------------
000744     05  CCERT3L PIC S9(0004) COMP.
000745     05  CCERT3F PIC  X(0001).
000746     05  FILLER REDEFINES CCERT3F.
000747         10  CCERT3A PIC  X(0001).
000748     05  CCERT3I PIC  X(0010).
000749*    -------------------------------
000750     05  CSFX3L PIC S9(0004) COMP.
000751     05  CSFX3F PIC  X(0001).
000752     05  FILLER REDEFINES CSFX3F.
000753         10  CSFX3A PIC  X(0001).
000754     05  CSFX3I PIC  X(0001).
000755*    -------------------------------
000756     05  CEFFDT3L PIC S9(0004) COMP.
000757     05  CEFFDT3F PIC  X(0001).
000758     05  FILLER REDEFINES CEFFDT3F.
000759         10  CEFFDT3A PIC  X(0001).
000760     05  CEFFDT3I PIC  9(6).
000761*    -------------------------------
000762     05  CLSTNM3L PIC S9(0004) COMP.
000763     05  CLSTNM3F PIC  X(0001).
000764     05  FILLER REDEFINES CLSTNM3F.
000765         10  CLSTNM3A PIC  X(0001).
000766     05  CLSTNM3I PIC  X(0015).
000767*    -------------------------------
000768     05  CKIND5L PIC S9(0004) COMP.
000769     05  CKIND5F PIC  X(0001).
000770     05  FILLER REDEFINES CKIND5F.
000771         10  CKIND5A PIC  X(0001).
000772     05  CKIND5I PIC  X(0002).
000773*    -------------------------------
000774     05  CCANDT5L PIC S9(0004) COMP.
000775     05  CCANDT5F PIC  X(0001).
000776     05  FILLER REDEFINES CCANDT5F.
000777         10  CCANDT5A PIC  X(0001).
000778     05  CCANDT5I PIC  9(6).
000779*    -------------------------------
000780     05  CRFUND5L PIC S9(0004) COMP.
000781     05  CRFUND5F PIC  X(0001).
000782     05  FILLER REDEFINES CRFUND5F.
000783         10  CRFUND5A PIC  X(0001).
000784     05  CRFUND5I PIC  S9(9)V9(2).
000785*    -------------------------------
000786     05  CMTHD5L PIC S9(0004) COMP.
000787     05  CMTHD5F PIC  X(0001).
000788     05  FILLER REDEFINES CMTHD5F.
000789         10  CMTHD5A PIC  X(0001).
000790     05  CMTHD5I PIC  X(0001).
000791*    -------------------------------
000792     05  CKIND6L PIC S9(0004) COMP.
000793     05  CKIND6F PIC  X(0001).
000794     05  FILLER REDEFINES CKIND6F.
000795         10  CKIND6A PIC  X(0001).
000796     05  CKIND6I PIC  X(0002).
000797*    -------------------------------
000798     05  CCANDT6L PIC S9(0004) COMP.
000799     05  CCANDT6F PIC  X(0001).
000800     05  FILLER REDEFINES CCANDT6F.
000801         10  CCANDT6A PIC  X(0001).
000802     05  CCANDT6I PIC  9(6).
000803*    -------------------------------
000804     05  CRFUND6L PIC S9(0004) COMP.
000805     05  CRFUND6F PIC  X(0001).
000806     05  FILLER REDEFINES CRFUND6F.
000807         10  CRFUND6A PIC  X(0001).
000808     05  CRFUND6I PIC  S9(9)V9(2).
000809*    -------------------------------
000810     05  CMTHD6L PIC S9(0004) COMP.
000811     05  CMTHD6F PIC  X(0001).
000812     05  FILLER REDEFINES CMTHD6F.
000813         10  CMTHD6A PIC  X(0001).
000814     05  CMTHD6I PIC  X(0001).
000815*    -------------------------------
000816     05  CCHK3L PIC S9(0004) COMP.
000817     05  CCHK3F PIC  X(0001).
000818     05  FILLER REDEFINES CCHK3F.
000819         10  CCHK3A PIC  X(0001).
000820     05  CCHK3I PIC  X(0001).
000821*    -------------------------------
000822     05  CPAYEE3L PIC S9(0004) COMP.
000823     05  CPAYEE3F PIC  X(0001).
000824     05  FILLER REDEFINES CPAYEE3F.
000825         10  CPAYEE3A PIC  X(0001).
000826     05  CPAYEE3I PIC  X(0006).
000827*    -------------------------------
000828     05  CLIVES3L PIC S9(0004) COMP.
000829     05  CLIVES3F PIC  X(0001).
000830     05  FILLER REDEFINES CLIVES3F.
000831         10  CLIVES3A PIC  X(0001).
000832     05  CLIVES3I PIC  9(3).
000833*    -------------------------------
000834     05  CCANRN3L PIC S9(0004) COMP.
000835     05  CCANRN3F PIC  X(0001).
000836     05  FILLER REDEFINES CCANRN3F.
000837         10  CCANRN3A PIC  X(0001).
000838     05  CCANRN3I PIC  X(0001).
000839*    -------------------------------
000840     05  CSEQ4L PIC S9(0004) COMP.
000841     05  CSEQ4F PIC  X(0001).
000842     05  FILLER REDEFINES CSEQ4F.
000843         10  CSEQ4A PIC  X(0001).
000844     05  CSEQ4I PIC  X(0004).
000845*    -------------------------------
000846     05  CCERT4L PIC S9(0004) COMP.
000847     05  CCERT4F PIC  X(0001).
000848     05  FILLER REDEFINES CCERT4F.
000849         10  CCERT4A PIC  X(0001).
000850     05  CCERT4I PIC  X(0010).
000851*    -------------------------------
000852     05  CSFX4L PIC S9(0004) COMP.
000853     05  CSFX4F PIC  X(0001).
000854     05  FILLER REDEFINES CSFX4F.
000855         10  CSFX4A PIC  X(0001).
000856     05  CSFX4I PIC  X(0001).
000857*    -------------------------------
000858     05  CEFFDT4L PIC S9(0004) COMP.
000859     05  CEFFDT4F PIC  X(0001).
000860     05  FILLER REDEFINES CEFFDT4F.
000861         10  CEFFDT4A PIC  X(0001).
000862     05  CEFFDT4I PIC  9(6).
000863*    -------------------------------
000864     05  CLSTNM4L PIC S9(0004) COMP.
000865     05  CLSTNM4F PIC  X(0001).
000866     05  FILLER REDEFINES CLSTNM4F.
000867         10  CLSTNM4A PIC  X(0001).
000868     05  CLSTNM4I PIC  X(0015).
000869*    -------------------------------
000870     05  CKIND7L PIC S9(0004) COMP.
000871     05  CKIND7F PIC  X(0001).
000872     05  FILLER REDEFINES CKIND7F.
000873         10  CKIND7A PIC  X(0001).
000874     05  CKIND7I PIC  X(0002).
000875*    -------------------------------
000876     05  CCANDT7L PIC S9(0004) COMP.
000877     05  CCANDT7F PIC  X(0001).
000878     05  FILLER REDEFINES CCANDT7F.
000879         10  CCANDT7A PIC  X(0001).
000880     05  CCANDT7I PIC  9(6).
000881*    -------------------------------
000882     05  CRFUND7L PIC S9(0004) COMP.
000883     05  CRFUND7F PIC  X(0001).
000884     05  FILLER REDEFINES CRFUND7F.
000885         10  CRFUND7A PIC  X(0001).
000886     05  CRFUND7I PIC  S9(9)V9(2).
000887*    -------------------------------
000888     05  CMTHD7L PIC S9(0004) COMP.
000889     05  CMTHD7F PIC  X(0001).
000890     05  FILLER REDEFINES CMTHD7F.
000891         10  CMTHD7A PIC  X(0001).
000892     05  CMTHD7I PIC  X(0001).
000893*    -------------------------------
000894     05  CKIND8L PIC S9(0004) COMP.
000895     05  CKIND8F PIC  X(0001).
000896     05  FILLER REDEFINES CKIND8F.
000897         10  CKIND8A PIC  X(0001).
000898     05  CKIND8I PIC  X(0002).
000899*    -------------------------------
000900     05  CCANDT8L PIC S9(0004) COMP.
000901     05  CCANDT8F PIC  X(0001).
000902     05  FILLER REDEFINES CCANDT8F.
000903         10  CCANDT8A PIC  X(0001).
000904     05  CCANDT8I PIC  9(6).
000905*    -------------------------------
000906     05  CRFUND8L PIC S9(0004) COMP.
000907     05  CRFUND8F PIC  X(0001).
000908     05  FILLER REDEFINES CRFUND8F.
000909         10  CRFUND8A PIC  X(0001).
000910     05  CRFUND8I PIC  S9(9)V9(2).
000911*    -------------------------------
000912     05  CMTHD8L PIC S9(0004) COMP.
000913     05  CMTHD8F PIC  X(0001).
000914     05  FILLER REDEFINES CMTHD8F.
000915         10  CMTHD8A PIC  X(0001).
000916     05  CMTHD8I PIC  X(0001).
000917*    -------------------------------
000918     05  CCHK4L PIC S9(0004) COMP.
000919     05  CCHK4F PIC  X(0001).
000920     05  FILLER REDEFINES CCHK4F.
000921         10  CCHK4A PIC  X(0001).
000922     05  CCHK4I PIC  X(0001).
000923*    -------------------------------
000924     05  CPAYEE4L PIC S9(0004) COMP.
000925     05  CPAYEE4F PIC  X(0001).
000926     05  FILLER REDEFINES CPAYEE4F.
000927         10  CPAYEE4A PIC  X(0001).
000928     05  CPAYEE4I PIC  X(0006).
000929*    -------------------------------
000930     05  CLIVES4L PIC S9(0004) COMP.
000931     05  CLIVES4F PIC  X(0001).
000932     05  FILLER REDEFINES CLIVES4F.
000933         10  CLIVES4A PIC  X(0001).
000934     05  CLIVES4I PIC  9(3).
000935*    -------------------------------
000936     05  CCANRN4L PIC S9(0004) COMP.
000937     05  CCANRN4F PIC  X(0001).
000938     05  FILLER REDEFINES CCANRN4F.
000939         10  CCANRN4A PIC  X(0001).
000940     05  CCANRN4I PIC  X(0001).
000941*    -------------------------------
000942     05  CERMSG1L PIC S9(0004) COMP.
000943     05  CERMSG1F PIC  X(0001).
000944     05  FILLER REDEFINES CERMSG1F.
000945         10  CERMSG1A PIC  X(0001).
000946     05  CERMSG1I PIC  X(0079).
000947*    -------------------------------
000948     05  CERMSG2L PIC S9(0004) COMP.
000949     05  CERMSG2F PIC  X(0001).
000950     05  FILLER REDEFINES CERMSG2F.
000951         10  CERMSG2A PIC  X(0001).
000952     05  CERMSG2I PIC  X(0079).
000953*    -------------------------------
000954     05  CPFENTRL PIC S9(0004) COMP.
000955     05  CPFENTRF PIC  X(0001).
000956     05  FILLER REDEFINES CPFENTRF.
000957         10  CPFENTRA PIC  X(0001).
000958     05  CPFENTRI PIC  9(2).
000959*    -------------------------------
000960     05  CDELHDGL PIC S9(0004) COMP.
000961     05  CDELHDGF PIC  X(0001).
000962     05  FILLER REDEFINES CDELHDGF.
000963         10  CDELHDGA PIC  X(0001).
000964     05  CDELHDGI PIC  X(0017).
000965 01  EL630CO REDEFINES EL630BI.
000966     05  FILLER            PIC  X(0012).
000967*    -------------------------------
000968     05  FILLER            PIC  X(0003).
000969     05  CDATEO PIC  X(0008).
000970*    -------------------------------
000971     05  FILLER            PIC  X(0003).
000972     05  CTIMEO PIC  99.99.
000973*    -------------------------------
000974     05  FILLER            PIC  X(0003).
000975     05  CBATCHO PIC  X(0008).
000976*    -------------------------------
000977     05  FILLER            PIC  X(0003).
000978     05  CMOENDO PIC  X(0008).
000979*    -------------------------------
000980     05  FILLER            PIC  X(0003).
000981     05  CACCTNMO PIC  X(0030).
000982*    -------------------------------
000983     05  FILLER            PIC  X(0003).
000984     05  CSEQ1O PIC  X(0004).
000985*    -------------------------------
000986     05  FILLER            PIC  X(0003).
000987     05  CCERT1O PIC  X(0010).
000988*    -------------------------------
000989     05  FILLER            PIC  X(0003).
000990     05  CSFX1O PIC  X(0001).
000991*    -------------------------------
000992     05  FILLER            PIC  X(0003).
000993     05  CEFFDT1O PIC  999999.
000994*    -------------------------------
000995     05  FILLER            PIC  X(0003).
000996     05  CLSTNM1O PIC  X(0015).
000997*    -------------------------------
000998     05  FILLER            PIC  X(0003).
000999     05  CKIND1O PIC  X(0002).
001000*    -------------------------------
001001     05  FILLER            PIC  X(0003).
001002     05  CCANDT1O PIC  999999.
001003*    -------------------------------
001004     05  FILLER            PIC  X(0003).
001005     05  CRFUND1O PIC  9999999.99-.
001006*    -------------------------------
001007     05  FILLER            PIC  X(0003).
001008     05  CMTHD1O PIC  X(0001).
001009*    -------------------------------
001010     05  FILLER            PIC  X(0003).
001011     05  CKIND2O PIC  X(0002).
001012*    -------------------------------
001013     05  FILLER            PIC  X(0003).
001014     05  CCANDT2O PIC  999999.
001015*    -------------------------------
001016     05  FILLER            PIC  X(0003).
001017     05  CRFUND2O PIC  9999999.99-.
001018*    -------------------------------
001019     05  FILLER            PIC  X(0003).
001020     05  CMTHD2O PIC  X(0001).
001021*    -------------------------------
001022     05  FILLER            PIC  X(0003).
001023     05  CCHK1O PIC  X(0001).
001024*    -------------------------------
001025     05  FILLER            PIC  X(0003).
001026     05  CPAYEE1O PIC  X(0006).
001027*    -------------------------------
001028     05  FILLER            PIC  X(0003).
001029     05  CLIVES1O PIC  999.
001030*    -------------------------------
001031     05  FILLER            PIC  X(0003).
001032     05  CCANRN1O PIC  X(0001).
001033*    -------------------------------
001034     05  FILLER            PIC  X(0003).
001035     05  CSEQ2O PIC  X(0004).
001036*    -------------------------------
001037     05  FILLER            PIC  X(0003).
001038     05  CCERT2O PIC  X(0010).
001039*    -------------------------------
001040     05  FILLER            PIC  X(0003).
001041     05  CSFX2O PIC  X(0001).
001042*    -------------------------------
001043     05  FILLER            PIC  X(0003).
001044     05  CEFFDT2O PIC  999999.
001045*    -------------------------------
001046     05  FILLER            PIC  X(0003).
001047     05  CLSTNM2O PIC  X(0015).
001048*    -------------------------------
001049     05  FILLER            PIC  X(0003).
001050     05  CKIND3O PIC  X(0002).
001051*    -------------------------------
001052     05  FILLER            PIC  X(0003).
001053     05  CCANDT3O PIC  999999.
001054*    -------------------------------
001055     05  FILLER            PIC  X(0003).
001056     05  CRFUND3O PIC  9999999.99-.
001057*    -------------------------------
001058     05  FILLER            PIC  X(0003).
001059     05  CMTHD3O PIC  X(0001).
001060*    -------------------------------
001061     05  FILLER            PIC  X(0003).
001062     05  CKIND4O PIC  X(0002).
001063*    -------------------------------
001064     05  FILLER            PIC  X(0003).
001065     05  CCANDT4O PIC  999999.
001066*    -------------------------------
001067     05  FILLER            PIC  X(0003).
001068     05  CRFUND4O PIC  9999999.99-.
001069*    -------------------------------
001070     05  FILLER            PIC  X(0003).
001071     05  CMTHD4O PIC  X(0001).
001072*    -------------------------------
001073     05  FILLER            PIC  X(0003).
001074     05  CCHK2O PIC  X(0001).
001075*    -------------------------------
001076     05  FILLER            PIC  X(0003).
001077     05  CPAYEE2O PIC  X(0006).
001078*    -------------------------------
001079     05  FILLER            PIC  X(0003).
001080     05  CLIVES2O PIC  999.
001081*    -------------------------------
001082     05  FILLER            PIC  X(0003).
001083     05  CCANRN2O PIC  X(0001).
001084*    -------------------------------
001085     05  FILLER            PIC  X(0003).
001086     05  CSEQ3O PIC  X(0004).
001087*    -------------------------------
001088     05  FILLER            PIC  X(0003).
001089     05  CCERT3O PIC  X(0010).
001090*    -------------------------------
001091     05  FILLER            PIC  X(0003).
001092     05  CSFX3O PIC  X(0001).
001093*    -------------------------------
001094     05  FILLER            PIC  X(0003).
001095     05  CEFFDT3O PIC  999999.
001096*    -------------------------------
001097     05  FILLER            PIC  X(0003).
001098     05  CLSTNM3O PIC  X(0015).
001099*    -------------------------------
001100     05  FILLER            PIC  X(0003).
001101     05  CKIND5O PIC  X(0002).
001102*    -------------------------------
001103     05  FILLER            PIC  X(0003).
001104     05  CCANDT5O PIC  999999.
001105*    -------------------------------
001106     05  FILLER            PIC  X(0003).
001107     05  CRFUND5O PIC  9999999.99-.
001108*    -------------------------------
001109     05  FILLER            PIC  X(0003).
001110     05  CMTHD5O PIC  X(0001).
001111*    -------------------------------
001112     05  FILLER            PIC  X(0003).
001113     05  CKIND6O PIC  X(0002).
001114*    -------------------------------
001115     05  FILLER            PIC  X(0003).
001116     05  CCANDT6O PIC  999999.
001117*    -------------------------------
001118     05  FILLER            PIC  X(0003).
001119     05  CRFUND6O PIC  9999999.99-.
001120*    -------------------------------
001121     05  FILLER            PIC  X(0003).
001122     05  CMTHD6O PIC  X(0001).
001123*    -------------------------------
001124     05  FILLER            PIC  X(0003).
001125     05  CCHK3O PIC  X(0001).
001126*    -------------------------------
001127     05  FILLER            PIC  X(0003).
001128     05  CPAYEE3O PIC  X(0006).
001129*    -------------------------------
001130     05  FILLER            PIC  X(0003).
001131     05  CLIVES3O PIC  999.
001132*    -------------------------------
001133     05  FILLER            PIC  X(0003).
001134     05  CCANRN3O PIC  X(0001).
001135*    -------------------------------
001136     05  FILLER            PIC  X(0003).
001137     05  CSEQ4O PIC  X(0004).
001138*    -------------------------------
001139     05  FILLER            PIC  X(0003).
001140     05  CCERT4O PIC  X(0010).
001141*    -------------------------------
001142     05  FILLER            PIC  X(0003).
001143     05  CSFX4O PIC  X(0001).
001144*    -------------------------------
001145     05  FILLER            PIC  X(0003).
001146     05  CEFFDT4O PIC  999999.
001147*    -------------------------------
001148     05  FILLER            PIC  X(0003).
001149     05  CLSTNM4O PIC  X(0015).
001150*    -------------------------------
001151     05  FILLER            PIC  X(0003).
001152     05  CKIND7O PIC  X(0002).
001153*    -------------------------------
001154     05  FILLER            PIC  X(0003).
001155     05  CCANDT7O PIC  999999.
001156*    -------------------------------
001157     05  FILLER            PIC  X(0003).
001158     05  CRFUND7O PIC  9999999.99-.
001159*    -------------------------------
001160     05  FILLER            PIC  X(0003).
001161     05  CMTHD7O PIC  X(0001).
001162*    -------------------------------
001163     05  FILLER            PIC  X(0003).
001164     05  CKIND8O PIC  X(0002).
001165*    -------------------------------
001166     05  FILLER            PIC  X(0003).
001167     05  CCANDT8O PIC  999999.
001168*    -------------------------------
001169     05  FILLER            PIC  X(0003).
001170     05  CRFUND8O PIC  9999999.99-.
001171*    -------------------------------
001172     05  FILLER            PIC  X(0003).
001173     05  CMTHD8O PIC  X(0001).
001174*    -------------------------------
001175     05  FILLER            PIC  X(0003).
001176     05  CCHK4O PIC  X(0001).
001177*    -------------------------------
001178     05  FILLER            PIC  X(0003).
001179     05  CPAYEE4O PIC  X(0006).
001180*    -------------------------------
001181     05  FILLER            PIC  X(0003).
001182     05  CLIVES4O PIC  999.
001183*    -------------------------------
001184     05  FILLER            PIC  X(0003).
001185     05  CCANRN4O PIC  X(0001).
001186*    -------------------------------
001187     05  FILLER            PIC  X(0003).
001188     05  CERMSG1O PIC  X(0079).
001189*    -------------------------------
001190     05  FILLER            PIC  X(0003).
001191     05  CERMSG2O PIC  X(0079).
001192*    -------------------------------
001193     05  FILLER            PIC  X(0003).
001194     05  CPFENTRO PIC  99.
001195*    -------------------------------
001196     05  FILLER            PIC  X(0003).
001197     05  CDELHDGO PIC  X(0017).
001198*    -------------------------------
      *<<((file: EL6301S))
000523
000524     EJECT
000525
000526 01  MAP-B REDEFINES EL630BI.
000527     12  FILLER                      PIC X(42).
000528     12  DATA-AREA-B.
000529         16  BSEQ-LEN                PIC S9(4)  COMP.
000530         16  BSEQ-ATTRB              PIC X.
000531         16  BSEQ                    PIC 9(4).
000532         16  BMO-END-LEN             PIC S9(4)  COMP.
000533         16  BMO-END-ATTRB           PIC X.
000534         16  BMO-END                 PIC X(8).
000535         16  BACCT-NM-LEN            PIC S9(4)  COMP.
000536         16  BACCT-NM-ATTRB          PIC X.
000537         16  BACCT-NM                PIC X(30).
000538         16  BCERT-LEN               PIC S9(4)  COMP.
000539         16  BCERT-ATTRB             PIC X.
000540         16  BCERT                   PIC X(10).
000541         16  BSFX-LEN                PIC S9(4)  COMP.
000542         16  BSFX-ATTRB              PIC X.
000543         16  BSFX                    PIC X.
000544
000545         16  BVINHD-LEN              PIC S9(4)  COMP.
000546         16  BVINHD-ATTRB            PIC X.
000547         16  BVINNDI                 PIC X(5).
000548         16  BVIN-LEN                PIC S9(4)  COMP.
000549         16  BVIN-ATTRB              PIC X.
000550         16  BVIN-NOI                PIC X(17).
000551
000552
000553
000554         16  BLN-OFFICER-LEN         PIC S9(4)  COMP.
000555         16  BLN-OFFICER-ATTRB       PIC X.
000556         16  BLN-OFFICER             PIC X(5).
000557         16  B1ST-NAME-LEN           PIC S9(4)  COMP.
000558         16  B1ST-NAME-ATTRB         PIC X.
000559         16  B1ST-NAME               PIC X(10).
000560         16  BINIT-LEN               PIC S9(4)  COMP.
000561         16  BINIT-ATTRB             PIC X.
000562         16  BINIT                   PIC X.
000563         16  BLAST-NAME-LEN          PIC S9(4)  COMP.
000564         16  BLAST-NAME-ATTRB        PIC X.
000565         16  BLAST-NAME              PIC X(15).
000566         16  BAGE-LEN                PIC S9(4)  COMP.
000567         16  BAGE-ATTRB              PIC X.
000568         16  BAGE                    PIC 99.
000569
000570         16  BBIRTH-LEN              PIC S9(4)  COMP.
000571         16  BBIRTH-ATTRB            PIC X.
000572         16  BBIRTH-DT               PIC 9(6).
000573
000574
000575
000576         16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.
000577         16  BJNT-1ST-NAME-ATTRB     PIC X.
000578         16  BJNT-1ST-NAME           PIC X(10).
000579         16  BJNT-INIT-LEN           PIC S9(4)   COMP.
000580         16  BJNT-INIT-ATTRB         PIC X.
000581         16  BJNT-INIT               PIC X.
000582         16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.
000583         16  BJNT-LST-NAME-ATTRB     PIC X.
000584         16  BJNT-LST-NAME           PIC X(15).
000585         16  BJNT-AGE-LEN            PIC S9(4)   COMP.
000586         16  BJNT-AGE-ATTRB          PIC X.
000587         16  BJNT-AGE                PIC 99.
000588
000589         16  BJNTDOB-LEN             PIC S9(4)  COMP.
000590         16  BJNTDOB-ATTRB           PIC X.
000591         16  BJNTDOB-DT              PIC 9(6).
000592
000593
000594
000595         16  BADDRS1-LEN             PIC S9(4)  COMP.
000596         16  BADDRS1-ATTRB           PIC X.
000597         16  BADDRS1                 PIC X(30).
000598         16  BADDRS2-LEN             PIC S9(4)  COMP.
000599         16  BADDRS2-ATTRB           PIC X.
000600         16  BADDRS2                 PIC X(30).
000601         16  BCITY-LEN               PIC S9(4)  COMP.
000602         16  BCITY-ATTRB             PIC X.
000603         16  BCITY                   PIC X(28).
000604         16  BSTATE-LEN              PIC S9(4)  COMP.
000605         16  BSTATE-ATTRB            PIC X.
000606         16  BSTATE                  PIC XX.
000607         16  BZIPCDE-LEN             PIC S9(4)  COMP.
000608         16  BZIPCDE-ATTRB           PIC X.
000609         16  BZIPCDE                 PIC X(10).
000610         16  BBENEFICIARY-LEN        PIC S9(4)   COMP.
000611         16  BBENEFICIARY-ATTRB      PIC X.
000612         16  BBENEFICIARY            PIC X(25).
000613         16  BCADDR1-LEN             PIC S9(4)  COMP.
000614         16  BCADDR1-ATTRB           PIC X.
000615         16  BCADDR1                 PIC X(30).
000616         16  BCADDR2-LEN             PIC S9(4)  COMP.
000617         16  BCADDR2-ATTRB           PIC X.
000618         16  BCADDR2                 PIC X(30).
000619         16  BCCITY-LEN              PIC S9(4)  COMP.
000620         16  BCCITY-ATTRB            PIC X.
000621         16  BCCITY                  PIC X(28).
000622         16  BCSTATE-LEN             PIC S9(4)  COMP.
000623         16  BCSTATE-ATTRB           PIC X.
000624         16  BCSTATE                 PIC XX.
000625         16  BCZIPCD-LEN             PIC S9(4)  COMP.
000626         16  BCZIPCD-ATTRB           PIC X.
000627         16  BCZIPCD                 PIC X(10).
000628         16  BEFFDT-LEN              PIC S9(4)  COMP.
000629         16  BEFFDT-ATTRB            PIC X.
000630         16  BEFFDT                  PIC X(6).
000631         16  B1ST-PMT-LEN            PIC S9(4)  COMP.
000632         16  B1ST-PMT-ATTRB          PIC X.
000633         16  B1ST-PMT                PIC 9(6).
000634         16  BAPR-LEN                PIC S9(4)  COMP.
000635         16  BAPR-ATTRB              PIC X.
000636         16  BAPR-IN                 PIC X(7).
000637         16  BAPR-OUT REDEFINES BAPR-IN
000638                                     PIC 99.9999.
000639         16  BLN-TERM-LEN            PIC S9(4)  COMP.
000640         16  BLN-TERM-ATTRB          PIC X.
000641         16  BLN-TERMI               PIC 9(3).
000642         16  BLN-TERMO REDEFINES
000643                          BLN-TERMI  PIC ZZZ.
000644
000645         16  BKIND1-LEN           PIC S9(4)  COMP.
000646         16  BKIND1-ATTRB         PIC X.
000647         16  BKIND1               PIC XX.
000648         16  BTYPE1-LEN           PIC S9(4)  COMP.
000649         16  BTYPE1-ATTRB         PIC X.
000650         16  BTYPE1               PIC X(3).
000651         16  BTERM1-LEN           PIC S9(4)  COMP.
000652         16  BTERM1-ATTRB         PIC X.
000653         16  BTERM1I              PIC 999.
000654         16  BTERM1O REDEFINES
000655                        BTERM1I   PIC ZZZ.
000656         16  BBENE1-LEN            PIC S9(4)  COMP.
000657         16  BBENE1-ATTRB          PIC X.
000658         16  BBENE1I               PIC 9(10)V99.
000659*        16  BBENE1I               PIC 9(12).
000660         16  BBENE1O REDEFINES
000661                           BBENE1I PIC Z(9).99.
000662         16  BPREM1-LEN           PIC S9(4)  COMP.
000663         16  BPREM1-ATTRB         PIC X.
000664         16  BPREM1I              PIC 9(9)V99.
000665*        16  BPREM1I              PIC 9(11).
000666         16  BPREM1O REDEFINES
000667                          BPREM1I PIC Z(7).99-.
000668         16  BALT-BEN1-LEN        PIC S9(4)  COMP.
000669         16  BALT-BEN1-ATTRB      PIC X.
000670         16  BALT-BEN1I           PIC 9(10)V99.
000671*        16  BALT-BEN1I           PIC 9(12).
000672         16  BALT-BEN1O REDEFINES
000673                           BALT-BEN1I PIC Z(9).ZZ.
000674         16  BALT-PREM1-LEN       PIC S9(4)  COMP.
000675         16  BALT-PREM1-ATTRB     PIC X.
000676         16  BALT-PREM1I          PIC 9(7)V99.
000677*        16  BALT-PREM1I          PIC 9(9).
000678         16  BALT-PREM1O REDEFINES
000679                           BALT-PREM1I PIC Z(6).ZZ.
000680
000681         16  BKIND2-LEN           PIC S9(4)  COMP.
000682         16  BKIND2-ATTRB         PIC X.
000683         16  BKIND2               PIC XX.
000684         16  BTYPE2-LEN           PIC S9(4)  COMP.
000685         16  BTYPE2-ATTRB         PIC X.
000686         16  BTYPE2               PIC X(3).
000687         16  BTERM2-LEN           PIC S9(4)  COMP.
000688         16  BTERM2-ATTRB         PIC X.
000689         16  BTERM2I              PIC 999.
000690         16  BTERM2O REDEFINES
000691                        BTERM2I   PIC ZZZ.
000692         16  BBENE2-LEN            PIC S9(4)  COMP.
000693         16  BBENE2-ATTRB          PIC X.
000694         16  BBENE2I               PIC 9(10)V99.
000695*        16  BBENE2I               PIC 9(12).
000696         16  BBENE2O REDEFINES
000697                           BBENE2I PIC Z(9).99.
000698         16  BPREM2-LEN           PIC S9(4)  COMP.
000699         16  BPREM2-ATTRB         PIC X.
000700         16  BPREM2I              PIC 9(9)V99.
000701*        16  BPREM2I              PIC 9(11).
000702         16  BPREM2O REDEFINES
000703                          BPREM2I PIC Z(7).99-.
000704         16  BCRIT-PERD2-LEN      PIC S9(4)  COMP.
000705         16  BCRIT-PERD2-ATTRB    PIC X.
000706         16  BCRIT-PERD2I         PIC 99.
000707         16  BCRIT-PERD2O REDEFINES
000708                 BCRIT-PERD2I PIC ZZ.
000709         16  BALT-BEN2-LEN        PIC S9(4)  COMP.
000710         16  BALT-BEN2-ATTRB      PIC X.
000711         16  BALT-BEN2I           PIC 9(10)V99.
000712*        16  BALT-BEN2I           PIC 9(12).
000713         16  BALT-BEN2O REDEFINES
000714                           BALT-BEN2I PIC Z(9).ZZ.
000715         16  BALT-PREM2-LEN       PIC S9(4)  COMP.
000716         16  BALT-PREM2-ATTRB     PIC X.
000717         16  BALT-PREM2I          PIC 9(7)V99.
000718*        16  BALT-PREM2I          PIC 9(9).
000719         16  BALT-PREM2O REDEFINES
000720                           BALT-PREM2I PIC Z(6).ZZ.
000721
000722 01  MAP-C REDEFINES EL630BI.
000723     12  FILLER                  PIC X(86).
000724     12  DATA-AREA-C             OCCURS 4 TIMES.
000725         16  CSEQ-LEN                PIC S9(4)  COMP.
000726         16  CSEQ-ATTRB              PIC X.
000727         16  CSEQ                    PIC 9(4).
000728         16  CCERT-LEN               PIC S9(4)  COMP.
000729         16  CCERT-ATTRB             PIC X.
000730         16  CCERT                   PIC X(10).
000731         16  CSFX-LEN                PIC S9(4)  COMP.
000732         16  CSFX-ATTRB              PIC X.
000733         16  CSFX                    PIC X.
000734         16  CEFFDT-LEN              PIC S9(4)  COMP.
000735         16  CEFFDT-ATTRB            PIC X.
000736         16  CEFFDT                  PIC 9(6).
000737         16  CLAST-NAME-LEN          PIC S9(4)  COMP.
000738         16  CLAST-NAME-ATTRB        PIC X.
000739         16  CLAST-NAME              PIC X(15).
000740         16  CANCEL-INFO.
000741             20  CKIND1-LEN          PIC S9(4)  COMP.
000742             20  CKIND1-ATTRB        PIC X.
000743             20  CKIND1              PIC XX.
000744             20  CCANDT1-LEN         PIC S9(4)  COMP.
000745             20  CCANDT1-ATTRB       PIC X.
000746             20  CCANDT1             PIC 9(6).
000747             20  CREFUND1-LEN        PIC S9(4)  COMP.
000748             20  CREFUND1-ATTRB      PIC X.
000749             20  CREFUND1I           PIC S9(9)V99.
000750*            20  CREFUND1I           PIC X(11).
000751             20  CREFUND1O REDEFINES
000752                           CREFUND1I PIC Z(7).99-.
000753             20  CMTHD1-LEN          PIC S9(4)  COMP.
000754             20  CMTHD1-ATTRB        PIC X.
000755             20  CMTHD1              PIC X.
000756             20  CKIND2-LEN          PIC S9(4)  COMP.
000757             20  CKIND2-ATTRB        PIC X.
000758             20  CKIND2              PIC XX.
000759             20  CCANDT2-LEN         PIC S9(4)  COMP.
000760             20  CCANDT2-ATTRB       PIC X.
000761             20  CCANDT2             PIC 9(6).
000762             20  CREFUND2-LEN        PIC S9(4)  COMP.
000763             20  CREFUND2-ATTRB      PIC X.
000764             20  CREFUND2I           PIC S9(9)V99.
000765*            20  CREFUND2I           PIC X(11).
000766             20  CREFUND2O REDEFINES
000767                           CREFUND2I PIC Z(7).99-.
000768             20  CMTHD2-LEN          PIC S9(4)  COMP.
000769             20  CMTHD2-ATTRB        PIC X.
000770             20  CMTHD2              PIC X.
000771             20  CCHK-LEN            PIC S9(4)  COMP.
000772             20  CCHK-ATTRB          PIC X.
000773             20  CCHK                PIC X.
000774             20  CPAYEE-LEN          PIC S9(4)  COMP.
000775             20  CPAYEE-ATTRB        PIC X.
000776             20  CPAYEE              PIC X(6).
000777             20  CLIVES-LEN          PIC S9(4)  COMP.
000778             20  CLIVES-ATTRB        PIC X.
000779             20  CLIVESI             PIC 999.
000780             20  CLIVESO REDEFINES
000781                          CLIVESI    PIC ZZZ.
000782             20  CCANREA-LEN         PIC S9(4)  COMP.
000783             20  CCANREA-ATTRB       PIC X.
000784             20  CCANREA             PIC X.
000785     EJECT
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
000787 01  DFHCOMMAREA             PIC X(1900).
000788
000789     EJECT
000790*01 PARMLIST .
000791*    02  FILLER              PIC S9(8)   COMP.
000792*    02  ERPNDB-POINTER      PIC S9(8)   COMP.
000793*    02  ELCNTL-POINTER      PIC S9(8)   COMP.
000794*    02  ELCERT-POINTER      PIC S9(8)   COMP.
000795*    02  ERPNDM-POINTER      PIC S9(8)   COMP.
000796
000797     EJECT
000798
000799*    COPY ERCPNDB.
      *>>((file: ERCPNDB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.025                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
000009*                                                                *
000010******************************************************************
000011*   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
000012*         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
000013******************************************************************
000014*                                                                *
000015*                                                                *
000016*   FILE TYPE = VSAM,KSDS                                        *
000017*   RECORD SIZE = 585  RECFORM = FIXED                           *
000018*                                                                *
000019*   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
000020*       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
000021*                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
000022*                                                 RKP=13,LEN=36  *
000023*       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
000024*                                      AND CHG-SEQ.)             *
000025*                                                RKP=49,LEN=11   *
000026*       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
000027*                                      AND CHG-SEQ.)             *
000028*                                                RKP=60,LEN=15   *
000029*                                                                *
000030*   LOG = NO                                                     *
000031*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000032******************************************************************
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000042* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000043* 011904                   PEMA  ADD TOTAL FEE PROCESSING
000044* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000045* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
000046* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000047* 032306                   PEMA  ADD BOW LOAN NUMBER
000048* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
000049* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
000050* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000051* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000052* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
000053* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
000054* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000055* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
000056* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
000057* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000058* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
000059* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000060* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
000061******************************************************************
000062
000063 01  PENDING-BUSINESS.
000064     12  PB-RECORD-ID                     PIC XX.
000065         88  VALID-PB-ID                        VALUE 'PB'.
000066
000067     12  PB-CONTROL-PRIMARY.
000068         16  PB-COMPANY-CD                PIC X.
000069         16  PB-ENTRY-BATCH               PIC X(6).
000070         16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
000071         16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
000072
000073     12  PB-CONTROL-BY-ACCOUNT.
000074         16  PB-COMPANY-CD-A1             PIC X.
000075         16  PB-CARRIER                   PIC X.
000076         16  PB-GROUPING.
000077             20  PB-GROUPING-PREFIX       PIC XXX.
000078             20  PB-GROUPING-PRIME        PIC XXX.
000079         16  PB-STATE                     PIC XX.
000080         16  PB-ACCOUNT.
000081             20  PB-ACCOUNT-PREFIX        PIC X(4).
000082             20  PB-ACCOUNT-PRIME         PIC X(6).
000083         16  PB-CERT-EFF-DT               PIC XX.
000084         16  PB-CERT-NO.
000085             20  PB-CERT-PRIME            PIC X(10).
000086             20  PB-CERT-SFX              PIC X.
000087         16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
000088
000089         16  PB-RECORD-TYPE               PIC X.
000090             88  PB-MAILING-DATA                VALUE '0'.
000091             88  PB-ISSUE                       VALUE '1'.
000092             88  PB-CANCELLATION                VALUE '2'.
000093             88  PB-BATCH-TRAILER               VALUE '9'.
000094
000095     12  PB-CONTROL-BY-ORIG-BATCH.
000096         16  PB-ORIGINAL-COMPANY-CD       PIC X.
000097         16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
000098         16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
000099         16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
000100
000101     12  PB-CONTROL-BY-CSR.
000102         16  PB-CSR-COMPANY-CD            PIC X.
000103         16  PB-CSR-ID                    PIC X(4).
000104         16  PB-CSR-ENTRY-BATCH           PIC X(6).
000105         16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
000106         16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
000107******************************************************************
000108*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
000109******************************************************************
000110
000111     12  PB-LAST-MAINT-DT                 PIC XX.
000112     12  PB-LAST-MAINT-BY                 PIC X(4).
000113     12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000114
000115     12  PB-RECORD-BODY                   PIC X(375).
000116
000117     12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
000118         16  PB-CERT-ORIGIN               PIC X.
000119             88  CLASIC-CREATED-CERT         VALUE '1'.
000120         16  PB-I-NAME.
000121             20  PB-I-INSURED-LAST-NAME   PIC X(15).
000122             20  PB-I-INSURED-FIRST-NAME.
000123                 24  PB-I-INSURED-1ST-INIT PIC X.
000124                 24  FILLER                PIC X(9).
000125             20  PB-I-INSURED-MIDDLE-INIT PIC X.
000126         16  PB-I-AGE                     PIC S99   COMP-3.
000127         16  PB-I-JOINT-AGE               PIC S99   COMP-3.
000128         16  PB-I-BIRTHDAY                PIC XX.
000129         16  PB-I-INSURED-SEX             PIC X.
000130             88  PB-SEX-MALE     VALUE 'M'.
000131             88  PB-SEX-FEMALE   VALUE 'F'.
000132
000133         16  PB-I-LF-TERM                 PIC S999   COMP-3.
000134         16  PB-I-AH-TERM                 PIC S999   COMP-3.
000135         16  PB-I-LOAN-TERM               PIC S999   COMP-3.
000136         16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
000137         16  PB-I-SKIP-CODE               PIC X.
000138             88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
000139             88  PB-SKIP-JULY              VALUE '1'.
000140             88  PB-SKIP-AUGUST            VALUE '2'.
000141             88  PB-SKIP-SEPTEMBER         VALUE '3'.
000142             88  PB-SKIP-JULY-AUG          VALUE '4'.
000143             88  PB-SKIP-AUG-SEPT          VALUE '5'.
000144             88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
000145             88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
000146             88  PB-SKIP-JUNE              VALUE '8'.
000147             88  PB-SKIP-JUNE-JULY         VALUE '9'.
000148             88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
000149             88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
000150         16  PB-I-TERM-TYPE               PIC X.
000151             88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
000152             88  PB-PAID-WEEKLY            VALUE 'W'.
000153             88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
000154             88  PB-PAID-BI-WEEKLY         VALUE 'B'.
000155             88  PB-PAID-13-YEARLY         VALUE 'T'.
000156         16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
000157         16  PB-I-POLICY-FORM-NO          PIC X(12).
000158         16  PB-I-DATA-ENTRY-SW           PIC X.
000159             88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
000160             88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
000161             88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
000162             88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
000163         16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
000164         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
000165*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
000166         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
000167         16  PB-I-LETTER-REQD             PIC X.
000168
000169         16  PB-I-LIFE-BENEFIT-CD         PIC XX.
000170             88  PB-VALID-LIFE               VALUE '01' THRU '89'.
000171             88  PB-INVALID-LIFE             VALUE '  ' '00'
000172                                                   '90' THRU '99'.
000173         16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
000174                                          PIC XX.
000175         16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000176         16  PB-I-AMOUNT-FINANCED REDEFINES
000177                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
000178         16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
000179         16  PB-I-UNPAID-CASH-PRICE REDEFINES
000180                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
000181         16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000182         16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
000183         16  PB-I-CLP-AMOUNT REDEFINES
000184                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
000185         16  PB-I-LF-CALC-FLAG            PIC X.
000186             88 PB-COMP-LF-PREM               VALUE '?'.
000187         16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
000188         16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
000189         16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
000190         16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
000191         16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
000192         16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
000193         16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
000194         16  PB-I-LF-ABBR                 PIC XXX.
000195         16  PB-I-LF-INPUT-CD             PIC XX.
000196
000197         16  PB-I-AH-BENEFIT-CD           PIC XX.
000198             88  PB-VALID-AH                 VALUE '01' THRU '89'.
000199             88  PB-INVALID-AH               VALUE '  ' '00'
000200                                                   '90' THRU '99'.
000201         16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000202         16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000203         16  PB-I-AH-CALC-FLAG            PIC X.
000204             88 PB-COMP-AH-PREM                  VALUE '?'.
000205         16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
000206         16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
000207         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
000208         16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
000209         16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
000210         16  PB-I-AH-ABBR                 PIC XXX.
000211         16  PB-I-AH-INPUT-CD             PIC XXX.
000212
000213         16  PB-I-SPECIAL-REIN-CODE       PIC X.
000214         16  PB-I-REIN-TABLE              PIC XXX.
000215         16  PB-I-BUSINESS-TYPE           PIC 99.
000216         16  PB-I-INDV-GRP-CD             PIC X.
000217         16  PB-I-MORT-CODE.
000218             20  PB-I-TABLE               PIC X.
000219             20  PB-I-INTEREST            PIC XX.
000220             20  PB-I-MORT-TYP            PIC X.
000221         16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
000222         16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
000223         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
000224         16  PB-I-INDV-GRP-OVRD           PIC X.
000225         16  PB-I-RATE-CLASS-OVRD         PIC XX.
000226         16  PB-I-SIG-SW                  PIC X.
000227             88  PB-POLICY-SIGNED             VALUE 'Y'.
000228         16  PB-I-RATE-CLASS              PIC XX.
000229         16  PB-I-RATE-DEVIATION-LF       PIC XXX.
000230         16  PB-I-RATE-DEVIATION-AH       PIC XXX.
000231         16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
000232         16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
000233         16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
000234         16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
000235         16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
000236         16  PB-I-BENEFIT-TYPE            PIC XXX.
000237         16  PB-I-OB-FLAG                 PIC X.
000238             88  PB-I-OB                      VALUE 'B'.
000239             88  PB-I-SUMMARY                 VALUE 'Z'.
000240         16  PB-I-ENTRY-STATUS            PIC X.
000241             88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
000242                                              'M' '5' '9' '2'.
000243             88  PB-I-NORMAL-ENTRY            VALUE '1'.
000244             88  PB-I-POLICY-PENDING          VALUE '2'.
000245             88  PB-I-CONVERSION-ENTRY        VALUE '4'.
000246             88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
000247             88  PB-I-POLICY-IS-CASH          VALUE 'C'.
000248             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
000249             88  PB-I-REIN-ONLY               VALUE '9'.
000250             88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
000251             88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
000252             88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
000253             88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
000254         16  PB-I-INT-CODE                PIC X.
000255             88  PB-ADD-ON-INTEREST           VALUE 'A'.
000256             88  PB-SIMPLE-INTEREST           VALUE 'S'.
000257         16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000258         16  PB-I-SOC-SEC-NO              PIC X(11).
000259         16  PB-I-MEMBER-NO               PIC X(12).
000260         16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
000261*        16  PB-I-LOAN-OFFICER            PIC XXX.
000262         16  PB-I-OLD-LOF                 PIC XXX.
000263         16  PB-I-LF-EXPIRE-DT            PIC XX.
000264         16  PB-I-AH-EXPIRE-DT            PIC XX.
000265         16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
000266         16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
000267         16  PB-I-LIFE-INDICATOR          PIC X.
000268             88  PB-I-JOINT-COVERAGE         VALUE 'J'.
000269         16  PB-I-LIVES                   PIC S9(7)       COMP-3.
000270         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
000271                                          PIC S9(5)V99    COMP-3.
000272         16  PB-I-MAIL-ADDRS-SW           PIC X.
000273             88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
000274             88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
000275         16  PB-I-1ST-PMT-DT              PIC XX.
000276         16  PB-I-JOINT-INSURED.
000277             20 PB-I-JOINT-LAST-NAME      PIC X(15).
000278             20 PB-I-JOINT-FIRST-NAME.
000279                24  PB-I-JOINT-FIRST-INIT PIC X.
000280                24  FILLER                PIC X(9).
000281             20 PB-I-JOINT-MIDDLE-INIT    PIC X.
000282*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
000283         16  PB-I-BENEFICIARY-NAME.
000284             20  PB-I-BANK-NUMBER         PIC X(10).
000285             20  FILLER                   PIC X(15).
000286         16  PB-I-LAST-ADD-ON-DT          PIC XX.
000287         16  PB-I-REFERENCE               PIC X(12).
000288         16  FILLER REDEFINES PB-I-REFERENCE.
000289             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
000290             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
000291             20  PB-I-CLP-STATE           PIC XX.
000292         16  PB-I-UNDERWRITING-STATUS     PIC X.
000293             88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
000294             88  PB-I-POLICY-DECLINED         VALUE 'D'.
000295             88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
000296         16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
000297         16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
000298         16  PB-I-RESIDENT-STATE          PIC XX.
000299         16  PB-I-RATE-CODE               PIC X(4).
000300         16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
000301         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000302         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000303         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
000304         16  PB-I-BANK-NOCHRGB            PIC 99.
000305         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
000306         16  PB-I-JOINT-BIRTHDAY          PIC XX.
000307
000308     12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
000309         16  PB-C-LF-CANCEL-VOID-SW       PIC X.
000310             88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
000311         16  PB-C-CANCEL-ORIGIN           PIC X.
000312             88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
000313         16  PB-C-LF-CANCEL-DT            PIC XX.
000314         16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000315         16  PB-C-LF-CALC-REQ             PIC X.
000316             88 PB-COMP-LF-CANCEL            VALUE '?'.
000317         16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
000318         16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
000319         16  PB-C-AH-CANCEL-VOID-SW       PIC X.
000320             88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
000321         16  PB-C-AH-CANCEL-DT            PIC XX.
000322         16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000323         16  PB-C-AH-CALC-REQ             PIC X.
000324             88 PB-COMP-AH-CANCEL            VALUE '?'.
000325         16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
000326         16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
000327         16  PB-C-LAST-NAME               PIC X(15).
000328         16  PB-C-REFUND-SW               PIC X.
000329             88  PB-C-REFUND-CREATED          VALUE 'Y'.
000330             88  PB-C-REFUND-REQUESTED        VALUE 'R'.
000331         16  PB-C-LIVES                   PIC S9(3)       COMP-3.
000332         16  PB-C-PAYEE-CODE              PIC X(6).
000333         16  PB-C-LF-REFUND-OVERRIDE      PIC X.
000334         16  PB-C-AH-REFUND-OVERRIDE      PIC X.
000335         16  PB-C-LF-COMM-CHARGEBACK      PIC X.
000336         16  PB-C-AH-COMM-CHARGEBACK      PIC X.
000337         16  PB-C-REFERENCE               PIC X(12).
000338         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000339         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000340         16  PB-C-POST-CARD-IND           PIC X.
000341         16  PB-C-CANCEL-REASON           PIC X.
000342         16  PB-C-REF-INTERFACE-SW        PIC X.
000343         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
000344         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
000345         16  FILLER                       PIC X(01).
000346*        16  FILLER                       PIC X(18).
000347         16  PB-C-POLICY-FORM-NO          PIC X(12).
000348*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
000349         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
000350         16  PB-CANCELED-CERT-DATA.
000351             20  PB-CI-INSURED-NAME.
000352                 24  PB-CI-LAST-NAME      PIC X(15).
000353                 24  PB-CI-INITIALS       PIC XX.
000354             20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
000355             20  PB-CI-INSURED-SEX        PIC X.
000356             20  PB-CI-LF-TERM            PIC S999        COMP-3.
000357             20  PB-CI-LF-BENEFIT-CD      PIC XX.
000358             20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
000359             20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000360             20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000361             20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
000362             20  PB-CI-AH-TERM            PIC S999        COMP-3.
000363             20  PB-CI-AH-BENEFIT-CD      PIC XX.
000364             20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000365             20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000366             20  PB-CI-RATE-CLASS         PIC XX.
000367             20  PB-CI-RATE-DEV-LF        PIC XXX.
000368             20  PB-CI-RATE-DEV-AH        PIC XXX.
000369             20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
000370             20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
000371             20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
000372             20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
000373             20  PB-CI-LF-ABBR            PIC X(3).
000374             20  PB-CI-AH-ABBR            PIC X(3).
000375             20  PB-CI-OB-FLAG            PIC X.
000376                 88  PB-CI-OB                VALUE 'B'.
000377             20  PB-CI-LF-POLICY-STATUS   PIC X.
000378                 88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
000379                                           'M' '4' '5' '9' '2'.
000380                 88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
000381                 88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
000382                 88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
000383                 88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
000384                 88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
000385                 88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
000386                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000387                 88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
000388                 88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000389                 88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
000390                 88  PB-CI-LF-REIN-ONLY              VALUE '9'.
000391                 88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
000392                 88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
000393             20  PB-CI-AH-POLICY-STATUS   PIC X.
000394                 88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
000395                                           'M' '4' '5' '9' '2'.
000396                 88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
000397                 88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
000398                 88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
000399                 88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
000400                 88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
000401                 88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
000402                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000403                 88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
000404                 88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000405                 88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
000406                 88  PB-CI-AH-REIN-ONLY              VALUE '9'.
000407                 88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
000408                 88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
000409             20  PB-CI-PAY-FREQUENCY      PIC 99.
000410             20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000411             20  PB-CI-SOC-SEC-NO         PIC X(11).
000412             20  PB-CI-MEMBER-NO          PIC X(12).
000413             20  PB-CI-INT-CODE           PIC X.
000414                 88  PB-CI-ADD-ON                  VALUE 'A'.
000415                 88  PB-CI-SIMPLE                  VALUE 'S'.
000416             20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
000417             20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
000418             20  PB-CI-COMP-EXCP-SW       PIC X.
000419                 88  PB-CI-NO-COMP-EXCP            VALUE ' '.
000420                 88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
000421             20  PB-CI-ENTRY-STATUS       PIC X.
000422             20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
000423             20  PB-CI-AH-PAID-THRU-DT    PIC XX.
000424             20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
000425             20  PB-CI-DEATH-DT           PIC XX.
000426             20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
000427             20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
000428             20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000429             20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000430             20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
000431             20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
000432             20  PB-CI-ENTRY-DT              PIC XX.
000433             20  PB-CI-ENTRY-BATCH           PIC X(6).
000434             20  PB-CI-LF-EXPIRE-DT          PIC XX.
000435             20  PB-CI-AH-EXPIRE-DT          PIC XX.
000436             20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
000437             20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
000438             20  PB-CI-OLD-LOF               PIC XXX.
000439*            20  PB-CI-LOAN-OFFICER          PIC XXX.
000440             20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
000441             20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
000442             20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
000443             20  PB-CI-INDV-GRP-CD           PIC X.
000444             20  PB-CI-BENEFICIARY-NAME.
000445                 24  PB-CI-BANK-NUMBER       PIC X(10).
000446                 24  FILLER                  PIC X(15).
000447             20  PB-CI-NOTE-SW               PIC X.
000448             20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
000449             20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
000450             20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
000451             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
000452             20  PB-CI-LOAN-OFFICER          PIC X(5).
000453             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
000454             20  PB-CI-FIRST-NAME            PIC X(10).
000455             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
000456
000457         16  FILLER                       PIC X(13).
000458*032306  16  FILLER                       PIC X(27).
000459*        16  FILLER                       PIC X(46).
000460
000461     12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
000462         16  FILLER                       PIC X(10).
000463         16  PB-M-INSURED-LAST-NAME       PIC X(15).
000464         16  PB-M-INSURED-FIRST-NAME      PIC X(10).
000465         16  PB-M-INSURED-MID-INIT        PIC X.
000466         16  PB-M-INSURED-AGE             PIC 99.
000467         16  PB-M-INSURED-BIRTHDAY        PIC XX.
000468         16  PB-M-INSURED-SEX             PIC X.
000469         16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
000470         16  PB-M-INSURED-ADDRESS-1       PIC X(30).
000471         16  PB-M-INSURED-ADDRESS-2       PIC X(30).
000472         16  PB-M-INSURED-CITY-STATE.
000473             20  PB-M-INSURED-CITY        PIC X(28).
000474             20  PB-M-INSURED-STATE       PIC XX.
000475         16  PB-M-INSURED-ZIP-CODE.
000476             20  PB-M-INSURED-ZIP-PRIME.
000477                 24  PB-M-INSURED-ZIP-1   PIC X.
000478                     88  PB-M-CANADIAN-POST-CODE
000479                                             VALUE 'A' THRU 'Z'.
000480                 24  FILLER               PIC X(4).
000481             20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
000482         16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
000483                                        PB-M-INSURED-ZIP-CODE.
000484             20  PM-M-INS-CAN-POST1       PIC XXX.
000485             20  PM-M-INS-CAN-POST2       PIC XXX.
000486             20  FILLER                   PIC XXX.
000487         16  PB-M-INSURED-PHONE-NO        PIC 9(10).
000488         16  PB-M-JOINT-BIRTHDAY          PIC XX.
000489         16  PB-M-CRED-BENE-NAME          PIC X(30).
000490         16  PB-M-CRED-BENE-ADDR1         PIC X(30).
000491         16  PB-M-CRED-BENE-ADDR2         PIC X(30).
000492         16  PB-M-CRED-BENE-CITYST.
000493             20  PB-M-CRED-BENE-CITY      PIC X(28).
000494             20  PB-M-CRED-BENE-STATE     PIC XX.
000495
000496         16  FILLER                       PIC X(92).
000497
000498     12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
000499         16  FILLER                       PIC X(10).
000500         16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000501         16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000502         16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000503         16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000504         16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000505         16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000506         16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000507         16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000508         16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000509         16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000510         16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000511         16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000512         16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
000513         16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
000514         16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
000515         16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
000516         16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
000517         16  PB-ACCOUNT-NAME              PIC X(30).
000518         16  PB-PREM-REF-RPT-FLAG         PIC X.
000519         16  PB-REFERENCE                 PIC X(12).
000520         16  PB-B-RECEIVED-DT             PIC XX.
000521         16  FILLER                       PIC X(234).
000522
000523     12  PB-RECORD-STATUS.
000524         16  PB-CREDIT-SELECT-DT          PIC XX.
000525         16  PB-CREDIT-ACCEPT-DT          PIC XX.
000526         16  PB-BILLED-DT                 PIC XX.
000527         16  PB-BILLING-STATUS            PIC X.
000528             88  PB-ENTRY-REVERSED            VALUE 'R'.
000529             88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
000530             88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
000531         16  PB-RECORD-BILL               PIC X.
000532             88  PB-RECORD-ON-HOLD            VALUE 'H'.
000533             88  PB-RECORD-RETURNED           VALUE 'R'.
000534             88  PB-RECORD-ENDORSED           VALUE 'E'.
000535             88  PB-OVERRIDE-LIFE             VALUE 'L'.
000536             88  PB-OVERRIDE-AH               VALUE 'A'.
000537             88  PB-OVERRIDE-BOTH             VALUE 'B'.
000538         16  PB-BATCH-ENTRY               PIC X.
000539             88  PB-POLICY-IS-DECLINED        VALUE 'D'.
000540             88  PB-REIN-ONLY-CERT            VALUE 'R'.
000541             88  PB-REISSUED-CERT             VALUE 'E'.
000542             88  PB-CASH-CERT                 VALUE 'C'.
000543             88  PB-MONTHLY-CERT              VALUE 'M'.
000544             88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
000545             88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
000546             88  PB-POLICY-IS-VOIDED          VALUE 'V'.
000547         16  PB-FORCE-CODE                PIC X.
000548             88  PB-FORCE-OFF                 VALUE ' ' '0'.
000549             88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
000550             88  PB-CANCEL-FORCE              VALUE '8'.
000551             88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
000552             88  PB-ALL-CANCEL-FORCED         VALUE '8'.
000553             88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
000554             88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
000555             88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
000556             88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
000557             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
000558             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
000559         16  PB-FATAL-FLAG                PIC X.
000560             88  PB-FATAL-ERRORS              VALUE 'X'.
000561         16  PB-FORCE-ER-CD               PIC X.
000562             88  PB-FORCE-ERRORS              VALUE 'F'.
000563             88  PB-UNFORCED-ERRORS           VALUE 'X'.
000564         16  PB-WARN-ER-CD                PIC X.
000565             88  PB-WARNING-ERRORS            VALUE 'W'.
000566         16  FILLER                       PIC X.
000567         16  PB-OUT-BAL-CD                PIC X.
000568             88  PB-OUT-OF-BAL                VALUE 'O'.
000569         16  PB-LIFE-OVERRIDE-L1          PIC X.
000570         16  PB-AH-OVERRIDE-L1            PIC X.
000571         16  PB-INPUT-DT                  PIC XX.
000572         16  PB-INPUT-BY                  PIC X(4).
000573         16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
000574         16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
000575         16  PB-TOLERANCE-REJECT-SW       PIC X.
000576         16  PB-LF-EARNING-METHOD         PIC X.
000577         16  PB-AH-EARNING-METHOD         PIC X.
000578         16  PB-LF-TERM-CALC-METHOD       PIC X.
000579         16  PB-AH-TERM-CALC-METHOD       PIC X.
000580         16  PB-REIN-CD                   PIC XXX.
000581         16  PB-LF-REFUND-TYPE            PIC X.
000582         16  PB-AH-REFUND-TYPE            PIC X.
000583         16  PB-ACCT-EFF-DT               PIC XX.
000584         16  PB-ACCT-EXP-DT               PIC XX.
000585         16  PB-COMPANY-ID                PIC X(3).
000586         16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000587         16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000588         16  PB-SV-CARRIER                PIC X.
000589         16  PB-SV-GROUPING               PIC X(6).
000590         16  PB-SV-STATE                  PIC XX.
000591         16  PB-CONFIRMATION-REPT-DT      PIC XX.
000592         16  PB-GA-BILLING-INFO.
000593             20  PB-GA-BILL-DT OCCURS 5 TIMES
000594                                          PIC XX.
000595         16  PB-SV-REMIT-TO  REDEFINES
000596             PB-GA-BILLING-INFO           PIC X(10).
000597         16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
000598         16  PB-I-LOAN-OFFICER            PIC X(5).
000599         16  PB-I-VIN                     PIC X(17).
000600
000601         16  FILLER                       PIC X(04).
000602         16  IMNET-BYPASS-SW              PIC X.
000603
000604******************************************************************
000605*                COMMON EDIT ERRORS                              *
000606******************************************************************
000607
000608     12  PB-COMMON-ERRORS.
000609         16  PB-COMMON-ERROR    OCCURS 10 TIMES
000610                                           PIC S9(4)     COMP.
000611
000612******************************************************************
      *<<((file: ERCPNDB))
000800     EJECT
000801
000802*    COPY ELCCNTL.
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
000803     EJECT
000804*    COPY ERCACCT.
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
000805     EJECT
000806*    COPY ELCCERT.
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
000807     EJECT
000808
000809*    COPY ERCPNDM.
      *>>((file: ERCPNDM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDM                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING MAILING DATA                      *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
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
000027* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
000028* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000029* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
000030******************************************************************
000031
000032 01  PENDING-MAILING-DATA.
000033     12  PM-RECORD-ID                      PIC XX.
000034         88  VALID-MA-ID                       VALUE 'PM'.
000035
000036     12  PM-CONTROL-PRIMARY.
000037         16  PM-COMPANY-CD                 PIC X.
000038         16  PM-ENTRY-BATCH                PIC X(6).
000039         16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
000040         16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
000041
000042     12  FILLER                            PIC X(14).
000043
000044     12  PM-ACCESS-CONTROL.
000045         16  PM-SOURCE-SYSTEM              PIC XX.
000046             88  PM-FROM-CREDIT                VALUE 'CR'.
000047             88  PM-FROM-VSI                   VALUE 'VS'.
000048             88  PM-FROM-WARRANTY              VALUE 'WA'.
000049             88  PM-FROM-OTHER                 VALUE 'OT'.
000050         16  PM-RECORD-ADD-DT              PIC XX.
000051         16  PM-RECORD-ADDED-BY            PIC XXXX.
000052         16  PM-LAST-MAINT-DT              PIC XX.
000053         16  PM-LAST-MAINT-BY              PIC XXXX.
000054         16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000055
000056     12  PM-PROFILE-INFO.
000057         16  PM-QUALIFY-CODE-1             PIC XX.
000058         16  PM-QUALIFY-CODE-2             PIC XX.
000059         16  PM-QUALIFY-CODE-3             PIC XX.
000060         16  PM-QUALIFY-CODE-4             PIC XX.
000061         16  PM-QUALIFY-CODE-5             PIC XX.
000062
000063         16  PM-INSURED-LAST-NAME          PIC X(15).
000064         16  PM-INSURED-FIRST-NAME         PIC X(10).
000065         16  PM-INSURED-MIDDLE-INIT        PIC X.
000066         16  PM-INSURED-ISSUE-AGE          PIC 99.
000067         16  PM-INSURED-BIRTH-DT           PIC XX.
000068         16  PM-INSURED-SEX                PIC X.
000069             88  PM-SEX-MALE                   VALUE 'M'.
000070             88  PM-SEX-FEMALE                 VALUE 'F'.
000071         16  PM-INSURED-SOC-SEC-NO         PIC X(11).
000072
000073         16  PM-ADDRESS-CORRECTED          PIC X.
000074         16  PM-JOINT-BIRTH-DT             PIC XX.
000075*        16  FILLER                        PIC X(12).
000076
000077         16  PM-ADDRESS-LINE-1             PIC X(30).
000078         16  PM-ADDRESS-LINE-2             PIC X(30).
000079         16  PM-CITY-STATE.
000080             20  PM-CITY                   PIC X(28).
000081             20  PM-STATE                  PIC XX.
000082         16  PM-ZIP.
000083             20  PM-ZIP-CODE.
000084                 24  PM-ZIP-1              PIC X.
000085                     88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000086                 24  FILLER                PIC X(4).
000087             20  PM-ZIP-PLUS4              PIC X(4).
000088         16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
000089             20  PM-CAN-POST1              PIC XXX.
000090             20  PM-CAN-POST2              PIC XXX.
000091             20  FILLER                    PIC XXX.
000092
000093         16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
000094         16  pm-city-st-zip-verified       pic x.
000095         16  FILLER                        PIC XX.
000096
000097     12  PM-CRED-BENE-INFO.
000098         16  PM-CRED-BENE-NAME             PIC X(25).
000099         16  PM-CRED-BENE-ADDR             PIC X(30).
000100         16  PM-CRED-BENE-ADDR2            PIC X(30).
000101         16  PM-CRED-BENE-CTYST.
000102             20  PM-CRED-BENE-CITY         PIC X(28).
000103             20  PM-CRED-BENE-STATE        PIC XX.
000104         16  PM-CRED-BENE-ZIP.
000105             20  PM-CB-ZIP-CODE.
000106                 24  PM-CB-ZIP-1           PIC X.
000107                     88  PM-CB-CANADIAN-POST-CODE
000108                                  VALUE 'A' THRU 'Z'.
000109                 24  FILLER                PIC X(4).
000110             20  PM-CB-ZIP-PLUS4           PIC X(4).
000111         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
000112             20  PM-CB-CAN-POST1           PIC XXX.
000113             20  PM-CB-CAN-POST2           PIC XXX.
000114             20  FILLER                    PIC XXX.
000115     12  PM-POST-CARD-MAIL-DATA.
000116         16  PM-MAIL-DATA OCCURS 7.
000117             20  PM-MAIL-TYPE              PIC X.
000118                 88  PM-12MO-MAILING           VALUE '1'.
000119                 88  PM-EXP-MAILING            VALUE '2'.
000120             20  PM-MAIL-STATUS            PIC X.
000121                 88  PM-MAIL-ST-MAILED         VALUE '1'.
000122                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
000123                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
000124             20  PM-MAIL-DATE              PIC XX.
000125     12  FILLER                            PIC XX.
000126     12  FILLER                            PIC X(12).
000127*    12  FILLER                            PIC X(30).
000128
000129******************************************************************
      *<<((file: ERCPNDM))
000810     EJECT
000811
000812 01  var                         pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL6301' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000813 VCOBOL-DUMMY-PROCEDURE.
000814
000815     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000816     MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
000817
000818     MOVE +2                     TO EMI-NUMBER-OF-LINES.
000819
000820     move spaces to ws-kix-myenv
000821     set P to address of KIXSYS
000822     CALL "getenv" using by value P returning var-ptr
000823     if var-ptr = null then
000824        display ' kixsys not set '
000825     else
000826        set address of var to var-ptr
000827        move 0 to env-var-len
000828        inspect var tallying env-var-len
000829          for characters before X'00'
000830        unstring var (1:env-var-len) delimited by '/'
000831           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000832              WS-KIX-SYS
000833        end-unstring
000834     end-if
000835     perform varying a1 from +1 by +1 until a1 > +10
000836        if ws-kix-myenv (a1:1) = low-values or high-values
000837           display ' found low or hi val '
000838           move spaces to ws-kix-myenv (a1:1)
000839        end-if
000840     end-perform
000841
000842     set P to address of KIXHOST
000843     CALL "getenv" using by value P returning var-ptr
000844     if var-ptr = null then
000845        display ' kixhost not set '
000846     else
000847        set address of var to var-ptr
000848        move 0 to env-var-len
000849        inspect var tallying env-var-len
000850          for characters before X'00'
000851        MOVE var(1:env-var-len)  to ws-kixhost
000852        DISPLAY ' WS KIX HOST ' WS-KIXHOST
000853     end-if.
000854
000855*    display ' env *' ws-kix-myenv '*'
000856     IF EIBCALEN = 0
000857         GO TO 8800-UNAUTHORIZED-ACCESS.
000858
000859     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000860     MOVE '5'                    TO DC-OPTION-CODE.
000861     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
000862     MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
000863     MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
000864     MOVE DC-GREG-DATE-1-MDY     TO WS-COMPARE-CURRENT-DT.
000865
000866     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000867         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000868             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000869             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000870             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000871             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000872             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000873             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000874             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000875             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000876         ELSE
000877             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000878             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000879             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000880             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000881             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000882             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000883             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000884             MOVE SPACES               TO PI-SAVED-PROGRAM-6.
000885
000886     MOVE LOW-VALUES             TO EL630BI.
000887
000888     IF EIBTRNID NOT = TRANS-EXA6
000889         MOVE ZEROS              TO PI-LF-ISS-ENTERED
000890                                    PI-LF-CAN-ENTERED
000891                                    PI-AH-ISS-ENTERED
000892                                    PI-AH-CAN-ENTERED
000893                                    PI-ISS-CNT-ENTERED
000894                                    PI-CAN-CNT-ENTERED
000895         IF PI-MAINT-FUNC = 'N'
000896            MOVE +0              TO PI-LAST-SEQ-NO-ADDED
000897            MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO
000898            IF PI-MAP-NAME = EL630B
000899               PERFORM 8550-SET-MAP-SEQ-NOS
000900               GO TO 8100-SEND-INITIAL-MAP
000901            ELSE
000902               PERFORM 8550-SET-MAP-SEQ-NOS
000903                       VARYING WS-SUB2 FROM 1 BY 1
000904                       UNTIL WS-SUB2   GREATER 4
000905                 GO TO 8100-SEND-INITIAL-MAP
000906         ELSE
000907             GO TO 3000-CONTINUE-ENTRY.
000908
000909     
      * EXEC CICS HANDLE CONDITION
000910*        PGMIDERR  (9600-PGMID-ERROR)
000911*        ERROR     (9990-ABEND)
000912*    END-EXEC.
      *    MOVE '"$L.                  ! " #00006327' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303036333237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000913
000914     IF EIBAID = DFHCLEAR
000915         MOVE SPACE TO PI-DISPLAY-SW
000916                       PI-BROWSE-SW
000917         GO TO 9400-CLEAR.
000918
000919     EJECT
000920
000921 0200-RECEIVE.
000922     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000923         MOVE ER-0008            TO EMI-ERROR
000924         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000925         IF PI-MAP-NAME = EL630B
000926             MOVE -1             TO BPFENTRL
000927             GO TO 8200-SEND-DATAONLY
000928         ELSE
000929             MOVE -1             TO CPFENTRL
000930             GO TO 8200-SEND-DATAONLY.
000931
000932     
      * EXEC CICS RECEIVE
000933*        MAP      (PI-MAP-NAME)
000934*        MAPSET   (MAPSET-EL6301S)
000935*        INTO     (EL630BI)
000936*    END-EXEC.
           MOVE LENGTH OF
            EL630BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006350' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036333530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL630BI, 
                 DFHEIV11, 
                 MAPSET-EL6301S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000937
000938     INSPECT EL630BI CONVERTING '_' TO ' '.
000939
000940     IF PI-MAP-NAME = EL630B
000941         IF BPFENTRL GREATER ZERO
000942             IF EIBAID NOT = DFHENTER
000943                 MOVE ER-0004    TO EMI-ERROR
000944                 MOVE AL-UNBOF   TO BPFENTRA
000945                 MOVE -1         TO BPFENTRL
000946                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000947                 GO TO 8200-SEND-DATAONLY
000948             ELSE
000949                 IF BPFENTRI NUMERIC AND
000950                    BPFENTRI GREATER 0 AND LESS 23
000951                     MOVE PF-VALUES (BPFENTRI) TO EIBAID
000952                 ELSE
000953                     MOVE ER-0029  TO EMI-ERROR
000954                     MOVE AL-UNBOF TO BPFENTRA
000955                     MOVE -1       TO BPFENTRL
000956                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000957                     GO TO 8200-SEND-DATAONLY
000958         ELSE
000959             NEXT SENTENCE
000960     ELSE
000961     IF PI-MAP-NAME = EL630C
000962         IF CPFENTRL GREATER ZERO
000963             IF EIBAID NOT = DFHENTER
000964                 MOVE ER-0004    TO EMI-ERROR
000965                 MOVE AL-UNBOF   TO CPFENTRA
000966                 MOVE -1         TO CPFENTRL
000967                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000968                 GO TO 8200-SEND-DATAONLY
000969             ELSE
000970                 IF CPFENTRI NUMERIC AND
000971                    CPFENTRI GREATER 0 AND LESS 23
000972                     MOVE PF-VALUES (CPFENTRI) TO EIBAID
000973                 ELSE
000974                     MOVE ER-0029  TO EMI-ERROR
000975                     MOVE AL-UNBOF TO BPFENTRA
000976                     MOVE -1       TO BPFENTRL
000977                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000978                     GO TO 8200-SEND-DATAONLY.
000979
000980     EJECT
000981******************************************************************
000982*   PF KEY FUNCTIONS:                                            *
000983*                                                                *
000984*   PF1 = BROWSE FOWARD                                          *
000985*   PF2 = BROWSE BACKWARD                                        *
000986*   PF3 = ADD ISSUE RECORD                                       *
000987*   PF4 = ADD CANCEL RECORD                                      *
000988*   PF5 = RESET TABS (OPEN PROTECTED FIELDS)                     *
000989*   PF6 = DELETE ENTRY                                           *
000990******************************************************************
000991
000992 0300-CHECK-PFKEYS.
000993     IF EIBAID = DFHPF12
000994         GO TO 9500-PF12.
000995
000996     IF EIBAID NOT = DFHPF5
000997        MOVE SPACE               TO PI-BROWSE-SW.
000998
000999     IF EIBAID = DFHENTER
001000         GO TO 1000-EDIT-MAPB.
001001
001002     IF EIBAID = DFHPF1
001003         MOVE 'Y'                TO PI-BROWSE-SW
001004         GO TO 2000-BROWSE-FWD.
001005
001006     IF EIBAID = DFHPF2
001007         MOVE 'Y'                TO PI-BROWSE-SW
001008         GO TO 2100-BROWSE-BKWD.
001009
001010     IF EIBAID = DFHPF3
001011         MOVE SPACE              TO PI-DISPLAY-SW
001012         MOVE LOW-VALUES         TO EL630BI
001013         ADD +1                     PI-LAST-SEQ-NO-ADDED
001014               GIVING PI-NEXT-DISPLAY-SEQ-NO
001015         MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
001016         MOVE EL630B             TO PI-MAP-NAME
001017         PERFORM 8550-SET-MAP-SEQ-NOS
001018         GO TO 8100-SEND-INITIAL-MAP.
001019
001020     IF EIBAID = DFHPF4
001021         MOVE SPACE              TO PI-DISPLAY-SW
001022         MOVE LOW-VALUES         TO MAP-C
001023         ADD +1                     PI-LAST-SEQ-NO-ADDED
001024               GIVING PI-NEXT-DISPLAY-SEQ-NO
001025         MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
001026         MOVE EL630C             TO PI-MAP-NAME
001027         PERFORM 8550-SET-MAP-SEQ-NOS
001028                VARYING WS-SUB2 FROM 1 BY 1
001029                UNTIL WS-SUB2 GREATER +4
001030         GO TO 8100-SEND-INITIAL-MAP.
001031
001032     IF EIBAID = DFHPf5
001033        IF PI-BROWSE
001034           IF PI-MAP-NAME = EL630B
001035              MOVE -1          TO BCERTL
001036              GO TO 8200-SEND-DATAONLY
001037           ELSE
001038              MOVE -1          TO CCERT-LEN (1)
001039              GO TO 8200-SEND-DATAONLY.
001040
001041     IF EIBAID = DFHPF5
001042        IF PI-MAP-NAME = EL630B
001043           PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT
001044           MOVE -1             TO BCERTL
001045           GO TO 8200-SEND-DATAONLY
001046        ELSE
001047           PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT
001048           ADD +1    PI-LAST-SEQ-NO-ADDED
001049                  GIVING PI-NEXT-DISPLAY-SEQ-NO
001050          MOVE -1             TO CCERT-LEN  (1)
001051          GO TO 8200-SEND-DATAONLY.
001052
001053     IF EIBAID = DFHPF6
001054         IF PI-LAST-FUNC-DISPLAY
001055             GO TO 6000-DELETE-PEND-BUS-RECORD
001056         ELSE
001057             MOVE ER-2594        TO EMI-ERROR
001058             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001059             IF PI-MAP-NAME = EL630B
001060                 MOVE -1         TO BPFENTRL
001061                 GO TO 8200-SEND-DATAONLY
001062             ELSE
001063                 MOVE -1         TO CPFENTRL
001064                 GO TO 8200-SEND-DATAONLY.
001065
001066     MOVE ER-0008 TO EMI-ERROR.
001067     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001068
001069     IF PI-MAP-NAME = EL630B
001070         MOVE -1                 TO BPFENTRL
001071     ELSE
001072         MOVE -1                 TO CPFENTRL.
001073
001074     GO TO 8200-SEND-DATAONLY.
001075
001076     EJECT
001077 0600-PROTECT-FIELDS.
001078     IF PI-COMPANY-ID = 'MON'
001079         MOVE AL-UANOF         TO BSFX-ATTRB
001080     ELSE
001081     IF PI-COMPANY-ID = 'PEM' OR
001082                        'CGL' OR
001083                        'TIH' OR
001084                        'TII' OR
001085                        'FGL' OR
001086                        'OFL'
001087        NEXT SENTENCE
001088     ELSE
001089        IF NOT PI-ISS-SUFFIX-KEYED
001090           MOVE AL-SANOF         TO BSFX-ATTRB.
001091
001092     IF PI-PROCESSOR-ID = 'LGXX'
001093        IF NOT PI-ISS-SUFFIX-KEYED
001094           MOVE AL-SANOF         TO BSFX-ATTRB.
001095
001096*    IF NOT PI-IG-KEYED
001097*        MOVE AL-SANOF           TO BIND-GRP-ATTRB.
001098
001099*    IF NOT PI-1ST-PMT-KEYED
001100*        MOVE AL-SANOF           TO B1ST-PMT-ATTRB.
001101
001102*    IF NOT PI-DAYS-KEYED
001103*        MOVE AL-SANOF           TO BDAYS-ATTRB.
001104
001105     IF PI-COMPANY-ID = 'PEM' OR 'CID' OR 'DCC' OR
001106                        'CGL' OR 'AHL' or 'VPP' or
001107                        'TIH' OR 'FNL' or
001108                        'TII' OR
001109                        'FGL' OR
001110                        'OFL'
001111        NEXT SENTENCE
001112     ELSE
001113        IF NOT PI-APR-KEYED
001114           MOVE AL-SANOF         TO BAPR-ATTRB.
001115
001116     IF NOT PI-VIN-KEYED
001117        MOVE AL-SANOF            TO BVIN-ATTRB
001118     END-IF
001119
001120     IF PI-PROCESSOR-ID = 'LGXX'
001121        IF NOT PI-APR-KEYED
001122           MOVE AL-SANOF         TO BAPR-ATTRB.
001123
001124*    IF NOT PI-FREQ-KEYED
001125*        MOVE AL-SANOF           TO BFREQ-ATTRB.
001126
001127*    IF PI-COMPANY-ID = 'PEM' OR
001128*                       'NCL' OR
001129*                       'CGL' OR
001130*                       'TIH' OR
001131*                       'TII' OR
001132*                       'FGL' OR
001133*                       'OFL' OR
001134*                       'TMS' OR
001135*                       'FLA' OR
001136*                       'CRI'
001137*       NEXT SENTENCE
001138*    ELSE
001139*       IF NOT PI-SIG-KEYED
001140*          MOVE AL-SANOF         TO BSIG-ATTRB.
001141
001142*    IF NOT PI-ISS-LIVES-KEYED
001143*        MOVE AL-SANOF           TO BLIVES-ATTRB.
001144
001145*    IF NOT PI-MEMBER-KEYED
001146*        MOVE AL-SANOF           TO BMEM-NO-ATTRB.
001147
001148*    IF PI-COMPANY-ID = 'HER'
001149*        NEXT SENTENCE
001150*    ELSE
001151*        IF NOT PI-MICRO-NO-KEYED
001152*            MOVE AL-SANOF       TO BMICROFILM-NO-ATTRB.
001153
001154*    IF NOT PI-MODE-KEYED
001155*        MOVE AL-SANOF           TO BMODE-ATTRB.
001156
001157*    IF NOT PI-PMTS-KEYED
001158*        MOVE AL-SANOF           TO BPMTS-ATTRB.
001159
001160     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
001161           OR 'FNL'
001162        CONTINUE
001163     ELSE
001164        IF NOT PI-LN-OFFICER-KEYED
001165           MOVE AL-SANOF         TO BLN-OFFICER-ATTRB
001166        END-IF
001167     END-IF
001168
001169     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
001170        OR 'FNL'
001171        CONTINUE
001172     ELSE
001173        IF NOT PI-LNTRM-KEYED
001174           MOVE AL-SANOF         TO BLN-TERM-ATTRB
001175        END-IF
001176     END-IF
001177
001178*    IF NOT PI-ENTRY-KEYED
001179*        MOVE AL-SANOF           TO BENTRY-ATTRB.
001180
001181*    IF NOT PI-FORCE-KEYED
001182*        MOVE AL-SANOF           TO BFORCE-ATTRB.
001183
001184*    IF NOT PI-BILLCD-KEYED
001185*        MOVE AL-SANOF           TO BBILLCD-ATTRB.
001186
001187*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
001188*       NEXT SENTENCE
001189*    ELSE
001190*       IF NOT PI-POLICY-KEYED
001191*          MOVE AL-SANOF           TO BPOLICY-ATTRB.
001192
001193*    IF NOT PI-RINCD-KEYED
001194*        MOVE AL-SANOF           TO BRINCD-ATTRB.
001195
001196*    IF NOT PI-RTCLS-KEYED
001197*        MOVE AL-SANOF           TO BRTCLS-ATTRB.
001198
001199*    IF PI-COMPANY-ID = 'PEM' OR
001200*                       'CGL' OR
001201*                       'TIH' OR
001202*                       'TII' OR
001203*                       'FGL' OR
001204*                       'OFL' OR
001205*                       'LBL' OR 'LGX'
001206*       NEXT SENTENCE
001207*    ELSE
001208*       IF NOT PI-EXPIRE-KEYED
001209*          MOVE AL-SANOF         TO BEXPIRE-ATTRB (1)
001210*                                   BEXPIRE-ATTRB (2).
001211*    IF PI-PROCESSOR-ID = 'LGXX'
001212*       IF NOT PI-EXPIRE-KEYED
001213*          MOVE AL-SANOF         TO BEXPIRE-ATTRB (1)
001214*                                   BEXPIRE-ATTRB (2).
001215
001216*    IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
001217*       CONTINUE
001218*    ELSE
001219        IF NOT PI-CRIT-PERD-KEYED
001220           MOVE AL-SANOF         TO BCRIT-PERD2-ATTRB
001221        END-IF
001222*    END-IF
001223
001224*    IF NOT PI-PMT-KEYED
001225*        MOVE AL-SANOF           TO BPMT-ATTRB.
001226
001227*    IF NOT PI-SKPCD-KEYED
001228*        MOVE AL-SANOF           TO BSKPCD-ATTRB.
001229
001230     IF PI-COMPANY-ID = 'CRI' OR 'LGX'
001231        OR 'FLA' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
001232        OR 'FNL'
001233        CONTINUE
001234     ELSE
001235        IF PI-BIRTH-DATE-IS-INPUT
001236           MOVE AL-SANOF         TO BAGE-ATTRB
001237        ELSE
001238           MOVE AL-SANOF         TO BBIRTH-ATTRB
001239                                    BJNTDOB-ATTRB
001240         END-IF
001241     END-IF
001242
001243     IF PI-COMPANY-ID = 'CRI' OR 'LGX'
001244         IF PI-BIRTHDT-KEYED  AND  NOT PI-AGE-KEYED
001245             MOVE AL-SANOF       TO BAGE-ATTRB.
001246
001247     IF PI-COMPANY-ID = 'CRI' OR 'LGX'
001248         IF PI-AGE-KEYED  AND  NOT PI-BIRTHDT-KEYED
001249             MOVE AL-SANOF       TO BBIRTH-ATTRB.
001250
001251     IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR
001252        'FGL' OR 'OFL'
001253        CONTINUE
001254     ELSE
001255        IF NOT PI-JNT-AGE-KEYED
001256           MOVE AL-SANOF         TO BJNT-AGE-ATTRB
001257        END-IF
001258     END-IF
001259
001260     IF NOT PI-JNT-NAME-KEYED
001261        MOVE AL-SANOF            TO BJNT-INIT-ATTRB
001262                                    BJNT-LST-NAME-ATTRB
001263                                    BJNT-1ST-NAME-ATTRB
001264                                    BJNTDOB-ATTRB
001265     end-if
001266
001267     IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
001268        OR 'FNL'
001269        NEXT SENTENCE
001270     ELSE
001271        IF NOT PI-BENEFICIARY-KEYED
001272            MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.
001273
001274     IF NOT PI-ALT-BEN-KEYED
001275         MOVE AL-SANOF           TO BALT-BEN1-ATTRB
001276     END-IF
001277
001278     IF NOT PI-ALT-PREM-KEYED
001279         MOVE AL-SANOF           TO BALT-PREM1-ATTRB
001280     END-IF
001281
001282     IF NOT PI-ALT-BEN-KEYED
001283         MOVE AL-SANOF           TO BALT-BEN2-ATTRB
001284     END-IF
001285
001286     IF NOT PI-ALT-PREM-KEYED
001287         MOVE AL-SANOF           TO BALT-PREM2-ATTRB
001288     END-IF
001289
001290     IF PI-PROCESSOR-ID = 'LGXX'
001291        CONTINUE
001292     ELSE
001293        IF PI-COMPANY-ID = 'PEM' OR
001294                           'CGL' OR
001295                           'TIH' OR
001296                           'TII' OR
001297                           'FGL' OR
001298                           'OFL'
001299           MOVE AL-SANOF            TO BADDRS2-ATTRB
001300*                                      BPHONE-ATTRB
001301        ELSE
001302           IF PI-COMPANY-ID ='CID' or 'AHL' OR 'FNL'
001303              MOVE AL-SANOF         TO BADDRS2-ATTRB
001304           END-IF
001305        END-IF
001306     END-IF
001307
001308     IF PI-COMPANY-ID = 'TMS'
001309*        MOVE AL-UANOF              TO BSIG-ATTRB
001310         MOVE AL-UANOF              TO BJNT-AGE-ATTRB
001311                                       BJNT-LST-NAME-ATTRB
001312                                       BJNT-1ST-NAME-ATTRB
001313                                       BJNT-INIT-ATTRB
001314                                       BAPR-ATTRB.
001315
001316 0600-EXIT.
001317     EXIT.
001318
001319     EJECT
001320 0610-UNPROTECT-FIELDS.
001321     IF PI-COMPANY-ID = 'PEM' OR
001322                        'CGL' OR
001323                        'TIH' OR
001324                        'TII' OR
001325                        'FGL' OR
001326                        'OFL'
001327        NEXT SENTENCE
001328     ELSE
001329        IF NOT PI-ISS-SUFFIX-KEYED
001330           MOVE AL-UANOF         TO BSFX-ATTRB.
001331
001332     IF PI-PROCESSOR-ID = 'LGXX'
001333        IF NOT PI-ISS-SUFFIX-KEYED
001334           MOVE AL-UANOF         TO BSFX-ATTRB.
001335
001336*    IF NOT PI-IG-KEYED
001337*        MOVE AL-UANOF           TO BIND-GRP-ATTRB.
001338
001339*    IF NOT PI-1ST-PMT-KEYED
001340*        MOVE AL-UANOF           TO B1ST-PMT-ATTRB.
001341
001342*    IF NOT PI-DAYS-KEYED
001343*        MOVE AL-UANOF           TO BDAYS-ATTRB.
001344
001345     IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR 'FGL'
001346        OR 'OFL' OR 'CID' OR 'DCC' or 'AHL' or 'VPP' OR 'FNL'
001347        CONTINUE
001348     ELSE
001349        IF NOT PI-APR-KEYED
001350           MOVE AL-UANOF         TO BAPR-ATTRB
001351        END-IF
001352     END-IF
001353
001354     IF PI-PROCESSOR-ID = 'LGXX'
001355        IF NOT PI-APR-KEYED
001356            MOVE AL-UANOF        TO BAPR-ATTRB.
001357
001358*    IF NOT PI-FREQ-KEYED
001359*        MOVE AL-UANOF           TO BFREQ-ATTRB.
001360
001361*    IF PI-COMPANY-ID = 'PEM' OR
001362*                       'NCL' OR
001363*                       'CGL' OR
001364*                       'TIH' OR
001365*                       'TII' OR
001366*                       'FGL' OR
001367*                       'OFL' OR
001368*                       'TMS' OR
001369*                       'FLA' OR
001370*                       'CRI'
001371*       NEXT SENTENCE
001372*    ELSE
001373*       IF NOT PI-SIG-KEYED
001374*          MOVE AL-UANOF           TO BSIG-ATTRB.
001375
001376*    IF NOT PI-ISS-LIVES-KEYED
001377*        MOVE AL-UANOF           TO BLIVES-ATTRB.
001378
001379*    IF NOT PI-MEMBER-KEYED
001380*        MOVE AL-UANOF           TO BMEM-NO-ATTRB.
001381
001382*    IF PI-COMPANY-ID = 'HER'
001383*        NEXT SENTENCE
001384*    ELSE
001385*        IF NOT PI-MICRO-NO-KEYED
001386*            MOVE AL-UNNOF       TO BMICROFILM-NO-ATTRB.
001387
001388*    IF NOT PI-MODE-KEYED
001389*        MOVE AL-UANOF           TO BMODE-ATTRB.
001390
001391*    IF NOT PI-PMTS-KEYED
001392*        MOVE AL-UANOF           TO BPMTS-ATTRB.
001393
001394     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
001395           OR 'FNL'
001396        CONTINUE
001397     ELSE
001398        IF NOT PI-LN-OFFICER-KEYED
001399           MOVE AL-UANOF         TO BLN-OFFICER-ATTRB
001400        END-IF
001401     END-IF
001402
001403*    IF NOT PI-ENTRY-KEYED
001404*        MOVE AL-UANOF           TO BENTRY-ATTRB.
001405
001406*    IF NOT PI-FORCE-KEYED
001407*        MOVE AL-UANOF           TO BFORCE-ATTRB.
001408
001409*    IF NOT PI-BILLCD-KEYED
001410*        MOVE AL-UANOF           TO BBILLCD-ATTRB.
001411
001412*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
001413*       NEXT SENTENCE
001414*    ELSE
001415*       IF NOT PI-POLICY-KEYED
001416*          MOVE AL-UANOF           TO BPOLICY-ATTRB.
001417
001418*    IF NOT PI-RINCD-KEYED
001419*        MOVE AL-UANOF           TO BRINCD-ATTRB.
001420
001421*    IF NOT PI-RTCLS-KEYED
001422*        MOVE AL-UANOF           TO BRTCLS-ATTRB.
001423
001424     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
001425        OR 'FNL'
001426        CONTINUE
001427     ELSE
001428        IF NOT PI-LNTRM-KEYED
001429           MOVE AL-UANOF         TO BLN-TERM-ATTRB
001430        END-IF
001431     END-IF
001432
001433*    IF PI-COMPANY-ID = 'LBL' OR 'LGX'
001434*        NEXT SENTENCE
001435*    ELSE
001436*        IF NOT PI-EXPIRE-KEYED
001437*            MOVE AL-UANOF       TO BEXPIRE-ATTRB (1)
001438*                                   BEXPIRE-ATTRB (2).
001439
001440*    IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
001441*       CONTINUE
001442*    ELSE
001443        IF NOT PI-CRIT-PERD-KEYED
001444           MOVE AL-UANOF         TO BCRIT-PERD2-ATTRB
001445        END-IF
001446*    END-IF
001447
001448*    IF NOT PI-PMT-KEYED
001449*        MOVE AL-UANOF           TO BPMT-ATTRB.
001450
001451*    IF NOT PI-SKPCD-KEYED
001452*        MOVE AL-UANOF           TO BSKPCD-ATTRB.
001453
001454     IF PI-COMPANY-ID = 'CRI' OR 'LGX' OR 'CID' OR 'DCC' or 'VPP'
001455        or 'AHL' OR 'FNL'
001456        CONTINUE
001457     ELSE
001458        IF PI-BIRTH-DATE-IS-INPUT
001459           MOVE AL-UANOF         TO BAGE-ATTRB
001460        ELSE
001461           MOVE AL-UANOF         TO BBIRTH-ATTRB
001462                                    BJNTDOB-ATTRB
001463        END-IF
001464     END-IF
001465
001466     IF PI-COMPANY-ID = 'FLA'
001467        IF BAGE-LEN NOT GREATER THAN +0
001468           MOVE AL-UANOF       TO BAGE-ATTRB.
001469
001470     IF PI-COMPANY-ID = 'FLA'
001471        IF BAGE-LEN NOT GREATER THAN +0
001472           MOVE AL-UANOF       TO BBIRTH-ATTRB.
001473
001474     IF PI-COMPANY-ID = 'CRI' OR
001475                        'LGX'
001476        IF NOT PI-AGE-KEYED
001477           IF BAGE-LEN NOT GREATER THAN +0
001478              MOVE AL-UANOF     TO BAGE-ATTRB.
001479
001480     IF PI-COMPANY-ID = 'CRI' OR
001481                        'LGX'
001482         IF NOT PI-BIRTHDT-KEYED
001483             MOVE AL-UANOF       TO BBIRTH-ATTRB.
001484
001485     IF PI-COMPANY-ID = 'PEM' OR
001486                        'CGL' OR
001487                        'TIH' OR
001488                        'TII' OR
001489                        'FGL' OR
001490                        'OFL'
001491        NEXT SENTENCE
001492     ELSE
001493        IF NOT PI-JNT-AGE-KEYED
001494           MOVE AL-UANOF           TO BJNT-AGE-ATTRB.
001495
001496     if not pi-vin-keyed
001497        move al-uanof            to bvin-attrb
001498     end-if
001499
001500     IF NOT PI-JNT-NAME-KEYED
001501        MOVE AL-UANOF            TO BJNT-INIT-ATTRB
001502                                    BJNT-LST-NAME-ATTRB
001503                                    BJNT-1ST-NAME-ATTRB
001504                                    BJNTDOB-ATTRB
001505     end-if
001506
001507     IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
001508        OR 'FNL'
001509        NEXT SENTENCE
001510     ELSE
001511        IF NOT PI-BENEFICIARY-KEYED
001512            MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.
001513
001514     IF NOT PI-ALT-BEN-KEYED
001515         MOVE AL-UANOF           TO BALT-BEN1-ATTRB
001516     END-IF
001517
001518     IF NOT PI-ALT-PREM-KEYED
001519         MOVE AL-UANOF           TO BALT-PREM1-ATTRB
001520     END-IF
001521
001522     if pi-company-id = 'DCC' or 'VPP'
001523        IF NOT PI-ALT-BEN-KEYED
001524           MOVE AL-UANOF         TO BALT-BEN2-ATTRB
001525        END-IF
001526        IF NOT PI-ALT-PREM-KEYED
001527           MOVE AL-UANOF         TO BALT-PREM2-ATTRB
001528        END-IF
001529     end-if
001530
001531     IF PI-PROCESSOR-ID = 'LGXX'
001532        CONTINUE
001533     ELSE
001534        IF PI-COMPANY-ID = 'PEM' OR
001535                           'CGL' OR
001536                           'TIH' OR
001537                           'TII' OR
001538                           'FGL' OR
001539                           'OFL'
001540           MOVE AL-UANOF            TO BADDRS2-ATTRB
001541*                                      BPHONE-ATTRB
001542        ELSE
001543           IF (PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL') AND
001544              (PI-MAIL-YES)
001545              MOVE AL-UANOF         TO BADDRS2-ATTRB
001546           END-IF
001547        END-IF
001548     END-IF
001549
001550     .
001551 0610-EXIT.
001552     EXIT.
001553
001554     EJECT
001555
001556 0700-PROTECT-FIELDS.
001557
001558     IF PI-COMPANY-ID = 'PEM' OR
001559                        'CGL' OR
001560                        'TIH' OR
001561                        'TII' OR
001562                        'FGL' OR
001563                        'OFL'
001564        NEXT SENTENCE
001565     ELSE
001566        IF NOT PI-CAN-SUFFIX-KEYED
001567           MOVE AL-SANOF         TO CSFX-ATTRB (1)
001568                                    CSFX-ATTRB (2)
001569                                    CSFX-ATTRB (3)
001570                                    CSFX-ATTRB (4).
001571
001572     IF PI-PROCESSOR-ID = 'LGXX'
001573        IF NOT PI-CAN-SUFFIX-KEYED
001574           MOVE AL-SANOF         TO CSFX-ATTRB (1)
001575                                    CSFX-ATTRB (2)
001576                                    CSFX-ATTRB (3)
001577                                    CSFX-ATTRB (4).
001578
001579     IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL' OR 'FNL'
001580        MOVE AL-SANOF            TO CLAST-NAME-ATTRB (1)
001581                                    CLAST-NAME-ATTRB (2)
001582                                    CLAST-NAME-ATTRB (3)
001583                                    CLAST-NAME-ATTRB (4).
001584
001585     IF NOT PI-CAN-LIVES-KEYED
001586        MOVE AL-SANOF            TO CLIVES-ATTRB (1)
001587                                    CLIVES-ATTRB (2)
001588                                    CLIVES-ATTRB (3)
001589                                    CLIVES-ATTRB (4).
001590
001591     IF NOT PI-CAN-REA-KEYED
001592        MOVE AL-SANOF            TO CCANREA-ATTRB (1)
001593                                    CCANREA-ATTRB (2)
001594                                    CCANREA-ATTRB (3)
001595                                    CCANREA-ATTRB (4)
001596     END-IF
001597
001598*    IF PI-COMPANY-ID = 'HER'
001599*        NEXT SENTENCE
001600*    ELSE
001601*        IF NOT PI-MICRO-NO-KEYED
001602*            MOVE AL-SANOF       TO CMICRO-NO-ATTRB (1)
001603*                                   CMICRO-NO-ATTRB (2)
001604*                                   CMICRO-NO-ATTRB (3)
001605*                                   CMICRO-NO-ATTRB (4).
001606
001607     IF NOT PI-PAYEE-KEYED
001608         MOVE AL-SANOF           TO CPAYEE-ATTRB (1)
001609                                    CPAYEE-ATTRB (2)
001610                                    CPAYEE-ATTRB (3)
001611                                    CPAYEE-ATTRB (4).
001612     IF NOT PI-CHK-REQ-KEYED
001613         MOVE AL-SANOF           TO CCHK-ATTRB   (1)
001614                                    CCHK-ATTRB   (2)
001615                                    CCHK-ATTRB   (3)
001616                                    CCHK-ATTRB   (4).
001617
001618     IF NOT PI-REFUND-MTHD-KEYED
001619         MOVE AL-SANOF           TO CMTHD1-ATTRB (1)
001620                                    CMTHD2-ATTRB (1)
001621                                    CMTHD1-ATTRB (2)
001622                                    CMTHD2-ATTRB (2)
001623                                    CMTHD1-ATTRB (3)
001624                                    CMTHD2-ATTRB (3)
001625                                    CMTHD1-ATTRB (4)
001626                                    CMTHD2-ATTRB (4).
001627
001628 0700-EXIT.
001629     EXIT.
001630
001631     EJECT
001632 0710-UNPROTECT-FIELDS.
001633     IF PI-COMPANY-ID = 'PEM' OR
001634                        'CGL' OR
001635                        'TIH' OR
001636                        'TII' OR
001637                        'FGL' OR
001638                        'OFL'
001639        NEXT SENTENCE
001640     ELSE
001641        IF NOT PI-CAN-SUFFIX-KEYED
001642         MOVE AL-UANOF           TO CSFX-ATTRB (1)
001643                                    CSFX-ATTRB (2)
001644                                    CSFX-ATTRB (3)
001645                                    CSFX-ATTRB (4).
001646
001647     IF PI-PROCESSOR-ID = 'LGXX'
001648        IF NOT PI-CAN-SUFFIX-KEYED
001649           MOVE AL-UANOF         TO CSFX-ATTRB (1)
001650                                    CSFX-ATTRB (2)
001651                                    CSFX-ATTRB (3)
001652                                    CSFX-ATTRB (4).
001653
001654     IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL' OR 'FNL'
001655        MOVE AL-UANOF            TO CLAST-NAME-ATTRB (1)
001656                                    CLAST-NAME-ATTRB (2)
001657                                    CLAST-NAME-ATTRB (3)
001658                                    CLAST-NAME-ATTRB (4).
001659
001660     IF NOT PI-CAN-LIVES-KEYED
001661         MOVE AL-UANOF           TO CLIVES-ATTRB (1)
001662                                    CLIVES-ATTRB (2)
001663                                    CLIVES-ATTRB (3)
001664                                    CLIVES-ATTRB (4).
001665
001666     IF NOT PI-CAN-REA-KEYED
001667        MOVE AL-UANOF            TO CCANREA-ATTRB (1)
001668                                    CCANREA-ATTRB (2)
001669                                    CCANREA-ATTRB (3)
001670                                    CCANREA-ATTRB (4)
001671     END-IF
001672
001673*    IF PI-COMPANY-ID = 'HER'
001674*        NEXT SENTENCE
001675*    ELSE
001676*        IF NOT PI-MICRO-NO-KEYED
001677*            MOVE AL-UNNOF       TO CMICRO-NO-ATTRB (1)
001678*                                   CMICRO-NO-ATTRB (2)
001679*                                   CMICRO-NO-ATTRB (3)
001680*                                   CMICRO-NO-ATTRB (4).
001681
001682     IF NOT PI-PAYEE-KEYED
001683         MOVE AL-UANOF           TO CPAYEE-ATTRB (1)
001684                                    CPAYEE-ATTRB (2)
001685                                    CPAYEE-ATTRB (3)
001686                                    CPAYEE-ATTRB (4).
001687     IF NOT PI-CHK-REQ-KEYED
001688         MOVE AL-UANOF           TO CCHK-ATTRB   (1)
001689                                    CCHK-ATTRB   (2)
001690                                    CCHK-ATTRB   (3)
001691                                    CCHK-ATTRB   (4).
001692
001693     IF NOT PI-REFUND-MTHD-KEYED
001694         MOVE AL-UANOF           TO CMTHD1-ATTRB (1)
001695                                    CMTHD2-ATTRB (1)
001696                                    CMTHD1-ATTRB (2)
001697                                    CMTHD2-ATTRB (2)
001698                                    CMTHD1-ATTRB (3)
001699                                    CMTHD2-ATTRB (3)
001700                                    CMTHD1-ATTRB (4)
001701                                    CMTHD2-ATTRB (4).
001702
001703 0710-EXIT.
001704     EXIT.
001705     EJECT
001706 1000-EDIT-MAPB.
001707     IF PI-MAP-NAME NOT = EL630B
001708         GO TO 1100-EDIT-MAPC.
001709
001710     IF PI-LAST-FUNC-DISPLAY
001711       AND BSFX-LEN           = ZEROS
001712       AND B1ST-NAME-LEN      = ZEROS
001713       AND BLAST-NAME-LEN     = ZEROS
001714       AND BINIT-LEN          = ZEROS
001715       AND BJNT-1ST-NAME-LEN  = ZEROS
001716       AND BJNT-INIT-LEN      = ZEROS
001717       AND BJNT-LST-NAME-LEN  = ZEROS
001718       AND BAGE-LEN           = ZEROS
001719*      AND BIND-GRP-LEN       = ZEROS
001720       AND BAPR-LEN           = ZEROS
001721*      AND BFREQ-LEN          = ZEROS
001722*      AND BSIG-LEN           = ZEROS
001723       AND BTERM1-LEN         = ZEROS
001724       AND BTERM2-LEN         = ZEROS
001725       AND BTYPE1-LEN         = ZEROS
001726       AND BTYPE2-LEN         = ZEROS
001727       AND BBENE1-LEN         = ZEROS
001728       AND BBENE2-LEN         = ZEROS
001729       AND BALT-BEN1-LEN      = ZEROS
001730       AND BPREM1-LEN         = ZEROS
001731       AND BPREM2-LEN         = ZEROS
001732       AND BALT-PREM1-LEN     = ZEROS
001733       AND BVIN-LEN           = ZEROS
001734*      AND BLIVES-LEN         = ZEROS
001735*      AND BPOLICY-LEN        = ZEROS
001736*      AND BENTRY-LEN         = ZEROS
001737*      AND BFORCE-LEN         = ZEROS
001738*      AND BRINCD-LEN         = ZEROS
001739*      AND BBILLCD-LEN        = ZEROS
001740*      AND BMEM-NO-LEN        = ZEROS
001741*      AND BMICROFILM-NO-LEN  = ZEROS
001742       AND BJNT-AGE-LEN       = ZEROS
001743       AND BBENEFICIARY-LEN   = ZEROS
001744       AND BCADDR1-LEN        = ZEROS
001745       AND BCADDR2-LEN        = ZEROS
001746       AND BCCITY-LEN         = ZEROS
001747       AND BCSTATE-LEN        = ZEROS
001748       AND BCZIPCD-LEN        = ZEROS
001749       AND BBIRTH-LEN         = ZEROS
001750       AND BJNTDOB-LEN        = ZEROS
001751*      AND BMODE-LEN          = ZEROS
001752*      AND BPMTS-LEN          = ZEROS
001753       AND BLN-OFFICER-LEN    = ZEROS
001754*      AND BDAYS-LEN          = ZEROS
001755       AND BLN-TERM-LEN       = ZEROS
001756*      AND BEXPIRE-LEN (1)    = ZEROS
001757*      AND BEXPIRE-LEN (2)    = ZEROS
001758*      AND BPMT-LEN           = ZEROS
001759       AND B1ST-PMT-LEN       = ZEROS
001760*      AND BSKPCD-LEN         = ZEROS
001761       AND BADDRS1-LEN        = ZEROS
001762       AND BADDRS2-LEN        = ZEROS
001763       AND BCITY-LEN          = ZEROS
001764       AND BSTATE-LEN         = ZEROS
001765       AND BZIPCDE-LEN        = ZEROS
001766       AND BAGE-LEN           = ZEROS
001767*      AND BZIP4-LEN          = ZEROS
001768*      AND BPHONE-LEN         = ZEROS
001769         MOVE SPACE              TO PI-DISPLAY-SW
001770         GO TO 1030-NOTHING-TO-EDIT.
001771
001772 1010-EDIT-MAPB.
001773     IF BCERT-LEN             = ZEROS
001774       AND BLAST-NAME-LEN     = ZEROS
001775       AND BEFFDT-LEN         = ZEROS
001776       AND NOT PI-LAST-FUNC-DISPLAY
001777         GO TO 1030-NOTHING-TO-EDIT.
001778
001779     MOVE AL-SABON               TO BSEQ-ATTRB.
001780
001781     IF BCERT-LEN  GREATER ZEROS
001782       AND PI-LAST-FUNC-DISPLAY
001783         NEXT SENTENCE
001784     ELSE
001785         IF BCERT-LEN  GREATER ZEROS
001786             MOVE AL-UANON       TO BCERT-ATTRB
001787         ELSE
001788             MOVE -1             TO BCERT-LEN
001789             MOVE ER-2218        TO EMI-ERROR
001790             MOVE AL-UABON       TO BCERT-ATTRB
001791             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001792
001793     IF BSFX-LEN  NOT = ZEROS
001794         MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW
001795         MOVE AL-UANON           TO BSFX-ATTRB.
001796
001797     IF BEFFDT-LEN  = ZEROS
001798       AND PI-LAST-FUNC-DISPLAY
001799         NEXT SENTENCE
001800     ELSE
001801         IF BEFFDT-LEN   GREATER ZEROS
001802             MOVE AL-UNNON           TO BEFFDT-ATTRB
001803             IF BEFFDT   NUMERIC
001804                 MOVE 4              TO DC-OPTION-CODE
001805                 MOVE BEFFDT    TO DC-GREG-DATE-1-MDY
001806                 PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
001807                 MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT
001808                 IF NO-CONVERSION-ERROR
001809                     IF WS-CONVERTED-EFFDT NOT LESS
001810                       PI-ACCT-LOW-EFF-DT  AND LESS
001811                       PI-ACCT-HIGH-EXP-DT
001812                         PERFORM 1500-EDIT-ACCT-DT-RANGES THRU
001813                                 1590-EXIT
001814                     ELSE
001815                         MOVE 'Y' TO PI-FIN-RESP-ERROR-SW
001816*                        MOVE -1       TO BEFFDT-LEN
001817*                        MOVE ER-2589  TO EMI-ERROR
001818*                        MOVE AL-UNBON TO BEFFDT-ATTRB
001819*                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001820                 ELSE
001821                     MOVE -1         TO BEFFDT-LEN
001822                     MOVE ER-2226    TO EMI-ERROR
001823                     MOVE AL-UNBON   TO BEFFDT-ATTRB
001824                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001825             ELSE
001826                 MOVE -1             TO BEFFDT-LEN
001827                 MOVE ER-2223        TO EMI-ERROR
001828                 MOVE AL-UNBON       TO BEFFDT-ATTRB
001829                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001830         ELSE
001831             MOVE -1                 TO BEFFDT-LEN
001832             MOVE ER-2220            TO EMI-ERROR
001833             MOVE AL-UNBON           TO BEFFDT-ATTRB
001834             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001835
001836*    IF PI-COMPANY-ID = ('PEM' OR 'CRI')
001837*      AND NOT PI-LAST-FUNC-DISPLAY
001838*       IF BCERT      = BCERTV   AND
001839*          BSFX       = BSFXV    AND
001840*          BEFFDT     = BEFFDTV  AND
001841*          BLAST-NAME = BLAST-NAMEV
001842*          NEXT SENTENCE
001843*       ELSE
001844*          MOVE -1               TO BCERT-LEN
001845*          MOVE -1               TO BLAST-NAME-LEN
001846*          MOVE ER-3166          TO EMI-ERROR
001847*          MOVE AL-UNBON         TO BCERT-ATTRB
001848*                                   BCERTV-ATTRB
001849*                                   BSFX-ATTRB
001850*                                   BSFXV-ATTRB
001851*                                   BEFFDT-ATTRB
001852*                                   BEFFDTV-ATTRB
001853*                                   BLAST-NAME-ATTRB
001854*                                   BLAST-NAMEV-ATTRB
001855*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001856
001857     IF BLAST-NAME-LEN   GREATER ZEROS
001858         MOVE AL-UANON           TO BLAST-NAME-ATTRB.
001859
001860     IF B1ST-NAME-LEN    GREATER ZEROS
001861         MOVE AL-UANON           TO B1ST-NAME-ATTRB.
001862
001863     IF BINIT-LEN        GREATER ZEROS
001864         MOVE AL-UANON           TO BINIT-ATTRB.
001865
001866     IF BAGE-LEN > 0
001867        MOVE 'Y'                 TO PI-AGE-KEYED-SW
001868        IF BAGE NUMERIC
001869           MOVE BAGE             TO WS-BAGE
001870           MOVE AL-UNNON         TO BAGE-ATTRB
001871        ELSE
001872           MOVE -1             TO BAGE-LEN
001873           MOVE ER-2223        TO EMI-ERROR
001874           MOVE AL-UNBON       TO BAGE-ATTRB
001875           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001876
001877     MOVE +0                     TO WS-SUB1
001878
001879     .
001880 1020-EDIT-COVERAGES.
001881     IF NOT MODIFY-CAP
001882          MOVE 'UPDATE'       TO SM-READ
001883          PERFORM 9995-SECURITY-VIOLATION
001884          MOVE ER-0070        TO EMI-ERROR
001885          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001886          GO TO 8100-SEND-INITIAL-MAP.
001887
001888     ADD +1                      TO WS-SUB1.
001889
001890*    IF WS-SUB1 GREATER +2
001891*       GO TO 1025-CONT-EDIT.
001892
001893*    IF BTYPE1-LEN       > ZEROS OR
001894*       BTYPE2-LEN       > ZEROS OR
001895*       BTERM1-LEN       > ZEROS OR
001896*       BTERM2-LEN       > ZEROS OR
001897*       BBENE1-LEN       > ZEROS OR
001898*       BBENE2-LEN       > ZEROS OR
001899*       BPREM1-LEN       > ZEROS OR
001900*       BPREM2-LEN       > ZEROS OR
001901*       BCRIT-PERD2-LEN  > ZEROS OR
001902*       BEXPIRE-LEN      > ZEROS OR
001903*       BALT-PREM1-LEN   > ZEROS OR
001904*       BALT-BEN1-LEN    > ZEROS OR
001905*       BALT-PREM2-LEN   > ZEROS OR
001906*       BALT-BEN2-LEN    > ZEROS
001907*       MOVE 'Y'                 TO WS-DATA-KEYED-SW
001908*    ELSE
001909*       GO TO 1020-EDIT-COVERAGES
001910*    END-IF
001911
001912     IF BTYPE1-LEN       > ZEROS OR
001913        BTERM1-LEN       > ZEROS OR
001914        BBENE1-LEN       > ZEROS OR
001915        BPREM1-LEN       > ZEROS OR
001916        BALT-PREM1-LEN   > ZEROS OR
001917        BALT-BEN1-LEN    > ZEROS
001918        MOVE 'Y'                 TO WS-DATA-KEYED-SW
001919     ELSE
001920        GO TO 1020-EDIT-BENEFIT-2
001921     END-IF
001922
001923     MOVE +1                     TO WS-SUB1
001924     IF NOT PI-LAST-FUNC-DISPLAY
001925        IF BTYPE1-LEN  > ZEROS
001926           MOVE AL-UANON         TO BTYPE1-ATTRB
001927           PERFORM 1040-EDIT-INPUT-CODE
001928                                 THRU 1059-EXIT
001929        END-IF
001930     ELSE
001931        IF BTYPE1-LEN > ZEROS
001932           IF BTYPE1 = SPACES OR ZEROS
001933              MOVE AL-UANON      TO BTYPE1-ATTRB
001934           ELSE
001935              MOVE AL-UANON      TO BTYPE1-ATTRB
001936              PERFORM 1040-EDIT-INPUT-CODE
001937                                 THRU 1059-EXIT
001938           END-IF
001939        END-IF
001940     END-IF
001941
001942*    IF BPMTS-LEN GREATER ZEROS
001943*        MOVE 'Y'                TO PI-PMTS-KEYED-SW
001944*        MOVE BPMTS-IN           TO DEEDIT-FIELD
001945*        PERFORM 8600-DEEDIT
001946*        IF DEEDIT-FIELD-V0 NUMERIC
001947*           MOVE DEEDIT-FIELD-V0 TO WS-BPMTS
001948*           MOVE AL-UNNON        TO BPMTS-ATTRB.
001949
001950*    IF BPMT-LEN GREATER ZEROS
001951*        MOVE 'Y'                TO PI-PMT-KEYED-SW
001952*        EXEC CICS BIF DEEDIT
001953*            FIELD  (BPMTI)
001954*            LENGTH (9)
001955*        END-EXEC
001956*        IF BPMTI NUMERIC
001957*          MOVE BPMTI           TO WS-BPMT
001958*          MOVE AL-UNNON        TO BPMT-ATTRB.
001959
001960*    IF BDAYS-LEN GREATER ZEROS
001961*       MOVE 'Y'                TO PI-DAYS-KEYED-SW
001962*       MOVE BDAYSI             TO DEEDIT-FIELD
001963*       PERFORM 8600-DEEDIT
001964*       IF DEEDIT-FIELD-V0 NUMERIC
001965*          MOVE DEEDIT-FIELD-V0  TO WS-BDAYS
001966*          MOVE AL-UNNON        TO BDAYS-ATTRB
001967*       ELSE
001968*          MOVE -1              TO BDAYS-LEN
001969*          MOVE ER-7530         TO EMI-ERROR
001970*          MOVE AL-UNBON        TO BDAYS-ATTRB
001971*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001972
001973     IF BTERM1-LEN > ZEROS
001974        CONTINUE
001975     ELSE
001976        IF BLN-TERM-LEN > ZERO  AND
001977           WS-BLN-TERM NUMERIC
001978           MOVE WS-BLN-TERM      TO BTERM1I
001979           MOVE +3               TO BTERM1-LEN
001980        END-IF
001981     END-IF
001982
001983*    IF  WS-TERM-IN-DAYS-FOUND  AND
001984*        BMODE-LEN   GREATER ZERO
001985*          PERFORM 1090-CALCULATE-MONTHLY-TERM THRU 1094-EXIT
001986*       ELSE
001987*          IF BMODE-LEN   GREATER ZEROS AND
001988*             BPMTS-LEN   GREATER ZEROS
001989*              PERFORM 1080-TERM-CONVERSION THRU 1089-EXIT.
001990
001991     IF PI-LAST-FUNC-DISPLAY
001992        IF BTERM1-LEN  = ZEROS
001993           CONTINUE
001994        ELSE
001995           MOVE BTERM1I          TO DEEDIT-FIELD
001996           PERFORM 8600-DEEDIT
001997           IF DEEDIT-FIELD-V0 NUMERIC
001998              MOVE DEEDIT-FIELD-V0
001999                                 TO WS-BTERM1
002000              IF WS-BTERM1 > ZERO
002001                 MOVE AL-UNNON   TO BTERM1-ATTRB
002002              ELSE
002003                 MOVE ER-2241    TO EMI-ERROR
002004                 MOVE -1         TO BTERM1-LEN
002005                 MOVE AL-UNBOF   TO BTERM1-ATTRB
002006                 PERFORM 9900-ERROR-FORMAT
002007                                 THRU 9900-EXIT
002008              END-IF
002009           ELSE
002010              MOVE ER-2223       TO EMI-ERROR
002011              MOVE -1            TO BTERM1-LEN
002012              MOVE AL-UNBON      TO BTERM1-ATTRB
002013              PERFORM 9900-ERROR-FORMAT
002014                                 THRU 9900-EXIT
002015           END-IF
002016        END-IF
002017     ELSE
002018        IF BTERM1-LEN > ZEROS
002019           MOVE BTERM1I          TO DEEDIT-FIELD
002020           PERFORM 8600-DEEDIT
002021           IF DEEDIT-FIELD-V0      NUMERIC
002022              IF DEEDIT-FIELD-V0 > ZERO
002023                 MOVE DEEDIT-FIELD-V0 TO WS-BTERM1
002024                 MOVE AL-UNNON        TO BTERM1-ATTRB
002025              ELSE
002026                 MOVE ER-2241         TO EMI-ERROR
002027                 MOVE -1              TO BTERM1-LEN
002028                 MOVE AL-UNBOF        TO BTERM1-ATTRB
002029                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002030              END-IF
002031           ELSE
002032              MOVE ER-2223             TO EMI-ERROR
002033              MOVE -1                  TO BTERM1-LEN
002034              MOVE AL-UNBON            TO BTERM1-ATTRB
002035              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002036           END-IF
002037        ELSE
002038           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002039              MOVE ER-2240             TO EMI-ERROR
002040              MOVE -1                  TO BTERM1-LEN
002041              MOVE AL-UNBOF            TO BTERM1-ATTRB
002042              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002043           END-IF
002044        END-IF
002045     END-IF
002046
002047
002048     IF BBENE1-LEN = ZEROS
002049        AND PI-LAST-FUNC-DISPLAY
002050        CONTINUE
002051     ELSE
002052        IF BBENE1-LEN > ZEROS
002053           MOVE AL-UNNON           TO BBENE1-ATTRB
002054           
      * EXEC CICS BIF DEEDIT
002055*              FIELD  (BBENE1I)
002056*              LENGTH (12)
002057*          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007472' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BBENE1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002058           IF BBENE1I NUMERIC
002059              IF BBENE1I > ZEROS
002060                 MOVE BBENE1I TO WS-BBEN1
002061*          MOVE BBENE1I           TO DEEDIT-FIELD
002062*          PERFORM 8600-DEEDIT
002063*          IF DEEDIT-FIELD-V2  NUMERIC
002064*             IF DEEDIT-FIELD-V2 > ZEROS
002065*                MOVE DEEDIT-FIELD-V2 TO WS-BBEN1
002066              ELSE
002067                 MOVE ER-7632    TO EMI-ERROR
002068                 MOVE -1         TO BBENE1-LEN
002069                 MOVE AL-UNBOF   TO BBENE1-ATTRB
002070                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002071              END-IF
002072           ELSE
002073              MOVE ER-2223         TO EMI-ERROR
002074              MOVE AL-UNBON        TO BBENE1-ATTRB
002075              MOVE -1              TO BBENE1-LEN
002076           END-IF
002077        ELSE
002078           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002079             MOVE ER-7632    TO EMI-ERROR
002080             MOVE -1         TO BBENE1-LEN
002081             MOVE AL-UNBOF   TO BBENE1-ATTRB
002082             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002083           END-IF
002084        END-IF
002085     END-IF
002086
002087
002088     IF BPREM1-LEN = ZEROS
002089        AND PI-LAST-FUNC-DISPLAY
002090        CONTINUE
002091     ELSE
002092        IF BPREM1-LEN > ZEROS
002093           MOVE AL-UNNON           TO BPREM1-ATTRB
002094*          MOVE BPREM1I            TO DEEDIT-FIELD
002095*          PERFORM 8600-DEEDIT
002096*          IF DEEDIT-FIELD-V2  NUMERIC
002097*             IF DEEDIT-FIELD-V2 GREATER ZEROS
002098*                MOVE DEEDIT-FIELD-V2 TO WS-BPREM1
002099           
      * EXEC CICS BIF DEEDIT
002100*              FIELD  (BPREM1I)
002101*              LENGTH (11)
002102*          END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007517' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPREM1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002103           IF BPREM1I NUMERIC
002104              IF BPREM1I > ZEROS
002105                 MOVE BPREM1I TO WS-BPREM1
002106              ELSE
002107                 MOVE ER-7633    TO EMI-ERROR
002108                 MOVE -1         TO BPREM1-LEN
002109                 MOVE AL-UNBOF   TO BPREM1-ATTRB
002110                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002111              END-IF
002112           ELSE
002113              MOVE AL-UNBON   TO BPREM1-ATTRB
002114              MOVE ER-2223         TO EMI-ERROR
002115              MOVE -1              TO BPREM1-LEN
002116           END-IF
002117        ELSE
002118           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002119              MOVE ER-7633    TO EMI-ERROR
002120              MOVE -1         TO BPREM1-LEN
002121              MOVE AL-UNBOF   TO BPREM1-ATTRB
002122              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002123           END-IF
002124        END-IF
002125     END-IF
002126
002127     IF BALT-BEN1-LEN = ZEROS
002128        AND PI-LAST-FUNC-DISPLAY
002129        CONTINUE
002130     ELSE
002131        IF BALT-BEN1-LEN > ZEROS
002132           MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW
002133           MOVE AL-UNNON         TO BALT-BEN1-ATTRB
002134*          MOVE BALT-BEN1I       TO DEEDIT-FIELD
002135*          PERFORM 8600-DEEDIT
002136*          IF DEEDIT-FIELD-V2  NUMERIC
002137*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN1
002138           
      * EXEC CICS BIF DEEDIT
002139*              FIELD  (BALT-BEN1I)
002140*              LENGTH (12)
002141*          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007556' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-BEN1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002142           IF BALT-BEN1I NUMERIC
002143              MOVE BALT-BEN1I TO WS-BALT-BEN1
002144           ELSE
002145              MOVE ER-2223       TO EMI-ERROR
002146              MOVE AL-UNBON      TO BALT-BEN1-ATTRB
002147              MOVE -1            TO BALT-BEN1-LEN
002148           END-IF
002149        END-IF
002150     END-IF
002151
002152     IF BALT-PREM1-LEN = ZEROS
002153        AND PI-LAST-FUNC-DISPLAY
002154        NEXT SENTENCE
002155     ELSE
002156        IF BALT-PREM1-LEN > ZEROS
002157           MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW
002158           MOVE AL-UNNON         TO BALT-PREM1-ATTRB
002159*          MOVE BALT-PREM1I      TO DEEDIT-FIELD
002160*          PERFORM 8600-DEEDIT
002161*          IF DEEDIT-FIELD-V2  NUMERIC
002162*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM1
002163           
      * EXEC CICS BIF DEEDIT
002164*              FIELD  (BALT-PREM1I)
002165*              LENGTH (9)
002166*          END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007581' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-PREM1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002167           IF BALT-PREM1I NUMERIC
002168              MOVE BALT-PREM1I TO WS-BALT-PREM1
002169           ELSE
002170              MOVE AL-UNBON      TO BALT-PREM1-ATTRB
002171              MOVE ER-2223       TO EMI-ERROR
002172              MOVE -1            TO BALT-PREM1-LEN
002173           END-IF
002174        END-IF
002175     END-IF
002176
002177     .
002178 1020-EDIT-BENEFIT-2.
002179
002180     IF BTYPE2-LEN       > ZEROS OR
002181        BTERM2-LEN       > ZEROS OR
002182        BBENE2-LEN       > ZEROS OR
002183        BPREM2-LEN       > ZEROS OR
002184        BCRIT-PERD2-LEN  > ZEROS OR
002185        BALT-PREM2-LEN   > ZEROS OR
002186        BALT-BEN2-LEN    > ZEROS
002187        MOVE 'Y'                 TO WS-DATA-KEYED-SW
002188     ELSE
002189        GO TO 1025-CONT-EDIT
002190     END-IF
002191
002192     MOVE +2                     TO WS-SUB1
002193     IF NOT PI-LAST-FUNC-DISPLAY
002194        IF BTYPE2-LEN  > ZEROS
002195           MOVE AL-UANON         TO BTYPE2-ATTRB
002196           PERFORM 1040-EDIT-INPUT-CODE
002197                                 THRU 1059-EXIT
002198        END-IF
002199     ELSE
002200        IF BTYPE2-LEN > ZEROS
002201           IF BTYPE2 = SPACES OR ZEROS
002202              MOVE AL-UANON      TO BTYPE2-ATTRB
002203           ELSE
002204              MOVE AL-UANON      TO BTYPE2-ATTRB
002205              PERFORM 1040-EDIT-INPUT-CODE
002206                                 THRU 1059-EXIT
002207           END-IF
002208        END-IF
002209     END-IF
002210
002211     IF BTERM2-LEN > ZEROS
002212        CONTINUE
002213     ELSE
002214        IF BLN-TERM-LEN > ZERO  AND
002215           WS-BLN-TERM NUMERIC
002216           MOVE WS-BLN-TERM      TO BTERM2I
002217           MOVE +3               TO BTERM2-LEN
002218        END-IF
002219     END-IF
002220
002221     IF PI-LAST-FUNC-DISPLAY
002222        IF BTERM2-LEN  = ZEROS
002223           CONTINUE
002224        ELSE
002225           MOVE BTERM2I          TO DEEDIT-FIELD
002226           PERFORM 8600-DEEDIT
002227           IF DEEDIT-FIELD-V0 NUMERIC
002228              MOVE DEEDIT-FIELD-V0
002229                                 TO WS-BTERM2
002230              IF WS-BTERM2 > ZERO
002231                 MOVE AL-UNNON   TO BTERM2-ATTRB
002232              ELSE
002233                 MOVE ER-2241    TO EMI-ERROR
002234                 MOVE -1         TO BTERM2-LEN
002235                 MOVE AL-UNBOF   TO BTERM2-ATTRB
002236                 PERFORM 9900-ERROR-FORMAT
002237                                 THRU 9900-EXIT
002238              END-IF
002239           ELSE
002240              MOVE ER-2223       TO EMI-ERROR
002241              MOVE -1            TO BTERM2-LEN
002242              MOVE AL-UNBON      TO BTERM2-ATTRB
002243              PERFORM 9900-ERROR-FORMAT
002244                                 THRU 9900-EXIT
002245           END-IF
002246        END-IF
002247     ELSE
002248        IF BTERM2-LEN > ZEROS
002249           MOVE BTERM2I          TO DEEDIT-FIELD
002250           PERFORM 8600-DEEDIT
002251           IF DEEDIT-FIELD-V0      NUMERIC
002252              IF DEEDIT-FIELD-V0 > ZERO
002253                 MOVE DEEDIT-FIELD-V0 TO WS-BTERM2
002254                 MOVE AL-UNNON        TO BTERM2-ATTRB
002255              ELSE
002256                 MOVE ER-2241         TO EMI-ERROR
002257                 MOVE -1              TO BTERM2-LEN
002258                 MOVE AL-UNBOF        TO BTERM2-ATTRB
002259                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002260              END-IF
002261           ELSE
002262              MOVE ER-2223             TO EMI-ERROR
002263              MOVE -1                  TO BTERM2-LEN
002264              MOVE AL-UNBON            TO BTERM2-ATTRB
002265              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002266           END-IF
002267        ELSE
002268           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002269              MOVE ER-2240             TO EMI-ERROR
002270              MOVE -1                  TO BTERM2-LEN
002271              MOVE AL-UNBOF            TO BTERM2-ATTRB
002272              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002273           END-IF
002274        END-IF
002275     END-IF
002276
002277     IF BBENE2-LEN = ZEROS
002278        AND PI-LAST-FUNC-DISPLAY
002279        CONTINUE
002280     ELSE
002281        IF BBENE2-LEN > ZEROS
002282           MOVE AL-UNNON           TO BBENE2-ATTRB
002283           
      * EXEC CICS BIF DEEDIT
002284*              FIELD  (BBENE2I)
002285*              LENGTH (12)
002286*          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007701' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037373031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BBENE2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002287           IF BBENE2I NUMERIC
002288              IF BBENE2I > ZEROS
002289                 MOVE BBENE2I TO WS-BBEN2
002290*          MOVE BBENE2I           TO DEEDIT-FIELD
002291*          PERFORM 8600-DEEDIT
002292*          IF DEEDIT-FIELD-V2  NUMERIC
002293*             IF DEEDIT-FIELD-V2 > ZEROS
002294*                MOVE DEEDIT-FIELD-V2 TO WS-BBEN2
002295              ELSE
002296                 MOVE ER-7632    TO EMI-ERROR
002297                 MOVE -1         TO BBENE2-LEN
002298                 MOVE AL-UNBOF   TO BBENE2-ATTRB
002299                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002300              END-IF
002301           ELSE
002302              MOVE ER-2223         TO EMI-ERROR
002303              MOVE AL-UNBON        TO BBENE2-ATTRB
002304              MOVE -1              TO BBENE2-LEN
002305           END-IF
002306        ELSE
002307           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002308             MOVE ER-7632    TO EMI-ERROR
002309             MOVE -1         TO BBENE2-LEN
002310             MOVE AL-UNBOF   TO BBENE2-ATTRB
002311             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002312           END-IF
002313        END-IF
002314     END-IF
002315
002316     IF BPREM2-LEN = ZEROS
002317        AND PI-LAST-FUNC-DISPLAY
002318        CONTINUE
002319     ELSE
002320        IF BPREM2-LEN > ZEROS
002321           MOVE AL-UNNON           TO BPREM2-ATTRB
002322*          MOVE BPREM2I            TO DEEDIT-FIELD
002323*          PERFORM 8600-DEEDIT
002324*          IF DEEDIT-FIELD-V2  NUMERIC
002325*             IF DEEDIT-FIELD-V2 GREATER ZEROS
002326*                MOVE DEEDIT-FIELD-V2 TO WS-BPREM2
002327           
      * EXEC CICS BIF DEEDIT
002328*              FIELD  (BPREM2I)
002329*              LENGTH (11)
002330*          END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007745' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPREM2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002331           IF BPREM2I NUMERIC
002332              IF BPREM2I > ZEROS
002333                 MOVE BPREM2I TO WS-BPREM2
002334              ELSE
002335                 MOVE ER-7633    TO EMI-ERROR
002336                 MOVE -1         TO BPREM2-LEN
002337                 MOVE AL-UNBOF   TO BPREM2-ATTRB
002338                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002339              END-IF
002340           ELSE
002341              MOVE AL-UNBON   TO BPREM2-ATTRB
002342              MOVE ER-2223         TO EMI-ERROR
002343              MOVE -1              TO BPREM2-LEN
002344           END-IF
002345        ELSE
002346           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
002347              MOVE ER-7633    TO EMI-ERROR
002348              MOVE -1         TO BPREM2-LEN
002349              MOVE AL-UNBOF   TO BPREM2-ATTRB
002350              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002351           END-IF
002352        END-IF
002353     END-IF
002354
002355*    IF BEXPIRE-LEN (WS-SUB1)   GREATER ZEROS
002356*       MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW
002357*        IF BEXPIRE (WS-SUB1)    NUMERIC
002358*            MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)
002359*            MOVE 4              TO DC-OPTION-CODE
002360*            MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY
002361*            PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
002362*            MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)
002363*            IF NO-CONVERSION-ERROR
002364*                NEXT SENTENCE
002365*            ELSE
002366*                MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)
002367*                MOVE ER-2531    TO EMI-ERROR
002368*                MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)
002369*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002370*        ELSE
002371*            MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)
002372*            MOVE ER-2532        TO EMI-ERROR
002373*            MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)
002374*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002375
002376     IF BCRIT-PERD2-LEN > ZEROS
002377        MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW
002378        MOVE BCRIT-PERD2I            TO DEEDIT-FIELD
002379        PERFORM 8600-DEEDIT
002380        IF DEEDIT-FIELD-V0 NUMERIC
002381           MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD2
002382           MOVE AL-UNNON             TO BCRIT-PERD2-ATTRB
002383        ELSE
002384           MOVE -1                   TO BCRIT-PERD2-LEN
002385           MOVE AL-UNBON             TO BCRIT-PERD2-ATTRB
002386           MOVE ER-2223              TO EMI-ERROR
002387           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002388        END-IF
002389     END-IF
002390
002391     IF BALT-BEN2-LEN = ZEROS
002392        AND PI-LAST-FUNC-DISPLAY
002393        CONTINUE
002394     ELSE
002395        IF BALT-BEN2-LEN > ZEROS
002396           MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW
002397           MOVE AL-UNNON         TO BALT-BEN2-ATTRB
002398*          MOVE BALT-BEN2I       TO DEEDIT-FIELD
002399*          PERFORM 8600-DEEDIT
002400*          IF DEEDIT-FIELD-V2  NUMERIC
002401*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN2
002402           
      * EXEC CICS BIF DEEDIT
002403*              FIELD  (BALT-BEN2I)
002404*              LENGTH (12)
002405*          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007820' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037383230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-BEN2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002406           IF BALT-BEN2I NUMERIC
002407              MOVE BALT-BEN2I TO WS-BALT-BEN2
002408           ELSE
002409              MOVE ER-2223       TO EMI-ERROR
002410              MOVE AL-UNBON      TO BALT-BEN2-ATTRB
002411              MOVE -1            TO BALT-BEN2-LEN
002412           END-IF
002413        END-IF
002414     END-IF
002415
002416     IF BALT-PREM2-LEN = ZEROS
002417        AND PI-LAST-FUNC-DISPLAY
002418        NEXT SENTENCE
002419     ELSE
002420        IF BALT-PREM2-LEN > ZEROS
002421           MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW
002422           MOVE AL-UNNON         TO BALT-PREM2-ATTRB
002423*          MOVE BALT-PREM2I      TO DEEDIT-FIELD
002424*          PERFORM 8600-DEEDIT
002425*          IF DEEDIT-FIELD-V2  NUMERIC
002426*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM2
002427           
      * EXEC CICS BIF DEEDIT
002428*              FIELD  (BALT-PREM2I)
002429*              LENGTH (9)
002430*          END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007845' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037383435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-PREM2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002431           IF BALT-PREM2I NUMERIC
002432              MOVE BALT-PREM2I TO WS-BALT-PREM2
002433           ELSE
002434              MOVE AL-UNBON      TO BALT-PREM2-ATTRB
002435              MOVE ER-2223       TO EMI-ERROR
002436              MOVE -1            TO BALT-PREM2-LEN
002437           END-IF
002438        END-IF
002439     END-IF
002440
002441*    GO TO 1020-EDIT-COVERAGES.
002442     .
002443 1025-CONT-EDIT.
002444*    IF BLIVES-LEN               GREATER ZEROS
002445*       MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW
002446*       MOVE BLIVESI               TO DEEDIT-FIELD
002447*       PERFORM 8600-DEEDIT
002448*       IF DEEDIT-FIELD-V0 NUMERIC
002449*          MOVE DEEDIT-FIELD-V0    TO WS-BLIVES
002450*          MOVE AL-UNNON           TO BLIVES-ATTRB
002451*       ELSE
002452*          MOVE -1                 TO BLIVES-LEN
002453*          MOVE AL-UNBON           TO BLIVES-ATTRB
002454*          MOVE ER-2223            TO EMI-ERROR
002455*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002456
002457     IF (BSTATE-LEN > 0)
002458        AND (BSTATE NOT = '  ' AND '00')
002459        MOVE AL-UANON            TO BSTATE-ATTRB
002460        MOVE SPACES              TO ELCNTL-KEY
002461        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
002462        MOVE '3'                 TO ELCNTL-REC-TYPE
002463        MOVE BSTATE              TO ELCNTL-ACCESS
002464        MOVE +0                  TO ELCNTL-SEQ
002465        
      * EXEC CICS READ
002466*          DATASET   (FILE-ID-ELCNTL)
002467*          SET       (ADDRESS OF CONTROL-FILE)
002468*          RIDFLD    (ELCNTL-KEY)
002469*          RESP      (WS-RESPONSE)
002470*       END-EXEC
      *    MOVE '&"S        E          (  N#00007883' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037383833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002471        IF RESP-NORMAL
002472           CONTINUE
002473        ELSE
002474           MOVE ER-2963          TO EMI-ERROR
002475*          MOVE -1               TO BSTATE-LEN
002476           MOVE -1               TO BPFENTRL
002477           MOVE AL-UABON         TO BSTATE-ATTRB
002478           PERFORM 9900-ERROR-FORMAT
002479                                 THRU 9900-EXIT
002480           SUBTRACT +1 FROM EMI-FATAL-CTR
002481        END-IF
002482*    ELSE
002483*       MOVE ER-2209          TO EMI-ERROR
002484*       MOVE -1               TO BSTATE-LEN
002485*       MOVE AL-UABON         TO BSTATE-ATTRB
002486*       PERFORM 9900-ERROR-FORMAT
002487*                             THRU 9900-EXIT
002488*       SUBTRACT +1 FROM EMI-FATAL-CTR
002489     END-IF
002490
002491     IF BCSTATE-LEN > 0
002492        MOVE SPACES              TO ELCNTL-KEY
002493        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
002494        MOVE '3'                 TO ELCNTL-REC-TYPE
002495        MOVE BCSTATE             TO ELCNTL-ACCESS
002496        MOVE +0                  TO ELCNTL-SEQ
002497        
      * EXEC CICS READ
002498*          DATASET   (FILE-ID-ELCNTL)
002499*          SET       (ADDRESS OF CONTROL-FILE)
002500*          RIDFLD    (ELCNTL-KEY)
002501*          RESP      (WS-RESPONSE)
002502*       END-EXEC
      *    MOVE '&"S        E          (  N#00007915' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037393135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002503        IF RESP-NORMAL
002504           CONTINUE
002505        ELSE
002506           MOVE ER-2964          TO EMI-ERROR
002507*          MOVE -1               TO BCSTATE-LEN
002508           MOVE -1               TO BPFENTRL
002509           MOVE AL-UABON         TO BCSTATE-ATTRB
002510           PERFORM 9900-ERROR-FORMAT
002511                                 THRU 9900-EXIT
002512           SUBTRACT +1 FROM EMI-FATAL-CTR
002513        END-IF
002514     END-IF
002515
002516     if bvin-len <> zeros
002517        move 'Y'                 to pi-vin-keyed-sw
002518        move al-uanon            to bvin-attrb
002519     end-if
002520
002521     IF BJNT-1ST-NAME-LEN     GREATER ZEROS OR
002522        BJNT-INIT-LEN         GREATER ZEROS OR
002523        BJNT-LST-NAME-LEN     GREATER ZEROS or
002524        BJNTDOB-len           greater zeros
002525         MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW
002526         MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB
002527                                    BJNT-INIT-ATTRB
002528                                    BJNT-LST-NAME-ATTRB
002529                                    BJNTDOB-ATTRB.
002530
002531     IF BJNT-AGE-LEN GREATER ZEROS
002532        MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW
002533        IF BJNT-AGE NUMERIC
002534           MOVE BJNT-AGE         TO WS-BJNT-AGE
002535           MOVE AL-UNNON         TO BJNT-AGE-ATTRB
002536        ELSE
002537           MOVE -1             TO BJNT-AGE-LEN
002538           MOVE ER-2223        TO EMI-ERROR
002539           MOVE AL-UNBON       TO BJNT-AGE-ATTRB
002540           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002541
002542*    IF BMICROFILM-NO-LEN  GREATER  ZEROS
002543*        MOVE 'Y'                TO  PI-MICRO-NO-KEYED-SW
002544*        MOVE BMICROFILM-NOI     TO  DEEDIT-FIELD
002545*        PERFORM 8600-DEEDIT
002546*        IF DEEDIT-FIELD-V0  NUMERIC
002547*            MOVE DEEDIT-FIELD-V0
002548*                                TO  WS-I-MICRO-NO
002549*            MOVE AL-UNNON       TO  BMICROFILM-NO-ATTRB
002550*        ELSE
002551*            MOVE -1             TO  BMICROFILM-NO-LEN
002552*            MOVE AL-UNBON       TO  BMICROFILM-NO-ATTRB
002553*            MOVE ER-2701        TO  EMI-ERROR
002554*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002555
002556     IF BBENEFICIARY-LEN GREATER ZEROS
002557         MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW
002558         MOVE AL-UANON           TO BBENEFICIARY-ATTRB.
002559
002560     IF B1ST-PMT-LEN GREATER ZEROS
002561        MOVE 'Y'                     TO PI-1ST-PMT-KEYED-SW
002562        IF B1ST-PMT = SPACES
002563           MOVE LOW-VALUES           TO WS-CONVERTED-1ST-PMT-DT
002564        ELSE
002565           MOVE B1ST-PMT             TO DEEDIT-FIELD
002566           PERFORM 8600-DEEDIT
002567           MOVE DEEDIT-FIELD-V0      TO DC-GREG-DATE-1-MDY
002568           MOVE AL-UNNON             TO B1ST-PMT-ATTRB
002569           MOVE 4                    TO DC-OPTION-CODE
002570           PERFORM 8500-DATE-CONVERT
002571           IF NO-CONVERSION-ERROR
002572              MOVE DC-BIN-DATE-1     TO WS-CONVERTED-1ST-PMT-DT
002573              MOVE AL-UANON          TO B1ST-PMT-ATTRB
002574           ELSE
002575              MOVE -1                TO B1ST-PMT-LEN
002576              MOVE ER-2200           TO EMI-ERROR
002577              MOVE AL-UNBON          TO B1ST-PMT-ATTRB
002578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002579
002580     IF BLN-TERM-LEN GREATER ZEROS
002581        MOVE 'Y'                 TO PI-LNTRM-KEYED-SW
002582        MOVE BLN-TERMI           TO DEEDIT-FIELD
002583        PERFORM 8600-DEEDIT
002584        IF DEEDIT-FIELD-V0 NUMERIC
002585           MOVE DEEDIT-FIELD-V0  TO WS-BLN-TERM
002586           IF WS-BLN-TERM  GREATER ZERO
002587              MOVE AL-UNNON TO BLN-TERM-ATTRB
002588                 ELSE
002589                     MOVE ER-2241  TO EMI-ERROR
002590                     MOVE -1       TO BLN-TERM-LEN
002591                     MOVE AL-UNBOF TO BLN-TERM-ATTRB
002592                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002593             ELSE
002594                 MOVE ER-2223      TO EMI-ERROR
002595                 MOVE -1           TO BLN-TERM-LEN
002596                 MOVE AL-UNBON     TO BLN-TERM-ATTRB
002597                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002598
002599     IF BLN-OFFICER-LEN GREATER ZEROS
002600         MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW
002601         MOVE AL-UANON           TO BLN-OFFICER-ATTRB.
002602
002603*    IF BMODE-LEN GREATER ZEROS
002604*        MOVE BMODE              TO WS-MODE-CODE
002605*        MOVE 'Y'                TO PI-MODE-KEYED-SW
002606*        IF WS-MODE-CODE-VALID
002607*            MOVE AL-UANON       TO BMODE-ATTRB
002608*        ELSE
002609*            MOVE -1             TO BMODE-LEN
002610*            MOVE ER-2591        TO EMI-ERROR
002611*            MOVE AL-UABON       TO BMODE-ATTRB
002612*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002613
002614*    IF BFREQ-LEN GREATER ZEROS
002615*       MOVE 'Y'                 TO PI-FREQ-KEYED-SW
002616*       MOVE BFREQI              TO DEEDIT-FIELD
002617*       PERFORM 8600-DEEDIT
002618*       IF DEEDIT-FIELD-V0 NUMERIC
002619*          MOVE DEEDIT-FIELD-V0  TO WS-BFREQ
002620*          MOVE AL-UNNON         TO BFREQ-ATTRB
002621*       ELSE
002622*          MOVE -1             TO BFREQ-LEN
002623*          MOVE ER-2223        TO EMI-ERROR
002624*          MOVE AL-UNBON       TO BFREQ-ATTRB
002625*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002626
002627*    IF BPMTS-LEN GREATER ZEROS
002628*        MOVE 'Y'                TO PI-PMTS-KEYED-SW
002629*        MOVE BPMTS-IN           TO DEEDIT-FIELD
002630*        PERFORM 8600-DEEDIT
002631*        IF DEEDIT-FIELD-V0 NUMERIC
002632*           MOVE DEEDIT-FIELD-V0 TO WS-BPMTS
002633*           MOVE AL-UNNON        TO BPMTS-ATTRB
002634*        ELSE
002635*            MOVE -1             TO BPMTS-LEN
002636*            MOVE ER-2592        TO EMI-ERROR
002637*            MOVE AL-UNBON       TO BPMTS-ATTRB
002638*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002639*
002640*    IF BPMT-LEN GREATER ZEROS
002641*        MOVE 'Y'                TO PI-PMT-KEYED-SW
002642*        MOVE BPMTI              TO DEEDIT-FIELD
002643*        PERFORM 8600-DEEDIT
002644*        IF DEEDIT-FIELD-V2     NUMERIC
002645*          MOVE DEEDIT-FIELD-V2 TO WS-BPMT
002646*          MOVE AL-UNNON           TO BPMT-ATTRB
002647*        EXEC CICS BIF DEEDIT
002648*            FIELD  (BPMTI)
002649*            LENGTH (9)
002650*        END-EXEC
002651*        IF BPMTI NUMERIC
002652*          MOVE BPMTI              TO WS-BPMT
002653*          MOVE AL-UNNON           TO BPMT-ATTRB
002654*        ELSE
002655*          MOVE -1                 TO BPMT-LEN
002656*          MOVE ER-2529            TO EMI-ERROR
002657*          MOVE AL-UNBON           TO BPMT-ATTRB
002658*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002659
002660*    IF BRINCD-LEN              GREATER ZEROS
002661*        MOVE 'Y'                TO PI-RINCD-KEYED-SW
002662*        MOVE AL-UANON           TO BRINCD-ATTRB.
002663*
002664*    IF BENTRY-LEN           GREATER ZEROS
002665*        MOVE 'Y'                TO PI-ENTRY-KEYED-SW
002666*        MOVE BENTRY             TO WS-ENTRY-CODE
002667*        IF WS-ENTRY-CODE-VALID
002668*           MOVE AL-UANON       TO BENTRY-ATTRB
002669*        ELSE
002670*           MOVE ER-2224        TO EMI-ERROR
002671*           MOVE -1             TO BENTRY-LEN
002672*           MOVE AL-UABON       TO BENTRY-ATTRB
002673*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002674
002675*    IF BFORCE-LEN           GREATER ZEROS
002676*        MOVE 'Y'                TO PI-FORCE-KEYED-SW
002677*        MOVE BFORCE             TO WS-FORCE-CODE
002678*        IF WS-FORCE-CODE-VALID
002679*           MOVE AL-UANON       TO BFORCE-ATTRB
002680*        ELSE
002681*           MOVE ER-2670        TO EMI-ERROR
002682*           MOVE -1             TO BFORCE-LEN
002683*           MOVE AL-UABON       TO BFORCE-ATTRB
002684*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002685
002686*    IF BBILLCD-LEN             GREATER ZEROS
002687*        MOVE 'Y'                TO PI-BILLCD-KEYED-SW
002688*        MOVE AL-UANON           TO BBILLCD-ATTRB.
002689
002690*    IF  BSKPCD-LEN          GREATER ZEROS
002691*        MOVE 'Y'                TO PI-SKPCD-KEYED-SW
002692*        MOVE BSKPCD             TO WS-SKIP-CODE
002693*        IF WS-SKIP-CODE-VALID
002694*            MOVE AL-UANON       TO BSKPCD-ATTRB
002695*        ELSE
002696*            MOVE -1             TO BSKPCD-LEN
002697*            MOVE ER-2683        TO EMI-ERROR
002698*            MOVE AL-UABON       TO BSKPCD-ATTRB
002699*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002700
002701*    IF BIND-GRP-LEN GREATER ZEROS
002702*        MOVE 'Y'                TO PI-IG-KEYED-SW
002703*        MOVE AL-UANON           TO BIND-GRP-ATTRB.
002704
002705*    IF BSIG-LEN GREATER ZEROS
002706*        MOVE 'Y'                TO PI-SIG-KEYED-SW
002707*        IF PI-COMPANY-ID = 'CRI'
002708*            IF BSIG = 'S' OR 'J' OR ' '
002709*                MOVE AL-UANON   TO BSIG-ATTRB
002710*            ELSE
002711*                MOVE -1         TO BSIG-LEN
002712*                MOVE ER-2702    TO EMI-ERROR
002713*                MOVE AL-UABON   TO BSIG-ATTRB
002714*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002715*        ELSE
002716*        IF PI-COMPANY-ID = 'NCL'
002717*            IF BSIG = 'S' OR 'J' OR 'N' OR 'Y' OR ' '
002718*                MOVE AL-UANON   TO BSIG-ATTRB
002719*            ELSE
002720*                MOVE -1         TO BSIG-LEN
002721*                MOVE ER-1923    TO EMI-ERROR
002722*                MOVE AL-UABON   TO BSIG-ATTRB
002723*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002724*        ELSE
002725*        IF PI-COMPANY-ID = 'TMS'
002726*            IF BSIG = 'N' OR 'U' OR 'O' OR ' '
002727*                MOVE AL-UANON   TO BSIG-ATTRB
002728*            ELSE
002729*                MOVE -1         TO BSIG-LEN
002730*                MOVE ER-2700    TO EMI-ERROR
002731*                MOVE AL-UABON   TO BSIG-ATTRB
002732*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002733*        ELSE
002734*            IF BSIG   = 'Y' OR ' '
002735*                MOVE AL-UANON   TO BSIG-ATTRB
002736*            ELSE
002737*                MOVE -1         TO BSIG-LEN
002738*                MOVE ER-2651    TO EMI-ERROR
002739*                MOVE AL-UABON   TO BSIG-ATTRB
002740*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002741
002742*    IF BPOLICY-LEN GREATER ZEROS
002743*        MOVE 'Y'                TO PI-POLICY-KEYED-SW
002744*        MOVE AL-UANON           TO BPOLICY-ATTRB.
002745
002746*    IF BRTCLS-LEN GREATER ZEROS
002747*        MOVE 'Y'                TO PI-RTCLS-KEYED-SW
002748*        MOVE AL-UANON           TO BRTCLS-ATTRB.
002749
002750     IF BAPR-LEN > 0
002751        MOVE 'Y'                 TO PI-APR-KEYED-SW
002752        MOVE +0                  TO  WS-BAPR
002753        PERFORM VARYING WS-SUB FROM +1 BY +1 UNTIL
002754           (WS-SUB > 7)
002755           OR (BAPR-IN (WS-SUB:1) NUMERIC)
002756           OR (BAPR-IN (WS-SUB:1) = '.')
002757        END-PERFORM
002758        IF WS-SUB > 7
002759           MOVE ER-2471       TO EMI-ERROR
002760           MOVE -1            TO BAPR-LEN
002761           MOVE AL-UNBON      TO BAPR-ATTRB
002762           PERFORM 9900-ERROR-FORMAT
002763                                 THRU 9900-EXIT
002764        ELSE
002765           IF BAPR-IN (WS-SUB:1) NUMERIC
002766              MOVE WS-SUB           TO WS-BEG
002767              PERFORM VARYING WS-SUB FROM WS-SUB BY +1 UNTIL
002768                 (WS-SUB > 7)
002769                 OR (BAPR-IN (WS-SUB:1) NOT NUMERIC)
002770              END-PERFORM
002771              COMPUTE WS-END = WS-SUB - WS-BEG
002772              MOVE BAPR-IN (WS-BEG:WS-END)
002773                                 TO WS-APR-WHOLE-NUM
002774           END-IF
002775           IF BAPR-IN (WS-SUB:1) = '.'
002776              COMPUTE WS-BEG = WS-SUB + 1
002777              IF (WS-BEG < 8)
002778                 AND (BAPR-IN (WS-BEG:1) NUMERIC)
002779                 PERFORM VARYING WS-SUB FROM WS-BEG BY +1 UNTIL
002780                    (WS-SUB > 7)
002781                    OR (BAPR-IN (WS-SUB:1) NOT NUMERIC)
002782                 END-PERFORM
002783                 COMPUTE WS-END = WS-SUB - WS-BEG
002784                 MOVE BAPR-IN (WS-BEG:WS-END)
002785                                 TO WS-APR-DEC (1:WS-END)
002786              END-IF
002787           END-IF
002788           MOVE AL-UNNON      TO BAPR-ATTRB
002789           MOVE WS-BAPR       TO BAPR-OUT
002790        END-IF
002791     END-IF
002792
002793     IF BJNTDOB-LEN > ZEROS
002794        MOVE 'Y'                 TO PI-1ST-PMT-KEYED-SW
002795        IF BJNTDOB-DT = SPACES
002796           MOVE LOW-VALUES       TO WS-CONVERTED-BIRTH (2)
002797        ELSE
002798           MOVE BJNTDOB-DT       TO DEEDIT-FIELD
002799           PERFORM 8600-DEEDIT
002800           MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
002801           MOVE AL-UNNON         TO BJNTDOB-ATTRB
002802           MOVE 4                TO DC-OPTION-CODE
002803           PERFORM 8500-DATE-CONVERT
002804           IF NO-CONVERSION-ERROR
002805              IF DC-BIN-DATE-1 > WS-CONVERTED-EFFDT
002806                 MOVE DC-BIN-DATE-1 TO WS-WORK-BIN-DT
002807                 SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED
002808                 MOVE WS-WORK-BIN-DT TO DC-BIN-DATE-1
002809              END-IF
002810              MOVE DC-BIN-DATE-1 TO WS-CONVERTED-BIRTH (2)
002811              MOVE AL-UANON      TO BJNTDOB-ATTRB
002812           ELSE
002813              MOVE -1            TO BJNTDOB-LEN
002814              MOVE ER-2228       TO EMI-ERROR
002815              MOVE AL-UNBON      TO BJNTDOB-ATTRB
002816              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002817           END-IF
002818        END-IF
002819     END-IF
002820
002821     IF BBIRTH-LEN GREATER ZEROS
002822        NEXT SENTENCE
002823     ELSE
002824        GO TO 1027-CHECK-FUNCTION.
002825
002826     IF BBIRTH-DT NOT NUMERIC
002827        GO TO 1026-CHECK-ERROR.
002828
002829     MOVE 'Y'                    TO PI-BIRTHDT-KEYED-SW
002830     MOVE AL-UNNON               TO BBIRTH-ATTRB
002831     MOVE 4                      TO DC-OPTION-CODE
002832     MOVE BBIRTH-DT              TO DC-GREG-DATE-1-MDY
002833     MOVE DC-GREG-DATE-1-MDY     TO WS-SAVE-BIRTH-DATE
002834     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
002835     MOVE DC-BIN-DATE-1          TO WS-CONVERTED-BIRTH (1)
002836     IF (DATE-CONVERSION-ERROR)
002837*    IF (DATE-CONVERSION-ERROR) OR
002838*       ((DC-BIN-DATE-1 GREATER THAN WS-CONVERTED-EFFDT) AND
002839*       (WS-SAVE-BIRTH-YR GREATER THAN '22'))
002840        MOVE -1                  TO BBIRTH-LEN
002841        MOVE ER-2228             TO EMI-ERROR
002842        MOVE AL-UNBON            TO BBIRTH-ATTRB
002843        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002844        GO TO 1027-CHECK-FUNCTION
002845     END-IF
002846
002847     IF (NO-CONVERSION-ERROR) AND
002848        (DC-BIN-DATE-1 GREATER THAN WS-CONVERTED-EFFDT)
002849        MOVE DC-BIN-DATE-1       TO WS-WORK-BIN-DT
002850        SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED
002851        MOVE WS-WORK-BIN-DT      TO DC-BIN-DATE-1
002852                                    WS-CONVERTED-BIRTH (1)
002853        MOVE AL-UANON            TO BBIRTH-ATTRB
002854     ELSE
002855        MOVE DC-BIN-DATE-1       TO WS-CONVERTED-BIRTH (1)
002856        MOVE AL-UANON            TO BBIRTH-ATTRB
002857     END-IF
002858
002859
002860*    IF BAGE-LEN   = ZERO
002861*       PERFORM 1095-CALC-AGE THRU 1099-EXIT.
002862
002863     GO TO 1028-CHECK-MEMNO.
002864
002865 1026-CHECK-ERROR.
002866
002867        IF PI-BIRTH-DATE-IS-INPUT
002868           MOVE ER-2442    TO EMI-ERROR
002869           MOVE -1         TO BBIRTH-LEN
002870           MOVE AL-UNBON   TO BBIRTH-ATTRB
002871           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002872        ELSE
002873          MOVE -1         TO BBIRTH-LEN
002874          MOVE ER-2223    TO EMI-ERROR
002875          MOVE AL-UNBON   TO BBIRTH-ATTRB
002876          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002877
002878 1027-CHECK-FUNCTION.
002879
002880     IF PI-LAST-FUNC-DISPLAY
002881        NEXT SENTENCE
002882     ELSE
002883        IF PI-COMPANY-ID NOT = 'CRI' AND 'LGX'
002884           AND 'CID' AND 'DCC' and 'AHL' and 'VPP' AND 'FNL'
002885           IF PI-BIRTH-DATE-IS-INPUT
002886              MOVE ER-2442       TO EMI-ERROR
002887              MOVE -1            TO BBIRTH-LEN
002888              MOVE AL-UNBON      TO BBIRTH-ATTRB
002889              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002890          ELSE
002891              MOVE AL-SANOF      TO BBIRTH-ATTRB.
002892
002893 1028-CHECK-MEMNO.
002894
002895*    IF BMEM-NO-LEN GREATER ZEROS
002896*        MOVE 'Y'                TO PI-MEMBER-KEYED-SW
002897*        MOVE AL-UANON           TO BMEM-NO-ATTRB.
002898
002899*    IF  BPHONE-LEN GREATER ZEROS
002900*        MOVE BPHONE             TO DEEDIT-FIELD
002901*        PERFORM 8600-DEEDIT
002902*        MOVE DEEDIT-FIELD-V0    TO WS-BPHONE
002903*        MOVE AL-UANON           TO BPHONE-ATTRB.
002904
002905*    IF BENTRYI = 'D' OR 'V'
002906*       GO TO 1029-CHECK-ERRORS.
002907
002908     IF NOT PI-LAST-FUNC-DISPLAY
002909        AND WS-DATA-NOT-KEYED
002910        MOVE ER-7400             TO EMI-ERROR
002911        MOVE -1                  TO BTYPE1-LEN
002912        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002913        MOVE 'Y'                 TO PI-ERROR-SW.
002914
002915 1029-CHECK-ERRORS.
002916
002917     IF EMI-ERROR = ZEROS
002918         MOVE 'Y'                TO PI-UPDATE-SW
002919         MOVE SPACE              TO PI-DISPLAY-SW
002920         PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
002921     ELSE
002922        IF (EMI-FATAL-CTR = ZEROS)
002923           AND (EMI-FORCABLE-CTR = ZEROS)
002924           MOVE 'Y'              TO PI-UPDATE-SW
002925           MOVE SPACE            TO PI-DISPLAY-SW
002926           PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
002927           MOVE 'Y'              TO PI-ERROR-SW
002928        ELSE
002929           MOVE ZEROS              TO EMI-ERROR
002930           MOVE 'Y'                TO PI-ERROR-SW
002931        END-IF
002932     END-IF
002933
002934     .
002935 1030-NOTHING-TO-EDIT.
002936     IF PI-DATA-ERRORS
002937         MOVE AL-SABON           TO BSEQ-ATTRB
002938         GO TO 8200-SEND-DATAONLY.
002939
002940     IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED
002941         SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
002942         MOVE ER-0000        TO EMI-ERROR
002943         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002944         GO TO 2000-BROWSE-FWD.
002945
002946     MOVE LOW-VALUES     TO EL630BI.
002947     ADD +1                 PI-LAST-SEQ-NO-ADDED
002948                            GIVING PI-NEXT-DISPLAY-SEQ-NO.
002949     MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
002950
002951     PERFORM 8550-SET-MAP-SEQ-NOS.
002952
002953     MOVE ER-0000        TO EMI-ERROR.
002954     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002955     GO TO 8100-SEND-INITIAL-MAP.
002956
002957     EJECT
002958
002959 1040-EDIT-INPUT-CODE.
002960     IF WS-SUB1 = +2
002961        GO TO 1050-EDIT-INPUT-AH-CODE.
002962
002963     MOVE SPACES                 TO ELCNTL-ACCESS.
002964     MOVE 'L'                    TO ELCNTL-REC-TYPE.
002965     PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
002966
002967     IF EMI-ERROR = 9999
002968         GO TO 1048-NO-RECORD.
002969
002970     MOVE +1 TO WS-EDIT-SUB.
002971
002972 1041-SEARCH-LOOP.
002973     IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS
002974         GO TO 1047-NO-MATCH-FOUND.
002975
002976     IF BTYPE1  = CF-LIFE-CODE-IN (WS-EDIT-SUB)
002977         MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE
002978         PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
002979         GO TO 1059-EXIT.
002980
002981     ADD 1   TO WS-EDIT-SUB.
002982
002983     IF WS-EDIT-SUB GREATER 120
002984         GO TO 1047-NO-MATCH-FOUND.
002985
002986     GO TO 1041-SEARCH-LOOP.
002987
002988 1047-NO-MATCH-FOUND.
002989*    MOVE ER-2424                TO EMI-ERROR.
002990*    MOVE AL-UABON               TO BTYPE1-ATTRB
002991*    MOVE -1                     TO BTYPE1-LEN
002992*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002993*    MOVE 'Y'                    TO ERROR-SW.
002994
002995     MOVE BTYPE1                 TO WS-EDITED-LF-CODE.
002996     PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
002997
002998     GO TO 1059-EXIT.
002999
003000 1048-NO-RECORD.
003001*    MOVE ER-2423                TO EMI-ERROR.
003002*    MOVE AL-UABON               TO BTYPE1-ATTRB
003003*    MOVE -1                     TO BTYPE1-LEN
003004*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003005*    MOVE 'Y'                    TO ERROR-SW.
003006
003007     MOVE BTYPE1                 TO WS-EDITED-LF-CODE.
003008     PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
003009
003010     GO TO 1059-EXIT.
003011
003012 1050-EDIT-INPUT-AH-CODE.
003013     MOVE SPACES                 TO ELCNTL-ACCESS.
003014     MOVE 'A'                    TO ELCNTL-REC-TYPE.
003015
003016     PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
003017
003018     IF EMI-ERROR = 9999
003019         GO TO 1058-NO-RECORD.
003020
003021     MOVE +1 TO WS-EDIT-SUB.
003022
003023 1051-SEARCH-LOOP.
003024     IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS
003025         GO TO 1057-NO-MATCH-FOUND.
003026
003027     IF BTYPE2    = CF-AH-CODE-IN (WS-EDIT-SUB)
003028         MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE
003029         PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
003030         GO TO 1059-EXIT.
003031
003032     ADD +1  TO WS-EDIT-SUB.
003033
003034     IF WS-EDIT-SUB GREATER +96
003035         GO TO 1057-NO-MATCH-FOUND.
003036
003037     GO TO 1051-SEARCH-LOOP.
003038
003039 1057-NO-MATCH-FOUND.
003040*    MOVE ER-2428                TO EMI-ERROR.
003041*    MOVE AL-UABON               TO BTYPE2-ATTRB
003042*    MOVE -1                     TO BTYPE2-LEN
003043*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003044*    MOVE 'Y'                    TO ERROR-SW.
003045
003046     MOVE BTYPE2                 TO WS-EDITED-AH-CODE.
003047     PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
003048
003049     GO TO 1059-EXIT.
003050
003051 1058-NO-RECORD.
003052*    MOVE ER-2427                TO EMI-ERROR.
003053*    MOVE AL-UABON               TO BTYPE2-ATTRB
003054*    MOVE -1                     TO BTYPE2-LEN
003055*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003056*    MOVE 'Y'                    TO ERROR-SW.
003057
003058     MOVE BTYPE2                 TO WS-EDITED-AH-CODE.
003059     PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
003060
003061 1059-EXIT.
003062     EXIT.
003063
003064     EJECT
003065
003066 1060-BENEFIT-MASTER-READ.
003067     MOVE SPACES                 TO ELCNTL-ACCESS.
003068
003069     IF ELCNTL-REC-TYPE = 'L'
003070         MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD
003071                                    ELCNTL-HI-BEN
003072         MOVE '4'                TO ELCNTL-REC-TYPE
003073     ELSE
003074         MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD
003075                                    ELCNTL-HI-BEN
003076         MOVE '5'                TO ELCNTL-REC-TYPE.
003077
003078     PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
003079
003080     IF EMI-ERROR = 9999
003081         GO TO 1062-NO-RECORD.
003082
003083     IF ELCNTL-COMPANY-ID NOT = CF-COMPANY-ID  OR
003084        ELCNTL-REC-TYPE   NOT = CF-RECORD-TYPE
003085           GO TO 1062-NO-RECORD.
003086
003087     perform varying ws-sub from +1 by +1 until
003088        (ws-sub > +8)
003089        or (cf-benefit-code (ws-sub) = ws-ben-cd)
003090     end-perform
003091
003092     IF WS-SUB NOT = +9
003093         IF ELCNTL-REC-TYPE = '4'
003094             MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC
003095             move cf-co-earnings-calc (ws-sub)
003096                                 to ws-lf-earnings-calc
003097         ELSE
003098             MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC
003099     ELSE
003100         GO TO 1063-NO-MATCH-FOUND.
003101
003102
003103     IF  CF-TERM-IN-DAYS (WS-SUB)
003104         MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.
003105
003106     GO TO 1069-EXIT.
003107
003108 1062-NO-RECORD.
003109     MOVE ER-2426                TO EMI-ERROR.
003110
003111     IF ELCNTL-REC-TYPE = '4'
003112         MOVE AL-UABON           TO BTYPE1-ATTRB
003113         MOVE -1                 TO BTYPE1-LEN
003114     ELSE
003115         MOVE AL-UABON           TO BTYPE2-ATTRB
003116         MOVE -1                 TO BTYPE2-LEN
003117     END-IF
003118
003119     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003120     MOVE 'Y'                    TO ERROR-SW.
003121
003122     GO TO 1069-EXIT.
003123
003124 1063-NO-MATCH-FOUND.
003125     IF ELCNTL-REC-TYPE = '4'
003126         MOVE ER-2425            TO EMI-ERROR
003127         MOVE AL-UABON           TO BTYPE1-ATTRB
003128         MOVE -1                 TO BTYPE1-LEN
003129     ELSE
003130         MOVE ER-2429            TO EMI-ERROR
003131         MOVE AL-UABON           TO BTYPE2-ATTRB
003132         MOVE -1                 TO BTYPE2-LEN
003133     END-IF
003134
003135     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003136     MOVE 'Y'                    TO ERROR-SW.
003137     GO TO 1069-EXIT.
003138
003139 1069-EXIT.
003140     EXIT.
003141
003142     EJECT
003143
003144 1070-ELCNTL-READ.
003145     
      * EXEC CICS HANDLE CONDITION
003146*        NOTFND  (1078-NO-RECORD)
003147*        ENDFILE (1078-NO-RECORD)
003148*    END-EXEC.
      *    MOVE '"$I''                  ! # #00008563' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303038353633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003149
003150     IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'
003151         
      * EXEC CICS READ
003152*            DATASET (FILE-ID-ELCNTL)
003153*            SET     (ADDRESS OF CONTROL-FILE)
003154*            RIDFLD  (ELCNTL-KEY)
003155*            GTEQ
003156*        END-EXEC
      *    MOVE '&"S        G          (   #00008569' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303038353639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003157     ELSE
003158         
      * EXEC CICS READ
003159*            DATASET (FILE-ID-ELCNTL)
003160*            SET     (ADDRESS OF CONTROL-FILE)
003161*            RIDFLD  (ELCNTL-KEY)
003162*        END-EXEC.
      *    MOVE '&"S        E          (   #00008576' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038353736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003163
003164     GO TO 1079-EXIT.
003165
003166 1078-NO-RECORD.
003167     MOVE ER-9999                TO EMI-ERROR.
003168
003169 1079-EXIT.
003170     EXIT.
003171
003172     EJECT
003173
003174*1080-TERM-CONVERSION.
003175*    IF BMODE   = ' ' OR 'M'
003176*        MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE
003177*        GO TO 1085-ROUND-TERM.
003178
003179*    IF BMODE   = 'S'
003180*        COMPUTE WS-CALC-TERM = WS-BPMTS / 2
003181*        GO TO 1085-ROUND-TERM.
003182
003183*    IF BMODE   = 'W'
003184*        COMPUTE WS-CALC-TERM = WS-BPMTS / 4.33333
003185*        GO TO 1085-ROUND-TERM.
003186
003187*    IF BMODE   = 'B'
003188*        COMPUTE WS-CALC-TERM = WS-BPMTS / 2.16667
003189*        GO TO 1085-ROUND-TERM.
003190
003191*    IF BMODE   = 'T'
003192*        COMPUTE WS-CALC-TERM = WS-BPMTS / 1.08334.
003193
003194*1085-ROUND-TERM.
003195*    IF WS-CALC-TERM-REMAIN GREATER .00000
003196*       ADD +1 TO WS-CALC-TERM.
003197*    MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.
003198
003199*    IF  BTYPE-LEN       (WS-SUB1)  GREATER ZEROS
003200*        IF  WS-KIND-MONTHLY
003201*            IF BTERM-LEN    (WS-SUB1)  = ZEROS
003202*               IF BPREM-LEN (WS-SUB1)  GREATER ZEROS
003203*                  IF  BPMT-LEN   GREATER ZEROS
003204*                      NEXT SENTENCE
003205*                  ELSE
003206*                      GO TO 1087-EDIT-TERM.
003207
003208*    IF PI-COMPANY-ID IS EQUAL TO 'HAN' OR 'JHL'
003209*        IF BBEN-LEN (WS-SUB1) IS GREATER THAN ZEROS
003210*            GO TO 1087-EDIT-TERM.
003211
003212*    IF  BMODE   = 'M' OR ' '
003213*        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).
003214
003215*    IF  BMODE   = 'W'
003216*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.
003217
003218*    IF  BMODE   = 'S'
003219*        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.
003220
003221*    IF  BMODE   = 'B'
003222*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.
003223
003224*    IF  BMODE   = 'T'
003225*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.
003226
003227*    IF WS-SUB1 = +1
003228*        COMPUTE WS-BBEN  (WS-SUB1) = WS-BPMT * WS-BPMTS.
003229
003230*    IF PI-COMPANY-ID = 'CRI'       OR  'LGX'
003231*        IF WS-SUB1 = +1  AND
003232*           BBEN-LEN (WS-SUB1) GREATER ZEROS
003233*             GO TO 1087-EDIT-TERM.
003234
003235*    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).
003236*    MOVE +12                    TO BBEN-LEN    (WS-SUB1).
003237*    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).
003238
003239*1087-EDIT-TERM.
003240*    IF BTERM-LEN (WS-SUB1)  = ZEROS
003241*        MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)
003242*                                     WS-BTERM    (WS-SUB1)
003243*        MOVE +3                   TO BTERM-LEN   (WS-SUB1)
003244*        GO TO 1089-EXIT.
003245
003246*    MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.
003247*    PERFORM 8600-DEEDIT.
003248*    IF DEEDIT-FIELD-V0 NUMERIC
003249*       MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)
003250*       IF WS-BTERM (WS-SUB1)  GREATER ZERO
003251*          MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)
003252*       ELSE
003253*          MOVE ER-2241         TO EMI-ERROR
003254*          MOVE -1              TO BTERM-LEN   (WS-SUB1)
003255*          MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)
003256*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003257*          GO TO 1089-EXIT.
003258
003259*    IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE
003260*        MOVE -1                   TO BTERM-LEN   (WS-SUB1)
003261*        MOVE ER-2593              TO EMI-ERROR
003262*        MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)
003263*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003264
003265*1089-EXIT.
003266*    EXIT.
003267
003268*1090-CALCULATE-MONTHLY-TERM.
003269*    IF  BTYPE-LEN  (WS-SUB1)  GREATER ZEROS
003270*        IF BTERM-LEN (WS-SUB1)  = ZEROS
003271*           IF BPREM-LEN ( WS-SUB1) GREATER ZEROS
003272*              IF  BPMT-LEN   GREATER ZEROS
003273*                  NEXT SENTENCE
003274*              ELSE
003275*                  GO TO 1094-EXIT.
003276
003277*    COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.
003278
003279*    IF WS-CALC-TERM-REMAIN GREATER .00000
003280*       ADD +1 TO WS-CALC-TERM.
003281
003282*    MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.
003283
003284*    MOVE WS-CALC-TERM           TO  BTERMI    (WS-SUB1).
003285*    MOVE +3                     TO  BTERM-LEN (WS-SUB1).
003286
003287*    IF  BMODE   = 'M'  OR  ' '
003288*        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).
003289
003290*    IF  BMODE   = 'W'
003291*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.
003292
003293*    IF  BMODE   = 'S'
003294*        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.
003295
003296*    IF  BMODE   = 'B'
003297*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.
003298
003299*    IF  BMODE   = 'T'
003300*        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.
003301
003302*    IF WS-SUB1 = +1
003303*       COMPUTE WS-BBEN  (WS-SUB1) =
003304*               WS-BBEN (WS-SUB1) * WS-CALC-TERM.
003305
003306*    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).
003307*    MOVE +12                    TO BBEN-LEN    (WS-SUB1).
003308*    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).
003309
003310*1094-EXIT.
003311*    EXIT.
003312
003313 1095-CALC-AGE.
003314     MOVE WS-CONVERTED-BIRTH (1) TO DC-BIN-DATE-1
003315     MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2.
003316     MOVE 1 TO DC-OPTION-CODE.
003317     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
003318     IF NO-CONVERSION-ERROR
003319         COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12
003320         MOVE WS-BAGE            TO BAGE
003321         MOVE +2                 TO BAGE-LEN
003322         MOVE AL-UNNON TO BAGE-ATTRB.
003323
003324 1099-EXIT.
003325     EXIT.
003326
003327     EJECT
003328
003329 1100-EDIT-MAPC.
003330     MOVE +1                     TO WS-SUB2.
003331
003332     IF PI-LAST-FUNC-DISPLAY
003333        AND CLAST-NAME-LEN (1)   = ZEROS
003334        AND CLAST-NAME-LEN (2)   = ZEROS
003335        AND CLAST-NAME-LEN (3)   = ZEROS
003336        AND CLAST-NAME-LEN (4)   = ZEROS
003337        AND CCANDT1-LEN     (1) = ZEROS
003338        AND CCANDT2-LEN     (1) = ZEROS
003339        AND CCANDT1-LEN     (2) = ZEROS
003340        AND CCANDT2-LEN     (2) = ZEROS
003341        AND CCANDT1-LEN     (3) = ZEROS
003342        AND CCANDT2-LEN     (3) = ZEROS
003343        AND CCANDT1-LEN     (4) = ZEROS
003344        AND CCANDT2-LEN     (4) = ZEROS
003345        AND CMTHD1-LEN      (1) = ZEROS
003346        AND CMTHD2-LEN      (1) = ZEROS
003347        AND CMTHD1-LEN      (2) = ZEROS
003348        AND CMTHD2-LEN      (2) = ZEROS
003349        AND CMTHD1-LEN      (3) = ZEROS
003350        AND CMTHD2-LEN      (3) = ZEROS
003351        AND CMTHD1-LEN      (4) = ZEROS
003352        AND CMTHD2-LEN      (4) = ZEROS
003353        AND CREFUND1-LEN    (1) = ZEROS
003354        AND CREFUND2-LEN    (1) = ZEROS
003355        AND CREFUND1-LEN    (2) = ZEROS
003356        AND CREFUND2-LEN    (2) = ZEROS
003357        AND CREFUND1-LEN    (3) = ZEROS
003358        AND CREFUND2-LEN    (3) = ZEROS
003359        AND CREFUND1-LEN    (4) = ZEROS
003360        AND CREFUND2-LEN    (4) = ZEROS
003361        AND CLIVES-LEN     (1) = ZEROS
003362        AND CLIVES-LEN     (2) = ZEROS
003363        AND CLIVES-LEN     (3) = ZEROS
003364        AND CLIVES-LEN     (4) = ZEROS
003365        AND CCANREA-LEN    (1) = ZEROS
003366        AND CCANREA-LEN    (2) = ZEROS
003367        AND CCANREA-LEN    (3) = ZEROS
003368        AND CCANREA-LEN    (4) = ZEROS
003369        AND CPAYEE-LEN     (1) = ZEROS
003370        AND CPAYEE-LEN     (2) = ZEROS
003371        AND CPAYEE-LEN     (3) = ZEROS
003372        AND CPAYEE-LEN     (4) = ZEROS
003373        AND CCHK-LEN       (1) = ZEROS
003374        AND CCHK-LEN       (2) = ZEROS
003375        AND CCHK-LEN       (3) = ZEROS
003376        AND CCHK-LEN       (4) = ZEROS
003377         GO TO 1130-NOTHING-TO-EDIT.
003378
003379 1110-EDIT-MAPC-LOOP.
003380     IF CCERT-LEN       (WS-SUB2) = ZEROS
003381       AND CEFFDT-LEN   (WS-SUB2) = ZEROS
003382       AND CCANDT1-LEN  (WS-SUB2) = ZEROS
003383       AND CCANDT2-LEN  (WS-SUB2) = ZEROS
003384       AND CMTHD1-LEN   (WS-SUB2) = ZEROS
003385       AND CMTHD2-LEN   (WS-SUB2) = ZEROS
003386       AND CREFUND1-LEN (WS-SUB2) = ZEROS
003387       AND CREFUND2-LEN (WS-SUB2) = ZEROS
003388       AND NOT PI-LAST-FUNC-DISPLAY
003389         GO TO 1120-INCREMENT-OCCURRENCE.
003390
003391     MOVE 'Y'                    TO WS-DATA-KEYED-SW.
003392
003393     MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
003394
003395     IF CCERT-LEN (WS-SUB2) = ZEROS
003396       AND PI-LAST-FUNC-DISPLAY
003397         NEXT SENTENCE
003398     ELSE
003399         IF CCERT-LEN (WS-SUB2)  NOT = ZEROS
003400             MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)
003401         ELSE
003402             MOVE -1             TO CCERT-LEN   (WS-SUB2)
003403             MOVE ER-2218        TO EMI-ERROR
003404             MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)
003405             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003406
003407     IF CSFX-LEN (WS-SUB2)      GREATER ZEROS
003408         MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW
003409         MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).
003410
003411     IF CEFFDT-LEN (WS-SUB2) = ZEROS
003412       AND PI-LAST-FUNC-DISPLAY
003413         NEXT SENTENCE
003414     ELSE
003415         IF CEFFDT-LEN (WS-SUB2) GREATER ZEROS
003416             MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)
003417             IF CEFFDT (WS-SUB2) NUMERIC
003418                 MOVE 4              TO DC-OPTION-CODE
003419                 MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY
003420                 PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003421                 MOVE DC-BIN-DATE-1  TO
003422                             WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
003423                             WS-CONVERTED-EFFDT
003424                 IF NO-CONVERSION-ERROR
003425                     IF WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
003426                       NOT LESS PI-ACCT-LOW-EFF-DT
003427                       AND LESS PI-ACCT-HIGH-EXP-DT
003428                         PERFORM 1500-EDIT-ACCT-DT-RANGES THRU
003429                                 1590-EXIT
003430                     ELSE
003431                         NEXT SENTENCE
003432*                        MOVE -1       TO CEFFDT-LEN (WS-SUB2)
003433*                        MOVE ER-2589  TO EMI-ERROR
003434*                        MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)
003435*                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003436                 ELSE
003437                     MOVE -1         TO CEFFDT-LEN (WS-SUB2)
003438                     MOVE ER-2226    TO EMI-ERROR
003439                     MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)
003440                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003441             ELSE
003442                 MOVE -1             TO CEFFDT-LEN (WS-SUB2)
003443                 MOVE ER-2223        TO EMI-ERROR
003444                 MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)
003445                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003446         ELSE
003447             MOVE -1                 TO CEFFDT-LEN (WS-SUB2)
003448             MOVE ER-2220            TO EMI-ERROR
003449             MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)
003450             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003451
003452     IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
003453         MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).
003454
003455     EJECT
003456
003457 1115-EDIT-COVERAGES.
003458     IF CCANDT1-LEN (WS-SUB2) = ZEROS
003459       AND PI-LAST-FUNC-DISPLAY
003460         NEXT SENTENCE
003461     ELSE
003462         IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
003463             MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)
003464             IF PI-LAST-FUNC-DISPLAY AND
003465                CCANDT1 (WS-SUB2) = SPACES
003466                 MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2)
003467             ELSE
003468                IF CCANDT1 (WS-SUB2) NUMERIC
003469                   MOVE 4              TO DC-OPTION-CODE
003470                   MOVE CCANDT1 (WS-SUB2) TO
003471                                      DC-GREG-DATE-1-MDY
003472                   PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003473                   MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1
003474                                                         (WS-SUB2)
003475                   IF NO-CONVERSION-ERROR
003476                      NEXT SENTENCE
003477                   ELSE
003478                      MOVE -1       TO CCANDT1-LEN   (WS-SUB2)
003479                      MOVE ER-2227  TO EMI-ERROR
003480                      MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)
003481                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003482                ELSE
003483                   MOVE -1         TO CCANDT1-LEN   (WS-SUB2)
003484                   MOVE ER-2223    TO EMI-ERROR
003485                   MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)
003486                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003487         ELSE
003488             IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS
003489                MOVE -1             TO CCANDT1-LEN   (WS-SUB2)
003490                MOVE ER-2222        TO EMI-ERROR
003491                MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)
003492                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003493
003494     IF CREFUND1-LEN (WS-SUB2) = ZEROS
003495       AND PI-LAST-FUNC-DISPLAY
003496         NEXT SENTENCE
003497     ELSE
003498         IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS
003499            MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)
003500*           MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11
003501*           PERFORM 8600-DEEDIT
003502*           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).
003503            
      * EXEC CICS BIF DEEDIT
003504*               FIELD  (CREFUND1I (WS-SUB2))
003505*               LENGTH (11)
003506*           END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008921' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CREFUND1I(WS-SUB2), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003507            MOVE CREFUND1I (WS-SUB2) TO WS-CREFUND1 (WS-SUB2).
003508
003509******************************************************************
003510*********** REFUND METHODS CORRESPOND TO EARNING METHODS *********
003511*********** METHOD 'R' INDICATES A REPOSSESSION FOR 'FLC'*********
003512******************************************************************
003513     IF CMTHD1-LEN (WS-SUB2) = ZEROS
003514       AND PI-LAST-FUNC-DISPLAY
003515         NEXT SENTENCE
003516     ELSE
003517         IF CMTHD1-LEN (WS-SUB2) NOT = ZEROS
003518            MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW
003519            MOVE AL-UNNON       TO CMTHD1-ATTRB (WS-SUB2)
003520            IF CMTHD1 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'
003521                                   OR '5' OR '6' OR '8' OR ' '
003522*                                  OR 'R'
003523               NEXT SENTENCE
003524            ELSE
003525                MOVE -1         TO CMTHD1-LEN   (WS-SUB2)
003526                MOVE ER-0582    TO EMI-ERROR
003527                MOVE AL-UNBON   TO CMTHD1-ATTRB (WS-SUB2)
003528                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003529
003530     MOVE SPACES                  TO WS-CONVERTED-CANDT2 (WS-SUB2)
003531     IF CCANDT2-LEN (WS-SUB2) = ZEROS
003532       AND PI-LAST-FUNC-DISPLAY
003533         NEXT SENTENCE
003534     ELSE
003535         IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
003536             MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)
003537             IF PI-LAST-FUNC-DISPLAY AND
003538                CCANDT2 (WS-SUB2) = SPACES
003539                 MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2)
003540             ELSE
003541                IF CCANDT2 (WS-SUB2) NUMERIC
003542                   MOVE 4              TO DC-OPTION-CODE
003543                   MOVE CCANDT2 (WS-SUB2) TO
003544                                      DC-GREG-DATE-1-MDY
003545                   PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003546                   MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2
003547                                                         (WS-SUB2)
003548                   IF NO-CONVERSION-ERROR
003549                      NEXT SENTENCE
003550                   ELSE
003551                      MOVE -1       TO CCANDT2-LEN   (WS-SUB2)
003552                      MOVE ER-2227  TO EMI-ERROR
003553                      MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)
003554                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003555                ELSE
003556                   MOVE -1         TO CCANDT2-LEN   (WS-SUB2)
003557                   MOVE ER-2223    TO EMI-ERROR
003558                   MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)
003559                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003560****     ELSE
003561****         IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS
003562****            MOVE -1             TO CCANDT2-LEN   (WS-SUB2)
003563****            MOVE ER-2222        TO EMI-ERROR
003564****            MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)
003565****            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003566
003567     IF CREFUND2-LEN (WS-SUB2) = ZEROS
003568       AND PI-LAST-FUNC-DISPLAY
003569         NEXT SENTENCE
003570     ELSE
003571         IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS
003572            MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)
003573*           MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11
003574*           PERFORM 8600-DEEDIT
003575*           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).
003576            
      * EXEC CICS BIF DEEDIT
003577*               FIELD  (CREFUND2I (WS-SUB2))
003578*               LENGTH (11)
003579*           END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008994' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CREFUND2I(WS-SUB2), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003580            MOVE CREFUND2I (WS-SUB2) TO WS-CREFUND2 (WS-SUB2).
003581
003582     IF CMTHD2-LEN (WS-SUB2) = ZEROS
003583       AND PI-LAST-FUNC-DISPLAY
003584         NEXT SENTENCE
003585     ELSE
003586         IF CMTHD2-LEN (WS-SUB2) NOT = ZEROS
003587            MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW
003588            MOVE AL-UNNON       TO CMTHD2-ATTRB (WS-SUB2)
003589            IF CMTHD2 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'
003590                                   OR '5' OR '6' OR '8' OR ' '
003591*                                  OR 'R'
003592               NEXT SENTENCE
003593            ELSE
003594                MOVE -1         TO CMTHD2-LEN   (WS-SUB2)
003595                MOVE ER-0582    TO EMI-ERROR
003596                MOVE AL-UNBON   TO CMTHD2-ATTRB (WS-SUB2)
003597                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003598
003599     IF CLIVES-LEN  (WS-SUB2)  GREATER ZEROS
003600         MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW
003601         MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD
003602         PERFORM 8600-DEEDIT
003603         IF DEEDIT-FIELD-V0 NUMERIC
003604             MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)
003605             MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)
003606         ELSE
003607             MOVE -1             TO CLIVES-LEN   (WS-SUB2)
003608             MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)
003609             MOVE ER-2223        TO EMI-ERROR
003610             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003611
003612     IF CCANREA-LEN (WS-SUB2)  GREATER  ZEROS
003613        MOVE 'Y'                TO  PI-CAN-REA-KEYED-SW
003614        PERFORM 1150-CHECK-CANCEL-REASON THRU 1150-EXIT
003615        IF SQLCODE = ZERO
003616            MOVE CCANREA (WS-SUB2) TO  WS-CAN-REA (WS-SUB2)
003617            MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)
003618*        IF CCANREA (WS-SUB2) = 'R' OR ' '
003619*           MOVE CCANREA (WS-SUB2)
003620*                                TO  WS-CAN-REA (WS-SUB2)
003621*            MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)
003622        ELSE
003623        IF SQLCODE = 100
003624             MOVE -1             TO  CCANREA-LEN (WS-SUB2)
003625             MOVE AL-UABON       TO  CCANREA-ATTRB (WS-SUB2)
003626             MOVE ER-9841        TO  EMI-ERROR
003627             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003628
003629     IF CPAYEE-LEN  (WS-SUB2)  GREATER ZEROS
003630        MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.
003631
003632     IF CCHK-LEN    (WS-SUB2)  GREATER ZEROS
003633        MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW
003634        IF  CCHK    (WS-SUB2)  = 'R' OR ' '
003635            NEXT SENTENCE
003636        ELSE
003637            MOVE ER-7405         TO EMI-ERROR
003638            MOVE -1              TO CCHK-LEN      (WS-SUB2)
003639            MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)
003640            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003641
003642     IF PI-LAST-FUNC-DISPLAY
003643        NEXT SENTENCE
003644     ELSE
003645        IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS OR
003646           CCANDT2-LEN (WS-SUB2) GREATER ZEROS
003647             NEXT SENTENCE
003648           ELSE
003649             MOVE ER-2222          TO EMI-ERROR
003650             MOVE -1               TO CCANDT1-LEN   (WS-SUB2)
003651             MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)
003652             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003653
003654     IF PI-LAST-FUNC-DISPLAY
003655        NEXT SENTENCE
003656     ELSE
003657        IF WS-CONVERTED-CANDT1 (WS-SUB2) NOT = LOW-VALUES AND
003658           WS-CONVERTED-CANDT2 (WS-SUB2) = SPACES AND
003659           CREFUND2-LEN (WS-SUB2) GREATER ZEROS
003660             MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO
003661                  WS-CONVERTED-CANDT2 (WS-SUB2)
003662             MOVE CCANDT1     (WS-SUB2) TO CCANDT2     (WS-SUB2)
003663             MOVE CCANDT1-LEN (WS-SUB2) TO CCANDT2-LEN (WS-SUB2).
003664
003665     IF  EMI-ERROR = ZEROS
003666         MOVE 'Y'                TO PI-UPDATE-SW
003667     ELSE
003668         MOVE 'Y'                TO PI-ERROR-SW.
003669
003670 1120-INCREMENT-OCCURRENCE.
003671     ADD +1                      TO WS-SUB2.
003672
003673     IF WS-SUB2 GREATER +4 OR PI-LAST-FUNC-DISPLAY
003674         NEXT SENTENCE
003675     ELSE
003676         GO TO 1110-EDIT-MAPC-LOOP.
003677
003678     IF PI-DATA-ERRORS
003679         MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2)
003680                                    CSEQ-ATTRB (3) CSEQ-ATTRB (4)
003681        GO TO 8200-SEND-DATAONLY
003682     ELSE
003683        PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT.
003684
003685 1130-NOTHING-TO-EDIT.
003686     IF NOT PI-LAST-FUNC-DISPLAY
003687        AND WS-DATA-NOT-KEYED
003688          MOVE ER-7400             TO EMI-ERROR
003689          MOVE -1                  TO CCANDT1-LEN (1)
003690          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003691          MOVE 'Y'                 TO PI-ERROR-SW
003692          GO TO 8200-SEND-DATAONLY.
003693
003694     IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED
003695        SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
003696        MOVE ER-0000            TO EMI-ERROR
003697        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003698        GO TO 2000-BROWSE-FWD.
003699
003700     MOVE LOW-VALUES     TO EL630BI.
003701
003702     MOVE SPACE          TO PI-DISPLAY-SW.
003703
003704     ADD +1                 PI-LAST-SEQ-NO-ADDED
003705                            GIVING PI-NEXT-DISPLAY-SEQ-NO.
003706
003707     MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
003708
003709     PERFORM 8550-SET-MAP-SEQ-NOS
003710                  VARYING WS-SUB2 FROM +1 BY +1
003711                  UNTIL WS-SUB2 GREATER +4.
003712
003713     MOVE ER-0000            TO EMI-ERROR.
003714     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003715     GO TO 8100-SEND-INITIAL-MAP.
003716
003717 1150-CHECK-CANCEL-REASON.
003718
003719*      Read Napersoft CancelReasons Lookup Table
003720     MOVE 'naperadmin'           TO USR
003721     MOVE 'cCm8naper'            TO PASS
003722     IF WS-KIXHOST = 'logictest'
003723        MOVE 'HOVTSTDB01_NaperRepo'
003724                                 TO SVR
003725        MOVE '1029'              TO WS-LOOKUPID
003726     ELSE
003727        MOVE 'SDVDB01_NaperRepo' TO SVR
003728        MOVE '1029'              TO WS-LOOKUPID
003729     END-IF
003730
003731     MOVE SPACES                 TO WS-LOOKUP-VALUE
003732
003733     PERFORM 9000-CONNECT-TO-DB  THRU 9000-EXIT
003734
003735     MOVE CCANREA (WS-SUB2) TO WS-LOOKUPNAME
003736
003738     EXEC SQL
                 SELECT LOOKUPVALUE
003739             INTO :WS-LOOKUP-VALUE
003740             FROM LOOKUPVALUES
003741               WHERE LOOKUPID = :WS-LOOKUPID
003742                 AND LOOKUPNAME = :WS-LOOKUPNAME
003743     END-EXEC
003744
003745     IF SQLCODE = 0
003746      OR SQLCODE = 100
003747        CONTINUE
003748     ELSE
003749        DISPLAY "ERROR: INVALID CancelReasons Lookup Table SELECT"
003750        DISPLAY ' SQL RETURN CODE ' SQLCODE
003751        DISPLAY ' SQL ERR MESS    ' SQLERRMC
003752     END-IF.
003753
003754     PERFORM 9050-DISCONNECT THRU 9050-EXIT.
003755
003756 1150-EXIT.
003757     EXIT.
003758
003759     EJECT
003760
003761 1500-EDIT-ACCT-DT-RANGES.
003762
003763******************************************************************
003764*                                                                *
003765*         E D I T   A C C O U N T   D A T E   R A N G E S        *
003766*                                                                *
003767*                                                                *
003768*    NOTE:  IT IS ONLY NECESSARY TO EDIT THE DATE RANGES         *
003769*           FOR COMPANYS THAT USE THE ACCOUNTS RECEIVABLE        *
003770*           SYSTEM.                                              *
003771*                                                                *
003772*    1.  DETERMINE THE DATE RANGE FOR THE EFFECTIVE DATE.        *
003773*                                                                *
003774*    2.  VERIFY THE ACCOUNT AGENT.  THE ACCOUNT AGENT SHOULD BE  *
003775*        THE SAME FOR THE ENTIRE BATCH.  IF IT CHANGES, IT IS    *
003776*        AN ERROR.                                               *
003777*                                                                *
003778*    3.  VERIFY THAT THE FINANCIAL RESPONSIBLITY.  THE FINANCIAL *
003779*        RESPONSIBILITY SHOULD BE THE SAME FOR THE ENTIRE BATCH. *
003780*        IF IT CHANGES, IT IS AN ERROR.                          *
003781*                                                                *
003782******************************************************************
003783
003784     IF PI-AR-PROCESSING
003785        NEXT SENTENCE
003786     ELSE
003787        GO TO 1590-EXIT.
003788
003789     MOVE +0                     TO WS-ACCT-SUB.
003790
003791 1525-FIND-ACCT-DT-RANGE.
003792
003793     ADD  +1                     TO WS-ACCT-SUB.
003794
003795     IF WS-ACCT-SUB GREATER +32
003796        MOVE  ER-2119            TO EMI-ERROR
003797        IF PI-MAP-NAME = EL630B
003798           MOVE -1               TO BPFENTRL
003799           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003800           GO TO 8200-SEND-DATAONLY
003801        ELSE
003802           MOVE -1               TO CEFFDT-LEN (WS-SUB2)
003803           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003804           GO TO 1590-EXIT.
003805
003806     IF WS-CONVERTED-EFFDT NOT LESS PI-ACCT-EFF-DT
003807                                                 (WS-ACCT-SUB)
003808        IF WS-CONVERTED-EFFDT  LESS PI-ACCT-EXP-DT
003809                                                 (WS-ACCT-SUB)
003810           NEXT SENTENCE
003811        ELSE
003812           GO TO 1525-FIND-ACCT-DT-RANGE.
003813
003814     IF PI-ACCOUNT-AGENT = SPACES
003815        MOVE PI-ACCT-AGENT  (WS-ACCT-SUB) TO PI-ACCOUNT-AGENT
003816        MOVE PI-REMIT-AGENT (WS-ACCT-SUB) TO PI-FIN-RESP.
003817
003818     IF PI-ACCT-AGENT  (WS-ACCT-SUB) = PI-ACCOUNT-AGENT
003819        NEXT SENTENCE
003820     ELSE
003821        MOVE 'Y'                 TO PI-ACCT-AGENT-ERROR-SW.
003822
003823     IF PI-REMIT-AGENT (WS-ACCT-SUB) = PI-FIN-RESP
003824        NEXT SENTENCE
003825     ELSE
003826        MOVE 'Y'                 TO PI-FIN-RESP-ERROR-SW.
003827
003828 1590-EXIT.
003829      EXIT.
003830
003831     EJECT
003832
003833 2000-BROWSE-FWD.
003834     MOVE LOW-VALUES             TO EL630BI.
003835
003836     ADD +1                      TO PI-SAV-BATCH-SEQ.
003837
003838     
      * EXEC CICS HANDLE CONDITION
003839*        NOTFND (2020-END-FILE)
003840*    END-EXEC.
      *    MOVE '"$I                   ! $ #00009256' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303039323536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003841
003842     
      * EXEC CICS READ
003843*        SET     (ADDRESS OF PENDING-BUSINESS)
003844*        DATASET (FILE-ID-ERPNDB)
003845*        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
003846*        GTEQ
003847*    END-EXEC.
      *    MOVE '&"S        G          (   #00009260' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039323630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003848
003849     IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND
003850        PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
003851         NEXT SENTENCE
003852     ELSE
003853         GO TO 2020-END-FILE.
003854
003855     IF PB-BATCH-TRAILER
003856         GO TO 2020-END-FILE.
003857
003858     MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.
003859
003860     IF PB-ISSUE
003861         MOVE EL630B             TO PI-MAP-NAME
003862         MOVE AL-SANOF           TO BDELHDGA
003863         PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
003864     ELSE
003865         MOVE EL630C             TO PI-MAP-NAME
003866         MOVE AL-SANOF           TO CDELHDGA
003867         PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
003868
003869 2010-SEND-MAP.
003870     GO TO 8100-SEND-INITIAL-MAP.
003871
003872 2020-END-FILE.
003873     MOVE SPACE                  TO PI-DISPLAY-SW.
003874
003875     IF PI-MAP-NAME = EL630B
003876         MOVE LOW-VALUES         TO EL630BI
003877         MOVE -1                 TO BPFENTRL
003878         ADD +1                     PI-LAST-SEQ-NO-ADDED
003879               GIVING PI-NEXT-DISPLAY-SEQ-NO
003880         PERFORM 8550-SET-MAP-SEQ-NOS
003881     ELSE
003882         MOVE LOW-VALUES         TO EL630BI
003883         MOVE -1                 TO CPFENTRL
003884         ADD +1                     PI-LAST-SEQ-NO-ADDED
003885               GIVING PI-NEXT-DISPLAY-SEQ-NO
003886         PERFORM 8550-SET-MAP-SEQ-NOS
003887                 VARYING WS-SUB2 FROM +1 BY +1
003888                 UNTIL WS-SUB2 GREATER +4.
003889
003890     MOVE ER-2217                TO EMI-ERROR.
003891     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003892     GO TO 2010-SEND-MAP.
003893
003894     EJECT
003895
003896 2100-BROWSE-BKWD.
003897     MOVE LOW-VALUES             TO EL630BI.
003898
003899     SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.
003900
003901     IF PI-SAV-BATCH-SEQ NOT GREATER +0
003902         GO TO 2120-END-FILE.
003903
003904     
      * EXEC CICS HANDLE CONDITION
003905*        NOTFND (2100-BROWSE-BKWD)
003906*    END-EXEC.
      *    MOVE '"$I                   ! % #00009322' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303039333232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003907
003908     
      * EXEC CICS READ
003909*        SET     (ADDRESS OF PENDING-BUSINESS)
003910*        DATASET (FILE-ID-ERPNDB)
003911*        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
003912*    END-EXEC.
      *    MOVE '&"S        E          (   #00009326' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039333236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003913
003914     IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND
003915        PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
003916         NEXT SENTENCE
003917     ELSE
003918         GO TO 2120-END-FILE.
003919
003920     IF PB-BATCH-TRAILER
003921         GO TO 2120-END-FILE.
003922
003923     IF PB-ISSUE
003924         MOVE EL630B             TO PI-MAP-NAME
003925         MOVE AL-SANOF           TO BDELHDGA
003926         PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
003927     ELSE
003928         MOVE EL630C             TO PI-MAP-NAME
003929         MOVE AL-SANOF           TO CDELHDGA
003930         PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
003931
003932 2110-SEND-MAP.
003933     GO TO 8100-SEND-INITIAL-MAP.
003934
003935 2120-END-FILE.
003936     MOVE ER-2431                TO EMI-ERROR.
003937     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003938     MOVE ER-0000                TO EMI-ERROR.
003939     MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.
003940     GO TO 2000-BROWSE-FWD.
003941
003942     EJECT
003943
003944 3000-CONTINUE-ENTRY.
003945     MOVE PI-SAV-ENDING-ERPNDB-KEY TO ERPNDB-KEY.
003946
003947     
      * EXEC CICS HANDLE CONDITION
003948*        NOTFND (3300-REC-NOT-FND)
003949*    END-EXEC.
      *    MOVE '"$I                   ! & #00009365' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303039333635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003950
003951     
      * EXEC CICS STARTBR
003952*        DATASET (FILE-ID-ERPNDB)
003953*        RIDFLD  (ERPNDB-KEY)
003954*        GTEQ
003955*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009369' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039333639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003956
003957 3100-READ-LOOP.
003958     
      * EXEC CICS HANDLE CONDITION
003959*        ENDFILE (3200-END-BROWSE)
003960*    END-EXEC.
      *    MOVE '"$''                   ! '' #00009376' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303039333736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003961
003962     
      * EXEC CICS READNEXT
003963*        SET     (ADDRESS OF PENDING-BUSINESS)
003964*        DATASET (FILE-ID-ERPNDB)
003965*        RIDFLD  (ERPNDB-KEY)
003966*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009380' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039333830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003967
003968     IF PB-COMPANY-CD  = PI-SAV-COMP-CD   AND
003969        PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
003970         NEXT SENTENCE
003971     ELSE
003972         GO TO 3200-END-BROWSE.
003973
003974     IF NOT PB-BATCH-TRAILER
003975         GO TO 3110-NOT-BATCH-TRAILER.
003976
003977 3105-PRIME-PI-COUNTS.
003978     IF PI-LF-ISS-REMITTED = ZEROS
003979         MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.
003980
003981     IF PI-AH-ISS-REMITTED = ZEROS
003982         MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.
003983
003984     IF PI-ISS-CNT-REMITTED = ZEROS
003985         MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.
003986
003987     IF PI-CAN-CNT-REMITTED = ZEROS
003988         MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.
003989
003990     IF PI-LF-CAN-REMITTED = ZEROS
003991         MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.
003992
003993     IF PI-AH-CAN-REMITTED = ZEROS
003994         MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.
003995
003996     GO TO 3200-END-BROWSE.
003997
003998 3110-NOT-BATCH-TRAILER.
003999     IF PB-ISSUE
004000         ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED
004001         ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED
004002         ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED
004003         ADD +1                      TO PI-ISS-CNT-ENTERED
004004     ELSE
004005         ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED
004006         ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED
004007         ADD +1                      TO PI-CAN-CNT-ENTERED.
004008
004009     MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED
004010                                        PI-SAV-BATCH-SEQ.
004011
004012     GO TO 3100-READ-LOOP.
004013
004014 3200-END-BROWSE.
004015     
      * EXEC CICS ENDBR
004016*        DATASET (FILE-ID-ERPNDB)
004017*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009433' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039343333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004018
004019     ADD +1                         PI-LAST-SEQ-NO-ADDED
004020                            GIVING PI-NEXT-DISPLAY-SEQ-NO.
004021
004022     IF PI-MAINT-FUNC = 'B'
004023         MOVE ZEROS              TO PI-SAV-BATCH-SEQ
004024         GO TO 2000-BROWSE-FWD
004025     ELSE
004026         ADD +1                  TO PI-SAV-BATCH-SEQ.
004027
004028     IF PI-LF-ISS-REMITTED  = ZEROS  AND
004029        PI-AH-ISS-REMITTED  = ZEROS  AND
004030        PI-LF-CAN-REMITTED  = ZEROS  AND
004031        PI-AH-CAN-REMITTED  = ZEROS  AND
004032        PI-ISS-CNT-REMITTED = ZEROS  AND
004033        PI-CAN-CNT-REMITTED = ZEROS
004034         MOVE  EL630B            TO PI-MAP-NAME
004035     ELSE
004036         IF PI-LF-ISS-REMITTED  = ZEROS AND
004037            PI-AH-ISS-REMITTED  = ZEROS AND
004038            PI-ISS-CNT-REMITTED = ZEROS
004039             MOVE  EL630C        TO PI-MAP-NAME
004040         ELSE
004041             MOVE  EL630B        TO PI-MAP-NAME.
004042
004043     IF PI-MAP-NAME = EL630B
004044         PERFORM 8550-SET-MAP-SEQ-NOS
004045     ELSE
004046         PERFORM 8550-SET-MAP-SEQ-NOS
004047                 VARYING WS-SUB2 FROM +1 BY +1
004048                 UNTIL WS-SUB2 GREATER +4.
004049
004050     GO TO 8100-SEND-INITIAL-MAP.
004051
004052 3300-REC-NOT-FND.
004053     MOVE ER-2212                TO EMI-ERROR.
004054     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004055     GO TO 8100-SEND-INITIAL-MAP.
004056
004057     EJECT
004058
004059 4000-BUILD-ISSUE-RECORD.
004060     IF BSEQ  GREATER PI-LAST-SEQ-NO-ADDED
004061         GO TO 4100-ADD-ISSUE-RECORD.
004062
004063******************************************************************
004064*   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
004065*   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
004066*   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
004067*   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
004068*   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
004069*   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *
004070******************************************************************
004071
004072     MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
004073     MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
004074     MOVE BSEQ                   TO ERPNDB-BATCH-SEQ.
004075
004076     
      * EXEC CICS HANDLE CONDITION
004077*        NOTFND (4100-ADD-ISSUE-RECORD)
004078*    END-EXEC.
      *    MOVE '"$I                   ! ( #00009494' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303039343934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004079
004080     
      * EXEC CICS READ
004081*        SET     (ADDRESS OF PENDING-BUSINESS)
004082*        DATASET (FILE-ID-ERPNDB)
004083*        RIDFLD  (ERPNDB-KEY)
004084*        UPDATE
004085*    END-EXEC.
      *    MOVE '&"S        EU         (   #00009498' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039343938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004086
004087     MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.
004088
004089     MOVE 'B'                    TO JP-RECORD-TYPE
004090     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
004091     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
004092     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
004093     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Issue Image
004094
004095     MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
004096     MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
004097     MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
004098
004099     IF BLAST-NAME-LEN GREATER ZEROS
004100         MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
004101
004102     IF B1ST-NAME-LEN GREATER ZEROS
004103         MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
004104
004105     IF BINIT-LEN     GREATER ZEROS
004106         MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
004107
004108     IF BAGE-LEN      GREATER ZEROS
004109         MOVE WS-BAGE            TO PB-I-AGE.
004110
004111     IF BJNT-AGE-LEN  GREATER ZEROS
004112         MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.
004113
004114     IF BBIRTH-LEN > ZEROS
004115        MOVE WS-CONVERTED-BIRTH (1)
004116                                 TO PB-I-BIRTHDAY
004117     END-IF
004118
004119     IF BJNTDOB-LEN > ZEROS
004120        MOVE WS-CONVERTED-BIRTH (2)
004121                                 TO PB-I-JOINT-BIRTHDAY
004122     end-if
004123
004124     IF BTERM1-LEN > ZEROS
004125        MOVE WS-BTERM1           TO PB-I-LF-TERM.
004126
004127     IF BTERM2-LEN > ZEROS
004128        MOVE WS-BTERM2             TO PB-I-AH-TERM.
004129
004130     IF BLN-TERM-LEN    GREATER ZEROS
004131         MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM.
004132
004133*    IF BFREQ-LEN       GREATER ZEROS
004134*        MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY.
004135
004136*    IF BSKPCD-LEN      GREATER ZEROS
004137*        MOVE BSKPCD             TO PB-I-SKIP-CODE.
004138
004139*    IF BMODE-LEN       GREATER ZEROS
004140*        MOVE BMODE              TO PB-I-TERM-TYPE.
004141
004142*    IF BPMTS-LEN       GREATER ZEROS
004143*        MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS.
004144
004145*    IF BPMT-LEN        GREATER ZEROS
004146*        MOVE WS-BPMT            TO PB-I-PAYMENT-AMOUNT.
004147
004148*    IF BPOLICY-LEN     GREATER ZEROS
004149*        MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.
004150
004151******************************************************************
004152*          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *
004153*                                                                *
004154*          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
004155******************************************************************
004156
004157     IF BTYPE1-LEN > ZEROS
004158        IF BTYPE1           NOT = SPACES AND ZEROS
004159           MOVE BTYPE1            TO PB-I-LF-INPUT-CD
004160           MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
004161           MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR
004162        ELSE
004163           IF BTYPE1      = SPACES
004164              MOVE SPACES         TO PB-I-LF-INPUT-CD
004165                                     PB-I-LF-ABBR
004166              MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD
004167           ELSE
004168              SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
004169              SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM
004170                       PI-LF-ISS-ENTERED
004171              MOVE SPACES         TO PB-I-LF-INPUT-CD
004172                                     PB-I-LF-ABBR
004173              MOVE ZEROS          TO PB-I-LF-TERM
004174                                     PB-I-LF-BENEFIT-AMT
004175                                     PB-I-LF-PREMIUM-AMT
004176                                     PB-I-LF-BENEFIT-CD
004177                                     PB-I-LF-PREM-CALC
004178                                     PB-I-LF-ALT-BENEFIT-AMT
004179                                     PB-I-LF-ALT-PREMIUM-AMT
004180                                     PB-I-LF-CRIT-PER
004181              MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.
004182
004183     IF  BBENE1-LEN > ZEROS
004184         MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT.
004185
004186     IF  BALT-BEN1-LEN > ZEROS
004187         MOVE WS-BALT-BEN1          TO PB-I-LF-ALT-BENEFIT-AMT.
004188
004189     IF  BPREM1-LEN > ZEROS
004190         IF WS-BPREM1       = WS-ALL-NINES OR
004191            WS-BPREM1       GREATER WS-ALL-NINES
004192            SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
004193            MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
004194            MOVE '?'             TO PB-I-LF-CALC-FLAG
004195         ELSE
004196            SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
004197            MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT
004198            ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED
004199            MOVE SPACE           TO PB-I-LF-CALC-FLAG.
004200
004201     IF  BALT-PREM1-LEN > ZEROS
004202         SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
004203         MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT
004204         ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED.
004205
004206     IF  BALT-PREM2-LEN > ZEROS
004207         MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
004208     END-IF
004209
004210******************************************************************
004211*          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *
004212*                                                                *
004213*          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
004214******************************************************************
004215
004216     IF BTYPE2-LEN > ZEROS
004217        IF BTYPE2           NOT = SPACES AND ZEROS
004218           MOVE BTYPE2            TO PB-I-AH-INPUT-CD
004219           MOVE WS-EDITED-LF-CODE TO PB-I-AH-BENEFIT-CD
004220           MOVE WS-LF-ABBR-DESC   TO PB-I-AH-ABBR
004221        ELSE
004222           IF BTYPE2      = SPACES
004223              MOVE SPACES         TO PB-I-AH-INPUT-CD
004224                                     PB-I-AH-ABBR
004225              MOVE ZEROS          TO PB-I-AH-BENEFIT-CD
004226           ELSE
004227              SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
004228              MOVE SPACES         TO PB-I-AH-INPUT-CD
004229                                     PB-I-AH-ABBR
004230              MOVE ZEROS          TO PB-I-AH-TERM
004231                                     PB-I-AH-BENEFIT-AMT
004232                                     PB-I-AH-PREMIUM-AMT
004233                                     PB-I-AH-BENEFIT-CD
004234                                     PB-I-AH-PREM-CALC
004235                                     PB-I-AH-CRIT-PER
004236                                     PB-I-TOT-FEES
004237                                     PB-I-TOT-FEES-CALC
004238              MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.
004239
004240     IF  BBENE2-LEN > ZEROS
004241         MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT.
004242
004243     IF  BPREM2-LEN > ZEROS
004244         IF WS-BPREM2       = WS-ALL-NINES OR
004245            WS-BPREM2       GREATER WS-ALL-NINES
004246            SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
004247            MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT
004248            MOVE '?'                TO PB-I-AH-CALC-FLAG
004249         ELSE
004250            SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
004251            MOVE WS-BPREM2       TO PB-I-AH-PREMIUM-AMT
004252            ADD  WS-BPREM2       TO PI-AH-ISS-ENTERED
004253            MOVE SPACE           TO PB-I-AH-CALC-FLAG.
004254
004255*    IF BCRIT-PERD2-LEN > ZEROS
004256*       MOVE WS-BCRIT-PERD2 (1)   TO PB-I-LF-CRIT-PER.
004257     MOVE ZEROS                  TO PB-I-LF-CRIT-PER
004258
004259     IF BCRIT-PERD2-LEN > ZEROS
004260        MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER.
004261
004262*    IF BIND-GRP-LEN     GREATER ZEROS
004263*        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.
004264
004265*    IF BRTCLS-LEN       GREATER ZEROS
004266*        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.
004267
004268*    IF BSIG-LEN         GREATER ZEROS
004269*        MOVE BSIG               TO PB-I-SIG-SW.
004270
004271     IF BAPR-LEN > zeros
004272        MOVE WS-BAPR             TO PB-I-LOAN-APR
004273     else
004274        if (pi-company-id = 'CID')
004275           and (ws-lf-earnings-calc = '5')
004276           move +99.9999         to pb-i-loan-apr
004277        else
004278           move zeros            to pb-i-loan-apr
004279        end-if
004280     end-if
004281
004282*    IF BMEM-NO-LEN      GREATER ZEROS
004283*        MOVE BMEM-NO        TO PB-I-MEMBER-NO.
004284
004285*    IF BMICROFILM-NO-LEN  GREATER  ZEROS
004286*        MOVE WS-I-MICRO-NO      TO  PB-I-MICROFILM-NO.
004287
004288     IF BLN-OFFICER-LEN  GREATER ZEROS
004289         MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
004290
004291*    IF BEXPIRE-LEN    (1)  GREATER ZEROS
004292*       MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT.
004293
004294*    IF BEXPIRE-LEN    (2)  GREATER ZEROS
004295*       MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT.
004296
004297     IF B1ST-PMT-LEN    GREATER ZEROS
004298        MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.
004299
004300*    IF BDAYS-LEN       GREATER ZEROS
004301*       MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS
004302*                                   PB-I-EXTENTION-DAYS.
004303
004304*    IF BDAYS-LEN GREATER ZEROS
004305*       MOVE '2'                 TO PB-I-DATA-ENTRY-SW.
004306
004307*    IF BEXPIRE-LEN    (1)  GREATER ZEROS
004308*       IF WS-CONVERTED-EXPIRDT   (1) GREATER LOW-VALUES
004309*          MOVE '3'              TO PB-I-DATA-ENTRY-SW.
004310
004311*    IF BEXPIRE-LEN    (2)  GREATER ZEROS
004312*       IF WS-CONVERTED-EXPIRDT   (2) GREATER LOW-VALUES
004313*          MOVE '3'              TO PB-I-DATA-ENTRY-SW.
004314
004315     IF B1ST-PMT-LEN GREATER ZEROS
004316        MOVE '4'                 TO PB-I-DATA-ENTRY-SW.
004317
004318*    IF PB-EXT-DAYS-PROCESSING
004319*       IF PB-I-EXTENTION-DAYS = ZEROS
004320*          MOVE '1'              TO PB-I-DATA-ENTRY-SW.
004321
004322*    IF PB-EXPIRE-DT-PROCESSING
004323*       IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND
004324*          PB-I-AH-EXPIRE-DT = LOW-VALUES
004325*          MOVE '1'              TO PB-I-DATA-ENTRY-SW.
004326
004327     IF PB-1ST-PMT-DT-PROCESSING
004328        IF PB-I-1ST-PMT-DT = LOW-VALUES
004329           MOVE '1'              TO PB-I-DATA-ENTRY-SW.
004330
004331*    IF BRINCD-LEN               GREATER ZEROS
004332*       MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.
004333
004334*    IF BBILLCD-LEN              GREATER ZEROS
004335*       MOVE BBILLCD             TO PB-RECORD-BILL
004336*    ELSE
004337*        IF PB-COMPANY-ID = 'LAP'  OR  'RMC'
004338*            MOVE 'H'            TO PB-RECORD-BILL.
004339
004340*    IF BENTRY-LEN   NOT = ZEROS
004341*       IF BENTRY = 'U' OR 'D'
004342*          MOVE BENTRY           TO PB-I-UNDERWRITING-STATUS
004343*                                   PB-BATCH-ENTRY
004344*       ELSE
004345*          MOVE BENTRY           TO PB-BATCH-ENTRY.
004346
004347*    IF BFORCE-LEN   NOT = ZEROS
004348*       MOVE BFORCE              TO PB-FORCE-CODE.
004349
004350     IF BVIN-LEN > ZEROS
004351        MOVE BVIN-NOI            TO PB-I-VIN
004352     END-IF
004353
004354*    IF BLIVES-LEN          GREATER ZEROS
004355*       MOVE WS-BLIVES           TO PB-I-LIVES.
004356
004357     IF BJNT-1ST-NAME-LEN   GREATER ZEROS
004358         MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
004359
004360     IF BJNT-INIT-LEN       GREATER ZEROS
004361         MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
004362
004363     IF BJNT-LST-NAME-LEN   GREATER ZEROS
004364         MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
004365
004366     IF BBENEFICIARY-LEN    GREATER ZEROS
004367         MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
004368
004369     MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
004370     MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
004371
004372     IF PI-MAIL-YES
004373        MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
004374
004375     MOVE 'C'                    TO JP-RECORD-TYPE.
004376     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
004377     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
004378
004379     
      * EXEC CICS REWRITE
004380*        DATASET (FILE-ID-ERPNDB)
004381*        FROM    (PENDING-BUSINESS)
004382*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009797' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004383
004384     MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.
004385
004386     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Issue Image
004387
004388     IF EIBAID = DFHENTER
004389         MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
004390         ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO
004391         MOVE AL-SABON               TO BSEQ-ATTRB.
004392
004393     IF PI-MAIL-YES
004394        NEXT SENTENCE
004395     ELSE
004396        GO TO 4900-EXIT.
004397
004398     IF BLAST-NAME-LEN = ZEROS AND
004399        B1ST-NAME-LEN  = ZEROS AND
004400        BINIT-LEN      = ZEROS AND
004401        BADDRS1-LEN    = ZEROS AND
004402        BADDRS2-LEN    = ZEROS AND
004403        BCITY-LEN      = ZEROS AND
004404        BSTATE-LEN     = ZEROS AND
004405        BZIPCDE-LEN    = ZEROS AND
004406*       BZIP4-LEN      = ZEROS AND
004407*       BPHONE-LEN     = ZEROS AND
004408        BBENEFICIARY-LEN = ZEROS AND
004409        BCADDR1-LEN    = ZEROS AND
004410        BCADDR2-LEN    = ZEROS AND
004411        BCCITY-LEN     = ZEROS AND
004412        BCSTATE-LEN    = ZEROS AND
004413        BCZIPCD-LEN    = ZEROS
004414        GO TO 4900-EXIT.
004415
004416     
      * EXEC CICS HANDLE CONDITION
004417*        NOTFND (4185-ADD-MAILING-RECORD)
004418*    END-EXEC.
      *    MOVE '"$I                   ! ) #00009834' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303039383334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004419
004420     
      * EXEC CICS READ
004421*        SET     (ADDRESS OF PENDING-MAILING-DATA)
004422*        DATASET (FILE-ID-ERPNDM)
004423*        RIDFLD  (ERPNDM-KEY)
004424*        UPDATE
004425*    END-EXEC.
      *    MOVE '&"S        EU         (   #00009838' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039383338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004426
004427     MOVE 'B'                    TO JP-RECORD-TYPE.
004428     MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
004429     MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
004430     MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
004431     PERFORM 8400-LOG-JOURNAL-RECORD.
004432
004433     MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.
004434     MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
004435     MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.
004436
004437     IF BLAST-NAME-LEN      GREATER ZEROS
004438         MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
004439
004440     IF B1ST-NAME-LEN       GREATER ZEROS
004441         MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
004442
004443     IF BINIT-LEN           GREATER ZEROS
004444         MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
004445
004446     IF BAGE-LEN            GREATER ZEROS
004447         MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.
004448
004449     IF BBIRTH-LEN > ZEROS
004450        MOVE  WS-CONVERTED-BIRTH (1) TO PM-INSURED-BIRTH-DT
004451     END-IF
004452
004453     IF BJNTDOB-LEN > ZEROS
004454        MOVE  WS-CONVERTED-BIRTH (2) TO PM-JOINT-BIRTH-DT
004455     END-IF
004456
004457     IF BLAST-NAME-LEN      GREATER ZEROS
004458         MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
004459
004460     IF BINIT-LEN           GREATER ZEROS
004461         MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
004462
004463     IF BADDRS1-LEN         GREATER ZERO
004464         MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
004465
004466     IF BADDRS2-LEN         GREATER ZERO
004467         MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
004468
004469     IF BCITY-LEN > 0
004470        MOVE BCITY               TO PM-CITY
004471     END-IF
004472
004473     IF BSTATE-LEN > 0
004474        MOVE BSTATE              TO PM-STATE
004475     END-IF
004476
004477     IF BZIPCDE-LEN GREATER ZEROS
004478        MOVE BZIPCDE             TO WS-ZIP-CODE
004479     ELSE
004480        GO TO 4010-CRED-BENE
004481     END-IF
004482
004483     IF WS-CANADIAN-ZIP
004484        IF WS-ZIP-4 = SPACE  OR  '-'
004485           MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
004486           MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
004487        ELSE
004488           MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
004489           MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
004490        END-IF
004491     ELSE
004492        IF WS-ZIP-6 = SPACE  OR  '-'
004493           MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
004494           MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
004495        ELSE
004496           MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
004497           MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4
004498        END-IF
004499     END-IF
004500
004501     .
004502 4010-CRED-BENE.
004503
004504     IF BBENEFICIARY-LEN > ZEROS
004505        MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
004506     END-IF
004507     IF BCADDR1-LEN > ZEROS
004508        MOVE BCADDR1             TO PM-CRED-BENE-ADDR
004509     END-IF
004510     IF BCADDR2-LEN > ZEROS
004511        MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
004512     END-IF
004513     IF BCCITY-LEN > ZEROS
004514        MOVE BCCITY              TO PM-CRED-BENE-CITY
004515     END-IF
004516     IF BCSTATE-LEN > ZEROS
004517        MOVE BCSTATE             TO PM-CRED-BENE-STATE
004518     END-IF
004519
004520     IF BCZIPCD-LEN > ZEROS
004521        MOVE BCZIPCD             TO WS-ZIP-CODE
004522     ELSE
004523        GO TO 4010-CONTINUE
004524     END-IF
004525
004526     IF WS-CANADIAN-ZIP
004527        IF WS-ZIP-4 = SPACE  OR  '-'
004528           MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
004529           MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
004530        ELSE
004531           MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
004532           MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
004533        END-IF
004534     ELSE
004535        IF WS-ZIP-6 = SPACE  OR  '-'
004536           MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
004537           MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
004538        ELSE
004539           MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
004540           MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
004541        END-IF
004542     END-IF
004543
004544     .
004545 4010-CONTINUE.
004546
004547*    IF BPHONE-LEN          GREATER ZERO
004548*        MOVE WS-BPHONE          TO PM-PHONE-NO.
004549
004550     MOVE 'C'                    TO JP-RECORD-TYPE.
004551     MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
004552     MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
004553     MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
004554
004555     
      * EXEC CICS REWRITE
004556*        DATASET (FILE-ID-ERPNDM)
004557*        FROM    (PENDING-MAILING-DATA)
004558*    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009973' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039393733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004559
004560     PERFORM 8400-LOG-JOURNAL-RECORD.
004561
004562     GO TO 4900-EXIT.
004563
004564     EJECT
004565
004566 4100-ADD-ISSUE-RECORD.
004567     
      * EXEC CICS GETMAIN
004568*        SET     (ADDRESS OF PENDING-BUSINESS)
004569*        LENGTH  (ERPNDB-RECORD-LENGTH)
004570*        INITIMG (GETMAIN-SPACE)
004571*    END-EXEC.
      *    MOVE ',"IL                  $   #00009985' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039393835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDB-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004572
004573     MOVE 'PB'                   TO PB-RECORD-ID.
004574     MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
004575                                    PB-COMPANY-CD-A1.
004576     MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
004577     MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
004578     MOVE BSEQ                   TO PB-BATCH-SEQ-NO.
004579
004580     IF BSEQ   GREATER PI-LAST-SEQ-NO-ADDED
004581         MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.
004582
004583     MOVE PI-SAV-CARRIER         TO PB-CARRIER.
004584     MOVE PI-SAV-GROUPING        TO PB-GROUPING.
004585     MOVE PI-SAV-STATE           TO PB-STATE.
004586     MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.
004587     MOVE '1'                    TO PB-RECORD-TYPE.
004588     MOVE BCERT                  TO PB-CERT-PRIME.
004589     MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.
004590     MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
004591                                    PB-ALT-CHG-SEQ-NO.
004592
004593     MOVE +0                     TO PB-NO-OF-ERRORS.
004594
004595     MOVE LOW-VALUES             TO PB-COMMON-ERRORS.
004596
004597     MOVE ZEROS                  TO PB-I-LOAN-TERM
004598                                    PB-I-LF-POLICY-FEE
004599                                    PB-I-LF-PREM-CALC
004600                                    PB-I-LF-ALT-PREM-CALC
004601                                    PB-I-LF-RATE
004602                                    PB-I-LF-ALT-RATE
004603                                    PB-I-LF-REI-RATE
004604                                    PB-I-LF-ALT-REI-RATE
004605                                    PB-I-RATE-DEV-PCT-LF
004606                                    PB-I-CANCEL-FEE
004607                                    PB-I-AH-PREM-CALC
004608                                    PB-I-TOT-FEES
004609                                    PB-I-TOT-FEES-CALC
004610                                    PB-I-AH-RATE
004611                                    PB-I-AH-REI-RATE
004612                                    PB-I-AH-RATE-TRM
004613                                    PB-I-RATE-DEV-PCT-AH
004614                                    PB-I-BUSINESS-TYPE
004615                                    PB-I-LIFE-COMMISSION
004616                                    PB-I-JOINT-COMMISSION
004617                                    PB-I-AH-COMMISSION
004618                                    PB-I-CURR-SEQ
004619                                    PB-CHG-COUNT
004620                                    PB-LF-BILLED-AMTS
004621                                    PB-AH-BILLED-AMTS
004622                                    PB-CALC-TOLERANCE
004623                                    PB-I-EXTENTION-DAYS
004624*                                   PB-I-MICROFILM-NO
004625                                    PB-I-TERM-IN-DAYS
004626                                    PB-I-STATE-TAX
004627                                    PB-I-MUNI-TAX
004628                                    PB-I-NUM-BILLED.
004629
004630     MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
004631                                    PB-I-LF-EXPIRE-DT
004632                                    PB-I-AH-EXPIRE-DT
004633                                    PB-I-1ST-PMT-DT
004634                                    PB-BILLED-DT
004635                                    PB-ACCT-EFF-DT
004636                                    PB-ACCT-EXP-DT.
004637
004638
004639     MOVE 'X'                    TO PB-FATAL-FLAG.
004640
004641     IF PI-MAIL-YES
004642        MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
004643
004644     IF PI-NB-MONTH-END-DT NOT = SPACES
004645        MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
004646       ELSE
004647        MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.
004648
004649     IF BSFX-LEN            GREATER ZEROS
004650         MOVE BSFX               TO PB-CERT-SFX.
004651
004652     IF BLAST-NAME-LEN      GREATER ZEROS
004653         MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
004654
004655     IF B1ST-NAME-LEN       GREATER ZEROS
004656         MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
004657
004658     IF BINIT-LEN           GREATER ZEROS
004659         MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
004660
004661     IF BAGE-LEN            GREATER ZEROS
004662         MOVE WS-BAGE            TO PB-I-AGE
004663     ELSE
004664         MOVE ZEROS              TO PB-I-AGE.
004665
004666     IF BJNT-AGE-LEN        GREATER ZEROS
004667         MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE
004668     ELSE
004669         MOVE ZEROS              TO PB-I-JOINT-AGE.
004670
004671     IF BBIRTH-LEN > ZEROS
004672        MOVE WS-CONVERTED-BIRTH (1) TO PB-I-BIRTHDAY
004673     ELSE
004674         MOVE LOW-VALUES          TO PB-I-BIRTHDAY.
004675
004676     IF BJNTDOB-LEN > ZEROS
004677        MOVE WS-CONVERTED-BIRTH (2)
004678                                 TO PB-I-JOINT-BIRTHDAY
004679     ELSE
004680        MOVE LOW-VALUES          TO PB-I-JOINT-BIRTHDAY
004681     END-IF
004682
004683     IF BTERM1-LEN > ZEROS
004684        MOVE WS-BTERM1              TO PB-I-LF-TERM
004685     ELSE
004686        MOVE ZEROS               TO PB-I-LF-TERM.
004687
004688     IF BTERM2-LEN > ZEROS
004689        MOVE WS-BTERM2              TO PB-I-AH-TERM
004690     ELSE
004691        MOVE ZEROS              TO PB-I-AH-TERM.
004692
004693     IF BLN-TERM-LEN        GREATER ZEROS
004694         MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM
004695     ELSE
004696         MOVE ZEROS              TO PB-I-LOAN-TERM.
004697
004698*    IF BFREQ-LEN           GREATER ZEROS
004699*        MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY
004700*    ELSE
004701         MOVE ZEROS              TO PB-I-PAY-FREQUENCY.
004702
004703*    IF BSKPCD-LEN          GREATER ZEROS
004704*        MOVE BSKPCD             TO PB-I-SKIP-CODE.
004705
004706*    IF BMODE-LEN           GREATER ZEROS
004707*        MOVE BMODE              TO PB-I-TERM-TYPE.
004708
004709*    IF BPMTS-LEN           GREATER ZEROS
004710*        MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS
004711*    ELSE
004712         MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.
004713
004714*    IF BPMT-LEN            GREATER ZEROS
004715*        MOVE WS-BPMT            TO PB-I-PAYMENT-AMOUNT
004716*    ELSE
004717         MOVE ZEROS              TO PB-I-PAYMENT-AMOUNT.
004718
004719*    IF BPOLICY-LEN         GREATER ZEROS
004720*        MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.
004721
004722     IF BTYPE1-LEN > ZEROS
004723        IF BTYPE1           NOT = ZEROS OR SPACES
004724           MOVE BTYPE1            TO PB-I-LF-INPUT-CD
004725           MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
004726           MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR
004727        ELSE
004728           MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD
004729     ELSE
004730           MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.
004731
004732     IF  BBENE1-LEN > ZEROS
004733         MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT
004734     ELSE
004735         MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.
004736
004737     IF  BALT-BEN1-LEN > ZEROS
004738         MOVE WS-BALT-BEN1       TO PB-I-LF-ALT-BENEFIT-AMT
004739     ELSE
004740         MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.
004741
004742     IF  BPREM1-LEN > ZEROS
004743         IF WS-BPREM1      = WS-ALL-NINES OR
004744            WS-BPREM1      GREATER WS-ALL-NINES
004745            MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
004746            MOVE '?'             TO PB-I-LF-CALC-FLAG
004747         ELSE
004748            ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED
004749            MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT
004750     ELSE
004751         MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.
004752
004753     IF  BALT-PREM1-LEN > ZEROS
004754         MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT
004755         ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED
004756     ELSE
004757         MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.
004758
004759     IF  BALT-PREM2-LEN > ZEROS
004760         MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
004761     ELSE
004762         MOVE ZEROS              TO PB-I-TOT-FEES
004763     END-IF
004764
004765     IF BTYPE2-LEN > ZEROS
004766        IF BTYPE2           NOT = ZEROS
004767           MOVE BTYPE2            TO PB-I-AH-INPUT-CD
004768           MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD
004769           MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR
004770        ELSE
004771           MOVE ZEROS             TO PB-I-AH-BENEFIT-CD
004772     ELSE
004773           MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.
004774
004775     IF  BBENE2-LEN > ZEROS
004776         MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT
004777     ELSE
004778         MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.
004779
004780     IF  BPREM2-LEN > ZEROS
004781         IF WS-BPREM2      = WS-ALL-NINES OR
004782            WS-BPREM2      GREATER WS-ALL-NINES
004783            MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT
004784            MOVE '?'              TO PB-I-AH-CALC-FLAG
004785         ELSE
004786            ADD  WS-BPREM2        TO PI-AH-ISS-ENTERED
004787            MOVE WS-BPREM2        TO PB-I-AH-PREMIUM-AMT
004788     ELSE
004789         MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.
004790
004791     IF PB-COMPANY-ID = 'NSL'
004792     IF PB-I-AGE       GREATER 49   OR
004793        PB-I-JOINT-AGE GREATER 49
004794         MOVE 'H'                TO PB-RECORD-BILL
004795       ELSE
004796     IF PB-I-LF-BENEFIT-AMT GREATER +14999.99 OR
004797        PB-I-AH-BENEFIT-AMT GREATER +14999.99
004798          MOVE 'H'                TO PB-RECORD-BILL.
004799
004800*    IF BCRIT-PERD-LEN      (1)   GREATER ZEROS
004801*       MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER
004802*    ELSE
004803        MOVE ZEROS                TO PB-I-LF-CRIT-PER.
004804
004805     IF BCRIT-PERD2-LEN > ZEROS
004806        MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER
004807     ELSE
004808        MOVE ZEROS                TO PB-I-AH-CRIT-PER.
004809
004810*    IF BIND-GRP-LEN        GREATER ZEROS
004811*        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.
004812
004813*    IF BRTCLS-LEN          GREATER ZEROS
004814*        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.
004815
004816*    IF BSIG-LEN            GREATER ZEROS
004817*        MOVE BSIG               TO PB-I-SIG-SW.
004818
004819     IF BAPR-LEN            GREATER ZEROS
004820         MOVE WS-BAPR            TO PB-I-LOAN-APR
004821     ELSE
004822        if (pi-company-id = 'CID')
004823           and (ws-lf-earnings-calc = '5')
004824           move +99.9999         to pb-i-loan-apr
004825        else
004826           move zeros            to pb-i-loan-apr
004827        end-if
004828     end-if
004829
004830*    IF BMEM-NO-LEN         GREATER ZEROS
004831*        MOVE BMEM-NO        TO PB-I-MEMBER-NO.
004832
004833*    IF BMICROFILM-NO-LEN  GREATER  ZEROS
004834*        MOVE WS-I-MICRO-NO      TO  PB-I-MICROFILM-NO.
004835
004836     IF BLN-OFFICER-LEN     GREATER ZEROS
004837         MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
004838
004839*    IF BEXPIRE-LEN    (1)  GREATER ZEROS
004840*       MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT
004841*    ELSE
004842        MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.
004843
004844*    IF BEXPIRE-LEN    (2)  GREATER ZEROS
004845*       MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT
004846*    ELSE
004847        MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.
004848
004849     IF B1ST-PMT-LEN        GREATER ZEROS
004850        MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.
004851
004852*    IF BDAYS-LEN           GREATER ZEROS
004853*       MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS
004854*                                   PB-I-EXTENTION-DAYS
004855*    ELSE
004856        MOVE ZEROS               TO PB-I-TERM-IN-DAYS
004857                                    PB-I-EXTENTION-DAYS.
004858
004859*    IF BDAYS-LEN GREATER ZEROS
004860*       MOVE '2'                 TO PB-I-DATA-ENTRY-SW.
004861
004862*    IF BEXPIRE-LEN    (1)  GREATER ZEROS
004863*       IF WS-CONVERTED-EXPIRDT   (1) GREATER LOW-VALUES
004864*          MOVE '3'              TO PB-I-DATA-ENTRY-SW.
004865
004866*    IF BEXPIRE-LEN    (2)  GREATER ZEROS
004867*       IF WS-CONVERTED-EXPIRDT   (2) GREATER LOW-VALUES
004868*          MOVE '3'              TO PB-I-DATA-ENTRY-SW.
004869
004870     IF B1ST-PMT-LEN GREATER ZEROS
004871        MOVE '4'                 TO PB-I-DATA-ENTRY-SW.
004872
004873*    IF PB-EXT-DAYS-PROCESSING
004874*       IF PB-I-EXTENTION-DAYS = ZEROS
004875*          MOVE '1'              TO PB-I-DATA-ENTRY-SW.
004876
004877*    IF PB-EXPIRE-DT-PROCESSING
004878*       IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND
004879*          PB-I-AH-EXPIRE-DT = LOW-VALUES
004880*            MOVE '1'            TO PB-I-DATA-ENTRY-SW.
004881
004882     IF PB-1ST-PMT-DT-PROCESSING
004883        IF PB-I-1ST-PMT-DT = LOW-VALUES
004884           MOVE '1'              TO PB-I-DATA-ENTRY-SW.
004885
004886*    IF BRINCD-LEN               GREATER ZEROS
004887*       MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.
004888
004889*    IF BBILLCD-LEN              GREATER ZEROS
004890*       MOVE BBILLCD             TO PB-RECORD-BILL
004891*    ELSE
004892*        IF PB-COMPANY-ID = 'LAP'  OR  'RMC'
004893*            MOVE 'H'            TO PB-RECORD-BILL.
004894
004895*    IF BENTRY-LEN   NOT = ZEROS
004896*       IF BENTRY = 'U' OR 'D'
004897*          MOVE BENTRY        TO PB-I-UNDERWRITING-STATUS
004898*                                PB-BATCH-ENTRY
004899*       ELSE
004900*          MOVE BENTRY        TO PB-BATCH-ENTRY.
004901
004902*    IF BFORCE-LEN   NOT = ZEROS
004903*       MOVE BFORCE              TO PB-FORCE-CODE.
004904
004905     IF BVIN-LEN > ZEROS
004906        MOVE BVIN-NOI            TO PB-I-VIN
004907     ELSE
004908        MOVE SPACES              TO PB-I-VIN
004909     END-IF
004910
004911*    IF BLIVES-LEN          GREATER ZEROS
004912*       MOVE WS-BLIVES           TO PB-I-LIVES
004913*    ELSE
004914        MOVE ZEROS               TO PB-I-LIVES.
004915
004916     IF BJNT-1ST-NAME-LEN   GREATER ZEROS
004917         MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
004918
004919     IF BJNT-INIT-LEN       GREATER ZEROS
004920         MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
004921
004922     IF BJNT-LST-NAME-LEN   GREATER ZEROS
004923         MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
004924
004925     IF BBENEFICIARY-LEN    GREATER ZEROS
004926         MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
004927
004928     MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
004929     MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
004930
004931 4175-WRITE-PB-RECORD.
004932     MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
004933                                    PB-INPUT-BY.
004934     MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
004935     MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
004936                                    PB-INPUT-DT.
004937
004938     MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.
004939
004940     MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.
004941     MOVE PI-CSR-ID              TO PB-CSR-ID.
004942     MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.
004943     MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.
004944     MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.
004945
004946     MOVE 'A'                    TO JP-RECORD-TYPE.
004947     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
004948     MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY
004949                                    ERPNDM-KEY.
004950
004951*    MOVE PI-SAV-REFERENCE       TO PB-I-REFERENCE.
004952     ADD +1                      TO PI-SAV-BATCH-SEQ.
004953
004954     
      * EXEC CICS HANDLE CONDITION
004955*        DUPREC (4200-DUPLICATE-ALT-INDEX)
004956*    END-EXEC.
      *    MOVE '"$%                   ! * #00010372' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303130333732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004957
004958     
      * EXEC CICS WRITE
004959*        DATASET (FILE-ID-ERPNDB)
004960*        FROM    (PENDING-BUSINESS)
004961*        RIDFLD  (PB-CONTROL-PRIMARY)
004962*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010376' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130333736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004963
004964     ADD +1                      TO PI-ISS-CNT-ENTERED.
004965
004966     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
004967     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
004968     PERFORM 8400-LOG-JOURNAL-RECORD  *>  Add Issue Image
004969
004970     MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.
004971     MOVE AL-SABON               TO BSEQ-ATTRB.
004972
004973     ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.
004974
004975     EJECT
004976
004977******************************************************************
004978*    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *
004979*    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *
004980*    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *
004981******************************************************************
004982
004983     IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE
004984        ELSE
004985         GO TO 4185-ADD-MAILING-RECORD.
004986
004987     IF  PI-ISSUE-ADDED
004988         GO TO 4185-ADD-MAILING-RECORD.
004989
004990     MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.
004991
004992     
      * EXEC CICS HANDLE CONDITION
004993*        NOTFND (4185-ADD-MAILING-RECORD)
004994*    END-EXEC.
      *    MOVE '"$I                   ! + #00010410' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303130343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004995
004996     MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.
004997     MOVE PI-SAV-FC-CARRIER      TO  ELCERT-CARRIER.
004998     MOVE PI-SAV-FC-GROUPING     TO  ELCERT-GROUPING.
004999     MOVE PI-SAV-FC-STATE        TO  ELCERT-STATE.
005000
005001     
      * EXEC CICS READ
005002*        SET     (ADDRESS OF CERTIFICATE-MASTER)
005003*        DATASET (FILE-ID-ELCERT)
005004*        RIDFLD  (ELCERT-KEY)
005005*        LENGTH  (ELCERT-RECORD-LENGTH)
005006*        UPDATE
005007*    END-EXEC.
      *    MOVE '&"SL       EU         (   #00010419' TO DFHEIV0
           MOVE X'2622534C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130343139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 DFHEIV20, 
                 ELCERT-RECORD-LENGTH, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005008
005009     IF  CERT-WAS-CREATED-FOR-CLAIM
005010         GO TO 4185-ADD-MAILING-RECORD.
005011
005012     go to 4900-exit
005013     .
005014 4185-ADD-MAILING-RECORD.
005015
005016     IF  PI-MAIL-YES
005017
005018         IF  BADDRS1-LEN > ZERO
005019                 OR
005020
005021             BBENEFICIARY-LEN > ZERO
005022                  OR
005023             BADDRS2-LEN > ZERO
005024                 OR
005025             BCITY-LEN > ZERO
005026                 OR
005027             BSTATE-LEN > ZERO
005028                 OR
005029             BCADDR1-LEN > ZERO
005030                 OR
005031             BCADDR2-LEN > ZERO
005032                 OR
005033             BCCITY-LEN > ZERO
005034                 OR
005035             BCSTATE-LEN > ZERO
005036             NEXT SENTENCE
005037
005038         ELSE
005039             GO TO 4900-EXIT
005040
005041     ELSE
005042         GO TO 4900-EXIT.
005043
005044     
      * EXEC CICS GETMAIN
005045*        SET     (ADDRESS OF PENDING-MAILING-DATA)
005046*        LENGTH  (ERPNDM-RECORD-LENGTH)
005047*        INITIMG (GETMAIN-SPACE)
005048*    END-EXEC.
      *    MOVE ',"IL                  $   #00010462' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130343632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDM-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005049
005050     MOVE 'PM'                   TO PM-RECORD-ID.
005051     MOVE 'ER'                   TO PM-SOURCE-SYSTEM.
005052
005053     MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY
005054                                    PM-RECORD-ADDED-BY.
005055     MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
005056     MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT
005057                                    PM-RECORD-ADD-DT.
005058
005059     MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.
005060
005061     IF BLAST-NAME-LEN      GREATER ZEROS
005062         MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
005063
005064     IF B1ST-NAME-LEN       GREATER ZEROS
005065         MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
005066
005067     IF BINIT-LEN           GREATER ZEROS
005068         MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
005069
005070     IF BAGE-LEN            GREATER ZEROS
005071         MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE
005072     ELSE
005073         MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.
005074
005075     IF BBIRTH-LEN          GREATER ZEROS
005076        MOVE  WS-CONVERTED-BIRTH (1) TO PM-INSURED-BIRTH-DT
005077     ELSE
005078        MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.
005079
005080     IF BJNTDOB-LEN > ZEROS
005081        MOVE WS-CONVERTED-BIRTH (2)
005082                                 TO PM-JOINT-BIRTH-DT
005083     ELSE
005084        MOVE LOW-VALUES          TO PM-JOINT-BIRTH-DT
005085     END-IF
005086
005087     IF BLAST-NAME-LEN      GREATER ZEROS
005088         MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
005089
005090     IF BINIT-LEN           GREATER ZEROS
005091         MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
005092
005093     IF BADDRS1-LEN         GREATER ZERO
005094         MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
005095
005096     IF BADDRS2-LEN         GREATER ZERO
005097         MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
005098
005099     IF BCITY-LEN > 0
005100        MOVE BCITY               TO PM-CITY
005101     END-IF
005102
005103     IF BSTATE-LEN > 0
005104        MOVE BSTATE              TO PM-STATE
005105     END-IF
005106
005107     IF BZIPCDE-LEN GREATER ZEROS
005108         MOVE BZIPCDE            TO  WS-ZIP-CODE
005109     ELSE
005110         GO TO 4188-CRED-BENE.
005111
005112     IF WS-CANADIAN-ZIP
005113         IF WS-ZIP-4 = SPACE  OR  '-'
005114             MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
005115             MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
005116         ELSE
005117             MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
005118             MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
005119     ELSE
005120         IF WS-ZIP-6 = SPACE  OR  '-'
005121             MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
005122             MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
005123         ELSE
005124             MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
005125             MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.
005126
005127     .
005128 4188-CRED-BENE.
005129
005130
005131     IF BBENEFICIARY-LEN > ZEROS
005132        MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
005133     END-IF
005134     IF BCADDR1-LEN > ZEROS
005135        MOVE BCADDR1             TO PM-CRED-BENE-ADDR
005136     END-IF
005137     IF BCADDR2-LEN > ZEROS
005138        MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
005139     END-IF
005140     IF BCCITY-LEN > ZEROS
005141        MOVE BCCITY              TO PM-CRED-BENE-CITY
005142     END-IF
005143     IF BCSTATE-LEN > ZEROS
005144        MOVE BCSTATE             TO PM-CRED-BENE-STATE
005145     END-IF
005146
005147     IF BCZIPCD-LEN > ZEROS
005148        MOVE BCZIPCD             TO WS-ZIP-CODE
005149     ELSE
005150        GO TO 4188-CONTINUE
005151     END-IF
005152
005153     IF WS-CANADIAN-ZIP
005154        IF WS-ZIP-4 = SPACE  OR  '-'
005155           MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
005156           MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
005157        ELSE
005158           MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
005159           MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
005160        END-IF
005161     ELSE
005162        IF WS-ZIP-6 = SPACE  OR  '-'
005163           MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
005164           MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
005165        ELSE
005166           MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
005167           MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
005168        END-IF
005169     END-IF
005170
005171     .
005172 4188-CONTINUE.
005173
005174*    IF BPHONE-LEN          GREATER ZERO
005175*        MOVE WS-BPHONE          TO PM-PHONE-NO
005176*    ELSE
005177         MOVE ZEROS              TO PM-PHONE-NO.
005178
005179     MOVE 'A'                    TO JP-RECORD-TYPE.
005180     MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
005181     MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
005182     MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
005183
005184     
      * EXEC CICS WRITE
005185*        DATASET (FILE-ID-ERPNDM)
005186*        FROM    (PENDING-MAILING-DATA)
005187*        RIDFLD  (PM-CONTROL-PRIMARY)
005188*    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010602' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130363032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 PM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005189
005190     PERFORM 8400-LOG-JOURNAL-RECORD.
005191
005192     MOVE LOW-VALUES             TO MAP-B.
005193
005194     GO TO 4900-EXIT.
005195
005196 4200-DUPLICATE-ALT-INDEX.
005197     MOVE ER-2247                TO EMI-ERROR.
005198     MOVE -1                     TO BCERT-LEN.
005199     MOVE AL-UABON               TO BCERT-ATTRB.
005200     MOVE AL-UNBON               TO BEFFDT-ATTRB.
005201     MOVE 'Y'                    TO PI-ERROR-SW.
005202
005203     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005204
005205     IF BPREM1-LEN > ZEROS
005206         SUBTRACT WS-BPREM1
005207                                 FROM PI-LF-ISS-ENTERED.
005208
005209     IF BALT-PREM1-LEN > ZEROS
005210         SUBTRACT WS-BALT-PREM1
005211                                 FROM PI-LF-ISS-ENTERED.
005212
005213     IF BPREM2-LEN > ZEROS
005214         SUBTRACT WS-BPREM2
005215                                 FROM PI-AH-ISS-ENTERED.
005216
005217     SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED
005218                                      PI-SAV-BATCH-SEQ.
005219
005220 4900-EXIT.
005221     EXIT.
005222
005223     EJECT
005224
005225 5000-BUILD-CANCEL-RECORD.
005226     MOVE +0                     TO WS-SUB2.
005227
005228 5025-PROCESS-CANCEL.
005229     ADD +1                      TO WS-SUB2.
005230
005231     IF PI-LAST-FUNC-DISPLAY
005232        IF WS-SUB2 GREATER +1
005233           GO TO 5900-EXIT.
005234
005235     IF WS-SUB2 GREATER +4
005236        GO TO 5900-EXIT.
005237
005238     IF CCERT-LEN    (WS-SUB2) = ZEROS AND
005239        CEFFDT-LEN   (WS-SUB2) = ZEROS AND
005240        CCANDT1-LEN  (WS-SUB2) = ZEROS AND
005241        CCANDT2-LEN  (WS-SUB2) = ZEROS AND
005242        CMTHD1-LEN   (WS-SUB2) = ZEROS AND
005243        CMTHD2-LEN   (WS-SUB2) = ZEROS AND
005244        CREFUND1-LEN (WS-SUB2) = ZEROS AND
005245        CREFUND2-LEN (WS-SUB2) = ZEROS AND
005246        CLIVES-LEN   (WS-SUB2) = ZEROS AND
005247        CCHK-LEN     (WS-SUB2) = ZEROS
005248           GO TO 5025-PROCESS-CANCEL.
005249
005250     IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED
005251         GO TO 5100-ADD-CANCEL-RECORD.
005252
005253******************************************************************
005254*   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
005255*   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
005256*   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
005257*   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
005258*   THROUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS  *
005259*   REWRITTEN, ELSE A NEW PB-RECORD IS ADDED.                    *
005260******************************************************************
005261
005262     MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
005263     MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
005264     MOVE CSEQ (WS-SUB2)         TO ERPNDB-BATCH-SEQ.
005265
005266     
      * EXEC CICS HANDLE CONDITION
005267*        NOTFND (5100-ADD-CANCEL-RECORD)
005268*    END-EXEC.
      *    MOVE '"$I                   ! , #00010684' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303130363834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005269
005270     
      * EXEC CICS READ
005271*        SET     (ADDRESS OF PENDING-BUSINESS)
005272*        DATASET (FILE-ID-ERPNDB)
005273*        RIDFLD  (ERPNDB-KEY)
005274*        UPDATE
005275*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010688' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130363838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005276
005277     MOVE 'B'                    TO JP-RECORD-TYPE
005278     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
005279     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
005280     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
005281
005282     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Cancel Image
005283
005284     MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
005285     MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
005286     MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
005287
005288     IF CSFX-LEN  (WS-SUB2) GREATER ZEROS
005289        MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
005290
005291     IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
005292         MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
005293
005294     IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS
005295         IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
005296            WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES
005297            SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
005298            MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
005299            MOVE '?'              TO PB-C-LF-CALC-REQ
005300         ELSE
005301            SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
005302            ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
005303            MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT
005304            MOVE SPACE                 TO PB-C-LF-CALC-REQ.
005305
005306     IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS
005307         IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
005308            WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES
005309            SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
005310            MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
005311            MOVE '?'              TO PB-C-AH-CALC-REQ
005312         ELSE
005313            SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
005314            ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
005315            MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT
005316            MOVE SPACE                 TO PB-C-AH-CALC-REQ.
005317
005318******************************************************************
005319*      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *
005320******************************************************************
005321
005322     IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
005323        MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT
005324        IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES
005325             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
005326             MOVE ZEROS          TO PB-C-LF-REF-CALC
005327                                    PB-C-LF-CANCEL-AMT.
005328
005329     IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
005330        MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT
005331        IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES
005332             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
005333             MOVE ZEROS          TO PB-C-AH-REF-CALC
005334                                    PB-C-AH-CANCEL-AMT.
005335
005336     IF CMTHD1-LEN (WS-SUB2) GREATER THAN +0
005337        MOVE CMTHD1 (WS-SUB2)    TO PB-C-LF-REFUND-OVERRIDE.
005338
005339     IF CMTHD2-LEN (WS-SUB2) GREATER THAN +0
005340        MOVE CMTHD2 (WS-SUB2)    TO PB-C-AH-REFUND-OVERRIDE.
005341
005342     IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS
005343        MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.
005344
005345     IF CCANREA-LEN (WS-SUB2) > ZEROS
005346        MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
005347     END-IF
005348
005349*    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS
005350*        MOVE WS-MICRO-NO (WS-SUB2)
005351*                                TO  PB-C-MICROFILM-NO.
005352
005353     IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS
005354        MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
005355
005356     IF CCHK-LEN    (WS-SUB2) GREATER ZEROS
005357        MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
005358
005359     MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
005360     MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
005361
005362     MOVE 'C'                    TO JP-RECORD-TYPE.
005363     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
005364     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
005365     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
005366
005367     
      * EXEC CICS REWRITE
005368*        DATASET (FILE-ID-ERPNDB)
005369*        FROM    (PENDING-BUSINESS)
005370*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010785' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130373835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005371
005372     MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.
005373
005374     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Cancel Image
005375
005376     MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
005377
005378     IF EIBAID = DFHENTER
005379         MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
005380         ADD +1 TO PI-NEXT-DISPLAY-SEQ-NO
005381         MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
005382
005383     GO TO 5900-EXIT.
005384
005385     EJECT
005386
005387 5100-ADD-CANCEL-RECORD.
005388     
      * EXEC CICS GETMAIN
005389*        SET     (ADDRESS OF PENDING-BUSINESS)
005390*        LENGTH  (ERPNDB-RECORD-LENGTH)
005391*        INITIMG (GETMAIN-SPACE)
005392*    END-EXEC.
      *    MOVE ',"IL                  $   #00010806' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130383036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDB-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005393
005394     MOVE 'PB'                   TO PB-RECORD-ID.
005395     MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
005396                                    PB-COMPANY-CD-A1.
005397     MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
005398     MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
005399     MOVE CSEQ (WS-SUB2)         TO PB-BATCH-SEQ-NO.
005400
005401     IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED
005402        MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.
005403
005404     MOVE PI-SAV-CARRIER         TO PB-CARRIER.
005405     MOVE PI-SAV-GROUPING        TO PB-GROUPING.
005406     MOVE PI-SAV-STATE           TO PB-STATE.
005407     MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.
005408     MOVE '2'                    TO PB-RECORD-TYPE.
005409     MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.
005410     MOVE WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
005411                                 TO PB-CERT-EFF-DT.
005412
005413*    MOVE PI-SAV-REFERENCE       TO PB-C-REFERENCE.
005414
005415     MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
005416                                    PB-ALT-CHG-SEQ-NO.
005417
005418     MOVE +0                     TO PB-NO-OF-ERRORS.
005419
005420     MOVE LOW-VALUES             TO PB-COMMON-ERRORS.
005421
005422     MOVE ZEROS                  TO PB-C-LF-REF-CALC
005423                                    PB-C-AH-REF-CALC
005424                                    PB-C-LF-RFND-CLP
005425                                    PB-C-AH-RFND-CLP
005426                                    PB-CI-INSURED-AGE
005427                                    PB-CI-LF-TERM
005428                                    PB-CI-AH-TERM
005429                                    PB-CI-LF-BENEFIT-CD
005430                                    PB-CI-LF-BENEFIT-AMT
005431                                    PB-CI-LF-ALT-BENEFIT-AMT
005432                                    PB-CI-LF-PREMIUM-AMT
005433                                    PB-CI-LF-ALT-PREMIUM-AMT
005434                                    PB-CI-AH-BENEFIT-CD
005435                                    PB-CI-AH-BENEFIT-AMT
005436                                    PB-CI-AH-PREMIUM-AMT
005437                                    PB-CI-PAY-FREQUENCY
005438                                    PB-CI-LOAN-APR
005439                                    PB-CI-LOAN-TERM
005440                                    PB-CI-LIFE-COMMISSION
005441                                    PB-CI-AH-COMMISSION
005442                                    PB-CI-CURR-SEQ
005443                                    PB-CI-AH-CANCEL-AMT
005444                                    PB-CI-LF-CANCEL-AMT
005445                                    PB-CI-RATE-DEV-PCT-LF
005446                                    PB-CI-RATE-DEV-PCT-AH
005447                                    PB-CI-EXTENTION-DAYS
005448                                    PB-CI-TERM-IN-DAYS
005449                                    PB-CI-LIVES
005450                                    PB-CI-LF-CRIT-PER
005451                                    PB-CI-AH-CRIT-PER
005452                                    PB-C-LF-REM-TERM
005453                                    PB-C-AH-REM-TERM
005454                                    PB-CHG-COUNT
005455                                    PB-LF-BILLED-AMTS
005456                                    PB-AH-BILLED-AMTS
005457*                                   PB-C-MICROFILM-NO
005458                                    PB-C-INT-ON-REFS
005459                                    PB-CALC-TOLERANCE.
005460
005461     MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT
005462                                    PB-CI-AH-SETTLEMENT-DT
005463                                    PB-CI-DEATH-DT
005464                                    PB-CI-LF-PRIOR-CANCEL-DT
005465                                    PB-CI-AH-PRIOR-CANCEL-DT
005466                                    PB-CI-ENTRY-DT
005467                                    PB-CI-LF-EXPIRE-DT
005468                                    PB-CI-AH-EXPIRE-DT
005469                                    PB-CI-LOAN-1ST-PMT-DT
005470                                    PB-C-LF-CANCEL-DT
005471                                    PB-C-AH-CANCEL-DT
005472                                    PB-CREDIT-ACCEPT-DT
005473                                    PB-BILLED-DT
005474                                    PB-ACCT-EFF-DT
005475                                    PB-ACCT-EXP-DT.
005476
005477     IF PI-NB-MONTH-END-DT NOT = SPACES
005478        MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
005479       ELSE
005480        MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.
005481
005482     MOVE 'X'                    TO PB-FATAL-FLAG.
005483
005484     IF CSFX-LEN  (WS-SUB2) GREATER ZEROS
005485        MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
005486
005487     IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
005488         MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
005489
005490     IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
005491        MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.
005492
005493     IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
005494        MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.
005495
005496     IF CMTHD1-LEN (WS-SUB2) GREATER ZEROS
005497        MOVE CMTHD1 (WS-SUB2) TO PB-C-LF-REFUND-OVERRIDE.
005498
005499     IF CMTHD2-LEN (WS-SUB2) GREATER ZEROS
005500        MOVE CMTHD2 (WS-SUB2) TO PB-C-AH-REFUND-OVERRIDE.
005501
005502     IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS
005503         IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
005504            WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES
005505            MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
005506            MOVE '?'              TO PB-C-LF-CALC-REQ
005507         ELSE
005508            ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
005509            MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT
005510     ELSE
005511         MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.
005512
005513     IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS
005514         IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
005515            WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES
005516            MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
005517            MOVE '?'              TO PB-C-AH-CALC-REQ
005518         ELSE
005519            ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
005520            MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT
005521     ELSE
005522         MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.
005523
005524     IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS
005525        MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES
005526     ELSE
005527        MOVE ZEROS               TO PB-C-LIVES.
005528
005529     IF CCANREA-LEN (WS-SUB2) > ZEROS
005530        MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
005531     END-IF
005532
005533*    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS
005534*        MOVE WS-MICRO-NO (WS-SUB2)
005535*                                TO  PB-C-MICROFILM-NO
005536*    ELSE
005537*        MOVE ZEROS              TO  PB-C-MICROFILM-NO.
005538
005539     IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS
005540        MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
005541
005542     IF CCHK-LEN    (WS-SUB2) GREATER ZEROS
005543        MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
005544
005545     IF PB-COMPANY-ID = 'LAP'  OR  'RMC'
005546         MOVE 'H'                TO PB-RECORD-BILL.
005547
005548     MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
005549     MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
005550
005551     MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
005552                                    PB-INPUT-BY.
005553     MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
005554     MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
005555                                    PB-INPUT-DT.
005556
005557     MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.
005558
005559     MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.
005560     MOVE PI-CSR-ID              TO PB-CSR-ID.
005561     MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.
005562     MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.
005563     MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.
005564
005565     MOVE 'A'                    TO JP-RECORD-TYPE.
005566     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
005567     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
005568     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
005569     MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY.
005570     ADD +1                      TO PI-SAV-BATCH-SEQ.
005571
005572     
      * EXEC CICS HANDLE CONDITION
005573*        DUPREC (5200-DUPLICATE-ALT-INDEX)
005574*    END-EXEC.
      *    MOVE '"$%                   ! - #00010990' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303130393930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005575
005576     
      * EXEC CICS WRITE
005577*        DATASET (FILE-ID-ERPNDB)
005578*        FROM    (PENDING-BUSINESS)
005579*        RIDFLD  (PB-CONTROL-PRIMARY)
005580*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010994' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130393934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005581
005582     ADD +1                      TO PI-CAN-CNT-ENTERED.
005583
005584     PERFORM 8400-LOG-JOURNAL-RECORD  *> Add BHDR Image
005585
005586     MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
005587     MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).
005588     MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).
005589
005590     ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.
005591
005592     GO TO 5025-PROCESS-CANCEL.
005593
005594 5200-DUPLICATE-ALT-INDEX.
005595     MOVE ER-2247                TO EMI-ERROR.
005596     MOVE -1                     TO CCERT-LEN    (WS-SUB2).
005597     MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).
005598     MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).
005599     MOVE 'Y'                    TO PI-ERROR-SW.
005600
005601     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005602
005603     IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS
005604         SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.
005605
005606     IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS
005607         SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.
005608
005609     GO TO 8200-SEND-DATAONLY.
005610
005611 5900-EXIT.
005612     EXIT.
005613
005614     EJECT
005615
005616 6000-DELETE-PEND-BUS-RECORD.
005617     MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
005618     MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
005619
005620     IF PI-MAP-NAME = EL630B
005621         MOVE BSEQ               TO ERPNDB-BATCH-SEQ
005622     ELSE
005623         MOVE CSEQ (1)           TO ERPNDB-BATCH-SEQ.
005624
005625     
      * EXEC CICS HANDLE CONDITION
005626*        NOTFND (6990-REC-NOTFND)
005627*    END-EXEC.
      *    MOVE '"$I                   ! . #00011043' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303131303433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005628
005629     
      * EXEC CICS READ
005630*        SET     (ADDRESS OF PENDING-BUSINESS)
005631*        DATASET (FILE-ID-ERPNDB)
005632*        RIDFLD  (ERPNDB-KEY)
005633*        UPDATE
005634*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011047' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131303437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005635
005636******************************************************************
005637*    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *
005638*    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *
005639*    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*
005640*    THROUGH REVIEW AND CORRECTION.                              *
005641******************************************************************
005642
005643     IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE
005644        ELSE
005645         GO TO 6880-DELETE-ERROR.
005646
005647     IF PB-ISSUE
005648         SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED
005649         SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
005650         SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED
005651         SUBTRACT +1 FROM PI-ISS-CNT-ENTERED
005652     ELSE
005653         SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
005654         SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
005655         SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.
005656
005657 6300-DELETE-PB-RECORD.
005658     MOVE 'D'                    TO JP-RECORD-TYPE.
005659     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
005660     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
005661     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
005662
005663     
      * EXEC CICS DELETE
005664*        DATASET (FILE-ID-ERPNDB)
005665*    END-EXEC.
      *    MOVE '&(                    &   #00011081' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303131303831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005666
005667     PERFORM 8400-LOG-JOURNAL-RECORD  *> Journal Delete
005668
005669     MOVE 'Y'                    TO PI-UPDATE-SW.
005670     MOVE ER-0000                TO EMI-ERROR.
005671
005672     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005673
005674     ADD +1             PI-LAST-SEQ-NO-ADDED
005675                     GIVING PI-NEXT-DISPLAY-SEQ-NO.
005676
005677     IF PI-MAP-NAME = EL630B
005678         MOVE LOW-VALUES         TO MAP-B
005679         PERFORM 8550-SET-MAP-SEQ-NOS
005680     ELSE
005681         MOVE SPACE              TO PI-DISPLAY-SW
005682         MOVE LOW-VALUES         TO MAP-C
005683         PERFORM 8550-SET-MAP-SEQ-NOS
005684                 VARYING WS-SUB2 FROM +1 BY +1
005685                 UNTIL WS-SUB2 GREATER +5.
005686
005687     GO TO 8100-SEND-INITIAL-MAP.
005688
005689 6410-READ-ERACCT.
005690
005691     MOVE PB-CONTROL-BY-ACCOUNT  TO ERACCT-KEY
005692     MOVE LOW-VALUES             TO ERACCT-FILL
005693
005694     
      * EXEC CICS READ
005695*         DATASET   ('ERACCT')
005696*         INTO      (ACCOUNT-MASTER)
005697*         RIDFLD    (ERACCT-KEY)
005698*         GTEQ
005699*         RESP      (WS-RESPONSE)
005700*    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00011112' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303131313132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005701
005702     .
005703 6410-EXIT.
005704     EXIT.
005705
005706 6880-DELETE-ERROR.
005707     
      * EXEC CICS UNLOCK
005708*         DATASET (FILE-ID-ERPNDB)
005709*    END-EXEC.
      *    MOVE '&*                    #   #00011125' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131313235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005710
005711     MOVE ER-2901        TO EMI-ERROR.
005712     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005713     IF PI-MAP-NAME = EL630B
005714         MOVE -1                 TO BPFENTRL
005715     ELSE
005716         MOVE -1                 TO CPFENTRL.
005717
005718     GO TO 8200-SEND-DATAONLY.
005719
005720 6990-REC-NOTFND.
005721     MOVE ER-2433                TO EMI-ERROR
005722
005723     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005724
005725     IF PI-MAP-NAME = EL630B
005726         MOVE -1                 TO BPFENTRL
005727     ELSE
005728         MOVE -1                 TO CPFENTRL.
005729
005730     GO TO 8200-SEND-DATAONLY.
005731
005732     EJECT
005733
005734 7000-FORMAT-ISSUE-SCREEN.
005735     MOVE 'Y'                        TO PI-DISPLAY-SW.
005736     MOVE LOW-VALUES                 TO DATA-AREA-B.
005737     MOVE -1                         TO BPFENTRL.
005738     MOVE PB-BATCH-SEQ-NO            TO BSEQ.
005739     MOVE AL-SABON                   TO BSEQ-ATTRB.
005740     MOVE PB-CERT-PRIME              TO BCERT.
005741     MOVE AL-SANON                   TO BCERT-ATTRB.
005742     MOVE PB-CERT-SFX                TO BSFX.
005743     MOVE AL-SANOF                   TO BSFX-ATTRB.
005744     MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
005745     MOVE SPACE                      TO DC-OPTION-CODE.
005746     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
005747     MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.
005748     MOVE AL-SANON                   TO BEFFDT-ATTRB.
005749
005750     MOVE PI-LIFE-OVERRIDE-L2        TO BKIND1
005751     MOVE PI-AH-OVERRIDE-L2          TO BKIND2
005752
005753     IF PB-I-INSURED-LAST-NAME GREATER SPACES
005754        MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.
005755
005756     IF PB-I-INSURED-FIRST-NAME GREATER SPACES
005757        MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.
005758
005759     IF PB-I-INSURED-MIDDLE-INIT GREATER SPACES
005760        MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.
005761
005762     IF PB-I-AGE GREATER ZEROS
005763        MOVE PB-I-AGE                TO BAGE.
005764
005765     IF PB-I-JOINT-AGE GREATER ZEROS
005766        MOVE PB-I-JOINT-AGE          TO BJNT-AGE.
005767
005768     IF PB-I-BIRTHDAY NOT = LOW-VALUES
005769        MOVE PB-I-BIRTHDAY           TO DC-BIN-DATE-1
005770        MOVE SPACE                   TO DC-OPTION-CODE
005771        PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
005772        MOVE DC-GREG-DATE-1-MDY      TO BBIRTH-DT.
005773
005774     IF PB-I-JOINT-BIRTHDAY NOT = LOW-VALUES
005775        MOVE PB-I-JOINT-BIRTHDAY TO DC-BIN-DATE-1
005776        MOVE SPACE               TO DC-OPTION-CODE
005777        PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
005778        MOVE DC-GREG-DATE-1-MDY  TO BJNTDOB-DT
005779     END-IF
005780
005781     IF PB-I-LF-TERM GREATER ZEROS
005782        MOVE PB-I-LF-TERM            TO BTERM1O.
005783
005784     IF PB-I-AH-TERM GREATER ZEROS
005785        MOVE PB-I-AH-TERM            TO BTERM2O.
005786
005787     IF PB-I-LOAN-TERM GREATER ZEROS
005788        MOVE PB-I-LOAN-TERM          TO BLN-TERMO.
005789
005790*    IF PB-I-PAY-FREQUENCY GREATER ZEROS
005791*       MOVE PB-I-PAY-FREQUENCY      TO BFREQO.
005792
005793*    IF PB-I-SKIP-CODE GREATER SPACES
005794*       MOVE PB-I-SKIP-CODE          TO BSKPCD.
005795
005796*    IF PB-I-TERM-TYPE GREATER SPACES
005797*       MOVE PB-I-TERM-TYPE          TO BMODE.
005798
005799*    IF PB-I-NO-OF-PAYMENTS GREATER ZEROS
005800*       MOVE PB-I-NO-OF-PAYMENTS     TO BPMTS-OUT.
005801
005802*    IF PB-I-PAYMENT-AMOUNT GREATER ZEROS
005803*       MOVE PB-I-PAYMENT-AMOUNT     TO BPMTO.
005804
005805*    IF PB-I-POLICY-FORM-NO GREATER SPACES
005806*       MOVE PB-I-POLICY-FORM-NO     TO BPOLICY.
005807
005808     IF PB-I-LF-INPUT-CD GREATER SPACES
005809        MOVE PB-I-LF-INPUT-CD        TO BTYPE1.
005810
005811     IF PB-I-LF-BENEFIT-AMT GREATER ZEROS
005812        MOVE PB-I-LF-BENEFIT-AMT     TO BBENE1O.
005813
005814     IF PB-I-LF-ALT-BENEFIT-AMT GREATER ZEROS
005815        MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BEN1O.
005816
005817     IF PB-I-LF-PREMIUM-AMT GREATER ZEROS
005818        MOVE PB-I-LF-PREMIUM-AMT     TO BPREM1O.
005819
005820     IF PB-I-LF-ALT-PREMIUM-AMT GREATER ZEROS
005821        MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREM1O.
005822
005823     IF PB-I-AH-INPUT-CD GREATER SPACES
005824        MOVE PB-I-AH-INPUT-CD        TO BTYPE2.
005825
005826     IF PB-I-AH-BENEFIT-AMT GREATER ZEROS
005827        MOVE PB-I-AH-BENEFIT-AMT     TO BBENE2O.
005828
005829     IF PB-I-AH-PREMIUM-AMT GREATER ZEROS
005830        MOVE PB-I-AH-PREMIUM-AMT     TO BPREM2O.
005831
005832*    IF PB-I-LF-CRIT-PER GREATER ZEROS
005833*       MOVE PB-I-LF-CRIT-PER        TO BCRIT-PERDO (1).
005834
005835     IF PB-I-AH-CRIT-PER GREATER ZEROS
005836        MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERD2O.
005837
005838*    IF PB-BATCH-ENTRY GREATER SPACES
005839*       MOVE PB-BATCH-ENTRY          TO BENTRY.
005840
005841*    IF PB-FORCE-CODE  GREATER SPACES
005842*       MOVE PB-FORCE-CODE           TO BFORCE.
005843
005844*    IF PB-I-SPECIAL-REIN-CODE GREATER SPACE
005845*      MOVE PB-I-SPECIAL-REIN-CODE   TO BRINCD.
005846
005847*    IF PB-RECORD-BILL         GREATER SPACE
005848*      MOVE PB-RECORD-BILL           TO BBILLCD.
005849
005850*    IF PB-I-INDV-GRP-OVRD GREATER SPACES
005851*       MOVE PB-I-INDV-GRP-OVRD      TO BIND-GRP.
005852
005853*    IF PB-I-RATE-CLASS-OVRD GREATER SPACES
005854*       MOVE PB-I-RATE-CLASS-OVRD    TO BRTCLS.
005855
005856*    IF PB-I-SIG-SW GREATER SPACES
005857*        MOVE PB-I-SIG-SW            TO BSIG.
005858
005859     IF PB-I-LOAN-APR GREATER ZEROS
005860        MOVE PB-I-LOAN-APR           TO BAPR-OUT.
005861
005862*    IF PB-I-MEMBER-NO GREATER SPACES
005863*       MOVE PB-I-MEMBER-NO          TO BMEM-NO.
005864
005865*    IF PI-COMPANY-ID EQUAL 'HER'
005866*        MOVE PB-I-MEMBER-NO     TO  WS-MEMBER-NO
005867*        IF WS-MEMBER-NO-1-8  IS NUMERIC
005868*            IF WS-MEMBER-NO-1-8  GREATER  ZEROS
005869*                MOVE WS-MEMBER-NO-1-8
005870*                                TO  BMICROFILM-NOO.
005871
005872*    IF PB-I-MICROFILM-NO  IS NUMERIC
005873*        IF PB-I-MICROFILM-NO  GREATER  ZEROS
005874*            MOVE PB-I-MICROFILM-NO
005875*                                TO  BMICROFILM-NOO.
005876
005877     IF PB-I-LOAN-OFFICER GREATER SPACES
005878        MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.
005879
005880*    IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES
005881*       MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1
005882*       MOVE SPACE               TO DC-OPTION-CODE
005883*       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
005884*       MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (1).
005885
005886*    IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES
005887*       MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1
005888*       MOVE SPACE               TO DC-OPTION-CODE
005889*       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
005890*       MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (2).
005891
005892     IF PB-I-1ST-PMT-DT GREATER LOW-VALUES
005893        MOVE PB-I-1ST-PMT-DT     TO DC-BIN-DATE-1
005894        MOVE SPACE               TO DC-OPTION-CODE
005895        PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
005896        MOVE DC-GREG-DATE-1-MDY  TO B1ST-PMT.
005897
005898*    IF PB-I-EXTENTION-DAYS NUMERIC
005899*       IF PB-I-EXTENTION-DAYS NOT = ZEROS
005900*          MOVE PB-I-EXTENTION-DAYS  TO BDAYSO
005901*       ELSE
005902*          IF PB-I-TERM-IN-DAYS NUMERIC
005903*             IF PB-I-TERM-IN-DAYS NOT = ZEROS
005904*                MOVE PB-I-TERM-IN-DAYS TO BDAYSO.
005905
005906     IF PB-I-VIN > ZEROS
005907        MOVE PB-I-VIN            TO BVIN-NOI
005908     END-IF
005909
005910*    IF PB-I-LIVES GREATER ZEROS
005911*       MOVE PB-I-LIVES              TO BLIVESO.
005912
005913     IF PB-I-JOINT-FIRST-NAME GREATER SPACES
005914        MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.
005915
005916     IF PB-I-JOINT-MIDDLE-INIT GREATER SPACES
005917        MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.
005918
005919     IF PB-I-JOINT-LAST-NAME GREATER SPACES
005920        MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.
005921
005922     IF PB-I-BENEFICIARY-NAME GREATER SPACES
005923        MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.
005924
005925     MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.
005926
005927     
      * EXEC CICS HANDLE CONDITION
005928*        NOTFND (7090-EXIT)
005929*    END-EXEC.
      *    MOVE '"$I                   ! / #00011345' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303131333435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005930
005931     
      * EXEC CICS READ
005932*        SET     (ADDRESS OF PENDING-MAILING-DATA)
005933*        DATASET (FILE-ID-ERPNDM)
005934*        RIDFLD  (ERPNDM-KEY)
005935*        UPDATE
005936*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011349' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131333439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005937
005938     IF PM-ADDRESS-LINE-1 GREATER SPACES
005939        MOVE PM-ADDRESS-LINE-1       TO BADDRS1.
005940
005941     IF PM-ADDRESS-LINE-2 GREATER SPACES
005942        MOVE PM-ADDRESS-LINE-2       TO BADDRS2.
005943
005944     IF PM-CITY > SPACES
005945        MOVE PM-CITY                 TO BCITY
005946     END-IF
005947
005948     IF PM-STATE > SPACES
005949        MOVE PM-STATE                TO BSTATE
005950     END-IF
005951
005952     IF PM-ZIP            GREATER SPACES
005953         MOVE SPACES               TO WS-ZIP-CODE
005954         IF PM-CANADIAN-POST-CODE
005955             MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1
005956             MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2
005957             MOVE WS-ZIP-CODE      TO BZIPCDE
005958         ELSE
005959             MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE
005960             MOVE WS-ZIP-CODE      TO BZIPCDE
005961             IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
005962                 MOVE '-'          TO WS-ZIP-AM-2-DASH
005963                 MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4
005964                 MOVE WS-ZIP-CODE  TO BZIPCDE.
005965
005966*    IF PM-PHONE-NO NUMERIC
005967*       IF PM-PHONE-NO GREATER ZEROS
005968*          MOVE PM-PHONE-NO          TO  BPHONE-NO
005969*          INSPECT BPHONE-NO CONVERTING ' ' TO '-'.
005970
005971
005972     IF PM-CRED-BENE-ADDR > SPACES
005973        MOVE PM-CRED-BENE-ADDR       TO BCADDR1
005974     END-IF
005975
005976     IF PM-CRED-BENE-ADDR2 > SPACES
005977        MOVE PM-CRED-BENE-ADDR2     TO BCADDR2
005978     END-IF
005979
005980     IF PM-CRED-BENE-CITY > SPACES
005981        MOVE PM-CRED-BENE-CITY       TO BCCITY
005982     END-IF
005983     IF PM-CRED-BENE-STATE > SPACES
005984        MOVE PM-CRED-BENE-STATE      TO BCSTATE
005985     END-IF
005986
005987     IF PM-CRED-BENE-ZIP > SPACES
005988        MOVE SPACES               TO WS-ZIP-CODE
005989        IF PM-CB-CANADIAN-POST-CODE
005990           MOVE PM-CB-CAN-POST1     TO WS-ZIP-CAN-2-POST1
005991           MOVE PM-CB-CAN-POST2     TO WS-ZIP-CAN-2-POST2
005992           MOVE WS-ZIP-CODE      TO BZIPCDE
005993        ELSE
005994           MOVE PM-CB-ZIP-CODE      TO WS-ZIP-AM-2-CODE
005995           MOVE WS-ZIP-CODE      TO BCZIPCD
005996           IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
005997              MOVE '-'          TO WS-ZIP-AM-2-DASH
005998              MOVE PM-CB-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4
005999              MOVE WS-ZIP-CODE  TO BCZIPCD
006000           END-IF
006001        END-IF
006002     END-IF
006003
006004     .
006005 7090-EXIT.
006006     EXIT.
006007
006008     EJECT
006009
006010 7100-FORMAT-CANCEL-SCREEN.
006011     MOVE 'Y'                    TO PI-DISPLAY-SW.
006012
006013     MOVE LOW-VALUES             TO DATA-AREA-C (2)
006014                                    DATA-AREA-C (3).
006015
006016     MOVE -1                     TO CPFENTRL.
006017
006018     MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).
006019     MOVE AL-SABON               TO CSEQ-ATTRB  (1).
006020     MOVE PB-CERT-PRIME          TO CCERT       (1).
006021     MOVE AL-SANON               TO CCERT-ATTRB (1).
006022     MOVE PB-CERT-SFX            TO CSFX        (1).
006023     MOVE AL-SANON               TO CSFX-ATTRB  (1).
006024
006025     MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.
006026     MOVE SPACE                  TO DC-OPTION-CODE.
006027     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
006028     MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).
006029     MOVE AL-SANON               TO CEFFDT-ATTRB (1).
006030     MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).
006031
006032     IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
006033        MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1
006034        MOVE SPACE               TO DC-OPTION-CODE
006035        PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
006036        MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)
006037        MOVE AL-UANON            TO CCANDT1-ATTRB (1).
006038
006039     IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES
006040        MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
006041        MOVE SPACE               TO DC-OPTION-CODE
006042        PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
006043        MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)
006044        MOVE AL-UANON            TO CCANDT2-ATTRB (1).
006045
006046     IF PB-C-LF-REFUND-OVERRIDE EQUAL SPACES
006047        NEXT SENTENCE
006048     ELSE
006049        MOVE PB-C-LF-REFUND-OVERRIDE
006050                                 TO CMTHD1 (1)
006051        MOVE AL-UANON            TO CMTHD1-ATTRB (1).
006052
006053     IF PB-C-AH-REFUND-OVERRIDE EQUAL SPACES
006054        NEXT SENTENCE
006055     ELSE
006056        MOVE PB-C-AH-REFUND-OVERRIDE
006057                                 TO CMTHD2 (1)
006058        MOVE AL-UANON            TO CMTHD2-ATTRB (1).
006059
006060     IF PB-C-LF-CANCEL-AMT NOT =  ZEROS
006061        MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)
006062        MOVE AL-UNNON            TO CREFUND1-ATTRB (1).
006063
006064     IF PB-C-AH-CANCEL-AMT NOT =  ZEROS
006065        MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)
006066        MOVE AL-UNNON            TO CREFUND2-ATTRB (1).
006067
006068     IF PB-C-LIVES GREATER ZEROS
006069        MOVE PB-C-LIVES          TO CLIVESO      (1)
006070        MOVE AL-UNNON            TO CLIVES-ATTRB (1).
006071
006072     IF PB-C-CANCEL-REASON GREATER SPACES
006073        MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
006074        MOVE AL-UANON            TO CCANREA-ATTRB (1)
006075     ELSE
006076        PERFORM 6410-READ-ERACCT THRU 6410-EXIT
006077        IF AM-GPCD > 1
006078          AND AM-GPCD < 6
006079           MOVE 'Y'                 TO PB-C-CANCEL-REASON
006080           MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
006081           MOVE AL-UANON            TO CCANREA-ATTRB (1)
006082        END-IF
006083     END-IF.
006084
006085*    IF PB-C-MICROFILM-NO  IS NUMERIC
006086*        IF PB-C-MICROFILM-NO  NOT =  ZEROS
006087*            MOVE PB-C-MICROFILM-NO
006088*                                TO  CMICRO-NOO (1)
006089*            MOVE AL-UNNON       TO  CMICRO-NO-ATTRB (1).
006090
006091     IF PB-C-PAYEE-CODE GREATER SPACES
006092        MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)
006093        MOVE AL-UANON            TO CPAYEE-ATTRB (1).
006094
006095     IF PB-C-REFUND-SW  GREATER SPACES
006096        MOVE PB-C-REFUND-SW      TO CCHK         (1)
006097        MOVE AL-UANON            TO CCHK-ATTRB   (1).
006098
006099     IF PB-C-LAST-NAME GREATER SPACES
006100        MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)
006101        MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).
006102
006103     PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1
006104                                 UNTIL WS-SUB2 GREATER +4.
006105
006106     GO TO 7190-EXIT.
006107
006108 7180-PROTECT-FIELDS.
006109     MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)
006110                                    CSFX-ATTRB       (WS-SUB2)
006111                                    CEFFDT-ATTRB     (WS-SUB2)
006112                                    CLAST-NAME-ATTRB (WS-SUB2)
006113                                    CCANDT1-ATTRB    (WS-SUB2)
006114                                    CCANDT2-ATTRB    (WS-SUB2)
006115                                    CMTHD1-ATTRB     (WS-SUB2)
006116                                    CMTHD2-ATTRB     (WS-SUB2)
006117                                    CREFUND1-ATTRB   (WS-SUB2)
006118                                    CREFUND2-ATTRB   (WS-SUB2)
006119                                    CCHK-ATTRB       (WS-SUB2)
006120                                    CPAYEE-ATTRB     (WS-SUB2)
006121                                    CLIVES-ATTRB     (WS-SUB2)
006122                                    CCANREA-ATTRB    (WS-SUB2).
006123
006124 7190-EXIT.
006125
006126     EJECT
006127
006128 8100-SEND-INITIAL-MAP.
006129     IF PI-MAP-NAME = EL630B
006130         NEXT SENTENCE
006131     ELSE
006132         GO TO 8110-SEND-INITIAL-CANCEL-MAP.
006133
006134*    MOVE PI-MEMBER-CAPTION        TO BCAPTNO.
006135
006136     IF EIBAID NOT = DFHPF1   AND
006137        EIBAID NOT = DFHPF2   AND
006138        EIBAID NOT = DFHPF5   AND
006139        PI-MAINT-FUNC NOT = 'B'
006140          PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.
006141
006142     MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.
006143     MOVE PI-AM-NAME             TO BACCTNMO
006144     IF ((PI-AM-ADDR1 NOT = SPACES)
006145        OR (PI-AM-ADDR2 NOT = SPACES))
006146        IF BNFICRYO (1:5) = LOW-VALUES OR '_____'
006147           MOVE PI-AM-NAME       TO BNFICRYO
006148           MOVE AL-UANON         TO BBENEFICIARY-ATTRB
006149        END-IF
006150        IF BCADDR1 (1:5) = LOW-VALUES OR '_____'
006151           MOVE PI-AM-ADDR2      TO BCADDR1
006152           MOVE AL-UANON         TO BCADDR1-ATTRB
006153        END-IF
006154        IF BCADDR2 = SPACES OR LOW-VALUES
006155*          MOVE PI-AM-ADDR2      TO BCADDR2
006156           MOVE AL-UANON         TO BCADDR2-ATTRB
006157        END-IF
006158        IF BCCITY = SPACES OR LOW-VALUES
006159           MOVE PI-AM-CITY       TO BCCITY
006160           MOVE AL-UANON         TO BCCITY-ATTRB
006161        END-IF
006162        IF BCSTATE = SPACES OR LOW-VALUES
006163           MOVE PI-AM-STATE      TO BCSTATE
006164           MOVE AL-UANON         TO BCSTATE-ATTRB
006165        END-IF
006166        IF BCZIPCD = SPACES OR LOW-VALUES
006167           MOVE PI-AM-ZIP        TO BCZIPCD
006168           MOVE AL-UANON         TO BCZIPCD-ATTRB
006169        END-IF
006170     END-IF
006171
006172     IF PI-AM-EDIT-LOAN-OFC = 'N'
006173        MOVE AL-SANOF            TO BLN-OFFICER-ATTRB
006174     END-IF
006175
006176     IF PI-MAIL-YES
006177        continue
006178     ELSE
006179        MOVE AL-SANOF            TO BADDRS1-ATTRB
006180                                    BADDRS2-ATTRB
006181                                    BCITY-ATTRB
006182                                    BSTATE-ATTRB
006183                                    BZIPCDE-ATTRB
006184*                                   BZIP4-ATTRB
006185*                                   BPHONE-ATTRB
006186     END-IF
006187
006188     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
006189        MOVE AL-SADOF            TO balt-ben2-attrb
006190                                    balt-prem2-attrb
006191     END-IF
006192
006193     IF PI-COMPANY-ID NOT = 'DCC' and 'CID' and 'VPP'
006194        MOVE AL-SADOF            TO BVINHD-ATTRB
006195                                    BVIN-ATTRB
006196     END-IF
006197
006198*    IF PI-LAST-FUNC-DISPLAY
006199*          MOVE AL-SADOF            TO BCERTV-ATTRB
006200*                                      BSFXV-ATTRB
006201*                                      BEFFDTV-ATTRB
006202*                                      BLAST-NAMEV-ATTRB
006203*    ELSE
006204*       IF PI-COMPANY-ID = ('PEM' OR 'CRI')
006205*          MOVE AL-UANOF            TO BCERTV-ATTRB
006206*                                      BSFXV-ATTRB
006207*                                      BEFFDTV-ATTRB
006208*                                      BLAST-NAMEV-ATTRB
006209*       ELSE
006210*          MOVE AL-SADOF            TO BCERTV-ATTRB
006211*                                      BSFXV-ATTRB
006212*                                      BEFFDTV-ATTRB
006213*                                      BLAST-NAMEV-ATTRB.
006214
006215     IF PI-LAST-FUNC-DISPLAY
006216        NEXT SENTENCE
006217     ELSE
006218        IF PI-COMPANY-ID = 'PEM' OR 'CRI'
006219           MOVE AL-UADOF            TO BCERT-ATTRB
006220                                       BSFX-ATTRB
006221                                       BEFFDT-ATTRB
006222                                       BLAST-NAME-ATTRB
006223        ELSE
006224           MOVE AL-UANOF            TO BCERT-ATTRB
006225                                       BEFFDT-ATTRB.
006226
006227     IF PI-NB-MONTH-END-DT NOT = SPACES
006228        MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1
006229       ELSE
006230        MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.
006231
006232     MOVE SPACE                  TO DC-OPTION-CODE.
006233     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
006234     MOVE DC-GREG-DATE-1-EDIT    TO BMOENDO.
006235
006236     MOVE WS-CURRENT-DT          TO BDATEO.
006237     MOVE EIBTIME                TO TIME-IN.
006238     MOVE TIME-OUT               TO BTIMEO.
006239
006240     MOVE PI-LIFE-OVERRIDE-L2    TO BKIND1
006241     MOVE AL-SABOF               TO BKIND1-ATTRB
006242     MOVE PI-AH-OVERRIDE-L2      TO BKIND2
006243     MOVE AL-SABOF               TO BKIND2-ATTRB
006244
006245     IF PI-DATA-ERRORS
006246        MOVE SPACE               TO PI-ERROR-SW
006247     ELSE
006248        IF EIBAID = DFHPF1 OR DFHPF2
006249           CONTINUE
006250        ELSE
006251           MOVE -1               TO BCERTL
006252         END-IF
006253     END-IF
006254
006255     MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.
006256     MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.
006257
006258     
      * EXEC CICS SEND
006259*        MAP      (PI-MAP-NAME)
006260*        MAPSET   (MAPSET-EL6301S)
006261*        FROM     (EL630BI)
006262*        ERASE
006263*        CURSOR
006264*    END-EXEC.
           MOVE LENGTH OF
            EL630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00011676' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303131363736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL630BI, 
                 DFHEIV12, 
                 MAPSET-EL6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006265
006266     GO TO 9100-RETURN-TRAN.
006267
006268     EJECT
006269 8110-SEND-INITIAL-CANCEL-MAP.
006270     IF EIBAID NOT = DFHPF5  AND
006271        EIBAID NOT = DFHPF1  AND
006272        EIBAID NOT = DFHPF2  AND
006273        PI-MAINT-FUNC NOT = 'B'
006274          PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.
006275
006276     MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.
006277     MOVE PI-AM-NAME             TO CACCTNMO.
006278
006279     IF PI-NB-MONTH-END-DT NOT = SPACES
006280        MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1
006281       ELSE
006282        MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.
006283
006284     MOVE SPACE                  TO DC-OPTION-CODE.
006285     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
006286     MOVE DC-GREG-DATE-1-EDIT    TO CMOENDO.
006287
006288     MOVE WS-CURRENT-DT          TO CDATEO.
006289     MOVE EIBTIME                TO TIME-IN.
006290     MOVE TIME-OUT               TO CTIMEO.
006291
006292     MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1)
006293                                    CKIND1 (2)
006294                                    CKIND1 (3)
006295                                    CKIND1 (4).
006296     MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1)
006297                                    CKIND2 (2)
006298                                    CKIND2 (3)
006299                                    CKIND2 (4).
006300     MOVE AL-SABOF               TO CKIND1-ATTRB (1)
006301                                    CKIND1-ATTRB (2)
006302                                    CKIND1-ATTRB (3)
006303                                    CKIND1-ATTRB (4)
006304                                    CKIND2-ATTRB (1)
006305                                    CKIND2-ATTRB (2)
006306                                    CKIND2-ATTRB (3)
006307                                    CKIND2-ATTRB (4).
006308
006309     IF PI-DATA-ERRORS
006310         MOVE SPACE              TO PI-ERROR-SW
006311     ELSE
006312         IF EIBAID = DFHPF1 OR DFHPF2
006313             NEXT SENTENCE
006314         ELSE
006315             MOVE -1             TO CCERT1L.
006316
006317     MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.
006318     MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.
006319
006320     
      * EXEC CICS SEND
006321*        MAP      (PI-MAP-NAME)
006322*        MAPSET   (MAPSET-EL6301S)
006323*        FROM     (EL630BI)
006324*        ERASE
006325*        CURSOR
006326*    END-EXEC.
           MOVE LENGTH OF
            EL630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00011738' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303131373338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL630BI, 
                 DFHEIV12, 
                 MAPSET-EL6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006327
006328     GO TO 9100-RETURN-TRAN.
006329
006330     EJECT
006331
006332 8200-SEND-DATAONLY.
006333     MOVE SPACE              TO PI-ERROR-SW.
006334
006335     IF PI-MAP-NAME = EL630B
006336*        MOVE PI-MEMBER-CAPTION      TO BCAPTNO
006337         MOVE WS-CURRENT-DT          TO BDATEO
006338         MOVE EIBTIME                TO TIME-IN
006339         MOVE TIME-OUT               TO BTIMEO
006340         MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O
006341         MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O
006342         
      * EXEC CICS SEND
006343*            MAP      (PI-MAP-NAME)
006344*            MAPSET   (MAPSET-EL6301S)
006345*            FROM     (EL630BI)
006346*            DATAONLY
006347*            CURSOR
006348*        END-EXEC
           MOVE LENGTH OF
            EL630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00011760' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303131373630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL630BI, 
                 DFHEIV12, 
                 MAPSET-EL6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006349     ELSE
006350         MOVE WS-CURRENT-DT          TO CDATEO
006351         MOVE EIBTIME                TO TIME-IN
006352         MOVE TIME-OUT               TO CTIMEO
006353         MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O
006354         MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O
006355         
      * EXEC CICS SEND
006356*            MAP      (PI-MAP-NAME)
006357*            MAPSET   (MAPSET-EL6301S)
006358*            FROM     (EL630BI)
006359*            DATAONLY
006360*            CURSOR
006361*        END-EXEC.
           MOVE LENGTH OF
            EL630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00011773' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303131373733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL630BI, 
                 DFHEIV12, 
                 MAPSET-EL6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006362
006363     GO TO 9100-RETURN-TRAN.
006364
006365     EJECT
006366
006367 8300-SEND-TEXT.
006368     
      * EXEC CICS SEND TEXT
006369*        FROM     (LOGOFF-TEXT)
006370*        LENGTH   (LOGOFF-LENGTH)
006371*        ERASE
006372*        FREEKB
006373*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00011786' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303131373836' TO DFHEIV0
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
           
006374
006375     
      * EXEC CICS RETURN
006376*    END-EXEC.
      *    MOVE '.(                    ''   #00011793' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131373933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006377
006378 8350-SEND-WARNING.
006379     
      * EXEC CICS SEND TEXT
006380*        FROM     (WARNING-TEXT)
006381*        LENGTH   (WARNING-LENGTH)
006382*        ERASE
006383*        FREEKB
006384*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00011797' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303131373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WARNING-TEXT, 
                 WARNING-LENGTH, 
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
           
006385
006386     GO TO 9100-RETURN-TRAN.
006387
006388 8400-LOG-JOURNAL-RECORD.
006389
006390     if (pi-journal-file-id > 0)
006391        and (jp-file-id = file-id-erpndb)
006392
006393        move eibdate             to jp-date
006394        move eibtime             to jp-time
006395        MOVE PI-PROCESSOR-ID     TO JP-USER-ID
006396        MOVE 03                  TO PI-JOURNAL-FILE-ID
006397        MOVE THIS-PGM            TO JP-PROGRAM-ID
006398
006399**      length is 585 plus 30 extra for jrnl stuff
006400**      system already accounts for the 34.
006401
006402        
      * EXEC CICS JOURNAL
006403*          JFILEID   (PI-JOURNAL-FILE-ID)
006404*          JTYPEID   ('EL')
006405*          FROM      (JOURNAL-RECORD)
006406*          LENGTH    (615)
006407*          resp      (ws-response)
006408*       END-EXEC
           MOVE 'EL' TO DFHEIV7
           MOVE 615
             TO DFHEIV11
      *    MOVE '4"LF                  (  N#00011820' TO DFHEIV0
           MOVE X'34224C462020202020202020' &
                X'202020202020202020202820' &
                X'204E233030303131383230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-JOURNAL-FILE-ID, 
                 DFHEIV7, 
                 JOURNAL-RECORD, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006409
006410        if resp-normal
006411           continue
006412        else
006413           display ' error-el6301-journal ' ws-response
006414        end-if
006415     end-if
006416
006417*    EXEC CICS JOURNAL
006418*        JFILEID     (PI-JOURNAL-FILE-ID)
006419*        JTYPEID     ('EL')
006420*        FROM        (JOURNAL-RECORD)
006421*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
006422*        END-EXEC.
006423
006424     .
006425 8400-exit.
006426     exit.
006427
006428 8500-DATE-CONVERT.
006429     
      * EXEC CICS LINK
006430*        PROGRAM  (LINK-ELDATCV)
006431*        COMMAREA (DATE-CONVERSION-DATA)
006432*        LENGTH   (DC-COMM-LENGTH)
006433*    END-EXEC.
      *    MOVE '."C                   (   #00011847' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303131383437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006434
006435 8500-EXIT.
006436     EXIT.
006437
006438     EJECT
006439
006440 8550-SET-MAP-SEQ-NOS.
006441     IF PI-MAP-NAME = EL630B
006442         MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
006443         MOVE AL-SABON               TO BSEQ-ATTRB
006444     ELSE
006445         MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
006446         MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
006447
006448     ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.
006449
006450 8555-EXIT.
006451     EXIT.
006452
006453 8600-DEEDIT.
006454     
      * EXEC CICS BIF DEEDIT
006455*        FIELD   (DEEDIT-FIELD)
006456*        LENGTH  (15)
006457*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00011872' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131383732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006458
006459 8600-EXIT.
006460     EXIT.
006461
006462 8800-UNAUTHORIZED-ACCESS.
006463     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
006464     GO TO 8300-SEND-TEXT.
006465
006466 9000-CONNECT-TO-DB.
006467
006468     IF SVR > SPACES
006469        CONTINUE
006470     ELSE
006471*  The below is unnecessary but I'll play along
006472        MOVE 'PROD_LOGIC'           TO SVR
006473        MOVE 'appuser'              TO USR
006474        MOVE 'appuser@cso'          TO PASS
006475     END-IF
006476
006477     STRING
006478         USR DELIMITED SPACE
006479         "." DELIMITED SIZE
006480         PASS DELIMITED SPACE INTO USR-PASS
006481     END-STRING
006482
006484     EXEC SQL
              CONNECT TO :SVR USER :USR-PASS
006485     END-EXEC
006486
006487     IF SQLCODE NOT = 0
006488        DISPLAY "ERROR: CANNOT CONNECT "
006489        DISPLAY SQLCODE
006490        DISPLAY SQLERRMC
006491     END-IF
006492
006493     .
006494 9000-EXIT.
006495     EXIT.
006496 9050-DISCONNECT.
006497
006499     EXEC SQL
              DISCONNECT
006500     END-EXEC
006501     .
006502 9050-EXIT.
006503     EXIT.
006504
006505 9000-RETURN-CICS.
006506     
      * EXEC CICS RETURN
006507*    END-EXEC.
      *    MOVE '.(                    ''   #00011924' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006508
006509 9100-RETURN-TRAN.
006510     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
006511
006512     IF  PI-MAP-NAME = EL630B
006513         MOVE '630B'             TO PI-CURRENT-SCREEN-NO.
006514
006515     IF  PI-MAP-NAME = EL630C
006516         MOVE '630C'             TO PI-CURRENT-SCREEN-NO.
006517
006518     
      * EXEC CICS RETURN
006519*        TRANSID    (TRANS-EXA6)
006520*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
006521*        LENGTH     (WS-COMM-LENGTH)
006522*    END-EXEC.
      *    MOVE '.(CT                  ''   #00011936' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131393336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXA6, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006523
006524 9300-XCTL.
006525     
      * EXEC CICS XCTL
006526*        PROGRAM    (PGM-NAME)
006527*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
006528*        LENGTH     (WS-COMM-LENGTH)
006529*    END-EXEC.
      *    MOVE '.$C                   %   #00011943' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131393433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006530
006531 9400-CLEAR.
006532     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
006533     GO TO 9300-XCTL.
006534
006535 9500-PF12.
006536     MOVE XCTL-EL010             TO PGM-NAME.
006537     GO TO 9300-XCTL.
006538
006539 9600-PGMID-ERROR.
006540     
      * EXEC CICS HANDLE CONDITION
006541*        PGMIDERR    (8300-SEND-TEXT)
006542*    END-EXEC.
      *    MOVE '"$L                   ! 0 #00011958' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303131393538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006543
006544     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
006545     MOVE ' '                    TO PI-ENTRY-CD-1.
006546     MOVE XCTL-EL005            TO PGM-NAME.
006547     MOVE PGM-NAME               TO LOGOFF-PGM.
006548     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
006549     GO TO 9300-XCTL.
006550
006551 9900-ERROR-FORMAT.
006552     IF PI-MAP-NAME = EL630B
006553        MOVE 2                   TO EMI-NUMBER-OF-LINES
006554       ELSE
006555        MOVE 1                   TO EMI-NUMBER-OF-LINES.
006556
006557     IF NOT EMI-ERRORS-COMPLETE
006558         MOVE LINK-EL001         TO PGM-NAME
006559         
      * EXEC CICS LINK
006560*            PROGRAM    (PGM-NAME)
006561*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
006562*            LENGTH     (EMI-COMM-LENGTH)
006563*        END-EXEC.
      *    MOVE '."C                   (   #00011977' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303131393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006564
006565 9900-EXIT.
006566     EXIT.
006567
006568 9990-ABEND.
006569     MOVE LINK-EL004             TO PGM-NAME.
006570     MOVE DFHEIBLK               TO EMI-LINE1.
006571     
      * EXEC CICS LINK
006572*        PROGRAM   (PGM-NAME)
006573*        COMMAREA  (EMI-LINE1)
006574*        LENGTH    (72)
006575*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00011989' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303131393839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006576
006577     IF PI-MAP-NAME = EL630B
006578         MOVE -1 TO BPFENTRL
006579     ELSE
006580         MOVE -1 TO CPFENTRL.
006581
006582     GO TO 8200-SEND-DATAONLY.
006583
006584     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
006585
006586 9995-SECURITY-VIOLATION.
006587*    COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00012023' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132303233' TO DFHEIV0
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
006588
006589 9995-EXIT.
006590     EXIT.
006591

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1078-NO-RECORD,
                     1078-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2020-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2100-BROWSE-BKWD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3300-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3200-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4100-ADD-ISSUE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5100-ADD-CANCEL-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6990-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7090-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
