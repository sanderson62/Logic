      *((program: EL160.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL160 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/16/95 15:27:49.
000007*                            VMOD=2.018.
000008*
000009*
000010*AUTHOR.        LOGIC, INC.
000011*               DALLAS, TEXAS.
000012
000013*REMARKS. TRANSACTION EX33 - CLAIM AUDIT.
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000023* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000024* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000025* 080322  CR2021100800003  TANA  Add B and H claim types
000026******************************************************************
000027
000028     EJECT
000029 ENVIRONMENT DIVISION.
000030
000031 DATA DIVISION.
000032
000033 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000034 77  FILLER  PIC X(32)  VALUE '********************************'.
000035 77  FILLER  PIC X(32)  VALUE '*   EL160  WORKING STORAGE     *'.
000036 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.018 **********'.
000037
000038 01  LCP-TIME-OF-DAY-XX.
000039     05  LCP-TIME-OF-DAY-68        PIC 9(6).
000040     05  FILLER                    PIC 99.
000041 01  LCP-CICS-TIME                 PIC 9(15).
000042
000043*    COPY ELCSCTM.
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
000044
000045*    COPY ELCSCRTY.
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
000046
000047 01  WS-DATE-AREA.
000048     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000049     05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
000050
000051 01  LITERALS-NUMBERS.
000052     12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.
000053     12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
000054     12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
000055     12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
000056     12  XCTL-EL1602             PIC X(8)    VALUE 'EL1602'.
000057     12  THIS-PGM                PIC X(8)    VALUE 'EL160'.
000058     12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
000059     12  REM-TERM-PGM            PIC X(8)    VALUE 'ELRTRM '.
000060     12  THIS-TRAN               PIC X(4)    VALUE 'EX33'.
000061     12  LIT-MAP                 PIC X(4)    VALUE '160A'.
000062     12  LIT-MAP-2               PIC X(4)    VALUE '160B'.
000063     12  MAX-TS-PAGES            PIC 9999    VALUE 250.
000064     12  ALL-NINES               PIC 9(7)V99  VALUE 9999999.99.
000065
000066 01  EDIT-WORK-AREA.
000067     12  WS-SEX-SELECTION        PIC X VALUE SPACE.
000068         88  MALE-SELECTION       VALUE 'M'.
000069         88  FEMALE-SELECTION     VALUE 'F'.
000070     12  CALL-PGM                PIC X(8).
000071     12  TRANS-ID                PIC X(4).
000072     12  CHECK-PFKEYS            PIC 99.
000073     12  TEST-RESP               PIC X.
000074     12  DAYS-PAID               PIC ZZZZ9.
000075     12  PMTS-MADE               PIC ZZZZZ9.
000076     12  EDIT-DOLLARS-8          PIC ZZZZZ.99.
000077     12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.
000078     12  EDIT-APR                PIC ZZ9.9999.
000079     12  COUNT-2                 PIC 99.
000080     12  HOLD-BENEFIT            PIC XX.
000081
000082     12  WS-RESPONSE             PIC S9(8) COMP.
000083         88  WS-RESP-NORMAL                VALUE +00.
000084         88  WS-RESP-NOTFND                VALUE +13.
000085
000086     12  TEST-DATE.
000087         16  FILLER              PIC XX.
000088         16  BIF-DATE            PIC X(6).
000089
000090     12  TEST-AMT.
000091         16  FILLER              PIC X.
000092         16  BIF-AMT             PIC X(9).
000093         16  AMT-BIF REDEFINES BIF-AMT PIC 9(7)V99.
000094
000095     12  W-FILE-ID               PIC X(8) VALUE 'ELMSTR'.
000096     12  W-VALID-FILE-IND        PIC X(8).
000097         88  W-VALID-FILE                  VALUE ' ' 'M' 'R'.
000098         88  W-RETRIEVE                    VALUE 'R'.
000099         88  W-MASTER                      VALUE ' ' 'M'.
000100
000101     12  WORK-DATE-MDY.
000102         16  MONTH-WORK          PIC XX.
000103         16  FILLER              PIC X(4).
000104         16  YEAR-WORK           PIC XX.
000105
000106     12  WORK-DATE-MY.
000107         16  WORK-MONTH          PIC XX.
000108         16  FILLER              PIC X       VALUE '/'.
000109         16  WORK-YEAR           PIC XX.
000110
000111     12  HOLD-TERM-REM.
000112         16  HOLD-ORIG-TERM      PIC ZZ9.
000113         16  FILLER              PIC X       VALUE '/'.
000114         16  HOLD-REM            PIC ZZ9.
000115
000116     12  CURRENT-DATE-BIN        PIC X(2).
000117
000118     12  WS-FORM-SAVE            PIC X(12).
000119
000120     12  WS-AGE                  PIC 9(04).
000121     12  WS-AGE-R REDEFINES WS-AGE.
000122         16  WS-AGE-1-2          PIC 9(02).
000123         16  WS-AGE-3-4          PIC 9(02).
000124
000125     12  WS-PAID-TO-HDG          PIC X(29)   VALUE
000126         'CAUSE CD  EST. END   PAID  TO'.
000127
000128 01  CNTL-WORK-AREA.
000129     12  CARRIER-CNTL            PIC X.
000130     12  INC-DATE-LOW-CNTL       PIC XX.
000131     12  INC-DATE-HIGH-CNTL      PIC XX.
000132     12  GROUP-CNTL              PIC X(06).
000133     12  LST-PMT-LOW-CNTL        PIC XX.
000134     12  LST-PMT-HIGH-CNTL       PIC XX.
000135     12  STATE-CNTL              PIC XX.
000136     12  MO-OPEN-LOW-CNTL        PIC S9(4)       COMP.
000137     12  MO-OPEN-HIGH-CNTL       PIC S9(4)       COMP.
000138     12  ACCOUNT-CNTL            PIC X(10).
000139     12  AMT-PAID-LOW-CNTL       PIC S9(7)V99    COMP-3.
000140     12  AMT-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.
000141     12  TYPE-CNTL               PIC X.
000142     12  CAUSE-CD-LOW-CNTL       PIC X(6).
000143     12  CAUSE-CD-HIGH-CNTL      PIC X(6).
000144     12  DEN-CNTL                PIC X.
000145     12  REP-DATE-LOW-CNTL       PIC XX.
000146     12  REP-DATE-HIGH-CNTL      PIC XX.
000147     12  PROC-CNTL               PIC X(4).
000148     12  LST-PAID-LOW-CNTL       PIC S9(7)V99   COMP-3.
000149     12  LST-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.
000150     12  PREM-CNTL               PIC X.
000151     12  MNT-DATE-LOW-CNTL       PIC XX.
000152     12  MNT-DATE-HIGH-CNTL      PIC XX.
000153     12  REQ-CNTL                PIC X.
000154     12  EST-DATE-LOW-CNTL       PIC XX.
000155     12  EST-DATE-HIGH-CNTL      PIC XX.
000156     12  SUPR-CNTL               PIC X.
000157     12  FOL-DATE-LOW-CNTL       PIC XX.
000158     12  FOL-DATE-HIGH-CNTL      PIC XX.
000159     12  CERT-CNTL               PIC X.
000160     12  DAYS-LOW-CNTL           PIC S9(4)       COMP.
000161     12  DAYS-HIGH-CNTL          PIC S9(4)       COMP.
000162     12  PRI-CNTL                PIC X.
000163     12  AUTO-CNTL               PIC X.
000164     12  OPCL-CNTL               PIC X.
000165
000166 01  TIME-IN.
000167     12  UN-HOURS                PIC XX.
000168     12  UN-MINUTES              PIC XX.
000169     12  FILLER                  PIC X(4).
000170
000171 01  TIME-OUT.
000172     12  FOR-HOURS               PIC XX.
000173     12  FILLER                  PIC X       VALUE '.'.
000174     12  FOR-MINUTES             PIC XX.
000175
000176 01  ERROR-NUMBERS.
000177     12  ER-0008                 PIC X(4)    VALUE '0008'.
000178     12  ER-0029                 PIC X(4)    VALUE '0029'.
000179     12  ER-0042                 PIC X(4)    VALUE '0042'.
000180     12  ER-0046                 PIC X(4)    VALUE '0046'.
000181     12  ER-0070                 PIC X(4)    VALUE '0070'.
000182     12  ER-0142                 PIC X(4)    VALUE '0142'.
000183     12  ER-0143                 PIC X(4)    VALUE '0143'.
000184     12  ER-0154                 PIC X(4)    VALUE '0154'.
000185     12  ER-0169                 PIC X(4)    VALUE '0169'.
000186     12  ER-0172                 PIC X(4)    VALUE '0172'.
000187     12  ER-0192                 PIC X(4)    VALUE '0192'.
000188     12  ER-0199                 PIC X(4)    VALUE '0199'.
000189     12  ER-0205                 PIC X(4)    VALUE '0205'.
000190     12  ER-0206                 PIC X(4)    VALUE '0206'.
000191     12  ER-0219                 PIC X(4)    VALUE '0219'.
000192     12  ER-0227                 PIC X(4)    VALUE '0227'.
000193     12  ER-0273                 PIC X(4)    VALUE '0273'.
000194     12  ER-0274                 PIC X(4)    VALUE '0274'.
000195     12  ER-0282                 PIC X(4)    VALUE '0282'.
000196     12  ER-0283                 PIC X(4)    VALUE '0283'.
000197     12  ER-0304                 PIC X(4)    VALUE '0304'.
000198     12  ER-0306                 PIC X(4)    VALUE '0306'.
000199     12  ER-0307                 PIC X(4)    VALUE '0307'.
000200     12  ER-0308                 PIC X(4)    VALUE '0308'.
000201     12  ER-0334                 PIC X(4)    VALUE '0334'.
000202     12  ER-0335                 PIC X(4)    VALUE '0335'.
000203     12  ER-0419                 PIC X(4)    VALUE '0419'.
000204     12  ER-0767                 PIC X(4)    VALUE '0767'.
000205     12  ER-0970                 PIC X(4)    VALUE '0970'.
000206     12  ER-2381                 PIC X(4)    VALUE '2381'.
000207     12  ER-2848                 PIC X(4)    VALUE '2848'.
000208     12  ER-9483                 PIC X(4)    VALUE '9483'.
000209     12  ER-9811                 PIC X(4)    VALUE '9811'.
000210
000211 01  HOLD-KEY.
000212     12  HOLD-TERM               PIC X(4).
000213     12  KEY-QUAL                PIC X(4).
000214
000215 01  ERROR-SWITCHES.
000216     12  ERROR-SWITCH            PIC X.
000217         88  SCREEN-ERROR                    VALUE 'X'.
000218
000219     12  BUILD-SWITCH            PIC X.
000220         88  NO-RECORDS                      VALUE 'X'.
000221         88  BUILD-COMPLETE                  VALUE 'Y'.
000222         88  SCREEN-HAS-ERRORS               VALUE 'X'.
000223
000224     12  PROC-SWITCH             PIC X.
000225         88  PROC-SELECTED                   VALUE 'X'.
000226
000227 01  MSTR-KEY.
000228     12  MSTR-COMPANY-CODE       PIC X.
000229     12  MSTR-CARRIER            PIC X.
000230     12  REST-OF-KEY             PIC X(18).
000231
000232 01  MSTR-KEY-4.
000233     12  MSTR-COMPANY-CODE-4     PIC X.
000234     12  MSTR-USER-ID-4          PIC X(4).
000235
000236 01  CERT-KEY.
000237     12  CERT-COMPANY-CODE       PIC X.
000238     12  CERT-CARRIER            PIC X.
000239     12  CERT-GROUP              PIC X(6).
000240     12  CERT-STATE              PIC XX.
000241     12  CERT-ACCOUNT            PIC X(10).
000242     12  CERT-DATE               PIC XX.
000243     12  CERT-CERT               PIC X(11).
000244
000245 01  TRLR-KEY.
000246     12  TRLR-MAIN-KEY           PIC X(20).
000247     12  TRLR-SEQ-NO             PIC 9(4)    COMP.
000248
000249 01  CNTL-KEY.
000250     12  COMPANY-ID              PIC X(3).
000251     12  RECORD-TYPE             PIC X.
000252     12  ACCESS-CD-GENL          PIC X(4).
000253     12  SEQUENCE-NO             PIC 9(4)    COMP.
000254
000255 01  BENEFIT-KEY.
000256     12  BEN-CO-ID               PIC X(3).
000257     12  BEN-REC-TYPE            PIC X.
000258     12  FILLER                  PIC XX.
000259     12  BEN-ACC-CD              PIC XX.
000260     12  BEN-SEQ-NO              PIC S9(4)   COMP.
000261
000262 01  EMPLCY-KEY.
000263     12  EMPLCY-COMPANY-CD       PIC X(01).
000264     12  EMPLCY-CARRIER          PIC X(01).
000265     12  EMPLCY-GROUPING         PIC X(06).
000266     12  EMPLCY-STATE            PIC X(02).
000267     12  EMPLCY-PRODUCER         PIC X(10).
000268     12  EMPLCY-EFF-DT           PIC X(02).
000269     12  EMPLCY-REFERENCE-NO     PIC X(20).
000270
000271 01  EMPLAN-KEY.
000272     12  EMPLAN-COMPANY-CD       PIC X(01).
000273     12  EMPLAN-CARRIER          PIC X(01).
000274     12  EMPLAN-GROUPING         PIC X(06).
000275     12  EMPLAN-STATE            PIC X(02).
000276     12  EMPLAN-PRODUCER         PIC X(10).
000277     12  EMPLAN-PLAN-CODE        PIC X(02).
000278     12  EMPLAN-REV-NO           PIC 9(03).
000279
000280 01  COMP-LENGTHS.
000281     12  COUNT-1                 PIC S9(4)   COMP.
000282     12  EL160A-LENGTH           PIC S9(4)   COMP VALUE +515.
000283     12  EL160B-LENGTH           PIC S9(4)   COMP VALUE +881.
000284     12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
000285     12  MO-DAY-LENGTH           PIC S9(4)   COMP VALUE +3.
000286     12  AMT-LENGTH              PIC S9(4)   COMP VALUE +10.
000287
000288     EJECT
000289*    COPY ELCINTF.
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
000290
000291     12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
000292         16  PI-TS-COUNT         PIC S9(4)   COMP.
000293         16  PI-TS-COUNT-1       PIC S9(4)   COMP.
000294         16  PI-EL160-KEY        PIC X(8).
000295         16  PI-EL1602-KEY       PIC X(8).
000296         16  PI-PRINT-OPTION     PIC X.
000297         16  PI-FORMAT-OPTION    PIC X.
000298         16  PI-PRINT-ID         PIC X(4).
000299         16  PI-ALT-PRINT-ID     PIC X(4).
000300         16  PI-FILE-ID-IND      PIC X.
000301             88  PI-RETRIEVAL-FILE           VALUE 'R'.
000302             88  PI-MASTER-FILE              VALUE 'M'.
000303         16  FILLER              PIC X(609).
000304
000305     EJECT
000306*    COPY ELCLOGOF SUPPRESS.
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
000307
000308*    COPY ELCCALC.
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
000309     EJECT
000310*    COPY EL160S.
      *>>((file: EL160S))
000001 01  EL160AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  DATEL PIC S9(0004) COMP.
000005     05  DATEF PIC  X(0001).
000006     05  FILLER REDEFINES DATEF.
000007         10  DATEA PIC  X(0001).
000008     05  DATEI PIC  X(0008).
000009*    -------------------------------
000010     05  TIMEL PIC S9(0004) COMP.
000011     05  TIMEF PIC  X(0001).
000012     05  FILLER REDEFINES TIMEF.
000013         10  TIMEA PIC  X(0001).
000014     05  TIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  CFILEIDL PIC S9(0004) COMP.
000017     05  CFILEIDF PIC  X(0001).
000018     05  FILLER REDEFINES CFILEIDF.
000019         10  CFILEIDA PIC  X(0001).
000020     05  CFILEIDI PIC  X(0001).
000021*    -------------------------------
000022     05  CARRSL PIC S9(0004) COMP.
000023     05  CARRSF PIC  X(0001).
000024     05  FILLER REDEFINES CARRSF.
000025         10  CARRSA PIC  X(0001).
000026     05  CARRSI PIC  X(0001).
000027*    -------------------------------
000028     05  INCLSL PIC S9(0004) COMP.
000029     05  INCLSF PIC  X(0001).
000030     05  FILLER REDEFINES INCLSF.
000031         10  INCLSA PIC  X(0001).
000032     05  INCLSI PIC  X(0008).
000033*    -------------------------------
000034     05  INCHSL PIC S9(0004) COMP.
000035     05  INCHSF PIC  X(0001).
000036     05  FILLER REDEFINES INCHSF.
000037         10  INCHSA PIC  X(0001).
000038     05  INCHSI PIC  X(0008).
000039*    -------------------------------
000040     05  GRPSL PIC S9(0004) COMP.
000041     05  GRPSF PIC  X(0001).
000042     05  FILLER REDEFINES GRPSF.
000043         10  GRPSA PIC  X(0001).
000044     05  GRPSI PIC  X(0006).
000045*    -------------------------------
000046     05  PMTDLSL PIC S9(0004) COMP.
000047     05  PMTDLSF PIC  X(0001).
000048     05  FILLER REDEFINES PMTDLSF.
000049         10  PMTDLSA PIC  X(0001).
000050     05  PMTDLSI PIC  X(0008).
000051*    -------------------------------
000052     05  PMTDHSL PIC S9(0004) COMP.
000053     05  PMTDHSF PIC  X(0001).
000054     05  FILLER REDEFINES PMTDHSF.
000055         10  PMTDHSA PIC  X(0001).
000056     05  PMTDHSI PIC  X(0008).
000057*    -------------------------------
000058     05  STATESL PIC S9(0004) COMP.
000059     05  STATESF PIC  X(0001).
000060     05  FILLER REDEFINES STATESF.
000061         10  STATESA PIC  X(0001).
000062     05  STATESI PIC  X(0002).
000063*    -------------------------------
000064     05  OPENLSL PIC S9(0004) COMP.
000065     05  OPENLSF PIC  X(0001).
000066     05  FILLER REDEFINES OPENLSF.
000067         10  OPENLSA PIC  X(0001).
000068     05  OPENLSI PIC  X(0003).
000069*    -------------------------------
000070     05  OPENHSL PIC S9(0004) COMP.
000071     05  OPENHSF PIC  X(0001).
000072     05  FILLER REDEFINES OPENHSF.
000073         10  OPENHSA PIC  X(0001).
000074     05  OPENHSI PIC  X(0003).
000075*    -------------------------------
000076     05  ACCTSL PIC S9(0004) COMP.
000077     05  ACCTSF PIC  X(0001).
000078     05  FILLER REDEFINES ACCTSF.
000079         10  ACCTSA PIC  X(0001).
000080     05  ACCTSI PIC  X(0010).
000081*    -------------------------------
000082     05  AMTLSL PIC S9(0004) COMP.
000083     05  AMTLSF PIC  X(0001).
000084     05  FILLER REDEFINES AMTLSF.
000085         10  AMTLSA PIC  X(0001).
000086     05  AMTLSI PIC  X(0010).
000087*    -------------------------------
000088     05  AMTHSL PIC S9(0004) COMP.
000089     05  AMTHSF PIC  X(0001).
000090     05  FILLER REDEFINES AMTHSF.
000091         10  AMTHSA PIC  X(0001).
000092     05  AMTHSI PIC  X(0010).
000093*    -------------------------------
000094     05  TYPESL PIC S9(0004) COMP.
000095     05  TYPESF PIC  X(0001).
000096     05  FILLER REDEFINES TYPESF.
000097         10  TYPESA PIC  X(0001).
000098     05  TYPESI PIC  X(0001).
000099*    -------------------------------
000100     05  CAUSELSL PIC S9(0004) COMP.
000101     05  CAUSELSF PIC  X(0001).
000102     05  FILLER REDEFINES CAUSELSF.
000103         10  CAUSELSA PIC  X(0001).
000104     05  CAUSELSI PIC  X(0006).
000105*    -------------------------------
000106     05  CAUSEHSL PIC S9(0004) COMP.
000107     05  CAUSEHSF PIC  X(0001).
000108     05  FILLER REDEFINES CAUSEHSF.
000109         10  CAUSEHSA PIC  X(0001).
000110     05  CAUSEHSI PIC  X(0006).
000111*    -------------------------------
000112     05  DENSL PIC S9(0004) COMP.
000113     05  DENSF PIC  X(0001).
000114     05  FILLER REDEFINES DENSF.
000115         10  DENSA PIC  X(0001).
000116     05  DENSI PIC  X(0001).
000117*    -------------------------------
000118     05  REPLSL PIC S9(0004) COMP.
000119     05  REPLSF PIC  X(0001).
000120     05  FILLER REDEFINES REPLSF.
000121         10  REPLSA PIC  X(0001).
000122     05  REPLSI PIC  X(0008).
000123*    -------------------------------
000124     05  REPHSL PIC S9(0004) COMP.
000125     05  REPHSF PIC  X(0001).
000126     05  FILLER REDEFINES REPHSF.
000127         10  REPHSA PIC  X(0001).
000128     05  REPHSI PIC  X(0008).
000129*    -------------------------------
000130     05  PROCSL PIC S9(0004) COMP.
000131     05  PROCSF PIC  X(0001).
000132     05  FILLER REDEFINES PROCSF.
000133         10  PROCSA PIC  X(0001).
000134     05  PROCSI PIC  X(0004).
000135*    -------------------------------
000136     05  PMTLSL PIC S9(0004) COMP.
000137     05  PMTLSF PIC  X(0001).
000138     05  FILLER REDEFINES PMTLSF.
000139         10  PMTLSA PIC  X(0001).
000140     05  PMTLSI PIC  X(0010).
000141*    -------------------------------
000142     05  PMTHSL PIC S9(0004) COMP.
000143     05  PMTHSF PIC  X(0001).
000144     05  FILLER REDEFINES PMTHSF.
000145         10  PMTHSA PIC  X(0001).
000146     05  PMTHSI PIC  X(0010).
000147*    -------------------------------
000148     05  PREMSL PIC S9(0004) COMP.
000149     05  PREMSF PIC  X(0001).
000150     05  FILLER REDEFINES PREMSF.
000151         10  PREMSA PIC  X(0001).
000152     05  PREMSI PIC  X(0001).
000153*    -------------------------------
000154     05  MNTLSL PIC S9(0004) COMP.
000155     05  MNTLSF PIC  X(0001).
000156     05  FILLER REDEFINES MNTLSF.
000157         10  MNTLSA PIC  X(0001).
000158     05  MNTLSI PIC  X(0008).
000159*    -------------------------------
000160     05  MNTHSL PIC S9(0004) COMP.
000161     05  MNTHSF PIC  X(0001).
000162     05  FILLER REDEFINES MNTHSF.
000163         10  MNTHSA PIC  X(0001).
000164     05  MNTHSI PIC  X(0008).
000165*    -------------------------------
000166     05  REQSL PIC S9(0004) COMP.
000167     05  REQSF PIC  X(0001).
000168     05  FILLER REDEFINES REQSF.
000169         10  REQSA PIC  X(0001).
000170     05  REQSI PIC  X(0001).
000171*    -------------------------------
000172     05  ESTLSL PIC S9(0004) COMP.
000173     05  ESTLSF PIC  X(0001).
000174     05  FILLER REDEFINES ESTLSF.
000175         10  ESTLSA PIC  X(0001).
000176     05  ESTLSI PIC  X(0008).
000177*    -------------------------------
000178     05  ESTHSL PIC S9(0004) COMP.
000179     05  ESTHSF PIC  X(0001).
000180     05  FILLER REDEFINES ESTHSF.
000181         10  ESTHSA PIC  X(0001).
000182     05  ESTHSI PIC  X(0008).
000183*    -------------------------------
000184     05  SUPRSL PIC S9(0004) COMP.
000185     05  SUPRSF PIC  X(0001).
000186     05  FILLER REDEFINES SUPRSF.
000187         10  SUPRSA PIC  X(0001).
000188     05  SUPRSI PIC  X(0001).
000189*    -------------------------------
000190     05  FOLLSL PIC S9(0004) COMP.
000191     05  FOLLSF PIC  X(0001).
000192     05  FILLER REDEFINES FOLLSF.
000193         10  FOLLSA PIC  X(0001).
000194     05  FOLLSI PIC  X(0008).
000195*    -------------------------------
000196     05  FOLHSL PIC S9(0004) COMP.
000197     05  FOLHSF PIC  X(0001).
000198     05  FILLER REDEFINES FOLHSF.
000199         10  FOLHSA PIC  X(0001).
000200     05  FOLHSI PIC  X(0008).
000201*    -------------------------------
000202     05  CERTSL PIC S9(0004) COMP.
000203     05  CERTSF PIC  X(0001).
000204     05  FILLER REDEFINES CERTSF.
000205         10  CERTSA PIC  X(0001).
000206     05  CERTSI PIC  X(0001).
000207*    -------------------------------
000208     05  DAYSLSL PIC S9(0004) COMP.
000209     05  DAYSLSF PIC  X(0001).
000210     05  FILLER REDEFINES DAYSLSF.
000211         10  DAYSLSA PIC  X(0001).
000212     05  DAYSLSI PIC  X(0003).
000213*    -------------------------------
000214     05  DAYSHSL PIC S9(0004) COMP.
000215     05  DAYSHSF PIC  X(0001).
000216     05  FILLER REDEFINES DAYSHSF.
000217         10  DAYSHSA PIC  X(0001).
000218     05  DAYSHSI PIC  X(0003).
000219*    -------------------------------
000220     05  PRISL PIC S9(0004) COMP.
000221     05  PRISF PIC  X(0001).
000222     05  FILLER REDEFINES PRISF.
000223         10  PRISA PIC  X(0001).
000224     05  PRISI PIC  X(0001).
000225*    -------------------------------
000226     05  AUTOSL PIC S9(0004) COMP.
000227     05  AUTOSF PIC  X(0001).
000228     05  FILLER REDEFINES AUTOSF.
000229         10  AUTOSA PIC  X(0001).
000230     05  AUTOSI PIC  X(0001).
000231*    -------------------------------
000232     05  PRTOPTL PIC S9(0004) COMP.
000233     05  PRTOPTF PIC  X(0001).
000234     05  FILLER REDEFINES PRTOPTF.
000235         10  PRTOPTA PIC  X(0001).
000236     05  PRTOPTI PIC  X(0001).
000237*    -------------------------------
000238     05  OPCLSL PIC S9(0004) COMP.
000239     05  OPCLSF PIC  X(0001).
000240     05  FILLER REDEFINES OPCLSF.
000241         10  OPCLSA PIC  X(0001).
000242     05  OPCLSI PIC  X(0001).
000243*    -------------------------------
000244     05  FMTOPTL PIC S9(0004) COMP.
000245     05  FMTOPTF PIC  X(0001).
000246     05  FILLER REDEFINES FMTOPTF.
000247         10  FMTOPTA PIC  X(0001).
000248     05  FMTOPTI PIC  X(0001).
000249*    -------------------------------
000250     05  ASEXL PIC S9(0004) COMP.
000251     05  ASEXF PIC  X(0001).
000252     05  FILLER REDEFINES ASEXF.
000253         10  ASEXA PIC  X(0001).
000254     05  ASEXI PIC  X(0001).
000255*    -------------------------------
000256     05  ALTPRTL PIC S9(0004) COMP.
000257     05  ALTPRTF PIC  X(0001).
000258     05  FILLER REDEFINES ALTPRTF.
000259         10  ALTPRTA PIC  X(0001).
000260     05  ALTPRTI PIC  X(0004).
000261*    -------------------------------
000262     05  MSG1L PIC S9(0004) COMP.
000263     05  MSG1F PIC  X(0001).
000264     05  FILLER REDEFINES MSG1F.
000265         10  MSG1A PIC  X(0001).
000266     05  MSG1I PIC  X(0075).
000267*    -------------------------------
000268     05  MSG2L PIC S9(0004) COMP.
000269     05  MSG2F PIC  X(0001).
000270     05  FILLER REDEFINES MSG2F.
000271         10  MSG2A PIC  X(0001).
000272     05  MSG2I PIC  X(0075).
000273*    -------------------------------
000274     05  PFKEYL PIC S9(0004) COMP.
000275     05  PFKEYF PIC  X(0001).
000276     05  FILLER REDEFINES PFKEYF.
000277         10  PFKEYA PIC  X(0001).
000278     05  PFKEYI PIC  X(0002).
000279 01  EL160AO REDEFINES EL160AI.
000280     05  FILLER            PIC  X(0012).
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  DATEO PIC  X(0008).
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  TIMEO PIC  X(0005).
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  CFILEIDO PIC  X(0001).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  CARRSO PIC  X(0001).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  INCLSO PIC  X(0008).
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  INCHSO PIC  X(0008).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  GRPSO PIC  X(0006).
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  PMTDLSO PIC  X(0008).
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  PMTDHSO PIC  X(0008).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  STATESO PIC  X(0002).
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  OPENLSO PIC  X(0003).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  OPENHSO PIC  X(0003).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  ACCTSO PIC  X(0010).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  AMTLSO PIC  Z(7).99.
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  AMTHSO PIC  Z(7).99.
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  TYPESO PIC  X(0001).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  CAUSELSO PIC  X(0006).
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  CAUSEHSO PIC  X(0006).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  DENSO PIC  X(0001).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  REPLSO PIC  X(0008).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  REPHSO PIC  X(0008).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  PROCSO PIC  X(0004).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  PMTLSO PIC  Z(7).99.
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  PMTHSO PIC  Z(7).99.
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  PREMSO PIC  X(0001).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  MNTLSO PIC  X(0008).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  MNTHSO PIC  X(0008).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  REQSO PIC  X(0001).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  ESTLSO PIC  X(0008).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  ESTHSO PIC  X(0008).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  SUPRSO PIC  X(0001).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  FOLLSO PIC  X(0008).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  FOLHSO PIC  X(0008).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  CERTSO PIC  X(0001).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  DAYSLSO PIC  X(0003).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  DAYSHSO PIC  X(0003).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  PRISO PIC  X(0001).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  AUTOSO PIC  X(0001).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  PRTOPTO PIC  X(0001).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  OPCLSO PIC  X(0001).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  FMTOPTO PIC  X(0001).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  ASEXO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  ALTPRTO PIC  X(0004).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  MSG1O PIC  X(0075).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  MSG2O PIC  X(0075).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  PFKEYO PIC  X(0002).
000419*    -------------------------------
000420 01  EL160BI.
000421     05  FILLER            PIC  X(0012).
000422*    -------------------------------
000423     05  DATEBL PIC S9(0004) COMP.
000424     05  DATEBF PIC  X(0001).
000425     05  FILLER REDEFINES DATEBF.
000426         10  DATEBA PIC  X(0001).
000427     05  DATEBI PIC  X(0008).
000428*    -------------------------------
000429     05  TIMEBL PIC S9(0004) COMP.
000430     05  TIMEBF PIC  X(0001).
000431     05  FILLER REDEFINES TIMEBF.
000432         10  TIMEBA PIC  X(0001).
000433     05  TIMEBI PIC  X(0005).
000434*    -------------------------------
000435     05  TITLEL PIC S9(0004) COMP.
000436     05  TITLEF PIC  X(0001).
000437     05  FILLER REDEFINES TITLEF.
000438         10  TITLEA PIC  X(0001).
000439     05  TITLEI PIC  X(0028).
000440*    -------------------------------
000441     05  PIKEYL PIC S9(0004) COMP.
000442     05  PIKEYF PIC  X(0001).
000443     05  FILLER REDEFINES PIKEYF.
000444         10  PIKEYA PIC  X(0001).
000445     05  PIKEYI PIC  X(0039).
000446*    -------------------------------
000447     05  SCNERRL PIC S9(0004) COMP.
000448     05  SCNERRF PIC  X(0001).
000449     05  FILLER REDEFINES SCNERRF.
000450         10  SCNERRA PIC  X(0001).
000451     05  SCNERRI PIC  X(0004).
000452*    -------------------------------
000453     05  USERSAVL PIC S9(0004) COMP.
000454     05  USERSAVF PIC  X(0001).
000455     05  FILLER REDEFINES USERSAVF.
000456         10  USERSAVA PIC  X(0001).
000457     05  USERSAVI PIC  X(0004).
000458*    -------------------------------
000459     05  TIMESAVL PIC S9(0004) COMP.
000460     05  TIMESAVF PIC  X(0001).
000461     05  FILLER REDEFINES TIMESAVF.
000462         10  TIMESAVA PIC  X(0001).
000463     05  TIMESAVI PIC  9(07).
000464*    -------------------------------
000465     05  NOSCRNL PIC S9(0004) COMP.
000466     05  NOSCRNF PIC  X(0001).
000467     05  FILLER REDEFINES NOSCRNF.
000468         10  NOSCRNA PIC  X(0001).
000469     05  NOSCRNI PIC  9999.
000470*    -------------------------------
000471     05  TOTSCRNL PIC S9(0004) COMP.
000472     05  TOTSCRNF PIC  X(0001).
000473     05  FILLER REDEFINES TOTSCRNF.
000474         10  TOTSCRNA PIC  X(0001).
000475     05  TOTSCRNI PIC  X(0004).
000476*    -------------------------------
000477     05  CLAIML PIC S9(0004) COMP.
000478     05  CLAIMF PIC  X(0001).
000479     05  FILLER REDEFINES CLAIMF.
000480         10  CLAIMA PIC  X(0001).
000481     05  CLAIMI PIC  X(0007).
000482*    -------------------------------
000483     05  TYPEL PIC S9(0004) COMP.
000484     05  TYPEF PIC  X(0001).
000485     05  FILLER REDEFINES TYPEF.
000486         10  TYPEA PIC  X(0001).
000487     05  TYPEI PIC  X(0001).
000488*    -------------------------------
000489     05  CERTL PIC S9(0004) COMP.
000490     05  CERTF PIC  X(0001).
000491     05  FILLER REDEFINES CERTF.
000492         10  CERTA PIC  X(0001).
000493     05  CERTI PIC  X(0010).
000494*    -------------------------------
000495     05  CERTSXL PIC S9(0004) COMP.
000496     05  CERTSXF PIC  X(0001).
000497     05  FILLER REDEFINES CERTSXF.
000498         10  CERTSXA PIC  X(0001).
000499     05  CERTSXI PIC  X(0001).
000500*    -------------------------------
000501     05  CARRL PIC S9(0004) COMP.
000502     05  CARRF PIC  X(0001).
000503     05  FILLER REDEFINES CARRF.
000504         10  CARRA PIC  X(0001).
000505     05  CARRI PIC  X(0001).
000506*    -------------------------------
000507     05  STATUSL PIC S9(0004) COMP.
000508     05  STATUSF PIC  X(0001).
000509     05  FILLER REDEFINES STATUSF.
000510         10  STATUSA PIC  X(0001).
000511     05  STATUSI PIC  X(0001).
000512*    -------------------------------
000513     05  PROCL PIC S9(0004) COMP.
000514     05  PROCF PIC  X(0001).
000515     05  FILLER REDEFINES PROCF.
000516         10  PROCA PIC  X(0001).
000517     05  PROCI PIC  X(0004).
000518*    -------------------------------
000519     05  FILEL PIC S9(0004) COMP.
000520     05  FILEF PIC  X(0001).
000521     05  FILLER REDEFINES FILEF.
000522         10  FILEA PIC  X(0001).
000523     05  FILEI PIC  X(0004).
000524*    -------------------------------
000525     05  CREDCDL PIC S9(0004) COMP.
000526     05  CREDCDF PIC  X(0001).
000527     05  FILLER REDEFINES CREDCDF.
000528         10  CREDCDA PIC  X(0001).
000529     05  CREDCDI PIC  X(0016).
000530*    -------------------------------
000531     05  MLNAMEL PIC S9(0004) COMP.
000532     05  MLNAMEF PIC  X(0001).
000533     05  FILLER REDEFINES MLNAMEF.
000534         10  MLNAMEA PIC  X(0001).
000535     05  MLNAMEI PIC  X(0015).
000536*    -------------------------------
000537     05  MFNAMEL PIC S9(0004) COMP.
000538     05  MFNAMEF PIC  X(0001).
000539     05  FILLER REDEFINES MFNAMEF.
000540         10  MFNAMEA PIC  X(0001).
000541     05  MFNAMEI PIC  X(0015).
000542*    -------------------------------
000543     05  MMINITL PIC S9(0004) COMP.
000544     05  MMINITF PIC  X(0001).
000545     05  FILLER REDEFINES MMINITF.
000546         10  MMINITA PIC  X(0001).
000547     05  MMINITI PIC  X(0001).
000548*    -------------------------------
000549     05  SEXL PIC S9(0004) COMP.
000550     05  SEXF PIC  X(0001).
000551     05  FILLER REDEFINES SEXF.
000552         10  SEXA PIC  X(0001).
000553     05  SEXI PIC  X(0001).
000554*    -------------------------------
000555     05  BIRTHL PIC S9(0004) COMP.
000556     05  BIRTHF PIC  X(0001).
000557     05  FILLER REDEFINES BIRTHF.
000558         10  BIRTHA PIC  X(0001).
000559     05  BIRTHI PIC  X(0008).
000560*    -------------------------------
000561     05  SOCIALL PIC S9(0004) COMP.
000562     05  SOCIALF PIC  X(0001).
000563     05  FILLER REDEFINES SOCIALF.
000564         10  SOCIALA PIC  X(0001).
000565     05  SOCIALI PIC  X(0011).
000566*    -------------------------------
000567     05  OCCL PIC S9(0004) COMP.
000568     05  OCCF PIC  X(0001).
000569     05  FILLER REDEFINES OCCF.
000570         10  OCCA PIC  X(0001).
000571     05  OCCI PIC  X(0006).
000572*    -------------------------------
000573     05  CBENEL PIC S9(0004) COMP.
000574     05  CBENEF PIC  X(0001).
000575     05  FILLER REDEFINES CBENEF.
000576         10  CBENEA PIC  X(0001).
000577     05  CBENEI PIC  X(0010).
000578*    -------------------------------
000579     05  BHEADL PIC S9(0004) COMP.
000580     05  BHEADF PIC  X(0001).
000581     05  FILLER REDEFINES BHEADF.
000582         10  BHEADA PIC  X(0001).
000583     05  BHEADI PIC  X(0029).
000584*    -------------------------------
000585     05  CAUSEL PIC S9(0004) COMP.
000586     05  CAUSEF PIC  X(0001).
000587     05  FILLER REDEFINES CAUSEF.
000588         10  CAUSEA PIC  X(0001).
000589     05  CAUSEI PIC  X(0026).
000590*    -------------------------------
000591     05  CCAUSCDL PIC S9(0004) COMP.
000592     05  CCAUSCDF PIC  X(0001).
000593     05  FILLER REDEFINES CCAUSCDF.
000594         10  CCAUSCDA PIC  X(0001).
000595     05  CCAUSCDI PIC  X(0006).
000596*    -------------------------------
000597     05  ENDL PIC S9(0004) COMP.
000598     05  ENDF PIC  X(0001).
000599     05  FILLER REDEFINES ENDF.
000600         10  ENDA PIC  X(0001).
000601     05  ENDI PIC  X(0008).
000602*    -------------------------------
000603     05  PDTHRUL PIC S9(0004) COMP.
000604     05  PDTHRUF PIC  X(0001).
000605     05  FILLER REDEFINES PDTHRUF.
000606         10  PDTHRUA PIC  X(0001).
000607     05  PDTHRUI PIC  X(0008).
000608*    -------------------------------
000609     05  PDAMTL PIC S9(0004) COMP.
000610     05  PDAMTF PIC  X(0001).
000611     05  FILLER REDEFINES PDAMTF.
000612         10  PDAMTA PIC  X(0001).
000613     05  PDAMTI PIC  9(7)V99.
000614*    -------------------------------
000615     05  NODAYSL PIC S9(0004) COMP.
000616     05  NODAYSF PIC  X(0001).
000617     05  FILLER REDEFINES NODAYSF.
000618         10  NODAYSA PIC  X(0001).
000619     05  NODAYSI PIC  9(5).
000620*    -------------------------------
000621     05  NOPMTSL PIC S9(0004) COMP.
000622     05  NOPMTSF PIC  X(0001).
000623     05  FILLER REDEFINES NOPMTSF.
000624         10  NOPMTSA PIC  X(0001).
000625     05  NOPMTSI PIC  9(4).
000626*    -------------------------------
000627     05  INCL PIC S9(0004) COMP.
000628     05  INCF PIC  X(0001).
000629     05  FILLER REDEFINES INCF.
000630         10  INCA PIC  X(0001).
000631     05  INCI PIC  X(0008).
000632*    -------------------------------
000633     05  REPL PIC S9(0004) COMP.
000634     05  REPF PIC  X(0001).
000635     05  FILLER REDEFINES REPF.
000636         10  REPA PIC  X(0001).
000637     05  REPI PIC  X(0008).
000638*    -------------------------------
000639     05  ESTL PIC S9(0004) COMP.
000640     05  ESTF PIC  X(0001).
000641     05  FILLER REDEFINES ESTF.
000642         10  ESTA PIC  X(0001).
000643     05  ESTI PIC  X(0008).
000644*    -------------------------------
000645     05  MNTDTL PIC S9(0004) COMP.
000646     05  MNTDTF PIC  X(0001).
000647     05  FILLER REDEFINES MNTDTF.
000648         10  MNTDTA PIC  X(0001).
000649     05  MNTDTI PIC  X(0008).
000650*    -------------------------------
000651     05  MNTTYPEL PIC S9(0004) COMP.
000652     05  MNTTYPEF PIC  X(0001).
000653     05  FILLER REDEFINES MNTTYPEF.
000654         10  MNTTYPEA PIC  X(0001).
000655     05  MNTTYPEI PIC  X(0006).
000656*    -------------------------------
000657     05  PRICDL PIC S9(0004) COMP.
000658     05  PRICDF PIC  X(0001).
000659     05  FILLER REDEFINES PRICDF.
000660         10  PRICDA PIC  X(0001).
000661     05  PRICDI PIC  X(0001).
000662*    -------------------------------
000663     05  SUPVL PIC S9(0004) COMP.
000664     05  SUPVF PIC  X(0001).
000665     05  FILLER REDEFINES SUPVF.
000666         10  SUPVA PIC  X(0001).
000667     05  SUPVI PIC  X(0001).
000668*    -------------------------------
000669     05  LOANNOL PIC S9(0004) COMP.
000670     05  LOANNOF PIC  X(0001).
000671     05  FILLER REDEFINES LOANNOF.
000672         10  LOANNOA PIC  X(0001).
000673     05  LOANNOI PIC  X(0008).
000674*    -------------------------------
000675     05  LOANBALL PIC S9(0004) COMP.
000676     05  LOANBALF PIC  X(0001).
000677     05  FILLER REDEFINES LOANBALF.
000678         10  LOANBALA PIC  X(0001).
000679     05  LOANBALI PIC  9(10)V99.
000680*    -------------------------------
000681     05  CERTEFFL PIC S9(0004) COMP.
000682     05  CERTEFFF PIC  X(0001).
000683     05  FILLER REDEFINES CERTEFFF.
000684         10  CERTEFFA PIC  X(0001).
000685     05  CERTEFFI PIC  X(0008).
000686*    -------------------------------
000687     05  CERTACTL PIC S9(0004) COMP.
000688     05  CERTACTF PIC  X(0001).
000689     05  FILLER REDEFINES CERTACTF.
000690         10  CERTACTA PIC  X(0001).
000691     05  CERTACTI PIC  X(0010).
000692*    -------------------------------
000693     05  CERTSTL PIC S9(0004) COMP.
000694     05  CERTSTF PIC  X(0001).
000695     05  FILLER REDEFINES CERTSTF.
000696         10  CERTSTA PIC  X(0001).
000697     05  CERTSTI PIC  X(0002).
000698*    -------------------------------
000699     05  CERTCARL PIC S9(0004) COMP.
000700     05  CERTCARF PIC  X(0001).
000701     05  FILLER REDEFINES CERTCARF.
000702         10  CERTCARA PIC  X(0001).
000703     05  CERTCARI PIC  X(0001).
000704*    -------------------------------
000705     05  CERTGRPL PIC S9(0004) COMP.
000706     05  CERTGRPF PIC  X(0001).
000707     05  FILLER REDEFINES CERTGRPF.
000708         10  CERTGRPA PIC  X(0001).
000709     05  CERTGRPI PIC  X(0006).
000710*    -------------------------------
000711     05  SOCSECL PIC S9(0004) COMP.
000712     05  SOCSECF PIC  X(0001).
000713     05  FILLER REDEFINES SOCSECF.
000714         10  SOCSECA PIC  X(0001).
000715     05  SOCSECI PIC  X(0011).
000716*    -------------------------------
000717     05  CLNAMEL PIC S9(0004) COMP.
000718     05  CLNAMEF PIC  X(0001).
000719     05  FILLER REDEFINES CLNAMEF.
000720         10  CLNAMEA PIC  X(0001).
000721     05  CLNAMEI PIC  X(0015).
000722*    -------------------------------
000723     05  CFNAMEL PIC S9(0004) COMP.
000724     05  CFNAMEF PIC  X(0001).
000725     05  FILLER REDEFINES CFNAMEF.
000726         10  CFNAMEA PIC  X(0001).
000727     05  CFNAMEI PIC  X(0010).
000728*    -------------------------------
000729     05  CINITL PIC S9(0004) COMP.
000730     05  CINITF PIC  X(0001).
000731     05  FILLER REDEFINES CINITF.
000732         10  CINITA PIC  X(0001).
000733     05  CINITI PIC  X(0001).
000734*    -------------------------------
000735     05  INSAGEL PIC S9(0004) COMP.
000736     05  INSAGEF PIC  X(0001).
000737     05  FILLER REDEFINES INSAGEF.
000738         10  INSAGEA PIC  X(0001).
000739     05  INSAGEI PIC  X(0002).
000740*    -------------------------------
000741     05  CJLNAMEL PIC S9(0004) COMP.
000742     05  CJLNAMEF PIC  X(0001).
000743     05  FILLER REDEFINES CJLNAMEF.
000744         10  CJLNAMEA PIC  X(0001).
000745     05  CJLNAMEI PIC  X(0015).
000746*    -------------------------------
000747     05  CJFAMEL PIC S9(0004) COMP.
000748     05  CJFAMEF PIC  X(0001).
000749     05  FILLER REDEFINES CJFAMEF.
000750         10  CJFAMEA PIC  X(0001).
000751     05  CJFAMEI PIC  X(0010).
000752*    -------------------------------
000753     05  CJINITL PIC S9(0004) COMP.
000754     05  CJINITF PIC  X(0001).
000755     05  FILLER REDEFINES CJINITF.
000756         10  CJINITA PIC  X(0001).
000757     05  CJINITI PIC  X(0001).
000758*    -------------------------------
000759     05  JAGEL PIC S9(0004) COMP.
000760     05  JAGEF PIC  X(0001).
000761     05  FILLER REDEFINES JAGEF.
000762         10  JAGEA PIC  X(0001).
000763     05  JAGEI PIC  X(0002).
000764*    -------------------------------
000765     05  CVDESCRL PIC S9(0004) COMP.
000766     05  CVDESCRF PIC  X(0001).
000767     05  FILLER REDEFINES CVDESCRF.
000768         10  CVDESCRA PIC  X(0001).
000769     05  CVDESCRI PIC  X(0006).
000770*    -------------------------------
000771     05  CVKINDL PIC S9(0004) COMP.
000772     05  CVKINDF PIC  X(0001).
000773     05  FILLER REDEFINES CVKINDF.
000774         10  CVKINDA PIC  X(0001).
000775     05  CVKINDI PIC  X(0003).
000776*    -------------------------------
000777     05  CVCDL PIC S9(0004) COMP.
000778     05  CVCDF PIC  X(0001).
000779     05  FILLER REDEFINES CVCDF.
000780         10  CVCDA PIC  X(0001).
000781     05  CVCDI PIC  X(0002).
000782*    -------------------------------
000783     05  CVOTRML PIC S9(0004) COMP.
000784     05  CVOTRMF PIC  X(0001).
000785     05  FILLER REDEFINES CVOTRMF.
000786         10  CVOTRMA PIC  X(0001).
000787     05  CVOTRMI PIC  X(0003).
000788*    -------------------------------
000789     05  CVRTRML PIC S9(0004) COMP.
000790     05  CVRTRMF PIC  X(0001).
000791     05  FILLER REDEFINES CVRTRMF.
000792         10  CVRTRMA PIC  X(0001).
000793     05  CVRTRMI PIC  X(0003).
000794*    -------------------------------
000795     05  CVOBENEL PIC S9(0004) COMP.
000796     05  CVOBENEF PIC  X(0001).
000797     05  FILLER REDEFINES CVOBENEF.
000798         10  CVOBENEA PIC  X(0001).
000799     05  CVOBENEI PIC  9(9)V99.
000800*    -------------------------------
000801     05  CVFORML PIC S9(0004) COMP.
000802     05  CVFORMF PIC  X(0001).
000803     05  FILLER REDEFINES CVFORMF.
000804         10  CVFORMA PIC  X(0001).
000805     05  CVFORMI PIC  X(0012).
000806*    -------------------------------
000807     05  CVCNCDTL PIC S9(0004) COMP.
000808     05  CVCNCDTF PIC  X(0001).
000809     05  FILLER REDEFINES CVCNCDTF.
000810         10  CVCNCDTA PIC  X(0001).
000811     05  CVCNCDTI PIC  X(0008).
000812*    -------------------------------
000813     05  CVEXITL PIC S9(0004) COMP.
000814     05  CVEXITF PIC  X(0001).
000815     05  FILLER REDEFINES CVEXITF.
000816         10  CVEXITA PIC  X(0001).
000817     05  CVEXITI PIC  X(0008).
000818*    -------------------------------
000819     05  CVSTATL PIC S9(0004) COMP.
000820     05  CVSTATF PIC  X(0001).
000821     05  FILLER REDEFINES CVSTATF.
000822         10  CVSTATA PIC  X(0001).
000823     05  CVSTATI PIC  X(0006).
000824*    -------------------------------
000825     05  CMEMCAPL PIC S9(0004) COMP.
000826     05  CMEMCAPF PIC  X(0001).
000827     05  FILLER REDEFINES CMEMCAPF.
000828         10  CMEMCAPA PIC  X(0001).
000829     05  CMEMCAPI PIC  X(0010).
000830*    -------------------------------
000831     05  CAPRL PIC S9(0004) COMP.
000832     05  CAPRF PIC  X(0001).
000833     05  FILLER REDEFINES CAPRF.
000834         10  CAPRA PIC  X(0001).
000835     05  CAPRI PIC  9(4)V9(4).
000836*    -------------------------------
000837     05  CPFREQL PIC S9(0004) COMP.
000838     05  CPFREQF PIC  X(0001).
000839     05  FILLER REDEFINES CPFREQF.
000840         10  CPFREQA PIC  X(0001).
000841     05  CPFREQI PIC  99.
000842*    -------------------------------
000843     05  CINDGRPL PIC S9(0004) COMP.
000844     05  CINDGRPF PIC  X(0001).
000845     05  FILLER REDEFINES CINDGRPF.
000846         10  CINDGRPA PIC  X(0001).
000847     05  CINDGRPI PIC  X(0001).
000848*    -------------------------------
000849     05  CPREMTPL PIC S9(0004) COMP.
000850     05  CPREMTPF PIC  X(0001).
000851     05  FILLER REDEFINES CPREMTPF.
000852         10  CPREMTPA PIC  X(0001).
000853     05  CPREMTPI PIC  X(0002).
000854*    -------------------------------
000855     05  CREINCDL PIC S9(0004) COMP.
000856     05  CREINCDF PIC  X(0001).
000857     05  FILLER REDEFINES CREINCDF.
000858         10  CREINCDA PIC  X(0001).
000859     05  CREINCDI PIC  X(0003).
000860*    -------------------------------
000861     05  CMEMBERL PIC S9(0004) COMP.
000862     05  CMEMBERF PIC  X(0001).
000863     05  FILLER REDEFINES CMEMBERF.
000864         10  CMEMBERA PIC  X(0001).
000865     05  CMEMBERI PIC  X(0012).
000866*    -------------------------------
000867     05  MSGBL PIC S9(0004) COMP.
000868     05  MSGBF PIC  X(0001).
000869     05  FILLER REDEFINES MSGBF.
000870         10  MSGBA PIC  X(0001).
000871     05  MSGBI PIC  X(0075).
000872*    -------------------------------
000873     05  PFKEYBL PIC S9(0004) COMP.
000874     05  PFKEYBF PIC  X(0001).
000875     05  FILLER REDEFINES PFKEYBF.
000876         10  PFKEYBA PIC  X(0001).
000877     05  PFKEYBI PIC  X(0002).
000878 01  EL160BO REDEFINES EL160BI.
000879     05  FILLER            PIC  X(0012).
000880*    -------------------------------
000881     05  FILLER            PIC  X(0003).
000882     05  DATEBO PIC  X(0008).
000883*    -------------------------------
000884     05  FILLER            PIC  X(0003).
000885     05  TIMEBO PIC  X(0005).
000886*    -------------------------------
000887     05  FILLER            PIC  X(0003).
000888     05  TITLEO PIC  X(0028).
000889*    -------------------------------
000890     05  FILLER            PIC  X(0003).
000891     05  PIKEYO PIC  X(0039).
000892*    -------------------------------
000893     05  FILLER            PIC  X(0003).
000894     05  SCNERRO PIC  X(0004).
000895*    -------------------------------
000896     05  FILLER            PIC  X(0003).
000897     05  USERSAVO PIC  X(0004).
000898*    -------------------------------
000899     05  FILLER            PIC  X(0003).
000900     05  TIMESAVO PIC  9(07).
000901*    -------------------------------
000902     05  FILLER            PIC  X(0003).
000903     05  NOSCRNO PIC  X(0004).
000904*    -------------------------------
000905     05  FILLER            PIC  X(0003).
000906     05  TOTSCRNO PIC  X(0004).
000907*    -------------------------------
000908     05  FILLER            PIC  X(0003).
000909     05  CLAIMO PIC  X(0007).
000910*    -------------------------------
000911     05  FILLER            PIC  X(0003).
000912     05  TYPEO PIC  X(0001).
000913*    -------------------------------
000914     05  FILLER            PIC  X(0003).
000915     05  CERTO PIC  X(0010).
000916*    -------------------------------
000917     05  FILLER            PIC  X(0003).
000918     05  CERTSXO PIC  X(0001).
000919*    -------------------------------
000920     05  FILLER            PIC  X(0003).
000921     05  CARRO PIC  X(0001).
000922*    -------------------------------
000923     05  FILLER            PIC  X(0003).
000924     05  STATUSO PIC  X(0001).
000925*    -------------------------------
000926     05  FILLER            PIC  X(0003).
000927     05  PROCO PIC  X(0004).
000928*    -------------------------------
000929     05  FILLER            PIC  X(0003).
000930     05  FILEO PIC  X(0004).
000931*    -------------------------------
000932     05  FILLER            PIC  X(0003).
000933     05  CREDCDO PIC  X(0016).
000934*    -------------------------------
000935     05  FILLER            PIC  X(0003).
000936     05  MLNAMEO PIC  X(0015).
000937*    -------------------------------
000938     05  FILLER            PIC  X(0003).
000939     05  MFNAMEO PIC  X(0015).
000940*    -------------------------------
000941     05  FILLER            PIC  X(0003).
000942     05  MMINITO PIC  X(0001).
000943*    -------------------------------
000944     05  FILLER            PIC  X(0003).
000945     05  SEXO PIC  X(0001).
000946*    -------------------------------
000947     05  FILLER            PIC  X(0003).
000948     05  BIRTHO PIC  X(0008).
000949*    -------------------------------
000950     05  FILLER            PIC  X(0003).
000951     05  SOCIALO PIC  X(0011).
000952*    -------------------------------
000953     05  FILLER            PIC  X(0003).
000954     05  OCCO PIC  X(0006).
000955*    -------------------------------
000956     05  FILLER            PIC  X(0003).
000957     05  CBENEO PIC  X(0010).
000958*    -------------------------------
000959     05  FILLER            PIC  X(0003).
000960     05  BHEADO PIC  X(0029).
000961*    -------------------------------
000962     05  FILLER            PIC  X(0003).
000963     05  CAUSEO PIC  X(0026).
000964*    -------------------------------
000965     05  FILLER            PIC  X(0003).
000966     05  CCAUSCDO PIC  X(0006).
000967*    -------------------------------
000968     05  FILLER            PIC  X(0003).
000969     05  ENDO PIC  X(0008).
000970*    -------------------------------
000971     05  FILLER            PIC  X(0003).
000972     05  PDTHRUO PIC  X(0008).
000973*    -------------------------------
000974     05  FILLER            PIC  X(0003).
000975     05  PDAMTO PIC  Z(06).99.
000976*    -------------------------------
000977     05  FILLER            PIC  X(0003).
000978     05  NODAYSO PIC  ZZZ99.
000979*    -------------------------------
000980     05  FILLER            PIC  X(0003).
000981     05  NOPMTSO PIC  ZZ99.
000982*    -------------------------------
000983     05  FILLER            PIC  X(0003).
000984     05  INCO PIC  X(0008).
000985*    -------------------------------
000986     05  FILLER            PIC  X(0003).
000987     05  REPO PIC  X(0008).
000988*    -------------------------------
000989     05  FILLER            PIC  X(0003).
000990     05  ESTO PIC  X(0008).
000991*    -------------------------------
000992     05  FILLER            PIC  X(0003).
000993     05  MNTDTO PIC  X(0008).
000994*    -------------------------------
000995     05  FILLER            PIC  X(0003).
000996     05  MNTTYPEO PIC  X(0006).
000997*    -------------------------------
000998     05  FILLER            PIC  X(0003).
000999     05  PRICDO PIC  X(0001).
001000*    -------------------------------
001001     05  FILLER            PIC  X(0003).
001002     05  SUPVO PIC  X(0001).
001003*    -------------------------------
001004     05  FILLER            PIC  X(0003).
001005     05  LOANNOO PIC  X(0008).
001006*    -------------------------------
001007     05  FILLER            PIC  X(0003).
001008     05  LOANBALO PIC  Z,ZZZ,Z99.99.
001009*    -------------------------------
001010     05  FILLER            PIC  X(0003).
001011     05  CERTEFFO PIC  X(0008).
001012*    -------------------------------
001013     05  FILLER            PIC  X(0003).
001014     05  CERTACTO PIC  X(0010).
001015*    -------------------------------
001016     05  FILLER            PIC  X(0003).
001017     05  CERTSTO PIC  X(0002).
001018*    -------------------------------
001019     05  FILLER            PIC  X(0003).
001020     05  CERTCARO PIC  X(0001).
001021*    -------------------------------
001022     05  FILLER            PIC  X(0003).
001023     05  CERTGRPO PIC  X(0006).
001024*    -------------------------------
001025     05  FILLER            PIC  X(0003).
001026     05  SOCSECO PIC  X(0011).
001027*    -------------------------------
001028     05  FILLER            PIC  X(0003).
001029     05  CLNAMEO PIC  X(0015).
001030*    -------------------------------
001031     05  FILLER            PIC  X(0003).
001032     05  CFNAMEO PIC  X(0010).
001033*    -------------------------------
001034     05  FILLER            PIC  X(0003).
001035     05  CINITO PIC  X(0001).
001036*    -------------------------------
001037     05  FILLER            PIC  X(0003).
001038     05  INSAGEO PIC  X(0002).
001039*    -------------------------------
001040     05  FILLER            PIC  X(0003).
001041     05  CJLNAMEO PIC  X(0015).
001042*    -------------------------------
001043     05  FILLER            PIC  X(0003).
001044     05  CJFAMEO PIC  X(0010).
001045*    -------------------------------
001046     05  FILLER            PIC  X(0003).
001047     05  CJINITO PIC  X(0001).
001048*    -------------------------------
001049     05  FILLER            PIC  X(0003).
001050     05  JAGEO PIC  X(0002).
001051*    -------------------------------
001052     05  FILLER            PIC  X(0003).
001053     05  CVDESCRO PIC  X(0006).
001054*    -------------------------------
001055     05  FILLER            PIC  X(0003).
001056     05  CVKINDO PIC  X(0003).
001057*    -------------------------------
001058     05  FILLER            PIC  X(0003).
001059     05  CVCDO PIC  X(0002).
001060*    -------------------------------
001061     05  FILLER            PIC  X(0003).
001062     05  CVOTRMO PIC  999.
001063*    -------------------------------
001064     05  FILLER            PIC  X(0003).
001065     05  CVRTRMO PIC  999.
001066*    -------------------------------
001067     05  FILLER            PIC  X(0003).
001068     05  CVOBENEO PIC  ZZZZZZZZ.ZZ.
001069*    -------------------------------
001070     05  FILLER            PIC  X(0003).
001071     05  CVFORMO PIC  X(0012).
001072*    -------------------------------
001073     05  FILLER            PIC  X(0003).
001074     05  CVCNCDTO PIC  X(0008).
001075*    -------------------------------
001076     05  FILLER            PIC  X(0003).
001077     05  CVEXITO PIC  X(0008).
001078*    -------------------------------
001079     05  FILLER            PIC  X(0003).
001080     05  CVSTATO PIC  X(0006).
001081*    -------------------------------
001082     05  FILLER            PIC  X(0003).
001083     05  CMEMCAPO PIC  X(0010).
001084*    -------------------------------
001085     05  FILLER            PIC  X(0003).
001086     05  CAPRO PIC  9(3).9(4).
001087*    -------------------------------
001088     05  FILLER            PIC  X(0003).
001089     05  CPFREQO PIC  99.
001090*    -------------------------------
001091     05  FILLER            PIC  X(0003).
001092     05  CINDGRPO PIC  X(0001).
001093*    -------------------------------
001094     05  FILLER            PIC  X(0003).
001095     05  CPREMTPO PIC  X(0002).
001096*    -------------------------------
001097     05  FILLER            PIC  X(0003).
001098     05  CREINCDO PIC  X(0003).
001099*    -------------------------------
001100     05  FILLER            PIC  X(0003).
001101     05  CMEMBERO PIC  X(0012).
001102*    -------------------------------
001103     05  FILLER            PIC  X(0003).
001104     05  MSGBO PIC  X(0075).
001105*    -------------------------------
001106     05  FILLER            PIC  X(0003).
001107     05  PFKEYBO PIC  X(0002).
001108*    -------------------------------
      *<<((file: EL160S))
000311     EJECT
000312*    COPY ELCMSTR.
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
000313     EJECT
000314*    COPY ELCDATE.
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
000315     EJECT
000316*    COPY ELCATTR.
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
000317
000318*    COPY ELCAID.
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
000319
000320 01  FILLER REDEFINES DFHAID.
000321     12  FILLER                  PIC X(8).
000322     12  AID-KEYS OCCURS 24 TIMES.
000323         16  FILLER              PIC X.
000324
000325*    COPY ELCEMIB.
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
000326
000327     EJECT
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
000329
000330 01  DFHCOMMAREA                 PIC X(1024).
000331
000332 01  CLAIM-MASTER-L              PIC X(350).
000333
000334     EJECT
000335*    COPY ELCCERT.
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
000336     EJECT
000337*    COPY ELCTRLR.
      *>>((file: ELCTRLR))
000001******************************************************************
000002*                                                                *
000003*                            ELCTRLR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.014                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 200    RECFORM = FIXED                         *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
000013*       ALTERNATE INDEX = NONE                                   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000026* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
000027* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
000028* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
000029* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
000030* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
000031* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
000032* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
000033* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
000034* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
000035* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
000036* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
000037* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
000038* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
000039* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
000040* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000041* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
000042* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
000043* 040814    2014030500002  AJRA  ADD ICD CODES
000044* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000045* 013017  CR2016053100001  PEMA  ACH PROCESSING
000046* 062217  CR2017050300002  TANA  ADD AUTH RCVD
000047* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000048* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
000049******************************************************************
000050 01  ACTIVITY-TRAILERS.
000051     12  AT-RECORD-ID                    PIC XX.
000052         88  VALID-AT-ID                       VALUE 'AT'.
000053
000054     12  AT-CONTROL-PRIMARY.
000055         16  AT-COMPANY-CD               PIC X.
000056         16  AT-CARRIER                  PIC X.
000057         16  AT-CLAIM-NO                 PIC X(7).
000058         16  AT-CERT-NO.
000059             20  AT-CERT-PRIME           PIC X(10).
000060             20  AT-CERT-SFX             PIC X.
000061         16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
000062             88  AT-1ST-TRL-AVAIL             VALUE +4095.
000063             88  AT-LAST-TRL-AVAIL            VALUE +100.
000064             88  AT-RESV-EXP-HIST-TRL         VALUE +0.
000065             88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
000066             88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
000067             88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
000068             88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
000069             88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
000070             88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
000071             88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
000072             88  AT-DIAGNOSIS-TRL             VALUE +90.
000073             88  AT-BENEFICIARY-TRL           VALUE +91.
000074             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
000075             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
000076             88  AT-VFY-CAUSAL-STATE          VALUE +94.
000077             88  AT-ERROR-MSGS-TRL            VALUE +95.
000078
000079     12  AT-TRAILER-TYPE                 PIC X.
000080         88  RESERVE-EXPENSE-TR               VALUE '1'.
000081         88  PAYMENT-TR                       VALUE '2'.
000082         88  AUTO-PAY-TR                      VALUE '3'.
000083         88  CORRESPONDENCE-TR                VALUE '4'.
000084         88  ADDRESS-TR                       VALUE '5'.
000085         88  GENERAL-INFO-TR                  VALUE '6'.
000086         88  AUTO-PROMPT-TR                   VALUE '7'.
000087         88  DENIAL-TR                        VALUE '8'.
000088         88  INCURRED-CHG-TR                  VALUE '9'.
000089         88  FORM-CONTROL-TR                  VALUE 'A'.
000090
000091     12  AT-RECORDED-DT                  PIC XX.
000092     12  AT-RECORDED-BY                  PIC X(4).
000093     12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
000094
000095     12  AT-TRAILER-BODY                 PIC X(165).
000096
000097     12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
000098         16  AT-RESERVE-CONTROLS.
000099             20  AT-MANUAL-SW            PIC X.
000100                 88  AT-MANUAL-RESERVES-USED VALUE '1'.
000101             20  AT-FUTURE-SW            PIC X.
000102                 88  AT-FUTURE-RESERVES-USED VALUE '1'.
000103             20  AT-PTC-SW               PIC X.
000104                 88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
000105             20  AT-IBNR-SW              PIC X.
000106                 88  AT-IBNR-RESERVES-USED   VALUE '1'.
000107             20  AT-PTC-LF-SW            PIC X.
000108                 88  AT-LF-PTC-USED          VALUE '1'.
000109             20  AT-CDT-ACCESS-METHOD    PIC X.
000110                 88  AT-CDT-ROUND-NEAR       VALUE '1'.
000111                 88  AT-CDT-ROUND-HIGH       VALUE '2'.
000112                 88  AT-CDT-INTERPOLATED     VALUE '3'.
000113             20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
000114         16  AT-LAST-COMPUTED-DT         PIC XX.
000115         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
000116         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
000117         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
000118         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000119         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000120         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
000121         16  AT-EXPENSE-CONTROLS.
000122             20  AT-EXPENSE-METHOD       PIC X.
000123                 88  NO-EXPENSE-CALCULATED    VALUE '1'.
000124                 88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
000125                 88  PERCENT-OF-PMT           VALUE '3'.
000126                 88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
000127             20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
000128             20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
000129         16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
000130         16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
000131
000132         16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
000133         16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
000134
000135*        16  FILLER                      PIC X(53).
000136         16  FILLER                      PIC X(47).
000137
000138         16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
000139         16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
000140
000141         16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
000142             20  AT-OPEN-CLOSE-DATE      PIC XX.
000143             20  AT-OPEN-CLOSE-TYPE      PIC X.
000144*                    C = CLOSED
000145*                    O = OPEN
000146             20  AT-OPEN-CLOSE-REASON    PIC X(5).
000147*                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
000148
000149     12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
000150         16  AT-PAYMENT-TYPE             PIC X.
000151             88  PARTIAL-PAYMENT                VALUE '1'.
000152             88  FINAL-PAYMENT                  VALUE '2'.
000153             88  LUMP-SUM-PAYMENT               VALUE '3'.
000154             88  ADDITIONAL-PAYMENT             VALUE '4'.
000155             88  CHARGEABLE-EXPENSE             VALUE '5'.
000156             88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
000157             88  VOIDED-PAYMENT                 VALUE '9'.
000158             88  TRANSFER                       VALUE 'T'.
000159             88  LIFE-INTEREST                  VALUE 'I'.
000160
000161         16  AT-CLAIM-TYPE               PIC X.
000162             88  PAID-FOR-AH                    VALUE 'A'.
000163             88  PAID-FOR-LIFE                  VALUE 'L'.
000164             88  PAID-FOR-IUI                   VALUE 'I'.
000165             88  PAID-FOR-GAP                   VALUE 'G'.
000166             88  PAID-FOR-FAM                   VALUE 'F'.
000167             88  PAID-FOR-OTH                   VALUE 'O'.
000168         16  AT-CLAIM-PREM-TYPE          PIC X.
000169             88  AT-SINGLE-PREMIUM              VALUE '1'.
000170             88  AT-O-B-COVERAGE                VALUE '2'.
000171             88  AT-OPEN-END-COVERAGE           VALUE '3'.
000172         16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
000173         16  AT-CHECK-NO                 PIC X(7).
000174         16  AT-PAID-FROM-DT             PIC XX.
000175         16  AT-PAID-THRU-DT             PIC XX.
000176         16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
000177         16  AT-ACH-PAYMENT              PIC X.
000178*        16  FILLER                      PIC X.
000179         16  AT-PAYEES-NAME              PIC X(30).
000180         16  AT-PAYMENT-ORIGIN           PIC X.
000181             88  ONLINE-MANUAL-PMT              VALUE '1'.
000182             88  ONLINE-AUTO-PMT                VALUE '2'.
000183             88  OFFLINE-PMT                    VALUE '3'.
000184         16  AT-CHECK-WRITTEN-DT         PIC XX.
000185         16  AT-TO-BE-WRITTEN-DT         PIC XX.
000186         16  AT-VOID-DATA.
000187             20  AT-VOID-DT              PIC XX.
000188*00144       20  AT-VOID-REASON          PIC X(30).
000189             20  AT-VOID-REASON          PIC X(26).
000190         16  AT-PMT-APPROVED-BY          PIC X(04).
000191         16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
000192         16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
000193         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
000194                                         PIC S99V9(5)  COMP-3.
000195         16  AT-CREDIT-INTERFACE.
000196             20  AT-PMT-SELECT-DT        PIC XX.
000197                 88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
000198             20  AT-PMT-ACCEPT-DT        PIC XX.
000199                 88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
000200             20  AT-VOID-SELECT-DT       PIC XX.
000201                 88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
000202             20  AT-VOID-ACCEPT-DT       PIC XX.
000203                 88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
000204
000205         16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
000206                 88  PAYMENT-NOT-QUEUED           VALUE ZERO.
000207                 88  CONVERSION-PAYMENT           VALUE +99999999.
000208         16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
000209
000210         16  AT-FORCE-CONTROL            PIC X.
000211             88  PAYMENT-WAS-FORCED           VALUE '1'.
000212         16  AT-PREV-LAST-PMT-DT         PIC XX.
000213         16  AT-PREV-PAID-THRU-DT        PIC XX.
000214         16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
000215         16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
000216         16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
000217         16  AT-BENEFIT-TYPE             PIC X.
000218
000219         16  AT-EXPENSE-TYPE             PIC X.
000220         16  AT-PAYMENT-APPROVAL-SW      PIC X.
000221
000222         16  AT-PAYEE-TYPE-CD.
000223             20  AT-PAYEE-TYPE           PIC X.
000224                 88  INSURED-PAID           VALUE 'I'.
000225                 88  BENEFICIARY-PAID       VALUE 'B'.
000226                 88  ACCOUNT-PAID           VALUE 'A'.
000227                 88  OTHER-1-PAID           VALUE 'O'.
000228                 88  OTHER-2-PAID           VALUE 'Q'.
000229                 88  DOCTOR-PAID            VALUE 'P'.
000230                 88  EMPLOYER-PAID          VALUE 'E'.
000231             20  AT-PAYEE-SEQ            PIC X.
000232
000233         16  AT-CASH-PAYMENT             PIC X.
000234         16  AT-GROUPED-PAYMENT          PIC X.
000235         16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
000236         16  AT-APPROVAL-LEVEL-REQD      PIC X.
000237         16  AT-APPROVED-LEVEL           PIC X.
000238         16  AT-VOID-TYPE                PIC X.
000239             88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
000240             88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
000241         16  AT-AIG-UNEMP-IND            PIC X.
000242             88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
000243         16  AT-ASSOCIATES               PIC X.
000244             88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
000245             88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
000246
000247         16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
000248         16  AT-CV-PMT-CODE              PIC X.
000249             88  FULL-DEATH-PAYMENT         VALUE '1'.
000250             88  HALF-DEATH-PAYMENT         VALUE '2'.
000251             88  FULL-ADD-PAYMENT           VALUE '3'.
000252             88  HALF-ADD-PAYMENT           VALUE '4'.
000253             88  FULL-RIDER-PAYMENT         VALUE '5'.
000254             88  HALF-RIDER-PAYMENT         VALUE '6'.
000255             88  NON-CHG-EXP-PAYMENT        VALUE '7'.
000256             88  ADDL-PAYMENT               VALUE '8'.
000257
000258         16  AT-EOB-CODE1                PIC XXX.
000259         16  AT-EOB-CODE2                PIC XXX.
000260         16  AT-EOB-CODE3                PIC XXX.
000261         16  FILLER REDEFINES AT-EOB-CODE3.
000262             20  AT-PRINT-CLM-FORM       PIC X.
000263             20  AT-PRINT-SURVEY         PIC X.
000264             20  AT-SPECIAL-RELEASE      PIC X.
000265         16  AT-EOB-CODE4                PIC XXX.
000266         16  FILLER REDEFINES AT-EOB-CODE4.
000267             20  AT-INT-PMT-SELECT-DT    PIC XX.
000268             20  FILLER                  PIC X.
000269         16  AT-EOB-CODE5                PIC XXX.
000270         16  FILLER REDEFINES AT-EOB-CODE5.
000271             20  AT-PMT-PROOF-DT         PIC XX.
000272             20  FILLER                  PIC X.
000273
000274         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
000275             88  AT-PRINT-EOB            VALUE 'Y'.
000276
000277         16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
000278         16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
000279
000280     12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
000281         16  AT-SCHEDULE-START-DT        PIC XX.
000282         16  AT-SCHEDULE-END-DT          PIC XX.
000283         16  AT-TERMINATED-DT            PIC XX.
000284         16  AT-LAST-PMT-TYPE            PIC X.
000285             88  LAST-PMT-IS-FINAL              VALUE 'F'.
000286             88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
000287         16  AT-FIRST-PMT-DATA.
000288             20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
000289             20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
000290             20  AT-1ST-PAY-THRU-DT      PIC XX.
000291         16  AT-REGULAR-PMT-DATA.
000292             20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
000293             20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
000294             20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
000295         16  AT-AUTO-PAYEE-CD.
000296             20  AT-AUTO-PAYEE-TYPE      PIC X.
000297                 88  INSURED-PAID-AUTO      VALUE 'I'.
000298                 88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
000299                 88  ACCOUNT-PAID-AUTO      VALUE 'A'.
000300                 88  OTHER-1-PAID-AUTO      VALUE 'O'.
000301                 88  OTHER-2-PAID-AUTO      VALUE 'Q'.
000302             20  AT-AUTO-PAYEE-SEQ       PIC X.
000303         16  AT-AUTO-PAY-DAY             PIC 99.
000304         16  AT-AUTO-CASH                PIC X.
000305             88  AT-CASH                      VALUE 'Y'.
000306             88  AT-NON-CASH                  VALUE 'N'.
000307*        16  FILLER                      PIC X(129).
000308         16  AT-AUTO-END-LETTER          PIC X(4).
000309         16  FILLER                      PIC X(125).
000310
000311         16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
000312         16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
000313
000314     12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
000315         16  AT-LETTER-SENT-DT           PIC XX.
000316         16  AT-RECEIPT-FOLLOW-UP        PIC XX.
000317         16  AT-AUTO-RE-SEND-DT          PIC XX.
000318         16  AT-LETTER-ANSWERED-DT       PIC XX.
000319         16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
000320         16  AT-LETTER-ORIGIN            PIC X.
000321             88  ONLINE-CREATION              VALUE '1' '3'.
000322             88  OFFLINE-CREATION             VALUE '2' '4'.
000323             88  NAPER-ONLINE-CREATION        VALUE '3'.
000324             88  NAPER-OFFLINE-CREATION       VALUE '4'.
000325         16  AT-STD-LETTER-FORM          PIC X(4).
000326         16  AT-REASON-TEXT              PIC X(70).
000327         16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
000328         16  AT-ADDRESEE-TYPE            PIC X.
000329              88  INSURED-ADDRESEE            VALUE 'I'.
000330              88  BENEFICIARY-ADDRESEE        VALUE 'B'.
000331              88  ACCOUNT-ADDRESEE            VALUE 'A'.
000332              88  PHYSICIAN-ADDRESEE          VALUE 'P'.
000333              88  EMPLOYER-ADDRESEE           VALUE 'E'.
000334              88  OTHER-ADDRESEE-1            VALUE 'O'.
000335              88  OTHER-ADDRESEE-2            VALUE 'Q'.
000336         16  AT-ADDRESSEE-NAME           PIC X(30).
000337         16  AT-INITIAL-PRINT-DATE       PIC XX.
000338         16  AT-RESEND-PRINT-DATE        PIC XX.
000339         16  AT-CORR-SOL-UNSOL           PIC X.
000340         16  AT-LETTER-PURGED-DT         PIC XX.
000341*
000342*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
000343*
000344         16  AT-CSO-REDEFINITION.
000345             20  AT-RESEND-LETTER-FORM   PIC X(4).
000346             20  AT-AUTO-CLOSE-IND       PIC X(1).
000347             20  AT-LETTER-TO-BENE       PIC X(1).
000348             20  AT-STOP-LETTER-DT       PIC X(2).
000349             20  AT-AUTH-RCVD            PIC X(1).
000350             20  FILLER                  PIC X(18).
000351*             20  FILLER                  PIC X(27).
000352             20  AT-CSO-LETTER-STATUS    PIC X.
000353                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
000354                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
000355                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
000356             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
000357             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
000358*
000359*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
000360*
000361*        16  FILLER                      PIC X(26).
000362*
000363*        16  AT-DMD-BSR-CODE             PIC X.
000364*            88  AT-AUTOMATED-BSR              VALUE 'A'.
000365*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
000366*
000367*        16  AT-DMD-LETTER-STATUS        PIC X.
000368*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
000369*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
000370*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
000371*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
000372*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
000373
000374         16  AT-CORR-LAST-MAINT-DT       PIC XX.
000375         16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
000376
000377     12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
000378         16  AT-ADDRESS-TYPE             PIC X.
000379             88  INSURED-ADDRESS               VALUE 'I'.
000380             88  BENEFICIARY-ADDRESS           VALUE 'B'.
000381             88  ACCOUNT-ADDRESS               VALUE 'A'.
000382             88  PHYSICIAN-ADDRESS             VALUE 'P'.
000383             88  EMPLOYER-ADDRESS              VALUE 'E'.
000384             88  OTHER-ADDRESS-1               VALUE 'O'.
000385             88  OTHER-ADDRESS-2               VALUE 'Q'.
000386         16  AT-MAIL-TO-NAME             PIC X(30).
000387         16  AT-ADDRESS-LINE-1           PIC X(30).
000388         16  AT-ADDRESS-LINE-2           PIC X(30).
000389         16  AT-CITY-STATE.
000390             20  AT-CITY                 PIC X(28).
000391             20  AT-STATE                PIC XX.
000392         16  AT-ZIP.
000393             20  AT-ZIP-CODE.
000394                 24  AT-ZIP-1ST          PIC X.
000395                     88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000396                 24  FILLER              PIC X(4).
000397             20  AT-ZIP-PLUS4            PIC X(4).
000398         16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
000399             20  AT-CAN-POSTAL-1         PIC XXX.
000400             20  AT-CAN-POSTAL-2         PIC XXX.
000401             20  FILLER                  PIC XXX.
000402         16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
000403*         16  FILLER                      PIC X(23).
000404         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
000405         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
000406         16  FILLER                      PIC X(13).
000407         16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
000408         16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
000409
000410     12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000411         16  AT-INFO-LINE-1              PIC X(60).
000412         16  FILLER REDEFINES AT-INFO-LINE-1.
000413             20  AT-NOTE-ERROR-NO OCCURS 15
000414                                         PIC X(4).
000415         16  AT-INFO-LINE-2              PIC X(60).
000416         16  FILLER REDEFINES AT-INFO-LINE-2.
000417             20  AT-ICD-CODE-1           PIC X(8).
000418             20  AT-ICD-CODE-2           PIC X(8).
000419             20  FILLER                  PIC X(44).
000420         16  AT-INFO-TRAILER-TYPE        PIC X.
000421             88  AT-ERRORS-NOTE          VALUE 'E'.
000422             88  AT-PAYMENT-NOTE         VALUE 'P'.
000423             88  AT-CALL-NOTE            VALUE 'C'.
000424             88  AT-MAINT-NOTE           VALUE 'M'.
000425             88  AT-CERT-CHANGE          VALUE 'X'.
000426             88  AT-APPROVAL-NOTE        VALUE 'R'.
000427             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
000428             88  AT-CERT-CANCELLED       VALUE 'T'.
000429         16  AT-CALL-TYPE                PIC X.
000430             88  AT-PHONE-CALL-IN        VALUE 'I'.
000431             88  AT-PHONE-CALL-NEW       VALUE 'N'.
000432             88  AT-PHONE-CALL-OUT       VALUE 'O'.
000433         16  AT-NOTE-CONTINUATION        PIC X.
000434             88  AT-CONTINUED-NOTE       VALUE 'X'.
000435         16  AT-EOB-CODES-EXIST          PIC X.
000436             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
000437         16  FILLER                      PIC X(35).
000438         16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
000439         16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
000440
000441     12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
000442         16  AT-PROMPT-LINE-1            PIC X(60).
000443         16  AT-PROMPT-LINE-2            PIC X(60).
000444         16  AT-PROMPT-START-DT          PIC XX.
000445         16  AT-PROMPT-END-DT            PIC XX.
000446         16  FILLER                      PIC X(35).
000447         16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
000448         16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
000449
000450     12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000451         16  AT-DENIAL-INFO-1            PIC X(60).
000452         16  AT-DENIAL-INFO-2            PIC X(60).
000453         16  AT-DENIAL-DT                PIC XX.
000454         16  AT-RETRACTION-DT            PIC XX.
000455         16  AT-DENIAL-REASON-CODE       PIC X(4).
000456*         16  FILLER                      PIC X(31).
000457         16  AT-DENIAL-PROOF-DT          PIC XX.
000458         16  FILLER                      PIC X(29).
000459         16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
000460         16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
000461
000462     12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
000463         16  AT-OLD-INCURRED-DT          PIC XX.
000464         16  AT-OLD-REPORTED-DT          PIC XX.
000465         16  AT-OLD-ESTABLISHED-DT       PIC XX.
000466         16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
000467         16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
000468         16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
000469         16  AT-OLD-PAID-THRU-DT         PIC XX.
000470         16  AT-LAST-PMT-MADE-DT         PIC XX.
000471         16  FILLER                      PIC X(26).
000472         16  AT-OLD-DIAG-CODE            PIC X(6).
000473         16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
000474         16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
000475         16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
000476         16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
000477         16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
000478         16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
000479         16  AT-OLD-DIAG-DESCRIP         PIC X(60).
000480         16  AT-OLD-ICD-CODE-1           PIC X(8).
000481         16  AT-OLD-ICD-CODE-2           PIC X(8).
000482         16  FILLER                      PIC X(9).
000483         16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
000484
000485     12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
000486         16  AT-FORM-SEND-ON-DT          PIC XX.
000487         16  AT-FORM-FOLLOW-UP-DT        PIC XX.
000488         16  AT-FORM-RE-SEND-DT          PIC XX.
000489         16  AT-FORM-ANSWERED-DT         PIC XX.
000490         16  AT-FORM-PRINTED-DT          PIC XX.
000491         16  AT-FORM-REPRINT-DT          PIC XX.
000492         16  AT-FORM-TYPE                PIC X.
000493             88  INITIAL-FORM                  VALUE '1'.
000494             88  PROGRESS-FORM                 VALUE '2'.
000495         16  AT-INSTRUCT-LN-1            PIC X(28).
000496         16  AT-INSTRUCT-LN-2            PIC X(28).
000497         16  AT-INSTRUCT-LN-3            PIC X(28).
000498         16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
000499         16  AT-FORM-ADDRESS             PIC X.
000500             88  FORM-TO-INSURED              VALUE 'I'.
000501             88  FORM-TO-ACCOUNT              VALUE 'A'.
000502             88  FORM-TO-OTHER-1              VALUE 'O'.
000503             88  FORM-TO-OTHER-2              VALUE 'Q'.
000504         16  AT-RELATED-1.
000505             20 AT-REL-CARR-1            PIC X.
000506             20 AT-REL-CLAIM-1           PIC X(7).
000507             20 AT-REL-CERT-1            PIC X(11).
000508         16  AT-RELATED-2.
000509             20 AT-REL-CARR-2            PIC X.
000510             20 AT-REL-CLAIM-2           PIC X(7).
000511             20 AT-REL-CERT-2            PIC X(11).
000512         16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
000513         16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
000514         16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
000515         16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
000516         16  AT-FORM-REM-PRINT-DT        PIC XX.
000517         16  AT-STOP-FORM-DT             PIC X(2).
000518
000519         16  FILLER                      PIC X(09).
000520         16  AT-FORM-LAST-MAINT-DT       PIC XX.
000521         16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
000522******************************************************************
      *<<((file: ELCTRLR))
000338     EJECT
000339*    COPY ELCCNTL.
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
000340     EJECT
000341*    COPY MPCPLCY.
      *>>((file: MPCPLCY))
000001******************************************************************
000002*                                                                *
000003*                           MPCPLCY                              *
000004*                            VMOD=1.024                          *
000005*                                                                *
000006*   FILE DESCRIPTION = POLICY MASTER                             *
000007*                                                                *
000008*   FILE TYPE = VSAM,KSDS                                        *
000009*   RECORD SIZE = 1200 RECFORM = FIXED                           *
000010*                                                                *
000011*   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
000012*       ALTERNATE PATH2 = ** NOT USED **                         *
000013*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000014*       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
000015*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
000016*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
000017*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
000018*                                                                *
000019*   LOG = YES                                                    *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021******************************************************************
000022**WARNING*********************************************************
000023**ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
000024**TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
000025**                             MPCPHSTD                          *
000026**                             MPCPHSTC                          *
000027**                             MPCPHSTT                          *
000028**                                                               *
000029******************************************************************
000030
000031 01  POLICY-MASTER.
000032     12  PM-RECORD-ID                      PIC XX.
000033         88  VALID-PM-ID                      VALUE 'PM'.
000034
000035******************************************************************
000036*   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
000037******************************************************************
000038
000039     12  PM-CONTROL-PRIMARY.
000040         16  PM-PRODUCER-PRIMARY.
000041             20  PM-PROD-PRIMARY.
000042                 24  PM-COMPANY-CD         PIC X.
000043                 24  PM-CGSP-KEY.
000044                     28  PM-CARRIER        PIC X.
000045                     28  PM-GROUPING.
000046                         32  PM-GROUPING-PREFIX
000047                                           PIC X(3).
000048                         32  PM-GROUPING-PRIME
000049                                           PIC X(3).
000050                     28  PM-STATE          PIC X(2).
000051                     28  PM-PRODUCER.
000052                         32  PM-PRODUCER-PREFIX
000053                                           PIC X(4).
000054                         32  PM-PRODUCER-PRIME
000055                                           PIC X(6).
000056             20  PM-POLICY-EFF-DT              PIC XX.
000057         16  PM-REFERENCE-NUMBER.
000058             20  PM-REFNO-PRIME            PIC X(18).
000059             20  PM-REFNO-SFX              PIC XX.
000060
000061******************************************************************
000062*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000063******************************************************************
000064
000065     12  PM-CONTROL-BY-SSN.
000066         16  PM-COMPANY-CD-A3              PIC X.
000067         16  PM-SOC-SEC-NO.
000068             20  PM-SSN-STATE              PIC XX.
000069             20  PM-SSN-PRODUCER           PIC X(6).
000070             20  PM-SSN-LN3.
000071                 25  PM-INSURED-INITIALS-A3.
000072                     30 PM-INSURED-INITIAL1-A3 PIC X.
000073                     30 PM-INSURED-INITIAL2-A3 PIC X.
000074                 25 PM-PART-LAST-NAME-A3         PIC X.
000075         16  PM-DATE-A3                     PIC XX.
000076         16  PM-TIME-A3                     PIC S9(04)   COMP.
000077
000078******************************************************************
000079*       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
000080******************************************************************
000081
000082     12  PM-CONTROL-BY-POLICY-NO.
000083         16  PM-COMPANY-CD-A4              PIC X.
000084         16  PM-POLICY-NO-A4.
000085             20  PM-POLICY-PRIME-A4        PIC X(18).
000086             20  PM-POLICY-SFX-A4          PIC XX.
000087         16  PM-DATE-A4                    PIC XX.
000088         16  PM-TIME-A4                    PIC S9(04)   COMP.
000089
000090******************************************************************
000091*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
000092******************************************************************
000093
000094     12  PM-CONTROL-BY-ACCOUNT.
000095         16  PM-COMPANY-CD-A5              PIC X.
000096         16  PM-BANK-ACCOUNT-NUMBER        PIC X(20).
000097         16  PM-DATE-A5                    PIC XX.
000098         16  PM-TIME-A5                    PIC S9(07)   COMP.
000099
000100******************************************************************
000101*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
000102******************************************************************
000103
000104     12  PM-CONTROL-BY-TRANSIT.
000105         16  PM-COMPANY-CD-A6              PIC X.
000106         16  PM-BANK-TRANSIT-NUMBER.
000107             20  PM-FEDERAL-NUMBER         PIC X(4).
000108             20  PM-BANK-NUMBER            PIC X(4).
000109         16  PM-DATE-A6                    PIC XX.
000110         16  PM-TIME-A6                    PIC S9(07)   COMP.
000111
000112******************************************************************
000113*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
000114******************************************************************
000115
000116     12  PM-CONTROL-BY-LOAN-NO.
000117         16  PM-COMPANY-CD-A7              PIC X.
000118         16  PM-LOAN-NUMBER                PIC X(20).
000119         16  PM-DATE-A7                    PIC XX.
000120         16  PM-TIME-A7                    PIC S9(07)   COMP.
000121
000122******************************************************************
000123*                 FILE SYNCHRONIZATION DATA                      *
000124******************************************************************
000125
000126     12  FILLER                            PIC X(05).
000127     12  PM-FILE-SYNCH-DATA.
000128         16  PM-LAST-CHANGE-DT             PIC XX.
000129         16  PM-LAST-CHANGE-TIME           PIC S9(7)    COMP.
000130         16  PM-LAST-CHANGE-PROCESSOR      PIC X(4).
000131     12  FILLER                            PIC X(05).
000132
000133******************************************************************
000134*                    INSUREDS PROFILE DATA                       *
000135******************************************************************
000136
000137     12  PM-INSURED-PROFILE-DATA.
000138         16  PM-INSURED-NAME.
000139             20  PM-INSURED-LAST-NAME     PIC X(15).
000140             20  PM-INSURED-FIRST-NAME.
000141                 24  PM-INSURED-1ST-INIT PIC X.
000142                 24  FILLER               PIC X(9).
000143             20  PM-INSURED-MIDDLE-INIT PIC X.
000144         16  PM-INSURED-ADDRESS.
000145             20  PM-ADDRESS-LINE-1         PIC X(30).
000146             20  PM-ADDRESS-LINE-2         PIC X(30).
000147             20  PM-CITY                   PIC X(25).
000148             20  PM-RESIDENT-STATE         PIC XX.
000149             20  PM-ZIP-CD.
000150                 24  PM-ZIP-FIRST-FIVE     PIC X(5).
000151                 24  PM-ZIP-PLUS-FOUR      PIC X(4).
000152         16  PM-INSURED-PERSONAL.
000153             20  PM-INSURED-OCC-CLASS      PIC X.
000154                 88  PM-PREFERRED            VALUE '1'.
000155                 88  PM-STANDARD             VALUE '2'.
000156                 88  PM-HAZARDOUS            VALUE '3'.
000157                 88  PM-VERY-HAZARDOUS       VALUE '4'.
000158                 88  PM-EXTREME-HAZARDOUS VALUE '5'.
000159                 88  PM-NOT-OCC              VALUE '6'.
000160                 88  PM-OCC-UNKNOWN          VALUE '9'.
000161             20  PM-INSURED-OCC-CD         PIC X(3).
000162             20  PM-INSURED-OCC-CD-NUM REDEFINES
000163                 PM-INSURED-OCC-CD         PIC 9(3).
000164             20  PM-INSURED-SEX            PIC X.
000165                 88  PM-INSURED-SEX-MALE      VALUE 'M'.
000166                 88  PM-INSURED-SEX-FEMALE VALUE 'F'.
000167             20  PM-INSURED-BIRTH-DT       PIC XX.
000168             20  PM-INSURED-ISSUE-AGE      PIC S9(3)     COMP-3.
000169             20  PM-INSURED-HEIGHT-FT      PIC S9(3)     COMP-3.
000170             20  PM-INSURED-HEIGHT-IN      PIC S9(3)     COMP-3.
000171             20  PM-INSURED-WEIGHT         PIC S9(3)     COMP-3.
000172             20  PM-INSURED-BIRTH-STATE PIC XX.
000173             20  PM-INSURED-PHONE-NO       PIC X(13).
000174             20  PM-INSURED-RATED-AGE      PIC S9(3)     COMP-3.
000175         16  PM-INS-LANGUAGE-IND           PIC X(01).
000176             88  PM-ENGLISH                           VALUE 'E'.
000177             88  PM-FRENCH                            VALUE 'F'.
000178             88  PM-SPANISH                           VALUE 'S'.
000179         16  PM-INSURED-TOT-BENEFIT        PIC S9(7)V99  COMP-3.
000180
000181         16  PM-INSURED-AGE-IND            PIC X(01).
000182             88  PM-INSURED-AGE-75-REACHED            VALUE 'Y'.
000183     12  FILLER                            PIC X(13).
000184
000185******************************************************************
000186*                JOINT INSUREDS PROFILE DATA                     *
000187******************************************************************
000188
000189     12  PM-JOINT-PROFILE-DATA.
000190         16  PM-JOINT-NAME.
000191             20  PM-JOINT-LAST-NAME        PIC X(15).
000192             20  PM-JOINT-FIRST-NAME.
000193                 24  PM-JOINT-1ST-INIT     PIC X.
000194                 24  FILLER                PIC X(9).
000195             20  PM-JOINT-MIDDLE-INIT      PIC X.
000196         16  PM-JOINT-SOC-SEC-NO.
000197             20  PM-JT-SSN-STATE           PIC XX.
000198             20  PM-JT-SSN-PRODUCER        PIC X(6).
000199             20  PM-JT-SSN-LN3.
000200                 25  PM-JT-INSURED-INITIALS-A3.
000201                     30 PM-JT-INSURED-INITIAL1-A3 PIC X.
000202                     30 PM-JT-INSURED-INITIAL2-A3 PIC X.
000203                 25 PM-JT-PART-LAST-NAME-A3        PIC X.
000204         16  PM-JOINT-PERSONAL.
000205             20  PM-JOINT-OCC-CLASS        PIC X.
000206                 88 PM-JNT-PREFERRED          VALUE '1'.
000207                 88 PM-JNT-STANDARD           VALUE '2'.
000208                 88 PM-JNT-HAZARDOUS          VALUE '3'.
000209                 88 PM-JNT-VERY-HAZARDOUS     VALUE '4'.
000210                 88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
000211                 88 PM-JNT-NOT-OCC            VALUE '6'.
000212                 88 PM-JNT-OCC-UNKNOWN        VALUE '9'.
000213             20  PM-JOINT-OCC-CD           PIC X(3).
000214             20  PM-JOINT-SEX              PIC X.
000215                 88  PM-JOINT-SEX-MALE        VALUE 'M'.
000216                 88  PM-JOINT-SEX-FEMALE      VALUE 'F'.
000217             20  PM-JOINT-BIRTH-DT         PIC XX.
000218             20  PM-JOINT-ISSUE-AGE        PIC S9(3)     COMP-3.
000219             20  PM-JOINT-HEIGHT-FT        PIC S9(3)     COMP-3.
000220             20  PM-JOINT-HEIGHT-IN        PIC S9(3)     COMP-3.
000221             20  PM-JOINT-WEIGHT           PIC S9(3)     COMP-3.
000222             20  PM-JOINT-BIRTH-STATE      PIC XX.
000223             20  PM-JOINT-RATED-AGE        PIC S9(3)     COMP-3.
000224         16  PM-JOINT-TOT-BENEFIT          PIC S9(7)V99  COMP-3.
000225         16  PM-JOINT-AGE-IND              PIC X(01).
000226             88  PM-JOINT-AGE-75-REACHED              VALUE 'Y'.
000227
000228     12  FILLER                            PIC X(12).
000229
000230******************************************************************
000231*                  INSURANCE COVERAGE DATA                       *
000232******************************************************************
000233
000234     12  PM-INS-COVERAGE-DATA.
000235         16  PM-FREE-PERIOD                PIC S9(03)    COMP-3.
000236         16  PM-LOAN-TERM                  PIC S9(3)     COMP-3.
000237         16  PM-LOAN-APR                   PIC S9V9999   COMP-3.
000238         16  PM-LOAN-DT                    PIC XX.
000239         16  PM-LOAN-PYMT                  PIC S9(5)V99  COMP-3.
000240         16  PM-LOAN-BALC                  PIC S9(7)V99  COMP-3.
000241         16  PM-INS-BENEFIT-MONTHS         PIC S9(3)     COMP-3.
000242         16  PM-INS-MONTH-BENEFIT          PIC S9(7)V99  COMP-3.
000243         16  PM-INS-TOTAL-BENEFIT          PIC S9(7)V99  COMP-3.
000244         16  PM-INS-PLAN-TYPE              PIC X.
000245             88  PM-AH-MORT-PLAN              VALUE 'A'.
000246             88  PM-AD-D-MORT-PLAN            VALUE 'E'.
000247             88  PM-DISMEM-MORT-PLAN          VALUE 'D'.
000248             88  PM-LIFE-MORT-PLAN            VALUE 'L'.
000249         16  PM-INS-PLAN-CD                PIC XX.
000250         16  PM-INS-PLAN-REVISION          PIC X(3).
000251         16  PM-INS-POLICY-FORM            PIC X(12).
000252         16  PM-INS-MSTR-POLICY.
000253             20  PM-FREE-TYPE              PIC X(04).
000254             20  FILLER                    PIC X(08).
000255         16  PM-INS-MSTR-APP.
000256             20  FILLER                    PIC X(11).
000257             20  PM-INS-B-C-TYPE           PIC X(01).
000258         16  PM-INS-RATE-CD                PIC X(5).
000259         16  PM-INS-SEX-RATING             PIC X.
000260             88  PM-NOT-SEX-RATED              VALUE '1'.
000261             88  PM-SEX-RATED                  VALUE '2'.
000262         16  PM-INS-SUBSTANDARD-PCT        PIC S9V9999   COMP-3.
000263         16  PM-INS-SUBSTANDARD-TYPE       PIC X.
000264         16  PM-INS-TERMINATION-DT         PIC XX.
000265         16  PM-INS-MONTH-PREMIUM      PIC S9(5)V999999  COMP-3.
000266         16  PM-INS-CALC-MO-PREM       PIC S9(5)V999999  COMP-3.
000267         16  PM-REINSURANCE-TABLE          PIC X(3).
000268         16  PM-MORTALITY-CD               PIC X(4).
000269         16  PM-INS-TYPE                   PIC X.
000270             88  PM-INDIVIDUAL                VALUES ARE '1' 'I'.
000271             88  PM-GROUP                     VALUES ARE '2' 'G'.
000272         16  PM-LOAN-OFFICER               PIC X(5).
000273         16  PM-POLICY-FEE                 PIC S9(3)V99 COMP-3.
000274         16  PM-DEPENDENT-COUNT            PIC S99      COMP-3.
000275         16  PM-CWA-AMOUNT                 PIC S9(5)V99  COMP-3.
000276         16  PM-LAST-AUTO-RERATE-DT        PIC XX.
000277         16  PM-PREM-FINANCED-SW           PIC X.
000278             88  PM-PREM-FINANCED              VALUE 'Y'.
000279             88  PM-PREM-NOT-FINANCED          VALUE 'N'.
000280
000281         16  PM-INS-TERM-LETTER-IND        PIC X.
000282             88  PM-TERM-INITIALIZED           VALUE 'Y'.
000283         16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99     COMP-3.
000284     12  FILLER                            PIC X(11).
000285
000286******************************************************************
000287*                    POLICY BILLING DATA                         *
000288******************************************************************
000289
000290     12  PM-BILLING-DATA.
000291         16  PM-BILLING-MODE               PIC X(1).
000292             88  PM-ANNUAL                    VALUE '1'.
000293             88  PM-SEMI-ANNUAL               VALUE '2'.
000294             88  PM-QUARTERLY                 VALUE '3'.
000295             88  PM-MONTHLY                   VALUE '4'.
000296             88  PM-BI-MONTHLY                VALUE '5'.
000297             88  PM-SINGLE-PREM               VALUE '6'.
000298         16  PM-BILLING-SCHEDULE           PIC X(1).
000299         16  PM-BILLING-SW                 PIC X(1).
000300             88  PM-FIRST-BILLING             VALUE 'Y'.
000301             88  PM-PAID-IN-ADVANCE           VALUE 'A'.
000302             88  PM-POLICY-FEE-REFUNDED       VALUE 'F'.
000303         16  PM-BILLING-TYPE               PIC X(1).
000304             88  PM-LIST-BILL                 VALUE '1'.
000305             88  PM-TAPE-BILL                 VALUE '2'.
000306             88  PM-TAPE-LIST-BILL            VALUE '3'.
000307             88  PM-GROUP-BILL          VALUE ARE '1' '2' '3'.
000308             88  PM-DIRECT-BILL               VALUE '4'.
000309             88  PM-PAC-BILL            VALUE ARE '5' 'C' 'S'.
000310             88  PM-CHARGE-CARD-BILL          VALUE '6'.
000311             88  PM-INDIV-BILL
000312                                  VALUE ARE '4' '5' '6' 'C' 'S'.
000313             88  PM-GRP-PLCY-BILL             VALUE '7'.
000314             88  PM-GRP-PLCY-PAC              VALUE '8'.
000315             88  PM-GRP-PLCY-CR-CRD           VALUE '9'.
000316             88  PM-GRP-PLCY            VALUE ARE '7' '8' '9'.
000317             88  PM-GRP-PROD                  VALUE 'A'.
000318             88  PM-EFT-CHECKING              VALUE 'C'.
000319             88  PM-EFT-SAVINGS               VALUE 'S'.
000320         16  PM-PAYMENT-AMT                PIC S9(5)V99  COMP-3.
000321         16  PM-OVER-SHORT-AMT             PIC S9(5)V99  COMP-3.
000322         16  PM-LAST-BILL-DT               PIC XX.
000323         16  PM-LAST-BILL-AMT              PIC S9(5)V99  COMP-3.
000324         16  PM-BILL-TO-DT                 PIC XX.
000325         16  PM-LAST-PYMT-DT               PIC XX.
000326         16  PM-PAID-TO-DT                 PIC XX.
000327         16  PM-PYMT-INVOICE-NUMBER        PIC X(6).
000328         16  PM-MONTHS-PAID                PIC S9(3)     COMP-3.
000329         16  PM-TOTAL-PREM-RECVD           PIC S9(7)V99  COMP-3.
000330         16  PM-BILLING-GROUPING-CODE      PIC X(6).
000331         16  PM-CHARGE-CARD-EXP-DT         PIC X(2).
000332         16  PM-CHARGE-CARD-TYPE           PIC X(2).
000333             88  PM-VISA                      VALUE 'VI'.
000334             88  PM-MSTR-CARD                 VALUE 'MC'.
000335             88  PM-DINERS-CLUB               VALUE 'DN'.
000336             88  PM-DISCOVER                  VALUE 'DS'.
000337             88  PM-CARTE-BLANCHE             VALUE 'CB'.
000338             88  PM-AMERICAN-EXPRESS          VALUE 'AE'.
000339         16  PM-BILL-INVOICE-NUMBER        PIC X(6).
000340         16  PM-BILL-DAY                   PIC S99       COMP-3.
000341         16  PM-RES-PREM-TAX           PIC S9(3)V999999  COMP-3.
000342     12  FILLER                            PIC X(15).
000343
000344******************************************************************
000345*                     CLAIM PAYMENT DATA                         *
000346******************************************************************
000347
000348     12  PM-CLAIM-PAYMENT-DATA.
000349         16  PM-CLAIM-BENEFICIARY-NAME     PIC X(25).
000350         16  PM-CLAIM-INTERFACE-SW         PIC X.
000351             88  PM-NO-CLAIM-ATTACHED         VALUE SPACE.
000352             88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
000353             88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
000354             88  PM-CLAIM-CLOSED              VALUE '3'.
000355             88  PM-ACTIVE-CLAIM              VALUE '1' '2'.
000356             88  PM-CLAIM-ATTACHED            VALUE '1' '2' '3'.
000357         16  PM-CLAIM-INCURRED-DT          PIC XX.
000358         16  PM-CLAIM-PAID-TO-DT           PIC XX.
000359         16  PM-CLAIM-PAYMENT-CNT          PIC S9(3)     COMP-3.
000360         16  PM-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99  COMP-3.
000361         16  PM-CLAIM-EXPENSES-ITD         PIC S9(7)V99  COMP-3.
000362         16  PM-CLAIM-PAYMENTS-ITD         PIC S9(7)V99  COMP-3.
000363         16  PM-CLAIM-ACCUMULATOR          PIC S9(7)V99  COMP-3.
000364         16  PM-CLAIM-ATTACH-CNT           PIC S9(3)     COMP-3.
000365         16  PM-CLAIM-LIFE-ITD             PIC S9(7)V99  COMP-3.
000366         16  PM-CLAIM-AH-ITD               PIC S9(7)V99  COMP-3.
000367         16  PM-CLAIM-RIDER-ITD            PIC S9(7)V99  COMP-3.
000368
000369     12  FILLER                            PIC X(03).
000370
000371******************************************************************
000372*                POLICY STATUS AND DISPOSITION                   *
000373******************************************************************
000374
000375     12  PM-STATUS-DISPOSITION-DATA.
000376         16  PM-ISSUE-EOM-DT               PIC XX.
000377         16  PM-REPLACEMENT-SWITCH         PIC X.
000378         16  PM-APPL-SIGN-DT               PIC XX.
000379         16  PM-UNDERWRITER                PIC X(3).
000380         16  PM-ENTRY-PROCESSOR            PIC X(4).
000381         16  PM-ENTRY-STATUS               PIC X.
000382             88  PM-NORMAL                    VALUE '1'.
000383             88  PM-TAKE-OVER                 VALUE '2'.
000384             88  PM-CONVERSION                VALUE '4'.
000385             88  PM-RE-ISSUE                  VALUE '5'.
000386             88  PM-REINSURANCE-ONLY          VALUE '9'.
000387         16  PM-ENTRY-DT                   PIC XX.
000388         16  PM-ENTRY-TIME                 PIC S9(7) COMP-3.
000389         16  PM-EXIT-DT                    PIC XX.
000390         16  PM-CURRENT-STATUS             PIC X.
000391             88  PM-LAPSE                     VALUE '0'.
000392             88  PM-ACTIVE                    VALUE '1'.
000393             88  PM-PENDING-ISSUE             VALUE '2'.
000394             88  PM-DECLINED                  VALUE '3'.
000395             88  PM-PENDING-CANCEL            VALUE '4'.
000396             88  PM-PENDING-ISSUE-ERROR       VALUE '5'.
000397             88  PM-CLAIM-APPLIED             VALUE '6'.
000398             88  PM-CANCEL                    VALUE '7'.
000399             88  PM-PENDING-UNWTR-REVW        VALUE '8'.
000400             88  PM-PENDING-CANCEL-ERROR      VALUE '9'.
000401             88  PM-CANCEL-TRANSFER           VALUE 'C'.
000402             88  PM-CLAIM-SETTLEMENT          VALUE 'F'.
000403             88  PM-TERMINATE                 VALUE 'T'.
000404** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
000405** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
000406** THESE GROUPS.
000407             88  PM-TYPE-STAT-1
000408                     VALUES ARE '0' '1' '4' '6' '7' '9'
000409                                'C' 'F' 'T'.
000410             88  PM-TYPE-STAT-2
000411                     VALUES ARE '2' '3' '5' '8'.
000412             88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
000413             88  PM-PENDING-STATUS
000414                                VALUES ARE '2' '4' '5' '8' '9'.
000415             88  PM-PENDING-ISSUE-STATUS
000416                                VALUES ARE '2' '5' '8'.
000417             88  PM-CANCEL-STATUS
000418                                VALUES ARE '4' '7' '9' 'C'.
000419         16  PM-CANCEL-CAUSE-CD            PIC X(3).
000420         16  PM-CANCEL-DT                  PIC XX.
000421         16  PM-REFUND-AMT                 PIC S9(5)V99  COMP-3.
000422         16  PM-CALC-REFUND-AMT            PIC S9(5)V99  COMP-3.
000423         16  PM-DECLINE-CD                 PIC X(3).
000424         16  PM-DECLINE-DT                 PIC XX.
000425         16  PM-LAST-LAPSE-DT              PIC XX.
000426         16  PM-LAST-REINSTATE-DT          PIC XX.
000427         16  PM-SECURITY-ACCESS-CODE       PIC X.
000428         16  PM-PREV-CONTROL-PRIMARY.
000429             20  PM-PREV-COMPANY-CD             PIC X.
000430             20  PM-PREV-CARRIER                PIC X.
000431             20  PM-PREV-GROUPING.
000432                 24  PM-PREV-GROUPING-PREFIX PIC X(3).
000433                 24  PM-PREV-GROUPING-PRIME     PIC X(3).
000434             20  PM-PREV-STATE                  PIC XX.
000435             20  PM-PREV-PRODUCER.
000436                 24  PM-PREV-PRODUCER-PREFIX PIC X(4).
000437                 24  PM-PREV-PRODUCER-PRIME     PIC X(6).
000438             20  PM-PREV-POLICY-EFF-DT          PIC XX.
000439             20  PM-PREV-REFERENCE-NUMBER.
000440                 24  PM-PREV-REFNO-PRIME        PIC X(18).
000441                 24  PM-PREV-REFNO-SFX          PIC XX.
000442         16  PM-ACTION-DT                  PIC XX.
000443         16  PM-ACTION-CODE                PIC X(3).
000444         16  PM-ACTION-DT-2                PIC XX.
000445         16  PM-ACTION-CODE-2              PIC X(3).
000446         16  PM-ACTION-DT-3                PIC XX.
000447         16  PM-ACTION-CODE-3              PIC X(3).
000448         16  PM-ACTION-DT-4                PIC XX.
000449         16  PM-ACTION-CODE-4              PIC X(3).
000450         16  PM-ACTION-DT-5                PIC XX.
000451         16  PM-ACTION-CODE-5              PIC X(3).
000452
000453         16  PM-KEY-CHANGE                 PIC X.
000454                 88  PM-NO-KEY-CHG      VALUES ARE ' ' 'N'.
000455                 88  PM-KEY-CHG              VALUE 'Y'.
000456         16  PM-KEY-CHANGE-DT              PIC XX.
000457
000458         16  PM-RTI-INDICATOR              PIC X.
000459         16  PM-REASON-CODE                PIC X(3).
000460         16  PM-IN-OUT-PROCESSING-IND      PIC X(1).
000461             88  PM-IN-OUT-PROCESSING      VALUE 'Y'.
000462             88  PM-NOT-IN-OUT-PROCESSING  VALUE SPACES.
000463
000464     12  FILLER                            PIC X(12).
000465
000466******************************************************************
000467*                 AGENT AND COMMISSION DATA                      *
000468******************************************************************
000469
000470     12  PM-COMMISSION-DATA.
000471         16  PM-REMIT-TO                   PIC S9(3) COMP-3.
000472         16  PM-COMM-CHANGE-SW             PIC X.
000473                 88  PM-COMMISSION-CHANGE     VALUE 'Y'.
000474         16  PM-AGENT-INFORMATION OCCURS     5 TIMES.
000475             20  PM-AGENT-NUMBER           PIC X(10).
000476             20  PM-AGENT-TYPE             PIC X.
000477                 88  PM-PRODUCER-LEVEL-AGENT
000478                                              VALUES ARE 'C' 'D'.
000479                 88  PM-AGENT-GROSS           VALUE 'C'.
000480                 88  PM-AGENT-REINS           VALUE 'R'.
000481                 88  PM-AGENT-GROSS-REINS     VALUE 'D'.
000482                 88  PM-OVERWRITE-GROSS       VALUE 'O'.
000483                 88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
000484                 88  PM-OVERWRITE-REINS       VALUE 'T'.
000485                 88  PM-REINS-ONLY            VALUE 'W'.
000486             20  PM-COMMISSION-BILL-PAID PIC X(1).
000487                 88  PM-GENERATE-BILL         VALUE 'B'.
000488                 88  PM-GENERATE-PAID         VALUE 'P'.
000489             20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
000490             20  PM-COMP-1ST-YEAR-TYPE     PIC X(1).
000491                 88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
000492                 88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
000493                 88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
000494             20  PM-RENEWAL-DATA.
000495                 24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
000496                     28  PM-RENEW-MONTHS     PIC S999    COMP-3.
000497                     28  PM-RENEW-COMMISSION
000498                                             PIC S99V999 COMP-3.
000499                     28  PM-RENEW-TYPE       PIC X(1).
000500                         88  PM-COMP-RENEW-PERCENT      VALUE '1'.
000501                         88  PM-COMP-RENEW-DOLLARS      VALUE '2'.
000502                         88  PM-COMP-RENEW-NOT-USED     VALUE '3'.
000503             20  PM-COMP-RECALC-FLAG       PIC X(1).
000504                 88  PM-BYPASS-RECALC         VALUE 'N'.
000505     12  FILLER                            PIC X(20).
000506******************************************************************
000507*             CUSTOMER DATA                                      *
000508******************************************************************
000509     12  PM-CUSTOMER-ID                    PIC X(20).
000510******************************************************************
000511     12  FILLER                            PIC X(43).
000512******************************************************************
      *<<((file: MPCPLCY))
000342     EJECT
000343*    COPY MPCPLAN.
      *>>((file: MPCPLAN))
000001******************************************************************
000002*                                                                *
000003*                            MPCPLAN                             *
000004*                            VMOD=1.012                          *
000005*                                                                *
000006*   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
000007*                                                                *
000008*   THIS COPYBOOK IS USED FOR THE ONLINE                         *
000009*   PLAN CODE MASTER FILE.                                       *
000010*                                                                *
000011*   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
000012*                                                                *
000013*   FILE TYPE = VSAM,KSDS                                        *
000014*   RECORD SIZE = 450  RECFORM = FIX                             *
000015*                                                                *
000016*   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
000017*       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
000018*                                                                *
000019*   LOG = NO                                                     *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021*                                                                *
000022*                                                                *
000023******************************************************************
000024
000025 01  PRODUCER-PLANS.
000026     12  PP-RECORD-ID                      PIC  X(02).
000027         88  VALID-PP-ID                      VALUE 'PP'.
000028
000029******************************************************************
000030*   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
000031******************************************************************
000032
000033     12  PP-CONTROL-PRIMARY.
000034         16  PP-PROD-PRIMARY.
000035             20  PP-COMPANY-CD             PIC  X(01).
000036             20  PP-CONTROL-A.
000037                 24  PP-CARRIER            PIC  X(01).
000038                 24  PP-GROUPING.
000039                     28  PP-GROUPING-PREFIX
000040                                           PIC  X(03).
000041                     28  PP-GROUPING-PRIME PIC  X(03).
000042                 24  PP-STATE              PIC  X(02).
000043                 24  PP-PRODUCER.
000044                     28  PP-PRODUCER-PREFIX
000045                                           PIC  X(04).
000046                     28  PP-PRODUCER-PRIME PIC  X(06).
000047         16  PP-PRODUCER-PLAN.
000048             20  PP-PLAN-CODE              PIC  X(02).
000049             20  PP-PLAN-REVISION          PIC  9(03).
000050     12  FILLER                            PIC  X(20).
000051
000052******************************************************************
000053*      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
000054******************************************************************
000055
000056     12  PP-CONTROL-BY-VAR-GRP.
000057         16  PP-COMPANY-CD-A1              PIC  X(01).
000058         16  PP-VG-CARRIER                 PIC  X(01).
000059         16  PP-VG-GROUPING                PIC  X(06).
000060         16  PP-VG-STATE                   PIC  X(02).
000061         16  PP-VG-PRODUCER                PIC  X(10).
000062         16  PP-VG-PLAN-CODE               PIC  X(02).
000063         16  PP-VG-PLAN-REVISION           PIC  X(03).
000064     12  FILLER                            PIC  X(20).
000065
000066******************************************************************
000067*                PRODUCER SECURITY DATA                          *
000068******************************************************************
000069
000070     12  PP-SECURITY-ACCESS-CODE           PIC  X(01).
000071     12  PP-POLICY-CNT                     PIC S9(07)    COMP-3.
000072
000073******************************************************************
000074*                FILE SYNCHRONIZATION DATA                       *
000075******************************************************************
000076
000077     12  PP-MAINT-INFORMATION.
000078         16  PP-LAST-MAINT-DATE            PIC  X(02).
000079         16  PP-LAST-MAINT-HHMMSS          PIC S9(07)    COMP-3.
000080         16  PP-LAST-MAINT-USER            PIC  X(04).
000081     12  FILLER                            PIC  X(10).
000082
000083******************************************************************
000084*                   CRITICAL FILE DATES                          *
000085******************************************************************
000086
000087     12  PP-PLAN-DATES.
000088         16  PP-PLAN-EFFECT-DATE           PIC  X(02).
000089         16  PP-PLAN-EXPIRE-DATE           PIC  X(02).
000090
000091     12  FILLER                            PIC  X(10).
000092
000093******************************************************************
000094*                GENERAL INFORMATION                             *
000095******************************************************************
000096
000097     12  PP-GENERAL-INFORMATION.
000098         16  PP-ALPHA-SEARCH-SW            PIC  X(01).
000099             88  PP-MIB-ALPHA-ALL              VALUE '1'.
000100             88  PP-MIB-ALPHA-NONE             VALUE '2'.
000101             88  PP-MIB-ALPHA-EXCEEDED         VALUE '3'.
000102             88  PP-CLIENT-ALPHA-ALL           VALUE 'A'.
000103             88  PP-CLIENT-ALPHA-NONE          VALUE 'B'.
000104             88  PP-CLIENT-ALPHA-EXCEEDED      VALUE 'C'.
000105             88  PP-BOTH-ALPHA-ALL             VALUE 'X'.
000106             88  PP-BOTH-ALPHA-NONE            VALUE 'Y'.
000107             88  PP-BOTH-ALPHA-EXCEEDED        VALUE 'Z'.
000108             88  PP-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
000109                                                     'A' 'B' 'C'
000110                                                     'X' 'Y' 'Z'.
000111         16  PP-BENEFIT-TYPE               PIC  X(01).
000112             88  PP-BENEFIT-IS-LEVEL            VALUE '1'.
000113             88  PP-BENEFIT-REDUCES             VALUE '2'.
000114         16  PP-DAYS-TO-1ST-NOTICE         PIC  9(02).
000115         16  PP-DAYS-TO-2ND-NOTICE         PIC  9(02).
000116         16  PP-DAYS-TO-3RD-NOTICE         PIC  9(02).
000117         16  PP-DAYS-TO-4TH-NOTICE         PIC  9(02).
000118         16  PP-EFF-DT-RULE-SW             PIC  X(01).
000119             88  PP-EFF-DT-ENTER               VALUE 'E'.
000120             88  PP-EFF-DT-MONTH               VALUE 'M'.
000121             88  PP-EFF-DT-QTR                 VALUE 'Q'.
000122             88  PP-EFF-DT-SEMI                VALUE 'S'.
000123             88  PP-EFF-DT-ANN                 VALUE 'A'.
000124         16  PP-FREE-EXAM-DAYS             PIC S9(03)   COMP-3.
000125         16  PP-GRACE-PERIOD               PIC S9(03)   COMP-3.
000126         16  PP-HEALTH-QUESTIONS           PIC  9(01).
000127         16  PP-NUMBER-LAPSE-NOTICES       PIC S9(03)   COMP-3.
000128         16  PP-MIB-SEARCH-SW              PIC  X(01).
000129             88  PP-MIB-SEARCH-ALL             VALUE '1'.
000130             88  PP-MIB-SEARCH-NONE            VALUE '2'.
000131             88  PP-MIB-SEARCH-EXCEEDED        VALUE '3'.
000132             88  PP-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
000133         16  PP-PLAN-ABBREV                PIC  X(03).
000134         16  PP-PLAN-AGES.
000135             20  PP-MINIMUM-AGE            PIC S9(03)   COMP-3.
000136             20  PP-MAXIMUM-AGE            PIC S9(03)   COMP-3.
000137             20  PP-MAXIMUM-ATTAIN-AGE     PIC S9(03)   COMP-3.
000138         16  PP-PLAN-BENEFITS.
000139             20  PP-CLAIM-CAP              PIC S9(07)V99 COMP-3.
000140             20  PP-MINIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
000141             20  PP-MAXIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
000142             20  PP-MAXIMUM-MONTHLY-BENEFIT
000143                                           PIC S9(07)V99 COMP-3.
000144         16  PP-PLAN-DESCRIPTION           PIC  X(10).
000145         16  PP-POLICY-FEE                 PIC S9(03)V9(02)
000146                                                        COMP-3.
000147         16  PP-PLAN-IND-GRP               PIC  X(01).
000148         16  PP-PLAN-SNGL-JNT              PIC  X(01).
000149             88  PP-COMBINED-PLAN             VALUE 'C'.
000150             88  PP-JNT-PLAN                  VALUE 'J'.
000151             88  PP-SNGL-PLAN                 VALUE 'S'.
000152         16  PP-PLAN-TERMS.
000153             20  PP-MINIMUM-TERM           PIC S9(03)   COMP-3.
000154             20  PP-MAXIMUM-TERM           PIC S9(03)   COMP-3.
000155         16  PP-PLAN-TYPE                  PIC  X(01).
000156             88  PP-AH-MORT-PLAN              VALUE 'A'.
000157             88  PP-AD-D-MORT-PLAN            VALUE 'E'.
000158             88  PP-DISMEM-MORT-PLAN          VALUE 'D'.
000159             88  PP-LIFE-MORT-PLAN            VALUE 'L'.
000160         16  PP-PREMIUM-TOLERANCES.
000161             20  PP-PREM-TOLERANCE         PIC S9(03)   COMP-3.
000162             20  PP-PREM-TOLERANCE-PCT     PIC SV9(03)  COMP-3.
000163         16  PP-RATE-CODE                  PIC  X(05).
000164         16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03)   COMP-3.
000165         16  PP-REPLACEMENT-LAW-SW         PIC  X(01).
000166             88  PP-NO-REPLACE                VALUE '1'.
000167             88  PP-REPLACE-APPLIES           VALUE '2'.
000168             88  PP-VALID-REPLACEMENT-LAW     VALUE '1' '2'.
000169         16  PP-RETRO-RETENTION            PIC S9V9(04) COMP-3.
000170         16  PP-RERATE-CNTL                PIC  X(01).
000171             88  PP-RERATE-WITH-ISSUE-AGE       VALUE '1'.
000172             88  PP-RERATE-WITH-CURRENT-AGE     VALUE '2'.
000173             88  PP-DO-NOT-RERATE               VALUE '3' ' '.
000174             88  PP-AUTO-RECALC                 VALUE '4'.
000175         16  PP-SEX-RATING                 PIC  X(01).
000176             88  PP-NOT-SEX-RATED             VALUE '1'.
000177             88  PP-SEX-RATED                 VALUE '2'.
000178         16  PP-SUBSTANDARD-DATA.
000179             20  PP-SUBSTANDARD-PERCENT    PIC S9(01)V9(04).
000180             20  PP-SUBSTANDARD-TYPE       PIC  X(01).
000181                 88  PP-PCT-OF-BENEFIT        VALUE '1'.
000182                 88  PP-PCT-OF-PREMIUM        VALUE '2'.
000183                 88  PP-NOT-APPLICABLE        VALUE '3'.
000184         16  PP-YEARS-TO-NEXT-RERATE       PIC  9(02).
000185         16  PP-DEPENDANT-COVERAGE         PIC  X(01).
000186             88  PP-DEP-COVERED               VALUE 'Y'.
000187             88  PP-DEP-NOT-COVERED           VALUE 'N' ' '.
000188         16  PP-REFUND-CALC                PIC  X(01).
000189             88  PP-RFND-MP-REFUND     VALUES ARE ' ' LOW-VALUES.
000190             88  PP-RFND-BY-R78               VALUE '1'.
000191             88  PP-RFND-BY-PRO-RATA          VALUE '2'.
000192             88  PP-RFND-AS-CALIF             VALUE '3'.
000193             88  PP-RFND-AS-TEXAS             VALUE '4'.
000194             88  PP-RFND-IS-NET-PAY           VALUE '5'.
000195             88  PP-RFND-ANTICIPATION         VALUE '6'.
000196             88  PP-RFND-MEAN                 VALUE '8'.
000197             88  PP-VALID-REFUND       VALUES ARE ' ' '1' '2' '3'
000198                                                  '4' '5' '6' '8'
000199                                                  LOW-VALUES.
000200         16  PP-ALT-RATE-CODE              PIC  X(05).
000201
000202     12  FILLER                            PIC  X(39).
000203
000204******************************************************************
000205*                     PLAN FORMS AND LETTERS                     *
000206******************************************************************
000207
000208     12  PP-PLAN-MASTER-FORMS.
000209         16  PP-POLICY-FORM                PIC  X(12).
000210         16  PP-MASTER-APPLICATION         PIC  X(12).
000211         16  PP-MASTER-POLICY              PIC  X(12).
000212     12  PP-DELINQUENCY-NOTICE-FORMS.
000213         16  PP-1ST-NOTICE-FORM            PIC  X(04).
000214         16  PP-2ND-NOTICE-FORM            PIC  X(04).
000215         16  PP-3RD-NOTICE-FORM            PIC  X(04).
000216         16  PP-4TH-NOTICE-FORM            PIC  X(04).
000217     12  FILLER                            PIC  X(32).
000218     12  PP-TERMINATION-FORM               PIC  X(04).
000219     12  FILLER                            PIC  X(08).
000220     12  PP-ISSUE-LETTER                   PIC  X(04).
000221
000222     12  FILLER                            PIC  X(80).
000223******************************************************************
      *<<((file: MPCPLAN))
000344     EJECT
000345*    COPY ELCRETR.
      *>>((file: ELCRETR))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCRETR.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
000009*                                                                *
000010*   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
000011*   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
000012*   ****      DUPLICATED IN THE OTHER.                           *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350  RECFORM = FIXED                           *
000016*                                                                *
000017*   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
000018*       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
000019*       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000020*       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
000021*       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
000022*                                                 RKP=75,LEN=21  *
000023*                                                                *
000024*   LOG = YES                                                    *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  RETRIEVE-MASTER.
000028     12  RL-RECORD-ID                PIC XX.
000029         88  VALID-RL-ID         VALUE 'RL'.
000030
000031     12  RL-CONTROL-PRIMARY.
000032         16  RL-COMPANY-CD           PIC X.
000033         16  RL-CARRIER              PIC X.
000034         16  RL-CLAIM-NO             PIC X(7).
000035         16  RL-CERT-NO.
000036             20  RL-CERT-PRIME       PIC X(10).
000037             20  RL-CERT-SFX         PIC X.
000038
000039     12  RL-CONTROL-BY-NAME.
000040         16  RL-COMPANY-CD-A1        PIC X.
000041         16  RL-INSURED-LAST-NAME    PIC X(15).
000042         16  RL-INSURED-NAME.
000043             20  RL-INSURED-1ST-NAME PIC X(12).
000044             20  RL-INSURED-MID-INIT PIC X.
000045
000046     12  RL-CONTROL-BY-SSN.
000047         16  RL-COMPANY-CD-A2        PIC X.
000048         16  RL-SOC-SEC-NO.
000049             20  RL-SSN-STATE        PIC XX.
000050             20  RL-SSN-ACCOUNT      PIC X(6).
000051             20  RL-SSN-LN3          PIC X(3).
000052
000053     12  RL-CONTROL-BY-CERT-NO.
000054         16  RL-COMPANY-CD-A4        PIC X.
000055         16  RL-CERT-NO-A4.
000056             20  RL-CERT-A4-PRIME    PIC X(10).
000057             20  RL-CERT-A4-SFX      PIC X.
000058
000059     12  RL-CONTROL-BY-CCN.
000060         16  RL-COMPANY-CD-A5        PIC X.
000061         16  RL-CCN-A5.
000062             20  RL-CCN-NO.
000063                 24  RL-CCN-PREFIX-A5 PIC X(4).
000064                 24  RL-CCN-PRIME-A5 PIC X(12).
000065             20  RL-CCN-FILLER-A5    PIC X(4).
000066
000067     12  RL-INSURED-PROFILE-DATA.
000068         16  RL-INSURED-BIRTH-DT     PIC XX.
000069         16  RL-INSURED-SEX-CD       PIC X.
000070             88  RL-INSURED-IS-MALE     VALUE 'M'.
000071             88  RL-INSURED-IS-FEMALE   VALUE 'F'.
000072             88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
000073         16  RL-INSURED-OCC-CD       PIC X(6).
000074         16  FILLER                  PIC X(5).
000075
000076     12  RL-PROCESSING-INFO.
000077         16  RL-PROCESSOR-ID         PIC X(4).
000078         16  RL-CLAIM-STATUS         PIC X.
000079             88  RL-CLAIM-IS-OPEN       VALUE 'O'.
000080             88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
000081         16  RL-CLAIM-TYPE           PIC X.
000082*            88  RL-AH-CLAIM            VALUE 'A'.
000083*            88  RL-LIFE-CLAIM          VALUE 'L'.
000084*            88  RL-PROPERTY-CLAIM      VALUE 'P'.
000085*            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
000086         16  RL-CLAIM-PREM-TYPE      PIC X.
000087             88  RL-SINGLE-PREMIUM         VALUE '1'.
000088             88  RL-O-B-COVERAGE           VALUE '2'.
000089             88  RL-OPEN-END-COVERAGE      VALUE '3'.
000090         16  RL-INCURRED-DT          PIC XX.
000091         16  RL-REPORTED-DT          PIC XX.
000092         16  RL-FILE-ESTABLISH-DT    PIC XX.
000093         16  RL-EST-END-OF-DISAB-DT  PIC XX.
000094         16  RL-LAST-PMT-DT          PIC XX.
000095         16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000096         16  RL-PAID-THRU-DT         PIC XX.
000097         16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000098         16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000099         16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000100         16  RL-PMT-CALC-METHOD      PIC X.
000101             88  RL-360-DAY-YR          VALUE '1'.
000102             88  RL-365-DAY-YR          VALUE '2'.
000103             88  RL-FULL-MONTHS         VALUE '3'.
000104         16  RL-CAUSE-CD             PIC X(6).
000105
000106         16  RL-PRIME-CERT-NO.
000107             20  RL-PRIME-CERT-PRIME PIC X(10).
000108             20  RL-PRIME-CERT-SFX   PIC X.
000109
000110         16  RL-SYSTEM-IDENTIFIER    PIC XX.
000111             88  RL-CREDIT-CLAIM        VALUE 'CR'.
000112             88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
000113
000114         16  RL-MICROFILM-NO         PIC X(10).
000115         16  RL-PROG-FORM-TYPE       PIC X.
000116         16  RL-LAST-ADD-ON-DT       PIC XX.
000117
000118         16  RL-LAST-REOPEN-DT       PIC XX.
000119         16  RL-LAST-CLOSE-DT        PIC XX.
000120         16  RL-LAST-CLOSE-REASON    PIC X.
000121             88  RL-FINAL-PAID          VALUE '1'.
000122             88  RL-CLAIM-DENIED        VALUE '2'.
000123             88  RL-AUTO-CLOSE          VALUE '3'.
000124             88  RL-MANUAL-CLOSE        VALUE '4'.
000125         16  RL-ASSOC-CERT-SEQU      PIC S99.
000126         16  RL-ASSOC-CERT-TOTAL     PIC S99.
000127         16  RL-CLAIM-PAYMENT-STATUS PIC 9.
000128             88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
000129         16  FILLER                  PIC X(5).
000130
000131     12  RL-CERTIFICATE-DATA.
000132         16  RL-CERT-ORIGIN          PIC X.
000133             88  RL-CERT-WAS-ONLINE     VALUE '1'.
000134             88  RL-CERT-WAS-CREATED    VALUE '2'.
000135             88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
000136         16  RL-CERT-KEY-DATA.
000137             20  RL-CERT-CARRIER     PIC X.
000138             20  RL-CERT-GROUPING    PIC X(6).
000139             20  RL-CERT-STATE       PIC XX.
000140             20  RL-CERT-ACCOUNT.
000141                 24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
000142                 24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
000143             20  RL-CERT-EFF-DT      PIC XX.
000144
000145     12  RL-STATUS-CONTROLS.
000146         16  RL-PRIORITY-CD          PIC X.
000147             88  RL-HIGHEST-PRIORITY    VALUE '9'.
000148         16  RL-SUPV-ATTN-CD         PIC X.
000149             88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
000150             88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
000151         16  RL-PURGED-DT            PIC XX.
000152         16  RL-RESTORED-DT          PIC XX.
000153         16  RL-NEXT-AUTO-PAY-DT     PIC XX.
000154         16  RL-NEXT-RESEND-DT       PIC XX.
000155         16  RL-NEXT-FOLLOWUP-DT     PIC XX.
000156         16  FILLER                  PIC XX.
000157         16  RL-LAST-MAINT-DT        PIC XX.
000158         16  RL-LAST-MAINT-USER      PIC X(4).
000159         16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000160         16  RL-LAST-MAINT-TYPE      PIC X.
000161             88  RL-CLAIM-SET-UP           VALUE ' '.
000162             88  RL-PAYMENT-MADE           VALUE '1'.
000163             88  RL-LETTER-SENT            VALUE '2'.
000164             88  RL-MASTER-WAS-ALTERED     VALUE '3'.
000165             88  RL-MASTER-WAS-RESTORED    VALUE '4'.
000166             88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
000167             88  RL-FILE-CONVERTED         VALUE '6'.
000168         16  RL-RELATED-CLAIM-NO     PIC X(7).
000169         16  RL-HISTORY-ARCHIVE-DT   PIC XX.
000170         16  RL-BENEFICIARY          PIC X(10).
000171         16  RL-FILE-ESTABLISHED-BY  PIC X(4).
000172         16  FILLER                  PIC X(6).
000173
000174     12  RL-TRAILER-CONTROLS.
000175         16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000176             88  RL-1ST-TRL-AVAIL       VALUE +4095.
000177             88  RL-LAST-TRL-AVAIL      VALUE +100.
000178             88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
000179         16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000180         16  FILLER                  PIC XX.
000181         16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000182         16  RL-ADDRESS-TRAILER-CNT.
000183             20  RL-INSURED-ADDR-CNT  PIC S9.
000184                 88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
000185             20  RL-ACCOUNT-ADDR-CNT  PIC S9.
000186                 88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
000187             20  RL-BENIF-ADDR-CNT    PIC S9.
000188                 88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
000189             20  RL-EMPLOYER-ADDR-CNT PIC S9.
000190                 88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
000191             20  RL-DOCTOR-ADDR-CNT   PIC S9.
000192                 88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
000193             20  RL-OTHER-1-ADDR-CNT  PIC S9.
000194                 88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
000195             20  RL-OTHER-2-ADDR-CNT  PIC S9.
000196                 88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
000197
000198     12  RL-CV-REFERENCE-NO.
000199         16  RL-CV-REFNO-PRIME       PIC X(18).
000200         16  RL-CV-REFNO-SFX         PIC XX.
000201
000202     12  RL-FILE-LOCATION            PIC X(4).
000203
000204     12  RL-PROCESS-ERRORS.
000205         16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000206             88  RL-NO-FATAL-ERRORS     VALUE ZERO.
000207         16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000208             88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
000209
000210     12  RL-PRODUCT-CD               PIC X.
000211
000212     12  RL-CURRENT-KEY-DATA.
000213         16  RL-CURRENT-CARRIER      PIC X.
000214         16  RL-CURRENT-GROUPING     PIC X(6).
000215         16  RL-CURRENT-STATE        PIC XX.
000216         16  RL-CURRENT-ACCOUNT      PIC X(10).
000217
000218     12  RL-ASSOCIATES               PIC X.
000219         88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
000220         88  RL-ASSOC-INTERFACE         VALUE 'I'.
000221         88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000222         88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
000223
000224     12  RL-ACTIVITY-CODE            PIC 99.
000225     12  RL-ACTIVITY-MAINT-DT        PIC XX.
000226     12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
000227
000228     12  RL-LAPSE-REPORT-CODE        PIC 9.
000229     12  RL-LAG-REPORT-CODE          PIC 9.
000230     12  RL-LOAN-TYPE                PIC XX.
000231     12  RL-LEGAL-STATE              PIC XX.
000232
000233     12  FILLER                      PIC X(5).
      *<<((file: ELCRETR))
000346     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL160' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000347 VCOBOL-DUMMY-PROCEDURE.
000348
000349     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000350     MOVE '5'                   TO DC-OPTION-CODE.
000351     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
000352     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000353     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000354
000355     IF EIBCALEN = ZERO
000356         GO TO 8800-UNAUTHORIZED-ACCESS.
000357
000358     
      * EXEC CICS HANDLE CONDITION
000359*        PGMIDERR (8820-XCTL-ERROR)
000360*        ERROR    (9990-ABEND)
000361*    END-EXEC.
      *    MOVE '"$L.                  ! " #00006529' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303036353239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000362
000363     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000364
000365     MOVE 2                      TO EMI-NUMBER-OF-LINES.
000366
000367     MOVE THIS-TRAN              TO TRANS-ID.
000368
000369     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000370         MOVE LOW-VALUES TO EL160AO
000371         MOVE ER-0008    TO EMI-ERROR
000372         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000373         MOVE -1         TO CARRSL
000374         GO TO 8110-SEND-DATA.
000375
000376     IF THIS-PGM NOT = PI-CALLING-PROGRAM
000377         MOVE LOW-VALUES TO EL160AO
000378         PERFORM 0500-BUILD-TS-KEY THRU 0510-EXIT
000379         GO TO 0100-UPDATE-PI.
000380
000381     IF EIBAID = DFHCLEAR
000382         GO TO 8200-RETURN-PRIOR.
000383
000384     IF PI-PROCESSOR-ID = 'LGXX'
000385         NEXT SENTENCE
000386     ELSE
000387         
      * EXEC CICS READQ TS
000388*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000389*            INTO    (SECURITY-CONTROL)
000390*            LENGTH  (SC-COMM-LENGTH)
000391*            ITEM    (SC-ITEM)
000392*        END-EXEC
      *    MOVE '*$II   L              ''   #00006558' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036353538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000393         MOVE SC-CLAIMS-DISPLAY (3)    TO  PI-DISPLAY-CAP
000394         MOVE SC-CLAIMS-UPDATE  (3)    TO  PI-MODIFY-CAP
000395         IF NOT DISPLAY-CAP
000396             MOVE 'READ'               TO  SM-READ
000397             PERFORM 9995-SECURITY-VIOLATION
000398             MOVE ER-0070              TO  EMI-ERROR
000399             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000400             MOVE -1                   TO CARRSL
000401             GO TO 8100-SEND-MAP.
000402
000403     
      * EXEC CICS RECEIVE
000404*        MAP    ('EL160A')
000405*        MAPSET ('EL160S')
000406*    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00006574' TO DFHEIV0
           MOVE X'382254202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036353734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000407
000408     IF PFKEYL > ZERO
000409         PERFORM 0200-TRANS-PF THRU 0210-EXIT.
000410
000411     IF SCREEN-ERROR
000412         MOVE ER-0008 TO EMI-ERROR
000413         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000414         MOVE -1 TO CARRSL
000415         GO TO 8110-SEND-DATA.
000416
000417     IF EIBAID = DFHPF12
000418         GO TO 8300-GET-HELP.
000419
000420     IF EIBAID = DFHPF23
000421         GO TO 8810-PF23-ENTERED.
000422
000423     IF EIBAID = DFHPF24
000424         GO TO 8400-RETURN-MASTER.
000425
000426     IF EIBAID NOT = DFHENTER
000427         MOVE ER-0029 TO EMI-ERROR
000428         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000429         MOVE -1 TO CARRSL
000430         GO TO 8110-SEND-DATA.
000431
000432     PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
000433
000434     IF SCREEN-HAS-ERRORS
000435         GO TO 8110-SEND-DATA.
000436
000437     IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
000438         MOVE 2500               TO  MAX-TS-PAGES.
000439
000440     IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
000441         MOVE 2500               TO  MAX-TS-PAGES.
000442
000443     PERFORM 2000-BUILD-TS THRU 2010-EXIT.
000444
000445     MOVE COUNT-1 TO PI-TS-COUNT-1.
000446
000447     IF PI-TS-COUNT-1 = ZEROES
000448         MOVE ER-0142 TO EMI-ERROR
000449         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000450         MOVE -1 TO CARRSL
000451         GO TO 8110-SEND-DATA.
000452
000453     PERFORM 5240-WRITE-TS-160A THRU 5250-EXIT.
000454
000455     MOVE XCTL-EL1602 TO CALL-PGM.
000456
000457     GO TO 9200-XCTL.
000458
000459     EJECT
000460
000461 0100-UPDATE-PI.
000462     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000463         GO TO 0110-UPDATE-UP.
000464
000465     MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6.
000466     MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5.
000467     MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4.
000468     MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3.
000469     MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2.
000470     MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1.
000471     MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM.
000472     MOVE THIS-PGM             TO PI-CALLING-PROGRAM.
000473
000474     PERFORM 0430-DELETE-TS THRU 0450-EXIT.
000475
000476     MOVE -1 TO CARRSL.
000477     GO TO 8100-SEND-MAP.
000478
000479 0110-UPDATE-UP.
000480     MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.
000481     MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM.
000482     MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1.
000483     MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2.
000484     MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3.
000485     MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4.
000486     MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5.
000487     MOVE SPACES               TO PI-SAVED-PROGRAM-6.
000488
000489     GO TO 0400-FIND-SCREEN.
000490
000491 0200-TRANS-PF.
000492     IF EIBAID NOT = DFHENTER
000493         MOVE 'X' TO ERROR-SWITCH
000494         GO TO 0210-EXIT.
000495
000496     IF PFKEYI NOT NUMERIC
000497         MOVE 'X' TO ERROR-SWITCH
000498         GO TO 0210-EXIT.
000499
000500     MOVE PFKEYI TO CHECK-PFKEYS.
000501
000502     IF CHECK-PFKEYS < 1 OR > 24
000503         MOVE 'X' TO ERROR-SWITCH
000504         GO TO 0210-EXIT.
000505
000506     MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
000507
000508 0210-EXIT.
000509     EXIT.
000510
000511 0400-FIND-SCREEN.
000512     
      * EXEC CICS HANDLE CONDITION
000513*        ITEMERR (0410-TS-NOTFND)
000514*        QIDERR  (0420-FIND-SCREEN-EXIT)
000515*    END-EXEC.
      *    MOVE '"$<N                  ! # #00006683' TO DFHEIV0
           MOVE X'22243C4E2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303036363833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000516
000517     
      * EXEC CICS READQ TS
000518*        QUEUE  (PI-EL160-KEY)
000519*        INTO   (EL160AI)
000520*        LENGTH (EL160A-LENGTH)
000521*    END-EXEC.
      *    MOVE '*$I    L              ''   #00006688' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036363838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 EL160AI, 
                 EL160A-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000522
000523     PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
000524
000525     PERFORM 0430-DELETE-TS THRU 0450-EXIT.
000526
000527     GO TO 0420-FIND-SCREEN-EXIT.
000528
000529 0410-TS-NOTFND.
000530     MOVE ER-0192                TO EMI-ERROR.
000531     MOVE -1                     TO CARRSL.
000532     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000533
000534 0420-FIND-SCREEN-EXIT.
000535     MOVE -1 TO CARRSL.
000536     GO TO 8100-SEND-MAP.
000537
000538 0430-DELETE-TS.
000539     
      * EXEC CICS HANDLE CONDITION
000540*        QIDERR (0440-DELETE-CONT)
000541*    END-EXEC.
      *    MOVE '"$N                   ! $ #00006710' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303036373130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000542
000543     
      * EXEC CICS DELETEQ TS
000544*        QUEUE (PI-EL1602-KEY)
000545*    END-EXEC.
      *    MOVE '*&                    #   #00006714' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036373134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000546
000547 0440-DELETE-CONT.
000548     
      * EXEC CICS HANDLE CONDITION
000549*        QIDERR (0450-EXIT)
000550*    END-EXEC.
      *    MOVE '"$N                   ! % #00006719' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303036373139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000551
000552     
      * EXEC CICS DELETEQ TS
000553*        QUEUE (PI-EL160-KEY)
000554*    END-EXEC.
      *    MOVE '*&                    #   #00006723' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036373233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000555
000556 0450-EXIT.
000557     EXIT.
000558
000559 0500-BUILD-TS-KEY.
000560     MOVE ZERO      TO COUNT-1.
000561     MOVE EIBTRMID  TO HOLD-TERM.
000562     MOVE LIT-MAP   TO KEY-QUAL.
000563     MOVE HOLD-KEY  TO PI-EL160-KEY.
000564     MOVE LIT-MAP-2 TO KEY-QUAL.
000565     MOVE HOLD-KEY  TO PI-EL1602-KEY.
000566
000567 0510-EXIT.
000568     EXIT.
000569
000570     EJECT
000571 1000-EDIT-SCREEN.
000572     MOVE SPACES TO CNTL-WORK-AREA.
000573
000574     IF CFILEIDL > ZEROS
000575         MOVE CFILEIDI           TO W-VALID-FILE-IND
000576         IF W-VALID-FILE
000577             IF W-RETRIEVE
000578                 MOVE 'ELRETR'   TO W-FILE-ID
000579                 MOVE 'R'        TO PI-FILE-ID-IND
000580             ELSE
000581                 MOVE 'ELMSTR'   TO W-FILE-ID
000582                 MOVE 'M'        TO PI-FILE-ID-IND
000583         ELSE
000584             MOVE ER-0970        TO EMI-ERROR
000585             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000586             MOVE AL-UABON       TO CFILEIDA
000587             MOVE -1             TO CFILEIDL
000588             GO TO 8110-SEND-DATA
000589     ELSE
000590         MOVE 'M'                TO CFILEIDO
000591                                    PI-FILE-ID-IND
000592         MOVE +1                 TO CFILEIDL
000593         MOVE 'ELMSTR'           TO W-FILE-ID.
000594
000595     IF ASEXI > LOW-VALUES
000596         MOVE ASEXI TO WS-SEX-SELECTION
000597         IF WS-SEX-SELECTION = 'M' OR 'F'
000598             MOVE AL-UANON       TO ASEXA
000599         ELSE
000600            MOVE ER-0219         TO EMI-ERROR
000601            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000602            MOVE AL-UABON        TO ASEXA
000603            MOVE -1              TO ASEXL
000604     ELSE
000605        MOVE AL-UANOF            TO ASEXA.
000606
000607     IF CARRSI > LOW-VALUES
000608         MOVE CARRSI   TO CARRIER-CNTL
000609         MOVE AL-UANON TO CARRSA
000610     ELSE
000611         MOVE AL-UANOF TO CARRSA
000612         MOVE SPACES   TO CARRIER-CNTL.
000613
000614     MOVE SPACE TO ERROR-SWITCH.
000615
000616     IF INCLSI > SPACES
000617         MOVE INCLSI TO TEST-DATE
000618         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000619         IF NO-CONVERSION-ERROR
000620            MOVE DC-GREG-DATE-1-EDIT TO INCLSI
000621            MOVE DC-BIN-DATE-1       TO INC-DATE-LOW-CNTL
000622            MOVE AL-UANON            TO INCLSA
000623         ELSE
000624            MOVE -1                  TO INCLSL
000625            MOVE AL-UABON            TO INCLSA
000626            MOVE LOW-VALUES          TO INC-DATE-LOW-CNTL
000627     ELSE
000628         MOVE AL-UANOF               TO INCLSA
000629         MOVE LOW-VALUES             TO INC-DATE-LOW-CNTL.
000630
000631     IF INCHSI > SPACES
000632         MOVE INCHSI TO TEST-DATE
000633         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000634         IF NO-CONVERSION-ERROR
000635            MOVE DC-GREG-DATE-1-EDIT TO INCHSI
000636            MOVE DC-BIN-DATE-1       TO INC-DATE-HIGH-CNTL
000637            MOVE AL-UANON            TO INCHSA
000638         ELSE
000639            MOVE -1                  TO INCHSL
000640            MOVE AL-UABON            TO INCHSA
000641            MOVE HIGH-VALUES         TO INC-DATE-HIGH-CNTL
000642     ELSE
000643         MOVE AL-UANOF               TO INCHSA
000644         MOVE HIGH-VALUES            TO INC-DATE-HIGH-CNTL.
000645
000646     IF INC-DATE-HIGH-CNTL < INC-DATE-LOW-CNTL
000647         MOVE 'X'      TO BUILD-SWITCH
000648         MOVE ER-0308  TO EMI-ERROR
000649         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000650         MOVE -1       TO INCLSL
000651         MOVE AL-UABON TO INCLSA INCHSA.
000652
000653     IF GRPSI > LOW-VALUES
000654         MOVE GRPSI    TO GROUP-CNTL
000655         MOVE AL-UANON TO GRPSA
000656     ELSE
000657         MOVE AL-UANOF TO GRPSA.
000658
000659     MOVE SPACE TO ERROR-SWITCH.
000660
000661     IF PMTDLSI > SPACES
000662         MOVE PMTDLSI TO TEST-DATE
000663         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000664         IF NO-CONVERSION-ERROR
000665            MOVE DC-GREG-DATE-1-EDIT TO PMTDLSI
000666            MOVE DC-BIN-DATE-1       TO LST-PMT-LOW-CNTL
000667            MOVE AL-UANON            TO PMTDLSA
000668         ELSE
000669            MOVE -1                  TO PMTDLSL
000670            MOVE AL-UABON            TO PMTDLSA
000671            MOVE LOW-VALUES          TO LST-PMT-LOW-CNTL
000672     ELSE
000673         MOVE AL-UANOF               TO PMTDLSA
000674         MOVE LOW-VALUES             TO LST-PMT-LOW-CNTL.
000675
000676     IF PMTDHSI > SPACES
000677         MOVE PMTDHSI TO TEST-DATE
000678         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000679         IF NO-CONVERSION-ERROR
000680            MOVE DC-GREG-DATE-1-EDIT TO PMTDHSI
000681            MOVE DC-BIN-DATE-1       TO LST-PMT-HIGH-CNTL
000682            MOVE AL-UANON            TO PMTDHSA
000683         ELSE
000684            MOVE -1                  TO PMTDHSL
000685            MOVE AL-UABON            TO PMTDHSA
000686            MOVE HIGH-VALUES         TO LST-PMT-HIGH-CNTL
000687     ELSE
000688         MOVE AL-UANOF               TO PMTDHSA
000689         MOVE HIGH-VALUES            TO LST-PMT-HIGH-CNTL.
000690
000691     IF LST-PMT-HIGH-CNTL < LST-PMT-LOW-CNTL
000692         MOVE 'X'      TO BUILD-SWITCH
000693         MOVE ER-0308  TO EMI-ERROR
000694         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000695         MOVE -1       TO PMTDLSL
000696         MOVE AL-UABON TO PMTDLSA PMTDHSA.
000697
000698     IF STATESI > LOW-VALUES
000699         MOVE STATESI  TO STATE-CNTL
000700         MOVE AL-UANON TO STATESA
000701     ELSE
000702         MOVE AL-UANOF TO STATESA.
000703
000704     MOVE SPACE TO ERROR-SWITCH.
000705
000706     IF OPENLSI = LOW-VALUES
000707         MOVE AL-UANOF     TO OPENLSA
000708         MOVE ZEROS        TO MO-OPEN-LOW-CNTL
000709     ELSE
000710         IF OPENLSI NOT NUMERIC
000711             MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
000712             MOVE ZEROS    TO MO-OPEN-LOW-CNTL
000713         ELSE
000714             MOVE OPENLSI  TO MO-OPEN-LOW-CNTL
000715             MOVE AL-UANON TO OPENLSA.
000716
000717     IF SCREEN-ERROR
000718         MOVE -1       TO OPENLSL
000719         MOVE AL-UABON TO OPENLSA
000720         MOVE ER-0306  TO EMI-ERROR
000721         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000722
000723     MOVE SPACE TO ERROR-SWITCH.
000724
000725     IF OPENHSI = LOW-VALUES
000726         MOVE AL-UANOF     TO OPENHSA
000727         MOVE 9999         TO MO-OPEN-HIGH-CNTL
000728     ELSE
000729         IF OPENHSI NOT NUMERIC
000730             MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
000731             MOVE 9999 TO MO-OPEN-HIGH-CNTL
000732         ELSE
000733             MOVE OPENHSI  TO MO-OPEN-HIGH-CNTL
000734             MOVE AL-UANON TO OPENHSA.
000735
000736     IF SCREEN-ERROR
000737         MOVE -1       TO OPENHSL
000738         MOVE AL-UABON TO OPENHSA
000739         MOVE ER-0306  TO EMI-ERROR
000740         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000741
000742     IF MO-OPEN-HIGH-CNTL < MO-OPEN-LOW-CNTL
000743         MOVE 'X'      TO BUILD-SWITCH
000744         MOVE ER-0308  TO EMI-ERROR
000745         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000746         MOVE -1       TO OPENLSL
000747         MOVE AL-UABON TO OPENLSA OPENHSA.
000748
000749     IF ACCTSI > LOW-VALUES
000750         MOVE ACCTSI   TO ACCOUNT-CNTL
000751         MOVE AL-UANON TO ACCTSA
000752     ELSE
000753         MOVE AL-UANOF TO ACCTSA.
000754
000755     MOVE SPACE TO ERROR-SWITCH.
000756
000757     IF AMTLSI > LOW-VALUES
000758         MOVE AMTLSI TO TEST-AMT
000759         PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
000760         MOVE -1       TO AMTLSL
000761     ELSE
000762         MOVE AL-UANOF TO AMTLSA
000763         MOVE ZEROS  TO AMT-PAID-LOW-CNTL.
000764
000765     IF SCREEN-ERROR
000766         MOVE -1           TO AMTLSL
000767         MOVE AL-UABON     TO AMTLSA
000768         MOVE ZEROS        TO AMT-PAID-LOW-CNTL
000769     ELSE
000770         IF AMTLSI > LOW-VALUES
000771             MOVE AMT-BIF  TO AMT-PAID-LOW-CNTL AMTLSO
000772             MOVE AL-UANON TO AMTLSA.
000773
000774     MOVE SPACE TO ERROR-SWITCH.
000775
000776     IF AMTHSI > LOW-VALUES
000777         MOVE AMTHSI       TO TEST-AMT
000778         PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
000779         MOVE -1           TO AMTHSL
000780     ELSE
000781         MOVE AL-UANOF     TO AMTHSA
000782         MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL.
000783
000784     IF SCREEN-ERROR
000785         MOVE -1           TO AMTHSL
000786         MOVE AL-UABON     TO AMTHSA
000787         MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL
000788     ELSE
000789         IF AMTHSI > LOW-VALUES
000790             MOVE AMT-BIF  TO AMT-PAID-HIGH-CNTL AMTHSO
000791             MOVE AL-UANON TO AMTHSA.
000792
000793     IF AMT-PAID-HIGH-CNTL < AMT-PAID-LOW-CNTL
000794         MOVE 'X'          TO BUILD-SWITCH
000795         MOVE ER-0308      TO EMI-ERROR
000796         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000797         MOVE -1           TO AMTLSL
000798         MOVE AL-UABON     TO AMTLSA AMTHSA.
000799
000800     MOVE SPACE TO ERROR-SWITCH.
000801
000802     IF TYPESI > LOW-VALUES
000803        IF TYPESI = PI-LIFE-OVERRIDE-L1 OR
000804                    PI-AH-OVERRIDE-L1   OR
000805*                   'I' OR 'G' OR 'F'
000806*                   'I' OR 'G' OR 'F' OR 'O'
000807                    'I' OR 'G' OR 'F' OR 'O' OR 'B' OR 'H'
000808           MOVE AL-UANON         TO TYPESA
000809           MOVE TYPESI           TO TYPE-CNTL
000810        ELSE
000811           MOVE AL-UABON         TO TYPESA
000812           MOVE -1               TO TYPESL
000813           MOVE 'X'              TO BUILD-SWITCH
000814           MOVE ER-0199          TO EMI-ERROR
000815           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000816     ELSE
000817        MOVE AL-UANOF            TO TYPESA.
000818
000819     IF CAUSELSI > SPACES
000820         MOVE CAUSELSI   TO CAUSE-CD-LOW-CNTL
000821     ELSE
000822         MOVE LOW-VALUES TO CAUSE-CD-LOW-CNTL.
000823
000824     IF CAUSEHSI > SPACES
000825         MOVE CAUSEHSI    TO CAUSE-CD-HIGH-CNTL
000826     ELSE
000827         MOVE HIGH-VALUES TO CAUSE-CD-HIGH-CNTL.
000828
000829     IF CAUSE-CD-HIGH-CNTL < CAUSE-CD-LOW-CNTL
000830         MOVE 'X'       TO BUILD-SWITCH
000831         MOVE ER-0308   TO EMI-ERROR
000832         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000833         MOVE -1        TO CAUSELSL
000834         MOVE AL-UABON  TO CAUSELSA CAUSEHSA.
000835
000836     MOVE SPACE TO ERROR-SWITCH.
000837
000838     IF DENSI > LOW-VALUES
000839         MOVE DENSI        TO TEST-RESP
000840         IF TEST-RESP = 'Y' OR 'N' OR SPACE
000841             MOVE AL-UANON TO DENSA
000842             MOVE DENSI    TO DEN-CNTL
000843         ELSE
000844            MOVE 'X'       TO BUILD-SWITCH
000845            MOVE ER-0046   TO EMI-ERROR
000846            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000847            MOVE AL-UABON  TO DENSA
000848            MOVE -1        TO DENSL
000849     ELSE
000850        MOVE AL-UANOF      TO DENSA.
000851
000852     IF REPLSI > SPACES
000853         MOVE REPLSI TO TEST-DATE
000854         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000855         IF NO-CONVERSION-ERROR
000856            MOVE DC-GREG-DATE-1-EDIT TO REPLSI
000857            MOVE DC-BIN-DATE-1       TO REP-DATE-LOW-CNTL
000858            MOVE AL-UANON            TO REPLSA
000859           ELSE
000860            MOVE -1                  TO REPLSL
000861            MOVE AL-UABON            TO REPLSA
000862            MOVE LOW-VALUES          TO REP-DATE-LOW-CNTL
000863     ELSE
000864         MOVE AL-UANOF               TO REPLSA
000865         MOVE LOW-VALUES             TO REP-DATE-LOW-CNTL.
000866
000867     IF REPHSI > SPACES
000868         MOVE REPHSI                 TO TEST-DATE
000869         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000870         IF NO-CONVERSION-ERROR
000871            MOVE DC-GREG-DATE-1-EDIT TO REPHSI
000872            MOVE DC-BIN-DATE-1       TO REP-DATE-HIGH-CNTL
000873            MOVE AL-UANON            TO REPHSA
000874           ELSE
000875            MOVE -1                  TO REPHSL
000876            MOVE AL-UABON            TO REPHSA
000877            MOVE HIGH-VALUES         TO REP-DATE-HIGH-CNTL
000878     ELSE
000879         MOVE AL-UANOF               TO REPHSA
000880         MOVE HIGH-VALUES            TO REP-DATE-HIGH-CNTL.
000881
000882     IF REP-DATE-HIGH-CNTL < REP-DATE-LOW-CNTL
000883         MOVE 'X'      TO BUILD-SWITCH
000884         MOVE ER-0308  TO EMI-ERROR
000885         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000886         MOVE -1       TO REPLSL
000887         MOVE AL-UABON TO REPLSA REPHSA.
000888
000889     MOVE SPACE            TO ERROR-SWITCH.
000890
000891     IF PROCSI > LOW-VALUES
000892         PERFORM 1060-CHECK-PROC THRU 1080-EXIT.
000893
000894     IF SCREEN-ERROR
000895         MOVE AL-UABON     TO PROCSA
000896         MOVE -1           TO PROCSL
000897     ELSE
000898         IF PROCSI = LOW-VALUES
000899             MOVE AL-UANOF TO PROCSA
000900         ELSE
000901             MOVE AL-UANON TO PROCSA
000902             MOVE PROCSI   TO PROC-CNTL.
000903
000904     MOVE SPACE TO ERROR-SWITCH.
000905
000906     IF PMTLSI > LOW-VALUES
000907         MOVE PMTLSI   TO TEST-AMT
000908         PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
000909         MOVE -1       TO PMTLSL
000910     ELSE
000911         MOVE AL-UANOF TO PMTHSA
000912         MOVE ZEROES   TO LST-PAID-LOW-CNTL.
000913
000914     IF SCREEN-ERROR
000915         MOVE -1       TO PMTLSL
000916         MOVE AL-UABON TO PMTLSA
000917         MOVE ZEROES   TO LST-PAID-LOW-CNTL
000918     ELSE
000919         IF PMTLSI > LOW-VALUES
000920             MOVE AMT-BIF  TO LST-PAID-LOW-CNTL PMTLSO
000921             MOVE AL-UANON TO PMTLSA.
000922
000923     MOVE SPACE TO ERROR-SWITCH.
000924
000925     IF PMTHSI > LOW-VALUES
000926         MOVE PMTHSI    TO TEST-AMT
000927         PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
000928     ELSE
000929         MOVE AL-UANOF  TO PMTHSA
000930         MOVE ALL-NINES TO LST-PAID-HIGH-CNTL.
000931
000932     IF SCREEN-ERROR
000933         MOVE -1        TO PMTHSL
000934         MOVE AL-UABON  TO PMTHSA
000935         MOVE ALL-NINES TO LST-PAID-HIGH-CNTL
000936     ELSE
000937         IF PMTHSI > LOW-VALUES
000938             MOVE AMT-BIF  TO LST-PAID-HIGH-CNTL PMTHSO
000939             MOVE AL-UANON TO PMTHSA.
000940
000941     IF LST-PAID-HIGH-CNTL < LST-PAID-LOW-CNTL
000942         MOVE 'X'      TO BUILD-SWITCH
000943         MOVE ER-0308  TO EMI-ERROR
000944         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000945         MOVE -1       TO PMTLSL
000946         MOVE AL-UABON TO PMTLSA PMTHSA.
000947
000948     MOVE SPACE TO ERROR-SWITCH.
000949
000950     IF PREMSI > LOW-VALUES
000951        IF PREMSI = SPACE OR
000952          (PREMSI > '0' AND < '4')
000953             MOVE AL-UANON       TO PREMSA
000954             MOVE PREMSI         TO PREM-CNTL
000955        ELSE
000956           MOVE 'X'              TO BUILD-SWITCH
000957           MOVE ER-0227          TO EMI-ERROR
000958           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000959           MOVE AL-UABON         TO PREMSA
000960           MOVE -1               TO PREMSL
000961     ELSE
000962        MOVE AL-UANOF            TO PREMSA.
000963
000964     MOVE SPACE TO ERROR-SWITCH.
000965
000966     IF MNTLSI > SPACES
000967         MOVE MNTLSI TO TEST-DATE
000968         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000969         IF NO-CONVERSION-ERROR
000970            MOVE DC-GREG-DATE-1-EDIT         TO MNTLSI
000971            MOVE DC-BIN-DATE-1   TO MNT-DATE-LOW-CNTL
000972            MOVE AL-UANON        TO MNTLSA
000973           ELSE
000974            MOVE -1              TO MNTLSL
000975            MOVE AL-UABON        TO MNTLSA
000976            MOVE LOW-VALUES      TO MNT-DATE-LOW-CNTL
000977     ELSE
000978         MOVE AL-UANOF   TO MNTLSA
000979         MOVE LOW-VALUES TO MNT-DATE-LOW-CNTL.
000980
000981     IF MNTHSI > SPACES
000982         MOVE MNTHSI TO TEST-DATE
000983         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
000984         IF NO-CONVERSION-ERROR
000985            MOVE DC-GREG-DATE-1-EDIT TO MNTHSI
000986            MOVE DC-BIN-DATE-1       TO MNT-DATE-HIGH-CNTL
000987            MOVE AL-UANON            TO MNTHSA
000988           ELSE
000989            MOVE -1                  TO MNTHSL
000990            MOVE AL-UABON            TO MNTHSA
000991            MOVE HIGH-VALUES         TO MNT-DATE-HIGH-CNTL
000992     ELSE
000993         MOVE AL-UANOF TO MNTHSA
000994         MOVE HIGH-VALUES TO MNT-DATE-HIGH-CNTL.
000995
000996     IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL
000997         MOVE 'X'       TO BUILD-SWITCH
000998         MOVE ER-0308   TO EMI-ERROR
000999         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001000         MOVE -1        TO MNTLSL
001001         MOVE AL-UABON  TO MNTLSA MNTHSA.
001002
001003     MOVE SPACE TO ERROR-SWITCH.
001004
001005     IF REQSI > LOW-VALUES
001006         MOVE REQSI TO TEST-RESP
001007         IF TEST-RESP = 'Y' OR 'N' OR SPACE
001008             MOVE AL-UANON       TO REQSA
001009             MOVE REQSI          TO REQ-CNTL
001010         ELSE
001011            MOVE 'X'             TO  BUILD-SWITCH
001012            MOVE ER-0046         TO EMI-ERROR
001013            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001014            MOVE AL-UABON        TO REQSA
001015            MOVE -1              TO REQSL
001016     ELSE
001017        MOVE AL-UANOF            TO REQSA.
001018
001019     MOVE SPACE TO ERROR-SWITCH.
001020
001021     IF ESTLSI > SPACES
001022         MOVE ESTLSI TO TEST-DATE
001023         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
001024         IF NO-CONVERSION-ERROR
001025            MOVE DC-GREG-DATE-1-EDIT TO ESTLSI
001026            MOVE DC-BIN-DATE-1       TO EST-DATE-LOW-CNTL
001027            MOVE AL-UANON            TO ESTLSA
001028            ELSE
001029            MOVE -1                  TO ESTLSL
001030            MOVE AL-UABON            TO ESTLSA
001031            MOVE LOW-VALUES          TO EST-DATE-LOW-CNTL
001032     ELSE
001033         MOVE AL-UANOF   TO ESTLSA
001034         MOVE LOW-VALUES TO EST-DATE-LOW-CNTL.
001035
001036     IF ESTHSI > SPACES
001037         MOVE ESTHSI TO TEST-DATE
001038         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
001039         IF NO-CONVERSION-ERROR
001040            MOVE DC-GREG-DATE-1-EDIT TO ESTHSI
001041            MOVE DC-BIN-DATE-1       TO EST-DATE-HIGH-CNTL
001042            MOVE AL-UANON            TO ESTHSA
001043            ELSE
001044            MOVE -1                  TO ESTHSL
001045            MOVE AL-UABON            TO ESTHSA
001046            MOVE HIGH-VALUES         TO EST-DATE-HIGH-CNTL
001047     ELSE
001048         MOVE AL-UANOF               TO ESTHSA
001049         MOVE HIGH-VALUES            TO EST-DATE-HIGH-CNTL.
001050
001051     IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL
001052         MOVE 'X'      TO BUILD-SWITCH
001053         MOVE ER-0308  TO EMI-ERROR
001054         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001055         MOVE -1       TO MNTLSL
001056         MOVE AL-UABON TO MNTLSA MNTHSA.
001057
001058     MOVE SPACE TO ERROR-SWITCH.
001059
001060     IF SUPRSI > LOW-VALUES
001061         MOVE SUPRSI TO TEST-RESP
001062         IF TEST-RESP = 'Y' OR 'N' OR SPACE
001063             MOVE AL-UANON       TO SUPRSA
001064             MOVE SUPRSI         TO SUPR-CNTL
001065         ELSE
001066            MOVE 'X'             TO  BUILD-SWITCH
001067            MOVE ER-0046         TO EMI-ERROR
001068            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001069            MOVE AL-UABON        TO SUPRSA
001070            MOVE -1              TO SUPRSL
001071     ELSE
001072        MOVE AL-UANOF            TO SUPRSA.
001073
001074     MOVE SPACE TO ERROR-SWITCH.
001075
001076     IF FOLLSI > SPACES
001077         MOVE FOLLSI TO TEST-DATE
001078         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
001079         IF NO-CONVERSION-ERROR
001080            MOVE DC-GREG-DATE-1-EDIT TO FOLLSI
001081            MOVE DC-BIN-DATE-1       TO FOL-DATE-LOW-CNTL
001082            MOVE AL-UANON            TO FOLLSA
001083           ELSE
001084            MOVE -1                  TO FOLLSL
001085            MOVE AL-UABON            TO FOLLSA
001086            MOVE LOW-VALUES          TO FOL-DATE-LOW-CNTL
001087     ELSE
001088         MOVE AL-UANOF               TO FOLLSA
001089         MOVE LOW-VALUES             TO FOL-DATE-LOW-CNTL.
001090
001091     MOVE SPACE TO ERROR-SWITCH.
001092
001093     IF FOLHSI > SPACES
001094         MOVE FOLHSI TO TEST-DATE
001095         PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
001096         IF NO-CONVERSION-ERROR
001097            MOVE DC-GREG-DATE-1-EDIT TO FOLHSI
001098            MOVE DC-BIN-DATE-1       TO FOL-DATE-HIGH-CNTL
001099            MOVE AL-UANON            TO FOLHSA
001100            ELSE
001101            MOVE -1                  TO FOLHSL
001102            MOVE AL-UABON            TO FOLHSA
001103            MOVE HIGH-VALUES         TO FOL-DATE-HIGH-CNTL
001104     ELSE
001105         MOVE AL-UANOF               TO FOLHSA
001106         MOVE HIGH-VALUES            TO FOL-DATE-HIGH-CNTL.
001107
001108     IF FOL-DATE-HIGH-CNTL < FOL-DATE-LOW-CNTL
001109         MOVE 'X'     TO BUILD-SWITCH
001110         MOVE ER-0308 TO EMI-ERROR
001111         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001112         MOVE -1      TO FOLLSL
001113         MOVE AL-UABON TO FOLLSA FOLHSA.
001114
001115     MOVE SPACE TO ERROR-SWITCH.
001116
001117     IF CERTSI > LOW-VALUES
001118         MOVE CERTSI TO TEST-RESP
001119         IF TEST-RESP = 'Y' OR 'N' OR SPACE
001120             MOVE AL-UANON       TO CERTSA
001121             MOVE CERTSI         TO CERT-CNTL
001122         ELSE
001123            MOVE 'X'             TO  BUILD-SWITCH
001124            MOVE ER-0046         TO EMI-ERROR
001125            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001126            MOVE AL-UABON        TO CERTSA
001127            MOVE -1              TO CERTSL
001128     ELSE
001129        MOVE AL-UANOF            TO CERTSA.
001130
001131     MOVE SPACE TO ERROR-SWITCH.
001132
001133     IF DAYSLSI = LOW-VALUES
001134         MOVE AL-UANOF TO DAYSLSA
001135         MOVE ZEROES   TO DAYS-LOW-CNTL
001136     ELSE
001137         IF DAYSLSI NOT NUMERIC
001138             MOVE ZEROS TO DAYS-LOW-CNTL
001139             MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH
001140         ELSE
001141             MOVE DAYSLSI  TO DAYS-LOW-CNTL
001142             MOVE AL-UANON TO DAYSLSA.
001143
001144     IF SCREEN-ERROR
001145         MOVE -1       TO DAYSLSL
001146         MOVE AL-UABON TO DAYSLSA
001147         MOVE ER-0307  TO EMI-ERROR
001148         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001149
001150     MOVE SPACE TO ERROR-SWITCH.
001151
001152     IF DAYSHSI = LOW-VALUES
001153         MOVE AL-UANOF     TO DAYSHSA
001154         MOVE 9999         TO DAYS-HIGH-CNTL
001155     ELSE
001156         IF DAYSHSI NOT NUMERIC
001157             MOVE 9999 TO DAYS-HIGH-CNTL
001158             MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
001159         ELSE
001160             MOVE DAYSHSI  TO DAYS-HIGH-CNTL
001161             MOVE AL-UANON TO DAYSHSA.
001162
001163     IF SCREEN-ERROR
001164         MOVE -1       TO DAYSHSL
001165         MOVE AL-UABON TO DAYSHSA
001166         MOVE ER-0307  TO EMI-ERROR
001167         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001168
001169     IF DAYS-HIGH-CNTL < DAYS-LOW-CNTL
001170         MOVE 'X'     TO BUILD-SWITCH
001171         MOVE ER-0308 TO EMI-ERROR
001172         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001173         MOVE -1       TO DAYSLSL
001174         MOVE AL-UABON TO DAYSLSA DAYSHSA.
001175
001176     MOVE SPACE TO ERROR-SWITCH.
001177
001178     IF PRISI > LOW-VALUES
001179         IF PRISI = SPACE OR (PRISI > '0' AND
001180                              NOT   > '9')
001181             MOVE AL-UANON       TO PRISA
001182             MOVE PRISI          TO PRI-CNTL
001183         ELSE
001184            MOVE 'X'             TO  BUILD-SWITCH
001185            MOVE ER-0274         TO EMI-ERROR
001186            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001187            MOVE AL-UABON        TO PRISA
001188            MOVE -1              TO PRISL
001189     ELSE
001190        MOVE AL-UANOF            TO PRISA.
001191
001192
001193     MOVE SPACE TO ERROR-SWITCH.
001194
001195     IF AUTOSI > LOW-VALUES
001196         MOVE AUTOSI TO TEST-RESP
001197         IF TEST-RESP = 'Y' OR 'N' OR SPACE
001198             MOVE AL-UANON        TO AUTOSA
001199             MOVE AUTOSI          TO AUTO-CNTL
001200         ELSE
001201            MOVE 'X'             TO  BUILD-SWITCH
001202            MOVE ER-0046         TO EMI-ERROR
001203            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001204            MOVE AL-UABON        TO AUTOSA
001205            MOVE -1              TO AUTOSL
001206     ELSE
001207        MOVE AL-UANOF            TO AUTOSA.
001208
001209     MOVE SPACE TO ERROR-SWITCH.
001210
001211     IF OPCLSI > LOW-VALUES
001212         MOVE OPCLSI TO TEST-RESP
001213         IF TEST-RESP = 'O' OR 'C' OR 'R' OR SPACE
001214             MOVE AL-UANON       TO OPCLSA
001215             MOVE OPCLSI         TO OPCL-CNTL
001216         ELSE
001217            MOVE 'X'             TO  BUILD-SWITCH
001218            MOVE ER-0767         TO EMI-ERROR
001219            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001220            MOVE AL-UABON        TO OPCLSA
001221            MOVE -1              TO OPCLSL
001222     ELSE
001223        MOVE AL-UANOF            TO OPCLSA.
001224
001225
001226     MOVE SPACE TO ERROR-SWITCH.
001227
001228     IF PRTOPTI > LOW-VALUES
001229         IF PRTOPTI = 'N' OR 'L'
001230             MOVE AL-UANON       TO PRTOPTA
001231             MOVE PRTOPTI        TO PI-PRINT-OPTION
001232         ELSE
001233            MOVE 'X'             TO BUILD-SWITCH
001234            MOVE ER-0334         TO EMI-ERROR
001235            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001236            MOVE AL-UABON        TO PRTOPTA
001237            MOVE -1              TO PRTOPTL
001238     ELSE
001239        MOVE 'L'                 TO PI-PRINT-OPTION
001240        MOVE AL-UANOF            TO PRTOPTA.
001241
001242     MOVE SPACE TO ERROR-SWITCH.
001243
001244     IF FMTOPTI > LOW-VALUES
001245         IF FMTOPTI = 'F' OR 'P'
001246             MOVE AL-UANON       TO FMTOPTA
001247             MOVE FMTOPTI        TO PI-FORMAT-OPTION
001248         ELSE
001249            MOVE 'X'             TO BUILD-SWITCH
001250            MOVE ER-0335         TO EMI-ERROR
001251            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001252            MOVE AL-UABON        TO FMTOPTA
001253            MOVE -1              TO FMTOPTL
001254     ELSE
001255        MOVE AL-UANOF            TO FMTOPTA.
001256
001257     IF ALTPRTI > LOW-VALUES
001258         MOVE AL-UANON                   TO  ALTPRTA
001259         MOVE ALTPRTI                    TO  PI-ALT-PRINT-ID
001260     ELSE
001261         IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES
001262             MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINT-ID
001263         ELSE
001264             MOVE SPACES                 TO  PI-ALT-PRINT-ID.
001265
001266     IF SCREEN-HAS-ERRORS
001267         GO TO 1010-EXIT.
001268
001269     MOVE SAVE-BIN-DATE TO CURRENT-DATE-BIN.
001270
001271 1010-EXIT.
001272     EXIT.
001273     EJECT
001274
001275 1060-CHECK-PROC.
001276     IF PROCSI = SPACES
001277         GO TO 1080-EXIT.
001278
001279     MOVE PI-COMPANY-ID TO COMPANY-ID.
001280     MOVE '2'           TO RECORD-TYPE.
001281     MOVE PROCSI        TO ACCESS-CD-GENL.
001282     MOVE ZEROS         TO SEQUENCE-NO.
001283
001284     
      * EXEC CICS HANDLE CONDITION
001285*        NOTFND  (1070-PROC-NOTFND)
001286*        NOTOPEN (6030-CNTL-NOT-OPEN)
001287*    END-EXEC.
      *    MOVE '"$IJ                  ! & #00007455' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303037343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001288
001289     PERFORM 5120-READ-CNTL THRU 5130-EXIT.
001290
001291     MOVE 'X' TO PROC-SWITCH.
001292     GO TO 1080-EXIT.
001293
001294 1070-PROC-NOTFND.
001295
001296     MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH.
001297     MOVE ER-0273 TO EMI-ERROR.
001298     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001299     MOVE -1 TO CARRSL.
001300
001301 1080-EXIT.
001302     EXIT.
001303
001304 1130-DEEDIT-DATE.
001305     
      * EXEC CICS BIF DEEDIT
001306*        FIELD  (TEST-DATE)
001307*        LENGTH (DATE-LENGTH)
001308*    END-EXEC.
      *    MOVE '@"L                   #   #00007476' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEST-DATE, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001309
001310     MOVE '4'      TO DC-OPTION-CODE.
001311     MOVE BIF-DATE TO DC-GREG-DATE-1-MDY.
001312     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001313     IF DATE-CONVERSION-ERROR
001314         MOVE 'X'     TO BUILD-SWITCH
001315         MOVE ER-0304 TO EMI-ERROR
001316         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001317
001318 1140-EXIT.
001319     EXIT.
001320
001321 1170-DEEDIT-AMT.
001322     
      * EXEC CICS BIF DEEDIT
001323*        FIELD  (TEST-AMT)
001324*        LENGTH (AMT-LENGTH)
001325*    END-EXEC.
      *    MOVE '@"L                   #   #00007493' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEST-AMT, 
                 AMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001326
001327     IF TEST-AMT NUMERIC
001328         GO TO 1170-EXIT.
001329
001330     MOVE 'X'     TO ERROR-SWITCH BUILD-SWITCH.
001331     MOVE ER-0419 TO EMI-ERROR.
001332     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001333
001334 1170-EXIT.
001335     EXIT.
001336     EJECT
001337 2000-BUILD-TS.
001338     MOVE ZEROS                  TO COUNT-1.
001339     MOVE PI-COMPANY-CD          TO MSTR-COMPANY-CODE.
001340     MOVE CARRIER-CNTL           TO MSTR-CARRIER
001341     MOVE SPACES                 TO REST-OF-KEY BUILD-SWITCH.
001342
001343     PERFORM 5000-START-BROWSE THRU 5020-EXIT.
001344
001345     IF NO-RECORDS
001346         GO TO 2010-EXIT.
001347
001348     PERFORM 5030-READ-FILE THRU 5050-EXIT.
001349
001350     PERFORM 3000-EDIT-RECORDS THRU 3020-EXIT
001351         UNTIL BUILD-COMPLETE.
001352
001353     PERFORM 5060-END-BROWSE THRU 5070-EXIT.
001354
001355 2010-EXIT.
001356     EXIT.
001357     EJECT
001358 3000-EDIT-RECORDS.
001359     MOVE SPACE TO ERROR-SWITCH.
001360
001361     IF  PI-COMPANY-ID = 'DMD'
001362             AND
001363         SETUP-ERRORS
001364         GO TO 3010-GET-NEXT.
001365
001366     IF WS-SEX-SELECTION NOT = SPACES
001367        IF WS-SEX-SELECTION NOT = CL-INSURED-SEX-CD
001368            GO TO 3010-GET-NEXT.
001369
001370     IF PROC-CNTL NOT = SPACES
001371        IF PROC-CNTL NOT = CL-PROCESSOR-ID
001372            GO TO 3010-GET-NEXT.
001373
001374     IF CARRIER-CNTL NOT = SPACES
001375        IF CARRIER-CNTL NOT = CL-CARRIER
001376            MOVE 'Y'             TO BUILD-SWITCH
001377            GO TO 3020-EXIT.
001378
001379     IF GROUP-CNTL NOT = SPACES
001380        IF GROUP-CNTL NOT = CL-CERT-GROUPING
001381         GO TO 3010-GET-NEXT.
001382
001383     IF STATE-CNTL NOT = SPACES
001384        IF STATE-CNTL NOT = CL-CERT-STATE
001385         GO TO 3010-GET-NEXT.
001386
001387     IF ACCOUNT-CNTL NOT = SPACES
001388         IF ACCOUNT-CNTL NOT = CL-CERT-ACCOUNT
001389         GO TO 3010-GET-NEXT.
001390
001391     IF TYPE-CNTL NOT = SPACES
001392         IF TYPE-CNTL NOT = CL-CLAIM-TYPE
001393         GO TO 3010-GET-NEXT.
001394
001395     IF DEN-CNTL NOT = SPACES
001396         PERFORM 3080-CHECK-DEN THRU 3080-EXIT.
001397
001398     IF SCREEN-ERROR
001399         GO TO 3010-GET-NEXT.
001400
001401     IF PREM-CNTL NOT = SPACES
001402        IF PREM-CNTL NOT = CL-CLAIM-PREM-TYPE
001403         GO TO 3010-GET-NEXT.
001404
001405     IF REQ-CNTL NOT = SPACES
001406         PERFORM 3110-CHECK-REQ THRU 3110-EXIT.
001407
001408     IF SCREEN-ERROR
001409         GO TO 3010-GET-NEXT.
001410
001411     IF SUPR-CNTL NOT = SPACES
001412         PERFORM 3120-CHECK-SUPR THRU 3120-EXIT.
001413
001414     IF SCREEN-ERROR
001415         GO TO 3010-GET-NEXT.
001416
001417     IF CERT-CNTL NOT = SPACES
001418         PERFORM 3130-CHECK-CERT THRU 3130-EXIT.
001419
001420     IF SCREEN-ERROR
001421         GO TO 3010-GET-NEXT.
001422
001423     IF PRI-CNTL NOT = SPACES
001424        IF PRI-CNTL NOT = CL-PRIORITY-CD
001425         GO TO 3010-GET-NEXT.
001426
001427     IF AUTO-CNTL NOT = SPACES
001428         PERFORM 3150-CHECK-AUTO THRU 3150-EXIT.
001429
001430     IF SCREEN-ERROR
001431         GO TO 3010-GET-NEXT.
001432
001433     IF OPCL-CNTL NOT = SPACES
001434         PERFORM 3180-CHECK-OPCL THRU 3180-EXIT.
001435
001436     IF SCREEN-ERROR
001437         GO TO 3010-GET-NEXT.
001438
001439     IF CL-INCURRED-DT < INC-DATE-LOW-CNTL
001440         GO TO 3010-GET-NEXT.
001441
001442     IF CL-INCURRED-DT > INC-DATE-HIGH-CNTL
001443         GO TO 3010-GET-NEXT.
001444
001445     IF CL-LAST-PMT-DT < LST-PMT-LOW-CNTL
001446         GO TO 3010-GET-NEXT.
001447
001448     IF CL-LAST-PMT-DT > LST-PMT-HIGH-CNTL
001449         GO TO 3010-GET-NEXT.
001450
001451     MOVE SPACE TO ERROR-SWITCH.
001452
001453     IF OPENLSI = LOW-VALUES
001454         NEXT SENTENCE
001455     ELSE
001456         PERFORM 3160-CALC-ELAPSED-MONTHS THRU 3160-EXIT
001457         IF DC-ELAPSED-MONTHS <       MO-OPEN-LOW-CNTL OR
001458            DC-ELAPSED-MONTHS > MO-OPEN-HIGH-CNTL
001459                GO TO 3010-GET-NEXT.
001460
001461     IF SCREEN-ERROR
001462         GO TO 3010-GET-NEXT.
001463
001464     IF AMTLSI > LOW-VALUES
001465         IF CL-TOTAL-PAID-AMT < AMT-PAID-LOW-CNTL
001466             GO TO 3010-GET-NEXT.
001467
001468     IF AMTHSI > LOW-VALUES
001469         IF CL-TOTAL-PAID-AMT > AMT-PAID-HIGH-CNTL
001470             GO TO 3010-GET-NEXT.
001471
001472     IF CAUSELSI > SPACES
001473         IF CL-CAUSE-CD < CAUSE-CD-LOW-CNTL
001474             GO TO 3010-GET-NEXT.
001475
001476     IF CAUSEHSI > SPACES
001477         IF CL-CAUSE-CD > CAUSE-CD-HIGH-CNTL
001478             GO TO 3010-GET-NEXT.
001479
001480     IF REPLSI > SPACES
001481         IF CL-REPORTED-DT < REP-DATE-LOW-CNTL
001482             GO TO 3010-GET-NEXT.
001483
001484     IF REPHSI > SPACES
001485         IF CL-REPORTED-DT > REP-DATE-HIGH-CNTL
001486             GO TO 3010-GET-NEXT.
001487
001488     IF PMTLSI > LOW-VALUES
001489         IF CL-LAST-PMT-AMT < LST-PAID-LOW-CNTL
001490             GO TO 3010-GET-NEXT.
001491
001492     IF PMTHSI > LOW-VALUES
001493         IF CL-LAST-PMT-AMT > LST-PAID-HIGH-CNTL
001494             GO TO 3010-GET-NEXT.
001495
001496     IF MNTLSI > SPACES
001497         IF CL-LAST-MAINT-DT < MNT-DATE-LOW-CNTL
001498             GO TO 3010-GET-NEXT.
001499
001500     IF MNTHSI > SPACES
001501         IF CL-LAST-MAINT-DT > MNT-DATE-HIGH-CNTL
001502             GO TO 3010-GET-NEXT.
001503
001504     IF ESTLSI > SPACES
001505         IF CL-FILE-ESTABLISH-DT < EST-DATE-LOW-CNTL
001506             GO TO 3010-GET-NEXT.
001507
001508     IF ESTHSI > SPACES
001509         IF CL-FILE-ESTABLISH-DT > EST-DATE-HIGH-CNTL
001510             GO TO 3010-GET-NEXT.
001511
001512     IF FOLLSI > SPACES
001513         IF CL-NEXT-FOLLOWUP-DT < FOL-DATE-LOW-CNTL
001514             GO TO 3010-GET-NEXT.
001515
001516     IF FOLHSI > SPACES
001517         IF CL-NEXT-FOLLOWUP-DT > FOL-DATE-HIGH-CNTL
001518             GO TO 3010-GET-NEXT.
001519
001520     PERFORM 3170-CALC-ELAPSED-DAYS THRU 3170-EXIT.
001521
001522     IF SCREEN-ERROR
001523         GO TO 3010-GET-NEXT.
001524
001525     IF DC-ELAPSED-DAYS < DAYS-LOW-CNTL
001526         GO TO 3010-GET-NEXT.
001527
001528     IF DC-ELAPSED-DAYS > DAYS-HIGH-CNTL
001529         GO TO 3010-GET-NEXT.
001530
001531     PERFORM 4000-BUILD-160B THRU 4000-EXIT.
001532
001533 3010-GET-NEXT.
001534     PERFORM 5030-READ-FILE THRU 5050-EXIT.
001535
001536     IF CL-COMPANY-CD NOT = PI-COMPANY-CD
001537         MOVE 'Y' TO BUILD-SWITCH.
001538
001539     IF COUNT-1 = MAX-TS-PAGES
001540        MOVE 'Y'                 TO BUILD-SWITCH.
001541
001542 3020-EXIT.
001543     EXIT.
001544
001545 3080-CHECK-DEN.
001546     IF DEN-CNTL = 'N'    AND
001547        NOT CLAIM-DENIED
001548         GO TO 3080-EXIT.
001549
001550     IF DEN-CNTL = 'Y'    AND
001551        CLAIM-DENIED
001552         GO TO 3080-EXIT.
001553
001554******************************************************************
001555*    IF DENIED CLAIMS ARE SELECTED AND THE LAST CLOSE REASON ON  *
001556*    THE CLAIM MASTER IS NOT 'DENIED', THE PROGRAM READS THE     *
001557*    ZERO TRAILER AND CHECKS THE OPEN/CLOSE HISTORY TO SEE IF    *
001558*    THE CLAIM HAS EVER BEEN DENIED TO DETERMINE IF IT MEETS     *
001559*    THE SELECTION CRITERIA.                                     *
001560******************************************************************
001561
001562     IF DEN-CNTL = 'Y'
001563       IF CL-PURGED-DT EQUAL LOW-VALUES
001564         MOVE CL-CONTROL-PRIMARY     TO  TRLR-MAIN-KEY
001565         MOVE +0                     TO  TRLR-SEQ-NO
001566         PERFORM 5100-READ-TRLR THRU 5110-EXIT
001567         IF ERROR-SWITCH IS EQUAL TO 'X'
001568           GO TO 3080-EXIT
001569         ELSE
001570           IF (AT-OPEN-CLOSE-REASON (1) = 'DENIE' OR 'DENIL') OR
001571              (AT-OPEN-CLOSE-REASON (2) = 'DENIE' OR 'DENIL') OR
001572              (AT-OPEN-CLOSE-REASON (3) = 'DENIE' OR 'DENIL') OR
001573              (AT-OPEN-CLOSE-REASON (4) = 'DENIE' OR 'DENIL') OR
001574              (AT-OPEN-CLOSE-REASON (5) = 'DENIE' OR 'DENIL') OR
001575              (AT-OPEN-CLOSE-REASON (6) = 'DENIE' OR 'DENIL')
001576               GO TO 3080-EXIT.
001577
001578     MOVE 'X' TO ERROR-SWITCH.
001579
001580 3080-EXIT.
001581     EXIT.
001582
001583 3110-CHECK-REQ.
001584     IF REQ-CNTL = 'Y' AND
001585        CL-NEXT-FOLLOWUP-DT > LOW-VALUES
001586         GO TO 3110-EXIT.
001587
001588     IF REQ-CNTL = 'N' AND
001589        CL-NEXT-FOLLOWUP-DT = LOW-VALUES
001590         GO TO 3110-EXIT.
001591
001592     MOVE 'X' TO ERROR-SWITCH.
001593
001594 3110-EXIT.
001595     EXIT.
001596
001597 3120-CHECK-SUPR.
001598     IF SUPR-CNTL = 'Y' AND
001599        CL-SUPV-ATTN-CD = 'Y'
001600         GO TO 3120-EXIT.
001601
001602     IF SUPR-CNTL = 'N' AND
001603        (CL-SUPV-ATTN-CD = 'N' OR SPACE)
001604         GO TO 3120-EXIT.
001605
001606     MOVE 'X' TO ERROR-SWITCH.
001607
001608 3120-EXIT.
001609     EXIT.
001610
001611 3130-CHECK-CERT.
001612     IF CERT-CNTL = 'Y' AND
001613        CERT-WAS-CREATED
001614         GO TO 3130-EXIT.
001615
001616     IF CERT-CNTL = 'N' AND
001617        NOT CERT-WAS-CREATED
001618         GO TO 3130-EXIT.
001619
001620     MOVE 'X' TO ERROR-SWITCH.
001621
001622 3130-EXIT.
001623     EXIT.
001624
001625 3150-CHECK-AUTO.
001626     IF AUTO-CNTL = 'Y' AND
001627        CL-AUTO-PAY-SEQ > ZERO
001628         GO TO 3150-EXIT.
001629
001630     IF AUTO-CNTL = 'N' AND
001631        CL-AUTO-PAY-SEQ = ZERO
001632         GO TO 3150-EXIT.
001633
001634     MOVE 'X' TO ERROR-SWITCH.
001635
001636 3150-EXIT.
001637     EXIT.
001638
001639 3160-CALC-ELAPSED-MONTHS.
001640     MOVE CURRENT-DATE-BIN      TO DC-BIN-DATE-2.
001641
001642     IF CL-LAST-REOPEN-DT = LOW-VALUES
001643         MOVE CL-REPORTED-DT    TO DC-BIN-DATE-1
001644     ELSE
001645         MOVE CL-LAST-REOPEN-DT TO DC-BIN-DATE-1.
001646
001647     MOVE '1'          TO DC-OPTION-CODE.
001648     MOVE SPACE        TO DC-ERROR-CODE.
001649     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001650     IF DATE-CONVERSION-ERROR
001651         MOVE 'X' TO ERROR-SWITCH.
001652
001653 3160-EXIT.
001654     EXIT.
001655
001656 3170-CALC-ELAPSED-DAYS.
001657     IF CL-LAST-PMT-DT = LOW-VALUES
001658         MOVE ZERO TO DC-ELAPSED-DAYS
001659         GO TO 3170-EXIT.
001660
001661     MOVE CURRENT-DATE-BIN TO DC-BIN-DATE-2.
001662     MOVE CL-LAST-PMT-DT   TO DC-BIN-DATE-1.
001663     MOVE '1'              TO DC-OPTION-CODE.
001664     MOVE SPACE            TO DC-ERROR-CODE.
001665     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001666     IF DATE-CONVERSION-ERROR
001667         MOVE 'X' TO ERROR-SWITCH.
001668
001669 3170-EXIT.
001670     EXIT.
001671
001672 3180-CHECK-OPCL.
001673     IF OPCL-CNTL = CL-CLAIM-STATUS
001674         GO TO 3180-EXIT.
001675
001676     IF OPCL-CNTL = 'R' AND CLAIM-IS-OPEN AND
001677        CL-LAST-REOPEN-DT  NOT = LOW-VALUES
001678           GO TO 3180-EXIT.
001679
001680     MOVE 'X' TO ERROR-SWITCH.
001681
001682 3180-EXIT.
001683     EXIT.
001684     EJECT
001685 4000-BUILD-160B.
001686
001687     MOVE LOW-VALUES             TO EL160BO.
001688     MOVE SPACES                 TO PI-CONTROL-IN-PROGRESS.
001689     PERFORM 4010-MOVE-MSTR THRU 4010-EXIT.
001690     MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY.
001691     MOVE +0                     TO TRLR-SEQ-NO.
001692     MOVE SPACE                  TO ERROR-SWITCH.
001693
001694     IF CL-PURGED-DT NOT EQUAL LOW-VALUES
001695        MOVE 'CLAIM IS PURGED ' TO CAUSEO
001696        MOVE AL-SABOF           TO CAUSEA
001697        GO TO 4000-BYPASS-TRLRS.
001698
001699     PERFORM 5100-READ-TRLR THRU 5110-EXIT.
001700
001701     IF SCREEN-ERROR
001702        MOVE ER-0205            TO SCNERRO
001703     ELSE
001704        MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY
001705        MOVE +90                    TO TRLR-SEQ-NO
001706        MOVE SPACE                  TO ERROR-SWITCH
001707        PERFORM 5100-READ-TRLR THRU 5110-EXIT
001708        IF NOT SCREEN-ERROR
001709           IF AT-TRAILER-TYPE EQUAL '6'
001710              MOVE AT-INFO-LINE-1       TO CAUSEO
001711              PERFORM 4020-MOVE-TRLR THRU 4020-EXIT.
001712
001713 4000-BYPASS-TRLRS.
001714
001715     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
001716         GO TO 4000-READ-EMPLCY.
001717
001718     MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
001719     MOVE CL-CERT-CARRIER        TO CERT-CARRIER PI-CARRIER.
001720     MOVE CL-CERT-GROUPING       TO CERT-GROUP.
001721     MOVE CL-CERT-STATE          TO CERT-STATE.
001722     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
001723     MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.
001724     MOVE CL-CERT-EFF-DT         TO CERT-DATE.
001725     MOVE SPACE                  TO ERROR-SWITCH.
001726
001727     PERFORM 5080-READ-CERT THRU 5090-EXIT.
001728
001729     IF SCREEN-ERROR
001730         MOVE ER-0206            TO SCNERRO
001731     ELSE
001732         PERFORM 4030-MOVE-CERT THRU 4030-EXIT.
001733
001734     MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.
001735     MOVE PI-UPDATE-BY           TO USERSAVO.
001736     MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.
001737
001738     PERFORM 5140-WRITE-TS THRU 5150-EXIT.
001739
001740     GO TO 4000-EXIT.
001741
001742 4000-READ-EMPLCY.
001743
001744     MOVE CL-COMPANY-CD          TO  EMPLCY-COMPANY-CD.
001745     MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER
001746                                     PI-CARRIER.
001747     MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.
001748     MOVE CL-CERT-STATE          TO  EMPLCY-STATE.
001749     MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.
001750     MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DT.
001751     MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO
001752                                     PI-MP-REFERENCE-NO.
001753     MOVE CL-CERT-NO             TO  PI-CERT-NO.
001754     MOVE SPACE                  TO  ERROR-SWITCH.
001755
001756     PERFORM 5095-READ-EMPLCY THRU 5095-EXIT.
001757
001758     IF SCREEN-ERROR
001759         MOVE ER-9483            TO  SCNERRO
001760         GO TO 4000-EXIT.
001761
001762     MOVE PM-COMPANY-CD          TO  EMPLAN-COMPANY-CD.
001763     MOVE PM-CARRIER             TO  EMPLAN-CARRIER.
001764     MOVE PM-GROUPING            TO  EMPLAN-GROUPING.
001765     MOVE PM-STATE               TO  EMPLAN-STATE.
001766     MOVE PM-PRODUCER            TO  EMPLAN-PRODUCER.
001767     MOVE PM-INS-PLAN-CD         TO  EMPLAN-PLAN-CODE.
001768     MOVE PM-INS-PLAN-REVISION   TO  EMPLAN-REV-NO.
001769
001770     PERFORM 5096-READ-EMPLAN THRU 5096-EXIT.
001771
001772     IF SCREEN-ERROR
001773         MOVE ER-9811            TO  SCNERRO
001774     ELSE
001775         PERFORM 4030-MOVE-EMPLCY THRU 4030-EMPLCY-EXIT.
001776
001777     MOVE PI-CONTROL-IN-PROGRESS TO  PIKEYO.
001778     MOVE PI-UPDATE-BY           TO  USERSAVO.
001779     MOVE PI-UPDATE-HHMMSS       TO  TIMESAVO.
001780
001781     PERFORM 5140-WRITE-TS THRU 5150-EXIT.
001782
001783 4000-EXIT.
001784     EXIT.
001785     EJECT
001786 4010-MOVE-MSTR.
001787     MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
001788     MOVE CL-CLAIM-TYPE          TO TYPEO.
001789     MOVE CL-CERT-PRIME          TO CERTO.
001790     MOVE CL-CERT-SFX            TO CERTSXO.
001791     MOVE CL-CCN                 TO CREDCDO.
001792     MOVE CL-CERT-CARRIER        TO CARRO.
001793     MOVE CL-CLAIM-STATUS        TO STATUSO.
001794     MOVE CL-PROCESSOR-ID        TO PROCO.
001795     MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
001796     MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
001797     MOVE CL-INSURED-MID-INIT    TO MMINITO.
001798     MOVE CL-INSURED-SEX-CD      TO SEXO.
001799
001800     IF CL-INSURED-BIRTH-DT > LOW-VALUES
001801         MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
001802         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001803         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001804         MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
001805     ELSE
001806         MOVE SPACES TO BIRTHO.
001807
001808     IF CL-SSN-STATE   = CL-CERT-STATE AND
001809        CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
001810         MOVE SPACES        TO SOCIALO
001811     ELSE
001812         MOVE CL-SOC-SEC-NO TO SOCIALO.
001813
001814     MOVE CL-INSURED-OCC-CD    TO OCCO.
001815     MOVE CL-BENEFICIARY       TO CBENEO.
001816     MOVE CL-CAUSE-CD          TO CCAUSCDO.
001817
001818     IF CL-EST-END-OF-DISAB-DT > LOW-VALUES
001819         MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
001820         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001821         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001822         MOVE DC-GREG-DATE-1-EDIT TO ENDO
001823     ELSE
001824         MOVE SPACES TO ENDO.
001825
001826     IF CL-PAID-THRU-DT > LOW-VALUES
001827        IF NOT PI-USES-PAID-TO
001828           MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1
001829           MOVE SPACES               TO  DC-OPTION-CODE
001830                                         DC-ERROR-CODE
001831           PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001832           MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO
001833        ELSE
001834           MOVE WS-PAID-TO-HDG       TO  BHEADO
001835           MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1
001836           MOVE '6'                  TO  DC-OPTION-CODE
001837           MOVE +1                   TO  DC-ELAPSED-DAYS
001838           MOVE +0                   TO  DC-ELAPSED-MONTHS
001839           PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001840           MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO
001841     ELSE
001842        MOVE SPACES                  TO  PDTHRUO
001843        IF PI-USES-PAID-TO
001844           MOVE WS-PAID-TO-HDG       TO  BHEADO.
001845
001846     MOVE CL-TOTAL-PAID-AMT  TO PDAMTO.
001847     MOVE CL-NO-OF-DAYS-PAID TO NODAYSO.
001848     MOVE CL-NO-OF-PMTS-MADE TO NOPMTSO.
001849
001850     IF CL-INCURRED-DT > LOW-VALUES
001851         MOVE CL-INCURRED-DT TO DC-BIN-DATE-1
001852         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001853         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001854         MOVE DC-GREG-DATE-1-EDIT TO INCO
001855     ELSE
001856         MOVE SPACES TO INCO.
001857
001858     IF CL-REPORTED-DT > LOW-VALUES
001859         MOVE CL-REPORTED-DT TO DC-BIN-DATE-1
001860         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001861         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001862         MOVE DC-GREG-DATE-1-EDIT TO REPO
001863     ELSE
001864         MOVE SPACES TO REPO.
001865
001866     IF CL-FILE-ESTABLISH-DT > LOW-VALUES
001867         MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
001868         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001869         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001870         MOVE DC-GREG-DATE-1-EDIT TO ESTO
001871     ELSE
001872         MOVE SPACES TO ESTO.
001873
001874*    IF CL-LAST-PMT-DT > LOW-VALUES
001875*        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1
001876*        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001877*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001878*        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO
001879*    ELSE
001880*        MOVE SPACES TO LSTPMTO.
001881
001882*    MOVE CL-LAST-PMT-AMT TO LSTAMTO.
001883
001884     IF CL-LAST-MAINT-DT > LOW-VALUES
001885         MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1
001886         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001887         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001888         MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
001889     ELSE
001890         MOVE SPACES TO MNTDTO.
001891
001892     IF CL-LAST-MAINT-TYPE = SPACE
001893         MOVE 'SET UP' TO MNTTYPEO
001894     ELSE
001895         IF CL-LAST-MAINT-TYPE = '1'
001896             MOVE 'PAYMNT' TO MNTTYPEO
001897         ELSE
001898             IF CL-LAST-MAINT-TYPE = '2'
001899                 MOVE 'LETTER' TO MNTTYPEO
001900             ELSE
001901                 IF CL-LAST-MAINT-TYPE = '3'
001902                     MOVE 'UPDATE' TO MNTTYPEO
001903                 ELSE
001904                     IF CL-LAST-MAINT-TYPE = '4'
001905                         MOVE 'RESTOR' TO MNTTYPEO
001906                     ELSE
001907                         IF CL-LAST-MAINT-TYPE = '5'
001908                             MOVE 'INC DT' TO MNTTYPEO
001909                         ELSE
001910                             IF CL-LAST-MAINT-TYPE = '6'
001911                                 MOVE ' CONV'  TO MNTTYPEO
001912                             ELSE
001913                                 MOVE SPACES TO MNTTYPEO.
001914
001915     MOVE CL-PRIORITY-CD       TO PRICDO.
001916     MOVE CL-SUPV-ATTN-CD      TO SUPVO.
001917     MOVE CL-FILE-LOCATION     TO FILEO.
001918     MOVE CL-LAST-MAINT-USER   TO PI-UPDATE-BY.
001919     MOVE CL-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.
001920
001921 4010-EXIT.
001922     EXIT.
001923     EJECT
001924 4020-MOVE-TRLR.
001925
001926 4020-EXIT.
001927     EXIT.
001928     EJECT
001929 4030-MOVE-CERT.
001930     IF CM-CERT-EFF-DT > LOW-VALUES
001931         MOVE CM-CERT-EFF-DT TO DC-BIN-DATE-1
001932         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001933         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001934         MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO
001935     ELSE
001936         MOVE SPACES              TO CERTEFFO.
001937
001938     MOVE CM-ACCOUNT              TO CERTACTO.
001939     MOVE CM-STATE                TO CERTSTO PI-STATE.
001940     MOVE CM-GROUPING             TO CERTGRPO.
001941     MOVE CM-CARRIER              TO CERTCARO.
001942     MOVE CM-INSURED-LAST-NAME    TO CLNAMEO.
001943     MOVE CM-INSURED-FIRST-NAME   TO CFNAMEO.
001944     MOVE CM-INSURED-INITIAL2     TO CINITO.
001945     MOVE CM-JT-LAST-NAME         TO CJLNAMEO.
001946     MOVE CM-JT-FIRST-NAME        TO CJFAMEO.
001947     MOVE CM-JT-INITIAL           TO CJINITO.
001948
001949     MOVE CM-INSURED-ISSUE-AGE    TO INSAGEO.
001950     MOVE CM-INSURED-JOINT-AGE    TO JAGEO.
001951     IF CM-SSN-STATE   = CM-STATE AND
001952        CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
001953         MOVE SPACES       TO SOCSECO
001954     ELSE
001955        MOVE CM-SOC-SEC-NO TO SOCSECO.
001956
001957     PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.
001958
001959     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
001960         CONTINUE
001961     ELSE
001962         GO TO 4030-MOVE-CERT-AH.
001963
001964     IF CM-LF-BENEFIT-CD = '00'
001965         GO TO 4030-MOVE-REST-OF-CERT.
001966
001967     MOVE CM-LF-BENEFIT-CD       TO CVCDO HOLD-BENEFIT
001968     MOVE PI-LIFE-OVERRIDE-L6    TO CVDESCRO.
001969     MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM
001970                                    CVOTRMO.
001971     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
001972     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
001973     MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.
001974     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
001975     MOVE '4'                    TO CP-REM-TERM-METHOD.
001976     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
001977     PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
001978     MOVE CP-REMAINING-TERM-3    TO CVRTRMO.
001979     MOVE CM-LF-BENEFIT-AMT      TO CVOBENEO.
001980     MOVE CM-POLICY-FORM-NO      TO CVFORMO.
001981
001982     IF CM-LF-CURRENT-STATUS = '8'
001983        IF CM-LF-CANCEL-DT NOT = LOW-VALUES
001984            MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
001985            MOVE ' '             TO DC-OPTION-CODE
001986            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001987            IF NOT DATE-CONVERSION-ERROR
001988                MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
001989
001990     IF CM-LF-CURRENT-STATUS = '7'
001991        IF CM-LF-DEATH-DT NOT = LOW-VALUES
001992            MOVE CM-LF-DEATH-DT     TO DC-BIN-DATE-1
001993            MOVE SPACE              TO DC-OPTION-CODE
001994            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001995            IF NOT DATE-CONVERSION-ERROR
001996                MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
001997
001998     IF CM-LF-CURRENT-STATUS = '8'
001999        IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES
002000            MOVE CM-LF-CANCEL-EXIT-DT TO DC-BIN-DATE-1
002001            MOVE SPACE                TO DC-OPTION-CODE
002002            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002003            IF NOT DATE-CONVERSION-ERROR
002004                MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
002005
002006     IF CM-LF-CURRENT-STATUS = '7'
002007        IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
002008            MOVE CM-LF-DEATH-EXIT-DT  TO DC-BIN-DATE-1
002009            MOVE SPACE                TO DC-OPTION-CODE
002010            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002011            IF NOT DATE-CONVERSION-ERROR
002012                MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
002013
002014     IF CM-LF-CURRENT-STATUS = '1' OR '4'
002015        IF CP-REMAINING-TERM-3 = ZERO
002016           MOVE 'EXPIRED'           TO CVSTATO
002017        ELSE
002018           MOVE 'ACTIVE'            TO CVSTATO.
002019
002020     IF CM-LF-CURRENT-STATUS = '2'
002021        MOVE 'PEND   '           TO CVSTATO.
002022     IF CM-LF-CURRENT-STATUS = '3'
002023        MOVE 'RESTORE'           TO CVSTATO.
002024     IF CM-LF-CURRENT-STATUS = '5'
002025        MOVE 'REISSUE'           TO CVSTATO.
002026     IF CM-LF-CURRENT-STATUS = '6'
002027        MOVE 'LMP DIS'           TO CVSTATO.
002028     IF CM-LF-CURRENT-STATUS = '7'
002029        MOVE 'DEATH  '           TO CVSTATO.
002030     IF CM-LF-CURRENT-STATUS = '8'
002031        MOVE 'CANCEL '           TO CVSTATO.
002032     IF CM-LF-CURRENT-STATUS = '9'
002033        MOVE 'RE-ONLY'           TO CVSTATO.
002034     IF CM-LF-CURRENT-STATUS = 'V'
002035        MOVE 'VOID   '           TO CVSTATO.
002036     IF CM-LF-CURRENT-STATUS = 'D'
002037        MOVE 'DECLINE'           TO CVSTATO.
002038
002039     MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.
002040     MOVE PI-COMPANY-ID    TO BEN-CO-ID.
002041     MOVE '4'              TO BEN-REC-TYPE.
002042     MOVE CM-LF-BENEFIT-CD TO BEN-ACC-CD.
002043     MOVE ZEROS            TO BEN-SEQ-NO.
002044
002045     PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.
002046
002047     IF SCREEN-ERROR
002048         MOVE ER-0282 TO SCNERRO
002049         GO TO 4030-MOVE-CERT-AH.
002050
002051     MOVE ZEROS TO COUNT-2.
002052     PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.
002053     IF SCREEN-ERROR
002054         MOVE ER-0282 TO SCNERRO
002055         GO TO 4030-MOVE-CERT-AH.
002056
002057     MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.
002058
002059 4030-MOVE-CERT-AH.
002060
002061     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
002062                                          OR 'B' OR 'H'
002063         CONTINUE
002064     ELSE
002065         GO TO 4030-MOVE-REST-OF-CERT.
002066
002067     IF CM-AH-BENEFIT-CD = '00'
002068         GO TO 4030-MOVE-REST-OF-CERT.
002069
002070     MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.
002071
002072     MOVE CM-AH-BENEFIT-CD TO CVCDO HOLD-BENEFIT.
002073     MOVE CM-AH-ORIG-TERM        TO CP-ORIGINAL-TERM
002074                                    CVOTRMO.
002075     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
002076     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
002077     MOVE CURRENT-DATE-BIN       TO CP-VALUATION-DT.
002078     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
002079     MOVE '4'                    TO CP-REM-TERM-METHOD.
002080     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
002081
002082     PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
002083
002084     MOVE CP-REMAINING-TERM-3    TO CVRTRMO.
002085     MOVE CM-AH-BENEFIT-AMT      TO CVOBENEO.
002086     MOVE CM-POLICY-FORM-NO      TO CVFORMO.
002087
002088     IF CM-AH-CURRENT-STATUS = '8'
002089        IF CM-AH-CANCEL-DT NOT = LOW-VALUES
002090            MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
002091            MOVE ' '             TO DC-OPTION-CODE
002092            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002093            IF NOT DATE-CONVERSION-ERROR
002094                MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
002095
002096     IF CM-AH-CURRENT-STATUS = '6'
002097        IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
002098            MOVE CM-AH-SETTLEMENT-DT     TO DC-BIN-DATE-1
002099            MOVE ' '             TO DC-OPTION-CODE
002100            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002101            IF NOT DATE-CONVERSION-ERROR
002102                MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
002103
002104     IF CM-AH-CURRENT-STATUS = '8'
002105        IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES
002106            MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1
002107            MOVE ' '             TO DC-OPTION-CODE
002108            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002109            IF NOT DATE-CONVERSION-ERROR
002110                MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
002111
002112     IF CM-AH-CURRENT-STATUS = '6'
002113        IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
002114            MOVE CM-AH-SETTLEMENT-EXIT-DT     TO DC-BIN-DATE-1
002115            MOVE ' '             TO DC-OPTION-CODE
002116            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002117            IF NOT DATE-CONVERSION-ERROR
002118                MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
002119
002120     IF CM-AH-CURRENT-STATUS = '1' OR = '4'
002121        IF CP-REMAINING-TERM-3 = ZEROS
002122           MOVE 'EXPIRED'           TO CVSTATO
002123        ELSE
002124           MOVE 'ACTIVE'            TO CVSTATO.
002125
002126     IF CM-AH-CURRENT-STATUS = '2'
002127        MOVE 'PEND   '           TO CVSTATO.
002128     IF CM-AH-CURRENT-STATUS = '3'
002129        MOVE 'RESTORE'           TO CVSTATO.
002130     IF CM-AH-CURRENT-STATUS = '5'
002131        MOVE 'REISSUE'           TO CVSTATO.
002132     IF CM-AH-CURRENT-STATUS = '6'
002133        MOVE 'LMP DIS'           TO CVSTATO.
002134     IF CM-AH-CURRENT-STATUS = '7'
002135        MOVE 'DEATH  '           TO CVSTATO.
002136     IF CM-AH-CURRENT-STATUS = '8'
002137        MOVE 'CANCEL '           TO CVSTATO.
002138     IF CM-AH-CURRENT-STATUS = '9'
002139        MOVE 'RE-ONLY'           TO CVSTATO.
002140     IF CM-AH-CURRENT-STATUS = 'V'
002141        MOVE 'VOID   '           TO CVSTATO.
002142     IF CM-AH-CURRENT-STATUS = 'D'
002143        MOVE 'DECLINE'           TO CVSTATO.
002144
002145     MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.
002146     MOVE PI-COMPANY-ID    TO BEN-CO-ID.
002147     MOVE '5'              TO BEN-REC-TYPE.
002148     MOVE CM-AH-BENEFIT-CD TO BEN-ACC-CD.
002149     MOVE ZEROES           TO BEN-SEQ-NO.
002150
002151     PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.
002152
002153     IF SCREEN-ERROR
002154         PERFORM 4070-CHECK-ERROR THRU 4080-EXIT
002155         GO TO 4030-MOVE-REST-OF-CERT.
002156
002157     MOVE ZEROS TO COUNT-2.
002158     PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.
002159
002160     IF SCREEN-ERROR
002161         PERFORM 4070-CHECK-ERROR THRU 4080-EXIT
002162         GO TO 4030-MOVE-REST-OF-CERT.
002163
002164     MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.
002165
002166 4030-MOVE-REST-OF-CERT.
002167     MOVE CM-LOAN-NUMBER         TO LOANNOO.
002168     MOVE CM-LOAN-BALANCE        TO LOANBALO.
002169     MOVE CM-LOAN-APR            TO CAPRO.
002170     MOVE CM-IND-GRP-TYPE        TO CINDGRPO.
002171
002172     IF CM-SING-PRM
002173         MOVE 'SP'               TO CPREMTPO
002174     ELSE
002175         IF CM-O-B-COVERAGE
002176             MOVE 'OB'           TO CPREMTPO
002177         ELSE
002178             IF CM-OPEN-END
002179                 MOVE 'OE'       TO CPREMTPO
002180             ELSE
002181                 MOVE SPACES     TO CPREMTPO.
002182
002183     MOVE CM-REIN-TABLE          TO CREINCDO.
002184
002185 4030-EXIT.
002186     EXIT.
002187     EJECT
002188 4030-MOVE-EMPLCY.
002189
002190     IF PM-POLICY-EFF-DT > LOW-VALUES
002191         MOVE PM-POLICY-EFF-DT       TO  DC-BIN-DATE-1
002192         MOVE SPACES                 TO  DC-OPTION-CODE
002193                                         DC-ERROR-CODE
002194         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002195         MOVE DC-GREG-DATE-1-EDIT    TO  CERTEFFO
002196     ELSE
002197         MOVE SPACES                 TO  CERTEFFO.
002198
002199     MOVE PM-PRODUCER             TO  CERTACTO.
002200     MOVE PM-STATE                TO  CERTSTO PI-STATE.
002201     MOVE PM-GROUPING             TO  CERTGRPO.
002202     MOVE PM-CARRIER              TO  CERTCARO.
002203     MOVE PM-INSURED-LAST-NAME    TO  CLNAMEO.
002204     MOVE PM-INSURED-FIRST-NAME   TO  CFNAMEO.
002205     MOVE PM-INSURED-MIDDLE-INIT  TO  CINITO.
002206     MOVE PM-JOINT-LAST-NAME      TO  CJLNAMEO.
002207     MOVE PM-JOINT-FIRST-NAME     TO  CJFAMEO.
002208     MOVE PM-JOINT-MIDDLE-INIT    TO  CJINITO.
002209
002210     MOVE PM-INSURED-ISSUE-AGE    TO  WS-AGE.
002211     MOVE WS-AGE-3-4              TO  INSAGEO.
002212     MOVE PM-JOINT-ISSUE-AGE      TO  WS-AGE.
002213     MOVE WS-AGE-3-4              TO  JAGEO.
002214
002215     IF PM-SSN-STATE   = PM-STATE AND
002216        PM-SSN-PRODUCER = PM-PRODUCER-PRIME
002217         MOVE SPACES              TO  SOCSECO
002218     ELSE
002219         MOVE PM-SOC-SEC-NO       TO  SOCSECO.
002220
002221     PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.
002222
002223     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
002224         CONTINUE
002225     ELSE
002226         GO TO 4030-MOVE-EMPLCY-AH.
002227
002228     IF PP-BENEFIT-IS-LEVEL
002229         MOVE 'L'                TO  CP-BENEFIT-TYPE
002230     ELSE
002231         MOVE 'R'                TO  CP-BENEFIT-TYPE.
002232
002233     MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD
002234                                     CP-RATING-METHOD.
002235
002236     MOVE PI-LIFE-OVERRIDE-L6    TO  CVDESCRO.
002237     MOVE PM-INS-PLAN-CD         TO  CVCDO
002238                                     HOLD-BENEFIT.
002239     MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM
002240                                     CVOTRMO.
002241     MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT
002242                                     CP-FIRST-PAY-DATE.
002243     MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.
002244     MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.
002245     MOVE '2'                    TO  CP-REM-TERM-METHOD
002246                                     CP-PROCESS-TYPE.
002247     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
002248     MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.
002249
002250     PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
002251
002252     IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')
002253         COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1
002254         MOVE CP-REMAINING-TERM-3    TO  CVRTRMO
002255     ELSE
002256         MOVE CP-REMAINING-TERM-3    TO  CVRTRMO.
002257
002258     MOVE PM-INS-TOTAL-BENEFIT   TO  CVOBENEO.
002259
002260     GO TO 4030-MOVE-REST-OF-EMPLCY.
002261
002262 4030-MOVE-EMPLCY-AH.
002263
002264     MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.
002265
002266     IF PP-BENEFIT-IS-LEVEL
002267         MOVE 'L'                TO  CP-BENEFIT-TYPE
002268     ELSE
002269         MOVE 'R'                TO  CP-BENEFIT-TYPE.
002270
002271     MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD
002272                                     CP-RATING-METHOD.
002273
002274     MOVE PI-AH-OVERRIDE-L6      TO  CVDESCRO.
002275     MOVE PM-INS-PLAN-CD         TO  CVCDO
002276                                     HOLD-BENEFIT.
002277     MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM
002278                                     CVOTRMO.
002279     MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT.
002280     MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE.
002281     MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.
002282     MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.
002283     MOVE '2'                    TO  CP-PROCESS-TYPE.
002284     MOVE '3'                    TO  CP-REM-TERM-METHOD.
002285     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
002286     MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.
002287
002288     PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
002289
002290     MOVE CP-REMAINING-TERM-1    TO  CVRTRMO.
002291
002292     MOVE PM-INS-MONTH-BENEFIT   TO  CVOBENEO.
002293
002294 4030-MOVE-REST-OF-EMPLCY.
002295
002296     MOVE PM-INS-POLICY-FORM     TO  CVFORMO.
002297
002298     IF PM-CURRENT-STATUS IS EQUAL TO '7'
002299         IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES
002300             MOVE PM-CANCEL-DT           TO  DC-BIN-DATE-1
002301             MOVE SPACE                  TO  DC-OPTION-CODE
002302             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002303             IF NOT DATE-CONVERSION-ERROR
002304                 MOVE DC-GREG-DATE-1-EDIT TO  CVCNCDTO.
002305
002306     IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)
002307         MOVE ' '                        TO  DC-OPTION-CODE
002308         MOVE PM-EXIT-DT                 TO  DC-BIN-DATE-1
002309         MOVE SPACE                      TO  DC-OPTION-CODE
002310         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002311         IF NOT DATE-CONVERSION-ERROR
002312             MOVE DC-GREG-DATE-1-EDIT    TO  CVEXITO.
002313
002314
002315     IF PM-CURRENT-STATUS IS EQUAL TO '0'
002316         MOVE 'LAPSED'           TO  CVSTATO.
002317     IF PM-CURRENT-STATUS IS EQUAL TO '1'
002318         MOVE 'ACTIVE'           TO  CVSTATO.
002319     IF PM-CURRENT-STATUS IS EQUAL TO '2'
002320        MOVE 'PEND   '           TO  CVSTATO.
002321     IF PM-CURRENT-STATUS IS EQUAL TO '3'
002322        MOVE 'DECLINE'           TO  CVSTATO.
002323     IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')
002324        MOVE 'PNDCNC'            TO  CVSTATO.
002325     IF PM-CURRENT-STATUS IS EQUAL TO '5'
002326        MOVE 'PNDISS'            TO  CVSTATO.
002327     IF PM-CURRENT-STATUS IS EQUAL TO '6'
002328        MOVE 'CLAIM'             TO  CVSTATO.
002329     IF PM-CURRENT-STATUS IS EQUAL TO '7'
002330        MOVE 'CANCEL '           TO  CVSTATO.
002331     IF PM-CURRENT-STATUS IS EQUAL TO '8'
002332        MOVE 'PNDUNW '           TO  CVSTATO.
002333     IF PM-CURRENT-STATUS IS EQUAL TO 'C'
002334        MOVE 'TRNSFR '           TO  CVSTATO.
002335     IF PM-CURRENT-STATUS IS EQUAL TO 'F'
002336        MOVE 'SETTLE '           TO  CVSTATO.
002337     IF PM-CURRENT-STATUS IS EQUAL TO 'T'
002338        MOVE 'TRMNAT '           TO  CVSTATO.
002339
002340     MOVE PP-PLAN-ABBREV         TO  CVKINDO.
002341
002342     MOVE PM-LOAN-NUMBER         TO  LOANNOO.
002343     MOVE PM-LOAN-BALC           TO  LOANBALO.
002344     MOVE PM-LOAN-APR            TO  CAPRO.
002345     MOVE PM-INS-TYPE            TO  CINDGRPO.
002346     MOVE PM-BILLING-MODE        TO  CPREMTPO.
002347
002348 4030-EMPLCY-EXIT.
002349     EXIT.
002350     EJECT
002351 4040-FIND-BENEFIT.
002352     ADD 1 TO COUNT-2.
002353
002354     IF COUNT-2 > 8
002355         GO TO 4050-BENEFIT-NOTFND.
002356
002357     IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT
002358         GO TO 4060-EXIT.
002359
002360     IF CF-BENEFIT-CODE (COUNT-2) > HOLD-BENEFIT
002361         GO TO 4050-BENEFIT-NOTFND.
002362
002363     GO TO 4040-FIND-BENEFIT.
002364
002365 4050-BENEFIT-NOTFND.
002366     MOVE 'X' TO ERROR-SWITCH.
002367
002368 4060-EXIT.
002369     EXIT.
002370     EJECT
002371
002372 4070-CHECK-ERROR.
002373     IF SCNERRO > SPACES
002374         GO TO 4080-EXIT.
002375
002376     MOVE ER-0283 TO SCNERRO.
002377
002378 4080-EXIT.
002379     EXIT.
002380
002381     EJECT
002382 5000-START-BROWSE.
002383     
      * EXEC CICS HANDLE CONDITION
002384*        NOTFND (5010-BAD-KEY)
002385*        NOTOPEN (6000-MSTR-NOT-OPEN)
002386*    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00008554' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303038353534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002387
002388     
      * EXEC CICS STARTBR
002389*        DATASET (W-FILE-ID)
002390*        RIDFLD (MSTR-KEY)
002391*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008559' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038353539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002392
002393     GO TO 5020-EXIT.
002394
002395 5010-BAD-KEY.
002396     MOVE 'X' TO BUILD-SWITCH.
002397
002398 5020-EXIT.
002399     EXIT.
002400
002401 5030-READ-FILE.
002402     
      * EXEC CICS HANDLE CONDITION
002403*        ENDFILE (5040-END-OF-FILE)
002404*    END-EXEC.
      *    MOVE '"$''                   ! ( #00008573' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303038353733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002405
002406     
      * EXEC CICS READNEXT
002407*        INTO    (CLAIM-MASTER)
002408*        DATASET (W-FILE-ID)
002409*        RIDFLD  (MSTR-KEY)
002410*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008577' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002411
002412     GO TO 5050-EXIT.
002413
002414 5040-END-OF-FILE.
002415     MOVE 'Y' TO BUILD-SWITCH.
002416
002417 5050-EXIT.
002418     EXIT.
002419
002420 5060-END-BROWSE.
002421     
      * EXEC CICS ENDBR
002422*        DATASET (W-FILE-ID)
002423*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008592' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038353932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002424
002425 5070-EXIT.
002426     EXIT.
002427
002428 5080-READ-CERT.
002429     
      * EXEC CICS HANDLE CONDITION
002430*        NOTFND (5085-CERT-NOTFND)
002431*    END-EXEC.
      *    MOVE '"$I                   ! ) #00008600' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303038363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002432
002433     
      * EXEC CICS READ
002434*        SET (ADDRESS OF CERTIFICATE-MASTER)
002435*        DATASET ('ELCERT')
002436*        RIDFLD (CERT-KEY)
002437*    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008604' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002438
002439     GO TO 5090-EXIT.
002440
002441 5085-CERT-NOTFND.
002442     MOVE 'X' TO ERROR-SWITCH.
002443
002444 5090-EXIT.
002445     EXIT.
002446
002447 5095-READ-EMPLCY.
002448     
      * EXEC CICS HANDLE CONDITION
002449*        NOTFND (5095-EMPLCY-NOTFND)
002450*    END-EXEC.
      *    MOVE '"$I                   ! * #00008619' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303038363139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002451
002452     
      * EXEC CICS READ
002453*        SET       (ADDRESS OF POLICY-MASTER)
002454*        DATASET   ('MPPLCY')
002455*        RIDFLD    (EMPLCY-KEY)
002456*    END-EXEC.
           MOVE 'MPPLCY' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008623' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002457
002458     GO TO 5095-EXIT.
002459
002460 5095-EMPLCY-NOTFND.
002461     MOVE 'X' TO ERROR-SWITCH.
002462
002463 5095-EXIT.
002464     EXIT.
002465
002466 5096-READ-EMPLAN.
002467     
      * EXEC CICS HANDLE CONDITION
002468*        NOTFND (5096-EMPLAN-NOTFND)
002469*    END-EXEC.
      *    MOVE '"$I                   ! + #00008638' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303038363338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002470
002471     
      * EXEC CICS READ
002472*        SET       (ADDRESS OF PRODUCER-PLANS)
002473*        DATASET   ('MPPLAN')
002474*        RIDFLD    (EMPLAN-KEY)
002475*    END-EXEC.
           MOVE 'MPPLAN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008642' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002476
002477     GO TO 5096-EXIT.
002478
002479 5096-EMPLAN-NOTFND.
002480     MOVE 'X' TO ERROR-SWITCH.
002481
002482 5096-EXIT.
002483     EXIT.
002484
002485 5100-READ-TRLR.
002486     
      * EXEC CICS HANDLE CONDITION
002487*        NOTFND (5105-TRLR-NOTFND)
002488*    END-EXEC.
      *    MOVE '"$I                   ! , #00008657' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303038363537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002489
002490     
      * EXEC CICS READ
002491*        SET (ADDRESS OF ACTIVITY-TRAILERS)
002492*        DATASET ('ELTRLR')
002493*        RIDFLD (TRLR-KEY)
002494*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008661' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002495
002496     GO TO 5110-EXIT.
002497
002498 5105-TRLR-NOTFND.
002499     MOVE 'X' TO ERROR-SWITCH.
002500
002501 5110-EXIT.
002502     EXIT.
002503
002504 5120-READ-CNTL.
002505     
      * EXEC CICS READ
002506*        SET (ADDRESS OF CONTROL-FILE)
002507*        DATASET ('ELCNTL')
002508*        RIDFLD (CNTL-KEY)
002509*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008676' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002510
002511     MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID.
002512
002513 5130-EXIT.
002514     EXIT.
002515
002516 5140-WRITE-TS.
002517     ADD 1 TO COUNT-1.
002518     
      * EXEC CICS WRITEQ TS
002519*        QUEUE (PI-EL1602-KEY)
002520*        FROM (EL160BO)
002521*        LENGTH (EL160B-LENGTH)
002522*        ITEM (COUNT-1)
002523*    END-EXEC.
      *    MOVE '*" I   L              ''   #00008689' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038363839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL160B-LENGTH, 
                 COUNT-1, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002524
002525 5150-EXIT.
002526     EXIT.
002527
002528 5240-WRITE-TS-160A.
002529     
      * EXEC CICS WRITEQ TS
002530*        QUEUE (PI-EL160-KEY)
002531*        FROM (EL160AO)
002532*        LENGTH (EL160A-LENGTH)
002533*    END-EXEC.
      *    MOVE '*"     L              ''   #00008700' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038373030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 EL160AO, 
                 EL160A-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002534
002535 5250-EXIT.
002536     EXIT.
002537
002538 5260-READ-BENEFIT.
002539     
      * EXEC CICS HANDLE CONDITION
002540*        NOTFND (5270-BENEFIT-NOTFND)
002541*    END-EXEC.
      *    MOVE '"$I                   ! - #00008710' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303038373130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002542
002543     
      * EXEC CICS READ
002544*        SET (ADDRESS OF CONTROL-FILE)
002545*        DATASET ('ELCNTL')
002546*        RIDFLD (BENEFIT-KEY)
002547*        GTEQ
002548*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00008714' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303038373134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002549
002550     IF CF-RECORD-TYPE = BEN-REC-TYPE
002551         GO TO 5280-EXIT.
002552
002553 5270-BENEFIT-NOTFND.
002554     MOVE 'X' TO ERROR-SWITCH.
002555
002556 5280-EXIT.
002557     EXIT.
002558     EJECT
002559 6000-MSTR-NOT-OPEN.
002560     MOVE ER-0154 TO EMI-ERROR.
002561     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002562     MOVE -1 TO CARRSL.
002563     GO TO 8110-SEND-DATA.
002564
002565 6030-CNTL-NOT-OPEN.
002566     MOVE ER-0042 TO EMI-ERROR.
002567     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002568     MOVE -1 TO CARRSL.
002569     GO TO 8110-SEND-DATA.
002570
002571     EJECT
002572 8100-SEND-MAP.
002573     IF  PI-CARRIER-SECURITY > SPACES
002574         MOVE PI-CARRIER-SECURITY TO CARRSO
002575         MOVE AL-SANON            TO CARRSA.
002576
002577     IF  PI-ACCOUNT-SECURITY > SPACES
002578         MOVE PI-ACCOUNT-SECURITY TO ACCTSO
002579         MOVE AL-SANON            TO ACCTSA.
002580
002581     IF  PI-ACCOUNT-SECURITY > SPACES        OR
002582         PI-CARRIER-SECURITY > SPACES
002583         MOVE 'N'                TO PRTOPTO
002584         MOVE AL-SANON           TO PRTOPTA
002585         MOVE ER-2381         TO EMI-ERROR
002586         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002587
002588     PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
002589
002590     
      * EXEC CICS SEND
002591*        MAP ('EL160A')
002592*        MAPSET ('EL160S')
002593*        ERASE
002594*        FREEKB
002595*        CURSOR
002596*    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00008761' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'204620204820202020202C20' &
                X'2020233030303038373631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002597
002598     GO TO 9000-RETURN-TRANS.
002599
002600 8110-SEND-DATA.
002601     IF  PI-CARRIER-SECURITY > SPACES
002602         MOVE PI-CARRIER-SECURITY TO CARRSO
002603         MOVE AL-SANON            TO CARRSA.
002604
002605     IF  PI-ACCOUNT-SECURITY > SPACES
002606         MOVE PI-ACCOUNT-SECURITY TO ACCTSO
002607         MOVE AL-SANON            TO ACCTSA.
002608
002609     IF  PI-ACCOUNT-SECURITY > SPACES        OR
002610         PI-CARRIER-SECURITY > SPACES
002611         MOVE 'N'                TO PRTOPTO
002612         MOVE AL-SANON           TO PRTOPTA
002613         MOVE ER-2381         TO EMI-ERROR
002614         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002615
002616     PERFORM 8120-FORMAT-TIME-DATE
002617             THRU 8130-EXIT.
002618
002619     
      * EXEC CICS SEND
002620*        MAP ('EL160A')
002621*        MAPSET ('EL160S')
002622*        DATAONLY
002623*        FREEKB
002624*        CURSOR
002625*    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00008790' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'204620204820202020202C20' &
                X'2020233030303038373930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002626
002627     GO TO 9000-RETURN-TRANS.
002628
002629 8120-FORMAT-TIME-DATE.
002630     MOVE SAVE-DATE      TO DATEO.
002631     
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
002632*    END-EXEC
      *    MOVE '0"A                   "   #00008802' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303038383032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002633     
      * EXEC CICS FORMATTIME
002634*              ABSTIME(LCP-CICS-TIME)
002635*              TIME(LCP-TIME-OF-DAY-XX)
002636*    END-EXEC
      *    MOVE 'j$(     (             #   #00008804' TO DFHEIV0
           MOVE X'6A2428202020202028202020' &
                X'202020202020202020202320' &
                X'2020233030303038383034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002637     MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.
002638     MOVE UN-HOURS       TO FOR-HOURS.
002639     MOVE UN-MINUTES     TO FOR-MINUTES.
002640     MOVE TIME-OUT       TO TIMEO.
002641     MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.
002642     MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
002643     MOVE EMI-MESSAGE-AREA (1) TO MSG1O.
002644
002645 8130-EXIT.
002646     EXIT.
002647
002648 8200-RETURN-PRIOR.
002649     MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.
002650     GO TO 9200-XCTL.
002651
002652 8300-GET-HELP.
002653     MOVE XCTL-EL010 TO CALL-PGM.
002654     GO TO 9200-XCTL.
002655
002656 8400-RETURN-MASTER.
002657     MOVE XCTL-EL126 TO CALL-PGM.
002658     GO TO 9200-XCTL.
002659
002660 8800-UNAUTHORIZED-ACCESS.
002661     MOVE UNACCESS-MSG TO LOGOFF-MSG.
002662     GO TO 8990-SEND-TEXT.
002663
002664 8810-PF23-ENTERED.
002665     MOVE EIBAID TO PI-ENTRY-CD-1.
002666     MOVE XCTL-EL005 TO CALL-PGM.
002667     GO TO 9200-XCTL.
002668
002669 8820-XCTL-ERROR.
002670     
      * EXEC CICS HANDLE CONDITION
002671*        PGMIDERR (8990-SEND-TEXT)
002672*    END-EXEC.
      *    MOVE '"$L                   ! . #00008841' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303038383431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002673
002674     MOVE SPACE        TO PI-ENTRY-CD-1.
002675     MOVE CALL-PGM     TO PI-CALLING-PROGRAM LOGOFF-PGM
002676     MOVE XCTL-EL005 TO CALL-PGM.
002677     MOVE PGMIDERR-MSG TO LOGOFF-FILL.
002678     GO TO 9200-XCTL.
002679
002680 8990-SEND-TEXT.
002681     
      * EXEC CICS SEND TEXT
002682*        FROM (LOGOFF-TEXT)
002683*        LENGTH (LOGOFF-LENGTH)
002684*        ERASE
002685*        FREEKB
002686*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00008852' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303038383532' TO DFHEIV0
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
           
002687
002688     GO TO 9100-RETURN-CICS.
002689     EJECT
002690 9000-RETURN-TRANS.
002691     
      * EXEC CICS RETURN
002692*        TRANSID (TRANS-ID)
002693*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
002694*        LENGTH (PI-COMM-LENGTH)
002695*    END-EXEC.
      *    MOVE '.(CT                  ''   #00008862' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038383632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002696     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
002697
002698 9100-RETURN-CICS.
002699     
      * EXEC CICS RETURN
002700*    END-EXEC.
      *    MOVE '.(                    ''   #00008870' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038383730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002701     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
002702
002703 9200-XCTL.
002704     
      * EXEC CICS XCTL
002705*        PROGRAM (CALL-PGM)
002706*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
002707*        LENGTH (PI-COMM-LENGTH)
002708*    END-EXEC.
      *    MOVE '.$C                   %   #00008875' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038383735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002709
002710 9300-GET-FREE-LOOK.
002711
002712     MOVE PI-COMPANY-ID TO COMPANY-ID.
002713     MOVE '3'           TO RECORD-TYPE.
002714     MOVE PI-STATE      TO ACCESS-CD-GENL.
002715     MOVE ZEROS         TO SEQUENCE-NO.
002716
002717     
      * EXEC CICS READ
002718*        SET (ADDRESS OF CONTROL-FILE)
002719*        DATASET ('ELCNTL')
002720*        RIDFLD (CNTL-KEY)
002721*        RESP   (WS-RESPONSE)
002722*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008888' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038383838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002723
002724     IF WS-RESP-NOTFND
002725        MOVE ER-2848   TO EMI-ERROR
002726        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002727        GO TO 8110-SEND-DATA
002728     ELSE
002729        MOVE CF-ST-FREE-LOOK-PERIOD
002730                        TO CP-FREE-LOOK.
002731
002732 9300-EXIT.
002733     EXIT.
002734
002735 9700-LINK-REM-TERM.
002736     
      * EXEC CICS LINK
002737*        PROGRAM (REM-TERM-PGM)
002738*        COMMAREA (CALCULATION-PASS-AREA)
002739*        LENGTH (CP-COMM-LENGTH)
002740*    END-EXEC.
      *    MOVE '."C                   (   #00008907' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REM-TERM-PGM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002741
002742 9700-EXIT.
002743     EXIT.
002744
002745 9800-CONVERT-DATE.
002746     
      * EXEC CICS LINK
002747*        PROGRAM    (DATE-CONV)
002748*        COMMAREA   (DATE-CONVERSION-DATA)
002749*        LENGTH     (DC-COMM-LENGTH)
002750*    END-EXEC.
      *    MOVE '."C                   (   #00008917' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002751
002752 9800-EXIT.
002753     EXIT.
002754
002755 9900-ERROR-FORMAT.
002756     IF EMI-ERRORS-COMPLETE
002757         GO TO 9900-EXIT.
002758
002759     
      * EXEC CICS LINK
002760*        PROGRAM ('EL001')
002761*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
002762*        LENGTH (EMI-COMM-LENGTH)
002763*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00008930' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002764
002765 9900-EXIT.
002766     EXIT.
002767
002768 9990-ABEND.
002769     MOVE DFHEIBLK TO EMI-LINE1.
002770
002771     
      * EXEC CICS LINK
002772*        PROGRAM   ('EL004')
002773*        COMMAREA  (EMI-LINE1)
002774*        LENGTH    (72)
002775*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008942' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002776
002777     GO TO 8110-SEND-DATA.
002778
002779 9995-SECURITY-VIOLATION.
002780*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00008969' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038393639' TO DFHEIV0
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
002781
002782 9995-EXIT.
002783     EXIT.
002784

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0410-TS-NOTFND,
                     0420-FIND-SCREEN-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0440-DELETE-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0450-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1070-PROC-NOTFND,
                     6030-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5010-BAD-KEY,
                     6000-MSTR-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5040-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5085-CERT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5095-EMPLCY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5096-EMPLAN-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5105-TRLR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5270-BENEFIT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
