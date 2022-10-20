      *((program: EL1275.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1275.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 11/21/95 10:11:33.
000007*                            VMOD=2.032.
000008*
000009*
000010*AUTHOR.    LOGIC, INC.
000011*           DALLAS, TEXAS.
000012
000013*REMARKS.  TRANSACTION - EXX5
000014
000015*        CERT COVERAGES DISPLAY PROGRAM.
000016
000017*    SCREENS     - EL127E - CERTIFICATE PROFILE
000018
000019*    ENTERED BY  - EL1273 - CERTIFICATE UPDATE
000020*                - EL1274 - CERTIFICATE PROFILE
000021
000022*    EXIT TO     - CALLING PROGRAM
000023*                - EL132  - CLAIM LOOK-UP
000024
000025*    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
000026*                  ELCNTL - CONTROL FILE
000027
000028*    NARRATIVE   - PROGRAM USES THE KEY TO THE CERTIFICATE MASTER
000029*                  PASSED IN THE COMMAREA TO DISPLAY THE
000030*                  COVERAGES ASSOCIATED WITH A CERTIFICATE AND
000031*                  RETURNS WITH THE TRANSACTION OF THE CALLING
000032*                  PROGRAM.
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000042* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
000043* 122002    2002111500006  PEMA  ADD MONTHLY PROCESSING
000044* 101509    2008100900003  AJRA  CALL NEW CERT NOTE SCREEN
000045* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000046* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
000047* 092118  CR2018073000001  PEMA  Add lf & ah refund method
000048* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000049* 080322  CR2021100800003  TANA  Add B and H claim types
000050******************************************************************
000051
000052
000053     EJECT
000054 ENVIRONMENT DIVISION.
000055
000056 DATA DIVISION.
000057
000058 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000059
000060 77  FILLER  PIC X(32)  VALUE '********************************'.
000061 77  FILLER  PIC X(32)  VALUE '*   EL1275 WORKING STORAGE     *'.
000062 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.032 *********'.
000063 77  P1                          PIC S999 COMP-3 VALUE +0.
000064 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000065 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
000066
000067
000068 01  WS-DATE-AREA.
000069     05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
000070     05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
000071     05  WS-VALUATION-DT             PIC X(2)    VALUE LOW-VALUES.
000072
000073 01  ws-elcrtt-sw                pic x  value spaces.
000074     88  good-hit-on-trlr          value 'Y'.
000075     88  crtt-not-found            value 'N'.
000076     88  crtt-not-read             value ' '.
000077
000078 01  WS-RESPONSE                 PIC S9(8)       COMP.
000079     88  RESP-NORMAL                 VALUE +00.
000080     88  RESP-NOTFND                 VALUE +13.
000081     88  RESP-DUPKEY                 VALUE +15.
000082     88  RESP-NOTOPEN                VALUE +19.
000083     88  RESP-ENDFILE                VALUE +20.
000084
000085 01  FILLER                          COMP-3.
000086     05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.
000087     05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.
000088     05  WS-TIME                     REDEFINES
000089         WS-TIME-WORK                PIC S9(3)V9(4).
000090     05  WS-NOT-FOUND                PIC S9       VALUE ZERO.
000091         88  BENEFIT-FOUND                        VALUE +1.
000092     05  ONE-MON-EARNED              PIC S9(7)V99 VALUE +0.
000093     05  WS-CALC-REFUND              PIC S9(7)V99 VALUE +0.
000094     05  WS-TERM                     PIC S9(4)    VALUE +0.
000095     05  WS-REMAINING-AMT            PIC S9(9)V99 VALUE +0.
000096     05  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
000097         88  PDEF-FOUND                          VALUE 'Y'.
000098
000099
000100 01  ERPDEF-KEY-SAVE             PIC X(18).
000101 01  ERPDEF-KEY.
000102     12  ERPDEF-COMPANY-CD       PIC X.
000103     12  ERPDEF-STATE            PIC XX.
000104     12  ERPDEF-PROD-CD          PIC XXX.
000105     12  F                       PIC X(7).
000106     12  ERPDEF-BEN-TYPE         PIC X.
000107     12  ERPDEF-BEN-CODE         PIC XX.
000108     12  ERPDEF-EXP-DT           PIC XX.
000109
000110 01  FILLER.
000111     05  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
000112         88  BROWSE-STARTED                      VALUE 'Y'.
000113     05  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
000114         88  ACCT-FOUND                          VALUE 'Y'.
000115     05  WS-CALC-CD                  PIC X.
000116
000117     05  WS-BENEFIT-TYPE             PIC X        VALUE SPACE.
000118     05  WS-BENEFIT-NO               PIC XX       VALUE ZERO.
000119     05  WS-BENEFIT-DESCRIP          PIC X(10)    VALUE SPACES.
000120     05  WS-COMMENT                  PIC X(10)    VALUE SPACES.
000121     05  WS-KIND                     PIC X(3)     VALUE SPACES.
000122     05  WS-LF-COVERAGE-TYPE         PIC X        VALUE SPACE.
000123     05  WS-BENEFIT-CODE             PIC X(02)    VALUE SPACES.
000124     05  WS-EARNINGS-CALC            PIC X(01)    VALUE SPACE.
000125     05  WS-OVRD-EARNINGS-CALC       PIC X(01)    VALUE SPACE.
000126     05  WS-STATE-ABBREV             PIC X(02)    VALUE SPACES.
000127     05  WS-CLAIM-SW                 PIC X        VALUE SPACE.
000128         88  CHECK-AH-CLAIM          VALUE 'A'.
000129         88  CHECK-LF-CLAIM          VALUE 'L'.
000130
000131     05  WS-BROWSE-SW                PIC X        VALUE SPACE.
000132     05  WS-FREE-LOOK                PIC X        VALUE SPACE.
000133
000134     05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1275S'.
000135     05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127E'.
000136     05  WS-MAP-NUMBER               PIC X(4)     VALUE '127E'.
000137
000138     05  WS-PROGRAM-ID               PIC X(8)     VALUE 'EL1275'.
000139
000140     05  QID.
000141         10  QID-TERM                PIC X(04)    VALUE SPACES.
000142         10  FILLER                  PIC X(04)    VALUE '127E'.
000143
000144     05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX5'.
000145     05  EL001                       PIC X(8)     VALUE 'EL001'.
000146     05  EL004                       PIC X(8)     VALUE 'EL004'.
000147     05  EL005                       PIC X(8)     VALUE 'EL005'.
000148     05  EL010                       PIC X(8)     VALUE 'EL010'.
000149     05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.
000150     05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.
000151     05  ELRAMT                      PIC X(8)     VALUE 'ELRAMT'.
000152     05  ELRFND                      PIC X(8)     VALUE 'ELRFND'.
000153
000154     05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
000155     05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELCERT'.
000156     05  WS-CLAIM-MASTER-DSID        PIC X(8)     VALUE 'ELMSTR5'.
000157
000158     05  DEEDIT-FIELD                PIC X(15).
000159     05  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
000160
000161     05  WS-ACCOUNT.
000162         10  FILLER                  PIC X(4).
000163         10  WS-ACCT                 PIC X(6).
000164
000165     05  WS-INDEX                    PIC S9(4)    VALUE ZERO
000166                                     COMP
000167                                     SYNC.
000168
000169     EJECT
000170     05  ER-0008                     PIC X(4)    VALUE '0008'.
000171     05  ER-0029                     PIC X(4)    VALUE '0029'.
000172     05  ER-0142                     PIC X(4)    VALUE '0142'.
000173     05  ER-7048                     PIC X(4)    VALUE '7048'.
000174     05  ER-7237                     PIC X(4)    VALUE '7237'.
000175
000176     05  WS-CONTROL-FILE-KEY.
000177         10  WS-CFK-COMPANY-ID       PIC X(3)     VALUE SPACES.
000178         10  WS-CFK-RECORD-TYPE      PIC X        VALUE ZERO.
000179*          88  STATE-MASTER                       VALUE '3'.
000180*          88  LF-BENEFIT-MASTER                  VALUE '4'.
000181*          88  AH-BENEFIT-MASTER                  VALUE '5'.
000182*          88  CARRIER-MASTER                     VALUE '6'.
000183         10  WS-CFK-ACCESS.
000184             15  WS-CFK-STATE        PIC XX       VALUE SPACES.
000185             15  WS-CFK-BENEFIT-NO                VALUE SPACES.
000186                 20  FILLER          PIC X.
000187                 20  WS-CFK-CARRIER  PIC X.
000188         10  WS-CFK-SEQUENCE-NO      PIC S9(4)    VALUE ZERO COMP.
000189
000190     05  WS-CERTIFICATE-KEY.
000191         10  WS-CK-COMPANY-CD        PIC X.
000192         10  WS-CK-CARRIER           PIC X.
000193         10  WS-CK-GROUPING          PIC X(6).
000194         10  WS-CK-STATE             PIC XX.
000195         10  WS-CK-ACCOUNT           PIC X(10).
000196         10  WS-CK-CERT-EFF-DT       PIC XX.
000197         10  WS-CK-CERT-NO.
000198             15  WS-CK-CERT-PRIME    PIC X(10).
000199             15  WS-CK-CERT-SFX      PIC X.
000200
000201     05  WS-CLAIM-KEY.
000202         10  WS-CL-COMPANY-CD        PIC X.
000203         10  WS-CL-CERT-NO.
000204             15  WS-CL-CERT-PRIME    PIC X(10).
000205             15  WS-CL-CERT-SFX      PIC X.
000206
000207     05  WS-ELCRTT-KEY.
000208         10  WS-CS-COMPANY-CD        PIC X.
000209         10  WS-CS-CARRIER           PIC X.
000210         10  WS-CS-GROUPING          PIC X(6).
000211         10  WS-CS-STATE             PIC XX.
000212         10  WS-CS-ACCOUNT           PIC X(10).
000213         10  WS-CS-CERT-EFF-DT       PIC XX.
000214         10  WS-CS-CERT-NO.
000215             15  WS-CS-CERT-PRIME    PIC X(10).
000216             15  WS-CS-CERT-SFX      PIC X.
000217         10  WS-CS-REC-TYPE          PIC X.
000218
000219     05  WS-ACCT-KEY.
000220         10  WS-AK-COMPANY-CD        PIC X.
000221         10  WS-AK-CARRIER           PIC X.
000222         10  WS-AK-GROUPING          PIC X(6).
000223         10  WS-AK-STATE             PIC XX.
000224         10  WS-AK-ACCOUNT           PIC X(10).
000225         10  WS-AK-ACCT-EXP-DT       PIC XX.
000226
000227
000228*                                COPY ERCACCT.
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
000229*                                COPY ELCCRTT.
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
000230
000231*                                    COPY ELCINTF.
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
000232     12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
000233         16  FILLER              PIC X(314).
000234         16  PI-1ST-TIME-SW      PIC X.
000235         16  PI-BENEFIT-IND      PIC X.
000236         16  PI-PREV-BENEFIT     PIC X.
000237         16  PI-PEND-SW          PIC X.
000238         16  PI-CLAIM-SW         PIC X.
000239         16  FILLER              PIC X(321).
000240
000241*                                    COPY ELCDATE.
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
000242*                                    COPY EL1275S.
      *>>((file: EL1275S))
000001 01  EL127EI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  EDATEL PIC S9(0004) COMP.
000005     05  EDATEF PIC  X(0001).
000006     05  FILLER REDEFINES EDATEF.
000007         10  EDATEA PIC  X(0001).
000008     05  EDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  ETIMEL PIC S9(0004) COMP.
000011     05  ETIMEF PIC  X(0001).
000012     05  FILLER REDEFINES ETIMEF.
000013         10  ETIMEA PIC  X(0001).
000014     05  ETIMEI PIC  X(0005).
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
000028     05  EMEMCAPL PIC S9(0004) COMP.
000029     05  EMEMCAPF PIC  X(0001).
000030     05  FILLER REDEFINES EMEMCAPF.
000031         10  EMEMCAPA PIC  X(0001).
000032     05  EMEMCAPI PIC  X(0010).
000033*    -------------------------------
000034     05  EASRISKL PIC S9(0004) COMP.
000035     05  EASRISKF PIC  X(0001).
000036     05  FILLER REDEFINES EASRISKF.
000037         10  EASRISKA PIC  X(0001).
000038     05  EASRISKI PIC  X(0001).
000039*    -------------------------------
000040     05  ECERTNOL PIC S9(0004) COMP.
000041     05  ECERTNOF PIC  X(0001).
000042     05  FILLER REDEFINES ECERTNOF.
000043         10  ECERTNOA PIC  X(0001).
000044     05  ECERTNOI PIC  X(0010).
000045*    -------------------------------
000046     05  ECRTSFXL PIC S9(0004) COMP.
000047     05  ECRTSFXF PIC  X(0001).
000048     05  FILLER REDEFINES ECRTSFXF.
000049         10  ECRTSFXA PIC  X(0001).
000050     05  ECRTSFXI PIC  X(0001).
000051*    -------------------------------
000052     05  EACCTNOL PIC S9(0004) COMP.
000053     05  EACCTNOF PIC  X(0001).
000054     05  FILLER REDEFINES EACCTNOF.
000055         10  EACCTNOA PIC  X(0001).
000056     05  EACCTNOI PIC  X(0010).
000057*    -------------------------------
000058     05  ESTATEL PIC S9(0004) COMP.
000059     05  ESTATEF PIC  X(0001).
000060     05  FILLER REDEFINES ESTATEF.
000061         10  ESTATEA PIC  X(0001).
000062     05  ESTATEI PIC  X(0002).
000063*    -------------------------------
000064     05  ECARIERL PIC S9(0004) COMP.
000065     05  ECARIERF PIC  X(0001).
000066     05  FILLER REDEFINES ECARIERF.
000067         10  ECARIERA PIC  X(0001).
000068     05  ECARIERI PIC  X(0001).
000069*    -------------------------------
000070     05  EGROUPL PIC S9(0004) COMP.
000071     05  EGROUPF PIC  X(0001).
000072     05  FILLER REDEFINES EGROUPF.
000073         10  EGROUPA PIC  X(0001).
000074     05  EGROUPI PIC  X(0006).
000075*    -------------------------------
000076     05  EEFFDTL PIC S9(0004) COMP.
000077     05  EEFFDTF PIC  X(0001).
000078     05  FILLER REDEFINES EEFFDTF.
000079         10  EEFFDTA PIC  X(0001).
000080     05  EEFFDTI PIC  X(0008).
000081*    -------------------------------
000082     05  EMEMNOL PIC S9(0004) COMP.
000083     05  EMEMNOF PIC  X(0001).
000084     05  FILLER REDEFINES EMEMNOF.
000085         10  EMEMNOA PIC  X(0001).
000086     05  EMEMNOI PIC  X(0012).
000087*    -------------------------------
000088     05  ECLPSTL PIC S9(0004) COMP.
000089     05  ECLPSTF PIC  X(0001).
000090     05  FILLER REDEFINES ECLPSTF.
000091         10  ECLPSTA PIC  X(0001).
000092     05  ECLPSTI PIC  X(0002).
000093*    -------------------------------
000094     05  ECLPL PIC S9(0004) COMP.
000095     05  ECLPF PIC  X(0001).
000096     05  FILLER REDEFINES ECLPF.
000097         10  ECLPA PIC  X(0001).
000098     05  ECLPI PIC  X(0009).
000099*    -------------------------------
000100     05  EACLPL PIC S9(0004) COMP.
000101     05  EACLPF PIC  X(0001).
000102     05  FILLER REDEFINES EACLPF.
000103         10  EACLPA PIC  X(0001).
000104     05  EACLPI PIC  X(0009).
000105*    -------------------------------
000106     05  EKINDL PIC S9(0004) COMP.
000107     05  EKINDF PIC  X(0001).
000108     05  FILLER REDEFINES EKINDF.
000109         10  EKINDA PIC  X(0001).
000110     05  EKINDI PIC  X(0012).
000111*    -------------------------------
000112     05  ECODEL PIC S9(0004) COMP.
000113     05  ECODEF PIC  X(0001).
000114     05  FILLER REDEFINES ECODEF.
000115         10  ECODEA PIC  X(0001).
000116     05  ECODEI PIC  X(0002).
000117*    -------------------------------
000118     05  EDESCL PIC S9(0004) COMP.
000119     05  EDESCF PIC  X(0001).
000120     05  FILLER REDEFINES EDESCF.
000121         10  EDESCA PIC  X(0001).
000122     05  EDESCI PIC  X(0010).
000123*    -------------------------------
000124     05  ETERML PIC S9(0004) COMP.
000125     05  ETERMF PIC  X(0001).
000126     05  FILLER REDEFINES ETERMF.
000127         10  ETERMA PIC  X(0001).
000128     05  ETERMI PIC  X(0003).
000129*    -------------------------------
000130     05  ERTERML PIC S9(0004) COMP.
000131     05  ERTERMF PIC  X(0001).
000132     05  FILLER REDEFINES ERTERMF.
000133         10  ERTERMA PIC  X(0001).
000134     05  ERTERMI PIC  X(0003).
000135*    -------------------------------
000136     05  ECOMENTL PIC S9(0004) COMP.
000137     05  ECOMENTF PIC  X(0001).
000138     05  FILLER REDEFINES ECOMENTF.
000139         10  ECOMENTA PIC  X(0001).
000140     05  ECOMENTI PIC  X(0010).
000141*    -------------------------------
000142     05  ETRMDAYL PIC S9(0004) COMP.
000143     05  ETRMDAYF PIC  X(0001).
000144     05  FILLER REDEFINES ETRMDAYF.
000145         10  ETRMDAYA PIC  X(0001).
000146     05  ETRMDAYI PIC  X(0005).
000147*    -------------------------------
000148     05  EEXTDAYL PIC S9(0004) COMP.
000149     05  EEXTDAYF PIC  X(0001).
000150     05  FILLER REDEFINES EEXTDAYF.
000151         10  EEXTDAYA PIC  X(0001).
000152     05  EEXTDAYI PIC  X(0005).
000153*    -------------------------------
000154     05  EENTSTL PIC S9(0004) COMP.
000155     05  EENTSTF PIC  X(0001).
000156     05  FILLER REDEFINES EENTSTF.
000157         10  EENTSTA PIC  X(0001).
000158     05  EENTSTI PIC  X(0010).
000159*    -------------------------------
000160     05  EPREML PIC S9(0004) COMP.
000161     05  EPREMF PIC  X(0001).
000162     05  FILLER REDEFINES EPREMF.
000163         10  EPREMA PIC  X(0001).
000164     05  EPREMI PIC  X(0011).
000165*    -------------------------------
000166     05  EALTPRML PIC S9(0004) COMP.
000167     05  EALTPRMF PIC  X(0001).
000168     05  FILLER REDEFINES EALTPRMF.
000169         10  EALTPRMA PIC  X(0001).
000170     05  EALTPRMI PIC  X(0011).
000171*    -------------------------------
000172     05  ECURSTL PIC S9(0004) COMP.
000173     05  ECURSTF PIC  X(0001).
000174     05  FILLER REDEFINES ECURSTF.
000175         10  ECURSTA PIC  X(0001).
000176     05  ECURSTI PIC  X(0010).
000177*    -------------------------------
000178     05  EBENEL PIC S9(0004) COMP.
000179     05  EBENEF PIC  X(0001).
000180     05  FILLER REDEFINES EBENEF.
000181         10  EBENEA PIC  X(0001).
000182     05  EBENEI PIC  X(0013).
000183*    -------------------------------
000184     05  EALTBENL PIC S9(0004) COMP.
000185     05  EALTBENF PIC  X(0001).
000186     05  FILLER REDEFINES EALTBENF.
000187         10  EALTBENA PIC  X(0001).
000188     05  EALTBENI PIC  X(0012).
000189*    -------------------------------
000190     05  EREMBENL PIC S9(0004) COMP.
000191     05  EREMBENF PIC  X(0001).
000192     05  FILLER REDEFINES EREMBENF.
000193         10  EREMBENA PIC  X(0001).
000194     05  EREMBENI PIC  X(0013).
000195*    -------------------------------
000196     05  EEXPDTL PIC S9(0004) COMP.
000197     05  EEXPDTF PIC  X(0001).
000198     05  FILLER REDEFINES EEXPDTF.
000199         10  EEXPDTA PIC  X(0001).
000200     05  EEXPDTI PIC  X(0008).
000201*    -------------------------------
000202     05  EREINSPL PIC S9(0004) COMP.
000203     05  EREINSPF PIC  X(0001).
000204     05  FILLER REDEFINES EREINSPF.
000205         10  EREINSPA PIC  X(0001).
000206     05  EREINSPI PIC  X(0012).
000207*    -------------------------------
000208     05  ECRITPDL PIC S9(0004) COMP.
000209     05  ECRITPDF PIC  X(0001).
000210     05  FILLER REDEFINES ECRITPDF.
000211         10  ECRITPDA PIC  X(0001).
000212     05  ECRITPDI PIC  X(0003).
000213*    -------------------------------
000214     05  EUEDTL PIC S9(0004) COMP.
000215     05  EUEDTF PIC  X(0001).
000216     05  FILLER REDEFINES EUEDTF.
000217         10  EUEDTA PIC  X(0001).
000218     05  EUEDTI PIC  X(0008).
000219*    -------------------------------
000220     05  EUEPREML PIC S9(0004) COMP.
000221     05  EUEPREMF PIC  X(0001).
000222     05  FILLER REDEFINES EUEPREMF.
000223         10  EUEPREMA PIC  X(0001).
000224     05  EUEPREMI PIC  X(0012).
000225*    -------------------------------
000226     05  EOMEARNL PIC S9(0004) COMP.
000227     05  EOMEARNF PIC  X(0001).
000228     05  FILLER REDEFINES EOMEARNF.
000229         10  EOMEARNA PIC  X(0001).
000230     05  EOMEARNI PIC  X(0013).
000231*    -------------------------------
000232     05  EITDREFL PIC S9(0004) COMP.
000233     05  EITDREFF PIC  X(0001).
000234     05  FILLER REDEFINES EITDREFF.
000235         10  EITDREFA PIC  X(0001).
000236     05  EITDREFI PIC  X(0011).
000237*    -------------------------------
000238     05  EITDPMTL PIC S9(0004) COMP.
000239     05  EITDPMTF PIC  X(0001).
000240     05  FILLER REDEFINES EITDPMTF.
000241         10  EITDPMTA PIC  X(0001).
000242     05  EITDPMTI PIC  X(0014).
000243*    -------------------------------
000244     05  EACCPCTL PIC S9(0004) COMP.
000245     05  EACCPCTF PIC  X(0001).
000246     05  FILLER REDEFINES EACCPCTF.
000247         10  EACCPCTA PIC  X(0001).
000248     05  EACCPCTI PIC  X(0006).
000249*    -------------------------------
000250     05  ECANDTL PIC S9(0004) COMP.
000251     05  ECANDTF PIC  X(0001).
000252     05  FILLER REDEFINES ECANDTF.
000253         10  ECANDTA PIC  X(0001).
000254     05  ECANDTI PIC  X(0008).
000255*    -------------------------------
000256     05  EPTHRHDL PIC S9(0004) COMP.
000257     05  EPTHRHDF PIC  X(0001).
000258     05  FILLER REDEFINES EPTHRHDF.
000259         10  EPTHRHDA PIC  X(0001).
000260     05  EPTHRHDI PIC  X(0011).
000261*    -------------------------------
000262     05  EPDTHRUL PIC S9(0004) COMP.
000263     05  EPDTHRUF PIC  X(0001).
000264     05  FILLER REDEFINES EPDTHRUF.
000265         10  EPDTHRUA PIC  X(0001).
000266     05  EPDTHRUI PIC  X(0008).
000267*    -------------------------------
000268     05  EREITBLL PIC S9(0004) COMP.
000269     05  EREITBLF PIC  X(0001).
000270     05  FILLER REDEFINES EREITBLF.
000271         10  EREITBLA PIC  X(0001).
000272     05  EREITBLI PIC  X(0003).
000273*    -------------------------------
000274     05  EEXITDTL PIC S9(0004) COMP.
000275     05  EEXITDTF PIC  X(0001).
000276     05  FILLER REDEFINES EEXITDTF.
000277         10  EEXITDTA PIC  X(0001).
000278     05  EEXITDTI PIC  X(0008).
000279*    -------------------------------
000280     05  EEXDATEL PIC S9(0004) COMP.
000281     05  EEXDATEF PIC  X(0001).
000282     05  FILLER REDEFINES EEXDATEF.
000283         10  EEXDATEA PIC  X(0001).
000284     05  EEXDATEI PIC  X(0008).
000285*    -------------------------------
000286     05  ESPECL PIC S9(0004) COMP.
000287     05  ESPECF PIC  X(0001).
000288     05  FILLER REDEFINES ESPECF.
000289         10  ESPECA PIC  X(0001).
000290     05  ESPECI PIC  X(0001).
000291*    -------------------------------
000292     05  EEXBTCHL PIC S9(0004) COMP.
000293     05  EEXBTCHF PIC  X(0001).
000294     05  FILLER REDEFINES EEXBTCHF.
000295         10  EEXBTCHA PIC  X(0001).
000296     05  EEXBTCHI PIC  X(0008).
000297*    -------------------------------
000298     05  EEXSTATL PIC S9(0004) COMP.
000299     05  EEXSTATF PIC  X(0001).
000300     05  FILLER REDEFINES EEXSTATF.
000301         10  EEXSTATA PIC  X(0001).
000302     05  EEXSTATI PIC  X(0008).
000303*    -------------------------------
000304     05  EDVCDL PIC S9(0004) COMP.
000305     05  EDVCDF PIC  X(0001).
000306     05  FILLER REDEFINES EDVCDF.
000307         10  EDVCDA PIC  X(0001).
000308     05  EDVCDI PIC  X(0003).
000309*    -------------------------------
000310     05  EEXITSTL PIC S9(0004) COMP.
000311     05  EEXITSTF PIC  X(0001).
000312     05  FILLER REDEFINES EEXITSTF.
000313         10  EEXITSTA PIC  X(0001).
000314     05  EEXITSTI PIC  X(0008).
000315*    -------------------------------
000316     05  ECLSTATL PIC S9(0004) COMP.
000317     05  ECLSTATF PIC  X(0001).
000318     05  FILLER REDEFINES ECLSTATF.
000319         10  ECLSTATA PIC  X(0001).
000320     05  ECLSTATI PIC  X(0008).
000321*    -------------------------------
000322     05  EPRRTL PIC S9(0004) COMP.
000323     05  EPRRTF PIC  X(0001).
000324     05  FILLER REDEFINES EPRRTF.
000325         10  EPRRTA PIC  X(0001).
000326     05  EPRRTI PIC  X(0008).
000327*    -------------------------------
000328     05  REFMETHL PIC S9(0004) COMP.
000329     05  REFMETHF PIC  X(0001).
000330     05  FILLER REDEFINES REFMETHF.
000331         10  REFMETHA PIC  X(0001).
000332     05  REFMETHI PIC  X(0008).
000333*    -------------------------------
000334     05  EALPRRTL PIC S9(0004) COMP.
000335     05  EALPRRTF PIC  X(0001).
000336     05  FILLER REDEFINES EALPRRTF.
000337         10  EALPRRTA PIC  X(0001).
000338     05  EALPRRTI PIC  X(0008).
000339*    -------------------------------
000340     05  ECEDHDL PIC S9(0004) COMP.
000341     05  ECEDHDF PIC  X(0001).
000342     05  FILLER REDEFINES ECEDHDF.
000343         10  ECEDHDA PIC  X(0001).
000344     05  ECEDHDI PIC  X(0013).
000345*    -------------------------------
000346     05  ECEDBENL PIC S9(0004) COMP.
000347     05  ECEDBENF PIC  X(0001).
000348     05  FILLER REDEFINES ECEDBENF.
000349         10  ECEDBENA PIC  X(0001).
000350     05  ECEDBENI PIC  X(0011).
000351*    -------------------------------
000352     05  EERMSG1L PIC S9(0004) COMP.
000353     05  EERMSG1F PIC  X(0001).
000354     05  FILLER REDEFINES EERMSG1F.
000355         10  EERMSG1A PIC  X(0001).
000356     05  EERMSG1I PIC  X(0079).
000357*    -------------------------------
000358     05  EERMSG2L PIC S9(0004) COMP.
000359     05  EERMSG2F PIC  X(0001).
000360     05  FILLER REDEFINES EERMSG2F.
000361         10  EERMSG2A PIC  X(0001).
000362     05  EERMSG2I PIC  X(0079).
000363*    -------------------------------
000364     05  EPFKEYL PIC S9(0004) COMP.
000365     05  EPFKEYF PIC  X(0001).
000366     05  FILLER REDEFINES EPFKEYF.
000367         10  EPFKEYA PIC  X(0001).
000368     05  EPFKEYI PIC  99.
000369*    -------------------------------
000370     05  EPFKEY7L PIC S9(0004) COMP.
000371     05  EPFKEY7F PIC  X(0001).
000372     05  FILLER REDEFINES EPFKEY7F.
000373         10  EPFKEY7A PIC  X(0001).
000374     05  EPFKEY7I PIC  X(0015).
000375*    -------------------------------
000376     05  EPFKEY6L PIC S9(0004) COMP.
000377     05  EPFKEY6F PIC  X(0001).
000378     05  FILLER REDEFINES EPFKEY6F.
000379         10  EPFKEY6A PIC  X(0001).
000380     05  EPFKEY6I PIC  X(0017).
000381 01  EL127EO REDEFINES EL127EI.
000382     05  FILLER            PIC  X(0012).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  EDATEO PIC  X(0008).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  ETIMEO PIC  99.99.
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  CMPNYIDO PIC  X(0003).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  USERIDO PIC  X(0004).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  EMEMCAPO PIC  X(0010).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  EASRISKO PIC  X(0001).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  ECERTNOO PIC  X(0010).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  ECRTSFXO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  EACCTNOO PIC  X(0010).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  ESTATEO PIC  X(0002).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  ECARIERO PIC  X(0001).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  EGROUPO PIC  X(0006).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  EEFFDTO PIC  X(0008).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  EMEMNOO PIC  X(0012).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  ECLPSTO PIC  X(0002).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  ECLPO PIC  ZZ,ZZ9.99.
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  EACLPO PIC  ZZ,ZZ9.99.
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  EKINDO PIC  X(0012).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  ECODEO PIC  X(0002).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  EDESCO PIC  X(0010).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  ETERMO PIC  ZZ9.
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  ERTERMO PIC  ZZ9.
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  ECOMENTO PIC  X(0010).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  ETRMDAYO PIC  ZZZZ9.
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  EEXTDAYO PIC  ZZZZ9.
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  EENTSTO PIC  X(0010).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  EPREMO PIC  ZZZ,ZZ9.99-.
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  EALTPRMO PIC  ZZZ,ZZ9.99-.
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  ECURSTO PIC  X(0010).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  EBENEO PIC  ZZ,ZZZ,ZZ9.99.
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  EALTBENO PIC  ZZZZZ,ZZ9.99.
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  EREMBENO PIC  ZZ,ZZZ,ZZ9.99.
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  EEXPDTO PIC  X(0008).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  EREINSPO PIC  Z,ZZZ,ZZ9.99.
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  ECRITPDO PIC  ZZ9.
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  EUEDTO PIC  X(0008).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  EUEPREMO PIC  ZZZZZ,ZZ9.99.
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  EOMEARNO PIC  ZZ,ZZZ,ZZ9.99.
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  EITDREFO PIC  ZZZZ,ZZ9.99.
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  EITDPMTO PIC  ZZZ,ZZZ,ZZ9.99.
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  EACCPCTO PIC  .99999.
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  ECANDTO PIC  X(0008).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  EPTHRHDO PIC  X(0011).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  EPDTHRUO PIC  X(0008).
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  EREITBLO PIC  X(0003).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  EEXITDTO PIC  X(0008).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  EEXDATEO PIC  X(0008).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  ESPECO PIC  X(0001).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  EEXBTCHO PIC  X(0008).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  EEXSTATO PIC  X(0008).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  EDVCDO PIC  X(0003).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  EEXITSTO PIC  X(0008).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  ECLSTATO PIC  X(0008).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  EPRRTO PIC  Z9.99999.
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  REFMETHO PIC  X(0008).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  EALPRRTO PIC  Z9.99999.
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  ECEDHDO PIC  X(0013).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  ECEDBENO PIC  ZZZZZZZ9.99.
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  EERMSG1O PIC  X(0079).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  EERMSG2O PIC  X(0079).
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  EPFKEYO PIC  99.
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  EPFKEY7O PIC  X(0015).
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  EPFKEY6O PIC  X(0017).
000572*    -------------------------------
      *<<((file: EL1275S))
000243*                                    COPY ELCCALC.
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
000244*                                    COPY ELCEMIB.
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
000245*                                    COPY ELCLOGOF.
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
000246*                                    COPY ELCATTR.
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
000247*                                    COPY ELCAID.
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
000248
000249 01  FILLER REDEFINES DFHAID.
000250     05  FILLER                      PIC X(8).
000251     05  PF-VALUES                   PIC X
000252         OCCURS 24 TIMES.
000253
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
000255
000256 01  DFHCOMMAREA                     PIC X(1024).
000257
000258*01 PARMLIST   COMP SYNC.
000259*    05  FILLER                      PIC S9(8).
000260*    05  ELCERT-POINTER              PIC S9(8).
000261*    05  ELCNTL-POINTER              PIC S9(8).
000262*    05  ELMSTR-POINTER              PIC S9(8).
000263
000264     EJECT
000265*                                    COPY ELCCERT.
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
000266     EJECT
000267*                                    COPY ELCCNTL.
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
000268     EJECT
000269*                                    COPY ELCMSTR.
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
000270
000271*                                COPY ERCPDEF.
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
000272
000273     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1275' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000274 VCOBOL-DUMMY-PROCEDURE.
000275
000276     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000277     MOVE '5'                    TO  DC-OPTION-CODE.
000278     PERFORM 8500-DATE-CONVERSION.
000279     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000280     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000281
000282*    NOTE *******************************************************
000283*         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
000284*         *  FROM ANOTHER MODULE.                               *
000285*         *******************************************************.
000286
000287     IF EIBCALEN NOT GREATER THAN ZERO
000288         MOVE UNACCESS-MSG       TO  LOGOFF-MSG
000289         GO TO 8300-SEND-TEXT.
000290
000291     
      * EXEC CICS HANDLE CONDITION
000292*        ERROR  (9990-ERROR)
000293*        QIDERR (0001-FIRST-TIME)
000294*    END-EXEC.
      *    MOVE '"$.N                  ! " #00005102' TO DFHEIV0
           MOVE X'22242E4E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303035313032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000295
000296     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000297     MOVE EIBTRMID               TO  QID-TERM.
000298
000299     
      * EXEC CICS READQ TS
000300*        QUEUE    (QID)
000301*        INTO     (PROGRAM-INTERFACE-BLOCK)
000302*        LENGTH   (PI-COMM-LENGTH)
000303*    END-EXEC.
      *    MOVE '*$I    L              ''   #00005110' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035313130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000304
000305     
      * EXEC CICS DELETEQ TS
000306*        QUEUE    (QID)
000307*    END-EXEC.
      *    MOVE '*&                    #   #00005116' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000308
000309 0001-FIRST-TIME.
000310
000311     IF PI-1ST-TIME-SW = '1'
000312         MOVE '2'                TO  PI-1ST-TIME-SW
000313         GO TO 0100-RECEIVE.
000314
000315     IF PI-1ST-TIME-SW = '2'
000316         GO TO 0100-RECEIVE.
000317
000318     MOVE '1'                    TO  PI-1ST-TIME-SW.
000319     GO TO 1000-DISPLAY-CERTIFICATE.
000320
000321     EJECT
000322 0100-RECEIVE.
000323     IF EIBAID = DFHCLEAR
000324         GO TO 9400-CLEAR.
000325
000326     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000327         MOVE ER-0008            TO  EMI-ERROR
000328         PERFORM 9900-ERROR-FORMAT
000329         MOVE -1                 TO  EPFKEYL
000330         GO TO 8200-SEND-DATAONLY.
000331
000332     
      * EXEC CICS RECEIVE
000333*        MAPSET (WS-MAPSET-NAME)
000334*        MAP    (WS-MAP-NAME)
000335*        INTO   (EL127EI)
000336*    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005143' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035313433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000337
000338     IF EPFKEYL = 0
000339         GO TO 0200-CHECK-PFKEYS.
000340
000341     IF EIBAID NOT = DFHENTER
000342         MOVE ER-0008            TO  EMI-ERROR
000343         MOVE -1                 TO  EPFKEYL
000344         GO TO 0300-INPUT-ERROR.
000345
000346     IF (EPFKEYI IS NUMERIC) AND
000347         (EPFKEYI IS GREATER THAN 0 AND LESS THAN 25)
000348             MOVE PF-VALUES (EPFKEYI)    TO  EIBAID
000349     ELSE
000350         MOVE ER-0029                    TO  EMI-ERROR
000351         GO TO 0300-INPUT-ERROR.
000352
000353 0200-CHECK-PFKEYS.
000354
000355     IF EIBAID = DFHPF3
000356        MOVE 'EL1274'            TO  WS-PROGRAM-ID
000357        GO TO 9300-XCTL.
000358
000359     IF EIBAID = DFHPF4
000360         MOVE '1'                TO  PI-1ST-TIME-SW
000361         MOVE 'EL1273'           TO  WS-PROGRAM-ID
000362         GO TO 9300-XCTL.
000363
000364     IF EIBAID = DFHPF5
000365         IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
000366             MOVE ER-0029        TO  EMI-ERROR
000367             MOVE -1             TO  EPFKEYL
000368             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000369             GO TO 8200-SEND-DATAONLY
000370         ELSE
000371             IF PI-COMPANY-ID = 'DMD'
000372                 MOVE 'EL401DMD' TO  WS-PROGRAM-ID
000373               ELSE
000374*00319                  MOVE 'EL1276'   TO  WS-PROGRAM-ID
000375                 MOVE 'EL1279'   TO  WS-PROGRAM-ID
000376             END-IF
000377             GO TO 9300-XCTL.
000378
000379     IF PI-MAIL-YES
000380         IF EIBAID = DFHPF6
000381             IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
000382                 MOVE ER-0029    TO  EMI-ERROR
000383                 MOVE -1         TO  EPFKEYL
000384                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000385                 GO TO 8200-SEND-DATAONLY
000386             ELSE
000387                 MOVE 'EL1277'   TO  WS-PROGRAM-ID
000388                 GO TO 9300-XCTL.
000389
000390     IF EIBAID IS EQUAL TO DFHPF7
000391         IF PI-CLAIM-SW IS EQUAL TO 'Y'
000392                    AND
000393            CREDIT-SESSION
000394             MOVE ' '                    TO  PI-1ST-TIME-SW
000395             
      * EXEC CICS WRITEQ TS
000396*                QUEUE    (QID)
000397*                FROM     (PROGRAM-INTERFACE-BLOCK)
000398*                LENGTH   (PI-COMM-LENGTH)
000399*            END-EXEC
      *    MOVE '*"     L              ''   #00005206' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035323036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000400             MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM
000401             MOVE 'EL132   '             TO  WS-PROGRAM-ID
000402             GO TO 9300-XCTL
000403         ELSE
000404             MOVE ER-0029                TO  EMI-ERROR
000405             MOVE -1                     TO  EPFKEYL
000406             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000407             GO TO 8200-SEND-DATAONLY.
000408
000409     IF EIBAID = DFHPF12
000410         MOVE 'EL010   '         TO  WS-PROGRAM-ID
000411         GO TO 9300-XCTL.
000412
000413     IF EIBAID = DFHPF23
000414         GO TO 9000-RETURN-CICS.
000415
000416     IF EIBAID = DFHPF24
000417         MOVE 'EL126   '         TO  WS-PROGRAM-ID
000418         GO TO 9300-XCTL.
000419
000420     IF EIBAID = DFHPF1 OR DFHPF2
000421         GO TO 1000-DISPLAY-CERTIFICATE.
000422
000423     IF EIBAID = DFHENTER
000424         GO TO 0400-EDIT.
000425
000426     MOVE ER-0029                TO  EMI-ERROR.
000427
000428 0300-INPUT-ERROR.
000429
000430     PERFORM 9900-ERROR-FORMAT.
000431     MOVE AL-UNBON               TO  EPFKEYA.
000432     MOVE -1                     TO  EPFKEYL.
000433     GO TO 8200-SEND-DATAONLY.
000434
000435 0400-EDIT.
000436     IF EUEDTL IS GREATER THAN 0
000437         MOVE EUEDTI                 TO  DEEDIT-FIELD
000438         PERFORM 7500-DEEDIT THRU 7500-EXIT
000439         IF DEEDIT-FIELD-V0 IS GREATER THAN 0
000440             MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
000441             MOVE '4'                TO  DC-OPTION-CODE
000442             PERFORM 8500-DATE-CONVERSION
000443             IF NO-CONVERSION-ERROR
000444                 MOVE DC-BIN-DATE-1  TO  WS-VALUATION-DT
000445                 GO TO 1000-DISPLAY-CERTIFICATE.
000446
000447     MOVE PI-CR-MONTH-END-DT         TO  WS-VALUATION-DT.
000448     EJECT
000449 1000-DISPLAY-CERTIFICATE.
000450
000451******************************************************************
000452*                                                                *
000453*               READ THE REQUESTED CERT RECORD                   *
000454*                                                                *
000455******************************************************************
000456
000457     MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
000458     MOVE PI-CARRIER             TO  WS-CK-CARRIER.
000459     MOVE PI-GROUPING            TO  WS-CK-GROUPING.
000460     MOVE PI-STATE               TO  WS-CK-STATE.
000461     MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
000462     MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
000463     MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
000464
000465     
      * EXEC CICS HANDLE CONDITION
000466*        NOTFND (8880-NOT-FOUND)
000467*    END-EXEC.
      *    MOVE '"$I                   ! # #00005276' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303035323736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000468
000469     
      * EXEC CICS READ
000470*        DATASET (WS-CERTIFICATE-MASTER-DSID)
000471*        RIDFLD  (WS-CERTIFICATE-KEY)
000472*        SET     (ADDRESS OF CERTIFICATE-MASTER)
000473*    END-EXEC.
      *    MOVE '&"S        E          (   #00005280' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035323830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000474
000475     EJECT
000476 2000-BUILD-OUTPUT-MAP.
000477
000478******************************************************************
000479*                                                                *
000480*              BUILD THE OUTPUT SCREEN TO BE DISPLAYED           *
000481*                                                                *
000482******************************************************************
000483
000484     MOVE LOW-VALUES             TO  EL127EO.
000485     MOVE CM-CERT-PRIME          TO  ECERTNOO.
000486     MOVE CM-CERT-SFX            TO  ECRTSFXO.
000487     MOVE CM-ACCOUNT             TO  EACCTNOO
000488                                     WS-ACCOUNT.
000489     MOVE CM-STATE               TO  ESTATEO.
000490     MOVE CM-CARRIER             TO  ECARIERO.
000491     MOVE CM-GROUPING            TO  EGROUPO.
000492
000493     IF CM-MEMB-STATE   = CM-STATE OR
000494        CM-MEMB-ACCOUNT = WS-ACCT
000495         MOVE SPACES             TO  EMEMNOO
000496     ELSE
000497         MOVE CM-MEMBER-NO       TO  EMEMNOO.
000498
000499     IF CM-CERT-EFF-DT NOT = LOW-VALUES
000500         MOVE SPACES             TO  DC-OPTION-CODE
000501         MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-1
000502         PERFORM 8500-DATE-CONVERSION
000503         MOVE DC-GREG-DATE-1-EDIT TO EEFFDTO.
000504
000505     MOVE CM-REIN-TABLE          TO  EREITBLO.
000506     MOVE CM-SPECIAL-REIN-CODE   TO  ESPECO.
000507
000508     IF PI-MAIL-YES
000509         MOVE AL-SANON           TO  EPFKEY6A
000510     ELSE
000511         MOVE AL-SADOF           TO  EPFKEY6A.
000512
000513     EJECT
000514******************************************************************
000515*                                                                *
000516*              PI-BENEFIT-INDICATOR VALUES:                      *
000517*                   1 = LIFE COVERAGE ONLY                       *
000518*                   2 = A&H  COVERAGE ONLY                       *
000519*                   3 = TWO  COVERAGES PRESENT                   *
000520*                                                                *
000521******************************************************************
000522     IF CM-LF-BENEFIT-CD NOT = '00'  AND
000523        CM-AH-BENEFIT-CD NOT = '00'
000524             MOVE '3'            TO  PI-BENEFIT-IND
000525         ELSE
000526             IF CM-LF-BENEFIT-CD NOT = '00'
000527                 MOVE '1'        TO  PI-BENEFIT-IND
000528             ELSE
000529                 IF CM-AH-BENEFIT-CD NOT = '00'
000530                     MOVE '2'    TO  PI-BENEFIT-IND
000531                 ELSE
000532                     MOVE '0'    TO  PI-BENEFIT-IND.
000533
000534     IF PI-BENEFIT-IND = '3' AND
000535         PI-1ST-TIME-SW = '1'
000536             GO TO 2100-LF-BENEFIT.
000537
000538     IF EIBAID = DFHPF1
000539         IF PI-BENEFIT-IND = '3'
000540             IF PI-PREV-BENEFIT = '1'
000541                 GO TO 2200-AH-BENEFIT.
000542
000543     IF EIBAID = DFHPF1
000544         IF PI-BENEFIT-IND = '3'
000545             IF PI-PREV-BENEFIT = '2'
000546                 MOVE ER-7237        TO  EMI-ERROR
000547                 MOVE -1             TO  EPFKEYL
000548                 PERFORM 9900-ERROR-FORMAT
000549                 GO TO 8200-SEND-DATAONLY.
000550
000551     IF EIBAID = DFHPF2
000552         IF PI-BENEFIT-IND = '3'
000553             IF PI-PREV-BENEFIT = '2'
000554                 GO TO 2100-LF-BENEFIT.
000555
000556     IF EIBAID = DFHPF2
000557         IF PI-BENEFIT-IND = '3'
000558             IF PI-PREV-BENEFIT = '1'
000559                 MOVE ER-7237        TO  EMI-ERROR
000560                 MOVE -1             TO  EPFKEYL
000561                 PERFORM 9900-ERROR-FORMAT
000562                 GO TO 8200-SEND-DATAONLY.
000563
000564     IF PI-BENEFIT-IND = '1'
000565         MOVE ER-7048                TO  EMI-ERROR
000566         PERFORM 9900-ERROR-FORMAT
000567         GO TO 2100-LF-BENEFIT.
000568
000569     IF PI-BENEFIT-IND = '2'
000570         MOVE ER-7048                TO  EMI-ERROR
000571         PERFORM 9900-ERROR-FORMAT
000572         GO TO 2200-AH-BENEFIT.
000573
000574     IF EIBAID = DFHENTER
000575         IF PI-PREV-BENEFIT IS EQUAL TO '1'
000576             GO TO 2100-LF-BENEFIT
000577         ELSE
000578             GO TO 2200-AH-BENEFIT.
000579
000580     GO TO 8200-SEND-DATAONLY.
000581
000582     EJECT
000583 2100-LF-BENEFIT.
000584     MOVE '1'                    TO  PI-PREV-BENEFIT.
000585
000586     MOVE '4'                    TO  WS-CFK-RECORD-TYPE.
000587     MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO.
000588     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
000589     PERFORM 3000-LOCATE-BENEFIT.
000590
000591     IF WS-CALC-CD IS EQUAL TO 'Z'
000592         MOVE AL-UNNOF           TO  ETERMA.
000593
000594     MOVE '2'                    TO CP-PROCESS-TYPE.
000595     MOVE WS-LF-COVERAGE-TYPE    TO  CP-BENEFIT-TYPE.
000596     MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
000597                                     CP-RATING-METHOD.
000598     MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
000599     MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
000600     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
000601     MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
000602     IF CM-LF-ORIG-TERM IS EQUAL TO 0
000603         MOVE 1                  TO  CP-ORIGINAL-TERM
000604     ELSE
000605         MOVE CM-LF-ORIG-TERM    TO  CP-ORIGINAL-TERM.
000606     MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
000607     MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
000608     MOVE '4'                    TO  CP-REM-TERM-METHOD.
000609     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
000610     MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
000611
000612*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
000613     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
000614     MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
000615     MOVE SPACES                 TO  WS-CFK-ACCESS.
000616     MOVE CM-STATE               TO  WS-CFK-STATE.
000617     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
000618     MOVE 'Y'                    TO  WS-FREE-LOOK.
000619
000620     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
000621
000622     MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
000623
000624     PERFORM 9800-LINK-REM-TERM.
000625
000626     MOVE PI-LIFE-OVERRIDE-L12   TO  EKINDO.
000627     MOVE CM-LF-BENEFIT-CD       TO  ECODEO.
000628     MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
000629     MOVE CM-LF-ORIG-TERM        TO  ETERMO.
000630     MOVE CP-REMAINING-TERM-3    TO  ERTERMO.
000631     IF CP-REMAINING-TERM-3 GREATER THAN CM-LF-ORIG-TERM
000632        MOVE CM-LF-ORIG-TERM     TO  ERTERMO.
000633     MOVE CM-LF-TERM-IN-DAYS     TO  ETRMDAYO.
000634     MOVE WS-COMMENT             TO  ECOMENTO.
000635     MOVE CM-LF-CRITICAL-PERIOD  TO  ECRITPDO.
000636     MOVE CM-LF-PREMIUM-AMT      TO  EPREMO.
000637     IF CP-EARNING-METHOD = 'B'
000638        MOVE CM-LF-ALT-PREMIUM-AMT  TO  EALTPRMO
000639     END-IF
000640     IF CM-LF-CLP NOT NUMERIC
000641        MOVE ZEROS               TO CM-LF-CLP
000642     END-IF
000643
000644     MOVE CM-CLP-STATE           TO  ECLPSTO
000645     MOVE CM-LF-CLP              TO  ECLPO
000646*    MOVE CM-ADDL-CLP            TO  EACLPO
000647
000648
000649     MOVE CM-LF-NSP-PREMIUM-AMT  TO  EREINSPO.
000650     MOVE CM-LF-BENEFIT-AMT      TO  EBENEO.
000651     IF CP-EARNING-METHOD = 'B'
000652        MOVE CM-LF-ALT-BENEFIT-AMT  TO  EALTBENO
000653     END-IF
000654     MOVE CM-LF-ITD-CANCEL-AMT   TO  EITDREFO.
000655     MOVE CM-LF-EXIT-BATCH       TO  EEXBTCHO.
000656     MOVE CM-LF-ITD-DEATH-AMT    TO  EITDPMTO.
000657     IF CM-LF-PREMIUM-RATE NUMERIC
000658         MOVE CM-LF-PREMIUM-RATE     TO  EPRRTO
000659     ELSE
000660         MOVE ZEROS                  TO  EPRRTO.
000661     IF CM-LF-ALT-PREMIUM-RATE NUMERIC
000662         MOVE CM-LF-ALT-PREMIUM-RATE TO  EALPRRTO
000663     ELSE
000664         MOVE ZEROS                  TO  EALPRRTO.
000665     MOVE CM-LF-DEV-CODE         TO  EDVCDO.
000666     MOVE CM-LIFE-COMM-PCT       TO  EACCPCTO.
000667     MOVE CM-REIN-TABLE          TO  EREITBLO.
000668     MOVE SPACES                 TO  EPDTHRUO.
000669
000670     MOVE SPACES                 TO  ECLSTATO.
000671     IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
000672         MOVE '*'                TO  EASRISKO
000673         MOVE 'L'                TO WS-CLAIM-SW
000674         PERFORM 7600-CHECK-FOR-CLAIM
000675     ELSE
000676         MOVE ' '                TO  PI-CLAIM-SW
000677                                     EASRISKO.
000678
000679     IF CM-LF-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
000680         MOVE SPACES                 TO  DC-OPTION-CODE
000681         MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
000682         PERFORM 8500-DATE-CONVERSION
000683         MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
000684
000685******************************************************************
000686*               DISPLAY ENTRY STATUS CODES                       *
000687******************************************************************
000688
000689     MOVE SPACES                 TO EENTSTO.
000690
000691     IF CM-ENTRY-STATUS = '1'
000692         MOVE 'NORM'             TO  EENTSTO.
000693     IF CM-ENTRY-STATUS = '2'
000694         MOVE 'PEND'             TO  EENTSTO
000695         MOVE 'P'                TO  PI-PEND-SW.
000696     IF CM-ENTRY-STATUS = '4'
000697         MOVE 'CONV'             TO  EENTSTO.
000698     IF CM-ENTRY-STATUS = '5'
000699         MOVE 'REIS'             TO  EENTSTO.
000700     IF CM-ENTRY-STATUS = 'M'
000701         MOVE 'MONTHLY'          TO  EENTSTO.
000702     IF CM-ENTRY-STATUS = '9'
000703         MOVE 'REIN'             TO  EENTSTO.
000704
000705     IF CM-ENTRY-STATUS = 'D'
000706         MOVE 'DECL'             TO  EENTSTO.
000707
000708     IF CM-ENTRY-STATUS = 'V'
000709         MOVE 'VOID'             TO  EENTSTO.
000710
000711     IF CM-ENTRY-STATUS = 'U'
000712         MOVE 'UNDERWRT'         TO  EENTSTO.
000713
000714******************************************************************
000715*                                                                *
000716*               DISPLAY CURRENT STATUS CODES                     *
000717*                                                                *
000718******************************************************************
000719
000720     MOVE SPACES                 TO ECURSTO.
000721
000722     IF CM-LF-CURRENT-STATUS = '1' OR '4'
000723         IF CP-REMAINING-TERM-3 = ZEROS
000724             MOVE 'EXPIRED'      TO  ECURSTO
000725         ELSE
000726             MOVE 'ACTIVE'       TO  ECURSTO.
000727
000728     IF CM-LF-CURRENT-STATUS = '2'
000729         MOVE 'PEND'             TO  ECURSTO
000730         MOVE 'P'                TO  PI-PEND-SW.
000731
000732     IF CM-LF-CURRENT-STATUS = '3'
000733         MOVE 'RESTORE'          TO  ECURSTO.
000734
000735     IF CM-LF-CURRENT-STATUS = '5'
000736         MOVE 'REISSUE'          TO  ECURSTO.
000737     IF CM-LF-CURRENT-STATUS = 'M'
000738         MOVE 'MONTHLY'          TO  ECURSTO.
000739
000740     IF CM-LF-CURRENT-STATUS = '6'
000741         MOVE 'LMP BEN'          TO  ECURSTO.
000742
000743     IF CM-LF-CURRENT-STATUS = '7'
000744         MOVE 'DEATH'            TO  ECURSTO.
000745
000746     IF CM-LF-CURRENT-STATUS = '8'
000747         MOVE 'CANCEL'           TO  ECURSTO.
000748
000749     IF CM-LF-CURRENT-STATUS = '9'
000750         MOVE 'RE-ONLY'          TO  ECURSTO.
000751
000752     IF CM-LF-CURRENT-STATUS = 'D'
000753         MOVE 'DECLINE'          TO  ECURSTO.
000754
000755     IF CM-LF-CURRENT-STATUS = 'V'
000756         MOVE 'VOID'             TO  ECURSTO.
000757
000758******************************************************************
000759*                                                                *
000760*            CALCULATE REMAINING BENEFIT AMOUNT                  *
000761*                                                                *
000762******************************************************************
000763
000764     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
000765     MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
000766     MOVE SPACES                 TO  WS-CFK-ACCESS.
000767     MOVE CM-STATE               TO  WS-CFK-STATE.
000768     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
000769
000770     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
000771
000772     MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREV
000773                                     CP-STATE-STD-ABBRV.
000774
000775     MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
000776
000777     MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
000778                                     CP-RATING-BENEFIT-AMT.
000779     MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
000780     MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ALTERNATE-BENEFIT.
000781     MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ALTERNATE-PREMIUM.
000782     MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
000783     MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
000784     MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
000785
000786     MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE
000787     MOVE CP-REMAINING-TERM-3    TO  CP-REMAINING-TERM.
000788
000789     IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
000790         MOVE ZEROS                  TO  EREMBENO
000791     ELSE
000792        PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
000793        MOVE ZEROS               TO CP-R-MAX-TOT-BEN
000794        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
000795           AND (ACCT-FOUND)
000796           AND (AM-DCC-PRODUCT-CODE <> SPACES)
000797           MOVE ' '             TO WS-PDEF-RECORD-SW
000798           PERFORM 2600-GET-MAX-BENEFIT
000799                                 THRU 2600-EXIT
000800           PERFORM 9800-LINK-REM-TERM
000801
000802           IF PDEF-FOUND
000803              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
000804                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
000805              ELSE
000806                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
000807              END-IF
000808              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
000809                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
000810              display ' att age ' ws-att-age
000811              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
000812                 (P1 > +11)
000813                 OR ((PD-PROD-CODE (P1) = 'L' or 'O')
000814                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
000815              END-PERFORM
000816              IF P1 < +12
000817                 MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
000818                 display ' max 1 ' cp-r-max-tot-ben
000819              END-IF
000820           END-IF
000821        END-IF
000822        PERFORM 9700-LINK-REM-AMT
000823        display ' max , rem ' cp-r-max-tot-ben ' '
000824            cp-remaining-amt
000825        MOVE CP-REMAINING-AMT       TO  EREMBENO
000826     END-IF
000827
000828     IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR
000829        WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'
000830         COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
000831                                    CM-LF-ITD-DEATH-AMT
000832         MOVE WS-REMAINING-AMT       TO  EREMBENO.
000833
000834******************************************************************
000835*                                                                *
000836*            CALCULATE UNEARNED PREMIUM AMOUNT                   *
000837*                      ONE MONTH EARNED PREMIUM AMOUNT           *
000838*                                                                *
000839******************************************************************
000840
000841     MOVE '2'                    TO  CP-PROCESS-TYPE.
000842     MOVE CM-STATE               TO  CP-STATE.
000843     MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
000844     MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
000845     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
000846
000847     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
000848     MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
000849     MOVE SPACES                 TO  WS-CFK-ACCESS.
000850     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
000851     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
000852     MOVE CF-LIFE-OVERRIDE-L1    TO  CP-LIFE-OVERRIDE-CODE.
000853     MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
000854     MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
000855     MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
000856
000857     IF CP-STATE-STD-ABBRV = 'OR'
000858         COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
000859                                         CM-LF-ALT-BENEFIT-AMT.
000860
000861     MOVE WS-BENEFIT-CODE        TO  CP-BENEFIT-CD.
000862     IF WS-CALC-CD IS EQUAL TO 'T'
000863         MOVE WS-OVRD-EARNINGS-CALC  TO  CP-EARNING-METHOD.
000864
000865     IF WS-CALC-CD IS EQUAL TO 'D'
000866         MOVE CM-LF-TERM-IN-DAYS          TO  CP-TERM-OR-EXT-DAYS
000867     ELSE
000868         IF CM-PMT-EXTENSION-DAYS IS NUMERIC
000869             MOVE CM-PMT-EXTENSION-DAYS   TO  CP-TERM-OR-EXT-DAYS
000870         ELSE
000871             MOVE ZEROS                   TO  CP-TERM-OR-EXT-DAYS.
000872
000873     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
000874
000875     IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
000876         MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
000877                                     WS-VALUATION-DT
000878     ELSE
000879         MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
000880
000881     PERFORM 9800-LINK-REM-TERM.
000882     MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
000883     PERFORM 9700-LINK-REM-AMT.
000884     MOVE CP-REMAINING-AMT       TO  CP-REMAINING-BENEFIT.
000885
000886     MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
000887     PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
000888     MOVE CP-CALC-REFUND         TO  EUEPREMO
000889                                     WS-CALC-REFUND.
000890     ADD +1                      TO  CP-REMAINING-TERM.
000891     IF CP-REMAINING-TERM IS EQUAL TO CM-LF-ORIG-TERM
000892         COMPUTE ONE-MON-EARNED = CM-LF-PREMIUM-AMT -
000893                                                CP-CALC-REFUND
000894     ELSE
000895         PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
000896         COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
000897                                                CP-CALC-REFUND.
000898
000899     MOVE ONE-MON-EARNED         TO  EOMEARNO.
000900
000901     IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
000902         MOVE SPACES                 TO  DC-OPTION-CODE
000903         MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
000904         PERFORM 8500-DATE-CONVERSION
000905         MOVE AL-UANON               TO  EUEDTA
000906         MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
000907
000908******************************************************************
000909*                                                                *
000910*                     DISPLAY CANCEL DATE                        *
000911*                             CANCEL EXIT DATE                   *
000912*                             CANCEL EXIT STATUS                 *
000913*                                                                *
000914******************************************************************
000915
000916     MOVE SPACES                       TO  EEXITDTO.
000917
000918     IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES
000919         MOVE SPACES                   TO  DC-OPTION-CODE
000920         MOVE CM-LF-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
000921         PERFORM 8500-DATE-CONVERSION
000922         IF NOT DATE-CONVERSION-ERROR
000923             MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
000924         ELSE
000925             MOVE SPACES               TO  EEXITDTO.
000926
000927     MOVE SPACES                       TO  ECANDTO.
000928
000929     IF CM-LF-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
000930         MOVE SPACES                   TO  DC-OPTION-CODE
000931         MOVE CM-LF-CANCEL-DT          TO  DC-BIN-DATE-1
000932         PERFORM 8500-DATE-CONVERSION
000933         IF NOT DATE-CONVERSION-ERROR
000934             MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
000935         ELSE
000936             MOVE SPACES               TO  ECANDTO.
000937
000938     IF CM-LF-STATUS-AT-CANCEL = ' '
000939         MOVE SPACES             TO  EEXITSTO.
000940     IF CM-LF-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
000941         MOVE 'ACTIVE'           TO  EEXITSTO.
000942     IF CM-LF-STATUS-AT-CANCEL = '6'
000943         MOVE 'PREV SETTLE'      TO  EEXITSTO.
000944     IF CM-LF-STATUS-AT-CANCEL = '7'
000945         MOVE 'PREV DTH'         TO  EEXITSTO.
000946     IF CM-LF-STATUS-AT-CANCEL = '8'
000947         MOVE 'PREV CANCEL'      TO  EEXITSTO.
000948
000949******************************************************************
000950*                                                                *
000951*                     DISPLAY CLAIM EXIT DATE                    *
000952*                             CLAIM EXIT STATUS                  *
000953*                                                                *
000954******************************************************************
000955
000956     MOVE SPACES                             TO  EEXDATEO.
000957
000958     IF CM-LF-DEATH-EXIT-DT IS NOT = LOW-VALUES AND
000959         (CM-LF-CURRENT-STATUS = '7' OR '8')
000960             MOVE SPACES                     TO  DC-OPTION-CODE
000961             MOVE CM-LF-DEATH-EXIT-DT        TO  DC-BIN-DATE-1
000962             PERFORM 8500-DATE-CONVERSION
000963             IF NOT DATE-CONVERSION-ERROR
000964                 MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
000965             ELSE
000966                 MOVE SPACES                 TO  EEXDATEO.
000967
000968     MOVE SPACES                 TO  EEXSTATO.
000969
000970     IF CM-LF-STATUS-AT-DEATH = ' '
000971         MOVE SPACES             TO  EEXSTATO.
000972     IF CM-LF-STATUS-AT-DEATH = '1' OR '2' OR '4' OR '9'
000973         MOVE 'ACTIVE'           TO  EEXSTATO.
000974     IF CM-LF-STATUS-AT-DEATH = '6'
000975         MOVE 'PREV SETTLE'      TO  EEXSTATO.
000976     IF CM-LF-STATUS-AT-DEATH = '7'
000977         MOVE 'PREV DTH'         TO  EEXSTATO.
000978     IF CM-LF-STATUS-AT-DEATH = '8'
000979         MOVE 'PREV CAN'         TO  EEXSTATO.
000980
000981     move cm-control-primary     to ws-elcrtt-key
000982     move 'C'                    to ws-cs-rec-type
000983     
      * exec cics read
000984*       dataset   ('ELCRTT')
000985*       ridfld    (ws-elcrtt-key)
000986*       into      (certificate-trailers)
000987*       resp      (ws-response)
000988*    end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005794' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035373934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 ws-elcrtt-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000989     if resp-normal
000990        set good-hit-on-trlr to true
000991         evaluate true
000992            when cs-lf-refund-method = '1'
000993               move 'R78'        to refmetho
000994            when cs-lf-refund-method = '2'
000995               move 'PRO'        to refmetho
000996            when cs-lf-refund-method = '3'
000997               move 'CALIF'      to refmetho
000998            when cs-lf-refund-method = '4'
000999               move 'IRR/FARM'   to refmetho
001000            when cs-lf-refund-method = '5'
001001               move 'NET'        to refmetho
001002            when cs-lf-refund-method = '6'
001003               move 'ANTIC'      to refmetho
001004            when cs-lf-refund-method = '8'
001005               move 'MEAN'       to refmetho
001006            when cs-lf-refund-method = '9'
001007               move 'SUMDI'      to refmetho
001008            when cs-lf-refund-method = 'G'
001009               move 'GAPNR'      to refmetho
001010            when cs-lf-refund-method = 'S'
001011               move 'GAPA'       to refmetho
001012            when cs-lf-refund-method = 'D'
001013               move 'DDF'        to refmetho
001014            when cs-lf-refund-method = 'I'
001015               move 'DDFIU'      to refmetho
001016            when cs-lf-refund-method = 'R'
001017               move 'REPO'       to refmetho
001018            when other
001019               move cs-lf-refund-method
001020                                 to refmetho
001021         end-evaluate
001022     else
001023        set crtt-not-found to true
001024     end-if
001025
001026
001027******************************************************************
001028*                                                                *
001029*                   DISPLAY EXTENSION DAYS                       *
001030*                                                                *
001031******************************************************************
001032
001033     IF CM-PMT-EXTENSION-DAYS NUMERIC
001034         MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
001035     ELSE
001036         MOVE ZEROS                  TO  EEXTDAYO.
001037
001038******************************************************************
001039*                                                                *
001040*                   DISPLAY CEDED BENEFIT                        *
001041*                                                                *
001042******************************************************************
001043
001044*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
001045*        IF CM-LF-CEDED-BENEFIT NUMERIC
001046*            MOVE CM-LF-CEDED-BENEFIT
001047*                                TO  ECEDBENO
001048*        ELSE
001049*            MOVE ZEROS          TO  ECEDBENO
001050*    ELSE
001051         MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
001052
001053     GO TO 8100-SEND-INITIAL-MAP.
001054     EJECT
001055 2200-AH-BENEFIT.
001056
001057     MOVE '2'                    TO  PI-PREV-BENEFIT.
001058
001059     MOVE '5'                    TO  WS-CFK-RECORD-TYPE.
001060     MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
001061     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
001062     PERFORM 3000-LOCATE-BENEFIT.
001063
001064     IF WS-CALC-CD IS EQUAL TO 'Z'
001065         MOVE AL-UNNOF           TO  ETERMA.
001066
001067     MOVE '2'                    TO  CP-PROCESS-TYPE.
001068     MOVE 'A'                    TO  CP-BENEFIT-TYPE.
001069     MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
001070                                     CP-RATING-METHOD.
001071     MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
001072     MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
001073     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
001074     MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
001075     IF CM-AH-ORIG-TERM IS EQUAL TO 0
001076         MOVE 1                  TO  CP-ORIGINAL-TERM
001077     ELSE
001078         MOVE CM-AH-ORIG-TERM    TO  CP-ORIGINAL-TERM.
001079     MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
001080     MOVE '4'                    TO  CP-REM-TERM-METHOD.
001081     MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
001082     MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
001083
001084*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
001085     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
001086     MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
001087     MOVE SPACES                 TO  WS-CFK-ACCESS.
001088     MOVE CM-STATE               TO  WS-CFK-STATE.
001089     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
001090     MOVE 'Y'                    TO  WS-FREE-LOOK.
001091
001092     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
001093
001094     MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
001095
001096     PERFORM 9800-LINK-REM-TERM.
001097
001098     MOVE PI-AH-OVERRIDE-L12     TO  EKINDO.
001099     MOVE CM-AH-BENEFIT-CD       TO  ECODEO.
001100     MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
001101     MOVE CM-AH-ORIG-TERM        TO  ETERMO.
001102     MOVE CP-REMAINING-TERM-1    TO  ERTERMO.
001103     MOVE WS-COMMENT             TO  ECOMENTO.
001104     MOVE CM-AH-CRITICAL-PERIOD  TO  ECRITPDO.
001105     MOVE CM-AH-PREMIUM-AMT      TO  EPREMO.
001106     MOVE CM-AH-NSP-PREMIUM-AMT  TO  EREINSPO.
001107     IF CM-AH-CLP NOT NUMERIC
001108        MOVE ZEROS               TO CM-AH-CLP
001109     END-IF
001110     IF CM-ADDL-CLP NOT NUMERIC
001111        MOVE ZEROS               TO CM-ADDL-CLP
001112     END-IF
001113     MOVE CM-CLP-STATE           TO  ECLPSTO
001114     MOVE CM-AH-CLP              TO  ECLPO
001115     MOVE CM-ADDL-CLP            TO  EACLPO
001116     MOVE CM-AH-BENEFIT-AMT      TO  EBENEO.
001117     MOVE CM-AH-ITD-CANCEL-AMT   TO  EITDREFO.
001118     MOVE CM-AH-EXIT-BATCH       TO  EEXBTCHO.
001119     MOVE CM-AH-ITD-AH-PMT       TO  EITDPMTO.
001120     IF CM-AH-PREMIUM-RATE NUMERIC
001121         MOVE CM-AH-PREMIUM-RATE TO  EPRRTO
001122     ELSE
001123         MOVE ZEROS              TO  EPRRTO.
001124     MOVE CM-AH-DEV-CODE         TO  EDVCDO.
001125     MOVE CM-AH-COMM-PCT         TO  EACCPCTO.
001126
001127     MOVE SPACES                 TO  ECLSTATO.
001128     IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
001129         MOVE '*'                TO  EASRISKO
001130         MOVE 'A'                TO  WS-CLAIM-SW
001131         PERFORM 7600-CHECK-FOR-CLAIM
001132     ELSE
001133         MOVE ' '                TO  PI-CLAIM-SW
001134                                     EASRISKO.
001135
001136     IF CM-AH-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
001137         MOVE SPACES                 TO  DC-OPTION-CODE
001138         MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
001139         PERFORM 8500-DATE-CONVERSION
001140         MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
001141
001142     IF CM-AH-PAID-THRU-DT IS NOT = LOW-VALUES AND SPACES
001143        IF NOT PI-USES-PAID-TO
001144           MOVE ' '                    TO  DC-OPTION-CODE
001145           MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
001146           PERFORM 8500-DATE-CONVERSION
001147           MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO
001148        ELSE
001149           MOVE '6'                    TO  DC-OPTION-CODE
001150           MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
001151           MOVE +1                     TO  DC-ELAPSED-DAYS
001152           MOVE +0                     TO  DC-ELAPSED-MONTHS
001153           PERFORM 8500-DATE-CONVERSION
001154           MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO.
001155
001156******************************************************************
001157*                                                                *
001158*               DISPLAY ENTRY STATUS CODES                       *
001159*                                                                *
001160******************************************************************
001161
001162     MOVE SPACES                 TO  EENTSTO.
001163
001164     IF CM-ENTRY-STATUS = '1'
001165         MOVE 'NORM'             TO  EENTSTO.
001166     IF CM-ENTRY-STATUS = '2'
001167         MOVE 'PEND'             TO  EENTSTO
001168         MOVE 'P'                TO  PI-PEND-SW.
001169     IF CM-ENTRY-STATUS = '4'
001170         MOVE 'CONV'             TO  EENTSTO.
001171     IF CM-ENTRY-STATUS = '5'
001172         MOVE 'REIS'             TO  EENTSTO.
001173     IF CM-ENTRY-STATUS = 'M'
001174         MOVE 'MONTHLY'          TO  EENTSTO.
001175     IF CM-ENTRY-STATUS = '9'
001176         MOVE 'REIN'             TO  EENTSTO.
001177
001178     IF CM-ENTRY-STATUS = 'D'
001179         MOVE 'DECL'             TO  EENTSTO.
001180
001181     IF CM-ENTRY-STATUS = 'V'
001182         MOVE 'VOID'             TO  EENTSTO.
001183
001184     IF CM-ENTRY-STATUS = 'U'
001185         MOVE 'UNDERWRT'         TO  EENTSTO.
001186
001187******************************************************************
001188*                                                                *
001189*               DISPLAY CURRENT STATUS CODES                     *
001190*                                                                *
001191******************************************************************
001192
001193     MOVE SPACES                 TO  ECURSTO.
001194
001195     IF CM-AH-CURRENT-STATUS = '1' OR '4'
001196         IF CP-REMAINING-TERM-3 = ZEROS
001197             MOVE 'EXPIRED'      TO  ECURSTO
001198         ELSE
001199             MOVE 'ACTIVE'       TO  ECURSTO.
001200
001201     IF CM-AH-CURRENT-STATUS = '2'
001202         MOVE 'PEND'             TO  ECURSTO
001203         MOVE 'P'                TO  PI-PEND-SW.
001204
001205     IF CM-AH-CURRENT-STATUS = '3'
001206         MOVE 'RESTORE'          TO  ECURSTO.
001207
001208     IF CM-AH-CURRENT-STATUS = '5'
001209         MOVE 'REISSUE'          TO  ECURSTO.
001210     IF CM-AH-CURRENT-STATUS = 'M'
001211         MOVE 'MONTHLY'          TO  ECURSTO.
001212
001213     IF CM-AH-CURRENT-STATUS = '6'
001214         MOVE 'LMP DIS'          TO  ECURSTO.
001215
001216     IF CM-AH-CURRENT-STATUS = '7'
001217         MOVE 'DEATH'            TO  ECURSTO.
001218
001219     IF CM-AH-CURRENT-STATUS = '8'
001220         MOVE 'CANCEL'           TO  ECURSTO.
001221
001222     IF CM-AH-CURRENT-STATUS = '9'
001223         MOVE 'RE-ONLY'          TO  ECURSTO.
001224
001225     IF CM-AH-CURRENT-STATUS = 'D'
001226         MOVE 'DECLINE'          TO  ECURSTO.
001227
001228     IF CM-AH-CURRENT-STATUS = 'V'
001229         MOVE 'VOID'             TO  ECURSTO.
001230
001231******************************************************************
001232*                                                                *
001233*            CALCULATE REMAINING BENEFIT AMOUNT                  *
001234*                                                                *
001235******************************************************************
001236
001237     IF CM-AH-CURRENT-STATUS = '6' OR '7' OR '8'
001238         MOVE ZEROS              TO  EREMBENO
001239     ELSE
001240        PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
001241        MOVE ZEROS               TO CP-R-MAX-MON-BEN
001242        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
001243           AND (ACCT-FOUND)
001244           AND (AM-DCC-PRODUCT-CODE = 'DDF')
001245           MOVE ' '             TO WS-PDEF-RECORD-SW
001246           PERFORM 2600-GET-MAX-BENEFIT
001247                                 THRU 2600-EXIT
001248           IF PDEF-FOUND
001249              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
001250                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
001251              ELSE
001252                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
001253              END-IF
001254              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
001255                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
001256
001257              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
001258                 (P1 > +11)
001259                 OR (PD-PROD-CODE (P1) = 'A'
001260                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
001261              END-PERFORM
001262              IF P1 < +12
001263                 MOVE PD-MAX-AMT (P1) TO CP-R-MAX-MON-BEN
001264              END-IF
001265           END-IF
001266        END-IF
001267        IF CP-R-MAX-MON-BEN = ZEROS
001268           MULTIPLY CM-AH-BENEFIT-AMT BY CP-REMAINING-TERM-3
001269                                    GIVING EREMBENO
001270        ELSE
001271           MULTIPLY CP-R-MAX-MON-BEN BY CP-REMAINING-TERM-3
001272                                    GIVING EREMBENO
001273        END-IF
001274     END-IF
001275
001276******************************************************************
001277*                                                                *
001278*            CALCULATE UNEARNED PREMIUM AMOUNT                   *
001279*                      ONE MONTH EARNED PREMIUM AMOUNT           *
001280*                                                                *
001281******************************************************************
001282
001283     MOVE '2'                    TO  CP-PROCESS-TYPE.
001284     MOVE CM-STATE               TO  CP-STATE.
001285     MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
001286     MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
001287     MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
001288     MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
001289                                     CP-RATING-BENEFIT-AMT.
001290     MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
001291     MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
001292     MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
001293
001294     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
001295     MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
001296     MOVE SPACES                 TO  WS-CFK-ACCESS.
001297     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
001298     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
001299     MOVE CF-AH-OVERRIDE-L1      TO  CP-AH-OVERRIDE-CODE.
001300     MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
001301     MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
001302     MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
001303
001304     MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
001305     MOVE PI-AH-OVERRIDE-L1      TO  WS-BENEFIT-TYPE.
001306     MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
001307     MOVE SPACES                 TO  WS-CFK-ACCESS.
001308     MOVE CM-STATE               TO  WS-CFK-STATE.
001309     MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
001310     PERFORM 3500-READ-CNTL THRU 3500-EXIT.
001311     MOVE CF-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
001312     MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
001313
001314     IF CP-STATE-STD-ABBRV = 'OR'
001315         COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
001316                                         CM-AH-ORIG-TERM.
001317
001318*    IF CF-TRUNCATED-LIFE (WS-INDEX)
001319*        MOVE CM-PAY-FREQUENCY   TO  CP-PAY-FREQUENCY
001320*        MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX) TO
001321*                                           CP-EARNING-METHOD.
001322
001323     MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
001324
001325     IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
001326         MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
001327                                     WS-VALUATION-DT
001328     ELSE
001329         MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
001330
001331     PERFORM 9800-LINK-REM-TERM.
001332     MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
001333     PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
001334     MOVE CP-CALC-REFUND         TO  EUEPREMO
001335                                     WS-CALC-REFUND.
001336     ADD +1                      TO  CP-REMAINING-TERM.
001337     IF CP-REMAINING-TERM IS EQUAL TO CM-AH-ORIG-TERM
001338         COMPUTE ONE-MON-EARNED = CM-AH-PREMIUM-AMT -
001339                                                CP-CALC-REFUND
001340     ELSE
001341         PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
001342         COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
001343                                                CP-CALC-REFUND.
001344
001345     MOVE ONE-MON-EARNED         TO  EOMEARNO.
001346
001347     IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
001348         MOVE SPACES                 TO  DC-OPTION-CODE
001349         MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
001350         PERFORM 8500-DATE-CONVERSION
001351         MOVE AL-UANON               TO  EUEDTA
001352         MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
001353
001354******************************************************************
001355*                                                                *
001356*                     DISPLAY CANCEL DATE                        *
001357*                             CANCEL EXIT DATE                   *
001358*                             CANCEL EXIT STATUS                 *
001359*                                                                *
001360******************************************************************
001361
001362     MOVE SPACES                       TO  EEXITDTO.
001363
001364     IF CM-AH-CANCEL-EXIT-DT IS NOT = LOW-VALUES AND SPACES
001365         MOVE SPACES                   TO  DC-OPTION-CODE
001366         MOVE CM-AH-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
001367         PERFORM 8500-DATE-CONVERSION
001368         IF NOT DATE-CONVERSION-ERROR
001369             MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
001370         ELSE
001371             MOVE SPACES               TO  EEXITDTO.
001372
001373     MOVE SPACES                       TO  ECANDTO.
001374
001375     IF CM-AH-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
001376         MOVE SPACES                   TO  DC-OPTION-CODE
001377         MOVE CM-AH-CANCEL-DT          TO  DC-BIN-DATE-1
001378         PERFORM 8500-DATE-CONVERSION
001379         IF NOT DATE-CONVERSION-ERROR
001380             MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
001381         ELSE
001382             MOVE SPACES               TO  ECANDTO.
001383
001384     MOVE SPACES                       TO  EEXITSTO.
001385
001386     IF CM-AH-STATUS-AT-CANCEL = ' '
001387         MOVE SPACES             TO  EEXITSTO.
001388     IF CM-AH-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
001389         MOVE 'ACTIVE'           TO  EEXITSTO.
001390     IF CM-AH-STATUS-AT-CANCEL = '6'
001391         MOVE 'PREV SETTLE'      TO  EEXITSTO.
001392     IF CM-AH-STATUS-AT-CANCEL = '7'
001393         MOVE 'PREV DTH'         TO  EEXITSTO.
001394     IF CM-AH-STATUS-AT-CANCEL = '8'
001395         MOVE 'PREV CANCEL'      TO  EEXITSTO.
001396
001397******************************************************************
001398*                                                                *
001399*                 DISPLAY CLAIM SETTLEMENT EXIT DATE             *
001400*                         CLAIM SETTLEMENT STATUS                *
001401*                                                                *
001402******************************************************************
001403
001404     MOVE SPACES                            TO  EEXDATEO.
001405
001406     IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES AND SPACE
001407        MOVE SPACES                    TO  DC-OPTION-CODE
001408        MOVE CM-AH-SETTLEMENT-EXIT-DT  TO  DC-BIN-DATE-1
001409        PERFORM 8500-DATE-CONVERSION
001410        IF NOT DATE-CONVERSION-ERROR
001411           MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
001412        ELSE
001413           MOVE SPACES                 TO  EEXDATEO.
001414
001415     MOVE SPACES                 TO  EEXSTATO.
001416
001417     IF CM-AH-STATUS-AT-SETTLEMENT = ' '
001418         MOVE SPACES             TO  EEXSTATO.
001419     IF CM-AH-STATUS-AT-SETTLEMENT EQUAL '1' OR '2' OR '4' OR '9'
001420         MOVE 'ACTIVE'           TO  EEXSTATO.
001421     IF CM-AH-STATUS-AT-SETTLEMENT = '6'
001422         MOVE 'PREV SETTLE'      TO  EEXSTATO.
001423     IF CM-AH-STATUS-AT-SETTLEMENT = '7'
001424         MOVE 'PREV DTH'         TO  EEXSTATO.
001425     IF CM-AH-STATUS-AT-SETTLEMENT = '8'
001426         MOVE 'PREV CAN'         TO  EEXSTATO.
001427
001428     if crtt-not-read
001429        move cm-control-primary  to ws-elcrtt-key
001430        move 'C'                 to ws-cs-rec-type
001431        
      * exec cics read
001432*          dataset   ('ELCRTT')
001433*          ridfld    (ws-elcrtt-key)
001434*          into      (certificate-trailers)
001435*          resp      (ws-response)
001436*       end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006242' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036323432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 ws-elcrtt-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001437        if resp-normal
001438           set good-hit-on-trlr  to true
001439        else
001440           set crtt-not-found    to true
001441        end-if
001442     end-if
001443     if good-hit-on-trlr
001444        evaluate true
001445           when cs-ah-refund-method = '1'
001446              move 'R78'         to refmetho
001447           when cs-ah-refund-method = '2'
001448              move 'PRO'         to refmetho
001449           when cs-ah-refund-method = '3'
001450              move 'CALIF'       to refmetho
001451           when cs-ah-refund-method = '4'
001452              move 'FARM'        to refmetho
001453           when cs-ah-refund-method = '5'
001454              move 'NET'         to refmetho
001455           when cs-ah-refund-method = '6'
001456              move 'ANTIC'       to refmetho
001457           when cs-ah-refund-method = '8'
001458              move 'MEAN'        to refmetho
001459           when cs-ah-refund-method = '9'
001460              move 'SUMDI'       to refmetho
001461           when cs-ah-refund-method = 'G'
001462              move 'GAPNR'       to refmetho
001463           when cs-ah-refund-method = 'S'
001464              move 'GAPA'        to refmetho
001465           when cs-ah-refund-method = 'D'
001466              move 'DDF'         to refmetho
001467           when cs-ah-refund-method = 'I'
001468              move 'DDFIU'       to refmetho
001469           when cs-ah-refund-method = 'R'
001470              move 'REPO'        to refmetho
001471           when other
001472              move cs-ah-refund-method
001473                                 to refmetho
001474        end-evaluate
001475     end-if
001476
001477******************************************************************
001478*                                                                *
001479*                   DISPLAY EXTENSION DAYS                       *
001480*                                                                *
001481******************************************************************
001482
001483     IF CM-PMT-EXTENSION-DAYS NUMERIC
001484         MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
001485     ELSE
001486         MOVE ZEROS                  TO  EEXTDAYO.
001487
001488******************************************************************
001489*                                                                *
001490*                   DISPLAY CEDED BENEFIT                        *
001491*                                                                *
001492******************************************************************
001493
001494*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
001495*        IF CM-AH-CEDED-BENEFIT NUMERIC
001496*            MOVE CM-AH-CEDED-BENEFIT
001497*                                TO  ECEDBENO
001498*             ELSE
001499*            MOVE ZEROS          TO  ECEDBENO
001500*    ELSE
001501         MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
001502
001503     GO TO 8100-SEND-INITIAL-MAP.
001504
001505     EJECT
001506
001507 2500-GET-ERACCT.
001508
001509     MOVE CM-CONTROL-PRIMARY (1:22)
001510                                 TO WS-ACCT-KEY
001511
001512     MOVE ' '                    TO WS-ACCT-RECORD-SW
001513
001514     
      * EXEC CICS STARTBR
001515*        DATASET  ('ERACCT')
001516*        RIDFLD   (WS-ACCT-KEY)
001517*        GTEQ
001518*        RESP     (WS-RESPONSE)
001519*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006325' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303036333235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001520
001521     IF NOT RESP-NORMAL
001522        GO TO 2500-EXIT
001523     END-IF
001524
001525     .
001526 2500-READ-ERACCT-NEXT.
001527
001528     
      * EXEC CICS READNEXT
001529*        DATASET  ('ERACCT')
001530*        INTO     (ACCOUNT-MASTER)
001531*        RIDFLD   (WS-ACCT-KEY)
001532*        RESP     (WS-RESPONSE)
001533*    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006339' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303036333339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001534
001535     IF NOT RESP-NORMAL
001536        GO TO 2500-EXIT
001537     END-IF
001538
001539     IF AM-CONTROL-PRIMARY (1:20) = CM-CONTROL-PRIMARY (1:20)
001540        IF (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
001541           AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
001542           SET ACCT-FOUND        TO TRUE
001543        ELSE
001544           GO TO 2500-READ-ERACCT-NEXT
001545     END-IF
001546
001547     .
001548 2500-EXIT.
001549     EXIT.
001550
001551
001552 2600-GET-MAX-BENEFIT.
001553
001554     MOVE ' '                    TO WS-PDEF-RECORD-SW
001555
001556     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
001557     MOVE CM-STATE               TO ERPDEF-STATE
001558     if cm-clp-state not = cm-state and spaces and zeros
001559        move cm-clp-state        to erpdef-state
001560     end-if
001561     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
001562     MOVE 'A'                    TO ERPDEF-BEN-TYPE
001563     MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
001564     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
001565     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
001566
001567     
      * EXEC CICS STARTBR
001568*        DATASET  ('ERPDEF')
001569*        RIDFLD   (ERPDEF-KEY)
001570*        GTEQ
001571*        RESP     (WS-RESPONSE)
001572*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006378' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303036333738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001573
001574     IF NOT RESP-NORMAL
001575        GO TO 2600-EXIT
001576     END-IF
001577
001578     .
001579 2600-READNEXT.
001580
001581     
      * EXEC CICS READNEXT
001582*       DATASET  ('ERPDEF')
001583*       SET      (ADDRESS OF PRODUCT-MASTER)
001584*       RIDFLD   (ERPDEF-KEY)
001585*       RESP     (WS-RESPONSE)
001586*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00006392' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303036333932' TO DFHEIV0
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
001587
001588     IF NOT RESP-NORMAL
001589        GO TO 2600-ENDBR
001590     END-IF
001591
001592     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
001593        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
001594           MOVE 'Y'              TO WS-PDEF-RECORD-SW
001595        ELSE
001596           GO TO 2600-READNEXT
001597        END-IF
001598     ELSE
001599        GO TO 2600-ENDBR
001600     END-IF
001601
001602     .
001603 2600-ENDBR.
001604
001605     
      * EXEC CICS ENDBR
001606*       DATASET  ('ERPDEF')
001607*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006416' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036343136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001608
001609     .
001610 2600-EXIT.
001611     EXIT.
001612
001613 3000-LOCATE-BENEFIT         SECTION.
001614
001615     
      * EXEC CICS HANDLE CONDITION
001616*            NOTFND  (3000-EXIT)
001617*    END-EXEC.
      *    MOVE '"$I                   ! $ #00006426' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303036343236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001618
001619     MOVE SPACES                 TO  WS-KIND
001620                                     WS-CFK-ACCESS.
001621     MOVE ZERO                   TO  WS-NOT-FOUND.
001622     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
001623     MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
001624
001625     
      * EXEC CICS READ
001626*        DATASET  (WS-CONTROL-FILE-DSID)
001627*        RIDFLD   (WS-CONTROL-FILE-KEY)
001628*        SET      (ADDRESS OF CONTROL-FILE)
001629*        GTEQ
001630*    END-EXEC.
      *    MOVE '&"S        G          (   #00006436' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303036343336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001631
001632     IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID OR
001633         WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
001634             GO TO 3000-EXIT.
001635
001636     MOVE +1                     TO  WS-INDEX.
001637
001638 3000-LOOKUP-BENEFIT.
001639
001640     IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
001641         MOVE CF-BENEFIT-ALPHA (WS-INDEX)   TO  WS-KIND
001642         MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO  WS-CALC-CD
001643         MOVE CF-BENEFIT-DESCRIP (WS-INDEX) TO  WS-BENEFIT-DESCRIP
001644         MOVE CF-BENEFIT-COMMENT (WS-INDEX) TO  WS-COMMENT
001645         MOVE CF-BENEFIT-CODE    (WS-INDEX) TO  WS-BENEFIT-CODE
001646         MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)         TO
001647                                             WS-LF-COVERAGE-TYPE
001648         MOVE CF-CO-EARNINGS-CALC (WS-INDEX)         TO
001649                                             WS-EARNINGS-CALC
001650         MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX)    TO
001651                                             WS-OVRD-EARNINGS-CALC
001652         MOVE +1                            TO  WS-NOT-FOUND
001653         GO TO 3000-EXIT.
001654
001655     IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
001656         GO TO 3000-EXIT.
001657
001658     IF WS-INDEX LESS THAN +8
001659         ADD +1                  TO  WS-INDEX
001660         GO TO 3000-LOOKUP-BENEFIT.
001661
001662 3000-EXIT.
001663     EXIT.
001664
001665 3500-READ-CNTL                  SECTION.
001666
001667     
      * EXEC CICS HANDLE CONDITION
001668*            NOTFND  (3500-NOTFND)
001669*    END-EXEC.
      *    MOVE '"$I                   ! % #00006478' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303036343738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001670
001671     
      * EXEC CICS READ
001672*        DATASET  (WS-CONTROL-FILE-DSID)
001673*        RIDFLD   (WS-CONTROL-FILE-KEY)
001674*        SET      (ADDRESS OF CONTROL-FILE)
001675*    END-EXEC.
      *    MOVE '&"S        E          (   #00006482' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036343832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001676
001677     IF WS-FREE-LOOK = 'Y'
001678         MOVE SPACE              TO WS-FREE-LOOK
001679         GO TO 3500-EXIT.
001680
001681     IF WS-CFK-RECORD-TYPE IS EQUAL TO '3'
001682         NEXT SENTENCE
001683     ELSE
001684         GO TO 3500-EXIT.
001685
001686     MOVE +1                     TO  WS-INDEX.
001687
001688 3500-FIND-ST-CALC-METHOD.
001689
001690     IF WS-BENEFIT-TYPE = CF-ST-BENEFIT-KIND (WS-INDEX)  AND
001691        WS-BENEFIT-NO = CF-ST-BENEFIT-CD (WS-INDEX)
001692         MOVE CF-ST-REM-TERM-CALC (WS-INDEX)    TO
001693                                     CP-REM-TERM-METHOD
001694         GO TO 3500-EXIT.
001695
001696     IF WS-INDEX LESS +50
001697         ADD +1                  TO  WS-INDEX
001698         GO TO 3500-FIND-ST-CALC-METHOD.
001699
001700 3500-NOTFND.
001701
001702     IF WS-FREE-LOOK = 'Y'
001703         MOVE SPACE              TO WS-FREE-LOOK
001704         MOVE ZERO               TO CF-ST-FREE-LOOK-PERIOD.
001705
001706 3500-EXIT.
001707     EXIT.
001708
001709     EJECT
001710 7500-DEEDIT.
001711     
      * EXEC CICS BIF
001712*         DEEDIT
001713*         FIELD  (DEEDIT-FIELD)
001714*         LENGTH (15)
001715*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006522' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036353232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001716
001717 7500-EXIT.
001718     EXIT.
001719
001720 7600-CHECK-FOR-CLAIM            SECTION.
001721
001722     MOVE WS-CK-COMPANY-CD  TO  WS-CL-COMPANY-CD.
001723     MOVE WS-CK-CERT-NO     TO  WS-CL-CERT-NO.
001724
001725     
      * EXEC CICS HANDLE CONDITION
001726*        NOTFND (7600-CONTINUE)
001727*        ENDFILE (7600-CONTINUE)
001728*    END-EXEC.
      *    MOVE '"$I''                  ! & #00006536' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303036353336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001729
001730     
      * EXEC CICS IGNORE CONDITION
001731*        DUPKEY
001732*    END-EXEC.
      *    MOVE '"*$                   !   #00006541' TO DFHEIV0
           MOVE X'222A24202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303036353431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001733
001734     
      * EXEC CICS STARTBR
001735*        DATASET   (WS-CLAIM-MASTER-DSID)
001736*        RIDFLD    (WS-CLAIM-KEY)
001737*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006545' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001738
001739     MOVE 'Y'                    TO  WS-BROWSE-SW.
001740
001741 7600-NEXT-CLAIM.
001742
001743     
      * EXEC CICS READNEXT
001744*        DATASET   (WS-CLAIM-MASTER-DSID)
001745*        RIDFLD    (WS-CLAIM-KEY)
001746*        SET       (ADDRESS OF CLAIM-MASTER)
001747*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006554' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036353534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001748
001749     IF WS-CK-COMPANY-CD NOT = WS-CL-COMPANY-CD  OR
001750        WS-CK-CERT-NO    NOT = WS-CL-CERT-NO
001751           GO TO 7600-CONTINUE.
001752
001753     IF CM-CARRIER     NOT = CL-CARRIER       OR
001754        CM-GROUPING    NOT = CL-CERT-GROUPING OR
001755        CM-STATE       NOT = CL-CERT-STATE    OR
001756        CM-ACCOUNT     NOT = CL-CERT-ACCOUNT  OR
001757        CM-CERT-EFF-DT NOT = CL-CERT-EFF-DT
001758           GO TO 7600-NEXT-CLAIM.
001759
001760     MOVE 'Y'                    TO  PI-CLAIM-SW.
001761
001762     IF CHECK-AH-CLAIM
001763        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
001764         OR 'F'
001765         OR 'B' OR 'H'
001766           IF CLAIM-IS-OPEN
001767              MOVE 'OPEN'   TO ECLSTATO
001768           ELSE
001769              MOVE 'CLOSED' TO ECLSTATO
001770           END-IF
001771        ELSE
001772           GO TO 7600-NEXT-CLAIM
001773        END-IF
001774     END-IF
001775
001776     IF CHECK-LF-CLAIM
001777         IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
001778             IF CLAIM-IS-OPEN
001779                 MOVE 'OPEN'   TO ECLSTATO
001780             ELSE
001781                 MOVE 'CLOSED' TO ECLSTATO
001782         ELSE
001783             GO TO 7600-NEXT-CLAIM.
001784
001785 7600-CONTINUE.
001786
001787     IF WS-BROWSE-SW IS EQUAL TO 'Y'
001788         
      * EXEC CICS ENDBR
001789*            DATASET   (WS-CLAIM-MASTER-DSID)
001790*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006599' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036353939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001791     ELSE
001792         MOVE 'NO CLAIM'         TO  ECLSTATO.
001793
001794 7600-EXIT.
001795     EXIT.
001796
001797     EJECT
001798 8100-SEND-INITIAL-MAP           SECTION.
001799
001800     MOVE SAVE-DATE              TO  EDATEO.
001801     MOVE EIBTIME                TO  WS-TIME-WORK.
001802     MOVE WS-TIME                TO  ETIMEO.
001803     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
001804     MOVE PI-PROCESSOR-ID        TO  USERIDO.
001805     MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
001806     MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
001807     MOVE -1                     TO  EPFKEYL.
001808
001809     IF PI-USES-PAID-TO
001810        MOVE 'PAID  TO  :'  TO EPTHRHDO.
001811
001812     IF CREDIT-SESSION
001813            AND
001814        PI-CLAIM-SW IS EQUAL TO 'Y'
001815         MOVE AL-SANON           TO  EPFKEY7A
001816     ELSE
001817         MOVE AL-SADOF           TO  EPFKEY7A.
001818
001819     
      * EXEC CICS SEND
001820*        FROM   (EL127EI)
001821*        MAPSET (WS-MAPSET-NAME)
001822*        MAP    (WS-MAP-NAME)
001823*        CURSOR
001824*        ERASE
001825*    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006630' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303036363330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001826
001827     GO TO 9100-RETURN-TRAN.
001828
001829 8100-EXIT.
001830     EXIT.
001831
001832     EJECT
001833 8200-SEND-DATAONLY SECTION.
001834
001835     IF PI-1ST-TIME-SW = '1'
001836         GO TO 8100-SEND-INITIAL-MAP.
001837
001838     MOVE SAVE-DATE              TO  EDATEO.
001839     MOVE EIBTIME                TO  WS-TIME-WORK.
001840     MOVE WS-TIME                TO  ETIMEO.
001841     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
001842     MOVE PI-PROCESSOR-ID        TO  USERIDO.
001843     MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
001844     MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
001845     MOVE -1                     TO  EPFKEYL.
001846
001847     IF PI-USES-PAID-TO
001848        MOVE 'PAID  TO  :'  TO EPTHRHDO.
001849
001850     IF CREDIT-SESSION
001851            AND
001852        PI-CLAIM-SW IS EQUAL TO 'Y'
001853         MOVE AL-SANON           TO  EPFKEY7A
001854     ELSE
001855         MOVE AL-SADOF           TO  EPFKEY7A.
001856
001857     
      * EXEC CICS SEND DATAONLY
001858*        FROM   (EL127EI)
001859*        MAPSET (WS-MAPSET-NAME)
001860*        MAP    (WS-MAP-NAME)
001861*        CURSOR
001862*    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006668' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303036363638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001863
001864     GO TO 9100-RETURN-TRAN.
001865
001866 8200-EXIT.
001867     EXIT.
001868
001869     EJECT
001870 8300-SEND-TEXT SECTION.
001871
001872     
      * EXEC CICS SEND TEXT
001873*        FROM   (LOGOFF-TEXT)
001874*        LENGTH (LOGOFF-LENGTH)
001875*        ERASE
001876*        FREEKB
001877*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006683' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303036363833' TO DFHEIV0
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
           
001878
001879     
      * EXEC CICS RETURN
001880*    END-EXEC.
      *    MOVE '.(                    ''   #00006690' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036363930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001881
001882 8300-EXIT.
001883     EXIT.
001884
001885     EJECT
001886 8500-DATE-CONVERSION SECTION.
001887
001888     
      * EXEC CICS LINK
001889*        PROGRAM  (ELDATCV)
001890*        COMMAREA (DATE-CONVERSION-DATA)
001891*        LENGTH   (DC-COMM-LENGTH)
001892*    END-EXEC.
      *    MOVE '."C                   (   #00006699' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036363939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001893
001894 8500-EXIT.
001895     EXIT.
001896
001897 8880-NOT-FOUND SECTION.
001898
001899     MOVE -1                     TO  EPFKEYL.
001900     MOVE ER-0142                TO  EMI-ERROR.
001901     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001902     GO TO 8100-SEND-INITIAL-MAP.
001903
001904 8880-EXIT.
001905     EXIT.
001906
001907 9000-RETURN-CICS SECTION.
001908
001909     MOVE EL005                  TO  WS-PROGRAM-ID.
001910     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
001911     PERFORM 9300-XCTL.
001912
001913 9000-EXIT.
001914     EXIT.
001915
001916 9100-RETURN-TRAN SECTION.
001917
001918     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
001919     MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
001920
001921     
      * EXEC CICS RETURN
001922*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001923*        LENGTH   (PI-COMM-LENGTH)
001924*        TRANSID  (WS-TRANS-ID)
001925*    END-EXEC.
      *    MOVE '.(CT                  ''   #00006732' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036373332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001926
001927 9100-EXIT.
001928     EXIT.
001929
001930 9300-XCTL SECTION.
001931
001932     MOVE DFHENTER               TO  EIBAID.
001933
001934     
      * EXEC CICS XCTL
001935*        PROGRAM  (WS-PROGRAM-ID)
001936*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001937*        LENGTH   (PI-COMM-LENGTH)
001938*    END-EXEC.
      *    MOVE '.$C                   %   #00006745' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PROGRAM-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001939
001940 9300-EXIT.
001941     EXIT.
001942
001943     EJECT
001944 9400-CLEAR SECTION.
001945
001946     MOVE ' '                    TO PI-1ST-TIME-SW.
001947     MOVE PI-RETURN-TO-PROGRAM   TO  WS-PROGRAM-ID.
001948     GO TO 9300-XCTL.
001949
001950 9400-EXIT.
001951     EXIT.
001952
001953 9600-PGMIDERR SECTION.
001954
001955     
      * EXEC CICS HANDLE CONDITION
001956*        PGMIDERR (8300-SEND-TEXT)
001957*    END-EXEC.
      *    MOVE '"$L                   ! '' #00006766' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303036373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001958
001959     MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.
001960
001961     MOVE EL005                  TO  WS-PROGRAM-ID
001962                                     LOGOFF-PGM.
001963     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
001964     MOVE SPACES                 TO  PI-ENTRY-CD-1.
001965     GO TO 9300-XCTL.
001966
001967 9600-EXIT.
001968     EXIT.
001969
001970 9700-LINK-REM-AMT               SECTION.
001971
001972     
      * EXEC CICS LINK
001973*        PROGRAM   (ELRAMT)
001974*        COMMAREA  (CALCULATION-PASS-AREA)
001975*        LENGTH    (CP-COMM-LENGTH)
001976*    END-EXEC.
      *    MOVE '."C                   (   #00006783' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036373833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRAMT, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001977
001978 9700-EXIT.
001979     EXIT.
001980
001981 9800-LINK-REM-TERM              SECTION.
001982
001983     
      * EXEC CICS LINK
001984*        PROGRAM   (ELRTRM)
001985*        COMMAREA  (CALCULATION-PASS-AREA)
001986*        LENGTH    (CP-COMM-LENGTH)
001987*    END-EXEC.
      *    MOVE '."C                   (   #00006794' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036373934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001988
001989 9800-EXIT.
001990     EXIT.
001991
001992 9850-LINK-REFUND-RTN            SECTION.
001993
001994     
      * EXEC CICS LINK
001995*        PROGRAM   (ELRFND)
001996*        COMMAREA  (CALCULATION-PASS-AREA)
001997*        LENGTH    (CP-COMM-LENGTH)
001998*    END-EXEC.
      *    MOVE '."C                   (   #00006805' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRFND, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001999
002000     IF CP-RETURN-CODE NOT EQUAL ZEROS
002001         MOVE ZEROS TO CP-CALC-REFUND.
002002
002003 9850-EXIT.
002004     EXIT.
002005
002006
002007     EJECT
002008 9900-ERROR-FORMAT SECTION.
002009
002010     ADD +1                      TO  WS-ERROR-COUNT.
002011
002012     IF EMI-ERRORS-COMPLETE
002013         MOVE ZERO               TO  EMI-ERROR
002014         GO TO 9900-EXIT.
002015
002016     
      * EXEC CICS LINK
002017*        PROGRAM  (EL001)
002018*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
002019*        LENGTH   (EMI-COMM-LENGTH)
002020*    END-EXEC.
      *    MOVE '."C                   (   #00006827' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002021
002022     MOVE ZERO                   TO  EMI-ERROR.
002023
002024 9900-EXIT.
002025     EXIT.
002026
002027 9990-ERROR SECTION.
002028
002029     MOVE DFHEIBLK               TO  EMI-LINE1.
002030
002031     
      * EXEC CICS LINK
002032*        PROGRAM  (EL004)
002033*        COMMAREA (EMI-LINE1)
002034*        LENGTH   (72)
002035*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006842' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036383432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002036
002037     GO TO 8100-SEND-INITIAL-MAP.
002038
002039 9990-EXIT.
002040     EXIT.
002041
002042 9999-LAST-PARAGRAPH SECTION.
002043
002044     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
002045

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR,
                     0001-FIRST-TIME
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3500-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7600-CONTINUE,
                     7600-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
