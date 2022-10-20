      *((program: EL105.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL105 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 12/13/94 09:22:24.
000007*                            VMOD=2.017
000008*
000009*
000010
000011*AUTHOR.    LOGIC, INC.
000012*           DALLAS, TEXAS.
000013
000014*DATE-COMPILED.
000015
000016*SECURITY.   *****************************************************
000017*            *                                                   *
000018*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000019*            *                                                   *
000020*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000021*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000022*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000023*            *                                                   *
000024*            *****************************************************
000025
000026
000027*REMARKS.    TRANSACTION - EX12
000028
000029*        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
000030*    FOR THE CARRIER CONTROL RECORDS.
000031
000032*    SCREENS     - EL105A - CARRIER MAINTENANCE
000033
000034*    ENTERED BY  - EL101 OR EL601 - MAINTENANCE MENU
000035
000036*    EXIT TO     - EL101 OR EL601 - MAINTENANCE MENU
000037
000038*    INPUT FILE  - ELCNTL - CONTROL FILE - CARRIER RECORDS
000039
000040*    OUTPUT FILE - ELCNTL - CONTROL FILE - CARRIER RECORDS
000041
000042*    COMMAREA    - PASSED
000043
000044*    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101 OR EL601
000045*                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
000046*                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
000047*                  ENTRIES (XCTL FROM CICS VIA EX12) THE SCREEN
000048*                  WILL BE READ AND ACTION WILL BE BASED ON THE
000049*                  MAINTENANCE TYPE INDICATED.
000050******************************************************************
000051*                   C H A N G E   L O G
000052*
000053* Changes are marked by the Change Effective date.
000054*-----------------------------------------------------------------
000055*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000056* EFFECTIVE    NUMBER
000057*-----------------------------------------------------------------
000058* 112103    2003080800002  SMVA  ADD CLP TOLERANCE % FOR SECURE PA
000059* 092705  CR2005050300006  PEMA  ADD SPP LEASES
000060* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000061* 032813  CR2012051000001  AJRA  DISPLAY NEXT REAUDIT CHECK NUMBER
000062* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000063******************************************************************
000064
000065     EJECT
000066 ENVIRONMENT DIVISION.
000067
000068 DATA DIVISION.
000069
000070 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000071 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
000072
000073 77  FILLER  PIC X(32)  VALUE '********************************'.
000074 77  FILLER  PIC X(32)  VALUE '*    EL105 WORKING STORAGE     *'.
000075 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.017 **********'.
000076
000077*                            COPY ELCSCTM.
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
000078*                            COPY ELCSCRTY.
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
000079
000080 01  WS-DATE-AREA.
000081     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000082     05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
000083
000084 01  WS-ZIP-CODE-AREA.
000085     12  WS-ZIP-CODE.
000086         16  WS-ZIP-1            PIC X.
000087             88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
000088         16  WS-ZIP-2-3          PIC XX.
000089         16  WS-ZIP-4            PIC X.
000090         16  WS-ZIP-5            PIC X.
000091         16  WS-ZIP-6            PIC X.
000092         16  FILLER              PIC X(4).
000093     12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
000094         16  WS-ZIP-AM-1-CODE    PIC X(5).
000095         16  WS-ZIP-AM-1-PLUS4   PIC X(4).
000096         16  FILLER              PIC X.
000097     12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
000098         16  WS-ZIP-AM-2-CODE    PIC X(5).
000099         16  WS-ZIP-AM-2-DASH    PIC X.
000100         16  WS-ZIP-AM-2-PLUS4   PIC X(4).
000101     12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
000102         16  WS-ZIP-CAN-1-POST1  PIC XXX.
000103         16  WS-ZIP-CAN-1-POST2  PIC XXX.
000104         16  FILLER              PIC X(4).
000105     12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
000106         16  WS-ZIP-CAN-2-POST1  PIC XXX.
000107         16  FILLER              PIC X.
000108         16  WS-ZIP-CAN-2-POST2  PIC XXX.
000109         16  FILLER              PIC XXX.
000110
000111     12  WS-ZIP-CODE-NUM         PIC 9(9).
000112
000113 01  FILLER                          COMP-3.
000114     05  TIME-IN                     PIC S9(7)       VALUE ZERO.
000115     05  TIME-OUT                    REDEFINES
000116         TIME-IN                     PIC S9(3)V9(4).
000117
000118     05  WS-EXPENSE-DOLLAR           PIC S9(3)V99 VALUE ZERO.
000119     05  WS-EXPENSE-PERCENT          PIC S9(3)V99 VALUE ZERO.
000120
000121     05  WS-SPP-LEASE-COMM           PIC S9(5)V99    VALUE +0.
000122     05  WS-CLP-TOL-PCT              PIC S9(1)V9(4)  VALUE ZEROS.
000123     05  WS-TOL-PREM-PCT             PIC S9(1)V9(4)  VALUE ZEROS.
000124     05  WS-TOL-REF-PCT              PIC S9(1)V9(4)  VALUE ZEROS.
000125     05  WS-CR-OVR-SHT-PCT           PIC S9(1)V9(4)  VALUE ZEROS.
000126
000127     05  WS-IBNR-UEP-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
000128     05  WS-IBNR-R78-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
000129     05  WS-IBNR-PRO-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
000130
000131 01  FILLER                          COMP  SYNC.
000132     05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.
000133     05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.
000134
000135     05  APHONE-LENGTH               PIC S9(4)       VALUE +12.
000136     05  AEXPCA-LENGTH               PIC S9(4)       VALUE +7.
000137     05  AEXPCP-LENGTH               PIC S9(4)       VALUE +7.
000138     05  ALQCA-LENGTH                PIC S9(4)       VALUE +7.
000139     05  ALMRP-LENGTH                PIC S9(4)       VALUE +13.
000140     05  ALQCD-LENGTH                PIC S9(4)       VALUE +4.
000141     05  ALMDPP-LENGTH               PIC S9(4)       VALUE +4.
000142     05  ALDBC-LENGTH                PIC S9(4)       VALUE +4.
000143     05  ALMAP-LENGTH                PIC S9(4)       VALUE +13.
000144     05  ALMBP-LENGTH                PIC S9(4)       VALUE +4.
000145     05  ALMAPM-LENGTH               PIC S9(4)       VALUE +4.
000146     05  APCTCDT-LENGTH              PIC S9(4)       VALUE +7.
000147     05  IBNRPCT-LENGTH              PIC S9(4)       VALUE +7.
000148     05  AUEPPCT-LENGTH              PIC S9(4)       VALUE +7.
000149     05  AR78PCT-LENGTH              PIC S9(4)       VALUE +7.
000150     05  APROPCT-LENGTH              PIC S9(4)       VALUE +7.
000151
000152 01  FILLER.
000153     05  XCTL-EL126                  PIC X(5)    VALUE 'EL126'.
000154     05  XCTL-EL626                  PIC X(5)    VALUE 'EL626'.
000155     05  XCTL-EM626                  PIC X(5)    VALUE 'EM626'.
000156     05  XCTL-GL800                  PIC X(5)    VALUE 'GL800'.
000157     05  GETMAIN-SPACE               PIC X  VALUE SPACE.
000158     05  WS-CONTROL-FILE-KEY.
000159         10  WS-CFK-COMPANY-ID       PIC X(3).
000160         10  WS-CFK-RECORD-TYPE      PIC X.
000161*          88  LF-BENEFIT-MASTER                     VALUE '4'.
000162*          88  AH-BENEFIT-MASTER                     VALUE '5'.
000163         10  FILLER                  PIC XXX.
000164         10  WS-CFK-CARRIER-NO       PIC X.
000165         10  WS-CFK-SEQUENCE-NO      PIC S9(4)  COMP.
000166
000167     05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL105S'.
000168     05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL105A'.
000169
000170     05  FILLER                      REDEFINES
000171         WS-MAP-NAME.
000172         10  FILLER                  PIC XX.
000173         10  WS-MAP-NUMBER           PIC X(4).
000174         10  FILLER                  PIC XX.
000175
000176     05  THIS-PGM                    PIC X(8)      VALUE 'EL105'.
000177
000178     05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
000179
000180     05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.
000181
000182     05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX12'.
000183
000184     EJECT
000185 01  ERROR-MESSAGES.
000186     12  ER-0000                 PIC X(4)  VALUE '0000'.
000187     12  ER-0004                 PIC X(4)  VALUE '0004'.
000188     12  ER-0006                 PIC X(4)  VALUE '0006'.
000189     12  ER-0023                 PIC X(4)  VALUE '0023'.
000190     12  ER-0029                 PIC X(4)  VALUE '0029'.
000191     12  ER-0042                 PIC X(4)  VALUE '0042'.
000192     12  ER-0050                 PIC X(4)  VALUE '0050'.
000193     12  ER-0052                 PIC X(4)  VALUE '0052'.
000194     12  ER-0053                 PIC X(4)  VALUE '0053'.
000195     12  ER-0070                 PIC X(4)  VALUE '0070'.
000196     12  ER-0090                 PIC X(4)  VALUE '0090'.
000197     12  ER-0091                 PIC X(4)  VALUE '0091'.
000198     12  ER-0092                 PIC X(4)  VALUE '0092'.
000199     12  ER-0093                 PIC X(4)  VALUE '0093'.
000200     12  ER-0094                 PIC X(4)  VALUE '0094'.
000201     12  ER-0095                 PIC X(4)  VALUE '0095'.
000202     12  ER-0096                 PIC X(4)  VALUE '0096'.
000203     12  ER-0097                 PIC X(4)  VALUE '0097'.
000204     12  ER-0098                 PIC X(4)  VALUE '0098'.
000205     12  ER-0099                 PIC X(4)  VALUE '0099'.
000206     12  ER-0100                 PIC X(4)  VALUE '0100'.
000207     12  ER-0101                 PIC X(4)  VALUE '0101'.
000208     12  ER-0102                 PIC X(4)  VALUE '0102'.
000209     12  ER-0103                 PIC X(4)  VALUE '0103'.
000210     12  ER-0104                 PIC X(4)  VALUE '0104'.
000211     12  ER-0105                 PIC X(4)  VALUE '0105'.
000212     12  ER-0106                 PIC X(4)  VALUE '0106'.
000213     12  ER-0107                 PIC X(4)  VALUE '0107'.
000214     12  ER-0108                 PIC X(4)  VALUE '0108'.
000215     12  ER-0109                 PIC X(4)  VALUE '0109'.
000216     12  ER-0110                 PIC X(4)  VALUE '0110'.
000217     12  ER-0111                 PIC X(4)  VALUE '0111'.
000218     12  ER-0112                 PIC X(4)  VALUE '0112'.
000219     12  ER-0113                 PIC X(4)  VALUE '0113'.
000220     12  ER-0114                 PIC X(4)  VALUE '0114'.
000221     12  ER-0117                 PIC X(4)  VALUE '0117'.
000222     12  ER-0118                 PIC X(4)  VALUE '0118'.
000223     12  ER-0119                 PIC X(4)  VALUE '0119'.
000224     12  ER-0120                 PIC X(4)  VALUE '0120'.
000225     12  ER-0121                 PIC X(4)  VALUE '0121'.
000226     12  ER-0122                 PIC X(4)  VALUE '0122'.
000227     12  ER-0123                 PIC X(4)  VALUE '0123'.
000228     12  ER-0124                 PIC X(4)  VALUE '0124'.
000229     12  ER-0125                 PIC X(4)  VALUE '0125'.
000230     12  ER-0145                 PIC X(4)  VALUE '0145'.
000231     12  ER-0173                 PIC X(4)  VALUE '0173'.
000232     12  ER-0193                 PIC X(4)  VALUE '0193'.
000233     12  ER-0497                 PIC X(4)  VALUE '0497'.
000234     12  ER-0529                 PIC X(4)  VALUE '0529'.
000235     12  ER-0637                 PIC X(4)  VALUE '0637'.
000236     12  ER-0638                 PIC X(4)  VALUE '0638'.
000237     12  ER-1778                 PIC X(4)  VALUE '1778'.
000238     12  ER-2010                 PIC X(4)  VALUE '2010'.
000239     12  ER-2014                 PIC X(4)  VALUE '2014'.
000240     12  ER-2308                 PIC X(4)  VALUE '2308'.
000241     12  ER-3270                 PIC X(4)  VALUE '3270'.
000242     12  ER-7008                 PIC X(4)  VALUE '7008'.
000243     12  ER-7532                 PIC X(4)  VALUE '7532'.
000244     12  ER-8017                 PIC X(4)  VALUE '8017'.
000245     12  ER-8127                 PIC X(4)  VALUE '8127'.
000246     12  ER-8128                 PIC X(4)  VALUE '8128'.
000247
000248     EJECT
000249*                                    COPY ELCINTF.
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
000250     12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
000251         16  PI-1ST-TIME-SW          PIC S9    COMP-3.
000252         16  PI-MODE                 PIC X.
000253         16  PI-CARRIER-NUMBER       PIC X.
000254         16  PI-NEXT-CARRIER-NUMBER  PIC X.
000255         16  PI-LINE-COUNT           PIC S9(3) COMP-3.
000256         16  PI-BROWSE-SW            PIC S9    COMP-3.
000257         16  PI-END-OF-FILE          PIC S9    COMP-3.
000258         16  PI-PREV-MODE            PIC X.
000259         16  PI-PREV-CARRIER         PIC X.
000260         16  FILLER                  PIC X(630).
000261
000262     EJECT
000263*                                    COPY ELCJPFX.
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
000264                                     PIC X(750).
000265     EJECT
000266*                                    COPY ELCEMIB.
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
000267     EJECT
000268*                                    COPY ELCDATE.
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
000269     EJECT
000270*                                    COPY EL105S.
      *>>((file: EL105S))
000001 01  EL105AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  ADATEL PIC S9(0004) COMP.
000005     05  ADATEF PIC  X(0001).
000006     05  FILLER REDEFINES ADATEF.
000007         10  ADATEA PIC  X(0001).
000008     05  ADATEI PIC  X(0008).
000009*    -------------------------------
000010     05  ATIMEL PIC S9(0004) COMP.
000011     05  ATIMEF PIC  X(0001).
000012     05  FILLER REDEFINES ATIMEF.
000013         10  ATIMEA PIC  X(0001).
000014     05  ATIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  AMAINTL PIC S9(0004) COMP.
000017     05  AMAINTF PIC  X(0001).
000018     05  FILLER REDEFINES AMAINTF.
000019         10  AMAINTA PIC  X(0001).
000020     05  AMAINTI PIC  X(0001).
000021*    -------------------------------
000022     05  ACARIERL PIC S9(0004) COMP.
000023     05  ACARIERF PIC  X(0001).
000024     05  FILLER REDEFINES ACARIERF.
000025         10  ACARIERA PIC  X(0001).
000026     05  ACARIERI PIC  X(0001).
000027*    -------------------------------
000028     05  ACONAMEL PIC S9(0004) COMP.
000029     05  ACONAMEF PIC  X(0001).
000030     05  FILLER REDEFINES ACONAMEF.
000031         10  ACONAMEA PIC  X(0001).
000032     05  ACONAMEI PIC  X(0030).
000033*    -------------------------------
000034     05  FD1L PIC S9(0004) COMP.
000035     05  FD1F PIC  X(0001).
000036     05  FILLER REDEFINES FD1F.
000037         10  FD1A PIC  X(0001).
000038     05  FD1I PIC  X(0019).
000039*    -------------------------------
000040     05  ALPHLL PIC S9(0004) COMP.
000041     05  ALPHLF PIC  X(0001).
000042     05  FILLER REDEFINES ALPHLF.
000043         10  ALPHLA PIC  X(0001).
000044     05  ALPHLI PIC  X(0006).
000045*    -------------------------------
000046     05  ACAREOFL PIC S9(0004) COMP.
000047     05  ACAREOFF PIC  X(0001).
000048     05  FILLER REDEFINES ACAREOFF.
000049         10  ACAREOFA PIC  X(0001).
000050     05  ACAREOFI PIC  X(0030).
000051*    -------------------------------
000052     05  FD2L PIC S9(0004) COMP.
000053     05  FD2F PIC  X(0001).
000054     05  FILLER REDEFINES FD2F.
000055         10  FD2A PIC  X(0001).
000056     05  FD2I PIC  X(0008).
000057*    -------------------------------
000058     05  ACLAIML PIC S9(0004) COMP.
000059     05  ACLAIMF PIC  X(0001).
000060     05  FILLER REDEFINES ACLAIMF.
000061         10  ACLAIMA PIC  X(0001).
000062     05  ACLAIMI PIC  X(0008).
000063*    -------------------------------
000064     05  ALPHCHL PIC S9(0004) COMP.
000065     05  ALPHCHF PIC  X(0001).
000066     05  FILLER REDEFINES ALPHCHF.
000067         10  ALPHCHA PIC  X(0001).
000068     05  ALPHCHI PIC  X(0001).
000069*    -------------------------------
000070     05  AADDR1L PIC S9(0004) COMP.
000071     05  AADDR1F PIC  X(0001).
000072     05  FILLER REDEFINES AADDR1F.
000073         10  AADDR1A PIC  X(0001).
000074     05  AADDR1I PIC  X(0030).
000075*    -------------------------------
000076     05  FD3L PIC S9(0004) COMP.
000077     05  FD3F PIC  X(0001).
000078     05  FILLER REDEFINES FD3F.
000079         10  FD3A PIC  X(0001).
000080     05  FD3I PIC  X(0008).
000081*    -------------------------------
000082     05  ACHECKL PIC S9(0004) COMP.
000083     05  ACHECKF PIC  X(0001).
000084     05  FILLER REDEFINES ACHECKF.
000085         10  ACHECKA PIC  X(0001).
000086     05  ACHECKI PIC  X(0008).
000087*    -------------------------------
000088     05  AADDR2L PIC S9(0004) COMP.
000089     05  AADDR2F PIC  X(0001).
000090     05  FILLER REDEFINES AADDR2F.
000091         10  AADDR2A PIC  X(0001).
000092     05  AADDR2I PIC  X(0030).
000093*    -------------------------------
000094     05  APHONEL PIC S9(0004) COMP.
000095     05  APHONEF PIC  X(0001).
000096     05  FILLER REDEFINES APHONEF.
000097         10  APHONEA PIC  X(0001).
000098     05  APHONEI PIC  S9(12).
000099*    -------------------------------
000100     05  ACITYSTL PIC S9(0004) COMP.
000101     05  ACITYSTF PIC  X(0001).
000102     05  FILLER REDEFINES ACITYSTF.
000103         10  ACITYSTA PIC  X(0001).
000104     05  ACITYSTI PIC  X(0030).
000105*    -------------------------------
000106     05  AZIPL PIC S9(0004) COMP.
000107     05  AZIPF PIC  X(0001).
000108     05  FILLER REDEFINES AZIPF.
000109         10  AZIPA PIC  X(0001).
000110     05  AZIPI PIC  X(0010).
000111*    -------------------------------
000112     05  ADOMSTL PIC S9(0004) COMP.
000113     05  ADOMSTF PIC  X(0001).
000114     05  FILLER REDEFINES ADOMSTF.
000115         10  ADOMSTA PIC  X(0001).
000116     05  ADOMSTI PIC  X(0002).
000117*    -------------------------------
000118     05  ANXTAUDL PIC S9(0004) COMP.
000119     05  ANXTAUDF PIC  X(0001).
000120     05  FILLER REDEFINES ANXTAUDF.
000121         10  ANXTAUDA PIC  X(0001).
000122     05  ANXTAUDI PIC  X(0008).
000123*    -------------------------------
000124     05  FD4L PIC S9(0004) COMP.
000125     05  FD4F PIC  X(0001).
000126     05  FILLER REDEFINES FD4F.
000127         10  FD4A PIC  X(0001).
000128     05  FD4I PIC  X(0032).
000129*    -------------------------------
000130     05  ACLNAML PIC S9(0004) COMP.
000131     05  ACLNAMF PIC  X(0001).
000132     05  FILLER REDEFINES ACLNAMF.
000133         10  ACLNAMA PIC  X(0001).
000134     05  ACLNAMI PIC  X(0001).
000135*    -------------------------------
000136     05  FD5L PIC S9(0004) COMP.
000137     05  FD5F PIC  X(0001).
000138     05  FILLER REDEFINES FD5F.
000139         10  FD5A PIC  X(0001).
000140     05  FD5I PIC  X(0018).
000141*    -------------------------------
000142     05  ALAL PIC S9(0004) COMP.
000143     05  ALAF PIC  X(0001).
000144     05  FILLER REDEFINES ALAF.
000145         10  ALAA PIC  X(0001).
000146     05  ALAI PIC  X(0001).
000147*    -------------------------------
000148     05  FD6L PIC S9(0004) COMP.
000149     05  FD6F PIC  X(0001).
000150     05  FILLER REDEFINES FD6F.
000151         10  FD6A PIC  X(0001).
000152     05  FD6I PIC  X(0032).
000153*    -------------------------------
000154     05  ACKNAML PIC S9(0004) COMP.
000155     05  ACKNAMF PIC  X(0001).
000156     05  FILLER REDEFINES ACKNAMF.
000157         10  ACKNAMA PIC  X(0001).
000158     05  ACKNAMI PIC  X(0001).
000159*    -------------------------------
000160     05  FD7L PIC S9(0004) COMP.
000161     05  FD7F PIC  X(0001).
000162     05  FILLER REDEFINES FD7F.
000163         10  FD7A PIC  X(0001).
000164     05  FD7I PIC  X(0012).
000165*    -------------------------------
000166     05  ACDTAL PIC S9(0004) COMP.
000167     05  ACDTAF PIC  X(0001).
000168     05  FILLER REDEFINES ACDTAF.
000169         10  ACDTAA PIC  X(0001).
000170     05  ACDTAI PIC  X(0001).
000171*    -------------------------------
000172     05  FD8L PIC S9(0004) COMP.
000173     05  FD8F PIC  X(0001).
000174     05  FILLER REDEFINES FD8F.
000175         10  FD8A PIC  X(0001).
000176     05  FD8I PIC  X(0026).
000177*    -------------------------------
000178     05  ACLCML PIC S9(0004) COMP.
000179     05  ACLCMF PIC  X(0001).
000180     05  FILLER REDEFINES ACLCMF.
000181         10  ACLCMA PIC  X(0001).
000182     05  ACLCMI PIC  X(0001).
000183*    -------------------------------
000184     05  FD9L PIC S9(0004) COMP.
000185     05  FD9F PIC  X(0001).
000186     05  FILLER REDEFINES FD9F.
000187         10  FD9A PIC  X(0001).
000188     05  FD9I PIC  X(0010).
000189*    -------------------------------
000190     05  APCTCDTL PIC S9(0004) COMP.
000191     05  APCTCDTF PIC  X(0001).
000192     05  FILLER REDEFINES APCTCDTF.
000193         10  APCTCDTA PIC  X(0001).
000194     05  APCTCDTI PIC  S9(5)V99.
000195*    -------------------------------
000196     05  FD9AL PIC S9(0004) COMP.
000197     05  FD9AF PIC  X(0001).
000198     05  FILLER REDEFINES FD9AF.
000199         10  FD9AA PIC  X(0001).
000200     05  FD9AI PIC  X(0008).
000201*    -------------------------------
000202     05  IBNRPCTL PIC S9(0004) COMP.
000203     05  IBNRPCTF PIC  X(0001).
000204     05  FILLER REDEFINES IBNRPCTF.
000205         10  IBNRPCTA PIC  X(0001).
000206     05  IBNRPCTI PIC  S999V9(4).
000207*    -------------------------------
000208     05  FD10L PIC S9(0004) COMP.
000209     05  FD10F PIC  X(0001).
000210     05  FILLER REDEFINES FD10F.
000211         10  FD10A PIC  X(0001).
000212     05  FD10I PIC  X(0028).
000213*    -------------------------------
000214     05  AEXPCML PIC S9(0004) COMP.
000215     05  AEXPCMF PIC  X(0001).
000216     05  FILLER REDEFINES AEXPCMF.
000217         10  AEXPCMA PIC  X(0001).
000218     05  AEXPCMI PIC  X(0001).
000219*    -------------------------------
000220     05  FD11L PIC S9(0004) COMP.
000221     05  FD11F PIC  X(0001).
000222     05  FILLER REDEFINES FD11F.
000223         10  FD11A PIC  X(0001).
000224     05  FD11I PIC  X(0009).
000225*    -------------------------------
000226     05  AEXPCPL PIC S9(0004) COMP.
000227     05  AEXPCPF PIC  X(0001).
000228     05  FILLER REDEFINES AEXPCPF.
000229         10  AEXPCPA PIC  X(0001).
000230     05  AEXPCPI PIC  S9(5)V99.
000231*    -------------------------------
000232     05  FD12L PIC S9(0004) COMP.
000233     05  FD12F PIC  X(0001).
000234     05  FILLER REDEFINES FD12F.
000235         10  FD12A PIC  X(0001).
000236     05  FD12I PIC  X(0008).
000237*    -------------------------------
000238     05  AEXPCAL PIC S9(0004) COMP.
000239     05  AEXPCAF PIC  X(0001).
000240     05  FILLER REDEFINES AEXPCAF.
000241         10  AEXPCAA PIC  X(0001).
000242     05  AEXPCAI PIC  S9(5)V99.
000243*    -------------------------------
000244     05  ABRETRL PIC S9(0004) COMP.
000245     05  ABRETRF PIC  X(0001).
000246     05  FILLER REDEFINES ABRETRF.
000247         10  ABRETRA PIC  X(0001).
000248     05  ABRETRI PIC  99.
000249*    -------------------------------
000250     05  FD13L PIC S9(0004) COMP.
000251     05  FD13F PIC  X(0001).
000252     05  FILLER REDEFINES FD13F.
000253         10  FD13A PIC  X(0001).
000254     05  FD13I PIC  X(0016).
000255*    -------------------------------
000256     05  ARESMANL PIC S9(0004) COMP.
000257     05  ARESMANF PIC  X(0001).
000258     05  FILLER REDEFINES ARESMANF.
000259         10  ARESMANA PIC  X(0001).
000260     05  ARESMANI PIC  X(0001).
000261*    -------------------------------
000262     05  FD14L PIC S9(0004) COMP.
000263     05  FD14F PIC  X(0001).
000264     05  FILLER REDEFINES FD14F.
000265         10  FD14A PIC  X(0001).
000266     05  FD14I PIC  X(0011).
000267*    -------------------------------
000268     05  ARESCDTL PIC S9(0004) COMP.
000269     05  ARESCDTF PIC  X(0001).
000270     05  FILLER REDEFINES ARESCDTF.
000271         10  ARESCDTA PIC  X(0001).
000272     05  ARESCDTI PIC  X(0001).
000273*    -------------------------------
000274     05  FD15L PIC S9(0004) COMP.
000275     05  FD15F PIC  X(0001).
000276     05  FILLER REDEFINES FD15F.
000277         10  FD15A PIC  X(0001).
000278     05  FD15I PIC  X(0010).
000279*    -------------------------------
000280     05  ARESIBNL PIC S9(0004) COMP.
000281     05  ARESIBNF PIC  X(0001).
000282     05  FILLER REDEFINES ARESIBNF.
000283         10  ARESIBNA PIC  X(0001).
000284     05  ARESIBNI PIC  X(0001).
000285*    -------------------------------
000286     05  FD16L PIC S9(0004) COMP.
000287     05  FD16F PIC  X(0001).
000288     05  FILLER REDEFINES FD16F.
000289         10  FD16A PIC  X(0001).
000290     05  FD16I PIC  X(0004).
000291*    -------------------------------
000292     05  ARESPTCL PIC S9(0004) COMP.
000293     05  ARESPTCF PIC  X(0001).
000294     05  FILLER REDEFINES ARESPTCF.
000295         10  ARESPTCA PIC  X(0001).
000296     05  ARESPTCI PIC  X(0001).
000297*    -------------------------------
000298     05  FD17L PIC S9(0004) COMP.
000299     05  FD17F PIC  X(0001).
000300     05  FILLER REDEFINES FD17F.
000301         10  FD17A PIC  X(0001).
000302     05  FD17I PIC  X(0014).
000303*    -------------------------------
000304     05  FD17AL PIC S9(0004) COMP.
000305     05  FD17AF PIC  X(0001).
000306     05  FILLER REDEFINES FD17AF.
000307         10  FD17AA PIC  X(0001).
000308     05  FD17AI PIC  X(0028).
000309*    -------------------------------
000310     05  AUEPPCTL PIC S9(0004) COMP.
000311     05  AUEPPCTF PIC  X(0001).
000312     05  FILLER REDEFINES AUEPPCTF.
000313         10  AUEPPCTA PIC  X(0001).
000314     05  AUEPPCTI PIC  S999V9(4).
000315*    -------------------------------
000316     05  FD17BL PIC S9(0004) COMP.
000317     05  FD17BF PIC  X(0001).
000318     05  FILLER REDEFINES FD17BF.
000319         10  FD17BA PIC  X(0001).
000320     05  FD17BI PIC  X(0006).
000321*    -------------------------------
000322     05  AR78PCTL PIC S9(0004) COMP.
000323     05  AR78PCTF PIC  X(0001).
000324     05  FILLER REDEFINES AR78PCTF.
000325         10  AR78PCTA PIC  X(0001).
000326     05  AR78PCTI PIC  S999V9(4).
000327*    -------------------------------
000328     05  FD17CL PIC S9(0004) COMP.
000329     05  FD17CF PIC  X(0001).
000330     05  FILLER REDEFINES FD17CF.
000331         10  FD17CA PIC  X(0001).
000332     05  FD17CI PIC  X(0006).
000333*    -------------------------------
000334     05  APROPCTL PIC S9(0004) COMP.
000335     05  APROPCTF PIC  X(0001).
000336     05  FILLER REDEFINES APROPCTF.
000337         10  APROPCTA PIC  X(0001).
000338     05  APROPCTI PIC  S999V9(4).
000339*    -------------------------------
000340     05  FD18L PIC S9(0004) COMP.
000341     05  FD18F PIC  X(0001).
000342     05  FILLER REDEFINES FD18F.
000343         10  FD18A PIC  X(0001).
000344     05  FD18I PIC  X(0027).
000345*    -------------------------------
000346     05  ALQCAL PIC S9(0004) COMP.
000347     05  ALQCAF PIC  X(0001).
000348     05  FILLER REDEFINES ALQCAF.
000349         10  ALQCAA PIC  X(0001).
000350     05  ALQCAI PIC  S9(5)V99.
000351*    -------------------------------
000352     05  FD19L PIC S9(0004) COMP.
000353     05  FD19F PIC  X(0001).
000354     05  FILLER REDEFINES FD19F.
000355         10  FD19A PIC  X(0001).
000356     05  FD19I PIC  X(0026).
000357*    -------------------------------
000358     05  ALMRPL PIC S9(0004) COMP.
000359     05  ALMRPF PIC  X(0001).
000360     05  FILLER REDEFINES ALMRPF.
000361         10  ALMRPA PIC  X(0001).
000362     05  ALMRPI PIC  S9(11)V99.
000363*    -------------------------------
000364     05  FD20L PIC S9(0004) COMP.
000365     05  FD20F PIC  X(0001).
000366     05  FILLER REDEFINES FD20F.
000367         10  FD20A PIC  X(0001).
000368     05  FD20I PIC  X(0018).
000369*    -------------------------------
000370     05  ALQCDL PIC S9(0004) COMP.
000371     05  ALQCDF PIC  X(0001).
000372     05  FILLER REDEFINES ALQCDF.
000373         10  ALQCDA PIC  X(0001).
000374     05  ALQCDI PIC  S9(4).
000375*    -------------------------------
000376     05  FD21L PIC S9(0004) COMP.
000377     05  FD21F PIC  X(0001).
000378     05  FILLER REDEFINES FD21F.
000379         10  FD21A PIC  X(0001).
000380     05  FD21I PIC  X(0026).
000381*    -------------------------------
000382     05  ALMDPPL PIC S9(0004) COMP.
000383     05  ALMDPPF PIC  X(0001).
000384     05  FILLER REDEFINES ALMDPPF.
000385         10  ALMDPPA PIC  X(0001).
000386     05  ALMDPPI PIC  S9(4).
000387*    -------------------------------
000388     05  FD22L PIC S9(0004) COMP.
000389     05  FD22F PIC  X(0001).
000390     05  FILLER REDEFINES FD22F.
000391         10  FD22A PIC  X(0001).
000392     05  FD22I PIC  X(0020).
000393*    -------------------------------
000394     05  ALDBCL PIC S9(0004) COMP.
000395     05  ALDBCF PIC  X(0001).
000396     05  FILLER REDEFINES ALDBCF.
000397         10  ALDBCA PIC  X(0001).
000398     05  ALDBCI PIC  S9(4).
000399*    -------------------------------
000400     05  FD23L PIC S9(0004) COMP.
000401     05  FD23F PIC  X(0001).
000402     05  FILLER REDEFINES FD23F.
000403         10  FD23A PIC  X(0001).
000404     05  FD23I PIC  X(0027).
000405*    -------------------------------
000406     05  ALMAPL PIC S9(0004) COMP.
000407     05  ALMAPF PIC  X(0001).
000408     05  FILLER REDEFINES ALMAPF.
000409         10  ALMAPA PIC  X(0001).
000410     05  ALMAPI PIC  S9(11)V99.
000411*    -------------------------------
000412     05  FD24L PIC S9(0004) COMP.
000413     05  FD24F PIC  X(0001).
000414     05  FILLER REDEFINES FD24F.
000415         10  FD24A PIC  X(0001).
000416     05  FD24I PIC  X(0022).
000417*    -------------------------------
000418     05  ALMBPL PIC S9(0004) COMP.
000419     05  ALMBPF PIC  X(0001).
000420     05  FILLER REDEFINES ALMBPF.
000421         10  ALMBPA PIC  X(0001).
000422     05  ALMBPI PIC  S9(4).
000423*    -------------------------------
000424     05  FD25L PIC S9(0004) COMP.
000425     05  FD25F PIC  X(0001).
000426     05  FILLER REDEFINES FD25F.
000427         10  FD25A PIC  X(0001).
000428     05  FD25I PIC  X(0025).
000429*    -------------------------------
000430     05  ALMAPML PIC S9(0004) COMP.
000431     05  ALMAPMF PIC  X(0001).
000432     05  FILLER REDEFINES ALMAPMF.
000433         10  ALMAPMA PIC  X(0001).
000434     05  ALMAPMI PIC  S9(4).
000435*    -------------------------------
000436     05  AEMSG1L PIC S9(0004) COMP.
000437     05  AEMSG1F PIC  X(0001).
000438     05  FILLER REDEFINES AEMSG1F.
000439         10  AEMSG1A PIC  X(0001).
000440     05  AEMSG1I PIC  X(0079).
000441*    -------------------------------
000442     05  APFKL PIC S9(0004) COMP.
000443     05  APFKF PIC  X(0001).
000444     05  FILLER REDEFINES APFKF.
000445         10  APFKA PIC  X(0001).
000446     05  APFKI PIC  99.
000447*    -------------------------------
000448     05  ALUDATEL PIC S9(0004) COMP.
000449     05  ALUDATEF PIC  X(0001).
000450     05  FILLER REDEFINES ALUDATEF.
000451         10  ALUDATEA PIC  X(0001).
000452     05  ALUDATEI PIC  X(0008).
000453*    -------------------------------
000454     05  ALUTIMEL PIC S9(0004) COMP.
000455     05  ALUTIMEF PIC  X(0001).
000456     05  FILLER REDEFINES ALUTIMEF.
000457         10  ALUTIMEA PIC  X(0001).
000458     05  ALUTIMEI PIC  X(0008).
000459*    -------------------------------
000460     05  ALUBYL PIC S9(0004) COMP.
000461     05  ALUBYF PIC  X(0001).
000462     05  FILLER REDEFINES ALUBYF.
000463         10  ALUBYA PIC  X(0001).
000464     05  ALUBYI PIC  X(0004).
000465 01  EL105AO REDEFINES EL105AI.
000466     05  FILLER            PIC  X(0012).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  ADATEO PIC  X(0008).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  ATIMEO PIC  99.99.
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  AMAINTO PIC  X(0001).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  ACARIERO PIC  X(0001).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  ACONAMEO PIC  X(0030).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  FD1O PIC  X(0019).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  ALPHLO PIC  X(0006).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  ACAREOFO PIC  X(0030).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  FD2O PIC  X(0008).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  ACLAIMO PIC  9(8).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  ALPHCHO PIC  X(0001).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  AADDR1O PIC  X(0030).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  FD3O PIC  X(0008).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  ACHECKO PIC  9(8).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  AADDR2O PIC  X(0030).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  APHONEO PIC  999B999B9999.
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  ACITYSTO PIC  X(0030).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  AZIPO PIC  X(0010).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  ADOMSTO PIC  X(0002).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  ANXTAUDO PIC  9(8).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  FD4O PIC  X(0032).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  ACLNAMO PIC  X(0001).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  FD5O PIC  X(0018).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  ALAO PIC  X(0001).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  FD6O PIC  X(0032).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  ACKNAMO PIC  X(0001).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  FD7O PIC  X(0012).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  ACDTAO PIC  X(0001).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  FD8O PIC  X(0026).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  ACLCMO PIC  X(0001).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  FD9O PIC  X(0010).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  APCTCDTO PIC  ZZ9.99-.
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  FD9AO PIC  X(0008).
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  IBNRPCTO PIC  Z.9(4)-.
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  FD10O PIC  X(0028).
000572*    -------------------------------
000573     05  FILLER            PIC  X(0003).
000574     05  AEXPCMO PIC  X(0001).
000575*    -------------------------------
000576     05  FILLER            PIC  X(0003).
000577     05  FD11O PIC  X(0009).
000578*    -------------------------------
000579     05  FILLER            PIC  X(0003).
000580     05  AEXPCPO PIC  ZZ9.99-.
000581*    -------------------------------
000582     05  FILLER            PIC  X(0003).
000583     05  FD12O PIC  X(0008).
000584*    -------------------------------
000585     05  FILLER            PIC  X(0003).
000586     05  AEXPCAO PIC  ZZ9.99-.
000587*    -------------------------------
000588     05  FILLER            PIC  X(0003).
000589     05  ABRETRO PIC  Z9.
000590*    -------------------------------
000591     05  FILLER            PIC  X(0003).
000592     05  FD13O PIC  X(0016).
000593*    -------------------------------
000594     05  FILLER            PIC  X(0003).
000595     05  ARESMANO PIC  X(0001).
000596*    -------------------------------
000597     05  FILLER            PIC  X(0003).
000598     05  FD14O PIC  X(0011).
000599*    -------------------------------
000600     05  FILLER            PIC  X(0003).
000601     05  ARESCDTO PIC  X(0001).
000602*    -------------------------------
000603     05  FILLER            PIC  X(0003).
000604     05  FD15O PIC  X(0010).
000605*    -------------------------------
000606     05  FILLER            PIC  X(0003).
000607     05  ARESIBNO PIC  X(0001).
000608*    -------------------------------
000609     05  FILLER            PIC  X(0003).
000610     05  FD16O PIC  X(0004).
000611*    -------------------------------
000612     05  FILLER            PIC  X(0003).
000613     05  ARESPTCO PIC  X(0001).
000614*    -------------------------------
000615     05  FILLER            PIC  X(0003).
000616     05  FD17O PIC  X(0014).
000617*    -------------------------------
000618     05  FILLER            PIC  X(0003).
000619     05  FD17AO PIC  X(0028).
000620*    -------------------------------
000621     05  FILLER            PIC  X(0003).
000622     05  AUEPPCTO PIC  Z.9(4)-.
000623*    -------------------------------
000624     05  FILLER            PIC  X(0003).
000625     05  FD17BO PIC  X(0006).
000626*    -------------------------------
000627     05  FILLER            PIC  X(0003).
000628     05  AR78PCTO PIC  Z.9(4)-.
000629*    -------------------------------
000630     05  FILLER            PIC  X(0003).
000631     05  FD17CO PIC  X(0006).
000632*    -------------------------------
000633     05  FILLER            PIC  X(0003).
000634     05  APROPCTO PIC  Z.9(4)-.
000635*    -------------------------------
000636     05  FILLER            PIC  X(0003).
000637     05  FD18O PIC  X(0027).
000638*    -------------------------------
000639     05  FILLER            PIC  X(0003).
000640     05  ALQCAO PIC  ZZ9.99-.
000641*    -------------------------------
000642     05  FILLER            PIC  X(0003).
000643     05  FD19O PIC  X(0026).
000644*    -------------------------------
000645     05  FILLER            PIC  X(0003).
000646     05  ALMRPO PIC  Z,ZZZ,ZZ9.99-.
000647*    -------------------------------
000648     05  FILLER            PIC  X(0003).
000649     05  FD20O PIC  X(0018).
000650*    -------------------------------
000651     05  FILLER            PIC  X(0003).
000652     05  ALQCDO PIC  ZZ9-.
000653*    -------------------------------
000654     05  FILLER            PIC  X(0003).
000655     05  FD21O PIC  X(0026).
000656*    -------------------------------
000657     05  FILLER            PIC  X(0003).
000658     05  ALMDPPO PIC  ZZ9-.
000659*    -------------------------------
000660     05  FILLER            PIC  X(0003).
000661     05  FD22O PIC  X(0020).
000662*    -------------------------------
000663     05  FILLER            PIC  X(0003).
000664     05  ALDBCO PIC  ZZ9-.
000665*    -------------------------------
000666     05  FILLER            PIC  X(0003).
000667     05  FD23O PIC  X(0027).
000668*    -------------------------------
000669     05  FILLER            PIC  X(0003).
000670     05  ALMAPO PIC  Z,ZZZ,ZZ9.99-.
000671*    -------------------------------
000672     05  FILLER            PIC  X(0003).
000673     05  FD24O PIC  X(0022).
000674*    -------------------------------
000675     05  FILLER            PIC  X(0003).
000676     05  ALMBPO PIC  ZZ9-.
000677*    -------------------------------
000678     05  FILLER            PIC  X(0003).
000679     05  FD25O PIC  X(0025).
000680*    -------------------------------
000681     05  FILLER            PIC  X(0003).
000682     05  ALMAPMO PIC  ZZ9-.
000683*    -------------------------------
000684     05  FILLER            PIC  X(0003).
000685     05  AEMSG1O PIC  X(0079).
000686*    -------------------------------
000687     05  FILLER            PIC  X(0003).
000688     05  APFKO PIC  X(0002).
000689*    -------------------------------
000690     05  FILLER            PIC  X(0003).
000691     05  ALUDATEO PIC  X(0008).
000692*    -------------------------------
000693     05  FILLER            PIC  X(0003).
000694     05  ALUTIMEO PIC  99B99B99.
000695*    -------------------------------
000696     05  FILLER            PIC  X(0003).
000697     05  ALUBYO PIC  X(0004).
000698*    -------------------------------
000699 01  EL105BI REDEFINES EL105AI.
000700     05  FILLER            PIC  X(0012).
000701*    -------------------------------
000702     05  BDATEL PIC S9(0004) COMP.
000703     05  BDATEF PIC  X(0001).
000704     05  FILLER REDEFINES BDATEF.
000705         10  BDATEA PIC  X(0001).
000706     05  BDATEI PIC  X(0008).
000707*    -------------------------------
000708     05  BTIMEL PIC S9(0004) COMP.
000709     05  BTIMEF PIC  X(0001).
000710     05  FILLER REDEFINES BTIMEF.
000711         10  BTIMEA PIC  X(0001).
000712     05  BTIMEI PIC  X(0005).
000713*    -------------------------------
000714     05  BMAINTL PIC S9(0004) COMP.
000715     05  BMAINTF PIC  X(0001).
000716     05  FILLER REDEFINES BMAINTF.
000717         10  BMAINTA PIC  X(0001).
000718     05  BMAINTI PIC  X(0001).
000719*    -------------------------------
000720     05  BCARIERL PIC S9(0004) COMP.
000721     05  BCARIERF PIC  X(0001).
000722     05  FILLER REDEFINES BCARIERF.
000723         10  BCARIERA PIC  X(0001).
000724     05  BCARIERI PIC  X(0001).
000725*    -------------------------------
000726     05  BSPLABLL PIC S9(0004) COMP.
000727     05  BSPLABLF PIC  X(0001).
000728     05  FILLER REDEFINES BSPLABLF.
000729         10  BSPLABLA PIC  X(0001).
000730     05  BSPLABLI PIC  X(0011).
000731*    -------------------------------
000732     05  BSECPAYL PIC S9(0004) COMP.
000733     05  BSECPAYF PIC  X(0001).
000734     05  FILLER REDEFINES BSECPAYF.
000735         10  BSECPAYA PIC  X(0001).
000736     05  BSECPAYI PIC  X(0001).
000737*    -------------------------------
000738     05  BCONAMEL PIC S9(0004) COMP.
000739     05  BCONAMEF PIC  X(0001).
000740     05  FILLER REDEFINES BCONAMEF.
000741         10  BCONAMEA PIC  X(0001).
000742     05  BCONAMEI PIC  X(0030).
000743*    -------------------------------
000744     05  BCAREOFL PIC S9(0004) COMP.
000745     05  BCAREOFF PIC  X(0001).
000746     05  FILLER REDEFINES BCAREOFF.
000747         10  BCAREOFA PIC  X(0001).
000748     05  BCAREOFI PIC  X(0030).
000749*    -------------------------------
000750     05  BADDR1L PIC S9(0004) COMP.
000751     05  BADDR1F PIC  X(0001).
000752     05  FILLER REDEFINES BADDR1F.
000753         10  BADDR1A PIC  X(0001).
000754     05  BADDR1I PIC  X(0030).
000755*    -------------------------------
000756     05  BADDR2L PIC S9(0004) COMP.
000757     05  BADDR2F PIC  X(0001).
000758     05  FILLER REDEFINES BADDR2F.
000759         10  BADDR2A PIC  X(0001).
000760     05  BADDR2I PIC  X(0030).
000761*    -------------------------------
000762     05  BPHONEL PIC S9(0004) COMP.
000763     05  BPHONEF PIC  X(0001).
000764     05  FILLER REDEFINES BPHONEF.
000765         10  BPHONEA PIC  X(0001).
000766     05  BPHONEI PIC  S9(12).
000767*    -------------------------------
000768     05  BCITYSTL PIC S9(0004) COMP.
000769     05  BCITYSTF PIC  X(0001).
000770     05  FILLER REDEFINES BCITYSTF.
000771         10  BCITYSTA PIC  X(0001).
000772     05  BCITYSTI PIC  X(0030).
000773*    -------------------------------
000774     05  BZIPL PIC S9(0004) COMP.
000775     05  BZIPF PIC  X(0001).
000776     05  FILLER REDEFINES BZIPF.
000777         10  BZIPA PIC  X(0001).
000778     05  BZIPI PIC  X(0010).
000779*    -------------------------------
000780     05  BDOMSTL PIC S9(0004) COMP.
000781     05  BDOMSTF PIC  X(0001).
000782     05  FILLER REDEFINES BDOMSTF.
000783         10  BDOMSTA PIC  X(0001).
000784     05  BDOMSTI PIC  X(0002).
000785*    -------------------------------
000786     05  BCTLABLL PIC S9(0004) COMP.
000787     05  BCTLABLF PIC  X(0001).
000788     05  FILLER REDEFINES BCTLABLF.
000789         10  BCTLABLA PIC  X(0001).
000790     05  BCTLABLI PIC  X(0009).
000791*    -------------------------------
000792     05  BCLPTOLL PIC S9(0004) COMP.
000793     05  BCLPTOLF PIC  X(0001).
000794     05  FILLER REDEFINES BCLPTOLF.
000795         10  BCLPTOLA PIC  X(0001).
000796     05  BCLPTOLI PIC  S9(3)V9(4).
000797*    -------------------------------
000798     05  BLCLABLL PIC S9(0004) COMP.
000799     05  BLCLABLF PIC  X(0001).
000800     05  FILLER REDEFINES BLCLABLF.
000801         10  BLCLABLA PIC  X(0001).
000802     05  BLCLABLI PIC  X(0011).
000803*    -------------------------------
000804     05  BLCOMML PIC S9(0004) COMP.
000805     05  BLCOMMF PIC  X(0001).
000806     05  FILLER REDEFINES BLCOMMF.
000807         10  BLCOMMA PIC  X(0001).
000808     05  BLCOMMI PIC  S9(6)V9(2).
000809*    -------------------------------
000810     05  BPRMTOLL PIC S9(0004) COMP.
000811     05  BPRMTOLF PIC  X(0001).
000812     05  FILLER REDEFINES BPRMTOLF.
000813         10  BPRMTOLA PIC  X(0001).
000814     05  BPRMTOLI PIC  S9(4)V99.
000815*    -------------------------------
000816     05  BREFTOLL PIC S9(0004) COMP.
000817     05  BREFTOLF PIC  X(0001).
000818     05  FILLER REDEFINES BREFTOLF.
000819         10  BREFTOLA PIC  X(0001).
000820     05  BREFTOLI PIC  S9(4)V99.
000821*    -------------------------------
000822     05  BOVSAMTL PIC S9(0004) COMP.
000823     05  BOVSAMTF PIC  X(0001).
000824     05  FILLER REDEFINES BOVSAMTF.
000825         10  BOVSAMTA PIC  X(0001).
000826     05  BOVSAMTI PIC  S9(4)V99.
000827*    -------------------------------
000828     05  BPRMPCTL PIC S9(0004) COMP.
000829     05  BPRMPCTF PIC  X(0001).
000830     05  FILLER REDEFINES BPRMPCTF.
000831         10  BPRMPCTA PIC  X(0001).
000832     05  BPRMPCTI PIC  S9(1)V9(4).
000833*    -------------------------------
000834     05  BREFPCTL PIC S9(0004) COMP.
000835     05  BREFPCTF PIC  X(0001).
000836     05  FILLER REDEFINES BREFPCTF.
000837         10  BREFPCTA PIC  X(0001).
000838     05  BREFPCTI PIC  S9(1)V9(4).
000839*    -------------------------------
000840     05  BOVSPCTL PIC S9(0004) COMP.
000841     05  BOVSPCTF PIC  X(0001).
000842     05  FILLER REDEFINES BOVSPCTF.
000843         10  BOVSPCTA PIC  X(0001).
000844     05  BOVSPCTI PIC  S9(1)V9(4).
000845*    -------------------------------
000846     05  DMDSW2L PIC S9(0004) COMP.
000847     05  DMDSW2F PIC  X(0001).
000848     05  FILLER REDEFINES DMDSW2F.
000849         10  DMDSW2A PIC  X(0001).
000850     05  DMDSW2I PIC  X(0020).
000851*    -------------------------------
000852     05  BCLCPRML PIC S9(0004) COMP.
000853     05  BCLCPRMF PIC  X(0001).
000854     05  FILLER REDEFINES BCLCPRMF.
000855         10  BCLCPRMA PIC  X(0001).
000856     05  BCLCPRMI PIC  X(0001).
000857*    -------------------------------
000858     05  BEMSG1L PIC S9(0004) COMP.
000859     05  BEMSG1F PIC  X(0001).
000860     05  FILLER REDEFINES BEMSG1F.
000861         10  BEMSG1A PIC  X(0001).
000862     05  BEMSG1I PIC  X(0079).
000863*    -------------------------------
000864     05  BPFKL PIC S9(0004) COMP.
000865     05  BPFKF PIC  X(0001).
000866     05  FILLER REDEFINES BPFKF.
000867         10  BPFKA PIC  X(0001).
000868     05  BPFKI PIC  99.
000869*    -------------------------------
000870     05  BLUDATEL PIC S9(0004) COMP.
000871     05  BLUDATEF PIC  X(0001).
000872     05  FILLER REDEFINES BLUDATEF.
000873         10  BLUDATEA PIC  X(0001).
000874     05  BLUDATEI PIC  X(0008).
000875*    -------------------------------
000876     05  BLUTIMEL PIC S9(0004) COMP.
000877     05  BLUTIMEF PIC  X(0001).
000878     05  FILLER REDEFINES BLUTIMEF.
000879         10  BLUTIMEA PIC  X(0001).
000880     05  BLUTIMEI PIC  X(0008).
000881*    -------------------------------
000882     05  BLUBYL PIC S9(0004) COMP.
000883     05  BLUBYF PIC  X(0001).
000884     05  FILLER REDEFINES BLUBYF.
000885         10  BLUBYA PIC  X(0001).
000886     05  BLUBYI PIC  X(0004).
000887 01  EL105BO REDEFINES EL105AI.
000888     05  FILLER            PIC  X(0012).
000889*    -------------------------------
000890     05  FILLER            PIC  X(0003).
000891     05  BDATEO PIC  X(0008).
000892*    -------------------------------
000893     05  FILLER            PIC  X(0003).
000894     05  BTIMEO PIC  99.99.
000895*    -------------------------------
000896     05  FILLER            PIC  X(0003).
000897     05  BMAINTO PIC  X(0001).
000898*    -------------------------------
000899     05  FILLER            PIC  X(0003).
000900     05  BCARIERO PIC  X(0001).
000901*    -------------------------------
000902     05  FILLER            PIC  X(0003).
000903     05  BSPLABLO PIC  X(0011).
000904*    -------------------------------
000905     05  FILLER            PIC  X(0003).
000906     05  BSECPAYO PIC  X(0001).
000907*    -------------------------------
000908     05  FILLER            PIC  X(0003).
000909     05  BCONAMEO PIC  X(0030).
000910*    -------------------------------
000911     05  FILLER            PIC  X(0003).
000912     05  BCAREOFO PIC  X(0030).
000913*    -------------------------------
000914     05  FILLER            PIC  X(0003).
000915     05  BADDR1O PIC  X(0030).
000916*    -------------------------------
000917     05  FILLER            PIC  X(0003).
000918     05  BADDR2O PIC  X(0030).
000919*    -------------------------------
000920     05  FILLER            PIC  X(0003).
000921     05  BPHONEO PIC  999B999B9999.
000922*    -------------------------------
000923     05  FILLER            PIC  X(0003).
000924     05  BCITYSTO PIC  X(0030).
000925*    -------------------------------
000926     05  FILLER            PIC  X(0003).
000927     05  BZIPO PIC  X(0010).
000928*    -------------------------------
000929     05  FILLER            PIC  X(0003).
000930     05  BDOMSTO PIC  X(0002).
000931*    -------------------------------
000932     05  FILLER            PIC  X(0003).
000933     05  BCTLABLO PIC  X(0009).
000934*    -------------------------------
000935     05  FILLER            PIC  X(0003).
000936     05  BCLPTOLO PIC  Z.9999-.
000937*    -------------------------------
000938     05  FILLER            PIC  X(0003).
000939     05  BLCLABLO PIC  X(0011).
000940*    -------------------------------
000941     05  FILLER            PIC  X(0003).
000942     05  BLCOMMO PIC  ZZ99.99-.
000943*    -------------------------------
000944     05  FILLER            PIC  X(0003).
000945     05  BPRMTOLO PIC  ZZ9.99.
000946*    -------------------------------
000947     05  FILLER            PIC  X(0003).
000948     05  BREFTOLO PIC  ZZ9.99.
000949*    -------------------------------
000950     05  FILLER            PIC  X(0003).
000951     05  BOVSAMTO PIC  ZZ9.99.
000952*    -------------------------------
000953     05  FILLER            PIC  X(0003).
000954     05  BPRMPCTO PIC  .9999.
000955*    -------------------------------
000956     05  FILLER            PIC  X(0003).
000957     05  BREFPCTO PIC  .9999.
000958*    -------------------------------
000959     05  FILLER            PIC  X(0003).
000960     05  BOVSPCTO PIC  .9999.
000961*    -------------------------------
000962     05  FILLER            PIC  X(0003).
000963     05  DMDSW2O PIC  X(0020).
000964*    -------------------------------
000965     05  FILLER            PIC  X(0003).
000966     05  BCLCPRMO PIC  X(0001).
000967*    -------------------------------
000968     05  FILLER            PIC  X(0003).
000969     05  BEMSG1O PIC  X(0079).
000970*    -------------------------------
000971     05  FILLER            PIC  X(0003).
000972     05  BPFKO PIC  X(0002).
000973*    -------------------------------
000974     05  FILLER            PIC  X(0003).
000975     05  BLUDATEO PIC  X(0008).
000976*    -------------------------------
000977     05  FILLER            PIC  X(0003).
000978     05  BLUTIMEO PIC  99B99B99.
000979*    -------------------------------
000980     05  FILLER            PIC  X(0003).
000981     05  BLUBYO PIC  X(0004).
000982*    -------------------------------
      *<<((file: EL105S))
000271     EJECT
000272*                                    COPY ELCLOGOF.
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
000273     EJECT
000274*                                    COPY ELCATTR.
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
000275     EJECT
000276*                                    COPY ELCAID.
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
000277 01  FILLER  REDEFINES DFHAID.
000278     05  FILLER                      PIC X(8).
000279     05  PF-VALUES                   PIC X   OCCURS 24.
000280
000281     EJECT
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
000283 01  DFHCOMMAREA                     PIC X(1024).
000284
000285     EJECT
000286*                                    COPY ELCCNTL.
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
000287     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL105' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000288 VCOBOL-DUMMY-PROCEDURE.
000289
000290 0000-MAINLINE SECTION.
000291
000292     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000293     MOVE '5'                   TO DC-OPTION-CODE.
000294     PERFORM 8500-DATE-CONVERSION.
000295     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000296     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000297
000298     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000299
000300     IF CREDIT-SESSION
000301         MOVE 'EL105B  '  TO  WS-MAP-NAME
000302     ELSE
000303         MOVE 'EL105A  '  TO  WS-MAP-NAME.
000304
000305*    NOTE *******************************************************
000306*         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
000307*         *  FROM ANOTHER MODULE.                               *
000308*         *******************************************************.
000309
000310     IF EIBCALEN NOT GREATER ZERO
000311         MOVE UNACCESS-MSG       TO  LOGOFF-MSG
000312         GO TO 8300-SEND-TEXT.
000313
000314     
      * EXEC CICS HANDLE CONDITION
000315*        PGMIDERR (9600-PGMIDERR)
000316*        NOTOPEN  (8700-NOT-OPEN)
000317*        DUPREC   (8800-DUPREC)
000318*        ERROR    (9990-ERROR)
000319*    END-EXEC.
      *    MOVE '"$LJ%.                ! " #00003694' TO DFHEIV0
           MOVE X'22244C4A252E202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033363934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000320
000321     EJECT
000322 0010-MAIN-LOGIC.
000323     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000324         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000325             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
000326             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
000327             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
000328             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
000329             MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
000330             MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
000331             MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
000332             MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
000333           ELSE
000334             MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
000335             MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
000336             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
000337             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
000338             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
000339             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
000340             MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
000341             MOVE SPACES               TO  PI-SAVED-PROGRAM-6
000342       ELSE
000343         GO TO 0020-MAIN-LOGIC.
000344
000345*    NOTE *******************************************************
000346*         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
000347*         *  INTERFACE BLOCK FOR THIS MODULE.                   *
000348*         *******************************************************.
000349     MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
000350                                     PI-PREV-MODE
000351                                     PI-PREV-CARRIER.
000352     MOVE ZERO                   TO  PI-1ST-TIME-SW
000353                                     PI-LINE-COUNT
000354                                     PI-BROWSE-SW
000355                                     PI-END-OF-FILE.
000356
000357*    NOTE *******************************************************
000358*         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *
000359*         *  FOR EL105.                                         *
000360*         *******************************************************.
000361     MOVE LOW-VALUES             TO  EL105AI.
000362     MOVE -1                     TO  AMAINTL.
000363     PERFORM 8100-SEND-INITIAL-MAP.
000364
000365     GO TO 9100-RETURN-TRAN.
000366
000367     EJECT
000368 0020-MAIN-LOGIC.
000369*    NOTE *******************************************************
000370*         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
000371*         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
000372*         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
000373*         *******************************************************.
000374     IF EIBAID = DFHCLEAR
000375         GO TO 9400-CLEAR.
000376
000377     IF NOT SYSTEM-DISPLAY-CAP
000378         MOVE 'READ'         TO SM-READ
000379         PERFORM 9995-SECURITY-VIOLATION
000380         MOVE ER-0070        TO EMI-ERROR
000381         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000382         PERFORM 8100-SEND-INITIAL-MAP
000383         GO TO 9100-RETURN-TRAN.
000384
000385     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000386         MOVE LOW-VALUES         TO  EL105AI
000387         MOVE ER-7008            TO  EMI-ERROR
000388         IF CREDIT-SESSION
000389             MOVE -1                 TO  BPFKL
000390             PERFORM 9900-ERROR-FORMAT
000391             PERFORM 8200-SEND-DATAONLY
000392             GO TO 9100-RETURN-TRAN
000393         ELSE
000394             MOVE -1                 TO  APFKL
000395             PERFORM 9900-ERROR-FORMAT
000396             PERFORM 8200-SEND-DATAONLY
000397             GO TO 9100-RETURN-TRAN.
000398
000399     
      * EXEC CICS RECEIVE
000400*        INTO   (EL105AI)
000401*        MAPSET (WS-MAPSET-NAME)
000402*        MAP    (WS-MAP-NAME)
000403*    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003779' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000404
000405     IF NOT CREDIT-SESSION
000406         IF APFKL GREATER ZERO
000407            IF EIBAID NOT = DFHENTER
000408                MOVE ER-0004           TO  EMI-ERROR
000409                MOVE AL-UNBON          TO  APFKA
000410                MOVE -1                TO  APFKL
000411                PERFORM 9900-ERROR-FORMAT
000412                PERFORM 8200-SEND-DATAONLY
000413                GO TO 9100-RETURN-TRAN
000414            ELSE
000415             IF APFKO GREATER ZERO AND LESS '25'
000416                 MOVE PF-VALUES (APFKI)  TO  EIBAID
000417               ELSE
000418                 MOVE ER-0029            TO  EMI-ERROR
000419                 MOVE AL-UNBOF           TO  APFKA
000420                 MOVE -1                 TO  APFKL
000421                 PERFORM 9900-ERROR-FORMAT
000422                 PERFORM 8200-SEND-DATAONLY
000423                 GO TO 9100-RETURN-TRAN
000424         ELSE
000425            MOVE AL-UNNOF            TO APFKA.
000426
000427     IF CREDIT-SESSION
000428         IF BPFKL GREATER ZERO
000429            IF EIBAID NOT = DFHENTER
000430                MOVE ER-0004           TO  EMI-ERROR
000431                MOVE AL-UNBON          TO  BPFKA
000432                MOVE -1                TO  BPFKL
000433                PERFORM 9900-ERROR-FORMAT
000434                PERFORM 8200-SEND-DATAONLY
000435                GO TO 9100-RETURN-TRAN
000436            ELSE
000437             IF BPFKO GREATER ZERO AND LESS '25'
000438                 MOVE PF-VALUES (BPFKI)  TO  EIBAID
000439               ELSE
000440                 MOVE ER-0029            TO  EMI-ERROR
000441                 MOVE AL-UNBOF           TO  BPFKA
000442                 MOVE -1                 TO  BPFKL
000443                 PERFORM 9900-ERROR-FORMAT
000444                 PERFORM 8200-SEND-DATAONLY
000445                 GO TO 9100-RETURN-TRAN
000446         ELSE
000447            MOVE AL-UNNOF            TO BPFKA.
000448
000449     IF EIBAID = DFHPF12
000450         MOVE 'EL010   '         TO  THIS-PGM
000451         GO TO 9300-XCTL.
000452
000453     IF EIBAID = DFHPF23
000454         GO TO 9000-RETURN-CICS.
000455
000456     IF EIBAID = DFHPF24
000457         IF  CREDIT-SESSION
000458             MOVE XCTL-EL626     TO THIS-PGM
000459             GO TO 9300-XCTL
000460         ELSE
000461             IF  CLAIM-SESSION
000462                 MOVE XCTL-EL126 TO THIS-PGM
000463                 GO TO 9300-XCTL
000464             ELSE
000465                 IF  MORTGAGE-SESSION
000466                     MOVE XCTL-EM626
000467                                 TO THIS-PGM
000468                     GO TO 9300-XCTL
000469                 ELSE
000470                     IF  GENERAL-LEDGER-SESSION
000471                         MOVE XCTL-GL800
000472                                 TO THIS-PGM
000473                         GO TO 9300-XCTL.
000474
000475     IF EIBAID = DFHENTER OR DFHPF1
000476         NEXT SENTENCE
000477       ELSE
000478         MOVE ER-7008               TO  EMI-ERROR
000479        IF CREDIT-SESSION
000480            MOVE -1                    TO  BPFKL
000481            PERFORM 9900-ERROR-FORMAT
000482            PERFORM 8200-SEND-DATAONLY
000483            GO TO 9100-RETURN-TRAN
000484        ELSE
000485            MOVE -1                    TO  APFKL
000486            PERFORM 9900-ERROR-FORMAT
000487            PERFORM 8200-SEND-DATAONLY
000488            GO TO 9100-RETURN-TRAN.
000489
000490     IF EIBAID = DFHPF1
000491       AND PI-END-OF-FILE = +1
000492         MOVE LOW-VALUES         TO  PI-NEXT-CARRIER-NUMBER
000493         MOVE ZERO               TO  PI-BROWSE-SW
000494                                     PI-END-OF-FILE
000495         GO TO 8000-DISPLAY-RECORDS.
000496
000497     EJECT
000498 0025-MAIN-LOGIC.
000499     IF AMAINTL NOT GREATER ZERO
000500       AND EIBAID = DFHPF1
000501         MOVE 'S'                TO  PI-MODE
000502                                     AMAINTO
000503         MOVE AL-UANON           TO  AMAINTA
000504         MOVE SPACES             TO  WS-CONTROL-FILE-KEY
000505         MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
000506         MOVE PI-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO
000507         MOVE '6'                TO  WS-CFK-RECORD-TYPE
000508         GO TO 8000-DISPLAY-RECORDS.
000509
000510     MOVE PI-MODE                TO PI-PREV-MODE.
000511     MOVE PI-CARRIER-NUMBER      TO PI-PREV-CARRIER.
000512
000513     IF AMAINTI = 'S' OR ' '
000514         NEXT SENTENCE
000515        ELSE
000516         IF EIBAID = DFHPF1
000517            MOVE ER-0050               TO  EMI-ERROR
000518            IF CREDIT-SESSION
000519                MOVE -1                TO  BPFKL
000520                PERFORM 9900-ERROR-FORMAT
000521            ELSE
000522                MOVE -1                TO  APFKL
000523                PERFORM 9900-ERROR-FORMAT.
000524
000525     IF AMAINTL GREATER ZERO
000526         IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'
000527             MOVE AMAINTI        TO  PI-MODE
000528             MOVE AL-UANON       TO  AMAINTA
000529           ELSE
000530             MOVE AL-UABOF       TO  AMAINTA
000531             MOVE -1             TO  AMAINTL
000532             MOVE ER-0023        TO EMI-ERROR
000533             PERFORM 9900-ERROR-FORMAT
000534       ELSE
000535         IF PI-1ST-TIME-SW NOT = ZERO
000536             NEXT SENTENCE
000537           ELSE
000538             IF EIBAID = DFHPF1
000539                 MOVE 'S'        TO PI-MODE
000540               ELSE
000541                 MOVE AL-UABOF   TO  AMAINTA
000542                 MOVE -1         TO  AMAINTL
000543                 MOVE ER-0023    TO EMI-ERROR
000544                 PERFORM 9900-ERROR-FORMAT.
000545
000546     IF ACARIERL GREATER  ZERO
000547         IF ACARIERI NOT = SPACES
000548             MOVE AL-UANON       TO  ACARIERA
000549             MOVE ACARIERI       TO  PI-CARRIER-NUMBER
000550           ELSE
000551             MOVE AL-UABOF       TO  ACARIERA
000552             MOVE -1             TO  ACARIERL
000553             MOVE ER-0193        TO EMI-ERROR
000554             PERFORM 9900-ERROR-FORMAT
000555       ELSE
000556         IF PI-1ST-TIME-SW NOT = ZERO
000557             NEXT SENTENCE
000558           ELSE
000559             IF EIBAID = DFHPF1
000560                 MOVE LOW-VALUES     TO  PI-NEXT-CARRIER-NUMBER
000561               ELSE
000562                 MOVE AL-UABOF       TO  ACARIERA
000563                 MOVE -1             TO  ACARIERL
000564                 MOVE ER-0193    TO EMI-ERROR
000565                 PERFORM 9900-ERROR-FORMAT.
000566
000567     IF EMI-FATAL-CTR GREATER ZERO
000568         PERFORM 8200-SEND-DATAONLY
000569         GO TO 9100-RETURN-TRAN.
000570
000571     MOVE +1                     TO  PI-1ST-TIME-SW.
000572
000573     IF  PI-MODE NOT = 'S'
000574         MOVE +1                 TO  PI-BROWSE-SW.
000575
000576     EJECT
000577 0100-MAIN-LOGIC.
000578     IF PI-MODE NOT = 'S'
000579         GO TO 0200-MAIN-LOGIC.
000580
000581     MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
000582     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
000583
000584     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
000585     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
000586
000587     IF EIBAID = DFHPF1
000588         MOVE PI-NEXT-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO
000589         GO TO 8000-DISPLAY-RECORDS.
000590
000591     GO TO 6000-SHOW-RECORD.
000592
000593     EJECT
000594 0200-MAIN-LOGIC.
000595     IF SYSTEM-MODIFY-CAP
000596        NEXT SENTENCE
000597       ELSE
000598        IF PI-MODE = 'A' OR 'C' OR 'D'
000599           MOVE 'UPDATE'              TO SM-READ
000600           PERFORM 9995-SECURITY-VIOLATION
000601           MOVE ER-0070               TO EMI-ERROR
000602           MOVE -1                    TO  AMAINTL
000603           MOVE AL-UABON              TO  AMAINTA
000604           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000605           PERFORM 8200-SEND-DATAONLY
000606           GO TO 9100-RETURN-TRAN.
000607
000608     IF PI-MODE NOT = 'C'
000609         GO TO 0300-MAIN-LOGIC.
000610
000611     MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
000612
000613     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
000614     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
000615     MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
000616     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
000617
000618     
      * EXEC CICS READ
000619*        DATASET (WS-CONTROL-FILE-DSID)
000620*        RIDFLD  (WS-CONTROL-FILE-KEY)
000621*        SET     (ADDRESS OF CONTROL-FILE)
000622*   END-EXEC.
      *    MOVE '&"S        E          (   #00003998' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033393938' TO DFHEIV0
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
           
000623
000624     MOVE CF-EXPENSE-DOLLAR      TO  WS-EXPENSE-DOLLAR.
000625     MOVE CF-EXPENSE-PERCENT     TO  WS-EXPENSE-PERCENT.
000626
000627     IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
000628         MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
000629     IF CF-IBNR-R78-PERCENT NOT NUMERIC
000630         MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
000631     IF CF-IBNR-PRO-PERCENT NOT NUMERIC
000632         MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
000633
000634     MOVE CF-IBNR-UEPRM-PERCENT  TO  WS-IBNR-UEP-PCT.
000635     MOVE CF-IBNR-R78-PERCENT    TO  WS-IBNR-R78-PCT.
000636     MOVE CF-IBNR-PRO-PERCENT    TO  WS-IBNR-PRO-PCT.
000637
000638     PERFORM 4000-EDIT-MAP-FIELDS.
000639
000640     IF EMI-FATAL-CTR  GREATER ZERO
000641         PERFORM 8200-SEND-DATAONLY
000642         GO TO 9100-RETURN-TRAN.
000643
000644     PERFORM 5100-CHANGE-RECORD.
000645
000646     MOVE ER-0000                TO EMI-ERROR.
000647     PERFORM 9900-ERROR-FORMAT.
000648     MOVE LOW-VALUES             TO  EL105AO.
000649     MOVE -1                     TO  AMAINTL.
000650     MOVE ZERO                   TO  PI-1ST-TIME-SW.
000651     GO TO 6000-SHOW-RECORD.
000652
000653     EJECT
000654 0300-MAIN-LOGIC.
000655*    NOTE *******************************************************
000656*         *          P R O C E S S   T H E   A D D S            *
000657*         *******************************************************.
000658     IF PI-MODE NOT = 'A'
000659         GO TO 0400-MAIN-LOGIC.
000660
000661     MOVE ZEROS                  TO  WS-IBNR-UEP-PCT
000662                                     WS-IBNR-R78-PCT
000663                                     WS-IBNR-PRO-PCT.
000664
000665     PERFORM 4000-EDIT-MAP-FIELDS.
000666
000667     IF EMI-FATAL-CTR GREATER ZERO
000668         PERFORM 8200-SEND-DATAONLY
000669         GO TO 9100-RETURN-TRAN.
000670
000671     PERFORM 5000-ADD-RECORD.
000672
000673     MOVE ER-0000                TO EMI-ERROR.
000674     PERFORM 9900-ERROR-FORMAT.
000675     MOVE LOW-VALUES             TO  EL105AO.
000676     MOVE -1                     TO  AMAINTL.
000677     PERFORM 8100-SEND-INITIAL-MAP.
000678     MOVE ZERO                   TO  PI-1ST-TIME-SW.
000679     GO TO 9100-RETURN-TRAN.
000680
000681     EJECT
000682 0400-MAIN-LOGIC.
000683*    NOTE *******************************************************
000684*         *         P R O C E S S   T H E   D E L E T E S       *
000685*         *******************************************************.
000686     IF PI-PREV-MODE    = 'S' AND
000687        PI-PREV-CARRIER = PI-CARRIER-NUMBER
000688           PERFORM 5200-DELETE-RECORD
000689        ELSE
000690           MOVE AL-UABOF       TO AMAINTA
000691           MOVE -1             TO AMAINTL
000692           MOVE ER-0145        TO EMI-ERROR
000693           PERFORM 9900-ERROR-FORMAT
000694           PERFORM 8200-SEND-DATAONLY
000695           GO TO 9100-RETURN-TRAN.
000696
000697     MOVE ER-0000                TO EMI-ERROR.
000698     PERFORM 9900-ERROR-FORMAT.
000699     MOVE LOW-VALUES             TO  EL105AO.
000700     PERFORM 8100-SEND-INITIAL-MAP.
000701     MOVE ZERO                   TO  PI-1ST-TIME-SW.
000702     GO TO 9100-RETURN-TRAN.
000703     EJECT
000704 4000-EDIT-MAP-FIELDS SECTION.
000705*    NOTE *******************************************************
000706*         *              EDIT SECURE PAY SWITCH.                *
000707*         *******************************************************.
000708     IF CREDIT-SESSION
000709         IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
000710             MOVE AL-SADOF           TO BSPLABLA
000711             MOVE AL-SADOF           TO BSECPAYA
000712         ELSE
000713             IF BSECPAYL GREATER ZERO
000714                 IF BSECPAYI = ' ' OR 'Y' OR 'N'
000715                     MOVE AL-UANON       TO  BSECPAYA
000716                     MOVE BSECPAYI       TO  BSECPAYO
000717                 ELSE
000718                     MOVE ER-3270        TO  EMI-ERROR
000719                     PERFORM 9900-ERROR-FORMAT
000720                     MOVE AL-UNBON       TO  BSECPAYA
000721                     MOVE -1             TO  BSECPAYL
000722                 END-IF
000723             END-IF
000724         END-IF
000725     END-IF.
000726
000727*    IF NOT CREDIT-SESSION
000728*        IF PI-COMPANY-ID = 'CID'
000729*            MOVE AL-SADOF           TO ASPLABLA
000730*            MOVE AL-SADOF           TO ASECPAYA
000731*        ELSE
000732*            IF ASECPAYL GREATER ZERO
000733*                IF ASECPAYI = ' ' OR 'Y' OR 'N'
000734*                    MOVE AL-UANON       TO  ASECPAYA
000735*                    MOVE ASECPAYI       TO  ASECPAYO
000736*                ELSE
000737*                    MOVE ER-3270        TO  EMI-ERROR
000738*                    PERFORM 9900-ERROR-FORMAT
000739*                    MOVE AL-UNBON       TO  ASECPAYA
000740*                    MOVE -1             TO  ASECPAYL
000741*                END-IF
000742*            END-IF
000743*        END-IF
000744*    END-IF.
000745
000746*    NOTE *******************************************************
000747*         *              EDIT THE PHONE NUMBER.                 *
000748*         *******************************************************.
000749     IF CREDIT-SESSION
000750         IF BPHONEL GREATER ZERO
000751             
      * EXEC CICS BIF DEEDIT
000752*                FIELD  (BPHONEI)
000753*                LENGTH (APHONE-LENGTH)
000754*            END-EXEC
      *    MOVE '@"L                   #   #00004131' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034313331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000755             IF BPHONEI NUMERIC
000756                 MOVE AL-UNNON       TO  BPHONEA
000757                 MOVE BPHONEI        TO  BPHONEO
000758                 INSPECT BPHONEO CONVERTING ' '    TO '-'
000759             ELSE
000760                 MOVE ER-0053           TO  EMI-ERROR
000761                 PERFORM 9900-ERROR-FORMAT
000762                 MOVE AL-UNBON       TO  BPHONEA
000763                 MOVE -1             TO  BPHONEL.
000764
000765     IF NOT CREDIT-SESSION
000766         IF APHONEL GREATER ZERO
000767             
      * EXEC CICS BIF DEEDIT
000768*                FIELD  (APHONEI)
000769*                LENGTH (APHONE-LENGTH)
000770*            END-EXEC
      *    MOVE '@"L                   #   #00004147' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034313437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000771             IF APHONEI NUMERIC
000772                 MOVE AL-UNNON       TO  APHONEA
000773                 MOVE APHONEI        TO  APHONEO
000774                 INSPECT APHONEO CONVERTING SPACES TO '-'
000775             ELSE
000776                 MOVE ER-0053           TO  EMI-ERROR
000777                 PERFORM 9900-ERROR-FORMAT
000778                 MOVE AL-UNBON       TO  APHONEA
000779                 MOVE -1             TO  APHONEL.
000780
000781*    NOTE *******************************************************
000782*         *              EDIT THE ZIP CODE.                     *
000783*         *******************************************************.
000784     IF CREDIT-SESSION
000785         IF BZIPL GREATER ZERO
000786             MOVE AL-UANON          TO  BZIPA.
000787
000788     IF NOT CREDIT-SESSION
000789         IF AZIPL GREATER ZERO
000790             MOVE AL-UANON          TO  AZIPA.
000791
000792*    NOTE *******************************************************
000793*         *              EDIT THE DOMICILE STATE CODE.          *
000794*         *******************************************************.
000795     IF CREDIT-SESSION
000796         IF BDOMSTL GREATER ZERO
000797            IF BDOMSTI ALPHABETIC-UPPER
000798                MOVE AL-UNNON       TO  BDOMSTA
000799             ELSE
000800                MOVE AL-UNBON       TO  BDOMSTA
000801                MOVE -1             TO  BDOMSTL
000802                MOVE ER-0529 TO  EMI-ERROR
000803                PERFORM 9900-ERROR-FORMAT.
000804
000805*    NOTE *******************************************************
000806*         *              EDIT THE CLP PERCENT.                  *
000807*         *******************************************************.
000808     IF CREDIT-SESSION
000809         IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
000810             MOVE AL-SADOF          TO  BCTLABLA
000811             MOVE AL-SADOF          TO  BCLPTOLA
000812         ELSE
000813             IF BCLPTOLL GREATER ZERO
000814                 
      * EXEC CICS BIF
000815*                    DEEDIT
000816*                    FIELD  (BCLPTOLI)
000817*                    LENGTH (7)
000818*                END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004194' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034313934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCLPTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000819                 IF BCLPTOLI NUMERIC
000820                     MOVE AL-UNNON      TO  BCLPTOLA
000821                     MOVE BCLPTOLI      TO  WS-CLP-TOL-PCT
000822                 ELSE
000823                     MOVE AL-UNBON      TO  BCLPTOLA
000824                     MOVE -1            TO  BCLPTOLL
000825                     MOVE ER-1778       TO  EMI-ERROR
000826                     PERFORM 9900-ERROR-FORMAT
000827                 END-IF
000828             END-IF
000829         END-IF
000830     END-IF.
000831
000832*    NOTE *******************************************************
000833*         *              EDIT THE LEASE COMM AMOUNT             *
000834*         *******************************************************
000835     IF CREDIT-SESSION
000836        IF BLCOMML > ZERO
000837           
      * EXEC CICS BIF
000838*               DEEDIT
000839*               FIELD  (BLCOMMI)
000840*               LENGTH (8)
000841*          END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004217' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034323137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BLCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000842           IF BLCOMMI NUMERIC
000843              MOVE AL-UNNON      TO BLCOMMA
000844              MOVE BLCOMMI       TO WS-SPP-LEASE-COMM
000845           ELSE
000846              MOVE AL-UNBON      TO BLCOMMA
000847              MOVE -1            TO BLCOMML
000848              MOVE ER-1778       TO EMI-ERROR
000849              PERFORM 9900-ERROR-FORMAT
000850           END-IF
000851        END-IF
000852     END-IF
000853
000854*    NOTE *******************************************************
000855*         *              EDIT THE PREMIUM TOLERANCE PERCENTAGE  *
000856*         *******************************************************.
000857     IF CREDIT-SESSION
000858         IF BPRMTOLL GREATER ZERO
000859             
      * EXEC CICS BIF DEEDIT
000860*                FIELD  (BPRMTOLI)
000861*                LENGTH (6)
000862*            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004239' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034323339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000863             IF BPRMTOLI NOT NUMERIC
000864                 MOVE -1             TO  BPRMTOLL
000865                 MOVE AL-UNBON       TO  BPRMTOLA
000866                 MOVE ER-2010           TO  EMI-ERROR
000867                 PERFORM 9900-ERROR-FORMAT
000868             ELSE
000869             IF BPRMTOLI GREATER THAN 9999
000870                 MOVE -1             TO  BPRMTOLL
000871                 MOVE AL-UNBON       TO  BPRMTOLA
000872                 MOVE ER-2010           TO  EMI-ERROR
000873                 PERFORM 9900-ERROR-FORMAT
000874             ELSE
000875                 MOVE AL-UNNON       TO  BPRMTOLA
000876                 MOVE BPRMTOLI       TO  BPRMTOLO.
000877
000878     IF CREDIT-SESSION
000879         IF BREFTOLL GREATER ZERO
000880             
      * EXEC CICS BIF DEEDIT
000881*                FIELD  (BREFTOLI)
000882*                LENGTH (6)
000883*            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004260' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034323630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000884             IF BREFTOLI NOT NUMERIC
000885                 MOVE -1             TO  BREFTOLL
000886                 MOVE AL-UNBON       TO  BREFTOLA
000887                 MOVE ER-2014           TO  EMI-ERROR
000888                 PERFORM 9900-ERROR-FORMAT
000889             ELSE
000890             IF BREFTOLI GREATER THAN 9999
000891                 MOVE -1             TO  BREFTOLL
000892                 MOVE AL-UNBON       TO  BREFTOLA
000893                 MOVE ER-2014           TO  EMI-ERROR
000894                 PERFORM 9900-ERROR-FORMAT
000895             ELSE
000896                 MOVE AL-UNNON       TO  BREFTOLA
000897                 MOVE BREFTOLI       TO  BREFTOLO.
000898
000899     IF CREDIT-SESSION
000900         IF BPRMPCTL GREATER ZERO
000901             
      * EXEC CICS BIF DEEDIT
000902*                FIELD  (BPRMPCTI)
000903*                LENGTH (5)
000904*            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004281' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034323831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000905             IF BPRMPCTI NUMERIC
000906                 MOVE AL-UNNON       TO  BPRMPCTA
000907                 MOVE BPRMPCTI       TO  WS-TOL-PREM-PCT
000908             ELSE
000909                 MOVE -1             TO  BPRMPCTL
000910                 MOVE AL-UNBON       TO  BPRMPCTA
000911                 MOVE ER-7532           TO  EMI-ERROR
000912                 PERFORM 9900-ERROR-FORMAT.
000913
000914     IF CREDIT-SESSION
000915         IF BREFPCTL GREATER ZERO
000916             
      * EXEC CICS BIF DEEDIT
000917*                FIELD  (BREFPCTI)
000918*                LENGTH (5)
000919*            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004296' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034323936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000920             IF BREFPCTI NUMERIC
000921                 MOVE AL-UNNON       TO  BREFPCTA
000922                 MOVE BREFPCTI       TO  WS-TOL-REF-PCT
000923             ELSE
000924                 MOVE -1             TO  BREFPCTL
000925                 MOVE AL-UNBON       TO  BREFPCTA
000926                 MOVE ER-7532           TO  EMI-ERROR
000927                 PERFORM 9900-ERROR-FORMAT.
000928
000929     IF CREDIT-SESSION
000930         IF BOVSPCTL GREATER ZERO
000931             
      * EXEC CICS BIF DEEDIT
000932*                FIELD  (BOVSPCTI)
000933*                LENGTH (5)
000934*            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004311' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034333131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BOVSPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000935             IF BOVSPCTI NUMERIC
000936                 MOVE AL-UNNON       TO  BOVSPCTA
000937                 MOVE BOVSPCTI       TO  WS-CR-OVR-SHT-PCT
000938             ELSE
000939                 MOVE -1             TO  BOVSPCTL
000940                 MOVE AL-UNBON       TO  BOVSPCTA
000941                 MOVE ER-7532           TO  EMI-ERROR
000942                 PERFORM 9900-ERROR-FORMAT
000943             END-IF
000944         END-IF.
000945
000946     IF CREDIT-SESSION
000947         GO TO 4900-EXIT.
000948
000949     IF CLAIM-SESSION
000950        IF ALPHCHL GREATER ZERO
000951           IF ALPHCHI ALPHABETIC
000952              MOVE AL-UANON       TO  ALPHCHA
000953           ELSE
000954              MOVE AL-UNBON       TO  ALPHCHA
000955              MOVE -1             TO  ALPHCHL
000956              MOVE ER-8128        TO  EMI-ERROR
000957              PERFORM 9900-ERROR-FORMAT.
000958
000959     IF ADOMSTL GREATER ZERO
000960         IF ADOMSTI ALPHABETIC-UPPER
000961             MOVE AL-UNNON       TO  ADOMSTA
000962         ELSE
000963             MOVE AL-UNBON       TO  ADOMSTA
000964             MOVE -1             TO  ADOMSTL
000965             MOVE ER-0529 TO  EMI-ERROR
000966             PERFORM 9900-ERROR-FORMAT.
000967
000968*    IF PI-COMPANY-ID = 'CID'
000969*        MOVE AL-SADOF           TO ACTLABLA
000970*        MOVE AL-SADOF           TO ACLPTOLA
000971*    ELSE
000972*        IF ACLPTOLL GREATER ZERO
000973*            EXEC CICS BIF
000974*                DEEDIT
000975*                FIELD  (ACLPTOLI)
000976*                LENGTH (6)
000977*            END-EXEC
000978*            IF ACLPTOLI NUMERIC
000979*                MOVE AL-UNNON       TO  ACLPTOLA
000980*                MOVE ACLPTOLI       TO  WS-CLP-TOL-PCT
000981*            ELSE
000982*                MOVE AL-UNBON       TO  ACLPTOLA
000983*                MOVE -1             TO  ACLPTOLL
000984*                MOVE ER-1778 TO  EMI-ERROR
000985*                PERFORM 9900-ERROR-FORMAT
000986*            END-IF
000987*        END-IF
000988*    END-IF.
000989
000990*    NOTE *******************************************************
000991*         *      EDIT THE CLAIM NUMBER ASSIGNMENT METHOD.       *
000992*         *******************************************************.
000993     IF ACLNAML GREATER ZERO
000994         IF (ACLNAMI = '1' OR '2' OR '3' OR '4' OR '5')
000995             MOVE AL-UNNON       TO  ACLNAMA
000996           ELSE
000997             MOVE AL-UNBON       TO  ACLNAMA
000998             MOVE -1             TO  ACLNAML
000999             MOVE ER-0090        TO EMI-ERROR
001000             PERFORM 9900-ERROR-FORMAT
001001       ELSE
001002         IF PI-MODE = 'A'
001003             MOVE AL-UNBOF       TO  ACLNAMA
001004             MOVE -1             TO  ACLNAML
001005             MOVE ER-0090        TO EMI-ERROR
001006             PERFORM 9900-ERROR-FORMAT.
001007
001008*    NOTE *******************************************************
001009*         *      EDIT THE CHECK NUMBER ASSIGNMENT METHOD.       *
001010*         *******************************************************.
001011     IF ACKNAML GREATER ZERO
001012         IF ACKNAMI GREATER ZERO
001013           AND ACKNAMI LESS '5'
001014             MOVE AL-UNNON       TO  ACKNAMA
001015           ELSE
001016             MOVE AL-UNBON       TO  ACKNAMA
001017             MOVE -1             TO  ACKNAML
001018             MOVE ER-0091        TO EMI-ERROR
001019             PERFORM 9900-ERROR-FORMAT
001020       ELSE
001021         IF PI-MODE = 'A'
001022             MOVE AL-UNBOF       TO  ACKNAMA
001023             MOVE -1             TO  ACKNAML
001024             MOVE ER-0091        TO EMI-ERROR
001025             PERFORM 9900-ERROR-FORMAT.
001026
001027*    NOTE *******************************************************
001028*         *            EDIT THE CLAIM STARTING NUMBER.          *
001029*         *******************************************************.
001030     IF ACLAIML GREATER ZERO
001031         
      * EXEC CICS BIF DEEDIT
001032*            FIELD  (ACLAIMI)
001033*            LENGTH (8)
001034*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004411' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034343131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACLAIMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001035         IF ACLAIMI NUMERIC
001036             MOVE AL-UNNON       TO  ACLAIMA
001037           ELSE
001038             MOVE AL-UNBON       TO  ACLAIMA
001039             MOVE -1             TO  ACLAIML
001040             MOVE ER-0637        TO EMI-ERROR
001041             PERFORM 9900-ERROR-FORMAT
001042
001043*    NOTE *******************************************************
001044*         *            EDIT THE CHECK STARTING NUMBER.          *
001045*         *******************************************************.
001046     IF ACHECKL GREATER ZERO
001047         
      * EXEC CICS BIF DEEDIT
001048*            FIELD  (ACHECKI)
001049*            LENGTH (8)
001050*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004427' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034343237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACHECKI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001051         IF ACHECKI NUMERIC
001052             MOVE AL-UNNON       TO  ACHECKA
001053           ELSE
001054             MOVE AL-UNBON       TO  ACHECKA
001055             MOVE -1             TO  ACHECKL
001056             MOVE ER-0638           TO  EMI-ERROR
001057             PERFORM 9900-ERROR-FORMAT
001058
001059*    NOTE *******************************************************
001060*         *              EDIT THE LETTER ARCHIVE.               *
001061*         *******************************************************.
001062     IF ALAL GREATER ZERO
001063         IF ALAI = 'Y' OR 'N'
001064             MOVE AL-UANON       TO  ALAA
001065           ELSE
001066             MOVE AL-UABON       TO  ALAA
001067             MOVE -1             TO  ALAL
001068             MOVE ER-0092        TO EMI-ERROR
001069             PERFORM 9900-ERROR-FORMAT
001070       ELSE
001071         IF PI-MODE = 'A'
001072             MOVE AL-UABOF       TO  ALAA
001073             MOVE -1             TO  ALAL
001074             MOVE ER-0092        TO EMI-ERROR
001075             PERFORM 9900-ERROR-FORMAT.
001076
001077     IF AEXPCML NOT GREATER ZERO
001078         GO TO 4200-EDIT-MAP-FIELDS.
001079
001080     IF AEXPCML GREATER ZERO
001081         IF AEXPCMI GREATER ZERO
001082           AND AEXPCMI LESS '5'
001083             MOVE AL-UNNON           TO  AEXPCMA
001084           ELSE
001085             MOVE AL-UNBON           TO  AEXPCMA
001086             MOVE -1                 TO  AEXPCML
001087             MOVE ER-0093            TO EMI-ERROR
001088             PERFORM 9900-ERROR-FORMAT.
001089
001090     IF AEXPCAL GREATER ZERO
001091         
      * EXEC CICS BIF DEEDIT
001092*            FIELD  (AEXPCAI)
001093*            LENGTH (AEXPCA-LENGTH)
001094*        END-EXEC
      *    MOVE '@"L                   #   #00004471' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034343731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCAI, 
                 AEXPCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001095         IF AEXPCAI NUMERIC
001096             MOVE AL-UNNON       TO  AEXPCAA
001097             MOVE AEXPCAI        TO  WS-EXPENSE-DOLLAR
001098                                     AEXPCAO
001099           ELSE
001100             MOVE AL-UNBON       TO  AEXPCAA
001101             MOVE -1             TO  AEXPCAL
001102             MOVE ER-0097        TO EMI-ERROR
001103             PERFORM 9900-ERROR-FORMAT.
001104
001105     IF AEXPCPL GREATER ZERO
001106         
      * EXEC CICS BIF DEEDIT
001107*            FIELD  (AEXPCPI)
001108*            LENGTH (AEXPCP-LENGTH)
001109*        END-EXEC
      *    MOVE '@"L                   #   #00004486' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034343836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCPI, 
                 AEXPCP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001110         IF AEXPCPI NUMERIC
001111             MOVE AEXPCPI        TO  WS-EXPENSE-PERCENT
001112                                     AEXPCPO
001113             MOVE AL-UNNON       TO  AEXPCPA
001114           ELSE
001115             MOVE AL-UNBON       TO  AEXPCPA
001116             MOVE -1             TO  AEXPCPL
001117             MOVE ER-0099        TO EMI-ERROR
001118             PERFORM 9900-ERROR-FORMAT.
001119
001120     IF CLAIM-SESSION
001121        IF ABRETRL GREATER ZERO
001122           IF ABRETRI NUMERIC
001123              MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS
001124           ELSE
001125              MOVE AL-UNBON           TO ABRETRA
001126              MOVE -1                 TO ABRETRL
001127              MOVE ER-8127            TO EMI-ERROR
001128              PERFORM 9900-ERROR-FORMAT.
001129
001130     IF AEXPCMI NOT = '1'
001131         GO TO 4120-EDIT-MAP-FIELDS.
001132
001133     IF WS-EXPENSE-PERCENT NOT = ZERO
001134         MOVE AL-UNBON           TO  AEXPCPA
001135         MOVE -1                 TO  AEXPCPL
001136         MOVE ER-0094            TO EMI-ERROR
001137         PERFORM 9900-ERROR-FORMAT.
001138
001139     IF WS-EXPENSE-DOLLAR NOT = ZERO
001140         MOVE AL-UNBON           TO  AEXPCAA
001141         MOVE -1                 TO  AEXPCAL
001142         MOVE ER-0095            TO EMI-ERROR
001143         PERFORM 9900-ERROR-FORMAT.
001144
001145     GO TO 4200-EDIT-MAP-FIELDS.
001146
001147 4120-EDIT-MAP-FIELDS.
001148     IF AEXPCMI NOT = '2'
001149         GO TO 4130-EDIT-MAP-FIELDS.
001150
001151     IF WS-EXPENSE-PERCENT NOT = ZERO
001152         MOVE AL-UNBON           TO  AEXPCPA
001153         MOVE -1                 TO  AEXPCPL
001154         MOVE ER-0096            TO EMI-ERROR
001155         PERFORM 9900-ERROR-FORMAT.
001156
001157     IF WS-EXPENSE-DOLLAR = ZERO
001158         MOVE AL-UNBON           TO  AEXPCAA
001159         MOVE -1                 TO  AEXPCAL
001160         MOVE ER-0098            TO EMI-ERROR
001161         PERFORM 9900-ERROR-FORMAT.
001162
001163     GO TO 4200-EDIT-MAP-FIELDS.
001164
001165 4130-EDIT-MAP-FIELDS.
001166     IF AEXPCMI NOT = '3'
001167         GO TO 4140-EDIT-MAP-FIELDS.
001168
001169     IF WS-EXPENSE-PERCENT = ZERO
001170         MOVE AL-UNBON           TO  AEXPCPA
001171         MOVE -1                 TO  AEXPCPL
001172         MOVE ER-0100            TO EMI-ERROR
001173         PERFORM 9900-ERROR-FORMAT.
001174
001175     IF WS-EXPENSE-DOLLAR NOT = ZERO
001176         MOVE AL-UNBON           TO  AEXPCAA
001177         MOVE -1                 TO  AEXPCAL
001178         MOVE ER-0117            TO EMI-ERROR
001179         PERFORM 9900-ERROR-FORMAT.
001180
001181     GO TO 4200-EDIT-MAP-FIELDS.
001182
001183 4140-EDIT-MAP-FIELDS.
001184     IF WS-EXPENSE-PERCENT NOT = ZERO
001185         MOVE AL-UNBON           TO  AEXPCPA
001186         MOVE -1                 TO  AEXPCPL
001187         MOVE ER-0101            TO EMI-ERROR
001188         PERFORM 9900-ERROR-FORMAT.
001189
001190     IF WS-EXPENSE-DOLLAR = ZERO
001191         MOVE AL-UABON           TO  AEXPCAA
001192         MOVE -1                 TO  AEXPCAL
001193         MOVE ER-0102            TO EMI-ERROR
001194         PERFORM 9900-ERROR-FORMAT.
001195
001196     GO TO 4200-EDIT-MAP-FIELDS.
001197
001198 4200-EDIT-MAP-FIELDS.
001199*    NOTE *******************************************************
001200*         *          EDIT THE CLAIM CALCULATION METHOD.         *
001201*         *******************************************************.
001202     IF ACLCML GREATER ZERO
001203         IF ACLCMI GREATER ZERO
001204           AND ACLCMI LESS '7'
001205             MOVE AL-UNNON       TO  ACLCMA
001206           ELSE
001207             MOVE AL-UNBON       TO  ACLCMA
001208             MOVE -1             TO  ACLCML
001209             MOVE ER-0103        TO EMI-ERROR
001210             PERFORM 9900-ERROR-FORMAT.
001211
001212*    NOTE *******************************************************
001213*         *      EDIT THE MANUAL RESERVES INDICATOR.            *
001214*         *******************************************************.
001215     IF ARESMANL GREATER ZERO
001216         IF ARESMANI = 'Y' OR 'N'
001217             MOVE AL-UANON       TO  ARESMANA
001218           ELSE
001219             MOVE AL-UABON       TO  ARESMANA
001220             MOVE -1             TO  ARESMANL
001221             MOVE ER-0107        TO EMI-ERROR
001222             PERFORM 9900-ERROR-FORMAT
001223       ELSE
001224         IF PI-MODE = 'A'
001225             MOVE AL-UABOF       TO  ARESMANA
001226             MOVE -1             TO  ARESMANL
001227             MOVE ER-0108        TO EMI-ERROR
001228             PERFORM 9900-ERROR-FORMAT.
001229
001230*    NOTE *******************************************************
001231*         *      EDIT THE CDT/FUTURE RESERVES INDICATOR.        *
001232*         *******************************************************.
001233     IF ARESCDTL GREATER ZERO
001234         IF ARESCDTI = 'Y' OR 'N'
001235             MOVE AL-UANON       TO  ARESCDTA
001236           ELSE
001237             MOVE AL-UABON       TO  ARESCDTA
001238             MOVE -1             TO  ARESCDTL
001239             MOVE ER-0109        TO EMI-ERROR
001240             PERFORM 9900-ERROR-FORMAT
001241       ELSE
001242         IF PI-MODE = 'A'
001243             MOVE AL-UABOF       TO  ARESCDTA
001244             MOVE -1             TO  ARESCDTL
001245             MOVE ER-0110        TO EMI-ERROR
001246             PERFORM 9900-ERROR-FORMAT.
001247
001248*    NOTE *******************************************************
001249*         *        EDIT THE IBN RESERVES INDICATOR.             *
001250*         *******************************************************.
001251     IF ARESIBNL GREATER ZERO
001252         IF ARESIBNI = 'Y' OR 'N' OR '1' OR '2'
001253             MOVE AL-UANON       TO  ARESIBNA
001254           ELSE
001255             MOVE AL-UABON       TO  ARESIBNA
001256             MOVE -1             TO  ARESIBNL
001257             MOVE ER-0111        TO EMI-ERROR
001258             PERFORM 9900-ERROR-FORMAT
001259       ELSE
001260         IF PI-MODE = 'A'
001261             MOVE AL-UABOF       TO  ARESIBNA
001262             MOVE -1             TO  ARESIBNL
001263             MOVE ER-0112        TO EMI-ERROR
001264             PERFORM 9900-ERROR-FORMAT.
001265
001266*    NOTE *******************************************************
001267*         *      EDIT THE PAY-TO-CURRENT RESERVES INDICATOR.    *
001268*         *******************************************************.
001269     IF ARESPTCL GREATER ZERO
001270         IF ARESPTCI = 'Y' OR 'N'
001271             MOVE AL-UANON       TO  ARESPTCA
001272           ELSE
001273             MOVE AL-UABON       TO  ARESPTCA
001274             MOVE -1             TO  ARESPTCL
001275             MOVE ER-0113        TO EMI-ERROR
001276             PERFORM 9900-ERROR-FORMAT
001277       ELSE
001278         IF PI-MODE = 'A'
001279             MOVE AL-UABOF       TO  ARESPTCA
001280             MOVE -1             TO  ARESPTCL
001281             MOVE ER-0114        TO EMI-ERROR
001282             PERFORM 9900-ERROR-FORMAT.
001283
001284*    NOTE *******************************************************
001285*         *      IF THE PERCENT OF CDT IS ENTERED THE CDT ACCESS*
001286*         *  MUST ALSO BE ENTERED ON ADD.                       *
001287*         *******************************************************.
001288     IF PI-MODE = 'A'
001289         IF ACDTAL NOT GREATER ZERO
001290           AND APCTCDTL GREATER ZERO
001291             MOVE -1             TO  ACDTAL
001292             MOVE AL-UNBOF       TO  ACDTAA
001293             MOVE AL-UNBON       TO  APCTCDTA
001294             MOVE ER-0104        TO EMI-ERROR
001295             PERFORM 9900-ERROR-FORMAT.
001296
001297*    NOTE *******************************************************
001298*         *      EDIT THE PERCENT OF CDT.                       *
001299*         *******************************************************.
001300     IF APCTCDTL GREATER ZERO
001301         
      * EXEC CICS BIF DEEDIT
001302*            FIELD  (APCTCDTI)
001303*            LENGTH (APCTCDT-LENGTH)
001304*        END-EXEC
      *    MOVE '@"L                   #   #00004681' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034363831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APCTCDTI, 
                 APCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001305         IF APCTCDTI NUMERIC
001306             MOVE AL-UNNON       TO  APCTCDTA
001307             MOVE APCTCDTI         TO  APCTCDTO
001308           ELSE
001309             MOVE AL-UNBON       TO  APCTCDTA
001310             MOVE -1             TO  APCTCDTL
001311             MOVE ER-0106           TO  EMI-ERROR
001312             PERFORM 9900-ERROR-FORMAT.
001313
001314*    NOTE *******************************************************
001315*         *      EDIT THE CDT ACCESS METHOD                     *
001316*         *******************************************************.
001317     IF ACDTAL GREATER THAN +0
001318         IF (ACDTAI = '1' OR '2' OR '3')
001319            OR
001320            (PI-COMPANY-ID EQUAL 'FLA' AND
001321             ACDTAI EQUAL '1' OR '2' OR '3' OR '4')
001322             MOVE AL-UANON       TO  ARESIBNA
001323           ELSE
001324             MOVE AL-UABON       TO  ACDTAA
001325             MOVE -1             TO  ACDTAL
001326             MOVE ER-0105        TO EMI-ERROR
001327             PERFORM 9900-ERROR-FORMAT.
001328
001329*    NOTE *******************************************************
001330*         *      EDIT THE IBNR PERCENT.                         *
001331*         *******************************************************.
001332     IF IBNRPCTL GREATER ZERO
001333         
      * EXEC CICS BIF DEEDIT
001334*            FIELD  (IBNRPCTI)
001335*            LENGTH (IBNRPCT-LENGTH)
001336*        END-EXEC
      *    MOVE '@"L                   #   #00004713' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRPCTI, 
                 IBNRPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001337         IF IBNRPCTI NUMERIC
001338             MOVE AL-UNNON       TO  IBNRPCTA
001339             MOVE IBNRPCTI       TO  IBNRPCTO
001340           ELSE
001341             MOVE AL-UNBON       TO  IBNRPCTA
001342             MOVE -1             TO  IBNRPCTL
001343             MOVE ER-0106        TO  EMI-ERROR
001344             PERFORM 9900-ERROR-FORMAT.
001345
001346*    NOTE *******************************************************
001347*         *      EDIT THE IBNR UNEARNED PREMIUM PERCENT         *
001348*         *******************************************************.
001349     IF AUEPPCTL GREATER ZERO
001350         
      * EXEC CICS BIF DEEDIT
001351*            FIELD  (AUEPPCTI)
001352*            LENGTH (AUEPPCT-LENGTH)
001353*        END-EXEC
      *    MOVE '@"L                   #   #00004730' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AUEPPCTI, 
                 AUEPPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001354         IF AUEPPCTI NUMERIC
001355             MOVE AL-UNNON       TO  AUEPPCTA
001356             MOVE AUEPPCTI       TO  WS-IBNR-UEP-PCT
001357                                     AUEPPCTO
001358           ELSE
001359             MOVE AL-UNBON       TO  AUEPPCTA
001360             MOVE -1             TO  AUEPPCTL
001361             MOVE ER-0106        TO  EMI-ERROR
001362             PERFORM 9900-ERROR-FORMAT.
001363
001364*    NOTE *******************************************************
001365*         *      EDIT THE IBNR UNEARNED RULE 78 PERCENT         *
001366*         *******************************************************.
001367     IF AR78PCTL GREATER ZERO
001368         
      * EXEC CICS BIF DEEDIT
001369*            FIELD  (AR78PCTI)
001370*            LENGTH (AR78PCT-LENGTH)
001371*        END-EXEC
      *    MOVE '@"L                   #   #00004748' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AR78PCTI, 
                 AR78PCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001372         IF AR78PCTI NUMERIC
001373             MOVE AL-UNNON       TO  AR78PCTA
001374             MOVE AR78PCTI       TO  WS-IBNR-R78-PCT
001375                                     AR78PCTO
001376           ELSE
001377             MOVE AL-UNBON       TO  AR78PCTA
001378             MOVE -1             TO  AR78PCTL
001379             MOVE ER-0106        TO  EMI-ERROR
001380             PERFORM 9900-ERROR-FORMAT.
001381
001382*    NOTE *******************************************************
001383*         *      EDIT THE IBNR UNEARNED PRORATA PERCENT         *
001384*         *******************************************************.
001385     IF APROPCTL GREATER ZERO
001386         
      * EXEC CICS BIF DEEDIT
001387*            FIELD  (APROPCTI)
001388*            LENGTH (APROPCT-LENGTH)
001389*        END-EXEC
      *    MOVE '@"L                   #   #00004766' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APROPCTI, 
                 APROPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001390         IF APROPCTI NUMERIC
001391             MOVE AL-UNNON       TO  APROPCTA
001392             MOVE APROPCTI       TO  WS-IBNR-PRO-PCT
001393                                     APROPCTO
001394           ELSE
001395             MOVE AL-UNBON       TO  APROPCTA
001396             MOVE -1             TO  APROPCTL
001397             MOVE ER-0106        TO  EMI-ERROR
001398             PERFORM 9900-ERROR-FORMAT.
001399
001400*    NOTE *******************************************************
001401*         *      EDIT THE IBNR R78 AND PRO TOTAL PERCENT        *
001402*         *******************************************************.
001403     IF WS-IBNR-UEP-PCT NOT = ZEROS
001404         IF (WS-IBNR-R78-PCT + WS-IBNR-PRO-PCT) NOT = +1.0
001405             MOVE AL-UNBON       TO  AR78PCTA
001406                                     APROPCTA
001407             MOVE -1             TO  AR78PCTL
001408                                     APROPCTL
001409             MOVE ER-2308        TO  EMI-ERROR
001410             PERFORM 9900-ERROR-FORMAT.
001411
001412*    NOTE *******************************************************
001413*         *      EDIT THE LIMITS - QUOTED/CALCULATED AMOUNT.    *
001414*         *******************************************************.
001415     IF ALQCAL GREATER ZERO
001416         
      * EXEC CICS BIF DEEDIT
001417*            FIELD  (ALQCAI)
001418*            LENGTH (ALQCA-LENGTH)
001419*        END-EXEC
      *    MOVE '@"L                   #   #00004796' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCAI, 
                 ALQCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001420         IF ALQCAI NUMERIC
001421             MOVE AL-UNNON       TO  ALQCAA
001422             MOVE ALQCAI         TO  ALQCAO
001423           ELSE
001424             MOVE AL-UNBON       TO  ALQCAA
001425             MOVE -1             TO  ALQCAL
001426             MOVE ER-0118        TO EMI-ERROR
001427             PERFORM 9900-ERROR-FORMAT.
001428
001429*    NOTE *******************************************************
001430*         *      EDIT THE LIMITS - MAXIMUM REGULAR PAYMENT.     *
001431*         *******************************************************.
001432     IF ALMRPL GREATER ZERO
001433         
      * EXEC CICS BIF DEEDIT
001434*            FIELD  (ALMRPI)
001435*            LENGTH (ALMRP-LENGTH)
001436*        END-EXEC
      *    MOVE '@"L                   #   #00004813' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMRPI, 
                 ALMRP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001437         IF ALMRPI NUMERIC
001438             MOVE AL-UNNON       TO  ALMRPA
001439             MOVE ALMRPI         TO  ALMRPO
001440           ELSE
001441             MOVE AL-UNBON       TO  ALMRPA
001442             MOVE -1             TO  ALMRPL
001443             MOVE ER-0119        TO EMI-ERROR
001444             PERFORM 9900-ERROR-FORMAT.
001445
001446*    NOTE *******************************************************
001447*         *      EDIT THE LIMITS - QUOTED/CALCULATION DAYS.     *
001448*         *******************************************************.
001449     IF ALQCDL GREATER ZERO
001450         
      * EXEC CICS BIF DEEDIT
001451*            FIELD  (ALQCDI)
001452*            LENGTH (ALQCD-LENGTH)
001453*        END-EXEC
      *    MOVE '@"L                   #   #00004830' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCDI, 
                 ALQCD-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001454         IF ALQCDI NUMERIC
001455             MOVE AL-UNNON       TO  ALQCDA
001456             MOVE ALQCDI         TO  ALQCDO
001457           ELSE
001458             MOVE AL-UNBON       TO  ALQCDA
001459             MOVE -1             TO  ALQCDL
001460             MOVE ER-0120        TO EMI-ERROR
001461             PERFORM 9900-ERROR-FORMAT.
001462
001463*    NOTE *******************************************************
001464*         *      EDIT THE LIMITS - MAXIMUM DAYS PER PAYMENT.    *
001465*         *******************************************************.
001466     IF ALMDPPL GREATER ZERO
001467         
      * EXEC CICS BIF DEEDIT
001468*            FIELD  (ALMDPPI)
001469*            LENGTH (ALMDPP-LENGTH)
001470*        END-EXEC
      *    MOVE '@"L                   #   #00004847' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMDPPI, 
                 ALMDPP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001471         IF ALMDPPI NUMERIC
001472             MOVE AL-UNNON       TO  ALMDPPA
001473             MOVE ALMDPPI        TO  ALMDPPO
001474           ELSE
001475             MOVE AL-UNBON       TO  ALMDPPA
001476             MOVE -1             TO  ALMDPPL
001477             MOVE ER-0121        TO EMI-ERROR
001478             PERFORM 9900-ERROR-FORMAT.
001479
001480*    NOTE *******************************************************
001481*         *      EDIT THE LIMITS - DAYS BEFORE CLOSED.          *
001482*         *******************************************************.
001483     IF ALDBCL GREATER ZERO
001484         
      * EXEC CICS BIF DEEDIT
001485*            FIELD  (ALDBCI)
001486*            LENGTH (ALDBC-LENGTH)
001487*        END-EXEC
      *    MOVE '@"L                   #   #00004864' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALDBCI, 
                 ALDBC-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001488         IF ALDBCI NUMERIC
001489             MOVE AL-UNNON       TO  ALDBCA
001490             MOVE ALDBCI         TO  ALDBCO
001491           ELSE
001492             MOVE AL-UNBON       TO  ALDBCA
001493             MOVE -1             TO  ALDBCL
001494             MOVE ER-0122        TO EMI-ERROR
001495             PERFORM 9900-ERROR-FORMAT.
001496
001497*    NOTE *******************************************************
001498*         *      EDIT THE LIMITS - MAXIMUM AUTOMATIC PAYMENT.   *
001499*         *******************************************************.
001500     IF ALMAPL GREATER ZERO
001501         
      * EXEC CICS BIF DEEDIT
001502*            FIELD  (ALMAPI)
001503*            LENGTH (ALMAP-LENGTH)
001504*        END-EXEC
      *    MOVE '@"L                   #   #00004881' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPI, 
                 ALMAP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001505         IF ALMAPI NUMERIC
001506             MOVE AL-UNNON       TO  ALMAPA
001507             MOVE ALMAPI         TO  ALMAPO
001508           ELSE
001509             MOVE AL-UNBON       TO  ALMAPA
001510             MOVE -1             TO  ALMAPL
001511             MOVE ER-0123            TO EMI-ERROR
001512             PERFORM 9900-ERROR-FORMAT.
001513
001514*    NOTE *******************************************************
001515*         *      EDIT THE LIMITS - MONTHS BEFORE PURGED.        *
001516*         *******************************************************.
001517     IF ALMBPL GREATER ZERO
001518         
      * EXEC CICS BIF DEEDIT
001519*            FIELD  (ALMBPI)
001520*            LENGTH (ALMBP-LENGTH)
001521*        END-EXEC
      *    MOVE '@"L                   #   #00004898' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMBPI, 
                 ALMBP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001522         IF ALMBPI NUMERIC
001523             MOVE AL-UNNON       TO  ALMBPA
001524             MOVE ALMBPI         TO  ALMBPO
001525           ELSE
001526             MOVE AL-UNBON       TO  ALMBPA
001527             MOVE -1             TO  ALMBPL
001528             MOVE ER-0124        TO EMI-ERROR
001529             PERFORM 9900-ERROR-FORMAT.
001530
001531*    NOTE *******************************************************
001532*         *      EDIT THE LIMITS - MAXIMUM AUTO-PAY MONTHS.     *
001533*         *******************************************************.
001534     IF ALMAPML GREATER ZERO
001535         
      * EXEC CICS BIF DEEDIT
001536*            FIELD  (ALMAPMI)
001537*            LENGTH (ALMAPM-LENGTH)
001538*        END-EXEC
      *    MOVE '@"L                   #   #00004915' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034393135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPMI, 
                 ALMAPM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001539         IF ALMAPMI NUMERIC
001540             MOVE AL-UNNON       TO  ALMAPMA
001541             MOVE ALMAPMI        TO  ALMAPMO
001542           ELSE
001543             MOVE AL-UNBON       TO  ALMAPMA
001544             MOVE -1             TO  ALMAPML
001545             MOVE ER-0125        TO EMI-ERROR
001546             PERFORM 9900-ERROR-FORMAT.
001547
001548 4900-EXIT.
001549     EXIT.
001550
001551     EJECT
001552 5000-ADD-RECORD SECTION.
001553     
      * EXEC CICS GETMAIN
001554*        SET     (ADDRESS OF CONTROL-FILE)
001555*        LENGTH  (750)
001556*        INITIMG (GETMAIN-SPACE)
001557*    END-EXEC.
           MOVE 750
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004933' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034393333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001558
001559     MOVE 'CF'                   TO  CF-RECORD-ID.
001560
001561     MOVE PI-COMPANY-ID          TO  CF-COMPANY-ID.
001562     MOVE '6'                    TO  CF-RECORD-TYPE.
001563     MOVE PI-CARRIER-NUMBER      TO  CF-CARRIER-CNTL.
001564     MOVE ZERO                   TO  CF-SEQUENCE-NO.
001565
001566     MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.
001567     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001568     MOVE '5'                    TO  DC-OPTION-CODE.
001569     PERFORM 8500-DATE-CONVERSION.
001570     MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.
001571     MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.
001572
001573     MOVE ZERO                   TO  CF-ZIP-CODE
001574                                     CF-ZIP-CODE-NUM
001575                                     CF-PHONE-NO
001576                                     CF-CLAIM-COUNTER
001577                                     CF-CHECK-COUNTER
001578                                     CF-EXPENSE-PERCENT
001579                                     CF-EXPENSE-DOLLAR
001580                                     CF-PERCENT-OF-CDT
001581                                     CF-IBNR-PERCENT
001582                                     CF-IBNR-UEPRM-PERCENT
001583                                     CF-IBNR-R78-PERCENT
001584                                     CF-IBNR-PRO-PERCENT
001585                                     CF-CALC-AMT-TOL
001586                                     CF-MAX-REG-PMT
001587                                     CF-MAX-REG-DAYS
001588                                     CF-MAX-AUTO-PMT
001589                                     CF-MAX-AUTO-MOS
001590                                     CF-CALC-DAYS-TOL
001591                                     CF-DAYS-BEFORE-CLOSED
001592                                     CF-MONTHS-BEFORE-PURGED
001593                                     CF-CR-TOL-PREM
001594                                     CF-CR-TOL-REFUND
001595                                     CF-CR-TOL-PREM-PCT
001596                                     CF-CR-TOL-REFUND-PCT
001597                                     CF-CARRIER-CLP-TOL-PCT
001598                                     CF-CARRIER-LEASE-COMM
001599                                     CF-CARRIER-NEXT-AUDIT-CHK-NO
001600
001601     MOVE '1'                    TO  CF-CLAIM-NO-METHOD
001602                                     CF-CHECK-NO-METHOD
001603                                     CF-EXPENSE-METHOD
001604                                     CF-CDT-ACCESS-METHOD
001605                                     CF-CLAIM-CALC-METHOD.
001606
001607     PERFORM 5900-MOVE-MAP-DATA.
001608
001609     MOVE 'A'                    TO  JP-RECORD-TYPE.
001610     MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
001611
001612     
      * EXEC CICS WRITE
001613*        DATASET (WS-CONTROL-FILE-DSID)
001614*        RIDFLD  (CF-CONTROL-PRIMARY)
001615*        FROM    (CONTROL-FILE)
001616*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004992' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034393932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001617
001618     PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
001619
001620 5090-EXIT.
001621     EXIT.
001622
001623     EJECT
001624 5100-CHANGE-RECORD SECTION.
001625     MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
001626
001627     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
001628     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
001629     MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
001630     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
001631
001632     
      * EXEC CICS READ UPDATE
001633*        DATASET (WS-CONTROL-FILE-DSID)
001634*        RIDFLD  (WS-CONTROL-FILE-KEY)
001635*        SET     (ADDRESS OF CONTROL-FILE)
001636*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005012' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035303132' TO DFHEIV0
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
           
001637
001638     MOVE 'B'                    TO  JP-RECORD-TYPE.
001639     MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
001640
001641     PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
001642
001643     PERFORM 5900-MOVE-MAP-DATA.
001644
001645     MOVE 'C'                    TO  JP-RECORD-TYPE.
001646     MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
001647
001648     MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.
001649     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001650     MOVE '5'                    TO  DC-OPTION-CODE.
001651     PERFORM 8500-DATE-CONVERSION.
001652     MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.
001653     MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.
001654
001655     
      * EXEC CICS REWRITE
001656*        DATASET (WS-CONTROL-FILE-DSID)
001657*        FROM    (CONTROL-FILE)
001658*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005035' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035303335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001659
001660     PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
001661
001662 5190-EXIT.
001663     EXIT.
001664
001665     EJECT
001666 5200-DELETE-RECORD SECTION.
001667     MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
001668
001669     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
001670     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
001671     MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
001672     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
001673
001674     
      * EXEC CICS READ UPDATE
001675*        DATASET (WS-CONTROL-FILE-DSID)
001676*        RIDFLD  (WS-CONTROL-FILE-KEY)
001677*        SET     (ADDRESS OF CONTROL-FILE)
001678*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005054' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035303534' TO DFHEIV0
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
           
001679
001680     MOVE 'D'                    TO  JP-RECORD-TYPE.
001681     MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
001682
001683     
      * EXEC CICS DELETE
001684*        DATASET (WS-CONTROL-FILE-DSID)
001685*    END-EXEC.
      *    MOVE '&(                    &   #00005063' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303035303633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001686
001687     PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
001688
001689 5290-EXIT.
001690     EXIT.
001691
001692     EJECT
001693 5900-MOVE-MAP-DATA SECTION.
001694
001695     IF NOT CREDIT-SESSION
001696         GO TO 5910-NOT-CREDIT-SESSION.
001697
001698     IF BSECPAYL GREATER ZERO
001699         MOVE BSECPAYI       TO  CF-SECPAY-SWITCH
001700     END-IF.
001701
001702
001703     IF BCONAMEL GREATER ZERO
001704         MOVE BCONAMEI           TO  CF-MAIL-TO-NAME.
001705
001706     IF BCAREOFL GREATER ZERO
001707         MOVE BCAREOFI           TO  CF-IN-CARE-OF.
001708
001709     IF BADDR1L GREATER ZERO
001710         MOVE BADDR1I            TO  CF-ADDRESS-LINE-1.
001711
001712     IF BADDR2L GREATER ZERO
001713         MOVE BADDR2I            TO  CF-ADDRESS-LINE-2.
001714
001715     IF BCITYSTL GREATER ZERO
001716         MOVE BCITYSTI           TO  CF-CITY-STATE.
001717
001718     IF BPHONEL GREATER ZERO
001719         
      * EXEC CICS BIF DEEDIT
001720*            FIELD  (BPHONEI)
001721*            LENGTH (APHONE-LENGTH)
001722*        END-EXEC
      *    MOVE '@"L                   #   #00005099' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035303939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001723         MOVE BPHONEI            TO  CF-PHONE-NO
001724                                     BPHONEO
001725         INSPECT BPHONEO CONVERTING SPACES TO '-'.
001726
001727     IF CF-ZIP-CODE-NUM NUMERIC  AND
001728        CF-ZIP-CODE-NUM NOT = ZEROS
001729         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
001730         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
001731
001732     IF BZIPL NOT = ZERO
001733         MOVE BZIPI              TO  WS-ZIP-CODE
001734         MOVE ZEROS              TO  CF-ZIP-CODE-NUM
001735     ELSE
001736         GO TO 5905-CONTINUE.
001737
001738     IF WS-CANADIAN-ZIP
001739         IF WS-ZIP-4 = SPACE  OR  '-'
001740             MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1
001741             MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2
001742         ELSE
001743             MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1
001744             MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2
001745     ELSE
001746         IF WS-ZIP-6 = SPACE  OR  '-'
001747             MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME
001748             MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4
001749         ELSE
001750             MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME
001751             MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.
001752
001753 5905-CONTINUE.
001754
001755     IF BPRMTOLL GREATER ZERO
001756         
      * EXEC CICS BIF DEEDIT
001757*            FIELD  (BPRMTOLI)
001758*            LENGTH (6)
001759*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005136' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001760         MOVE BPRMTOLI           TO  CF-CR-TOL-PREM.
001761
001762     IF BREFTOLL GREATER ZERO
001763         
      * EXEC CICS BIF DEEDIT
001764*            FIELD  (BREFTOLI)
001765*            LENGTH (6)
001766*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005143' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001767         MOVE BREFTOLI           TO  CF-CR-TOL-REFUND.
001768
001769     IF BOVSAMTL GREATER ZERO
001770         
      * EXEC CICS BIF DEEDIT
001771*            FIELD  (BOVSAMTI)
001772*            LENGTH (6)
001773*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005150' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BOVSAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001774         MOVE BOVSAMTI           TO  CF-CR-OVR-SHT-AMT
001775     END-IF.
001776
001777     IF BPRMPCTL GREATER ZERO
001778         MOVE WS-TOL-PREM-PCT    TO  CF-CR-TOL-PREM-PCT.
001779
001780     IF BREFPCTL GREATER ZERO
001781         MOVE WS-TOL-REF-PCT     TO  CF-CR-TOL-REFUND-PCT.
001782
001783     IF BOVSPCTL > 0
001784         MOVE WS-CR-OVR-SHT-PCT  TO  CF-CR-OVR-SHT-PCT
001785     END-IF.
001786
001787     IF BDOMSTL GREATER ZERO
001788         MOVE BDOMSTI            TO  CF-DOMICILE-STATE.
001789
001790     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
001791         MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
001792                                     CF-CARRIER-LEASE-COMM
001793     ELSE
001794         IF BCLPTOLL GREATER ZERO
001795             MOVE WS-CLP-TOL-PCT TO  CF-CARRIER-CLP-TOL-PCT
001796*        ELSE
001797*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
001798         END-IF
001799         IF BLCOMML > ZEROS
001800            MOVE WS-SPP-LEASE-COMM TO CF-CARRIER-LEASE-COMM
001801         END-IF
001802     END-IF
001803
001804*    NOTE *******************************************************
001805* DMD CUSTOM CODE        EDIT CALCULATE PREMIUM FLAG            *
001806*         *******************************************************.
001807     IF CREDIT-SESSION
001808      IF PI-COMPANY-ID NOT = 'DMD'
001809        MOVE 'Y'                 TO BCLCPRMI
001810        MOVE +1                  TO BCLCPRML.
001811
001812     IF CREDIT-SESSION
001813        IF BCLCPRMI = 'Y' OR 'N'
001814           MOVE BCLCPRMI       TO CF-RATING-SWITCH
001815        ELSE
001816           MOVE AL-UABON       TO BCLCPRMA
001817           MOVE -1             TO BCLCPRML
001818           MOVE ER-8017        TO EMI-ERROR
001819           PERFORM 9900-ERROR-FORMAT
001820           PERFORM 8200-SEND-DATAONLY
001821           GO TO 9100-RETURN-TRAN.
001822
001823     GO TO 5990-EXIT.
001824
001825 5910-NOT-CREDIT-SESSION.
001826
001827*    IF ASECPAYL GREATER ZERO
001828*        MOVE ASECPAYI           TO  CF-SECPAY-SWITCH.
001829
001830     IF ACONAMEL GREATER ZERO
001831         MOVE ACONAMEI           TO  CF-MAIL-TO-NAME.
001832
001833     IF ACAREOFL GREATER ZERO
001834         MOVE ACAREOFI           TO  CF-IN-CARE-OF.
001835
001836     IF AADDR1L GREATER ZERO
001837         MOVE AADDR1I            TO  CF-ADDRESS-LINE-1.
001838
001839     IF AADDR2L GREATER ZERO
001840         MOVE AADDR2I            TO  CF-ADDRESS-LINE-2.
001841
001842     IF ACITYSTL GREATER ZERO
001843         MOVE ACITYSTI           TO  CF-CITY-STATE.
001844
001845     IF APHONEL GREATER ZERO
001846         
      * EXEC CICS BIF DEEDIT
001847*            FIELD  (APHONEI)
001848*            LENGTH (APHONE-LENGTH)
001849*        END-EXEC
      *    MOVE '@"L                   #   #00005226' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035323236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001850         MOVE APHONEI            TO  CF-PHONE-NO
001851                                     APHONEO
001852         INSPECT APHONEO CONVERTING SPACES TO '-'.
001853
001854     IF CF-ZIP-CODE-NUM NUMERIC  AND
001855        CF-ZIP-CODE-NUM NOT = ZEROS
001856         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
001857         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
001858
001859     IF AZIPL NOT = ZERO
001860         MOVE AZIPI              TO  WS-ZIP-CODE
001861         MOVE ZEROS              TO  CF-ZIP-CODE-NUM
001862     ELSE
001863         GO TO 5915-CONTINUE.
001864
001865     IF WS-CANADIAN-ZIP
001866         IF WS-ZIP-4 = SPACE  OR  '-'
001867             MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1
001868             MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2
001869         ELSE
001870             MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1
001871             MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2
001872     ELSE
001873         IF WS-ZIP-6 = SPACE  OR  '-'
001874             MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME
001875             MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4
001876         ELSE
001877             MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME
001878             MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.
001879
001880 5915-CONTINUE.
001881
001882     IF ADOMSTL GREATER ZERO
001883         MOVE ADOMSTI            TO  CF-DOMICILE-STATE.
001884
001885*    IF PI-COMPANY-ID = 'CID'
001886*        MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
001887*    ELSE
001888*        IF ACLPTOLL GREATER ZERO
001889*            MOVE ACLPTOLI       TO  CF-CARRIER-CLP-TOL-PCT
001890*        ELSE
001891*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
001892*        END-IF
001893*    END-IF.
001894
001895     IF CLAIM-SESSION
001896        IF ALPHCHL GREATER ZERO
001897            IF ALPHCHI ALPHABETIC
001898               MOVE AL-UANON       TO ALPHCHA
001899               MOVE  ALPHCHI       TO CF-LAST-ALPHA-CHARACTER
001900            ELSE
001901               MOVE AL-UNBON       TO ALPHCHA
001902               MOVE -1             TO ALPHCHL
001903               MOVE ER-8128        TO EMI-ERROR
001904               PERFORM 9900-ERROR-FORMAT.
001905
001906     EJECT
001907     IF ACLNAML GREATER ZERO
001908         MOVE ACLNAMI            TO  CF-CLAIM-NO-METHOD.
001909
001910     IF ACKNAML GREATER ZERO
001911         MOVE ACKNAMI            TO  CF-CHECK-NO-METHOD.
001912
001913     IF ACLAIML GREATER ZERO
001914         
      * EXEC CICS BIF DEEDIT
001915*            FIELD  (ACLAIMI)
001916*            LENGTH (8)
001917*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005294' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035323934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACLAIMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001918         MOVE ACLAIMO            TO  CF-CLAIM-COUNTER.
001919
001920     IF ACHECKL GREATER ZERO
001921         
      * EXEC CICS BIF DEEDIT
001922*            FIELD  (ACHECKI)
001923*            LENGTH (8)
001924*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005301' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACHECKI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001925         MOVE ACHECKO            TO  CF-CHECK-COUNTER.
001926
001927     IF ALAL GREATER ZERO
001928         MOVE ALAI               TO  CF-LETTER-RESEND-OPT
001929         INSPECT CF-LETTER-RESEND-OPT CONVERTING 'YN' TO '1 '.
001930
001931     IF AEXPCML GREATER ZERO
001932         MOVE AEXPCMI            TO  CF-EXPENSE-METHOD.
001933
001934     IF AEXPCPL GREATER ZERO
001935         
      * EXEC CICS BIF DEEDIT
001936*            FIELD  (AEXPCPI)
001937*            LENGTH (AEXPCP-LENGTH)
001938*        END-EXEC
      *    MOVE '@"L                   #   #00005315' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCPI, 
                 AEXPCP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001939         MOVE AEXPCPI            TO  CF-EXPENSE-PERCENT
001940                                     AEXPCPO.
001941
001942     IF AEXPCAL GREATER ZERO
001943         
      * EXEC CICS BIF DEEDIT
001944*            FIELD  (AEXPCAI)
001945*            LENGTH (AEXPCA-LENGTH)
001946*        END-EXEC
      *    MOVE '@"L                   #   #00005323' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCAI, 
                 AEXPCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001947         MOVE AEXPCAI            TO  CF-EXPENSE-DOLLAR
001948                                     AEXPCAO.
001949     IF CLAIM-SESSION
001950        IF ABRETRL GREATER ZERO
001951           IF ABRETRI NUMERIC
001952              MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS
001953           ELSE
001954              MOVE AL-UNBON           TO ABRETRA
001955              MOVE -1                 TO ABRETRL
001956              MOVE ER-8127            TO EMI-ERROR
001957              PERFORM 9900-ERROR-FORMAT.
001958
001959     IF ACLCML GREATER ZERO
001960         MOVE ACLCMI             TO  CF-CLAIM-CALC-METHOD.
001961
001962     IF ARESMANL GREATER ZERO
001963         MOVE ARESMANI           TO  CF-MANUAL-SW
001964         INSPECT CF-MANUAL-SW CONVERTING 'YN' TO '1 '.
001965
001966     IF ARESCDTL GREATER ZERO
001967         MOVE ARESCDTI           TO  CF-FUTURE-SW
001968         INSPECT CF-FUTURE-SW CONVERTING 'YN' TO '1 '.
001969
001970     IF ARESIBNL GREATER ZERO
001971         MOVE ARESIBNI           TO  CF-IBNR-SW
001972         INSPECT CF-IBNR-SW CONVERTING 'YN' TO '1 '.
001973
001974     IF ARESPTCL GREATER ZERO
001975         MOVE ARESPTCI           TO  CF-PTC-SW
001976                                     CF-PTC-LF-SW
001977         INSPECT CF-PTC-SW CONVERTING 'YN' TO '1 '
001978         INSPECT CF-PTC-LF-SW CONVERTING 'YN' TO '1 '.
001979
001980     IF ACDTAL GREATER ZERO
001981         MOVE ACDTAI             TO  CF-CDT-ACCESS-METHOD.
001982
001983     IF APCTCDTL GREATER ZERO
001984         
      * EXEC CICS BIF DEEDIT
001985*            FIELD  (APCTCDTI)
001986*            LENGTH (APCTCDT-LENGTH)
001987*        END-EXEC
      *    MOVE '@"L                   #   #00005364' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APCTCDTI, 
                 APCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001988         MOVE APCTCDTI           TO  CF-PERCENT-OF-CDT
001989                                     APCTCDTO.
001990
001991     IF IBNRPCTL GREATER ZERO
001992         
      * EXEC CICS BIF DEEDIT
001993*            FIELD  (IBNRPCTI)
001994*            LENGTH (IBNRPCT-LENGTH)
001995*        END-EXEC
      *    MOVE '@"L                   #   #00005372' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRPCTI, 
                 IBNRPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001996         MOVE IBNRPCTI           TO  CF-IBNR-PERCENT
001997                                     IBNRPCTO.
001998
001999     IF AUEPPCTL GREATER ZERO
002000         
      * EXEC CICS BIF DEEDIT
002001*            FIELD  (AUEPPCTI)
002002*            LENGTH (AUEPPCT-LENGTH)
002003*        END-EXEC
      *    MOVE '@"L                   #   #00005380' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AUEPPCTI, 
                 AUEPPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002004         MOVE AUEPPCTI           TO  CF-IBNR-UEPRM-PERCENT
002005                                     AUEPPCTO.
002006
002007     IF AR78PCTL GREATER ZERO
002008         
      * EXEC CICS BIF DEEDIT
002009*            FIELD  (AR78PCTI)
002010*            LENGTH (AR78PCT-LENGTH)
002011*        END-EXEC
      *    MOVE '@"L                   #   #00005388' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AR78PCTI, 
                 AR78PCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002012         MOVE AR78PCTI           TO  CF-IBNR-R78-PERCENT
002013                                     AR78PCTO.
002014
002015     IF APROPCTL GREATER ZERO
002016         
      * EXEC CICS BIF DEEDIT
002017*            FIELD  (APROPCTI)
002018*            LENGTH (APROPCT-LENGTH)
002019*        END-EXEC
      *    MOVE '@"L                   #   #00005396' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035333936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APROPCTI, 
                 APROPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002020         MOVE APROPCTI           TO  CF-IBNR-PRO-PERCENT
002021                                     APROPCTO.
002022
002023     IF ALQCAL GREATER ZERO
002024         
      * EXEC CICS BIF DEEDIT
002025*            FIELD  (ALQCAI)
002026*            LENGTH (ALQCA-LENGTH)
002027*        END-EXEC
      *    MOVE '@"L                   #   #00005404' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCAI, 
                 ALQCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002028         MOVE ALQCAI             TO  CF-CALC-AMT-TOL
002029                                     ALQCAO.
002030
002031     IF ALMRPL GREATER ZERO
002032         
      * EXEC CICS BIF DEEDIT
002033*            FIELD  (ALMRPI)
002034*            LENGTH (ALMRP-LENGTH)
002035*        END-EXEC
      *    MOVE '@"L                   #   #00005412' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMRPI, 
                 ALMRP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002036         MOVE ALMRPI             TO  CF-MAX-REG-PMT
002037                                     ALMRPO.
002038
002039     IF ALQCDL GREATER ZERO
002040         
      * EXEC CICS BIF DEEDIT
002041*            FIELD  (ALQCDI)
002042*            LENGTH (ALQCD-LENGTH)
002043*        END-EXEC
      *    MOVE '@"L                   #   #00005420' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCDI, 
                 ALQCD-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002044         MOVE ALQCDI             TO  CF-CALC-DAYS-TOL
002045                                     ALQCDO.
002046     IF ALMDPPL GREATER ZERO
002047         
      * EXEC CICS BIF DEEDIT
002048*            FIELD  (ALMDPPI)
002049*            LENGTH (ALMDPP-LENGTH)
002050*        END-EXEC
      *    MOVE '@"L                   #   #00005427' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMDPPI, 
                 ALMDPP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002051         MOVE ALMDPPI            TO  CF-MAX-REG-DAYS
002052                                     ALMDPPO.
002053
002054     IF ALDBCL GREATER ZERO
002055         
      * EXEC CICS BIF DEEDIT
002056*            FIELD  (ALDBCI)
002057*            LENGTH (ALDBC-LENGTH)
002058*        END-EXEC
      *    MOVE '@"L                   #   #00005435' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALDBCI, 
                 ALDBC-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002059         MOVE ALDBCI             TO  CF-DAYS-BEFORE-CLOSED
002060                                     ALDBCO.
002061
002062     IF ALMAPL GREATER ZERO
002063         
      * EXEC CICS BIF DEEDIT
002064*            FIELD  (ALMAPI)
002065*            LENGTH (ALMAP-LENGTH)
002066*        END-EXEC
      *    MOVE '@"L                   #   #00005443' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPI, 
                 ALMAP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002067         MOVE ALMAPI             TO  CF-MAX-AUTO-PMT
002068                                     ALMAPO.
002069
002070     IF ALMBPL GREATER ZERO
002071         
      * EXEC CICS BIF DEEDIT
002072*            FIELD  (ALMBPI)
002073*            LENGTH (ALMBP-LENGTH)
002074*        END-EXEC
      *    MOVE '@"L                   #   #00005451' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMBPI, 
                 ALMBP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002075         MOVE ALMBPI             TO  CF-MONTHS-BEFORE-PURGED
002076                                     ALMBPO.
002077
002078     IF ALMAPML GREATER ZERO
002079         
      * EXEC CICS BIF DEEDIT
002080*            FIELD  (ALMAPMI)
002081*            LENGTH (ALMAPM-LENGTH)
002082*        END-EXEC
      *    MOVE '@"L                   #   #00005459' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPMI, 
                 ALMAPM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002083         MOVE ALMAPMI            TO  CF-MAX-AUTO-MOS
002084                                     ALMAPMO.
002085
002086 5990-EXIT.
002087     EXIT.
002088 EJECT
002089 6000-SHOW-RECORD SECTION.
002090     
      * EXEC CICS HANDLE CONDITION
002091*        NOTFND  (6060-NOT-FOUND)
002092*    END-EXEC.
      *    MOVE '"$I                   ! # #00005470' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303035343730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002093
002094     MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
002095
002096     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
002097     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
002098     MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
002099     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
002100
002101     
      * EXEC CICS READ
002102*        DATASET (WS-CONTROL-FILE-DSID)
002103*        RIDFLD  (WS-CONTROL-FILE-KEY)
002104*        SET     (ADDRESS OF CONTROL-FILE)
002105*    END-EXEC.
      *    MOVE '&"S        E          (   #00005481' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035343831' TO DFHEIV0
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
           
002106
002107     IF NOT CREDIT-SESSION
002108         GO TO 6010-NOT-CREDIT-SESSION.
002109
002110     MOVE CF-CARRIER-CNTL        TO  BCARIERO.
002111     MOVE AL-UANON               TO  BCARIERA.
002112     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
002113         MOVE AL-SADOF           TO  BSPLABLA
002114         MOVE AL-SADOF           TO  BSECPAYA
002115     ELSE
002116         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
002117     END-IF.
002118
002119     MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.
002120     MOVE CF-IN-CARE-OF          TO  BCAREOFO.
002121     MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.
002122     MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.
002123     MOVE CF-CITY-STATE          TO  BCITYSTO.
002124
002125     IF CREDIT-SESSION
002126        MOVE CF-RATING-SWITCH    TO  BCLCPRMO.
002127
002128     IF CF-ZIP-CODE-NUM NUMERIC  AND
002129        CF-ZIP-CODE-NUM NOT = ZEROS
002130         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
002131         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
002132
002133     MOVE SPACES                   TO WS-ZIP-CODE.
002134     IF CF-CANADIAN-POST-CODE
002135         MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
002136         MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
002137     ELSE
002138         MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
002139         IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
002140             MOVE '-'              TO WS-ZIP-AM-2-DASH
002141             MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
002142
002143     MOVE WS-ZIP-CODE            TO  BZIPO.
002144
002145     MOVE CF-DOMICILE-STATE      TO  BDOMSTO.
002146     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
002147         MOVE AL-SADOF               TO  BCTLABLA
002148                                         BCLPTOLA
002149                                         BLCOMMA
002150     ELSE
002151         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
002152         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
002153     END-IF.
002154     MOVE CF-PHONE-NO            TO  BPHONEO.
002155     INSPECT BPHONEO CONVERTING SPACES TO '-'.
002156
002157     IF CF-CR-TOL-PREM NUMERIC
002158         IF CF-CR-TOL-PREM NOT = ZEROS
002159             MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.
002160
002161     IF CF-CR-TOL-REFUND NUMERIC
002162         IF CF-CR-TOL-REFUND NOT = ZEROS
002163             MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.
002164
002165     IF CF-CR-OVR-SHT-AMT  NUMERIC
002166          IF CF-CR-OVR-SHT-AMT > +0
002167              MOVE CF-CR-OVR-SHT-AMT TO BOVSAMTO
002168          END-IF
002169     END-IF.
002170
002171     IF CF-CR-TOL-PREM-PCT NUMERIC
002172         IF CF-CR-TOL-PREM-PCT NOT = ZEROS
002173             MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.
002174
002175     IF CF-CR-TOL-REFUND-PCT NUMERIC
002176         IF CF-CR-TOL-REFUND-PCT NOT = ZEROS
002177             MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.
002178
002179     IF CF-CR-OVR-SHT-PCT  NUMERIC
002180          IF CF-CR-OVR-SHT-PCT > +0
002181             MOVE CF-CR-OVR-SHT-PCT TO  BOVSPCTO
002182          END-IF
002183     END-IF.
002184
002185     IF CREDIT-SESSION
002186       MOVE CF-RATING-SWITCH            TO  BCLCPRMO.
002187
002188     MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA
002189                                            BPRMPCTA  BREFPCTA.
002190
002191     GO TO 6030-DISPLAY-MAINT.
002192
002193 6010-NOT-CREDIT-SESSION.
002194     MOVE CF-CARRIER-CNTL        TO  ACARIERO.
002195     MOVE AL-UANON               TO  ACARIERA.
002196*    IF PI-COMPANY-ID = 'CID'
002197*        MOVE AL-SADOF           TO  ASPLABLA
002198*        MOVE AL-SADOF           TO  ASECPAYA
002199*    ELSE
002200*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
002201*    END-IF.
002202     MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.
002203     MOVE CF-IN-CARE-OF          TO  ACAREOFO.
002204     MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.
002205     MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.
002206     MOVE CF-CITY-STATE          TO  ACITYSTO.
002207
002208     IF CF-ZIP-CODE-NUM NUMERIC  AND
002209        CF-ZIP-CODE-NUM NOT = ZEROS
002210         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
002211         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
002212
002213     MOVE SPACES                   TO WS-ZIP-CODE.
002214     IF CF-CANADIAN-POST-CODE
002215         MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
002216         MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
002217     ELSE
002218         MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
002219         IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
002220             MOVE '-'              TO WS-ZIP-AM-2-DASH
002221             MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
002222
002223     MOVE WS-ZIP-CODE            TO  AZIPO.
002224     MOVE CF-DOMICILE-STATE      TO  ADOMSTO.
002225*    IF PI-COMPANY-ID = 'CID'
002226*        MOVE AL-SADOF               TO  ACTLABLA
002227*        MOVE AL-SADOF               TO  ACLPTOLA
002228*    ELSE
002229*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
002230*    END-IF.
002231
002232     IF CLAIM-SESSION
002233        MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.
002234
002235     MOVE CF-PHONE-NO            TO  APHONEO.
002236*    INSPECT APHONEI CONVERTING SPACES TO '-'.
002237     INSPECT APHONEO CONVERTING SPACES TO '-'.
002238     EJECT
002239
002240     MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.
002241     MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.
002242
002243     IF PI-PROCESSOR-ID = 'LGXX'
002244         MOVE AL-UNNOF           TO  ACLAIMA.
002245
002246     MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.
002247
002248     MOVE CF-CHECK-COUNTER       TO  ACHECKO.
002249
002250     IF PI-PROCESSOR-ID = 'LGXX'
002251         MOVE AL-UNNOF           TO  ACHECKA.
002252
002253     MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.
002254     MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.
002255     MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.
002256
002257     IF CLAIM-SESSION
002258        MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.
002259
002260     MOVE CF-LETTER-RESEND-OPT   TO  ALAO.
002261     INSPECT ALAO CONVERTING ' 1' TO 'NY'.
002262
002263     MOVE CF-MANUAL-SW           TO  ARESMANO.
002264     INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.
002265
002266     MOVE CF-FUTURE-SW           TO  ARESCDTO.
002267     INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.
002268
002269     MOVE CF-PTC-SW              TO  ARESPTCO.
002270     INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.
002271
002272     MOVE CF-IBNR-SW             TO  ARESIBNO.
002273     INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.
002274
002275     MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.
002276     MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.
002277     MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.
002278
002279     IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
002280         MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
002281     IF CF-IBNR-R78-PERCENT NOT NUMERIC
002282         MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
002283     IF CF-IBNR-PRO-PERCENT NOT NUMERIC
002284         MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
002285
002286     MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.
002287     MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.
002288     MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.
002289
002290     MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.
002291
002292     MOVE CF-CALC-AMT-TOL        TO  ALQCAO.
002293     MOVE CF-MAX-REG-PMT         TO  ALMRPO.
002294     MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.
002295     MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.
002296     MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.
002297     MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.
002298
002299     MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.
002300     MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.
002301
002302     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
002303
002304 6030-DISPLAY-MAINT.
002305     MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
002306     MOVE SPACES                 TO  DC-OPTION-CODE.
002307     PERFORM 8500-DATE-CONVERSION.
002308
002309     IF CREDIT-SESSION
002310         MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO
002311         MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO
002312         INSPECT BLUTIMEI CONVERTING SPACES TO '.'
002313         MOVE CF-LAST-MAINT-BY       TO  BLUBYO
002314         MOVE -1                     TO  BMAINTL
002315     ELSE
002316         MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO
002317         MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO
002318         INSPECT ALUTIMEI CONVERTING SPACES TO '.'
002319         MOVE CF-LAST-MAINT-BY       TO  ALUBYO
002320         MOVE -1                     TO  AMAINTL.
002321
002322     MOVE +1                     TO  PI-BROWSE-SW.
002323     ADD  +1                     TO  WS-CFK-SEQUENCE-NO.
002324
002325     PERFORM 8000-DISPLAY-RECORDS THRU 8010-DISPLAY-RECORDS.
002326
002327     
      * EXEC CICS ENDBR
002328*        DATASET (WS-CONTROL-FILE-DSID)
002329*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005707' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035373037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002330
002331     MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.
002332
002333     PERFORM 8100-SEND-INITIAL-MAP.
002334     GO TO 9100-RETURN-TRAN.
002335
002336 6060-NOT-FOUND.
002337     MOVE AL-UNBON               TO  ACARIERA.
002338     MOVE -1                     TO  ACARIERL.
002339     MOVE ER-0006                TO  EMI-ERROR.
002340     PERFORM 9900-ERROR-FORMAT.
002341
002342     PERFORM 8200-SEND-DATAONLY.
002343     GO TO 9100-RETURN-TRAN.
002344
002345     EJECT
002346
002347 8000-DISPLAY-RECORDS SECTION.
002348     
      * EXEC CICS HANDLE CONDITION
002349*        NOTFND  (8060-DISPLAY-RECORDS)
002350*        ENDFILE (8040-DISPLAY-RECORDS)
002351*    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005728' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303035373238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002352
002353     
      * EXEC CICS STARTBR
002354*         DATASET (WS-CONTROL-FILE-DSID)
002355*         RIDFLD  (WS-CONTROL-FILE-KEY)
002356*         GTEQ
002357*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005733' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035373333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002358
002359 8010-DISPLAY-RECORDS.
002360     
      * EXEC CICS READNEXT
002361*        DATASET (WS-CONTROL-FILE-DSID)
002362*        RIDFLD  (WS-CONTROL-FILE-KEY)
002363*        SET     (ADDRESS OF CONTROL-FILE)
002364*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005740' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035373430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002365
002366     IF WS-CFK-COMPANY-ID NOT = PI-COMPANY-ID
002367         GO TO 8040-DISPLAY-RECORDS.
002368
002369     IF CF-RECORD-TYPE NOT = '6'
002370         MOVE ZERO               TO  PI-1ST-TIME-SW
002371         MOVE SPACES             TO  PI-NEXT-CARRIER-NUMBER
002372         MOVE ER-0173            TO  EMI-ERROR
002373         PERFORM 9900-ERROR-FORMAT
002374         GO TO 8050-DISPLAY-RECORDS.
002375
002376 8015-DISPLAY-RECORDS.
002377     IF LCP-ONCTR-01 =  0
002378         ADD 1 TO LCP-ONCTR-01
002379        GO TO 8020-DISPLAY-RECORDS.
002380
002381     MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.
002382     MOVE +1                     TO  PI-BROWSE-SW.
002383     GO TO 8050-DISPLAY-RECORDS.
002384
002385 8020-DISPLAY-RECORDS.
002386     IF NOT CREDIT-SESSION
002387         GO TO 8025-NOT-CREDIT-SESSION.
002388
002389     MOVE LOW-VALUES             TO  EL105BO.
002390
002391     MOVE 'S'                    TO  BMAINTO.
002392     MOVE AL-UANON               TO  BMAINTA.
002393     MOVE -1                     TO  BMAINTL.
002394     MOVE CF-CARRIER-CNTL        TO  BCARIERO
002395                                     PI-CARRIER-NUMBER.
002396     MOVE AL-UANON               TO  BCARIERA.
002397     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
002398         MOVE AL-SADOF           TO  BSPLABLA
002399         MOVE AL-SADOF           TO  BSECPAYA
002400     ELSE
002401         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
002402     END-IF.
002403
002404     MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.
002405     MOVE CF-IN-CARE-OF          TO  BCAREOFO.
002406     MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.
002407     MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.
002408     MOVE CF-CITY-STATE          TO  BCITYSTO.
002409
002410     MOVE CF-RATING-SWITCH       TO  BCLCPRMO.
002411
002412     IF CF-ZIP-CODE-NUM NUMERIC  AND
002413        CF-ZIP-CODE-NUM NOT = ZEROS
002414         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
002415         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
002416
002417     MOVE SPACES                   TO WS-ZIP-CODE.
002418     IF CF-CANADIAN-POST-CODE
002419         MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
002420         MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
002421     ELSE
002422         MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
002423         IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
002424             MOVE '-'              TO WS-ZIP-AM-2-DASH
002425             MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
002426
002427     MOVE WS-ZIP-CODE            TO  BZIPO.
002428
002429     MOVE CF-DOMICILE-STATE      TO  BDOMSTO.
002430     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
002431         MOVE AL-SADOF               TO  BCTLABLA
002432                                         BCLPTOLA
002433                                         BLCOMMA
002434     ELSE
002435         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
002436         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
002437     END-IF.
002438     MOVE CF-PHONE-NO            TO  BPHONEO.
002439     INSPECT BPHONEO CONVERTING SPACES TO '-'.
002440
002441     IF CF-CR-TOL-PREM NUMERIC
002442         IF CF-CR-TOL-PREM NOT = ZEROS
002443             MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.
002444
002445     IF CF-CR-TOL-REFUND NUMERIC
002446         IF CF-CR-TOL-REFUND NOT = ZEROS
002447             MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.
002448
002449     IF CF-CR-OVR-SHT-AMT  NUMERIC AND
002450            CF-CR-OVR-SHT-AMT  > 0
002451        MOVE CF-CR-OVR-SHT-AMT TO  BOVSAMTO
002452     END-IF.
002453
002454     IF CF-CR-TOL-PREM-PCT NUMERIC
002455         IF CF-CR-TOL-PREM-PCT NOT = ZEROS
002456             MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.
002457
002458     IF CF-CR-TOL-REFUND-PCT NUMERIC
002459         IF CF-CR-TOL-REFUND-PCT NOT = ZEROS
002460             MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.
002461
002462     IF CF-CR-OVR-SHT-PCT NUMERIC AND
002463          CF-CR-OVR-SHT-PCT > 0
002464             MOVE CF-CR-OVR-SHT-PCT TO BOVSPCTO
002465     END-IF.
002466
002467     MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA
002468                                            BPRMPCTA  BREFPCTA.
002469
002470     GO TO 8030-DISPLAY-MAINT.
002471
002472 8025-NOT-CREDIT-SESSION.
002473
002474     MOVE CF-CARRIER-CNTL        TO  ACARIERO.
002475     MOVE AL-UANON               TO  ACARIERA.
002476*    IF PI-COMPANY-ID = 'CID'
002477*        MOVE AL-SADOF           TO  ASPLABLA
002478*        MOVE AL-SADOF           TO  ASECPAYA
002479*    ELSE
002480*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
002481*    END-IF.
002482     MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.
002483     MOVE CF-IN-CARE-OF          TO  ACAREOFO.
002484     MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.
002485     MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.
002486     MOVE CF-CITY-STATE          TO  ACITYSTO.
002487
002488     IF CF-ZIP-CODE-NUM NUMERIC  AND
002489        CF-ZIP-CODE-NUM NOT = ZEROS
002490         MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
002491         MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
002492
002493     MOVE SPACES                   TO WS-ZIP-CODE.
002494     IF CF-CANADIAN-POST-CODE
002495         MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
002496         MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
002497     ELSE
002498         MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
002499         IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
002500             MOVE '-'              TO WS-ZIP-AM-2-DASH
002501             MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
002502
002503     MOVE WS-ZIP-CODE            TO  AZIPO.
002504     MOVE CF-DOMICILE-STATE      TO  ADOMSTO.
002505*    IF PI-COMPANY-ID = 'CID'
002506*        MOVE AL-SADOF               TO  ACTLABLA
002507*        MOVE AL-SADOF               TO  ACLPTOLA
002508*    ELSE
002509*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
002510*    END-IF.
002511
002512     IF CLAIM-SESSION
002513        MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.
002514
002515     MOVE CF-PHONE-NO            TO  APHONEO.
002516     INSPECT APHONEO CONVERTING SPACES TO '-'.
002517
002518     EJECT
002519
002520     MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.
002521     MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.
002522
002523     IF PI-PROCESSOR-ID = 'LGXX'
002524         MOVE AL-UNNOF           TO  ACLAIMA.
002525
002526     MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.
002527
002528     MOVE CF-CHECK-COUNTER       TO  ACHECKO.
002529
002530     IF PI-PROCESSOR-ID = 'LGXX'
002531         MOVE AL-UNNOF           TO  ACHECKA.
002532
002533     MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.
002534     MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.
002535     MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.
002536
002537     IF CLAIM-SESSION
002538        MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.
002539
002540     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
002541
002542     MOVE CF-LETTER-RESEND-OPT   TO  ALAO.
002543     INSPECT ALAO CONVERTING ' 1' TO 'NY'.
002544
002545     MOVE CF-MANUAL-SW           TO  ARESMANO.
002546     INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.
002547
002548     MOVE CF-FUTURE-SW           TO  ARESCDTO.
002549     INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.
002550
002551     MOVE CF-PTC-SW              TO  ARESPTCO.
002552     INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.
002553
002554     MOVE CF-IBNR-SW             TO  ARESIBNO.
002555     INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.
002556
002557     MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.
002558     MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.
002559     MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.
002560
002561     IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
002562         MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
002563     IF CF-IBNR-R78-PERCENT NOT NUMERIC
002564         MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
002565     IF CF-IBNR-PRO-PERCENT NOT NUMERIC
002566         MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
002567
002568     MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.
002569     MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.
002570     MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.
002571
002572     MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.
002573
002574     MOVE CF-CALC-AMT-TOL        TO  ALQCAO.
002575     MOVE CF-MAX-REG-PMT         TO  ALMRPO.
002576     MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.
002577     MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.
002578     MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.
002579     MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.
002580
002581     MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.
002582     MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.
002583
002584 8030-DISPLAY-MAINT.
002585     MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
002586     MOVE SPACES                 TO  DC-OPTION-CODE.
002587     PERFORM 8500-DATE-CONVERSION.
002588
002589     IF CREDIT-SESSION
002590         MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO
002591         MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO
002592         INSPECT BLUTIMEI CONVERTING SPACES TO '.'
002593         MOVE CF-LAST-MAINT-BY       TO  BLUBYO
002594     ELSE
002595         MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO
002596         MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO
002597         INSPECT ALUTIMEI CONVERTING SPACES TO '.'
002598         MOVE CF-LAST-MAINT-BY       TO  ALUBYO.
002599
002600     MOVE -1                     TO  AMAINTL.
002601
002602     GO TO 8010-DISPLAY-RECORDS.
002603
002604 8040-DISPLAY-RECORDS.
002605     MOVE +1                     TO  PI-END-OF-FILE.
002606     MOVE ER-0173                TO  EMI-ERROR.
002607     MOVE SPACES                 TO  PI-NEXT-CARRIER-NUMBER.
002608     PERFORM 9900-ERROR-FORMAT.
002609
002610 8050-DISPLAY-RECORDS.
002611     
      * EXEC CICS ENDBR
002612*        DATASET (WS-CONTROL-FILE-DSID)
002613*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005991' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035393931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002614
002615     PERFORM 8100-SEND-INITIAL-MAP.
002616     GO TO 9100-RETURN-TRAN.
002617
002618 8060-DISPLAY-RECORDS.
002619     MOVE AL-UNBON               TO  ACARIERA.
002620     MOVE -1                     TO  ACARIERL.
002621     MOVE ER-0006                TO  EMI-ERROR.
002622     PERFORM 9900-ERROR-FORMAT.
002623
002624     PERFORM 8200-SEND-DATAONLY.
002625     GO TO 9100-RETURN-TRAN.
002626
002627     EJECT
002628 8100-SEND-INITIAL-MAP SECTION.
002629     MOVE SAVE-DATE              TO  ADATEO.
002630     MOVE EIBTIME                TO  TIME-IN.
002631     MOVE TIME-OUT               TO  ATIMEO.
002632     MOVE -1                     TO  AMAINTL
002633
002634****DMD CUSTOM  CODE******************
002635     IF PI-COMPANY-ID = 'DMD'
002636     IF CREDIT-SESSION
002637        MOVE AL-SANOF           TO DMDSW2A
002638        MOVE AL-UANON           TO BCLCPRMA.
002639****DMD CUSTOM  CODE******************
002640
002641     EJECT
002642     IF CREDIT-SESSION
002643         MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O
002644     ELSE
002645         MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
002646
002647     IF CLAIM-SESSION
002648        MOVE 'ALPHA'             TO ALPHLO.
002649
002650     
      * EXEC CICS SEND
002651*        FROM   (EL105AI)
002652*        MAPSET (WS-MAPSET-NAME)
002653*        MAP    (WS-MAP-NAME)
002654*        CURSOR ERASE
002655*    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006030' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303036303330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
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
           
002656
002657 8100-EXIT.
002658     EXIT.
002659
002660     EJECT
002661 8200-SEND-DATAONLY SECTION.
002662     MOVE SAVE-DATE              TO  ADATEO.
002663     MOVE EIBTIME                TO  TIME-IN.
002664     MOVE TIME-OUT               TO  ATIMEO.
002665
002666     IF CREDIT-SESSION
002667         MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O
002668     ELSE
002669         MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
002670
002671****DMD CUSTOM  CODE******************
002672     IF PI-COMPANY-ID = 'DMD'
002673     IF CREDIT-SESSION
002674        MOVE AL-SANOF           TO DMDSW2A
002675        MOVE AL-UANON           TO BCLCPRMA.
002676****DMD CUSTOM  CODE******************
002677
002678     IF CLAIM-SESSION
002679        MOVE 'ALPHA'             TO ALPHLO.
002680
002681     
      * EXEC CICS SEND DATAONLY
002682*        FROM   (EL105AI)
002683*        MAPSET (WS-MAPSET-NAME)
002684*        MAP    (WS-MAP-NAME)
002685*        CURSOR
002686*    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006061' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303036303631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
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
           
002687
002688 8200-EXIT.
002689     EXIT.
002690
002691     EJECT
002692 8300-SEND-TEXT SECTION.
002693     
      * EXEC CICS SEND TEXT
002694*        FROM   (LOGOFF-TEXT)
002695*        LENGTH (LOGOFF-LENGTH)
002696*        ERASE  FREEKB
002697*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006073' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303036303733' TO DFHEIV0
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
           
002698
002699     
      * EXEC CICS RETURN
002700*    END-EXEC.
      *    MOVE '.(                    ''   #00006079' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036303739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002701
002702 8300-EXIT.
002703     EXIT.
002704
002705     EJECT
002706 8400-LOG-JOURNAL-RECORD SECTION.
002707     IF PI-JOURNAL-FILE-ID = ZERO
002708         GO TO 8400-EXIT.
002709
002710     MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
002711     MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.
002712     MOVE THIS-PGM               TO  JP-PROGRAM-ID.
002713
002714*    EXEC CICS JOURNAL
002715*        JFILEID (PI-JOURNAL-FILE-ID)
002716*        JTYPEID (WS-JOURNAL-TYPE-ID)
002717*        FROM    (JOURNAL-RECORD)
002718*        LENGTH  (WS-JOURNAL-RECORD-LENGTH)
002719*    END-EXEC.
002720
002721 8400-EXIT.
002722     EXIT.
002723
002724 8500-DATE-CONVERSION SECTION.
002725     
      * EXEC CICS LINK
002726*        PROGRAM  ('ELDATCV')
002727*        COMMAREA (DATE-CONVERSION-DATA)
002728*        LENGTH   (DC-COMM-LENGTH)
002729*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006105' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036313035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002730
002731 8500-EXIT.
002732     EXIT.
002733
002734     EJECT
002735 8700-NOT-OPEN SECTION.
002736     MOVE ER-0042                TO EMI-ERROR.
002737     MOVE -1                     TO ACARIERL.
002738     PERFORM 9900-ERROR-FORMAT.
002739     PERFORM 8200-SEND-DATAONLY.
002740     GO TO 9100-RETURN-TRAN.
002741
002742 8700-EXIT.
002743      EXIT.
002744
002745 8800-DUPREC SECTION.
002746     MOVE ER-0497                TO EMI-ERROR.
002747     MOVE -1                     TO ACARIERL.
002748     PERFORM 9900-ERROR-FORMAT.
002749     PERFORM 8200-SEND-DATAONLY.
002750     GO TO 9100-RETURN-TRAN.
002751
002752 8800-EXIT.
002753      EXIT.
002754
002755     EJECT
002756 9000-RETURN-CICS SECTION.
002757     MOVE 'EL005   '             TO  THIS-PGM.
002758     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
002759     PERFORM 9300-XCTL.
002760
002761 9000-EXIT.
002762     EXIT.
002763
002764 9100-RETURN-TRAN SECTION.
002765     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
002766     MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
002767
002768     
      * EXEC CICS RETURN
002769*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
002770*        LENGTH   (PI-COMM-LENGTH)
002771*        TRANSID  (WS-TRANS-ID)
002772*    END-EXEC.
      *    MOVE '.(CT                  ''   #00006148' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036313438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002773     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
002774
002775 9100-EXIT.
002776     EXIT.
002777
002778 9300-XCTL SECTION.
002779     MOVE DFHENTER               TO  EIBAID
002780
002781     
      * EXEC CICS XCTL
002782*        PROGRAM  (THIS-PGM)
002783*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
002784*        LENGTH   (PI-COMM-LENGTH)
002785*    END-EXEC.
      *    MOVE '.$C                   %   #00006161' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036313631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002786
002787 9300-EXIT.
002788     EXIT.
002789
002790     EJECT
002791 9400-CLEAR SECTION.
002792     MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
002793     PERFORM 9300-XCTL.
002794
002795 9400-EXIT.
002796     EXIT.
002797
002798 9600-PGMIDERR SECTION.
002799     
      * EXEC CICS HANDLE CONDITION
002800*        PGMIDERR (8300-SEND-TEXT)
002801*    END-EXEC.
      *    MOVE '"$L                   ! % #00006179' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303036313739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002802
002803     MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
002804                                     LOGOFF-PGM.
002805
002806     MOVE 'EL005   '             TO  THIS-PGM.
002807     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
002808     MOVE SPACES                 TO  PI-ENTRY-CD-1.
002809     PERFORM 9300-XCTL.
002810
002811 9600-EXIT.
002812     EXIT.
002813
002814
002815     EJECT
002816 9900-ERROR-FORMAT SECTION.
002817     
      * EXEC CICS LINK
002818*        PROGRAM  ('EL001')
002819*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
002820*        LENGTH   (EMI-COMM-LENGTH)
002821*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00006197' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036313937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002822
002823 9900-EXIT.
002824     EXIT.
002825
002826     EJECT
002827 9990-ERROR SECTION.
002828     MOVE DFHEIBLK               TO EMI-LINE1.
002829
002830     
      * EXEC CICS LINK
002831*        PROGRAM  ('EL004')
002832*        COMMAREA (EMI-LINE1)
002833*        LENGTH   (72)
002834*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006210' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036323130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002835
002836     PERFORM 8200-SEND-DATAONLY.
002837     GO TO 9100-RETURN-TRAN.
002838
002839 9990-EXIT.
002840     EXIT.
002841
002842 9995-SECURITY-VIOLATION.
002843*           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00006241' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036323431' TO DFHEIV0
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

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-OPEN,
                     8800-DUPREC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 6060-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8060-DISPLAY-RECORDS,
                     8040-DISPLAY-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
