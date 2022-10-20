      *((program: EL153.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL153 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 06/20/94 09:17:09.
000007*                            VMOD=2.019
000008*
000009*
000010*AUTHOR.    LOGIC, INC.
000011*           DALLAS, TEXAS.
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
000024
000025*REMARKS.
000026*    SCREENS     - EL153S - NOTES AND REMINDERS
000027
000028*    ENTERED BY  - EL150 - STATUS AND DISPOSITION
000029
000030*    EXIT TO     - EL150 - CALLING PROGRAM
000031
000032*    INPUT FILE  - ELMSTR - CLAIM MASTER
000033*                - ELTRLR - ACTIVITY TRAILERS
000034
000035*    OUTPUT FILE - ELMSTR - CLAIM MASTER
000036*                - ELTRLR - ACTIVITY TRAILERS
000037
000038*    COMMAREA    - PASSED CLAIM NUMBER FROM PROG INTERFACE BLK
000039
000040*    ERROR-CODES ACCESSED - 132, 314, 133, 137, 29, 50, 315,
000041*                            316, 317, 08
000042*    NARRATIVE   - PROVIDE CREATION OF AUTO-PROMPT-TRAILER
000043*                  AND GENERAL-INFO-TRAILER
000044*                  IF THE AUTO PROMPT DATE GIVEN  IS LOWER
000045*                  THAN THE NEXT-REMINDER DATE OF THE CLAIM MASTER
000046*                  THIS FIELD IS UPDATED IN THE CLAIM MASTER
000047
000048******************************************************************
000049*                   C H A N G E   L O G
000050*
000051* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000052*-----------------------------------------------------------------
000053*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000054* EFFECTIVE    NUMBER
000055*-----------------------------------------------------------------
000056* 031102    2002022100003  SMVA  ADD CERT# TO EL153A SCREEN HEADER
000057* 062602    2002030700006  PEMA  Add note type of 'S'
000058*                                  (special review)
000059* 080106    2006052500001  AJRA  ADD NOTE TYPE 'N'(NOTE AND FILE)
000060* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000061* 102418  CR2018083000001  TANA  ADD NEW CALL TYPE
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
000071
000072 77  FILLER  PIC X(32)  VALUE '********************************'.
000073 77  FILLER  PIC X(32)  VALUE '*   EL153  WORKING STORAGE     *'.
000074 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.019 *********'.
000075
000076*                                    COPY ELCSCTM.
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
000077
000078*                                    COPY ELCSCRTY.
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
000081     12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000082     12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000083     12  CURRENT-PLUS3-SAVE          PIC XX      VALUE SPACES.
000084
000085 01  WS-SCRATCH-AREA.
000086     12  GETMAIN-SPACE               PIC X       VALUE SPACE.
000087     12  WS-TRLR-LENGTH              PIC S9(4)   VALUE +200  COMP.
000088     12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
000089
000090     12  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
000091
000092     12  WS-CURSOR                   PIC S9(4)   VALUE -1    COMP.
000093
000094     12  WS-MAP-NAME                 PIC X(8)    VALUE 'EL153A'.
000095     12  WS-MAPSET-NAME              PIC X(8)    VALUE 'EL153S'.
000096
000097     12  WS-TRANS-ID                 PIC X(4)    VALUE 'EX26'.
000098     12  THIS-PGM                    PIC X(8)    VALUE  'EL153'.
000099
000100     12  TIME-OUT.
000101         16  WS-TRANS-HOUR           PIC XX      VALUE SPACE.
000102         16  FILLER                  PIC X       VALUE '.'.
000103         16  WS-TRANS-MINUTE         PIC XX      VALUE SPACE.
000104
000105     12  WS-DATE-UNEDIT.
000106         16  FILLER                  PIC XX.
000107         16  WS-DATE-MDY.
000108             20  WS-DATE-MM          PIC XX.
000109             20  WS-DATE-DD          PIC XX.
000110             20  WS-DATE-YY          PIC XX.
000111
000112     12  WS-STR-DATE-BIN             PIC XX      VALUE LOW-VALUES.
000113     12  WS-END-DATE-BIN             PIC XX      VALUE LOW-VALUES.
000114
000115     12  SUB                         PIC S99     VALUE ZEROS.
000116     12  SUB-1                       PIC S99     VALUE ZEROS.
000117
000118     12  WS-FIRST-TIME-SW            PIC X       VALUE 'Y'.
000119
000120     12  WS-EDIT-NOTE.
000121         16  WS-EDIT-NOTE-1-4.
000122             20  WS-EDIT-NOTE-1-3    PIC X(3).
000123             20  FILLER              PIC X.
000124         16  FILLER                  PIC X(56).
000125
000126     12  WS-DATE-ERROR-SW            PIC X       VALUE SPACE.
000127         88  DATE-ERROR                          VALUE 'X'.
000128
000129     12  W-NOTE-TYPE                 PIC X       VALUE SPACE.
000130     12  W-CALL-TYPE                 PIC X       VALUE SPACE.
000131     12  W-NOTE-TYPE-IND             PIC X       VALUE SPACE.
000132
000133     12  TIME-IN                     PIC S9(7).
000134     12  WS-TIME  REDEFINES TIME-IN.
000135         16  FILLER                  PIC 9.
000136         16  WS-HOUR                 PIC 99.
000137         16  WS-MINUTE               PIC 99.
000138         16  FILLER                  PIC 99.
000139
000140     12  WS-TRAILER-KEY.
000141         16  WS-CLAIM-KEY.
000142             20  WS-KEY-COMPANY-CD       PIC X.
000143             20  WS-KEY-CARRIER          PIC X.
000144             20  WS-KEY-CLAIM-NO         PIC X(7).
000145             20  WS-KEY-CERT-NO.
000146                 24  WS-KEY-CERT-PRIME   PIC X(10).
000147                 24  WS-KEY-CERT-SFX     PIC X.
000148         16  WS-KEY-SEQUENCE-NO          PIC S9(4) COMP.
000149
000150     EJECT
000151 01  ERROR-MESSAGES.
000152     12  ER-0000                     PIC X(4)    VALUE '0000'.
000153     12  ER-0004                     PIC X(4)    VALUE '0004'.
000154     12  ER-0008                     PIC X(4)    VALUE '0008'.
000155     12  ER-0029                     PIC X(4)    VALUE '0029'.
000156     12  ER-0050                     PIC X(4)    VALUE '0050'.
000157     12  ER-0070                     PIC X(4)    VALUE '0070'.
000158     12  ER-0132                     PIC X(4)    VALUE '0132'.
000159     12  ER-0133                     PIC X(4)    VALUE '0133'.
000160     12  ER-0137                     PIC X(4)    VALUE '0137'.
000161     12  ER-0154                     PIC X(4)    VALUE '0154'.
000162     12  ER-0172                     PIC X(4)    VALUE '0172'.
000163     12  ER-0314                     PIC X(4)    VALUE '0314'.
000164     12  ER-0316                     PIC X(4)    VALUE '0316'.
000165     12  ER-0317                     PIC X(4)    VALUE '0317'.
000166     12  ER-0483                     PIC X(4)    VALUE '0483'.
000167     12  ER-0694                     PIC X(4)    VALUE '0694'.
000168     12  ER-0914                     PIC X(4)    VALUE '0914'.
000169     12  ER-0915                     PIC X(4)    VALUE '0915'.
000170     12  ER-0916                     PIC X(4)    VALUE '0916'.
000171     12  ER-0917                     PIC X(4)    VALUE '0917'.
000172     12  ER-0924                     PIC X(4)    VALUE '0924'.
000173     12  ER-7840                     PIC X(4)    VALUE '7840'.
000174     12  ER-7846                     PIC X(4)    VALUE '7846'.
000175     EJECT
000176*                                    COPY ELCAID.
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
000177 01  PF-AID REDEFINES DFHAID.
000178     05  FILLER                      PIC X(8).
000179     05  PF-VALUES  OCCURS 24        PIC X.
000180     EJECT
000181*                                    COPY ELCINTF.
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
000182     EJECT
000183*                                    COPY ELCATTR.
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
000184     EJECT
000185*                                    COPY ELCLOGOF.
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
000186     EJECT
000187*                                    COPY ELCDATE.
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
000188     EJECT
000189*                                    COPY ELCEMIB.
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
000190     EJECT
000191*                                    COPY EL153S.
      *>>((file: EL153S))
000001 01  EL153AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  MRNDATEL PIC S9(0004) COMP.
000005     05  MRNDATEF PIC  X(0001).
000006     05  FILLER REDEFINES MRNDATEF.
000007         10  MRNDATEA PIC  X(0001).
000008     05  MRNDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  MRNTIMEL PIC S9(0004) COMP.
000011     05  MRNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES MRNTIMEF.
000013         10  MRNTIMEA PIC  X(0001).
000014     05  MRNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  MCERTL PIC S9(0004) COMP.
000017     05  MCERTF PIC  X(0001).
000018     05  FILLER REDEFINES MCERTF.
000019         10  MCERTA PIC  X(0001).
000020     05  MCERTI PIC  X(0010).
000021*    -------------------------------
000022     05  MSTRDTL PIC S9(0004) COMP.
000023     05  MSTRDTF PIC  X(0001).
000024     05  FILLER REDEFINES MSTRDTF.
000025         10  MSTRDTA PIC  X(0001).
000026     05  MSTRDTI PIC  X(0008).
000027*    -------------------------------
000028     05  MENDDTL PIC S9(0004) COMP.
000029     05  MENDDTF PIC  X(0001).
000030     05  FILLER REDEFINES MENDDTF.
000031         10  MENDDTA PIC  X(0001).
000032     05  MENDDTI PIC  X(0008).
000033*    -------------------------------
000034     05  CALLTPL PIC S9(0004) COMP.
000035     05  CALLTPF PIC  X(0001).
000036     05  FILLER REDEFINES CALLTPF.
000037         10  CALLTPA PIC  X(0001).
000038     05  CALLTPI PIC  X(0001).
000039*    -------------------------------
000040     05  NOTETPL PIC S9(0004) COMP.
000041     05  NOTETPF PIC  X(0001).
000042     05  FILLER REDEFINES NOTETPF.
000043         10  NOTETPA PIC  X(0001).
000044     05  NOTETPI PIC  X(0001).
000045*    -------------------------------
000046     05  MLINE1L PIC S9(0004) COMP.
000047     05  MLINE1F PIC  X(0001).
000048     05  FILLER REDEFINES MLINE1F.
000049         10  MLINE1A PIC  X(0001).
000050     05  MLINE1I PIC  X(0060).
000051*    -------------------------------
000052     05  MLINE2L PIC S9(0004) COMP.
000053     05  MLINE2F PIC  X(0001).
000054     05  FILLER REDEFINES MLINE2F.
000055         10  MLINE2A PIC  X(0001).
000056     05  MLINE2I PIC  X(0060).
000057*    -------------------------------
000058     05  MLINE3L PIC S9(0004) COMP.
000059     05  MLINE3F PIC  X(0001).
000060     05  FILLER REDEFINES MLINE3F.
000061         10  MLINE3A PIC  X(0001).
000062     05  MLINE3I PIC  X(0060).
000063*    -------------------------------
000064     05  MLINE4L PIC S9(0004) COMP.
000065     05  MLINE4F PIC  X(0001).
000066     05  FILLER REDEFINES MLINE4F.
000067         10  MLINE4A PIC  X(0001).
000068     05  MLINE4I PIC  X(0060).
000069*    -------------------------------
000070     05  MLINE5L PIC S9(0004) COMP.
000071     05  MLINE5F PIC  X(0001).
000072     05  FILLER REDEFINES MLINE5F.
000073         10  MLINE5A PIC  X(0001).
000074     05  MLINE5I PIC  X(0060).
000075*    -------------------------------
000076     05  MLINE6L PIC S9(0004) COMP.
000077     05  MLINE6F PIC  X(0001).
000078     05  FILLER REDEFINES MLINE6F.
000079         10  MLINE6A PIC  X(0001).
000080     05  MLINE6I PIC  X(0060).
000081*    -------------------------------
000082     05  MLINE7L PIC S9(0004) COMP.
000083     05  MLINE7F PIC  X(0001).
000084     05  FILLER REDEFINES MLINE7F.
000085         10  MLINE7A PIC  X(0001).
000086     05  MLINE7I PIC  X(0060).
000087*    -------------------------------
000088     05  MLINE8L PIC S9(0004) COMP.
000089     05  MLINE8F PIC  X(0001).
000090     05  FILLER REDEFINES MLINE8F.
000091         10  MLINE8A PIC  X(0001).
000092     05  MLINE8I PIC  X(0060).
000093*    -------------------------------
000094     05  MLINE9L PIC S9(0004) COMP.
000095     05  MLINE9F PIC  X(0001).
000096     05  FILLER REDEFINES MLINE9F.
000097         10  MLINE9A PIC  X(0001).
000098     05  MLINE9I PIC  X(0060).
000099*    -------------------------------
000100     05  MLINE10L PIC S9(0004) COMP.
000101     05  MLINE10F PIC  X(0001).
000102     05  FILLER REDEFINES MLINE10F.
000103         10  MLINE10A PIC  X(0001).
000104     05  MLINE10I PIC  X(0060).
000105*    -------------------------------
000106     05  MLINE11L PIC S9(0004) COMP.
000107     05  MLINE11F PIC  X(0001).
000108     05  FILLER REDEFINES MLINE11F.
000109         10  MLINE11A PIC  X(0001).
000110     05  MLINE11I PIC  X(0060).
000111*    -------------------------------
000112     05  MLINE12L PIC S9(0004) COMP.
000113     05  MLINE12F PIC  X(0001).
000114     05  FILLER REDEFINES MLINE12F.
000115         10  MLINE12A PIC  X(0001).
000116     05  MLINE12I PIC  X(0060).
000117*    -------------------------------
000118     05  MLINE13L PIC S9(0004) COMP.
000119     05  MLINE13F PIC  X(0001).
000120     05  FILLER REDEFINES MLINE13F.
000121         10  MLINE13A PIC  X(0001).
000122     05  MLINE13I PIC  X(0060).
000123*    -------------------------------
000124     05  MLINE14L PIC S9(0004) COMP.
000125     05  MLINE14F PIC  X(0001).
000126     05  FILLER REDEFINES MLINE14F.
000127         10  MLINE14A PIC  X(0001).
000128     05  MLINE14I PIC  X(0060).
000129*    -------------------------------
000130     05  MERMSG1L PIC S9(0004) COMP.
000131     05  MERMSG1F PIC  X(0001).
000132     05  FILLER REDEFINES MERMSG1F.
000133         10  MERMSG1A PIC  X(0001).
000134     05  MERMSG1I PIC  X(0079).
000135*    -------------------------------
000136     05  MERMSG2L PIC S9(0004) COMP.
000137     05  MERMSG2F PIC  X(0001).
000138     05  FILLER REDEFINES MERMSG2F.
000139         10  MERMSG2A PIC  X(0001).
000140     05  MERMSG2I PIC  X(0079).
000141*    -------------------------------
000142     05  MPFNUMBL PIC S9(0004) COMP.
000143     05  MPFNUMBF PIC  X(0001).
000144     05  FILLER REDEFINES MPFNUMBF.
000145         10  MPFNUMBA PIC  X(0001).
000146     05  MPFNUMBI PIC  99.
000147*    -------------------------------
000148     05  PFKEY4L PIC S9(0004) COMP.
000149     05  PFKEY4F PIC  X(0001).
000150     05  FILLER REDEFINES PFKEY4F.
000151         10  PFKEY4A PIC  X(0001).
000152     05  PFKEY4I PIC  X(0014).
000153 01  EL153AO REDEFINES EL153AI.
000154     05  FILLER            PIC  X(0012).
000155*    -------------------------------
000156     05  FILLER            PIC  X(0003).
000157     05  MRNDATEO PIC  X(0008).
000158*    -------------------------------
000159     05  FILLER            PIC  X(0003).
000160     05  MRNTIMEO PIC  99.99.
000161*    -------------------------------
000162     05  FILLER            PIC  X(0003).
000163     05  MCERTO PIC  X(0010).
000164*    -------------------------------
000165     05  FILLER            PIC  X(0003).
000166     05  MSTRDTO PIC  X(0008).
000167*    -------------------------------
000168     05  FILLER            PIC  X(0003).
000169     05  MENDDTO PIC  X(0008).
000170*    -------------------------------
000171     05  FILLER            PIC  X(0003).
000172     05  CALLTPO PIC  X(0001).
000173*    -------------------------------
000174     05  FILLER            PIC  X(0003).
000175     05  NOTETPO PIC  X(0001).
000176*    -------------------------------
000177     05  FILLER            PIC  X(0003).
000178     05  MLINE1O PIC  X(0060).
000179*    -------------------------------
000180     05  FILLER            PIC  X(0003).
000181     05  MLINE2O PIC  X(0060).
000182*    -------------------------------
000183     05  FILLER            PIC  X(0003).
000184     05  MLINE3O PIC  X(0060).
000185*    -------------------------------
000186     05  FILLER            PIC  X(0003).
000187     05  MLINE4O PIC  X(0060).
000188*    -------------------------------
000189     05  FILLER            PIC  X(0003).
000190     05  MLINE5O PIC  X(0060).
000191*    -------------------------------
000192     05  FILLER            PIC  X(0003).
000193     05  MLINE6O PIC  X(0060).
000194*    -------------------------------
000195     05  FILLER            PIC  X(0003).
000196     05  MLINE7O PIC  X(0060).
000197*    -------------------------------
000198     05  FILLER            PIC  X(0003).
000199     05  MLINE8O PIC  X(0060).
000200*    -------------------------------
000201     05  FILLER            PIC  X(0003).
000202     05  MLINE9O PIC  X(0060).
000203*    -------------------------------
000204     05  FILLER            PIC  X(0003).
000205     05  MLINE10O PIC  X(0060).
000206*    -------------------------------
000207     05  FILLER            PIC  X(0003).
000208     05  MLINE11O PIC  X(0060).
000209*    -------------------------------
000210     05  FILLER            PIC  X(0003).
000211     05  MLINE12O PIC  X(0060).
000212*    -------------------------------
000213     05  FILLER            PIC  X(0003).
000214     05  MLINE13O PIC  X(0060).
000215*    -------------------------------
000216     05  FILLER            PIC  X(0003).
000217     05  MLINE14O PIC  X(0060).
000218*    -------------------------------
000219     05  FILLER            PIC  X(0003).
000220     05  MERMSG1O PIC  X(0079).
000221*    -------------------------------
000222     05  FILLER            PIC  X(0003).
000223     05  MERMSG2O PIC  X(0079).
000224*    -------------------------------
000225     05  FILLER            PIC  X(0003).
000226     05  MPFNUMBO PIC  X(0002).
000227*    -------------------------------
000228     05  FILLER            PIC  X(0003).
000229     05  PFKEY4O PIC  X(0014).
000230*    -------------------------------
      *<<((file: EL153S))
000192 01  EL153AI-R REDEFINES EL153AI.
000193*    12  FILLER                      PIC X(70).
000194     12  FILLER                      PIC X(74).
000195     12  EL153AI-OCCURS OCCURS 14 TIMES.
000196         16  EL153A-NOTE-LENGTH      PIC S9(4)     COMP.
000197         16  EL153A-NOTE-ATTRB       PIC X.
000198         16  EL153A-NOTE             PIC X(60).
000199
000200     EJECT
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
000202
000203 01  DFHCOMMAREA                     PIC X(1024).
000204     EJECT
000205*                                    COPY ELCMSTR.
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
000206     EJECT
000207*                                    COPY ELCTRLR.
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
000208     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL153' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000209 VCOBOL-DUMMY-PROCEDURE.
000210
000211     MOVE EIBDATE               TO  DC-JULIAN-YYDDD.
000212     MOVE '5'                   TO  DC-OPTION-CODE.
000213     PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.
000214     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000215     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000216
000217     MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
000218
000219     IF EIBCALEN NOT GREATER THAN ZEROS
000220       GO TO 9000-UNAUTHERR.
000221
000222     IF PI-COMPANY-ID = 'DMD'
000223         MOVE +3                TO  DC-ELAPSED-MONTHS
000224         MOVE +0                TO  DC-ELAPSED-DAYS
000225         MOVE '6'               TO  DC-OPTION-CODE
000226         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
000227         MOVE DC-BIN-DATE-2     TO  CURRENT-PLUS3-SAVE.
000228
000229     
      * EXEC CICS  HANDLE CONDITION
000230*           ERROR    (9990-ERROR)
000231*           PGMIDERR (9600-PGMIDERR)
000232*    END-EXEC.
      *    MOVE '"$.L                  ! " #00002112' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303032313132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000233
000234     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000235         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000236             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000237             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000238             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000239             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000240             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000241             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000242             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000243             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000244         ELSE
000245             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000246             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000247             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000248             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000249             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000250             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000251             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000252             MOVE SPACES               TO PI-SAVED-PROGRAM-6
000253     ELSE
000254         GO TO 0100-RECEIVE.
000255
000256     MOVE LOW-VALUES             TO  EL153AO.
000257
000258     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' or 'FNL'
000259        MOVE -1                  TO CALLTPL
000260     ELSE
000261*       IF PI-COMPANY-ID = 'CID'
000262*          MOVE -1               TO MLINE1L
000263*       ELSE
000264           MOVE -1               TO MSTRDTL
000265*       END-IF
000266     END-IF
000267
000268     MOVE +2                     TO  EMI-NUMBER-OF-LINES.
000269     MOVE SPACES                 TO  MERMSG1O
000270                                     MERMSG2O.
000271     MOVE '153A'                 TO  PI-CURRENT-SCREEN-NO.
000272     MOVE ER-0694                TO  EMI-ERROR.
000273     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000274     GO TO 8100-SEND-INITIAL-MAP.
000275
000276     EJECT
000277 0100-RECEIVE.
000278
000279     IF EIBAID = DFHCLEAR
000280         GO TO 9400-CLEAR.
000281
000282     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000283         MOVE LOW-VALUES         TO  EL153AO
000284         MOVE ER-0008            TO  EMI-ERROR
000285         MOVE -1                 TO  MLINE1L
000286         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000287         GO TO 8200-SEND-DATAONLY.
000288
000289     IF PI-PROCESSOR-ID = 'LGXX'
000290         NEXT SENTENCE
000291     ELSE
000292         
      * EXEC CICS READQ TS
000293*            QUEUE    (PI-SECURITY-TEMP-STORE-ID)
000294*            INTO     (SECURITY-CONTROL)
000295*            LENGTH   (SC-COMM-LENGTH)
000296*            ITEM     (SC-ITEM)
000297*        END-EXEC
      *    MOVE '*$II   L              ''   #00002175' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032313735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000298         MOVE SC-CLAIMS-DISPLAY (8)  TO  PI-DISPLAY-CAP
000299         MOVE SC-CLAIMS-UPDATE  (8)  TO  PI-MODIFY-CAP.
000300
000301     
      * EXEC CICS RECEIVE
000302*        MAP      (WS-MAP-NAME)
000303*        MAPSET   (WS-MAPSET-NAME)
000304*        INTO     (EL153AI)
000305*    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002184' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000306
000307     IF MPFNUMBL > +0
000308         IF EIBAID NOT = DFHENTER
000309             MOVE ER-0004        TO  EMI-ERROR
000310             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000311             MOVE AL-UNBOF       TO  MPFNUMBA
000312             MOVE -1             TO  MPFNUMBL
000313             GO TO 8200-SEND-DATAONLY.
000314
000315     IF MPFNUMBI IS NUMERIC
000316         IF MPFNUMBO > ZERO AND
000317            MPFNUMBO < 25
000318             MOVE PF-VALUES (MPFNUMBI)   TO  EIBAID
000319         ELSE
000320             MOVE ER-0029                TO  EMI-ERROR
000321             MOVE AL-UNBOF               TO  MPFNUMBA
000322             MOVE -1                     TO  MPFNUMBL
000323             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000324             GO TO 8200-SEND-DATAONLY.
000325
000326     IF EIBAID = DFHPF12
000327         MOVE 'EL010   '         TO  THIS-PGM
000328         GO TO 9300-XCTL.
000329
000330     IF EIBAID = DFHPF23
000331         MOVE EIBAID             TO  PI-ENTRY-CD-1
000332         MOVE 'EL005   '         TO  THIS-PGM
000333         GO TO 9300-XCTL.
000334
000335     IF EIBAID = DFHPF24
000336         MOVE 'EL126   '         TO  THIS-PGM
000337         GO TO 9300-XCTL.
000338
000339     IF (EIBAID = DFHENTER)
000340               OR
000341        (EIBAID = DFHPF4 AND
000342         PI-COMPANY-ID = 'DMD')
000343         NEXT SENTENCE
000344     ELSE
000345         MOVE ER-0008            TO  EMI-ERROR
000346         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000347         MOVE -1                 TO  MPFNUMBL
000348         MOVE AL-UNBON           TO  MPFNUMBA
000349         GO TO 8200-SEND-DATAONLY.
000350
000351 0200-PROCESSING-MAINLINE.
000352
000353     IF NOT MODIFY-CAP
000354         MOVE 'UPDATE'           TO  SM-READ
000355         PERFORM 9995-SECURITY-VIOLATION
000356         MOVE ER-0070            TO  EMI-ERROR
000357         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000358         MOVE -1                 TO  MPFNUMBL
000359         GO TO 8100-SEND-INITIAL-MAP.
000360
000361     MOVE +0                     TO  SUB.
000362     PERFORM 4000-CHECK-INPUT-LOOP THRU 4000-CHECK-EXIT.
000363
000364     IF NOT EMI-NO-ERRORS
000365         GO TO 8200-SEND-DATAONLY.
000366
000367     MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.
000368     MOVE PI-CARRIER             TO  WS-KEY-CARRIER.
000369     MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.
000370     MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.
000371
000372     IF (NOTETPL > +0)
000373*       AND (NOTETPI = 'S')
000374        IF (NOTETPI = 'S')
000375          GO TO 1200-BUILD-SPEC-REVIEW
000376        ELSE
000377          GO TO 1000-BUILD-NOTES
000378        END-IF
000379     ELSE
000380       IF MSTRDTI = (LOW-VALUES OR SPACES) AND
000381         MENDDTI = (LOW-VALUES OR SPACES)
000382         GO TO 1000-BUILD-NOTES
000383       ELSE
000384          GO TO 1500-BUILD-REMINDERS
000385       END-IF
000386     END-IF
000387     .
000388     EJECT
000389 1000-BUILD-NOTES.
000390
000391     MOVE SPACES TO W-CALL-TYPE W-NOTE-TYPE-IND W-NOTE-TYPE.
000392
000393     IF CALLTPI NOT = SPACES AND LOW-VALUES
000394         IF CALLTPI = 'I' OR 'O' OR 'N'
000395             MOVE CALLTPI    TO W-CALL-TYPE
000396             MOVE 'C'        TO W-NOTE-TYPE-IND
000397         ELSE
000398             MOVE ER-0915    TO EMI-ERROR
000399             MOVE AL-UABON   TO CALLTPA
000400             MOVE -1         TO CALLTPL
000401             MOVE 'X'        TO WS-DATE-ERROR-SW
000402             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000403             GO TO 8200-SEND-DATAONLY.
000404
000405     IF NOTETPI NOT = SPACES AND LOW-VALUES
000406         IF NOTETPI = 'N'
000407             MOVE NOTETPI    TO W-NOTE-TYPE
000408             MOVE 'N'        TO W-NOTE-TYPE-IND
000409         ELSE
000410             MOVE ER-7846    TO EMI-ERROR
000411             MOVE AL-UABON   TO NOTETPA
000412             MOVE -1         TO NOTETPL
000413             MOVE 'X'        TO WS-DATE-ERROR-SW
000414             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000415             GO TO 8200-SEND-DATAONLY
000416         END-IF
000417     END-IF.
000418
000419     MOVE +1                     TO  SUB   SUB-1.
000420     PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.
000421
000422     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
000423         MOVE +0                 TO  SUB
000424         PERFORM 6000-EXPAND-AIG-NOTES THRU 6000-EXIT.
000425
000426     PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.
000427     PERFORM 7100-GETMAIN-TRLR  THRU 7100-EXIT.
000428
000429     MOVE +15                    TO  SUB.
000430     PERFORM 3200-BUILD-GI-TRLR-LOOP THRU 3200-EXIT.
000431     MOVE +0                     TO  SUB   SUB-1.
000432
000433     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
000434     MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
000435     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
000436     MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
000437
000438     PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
000439
000440     MOVE ER-0000                TO  EMI-ERROR.
000441     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000442     MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
000443
000444     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
000445         MOVE -1                 TO  CALLTPL.
000446
000447     GO TO 8200-SEND-DATAONLY.
000448
000449     EJECT
000450 1200-BUILD-SPEC-REVIEW.
000451
000452     IF NOTETPI NOT = SPACES AND LOW-VALUES
000453         IF NOTETPI = 'S'
000454             MOVE NOTETPI    TO W-NOTE-TYPE
000455             MOVE 'S'        TO W-NOTE-TYPE-IND
000456         ELSE
000457             MOVE ER-7846    TO EMI-ERROR
000458             MOVE AL-UABON   TO NOTETPA
000459             MOVE -1         TO NOTETPL
000460             MOVE 'X'        TO WS-DATE-ERROR-SW
000461             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000462             GO TO 8200-SEND-DATAONLY.
000463
000464     MOVE +1                     TO  SUB   SUB-1.
000465     PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.
000466
000467     PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.
000468     PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.
000469     PERFORM 3150-BUILD-TRAILER-SR THRU 3150-EXIT.
000470
000471     PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
000472
000473     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
000474     MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
000475     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
000476     MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
000477
000478     PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
000479
000480     MOVE ER-0000                TO  EMI-ERROR.
000481     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000482     MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
000483
000484     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
000485         MOVE -1                 TO  CALLTPL.
000486
000487     GO TO 8200-SEND-DATAONLY.
000488
000489     EJECT
000490 1500-BUILD-REMINDERS.
000491
000492     PERFORM 2000-EDIT-SCREEN THRU 2000-EXIT.
000493
000494     IF NOT EMI-NO-ERRORS
000495         GO TO 8200-SEND-DATAONLY.
000496
000497     PERFORM 7000-READ-UP-CLAIM    THRU 7050-EXIT.
000498     PERFORM 2200-CHECK-DATES      THRU 2200-EXIT.
000499     PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.
000500     PERFORM 3000-REDUCE-SEQ       THRU 3000-EXIT.
000501     PERFORM 3100-BUILD-TRAILER-AP THRU 3100-EXIT.
000502
000503     PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
000504
000505     IF CL-NEXT-FOLLOWUP-DT = LOW-VALUES
000506         MOVE WS-STR-DATE-BIN        TO  CL-NEXT-FOLLOWUP-DT
000507     ELSE
000508         IF WS-STR-DATE-BIN < CL-NEXT-FOLLOWUP-DT AND
000509            WS-STR-DATE-BIN NOT = LOW-VALUES
000510             MOVE WS-STR-DATE-BIN    TO  CL-NEXT-FOLLOWUP-DT.
000511
000512     PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
000513
000514     MOVE ER-0000                TO  EMI-ERROR.
000515     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000516     MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
000517
000518     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
000519         MOVE -1                 TO  CALLTPL.
000520
000521     GO TO 8200-SEND-DATAONLY.
000522
000523     EJECT
000524 2000-EDIT-SCREEN.
000525     MOVE SPACES                 TO  WS-DATE-ERROR-SW.
000526
000527     IF MSTRDTI = SPACES OR LOW-VALUES
000528         MOVE LOW-VALUES         TO  WS-STR-DATE-BIN
000529     ELSE
000530         MOVE MSTRDTI            TO  WS-DATE-UNEDIT
000531         
      * EXEC CICS  BIF  DEEDIT
000532*            FIELD (WS-DATE-UNEDIT)
000533*            LENGTH (8)
000534*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002414' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032343134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DATE-UNEDIT, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000535         MOVE '4'                TO  DC-OPTION-CODE
000536         MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY
000537         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
000538         IF DATE-CONVERSION-ERROR
000539             MOVE ER-0314        TO  EMI-ERROR
000540             MOVE AL-UABON       TO  MSTRDTA
000541             MOVE -1             TO  MSTRDTL
000542             MOVE 'X'            TO  WS-DATE-ERROR-SW
000543             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000544         ELSE
000545             IF (PI-COMPANY-ID = 'DMD') AND
000546                (EIBAID NOT = DFHPF4)   AND
000547                (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
000548                 MOVE 'PF4=FORCE 7840' TO PFKEY4O
000549                 MOVE AL-SABON         TO PFKEY4A
000550                 MOVE ER-7840          TO EMI-ERROR
000551                 MOVE AL-UABON         TO MSTRDTA
000552                 MOVE -1               TO MSTRDTL
000553                 MOVE 'X'              TO WS-DATE-ERROR-SW
000554                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000555             ELSE
000556               MOVE SPACES               TO PFKEY4O
000557               MOVE AL-SADON             TO PFKEY4A
000558               MOVE AL-UANON             TO MSTRDTA
000559               MOVE DC-BIN-DATE-1        TO WS-STR-DATE-BIN
000560               MOVE DC-GREG-DATE-1-EDIT  TO MSTRDTI.
000561
000562     IF MENDDTI = SPACES OR = LOW-VALUES
000563         MOVE LOW-VALUES         TO  WS-END-DATE-BIN
000564     ELSE
000565         MOVE MENDDTI            TO  WS-DATE-UNEDIT
000566         
      * EXEC CICS  BIF  DEEDIT
000567*            FIELD (WS-DATE-UNEDIT)
000568*            LENGTH (8)
000569*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002449' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032343439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DATE-UNEDIT, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000570         MOVE '4'                TO  DC-OPTION-CODE
000571         MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY
000572         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
000573         IF  DATE-CONVERSION-ERROR
000574             MOVE ER-0314        TO  EMI-ERROR
000575             MOVE AL-UABON       TO  MENDDTA
000576             MOVE -1             TO  MENDDTL
000577             MOVE 'X'            TO  WS-DATE-ERROR-SW
000578             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000579         ELSE
000580             MOVE AL-UANON             TO  MENDDTA
000581             MOVE DC-BIN-DATE-1        TO  WS-END-DATE-BIN
000582             MOVE DC-GREG-DATE-1-EDIT  TO  MENDDTI.
000583 2000-EXIT.
000584      EXIT.
000585
000586     EJECT
000587 2200-CHECK-DATES.
000588     IF WS-STR-DATE-BIN  LESS THAN SAVE-BIN-DATE AND
000589        WS-END-DATE-BIN  LESS THAN SAVE-BIN-DATE
000590         MOVE ER-0316            TO  EMI-ERROR
000591         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000592         MOVE -1                 TO  MSTRDTL
000593         GO TO 8100-SEND-INITIAL-MAP.
000594
000595     IF WS-STR-DATE-BIN  GREATER THAN WS-END-DATE-BIN
000596         MOVE ER-0317            TO  EMI-ERROR
000597         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000598         MOVE -1                 TO  MSTRDTL
000599         GO TO 8100-SEND-INITIAL-MAP.
000600
000601 2200-EXIT.
000602      EXIT.
000603
000604     EJECT
000605 3000-REDUCE-SEQ.
000606     IF CL-LAST-TRL-AVAIL
000607         MOVE ER-0137            TO  EMI-ERROR
000608         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000609         MOVE -1                 TO  EL153A-NOTE-LENGTH (1)
000610         GO TO 8100-SEND-INITIAL-MAP.
000611
000612     SUBTRACT  1 FROM CL-TRAILER-SEQ-CNT.
000613     MOVE CL-TRAILER-SEQ-CNT     TO  WS-KEY-SEQUENCE-NO
000614                                     AT-SEQUENCE-NO.
000615
000616 3000-EXIT.
000617      EXIT.
000618
000619 3100-BUILD-TRAILER-AP.
000620     MOVE 'AT'                   TO  AT-RECORD-ID.
000621     MOVE '7'                    TO  AT-TRAILER-TYPE.
000622
000623     IF EL153A-NOTE-LENGTH (1) NOT > +0
000624         MOVE SPACES             TO  AT-PROMPT-LINE-1
000625     ELSE
000626         MOVE EL153A-NOTE  (1)   TO  AT-PROMPT-LINE-1.
000627
000628     IF EL153A-NOTE-LENGTH (2) NOT > +0
000629         MOVE SPACES             TO  AT-PROMPT-LINE-2
000630     ELSE
000631         MOVE EL153A-NOTE  (2)   TO  AT-PROMPT-LINE-2.
000632
000633     MOVE WS-STR-DATE-BIN        TO  AT-PROMPT-START-DT.
000634     MOVE WS-END-DATE-BIN        TO  AT-PROMPT-END-DT.
000635     MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
000636     MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
000637                                     AT-PROMPT-LAST-UPDATED-BY.
000638     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
000639     MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
000640                                     AT-PROMPT-LAST-MAINT-DT.
000641
000642 3100-EXIT.
000643      EXIT.
000644
000645     EJECT
000646 3150-BUILD-TRAILER-SR.
000647     MOVE 'AT'                   TO  AT-RECORD-ID.
000648     MOVE '6'                    TO  AT-TRAILER-TYPE.
000649
000650     IF EL153A-NOTE-LENGTH (1) NOT > +0
000651         MOVE SPACES             TO  AT-info-line-1
000652     ELSE
000653         MOVE EL153A-NOTE  (1)   TO  AT-info-line-1.
000654
000655     IF EL153A-NOTE-LENGTH (2) NOT > +0
000656         MOVE SPACES             TO  AT-info-line-2
000657     ELSE
000658         MOVE EL153A-NOTE  (2)   TO  AT-info-line-2.
000659
000660     MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.
000661     move +92                    to  ws-key-sequence-no
000662     MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
000663     MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
000664                                     AT-GEN-INFO-LAST-UPDATED-BY.
000665     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
000666     MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
000667                                     AT-GEN-INFO-LAST-MAINT-DT
000668     .
000669 3150-EXIT.
000670      EXIT.
000671
000672     EJECT
000673 3200-BUILD-GI-TRLR-LOOP.
000674
000675     SUBTRACT +1 FROM SUB.
000676
000677     IF SUB < +1
000678         GO TO 3200-EXIT.
000679
000680     IF EL153A-NOTE-LENGTH (SUB) = +0
000681         GO TO 3200-BUILD-GI-TRLR-LOOP.
000682
000683     MOVE SPACES                 TO ACTIVITY-TRAILERS.
000684     MOVE 'AT'                   TO  AT-RECORD-ID.
000685     MOVE '6'                    TO  AT-TRAILER-TYPE.
000686
000687     MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
000688     MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
000689                                     AT-GEN-INFO-LAST-UPDATED-BY.
000690     MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.
000691     MOVE W-CALL-TYPE            TO  AT-CALL-TYPE.
000692     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
000693     MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
000694                                     AT-GEN-INFO-LAST-MAINT-DT.
000695
000696     IF WS-FIRST-TIME-SW = 'Y'
000697         MOVE 'N'                TO  WS-FIRST-TIME-SW
000698         IF SUB NOT = 1 AND 2
000699             MOVE 'X'            TO AT-NOTE-CONTINUATION
000700         END-IF
000701         IF SUB = 1 OR 3 OR 5 OR 7 OR 9 OR 11 OR 13
000702             MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1
000703             PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT
000704             MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO
000705             PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT
000706             GO TO 3200-BUILD-GI-TRLR-LOOP
000707         ELSE
000708             MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-2
000709             SUBTRACT +1             FROM SUB
000710             MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1
000711             PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT
000712             MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO
000713             PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT
000714             GO TO 3200-BUILD-GI-TRLR-LOOP.
000715
000716     MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-2.
000717     SUBTRACT +1 FROM SUB.
000718     MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-1.
000719
000720     IF SUB NOT = 1
000721         MOVE 'X'                TO AT-NOTE-CONTINUATION.
000722
000723     PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT.
000724     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
000725     PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
000726
000727     GO TO 3200-BUILD-GI-TRLR-LOOP.
000728
000729 3200-EXIT.
000730      EXIT.
000731
000732     EJECT
000733 4000-CHECK-INPUT-LOOP.
000734
000735      ADD +1 TO SUB.
000736
000737      IF SUB > +14
000738          MOVE ER-0483           TO  EMI-ERROR
000739          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000740          MOVE -1                TO  EL153A-NOTE-LENGTH (1)
000741          GO TO 4000-CHECK-EXIT.
000742
000743      IF EL153A-NOTE-LENGTH (SUB) NOT > +0
000744          GO TO 4000-CHECK-INPUT-LOOP.
000745
000746 4000-CHECK-EXIT.
000747     EXIT.
000748
000749 4100-SQUASH-SCREEN.
000750
000751     IF SUB > +14
000752         GO TO 4100-EXIT.
000753
000754     IF EL153A-NOTE-LENGTH (SUB) > +0
000755         IF SUB = SUB-1
000756             ADD +1              TO  SUB   SUB-1
000757             GO TO 4100-SQUASH-SCREEN.
000758
000759     IF EL153A-NOTE-LENGTH (SUB) > +0
000760         MOVE EL153A-NOTE-LENGTH (SUB)   TO
000761                                      EL153A-NOTE-LENGTH (SUB-1)
000762         MOVE EL153A-NOTE-ATTRB  (SUB)   TO
000763                                      EL153A-NOTE-ATTRB  (SUB-1)
000764         MOVE EL153A-NOTE        (SUB)   TO  EL153A-NOTE (SUB-1)
000765         MOVE +0                         TO
000766                                      EL153A-NOTE-LENGTH (SUB)
000767         MOVE LOW-VALUES                 TO  EL153A-NOTE (SUB)
000768         MOVE AL-UANOF                   TO
000769                                      EL153A-NOTE-ATTRB  (SUB)
000770         ADD +1                          TO  SUB   SUB-1
000771     ELSE
000772         ADD +1                          TO  SUB.
000773
000774     GO TO 4100-SQUASH-SCREEN.
000775
000776 4100-EXIT.
000777     EXIT.
000778
000779     EJECT
000780 6000-EXPAND-AIG-NOTES.
000781
000782     ADD +1 TO SUB.
000783
000784     IF SUB > +14
000785         GO TO 6000-EXIT.
000786
000787     IF EL153A-NOTE-LENGTH (SUB) > +0
000788         MOVE EL153A-NOTE (SUB)          TO  WS-EDIT-NOTE
000789         INSPECT WS-EDIT-NOTE REPLACING ALL LOW-VALUES BY SPACES
000790         IF WS-EDIT-NOTE-1-4 = 'APC '
000791             MOVE 'AUTO-PAY CLAIM'       TO  EL153A-NOTE (SUB)
000792         ELSE
000793         IF WS-EDIT-NOTE-1-4 = 'MGR '
000794             MOVE 'MANAGEMENT REVIEW'    TO  EL153A-NOTE (SUB)
000795         ELSE
000796         IF WS-EDIT-NOTE-1-4 = 'SVR '
000797             MOVE 'SUPERVISOR REVIEW'    TO  EL153A-NOTE (SUB)
000798         ELSE
000799         IF WS-EDIT-NOTE-1-4 = 'OIO '
000800             MOVE 'OUTSIDE INVESTIGATION ORDERED'
000801                                         TO  EL153A-NOTE (SUB)
000802         ELSE
000803         IF WS-EDIT-NOTE-1-4 = 'LGR '
000804             MOVE 'LEGAL REVIEW'         TO  EL153A-NOTE (SUB)
000805         ELSE
000806         IF WS-EDIT-NOTE-1-4 = 'FRN '
000807             MOVE 'FORM REVIEWED/NO ADDITIONAL PAYMENT DUE AT THIS
000808-                 ' TIME'                TO  EL153A-NOTE (SUB)
000809         ELSE
000810         IF WS-EDIT-NOTE-1-4 = 'PRB '
000811             MOVE 'PHONE CALL RECEIVED--BRANCH'
000812                                         TO  EL153A-NOTE (SUB)
000813         ELSE
000814         IF WS-EDIT-NOTE-1-4 = 'PRG '
000815             MOVE 'PHONE CALL RECEIVED--GROUP'
000816                                         TO  EL153A-NOTE (SUB)
000817         ELSE
000818         IF WS-EDIT-NOTE-1-4 = 'PRH '
000819             MOVE 'PHONE CALL RECEIVED--HOSPITAL'
000820                                         TO  EL153A-NOTE (SUB)
000821         ELSE
000822         IF WS-EDIT-NOTE-1-4 = 'PRI '
000823             MOVE 'PHONE CALL RECEIVED--INSURED'
000824                                         TO  EL153A-NOTE (SUB)
000825         ELSE
000826         IF WS-EDIT-NOTE-1-4 = 'PRO '
000827             MOVE 'PHONE CALL RECEIVED--OTHER'
000828                                         TO  EL153A-NOTE (SUB)
000829         ELSE
000830         IF WS-EDIT-NOTE-1-4 = 'PRP '
000831             MOVE 'PHONE CALL RECEIVED--PHYSICIAN'
000832                                         TO  EL153A-NOTE (SUB)
000833         ELSE
000834         IF WS-EDIT-NOTE-1-4 = 'PMB '
000835             MOVE 'PHONE CALL MADE--BRANCH'
000836                                         TO  EL153A-NOTE (SUB)
000837         ELSE
000838         IF WS-EDIT-NOTE-1-4 = 'PMG '
000839             MOVE 'PHONE CALL MADE--GROUP'
000840                                         TO  EL153A-NOTE (SUB)
000841         ELSE
000842         IF WS-EDIT-NOTE-1-4 = 'PMH '
000843             MOVE 'PHONE CALL MADE--HOSPITAL'
000844                                         TO  EL153A-NOTE (SUB)
000845         ELSE
000846         IF WS-EDIT-NOTE-1-4 = 'PMI '
000847             MOVE 'PHONE CALL MADE--INSURED'
000848                                         TO  EL153A-NOTE (SUB)
000849         ELSE
000850         IF WS-EDIT-NOTE-1-4 = 'PMO '
000851             MOVE 'PHONE CALL MADE--OTHER'
000852                                         TO  EL153A-NOTE (SUB)
000853         ELSE
000854         IF WS-EDIT-NOTE-1-4 = 'PMP '
000855             MOVE 'PHONE CALL MADE--PHYSICIAN'
000856                                         TO  EL153A-NOTE (SUB)
000857         ELSE
000858         IF WS-EDIT-NOTE-1-3 = '01 '
000859             MOVE 'CONTINUING CLAIM FORM RCVD'
000860                                         TO  EL153A-NOTE (SUB)
000861         ELSE
000862         IF WS-EDIT-NOTE-1-3 = '02 '
000863             MOVE 'ACCOUNT INFORMATION RCVD'
000864                                         TO  EL153A-NOTE (SUB)
000865         ELSE
000866         IF WS-EDIT-NOTE-1-3 = '03 '
000867             MOVE 'MEDICAL HISTORY RCVD'
000868                                         TO  EL153A-NOTE (SUB)
000869         ELSE
000870         IF WS-EDIT-NOTE-1-3 = '04 '
000871             MOVE 'INSURED INFO RCVD'
000872                                         TO  EL153A-NOTE (SUB)
000873         ELSE
000874         IF WS-EDIT-NOTE-1-3 = '05 '
000875             MOVE 'EMPLOYER INFO RCVD'
000876                                         TO  EL153A-NOTE (SUB)
000877         ELSE
000878         IF WS-EDIT-NOTE-1-3 = '06 '
000879             MOVE 'PARTIAL INVESTIGATION RCVD.  CLAIM IS STILL PEN
000880-                 'DING'                 TO  EL153A-NOTE (SUB)
000881         ELSE
000882         IF WS-EDIT-NOTE-1-3 = '07 '
000883             MOVE 'DEATH CERTIFICATE RCVD'
000884                                         TO  EL153A-NOTE (SUB)
000885         ELSE
000886         IF WS-EDIT-NOTE-1-3 = '08 '
000887             MOVE 'ATTORNEY LTR RCVD'
000888                                         TO  EL153A-NOTE (SUB)
000889         ELSE
000890         IF WS-EDIT-NOTE-1-3 = '09 '
000891             MOVE 'EXAM RESULTS RCVD'
000892                                         TO  EL153A-NOTE (SUB)
000893         ELSE
000894         IF WS-EDIT-NOTE-1-3 = '10 '
000895             MOVE 'INS DEPT INQUIRY RCVD'
000896                                         TO  EL153A-NOTE (SUB)
000897         ELSE
000898         IF WS-EDIT-NOTE-1-3 = '11 '
000899             MOVE 'RETURNED MAIL RCVD'
000900                                         TO  EL153A-NOTE (SUB)
000901         ELSE
000902         IF WS-EDIT-NOTE-1-3 = '12 '
000903             MOVE 'INITIAL CLAIM RCVD'
000904                                         TO  EL153A-NOTE (SUB).
000905
000906     GO TO 6000-EXPAND-AIG-NOTES.
000907
000908 6000-EXIT.
000909     EXIT.
000910
000911     EJECT
000912 7000-READ-UP-CLAIM.
000913
000914     
      * EXEC CICS HANDLE CONDITION
000915*        NOTFND    (7040-NOTFND)
000916*        NOTOPEN   (9981-NOTOPEN-MSTR)
000917*    END-EXEC.
      *    MOVE '"$IJ                  ! # #00002797' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303032373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000918
000919     
      * EXEC CICS READ
000920*        DATASET   ('ELMSTR')
000921*        RIDFLD    (WS-CLAIM-KEY)
000922*        SET       (ADDRESS OF CLAIM-MASTER)
000923*        UPDATE
000924*    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00002802' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303032383032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000925
000926     GO TO 7050-EXIT.
000927
000928 7040-NOTFND.
000929
000930     MOVE ER-0133                TO  EMI-ERROR.
000931     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000932     MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
000933     GO TO 8100-SEND-INITIAL-MAP.
000934
000935 7050-EXIT.
000936     EXIT.
000937
000938 7055-REWRITE-CLAIM.
000939
000940     
      * EXEC CICS HANDLE CONDITION
000941*        DUPKEY   (7055-EXIT)
000942*    END-EXEC.
      *    MOVE '"$$                   ! $ #00002823' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303032383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000943
000944     
      * EXEC CICS REWRITE
000945*        DATASET   ('ELMSTR')
000946*        FROM      (CLAIM-MASTER)
000947*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00002827' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303032383237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000948
000949 7055-EXIT.
000950     EXIT.
000951
000952 7100-GETMAIN-TRLR.
000953     
      * EXEC CICS GETMAIN
000954*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
000955*        INITIMG  (GETMAIN-SPACE)
000956*        LENGTH   (WS-TRLR-LENGTH)
000957*    END-EXEC.
      *    MOVE ',"IL                  $   #00002836' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303032383336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000958
000959 7100-EXIT.
000960      EXIT.
000961
000962 7150-WRITE-TRAILER.
000963
000964     
      * EXEC CICS HANDLE CONDITION
000965*        DUPREC    (7190-DUPREC)
000966*        NOTOPEN   (9982-NOTOPEN-TRLR)
000967*    END-EXEC.
      *    MOVE '"$%J                  ! % #00002847' TO DFHEIV0
           MOVE X'2224254A2020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303032383437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000968
000969     
      * EXEC CICS WRITE
000970*        DATASET   ('ELTRLR')
000971*        RIDFLD    (WS-TRAILER-KEY)
000972*        FROM      (ACTIVITY-TRAILERS)
000973*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00002852' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303032383532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000974
000975     GO TO 7199-EXIT.
000976
000977 7190-DUPREC.
000978
000979     if w-note-type = 'S'
000980        MOVE ER-7846             TO EMI-ERROR
000981        MOVE -1                  TO notetpl
000982        PERFORM 9900-ERROR-FORMAT
000983                                 THRU 9900-EXIT
000984        GO TO 8200-SEND-DATAONLY
000985     else
000986        PERFORM 3000-REDUCE-SEQ  THRU 3000-EXIT
000987        GO TO 7150-WRITE-TRAILER
000988     end-if
000989
000990     .
000991 7199-EXIT.
000992     EXIT.
000993
000994      EJECT
000995 8100-SEND-INITIAL-MAP.
000996     MOVE EIBTIME                TO  TIME-IN.
000997     MOVE WS-HOUR                TO  WS-TRANS-HOUR.
000998     MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.
000999     MOVE TIME-OUT               TO  MRNTIMEO.
001000     MOVE SAVE-DATE              TO  MRNDATEO.
001001     MOVE PI-CERT-NO             TO  MCERTO.
001002
001003     
      * EXEC CICS SEND
001004*        MAP      (WS-MAP-NAME)
001005*        MAPSET   (WS-MAPSET-NAME)
001006*        FROM     (EL153AI)
001007*        FREEKB
001008*        ERASE
001009*        CURSOR
001010*    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E F  H L F ,   #00002886' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2046202048204C2046202C20' &
                X'2020233030303032383836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
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
           
001011
001012     GO TO 9100-RETURN-TRANS.
001013 8100-EXIT.
001014     EXIT.
001015
001016 8200-SEND-DATAONLY.
001017     MOVE EIBTIME                TO  TIME-IN.
001018     MOVE WS-HOUR                TO  WS-TRANS-HOUR.
001019     MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.
001020     MOVE TIME-OUT               TO  MRNTIMEO.
001021     MOVE SAVE-DATE              TO  MRNDATEO.
001022     MOVE PI-CERT-NO             TO  MCERTO.
001023
001024     
      * EXEC CICS SEND
001025*        MAP      (WS-MAP-NAME)
001026*        MAPSET   (WS-MAPSET-NAME)
001027*        FROM     (EL153AI)
001028*        FREEKB
001029*        DATAONLY
001030*        CURSOR
001031*    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT    F  H L F ,   #00002907' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2046202048204C2046202C20' &
                X'2020233030303032393037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
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
           
001032
001033     GO TO 9100-RETURN-TRANS.
001034
001035 8200-EXIT.
001036     EXIT.
001037
001038     EJECT
001039 8300-SEND-TEXT.
001040     
      * EXEC CICS SEND TEXT
001041*        FROM     (LOGOFF-TEXT)
001042*        ERASE
001043*        FREEKB
001044*        LENGTH   (LOGOFF-LENGTH)
001045*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002923' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303032393233' TO DFHEIV0
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
           
001046
001047     
      * EXEC CICS RETURN
001048*    END-EXEC.
      *    MOVE '.(                    ''   #00002930' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303032393330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001049
001050 8300-EXIT.
001051      EXIT.
001052
001053 8500-DATE-CONVERSION.
001054     
      * EXEC CICS LINK
001055*           PROGRAM  ('ELDATCV')
001056*           COMMAREA (DATE-CONVERSION-DATA)
001057*           LENGTH   (DC-COMM-LENGTH)
001058*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002937' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032393337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001059
001060 8500-EXIT.
001061      EXIT.
001062
001063     EJECT
001064 9000-UNAUTHERR.
001065     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001066     GO TO 8300-SEND-TEXT.
001067
001068 9100-RETURN-TRANS.
001069     
      * EXEC CICS RETURN
001070*        TRANSID  (WS-TRANS-ID)
001071*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001072*        LENGTH   (PI-COMM-LENGTH)
001073*    END-EXEC.
      *    MOVE '.(CT                  ''   #00002952' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303032393532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001074
001075 9100-EXIT.
001076      EXIT.
001077
001078 9300-XCTL.
001079     
      * EXEC CICS XCTL
001080*        PROGRAM  (THIS-PGM)
001081*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001082*        LENGTH   (PI-COMM-LENGTH)
001083*    END-EXEC.
      *    MOVE '.$C                   %   #00002962' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303032393632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001084
001085 9300-EXIT.
001086      EXIT.
001087
001088 9400-CLEAR.
001089     MOVE DFHENTER              TO  EIBAID.
001090     MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
001091     GO TO 9300-XCTL.
001092
001093 9400-EXIT.
001094     EXIT.
001095
001096     EJECT
001097 9600-PGMIDERR.
001098     
      * EXEC CICS HANDLE CONDITION
001099*        PGMIDERR (8300-SEND-TEXT)
001100*    END-EXEC.
      *    MOVE '"$L                   ! & #00002981' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303032393831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001101
001102     MOVE THIS-PGM               TO  LOGOFF-PGM
001103                                     PI-CALLING-PROGRAM.
001104     MOVE SPACES                 TO  PI-ENTRY-CD-1.
001105     MOVE 'EL005'                TO  THIS-PGM.
001106     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
001107     GO TO 9300-XCTL.
001108
001109 9600-EXIT.
001110      EXIT.
001111
001112     EJECT
001113 9900-ERROR-FORMAT.
001114     
      * EXEC CICS LINK
001115*        PROGRAM  ('EL001')
001116*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
001117*        LENGTH   (EMI-COMM-LENGTH)
001118*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00002997' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032393937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001119
001120     MOVE EMI-LINE1          TO MERMSG1O.
001121     MOVE EMI-LINE2          TO MERMSG2O.
001122
001123 9900-EXIT.
001124      EXIT.
001125
001126 9981-NOTOPEN-MSTR.
001127     MOVE ER-0154                TO  EMI-ERROR.
001128     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001129     MOVE -1                     TO  MPFNUMBL.
001130     GO TO 8100-SEND-INITIAL-MAP.
001131
001132 9981-EXIT.
001133      EXIT.
001134
001135 9982-NOTOPEN-TRLR.
001136     MOVE ER-0172                TO  EMI-ERROR.
001137     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001138     MOVE -1                     TO  MPFNUMBL.
001139     GO TO 8100-SEND-INITIAL-MAP.
001140
001141 9990-ERROR.
001142     
      * EXEC CICS LINK
001143*        PROGRAM  ('EL004')
001144*        COMMAREA (DFHEIBLK)
001145*        LENGTH   (64)
001146*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00003025' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033303235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001147
001148     GO TO 8200-SEND-DATAONLY.
001149
001150 9995-SECURITY-VIOLATION.
001151*                            COPY ELCSCTP SUPPRESS.
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
      *    MOVE '."C                   (   #00003052' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033303532' TO DFHEIV0
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
001152

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL153' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR,
                     9600-PGMIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 7040-NOTFND,
                     9981-NOTOPEN-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7055-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7190-DUPREC,
                     9982-NOTOPEN-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL153' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
