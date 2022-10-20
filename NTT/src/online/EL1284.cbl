      *((program: EL1284.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL1284.
000004*
000005*AUTHOR.           LOGIC,INC.
000006*                  DALLAS,TEXAS.
000007
000008*DATE-COMPILED.
000009*SECURITY.   *****************************************************
000010*            *                                                   *
000011*            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *
000012*            *                                                   *
000013*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000014*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000015*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000016*            *                                                   *
000017*            *****************************************************
000018*
000019*REMARKS.     TRANSACTION - EXXG - CLAIM MEMO MAINTENANCE.
000020*
000021******************************************************************
000022*                   C H A N G E   L O G
000023*
000024* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000025*-----------------------------------------------------------------
000026*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000027* EFFECTIVE    NUMBER
000028*-----------------------------------------------------------------
000029* 010416   2015072900002   TANA  NEW CLAIM MEMO SCREEN
000030* 040416   2016021500002   TANA  CHANGE REWRITE FIELD VALID VALUES
000031* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000032* 080322  CR2021100800003  TANA  Add B and H claim types
000033******************************************************************
000034*
000035 ENVIRONMENT DIVISION.
000036
000037     EJECT
000038 DATA DIVISION.
000039 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000040 77  FILLER  PIC X(32)  VALUE '********************************'.
000041 77  FILLER  PIC X(32)  VALUE '*     EL1284 WORKING STORAGE   *'.
000042 77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.
000043
000044*    COPY ELCSCTM.
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
000047     EJECT
000048 01  STANDARD-AREAS.
000049     12  GETMAIN-SPACE           PIC X       VALUE SPACE.
000050     12  MAP-NAME                PIC X(8)    VALUE 'EL1284A'.
000051     12  MAPSET-NAME             PIC X(8)    VALUE 'EL1284S '.
000052     12  SCRN-NUMBER             PIC X(4)    VALUE '128D'.
000053     12  TRANS-ID                PIC X(4)    VALUE 'EXXG'.
000054     12  THIS-PGM                PIC X(8)    VALUE 'EL1284'.
000055     12  PGM-NAME                PIC X(8).
000056     12  PGM-EL1276              PIC X(8)    VALUE 'EL1276'.
000057
000058     12  ELMEMO-FILE-ID          PIC X(8)    VALUE 'ELMEMO'.
000059     12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.
000060     12  QID.
000061         16  QID-TERM            PIC X(4).
000062         16  FILLER              PIC X(4)    VALUE '128D'.
000063     12  QID-ITEM                PIC S9(4)   COMP VALUE +0.
000064     12  SC-ITEM                 PIC S9(4)   COMP VALUE +1.
000065     12  WS-LINE                 PIC 9(3)    VALUE 0.
000066     12  WS-BLANK-LINES          PIC S9(4)   COMP VALUE +0.
000067
000068 01  WORK-AREA.
000069     12  SAVE-DATE           PIC X(8)    VALUE SPACES.
000070     12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
000071
000072     12  ELMEMO-LENGTH           PIC S9(4)   COMP VALUE +132.
000073     12  ELMEMO-KEY-LENGTH       PIC S9(4)   COMP VALUE +23.
000074     12  ELMEMO-START-LENGTH     PIC S9(4)   COMP VALUE +21.
000075     12  ELMEMO-KEY.
000076         16  ELMEMO-PARTIAL-KEY.
000077             20 ELMEMO-COMPANY-CD    PIC X.
000078             20 ELMEMO-RECORD-TYPE   PIC X.
000079             20 ELMEMO-CARRIER       PIC X.
000080             20 ELMEMO-CLAIM-NO      PIC  X(7).
000081             20 ELMEMO-CERT-NO.
000082                25 ELMEMO-CERT-PRIME PIC X(10).
000083                25 ELMEMO-CERT-SFX   PIC X.
000084         16 ELMEMO-SEQ           PIC S9(4) COMP.
000085     12  SV-PRIOR-KEY.
000086         20 SV-COMPANY-CD            PIC X.
000087         20 SV-RECORD-TYPE           PIC X.
000088         20 SV-CARRIER               PIC X.
000089         20 SV-CLAIM-NO              PIC  X(7).
000090         20 SV-CERT-NO.
000091            25 SV-CERT-PRIME         PIC X(10).
000092            25 SV-CERT-SFX           PIC X(1).
000093     12  ELCERT-KEY.
000094         16  ELCERT-COMPANY-CD        PIC X.
000095         16  ELCERT-CARRIER           PIC X.
000096         16  ELCERT-GROUPING          PIC X(6).
000097         16  ELCERT-STATE             PIC XX.
000098         16  ELCERT-ACCOUNT           PIC X(10).
000099         16  ELCERT-EFF-DT            PIC XX.
000100         16  ELCERT-CERT-NO.
000101             20  ELCERT-CERT-PRIME    PIC X(10).
000102             20  ELCERT-CERT-SFX      PIC X.
000103
000104     12  TIME-IN                 PIC S9(7).
000105     12  TIME-SPLIT REDEFINES TIME-IN.
000106         16  FILLER              PIC X.
000107         16  TIME-OUT            PIC 99V99.
000108         16  FILLER              PIC X(2).
000109     12  XCTL-005                PIC X(8)    VALUE 'EL005'.
000110     12  XCTL-126                PIC X(8)    VALUE 'EL126'.
000111     12  LINK-001                PIC X(8)    VALUE 'EL001'.
000112     12  LINK-004                PIC X(8)    VALUE 'EL004'.
000113     12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
000114     12  MAX-LINES               PIC 999     VALUE 300.
000115     12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 10.
000116     12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.
000117     12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.
000118     12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.
000119     12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.
000120     12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.
000121     12  WS-SUB                  PIC S9(3)   COMP-3.
000122     12  WS-SUB1                 PIC S9(3)   COMP-3.
000123     12  WS-ERASE-EOF            PIC X       VALUE X'80'.
000124     12  DEEDIT-DATE-INPUT.
000125         16  FILLER              PIC XX.
000126         16  DEEDIT-DATE         PIC X(6).
000127     12  WS-CURSOR-SET           PIC X       VALUE 'N'.
000128         88 CURSOR-SET                       VALUE 'Y'.
000129
000130     12  WS-SCREEN-LINE          PIC X       VALUE 'N'.
000131         88 SCREEN-LINE-FOUND                VALUE 'Y'.
000132
000133     12  ELMSTR-KEY.
000134         16  MSTR-COMP-CD    PIC X.
000135         16  MSTR-CARRIER    PIC X.
000136         16  MSTR-CLAIM-NO   PIC X(7).
000137         16  MSTR-CERT-NO.
000138             20  MSTR-CERT-NO-PRIME  PIC X(10).
000139             20  MSTR-CERT-NO-SUFX   PIC X.
000140
000141 01  COMP-LENGTHS.
000142     12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.
000143     12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
000144     12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
000145     12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.
000146     12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.
000147     12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.
000148     12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.
000149     12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
000150     12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.
000151     12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.
000152     12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.
000153     12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.
000154     12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.
000155     12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.
000156
000157     EJECT
000158 01  ERROR-MESSAGES.
000159     12  ER-0000             PIC X(04)       VALUE '0000'.
000160     12  ER-0004             PIC X(04)       VALUE '0004'.
000161     12  ER-0006             PIC X(04)       VALUE '0006'.
000162     12  ER-0008             PIC X(04)       VALUE '0008'.
000163     12  ER-0023             PIC X(04)       VALUE '0023'.
000164     12  ER-0029             PIC X(04)       VALUE '0029'.
000165     12  ER-0030             PIC X(04)       VALUE '0030'.
000166     12  ER-0031             PIC X(04)       VALUE '0031'.
000167     12  ER-0032             PIC X(04)       VALUE '0032'.
000168     12  ER-0033             PIC X(04)       VALUE '0033'.
000169     12  ER-0041             PIC X(04)       VALUE '0041'.
000170     12  ER-0044             PIC X(04)       VALUE '0044'.
000171     12  ER-0045             PIC X(04)       VALUE '0045'.
000172     12  ER-0046             PIC X(04)       VALUE '0046'.
000173     12  ER-0047             PIC X(04)       VALUE '0047'.
000174     12  ER-0048             PIC X(04)       VALUE '0048'.
000175     12  ER-0049             PIC X(04)       VALUE '0049'.
000176     12  ER-0050             PIC X(04)       VALUE '0050'.
000177     12  ER-0051             PIC X(04)       VALUE '0051'.
000178     12  ER-0066             PIC X(04)       VALUE '0066'.
000179     12  ER-0067             PIC X(04)       VALUE '0067'.
000180     12  ER-0069             PIC X(04)       VALUE '0069'.
000181     12  ER-0070             PIC X(04)       VALUE '0070'.
000182     12  ER-0140             PIC X(04)       VALUE '0140'.
000183     12  ER-0314             PIC X(04)       VALUE '0314'.
000184     12  ER-2954             PIC X(04)       VALUE '2954'.
000185     12  ER-3846             PIC X(04)       VALUE '3846'.
000186
000187     EJECT
000188*                       COPY ELCLOGOF.
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
000189     EJECT
000190*                       COPY ELCAID.
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
000191 01  FILLER  REDEFINES DFHAID.
000192     12  FILLER                  PIC X(8).
000193     12  PF-VALUES OCCURS 24 TIMES       PIC X.
000194     EJECT
000195*                       COPY ELCEMIB.
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
000196     EJECT
000197*                       COPY ELCINTF.
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
000198     EJECT
000199     12  PI-EL1284-AREA    REDEFINES PI-PROGRAM-WORK-AREA.
000200         16  FILLER              PIC X(318).
000201         16  PI-TOTAL-LINES      PIC S9(3).
000202         16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
000203         16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
000204         16  PI-UPDATE-SW        PIC X.
000205             88  PI-CHANGES-MADE             VALUE '1'.
000206         16  PI-LONG-HLTH-APP    PIC  X(01).
000207         16  PI-REWRITE          PIC  X(01).
000208         16  PI-CHKCOVG          PIC  X(01).
000209         16  PI-MR-RELEASE-DATE  PIC  X(08).
000210         16  PI-BILLING-NOTES-EXIST PIC X.
000211         16  PI-CERT-NOTES-EXIST    PIC X.
000212         16  PI-CLAIM-NOTES-EXIST   PIC X.
000213         16  PI-SET-NOTE-CHANGE  PIC X.
000214             88 PI-CHANGE-IN-NOTE-TYPE       VALUE 'Y'.
000215     EJECT
000216*                          COPY ELCATTR.
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
000217     EJECT
000218
000219*                          COPY ELCDATE.
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
000220     EJECT
000221*                          COPY EL1284S.
      *>>((file: EL1284S))
000001 01  EL1284AI.
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
000028     05  CLAIMNOL PIC S9(0004) COMP.
000029     05  CLAIMNOF PIC  X(0001).
000030     05  FILLER REDEFINES CLAIMNOF.
000031         10  CLAIMNOA PIC  X(0001).
000032     05  CLAIMNOI PIC  X(0007).
000033*    -------------------------------
000034     05  TYPEL PIC S9(0004) COMP.
000035     05  TYPEF PIC  X(0001).
000036     05  FILLER REDEFINES TYPEF.
000037         10  TYPEA PIC  X(0001).
000038     05  TYPEI PIC  X(0006).
000039*    -------------------------------
000040     05  CARRL PIC S9(0004) COMP.
000041     05  CARRF PIC  X(0001).
000042     05  FILLER REDEFINES CARRF.
000043         10  CARRA PIC  X(0001).
000044     05  CARRI PIC  X(0001).
000045*    -------------------------------
000046     05  CERTNOL PIC S9(0004) COMP.
000047     05  CERTNOF PIC  X(0001).
000048     05  FILLER REDEFINES CERTNOF.
000049         10  CERTNOA PIC  X(0001).
000050     05  CERTNOI PIC  X(0011).
000051*    -------------------------------
000052     05  FSTNMEL PIC S9(0004) COMP.
000053     05  FSTNMEF PIC  X(0001).
000054     05  FILLER REDEFINES FSTNMEF.
000055         10  FSTNMEA PIC  X(0001).
000056     05  FSTNMEI PIC  X(0012).
000057*    -------------------------------
000058     05  LASTNMEL PIC S9(0004) COMP.
000059     05  LASTNMEF PIC  X(0001).
000060     05  FILLER REDEFINES LASTNMEF.
000061         10  LASTNMEA PIC  X(0001).
000062     05  LASTNMEI PIC  X(0015).
000063*    -------------------------------
000064     05  TOTL PIC S9(0004) COMP.
000065     05  TOTF PIC  X(0001).
000066     05  FILLER REDEFINES TOTF.
000067         10  TOTA PIC  X(0001).
000068     05  TOTI PIC  X(0003).
000069*    -------------------------------
000070     05  LNGHLAPL PIC S9(0004) COMP.
000071     05  LNGHLAPF PIC  X(0001).
000072     05  FILLER REDEFINES LNGHLAPF.
000073         10  LNGHLAPA PIC  X(0001).
000074     05  LNGHLAPI PIC  X(0001).
000075*    -------------------------------
000076     05  REWRITEL PIC S9(0004) COMP.
000077     05  REWRITEF PIC  X(0001).
000078     05  FILLER REDEFINES REWRITEF.
000079         10  REWRITEA PIC  X(0001).
000080     05  REWRITEI PIC  X(0001).
000081*    -------------------------------
000082     05  CHKCOVGL PIC S9(0004) COMP.
000083     05  CHKCOVGF PIC  X(0001).
000084     05  FILLER REDEFINES CHKCOVGF.
000085         10  CHKCOVGA PIC  X(0001).
000086     05  CHKCOVGI PIC  X(0001).
000087*    -------------------------------
000088     05  MRRELDTL PIC S9(0004) COMP.
000089     05  MRRELDTF PIC  X(0001).
000090     05  FILLER REDEFINES MRRELDTF.
000091         10  MRRELDTA PIC  X(0001).
000092     05  MRRELDTI PIC  X(0008).
000093*    -------------------------------
000094     05  LN1L PIC S9(0004) COMP.
000095     05  LN1F PIC  X(0001).
000096     05  FILLER REDEFINES LN1F.
000097         10  LN1A PIC  X(0001).
000098     05  LN1I PIC  X(0003).
000099*    -------------------------------
000100     05  FNLINE1L PIC S9(0004) COMP.
000101     05  FNLINE1F PIC  X(0001).
000102     05  FILLER REDEFINES FNLINE1F.
000103         10  FNLINE1A PIC  X(0001).
000104     05  FNLINE1I PIC  X(0063).
000105*    -------------------------------
000106     05  MTBY1L PIC S9(0004) COMP.
000107     05  MTBY1F PIC  X(0001).
000108     05  FILLER REDEFINES MTBY1F.
000109         10  MTBY1A PIC  X(0001).
000110     05  MTBY1I PIC  X(0004).
000111*    -------------------------------
000112     05  MTDT1L PIC S9(0004) COMP.
000113     05  MTDT1F PIC  X(0001).
000114     05  FILLER REDEFINES MTDT1F.
000115         10  MTDT1A PIC  X(0001).
000116     05  MTDT1I PIC  X(0006).
000117*    -------------------------------
000118     05  LN2L PIC S9(0004) COMP.
000119     05  LN2F PIC  X(0001).
000120     05  FILLER REDEFINES LN2F.
000121         10  LN2A PIC  X(0001).
000122     05  LN2I PIC  X(0003).
000123*    -------------------------------
000124     05  FNLINE2L PIC S9(0004) COMP.
000125     05  FNLINE2F PIC  X(0001).
000126     05  FILLER REDEFINES FNLINE2F.
000127         10  FNLINE2A PIC  X(0001).
000128     05  FNLINE2I PIC  X(0063).
000129*    -------------------------------
000130     05  MTBY2L PIC S9(0004) COMP.
000131     05  MTBY2F PIC  X(0001).
000132     05  FILLER REDEFINES MTBY2F.
000133         10  MTBY2A PIC  X(0001).
000134     05  MTBY2I PIC  X(0004).
000135*    -------------------------------
000136     05  MTDT2L PIC S9(0004) COMP.
000137     05  MTDT2F PIC  X(0001).
000138     05  FILLER REDEFINES MTDT2F.
000139         10  MTDT2A PIC  X(0001).
000140     05  MTDT2I PIC  X(0006).
000141*    -------------------------------
000142     05  LN3L PIC S9(0004) COMP.
000143     05  LN3F PIC  X(0001).
000144     05  FILLER REDEFINES LN3F.
000145         10  LN3A PIC  X(0001).
000146     05  LN3I PIC  X(0003).
000147*    -------------------------------
000148     05  FNLINE3L PIC S9(0004) COMP.
000149     05  FNLINE3F PIC  X(0001).
000150     05  FILLER REDEFINES FNLINE3F.
000151         10  FNLINE3A PIC  X(0001).
000152     05  FNLINE3I PIC  X(0063).
000153*    -------------------------------
000154     05  MTBY3L PIC S9(0004) COMP.
000155     05  MTBY3F PIC  X(0001).
000156     05  FILLER REDEFINES MTBY3F.
000157         10  MTBY3A PIC  X(0001).
000158     05  MTBY3I PIC  X(0004).
000159*    -------------------------------
000160     05  MTDT3L PIC S9(0004) COMP.
000161     05  MTDT3F PIC  X(0001).
000162     05  FILLER REDEFINES MTDT3F.
000163         10  MTDT3A PIC  X(0001).
000164     05  MTDT3I PIC  X(0006).
000165*    -------------------------------
000166     05  LN4L PIC S9(0004) COMP.
000167     05  LN4F PIC  X(0001).
000168     05  FILLER REDEFINES LN4F.
000169         10  LN4A PIC  X(0001).
000170     05  LN4I PIC  X(0003).
000171*    -------------------------------
000172     05  FNLINE4L PIC S9(0004) COMP.
000173     05  FNLINE4F PIC  X(0001).
000174     05  FILLER REDEFINES FNLINE4F.
000175         10  FNLINE4A PIC  X(0001).
000176     05  FNLINE4I PIC  X(0063).
000177*    -------------------------------
000178     05  MTBY4L PIC S9(0004) COMP.
000179     05  MTBY4F PIC  X(0001).
000180     05  FILLER REDEFINES MTBY4F.
000181         10  MTBY4A PIC  X(0001).
000182     05  MTBY4I PIC  X(0004).
000183*    -------------------------------
000184     05  MTDT4L PIC S9(0004) COMP.
000185     05  MTDT4F PIC  X(0001).
000186     05  FILLER REDEFINES MTDT4F.
000187         10  MTDT4A PIC  X(0001).
000188     05  MTDT4I PIC  X(0006).
000189*    -------------------------------
000190     05  LN5L PIC S9(0004) COMP.
000191     05  LN5F PIC  X(0001).
000192     05  FILLER REDEFINES LN5F.
000193         10  LN5A PIC  X(0001).
000194     05  LN5I PIC  X(0003).
000195*    -------------------------------
000196     05  FNLINE5L PIC S9(0004) COMP.
000197     05  FNLINE5F PIC  X(0001).
000198     05  FILLER REDEFINES FNLINE5F.
000199         10  FNLINE5A PIC  X(0001).
000200     05  FNLINE5I PIC  X(0063).
000201*    -------------------------------
000202     05  MTBY5L PIC S9(0004) COMP.
000203     05  MTBY5F PIC  X(0001).
000204     05  FILLER REDEFINES MTBY5F.
000205         10  MTBY5A PIC  X(0001).
000206     05  MTBY5I PIC  X(0004).
000207*    -------------------------------
000208     05  MTDT5L PIC S9(0004) COMP.
000209     05  MTDT5F PIC  X(0001).
000210     05  FILLER REDEFINES MTDT5F.
000211         10  MTDT5A PIC  X(0001).
000212     05  MTDT5I PIC  X(0006).
000213*    -------------------------------
000214     05  LN6L PIC S9(0004) COMP.
000215     05  LN6F PIC  X(0001).
000216     05  FILLER REDEFINES LN6F.
000217         10  LN6A PIC  X(0001).
000218     05  LN6I PIC  X(0003).
000219*    -------------------------------
000220     05  FNLINE6L PIC S9(0004) COMP.
000221     05  FNLINE6F PIC  X(0001).
000222     05  FILLER REDEFINES FNLINE6F.
000223         10  FNLINE6A PIC  X(0001).
000224     05  FNLINE6I PIC  X(0063).
000225*    -------------------------------
000226     05  MTBY6L PIC S9(0004) COMP.
000227     05  MTBY6F PIC  X(0001).
000228     05  FILLER REDEFINES MTBY6F.
000229         10  MTBY6A PIC  X(0001).
000230     05  MTBY6I PIC  X(0004).
000231*    -------------------------------
000232     05  MTDT6L PIC S9(0004) COMP.
000233     05  MTDT6F PIC  X(0001).
000234     05  FILLER REDEFINES MTDT6F.
000235         10  MTDT6A PIC  X(0001).
000236     05  MTDT6I PIC  X(0006).
000237*    -------------------------------
000238     05  LN7L PIC S9(0004) COMP.
000239     05  LN7F PIC  X(0001).
000240     05  FILLER REDEFINES LN7F.
000241         10  LN7A PIC  X(0001).
000242     05  LN7I PIC  X(0003).
000243*    -------------------------------
000244     05  FNLINE7L PIC S9(0004) COMP.
000245     05  FNLINE7F PIC  X(0001).
000246     05  FILLER REDEFINES FNLINE7F.
000247         10  FNLINE7A PIC  X(0001).
000248     05  FNLINE7I PIC  X(0063).
000249*    -------------------------------
000250     05  MTBY7L PIC S9(0004) COMP.
000251     05  MTBY7F PIC  X(0001).
000252     05  FILLER REDEFINES MTBY7F.
000253         10  MTBY7A PIC  X(0001).
000254     05  MTBY7I PIC  X(0004).
000255*    -------------------------------
000256     05  MTDT7L PIC S9(0004) COMP.
000257     05  MTDT7F PIC  X(0001).
000258     05  FILLER REDEFINES MTDT7F.
000259         10  MTDT7A PIC  X(0001).
000260     05  MTDT7I PIC  X(0006).
000261*    -------------------------------
000262     05  LN8L PIC S9(0004) COMP.
000263     05  LN8F PIC  X(0001).
000264     05  FILLER REDEFINES LN8F.
000265         10  LN8A PIC  X(0001).
000266     05  LN8I PIC  X(0003).
000267*    -------------------------------
000268     05  FNLINE8L PIC S9(0004) COMP.
000269     05  FNLINE8F PIC  X(0001).
000270     05  FILLER REDEFINES FNLINE8F.
000271         10  FNLINE8A PIC  X(0001).
000272     05  FNLINE8I PIC  X(0063).
000273*    -------------------------------
000274     05  MTBY8L PIC S9(0004) COMP.
000275     05  MTBY8F PIC  X(0001).
000276     05  FILLER REDEFINES MTBY8F.
000277         10  MTBY8A PIC  X(0001).
000278     05  MTBY8I PIC  X(0004).
000279*    -------------------------------
000280     05  MTDT8L PIC S9(0004) COMP.
000281     05  MTDT8F PIC  X(0001).
000282     05  FILLER REDEFINES MTDT8F.
000283         10  MTDT8A PIC  X(0001).
000284     05  MTDT8I PIC  X(0006).
000285*    -------------------------------
000286     05  LN9L PIC S9(0004) COMP.
000287     05  LN9F PIC  X(0001).
000288     05  FILLER REDEFINES LN9F.
000289         10  LN9A PIC  X(0001).
000290     05  LN9I PIC  X(0003).
000291*    -------------------------------
000292     05  FNLINE9L PIC S9(0004) COMP.
000293     05  FNLINE9F PIC  X(0001).
000294     05  FILLER REDEFINES FNLINE9F.
000295         10  FNLINE9A PIC  X(0001).
000296     05  FNLINE9I PIC  X(0063).
000297*    -------------------------------
000298     05  MTBY9L PIC S9(0004) COMP.
000299     05  MTBY9F PIC  X(0001).
000300     05  FILLER REDEFINES MTBY9F.
000301         10  MTBY9A PIC  X(0001).
000302     05  MTBY9I PIC  X(0004).
000303*    -------------------------------
000304     05  MTDT9L PIC S9(0004) COMP.
000305     05  MTDT9F PIC  X(0001).
000306     05  FILLER REDEFINES MTDT9F.
000307         10  MTDT9A PIC  X(0001).
000308     05  MTDT9I PIC  X(0006).
000309*    -------------------------------
000310     05  LN10L PIC S9(0004) COMP.
000311     05  LN10F PIC  X(0001).
000312     05  FILLER REDEFINES LN10F.
000313         10  LN10A PIC  X(0001).
000314     05  LN10I PIC  X(0003).
000315*    -------------------------------
000316     05  FNLIN10L PIC S9(0004) COMP.
000317     05  FNLIN10F PIC  X(0001).
000318     05  FILLER REDEFINES FNLIN10F.
000319         10  FNLIN10A PIC  X(0001).
000320     05  FNLIN10I PIC  X(0063).
000321*    -------------------------------
000322     05  MTBY10L PIC S9(0004) COMP.
000323     05  MTBY10F PIC  X(0001).
000324     05  FILLER REDEFINES MTBY10F.
000325         10  MTBY10A PIC  X(0001).
000326     05  MTBY10I PIC  X(0004).
000327*    -------------------------------
000328     05  MTDT10L PIC S9(0004) COMP.
000329     05  MTDT10F PIC  X(0001).
000330     05  FILLER REDEFINES MTDT10F.
000331         10  MTDT10A PIC  X(0001).
000332     05  MTDT10I PIC  X(0006).
000333*    -------------------------------
000334     05  ERRMSGBL PIC S9(0004) COMP.
000335     05  ERRMSGBF PIC  X(0001).
000336     05  FILLER REDEFINES ERRMSGBF.
000337         10  ERRMSGBA PIC  X(0001).
000338     05  ERRMSGBI PIC  X(0072).
000339*    -------------------------------
000340     05  FUNCTL PIC S9(0004) COMP.
000341     05  FUNCTF PIC  X(0001).
000342     05  FILLER REDEFINES FUNCTF.
000343         10  FUNCTA PIC  X(0001).
000344     05  FUNCTI PIC  X(0001).
000345*    -------------------------------
000346     05  LINE1L PIC S9(0004) COMP.
000347     05  LINE1F PIC  X(0001).
000348     05  FILLER REDEFINES LINE1F.
000349         10  LINE1A PIC  X(0001).
000350     05  LINE1I PIC  999.
000351*    -------------------------------
000352     05  LINE2L PIC S9(0004) COMP.
000353     05  LINE2F PIC  X(0001).
000354     05  FILLER REDEFINES LINE2F.
000355         10  LINE2A PIC  X(0001).
000356     05  LINE2I PIC  999.
000357*    -------------------------------
000358     05  PFENTERL PIC S9(0004) COMP.
000359     05  PFENTERF PIC  X(0001).
000360     05  FILLER REDEFINES PFENTERF.
000361         10  PFENTERA PIC  X(0001).
000362     05  PFENTERI PIC  99.
000363*    -------------------------------
000364     05  PF6NOTEL PIC S9(0004) COMP.
000365     05  PF6NOTEF PIC  X(0001).
000366     05  FILLER REDEFINES PF6NOTEF.
000367         10  PF6NOTEA PIC  X(0001).
000368     05  PF6NOTEI PIC  X(0017).
000369 01  EL1284AO REDEFINES EL1284AI.
000370     05  FILLER            PIC  X(0012).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  DATEO PIC  X(0008).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  TIMEO PIC  99.99.
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  CMPNYIDO PIC  X(0003).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  USERIDO PIC  X(0004).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  CLAIMNOO PIC  X(0007).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  TYPEO PIC  X(0006).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  CARRO PIC  X(0001).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  CERTNOO PIC  X(0011).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  FSTNMEO PIC  X(0012).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  LASTNMEO PIC  X(0015).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  TOTO PIC  ZZ9.
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  LNGHLAPO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  REWRITEO PIC  X(0001).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  CHKCOVGO PIC  X(0001).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  MRRELDTO PIC  X(0008).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  LN1O PIC  ZZ9.
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  FNLINE1O PIC  X(0063).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  MTBY1O PIC  X(0004).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  MTDT1O PIC  X(0006).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  LN2O PIC  ZZ9.
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  FNLINE2O PIC  X(0063).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  MTBY2O PIC  X(0004).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  MTDT2O PIC  X(0006).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  LN3O PIC  ZZ9.
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  FNLINE3O PIC  X(0063).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  MTBY3O PIC  X(0004).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  MTDT3O PIC  X(0006).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  LN4O PIC  ZZ9.
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  FNLINE4O PIC  X(0063).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  MTBY4O PIC  X(0004).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  MTDT4O PIC  X(0006).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  LN5O PIC  ZZ9.
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  FNLINE5O PIC  X(0063).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  MTBY5O PIC  X(0004).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  MTDT5O PIC  X(0006).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  LN6O PIC  ZZ9.
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  FNLINE6O PIC  X(0063).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  MTBY6O PIC  X(0004).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  MTDT6O PIC  X(0006).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  LN7O PIC  ZZ9.
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  FNLINE7O PIC  X(0063).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  MTBY7O PIC  X(0004).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  MTDT7O PIC  X(0006).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  LN8O PIC  ZZ9.
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  FNLINE8O PIC  X(0063).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  MTBY8O PIC  X(0004).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  MTDT8O PIC  X(0006).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  LN9O PIC  ZZ9.
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  FNLINE9O PIC  X(0063).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  MTBY9O PIC  X(0004).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  MTDT9O PIC  X(0006).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  LN10O PIC  ZZ9.
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  FNLIN10O PIC  X(0063).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  MTBY10O PIC  X(0004).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  MTDT10O PIC  X(0006).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  ERRMSGBO PIC  X(0072).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  FUNCTO PIC  X(0001).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  LINE1O PIC  X(0003).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  LINE2O PIC  X(0003).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  PFENTERO PIC  X(0002).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  PF6NOTEO PIC  X(0017).
000554*    -------------------------------
      *<<((file: EL1284S))
000222     EJECT
000223*01  EL1284R REDEFINES EL1284AI.
000224 66 GROUP2  RENAMES LN1L THRU MTDT10I.
000225 01  EL1284R.
000226*    12  FILLER                  PIC X(202).
000227     12  SC-ALL-LINES.
000228      14 SC-LINES OCCURS 10 TIMES INDEXED BY SC-INDX.
000229         16  SC-LINL             PIC S9(4)   COMP.
000230         16  SC-LINA             PIC X.
000231         16  SC-LIN              PIC ZZ9.
000232         16  SC-TEXTL            PIC S9(4)   COMP.
000233         16  SC-TEXTA            PIC X.
000234         16  SC-TEXT             PIC X(63).
000235         16  SC-MTBYL            PIC S9(4)   COMP.
000236         16  SC-MTBYA            PIC X.
000237         16  SC-MTBY             PIC X(4).
000238         16  SC-MTDTL            PIC S9(4)   COMP.
000239         16  SC-MTDTA            PIC X.
000240         16  SC-MTDT             PIC X(6).
000241*    12  FILLER                  PIC X(116).
000242     EJECT
000243 01  RECORD-TABLE                PIC X(21900) VALUE SPACES.
000244 01  REC-TABLE  REDEFINES RECORD-TABLE.
000245     12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).
000246 01  REC-ENTRIES REDEFINES RECORD-TABLE.
000247     12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
000248         16  REC-TEXT                    PIC X(63).
000249         16  REC-LAST-MAINT-BY           PIC XXXX.
000250         16  REC-LAST-MAINT-DT           PIC XX.
000251         16  REC-LAST-MAINT-HHMMSS       PIC S9(7) COMP-3.
000252
000253 01  TS-WORK-AREA                        PIC X(3650).
000254     EJECT
000255*        COPY ELCMSTR.
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
000256
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
000258 01  DFHCOMMAREA                 PIC X(1500).
000259
000260*        COPY ELCMEMO.
      *>>((file: ELCMEMO))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                           ELCMEMO.                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE           *
000006*                                                                *
000007*                                                                *
000008*   FILE DESCRIPTION = CLAIM MEMO NOTES                          *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 132    RECFORM = FIXED                         *
000012*                                                                *
000013*   BASE CLUSTER NAME = ELMEMO             RKP=2,LEN=23          *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019 01  CLAIM-MEMO-FILE.
000020     12  MM-RECORD-ID                  PIC  XX.
000021         88  VALID-MM-ID                    VALUE 'MM'.
000022     12  MM-CONTROL-PRIMARY.
000023       16  MM-COMPANY-CD               PIC  X.
000024       16  MM-RECORD-TYPE              PIC  X.
000025           88  MM-HDR-REC                   VALUE '1'.
000026           88  MM-DETAIL-REC                VALUE '2'.
000027       16  MM-CARRIER                  PIC  X.
000028       16  MM-CLAIM-NO                 PIC  X(7).
000029       16  MM-CERT-NO.
000030           20  MM-CERT-PRIME           PIC  X(10).
000031           20  MM-CERT-SFX             PIC  X.
000032       16  MM-PAYMENT-SEQ-NO           PIC  S9(4)  COMP.
000033
000034   12  MM-CLAIM-MEMO                   PIC  X(63).
000035   12  MM-HEADER-INFO REDEFINES MM-CLAIM-MEMO.
000036       16  MM-LONG-HEALTH-APP          PIC  X(01).
000037       16  MM-REWRITE-IND              PIC  X(01).
000038       16  MM-CHECKED-OTHER-COVG       PIC  X(01).
000039       16  MM-MR-RELEASED-FROM-DATE    PIC  X(02).
000040   12  FILLER                          PIC  X(34).
000041
000042   12  MM-LAST-MAINT-DT                PIC  XX.
000043   12  MM-LAST-MAINT-BY                PIC  X(4).
000044   12  MM-LAST-MAINT-HHMMSS            PIC  S9(6) COMP-3.
      *<<((file: ELCMEMO))
000261
000262*        COPY ELCCERT.
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
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1284' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000263 VCOBOL-DUMMY-PROCEDURE.
000264
000265     MOVE EIBTRMID               TO QID-TERM.
000266     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000267     MOVE '5'                    TO DC-OPTION-CODE.
000268     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000269     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000270     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000271
000272     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK
000273     IF EIBCALEN = ZEROS
000274         GO TO 8800-UNAUTHORIZED-ACCESS
000275     END-IF.
000276
000277*    IF PI-CALLING-PROGRAM NOT = THIS-PGM AND PGM-EL1276
000278*        PERFORM 4900-SET-NOTES-FLAG THRU 4900-EXIT
000279*        IF CLAIM-SESSION
000280*            SET PI-CLAIM-NOTE TO TRUE
000281*        ELSE
000282*            SET PI-CERT-NOTE TO TRUE
000283*        END-IF
000284*    END-IF.
000285
000286*    IF PI-CALLING-PROGRAM = PGM-EL1276
000287*        MOVE THIS-PGM           TO PI-CALLING-PROGRAM
000288*        MOVE 'N'                TO PI-PF5-PRESSED
000289*        MOVE 'N'                TO PI-PF6-PRESSED
000290*    END-IF.
000291
000292     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000293        IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000294           MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000295           MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000296           MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000297           MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000298           MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000299           MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000300           MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000301           MOVE THIS-PGM TO PI-CALLING-PROGRAM
000302        ELSE
000303           MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000304           MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000305           MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000306           MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000307           MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000308           MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000309           MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000310           MOVE SPACES               TO PI-SAVED-PROGRAM-6
000311        END-IF
000312     END-IF.
000313
000314 1000-START.
000315     MOVE 'N' TO WS-CURSOR-SET
000316
000317     MOVE LOW-VALUES TO EL1284AI
000318                        EL1284R
000319     MOVE SPACES                 TO  ELMEMO-KEY
000320                                     ELCERT-KEY.
000321
000322     MOVE PI-COMPANY-CD          TO  ELMEMO-COMPANY-CD
000323                                     ELCERT-COMPANY-CD.
000324     MOVE PI-CARRIER             TO  ELMEMO-CARRIER
000325                                     ELCERT-CARRIER.
000326     MOVE PI-CLAIM-NO            TO  ELMEMO-CLAIM-NO
000327     MOVE PI-CERT-PRIME          TO  ELMEMO-CERT-PRIME
000328                                     ELCERT-CERT-PRIME.
000329     MOVE PI-CERT-SFX            TO  ELMEMO-CERT-SFX
000330                                     ELCERT-CERT-SFX.
000331     MOVE ZEROS                  TO  ELMEMO-RECORD-TYPE
000332     MOVE ZEROS                  TO  ELMEMO-SEQ.
000333     MOVE ELMEMO-PARTIAL-KEY     TO  SV-PRIOR-KEY.
000334
000335     IF EIBTRNID NOT = TRANS-ID
000336        INITIALIZE PI-EL1284-AREA
000337        MOVE '0'                 TO  PI-UPDATE-SW
000338        IF PI-PROCESSOR-ID NOT = 'LGXX'
000339            
      * EXEC CICS READQ TS
000340*                    QUEUE (PI-SECURITY-TEMP-STORE-ID)
000341*                    INTO (SECURITY-CONTROL)
000342*                    LENGTH (SC-COMM-LENGTH)
000343*                    ITEM  (SC-ITEM)
000344*           END-EXEC
      *    MOVE '*$II   L              ''   #00002403' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032343033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000345          MOVE SC-CREDIT-DISPLAY (32) TO PI-DISPLAY-CAP
000346          MOVE SC-CREDIT-UPDATE (32)  TO PI-MODIFY-CAP
000347        END-IF
000348     END-IF.
000349
000350     IF NOT DISPLAY-CAP
000351         MOVE 'READ'             TO  SM-READ
000352         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000353         MOVE ER-0070            TO  EMI-ERROR
000354         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000355         GO TO 8100-SEND-INITIAL-MAP
000356     END-IF.
000357
000358     
      * EXEC CICS HANDLE AID
000359*         CLEAR(9400-CLEAR)
000360*    END-EXEC.
      *    MOVE '"&=                  V! " #00002422' TO DFHEIV0
           MOVE X'22263D202020202020202020' &
                X'202020202020202020562120' &
                X'2220233030303032343232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000361
000362     
      * EXEC CICS HANDLE CONDITION
000363*         ERROR(9990-ABEND)
000364*         PGMIDERR(9600-PGMID-ERROR)
000365*    END-EXEC.
      *    MOVE '"$.L                  ! # #00002426' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303032343236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000366
000367     IF EIBTRNID NOT = TRANS-ID
000368         GO TO 7000-BUILD-TABLE
000369     END-IF.
000370
000371     IF PI-CHANGE-IN-NOTE-TYPE
000372         GO TO 7000-BUILD-TABLE
000373     END-IF.
000374
000375     EJECT
000376 2000-RECEIVE.
000377     IF EIBAID = DFHPA1 OR
000378        EIBAID = DFHPA2 OR
000379        EIBAID = DFHPA3
000380           MOVE ER-0008 TO EMI-ERROR
000381           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000382           GO TO 8200-SEND-DATAONLY
000383     END-IF.
000384
000385     
      * EXEC CICS RECEIVE
000386*         MAP(MAP-NAME)
000387*         MAPSET(MAPSET-NAME)
000388*         INTO(EL1284AI)
000389*    END-EXEC.
           MOVE LENGTH OF
            EL1284AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002449' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032343439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000390     MOVE GROUP2 TO EL1284R.
000391
000392
000393     IF PFENTERL = ZEROS
000394        GO TO 2001-CHECK-PFKEYS
000395     END-IF.
000396
000397     IF EIBAID NOT = DFHENTER
000398        MOVE ER-0004             TO EMI-ERROR
000399        GO TO 2002-INPUT-ERROR
000400     END-IF.
000401
000402     IF PFENTERI NUMERIC AND
000403        (PFENTERI > 00 AND  < 25)
000404        MOVE PF-VALUES (PFENTERI) TO EIBAID
000405     ELSE
000406        MOVE ER-0029 TO EMI-ERROR
000407        GO TO 2002-INPUT-ERROR
000408     END-IF.
000409
000410 2001-CHECK-PFKEYS.
000411     IF EIBAID = DFHPF23
000412        GO TO 9000-RETURN-CICS
000413     END-IF.
000414
000415     IF EIBAID = DFHPF24
000416        GO TO 9200-RETURN-MAIN-MENU
000417     END-IF.
000418
000419     IF EIBAID = DFHPF12
000420        GO TO 9500-PF12
000421     END-IF.
000422
000423     IF FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER
000424        IF FUNCTI = 'A' OR = SPACES
000425           NEXT SENTENCE
000426        ELSE
000427           MOVE ER-0050          TO EMI-ERROR
000428           MOVE -1 TO FUNCTL
000429           MOVE AL-UABON TO FUNCTA PFENTERA
000430           GO TO 2002-INPUT-ERROR
000431        END-IF
000432     END-IF.
000433
000434     IF EIBAID = DFHPF1
000435        MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER
000436        GO TO 7400-PAGE-ROUTINE
000437     END-IF.
000438
000439     IF EIBAID = DFHPF2
000440        SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER
000441        GO TO 7400-PAGE-ROUTINE
000442     END-IF.
000443
000444     IF EIBAID = DFHPF3
000445        MOVE 5                   TO ROLL-COUNTER
000446        GO TO 7400-PAGE-ROUTINE
000447     END-IF.
000448
000449     IF EIBAID = DFHPF4
000450        MOVE -5                  TO ROLL-COUNTER
000451        GO TO 7400-PAGE-ROUTINE
000452     END-IF.
000453
000454     IF EIBAID = DFHENTER
000455        GO TO 2003-EDIT-DATA
000456     END-IF.
000457
000458     MOVE ER-0029                TO EMI-ERROR.
000459
000460 2002-INPUT-ERROR.
000461     MOVE -1                     TO PFENTERL
000462     MOVE AL-UNBON               TO PFENTERA
000463     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000464     GO TO 8200-SEND-DATAONLY.
000465
000466 2003-EDIT-DATA.
000467
000468     IF FUNCTI = 'L'
000469         NEXT SENTENCE
000470     ELSE
000471         IF NOT MODIFY-CAP
000472             MOVE 'UPDATE'       TO  SM-READ
000473             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000474             MOVE ER-0070        TO  EMI-ERROR
000475             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000476             GO TO 8100-SEND-INITIAL-MAP
000477         END-IF
000478     END-IF.
000479
000480     IF FUNCTI NOT = 'Q'
000481        PERFORM 2010-EDIT-CHECK
000482     END-IF.
000483
000484     IF FUNCTL = ZEROS OR FUNCTI = SPACES
000485        GO TO 4000-CHANGE-ROUTINE
000486     END-IF.
000487
000488     IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR
000489                = 'I' OR = 'A' OR = 'L')
000490         NEXT SENTENCE
000491     ELSE
000492         MOVE ER-0023            TO EMI-ERROR
000493         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000494         MOVE AL-UABON           TO FUNCTA
000495         MOVE -1                 TO FUNCTL
000496         GO TO 8200-SEND-DATAONLY
000497     END-IF.
000498
000499     IF FUNCTI = 'D'  OR = 'I' OR = 'L'
000500        PERFORM 2500-LINE-CHECK THRU 2599-EXIT
000501     ELSE
000502        IF LINE1L NOT = ZEROS OR
000503           LINE2L NOT = ZEROS
000504           MOVE ER-0030          TO EMI-ERROR
000505           MOVE -1               TO LINE1L
000506           MOVE AL-UNBON         TO LINE1A LINE2A
000507           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000508           GO TO 8200-SEND-DATAONLY
000509        END-IF
000510     END-IF.
000511
000512
000513     IF FUNCTI = 'A'
000514        GO TO 5000-ADD-NEW-LINES
000515     END-IF.
000516     IF FUNCTI = 'Q'
000517        GO TO 9410-RETURN
000518     END-IF.
000519     IF FUNCTI = 'S'
000520        GO TO 4500-SAVE-DATA
000521     END-IF.
000522     IF PI-TOTAL-LINES = 0
000523        MOVE ER-0048             TO EMI-ERROR
000524        MOVE -1                  TO FUNCTL
000525        MOVE AL-UNBON            TO FUNCTA
000526        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000527        GO TO 8200-SEND-DATAONLY
000528     END-IF.
000529     IF FUNCTI = 'L'
000530        GO TO 5500-LOOKUP
000531     END-IF.
000532     IF FUNCTI = 'D'
000533        GO TO 3000-DELETE-LINES
000534     END-IF.
000535
000536     GO TO 3500-INSERT-LINES.
000537
000538     EJECT
000539 2010-EDIT-CHECK.
000540*
000541     IF LNGHLAPL > 0
000542        MOVE LNGHLAPI TO  PI-LONG-HLTH-APP
000543        MOVE '1'      TO PI-UPDATE-SW
000544     ELSE
000545        IF LNGHLAPA = WS-ERASE-EOF
000546           MOVE SPACES TO PI-LONG-HLTH-APP
000547           MOVE '1'    TO PI-UPDATE-SW
000548        END-IF
000549        MOVE PI-LONG-HLTH-APP TO LNGHLAPI
000550     END-IF.
000551*
000552     IF LNGHLAPI > SPACE
000553        IF LNGHLAPI = 'N' OR 'Y'
000554           CONTINUE
000555        ELSE
000556           MOVE -1                     TO LNGHLAPL
000557           MOVE AL-UABON               TO LNGHLAPA
000558           MOVE ER-0046                TO EMI-ERROR
000559           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000560           GO TO 8200-SEND-DATAONLY
000561        END-IF
000562     END-IF.
000563
000564*
000565     IF REWRITEL > 0
000566        MOVE REWRITEI TO PI-REWRITE
000567        MOVE '1'      TO PI-UPDATE-SW
000568     ELSE
000569        IF REWRITEA = WS-ERASE-EOF
000570           MOVE SPACES TO PI-REWRITE
000571           MOVE '1'    TO PI-UPDATE-SW
000572        END-IF
000573        MOVE PI-REWRITE TO REWRITEI
000574     END-IF
000575*
000576     IF REWRITEI > SPACE
000577        IF REWRITEI = 'N' OR 'Y' OR 'D'
000578           CONTINUE
000579        ELSE
000580           MOVE -1                     TO REWRITEL
000581           MOVE AL-UABON               TO REWRITEA
000582           MOVE ER-3846                TO EMI-ERROR
000583           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000584           GO TO 8200-SEND-DATAONLY
000585        END-IF
000586     END-IF.
000587*
000588     IF CHKCOVGL > 0
000589        MOVE CHKCOVGI TO PI-CHKCOVG
000590        MOVE '1'      TO PI-UPDATE-SW
000591     ELSE
000592        IF CHKCOVGA = WS-ERASE-EOF
000593           MOVE SPACES TO PI-CHKCOVG
000594           MOVE '1'    TO PI-UPDATE-SW
000595        END-IF
000596        MOVE PI-CHKCOVG TO CHKCOVGI
000597     END-IF
000598*
000599     IF CHKCOVGI > SPACE
000600        IF CHKCOVGI = 'N' OR 'Y'
000601           CONTINUE
000602        ELSE
000603           MOVE -1                     TO CHKCOVGL
000604           MOVE AL-UABON               TO CHKCOVGA
000605           MOVE ER-0046                TO EMI-ERROR
000606           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000607           GO TO 8200-SEND-DATAONLY
000608        END-IF
000609     END-IF.
000610*
000611     IF MRRELDTL > 0
000612        MOVE MRRELDTI TO PI-MR-RELEASE-DATE
000613        MOVE '1'      TO PI-UPDATE-SW
000614     ELSE
000615        IF MRRELDTA = WS-ERASE-EOF
000616           MOVE SPACES TO PI-MR-RELEASE-DATE
000617           MOVE '1'    TO PI-UPDATE-SW
000618        END-IF
000619        MOVE PI-MR-RELEASE-DATE TO MRRELDTI
000620     END-IF.
000621*
000622 2500-LINE-CHECK.
000623     IF LINE1L = ZEROS AND
000624        LINE2L = ZEROS
000625        MOVE ER-0069             TO EMI-ERROR
000626        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000627        MOVE -1                  TO LINE1L
000628        GO TO 8200-SEND-DATAONLY
000629     END-IF.
000630
000631     IF LINE1L NOT = ZEROS
000632        IF LINE1I = ZERO AND FUNCTI EQUAL 'L'
000633            MOVE 1               TO LINE1I
000634        END-IF
000635        IF LINE1I = ZERO AND FUNCTI EQUAL 'D'
000636           MOVE ER-0049          TO EMI-ERROR
000637           MOVE AL-UNBON         TO LINE1A
000638           MOVE -1               TO LINE1L
000639           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000640           GO TO 8200-SEND-DATAONLY
000641        END-IF
000642        IF LINE1I NOT NUMERIC OR
000643           LINE1I > PI-TOTAL-LINES
000644           MOVE ER-0031          TO EMI-ERROR
000645           MOVE AL-UNBON         TO LINE1A
000646           MOVE -1               TO LINE1L
000647           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000648           GO TO 8200-SEND-DATAONLY
000649        ELSE
000650           IF LINE2L = ZEROS
000651              MOVE 1             TO LINE2I
000652              IF FUNCTI = 'I'
000653                  GO TO 2510-MAX-CHECK
000654              ELSE
000655                  NEXT SENTENCE
000656              END-IF
000657           ELSE
000658              IF FUNCTI = 'I'
000659                 GO TO 2510-MAX-CHECK
000660              ELSE
000661                 IF LINE2I NOT NUMERIC
000662                    MOVE AL-UNBON TO LINE2A
000663                    MOVE ER-0032  TO EMI-ERROR
000664                    MOVE -1       TO LINE2L
000665                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000666                    GO TO 8200-SEND-DATAONLY
000667                 ELSE
000668                    NEXT SENTENCE
000669                 END-IF
000670              END-IF
000671           END-IF
000672        END-IF
000673     ELSE
000674        IF LINE2L = ZEROS
000675           NEXT SENTENCE
000676        ELSE
000677           MOVE -1               TO LINE2L
000678           MOVE ER-0041          TO EMI-ERROR
000679           MOVE AL-UNBON         TO LINE2A
000680           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000681           GO TO 8200-SEND-DATAONLY
000682        END-IF
000683     END-IF.
000684     GO TO 2599-EXIT.
000685 2510-MAX-CHECK.
000686     IF LINE2I NOT NUMERIC
000687        MOVE -1                  TO LINE2L
000688        MOVE ER-0032             TO EMI-ERROR
000689        MOVE AL-UNBON            TO LINE2A
000690        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000691        GO TO 8200-SEND-DATAONLY
000692     ELSE
000693        COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES
000694        IF ROLL-COUNTER GREATER THAN MAX-LINES
000695           MOVE -1               TO LINE2L
000696           MOVE ER-0044          TO EMI-ERROR
000697           MOVE AL-UNBON         TO LINE2A
000698           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000699           GO TO 8200-SEND-DATAONLY
000700        END-IF
000701     END-IF.
000702 2599-EXIT.
000703      EXIT.
000704     EJECT
000705 3000-DELETE-LINES.
000706     IF LINE2L = ZEROS AND LINE2I = 1
000707        MOVE LINE1I              TO LINE2I
000708     END-IF.
000709
000710     IF LINE2I > PI-TOTAL-LINES OR < LINE1I
000711        MOVE ER-0049             TO EMI-ERROR
000712        MOVE AL-UNBON            TO LINE2A
000713        MOVE -1                  TO LINE2L
000714        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000715        GO TO 8200-SEND-DATAONLY
000716     END-IF.
000717
000718     PERFORM 7450-SET-INDX THRU 7450-EXIT.
000719     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
000720             VARYING SC-INDX FROM 1 BY 1 UNTIL
000721             SC-INDX > NUM-LINES-PER-SCREEN
000722     SET TB-INDX TO LINE1I.
000723
000724     IF NOT EMI-NO-ERRORS
000725        GO TO 8200-SEND-DATAONLY
000726     END-IF.
000727
000728     SET TB-INDX TO LINE1I
000729     COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.
000730
000731     IF LINE2I NOT = PI-TOTAL-LINES
000732        SET TB-INDX1 TO LINE2I
000733        SET TB-INDX1 UP BY 1
000734        PERFORM 3100-DELETE-TABLE-ENTRIES
000735                UNTIL TB-INDX1 > PI-TOTAL-LINES
000736     END-IF.
000737
000738     PERFORM 3150-BLANK-TABLE-ENTRIES
000739             ROLL-COUNTER TIMES.
000740     SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.
000741
000742     IF PI-CURRENT-LINE > PI-TOTAL-LINES
000743        MOVE PI-TOTAL-LINES      TO PI-CURRENT-LINE
000744        SUBTRACT 1 FROM PI-CURRENT-LINE
000745     END-IF.
000746
000747     SET TB-INDX  TO PI-CURRENT-LINE
000748     MOVE LOW-VALUES             TO EL1284AI
000749                                    EL1284R
000750
000751     PERFORM 7800-MOVE-SAVE-TO-MAP.
000752     IF PI-CURRENT-LINE > ZERO
000753         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
000754             VARYING SC-INDX FROM 1 BY 1 UNTIL
000755             SC-INDX > NUM-LINES-PER-SCREEN
000756         PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT
000757     END-IF.
000758
000759     MOVE '1'                    TO PI-UPDATE-SW.
000760     IF PI-TOTAL-LINES = ZEROS
000761        MOVE ZEROS               TO PI-CURRENT-LINE
000762     END-IF.
000763
000764     MOVE -1 TO FUNCTL
000765     SET CURSOR-SET TO TRUE
000766
000767     GO TO 8100-SEND-INITIAL-MAP.
000768     EJECT
000769
000770 3100-DELETE-TABLE-ENTRIES.
000771     MOVE REC-ENT (TB-INDX1)     TO REC-ENT (TB-INDX)
000772     SET TB-INDX TB-INDX1 UP BY 1.
000773
000774 3150-BLANK-TABLE-ENTRIES.
000775     MOVE SPACES               TO REC-ENT (TB-INDX).
000776     MOVE SAVE-BIN-DATE        TO REC-LAST-MAINT-DT (TB-INDX).
000777     MOVE EIBTIME              TO REC-LAST-MAINT-HHMMSS (TB-INDX).
000778     MOVE PI-PROCESSOR-ID      TO REC-LAST-MAINT-BY (TB-INDX).
000779     SET TB-INDX UP BY 1.
000780     EJECT
000781 3500-INSERT-LINES.
000782
000783     PERFORM 7450-SET-INDX THRU 7450-EXIT.
000784     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
000785             VARYING SC-INDX FROM 1 BY 1 UNTIL
000786             SC-INDX > NUM-LINES-PER-SCREEN.
000787
000788     IF NOT EMI-NO-ERRORS
000789        GO TO 8200-SEND-DATAONLY
000790     END-IF.
000791
000792     SET TB-INDX TO PI-TOTAL-LINES.
000793     ADD LINE2I TO PI-TOTAL-LINES.
000794     SET TB-INDX1 TO PI-TOTAL-LINES.
000795     PERFORM 3600-INSERT-TABLE-ENTRIES
000796             UNTIL TB-INDX = LINE1I.
000797     SET TB-INDX UP BY 1.
000798
000799     IF LINE1I EQUAL ZERO
000800         SET PI-CURRENT-LINE TO 1
000801     ELSE
000802         SET PI-CURRENT-LINE TO LINE1I
000803     END-IF.
000804
000805     COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +
000806                            NUM-LINES-PER-SCREEN.
000807     IF TB-INDX NOT LESS THAN ROLL-COUNTER OR
000808                    LESS THAN PI-CURRENT-LINE
000809        SET SC-INDX TO 1
000810        SET SC-INDX DOWN BY 1
000811     ELSE
000812        SET ROLL-COUNTER TO TB-INDX
000813        COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE
000814                  + 1
000815        SET SC-INDX TO ROLL-COUNTER
000816     END-IF.
000817
000818     PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.
000819     SET TB-INDX TO PI-CURRENT-LINE.
000820     MOVE LOW-VALUES             TO EL1284AI
000821                                    EL1284R
000822
000823     IF SC-INDX NOT = ZERO
000824        MOVE -1 TO SC-TEXTL (SC-INDX)
000825        SET CURSOR-SET TO TRUE
000826     END-IF.
000827
000828     PERFORM 7800-MOVE-SAVE-TO-MAP.
000829
000830     PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
000831            VARYING SC-INDX FROM 1 BY 1 UNTIL
000832            SC-INDX > NUM-LINES-PER-SCREEN.
000833     PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT.
000834     MOVE '1'                    TO PI-UPDATE-SW.
000835
000836     MOVE -1 TO FUNCTL
000837     SET CURSOR-SET TO TRUE
000838
000839     GO TO 8100-SEND-INITIAL-MAP.
000840
000841 3600-INSERT-TABLE-ENTRIES.
000842     MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).
000843     SET TB-INDX TB-INDX1 DOWN BY 1.
000844     EJECT
000845
000846 4000-CHANGE-ROUTINE.
000847     PERFORM 7450-SET-INDX THRU 7450-EXIT.
000848     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
000849             VARYING SC-INDX FROM 1 BY 1 UNTIL
000850             SC-INDX > NUM-LINES-PER-SCREEN.
000851
000852     IF NOT EMI-NO-ERRORS
000853        GO TO 8200-SEND-DATAONLY
000854     END-IF.
000855
000856     PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
000857     MOVE SPACES                 TO ERRMSGBO.
000858     GO TO 8200-SEND-DATAONLY.
000859
000860     EJECT
000861 4500-SAVE-DATA.
000862     PERFORM 7450-SET-INDX THRU 7450-EXIT.
000863     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
000864             VARYING SC-INDX FROM 1 BY 1 UNTIL
000865             SC-INDX > NUM-LINES-PER-SCREEN.
000866     IF NOT EMI-NO-ERRORS
000867        GO TO 8200-SEND-DATAONLY
000868     END-IF.
000869
000870     
      * EXEC CICS HANDLE CONDITION
000871*         NOTFND(4610-ENDBR)
000872*         NOTOPEN(6000-NOT-OPEN)
000873*         ENDFILE(4610-ENDBR)
000874*    END-EXEC.
      *    MOVE '"$IJ''                 ! $ #00002934' TO DFHEIV0
           MOVE X'2224494A2720202020202020' &
                X'202020202020202020202120' &
                X'2420233030303032393334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000875
000876 4610-LOOP.
000877     
      * EXEC CICS READ
000878*        DATASET (ELMEMO-FILE-ID)
000879*        RIDFLD  (ELMEMO-KEY)
000880*        SET     (ADDRESS OF CLAIM-MEMO-FILE)
000881*        GTEQ
000882*    END-EXEC.
      *    MOVE '&"S        G          (   #00002941' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303032393431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000883
000884     MOVE MM-CONTROL-PRIMARY     TO ELMEMO-KEY.
000885
000886     IF ELMEMO-PARTIAL-KEY NOT = SV-PRIOR-KEY
000887         MOVE SV-PRIOR-KEY       TO ELMEMO-PARTIAL-KEY
000888         GO TO 4610-ENDBR
000889     END-IF.
000890
000891     
      * EXEC CICS DELETE
000892*        DATASET (ELMEMO-FILE-ID)
000893*        RIDFLD  (ELMEMO-KEY)
000894*    END-EXEC.
      *    MOVE '&(  R                 &   #00002955' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'2020233030303032393535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000895
000896     GO TO 4610-LOOP.
000897 4610-ENDBR.
000898     
      * EXEC CICS GETMAIN
000899*         LENGTH(ELMEMO-LENGTH)
000900*         SET(ADDRESS OF CLAIM-MEMO-FILE)
000901*         INITIMG(GETMAIN-SPACE)
000902*    END-EXEC.
      *    MOVE ',"IL                  $   #00002962' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303032393632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELMEMO-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000903
000904     PERFORM 4650-WRITE-HEADER THRU 4650-EXIT.
000905
000906     MOVE 1                      TO  ELMEMO-SEQ.
000907     MOVE ZERO                   TO  WS-BLANK-LINES.
000908
000909     PERFORM 4700-WRITE-FILE THRU 4799-EXIT
000910             VARYING TB-INDX FROM 1 BY 1 UNTIL
000911             TB-INDX > PI-TOTAL-LINES.
000912
000913     SUBTRACT WS-BLANK-LINES FROM PI-TOTAL-LINES.
000914
000915     GO TO 9410-RETURN.
000916
000917 4650-WRITE-HEADER.
000918
000919     MOVE SPACES                 TO  CLAIM-MEMO-FILE.
000920     MOVE ZERO                   TO  ELMEMO-SEQ.
000921     MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.
000922     MOVE  'MM'                  TO  MM-RECORD-ID.
000923     MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO
000924     MOVE LNGHLAPI               TO MM-LONG-HEALTH-APP
000925     MOVE REWRITEI               TO MM-REWRITE-IND
000926     MOVE CHKCOVGI               TO MM-CHECKED-OTHER-COVG
000927     IF MRRELDTI > SPACES
000928        MOVE MRRELDTI               TO DEEDIT-DATE-INPUT
000929        
      * EXEC CICS BIF DEEDIT
000930*           FIELD    (DEEDIT-DATE-INPUT)
000931*           LENGTH   (DATE-LENGTH)
000932*       END-EXEC
      *    MOVE '@"L                   #   #00002993' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032393933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000933        MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
000934        MOVE '4'            TO DC-OPTION-CODE
000935        PERFORM PERFORM 9700-LINK-DATE-CONVERT
000936        IF NOT DATE-CONVERSION-ERROR
000937            MOVE DC-BIN-DATE-1      TO MM-MR-RELEASED-FROM-DATE
000938*           MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
000939*           MOVE AL-UANON           TO INCA
000940*           MOVE DC-GREG-DATE-1-EDIT TO INCO
000941        ELSE
000942            MOVE -1         TO MRRELDTL
000943            MOVE ER-0314    TO EMI-ERROR
000944            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000945            GO TO 8200-SEND-DATAONLY
000946        END-IF
000947     END-IF.
000948
000949*        MOVE AL-UABON   TO INCA
000950*        MOVE -1         TO INCL
000951*        MOVE 'X'        TO ERROR-SWITCH.
000952
000953
000954     
      * EXEC CICS WRITE
000955*         DATASET(ELMEMO-FILE-ID)
000956*         FROM(CLAIM-MEMO-FILE)
000957*         RIDFLD(ELMEMO-KEY)
000958*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MEMO-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003018' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033303138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 CLAIM-MEMO-FILE, 
                 DFHEIV11, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000959
000960 4650-EXIT.
000961      EXIT.
000962
000963
000964 4700-WRITE-FILE.
000965*    IF REC-TEXT (TB-INDX)  EQUAL SPACES
000966*        ADD +1                  TO  WS-BLANK-LINES
000967*        GO TO 4799-EXIT
000968*    END-IF.
000969
000970     MOVE SPACES                 TO  CLAIM-MEMO-FILE.
000971     ADD 1                       TO  ELMEMO-SEQ.
000972     MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.
000973     MOVE  'MM'                  TO  MM-RECORD-ID.
000974     MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO
000975     MOVE REC-LAST-MAINT-BY (TB-INDX)
000976                                 TO  MM-LAST-MAINT-BY
000977     MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)
000978                                 TO  MM-LAST-MAINT-HHMMSS.
000979     MOVE REC-LAST-MAINT-DT (TB-INDX)
000980                                 TO  MM-LAST-MAINT-DT.
000981
000982     
      * EXEC CICS WRITE
000983*         DATASET(ELMEMO-FILE-ID)
000984*         FROM(CLAIM-MEMO-FILE)
000985*         RIDFLD(ELMEMO-KEY)
000986*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MEMO-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003046' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033303436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 CLAIM-MEMO-FILE, 
                 DFHEIV11, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000987 4799-EXIT.
000988      EXIT.
000989
000990     EJECT
000991
000992 4900-SET-NOTES-FLAG.
000993
000994     
      * EXEC CICS HANDLE CONDITION
000995*        NOTFND   (4900-EXIT)
000996*    END-EXEC.
      *    MOVE '"$I                   ! % #00003058' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303033303538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000997
000998     MOVE SPACES                 TO  ELCERT-KEY.
000999     MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
001000     MOVE PI-CARRIER             TO  ELCERT-CARRIER.
001001     MOVE PI-GROUPING            TO  ELCERT-GROUPING.
001002     MOVE PI-STATE               TO  ELCERT-STATE.
001003     MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
001004     MOVE PI-CERT-EFF-DT         TO  ELCERT-EFF-DT.
001005     MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
001006     MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
001007
001008     
      * EXEC CICS READ
001009*    EQUAL
001010*    DATASET   (ELCERT-FILE-ID)
001011*    SET       (ADDRESS OF CERTIFICATE-MASTER)
001012*    RIDFLD    (ELCERT-KEY)
001013*    END-EXEC.
      *    MOVE '&"S        E          (   #00003072' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033303732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001014
001015*    MOVE 'N'                    TO PI-BILLING-NOTES-EXIST
001016*                                   PI-CERT-NOTES-EXIST
001017*                                   PI-CLAIM-NOTES-EXIST.
001018*    IF CM-NOTE-SW EQUAL '2' OR '3' OR '6' OR '7'
001019*         MOVE 'Y'               TO PI-BILLING-NOTES-EXIST
001020*    END-IF.
001021*    IF CM-NOTE-SW EQUAL '1' OR '3' OR '5' OR '7'
001022*         MOVE 'Y'               TO PI-CERT-NOTES-EXIST
001023*    END-IF.
001024*    IF CM-NOTE-SW EQUAL '4' OR '5' OR '6' OR '7'
001025*         MOVE 'Y'               TO PI-CLAIM-NOTES-EXIST
001026*    END-IF.
001027
001028
001029 4900-EXIT.
001030      EXIT.
001031
001032     EJECT
001033 5000-ADD-NEW-LINES.
001034     PERFORM 7450-SET-INDX THRU 7450-EXIT.
001035     MOVE 'N'                    TO WS-SCREEN-LINE.
001036     MOVE 1                      TO WS-SUB.
001037     MOVE ZERO                   TO WS-SUB1.
001038     PERFORM VARYING SC-INDX FROM NUM-LINES-PER-SCREEN BY -1
001039             UNTIL SCREEN-LINE-FOUND OR SC-INDX < 1
001040         IF SC-TEXT (SC-INDX) >  SPACES
001041             SET WS-SUB          TO SC-INDX
001042             MOVE 'Y'            TO WS-SCREEN-LINE
001043         END-IF
001044     END-PERFORM.
001045     IF PI-TOTAL-LINES = 0
001046         MOVE 1                  TO WS-SUB1
001047     ELSE
001048         MOVE 2                  TO WS-SUB1
001049     END-IF.
001050     PERFORM VARYING SC-INDX FROM WS-SUB1 BY 1
001051             UNTIL SC-INDX GREATER THAN WS-SUB
001052         IF SC-TEXTL (SC-INDX) EQUAL ZEROS
001053             MOVE 1      TO SC-TEXTL (SC-INDX)
001054             MOVE SPACES TO SC-TEXT (SC-INDX)
001055             MOVE LOW-VALUES TO SC-MTBY (SC-INDX)
001056                                SC-MTDT (SC-INDX)
001057         END-IF
001058     END-PERFORM.
001059     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
001060             VARYING SC-INDX FROM 1 BY 1 UNTIL
001061             SC-INDX > NUM-LINES-PER-SCREEN.
001062
001063     IF NOT EMI-NO-ERRORS
001064        GO TO 8200-SEND-DATAONLY
001065     END-IF.
001066
001067     MOVE PI-TOTAL-LINES         TO  PI-CURRENT-LINE.
001068     PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
001069     MOVE LOW-VALUES             TO  EL1284AI
001070                                     EL1284R
001071     PERFORM 7800-MOVE-SAVE-TO-MAP.
001072
001073     SET TB-INDX TO PI-CURRENT-LINE.
001074     MOVE 'A'                    TO FUNCTI.
001075     MOVE -1                     TO  SC-TEXTL (2).
001076     SET CURSOR-SET TO TRUE
001077     MOVE AL-UANON               TO  FUNCTA.
001078     PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
001079             VARYING SC-INDX FROM 1 BY 1 UNTIL
001080             SC-INDX > NUM-LINES-PER-SCREEN.
001081     MOVE '1'                    TO PI-UPDATE-SW.
001082
001083     GO TO 8100-SEND-INITIAL-MAP.
001084     EJECT
001085 5500-LOOKUP.
001086     PERFORM 7500-READ-TS THRU 7599-EXIT.
001087     SET TB-INDX TO PI-CURRENT-LINE.
001088     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
001089             VARYING SC-INDX FROM 1 BY 1 UNTIL
001090             SC-INDX > NUM-LINES-PER-SCREEN.
001091
001092     IF NOT EMI-NO-ERRORS
001093        GO TO 8200-SEND-DATAONLY
001094     END-IF.
001095
001096     MOVE LINE1I                 TO  PI-CURRENT-LINE.
001097     SET TB-INDX                 TO PI-CURRENT-LINE.
001098     MOVE LOW-VALUES             TO  EL1284AI
001099                                     EL1284R
001100     PERFORM 7800-MOVE-SAVE-TO-MAP.
001101
001102     PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
001103             VARYING SC-INDX FROM 1 BY 1
001104             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
001105     PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
001106
001107     MOVE -1 TO FUNCTL
001108     SET CURSOR-SET TO TRUE
001109
001110     GO TO 8100-SEND-INITIAL-MAP.
001111     EJECT
001112 6000-NOT-OPEN.
001113     MOVE ER-2954                TO  EMI-ERROR.
001114     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001115
001116     IF EIBAID = DFHCLEAR
001117         GO TO 9410-RETURN
001118     ELSE
001119         GO TO 8100-SEND-INITIAL-MAP
001120     END-IF.
001121     EJECT
001122 7000-BUILD-TABLE.
001123
001124     SET TB-INDX TO 1.
001125     MOVE ZEROS                  TO  PI-TOTAL-LINES
001126                                     PI-CURRENT-LINE
001127                                     PI-TEMP-STOR-ITEMS
001128                                     PI-UPDATE-SW.
001129     MOVE LOW-VALUES             TO  EL1284AI
001130                                     EL1284R
001131
001132****IF TEMP STORAGE EXISTS, DELETE IT.
001133     IF PI-CHANGE-IN-NOTE-TYPE
001134         MOVE 'N' TO PI-SET-NOTE-CHANGE
001135     ELSE
001136         PERFORM 7500-READ-TS THRU 7599-EXIT
001137     END-IF.
001138
001139     IF PI-TEMP-STOR-ITEMS NOT = ZERO
001140        PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT
001141     END-IF.
001142
001143     
      * EXEC CICS HANDLE CONDITION
001144*         NOTFND(7010-ENDBR)
001145*         NOTOPEN(6000-NOT-OPEN)
001146*         ENDFILE(7010-ENDBR)
001147*    END-EXEC.
      *    MOVE '"$IJ''                 ! & #00003207' TO DFHEIV0
           MOVE X'2224494A2720202020202020' &
                X'202020202020202020202120' &
                X'2620233030303033323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001148
001149     
      * EXEC CICS STARTBR
001150*         DATASET(ELMEMO-FILE-ID)
001151*         RIDFLD(ELMEMO-KEY)
001152*         KEYLENGTH(ELMEMO-START-LENGTH)
001153*         GENERIC
001154*         GTEQ
001155*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &   #00003213' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303033323133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 ELMEMO-KEY, 
                 ELMEMO-START-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001156
001157 7001-LOOP.
001158     
      * EXEC CICS READNEXT
001159*         SET(ADDRESS OF CLAIM-MEMO-FILE)
001160*         DATASET(ELMEMO-FILE-ID)
001161*         RIDFLD(ELMEMO-KEY)
001162*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003222' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303033323232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001163
001164     IF MM-COMPANY-CD NOT = SV-COMPANY-CD
001165         GO TO 7010-ENDBR
001166     END-IF.
001167
001168     IF (MM-CARRIER = SV-CARRIER)
001169        AND (MM-CERT-NO = SV-CERT-NO)
001170        AND (MM-CLAIM-NO = SV-CLAIM-NO)
001171        AND (MM-RECORD-TYPE = SV-RECORD-TYPE)
001172        IF MM-PAYMENT-SEQ-NO = ZERO
001173           MOVE MM-LONG-HEALTH-APP TO LNGHLAPO
001174                                      PI-LONG-HLTH-APP
001175           MOVE MM-REWRITE-IND TO REWRITEO
001176                                  PI-REWRITE
001177           MOVE MM-CHECKED-OTHER-COVG TO CHKCOVGO
001178                                         PI-CHKCOVG
001179           MOVE MM-MR-RELEASED-FROM-DATE TO DC-BIN-DATE-1
001180           MOVE ' '                    TO DC-OPTION-CODE
001181           PERFORM 9700-LINK-DATE-CONVERT
001182           IF DC-EDIT1-MONTH > ZERO
001183              MOVE DC-GREG-DATE-1-EDIT    TO MRRELDTO
001184                                             PI-MR-RELEASE-DATE
001185           END-IF
001186        ELSE
001187           MOVE MM-CLAIM-MEMO TO REC-TEXT (TB-INDX)
001188           MOVE MM-LAST-MAINT-BY TO REC-LAST-MAINT-BY (TB-INDX)
001189           MOVE MM-LAST-MAINT-DT TO REC-LAST-MAINT-DT (TB-INDX)
001190           MOVE MM-LAST-MAINT-HHMMSS TO
001191                          REC-LAST-MAINT-HHMMSS (TB-INDX)
001192           SET TB-INDX UP BY 1
001193        END-IF
001194        GO TO 7001-LOOP
001195     END-IF.
001196
001197 7010-ENDBR.
001198     IF TB-INDX = 1
001199         MOVE ER-0006            TO EMI-ERROR
001200         MOVE 'A'                TO FUNCTI
001201         MOVE -1                 TO SC-TEXTL (1)
001202         MOVE AL-UANON           TO FUNCTA
001203         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001204         MOVE ZEROS              TO PI-TOTAL-LINES
001205         GO TO 8100-SEND-INITIAL-MAP
001206     END-IF.
001207
001208     
      * EXEC CICS ENDBR
001209*         DATASET(ELMEMO-FILE-ID)
001210*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003272' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033323732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001211
001212     SET TB-INDX DOWN BY 1.
001213     SET PI-TOTAL-LINES TO TB-INDX.
001214     MOVE 1                      TO PI-CURRENT-LINE.
001215
001216 7050-FORMAT-LINES.
001217     SET TB-INDX TO PI-CURRENT-LINE.
001218     PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
001219             VARYING SC-INDX FROM 1
001220             BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
001221     PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
001222     GO TO 8100-SEND-INITIAL-MAP.
001223     EJECT
001224 7100-FORMAT-SCREEN.
001225     IF TB-INDX > PI-TOTAL-LINES
001226        IF FUNCTI NOT = 'A'
001227           MOVE AL-PANON         TO SC-TEXTA (SC-INDX)
001228        END-IF
001229     END-IF.
001230
001231     IF TB-INDX > PI-TOTAL-LINES
001232         GO TO 7100-EXIT
001233     END-IF.
001234
001235     SET WS-LINE TO TB-INDX.
001236     MOVE WS-LINE TO SC-LIN (SC-INDX).
001237     MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).
001238     MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MTBY (SC-INDX).
001239     MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1.
001240     MOVE ' '                    TO DC-OPTION-CODE.
001241     PERFORM 9700-LINK-DATE-CONVERT.
001242     MOVE DC-GREG-DATE-1-MDY     TO SC-MTDT (SC-INDX).
001243     SET ROLL-COUNTER TO TB-INDX.
001244
001245     IF NOT MODIFY-CAP
001246         MOVE AL-PANOF           TO SC-TEXTA (SC-INDX)
001247         SET TB-INDX UP BY 1
001248         GO TO 7100-EXIT
001249     END-IF.
001250
001251     SET TB-INDX UP BY 1.
001252
001253 7100-EXIT.
001254      EXIT.
001255
001256 7200-PUT-TEMP-STOR.
001257     PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
001258     SET TS-INDX TO 1.
001259     MOVE 0                      TO PI-TEMP-STOR-ITEMS.
001260     PERFORM 7300-WRITE-TS THRU 7399-EXIT
001261             VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
001262             UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
001263 7249-EXIT.
001264      EXIT.
001265 7250-DELETE-TEMP-STOR.
001266     
      * EXEC CICS HANDLE CONDITION
001267*         QIDERR(7299-EXIT)
001268*    END-EXEC.
      *    MOVE '"$N                   ! '' #00003330' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303033333330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001269     
      * EXEC CICS DELETEQ TS
001270*         QUEUE(QID)
001271*    END-EXEC.
      *    MOVE '*&                    #   #00003333' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033333333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001272 7299-EXIT.
001273     EXIT.
001274     EJECT
001275 7300-WRITE-TS.
001276     MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.
001277     SET TS-INDX UP BY 1.
001278     ADD 1 TO PI-TEMP-STOR-ITEMS.
001279     
      * EXEC CICS WRITEQ TS
001280*         FROM(TS-WORK-AREA)
001281*         QUEUE(QID)
001282*         LENGTH(TS-LENGTH)
001283*         ITEM(PI-TEMP-STOR-ITEMS)
001284*    END-EXEC.
      *    MOVE '*" I   L              ''   #00003343' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033333433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001285 7399-EXIT.
001286     EXIT.
001287     EJECT
001288 7400-PAGE-ROUTINE.
001289
001290     IF PFENTERL NOT = ZEROS
001291        MOVE -1                  TO PFENTERL
001292        ELSE
001293        MOVE -1                  TO FUNCTL
001294     END-IF.
001295
001296     IF PI-TOTAL-LINES = 0
001297        MOVE ER-0047             TO EMI-ERROR
001298        MOVE -1                  TO FUNCTL
001299        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001300        GO TO 8200-SEND-DATAONLY
001301     END-IF.
001302
001303     COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.
001304
001305     IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
001306        MOVE ER-0067             TO EMI-ERROR
001307        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001308        MOVE 1 TO TEMP-CURR-LINE
001309     END-IF.
001310
001311     IF TEMP-CURR-LINE GREATER THAN PI-TOTAL-LINES
001312        MOVE ER-0066             TO EMI-ERROR
001313        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001314        COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1
001315                               - NUM-LINES-PER-SCREEN
001316        IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
001317           MOVE 1                TO TEMP-CURR-LINE
001318        END-IF
001319     END-IF.
001320
001321     PERFORM 7450-SET-INDX THRU 7450-EXIT.
001322     PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
001323             VARYING SC-INDX FROM 1 BY 1 UNTIL
001324             SC-INDX > NUM-LINES-PER-SCREEN.
001325
001326     IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS
001327        NEXT SENTENCE
001328     ELSE
001329        GO TO 8200-SEND-DATAONLY
001330     END-IF.
001331
001332     MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.
001333     SET TB-INDX TO PI-CURRENT-LINE.
001334     MOVE LOW-VALUES             TO EL1284AI
001335                                    EL1284R
001336     PERFORM 7800-MOVE-SAVE-TO-MAP.
001337
001338     PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
001339             VARYING SC-INDX FROM 1 BY 1
001340             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
001341     PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
001342     GO TO 8100-SEND-INITIAL-MAP.
001343     EJECT
001344
001345 7450-SET-INDX.
001346     IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0
001347        SET TB-INDX TO 1
001348     ELSE
001349        PERFORM 7500-READ-TS THRU 7599-EXIT
001350        IF PI-CURRENT-LINE = 0
001351           SET TB-INDX TO 1
001352        ELSE
001353           SET TB-INDX TO PI-CURRENT-LINE
001354        END-IF
001355     END-IF.
001356 7450-EXIT.
001357      EXIT.
001358     EJECT
001359 7500-READ-TS.
001360     
      * EXEC CICS HANDLE CONDITION
001361*         QIDERR(7590-TS-QIDERR)
001362*         ITEMERR(7585-QID-ITEMERR)
001363*    END-EXEC.
      *    MOVE '"$N<                  ! ( #00003424' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303033343234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001364     SET TS-INDX TO 1.
001365     MOVE 1                      TO QID-ITEM.
001366 7501-LOOP.
001367     
      * EXEC CICS READQ TS
001368*         INTO(TS-WORK-AREA)
001369*         QUEUE(QID)
001370*         LENGTH(TS-LENGTH)
001371*         ITEM(QID-ITEM)
001372*    END-EXEC.
      *    MOVE '*$II   L              ''   #00003431' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033343331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001373     MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).
001374     SET TS-INDX UP BY 1.
001375     ADD 1 TO QID-ITEM.
001376     GO TO 7501-LOOP.
001377
001378 7585-QID-ITEMERR.
001379     IF EIBTRNID NOT = TRANS-ID
001380        SUBTRACT 1 FROM QID-ITEM
001381        MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS
001382     END-IF.
001383     GO TO 7599-EXIT.
001384
001385 7590-TS-QIDERR.
001386     IF EIBTRNID = TRANS-ID
001387        AND EIBAID = DFHCLEAR
001388           GO TO 9410-RETURN
001389     END-IF.
001390     IF EIBTRNID = TRANS-ID
001391        MOVE ER-0033             TO EMI-ERROR
001392        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001393        GO TO 8100-SEND-INITIAL-MAP
001394     END-IF.
001395
001396 7599-EXIT.
001397      EXIT.
001398
001399     EJECT
001400 7600-UPDATE-TABLE-FROM-SCREEN.
001401
001402     IF SC-TEXTL (SC-INDX) NOT = ZEROS
001403         IF TB-INDX NOT > PI-TOTAL-LINES
001404             PERFORM 7700-MOVE-DATA THRU 7700-EXIT
001405             SET TB-INDX UP BY 1
001406         ELSE
001407             IF PI-TOTAL-LINES = MAX-LINES
001408                 MOVE ER-0051    TO EMI-ERROR
001409                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001410                 GO TO 8200-SEND-DATAONLY
001411             ELSE
001412                 PERFORM 7700-MOVE-DATA THRU 7700-EXIT
001413                 SET TB-INDX UP BY 1
001414                 ADD 1 TO PI-TOTAL-LINES
001415             END-IF
001416         END-IF
001417     ELSE
001418        IF TB-INDX NOT > PI-TOTAL-LINES
001419           SET TB-INDX UP BY 1
001420        END-IF
001421     END-IF.
001422
001423 7699-EXIT.
001424      EXIT.
001425
001426 7700-MOVE-DATA.
001427     MOVE '1'                    TO PI-UPDATE-SW.
001428
001429     IF SC-TEXTL (SC-INDX) NOT = ZEROS
001430        MOVE SC-TEXT (SC-INDX)  TO REC-TEXT (TB-INDX)
001431        MOVE PI-PROCESSOR-ID    TO REC-LAST-MAINT-BY (TB-INDX)
001432        MOVE EIBTIME            TO REC-LAST-MAINT-HHMMSS (TB-INDX)
001433        MOVE SAVE-BIN-DATE      TO REC-LAST-MAINT-DT (TB-INDX)
001434     END-IF.
001435
001436 7700-EXIT.
001437      EXIT.
001438 7800-MOVE-SAVE-TO-MAP.
001439     MOVE PI-LONG-HLTH-APP TO LNGHLAPI
001440     MOVE PI-REWRITE TO REWRITEI
001441     MOVE PI-CHKCOVG TO CHKCOVGI
001442     MOVE PI-MR-RELEASE-DATE TO MRRELDTI.
001443
001444     EJECT
001445 8100-SEND-INITIAL-MAP.
001446     MOVE SAVE-DATE              TO DATEO.
001447     MOVE EIBTIME                TO TIME-IN.
001448     MOVE TIME-OUT               TO TIMEO.
001449     MOVE PI-COMPANY-ID          TO CMPNYIDO.
001450     MOVE PI-PROCESSOR-ID        TO USERIDO.
001451     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.
001452     MOVE PI-CARRIER             TO CARRO
001453*    MOVE PI-GROUPING            TO FGROUPO.
001454*    MOVE PI-STATE               TO FSTO.
001455*    MOVE PI-ACCOUNT             TO FACOUNTO.
001456     MOVE PI-CERT-NO             TO CERTNOO
001457     MOVE PI-CLAIM-NO            TO CLAIMNOO
001458*    MOVE PI-CERT-SFX            TO FCRTSFXO.
001459*
001460     PERFORM 8150-READ-CLAIM.
001461*
001462     MOVE  ' '                   TO DC-OPTION-CODE.
001463     MOVE PI-CERT-EFF-DT         TO DC-BIN-DATE-1.
001464     PERFORM 9700-LINK-DATE-CONVERT.
001465*    MOVE DC-GREG-DATE-1-EDIT    TO FEFFDTO.
001466     MOVE PI-TOTAL-LINES         TO TOTO.
001467*    IF PI-CLAIM-NOTE
001468*        MOVE SCR-CLAIM-NOTE-TYPE TO FTYPEO
001469*        MOVE SCR-PF6-CERT        TO PF6NOTEO
001470*        MOVE SCR-CERT-YN         TO FTYPEYNO
001471*        IF PI-CERT-NOTES-EXIST = 'Y'
001472*            MOVE 'YES'           TO FCERTYNO
001473*        ELSE
001474*            MOVE 'NO '           TO FCERTYNO
001475*        END-IF
001476*    ELSE
001477*        MOVE SCR-CERT-NOTE-TYPE TO FTYPEO
001478*        MOVE SCR-PF6-CLAIM      TO PF6NOTEO
001479*        MOVE SCR-CLAIM-YN       TO FTYPEYNO
001480*        IF PI-CLAIM-NOTES-EXIST = 'Y'
001481*            MOVE 'YES'           TO FCERTYNO
001482*        ELSE
001483*            MOVE 'NO '           TO FCERTYNO
001484*        END-IF
001485*    END-IF.
001486*    IF PI-BILLING-NOTES-EXIST = 'Y'
001487*        MOVE 'YES'           TO FBILLYNO
001488*    ELSE
001489*        MOVE 'NO '           TO FBILLYNO
001490*    END-IF
001491
001492     MOVE EL1284R TO GROUP2.
001493     IF NOT CURSOR-SET
001494        MOVE -1                     TO LNGHLAPL
001495     END-IF
001496
001497     
      * EXEC CICS SEND
001498*         MAP(MAP-NAME)
001499*         MAPSET(MAPSET-NAME)
001500*         FROM(EL1284AO)
001501*         ERASE
001502*         CURSOR
001503*    END-EXEC.
           MOVE LENGTH OF
            EL1284AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003561' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303033353631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AO, 
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
           
001504
001505     GO TO 9100-RETURN-TRAN.
001506
001507 8150-READ-CLAIM.
001508     
      * EXEC CICS HANDLE CONDITION
001509*        NOTFND   (8150-NOT-FOUND)
001510*    END-EXEC.
      *    MOVE '"$I                   ! ) #00003572' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303033353732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001511
001512     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
001513     MOVE PI-CARRIER             TO MSTR-CARRIER.
001514     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
001515     MOVE PI-CERT-NO             TO MSTR-CERT-NO.
001516
001517     
      * EXEC CICS READ
001518*        UPDATE
001519*        DATASET   ('ELMSTR')
001520*        RIDFLD    (ELMSTR-KEY)
001521*        INTO      (CLAIM-MASTER)
001522*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00003581' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033353831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001523
001524     EVALUATE TRUE
001525        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
001526           MOVE PI-AH-OVERRIDE-L6
001527                                 TO TYPEO
001528        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
001529           MOVE PI-LIFE-OVERRIDE-L6
001530                                 TO TYPEO
001531        WHEN CL-CLAIM-TYPE = 'I'
001532           MOVE '  IU  '         TO TYPEO
001533        WHEN CL-CLAIM-TYPE = 'G'
001534           MOVE ' GAP  '         TO TYPEO
001535        WHEN CL-CLAIM-TYPE = 'F'
001536           MOVE ' FAM  '         TO TYPEO
001537        WHEN CL-CLAIM-TYPE = 'B'
001538           MOVE ' BRV  '         TO TYPEO
001539        WHEN CL-CLAIM-TYPE = 'H'
001540           MOVE ' HSP '          TO TYPEO
001541        WHEN CL-CLAIM-TYPE = 'O'
001542           MOVE ' OTH  '         TO TYPEO
001543     END-EVALUATE.
001544
001545     MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
001546     MOVE CL-INSURED-LAST-NAME   TO LASTNMEO.
001547
001548 8150-NOT-FOUND.
001549     MOVE SPACES    TO TYPEO.
001550*
001551 8200-SEND-DATAONLY.
001552     MOVE EIBTIME                TO TIME-IN.
001553     MOVE TIME-OUT               TO TIMEO.
001554     MOVE PI-COMPANY-ID          TO CMPNYIDO.
001555     MOVE PI-PROCESSOR-ID        TO USERIDO.
001556     MOVE PI-TOTAL-LINES         TO TOTO.
001557     MOVE EL1284R TO GROUP2.
001558
001559
001560     IF NOT EMI-NO-ERRORS
001561        MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO
001562     ELSE
001563        MOVE -1                  TO FUNCTL
001564     END-IF.
001565
001566     
      * EXEC CICS SEND
001567*         MAP(MAP-NAME)
001568*         MAPSET(MAPSET-NAME)
001569*         FROM(EL1284AO)
001570*         DATAONLY
001571*         CURSOR
001572*    END-EXEC.
           MOVE LENGTH OF
            EL1284AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003630' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303033363330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AO, 
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
           
001573
001574     GO TO 9100-RETURN-TRAN.
001575
001576 8300-SEND-TEXT.
001577     
      * EXEC CICS SEND TEXT
001578*         FROM(LOGOFF-TEXT)
001579*         ERASE
001580*         FREEKB
001581*         LENGTH(LOGOFF-LENGTH)
001582*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003641' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303033363431' TO DFHEIV0
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
           
001583
001584     PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
001585
001586     
      * EXEC CICS RETURN
001587*    END-EXEC.
      *    MOVE '.(                    ''   #00003650' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033363530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001588
001589 8800-UNAUTHORIZED-ACCESS.
001590     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001591     GO TO 8300-SEND-TEXT.
001592
001593 9000-RETURN-CICS.
001594     IF PI-CHANGES-MADE
001595        MOVE ER-0045             TO EMI-ERROR
001596        MOVE -1                  TO FUNCTL
001597        MOVE SPACES              TO PFENTERO
001598        MOVE AL-UNNOF            TO PFENTERA
001599        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001600        GO TO 8200-SEND-DATAONLY
001601     END-IF.
001602
001603     MOVE EIBAID                 TO PI-ENTRY-CD-1.
001604     MOVE XCTL-005               TO PGM-NAME.
001605     GO TO 9300-XCTL.
001606
001607 9100-RETURN-TRAN.
001608     MOVE SCRN-NUMBER            TO PI-CURRENT-SCREEN-NO.
001609     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
001610     
      * EXEC CICS RETURN
001611*         TRANSID(TRANS-ID)
001612*         COMMAREA(PROGRAM-INTERFACE-BLOCK)
001613*         LENGTH(PI-COMM-LENGTH)
001614*    END-EXEC.
      *    MOVE '.(CT                  ''   #00003674' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033363734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001615
001616
001617 9200-RETURN-MAIN-MENU.
001618     IF PI-CHANGES-MADE
001619        MOVE -1                  TO FUNCTL
001620        MOVE SPACES              TO PFENTERO
001621        MOVE AL-UNNOF            TO PFENTERA
001622        MOVE ER-0045             TO EMI-ERROR
001623        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001624        GO TO 8200-SEND-DATAONLY
001625     END-IF.
001626
001627     MOVE XCTL-126               TO PGM-NAME.
001628
001629 9300-XCTL.
001630     PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
001631     
      * EXEC CICS XCTL
001632*         PROGRAM  (PGM-NAME)
001633*         COMMAREA (PROGRAM-INTERFACE-BLOCK)
001634*         LENGTH   (PI-COMM-LENGTH)
001635*    END-EXEC.
      *    MOVE '.$C                   %   #00003695' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033363935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001636
001637 9400-CLEAR.
001638
001639     IF PI-CHANGES-MADE
001640         MOVE ER-0045            TO EMI-ERROR
001641         MOVE -1                 TO FUNCTL
001642         SET CURSOR-SET TO TRUE
001643         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001644         PERFORM 7800-MOVE-SAVE-TO-MAP
001645         IF PI-CURRENT-LINE GREATER THAN ZERO
001646             PERFORM 7500-READ-TS THRU 7599-EXIT
001647             SET TB-INDX TO PI-CURRENT-LINE
001648             PERFORM 7100-FORMAT-SCREEN  THRU  7100-EXIT
001649                 VARYING SC-INDX FROM 1 BY 1 UNTIL
001650                 SC-INDX GREATER NUM-LINES-PER-SCREEN
001651         END-IF
001652         GO TO 8100-SEND-INITIAL-MAP
001653     END-IF.
001654
001655 9410-RETURN.
001656*    IF PF5-PRESSED
001657*        MOVE PGM-EL1276           TO PGM-NAME
001658*    ELSE
001659*        IF PF6-PRESSED
001660*            IF PI-CLAIM-NOTE
001661*                SET PI-CERT-NOTE  TO TRUE
001662*            ELSE
001663*                SET PI-CLAIM-NOTE TO TRUE
001664*            END-IF
001665*            SET PI-CHANGE-IN-NOTE-TYPE TO TRUE
001666*            MOVE 'N'              TO PI-PF6-PRESSED
001667*            GO TO 1000-START
001668*        ELSE
001669             MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME
001670*        END-IF
001671*    END-IF.
001672     GO TO 9300-XCTL.
001673
001674 9500-PF12.
001675     IF PI-CHANGES-MADE
001676        MOVE -1                  TO FUNCTL
001677        MOVE SPACES              TO PFENTERO
001678        MOVE AL-UNNOF            TO PFENTERA
001679        MOVE ER-0045             TO EMI-ERROR
001680        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001681        GO TO 8200-SEND-DATAONLY
001682     END-IF.
001683
001684     MOVE 'EL010'                TO PGM-NAME.
001685     GO TO 9300-XCTL.
001686
001687     EJECT
001688
001689     EJECT
001690 9600-PGMID-ERROR.
001691     
      * EXEC CICS HANDLE CONDITION
001692*         PGMIDERR(8300-SEND-TEXT)
001693*    END-EXEC.
      *    MOVE '"$L                   ! * #00003755' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303033373535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001694
001695     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
001696     MOVE SPACES                 TO  PI-ENTRY-CD-1.
001697     MOVE XCTL-005               TO  PGM-NAME.
001698     MOVE PGM-NAME               TO  LOGOFF-PGM.
001699     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
001700
001701     GO TO 9300-XCTL.
001702
001703 9700-LINK-DATE-CONVERT.
001704
001705     MOVE LINK-ELDATCV           TO  PGM-NAME.
001706     
      * EXEC CICS LINK
001707*        PROGRAM    (PGM-NAME)
001708*        COMMAREA   (DATE-CONVERSION-DATA)
001709*        LENGTH     (DC-COMM-LENGTH)
001710*    END-EXEC.
      *    MOVE '."C                   (   #00003770' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001711
001712 9700-EXIT.
001713     EXIT.
001714
001715 9900-ERROR-FORMAT.
001716     IF NOT EMI-ERRORS-COMPLETE
001717        MOVE LINK-001            TO  PGM-NAME
001718        
      * EXEC CICS LINK
001719*            PROGRAM(PGM-NAME)
001720*            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
001721*            LENGTH(EMI-COMM-LENGTH)
001722*       END-EXEC
      *    MOVE '."C                   (   #00003782' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001723     END-IF.
001724 9900-EXIT.
001725     EXIT.
001726
001727 9990-ABEND.
001728     MOVE LINK-004               TO  PGM-NAME.
001729     MOVE DFHEIBLK               TO  EMI-LINE1.
001730
001731     
      * EXEC CICS LINK
001732*        PROGRAM   (PGM-NAME)
001733*        COMMAREA  (EMI-LINE1)
001734*        LENGTH    (72)
001735*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003795' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001736
001737     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGBO.
001738     MOVE -1                     TO  FUNCTL.
001739
001740     GO TO 8100-SEND-INITIAL-MAP.
001741
001742     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001743
001744 9995-SECURITY-VIOLATION.
001745*                                COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003827' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033383237' TO DFHEIV0
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
001746
001747 9995-EXIT.
001748     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9400-CLEAR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4610-ENDBR,
                     6000-NOT-OPEN,
                     4610-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 4900-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7010-ENDBR,
                     6000-NOT-OPEN,
                     7010-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7590-TS-QIDERR,
                     7585-QID-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8150-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
