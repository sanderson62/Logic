      *((program: EL1812.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1812.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 02/12/96 08:55:21.
000007*                            VMOD=2.006.
000008*
000009*AUTHOR.           LOGIC,INC.
000010*                  DALLAS,TEXAS.
000011
000012*REMARKS. TRANSACTION EX52 - FILE FOLDER LABEL PRINT PROGRAM.
000013******************************************************************
000014*                   C H A N G E   L O G
000015*
000016* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000017*-----------------------------------------------------------------
000018*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000019* EFFECTIVE    NUMBER
000020*-----------------------------------------------------------------
000021* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000022* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000023* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000024* 080322  CR2021100800003  TANA  Add B and H claim types
000025******************************************************************
000026     EJECT
000027 ENVIRONMENT DIVISION.
000028 DATA DIVISION.
000029 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000030 77  FILLER  PIC X(32)  VALUE '********************************'.
000031 77  FILLER  PIC X(32)  VALUE '*   EL1812  WORKING STORAGE    *'.
000032 77  FILLER  PIC X(32)  VALUE '******** VMOD=2.006 ************'.
000033
000034 01  WS-CONSTANTS.
000035     12  THIS-PGM                PIC X(8)    VALUE 'EL1812'.
000036     12  PGM-NAME                PIC X(8).
000037     12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
000038     12  MSTR-ID                 PIC X(8)    VALUE 'ELMSTR'.
000039
000040 01  WS-WORK-AREA.
000041     12  W-TIMER                 PIC S9(03) COMP-3 VALUE ZEROS.
000042     12  MSTR-KEY.
000043         16  MSTR-CO             PIC X.
000044         16  MSTR-CARRIER        PIC X.
000045         16  FILLER              PIC X(17).
000046
000047     12  ERROR-LINE              PIC X(80).
000048     12  BROWSE-STARTED          PIC X VALUE 'N'.
000049     12  SUB                     PIC 9  COMP-3.
000050     12  WS-SKIP                 PIC 99.
000051     12  WS-LABEL-HOLD-AREA.
000052         16  CONTROL-LINE.
000053             20  WS-CARR         PIC X.
000054             20  FILLER          PIC X     VALUE SPACES.
000055             20  WS-CLAIMNO      PIC X(7).
000056             20  FILLER          PIC X     VALUE SPACES.
000057             20  WS-CERTNO       PIC X(11).
000058             20  FILLER          PIC X(2)  VALUE SPACES.
000059             20  WS-TYPE         PIC X(6).
000060             20  FILLER          PIC X(1)  VALUE SPACES.
000061
000062         16  NAME-LINE           PIC X(30).
000063
000064         16  ST-ACCT-LINE.
000065             20  FILLER          PIC X(3)  VALUE 'ST-'.
000066             20  WS-STATE        PIC XX.
000067             20  FILLER          PIC X(4)  VALUE ' AC-'.
000068             20  WS-ACCT         PIC X(10).
000069             20  FILLER          PIC X(5) VALUE ' GRP-'.
000070             20  WS-GRP          PIC X(6).
000071
000072         16  DATE-LINE.
000073             20  FILLER          PIC X(4)  VALUE 'INC-'.
000074             20  WS-INCUR-DT     PIC X(8).
000075             20  FILLER          PIC X(7)  VALUE '  ESTB-'.
000076             20  WS-EST-DT       PIC X(8).
000077             20  FILLER          PIC XXX  VALUE SPACES.
000078*                                COPY ELCDMD34.
      *>>((file: ELCDMD34))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDMD34.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = DMD DLO034 PARAMETER AREA                 *
000008*                                                                *
000009*    LENGTH = 272    RECFRM = FIXED                              *
000010*                                                                *
000011******************************************************************
000012 01  DLO034-COMMUNICATION-AREA.
000013     12  DL34-PROCESS-TYPE             PIC X.
000014     12  DL34-COMPANY-ID               PIC XXX.
000015     12  DL34-PRINT-PROGRAM-ID         PIC X(8).
000016     12  DL34-USERID                   PIC X(4).
000017     12  DL34-PRINT-LINE               PIC X(250).
000018     12  DL34-OVERRIDE-PRINTER-ID      PIC X(4).
000019     12  DL34-RETURN-CODE              PIC XX.
000020 01  DLO034-REC-LENGTH                 PIC S9(4) COMP VALUE +272.
      *<<((file: ELCDMD34))
000079     EJECT
000080*                                COPY ELCNWA.
      *>>((file: ELCNWA))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCNWA.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                         *
000007*                                                               *
000008*            M O V E   N A M E   W O R K   A R E A.             *
000009*                                                               *
000010*****************************************************************.
000011
000012 01  WS-NAME-WORK-AREA.
000013     05  WS-INSURED-LAST-NAME        PIC X(15).
000014     05  WS-INSURED-1ST-NAME         PIC X(12).
000015     05  WS-INSURED-MID-INIT         PIC X.
000016
000017     05  WS-NAME-WORK.
000018         10  WS-NW                   PIC X
000019             OCCURS 30 TIMES INDEXED BY NWA-INDEX.
000020
000021     05  WS-NAME-WORK2.
000022         10  WS-NW2                  PIC X
000023             OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
000024                                        NWA-INDEX0.
000025
000026     05  WS-NAME-SW                  PIC S9          VALUE ZERO
000027                                     COMP-3.
000028
      *<<((file: ELCNWA))
000081     EJECT
000082*                                COPY ELCINTF.
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
000083     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
000084         16  PI-PRINT-TYPE       PIC X.
000085         16  PI-ON-DATE          PIC XX.
000086         16  PI-THRU-DATE        PIC XX.
000087         16  FILLER              PIC X(635).
000088     EJECT
000089*                                COPY ELPRTCVD.
      *>>((file: ELPRTCVD))
000001*****************************************************************
000002*                                                               *
000003*                            ELPRTCVD.                          *
000004*                            VMOD=2.001                         *
000005*****************************************************************.
000006
000007******************************************************************
000008***   WORK AREAS  FOR TERMINAL ONLINE PRINT ROUTINE
000009***                 -ELPRTCVD-
000010***   TO BE USED WITH PROCEDURE COPY MEMBER -ELPRTCVP-
000011******************************************************************
000012
000013 01  S-WORK-AREA                     SYNC.
000014     12  WS-LINE-LEN                 PIC S9(4)       VALUE +80
000015                                     COMP.
000016
000017     12  WS-LINE-LENGTH              PIC S9(4)       VALUE ZERO
000018                                     COMP.
000019
000020     12  WS-BUFFER-SIZE              PIC S9(4)       VALUE +1916
000021                                     COMP.
000022
000023     12  WS-BUFFER-LENGTH            PIC S9(4)       VALUE ZERO
000024                                     COMP.
000025
000026     12  WS-PROG-END                 PIC X           VALUE SPACES.
000027
000028     12  WS-PRINT-AREA.
000029         16  WS-PASSED-CNTL-CHAR     PIC X           VALUE SPACES.
000030           88  SINGLE-SPACE                          VALUE ' '.
000031           88  DOUBLE-SPACE                          VALUE '0'.
000032           88  TRIPLE-SPACE                          VALUE '-'.
000033           88  TOP-PAGE                              VALUE '1'.
000034
000035         16  WS-PASSED-DATA.
000036             20  WS-PRINT-BYTE       PIC X
000037                 OCCURS 132 TIMES    INDEXED BY PRT-INDEX.
000038
000039     12  WS-LINE-CNT                 PIC S9(3)        VALUE ZERO
000040                                     COMP-3.
000041     12  WS-WCC-CNTL                 PIC X(1)         VALUE 'H'.
000042
000043     12  WS-EM                       PIC S9(4)        VALUE +25
000044                                     COMP.
000045     12  FILLER   REDEFINES WS-EM.
000046         16  FILLER                  PIC X.
000047         16  T-EM                    PIC X.
000048
000049*    12  WS-SS                       PIC S9(4)        VALUE +21
000050     12  WS-SS                       PIC S9(4)        VALUE +10
000051                                     COMP.
000052     12  FILLER   REDEFINES WS-SS.
000053         16  FILLER                  PIC X.
000054         16  T-SS                    PIC X.
000055
000056     12  WS-TP                       PIC S9(4)      VALUE +12
000057                                     COMP.
000058     12  FILLER   REDEFINES WS-TP.
000059         16  FILLER                  PIC X.
000060         16  T-TP                    PIC X.
000061
000062     12  WS-FIRST-TIME-SW            PIC X           VALUE '1'.
000063         88  FIRST-TIME                              VALUE '1'.
000064         88  FIRST-LINE-NEXT-BUFFER                  VALUE '2'.
000065
000066     12  WS-BUFFER-AREA.
000067         16  WS-BUFFER-BYTE          PIC X
000068             OCCURS 1920 TIMES       INDEXED BY BUFFER-INDEX
000069                                                BUFFER-INDEX2.
000070
000071******************************************************************
      *<<((file: ELPRTCVD))
000090     EJECT
000091*                                COPY ELCDATE.
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
000092     EJECT
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
000094 01  DFHCOMMAREA                 PIC X(1024).
000095
000096*                                COPY ELCMSTR.
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
000097     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1812' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000098 VCOBOL-DUMMY-PROCEDURE.
000099
000100     MOVE SPACES                 TO DL34-PROCESS-TYPE.
000101
000102 0100-RETRIEVE-LOOP.
000103     
      * EXEC CICS HANDLE CONDITION
000104*         ENDDATA  (200-END-DATA)
000105*         NOTFND   (300-NOT-FOUND)
000106*         END-EXEC.
      *    MOVE '"$&I                  ! " #00001094' TO DFHEIV0
           MOVE X'222426492020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303031303934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000107
000108     
      * EXEC CICS RETRIEVE
000109*         INTO    (PROGRAM-INTERFACE-BLOCK)
000110*         LENGTH  (PI-COMM-LENGTH)
000111*         END-EXEC.
      *    MOVE '0*I L                 &   #00001099' TO DFHEIV0
           MOVE X'302A49204C20202020202020' &
                X'202020202020202020202620' &
                X'2020233030303031303939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000112
000113     GO TO 5000-PRINT-LABELS.
000114     EJECT
000115 200-END-DATA.
000116
000117* DLO034 CLOSE
000118     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
000119         MOVE 'C'                TO DL34-PROCESS-TYPE
000120         MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
000121         MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
000122         MOVE PI-PROCESSOR-ID    TO DL34-USERID
000123         MOVE SPACES             TO DL34-PRINT-LINE
000124                                    DL34-OVERRIDE-PRINTER-ID
000125         
      * EXEC CICS LINK
000126*            PROGRAM    ('DLO034')
000127*            COMMAREA   (DLO034-COMMUNICATION-AREA)
000128*            LENGTH     (DLO034-REC-LENGTH)
000129*        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00001116' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031313136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000130         IF DL34-RETURN-CODE NOT = 'OK'
000131             MOVE  '**DLO034 CLOSE ERROR - ABORT**'
000132                                 TO ERROR-LINE
000133             PERFORM 400-SEND-TEXT.
000134
000135     
      * EXEC CICS RETURN
000136*         END-EXEC.
      *    MOVE '.(                    ''   #00001126' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031313236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000137
000138 300-NOT-FOUND.
000139     MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.
000140     PERFORM 400-SEND-TEXT.
000141     GO TO 200-END-DATA.
000142
000143 400-SEND-TEXT.
000144     
      * EXEC CICS SEND TEXT
000145*         FROM    (ERROR-LINE)
000146*         LENGTH  (70)
000147*         END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00001135' TO DFHEIV0
           MOVE X'382620202020202054202020' &
                X'202020204820202046202D20' &
                X'2020233030303031313335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERROR-LINE, 
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
           
000148
000149     EJECT
000150 5000-PRINT-LABELS.
000151
000152* DLO034 OPEN WHEN DMD OR CID
000153     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
000154         IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
000155             MOVE 'O'                TO DL34-PROCESS-TYPE
000156             MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
000157             MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
000158             MOVE PI-PROCESSOR-ID    TO DL34-USERID
000159             MOVE SPACES             TO DL34-PRINT-LINE
000160             MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
000161             
      * EXEC CICS LINK
000162*                PROGRAM    ('DLO034')
000163*                COMMAREA   (DLO034-COMMUNICATION-AREA)
000164*                LENGTH     (DLO034-REC-LENGTH)
000165*            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00001152' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031313532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000166             IF DL34-RETURN-CODE NOT = 'OK'
000167                 MOVE  '**DLO034 OPEN ERROR - ABORT**'
000168                                     TO ERROR-LINE
000169                 PERFORM 400-SEND-TEXT
000170                 
      * EXEC CICS RETURN
000171*                END-EXEC.
      *    MOVE '.(                    ''   #00001161' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031313631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000172
000173     MOVE 30                     TO WS-LINE-LEN.
000174
000175     IF PI-PRINT-TYPE = '3'
000176        PERFORM 8000-ALIGNMENT-ROUTINE 6 TIMES
000177        MOVE 'X'                 TO WS-PROG-END
000178        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
000179        GO TO 0100-RETRIEVE-LOOP.
000180
000181     MOVE LOW-VALUES             TO MSTR-KEY.
000182     MOVE PI-COMPANY-CD          TO MSTR-CO.
000183
000184     IF NOT PI-NO-CARRIER-SECURITY
000185        MOVE PI-CARRIER-SECURITY TO MSTR-CARRIER.
000186
000187     
      * EXEC CICS HANDLE CONDITION
000188*         NOTFND   (0100-RETRIEVE-LOOP)
000189*         END-EXEC.
      *    MOVE '"$I                   ! # #00001178' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303031313738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000190
000191     
      * EXEC CICS STARTBR
000192*         DATASET  (MSTR-ID)
000193*         RIDFLD   (MSTR-KEY)
000194*         GTEQ
000195*         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001182' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303031313832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MSTR-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000196
000197     
      * EXEC CICS HANDLE CONDITION
000198*         NOTFND   (5050-END-BR)
000199*         ENDFILE  (5050-END-BR)
000200*         END-EXEC.
      *    MOVE '"$I''                  ! $ #00001188' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303031313838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000201
000202     MOVE 'Y'                    TO BROWSE-STARTED.
000203
000204 5010-READ-NEXT.
000205     
      * EXEC CICS READNEXT
000206*         DATASET  (MSTR-ID)
000207*         RIDFLD   (MSTR-KEY)
000208*         SET      (ADDRESS OF CLAIM-MASTER)
000209*         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001196' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303031313936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MSTR-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000210
000211     ADD +1                      TO W-TIMER.
000212     IF  W-TIMER EQUAL +30
000213         MOVE ZEROS              TO W-TIMER
000214         
      * EXEC CICS DELAY
000215*            INTERVAL(1) END-EXEC.
           MOVE 1 TO DFHEIV10
      *    MOVE '0$I                   &   #00001205' TO DFHEIV0
           MOVE X'302449202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303031323035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000216
000217     IF MSTR-CO NOT = PI-COMPANY-CD
000218        GO TO 5050-END-BR.
000219
000220     IF NOT PI-NO-CARRIER-SECURITY
000221         IF CL-CARRIER GREATER THAN PI-CARRIER-SECURITY
000222            GO TO 5050-END-BR.
000223
000224     IF NOT PI-NO-ACCOUNT-SECURITY
000225        IF CL-CERT-ACCOUNT NOT = PI-ACCOUNT-SECURITY
000226           GO TO 5010-READ-NEXT.
000227
000228     IF PI-ON-DATE = LOW-VALUES
000229        PERFORM 6000-BUILD-LABEL THRU 6099-EXIT
000230        GO TO 5010-READ-NEXT.
000231
000232     IF PI-THRU-DATE = LOW-VALUES
000233        IF PI-ON-DATE  =  CL-FILE-ESTABLISH-DT
000234           PERFORM 6000-BUILD-LABEL THRU 6099-EXIT
000235           GO TO 5010-READ-NEXT
000236        ELSE
000237           GO TO 5010-READ-NEXT.
000238
000239     IF CL-FILE-ESTABLISH-DT GREATER THAN PI-THRU-DATE  OR
000240                             LESS    THAN PI-ON-DATE
000241        GO TO 5010-READ-NEXT.
000242
000243     PERFORM 6000-BUILD-LABEL THRU 6099-EXIT.
000244     GO TO 5010-READ-NEXT.
000245
000246 5050-END-BR.
000247     IF BROWSE-STARTED = 'Y'
000248        
      * EXEC CICS ENDBR
000249*            DATASET  (MSTR-ID)
000250*            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001239' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303031323339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MSTR-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000251
000252     MOVE '1'                    TO WS-PASSED-CNTL-CHAR.
000253     MOVE 'LABEL PRINTING COMPLETED' TO WS-PASSED-DATA.
000254     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000255     MOVE SPACES                 TO WS-PASSED-DATA.
000256     MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.
000257     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000258     MOVE SPACES                 TO WS-PASSED-DATA.
000259     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000260     MOVE 'X'                    TO WS-PROG-END.
000261     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000262
000263     GO TO 0100-RETRIEVE-LOOP.
000264
000265     EJECT
000266 6000-BUILD-LABEL.
000267     MOVE CL-CARRIER             TO WS-CARR.
000268     MOVE CL-CLAIM-NO            TO WS-CLAIMNO.
000269     MOVE CL-CERT-NO             TO WS-CERTNO.
000270
000271     EVALUATE TRUE
000272     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
000273        MOVE PI-AH-OVERRIDE-L6   TO WS-TYPE
000274
000275     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
000276        MOVE PI-LIFE-OVERRIDE-L6 TO WS-TYPE
000277
000278     WHEN CL-CLAIM-TYPE = 'I'
000279        MOVE '  IU  '            TO WS-TYPE
000280
000281     WHEN CL-CLAIM-TYPE = 'G'
000282        MOVE ' GAP  '            TO WS-TYPE
000283
000284     WHEN CL-CLAIM-TYPE = 'F'
000285        MOVE ' FAM  '            TO WS-TYPE
000286
000287     WHEN CL-CLAIM-TYPE = 'B'
000288        MOVE ' BRV  '            TO WS-TYPE
000289
000290     WHEN CL-CLAIM-TYPE = 'H'
000291        MOVE ' HOSP '            TO WS-TYPE
000292
000293     WHEN CL-CLAIM-TYPE = 'O'
000294        MOVE ' OTH  '            TO WS-TYPE
000295
000296     END-EVALUATE
000297
000298
000299     IF CL-INCURRED-DT NOT = LOW-VALUE AND SPACES
000300        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
000301        MOVE SPACES              TO DC-OPTION-CODE
000302        PERFORM 9700-DATE-LINK THRU 9700-EXIT
000303        MOVE DC-GREG-DATE-1-EDIT TO WS-INCUR-DT
000304     ELSE
000305        MOVE SPACES              TO WS-INCUR-DT.
000306
000307     IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUE AND SPACES
000308        MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
000309        MOVE SPACES              TO DC-OPTION-CODE
000310        PERFORM 9700-DATE-LINK THRU 9700-EXIT
000311        MOVE DC-GREG-DATE-1-EDIT TO WS-EST-DT
000312     ELSE
000313        MOVE SPACES              TO WS-EST-DT.
000314
000315     PERFORM 7000-MOVE-NAME.
000316     MOVE WS-NAME-WORK           TO NAME-LINE.
000317
000318     MOVE CL-CERT-STATE          TO WS-STATE.
000319     MOVE CL-CERT-ACCOUNT        TO WS-ACCT.
000320     MOVE CL-CERT-GROUPING       TO WS-GRP.
000321     MOVE '1'                    TO WS-PASSED-CNTL-CHAR.
000322
000323     IF PI-PRINT-TYPE = '1'
000324        MOVE CONTROL-LINE        TO WS-PASSED-DATA
000325        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
000326        MOVE SPACE               TO WS-PASSED-CNTL-CHAR
000327        MOVE NAME-LINE           TO WS-PASSED-DATA
000328        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
000329     ELSE
000330        MOVE NAME-LINE           TO WS-PASSED-DATA
000331        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
000332        MOVE SPACE               TO WS-PASSED-CNTL-CHAR
000333        MOVE CONTROL-LINE        TO WS-PASSED-DATA
000334        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000335
000336     MOVE ST-ACCT-LINE           TO WS-PASSED-DATA.
000337
000338     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000339     MOVE DATE-LINE              TO WS-PASSED-DATA
000340     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000341
000342 6099-EXIT.
000343      EXIT.
000344     EJECT
000345 7000-MOVE-NAME   SECTION.
000346*                                COPY ELCMNS.
      *>>((file: ELCMNS))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCMNS.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                         *
000007*                                                               *
000008*                     M O V E   N A M E   R O U T I N E         *
000009*                                                               *
000010*                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
000011*            NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
000012*            NO EMBEDDED BLANKS.                                *
000013*                                                               *
000014*                  FIELD               VALUE                    *
000015*                                                               *
000016*                LAST NAME (CL15)      SMITH                    *
000017*                1ST NAME  (CL12)      JOHN                     *
000018*                MID NAME  (CL12)      ALLEN                    *
000019*                                                               *
000020*                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
000021*                                                               *
000022*                        SMITH, JOHN ALLEN                      *
000023*                                                               *
000024*                TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
000025*            STORAGE COPYBOOK:                                  *
000026*                                                               *
000027*                01  WS-NAME-WORK-AREA COPY ELCNWA.             *
000028*                                                               *
000029*****************************************************************.
000030
000031     MOVE SPACES                 TO  WS-NAME-WORK-AREA.
000032     MOVE ZERO                   TO  WS-NAME-SW.
000033     SET NWA-INDEX TO +1.
000034
000035     IF CL-INSURED-1ST-NAME = SPACES  AND
000036        CL-INSURED-MID-INIT = SPACES
000037         MOVE +1                 TO  WS-NAME-SW.
000038
000039     MOVE CL-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
000040     PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
000041
000042     MOVE CL-INSURED-1ST-NAME   TO  WS-NAME-WORK2.
000043     PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
000044
000045     SET NWA-INDEX UP BY +1.
000046     MOVE CL-INSURED-MID-INIT   TO  WS-NAME-WORK2.
000047     PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
000048
000049 5000-EXIT.
000050     EXIT.
000051
000052     EJECT
000053 5100-MOVE-NAME SECTION.
000054     IF WS-NAME-SW GREATER THAN +1
000055         GO TO 5190-EXIT.
000056
000057     IF WS-NAME-WORK2 = SPACES
000058         GO TO 5190-EXIT.
000059
000060     SET NWA-INDEX2 TO +1.
000061     SET NWA-INDEX3 TO +2.
000062
000063 5110-MOVE-NAME.
000064     MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
000065
000066     IF NWA-INDEX LESS THAN +30
000067         SET NWA-INDEX UP BY +1
000068       ELSE
000069         ADD +2  TO  WS-NAME-SW
000070         GO TO 5190-EXIT.
000071
000072     IF NWA-INDEX2 LESS THAN +20
000073         SET NWA-INDEX3 UP BY +1
000074         SET NWA-INDEX2 UP BY +1.
000075
000076     IF WS-NW2 (NWA-INDEX2) = SPACES AND
000077        WS-NW2 (NWA-INDEX3) = SPACES
000078         IF WS-NAME-SW = ZERO
000079             MOVE ','            TO  WS-NW (NWA-INDEX)
000080             SET NWA-INDEX UP BY +2
000081             MOVE +1             TO  WS-NAME-SW
000082             GO TO 5190-EXIT
000083           ELSE
000084             GO TO 5190-EXIT.
000085
000086     GO TO 5110-MOVE-NAME.
000087
000088 5190-EXIT.
000089     EXIT.
000090
      *<<((file: ELCMNS))
000347     EJECT
000348 8000-ALIGNMENT-ROUTINE.
000349     MOVE '1'                    TO WS-PASSED-CNTL-CHAR.
000350     MOVE ALL '*'                TO WS-PASSED-DATA.
000351     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
000352     MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.
000353     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.
000354
000355 8890-MSTR-NOT-OPEN.
000356     MOVE 'MASTER  FILE NOT OPEN ' TO ERROR-LINE.
000357     PERFORM 400-SEND-TEXT.
000358     GO TO 200-END-DATA.
000359
000360     EJECT
000361 9700-DATE-LINK.
000362     MOVE LINK-ELDATCV TO PGM-NAME.
000363
000364     
      * EXEC CICS LINK
000365*        PROGRAM   (PGM-NAME)
000366*        COMMAREA  (DATE-CONVERSION-DATA)
000367*        LENGTH    (DC-COMM-LENGTH)
000368*        END-EXEC.
      *    MOVE '."C                   (   #00001447' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031343437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000369
000370 9700-EXIT.
000371      EXIT.
000372     EJECT
000373*9800-PRINT-ROUTINE.             COPY ELPRTCVP.
000374 9800-PRINT-ROUTINE.
000375*    COPY ELPRTCVP.
      *>>((file: ELPRTCVP))
000001******************************************************************
000002***                                                              *
000003***                          ELPRTCVP.                           *
000004***                          VMOD=2.003                          *
000005***                                                              *
000006***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
000007***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
000008***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
000009***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
000010***                                                              *
000011***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
000012***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
000013***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
000014***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
000015***      SUCCESSFUL.                                             *
000016***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
000017***                         LENGTH OF THE LINE TO BE PRINTED     *
000018***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
000019***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
000020***                         TED THE FIRST TIME THRU THE ROUTINE. *
000021***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
000022***                         PROGRAM END SWITCH. INITIALIZED      *
000023***                         TO SPACE-     MOVE IN ANY NONBLANK   *
000024***                         TO IT WHEN PROGRAM IS FINISHED.      *
000025***      05  WS-PRINT-AREA.                                      *
000026***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
000027***          10  WS-PASSED-DATA          PIC X(132).             *
000028***                         USE THE DATA TO BE PRINTED IN THE    *
000029***                         WS-PASSED-DATA.                      *
000030***                         USE THE STANDARD CARRIAGE CONTROL    *
000031***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
000032***                           SINGLE-SPACE            VALUE ' '  *
000033***                           DOUBLE-SPACE            VALUE '0'  *
000034***                           TRIPLE-SPACE            VALUE '-'  *
000035***                           TOP-PAGE                VALUE '1'  *
000036***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
000037***            THE USE OF THIS FIELD IS OPTIONAL.                *
000038***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
000039***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
000040***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
000041***            CONDITION.                                        *
000042***                                                              *
000043******************************************************************
000044
000045 ELPRTCVP.
000046
000047*    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'
000048     IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'XXX'
000049         MOVE 'P'                TO DL34-PROCESS-TYPE
000050         MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
000051         MOVE PI-PROCESSOR-ID    TO DL34-USERID
000052         MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
000053         MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE
000054         MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
000055
000056         
      * EXEC CICS LINK
000057*            PROGRAM    ('DLO034')
000058*            COMMAREA   (DLO034-COMMUNICATION-AREA)
000059*            LENGTH     (DLO034-REC-LENGTH)
000060*        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   (   #00001515' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031353135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000061
000062            IF DL34-RETURN-CODE = 'OK'
000063                GO TO ELPRTCVP-EXIT
000064            ELSE
000065*               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
000066                GO TO ELPRTCVP-EXIT.
000067
000068     IF NOT FIRST-TIME
000069         GO TO ELPRTCVP-020.
000070
000071     IF WS-LINE-LEN NOT GREATER ZERO
000072         GO TO ELPRTCVP-EXIT.
000073
000074     MOVE '2'                    TO WS-FIRST-TIME-SW.
000075     MOVE LOW-VALUES             TO WS-BUFFER-AREA.
000076
000077     SET BUFFER-INDEX TO +1
000078
000079     IF EIBTRMID IS EQUAL TO 'AFLP'
000080         NEXT SENTENCE
000081     ELSE
000082         IF NOT TOP-PAGE
000083             MOVE T-TP           TO WS-BUFFER-BYTE (BUFFER-INDEX)
000084             SET BUFFER-INDEX UP BY +1.
000085
000086 ELPRTCVP-020.
000087     IF WS-PROG-END = SPACES
000088         GO TO ELPRTCVP-030.
000089
000090     MOVE SPACES                 TO WS-PROG-END.
000091
000092     IF BUFFER-INDEX GREATER +1
000093         PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
000094
000095     MOVE '1'                    TO WS-FIRST-TIME-SW.
000096
000097     GO TO ELPRTCVP-EXIT.
000098
000099 ELPRTCVP-030.
000100     IF WS-PASSED-DATA = SPACES
000101         SET PRT-INDEX TO +1
000102         GO TO ELPRTCVP-050.
000103
000104     SET PRT-INDEX TO WS-LINE-LEN.
000105
000106 ELPRTCVP-040.
000107     IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES
000108         GO TO ELPRTCVP-050.
000109
000110     IF PRT-INDEX GREATER +1
000111         SET PRT-INDEX DOWN BY +1
000112         GO TO ELPRTCVP-040.
000113
000114 ELPRTCVP-050.
000115     SET WS-LINE-LENGTH TO PRT-INDEX.
000116     SET BUFFER-INDEX2 TO BUFFER-INDEX.
000117     SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.
000118
000119     IF BUFFER-INDEX2 NOT LESS WS-BUFFER-SIZE
000120         PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
000121
000122     IF TRIPLE-SPACE
000123          ADD +2  TO  WS-LINE-CNT
000124          MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)
000125                                 WS-BUFFER-BYTE (BUFFER-INDEX + 1)
000126          SET BUFFER-INDEX UP BY +2.
000127
000128     IF DOUBLE-SPACE
000129          ADD +1  TO  WS-LINE-CNT
000130          MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)
000131          SET BUFFER-INDEX UP BY +1.
000132
000133     ADD +1 TO WS-LINE-CNT
000134************************************************************
000135*     BYPASS NEW LINE SYMBOL                               *
000136*        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *
000137*     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *
000138************************************************************
000139
000140     IF (BUFFER-INDEX GREATER +1 AND
000141         WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)  OR
000142         FIRST-LINE-NEXT-BUFFER
000143         MOVE ZERO               TO WS-FIRST-TIME-SW
000144     ELSE
000145         MOVE T-SS               TO WS-BUFFER-BYTE (BUFFER-INDEX)
000146         SET BUFFER-INDEX UP BY +1.
000147
000148**   NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR
000149
000150     IF TOP-PAGE
000151         MOVE +1                TO WS-LINE-CNT
000152         MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)
000153         SET BUFFER-INDEX UP BY +1.
000154
000155     SET PRT-INDEX TO +1.
000156
000157 ELPRTCVP-060.
000158     MOVE WS-PRINT-BYTE (PRT-INDEX)
000159                                 TO WS-BUFFER-BYTE (BUFFER-INDEX).
000160     SET BUFFER-INDEX UP BY +1.
000161
000162     IF PRT-INDEX LESS WS-LINE-LENGTH
000163         SET PRT-INDEX UP BY +1
000164         GO TO ELPRTCVP-060.
000165
000166 ELPRTCVP-EXIT.
000167     EXIT.
000168
000169 ELPRTCVP-PRINT-BUFFER.
000170     IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS
000171        MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX)
000172        SET BUFFER-INDEX UP BY 1.
000173
000174     MOVE  T-EM                  TO  WS-BUFFER-BYTE (BUFFER-INDEX)
000175     SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
000176
000177     
      * EXEC CICS SEND
000178*        FROM    (WS-BUFFER-AREA)
000179*        LENGTH  (WS-BUFFER-LENGTH)
000180*        CTLCHAR (WS-WCC-CNTL)
000181*        ERASE
000182*    END-EXEC.
      *    MOVE '$$    C E         L F ,   #00001636' TO DFHEIV0
           MOVE X'242420202020432045202020' &
                X'2020202020204C2046202C20' &
                X'2020233030303031363336' TO DFHEIV0
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
           
000183
000184     SET BUFFER-INDEX TO +1.
000185     MOVE '2'                    TO WS-FIRST-TIME-SW.
000186
000187 ELPRTCVP-PRINT-EXIT.
000188     EXIT.
000189
      *<<((file: ELPRTCVP))
000376
000377

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1812' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 200-END-DATA,
                     300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0100-RETRIEVE-LOOP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5050-END-BR,
                     5050-END-BR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1812' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
