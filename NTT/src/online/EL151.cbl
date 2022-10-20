      *((program: EL151.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL151 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 06/06/95 13:08:11.
000007*                            VMOD=2.012.
000008*
000009*AUTHOR.    LOGIC, INC.
000010*           DALLAS, TEXAS.
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
000024******************************************************************
000025*                   C H A N G E   L O G
000026*
000027* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000028*-----------------------------------------------------------------
000029*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000030* EFFECTIVE    NUMBER
000031*-----------------------------------------------------------------
000032* 050506    2006030600001  AJRA  ADD PROOF DATE TO DENIAL SCREEN
000033* 120808    2008100900001  PEMA  ADD FOR DENIAL CODE
000034* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000035* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000036* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < I
000037* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000038******************************************************************
000039*REMARKS.
000040
000041*    SCREENS     - EL151S - DENIALS
000042
000043*    ENTERED BY  - EL150 - STATUS AND DISPOSITION
000044
000045*    EXIT TO     - EL150 - CALLING PROGRAM
000046
000047*    INPUT FILE  - ELMSTR - CLAIM MASTER
000048*                - ELTRLR - ACTIVITY TRAILERS
000049*                - ERNOTE - CERTIFICATE NOTES
000050
000051*    OUTPUT FILE - ELMSTR - CLAIM MASTER
000052*                - ELTRLR - ACTIVITY TRAILERS
000053
000054*    COMMAREA    - PASSED
000055
000056*    ERROR-CODES ACCESSED - 132,  133, 270, 137, 29, 315, 008, 50
000057
000058*    NARRATIVE   - 1- PROVIDE CREATION OF DENIAL TRAILERS
000059*                  2- UPDATE CLAIM STATUS TYPE TO CLOSED (ELMSTR)
000060*                  3- UPDATE RESERVE-EXPENSE TRAILER STATUS TO
000061*                     CLOSED, REASON DENIAL
000062
000063     EJECT
000064 ENVIRONMENT DIVISION.
000065
000066 DATA DIVISION.
000067
000068 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000069
000070 77  FILLER  PIC X(32)  VALUE '********************************'.
000071 77  FILLER  PIC X(32)  VALUE '*   EL151  WORKING STORAGE     *'.
000072 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.012 ********'.
000073
000074*                            COPY ELCSCTM.
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
000075
000076*                            COPY ELCSCRTY.
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
000077
000078*                            COPY ELCNWA.
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
000079
000080 01  WS-DATE-AREA.
000081     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000082     05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
000083     05  WS-BLANK            PIC X       VALUE ' '.
000084
000085 01  WS-SCRATCH-AREA.
000086     05  GETMAIN-SPACE               PIC X     VALUE SPACE.
000087     05  WS-TRLR-LENGTH              PIC S9(4) VALUE +200  COMP.
000088     05  WS-DMO-LENGTH               PIC S9(4) VALUE +108  COMP.
000089     05  WS-DCT-LENGTH               PIC S9(4) VALUE +53   COMP.
000090     05  SC-ITEM                     PIC S9(4) VALUE +0001 COMP.
000091
000092     05  WS-TRANS-ID                 PIC X(4)   VALUE 'EX32'.
000093     05  WS-SEQ-NO-SAVED             PIC S9(4)   VALUE ZEROS.
000094
000095     05  WS-ORIG-SEQ-CNT             PIC S9(4)   VALUE ZEROS.
000096
000097     05  TR-SUB                      PIC S9(4)   VALUE ZERO  COMP.
000098     05  TR-SUB-2                    PIC S9(4)   VALUE ZERO  COMP.
000099     05  MISC-SUB                    PIC S9(03)  COMP-3  VALUE +0.
000100
000101     05  MAP-NAME                    PIC X(08)   VALUE 'EL151A'.
000102     05  MAPSET-NAME                 PIC X(08)   VALUE 'EL151S'.
000103
000104     05  ELTRLR-FILE-ID              PIC X(08)   VALUE 'ELTRLR'.
000105     05  ELMSTR-FILE-ID              PIC X(08)   VALUE 'ELMSTR'.
000106     05  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.
000107     05  ELARCH-FILE-ID              PIC X(08)   VALUE 'ELARCH'.
000108
000109     05  THIS-PGM                    PIC X(08)   VALUE  'EL151'.
000110
000111     05  WS-OC-LOAD-SW               PIC X(01)   VALUE SPACE.
000112         88  OC-HISTORY-LOADED       VALUE 'X'.
000113
000114     05  WS-CLAIM-SEQ-SW             PIC X(01)   VALUE SPACE.
000115         88  NO-SEQ-NUMBER           VALUE 'X'.
000116         88  SEQ-NUMBER-EXIST        VALUE ' '.
000117         88  NO-ACCT-TRLR            VALUE 'Y'.
000118
000119     05  WS-RESET-SW                 PIC X(01)   VALUE 'N'.
000120     05  WS-REC-FOUND-SW             PIC X(01)   VALUE 'N'.
000121     05  WS-BROWSE-SW                PIC X(01)   VALUE 'N'.
000122     05  WS-UPDATE-SW                PIC X(01)   VALUE 'N'.
000123
000124     05  TIME-IN                     PIC S9(7).
000125     05  TIME-OUT-R REDEFINES TIME-IN.
000126         10  FILLER                  PIC X(01).
000127         10  TIME-OUT                PIC 99V99.
000128         10  FILLER                  PIC X(02).
000129
000130     05  DEEDIT-FIELD                PIC X(15).
000131     05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
000132
000133     05  WS-PRF-DT                   PIC X(2).
000134     05  WS-MAX-LETTER-ANSWER-DT     PIC X(2)  VALUE LOW-VALUES.
000135
000136     EJECT
000137     05  DENIAL-TABLE.
000138         10  FILLER                  PIC X(44)    VALUE
000139             'DPE  PRE-EXISTING DISABILITY                '.
000140         10  FILLER                  PIC X(44)    VALUE
000141             'DDN  DISABILITY NOT COVERED DUE TO NATURE   '.
000142         10  FILLER                  PIC X(44)    VALUE
000143             'DIR  INSURED RECOVERED FROM DISABILITY      '.
000144         10  FILLER                  PIC X(44)    VALUE
000145             'DEM  EMPLOYMENT INDICATES OCCUPATION NOT MET'.
000146         10  FILLER                  PIC X(44)    VALUE
000147             'DWP  DISABILITY WAITING PERIOD NOT SATISFIED'.
000148         10  FILLER                  PIC X(44)    VALUE
000149             'DNC  NO COVERAGE                            '.
000150         10  FILLER                  PIC X(44)    VALUE
000151             'DAP  APPLICATION MISREPRESENTATION          '.
000152         10  FILLER                  PIC X(44)    VALUE
000153             'DMA  MIS-STATEMENT OF AGE                   '.
000154         10  FILLER                  PIC X(44)    VALUE
000155             'DSU  SUICIDE OR SELF-INFLICTED INJURY       '.
000156         10  FILLER                  PIC X(44)    VALUE
000157             'DOT  OTHER                                  '.
000158         10  FILLER                  PIC X(44)    VALUE
000159             'DAR  AWARE OF UNEMPLOY./RED. AT INCEPTION   '.
000160         10  FILLER                  PIC X(44)    VALUE
000161             'DNE  NOT EMPLOYED SIX MONTHS PRIOR          '.
000162         10  FILLER                  PIC X(44)    VALUE
000163             'DNR  NOT MADE REDUNDANT/NOT UNEMPLOYED      '.
000164         10  FILLER                  PIC X(44)    VALUE
000165             'DSE  SELF EMPLOYED                          '.
000166         10  FILLER                  PIC X(44)    VALUE
000167             'DTW  TEMPORARY WORK                         '.
000168         10  FILLER                  PIC X(44)    VALUE
000169             'LAP  APPLICATION MISREPRESENTATION          '.
000170         10  FILLER                  PIC X(44)    VALUE
000171             'LMA  MIS-STATEMENT OF AGE                   '.
000172         10  FILLER                  PIC X(44)    VALUE
000173             'LNC  NO COVERAGE                            '.
000174         10  FILLER                  PIC X(44)    VALUE
000175             'LOT  OTHER                                  '.
000176     05  DEN-TABLE REDEFINES DENIAL-TABLE OCCURS 19
000177         INDEXED BY DEN-INDEX.
000178         10  DENIAL-CODE.
000179             15  DENIAL-1-1          PIC X(01).
000180             15  DENIAL-2-3          PIC X(02).
000181             15  DENIAL-4-4          PIC X(01).
000182         10  FILLER                  PIC X(01).
000183         10  DENIAL-DESC             PIC X(39).
000184
000185     EJECT
000186 01  ACCESS-KEYS.
000187     05  WS-TRAILER-KEY.
000188         10  WS-CLAIM-KEY.
000189             15  WS-KEY-COMPANY-CD       PIC X.
000190             15  WS-KEY-CARRIER          PIC X.
000191             15  WS-KEY-CLAIM-NO         PIC X(7).
000192             15  WS-KEY-CERT-NO.
000193                 20  WS-KEY-CERT-PRIME   PIC X(10).
000194                 20  WS-KEY-CERT-SFX     PIC X.
000195         10  WS-KEY-SEQUENCE-NO      PIC   S9(4) COMP.
000196
000197     05  ELARCH-KEY.
000198         10  ELARCH-COMPANY-CD   PIC X(01).
000199         10  ELARCH-ARCHIVE-NO   PIC S9(08)  COMP.
000200         10  ELARCH-RECORD-TYPE  PIC X(01).
000201         10  ELARCH-SEQ-NO       PIC S9(04)  COMP.
000202
000203     05  WS-ELDENY-KEY.
000204         10  ELDENY-COMPANY-CD   PIC X.
000205         10  ELDENY-DENIAL-CODE  PIC X(4).
000206         10  FILLER              PIC X(10).
000207
000208     05  ELCNTL-KEY.
000209         10  ELCNTL-COMPANY-ID   PIC X(03).
000210         10  ELCNTL-RECORD-TYPE  PIC X(01).
000211         10  ELCNTL-ACCESS       PIC X(04).
000212         10  ELCNTL-SEQ-NO       PIC S9(04)  COMP.
000213
000214     05  W-NOTE-KEY.
000215         10  W-NOTE-COMP-CD      PIC X.
000216         10  W-NOTE-CERT-KEY.
000217             16  W-NOTE-CARRIER  PIC X.
000218             16  W-NOTE-GROUPING PIC X(6).
000219             16  W-NOTE-STATE    PIC XX.
000220             16  W-NOTE-ACCOUNT  PIC X(10).
000221             16  W-NOTE-EFF-DT   PIC XX.
000222             16  W-NOTE-CERT-NO  PIC X(11).
000223
000224 01  WS-RESPONSE             PIC S9(8)   COMP.
000225     88  RESP-NORMAL              VALUE +00.
000226     88  RESP-ERROR               VALUE +01.
000227     88  RESP-NOTFND              VALUE +13.
000228     88  RESP-NOTOPEN             VALUE +19.
000229     88  RESP-ENDFILE             VALUE +20.
000230
000231 01  ERROR-MESSAGES.
000232     12  ER-0000                 PIC X(4)  VALUE '0000'.
000233     12  ER-0004                 PIC X(4)  VALUE '0004'.
000234     12  ER-0008                 PIC X(4)  VALUE '0008'.
000235     12  ER-0021                 PIC X(4)  VALUE '0021'.
000236     12  ER-0029                 PIC X(4)  VALUE '0029'.
000237     12  ER-0050                 PIC X(4)  VALUE '0050'.
000238     12  ER-0070                 PIC X(4)  VALUE '0070'.
000239     12  ER-0132                 PIC X(4)  VALUE '0132'.
000240     12  ER-0133                 PIC X(4)  VALUE '0133'.
000241     12  ER-0137                 PIC X(4)  VALUE '0137'.
000242     12  ER-0154                 PIC X(4)  VALUE '0154'.
000243     12  ER-0172                 PIC X(4)  VALUE '0172'.
000244     12  ER-0270                 PIC X(4)  VALUE '0270'.
000245     12  ER-0483                 PIC X(4)  VALUE '0483'.
000246     12  ER-0872                 PIC X(4)  VALUE '0872'.
000247     12  ER-0873                 PIC X(4)  VALUE '0873'.
000248     12  ER-0884                 PIC X(4)  VALUE '0884'.
000249     12  ER-0919                 PIC X(4)  VALUE '0919'.
000250     12  ER-0921                 PIC X(4)  VALUE '0921'.
000251     12  ER-0946                 PIC X(4)  VALUE '0946'.
000252     12  ER-0947                 PIC X(4)  VALUE '0947'.
000253     12  ER-0948                 PIC X(4)  VALUE '0948'.
000254     12  ER-0949                 PIC X(4)  VALUE '0949'.
000255     12  ER-0950                 PIC X(4)  VALUE '0950'.
000256     12  ER-0951                 PIC X(4)  VALUE '0951'.
000257     12  ER-0954                 PIC X(4)  VALUE '0954'.
000258     12  ER-0974                 PIC X(4)  VALUE '0974'.
000259     12  ER-0975                 PIC X(4)  VALUE '0975'.
000260     12  ER-8051                 PIC X(4)  VALUE '8051'.
000261     12  ER-8052                 PIC X(4)  VALUE '8052'.
000262     12  ER-8053                 PIC X(4)  VALUE '8053'.
000263     12  ER-8054                 PIC X(4)  VALUE '8054'.
000264     12  ER-8055                 PIC X(4)  VALUE '8055'.
000265     12  ER-8056                 PIC X(4)  VALUE '8056'.
000266     12  ER-8057                 PIC X(4)  VALUE '8057'.
000267     12  ER-8058                 PIC X(4)  VALUE '8058'.
000268     12  ER-8059                 PIC X(4)  VALUE '8059'.
000269     12  ER-8060                 PIC X(4)  VALUE '8060'.
000270     12  ER-8061                 PIC X(4)  VALUE '8061'.
000271     12  ER-8062                 PIC X(4)  VALUE '8062'.
000272     12  ER-8063                 PIC X(4)  VALUE '8063'.
000273     12  ER-8064                 PIC X(4)  VALUE '8064'.
000274     12  ER-8065                 PIC X(4)  VALUE '8065'.
000275     12  ER-8066                 PIC X(4)  VALUE '8066'.
000276     12  ER-8152                 PIC X(4)  VALUE '8152'.
000277     12  ER-8153                 PIC X(4)  VALUE '8153'.
000278     12  ER-8154                 PIC X(4)  VALUE '8154'.
000279     12  ER-8155                 PIC X(4)  VALUE '8155'.
000280     EJECT
000281*                                    COPY ELCAID.
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
000282 01  PF-AID REDEFINES DFHAID.
000283     05  FILLER                      PIC X(8).
000284     05  PF-VALUES  OCCURS 24        PIC X.
000285     EJECT
000286*                                    COPY ELCINTF.
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
000287     12  PI-REDEFINES REDEFINES PI-PROGRAM-WORK-AREA.
000288         16  PI-PREV-TRLR-KEY        PIC X(22).
000289*00252          16  FILLER                  PIC X(618).
000290         16  PI-PROOF-DATE           PIC 9(6).
000291         16  FILLER                  PIC X(612).
000292
000293* DLO023
000294 01  DL23-COMM-LENGTH              PIC S9(4) COMP VALUE +132.
000295 01  WS-DLO-CODES-TABLE.
000296     12  DL23-SYSTEM-ID            PIC XX.
000297     12  DL23-RECORD-TYPE          PIC XX.
000298     12  DL23-RECORD-KEY           PIC X(6).
000299     12  DL23-RETURN-CODE          PIC XX.
000300     12  DL23-CODE-DESC.
000301         16  DL23-CODE-TEXT        PIC X(20).
000302         16  DL23-RET-CODE         PIC X(40).
000303     12  DL23-GEN-DESC-1           PIC X(20).
000304     12  DL23-GEN-DESC-2           PIC X(20).
000305     12  DL23-GEN-DESC-3           PIC X(20).
000306
000307 01  DMD-DATE-YYYYMMDD.
000308     12  DMD-DECADE          PIC XX      VALUE SPACES.
000309     12  DMD-YYMMDD.
000310         16  DMD-YY          PIC XX      VALUE SPACES.
000311         16  DMD-MM          PIC XX      VALUE SPACES.
000312         16  DMD-DD          PIC XX      VALUE SPACES.
000313
000314     12  W-NAME-LAST             PIC  X(15).
000315     12  W-NAME-FIRST            PIC  X(15).
000316     12  W-NAME-MIDDLE.
000317         16  FILLER              PIC  X(01).
000318         16  W-NAME-MIDDLE-2     PIC  X(01).
000319         16  FILLER              PIC  X(13).
000320
000321*                                    COPY ELCDCTB.
      *>>((file: ELCDCTB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDCTB.                            *
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   DESCRIPTION = DISTRIBUTION CONTROL TABLE MAINTENANCE PROGRAM *
000008*       COMMUNICATIONS AREA                                      *
000009*                                                                *
000010******************************************************************
000011 01  DCT-COMMUNICATION-AREA.
000012     12  DCT-BILLING-BANK-ID      PIC  X(05).
000013     12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).
000014     12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).
000015     12  DCT-PRODUCT-CODE         PIC  X(02).
000016     12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).
000017     12  DCT-RETURN-CODE          PIC  X(02).
000018     12  DCT-MAIL-CODE            PIC  X(05).
000019     12  DCT-DISTRIBUTION-CODE    PIC  X(04).
000020     12  DCT-MSA-ACCT-NO          PIC  X(07).
      *<<((file: ELCDCTB))
000322     EJECT
000323*                                    COPY ELCATTR.
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
000324     EJECT
000325*                                    COPY ELCLOGOF.
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
000326     EJECT
000327*                                    COPY ELCDATE.
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
000328     EJECT
000329*                                    COPY ELCEMIB.
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
000330     EJECT
000331*                                    COPY EL151S.
      *>>((file: EL151S))
000001 01  EL151AI.
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
000016     05  MLINE1L PIC S9(0004) COMP.
000017     05  MLINE1F PIC  X(0001).
000018     05  FILLER REDEFINES MLINE1F.
000019         10  MLINE1A PIC  X(0001).
000020     05  MLINE1I PIC  X(0060).
000021*    -------------------------------
000022     05  MLINE2L PIC S9(0004) COMP.
000023     05  MLINE2F PIC  X(0001).
000024     05  FILLER REDEFINES MLINE2F.
000025         10  MLINE2A PIC  X(0001).
000026     05  MLINE2I PIC  X(0060).
000027*    -------------------------------
000028     05  MDENCDL PIC S9(0004) COMP.
000029     05  MDENCDF PIC  X(0001).
000030     05  FILLER REDEFINES MDENCDF.
000031         10  MDENCDA PIC  X(0001).
000032     05  MDENCDI PIC  X(0004).
000033*    -------------------------------
000034     05  MPRFDTL PIC S9(0004) COMP.
000035     05  MPRFDTF PIC  X(0001).
000036     05  FILLER REDEFINES MPRFDTF.
000037         10  MPRFDTA PIC  X(0001).
000038     05  MPRFDTI PIC  X(0008).
000039*    -------------------------------
000040     05  MERMSG1L PIC S9(0004) COMP.
000041     05  MERMSG1F PIC  X(0001).
000042     05  FILLER REDEFINES MERMSG1F.
000043         10  MERMSG1A PIC  X(0001).
000044     05  MERMSG1I PIC  X(0070).
000045*    -------------------------------
000046     05  MERMSG2L PIC S9(0004) COMP.
000047     05  MERMSG2F PIC  X(0001).
000048     05  FILLER REDEFINES MERMSG2F.
000049         10  MERMSG2A PIC  X(0001).
000050     05  MERMSG2I PIC  X(0070).
000051*    -------------------------------
000052     05  MPFNUMBL PIC S9(0004) COMP.
000053     05  MPFNUMBF PIC  X(0001).
000054     05  FILLER REDEFINES MPFNUMBF.
000055         10  MPFNUMBA PIC  X(0001).
000056     05  MPFNUMBI PIC  99.
000057 01  EL151AO REDEFINES EL151AI.
000058     05  FILLER            PIC  X(0012).
000059*    -------------------------------
000060     05  FILLER            PIC  X(0003).
000061     05  MRNDATEO PIC  X(0008).
000062*    -------------------------------
000063     05  FILLER            PIC  X(0003).
000064     05  MRNTIMEO PIC  99.99.
000065*    -------------------------------
000066     05  FILLER            PIC  X(0003).
000067     05  MLINE1O PIC  X(0060).
000068*    -------------------------------
000069     05  FILLER            PIC  X(0003).
000070     05  MLINE2O PIC  X(0060).
000071*    -------------------------------
000072     05  FILLER            PIC  X(0003).
000073     05  MDENCDO PIC  X(0004).
000074*    -------------------------------
000075     05  FILLER            PIC  X(0003).
000076     05  MPRFDTO PIC  99B99B99.
000077*    -------------------------------
000078     05  FILLER            PIC  X(0003).
000079     05  MERMSG1O PIC  X(0070).
000080*    -------------------------------
000081     05  FILLER            PIC  X(0003).
000082     05  MERMSG2O PIC  X(0070).
000083*    -------------------------------
000084     05  FILLER            PIC  X(0003).
000085     05  MPFNUMBO PIC  99.
000086*    -------------------------------
      *<<((file: EL151S))
000332     EJECT
000333*                                    COPY ELCDMO.
      *>>((file: ELCDMO))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDMO.                             *
000005*                            VMOD=2.004                          *
000006*                                                                *
000007*   FILE DESCRIPTION = DLO025 (DMO FILE MAINTENANCE PGRM)        *
000008*        COMMUNICATION AREA                                      *
000009*   FILE TYPE = NA                                               *
000010*   RECORD SIZE = 110    RECFORM = FIXED                         *
000011*                                                                *
000012******************************************************************
000013 01  DMO-COMMUNICATION-AREA.
000014     12  DM-RECORD-TYPE                  PIC  X(02).
000015             88  DM-ISSUE-TRAN                VALUE 'CC'.
000016             88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.
000017             88  DM-CLAIM-PAYMENT             VALUE 'DR'.
000018     12  DM-DIST-CODE                    PIC  X(04).
000019     12  DM-MAIL-CODE                    PIC  X(05).
000020     12  DM-CREDIT-CARD-NUMBER           PIC  X(16).
000021     12  DM-INSURED-NAME                 PIC  X(30).
000022     12  DM-CLAIM-NO                     PIC  X(07).
000023     12  DM-CLAIM-TYPE                   PIC  X.
000024
000025     12  DM-STATUS-DATA-AREA.
000026         16  DM-CLAIM-STATUS             PIC  X.
000027             88  DM-OPEN-NO-PAYMENTS              VALUE '1'.
000028             88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.
000029             88  DM-CLOSED                        VALUE '3'.
000030             88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.
000031             88  DM-DEFAULT                       VALUE '9'.
000032         16  DM-STATUS-DATE              PIC  X(08).
000033******YYYYMMDD
000034         16  DM-STAT-CHANGE-TYPE         PIC  X.
000035             88  DM-MANUAL-CLOSE                  VALUE 'C'.
000036             88  DM-CLAIM-DENIED                  VALUE 'D'.
000037             88  DM-FINAL-PAYMENT                 VALUE 'F'.
000038             88  DM-INITIAL-PAYMENT               VALUE 'I'.
000039             88  DM-AUTO-CLOSE                    VALUE 'Q'.
000040             88  DM-RE-OPENED                     VALUE 'R'.
000041             88  DM-NEW-CLAIM-SETUP               VALUE 'S'.
000042             88  DM-VOIDED-PAYMENT                VALUE 'V'.
000043             88  DM-CLAIM-DELETED                 VALUE 'X'.
000044         16  DM-STAT-CARRIER             PIC X.
000045
000046     12  DM-DRAFT-DATA-AREA.
000047         16  DM-PAYMENT-TYPE             PIC  X.
000048             88  DM-VALID-CLAIM-TYPES VALUES 'L' 'D' 'U' 'A'.
000049         16  DM-PAYMENT-AMT              PIC  9(05)V9(02).
000050         16  DM-PAYMENT-DATE             PIC  X(08).
000051******YYYYMMDD
000052         16  DM-CERT-NO                  PIC  X(11).
000053         16  DM-TRLR-SEQ-NO              PIC  9(04).
000054         16  DM-CARRIER                  PIC  X.
000055
000056     12  DM-RETURN-CODE                  PIC  XX.
      *<<((file: ELCDMO))
000334     EJECT
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
000336 01  DFHCOMMAREA                     PICTURE X(1024).
000337     EJECT
000338*                                    COPY ELCMSTR.
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
000339     EJECT
000340*                                    COPY ELCTRLR.
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
000341     EJECT
000342*                                    COPY ELCARCH.
      *>>((file: ELCARCH))
000001******************************************************************
000002*                                                                *
000003*                            ELCARCH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.007                          *
000006*                                                                *
000007*   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 090  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
000013*       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
000019******************************************************************
000020 01  LETTER-ARCHIVE.
000021     12  LA-RECORD-ID                PIC XX.
000022         88  VALID-LA-ID                VALUE 'LA'.
000023
000024     12  LA-CONTROL-PRIMARY.
000025         16  LA-COMPANY-CD           PIC X.
000026         16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
000027         16  LA-RECORD-TYPE          PIC X.
000028             88  LA-HEADER-DATA         VALUE '1'.
000029             88  LA-ADDRESS-DATA        VALUE '2'.
000030             88  LA-TEXT-DATA           VALUE '3'.
000031             88  LA-FORM-CONTROL-HDR    VALUE '4'.
000032         16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
000033
000034     12  LA-CONTROL-BY-TYPE.
000035         16  LA-COMPANY-CD-A1        PIC X.
000036         16  LA-RECORD-TYPE-A1       PIC X.
000037         16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
000038         16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
000039
000040     12  LA-TEXT-RECORD.
000041         16  LA-SKIP-CONTROL         PIC XX.
000042             88  NO-LINES-SKIPPED       VALUE SPACES.
000043             88  SKIP-TO-NEXT-PAGE      VALUE '99'.
000044         16  LA-TEXT-LINE            PIC X(70).
000045
000046     12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
000047         16  FILLER                  PIC XX.
000048         16  LA-ADDRESS-LINE         PIC X(30).
000049         16  FILLER                  PIC X(40).
000050
000051     12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
000052         16  FILLER                  PIC XX.
000053         16  LA-CARRIER              PIC X.
000054         16  LA-CLAIM-NO             PIC X(7).
000055         16  LA-CERT-NO.
000056             20  LA-CERT-PRIME       PIC X(10).
000057             20  LA-CERT-SFX         PIC X.
000058         16  LA-NO-OF-COPIES         PIC S9.
000059         16  LA-RESEND-DATE          PIC XX.
000060         16  LA-PROCESSOR-CD         PIC X(4).
000061         16  LA-CREATION-DT          PIC XX.
000062         16  LA-INITIAL-PRINT-DATE   PIC XX.
000063         16  LA-RESEND-PRINT-DATE    PIC XX.
000064         16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
000065         16  LA-1ST-RESEND-PRINT-DT  PIC XX.
000066*
000067* -----  16  LA-DMD-ADDITIONAL-FIELDS.
000068*   I        20  LA-DMD-LETTER-FORM      PIC X(4).
000069*   I        20  LA-DMD-PROD-CODE        PIC XX.
000070*   I        20  LA-DMD-RES-ST           PIC XX.
000071*   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
000072*   I        20  LA-DMD-LETTER-STATUS    PIC X.
000073*  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
000074*  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
000075*  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
000076*   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
000077*   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
000078*   I        20  LA-DMD-UND-CODE         PIC XX.
000079*   I        20  LA-DMD-BEN-CODE         PIC XX.
000080*   V    16  FILLER                  PIC X(15).
000081* -----
000082*
000083* REINSERTED  CSO  MODS
000084*
000085         16  FILLER.
000086             20  FILLER                  PIC X(29).
000087             20  LA-CSO-LETTER-STATUS    PIC X.
000088                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
000089                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
000090                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
000091             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
000092             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
000093*
000094
000095     12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
000096         16  FILLER                  PIC XX.
000097         16  LA4-CARRIER             PIC X.
000098         16  LA4-CLAIM-NO            PIC X(7).
000099         16  LA4-CERT-NO.
000100             20  LA4-CERT-PRIME      PIC X(10).
000101             20  LA4-CERT-SFX        PIC X.
000102         16  LA4-NO-OF-COPIES        PIC S9.
000103         16  LA4-RESEND-DATE         PIC XX.
000104         16  LA4-PROCESSOR-CD        PIC X(4).
000105         16  LA4-CREATION-DT         PIC XX.
000106         16  LA4-INITIAL-PRINT-DATE  PIC XX.
000107         16  LA4-RESEND-PRINT-DATE   PIC XX.
000108         16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
000109         16  LA4-FORM-TYPE           PIC X.
000110             88  LA4-INITIAL-FORM    VALUE '1'.
000111             88  LA4-PROGRESS-FORM   VALUE '2'.
000112         16  LA4-FORM-REM-PRINT-DT   PIC X(02).
000113         16  LA4-STATE               PIC X(02).
000114         16  FILLER                  PIC X(31).
000115******************************************************************
      *<<((file: ELCARCH))
000343     EJECT
000344*                                    COPY ELCCNTL.
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
000345
000346*                                    COPY ELCDENY.
      *>>((file: ELCDENY))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDENY                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM DENIAL/RECESSION/REFORMATION TABLE              *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000010*   VSAM DENIAL TABLE                                            *
000011*                                                                *
000012*   FILE DESCRIPTION = DENIAL CODE TABLE                         *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 125   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELCDENY                   RKP=2,LEN=15   *
000018*       ALTERNATE PATH1 = ELDENY2 (ALT GROUPING) RKP=17,LEN=16   *
000019*                                                                *
000020*   LOG = NO                                                     *
000021*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000022*                                                                *
000023*                                                                *
000024******************************************************************
000025*                   C H A N G E   L O G
000026*
000027* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000028*-----------------------------------------------------------------
000029*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000030* EFFECTIVE    NUMBER
000031*-----------------------------------------------------------------
000032* 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
000033******************************************************************
000034
000035 01  DENIAL-CODES.
000036     12  DN-RECORD-ID                      PIC XX.
000037         88  VALID-DN-ID                      VALUE 'DN'.
000038
000039     12  DN-CONTROL-PRIMARY.
000040         16  DN-COMPANY-CD                 PIC X.
000041         16  DN-DENIAL-CODE                PIC X(4).
000042         16  FILLER                        PIC X(10).
000043
000044     12  DN-CONTROL-BY-TYPE.
000045         16  DN-COMPANY-CD-A1              PIC X.
000046         16  DN-RECORD-TYPE                PIC X.
000047         16  DN-DENIAL-CODE-A1             PIC X(4).
000048         16  FILLER                        PIC X(10).
000049     12  DN-MAINT-INFORMATION.
000050         16  DN-LAST-MAINT-DT              PIC XX.
000051         16  DN-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000052         16  DN-LAST-MAINT-USER            PIC X(4).
000053         16  FILLER                        PIC XX.
000054
000055     12  DN-DESCRIPTION                    PIC X(50).
000056     12  FILLER                            PIC X(30).
000057******************************************************************
      *<<((file: ELCDENY))
000347
000348*                                    COPY ELCDAR.
      *>>((file: ELCDAR))
000001******************************************************************
000002*                                                                *
000003*   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
000004*   FILE TYPE = VSAM,KSDS                                        *
000005*   RECORD SIZE = 25   RECFORM = FIXED                           *
000006*   BASE CLUSTER = DLYACTV                                       *
000007*   LOG = YES                                                    *
000008*   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
000009*               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
000010*               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
000011*               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
000012*               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
000013*               BY PROGRAM "LGINFCE".                            *
000014*                                                                *
000015******************************************************************
000016 01  DAILY-ACTIVITY-RECORD.
000017     05  DA-KEY.
000018         10  DA-COMP-CD          PIC X.
000019         10  DA-CARRIER          PIC X.
000020         10  DA-CLAIM-NO         PIC X(7).
000021         10  DA-CERT-NO.
000022             15  DA-CERT-PRIME   PIC X(10).
000023             15  DA-CERT-SFX     PIC X.
000024     05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
000025     05  DA-RECORD-TYPE          PIC X.
000026     05  FILLER                  PIC X(2).
000027******************************************************************
      *<<((file: ELCDAR))
000349     EJECT
000350*                                    COPY ERCDMDNT.
      *>>((file: ERCDMDNT))
000001******************************************************************
000002*                                                                *
000003*                            ERCDMDNT                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.005                          *
000006*                                                                *
000007*        FILE DESCRIPTION = DMD CERTIFICATE NOTES                *
000008*                                                                *
000009*        THIS COPYBOOK IS A REDEFINES OF ERCNOTE -               *
000010*                                                                *
000011*        FILE TYPE= VSAM,KSDS                                    *
000012*        RECORD SIZE = 825    RECFORM = FIXED                    *
000013*                                                                *
000014*        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
000015*                                                                *
000016*        LOG = YES                                               *
000017*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000018*                                                                *
000019******************************************************************
000020
000021 01  CERTIFICATE-NOTE.
000022     12  CN-RECORD-ID                     PIC  XX.
000023         88  VALID-CN-ID                      VALUE 'CN'.
000024
000025     12  CN-CONTROL-PRIMARY.
000026         16  CN-COMPANY-CD                PIC X.
000027         16  CN-CERT-KEY.
000028             20  CN-CARRIER               PIC X.
000029             20  CN-GROUPING.
000030                 24  CN-GROUPING-PREFIX   PIC XXX.
000031                 24  CN-GROUPING-PRIME    PIC XXX.
000032             20  CN-STATE                 PIC XX.
000033             20  CN-ACCOUNT.
000034                 24  CN-ACCOUNT-PREFIX    PIC X(4).
000035                 24  CN-ACCOUNT-PRIME     PIC X(6).
000036             20  CN-CERT-EFF-DT           PIC XX.
000037             20  CN-CERT-NO.
000038                 24  CN-CERT-PRIME        PIC X(10).
000039                 24  CN-CERT-SFX          PIC X.
000040
000041     12  CN-BILLING-START-LINE-NO         PIC 99.
000042     12  CN-BILLING-END-LINE-NO           PIC 99.
000043
000044     12  CN-LINES.
000045         16  CN-LINE                      PIC X(77)  OCCURS 10.
000046
000047     12  CN-CSI-NOTES REDEFINES CN-LINES.
000048         16  CN-CSI-TEXT-NOTES            PIC X(77)  OCCURS 6.
000049         16  CN-CSI-GENERAL-DATA-AREA.
000050             20  CN-CSI-GENERAL-DATA      PIC X(77)  OCCURS 2.
000051
000052         16  CN-CSI-GENERAL-DATA-R REDEFINES
000053             CN-CSI-GENERAL-DATA-AREA.
000054             20  CN-CSI-GEN-NOC-KEY           PIC X(11).
000055             20  CN-CSI-GEN-PRI-INSD-1ST-NAME PIC X(15).
000056             20  CN-CSI-GEN-SEC-INSD-1ST-NAME PIC X(15).
000057             20  CN-CSI-GEN-INSD-WORK-PHONE   PIC X(10).
000058             20  CN-CSI-GEN-INFRM-1ST-NAME    PIC X(15).
000059             20  CN-CSI-GEN-INFRM-LAST-NAME   PIC X(20).
000060             20  CN-CSI-GEN-INFRM-MI          PIC X.
000061             20  CN-CSI-GEN-INFRM-PHONE       PIC X(10).
000062             20  CN-CSI-GEN-INFRM-REL         PIC X(15).
000063             20  FILLER                       PIC XX.
000064             20  CN-CSI-GEN-DATA-SOURCE       PIC XX.
000065             20  FILLER                       PIC X(38).
000066
000067         16  CN-CSI-PRODUCT-DATA-AREA.
000068             20  CN-CSI-PRODUCT-DATA      PIC X(77)  OCCURS 2.
000069
000070         16  CN-CSI-CREDIT-CARD-DATA REDEFINES
000071             CN-CSI-PRODUCT-DATA-AREA.
000072             20  CN-CSI-CC-BILL-BANK-ID   PIC X(6).
000073             20  CN-CSI-CC-CANCEL-CD      PIC XX.
000074             20  CN-CSI-CC-CANCEL-DT      PIC X(8).
000075             20  CN-CSI-CC-CARD-TYPE      PIC XX.
000076             20  CN-CSI-CC-CHANGE-AGE     PIC 999.
000077             20  CN-CSI-CC-DIAGNOSIS-CD   PIC X(6).
000078             20  FILLER                   PIC XX.
000079             20  CN-CSI-CC-INSURED-BAL    PIC S9(5)V99  COMP-3.
000080             20  CN-CSI-CC-INTEREST-AMT   PIC S9(5)V99  COMP-3.
000081             20  CN-CSI-CC-INTEREST-PAID  PIC X.
000082             20  CN-CSI-CC-ISSUE-ST       PIC XX.
000083             20  CN-CSI-CC-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
000084             20  CN-CSI-CC-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
000085             20  CN-CSI-CC-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
000086             20  CN-CSI-CC-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
000087             20  CN-CSI-CC-OLD-ACCT-NO    PIC X(20).
000088             20  CN-CSI-CC-POLICY-TYPE    PIC XXX.
000089             20  CN-CSI-CC-PREMIUM-AMT    PIC S999V99   COMP-3.
000090             20  CN-CSI-CC-PREMIUM-RT     PIC S999V999  COMP-3.
000091             20  CN-CSI-CC-PREV-CLAIM-NO  PIC X(7).
000092             20  CN-CSI-CC-SIGNED-DT      PIC X(8).
000093             20  CN-CSI-CC-SPECIAL-TERM   PIC S999      COMP-3.
000094             20  CN-CSI-CC-STMNT-DT       PIC X(8).
000095             20  CN-CSI-CC-TERM-AGE       PIC 999.
000096             20  CN-CSI-CC-TOL-BALANCE    PIC S9(5)V99  COMP-3.
000097             20  CN-CSI-CC-WAIV-PREM-FLAG PIC X.
000098             20  CN-CSI-CC-ISSUE-DT       PIC X(8).
000099             20  CN-CSI-CC-BEN-CALC-SW    PIC X.
000100             20  CN-CSI-CC-TERM-ROUND-SW  PIC X.
000101             20  FILLER                   PIC X(25).
000102
000103         16  CN-CSI-FAMILY-LEAVE-DATA REDEFINES
000104             CN-CSI-CREDIT-CARD-DATA.
000105             20  CN-CSI-FL-BILL-BANK-ID   PIC X(6).
000106             20  CN-CSI-FL-CANCEL-CD      PIC XX.
000107             20  CN-CSI-FL-CANCEL-DT      PIC X(8).
000108             20  CN-CSI-FL-CARD-TYPE      PIC XX.
000109             20  CN-CSI-FL-CHANGE-AGE     PIC 999.
000110             20  CN-CSI-FL-DIAGNOSIS-CD   PIC X(6).
000111             20  FILLER                   PIC XX.
000112             20  CN-CSI-FL-INSURED-BAL    PIC S9(5)V99  COMP-3.
000113             20  CN-CSI-FL-INTEREST-AMT   PIC S9(5)V99  COMP-3.
000114             20  CN-CSI-FL-INTEREST-PAID  PIC X.
000115             20  CN-CSI-FL-ISSUE-ST       PIC XX.
000116             20  CN-CSI-FL-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
000117             20  CN-CSI-FL-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
000118             20  CN-CSI-FL-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
000119             20  CN-CSI-FL-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
000120             20  CN-CSI-FL-OLD-ACCT-NO    PIC X(20).
000121             20  CN-CSI-FL-POLICY-TYPE    PIC XXX.
000122             20  CN-CSI-FL-PREMIUM-AMT    PIC S999V99   COMP-3.
000123             20  CN-CSI-FL-PREMIUM-RT     PIC S999V999  COMP-3.
000124             20  CN-CSI-FL-PREV-CLAIM-NO  PIC X(7).
000125             20  CN-CSI-FL-SIGNED-DT      PIC X(8).
000126             20  CN-CSI-FL-SPECIAL-TERM   PIC S999      COMP-3.
000127             20  CN-CSI-FL-STMNT-DT       PIC X(8).
000128             20  CN-CSI-FL-TERM-AGE       PIC 999.
000129             20  CN-CSI-FL-TOL-BALANCE    PIC S9(5)V99  COMP-3.
000130             20  CN-CSI-FL-WAIV-PREM-FLAG PIC X.
000131             20  CN-CSI-FL-ISSUE-DT       PIC X(8).
000132             20  CN-CSI-FL-BEN-CALC-SW    PIC X.
000133             20  CN-CSI-FL-TERM-ROUND-SW  PIC X.
000134             20  CN-CSI-FL-LAST-DAY-WRKED PIC X(8).
000135             20  FILLER                   PIC X(17).
000136
000137         16  CN-CSI-SENIOR-LIFE-DATA REDEFINES
000138             CN-CSI-FAMILY-LEAVE-DATA.
000139             20  CN-CSI-SL-BENE-DOB       PIC X(8).
000140             20  CN-CSI-SL-BENE-NAME      PIC X(27).
000141             20  CN-CSI-SL-BENE-REL       PIC X(8).
000142             20  CN-CSI-SL-BENE-SSN       PIC S9(9)     COMP-3.
000143             20  CN-CSI-SL-BILL-BANK-ID   PIC X(6).
000144             20  CN-CSI-SL-CANCEL-DT      PIC X(8).
000145             20  CN-CSI-SL-DIAGNOSIS-CD   PIC X(6).
000146             20  CN-CSI-SL-INT-CHECK-DT   PIC X(8).
000147             20  CN-CSI-SL-INT-CHECK-NO   PIC S9(7)     COMP-3.
000148             20  CN-CSI-SL-INT-ON-PROCEEDS
000149                                          PIC S9(5)V99  COMP-3.
000150             20  CN-CSI-SL-ISSUE-DT       PIC X(8).
000151             20  CN-CSI-SL-ISSUE-ST       PIC XX.
000152             20  CN-CSI-SL-LIFE-PROCEEDS  PIC S9(5)V99  COMP-3.
000153             20  CN-CSI-SL-LOAN-INT-DUE   PIC S9(5)V99  COMP-3.
000154             20  CN-CSI-SL-POLICY-BENEFITS
000155                                          PIC S9(5)V99  COMP-3.
000156             20  CN-CSI-SL-POLICY-TYPE    PIC XXX.
000157             20  CN-CSI-SL-PREM-AMT       PIC S9(5)V99  COMP-3.
000158             20  CN-CSI-SL-PREM-CHECK-DT  PIC X(8).
000159             20  CN-CSI-SL-PREM-CHECK-NO  PIC S9(7)     COMP-3.
000160             20  CN-CSI-SL-PREM-DUE       PIC S9(5)V99  COMP-3.
000161             20  CN-CSI-SL-PREM-MODE      PIC 99.
000162             20  CN-CSI-SL-PREM-REFUND    PIC S9(5)V99  COMP-3.
000163             20  CN-CSI-SL-PREM-SUSP-DT   PIC X(8).
000164             20  CN-CSI-SL-SIGNED-DT      PIC X(8).
000165             20  CN-CSI-SL-STATE-NOT      PIC X.
000166             20  FILLER                   PIC XX.
000167
000168         16  CN-CSI-PURCH-PROP-DATA REDEFINES
000169             CN-CSI-SENIOR-LIFE-DATA.
000170             20  CN-CSI-PP-CARD-TYPE      PIC XX.
000171             20  CN-CSI-PP-CHANGE-AGE     PIC 999.
000172             20  CN-CSI-PP-BEN-PAID-TO-DATE
000173                                          PIC S9(5)V99  COMP-3.
000174             20  CN-CSI-PP-BILL-BANK-ID   PIC X(6).
000175             20  CN-CSI-PP-CANCEL-CD      PIC XX.
000176             20  CN-CSI-PP-CANCEL-DT      PIC X(8).
000177             20  CN-CSI-PP-DIAGNOSIS-CD   PIC X(6).
000178             20  CN-CSI-PP-ISSUE-DT       PIC X(8).
000179             20  CN-CSI-PP-ISSUE-ST       PIC XX.
000180             20  CN-CSI-PP-MANUFACTURER   PIC X(17).
000181             20  CN-CSI-PP-MODEL-NO       PIC X(8).
000182             20  CN-CSI-PP-OLD-ACCT-NO    PIC X(20).
000183             20  CN-CSI-PP-POLICY-TYPE    PIC XXX.
000184             20  CN-CSI-PP-PREMIUM-RT     PIC S999V999  COMP-3.
000185             20  CN-CSI-PP-PREV-CLAIM-NO  PIC X(7).
000186             20  CN-CSI-PP-PURCHASE-DT    PIC X(8).
000187             20  CN-CSI-PP-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
000188             20  CN-CSI-PP-REPAIR         PIC X.
000189             20  CN-CSI-PP-REPLACE        PIC X.
000190             20  CN-CSI-PP-SERIAL-NO      PIC X(16).
000191             20  CN-CSI-PP-SIGNED-DT      PIC X(8).
000192             20  CN-CSI-PP-STMNT-DT       PIC X(8).
000193             20  CN-CSI-PP-TERM-AGE       PIC 999.
000194             20  FILLER                   PIC X(5).
000195
000196         16  CN-CSI-EXT-WARR-DATA REDEFINES
000197             CN-CSI-PURCH-PROP-DATA.
000198             20  CN-CSI-EW-CARD-TYPE      PIC XX.
000199             20  CN-CSI-EW-CHANGE-AGE     PIC 999.
000200             20  CN-CSI-EW-BILL-BANK-ID   PIC X(6).
000201             20  CN-CSI-EW-CANCEL-CD      PIC XX.
000202             20  CN-CSI-EW-CANCEL-DT      PIC X(8).
000203             20  CN-CSI-EW-DIAGNOSIS-CD   PIC X(6).
000204             20  CN-CSI-EW-ISSUE-DT       PIC X(8).
000205             20  CN-CSI-EW-ISSUE-ST       PIC XX.
000206             20  CN-CSI-EW-MANUFACTURER   PIC X(17).
000207             20  CN-CSI-EW-MODEL-NO       PIC X(8).
000208             20  CN-CSI-EW-OLD-ACCT-NO    PIC X(20).
000209             20  CN-CSI-EW-POLICY-TYPE    PIC XXX.
000210             20  CN-CSI-EW-PREMIUM-RT     PIC S999V999  COMP-3.
000211             20  CN-CSI-EW-PREV-CLAIM-NO  PIC X(7).
000212             20  CN-CSI-EW-PURCHASE-DT    PIC X(8).
000213             20  CN-CSI-EW-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
000214             20  CN-CSI-EW-REPAIR-COST    PIC S9(5)V99  COMP-3.
000215             20  CN-CSI-EW-REPLACE        PIC X.
000216             20  CN-CSI-EW-SERIAL-NO      PIC X(16).
000217             20  CN-CSI-EW-SIGNED-DT      PIC X(8).
000218             20  CN-CSI-EW-STMNT-DT       PIC X(8).
000219             20  CN-CSI-EW-TERM-AGE       PIC 999.
000220             20  CN-CSI-EW-WARRANTY-NO    PIC 99.
000221             20  FILLER                   PIC X(4).
000222
000223     12  CN-LAST-MAINT-DT                 PIC XX.
000224     12  CN-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
000225     12  CN-LAST-MAINT-USER               PIC X(4).
000226     12  FILLER                           PIC X(6).
000227
000228******************************************************************
      *<<((file: ERCDMDNT))
000351     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL151' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000352 VCOBOL-DUMMY-PROCEDURE.
000353
000354     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000355     MOVE '5'                    TO DC-OPTION-CODE.
000356     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000357     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000358     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000359
000360 0001-PROCESSING-EXITS.
000361     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000362     IF EIBCALEN NOT GREATER THAN ZEROS
000363       GO TO 8800-UNAUTHORIZED-ACCESS.
000364
000365     
      * EXEC CICS  HANDLE CONDITION
000366*           ERROR    (9990-ABEND)
000367*           PGMIDERR (9600-PGMID-ERROR)
000368*    END-EXEC.
      *    MOVE '"$.L                  ! " #00004163' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303034313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000369
000370     IF PI-CALLING-PROGRAM  NOT  = THIS-PGM
000371         IF PI-RETURN-TO-PROGRAM NOT  =  THIS-PGM
000372             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000373             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000374             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000375             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000376             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000377             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000378             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000379             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000380         ELSE
000381             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000382             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000383             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000384             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000385             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000386             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000387             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000388             MOVE SPACES               TO PI-SAVED-PROGRAM-6.
000389
000390     IF EIBAID IS EQUAL TO DFHCLEAR
000391         GO TO 9400-CLEAR.
000392
000393     IF PI-PROCESSOR-ID = 'LGXX'
000394         NEXT SENTENCE
000395     ELSE
000396         
      * EXEC CICS READQ TS
000397*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000398*            INTO    (SECURITY-CONTROL)
000399*            LENGTH  (SC-COMM-LENGTH)
000400*            ITEM    (SC-ITEM)
000401*        END-EXEC
      *    MOVE '*$II   L              ''   #00004194' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034313934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000402         MOVE SC-CLAIMS-DISPLAY (6)    TO  PI-DISPLAY-CAP
000403         MOVE SC-CLAIMS-UPDATE  (6)    TO  PI-MODIFY-CAP.
000404
000405     IF EIBTRNID IS EQUAL TO WS-TRANS-ID
000406         GO TO 0200-RECEIVE.
000407
000408 0100-FIRST-TIME-IN.
000409
000410     MOVE LOW-VALUES                 TO  EL151AO
000411     MOVE -1                         TO  MLINE1L
000412     MOVE 1                          TO  EMI-NUMBER-OF-LINES.
000413
000414     MOVE SPACE                      TO  DC-ERROR-CODE
000415                                         WS-CLAIM-SEQ-SW
000416                                         MERMSG2O.
000417
000418     MOVE '153A'                     TO  PI-CURRENT-SCREEN-NO.
000419
000420     MOVE 0                          TO  PI-PROOF-DATE.
000421     PERFORM 3550-FIND-LETTER-TRLR THRU 3550-EXIT.
000422
000423     GO TO 8100-SEND-INITIAL-MAP.
000424
000425 0200-RECEIVE.
000426
000427     MOVE LOW-VALUES              TO  EL151AI.
000428
000429     IF (EIBAID IS EQUAL TO DFHPA1 OR DFHPA2 OR DFHPA3)
000430         MOVE ER-0008             TO  EMI-ERROR
000431         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000432         MOVE -1                  TO  MPFNUMBL
000433         GO TO 8200-SEND-DATAONLY.
000434
000435     
      * EXEC CICS RECEIVE
000436*        MAP      (MAP-NAME)
000437*        MAPSET   (MAPSET-NAME)
000438*        INTO     (EL151AI)
000439*    END-EXEC.
           MOVE LENGTH OF
            EL151AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004233' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034323333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000440
000441     IF MPFNUMBL IS EQUAL TO +0
000442         GO TO 0300-CHECK-PFKEYS.
000443
000444     IF EIBAID IS NOT EQUAL TO DFHENTER
000445         MOVE ER-0004             TO  EMI-ERROR
000446         GO TO 0320-INPUT-ERROR.
000447
000448     IF (MPFNUMBI NUMERIC) AND (MPFNUMBI GREATER 0 AND LESS 25)
000449         MOVE PF-VALUES (MPFNUMBI)    TO  EIBAID
000450     ELSE
000451         MOVE ER-0029                 TO  EMI-ERROR
000452         GO TO 0320-INPUT-ERROR.
000453
000454 0300-CHECK-PFKEYS.
000455
000456     IF EIBAID IS EQUAL TO DFHPF12
000457        MOVE  'EL010'            TO  THIS-PGM
000458        GO TO 9300-XCTL.
000459
000460     IF EIBAID IS EQUAL TO DFHPF23
000461        MOVE EIBAID              TO  PI-ENTRY-CD-1
000462        MOVE 'EL005'             TO  THIS-PGM
000463        GO TO 9300-XCTL.
000464
000465     IF EIBAID IS EQUAL TO DFHPF24
000466        MOVE EIBAID              TO  PI-ENTRY-CD-1
000467        MOVE  'EL126'            TO  THIS-PGM
000468        GO TO 9300-XCTL.
000469
000470     IF EIBAID IS EQUAL TO DFHENTER
000471        GO TO 0330-EDIT-DATA.
000472
000473     MOVE ER-0029                TO  EMI-ERROR.
000474
000475 0320-INPUT-ERROR.
000476     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000477
000478     MOVE -1                     TO  MPFNUMBL.
000479     GO TO 8200-SEND-DATAONLY.
000480
000481     EJECT
000482 0330-EDIT-DATA.
000483     IF NOT MODIFY-CAP
000484        MOVE 'UPDATE'             TO  SM-READ
000485        PERFORM 9995-SECURITY-VIOLATION
000486        MOVE ER-0070              TO  EMI-ERROR
000487        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000488        GO TO 8100-SEND-INITIAL-MAP.
000489
000490     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'
000491                     OR 'AHL' or 'VPP' or 'FNL'
000492         IF MLINE1I = (LOW-VALUES OR SPACES) AND
000493            MLINE2I = (LOW-VALUES OR SPACES) AND
000494            MDENCDI = (LOW-VALUES OR SPACES)
000495             MOVE ER-0483        TO  EMI-ERROR
000496             MOVE -1             TO  MLINE1L
000497             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000498             GO TO 8100-SEND-INITIAL-MAP
000499         ELSE
000500             NEXT SENTENCE
000501     ELSE
000502         IF MLINE1I  = (LOW-VALUES OR SPACES)  AND
000503            MLINE2I  = (LOW-VALUES OR SPACES)
000504             MOVE ER-0483        TO  EMI-ERROR
000505             MOVE -1             TO  MLINE1L
000506             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000507             GO TO 8100-SEND-INITIAL-MAP.
000508
000509     IF MPRFDTL > 0
000510         MOVE MPRFDTI TO DEEDIT-FIELD
000511         PERFORM 9650-DEEDIT      THRU 9650-EXIT
000512         MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
000513         MOVE PI-PROOF-DATE    TO MPRFDTO
000514         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
000515     END-IF.
000516
000517     IF PI-PROOF-DATE = ZEROS OR LOW-VALUES
000518*     IF  MPRFDTI  = LOW-VALUES OR SPACES
000519         MOVE ER-0872             TO EMI-ERROR
000520         MOVE -1                  TO MPRFDTL
000521         MOVE AL-UABON            TO MPRFDTA
000522         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000523         GO TO 8200-SEND-DATAONLY
000524     END-IF.
000525
000526     IF  PI-PROOF-DATE > 0
000527         MOVE PI-PROOF-DATE       TO DEEDIT-FIELD
000528         PERFORM 9650-DEEDIT      THRU 9650-EXIT
000529         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000530         MOVE '4'                 TO DC-OPTION-CODE
000531         PERFORM 9700-LINK-DATE-CONVERT THRU  9700-EXIT
000532         IF DATE-CONVERSION-ERROR
000533            MOVE ER-0021          TO EMI-ERROR
000534            MOVE -1               TO MPRFDTL
000535            MOVE AL-UABON         TO MPRFDTA
000536            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000537            GO TO 8200-SEND-DATAONLY
000538         ELSE
000539            IF DC-BIN-DATE-1 > SAVE-BIN-DATE
000540                MOVE ER-0873      TO EMI-ERROR
000541                MOVE -1           TO MPRFDTL
000542                MOVE AL-UABON     TO MPRFDTA
000543                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000544                GO TO 8200-SEND-DATAONLY
000545            ELSE
000546                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
000547                MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
000548                MOVE PI-PROOF-DATE    TO MPRFDTO
000549                INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
000550            END-IF
000551         END-IF
000552     END-IF.
000553
000554     MOVE LOW-VALUES             TO  WS-TRAILER-KEY.
000555     MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.
000556     MOVE PI-CARRIER             TO  WS-KEY-CARRIER.
000557     MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.
000558     MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.
000559
000560     PERFORM 7000-READ-ELMSTR-UPDATE THRU 7000-EXIT.
000561
000562     IF CL-LAST-TRL-AVAIL
000563         MOVE ER-0137            TO  EMI-ERROR
000564         MOVE -1                 TO  MLINE1L
000565         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000566         GO TO 8200-SEND-DATAONLY.
000567
000568     if ws-prf-dt < cl-incurred-dt
000569        MOVE ER-0873             TO EMI-ERROR
000570        MOVE -1                  TO MPRFDTL
000571        MOVE AL-UABON            TO MPRFDTA
000572        PERFORM 9900-ERROR-FORMAT
000573                                 THRU 9900-EXIT
000574        GO TO 8200-SEND-DATAONLY
000575     end-if
000576
000577     MOVE CL-TRAILER-SEQ-CNT     TO  WS-ORIG-SEQ-CNT.
000578     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
000579     MOVE CL-TRAILER-SEQ-CNT     TO  WS-SEQ-NO-SAVED.
000580     MOVE 'C'                    TO  CL-CLAIM-STATUS.
000581     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
000582     MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
000583     MOVE '2'                    TO  CL-LAST-CLOSE-REASON.
000584     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
000585     MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT
000586                                     CL-LAST-CLOSE-DT.
000587
000588     IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
000589         IF MDENCDL IS GREATER THAN +0
000590             MOVE MDENCDI        TO  CL-FILE-LOCATION.
000591
000592*************************************************************
000593*****         START BUILDING ACTIVITY RECORD HERE          **
000594*************************************************************
000595     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'
000596         PERFORM 2300-GETMAIN-DLYACTV THRU
000597                 2300-EXIT
000598         PERFORM 3500-BUILD-ACTIVITY-RECORD THRU
000599                 3500-EXIT
000600         PERFORM 2350-WRITE-DLYACTV THRU
000601                 2350-EXIT
000602     END-IF.
000603
000604*************************************************************
000605*****            END OF BUILDING ACTIVITY RECORD           **
000606*************************************************************
000607
000608     PERFORM 7120-READ-ELTRLR-UPDATE THRU 7120-EXIT.
000609
000610     MOVE +1                     TO  MISC-SUB.
000611     MOVE SPACES                 TO  WS-OC-LOAD-SW.
000612     PERFORM 1000-SET-OPEN-CLOSE-HIST THRU 1000-EXIT.
000613
000614     MOVE +01                    TO  TR-SUB
000615                                     TR-SUB-2.
000616     PERFORM 1010-UPDATE-TR1-OC   THRU 1010-EXIT
000617             UNTIL  OC-HISTORY-LOADED.
000618
000619     MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY
000620     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
000621     MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.
000622
000623     IF PI-COMPANY-ID = 'DMD'
000624         PERFORM 2000-CREATE-DMD-DMO  THRU  2000-EXIT.
000625
000626     PERFORM 7130-REWRITE-ELTRLR THRU 7130-EXIT.
000627
000628     PERFORM 7100-GETMAIN-ELTRLR  THRU 7100-EXIT.
000629     PERFORM 1050-BUILD-DENIAL-TRLR THRU 1090-EXIT.
000630
000631     PERFORM 7110-WRITE-ELTRLR    THRU 7110-EXIT.
000632
000633     PERFORM 6100-CHECK-AUTO-ACTIVITY THRU 6100-EXIT.
000634
000635     IF WS-REC-FOUND-SW IS EQUAL TO 'N'
000636         GO TO 0340-FINISH-EDIT-DATA.
000637
000638     IF CL-ACTIVITY-CODE IS EQUAL TO 09
000639         GO TO 0340-FINISH-EDIT-DATA.
000640
000641     IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK') AND
000642        (CL-ACTIVITY-CODE IS EQUAL TO 11)
000643         GO TO 0340-FINISH-EDIT-DATA.
000644
000645     IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
000646         PERFORM 6000-RESET-AUTO-ACTIVITY THRU 6000-EXIT
000647         IF WS-UPDATE-SW IS EQUAL TO 'Y'
000648             MOVE LOW-VALUES         TO  CL-NEXT-RESEND-DT
000649                                         CL-NEXT-FOLLOWUP-DT.
000650
000651     IF WS-RESET-SW IS EQUAL TO 'Y'
000652         MOVE ZEROS                  TO  CL-ACTIVITY-CODE
000653         MOVE SAVE-BIN-DATE          TO  CL-ACTIVITY-MAINT-DT
000654         MOVE 'DENY'                 TO  CL-ACTIVITY-MAINT-TYPE.
000655
000656 0340-FINISH-EDIT-DATA.
000657
000658     PERFORM 7010-REWRITE-ELMSTR  THRU 7010-EXIT.
000659
000660     MOVE ER-0000                TO EMI-ERROR.
000661     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000662     MOVE -1                     TO MLINE1L.
000663*    MOVE LOW-VALUES             TO MLINE1O
000664*                                   MLINE2O
000665*                                   MDENCDO.
000666     GO TO 8200-SEND-DATAONLY.
000667
000668     EJECT
000669 1000-SET-OPEN-CLOSE-HIST.
000670     PERFORM 1000-BUMP-OPEN-CLOSE-HIST UNTIL
000671        (MISC-SUB GREATER THAN +6 OR
000672        AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES).
000673
000674     IF MISC-SUB GREATER THAN +1
000675        SUBTRACT +1 FROM MISC-SUB
000676        IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = 'C'
000677           MOVE SAVE-BIN-DATE   TO AT-OPEN-CLOSE-DATE (MISC-SUB)
000678           MOVE 'C'             TO AT-OPEN-CLOSE-TYPE (MISC-SUB)
000679           MOVE 'DENIED'        TO AT-OPEN-CLOSE-REASON (MISC-SUB)
000680           MOVE 'X'             TO WS-OC-LOAD-SW.
000681
000682     GO TO 1000-EXIT.
000683
000684 1000-BUMP-OPEN-CLOSE-HIST.
000685     ADD +1 TO MISC-SUB.
000686
000687 1000-EXIT.
000688     EXIT.
000689
000690 1010-UPDATE-TR1-OC.
000691     IF  AT-OPEN-CLOSE-DATE (TR-SUB) = (LOW-VALUE OR SPACES)
000692         MOVE SAVE-BIN-DATE      TO AT-OPEN-CLOSE-DATE (TR-SUB)
000693         MOVE 'C'                TO AT-OPEN-CLOSE-TYPE (TR-SUB)
000694         MOVE 'DENIED'           TO AT-OPEN-CLOSE-REASON (TR-SUB)
000695         MOVE 'X'                TO WS-OC-LOAD-SW
000696         GO TO 1010-EXIT.
000697
000698     ADD 1  TO TR-SUB.
000699     IF  TR-SUB  GREATER THAN 6
000700         MOVE +01                TO TR-SUB
000701         MOVE +02                TO TR-SUB-2
000702         PERFORM 1020-BUMP    THRU 1020-EXIT
000703          UNTIL TR-SUB-2 GREATER THAN 6
000704         MOVE SPACES             TO AT-OPEN-CLOSE-DATE (TR-SUB).
000705
000706 1010-EXIT.
000707      EXIT.
000708
000709 1020-BUMP.
000710     MOVE AT-OPEN-CLOSE-DATE (TR-SUB-2) TO
000711                           AT-OPEN-CLOSE-DATE (TR-SUB).
000712     MOVE AT-OPEN-CLOSE-TYPE (TR-SUB-2) TO
000713                           AT-OPEN-CLOSE-TYPE (TR-SUB).
000714     MOVE AT-OPEN-CLOSE-REASON (TR-SUB-2) TO
000715                           AT-OPEN-CLOSE-REASON (TR-SUB).
000716
000717     ADD +1  TO TR-SUB
000718                TR-SUB-2.
000719
000720 1020-EXIT.
000721     EXIT.
000722
000723     EJECT
000724 1050-BUILD-DENIAL-TRLR.
000725
000726     MOVE 'AT'                   TO AT-RECORD-ID.
000727     MOVE '8'                    TO AT-TRAILER-TYPE.
000728
000729*DLO023
000730     IF PI-COMPANY-ID NOT = 'DMD'
000731         GO TO 1055-EDIT-CID-CODE
000732     END-IF
000733
000734     IF MDENCDL GREATER +0
000735         MOVE 'CL'               TO DL23-SYSTEM-ID
000736         MOVE 'DN'               TO DL23-RECORD-TYPE
000737         MOVE MDENCDI            TO DL23-RECORD-KEY
000738         
      * EXEC CICS LINK
000739*            PROGRAM    ('DLO023')
000740*            COMMAREA   (WS-DLO-CODES-TABLE)
000741*            LENGTH     (DL23-COMM-LENGTH)
000742*        END-EXEC
           MOVE 'DLO023' TO DFHEIV1
      *    MOVE '."C                   (   #00004536' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034353336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-CODES-TABLE, 
                 DL23-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000743         IF DL23-RETURN-CODE = 'OK'
000744             MOVE AL-UANON       TO MDENCDA
000745          ELSE
000746             MOVE AL-UABON       TO MDENCDA
000747             MOVE -1             TO MDENCDL
000748             MOVE ER-0884        TO EMI-ERROR
000749             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000750             PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
000751             PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000752             GO TO 8200-SEND-DATAONLY.
000753
000754     IF MLINE1I = LOW-VALUES OR SPACES
000755         MOVE SPACES         TO AT-DENIAL-INFO-1
000756      ELSE
000757         MOVE MLINE1I        TO AT-DENIAL-INFO-1.
000758
000759     GO TO 1070-CONTINUE.
000760
000761 1055-EDIT-CID-CODE.
000762
000763     IF MDENCDL > +0
000764        MOVE LOW-VALUES          TO WS-ELDENY-KEY
000765        MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
000766        MOVE MDENCDI             TO ELDENY-DENIAL-CODE
000767        
      * EXEC CICS READ
000768*          DATASET('ELDENY')
000769*          SET    (ADDRESS OF DENIAL-CODES)
000770*          RIDFLD (WS-ELDENY-KEY)
000771*          RESP   (WS-RESPONSE)
000772*       END-EXEC
           MOVE 'ELDENY' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004565' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034353635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELDENY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF DENIAL-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000773        IF RESP-NORMAL
000774           IF MLINE1L > +0
000775              STRING DN-DESCRIPTION ' ' MLINE1I
000776                 DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
000777              END-STRING
000778           ELSE
000779              MOVE DN-DESCRIPTION
000780                                 TO AT-DENIAL-INFO-1
000781           END-IF
000782
000783           MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
000784           MOVE AT-DENIAL-INFO-1 TO MLINE1O
000785           MOVE +1               TO MLINE1L
000786           MOVE AL-UANON         TO MLINE1A
000787           MOVE DN-RECORD-TYPE
000788                                 TO CL-DENIAL-TYPE
000789        ELSE
000790           MOVE AL-UABON         TO MDENCDA
000791           MOVE -1               TO MDENCDL
000792           MOVE ER-0884          TO EMI-ERROR
000793           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000794           PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
000795           PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000796           GO TO 8200-SEND-DATAONLY
000797        END-IF
000798     ELSE
000799        MOVE AL-UABON            TO MDENCDA
000800        MOVE -1                  TO MDENCDL
000801        MOVE ER-0884             TO EMI-ERROR
000802        PERFORM 9900-ERROR-FORMAT
000803                                 THRU 9900-EXIT
000804        PERFORM 2250-UNLOCK-CLAIM-MSTR
000805                                 THRU 2250-EXIT
000806        PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000807        GO TO 8200-SEND-DATAONLY
000808     END-IF
000809
000810     GO TO 1070-CONTINUE
000811
000812     .
000813 1060-CONTINUE.
000814     IF MDENCDL IS GREATER THAN +0
000815         SET DEN-INDEX TO +1
000816         SEARCH DEN-TABLE VARYING DEN-INDEX
000817             AT END
000818                 MOVE MLINE1I                TO  AT-DENIAL-INFO-1
000819         WHEN MDENCDI IS EQUAL TO DENIAL-CODE (DEN-INDEX)
000820             MOVE DENIAL-DESC (DEN-INDEX)    TO  AT-DENIAL-INFO-1
000821                                                 MLINE1O
000822     ELSE
000823         IF MLINE1I = LOW-VALUES OR SPACES
000824             MOVE SPACES         TO AT-DENIAL-INFO-1
000825         ELSE
000826             MOVE MLINE1I        TO AT-DENIAL-INFO-1.
000827
000828 1070-CONTINUE.
000829     IF  MLINE2I  = LOW-VALUES OR SPACES
000830         MOVE  SPACES            TO AT-DENIAL-INFO-2
000831     ELSE
000832         MOVE MLINE2I            TO AT-DENIAL-INFO-2.
000833
000834     IF (AT-DENIAL-INFO-1 IS EQUAL TO SPACES OR LOW-VALUES)
000835         MOVE AT-DENIAL-INFO-2   TO AT-DENIAL-INFO-1
000836         MOVE SPACES             TO AT-DENIAL-INFO-2.
000837
000838     MOVE WS-PRF-DT              TO AT-DENIAL-PROOF-DT.
000839     MOVE WS-SEQ-NO-SAVED        TO WS-KEY-SEQUENCE-NO.
000840
000841     MOVE WS-TRAILER-KEY         TO AT-CONTROL-PRIMARY.
000842     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
000843                                    AT-DENIAL-LAST-UPDATED-BY.
000844     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
000845     MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT
000846                                    AT-DENIAL-DT
000847                                    AT-DENIAL-LAST-MAINT-DT.
000848     MOVE MDENCDI                TO AT-DENIAL-REASON-CODE.
000849     MOVE LOW-VALUES             TO AT-RETRACTION-DT.
000850
000851 1090-EXIT.
000852      EXIT.
000853      EJECT
000854******************************************************************
000855*    DMD ONLY  -  CREATE THE DIRECT MARKETING OUTPUT FILE
000856******************************************************************
000857 2000-CREATE-DMD-DMO.
000858
000859     MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.
000860     MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.
000861     MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.
000862
000863     
      * EXEC CICS HANDLE CONDITION
000864*         NOTFND   (2000-NOTE-NOT-FOUND)
000865*         END-EXEC.
      *    MOVE '"$I                   ! # #00004661' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034363631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000866
000867     
      * EXEC CICS READ
000868*         DATASET('ERNOTE')
000869*         SET    (ADDRESS OF CERTIFICATE-NOTE)
000870*         RIDFLD (W-NOTE-KEY)
000871*         END-EXEC.
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00004665' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303034363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000872
000873     MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
000874     MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
000875     MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
000876
000877     IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES
000878         MOVE 'CC'               TO DCT-PRODUCT-CODE
000879     ELSE
000880         MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
000881
000882     MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
000883     MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
000884
000885     
      * EXEC CICS LINK
000886*        PROGRAM    ('DLO006')
000887*        COMMAREA   (DCT-COMMUNICATION-AREA)
000888*        LENGTH     (WS-DCT-LENGTH)
000889*    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00004683' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034363833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000890
000891     IF  DCT-RETURN-CODE = 'OK'
000892         GO TO 2000-CONT.
000893
000894     IF  DCT-RETURN-CODE = '01' OR '02'
000895         GO TO 2000-EXIT.
000896
000897     IF  DCT-RETURN-CODE = '03'
000898         MOVE ER-0951            TO EMI-ERROR
000899         MOVE -1                 TO MLINE1L
000900         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000901         PERFORM 2250-UNLOCK-CLAIM-MSTR
000902             THRU 2250-EXIT
000903         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000904         GO TO 8200-SEND-DATAONLY.
000905
000906     IF  DCT-RETURN-CODE = '06'
000907         MOVE ER-0921            TO EMI-ERROR
000908         MOVE -1                 TO MLINE1L
000909         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000910         PERFORM 2250-UNLOCK-CLAIM-MSTR
000911             THRU 2250-EXIT
000912         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000913         GO TO 8200-SEND-DATAONLY.
000914
000915     IF  DCT-RETURN-CODE = '07'
000916         MOVE ER-0919            TO EMI-ERROR
000917         MOVE -1                 TO MLINE1L
000918         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000919         PERFORM 2250-UNLOCK-CLAIM-MSTR
000920             THRU 2250-EXIT
000921         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000922         GO TO 8200-SEND-DATAONLY.
000923
000924     IF  DCT-RETURN-CODE = '04'
000925         MOVE ER-0946            TO EMI-ERROR
000926         MOVE -1                 TO MLINE1L
000927         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000928         PERFORM 2250-UNLOCK-CLAIM-MSTR
000929             THRU 2250-EXIT
000930         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000931         GO TO 8200-SEND-DATAONLY.
000932
000933     IF  DCT-RETURN-CODE = '05'
000934         MOVE ER-0947            TO EMI-ERROR
000935         MOVE -1                 TO MLINE1L
000936         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000937         PERFORM 2250-UNLOCK-CLAIM-MSTR
000938             THRU 2250-EXIT
000939         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000940         GO TO 8200-SEND-DATAONLY.
000941
000942     IF  DCT-RETURN-CODE = '08'
000943         MOVE ER-0948            TO EMI-ERROR
000944         MOVE -1                 TO MLINE1L
000945         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000946         PERFORM 2250-UNLOCK-CLAIM-MSTR
000947             THRU 2250-EXIT
000948         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000949         GO TO 8200-SEND-DATAONLY.
000950
000951     IF  DCT-RETURN-CODE = 'N1'
000952         MOVE ER-0950            TO EMI-ERROR
000953         MOVE -1                 TO MLINE1L
000954         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000955         PERFORM 2250-UNLOCK-CLAIM-MSTR
000956             THRU 2250-EXIT
000957         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000958         GO TO 8200-SEND-DATAONLY.
000959
000960     IF DCT-RETURN-CODE = 'E1'
000961         MOVE ER-0974            TO EMI-ERROR
000962         MOVE -1                 TO MLINE1L
000963         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000964         PERFORM 2250-UNLOCK-CLAIM-MSTR
000965             THRU 2250-EXIT
000966         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000967         GO TO 8200-SEND-DATAONLY.
000968
000969     IF DCT-RETURN-CODE = 'E2'
000970         MOVE ER-0975            TO EMI-ERROR
000971         MOVE -1                 TO MLINE1L
000972         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000973         PERFORM 2250-UNLOCK-CLAIM-MSTR
000974             THRU 2250-EXIT
000975         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000976         GO TO 8200-SEND-DATAONLY.
000977
000978     IF DCT-RETURN-CODE NOT = 'OK'
000979         MOVE ER-0949            TO EMI-ERROR
000980         MOVE -1                 TO MLINE1L
000981         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000982         PERFORM 2250-UNLOCK-CLAIM-MSTR
000983             THRU 2250-EXIT
000984         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
000985         GO TO 8200-SEND-DATAONLY.
000986
000987 2000-CONT.
000988
000989     MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
000990     MOVE 'CS'                   TO DM-RECORD-TYPE.
000991     MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
000992     MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
000993     MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
000994     MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
000995     MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
000996
000997     MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1.
000998     MOVE ' '                    TO  DC-OPTION-CODE.
000999     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
001000     MOVE DC-GREG-DATE-1-YMD     TO  DMD-YYMMDD.
001001
001002     IF DMD-YY GREATER THAN 70
001003         MOVE '19'               TO  DMD-DECADE
001004     ELSE
001005         MOVE '20'               TO  DMD-DECADE.
001006
001007     MOVE DMD-DATE-YYYYMMDD      TO  DM-STATUS-DATE.
001008     MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
001009     MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
001010     MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
001011     PERFORM 2100-FORMAT-LAST-NAME-1ST THRU 2100-EXIT.
001012     MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
001013
001014     MOVE 'D'                    TO DM-STAT-CHANGE-TYPE.
001015     MOVE CL-CARRIER             TO DM-STAT-CARRIER.
001016     MOVE '3'                    TO DM-CLAIM-STATUS.
001017
001018     
      * EXEC CICS LINK
001019*        PROGRAM    ('DLO025')
001020*        COMMAREA   (DMO-COMMUNICATION-AREA)
001021*        LENGTH     (WS-DMO-LENGTH)
001022*    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00004816' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001023
001024     IF  DM-RETURN-CODE = 'OK'
001025         GO TO 2000-EXIT.
001026
001027     IF  DM-RETURN-CODE = '01'
001028         MOVE ER-8051            TO EMI-ERROR
001029         MOVE -1                 TO MLINE1L
001030         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001031         PERFORM 2250-UNLOCK-CLAIM-MSTR
001032             THRU 2250-EXIT
001033         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001034         GO TO 8200-SEND-DATAONLY.
001035
001036     IF  DM-RETURN-CODE = '02'
001037         MOVE ER-8052            TO EMI-ERROR
001038         MOVE -1                 TO MLINE1L
001039         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001040         PERFORM 2250-UNLOCK-CLAIM-MSTR
001041             THRU 2250-EXIT
001042         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001043         GO TO 8200-SEND-DATAONLY.
001044
001045     IF  DM-RETURN-CODE = '03'
001046         MOVE ER-8053            TO EMI-ERROR
001047         MOVE -1                 TO MLINE1L
001048         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001049         PERFORM 2250-UNLOCK-CLAIM-MSTR
001050             THRU 2250-EXIT
001051         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001052         GO TO 8200-SEND-DATAONLY.
001053
001054     IF  DM-RETURN-CODE = '04'
001055         MOVE ER-8054            TO EMI-ERROR
001056         MOVE -1                 TO MLINE1L
001057         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001058         PERFORM 2250-UNLOCK-CLAIM-MSTR
001059             THRU 2250-EXIT
001060         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001061         GO TO 8200-SEND-DATAONLY.
001062
001063     IF  DM-RETURN-CODE = '05'
001064         MOVE ER-8055            TO EMI-ERROR
001065         MOVE -1                 TO MLINE1L
001066         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001067         PERFORM 2250-UNLOCK-CLAIM-MSTR
001068             THRU 2250-EXIT
001069         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001070         GO TO 8200-SEND-DATAONLY.
001071
001072     IF  DM-RETURN-CODE = '06'
001073         MOVE ER-8056            TO EMI-ERROR
001074         MOVE -1                 TO MLINE1L
001075         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001076         PERFORM 2250-UNLOCK-CLAIM-MSTR
001077             THRU 2250-EXIT
001078         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001079         GO TO 8200-SEND-DATAONLY.
001080
001081     IF  DM-RETURN-CODE = '07'
001082         MOVE ER-8057            TO EMI-ERROR
001083         MOVE -1                 TO MLINE1L
001084         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001085         PERFORM 2250-UNLOCK-CLAIM-MSTR
001086             THRU 2250-EXIT
001087         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001088         GO TO 8200-SEND-DATAONLY.
001089
001090     IF  DM-RETURN-CODE = '08'
001091         MOVE ER-8058            TO EMI-ERROR
001092         MOVE -1                 TO MLINE1L
001093         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001094         PERFORM 2250-UNLOCK-CLAIM-MSTR
001095             THRU 2250-EXIT
001096         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001097         GO TO 8200-SEND-DATAONLY.
001098
001099     IF  DM-RETURN-CODE = '09'
001100         MOVE ER-8059            TO EMI-ERROR
001101         MOVE -1                 TO MLINE1L
001102         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001103         PERFORM 2250-UNLOCK-CLAIM-MSTR
001104             THRU 2250-EXIT
001105         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001106         GO TO 8200-SEND-DATAONLY.
001107
001108     IF  DM-RETURN-CODE = '10'
001109         MOVE ER-8060            TO EMI-ERROR
001110         MOVE -1                 TO MLINE1L
001111         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001112         PERFORM 2250-UNLOCK-CLAIM-MSTR
001113             THRU 2250-EXIT
001114         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001115         GO TO 8200-SEND-DATAONLY.
001116
001117     IF  DM-RETURN-CODE = '11'
001118         MOVE ER-8061            TO EMI-ERROR
001119         MOVE -1                 TO MLINE1L
001120         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001121         PERFORM 2250-UNLOCK-CLAIM-MSTR
001122             THRU 2250-EXIT
001123         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001124         GO TO 8200-SEND-DATAONLY.
001125
001126     IF  DM-RETURN-CODE = '12'
001127         MOVE ER-8062            TO EMI-ERROR
001128         MOVE -1                 TO MLINE1L
001129         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001130         PERFORM 2250-UNLOCK-CLAIM-MSTR
001131             THRU 2250-EXIT
001132         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001133         GO TO 8200-SEND-DATAONLY.
001134
001135     IF  DM-RETURN-CODE = '13'
001136         MOVE ER-8063            TO EMI-ERROR
001137         MOVE -1                 TO MLINE1L
001138         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001139         PERFORM 2250-UNLOCK-CLAIM-MSTR
001140             THRU 2250-EXIT
001141         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001142         GO TO 8200-SEND-DATAONLY.
001143
001144     IF  DM-RETURN-CODE = '14'
001145         MOVE ER-8064            TO EMI-ERROR
001146         MOVE -1                 TO MLINE1L
001147         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001148         PERFORM 2250-UNLOCK-CLAIM-MSTR
001149             THRU 2250-EXIT
001150         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001151         GO TO 8200-SEND-DATAONLY.
001152
001153     IF  DM-RETURN-CODE = '15'
001154         MOVE ER-8065            TO EMI-ERROR
001155         MOVE -1                 TO MLINE1L
001156         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001157         PERFORM 2250-UNLOCK-CLAIM-MSTR
001158             THRU 2250-EXIT
001159         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001160         GO TO 8200-SEND-DATAONLY.
001161
001162     IF DM-RETURN-CODE = '16'
001163         MOVE ER-8154            TO EMI-ERROR
001164         MOVE -1                 TO MLINE1L
001165         PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
001166         PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
001167         PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT
001168         GO TO 8200-SEND-DATAONLY.
001169
001170     IF DM-RETURN-CODE = '17'
001171         MOVE ER-8155            TO EMI-ERROR
001172         MOVE -1                 TO MLINE1L
001173         PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
001174         PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
001175         PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT
001176         GO TO 8200-SEND-DATAONLY.
001177
001178     IF DM-RETURN-CODE = 'N1'
001179         MOVE ER-8152            TO EMI-ERROR
001180         MOVE -1                 TO MLINE1L
001181         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001182         PERFORM 2250-UNLOCK-CLAIM-MSTR
001183             THRU 2250-EXIT
001184         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001185         GO TO 8200-SEND-DATAONLY.
001186
001187     IF DM-RETURN-CODE = 'E1'
001188         MOVE ER-8153            TO EMI-ERROR
001189         MOVE -1                 TO MLINE1L
001190         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001191         PERFORM 2250-UNLOCK-CLAIM-MSTR
001192             THRU 2250-EXIT
001193         PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
001194         GO TO 8200-SEND-DATAONLY.
001195
001196     MOVE ER-8066                TO EMI-ERROR.
001197     MOVE -1                     TO MLINE1L.
001198     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001199     PERFORM 2250-UNLOCK-CLAIM-MSTR
001200         THRU 2250-EXIT.
001201     PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.
001202     GO TO 8200-SEND-DATAONLY.
001203
001204 2000-NOTE-NOT-FOUND.
001205
001206     MOVE ER-0954                TO EMI-ERROR.
001207     MOVE -1                     TO MLINE1L.
001208     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001209     PERFORM 2250-UNLOCK-CLAIM-MSTR
001210         THRU 2250-EXIT.
001211     PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.
001212     GO TO 8200-SEND-DATAONLY.
001213
001214 2000-EXIT.
001215     EXIT.
001216      EJECT
001217
001218 2100-FORMAT-LAST-NAME-1ST.
001219*****************************************************************
001220*             M O V E   N A M E   R O U T I N E                 *
001221*     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
001222*     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
001223*     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
001224*                  FIELD                   VALUE                *
001225*           W-NAME-LAST    (CL15)      SMITH                    *
001226*           W-NAME-FIRST   (CL15)      JOHN                     *
001227*           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
001228*     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
001229*     CONTAIN                                                   *
001230*                SMITH, JOHN ALLEN                              *
001231*     OR                                                        *
001232*                SMITH, JOHN A.                                 *
001233*     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
001234*     COPYBOOK, ELCNWA.                                         *
001235*****************************************************************.
001236
001237     MOVE SPACES                 TO  WS-NAME-WORK-AREA.
001238     MOVE ZERO                   TO  WS-NAME-SW.
001239     SET NWA-INDEX               TO +1.
001240
001241     IF  W-NAME-LAST EQUAL SPACES
001242             AND
001243         W-NAME-MIDDLE EQUAL SPACES
001244         MOVE +1                 TO WS-NAME-SW.
001245
001246     MOVE W-NAME-LAST            TO WS-NAME-WORK2.
001247     PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
001248
001249     MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
001250     PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
001251
001252     SET NWA-INDEX UP BY +1.
001253
001254     IF  W-NAME-MIDDLE NOT EQUAL SPACES
001255
001256         IF  W-NAME-MIDDLE-2 EQUAL SPACES
001257             MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
001258             SET NWA-INDEX UP BY +1
001259             MOVE '.'            TO WS-NW (NWA-INDEX)
001260             SET NWA-INDEX UP BY +2
001261
001262         ELSE
001263             MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
001264             PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
001265
001266 2100-EXIT.
001267     EXIT.
001268                                 EJECT
001269 2150-MOVE-NAME.
001270
001271     IF  WS-NAME-SW GREATER THAN +1
001272         GO TO 2150-EXIT.
001273
001274     IF  WS-NAME-WORK2 = SPACES
001275         GO TO 2150-EXIT.
001276
001277     SET NWA-INDEX2            TO +1.
001278     SET NWA-INDEX3            TO +2.
001279
001280 2150-MOVE-NAME-CYCLE.
001281
001282     MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
001283
001284     IF  NWA-INDEX LESS THAN +30
001285         SET NWA-INDEX UP BY +1
001286
001287     ELSE
001288         ADD +2                TO  WS-NAME-SW
001289         GO TO 2150-EXIT.
001290
001291     IF  NWA-INDEX2 LESS THAN +20
001292         SET NWA-INDEX3 UP BY +1
001293         SET NWA-INDEX2 UP BY +1.
001294
001295     IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
001296             AND
001297         WS-NW2 (NWA-INDEX3) EQUAL SPACES
001298
001299         IF  WS-NAME-SW EQUAL ZERO
001300             MOVE ','            TO  WS-NW (NWA-INDEX)
001301             SET NWA-INDEX UP BY +2
001302             MOVE +1             TO  WS-NAME-SW
001303             GO TO 2150-EXIT
001304
001305         ELSE
001306             GO TO 2150-EXIT.
001307
001308     GO TO 2150-MOVE-NAME-CYCLE.
001309
001310 2150-EXIT.
001311     EXIT.
001312                                 EJECT
001313 2250-UNLOCK-CLAIM-MSTR.
001314      
      * EXEC CICS UNLOCK
001315*          DATASET  (ELMSTR-FILE-ID)
001316*          END-EXEC.
      *    MOVE '&*                    #   #00005112' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001317 2250-EXIT.
001318     EXIT.
001319 2255-UNLOCK-TRLR.
001320      
      * EXEC CICS UNLOCK
001321*          DATASET (ELTRLR-FILE-ID)
001322*          END-EXEC.
      *    MOVE '&*                    #   #00005118' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001323 2255-EXIT.
001324     EXIT.
001326*************************************************************
001327*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
001328*************************************************************
001329 2300-GETMAIN-DLYACTV.
001330     
      * EXEC CICS GETMAIN
001331*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
001332*        LENGTH (25)
001333*        INITIMG (WS-BLANK)
001334*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00005127' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035313237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001335 2300-EXIT. EXIT.
001337*************************************************************
001338*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
001339*************************************************************
001340 2350-WRITE-DLYACTV.
001341     
      * EXEC CICS HANDLE CONDITION
001342*        NOTOPEN (9986-NOTOPEN-DLYACTV)
001343*        DUPREC (2350-EXIT)
001344*    END-EXEC.
      *    MOVE '"$J%                  ! $ #00005137' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303035313337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001345
001346     
      * EXEC CICS WRITE
001347*        DATASET ('DLYACTV')
001348*        RIDFLD (DA-KEY)
001349*        FROM (DAILY-ACTIVITY-RECORD)
001350*    END-EXEC.
           MOVE LENGTH OF
            DAILY-ACTIVITY-RECORD
             TO DFHEIV11
           MOVE 'DLYACTV' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00005142' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035313432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001351
001352 2350-EXIT.
001353     EXIT.
001354
001356*************************************************************
001357*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
001358*************************************************************
001359
001360 3500-BUILD-ACTIVITY-RECORD.
001361
001362     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
001363     MOVE WS-CLAIM-KEY           TO DA-KEY.
001364     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT
001365                                 GIVING DA-TRAILER-SEQ-NO.
001366     MOVE 'D'                    TO DA-RECORD-TYPE.
001367
001368 3500-EXIT.
001369     EXIT.
001371 3550-FIND-LETTER-TRLR.
001372      MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.
001373      MOVE PI-CARRIER                TO  WS-KEY-CARRIER.
001374      MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.
001375      MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.
001376      MOVE +100                      TO  WS-KEY-SEQUENCE-NO.
001377
001378 3550-STARTBR-ELTRLR.
001379
001380      
      * EXEC CICS HANDLE CONDITION
001381*         ENDFILE   (3550-END)
001382*         NOTFND    (3550-END)
001383*     END-EXEC.
      *    MOVE '"$''I                  ! % #00005174' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303035313734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001384
001385      
      * EXEC CICS STARTBR
001386*         DATASET   (ELTRLR-FILE-ID)
001387*         RIDFLD    (WS-TRAILER-KEY)
001388*         GTEQ
001389*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005179' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035313739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001390
001391      MOVE LOW-VALUES TO WS-MAX-LETTER-ANSWER-DT.
001392
001393 3550-READNEXT-ELTRLR.
001394
001395      
      * EXEC CICS READNEXT
001396*         DATASET   (ELTRLR-FILE-ID)
001397*         RIDFLD    (WS-TRAILER-KEY)
001398*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001399*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005189' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035313839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001400
001401      IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR
001402         (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR
001403         (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR
001404         (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)
001405          GO TO 3550-END.
001406
001407      IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'
001408          GO TO 3550-READNEXT-ELTRLR.
001409
001410     IF AT-LETTER-ANSWERED-DT GREATER THAN WS-MAX-LETTER-ANSWER-DT
001411         MOVE AT-LETTER-ANSWERED-DT TO WS-MAX-LETTER-ANSWER-DT
001412     END-IF.
001413
001414     GO TO 3550-READNEXT-ELTRLR.
001415
001416 3550-END.
001417
001418     IF WS-MAX-LETTER-ANSWER-DT NOT EQUAL LOW-VALUES AND SPACES
001419         MOVE SPACES             TO  DC-OPTION-CODE
001420         MOVE WS-MAX-LETTER-ANSWER-DT TO  DC-BIN-DATE-1
001421         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001422         MOVE DC-GREG-DATE-1-MDY TO PI-PROOF-DATE
001423         MOVE PI-PROOF-DATE TO  MPRFDTO
001424         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
001425     END-IF.
001426
001427      
      * EXEC CICS ENDBR
001428*         DATASET   (ELTRLR-FILE-ID)
001429*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005221' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001430
001431 3550-EXIT.
001432     EXIT.
001434 6000-RESET-AUTO-ACTIVITY.
001435******************************************************************
001436*    RESET ALL FUTURE LETTER ACTIVITY AWAITING FURTHER ACTION    *
001437*    (RESEND, FOLLOW-UP, ETC) BY MOVING LOW-VALUES TO THE        *
001438*    FOLLOW-UP OR RESEND DATES IN THE ACTIVITY TRAILER AND THE   *
001439*    RESEND DATE IS THE LETTER ARCHIVE, IF NECESSARY.            *
001440******************************************************************
001441      MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.
001442      MOVE PI-CARRIER                TO  WS-KEY-CARRIER.
001443      MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.
001444      MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.
001445      MOVE +100                      TO  WS-KEY-SEQUENCE-NO.
001446
001447 6000-STARTBR-ELTRLR.
001448
001449      
      * EXEC CICS HANDLE CONDITION
001450*         ENDFILE   (6000-END-RESET)
001451*         NOTFND    (6000-END-RESET)
001452*     END-EXEC.
      *    MOVE '"$''I                  ! & #00005242' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303035323432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001453
001454      
      * EXEC CICS STARTBR
001455*         DATASET   (ELTRLR-FILE-ID)
001456*         RIDFLD    (WS-TRAILER-KEY)
001457*         GTEQ
001458*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005247' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035323437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001459
001460      MOVE 'Y'                       TO  WS-BROWSE-SW.
001461
001462 6000-READNEXT-ELTRLR.
001463
001464      
      * EXEC CICS READNEXT
001465*         DATASET   (ELTRLR-FILE-ID)
001466*         RIDFLD    (WS-TRAILER-KEY)
001467*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001468*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005257' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035323537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001469
001470      IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR
001471         (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR
001472         (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR
001473         (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)
001474          GO TO 6000-END-RESET.
001475
001476      IF WS-TRAILER-KEY IS EQUAL TO PI-PREV-TRLR-KEY
001477          GO TO 6000-READNEXT-ELTRLR.
001478
001479      IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'
001480          GO TO 6000-READNEXT-ELTRLR.
001481
001482      IF (AT-LETTER-ANSWERED-DT NOT EQUAL LOW-VALUES AND SPACES)
001483          GO TO 6000-READNEXT-ELTRLR.
001484
001485      IF (AT-AUTO-RE-SEND-DT EQUAL LOW-VALUES OR SPACES)
001486        AND
001487         (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES OR SPACES)
001488          GO TO 6000-READNEXT-ELTRLR.
001489
001490      IF AT-RECEIPT-FOLLOW-UP IS LESS THAN SAVE-BIN-DATE AND
001491         AT-AUTO-RE-SEND-DT IS LESS THAN SAVE-BIN-DATE   AND
001492         AT-RESEND-PRINT-DATE IS LESS THAN SAVE-BIN-DATE
001493          GO TO 6000-READNEXT-ELTRLR.
001494
001495      IF (AT-AUTO-RE-SEND-DT NOT EQUAL LOW-VALUES AND SPACES)
001496        AND
001497         (AT-RESEND-PRINT-DATE NOT EQUAL LOW-VALUES AND SPACES)
001498        AND
001499         (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES)
001500          GO TO 6000-READNEXT-ELTRLR.
001501
001502 6000-END-BROWSE.
001503
001504      MOVE WS-TRAILER-KEY            TO  PI-PREV-TRLR-KEY.
001505
001506      
      * EXEC CICS ENDBR
001507*         DATASET   (ELTRLR-FILE-ID)
001508*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005299' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035323939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001509
001510      MOVE 'N'                       TO  WS-BROWSE-SW.
001511
001512 6000-READ-ELTRLR-UPDATE.
001513
001514      
      * EXEC CICS READ
001515*         DATASET   (ELTRLR-FILE-ID)
001516*         RIDFLD    (WS-TRAILER-KEY)
001517*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001518*         UPDATE
001519*     END-EXEC.
      *    MOVE '&"S        EU         (   #00005307' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035333037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001520
001521      MOVE 'Y'                       TO  WS-UPDATE-SW.
001522
001523      IF AT-AUTO-RE-SEND-DT IS NOT LESS THAN SAVE-BIN-DATE
001524          MOVE LOW-VALUES            TO  AT-AUTO-RE-SEND-DT
001525          MOVE PI-COMPANY-CD         TO  ELARCH-COMPANY-CD
001526          MOVE AT-LETTER-ARCHIVE-NO  TO  ELARCH-ARCHIVE-NO
001527          MOVE '1'                   TO  ELARCH-RECORD-TYPE
001528          MOVE +0                    TO  ELARCH-SEQ-NO
001529          PERFORM 6200-READ-ELARCH-UPDATE THRU 6200-EXIT.
001530
001531      IF AT-RECEIPT-FOLLOW-UP IS NOT LESS THAN SAVE-BIN-DATE
001532          MOVE LOW-VALUES            TO  AT-RECEIPT-FOLLOW-UP.
001533
001534 6000-REWRITE-ELTRLR.
001535
001536      
      * EXEC CICS REWRITE
001537*         DATASET   (ELTRLR-FILE-ID)
001538*         FROM      (ACTIVITY-TRAILERS)
001539*     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005329' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035333239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001540
001541      GO TO 6000-STARTBR-ELTRLR.
001542
001543 6000-END-RESET.
001544
001545      IF WS-BROWSE-SW IS EQUAL TO 'Y'
001546          MOVE 'N'                   TO  WS-BROWSE-SW
001547          
      * EXEC CICS ENDBR
001548*             DATASET   (ELTRLR-FILE-ID)
001549*         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005340' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035333430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001550
001551 6000-EXIT.
001552     EXIT.
001553
001554      EJECT
001555 6100-CHECK-AUTO-ACTIVITY.
001556******************************************************************
001557*    READ THE AUTOMATIC ACTIVITY RECORD TO DETERMINE IF THE      *
001558*    AUTOMATIC ACTIVITY CODE IN THE CLAIM MASTER IS TO BE RESET. *
001559******************************************************************
001560
001561      
      * EXEC CICS HANDLE CONDITION
001562*         NOTFND   (6100-NOT-FOUND)
001563*     END-EXEC.
      *    MOVE '"$I                   ! '' #00005354' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303035333534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001564
001565      MOVE PI-COMPANY-ID             TO  ELCNTL-COMPANY-ID.
001566      MOVE 'T'                       TO  ELCNTL-RECORD-TYPE.
001567      MOVE SPACES                    TO  ELCNTL-ACCESS.
001568      MOVE +0                        TO  ELCNTL-SEQ-NO.
001569
001570      
      * EXEC CICS READ
001571*         DATASET   (ELCNTL-FILE-ID)
001572*         RIDFLD    (ELCNTL-KEY)
001573*         SET       (ADDRESS OF CONTROL-FILE)
001574*     END-EXEC.
      *    MOVE '&"S        E          (   #00005363' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035333633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001575
001576      IF CL-ACTIVITY-CODE IS NOT EQUAL TO ZEROS
001577          MOVE CL-ACTIVITY-CODE      TO  MISC-SUB
001578          IF MISC-SUB IS GREATER THAN +9
001579              SUBTRACT +9 FROM MISC-SUB
001580              MOVE CF-USER-RESET-SW (MISC-SUB) TO  WS-RESET-SW
001581          ELSE
001582              MOVE CF-SYS-RESET-SW  (MISC-SUB) TO  WS-RESET-SW.
001583
001584      MOVE 'Y'                       TO  WS-REC-FOUND-SW.
001585      GO TO 6100-EXIT.
001586
001587 6100-NOT-FOUND.
001588
001589     MOVE 'N'                        TO  WS-RESET-SW
001590                                         WS-REC-FOUND-SW.
001591
001592 6100-EXIT.
001593     EXIT.
001594
001595      EJECT
001596 6200-READ-ELARCH-UPDATE.
001597******************************************************************
001598*  READ AND UPDATE THE RESEND DATE IN THE LETTER ARCHIVE RECORD  *
001599******************************************************************
001600
001601      
      * EXEC CICS HANDLE CONDITION
001602*         NOTFND   (6200-EXIT)
001603*     END-EXEC.
      *    MOVE '"$I                   ! ( #00005394' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303035333934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001604
001605      
      * EXEC CICS READ
001606*         DATASET   (ELARCH-FILE-ID)
001607*         RIDFLD    (ELARCH-KEY)
001608*         SET       (ADDRESS OF LETTER-ARCHIVE)
001609*         UPDATE
001610*     END-EXEC.
      *    MOVE '&"S        EU         (   #00005398' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035333938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001611
001612      MOVE LOW-VALUES                TO  LA-RESEND-DATE.
001613
001614 6200-REWRITE-ELARCH.
001615
001616      
      * EXEC CICS REWRITE
001617*         DATASET   (ELARCH-FILE-ID)
001618*         FROM      (LETTER-ARCHIVE)
001619*     END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005409' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001620
001621 6200-EXIT.
001622     EXIT.
001623
001624     EJECT
001625 7000-READ-ELMSTR-UPDATE.
001626
001627     
      * EXEC CICS  HANDLE CONDITION
001628*           NOTFND  (7000-NOTFND)
001629*           NOTOPEN (9981-NOTOPEN-MSTR)
001630*    END-EXEC.
      *    MOVE '"$IJ                  ! ) #00005420' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303035343230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001631
001632     
      * EXEC CICS  READ
001633*        DATASET  (ELMSTR-FILE-ID)
001634*        RIDFLD   (WS-CLAIM-KEY)
001635*        SET      (ADDRESS OF CLAIM-MASTER)
001636*        UPDATE
001637*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005425' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035343235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001638
001639     GO TO 7000-EXIT.
001640
001641 7000-NOTFND.
001642
001643     MOVE ER-0133                TO  EMI-ERROR.
001644     MOVE -1                     TO  MLINE1L.
001645     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001646     GO TO 8200-SEND-DATAONLY.
001647
001648 7000-EXIT.
001649      EXIT.
001650
001651 7010-REWRITE-ELMSTR.
001652
001653     
      * EXEC CICS HANDLE CONDITION
001654*        DUPKEY (7010-EXIT)
001655*        END-EXEC.
      *    MOVE '"$$                   ! * #00005446' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303035343436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001656
001657     
      * EXEC CICS  REWRITE
001658*        DATASET  (ELMSTR-FILE-ID)
001659*        FROM     (CLAIM-MASTER)
001660*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005450' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035343530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001661
001662 7010-EXIT.
001663      EXIT.
001664
001665     EJECT
001666 7100-GETMAIN-ELTRLR.
001667
001668     
      * EXEC CICS GETMAIN
001669*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
001670*        INITIMG (GETMAIN-SPACE)
001671*        LENGTH  (WS-TRLR-LENGTH)
001672*    END-EXEC.
      *    MOVE ',"IL                  $   #00005461' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035343631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001673
001674 7100-EXIT.
001675     EXIT.
001676
001677 7110-WRITE-ELTRLR.
001678     
      * EXEC CICS  HANDLE CONDITION
001679*           DUPREC   (7110-DUPREC)
001680*    END-EXEC.
      *    MOVE '"$%                   ! + #00005471' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303035343731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001681
001682     
      * EXEC CICS  WRITE
001683*           DATASET  (ELTRLR-FILE-ID)
001684*           RIDFLD   (WS-TRAILER-KEY)
001685*           FROM     (ACTIVITY-TRAILERS)
001686*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005475' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035343735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001687
001688     GO TO 7110-EXIT.
001689
001690 7110-DUPREC.
001691
001692     MOVE ER-0132                TO  EMI-ERROR.
001693     MOVE -1                     TO  MLINE1L.
001694     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001695     GO TO 8200-SEND-DATAONLY.
001696
001697 7110-EXIT.
001698      EXIT.
001699
001700 7120-READ-ELTRLR-UPDATE.
001701
001702     
      * EXEC CICS  HANDLE CONDITION
001703*           NOTFND  (9965-NO-TRLR-ERROR)
001704*           NOTOPEN (9982-NOTOPEN-TRLR)
001705*    END-EXEC.
      *    MOVE '"$IJ                  ! , #00005495' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303035343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001706
001707     
      * EXEC CICS  READ
001708*        DATASET  (ELTRLR-FILE-ID)
001709*        RIDFLD   (WS-TRAILER-KEY)
001710*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
001711*        UPDATE
001712*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005500' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035353030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001713
001714 7120-EXIT.
001715      EXIT.
001716
001717 7130-REWRITE-ELTRLR.
001718
001719     
      * EXEC CICS  REWRITE
001720*        DATASET  (ELTRLR-FILE-ID)
001721*        FROM     (ACTIVITY-TRAILERS)
001722*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005512' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035353132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001723
001724 7130-EXIT.
001725      EXIT.
001726
001727     EJECT
001728 8100-SEND-INITIAL-MAP.
001729
001730     MOVE SAVE-DATE                  TO  MRNDATEO.
001731     MOVE EIBTIME                    TO  TIME-IN.
001732     MOVE TIME-OUT                   TO  MRNTIMEO.
001733
001734     
      * EXEC CICS  SEND
001735*         MAP      (MAP-NAME)
001736*         MAPSET   (MAPSET-NAME)
001737*         FROM     (EL151AO)
001738*         ERASE
001739*         CURSOR
001740*    END-EXEC.
           MOVE LENGTH OF
            EL151AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005527' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303035353237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AO, 
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
           
001741
001742     GO TO 9100-RETURN-TRAN.
001743
001744 8200-SEND-DATAONLY.
001745
001746     MOVE SAVE-DATE                  TO  MRNDATEO.
001747     MOVE EIBTIME                    TO  TIME-IN.
001748     MOVE TIME-OUT                   TO  MRNTIMEO.
001749
001750     
      * EXEC CICS  SEND
001751*        MAP       (MAP-NAME)
001752*        MAPSET    (MAPSET-NAME)
001753*        FROM      (EL151AO)
001754*        DATAONLY
001755*        CURSOR
001756*    END-EXEC.
           MOVE LENGTH OF
            EL151AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005543' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303035353433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AO, 
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
           
001757
001758     GO TO 9100-RETURN-TRAN.
001759
001760 8300-SEND-TEXT.
001761     
      * EXEC CICS SEND TEXT
001762*        FROM     (LOGOFF-TEXT)
001763*        LENGTH   (LOGOFF-LENGTH)
001764*        ERASE
001765*        FREEKB
001766*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005554' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303035353534' TO DFHEIV0
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
           
001767
001768     
      * EXEC CICS RETURN
001769*        END-EXEC.
      *    MOVE '.(                    ''   #00005561' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035353631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001770
001771     EJECT
001772 9600-PGMID-ERROR.
001773     
      * EXEC CICS HANDLE CONDITION
001774*        PGMIDERR (8300-SEND-TEXT)
001775*    END-EXEC.
      *    MOVE '"$L                   ! - #00005566' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303035353636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001776
001777     MOVE THIS-PGM               TO LOGOFF-PGM
001778                                    PI-CALLING-PROGRAM.
001779     MOVE SPACES                 TO PI-ENTRY-CD-1.
001780     MOVE 'EL005'                TO THIS-PGM.
001781     MOVE THIS-PGM               TO LOGOFF-PGM.
001782     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
001783     GO TO 9300-XCTL.
001784
001785 8800-UNAUTHORIZED-ACCESS.
001786
001787     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001788     GO TO 8300-SEND-TEXT.
001789
001790 9100-RETURN-TRAN.
001791     MOVE EMI-ERROR-NUMBER (1)    TO  PI-LAST-ERROR-NO.
001792     MOVE '151A'                  TO  PI-CURRENT-SCREEN-NO.
001793
001794     
      * EXEC CICS  RETURN
001795*        TRANSID  (WS-TRANS-ID)
001796*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001797*        LENGTH   (PI-COMM-LENGTH)
001798*    END-EXEC.
      *    MOVE '.(CT                  ''   #00005587' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035353837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001799
001800 9300-XCTL.
001801     
      * EXEC CICS XCTL
001802*        PROGRAM  (THIS-PGM)
001803*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001804*        LENGTH   (PI-COMM-LENGTH)
001805*    END-EXEC.
      *    MOVE '.$C                   %   #00005594' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035353934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001806
001807 9400-CLEAR.
001808     MOVE PI-RETURN-TO-PROGRAM       TO  THIS-PGM.
001809     GO TO 9300-XCTL.
001810
001811 9650-DEEDIT.
001812     
      * EXEC CICS BIF DEEDIT
001813*        FIELD (DEEDIT-FIELD)
001814*        LENGTH(15)
001815*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005605' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035363035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001816
001817 9650-EXIT.
001818      EXIT.
001819
001820 9700-LINK-DATE-CONVERT.
001821     
      * EXEC CICS LINK
001822*           PROGRAM  ('ELDATCV')
001823*           COMMAREA (DATE-CONVERSION-DATA)
001824*           LENGTH   (DC-COMM-LENGTH)
001825*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005614' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001826
001827 9700-EXIT.
001828      EXIT.
001829
001830 9900-ERROR-FORMAT.
001831
001832     
      * EXEC CICS LINK
001833*        PROGRAM  ('EL001')
001834*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
001835*        LENGTH   (EMI-COMM-LENGTH)
001836*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00005625' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001837
001838     MOVE EMI-LINE1              TO  MERMSG1O.
001839
001840 9900-EXIT.
001841      EXIT.
001842
001843 9965-NO-TRLR-ERROR.
001844     MOVE ER-0270                TO  EMI-ERROR.
001845     MOVE -1                     TO  MLINE1L.
001846     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001847     GO TO 8200-SEND-DATAONLY.
001848
001849 9981-NOTOPEN-MSTR.
001850     MOVE ER-0154                TO EMI-ERROR
001851     MOVE -1                     TO  MLINE1L.
001852     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001853     GO TO 8200-SEND-DATAONLY.
001854
001855 9982-NOTOPEN-TRLR.
001856     MOVE ER-0172                TO EMI-ERROR
001857     MOVE -1                     TO  MLINE1L.
001858     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001859     GO TO 8200-SEND-DATAONLY.
001860
001861 9986-NOTOPEN-DLYACTV.
001862     MOVE '2955'                 TO EMI-ERROR.
001863     MOVE -1                     TO  MLINE1L.
001864     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001865     GO TO 8200-SEND-DATAONLY.
001866
001867 9995-SECURITY-VIOLATION.
001868*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005679' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363739' TO DFHEIV0
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
001869
001870 9995-EXIT.
001871     EXIT.
001872
001873 9990-ABEND.
001874     MOVE -1                      TO  MPFNUMBL.
001875     MOVE DFHEIBLK                TO  EMI-LINE1.
001876
001877     
      * EXEC CICS LINK
001878*          PROGRAM  ('EL004')
001879*          COMMAREA (EMI-LINE1)
001880*          LENGTH   (72)
001881*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005696' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001882
001883     MOVE EMI-LINE1               TO  MERMSG1O.
001884     GO TO 8200-SEND-DATAONLY.
001885

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL151' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9986-NOTOPEN-DLYACTV,
                     2350-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3550-END,
                     3550-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6000-END-RESET,
                     6000-END-RESET
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 6200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7000-NOTFND,
                     9981-NOTOPEN-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7010-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7110-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 9965-NO-TRLR-ERROR,
                     9982-NOTOPEN-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL151' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
