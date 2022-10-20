      *((program: EL1523.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1523.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 07/18/94 11:29:57.
000007*                            VMOD=2.006.
000008*
000009*
000010*AUTHOR.     LOGIC,INC.
000011*            DALLAS, TEXAS.
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
000025*REMARKS.    TRANSACTION - EXL1 - LETTER GENERATOR.
000026*            THIS PROGRAM GENERATES LETTERS WHEN PROPERLY CALLED
000027*            FROM ANOTHER ONLINE PROGRAM.  THIS PROGRAM DOES NOT
000028*            CAUSE LETTERS TO BE PRINTED.  INSTEAD IT AUTOMATICALL
000029*            ENTERS THEM INTO THE ARCHIVE FILE.
000030*
000031*
000032*
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000042* 031307                   PEMA  ADD 3.1 VARIABLE
000043* 090108    2007041300006  AJRA  ADD VARIABLE @@AUTOPYDT FOR AUTO
000044* 033110  CR2009122800001  AJRA  NAPERSOFT
000045* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
000046* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000047* 080322  CR2021100800003  TANA  Add B and H claim types
000048******************************************************************
000049
000050 ENVIRONMENT DIVISION.
000051
000052                                 EJECT
000053 DATA DIVISION.
000054 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000055*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
000056*77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
000057*                                  USAGE POINTER.
000058 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
000059 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
000060                                   USAGE POINTER.
000061 77  FILLER  PIC X(32) VALUE '********************************'.
000062 77  FILLER  PIC X(32) VALUE '*    EL1523 WORKING STORAGE    *'.
000063 77  FILLER  PIC X(32) VALUE '********** VMOD=2.006 **********'.
000064 77  B1                          PIC S999 COMP-3 VALUE +0.
000065
000066 01  W-PROGRAM-WORK-AREAS.
000067     12  FILLER                  PIC  X(17)
000068                                 VALUE 'PROGRAM WORK AREA'.
000069     12  W-ASKTIME-CTR           PIC S9(08) COMP.
000070     12  W-CORR-TRLR-SEQ         PIC S9(04) COMP.
000071     12  W-PI-ADDR-SEQ           PIC S9(04) COMP.
000072     12  W-POSITION2             PIC S9(04) COMP.
000073     12  W-POSITION21            PIC S9(04) COMP.
000074     12  W-SEQ-COUNTER           PIC S9(04) COMP.
000075
000076     12  W-CURRENT-LINE          PIC S9(03) COMP-3 VALUE +0.
000077     12  W-ROLL-COUNTER          PIC S9(03) VALUE +0     COMP-3.
000078     12  W-TOTAL-LINES           PIC S9(03) COMP-3 VALUE +0.
000079
000080     12  W-ARCH-SUPPRESS         PIC ZZZZZZZZ99.
000081     12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
000082                                 PIC  X(10).
000083
000084     12  W-CURRENT-ERROR         PIC  9(04).
000085
000086     12  W-DATE-WORK             PIC  9(07).
000087     12  W-DT-REDEF REDEFINES W-DATE-WORK.
000088         16  FILLER              PIC  X(02).
000089         16  W-DT-WORK           PIC  9(05).
000090
000091     12  W-NDX-WORK              PIC  9(02).
000092     12  W-SUB                   PIC  9(02).
000093     12  W-WORK-AMOUNT           PIC S9(09)V99 VALUE +0.
000094     12  W-ZIP-NUMERIC           PIC  9(09).
000095     12  W-ZIP-NONNUM   REDEFINES  W-ZIP-NUMERIC
000096                                 PIC  X(09).
000097
000098     12  W-ADDR-TYPE-CD.
000099         16  W-ADDR-TYPE         PIC  X(01).
000100         16  W-ADDR-SEQ          PIC  X(01).
000101         16  W-ADDR-SEQ-NUM REDEFINES
000102             W-ADDR-SEQ          PIC  9(01).
000103
000104     12  W-BEN-HOLD              PIC  X(02).
000105     12  W-BENEFIT-WORK          PIC  X(03).
000106     12  W-BEN-R REDEFINES W-BENEFIT-WORK.
000107         16  W-ELIM-DAYS         PIC  X(02).
000108         16  FILLER              PIC  X(01).
000109
000110     12  W-Z-CONTROL-DATA.
000111         16  W-NUMBER-OF-COPIES  PIC  9(01).
000112         16  FILLER              PIC  X(01).
000113         16  W-DAYS-TO-FOLLOW-UP PIC  9(03).
000114         16  FILLER              PIC  X(01).
000115         16  W-DAYS-TO-RESEND-1  PIC  9(03).
000116         16  FILLER              PIC  X(01).
000117         16  W-FORM-TO-RESEND    PIC  X(4).
000118         16  FILLER              PIC  X(1).
000119         16  W-PROMPT-LETTER     PIC  X(1).
000120         16  FILLER              PIC  X(1).
000121         16  W-ENCLOSURE-CD      PIC  X(3).
000122         16  FILLER              PIC  X(1).
000123         16  W-AUTO-CLOSE-IND    PIC  X(1).
000124         16  FILLER              PIC  X(1).
000125         16  W-LETTER-TO-BENE    PIC  X(1).
000126
000127     12  W-GROUPING.
000128         16  W-GROUP-3           PIC  X(03).
000129         16  FILLER              PIC  X(03).
000130
000131     12  W-NAME.
000132         16  W-FIRST-NAME        PIC  X(12).
000133         16  W-MIDDLE-NAME       PIC  X(12).
000134         16  W-LAST-NAME         PIC  X(15).
000135
000136     12  W-CREDIT-CARD-LOAN-NO.
000137         16  W-LOAN-NUMBER       PIC  X(08).
000138         16  W-CURRENT-LOAN-NO   PIC  X(12).
000139
000140     12  W-DEEDIT-FIELD          PIC  X(15).
000141     12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD
000142                                 PIC S9(15).
000143
000144     12  W-CURRENT-SAVE          PIC  X(02).
000145     12  W-EDIT-DATE-1.
000146         16  W-ED1-MM            PIC X(02).
000147         16  FILLER              PIC X(01)    VALUE '/'.
000148         16  W-ED1-DD            PIC X(02).
000149         16  FILLER              PIC X(01)    VALUE '/'.
000150         16  W-ED1-YY            PIC X(02).
000151     12  W-EDIT-DATE-2.
000152         16  W-ED2-DD            PIC X(02).
000153         16  FILLER              PIC X(01)    VALUE '/'.
000154         16  W-ED2-MM            PIC X(02).
000155         16  FILLER              PIC X(01)    VALUE '/'.
000156         16  W-ED2-YY            PIC X(02).
000157
000158     12  W-INSURED-LAST-NAME     PIC  X(15).
000159     12  W-INSURED-MID-INIT      PIC  X(01).
000160     12  W-INSURED-1ST-NAME      PIC  X(12).
000161
000162     12  W-NAME-WORK.
000163         16  W-NW                PIC  X(01)
000164             OCCURS 30 TIMES INDEXED BY W-NWA-NDX.
000165
000166     12  W-NAME-WORK2.
000167         16  W-NW2               PIC  X(01)
000168             OCCURS 20 TIMES INDEXED BY W-NWA-NDX2 W-NWA-NDX3.
000169
000170
000171     12  W-PGM-NAME              PIC  X(08).
000172     12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
000173     12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
000174         16  FILLER              PIC  9(01).
000175         16  W-PI-AREA           PIC  9(03).
000176         16  W-PI-PFX            PIC  9(03).
000177         16  W-PI-SFX            PIC  9(04).
000178     12  W-PHONE-OUT.
000179         16  W-PO-AREA           PIC  X(03).
000180         16  FILLER              PIC  X(01) VALUE '-'.
000181         16  W-PO-PFX            PIC  X(03).
000182         16  FILLER              PIC  X(01) VALUE '-'.
000183         16  W-PO-SFX            PIC  X(04).
000184
000185     12  W-SAVE-PROD-RECORD      PIC  X(2000) VALUE SPACES.
000186     12  W-SAVE-ACCT-RECORD      PIC  X(2000) VALUE SPACES.
000187     12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
000188     12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
000189
000190     12  W-SINGLE-LINE           PIC  X(70).
000191     12  W-SINGLE-LINE-BY-1 REDEFINES W-SINGLE-LINE.
000192         16  W-ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2
000193                                 PIC  X(01).
000194     12  W-STATE-LINE            PIC  X(01) VALUE 'N'.
000195     12  W-TEMP-AREA1.
000196         16  W-TEMP-1 OCCURS 29 TIMES INDEXED BY W-TA1
000197                                 PIC  X(01).
000198     12  W-TEMP-AREA2.
000199         16  W-TEMP-2 OCCURS 30 TIMES INDEXED BY W-TA2 W-TA21
000200                                                 W-MOVE-NDX
000201                                 PIC  X(01).
000202
000203     12  W-TEMP-CURR-LINE        PIC S9(03)   COMP-3.
000204     12  W-TIME-IN               PIC S9(07).
000205     12  W-TIME-OUT-R REDEFINES W-TIME-IN.
000206         16  FILLER              PIC  X(01).
000207         16  W-TIME-OUT          PIC  9(02)V9(02).
000208         16  FILLER              PIC  X(02).
000209     12  W-VAR-HOLD.
000210         16  W-V1                PIC  X(01).
000211         16  W-V2                PIC  X(01).
000212         16  W-V3                PIC  X(01).
000213         16  W-V4                PIC  X(01).
000214     12  W-V-HOLD REDEFINES W-VAR-HOLD.
000215         16  W-V-NUM             PIC  9(02).
000216         16  W-V-PERIOD          PIC  X(01).
000217         16  W-V-DECIMAL         PIC  9(01).
000218
000219     12  W-WORD-LENGTH           PIC S9(04)  COMP-3.
000220
000221     12  W-ZIP-CODE.
000222         16  W-AM-ZIP-CODE       PIC  X(05).
000223         16  W-AM-ZIP-DASH       PIC  X(01).
000224         16  W-AM-ZIP-PLUS4      PIC  X(04).
000225     12  W-ZIP-CODE-CANADIAN   REDEFINES  W-ZIP-CODE.
000226         16  W-CAN-POSTAL-1      PIC  X(03).
000227         16  FILLER              PIC  X(01).
000228         16  W-CAN-POSTAL-2      PIC  X(03).
000229         16  FILLER              PIC  X(03).
000230
000231 01  W-PROGRAM-SWITCHES.
000232     12  FILLER                  PIC  X(16)
000233                                      VALUE 'PROGRAM SWITCHES'.
000234
000235     12  W-ACCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
000236     12  W-ACCT-READ-SW          PIC  X(01) VALUE ' '.
000237     12  W-PROD-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
000238     12  W-PROD-READ-SW          PIC  X(01) VALUE ' '.
000239     12  W-ACTV-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
000240     12  W-COMP-READ-SW          PIC  X(01) VALUE ' '.
000241     12  W-DATA-FOUND-SW         PIC  X(01).
000242         88  NO-CHARACTERS-FOUND            VALUE 'N'.
000243     12  W-GETMAIN-SW            PIC  9(01) VALUE 0.
000244         88  W-NO-GETMAIN-DONE-YET          VALUE 0.
000245     12  W-LABELS-SW             PIC  X(01) VALUE SPACE.
000246     12  W-NAME-SW               PIC S9(01) COMP-3 VALUE ZERO.
000247     12  W-REVERSE-DATE-SW       PIC X(01)  VALUE SPACES.
000248         88  W-REVERSE-DATE                 VALUE 'Y'.
000249     12  W-TEXT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
000250
000251                                 EJECT
000252 01  W-ACCESS-KEYS.
000253     12  FILLER                  PIC  X(11)
000254                                 VALUE 'ACCESS KEYS'.
000255
000256     12  W-ACCT-KEY.
000257         16  W-ACCT-PARTIAL-KEY.
000258             20  W-ACCT-CO       PIC  X(01).
000259             20  W-ACCT-CARRIER  PIC  X(01).
000260             20  W-ACCT-GROUPING PIC  X(6).
000261             20  W-ACCT-STATE    PIC  X(02).
000262             20  W-ACCT-ACCOUNT  PIC  X(10).
000263         16  W-ACCT-EXP-DATE     PIC  X(02).
000264
000265     12  W-PROD-KEY.
000266         16  W-PROD-PARTIAL-KEY.
000267             20  W-PROD-CO       PIC  X(01).
000268             20  W-PROD-CARRIER  PIC  X(01).
000269             20  W-PROD-GROUPING PIC  X(6).
000270             20  W-PROD-STATE    PIC  X(02).
000271             20  W-PROD-PRODUCER PIC  X(10).
000272         16  W-PROD-EXP-DATE     PIC  X(02).
000273
000274     12  W-ACTV-KEY.
000275         16  W-ACTV-PARTIAL-KEY.
000276             20  W-ACTV-CO       PIC  X(01).
000277             20  W-ACTV-CARRIER  PIC  X(01).
000278             20  W-ACTV-CLAIM    PIC  X(07).
000279             20  W-ACTV-CERT-NUM PIC  X(11).
000280         16  W-ACTV-SEQ          PIC S9(04)   VALUE +0    COMP.
000281
000282     12  W-ARCH-KEY.
000283         16  W-ARCH-PARTIAL-KEY.
000284             20  W-ARCH-CO       PIC  X(01).
000285             20  W-ARCH-NUMBER   PIC S9(08)      COMP.
000286         16  W-ARCH-REC-TYPE     PIC  X(01).
000287         16  W-ARCH-SEQ          PIC S9(04)      COMP VALUE +0.
000288
000289     12  W-BENE-KEY.
000290         16  W-BENE-COMP-CD      PIC  X(01).
000291         16  W-BENE-REC-TYPE     PIC  X(01).
000292         16  W-BENE-NUMBER.
000293             20  W-BENE-CREDITOR PIC  X(03).
000294             20  W-BENE-FILLER   PIC  X(07).
000295
000296     12  W-CERT-KEY.
000297         16  W-CERT-CO           PIC  X(01).
000298         16  W-CERT-CARRIER      PIC  X(01).
000299         16  W-CERT-GROUPING     PIC  X(6).
000300         16  W-CERT-STATE        PIC  X(02).
000301         16  W-CERT-ACCOUNT      PIC  X(10).
000302         16  W-CERT-EFF-DT       PIC  X(02).
000303         16  W-CERT-CERT-NUM     PIC  X(11).
000304
000305     12  W-PLCY-KEY.
000306         16  W-PLCY-CO           PIC  X(01).
000307         16  W-PLCY-CARRIER      PIC  X(01).
000308         16  W-PLCY-GROUPING     PIC  X(6).
000309         16  W-PLCY-STATE        PIC  X(02).
000310         16  W-PLCY-PRODUCER     PIC  X(10).
000311         16  W-PLCY-EFF-DT       PIC  X(02).
000312         16  W-PLCY-REFERENCE-NO PIC  X(20).
000313
000314     12  W-CNTL-KEY.
000315         16  W-CNTL-CO           PIC  X(03).
000316         16  W-CNTL-RECORD-TYPE  PIC  X(01) VALUE '1'.
000317         16  W-CNTL-GENL.
000318             20  W-CNTL-GEN1     PIC  X(02) VALUE SPACES.
000319             20  W-CNTL-GEN2.
000320                24  W-CNTL-GEN3  PIC  X(01) VALUE SPACES.
000321                24  W-CNTL-GEN4  PIC  X(01) VALUE SPACES.
000322         16  W-CNTL-SEQ          PIC S9(04) VALUE +0     COMP.
000323
000324     12  W-CLM-KEY.
000325         16  W-CLM-CO            PIC  X(01).
000326         16  W-CLM-CARRIER       PIC  X(01).
000327         16  W-CLM-CLAIM         PIC  X(07).
000328         16  W-CLM-CERT-NUM      PIC  X(11).
000329
000330      12  W-COMP-KEY.
000331          16  W-COMP-COMPANY-CD  PIC  X(01).
000332          16  W-COMP-CARRIER     PIC  X(01).
000333          16  W-COMP-GROUPING    PIC  X(06).
000334          16  W-COMP-RESP-NO     PIC  X(10).
000335          16  W-COMP-ACCOUNT     PIC  X(10).
000336          16  W-COMP-TYPE        PIC  X(01).
000337
000338     12  W-TEXT-KEY.
000339         16  W-TEXT-PARTIAL-KEY.
000340             20  W-TEXT-CO       PIC  X(01).
000341             20  W-TEXT-LETTER   PIC  X(04).
000342         16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
000343         16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
000344
000345     12  W-PLAN-KEY.
000346         16  W-PLAN-CO           PIC  X(01).
000347         16  W-PLAN-CARRIER      PIC  X(01).
000348         16  W-PLAN-GROUPING     PIC  X(06).
000349         16  W-PLAN-STATE        PIC  X(02).
000350         16  W-PLAN-PRODUCER     PIC  X(10).
000351         16  W-PLAN-CODE         PIC  X(02).
000352         16  W-PLAN-REV-NO       PIC  9(03).
000353
000354     12  W-ARCH-SAVE-KEY         PIC  X(05).
000355     12  W-ACTV-SAVE-KEY         PIC  X(20).
000356     12  W-ACCT-SAVE-KEY         PIC  X(20).
000357     12  W-PROD-SAVE-KEY         PIC  X(20).
000358     12  W-TEXT-SAVE-KEY         PIC  X(05).
000359                                 EJECT
000360 01  W-PROGRAM-INTERFACE.
000361     12  FILLER                  PIC  X(14)
000362                                 VALUE 'INTERFACE AREA'.
000363*                                COPY ELCINTF.
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
000364     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
000365         16  PI-EL1523-WA.
000366             20  PI-FORM-NUMBER  PIC  X(04).
000367             20  PI-NUMBER-COPIES
000368                                 PIC  9(01).
000369             20  PI-ADDR-TYPE    PIC  X(02).
000370             20  PI-FOLLOW-UP-DATE
000371                                 PIC  X(02).
000372             20  PI-RESEND-DATE  PIC  X(02).
000373             20  PI-ERROR-CODE   PIC  9(04).
000374                 88  PI-NO-ERRORS-DETECTED VALUE 0000.
000375                 88  PI-FATAL-ERROR VALUES  0006 0013 0042
000376                                            0154 0168 0169
000377                                            0172 0179 0186
000378                                            0281 0332 2055
000379                                            3697 3698 3699
000380                                            3770 3771 3772
000381                                            7675 9106 9808
000382                                            9883 9887.
000383             20  PI-REASON       PIC  X(70).
000384             20  PI-ARCHIVE-NUMBER
000385                                 PIC  9(08).
000386             20  PI-ACCT-POINTER PIC S9(08) COMP.
000387             20  PI-ACTV-POINTER PIC S9(08) COMP.
000388             20  PI-ARCH-POINTER PIC S9(08) COMP.
000389             20  PI-VAR-POINTER  PIC S9(08) COMP.
000390             20  PI-PROD-POINTER PIC S9(08) COMP.
000391*00364          16  FILLER              PIC X(527).
000392         16  PI-AUTO-LETTER-DATE  PIC X(10).
000393         16  PI-RESEND-FORM-NUMBER PIC X(4).
000394         16  PI-PROMPT-LETTER     PIC X.
000395         16  PI-ENCLOSURE-CD      PIC X(3).
000396         16  PI-AUTO-CLOSE-IND    PIC X(1).
000397         16  PI-LETTER-TO-BENE    PIC X(1).
000398         16  FILLER               PIC X(507).
000399                                 EJECT
000400 01  W-PROGRAM-TABLE             PIC  X(14)
000401                                     VALUE 'PROGRAM TABLES'.
000402 01  W-LABEL-HOLD-AREA.
000403     12  W-LABEL-LINES OCCURS 6 TIMES
000404                        INDEXED BY W-NDX   W-NDX2.
000405         16  W-LABEL-ZIP.
000406             20  W-LABEL-1ST-ZIP
000407                                 PIC  X(05).
000408             20  FILLER          PIC  X(01).
000409             20  W-LABEL-2ND-ZIP
000410                                 PIC  X(04).
000411         16  FILLER              PIC  X(09).
000412         16  W-LAST-DIGIT        PIC  X(01).
000413         16  W-LAST-ZIP.
000414             20  W-LAST-1ST-ZIP  PIC  X(05).
000415             20  FILLER          PIC  X(01).
000416             20  W-LAST-2ND-ZIP PIC   X(04).
000417
000418
000419 01  W-RECORD-TABLE              PIC  X(21900) VALUE SPACES.
000420 01  W-REC-TABLE REDEFINES W-RECORD-TABLE.
000421     12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-NDX
000422                                 PIC  X(3650).
000423
000424 01  W-REC-ENTRIES REDEFINES W-RECORD-TABLE.
000425     12  W-REC-ENT OCCURS 300 TIMES INDEXED BY W-TB-NDX W-TB-NDX1.
000426         16  W-REC-TEXT          PIC  X(70).
000427         16  W-REC-PC            PIC  9(02).
000428         16  FILLER              PIC  X(01).
000429
000430                                 EJECT
000431 01  W-PROGRAM-VARIABLE-AREA.
000432     12  FILLER                  PIC  X(13)
000433                                      VALUE 'VARIABLE AREA'.
000434****************************************************
000435*       WHEN ADDING OR DELETING ENTRIES TO         *
000436*       THE SYSTEM-SUPPORTED-VARIABLES THE         *
000437*       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
000438*       TO MATCH THE NUMBER OF ENTRIES IN THE      *
000439*       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
000440*       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
000441*       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
000442****************************************************
000443
000444*  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
000445*  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA
000446
000447     12  SS-NUM-ENTRIES          PIC  9(03) VALUE 116      COMP-3.
000448     12  SS-COUNTER              PIC  9(03)                COMP-3.
000449     12  SS-WORK-AREA-LENGTH     PIC S9(04) VALUE +3340    COMP.
000450
000451 01  W-VARIABLE-WORK-AREA.
000452     12  W-VAR-CODE              PIC  X(04).
000453     12  W-VAR-LEN               PIC  9(02).
000454     12  W-VAR-DATA          PIC  X(100).
000455     12  W-VAR-DATA-R REDEFINES W-VAR-DATA.
000456       16  W-VAR-W-ONE-CHAR OCCURS 100 TIMES
000457                        INDEXED BY W-NDXV
000458                                 PIC  X(01).
000459 01  SYSTEM-SUPPORTED-VARIABLES.
000460*****COMPANY NAME
000461     12  SS01                    PIC  X(04) VALUE    '01.0'.
000462     12  SS01L                   PIC  9(02) VALUE 36.
000463     12  SS01D                   PIC  X(30) VALUE ALL '*'.
000464*****FULL COMPANY ADDRESS
000465     12  SS02-1                  PIC  X(04) VALUE    '02.1'.
000466     12  SS02-1L                 PIC  9(02) VALUE 36.
000467     12  SS02-1D                 PIC  X(30) VALUE ALL '*'.
000468     12  SS02-2                  PIC  X(04) VALUE    '02.2'.
000469     12  SS02-2L                 PIC  9(02) VALUE 36.
000470     12  SS02-2D                 PIC  X(30) VALUE ALL '*'.
000471     12  SS02-3                  PIC  X(04) VALUE    '02.3'.
000472     12  SS02-3L                 PIC  9(02) VALUE 36.
000473     12  SS02-3D                 PIC  X(30) VALUE ALL '*'.
000474     12  SS02-4                  PIC  X(04) VALUE    '02.4'.
000475     12  SS02-4L                 PIC  9(02) VALUE 36.
000476     12  SS02-4D                 PIC  X(30) VALUE ALL '*'.
000477     12  SS02-5                  PIC  X(04) VALUE    '02.5'.
000478     12  SS02-5L                 PIC  9(02) VALUE 36.
000479     12  SS02-5D                 PIC  X(30) VALUE ALL '*'.
000480*****CARRIER NAME
000481     12  SS03                    PIC  X(04) VALUE    '03.0'.
000482     12  SS03L                   PIC  9(02) VALUE 36.
000483     12  SS03D                   PIC  X(30) VALUE ALL '*'.
000484*****INVESTORS HERITAGE
000485* as Administrator for Investors Heritage Life Insurance Company
000486     12  SS03-1                  PIC X(4)  VALUE     '03.1'.
000487     12  SS03-1L                 PIC 99    VALUE 68.
000488     12  SS03-1D                 PIC X(62) VALUE ALL '*'.
000489*****FULL CARRIER ADDRESS
000490     12  SS04-1                  PIC  X(04) VALUE    '04.1'.
000491     12  SS04-1L                 PIC  9(02) VALUE 36.
000492     12  SS04-1D                 PIC  X(30) VALUE ALL '*'.
000493     12  SS04-2                  PIC  X(04) VALUE    '04.2'.
000494     12  SS04-2L                 PIC  9(02) VALUE 36.
000495     12  SS04-2D                 PIC  X(30) VALUE ALL '*'.
000496     12  SS04-3                  PIC  X(04) VALUE    '04.3'.
000497     12  SS04-3L                 PIC  9(02) VALUE 36.
000498     12  SS04-3D                 PIC  X(30) VALUE ALL '*'.
000499     12  SS04-4                  PIC  X(04) VALUE    '04.4'.
000500     12  SS04-4L                 PIC  9(02) VALUE 36.
000501     12  SS04-4D                 PIC  X(30) VALUE ALL '*'.
000502     12  SS04-5                  PIC  X(04) VALUE    '04.5'.
000503     12  SS04-5L                 PIC  9(02) VALUE 36.
000504     12  SS04-5D                 PIC  X(30) VALUE ALL '*'.
000505*****CARRIER PHONE NUMBER
000506     12  SS04-6                  PIC  X(04) VALUE    '04.6'.
000507     12  SS04-6L                 PIC  9(02) VALUE 18.
000508     12  SS04-6D                 PIC  X(12) VALUE ALL '*'.
000509*****FULL ADDRESEE LABEL
000510     12  SS05-1                  PIC  X(04) VALUE    '05.1'.
000511     12  SS05-1L                 PIC  9(02) VALUE 36.
000512     12  SS05-1D                 PIC  X(30) VALUE ALL '*'.
000513     12  SS05-2                  PIC  X(04) VALUE    '05.2'.
000514     12  SS05-2L                 PIC  9(02) VALUE 36.
000515     12  SS05-2D                 PIC  X(30) VALUE ALL '*'.
000516     12  SS05-3                  PIC  X(04) VALUE    '05.3'.
000517     12  SS05-3L                 PIC  9(02) VALUE 36.
000518     12  SS05-3D                 PIC  X(30) VALUE ALL '*'.
000519     12  SS05-4                  PIC  X(04) VALUE    '05.4'.
000520     12  SS05-4L                 PIC  9(02) VALUE 36.
000521     12  SS05-4D                 PIC  X(30) VALUE ALL '*'.
000522     12  SS05-5                  PIC  X(04) VALUE    '05.5'.
000523     12  SS05-5L                 PIC  9(02) VALUE 36.
000524     12  SS05-5D                 PIC  X(30) VALUE ALL '*'.
000525     12  SS05-6                  PIC  X(04) VALUE    '05.6'.
000526     12  SS05-6L                 PIC  9(02) VALUE 36.
000527     12  SS05-6D                 PIC  X(30) VALUE ALL '*'.
000528*****ACCOUNT NAME
000529     12  SS06                    PIC  X(04) VALUE    '06.0'.
000530     12  SS06L                   PIC  9(02) VALUE 36.
000531     12  SS06D                   PIC  X(30) VALUE ALL '*'.
000532*****FULL ACCOUNT ADDRESS
000533     12  SS07-1                  PIC  X(04) VALUE    '07.1'.
000534     12  SS07-1L                 PIC  9(02) VALUE 36.
000535     12  SS07-1D                 PIC  X(30) VALUE ALL '*'.
000536     12  SS07-2                  PIC  X(04) VALUE    '07.2'.
000537     12  SS07-2L                 PIC  9(02) VALUE 36.
000538     12  SS07-2D                 PIC  X(30) VALUE ALL '*'.
000539     12  SS07-3                  PIC  X(04) VALUE    '07.3'.
000540     12  SS07-3L                 PIC  9(02) VALUE 36.
000541     12  SS07-3D                 PIC  X(30) VALUE ALL '*'.
000542     12  SS07-4                  PIC  X(04) VALUE    '07.4'.
000543     12  SS07-4L                 PIC  9(02) VALUE 36.
000544     12  SS07-4D                 PIC  X(30) VALUE ALL '*'.
000545     12  SS07-5                  PIC  X(04) VALUE    '07.5'.
000546     12  SS07-5L                 PIC  9(02) VALUE 36.
000547     12  SS07-5D                 PIC  X(30) VALUE ALL '*'.
000548*****ACCOUNT PHONE NUMBER
000549     12  SS07-6                  PIC  X(04) VALUE    '07.6'.
000550     12  SS07-6L                 PIC  9(02) VALUE 18.
000551     12  SS07-6D                 PIC  X(12) VALUE ALL '*'.
000552*****EXECUTING PROCESSOR NAME
000553     12  SS08                    PIC  X(04) VALUE    '08.0'.
000554     12  SS08L                   PIC  9(02) VALUE 36.
000555     12  SS08D                   PIC  X(30) VALUE ALL '*'.
000556*****PROCESSOR TITLE
000557     12  SS09                    PIC  X(04) VALUE    '09.0'.
000558     12  SS09L                   PIC  9(02) VALUE 32.
000559     12  SS09D                   PIC  X(26) VALUE ALL '*'.
000560*****INSUREDS NAME
000561     12  SS10                    PIC  X(04) VALUE    '10.0'.
000562     12  SS10L                   PIC  9(02) VALUE 36.
000563     12  SS10D                   PIC  X(30) VALUE ALL '*'.
000564*****INSUREDS ADDRESS
000565     12  SS11-1                  PIC  X(04) VALUE    '11.1'.
000566     12  SS11-1L                 PIC  9(02) VALUE 36.
000567     12  SS11-1D                 PIC  X(30) VALUE ALL '*'.
000568     12  SS11-2                  PIC  X(04) VALUE    '11.2'.
000569     12  SS11-2L                 PIC  9(02) VALUE 36.
000570     12  SS11-2D                 PIC  X(30) VALUE ALL '*'.
000571     12  SS11-3                  PIC  X(04) VALUE    '11.3'.
000572     12  SS11-3L                 PIC  9(02) VALUE 36.
000573     12  SS11-3D                 PIC  X(30) VALUE ALL '*'.
000574     12  SS11-4                  PIC  X(04) VALUE    '11.4'.
000575     12  SS11-4L                 PIC  9(02) VALUE 36.
000576     12  SS11-4D                 PIC  X(30) VALUE ALL '*'.
000577*****INSUREDS NAME FROM ADDR TRAILER
000578     12  SS11-5                  PIC  X(04) VALUE    '11.5'.
000579     12  SS11-5L                 PIC  9(02) VALUE 36.
000580     12  SS11-5D                 PIC  X(30) VALUE ALL '*'.
000581*****INSUREDS PHONE NUMBER FROM ADDR TRAILER
000582     12  SS11-6                  PIC  X(04) VALUE    '11.6'.
000583     12  SS11-6L                 PIC  9(02) VALUE 18.
000584     12  SS11-6D                 PIC  X(12) VALUE ALL '*'.
000585*****CLAIM TYPE NAME
000586     12  SS12                    PIC  X(04) VALUE    '12.0'.
000587     12  SS12L                   PIC  9(02) VALUE 12.
000588     12  SS12D                   PIC  X(6) VALUE ALL '*'.
000589*****CLAIM INCURRED DATE
000590     12  SS13                    PIC  X(04) VALUE    '13.0'.
000591     12  SS13L                   PIC  9(02) VALUE 14.
000592     12  SS13D                   PIC  X(08) VALUE ALL '*'.
000593*****CLAIM REPORTED DATE
000594     12  SS14                    PIC  X(04) VALUE    '14.0'.
000595     12  SS14L                   PIC  9(02) VALUE 14.
000596     12  SS14D                   PIC  X(08) VALUE ALL '*'.
000597*****LAST PAYMENT DATE
000598     12  SS15                    PIC  X(04) VALUE    '15.0'.
000599     12  SS15L                   PIC  9(02) VALUE 14.
000600     12  SS15D                   PIC  X(08) VALUE ALL '*'.
000601*****LAST PAYMENT AMOUNT
000602     12  SS16                    PIC  X(04) VALUE    '16.0'.
000603     12  SS16L                   PIC  9(02) VALUE 17.
000604     12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.
000605*****CLAIM PAID THRU/TO DATE
000606     12  SS17                    PIC  X(04) VALUE    '17.0'.
000607     12  SS17L                   PIC  9(02) VALUE 14.
000608     12  SS17D                   PIC  X(08) VALUE ALL '*'.
000609*****TOTAL PAID TO DATE
000610     12  SS18                    PIC  X(04) VALUE    '18.0'.
000611     12  SS18L                   PIC  9(02) VALUE 17.
000612     12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.
000613*****DIAGNOSIS OR CAUSE
000614     12  SS19                    PIC  X(04) VALUE    '19.0'.
000615     12  SS19L                   PIC  9(02) VALUE 32.
000616     12  SS19D                   PIC  X(26) VALUE ALL '*'.
000617*****CAUSE CODE
000618     12  SS19-1                  PIC  X(04) VALUE    '19.1'.
000619     12  SS19-1L                 PIC  9(02) VALUE 12.
000620     12  SS19-1D                 PIC  X(6) VALUE ALL '*'.
000621*****CURRENT DATE
000622     12  SS20                    PIC  X(04) VALUE    '20.0'.
000623     12  SS20L                   PIC  9(02) VALUE 14.
000624     12  SS20D                   PIC  X(08) VALUE ALL '*'.
000625*****FULL CURRENT DATE
000626     12  SS21                    PIC  X(04) VALUE    '21.0'.
000627     12  SS21L                   PIC  9(02) VALUE 24.
000628     12  SS21D                   PIC  X(18) VALUE ALL '*'.
000629*****BENEFIT DESCRIPTION
000630     12  SS22                    PIC  X(04) VALUE    '22.0'.
000631     12  SS22L                   PIC  9(02) VALUE 16.
000632     12  SS22D                   PIC  X(10) VALUE ALL '*'.
000633*****CARRIER CODE IN CERT
000634     12  SS23                    PIC  X(04) VALUE    '23.0'.
000635     12  SS23L                   PIC  9(02) VALUE 9.
000636     12  SS23D                   PIC  X(03) VALUE ALL '*'.
000637*****GROUPING CODE IN CERT
000638     12  SS24                    PIC  X(04) VALUE    '24.0'.
000639     12  SS24L                   PIC  9(02) VALUE 12.
000640     12  SS24D                   PIC  X(6) VALUE ALL '*'.
000641*****ACCOUNT NUMBER IN CERT
000642     12  SS25                    PIC  X(04) VALUE    '25.0'.
000643     12  SS25L                   PIC  9(02) VALUE 16.
000644     12  SS25D                   PIC  X(10) VALUE ALL '*'.
000645*****CERTIFICATE NUMBER
000646     12  SS26                    PIC  X(04) VALUE    '26.0'.
000647     12  SS26L                   PIC  9(02) VALUE 17.
000648     12  SS26D                   PIC  X(11) VALUE ALL '*'.
000649*****CERT EFFECTIVE DATE
000650     12  SS27                    PIC  X(04) VALUE    '27.0'.
000651     12  SS27L                   PIC  9(02) VALUE 14.
000652     12  SS27D                   PIC  X(08) VALUE ALL '*'.
000653*****CERT EXPIRATION DATE
000654     12  SS28                    PIC  X(04) VALUE    '28.0'.
000655     12  SS28L                   PIC  9(02) VALUE 14.
000656     12  SS28D                   PIC  X(08) VALUE ALL '*'.
000657*****APPLICABLE COVERAGE TERM
000658     12  SS29                    PIC  X(04) VALUE    '29.0'.
000659     12  SS29L                   PIC  9(02) VALUE 9.
000660     12  SS29D                   PIC  X(03) VALUE ALL '*'.
000661*****APPLICABLE COVERAGE AMOUNT
000662     12  SS30                    PIC  X(04) VALUE    '30.0'.
000663     12  SS30L                   PIC  9(02) VALUE 18.
000664     12  SS30D                   PIC $$$$$,$$$.99 VALUE ZEROS.
000665*****APPLICABLE COVERAGE CANCEL DATE
000666     12  SS31                    PIC  X(04) VALUE    '31.0'.
000667     12  SS31L                   PIC  9(02) VALUE 14.
000668     12  SS31D                   PIC  X(08) VALUE ALL '*'.
000669*****APPLICABLE COVERAGE FORM NUMBER
000670     12  SS32                    PIC  X(04) VALUE    '32.0'.
000671     12  SS32L                   PIC  9(02) VALUE 18.
000672     12  SS32D                   PIC  X(12) VALUE ALL '*'.
000673*****INSURES AGE AT POLICY ISSUE
000674     12  SS33                    PIC  X(04) VALUE    '33.0'.
000675     12  SS33L                   PIC  9(02) VALUE 9.
000676     12  SS33D                   PIC  X(03) VALUE ALL '*'.
000677*****CLAIM NUMBER
000678     12  SS34                    PIC  X(04) VALUE    '34.0'.
000679     12  SS34L                   PIC  9(02) VALUE 13.
000680     12  SS34D                   PIC  X(07) VALUE ALL '*'.
000681*****LAST DENIAL TEXT
000682     12  SS35-1                  PIC  X(04) VALUE    '35.1'.
000683     12  SS35-1L                 PIC  9(02) VALUE 66.
000684     12  SS35-1D                 PIC  X(60) VALUE ALL '*'.
000685     12  SS35-2                  PIC  X(04) VALUE    '35.2'.
000686     12  SS35-2L                 PIC  9(02) VALUE 66.
000687     12  SS35-2D                 PIC  X(60) VALUE ALL '*'.
000688*****LOAN NUMBER
000689     12  SS36                    PIC  X(04) VALUE    '36.0'.
000690     12  SS36L                   PIC  9(02) VALUE 14.
000691     12  SS36D                   PIC  X(08) VALUE ALL '*'.
000692*****CREDIT CARD LOAN NUMBER
000693     12  SS36-1                  PIC  X(04) VALUE    '36.1'.
000694     12  SS36-1L                 PIC  9(02) VALUE 26.
000695     12  SS36-1D                 PIC  X(20) VALUE ALL '*'.
000696*****LOAN BALANCE
000697     12  SS37                    PIC  X(04) VALUE    '37.0'.
000698     12  SS37L                   PIC  9(02) VALUE 18.
000699     12  SS37D                   PIC $$$$$,$$$.99 VALUE ZEROS.
000700*****MEMBER NUMBER
000701     12  SS38                    PIC  X(04) VALUE    '38.0'.
000702     12  SS38L                   PIC  9(02) VALUE 18.
000703     12  SS38D                   PIC  X(12) VALUE ALL '*'.
000704*****INSURED NAME (FIRST M LAST)
000705     12  SS39                    PIC  X(04) VALUE    '39.0'.
000706     12  SS39L                   PIC  9(02) VALUE 36.
000707     12  SS39D                   PIC  X(30) VALUE ALL '*'.
000708*****INSURED LAST NAME ONLY
000709     12  SS40                    PIC  X(04) VALUE    '40.0'.
000710     12  SS40L                   PIC  9(02) VALUE 21.
000711     12  SS40D                   PIC  X(15) VALUE ALL '*'.
000712*****TITLE (MR/MS)
000713     12  SS41                    PIC  X(04) VALUE    '41.0'.
000714     12  SS41L                   PIC  9(02) VALUE 9.
000715     12  SS41D                   PIC  X(03) VALUE ALL '*'.
000716*****ELIMINATION PERIOD
000717     12  SS42                    PIC  X(04) VALUE    '42.0'.
000718     12  SS42L                   PIC  9(02) VALUE 9.
000719     12  SS42D                   PIC  X(03) VALUE ALL '*'.
000720*****BENEFICIARY NAME
000721     12  SS43                    PIC  X(04) VALUE    '43.0'.
000722     12  SS43L                   PIC  9(02) VALUE 36.
000723     12  SS43D                   PIC  X(30) VALUE ALL '*'.
000724*****BENEFICIARY ADDRESS
000725     12  SS44-1                  PIC  X(04) VALUE    '44.1'.
000726     12  SS44-1L                 PIC  9(02) VALUE 36.
000727     12  SS44-1D                 PIC  X(30) VALUE ALL '*'.
000728     12  SS44-2                  PIC  X(04) VALUE    '44.2'.
000729     12  SS44-2L                 PIC  9(02) VALUE 36.
000730     12  SS44-2D                 PIC  X(30) VALUE ALL '*'.
000731     12  SS44-3                  PIC  X(04) VALUE    '44.3'.
000732     12  SS44-3L                 PIC  9(02) VALUE 36.
000733     12  SS44-3D                 PIC  X(30) VALUE ALL '*'.
000734     12  SS44-4                  PIC  X(04) VALUE    '44.4'.
000735     12  SS44-4L                 PIC  9(02) VALUE 36.
000736     12  SS44-4D                 PIC  X(30) VALUE ALL '*'.
000737     12  SS44-5                  PIC  X(04) VALUE    '44.5'.
000738     12  SS44-5L                 PIC  9(02) VALUE 18.
000739     12  SS44-5D                 PIC  X(12) VALUE ALL '*'.
000740*****INSUREDS DATE OF BIRTH
000741     12  SS45                    PIC  X(04) VALUE    '45.0'.
000742     12  SS45L                   PIC  9(02) VALUE 14.
000743     12  SS45D                   PIC  X(08) VALUE ALL '*'.
000744*****INSUREDS SOC SEC NUMBER
000745     12  SS46                    PIC  X(04) VALUE    '46.0'.
000746     12  SS46L                   PIC  9(02) VALUE 17.
000747     12  SS46D                   PIC  X(11) VALUE ALL '*'.
000748*****PHYSICIANS  NAME
000749     12  SS47                    PIC  X(04) VALUE    '47.0'.
000750     12  SS47L                   PIC  9(02) VALUE 36.
000751     12  SS47D                   PIC  X(30) VALUE ALL '*'.
000752*****PHYSICIANS  ADDRESS
000753     12  SS47-1                  PIC  X(04) VALUE    '47.1'.
000754     12  SS47-1L                 PIC  9(02) VALUE 36.
000755     12  SS47-1D                 PIC  X(30) VALUE ALL '*'.
000756     12  SS47-2                  PIC  X(04) VALUE    '47.2'.
000757     12  SS47-2L                 PIC  9(02) VALUE 36.
000758     12  SS47-2D                 PIC  X(30) VALUE ALL '*'.
000759     12  SS47-3                  PIC  X(04) VALUE    '47.3'.
000760     12  SS47-3L                 PIC  9(02) VALUE 36.
000761     12  SS47-3D                 PIC  X(30) VALUE ALL '*'.
000762     12  SS47-4                  PIC  X(04) VALUE    '47.4'.
000763     12  SS47-4L                 PIC  9(02) VALUE 36.
000764     12  SS47-4D                 PIC  X(30) VALUE ALL '*'.
000765     12  SS47-5                  PIC  X(04) VALUE    '47.5'.
000766     12  SS47-5L                 PIC  9(02) VALUE 18.
000767     12  SS47-5D                 PIC  X(12) VALUE ALL '*'.
000768*****EMPLOYERS   NAME
000769     12  SS48                    PIC  X(04) VALUE    '48.0'.
000770     12  SS48L                   PIC  9(02) VALUE 36.
000771     12  SS48D                   PIC  X(30) VALUE ALL '*'.
000772*****EMPLOYERS   ADDRESS
000773     12  SS48-1                  PIC  X(04) VALUE    '48.1'.
000774     12  SS48-1L                 PIC  9(02) VALUE 36.
000775     12  SS48-1D                 PIC  X(30) VALUE ALL '*'.
000776     12  SS48-2                  PIC  X(04) VALUE    '48.2'.
000777     12  SS48-2L                 PIC  9(02) VALUE 36.
000778     12  SS48-2D                 PIC  X(30) VALUE ALL '*'.
000779     12  SS48-3                  PIC  X(04) VALUE    '48.3'.
000780     12  SS48-3L                 PIC  9(02) VALUE 36.
000781     12  SS48-3D                 PIC  X(30) VALUE ALL '*'.
000782     12  SS48-4                  PIC  X(04) VALUE    '48.4'.
000783     12  SS48-4L                 PIC  9(02) VALUE 36.
000784     12  SS48-4D                 PIC  X(30) VALUE ALL '*'.
000785     12  SS48-5                  PIC  X(04) VALUE    '48.5'.
000786     12  SS48-5L                 PIC  9(02) VALUE 18.
000787     12  SS48-5D                 PIC  X(12) VALUE ALL '*'.
000788*****OTHER1      NAME
000789     12  SS49                    PIC  X(04) VALUE    '49.0'.
000790     12  SS49L                   PIC  9(02) VALUE 36.
000791     12  SS49D                   PIC  X(30) VALUE ALL '*'.
000792*****OTHER1      ADDRESS
000793     12  SS49-1                  PIC  X(04) VALUE    '49.1'.
000794     12  SS49-1L                 PIC  9(02) VALUE 36.
000795     12  SS49-1D                 PIC  X(30) VALUE ALL '*'.
000796     12  SS49-2                  PIC  X(04) VALUE    '49.2'.
000797     12  SS49-2L                 PIC  9(02) VALUE 36.
000798     12  SS49-2D                 PIC  X(30) VALUE ALL '*'.
000799     12  SS49-3                  PIC  X(04) VALUE    '49.3'.
000800     12  SS49-3L                 PIC  9(02) VALUE 36.
000801     12  SS49-3D                 PIC  X(30) VALUE ALL '*'.
000802     12  SS49-4                  PIC  X(04) VALUE    '49.4'.
000803     12  SS49-4L                 PIC  9(02) VALUE 36.
000804     12  SS49-4D                 PIC  X(30) VALUE ALL '*'.
000805     12  SS49-5                  PIC  X(04) VALUE    '49.5'.
000806     12  SS49-5L                 PIC  9(02) VALUE 18.
000807     12  SS49-5D                 PIC  X(12) VALUE ALL '*'.
000808*****OTHER2      NAME
000809     12  SS50                    PIC  X(04) VALUE    '50.0'.
000810     12  SS50L                   PIC  9(02) VALUE 36.
000811     12  SS50D                   PIC  X(30) VALUE ALL '*'.
000812*****OTHER2      ADDRESS
000813     12  SS50-1                  PIC  X(04) VALUE    '50.1'.
000814     12  SS50-1L                 PIC  9(02) VALUE 36.
000815     12  SS50-1D                 PIC  X(30) VALUE ALL '*'.
000816     12  SS50-2                  PIC  X(04) VALUE    '50.2'.
000817     12  SS50-2L                 PIC  9(02) VALUE 36.
000818     12  SS50-2D                 PIC  X(30) VALUE ALL '*'.
000819     12  SS50-3                  PIC  X(04) VALUE    '50.3'.
000820     12  SS50-3L                 PIC  9(02) VALUE 36.
000821     12  SS50-3D                 PIC  X(30) VALUE ALL '*'.
000822     12  SS50-4                  PIC  X(04) VALUE    '50.4'.
000823     12  SS50-4L                 PIC  9(02) VALUE 36.
000824     12  SS50-4D                 PIC  X(30) VALUE ALL '*'.
000825     12  SS50-5                  PIC  X(04) VALUE    '50.5'.
000826     12  SS50-5L                 PIC  9(02) VALUE 18.
000827     12  SS50-5D                 PIC  X(12) VALUE ALL '*'.
000828*****A&H TERM TIMES MON. BEN.
000829     12  SS51                    PIC  X(04) VALUE    '51.0'.
000830     12  SS51L                   PIC  9(02) VALUE 17.
000831     12  SS51D                   PIC $$$$,$$$.99  VALUE ZEROS.
000832*****THIRD PARTY NAME
000833     12  SS52                    PIC  X(04) VALUE    '52.0'.
000834     12  SS52L                   PIC  9(02) VALUE 36.
000835     12  SS52D                   PIC  X(30) VALUE ALL '*'.
000836*****THIRD PARTY ADDRESS
000837     12  SS53-1                  PIC  X(04) VALUE    '53.1'.
000838     12  SS53-1L                 PIC  9(02) VALUE 36.
000839     12  SS53-1D                 PIC  X(30) VALUE ALL '*'.
000840     12  SS53-2                  PIC  X(04) VALUE    '53.2'.
000841     12  SS53-2L                 PIC  9(02) VALUE 36.
000842     12  SS53-2D                 PIC  X(30) VALUE ALL '*'.
000843     12  SS53-3                  PIC  X(04) VALUE    '53.3'.
000844     12  SS53-3L                 PIC  9(02) VALUE 36.
000845     12  SS53-3D                 PIC  X(30) VALUE ALL '*'.
000846     12  SS53-4                  PIC  X(04) VALUE    '53.4'.
000847     12  SS53-4L                 PIC  9(02) VALUE 36.
000848     12  SS53-4D                 PIC  X(30) VALUE ALL '*'.
000849     12  SS53-5                  PIC  X(04) VALUE    '53.5'.
000850     12  SS53-5L                 PIC  9(02) VALUE 36.
000851     12  SS53-5D                 PIC  X(30) VALUE ALL '*'.
000852*****THIRD PARTY PHONE NUMBER
000853     12  SS53-6                  PIC  X(04) VALUE    '53.6'.
000854     12  SS53-6L                 PIC  9(02) VALUE 18.
000855     12  SS53-6D                 PIC  X(12) VALUE ALL '*'.
000856*****CERTIFICATE SEQUENCE
000857     12  SS54                    PIC  X(04) VALUE    '54.0'.
000858     12  SS54L                   PIC  9(02) VALUE 09.
000859     12  SS54D                   PIC  X(03) VALUE ALL '*'.
000860*****CERTIFICATE TOTAL  E
000861     12  SS55                    PIC  X(04) VALUE    '55.0'.
000862     12  SS55L                   PIC  9(02) VALUE 09.
000863     12  SS55D                   PIC  X(03) VALUE ALL '*'.
000864*****CREDITOR ID
000865     12  SS56                    PIC  X(04) VALUE    '56.0'.
000866     12  SS56L                   PIC  9(02) VALUE 36.
000867     12  SS56D                   PIC  X(30) VALUE ALL '*'.
000868*****INSUREDS NAME (CERTIFICATE)
000869     12  SS57                    PIC X(4)  VALUE     '57.0'.
000870     12  SS57L                   PIC 99    VALUE 36.
000871     12  SS57D                   PIC X(30) VALUE ALL '*'.
000872*****JOINTS NAME (CERTIFICATE)
000873     12  SS58                    PIC X(4)  VALUE     '58.0'.
000874     12  SS58L                   PIC 99    VALUE 36.
000875     12  SS58D                   PIC X(30) VALUE ALL '*'.
000876*****POLICY REFERENCE NUMBER
000877     12  SS59                    PIC X(4)  VALUE     '59.0'.
000878     12  SS59L                   PIC 99    VALUE 26.
000879     12  SS59D                   PIC X(20) VALUE ALL '*'.
000880
000881****************************************************
000882*       WHEN ADDING OR DELETING ENTRIES TO         *
000883*       THE SYSTEM-SUPPORTED-VARIABLES THE         *
000884*       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
000885*       TO MATCH THE NUMBER OF ENTRIES IN THE      *
000886*       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
000887*       ALSO YOU NEED           TO INCREASE THE LENGTH OF *
000888*       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
000889****************************************************
000890                                 EJECT
000891*                                COPY ELCDATE.
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
000892                                 EJECT
000893 01  W-PROGRAM-CONSTANTS.
000894     12  FILLER                  PIC  X(17)
000895                                 VALUE 'PROGRAM CONSTANTS'.
000896     12  W-ACCT-LENGTH           PIC S9(04) COMP  VALUE +2000.
000897     12  W-PROD-LENGTH           PIC S9(04) COMP  VALUE +2000.
000898     12  W-ACTV-LENGTH           PIC S9(04) COMP  VALUE +200.
000899     12  W-ARCH-LENGTH           PIC S9(04) COMP  VALUE +90.
000900     12  W-NAPS-LENGTH           PIC S9(04) COMP  VALUE +150.
000901
000902     12  W-ACCT-ID               PIC  X(08)   VALUE 'ERACCT'.
000903     12  W-ACTV-ID               PIC  X(08)   VALUE 'ELTRLR'.
000904     12  W-ARCH-ID               PIC  X(08)   VALUE 'ELARCH'.
000905     12  W-BENE-ID               PIC  X(08)   VALUE 'ELBENE'.
000906     12  W-CLM-ID                PIC  X(08)   VALUE 'ELMSTR'.
000907     12  W-CNTL-ID               PIC  X(08)   VALUE 'ELCNTL'.
000908     12  W-CERT-ID               PIC  X(08)   VALUE 'ELCERT'.
000909     12  W-PROD-ID               PIC  X(08)   VALUE 'MPPROD'.
000910     12  W-PLCY-ID               PIC  X(08)   VALUE 'MPPLCY'.
000911     12  W-PLAN-ID               PIC  X(08)   VALUE 'MPPLAN'.
000912     12  W-NAPS-ID               PIC  X(08)   VALUE 'ELNAPS'.
000913     12  W-GETMAINSPACE          PIC  X(01) VALUE SPACE.
000914     12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.
000915     12  W-LOWER-CASE
000916                    PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
000917     12  W-MAX-LINES             PIC  9(03) VALUE 300.
000918     12  W-TEXT-ID               PIC  X(08) VALUE 'ELLETR'.
000919     12  W-TOP-FORM              PIC  X(70)
000920         VALUE '*****TOP OF FORM *****'.
000921     12  W-TRANSACTION           PIC  X(04) VALUE 'EXL1'.
000922     12  W-UPPER-CASE
000923                    PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
000924                                 EJECT
000925 01  ERROR-MESSAGES.
000926     12  ER-0006                 PIC  X(04) VALUE '0006'.
000927     12  ER-0013                 PIC  X(04) VALUE '0013'.
000928     12  ER-0042                 PIC  X(04) VALUE '0042'.
000929     12  ER-0051                 PIC  X(04) VALUE '0051'.
000930     12  ER-0154                 PIC  X(04) VALUE '0154'.
000931     12  ER-0168                 PIC  X(04) VALUE '0168'.
000932     12  ER-0169                 PIC  X(04) VALUE '0169'.
000933     12  ER-0172                 PIC  X(04) VALUE '0172'.
000934     12  ER-0176                 PIC  X(04) VALUE '0176'.
000935     12  ER-0178                 PIC  X(04) VALUE '0178'.
000936     12  ER-0179                 PIC  X(04) VALUE '0179'.
000937     12  ER-0180                 PIC  X(04) VALUE '0180'.
000938     12  ER-0181                 PIC  X(04) VALUE '0181'.
000939     12  ER-0186                 PIC  X(04) VALUE '0186'.
000940     12  ER-0191                 PIC  X(04) VALUE '0191'.
000941     12  ER-0206                 PIC  X(04) VALUE '0206'.
000942     12  ER-0281                 PIC  X(04) VALUE '0281'.
000943     12  ER-0332                 PIC  X(04) VALUE '0332'.
000944     12  ER-0413                 PIC  X(04) VALUE '0413'.
000945     12  ER-2055                 PIC  X(04) VALUE '2055'.
000946     12  ER-3697                 PIC  X(04) VALUE '3697'.
000947     12  ER-3698                 PIC  X(04) VALUE '3698'.
000948     12  ER-3699                 PIC  X(04) VALUE '3699'.
000949     12  ER-3766                 PIC  X(04) VALUE '3766'.
000950     12  ER-3770                 PIC  X(04) VALUE '3770'.
000951     12  ER-3771                 PIC  X(04) VALUE '3771'.
000952     12  ER-3772                 PIC  X(04) VALUE '3772'.
000953     12  ER-7675                 PIC  X(04) VALUE '7675'.
000954     12  ER-9106                 PIC  X(04) VALUE '9106'.
000955     12  ER-9483                 PIC  X(04) VALUE '9483'.
000956     12  ER-9808                 PIC  X(04) VALUE '9808'.
000957     12  ER-9883                 PIC  X(04) VALUE '9883'.
000958     12  ER-9886                 PIC  X(04) VALUE '9886'.
000959     12  ER-9887                 PIC  X(04) VALUE '9887'.
000960
000961                                 EJECT
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
000963 01  DFHCOMMAREA                 PIC  X(1024).
000964
000965*01 PARMLIST .
000966*    02  FILLER                  PIC S9(08)  COMP.
000967*    02  L-ACCT-POINTER          PIC S9(08)  COMP.
000968*    02  L-ACTV-POINTER          PIC S9(08)  COMP.
000969*    02  L-ARCH-POINTER          PIC S9(08)  COMP.
000970*    02  L-BENE-POINTER          PIC S9(08)  COMP.
000971*    02  L-CERT-POINTER          PIC S9(08)  COMP.
000972*    02  L-CLM-POINTER           PIC S9(08)  COMP.
000973*    02  L-CNTL-POINTER          PIC S9(08)  COMP.
000974*    02  L-COMP-POINTER          PIC S9(08)  COMP.
000975*    02  L-TEXT-POINTER          PIC S9(08)  COMP.
000976*    02  L-PROD-POINTER          PIC S9(08)  COMP.
000977*    02  L-PLCY-POINTER          PIC S9(08)  COMP.
000978*    02  L-PLAN-POINTER          PIC S9(08)  COMP.
000979*    02  L-VARIABLE-POINTER      PIC S9(08)  COMP.
000980
000981                                 EJECT
000982*                                COPY ERCACCT.
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
000983                                 EJECT
000984*                                COPY ELCTRLR.
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
000985                                 EJECT
000986*                                COPY ELCARCH.
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
000987                                 EJECT
000988*                                COPY ELCBENE.
      *>>((file: ELCBENE))
000001******************************************************************
000002*                                                                *
000003*                            ELCBENE.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.006                          *
000006*                                                                *
000007*   FILE DESCRIPTION = BENEFICIARY FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 500   RECFORM = FIX                            *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
000013*     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 013017  CR2016053100001  PEMA  ACH PROCESSING
000028* 082317  CR2017082100003  PEMA  Add sub type
000029* 032019  CR2019011400002  PEMA  Add email address for ach report
000030******************************************************************
000031
000032 01  BENEFICIARY-MASTER.
000033     12  BE-RECORD-ID                PIC XX.
000034         88  VALID-BE-ID                VALUE 'BE'.
000035
000036     12  BE-CONTROL-PRIMARY.
000037         16  BE-COMPANY-CD           PIC X.
000038         16  BE-RECORD-TYPE          PIC X.
000039             88  BENEFICIARY-RECORD  VALUE 'B'.
000040             88  ADJUSTOR-RECORD     VALUE 'A'.
000041         16  BE-BENEFICIARY          PIC X(10).
000042     12  BE-CONTROL-BY-NAME.
000043         16  BE-COMPANY-CD-A1        PIC X.
000044         16  BE-RECORD-TYPE-A1       PIC X.
000045         16  BE-MAIL-TO-NAME-A1      PIC X(30).
000046         16  BE-ALTERNATE-PRIME-A1   PIC X(10).
000047
000048     12  BE-LAST-MAINT-DT            PIC XX.
000049     12  BE-LAST-MAINT-BY            PIC X(4).
000050     12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000051
000052     12  BE-ADDRESS-DATA.
000053         16  BE-MAIL-TO-NAME         PIC X(30).
000054         16  BE-ADDRESS-LINE-1       PIC X(30).
000055         16  BE-ADDRESS-LINE-2       PIC X(30).
000056         16  BE-ADDRESS-LINE-3       PIC X(30).
000057         16  BE-CITY-STATE.
000058             20  BE-CITY             PIC X(28).
000059             20  BE-STATE            PIC XX.
000060         16  BE-ZIP-CODE.
000061             20  BE-ZIP-PRIME.
000062                 24  BE-ZIP-1ST      PIC X.
000063                     88  BE-CANADIAN-POST-CODE
000064                                         VALUE 'A' THRU 'Z'.
000065                 24  FILLER          PIC X(4).
000066             20  BE-ZIP-PLUS4        PIC X(4).
000067         16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
000068             20  BE-CAN-POSTAL-1     PIC XXX.
000069             20  BE-CAN-POSTAL-2     PIC XXX.
000070             20  FILLER              PIC XXX.
000071         16  BE-PHONE-NO             PIC 9(11)     COMP-3.
000072         16  BE-GROUP-CHECKS-Y-N     PIC X.
000073
000074******************************************************************
000075*    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
000076*    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
000077******************************************************************
000078     12  BE-CARRIER                  PIC X.
000079
000080     12  BE-ADDRESS-DATA2.
000081         16  BE-MAIL-TO-NAME2        PIC X(30).
000082         16  BE-ADDRESS-LINE-12      PIC X(30).
000083         16  BE-ADDRESS-LINE-22      PIC X(30).
000084         16  BE-ADDRESS-LINE-32      PIC X(30).
000085         16  BE-CITY-STATE2.
000086             20  BE-CITY2            PIC X(28).
000087             20  BE-STATE2           PIC XX.
000088         16  BE-ZIP-CODE2.
000089             20  BE-ZIP-PRIME2.
000090                 24  BE-ZIP-1ST2     PIC X.
000091                     88  BE-CANADIAN-POST-CODE2
000092                                         VALUE 'A' THRU 'Z'.
000093                 24  FILLER          PIC X(4).
000094             20  BE-ZIP-PLUS42       PIC X(4).
000095         16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
000096             20  BE-CAN-POSTAL-12    PIC XXX.
000097             20  BE-CAN-POSTAL-22    PIC XXX.
000098             20  FILLER              PIC XXX.
000099         16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
000100         16  BE-ACH-DATA.
000101             20  BE-ACH-YES-OR-NO    PIC X.
000102                 88  BE-ON-ACH       VALUE 'Y'.
000103                 88  BE-NOT-ON-ACH   VALUE 'N' ' '.
000104             20  BE-ACH-ABA-ROUTING-NUMBER
000105                                     PIC X(15).
000106             20  BE-ACH-BANK-ACCOUNT-NUMBER
000107                                     PIC X(20).
000108             20  BE-ACH-SUB-TYPE     PIC XX.
000109             20  BE-ACH-EMAIL-YN     PIC X.
000110                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
000111             20  be-ach-email-addr   PIC X(40).
000112         16  BE-BILLING-STMT-DATA.
000113*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
000114             20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
000115             20  BE-OUTPUT-TYPE      PIC X.
000116                 88  BE-FAX-OUTPUT         VALUE 'F'.
000117                 88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
000118
000119     12  filler                      PIC X(16).
000120******************************************************************
      *<<((file: ELCBENE))
000989                                 EJECT
000990*                                COPY ELCCERT.
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
000991                                 EJECT
000992*                                COPY ELCMSTR.
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
000993                                 EJECT
000994*                                COPY ELCCNTL.
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
000995                                 EJECT
000996*                                COPY ERCCOMP.
      *>>((file: ERCCOMP))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCOMP                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.019                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION MASTER                       *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 700   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022*                   C H A N G E   L O G
000023*
000024* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000025*-----------------------------------------------------------------
000026*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000027* EFFECTIVE    NUMBER
000028*-----------------------------------------------------------------
000029* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000030* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
000031* 092205    2005050300006  PEMA  ADD LEASE FEE
000032* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
000033* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
000034* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
000035* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000036* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
000037* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
000038******************************************************************
000039
000040 01  COMPENSATION-MASTER.
000041     12  CO-RECORD-ID                          PIC XX.
000042         88  VALID-CO-ID                          VALUE 'CO'.
000043
000044     12  CO-CONTROL-PRIMARY.
000045         16  CO-COMPANY-CD                     PIC X.
000046         16  CO-CONTROL.
000047             20  CO-CTL-1.
000048                 24  CO-CARR-GROUP.
000049                     28  CO-CARRIER            PIC X.
000050                     28  CO-GROUPING.
000051                         32  CO-GROUP-PREFIX   PIC XXX.
000052                         32  CO-GROUP-PRIME    PIC XXX.
000053                 24  CO-RESP-NO.
000054                     28  CO-RESP-PREFIX        PIC X(4).
000055                     28  CO-RESP-PRIME         PIC X(6).
000056             20  CO-CTL-2.
000057                 24  CO-ACCOUNT.
000058                     28  CO-ACCT-PREFIX        PIC X(4).
000059                     28  CO-ACCT-PRIME         PIC X(6).
000060         16  CO-TYPE                           PIC X.
000061             88  CO-COMPANY-TYPE                  VALUE 'C'.
000062             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
000063             88  CO-ACCOUNT-TYPE                  VALUE 'A'.
000064
000065     12  CO-MAINT-INFORMATION.
000066         16  CO-LAST-MAINT-DT                  PIC XX.
000067         16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000068         16  CO-LAST-MAINT-USER                PIC X(4).
000069     12  FILLER                                PIC XX.
000070     12  CO-STMT-TYPE                          PIC XXX.
000071     12  CO-COMP-TYPE                          PIC X.
000072         88  CO-COMP-IS-SPPDD                    VALUE '1'.
000073     12  CO-STMT-OWNER                         PIC X(4).
000074     12  CO-BALANCE-CONTROL                    PIC X.
000075         88  CO-CARRY-BALANCE                     VALUE 'Y'.
000076         88  CO-NO-BALANCE                        VALUE 'N'.
000077
000078     12  CO-INTERNAL-CONTROL-1                 PIC X.
000079         88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
000080         88  CO-AUTO-GENERATED                    VALUE 'Y'.
000081         88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
000082
000083     12  CO-INTERNAL-CONTROL-2                 PIC X.
000084         88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
000085         88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
000086
000087     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
000088     12  CO-GA-DIRECT-DEP                      PIC X.
000089     12  CO-FUTURE-SPACE                       PIC X.
000090         88  CO-FUTURE-NOT-USED                   VALUE ' '.
000091
000092     12  CO-ACCT-NAME                          PIC X(30).
000093     12  CO-MAIL-NAME                          PIC X(30).
000094     12  CO-ADDR-1                             PIC X(30).
000095     12  CO-ADDR-2                             PIC X(30).
000096     12  CO-ADDR-3.
000097         16  CO-ADDR-CITY                      PIC X(27).
000098         16  CO-ADDR-STATE                     PIC XX.
000099     12  CO-CSO-1099                           PIC X.
000100     12  CO-ZIP.
000101         16  CO-ZIP-PRIME.
000102             20  CO-ZIP-PRI-1ST                PIC X.
000103                 88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
000104             20  FILLER                        PIC X(4).
000105         16  CO-ZIP-PLUS4                      PIC X(4).
000106     12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
000107         16  CO-CAN-POSTAL-1                   PIC XXX.
000108         16  CO-CAN-POSTAL-2                   PIC XXX.
000109         16  FILLER                            PIC XXX.
000110     12  CO-SOC-SEC                            PIC X(13).
000111     12  CO-TELEPHONE.
000112         16  CO-AREA-CODE                      PIC XXX.
000113         16  CO-PREFIX                         PIC XXX.
000114         16  CO-PHONE                          PIC X(4).
000115
000116     12  CO-ROLADEX-PRINT-DT                   PIC XX.
000117
000118     12  CO-AR-BAL-LEVEL                       PIC X.
000119         88  CO-AR-REF-LVL                        VALUE '1'.
000120         88  CO-AR-BILL-REF-LVL                   VALUE '1'.
000121         88  CO-AR-BILL-LVL                       VALUE '2'.
000122         88  CO-AR-AGT-LVL                        VALUE '3'.
000123         88  CO-AR-FR-LVL                         VALUE '4'.
000124
000125     12  CO-AR-NORMAL-PRINT                    PIC X.
000126         88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
000127         88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
000128
000129     12  CO-AR-SUMMARY-CODE                    PIC X(6).
000130
000131     12  CO-AR-REPORTING                       PIC X.
000132         88  CO-AR-NET-REPORT                     VALUE 'N'.
000133         88  CO-AR-GROSS-REPORT                   VALUE 'G'.
000134
000135     12  CO-AR-PULL-CHECK                      PIC X.
000136         88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
000137         88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
000138
000139     12  CO-AR-BALANCE-PRINT                   PIC X.
000140         88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
000141
000142     12  CO-AR-LAST-RUN-CODE                   PIC X.
000143         88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
000144         88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
000145         88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
000146
000147     12  CO-LAST-EOM-STMT-DT                   PIC XX.
000148
000149     12  CO-USER-CODE                          PIC X.
000150     12  CO-REPORT-GROUP-ID                    PIC X(12).
000151
000152******************************************************************
000153*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
000154*    THE LAST MONTH END RUN.
000155******************************************************************
000156
000157     12  CO-LAST-ACTIVITY-DATE.
000158         16  CO-ACT-YEAR                       PIC 99.
000159         16  CO-ACT-MONTH                      PIC 99.
000160         16  CO-ACT-DAY                        PIC 99.
000161
000162     12  CO-LAST-STMT-DT.
000163         16  CO-LAST-STMT-YEAR                 PIC 99.
000164         16  CO-LAST-STMT-MONTH                PIC 99.
000165         16  CO-LAST-STMT-DAY                  PIC 99.
000166
000167     12  CO-MO-END-TOTALS.
000168         16  CO-MONTHLY-TOTALS.
000169             20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
000170             20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
000171             20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
000172             20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
000173             20  CO-END-BAL                PIC S9(7)V99   COMP-3.
000174
000175         16  CO-AGING-TOTALS.
000176             20  CO-CUR                    PIC S9(7)V99   COMP-3.
000177             20  CO-OV30                   PIC S9(7)V99   COMP-3.
000178             20  CO-OV60                   PIC S9(7)V99   COMP-3.
000179             20  CO-OV90                   PIC S9(7)V99   COMP-3.
000180
000181         16  CO-YTD-TOTALS.
000182             20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
000183             20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
000184
000185         16  CO-OVER-UNDER-TOTALS.
000186             20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
000187             20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
000188
000189     12  CO-MISCELLANEOUS-TOTALS.
000190         16  CO-FICA-TOTALS.
000191             20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
000192             20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
000193
000194         16  CO-CLAIM-TOTALS.
000195             20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
000196             20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
000197
000198******************************************************************
000199*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
000200*    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
000201******************************************************************
000202
000203     12  CO-CURRENT-TOTALS.
000204         16  CO-CURRENT-LAST-STMT-DT.
000205             20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
000206             20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
000207             20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
000208
000209         16  CO-CURRENT-MONTHLY-TOTALS.
000210             20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
000211             20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
000212             20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
000213             20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
000214             20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
000215
000216         16  CO-CURRENT-AGING-TOTALS.
000217             20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
000218             20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
000219             20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
000220             20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
000221
000222         16  CO-CURRENT-YTD-TOTALS.
000223             20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
000224             20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
000225
000226     12  CO-PAID-COMM-TOTALS.
000227         16  CO-YTD-PAID-COMMS.
000228             20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
000229             20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
000230
000231     12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
000232         88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
000233         88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
000234
000235     12  CO-DELINQUENT-LETTER-CODE         PIC X.
000236         88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
000237         88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
000238         88  CO-AGENT-1ST-LETTER              VALUE 'B'.
000239         88  CO-AGENT-2ND-LETTER              VALUE 'G'.
000240         88  CO-OVERWRITE-LETTER              VALUE 'O'.
000241         88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
000242         88  CO-FINAL-LETTER                  VALUE 'F'.
000243         88  CO-RECONCILING                   VALUE 'R'.
000244         88  CO-PHONE-CALL                    VALUE 'P'.
000245         88  CO-LEGAL                         VALUE 'L'.
000246         88  CO-COLLECTION-AGENCY             VALUE 'C'.
000247         88  CO-WRITE-OFF                     VALUE 'W'.
000248         88  CO-NO-ACTION                     VALUE 'N' ' '.
000249
000250     12  CO-CSR-CODE                       PIC X(4).
000251
000252     12  CO-GA-STATUS-INFO.
000253         16  CO-GA-EFFECTIVE-DT            PIC XX.
000254         16  CO-GA-TERMINATION-DT          PIC XX.
000255         16  CO-GA-STATUS-CODE             PIC X.
000256             88  CO-GA-ACTIVE                 VALUE 'A'.
000257             88  CO-GA-INACTIVE               VALUE 'I'.
000258             88  CO-GA-PENDING                VALUE 'P'.
000259         16  CO-GA-COMMENTS.
000260             20  CO-GA-COMMENT-1           PIC X(40).
000261             20  CO-GA-COMMENT-2           PIC X(40).
000262             20  CO-GA-COMMENT-3           PIC X(40).
000263             20  CO-GA-COMMENT-4           PIC X(40).
000264
000265     12  CO-RPTCD2                         PIC X(10).
000266     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
000267         16  CO-OV120                      PIC S9(7)V99   COMP-3.
000268         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
000269
000270     12  CO-TYPE-AGENT                     PIC X(01).
000271         88  CO-CORPORATION                   VALUE 'C'.
000272         88  CO-PARTNERSHIP                   VALUE 'P'.
000273         88  CO-SOLE-PROPRIETOR               VALUE 'S'.
000274         88  CO-TRUST                         VALUE 'T'.
000275         88  CO-UNKNOWN                       VALUE ' ' 'X'.
000276
000277     12  CO-FAXNO.
000278         16  CO-FAX-AREA-CODE                  PIC XXX.
000279         16  CO-FAX-PREFIX                     PIC XXX.
000280         16  CO-FAX-PHONE                      PIC X(4).
000281
000282     12  CO-BANK-INFORMATION.
000283         16  CO-BANK-TRANSIT-NO                PIC X(8).
000284         16  CO-BANK-TRANSIT-NON REDEFINES
000285             CO-BANK-TRANSIT-NO                PIC 9(8).
000286
000287         16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
000288     12  CO-MISC-DEDUCT-INFO REDEFINES
000289                  CO-BANK-INFORMATION.
000290         16  CO-MD-GL-ACCT                     PIC X(10).
000291         16  CO-MD-DIV                         PIC XX.
000292         16  CO-MD-CENTER                      PIC X(4).
000293         16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
000294         16  CO-CREATE-AP-CHECK                PIC X.
000295         16  CO-DELIVER-CK-TO-MEL              PIC X.
000296         16  FILLER                            PIC XXX.
000297     12  CO-ACH-STATUS                         PIC X.
000298         88  CO-ACH-ACTIVE                         VALUE 'A'.
000299         88  CO-ACH-PENDING                        VALUE 'P'.
000300
000301     12  CO-BILL-SW                            PIC X.
000302     12  CO-CONTROL-NAME                       PIC X(30).
000303     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
000304     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
000305     12  CO-CLP-STATE                          PIC XX.
000306     12  CO-FIRST-WRITTEN-DT                   PIC XX.
000307     12  CO-SPP-REFUND-EDIT                    PIC X.
000308
000309******************************************************************
      *<<((file: ERCCOMP))
000997                                 EJECT
000998*                                COPY ELCTEXT.
      *>>((file: ELCTEXT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCTEXT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.008                          *
000007*                                                                *
000008*   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
000009*                                     FORM LETTERS,              *
000010*                                     CERT FORM DISPLAY.
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 100   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
000016*       ALTERNATE INDEX = NONE                                   *
000017*                                                                *
000018*   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
000019*       ALTERNATE INDEX = NONE                                   *
000020*                                                                *
000021*   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
000022*       ALTERNATE INDEX = NONE                                   *
000023*                                                                *
000024*   LOG = NO                                                     *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  TEXT-FILES.
000028     12  TEXT-FILE-ID                PIC XX.
000029         88  FORMS-FILE-TEXT            VALUE 'TF'.
000030         88  LETTER-FILE-TEXT           VALUE 'TL'.
000031         88  HELP-FILE-TEXT             VALUE 'TH'.
000032
000033     12  TX-CONTROL-PRIMARY.
000034         16  TX-COMPANY-CD           PIC X.
000035             88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
000036         16  TX-ACCESS-CD-GENL       PIC X(12).
000037
000038         16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000039             20  TX-LETTER-NO        PIC X(4).
000040             20  FILLER              PIC X(8).
000041
000042         16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000043             20  TX-FORM-NO          PIC X(12).
000044
000045         16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000046             20  TX-HELP-TYPE        PIC X.
000047                 88  HELP-FOR-GENERAL   VALUE ' '.
000048                 88  HELP-BY-SCREEN     VALUE 'S'.
000049                 88  HELP-BY-ERROR      VALUE 'E'.
000050             20  TX-SCREEN-OR-ERROR  PIC X(4).
000051                 88  GENERAL-INFO-HELP  VALUE '0000'.
000052             20  TX-HELP-FOR-COMPANY  PIC XXX.
000053                 88  NOT-COMPANY-SPECIFIC VALUE '   '.
000054             20  FILLER              PIC X(4).
000055
000056         16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
000057
000058     12  TX-PROCESS-CONTROL          PIC XX.
000059         88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
000060
000061     12  TX-TEXT-LINE                PIC X(70).
000062
000063     12  TX-FORM-SQUEEZE-CONTROL     PIC X.
000064         88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
000065         88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
000066         88  TX-VALID-FORM-SQUEEZE-VALUE
000067                                        VALUE 'Y' ' '.
000068
000069     12  TX-LINE-SQUEEZE-CONTROL     PIC X.
000070         88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
000071         88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
000072         88  TX-DO-NOT-ADJUST           VALUE 'N'.
000073         88  TX-FORM-CONTROL-LINE       VALUE 'K'.
000074         88  TX-NEW-PARAGRAPH           VALUE 'P'.
000075         88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
000076         88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
000077                                              'K' 'N' ' '
000078                                              'Z'.
000079
000080     12  TX-ARCHIVE-SW               PIC X.
000081         88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
000082         88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
000083         88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
000084
000085     12  TX-LAST-MAINTENANCED-BY     PIC X(4).
000086     12  TX-LAST-MAINTENANCED-DT     PIC X(2).
000087
000088     12  TX-BSR-CODE                 PIC X.
000089         88  TX-BSR-LETTER              VALUE 'B'.
000090         88  TX-NON-BSR-LETTER          VALUE ' '.
000091
000092     12  FILLER                      PIC X.
000093*****************************************************************
      *<<((file: ELCTEXT))
000999                                 EJECT
001000*                                COPY MPCPROD.
      *>>((file: MPCPROD))
000001******************************************************************
000002*                                                                *
000003*                            MPCPROD                             *
000004*                            VMOD=1.010                          *
000005*                                                                *
000006*   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
000007*                                                                *
000008*   THIS COPYBOOK IS USED FOR THE ONLINE                         *
000009*   VSAM PRODUCER MASTER FILE.                                   *
000010*                                                                *
000011*   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
000012*                                                                *
000013*   FILE TYPE = VSAM,KSDS                                        *
000014*   RECORD SIZE = 2000 RECFORM = FIXED                           *
000015*                                                                *
000016*   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
000017*       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
000018*       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
000019*                                                                *
000020*   LOG = NO                                                     *
000021*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000022*                                                                *
000023*                                                                *
000024******************************************************************
000025
000026 01  PRODUCER-MASTER.
000027     12  PD-RECORD-ID                 PIC  X(02).
000028         88  PD-VALID-ID                   VALUE 'PD'.
000029
000030******************************************************************
000031*   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
000032******************************************************************
000033
000034     12  PD-CONTROL-PRIMARY-BATCH.
000035         16  FILLER                   PIC  X(20).
000036         16  PD-EXPIRE-DT.
000037             20  PD-EXPIRE-DT-YY      PIC  9(02).
000038             20  PD-EXPIRE-DT-MM      PIC  9(02).
000039             20  PD-EXPIRE-DT-DD      PIC  9(02).
000040     12  FILLER REDEFINES PD-CONTROL-PRIMARY-BATCH.
000041         16  PD-CONTROL-PRIMARY.
000042             20  PD-COMPANY-CD        PIC  X(01).
000043             20  PD-MSTR-CNTRL.
000044                 24  PD-CONTROL-A.
000045                     28  PD-CARRIER   PIC  X(01).
000046                     28  PD-GROUPING.
000047                         32 PD-GROUPING-PREFIX
000048                                      PIC  X(03).
000049                         32 PD-GROUPING-PRIME
000050                                      PIC  X(03).
000051                     28  PD-STATE     PIC  X(02).
000052                     28  PD-PRODUCER.
000053                         32  PD-PRODUCER-PREFIX
000054                                      PIC  X(04).
000055                         32  PD-PRODUCER-PRIME
000056                                      PIC  X(06).
000057                 24  PD-CNTRL-B.
000058                     28  PD-EXPIRE-DATE
000059                                      PIC  X(02).
000060         16  FILLER REDEFINES PD-CONTROL-PRIMARY.
000061             20  FILLER               PIC  X(01).
000062             20  PD-CGSPE-KEY         PIC  X(21).
000063         16  FILLER                   PIC  X(04).
000064     12  FILLER                       PIC  X(20).
000065
000066******************************************************************
000067*      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
000068******************************************************************
000069
000070     12  PD-CONTROL-BY-VAR-GRP.
000071         16  PD-VG-CCGSP-KEYLET.
000072             20  PD-COMPANY-CD-A1     PIC  X(01).
000073             20  PD-VG-CARRIER        PIC  X(01).
000074             20  PD-VG-GROUPING       PIC  X(06).
000075             20  PD-VG-STATE          PIC  X(02).
000076             20  PD-VG-PRODUCER       PIC  X(10).
000077         16  PD-VG-DATE.
000078             24  PD-VG-EXPIRE-DATE    PIC  X(02).
000079     12  FILLER                       PIC  X(20).
000080
000081
000082******************************************************************
000083*      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
000084******************************************************************
000085
000086     12  PD-CONTROL-BY-NAME.
000087         16  PD-COMPANY-CD-A2         PIC  X(01).
000088         16  PD-NAME-A2               PIC  X(30).
000089         16  PD-CGSPE-KEY-A2.
000090             20  PD-CARRIER-A2        PIC  X(01).
000091             20  PD-GROUPING-A2       PIC  X(06).
000092             20  PD-STATE-A2          PIC  X(02).
000093             20  PD-PRODUCER-A2       PIC  X(10).
000094             20  PD-EXPIRE-DATE-A2    PIC  X(02).
000095         16  PD-CURRENT-DATE-BIN-A2   PIC  X(02).
000096         16  PD-CURRENT-TIME-BIN-A2   PIC S9(04) COMP.
000097     12  FILLER                       PIC  X(20).
000098
000099******************************************************************
000100*                FILE SYNCHRONIZATION DATA                       *
000101******************************************************************
000102
000103     12  PD-MAINT-INFORMATION.
000104         16  PD-LAST-MAINT-DATE       PIC  X(02).
000105         16  PD-LAST-MAINT-HHMMSS     PIC S9(07) COMP-3.
000106         16  PD-LAST-MAINT-USER       PIC  X(04).
000107
000108******************************************************************
000109*                PRODUCER SECURITY DATA                          *
000110******************************************************************
000111
000112     12  PD-SECURITY-ACCESS-CODE      PIC  X(01).
000113
000114******************************************************************
000115*                DATES                                           *
000116******************************************************************
000117
000118     12  PD-ANNIVERSARY-DATE          PIC  X(02).
000119
000120     12  PD-AR-HI-DATE.
000121         16  PD-AR-HI-POLICY-DATE     PIC  X(02).
000122         16  FILLER                   PIC  X(04).
000123     12  PD-AR-HI-POLICY-DT REDEFINES PD-AR-HI-DATE.
000124         16  PD-AR-HI-POLICY-DT-YY    PIC  9(02).
000125         16  PD-AR-HI-POLICY-DT-MM    PIC  9(02).
000126         16  PD-AR-HI-POLICY-DT-DD    PIC  9(02).
000127
000128     12  PD-ENTRY-DATE                PIC  X(02).
000129
000130     12  PD-EFFECT-DTE.
000131         16  PD-EFFECT-DATE           PIC  X(02).
000132         16  FILLER                   PIC  X(04).
000133     12  PD-EFFECT-DT REDEFINES PD-EFFECT-DTE.
000134         16  PD-EFFECT-DT-YY          PIC  9(02).
000135         16  PD-EFFECT-DT-MM          PIC  9(02).
000136         16  PD-EFFECT-DT-DD          PIC  9(02).
000137
000138     12  PD-HI-DATE.
000139         16  PD-HI-POLICY-DATE        PIC  X(02).
000140         16  FILLER                   PIC  X(04).
000141     12  PD-HI-POLICY-DT REDEFINES PD-HI-DATE.
000142         16  PD-HI-POLICY-DT-YY       PIC  9(02).
000143         16  PD-HI-POLICY-DT-MM       PIC  9(02).
000144         16  PD-HI-POLICY-DT-DD       PIC  9(02).
000145
000146     12  PD-INACTIVE-DATE             PIC  X(02).
000147
000148     12  PD-LO-DATE.
000149         16  PD-LO-POLICY-DATE        PIC  X(02).
000150         16  FILLER                   PIC  X(04).
000151     12  PD-LO-POLICY-DT REDEFINES PD-LO-DATE.
000152         16  PD-LO-POLICY-DT-YY       PIC  9(02).
000153         16  PD-LO-POLICY-DT-MM       PIC  9(02).
000154         16  PD-LO-POLICY-DT-DD       PIC  9(02).
000155
000156     12  PD-POLICIES-PURGED-DATE      PIC  X(02).
000157
000158     12  PD-PREV-DATES.
000159         16  PD-PREV-EFF-DATE         PIC  X(02).
000160         16  FILLER                   PIC  X(04).
000161         16  PD-PREV-EXP-DATE         PIC  X(02).
000162         16  FILLER                   PIC  X(04).
000163     12  PD-PREV-DTS REDEFINES PD-PREV-DATES.
000164         16  PD-PREV-EFF-DT.
000165             20  PD-PREV-EFF-DT-YY    PIC  9(02).
000166             20  PD-PREV-EFF-DT-MM    PIC  9(02).
000167             20  PD-PREV-EFF-DT-DD    PIC  9(02).
000168         16  PD-PREV-EXP-DT.
000169             20  PD-PREV-EXP-DT-YY    PIC  9(02).
000170             20  PD-PREV-EXP-DT-MM    PIC  9(02).
000171             20  PD-PREV-EXP-DT-DD    PIC  9(02).
000172
000173     12  PD-1ST-PROD-DATE             PIC  X(02).
000174
000175     12  FILLER                       PIC  X(20).
000176
000177******************************************************************
000178*                MORTGAGE BILLING DATA                           *
000179******************************************************************
000180
000181     12  PD-CONTACT                   PIC  X(30).
000182     12  PD-BILLING-MONTHS.
000183         16  PD-BILLING-MONTH-ANNUAL  PIC  9(02).
000184         16  PD-BILLING-MONTH-SEMIANN PIC  9(02).
000185     12  PD-BILLING-ADVANCE-ARREARS   PIC  X(01).
000186         88  PD-BILL-ADVANCE              VALUE '1'.
000187         88  PD-BILL-ARREARS              VALUE '2'.
000188     12  PD-BILLING-MODE              PIC  X(01).
000189         88  PD-ANNUAL-BILL               VALUE '1'.
000190         88  PD-SEMI-ANNUAL-BILL          VALUE '2'.
000191         88  PD-QUARTERLY-BILL            VALUE '3'.
000192         88  PD-MONTHLY-BILL              VALUE '4'.
000193         88  PD-BI-MONTHLY-BILL           VALUE '5'.
000194         88  PD-SINGLE-PREM-BILL          VALUE '6'.
000195     12  PD-BILLING-GROUPING-CODE     PIC  X(06).
000196     12  PD-BILLING-SCHEDULE          PIC  X(01).
000197         88  PD-BILL-1ST-WEEK             VALUE '1'.
000198         88  PD-BILL-2ND-WEEK             VALUE '2'.
000199         88  PD-BILL-3RD-WEEK             VALUE '3'.
000200         88  PD-BILL-4TH-WEEK             VALUE '4'.
000201         88  PD-BILL-5TH-WEEK             VALUE '5'.
000202         88  PD-HOLD-BILL                 VALUE '6'.
000203         88  PD-NO-BILL                   VALUE '7'.
000204     12  PD-BILLING-SEQUENCE          PIC  X(01).
000205         88  PD-BILL-NAME-SEQU            VALUE '1'.
000206         88  PD-BILL-LOAN-SEQU            VALUE '2'.
000207         88  PD-BILL-PLCY-SEQU            VALUE '3'.
000208     12  PD-BILLING-TYPE              PIC  X(01).
000209         88  PD-LIST-BILL                 VALUE '1'.
000210         88  PD-TAPE-BILL                 VALUE '2'.
000211         88  PD-TAPE-LIST-BILL            VALUE '3'.
000212         88  PD-GROUP-BILL            VALUES ARE '1' '2' '3'.
000213         88  PD-DIRECT-BILL               VALUE '4'.
000214         88  PD-PAC                   VALUES ARE '5' 'C' 'S'.
000215         88  PD-CREDIT-CARD               VALUE '6'.
000216         88  PD-INDIV-BILL
000217                              VALUES ARE '4' '5' '6' 'C' 'S'.
000218         88  PD-GROUP-BY-POLICY           VALUE '7'.
000219         88  PD-GROUP-BY-POLICY-PAC       VALUE '8'.
000220         88  PD-GROUP-BY-POLICY-CRDC      VALUE '9'.
000221         88  PD-GROUP-BY-BILL             VALUE '7' '8' '9'.
000222         88  PD-GROUP-BY-PROD             VALUE 'A'.
000223         88  PD-EFT-CHECKING              VALUE 'C'.
000224         88  PD-EFT-SAVINGS               VALUE 'S'.
000225     12  PD-DATE-PAID                 PIC  X(02).
000226     12  PD-LAST-BILLING-DATE         PIC  X(02).
000227     12  PD-LAST-BILL-TO-DATE         PIC  X(02).
000228     12  PD-MAX-MONTHS-BILL           PIC S9(03)  COMP-3.
000229     12  PD-PAID-TO-DATE              PIC  X(02).
000230     12  PD-PREV-BILLING-DATE         PIC  X(02).
000231     12  PD-PREV-BILL-TO-DATE         PIC  X(02).
000232
000233     12  FILLER                       PIC  X(20).
000234
000235******************************************************************
000236*                PERSONAL DATA                                   *
000237******************************************************************
000238
000239     12  PD-ADDRS                     PIC  X(30).
000240     12  PD-CITY                      PIC  X(30).
000241     12  PD-CITY-CODE                 PIC  X(04).
000242     12  PD-COUNTY-CODE               PIC  X(03).
000243     12  PD-NAME                      PIC  X(30).
000244     12  PD-PARRISH-CODE              PIC  X(03).
000245     12  PD-PERSON                    PIC  X(30).
000246     12  PD-TEL-NO.
000247         16  PD-AREA-CODE             PIC  9(03).
000248         16  PD-TEL-PRE               PIC  9(03).
000249         16  PD-TEL-NBR               PIC  9(04).
000250     12  PD-ZIP.
000251         16  PD-ZIP-PRIME             PIC  X(05).
000252         16  PD-ZIP-PLUS4             PIC  X(04).
000253     12  PD-LANGUAGE-IND              PIC  X(01).
000254         88  PD-ENGLISH                          VALUE 'E'.
000255         88  PD-FRENCH                           VALUE 'F'.
000256         88  PD-SPANISH                          VALUE 'S'.
000257
000258     12  FILLER                       PIC  X(19).
000259
000260******************************************************************
000261*                REINSURANCE DATA                                *
000262******************************************************************
000263
000264     12  PD-REINS-TBL-CODE            PIC  X(03).
000265     12  PD-REIN-RECALC               PIC  X(01).
000266
000267     12  PD-REI-AH-FEE                PIC S9(01)V9(04) COMP-3.
000268     12  PD-REI-AH-PE                 PIC  X(01).
000269     12  PD-REI-AH-TAX                PIC S9(01)V9(04) COMP-3.
000270
000271     12  PD-REI-GROUP-A               PIC  X(06).
000272     12  PD-REI-GROUP-B               PIC  X(06).
000273
000274     12  PD-REI-LF-FEE                PIC S9(01)V9(04) COMP-3.
000275     12  PD-REI-LF-PE                 PIC  X(01).
000276     12  PD-REI-LF-TAX                PIC S9(01)V9(04) COMP-3.
000277
000278     12  PD-REI-MORT                  PIC  X(04).
000279     12  PD-REI-PRT-OW                PIC  X(01).
000280     12  PD-REI-PRT-ST                PIC  X(01).
000281
000282     12  PD-REI-ADD-FEE               PIC S9(01)V9(04) COMP-3.
000283     12  PD-REI-ADD-PE                PIC  X(01).
000284     12  PD-REI-ADD-TAX               PIC S9(01)V9(04) COMP-3.
000285
000286     12  PD-REI-DIS-FEE               PIC S9(01)V9(04) COMP-3.
000287     12  PD-REI-DIS-PE                PIC  X(01).
000288     12  PD-REI-DIS-TAX               PIC S9(01)V9(04) COMP-3.
000289
000290     12  FILLER                       PIC  X(10).
000291******************************************************************
000292*                RETRO DATA                                      *
000293******************************************************************
000294
000295     12  PD-RET-AH                    PIC S9(01)V9(04) COMP-3.
000296     12  PD-RET-GRP                   PIC  X(06).
000297     12  PD-RET-LF                    PIC S9(01)V9(04) COMP-3.
000298     12  PD-RET-MIN-LOSS-A            PIC SV9(03)      COMP-3.
000299     12  PD-RET-MIN-LOSS-L            PIC SV9(03)      COMP-3.
000300     12  PD-RET-P-E                   PIC  X(01).
000301     12  PD-RET-ST-TAX-USE            PIC  X(01).
000302         88  PD-CHARGE-ST-TAXES-ON-RETRO      VALUE 'Y' 'E' 'P'.
000303         88  PD-TAXES-NOT-IN-RETRO            VALUE 'N' ' '.
000304     12  PD-RET-Y-N                   PIC  X(01).
000305     12  PD-RET-ADD                   PIC S9(01)V9(04) COMP-3.
000306     12  PD-RET-MIN-LOSS-ADD          PIC SV9(03)      COMP-3.
000307     12  PD-RET-DIS                   PIC S9(01)V9(04) COMP-3.
000308     12  PD-RET-MIN-LOSS-DIS          PIC SV9(03)      COMP-3.
000309
000310     12  FILLER                       PIC  X(10).
000311
000312******************************************************************
000313*                     MANAGEMENT OPTIONS                         *
000314******************************************************************
000315
000316     12  PD-DEFAULT-UNWTR-CODE        PIC  X(03).
000317     12  PD-LAPSE-NOTICE-CNTL         PIC  X(01).
000318     12  PD-CORRESPONDENCE-CNTL       PIC  X(01).
000319     12  PD-RETAIN-BILLING-DATA-MTHS  PIC S9(03)  COMP-3.
000320     12  PD-RETAIN-CLAIM-DATA-MTHS    PIC S9(03)  COMP-3.
000321     12  PD-RETAIN-COMMISSION-MTHS    PIC S9(03)  COMP-3.
000322     12  PD-RETAIN-DELINQUENCY-MTHS   PIC S9(03)  COMP-3.
000323     12  PD-RETAIN-INSD-PROFILE-MTHS  PIC S9(03)  COMP-3.
000324     12  PD-RETAIN-INS-COVERAGE-MTHS  PIC S9(03)  COMP-3.
000325     12  PD-RETAIN-STATUS-DISP-MTHS   PIC S9(03)  COMP-3.
000326     12  PD-NUM-BILLING-CYCLES-RETAINED
000327                                      PIC S9(03)  COMP-3.
000328     12  PD-RETAIN-UNDERWRITER-HST-MTHS
000329                                      PIC S9(03)  COMP-3.
000330
000331     12  FILLER                       PIC X(098).
000332
000333
000334******************************************************************
000335*                MISCELLANEOUS DATA                              *
000336******************************************************************
000337
000338     12  PD-AH-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
000339     12  PD-AUTO-REFUND-SW            PIC  X(01).
000340         88  PD-AUTO-REFUNDS-USED             VALUE 'Y'.
000341         88  PD-AUTO-REFUNDS-NOT-USED         VALUE 'N' ' '.
000342     12  PD-BUSINESS-TYPE             PIC  9(02).
000343     12  PD-CAL-TABLE                 PIC  X(02).
000344     12  PD-COMMENTS.
000345         16  PD-COMMENT-LINE          PIC  X(50)
000346                                           OCCURS 5 TIMES.
000347     12  PD-EMPLOYER-STMT-USED        PIC  X(01).
000348     12  PD-GROUPED-CHECKS-Y-N        PIC  X(01).
000349     12  PD-IG                        PIC  X(01).
000350         88  PD-HAS-INDIVIDUAL                VALUE 'I'
000351                                                    '1'.
000352         88  PD-HAS-GROUP                     VALUE 'G'
000353                                                    '2'.
000354     12  PD-LF-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
000355     12  PD-REPORT-CODE-1             PIC  X(10).
000356     12  PD-REPORT-CODE-2             PIC  X(10).
000357     12  PD-RPT045A-SWITCH            PIC  X(01).
000358         88  PD-RPT045A-OFF                VALUE 'N'.
000359     12  PD-SPECIAL-BILLING-FREQ      PIC  X(01).
000360         88  PD-HAS-SPECIAL-BILL-FREQ         VALUE 'Y'.
000361         88  PD-NO-SPECIAL-BILL-FREQ          VALUE 'N' ' '.
000362     12  PD-STATUS                    PIC  X(01).
000363         88  PD-STATUS-ACTIVE                 VALUE '0'.
000364         88  PD-STATUS-INACTIVE               VALUE '1'.
000365     12  PD-STD-AH-TYPE               PIC  X(02).
000366     12  PD-TAX-NUMBER                PIC  X(11).
000367     12  PD-TOL-CLM                   PIC S9(03)V9(02) COMP-3.
000368     12  PD-USER-FIELDS.
000369         16  PD-USER-FLD-1            PIC  X(02).
000370         16  PD-USER-FLD-2            PIC  X(02).
000371         16  PD-USER-FLD-3            PIC  X(02).
000372         16  PD-USER-FLD-4            PIC  X(02).
000373         16  PD-USER-FLD-5            PIC  X(02).
000374     12  PD-USER-SELECT-OPTIONS.
000375         16  PD-USER-SELECT-1         PIC  X(10).
000376         16  PD-USER-SELECT-2         PIC  X(10).
000377         16  PD-USER-SELECT-3         PIC  X(10).
000378         16  PD-USER-SELECT-4         PIC  X(10).
000379         16  PD-USER-SELECT-5         PIC  X(10).
000380     12  PD-DIS-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
000381     12  PD-ADD-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
000382     12  FILLER                       PIC  X(20).
000383
000384******************************************************************
000385*                CLIENT USE AREAS                                *
000386******************************************************************
000387
000388     12  PD-CLIENT-USE-AREA-1         PIC  X(30).
000389     12  PD-CLIENT-USE-AREA-2         PIC  X(30).
000390     12  PD-CLIENT-USE-AREA-3         PIC  X(11).
000391     12  PD-CLIENT-USE-AREA-4         PIC  X(30).
000392     12  PD-CLIENT-USE-AREA-5         PIC  X(30).
000393     12  PD-CLIENT-USE-AREA-6         PIC  X(11).
000394     12  PD-CLIENT-USE-AREA-7         PIC  X(30).
000395     12  PD-CLIENT-USE-AREA-8         PIC  X(30).
000396     12  PD-CLIENT-USE-AREA-9         PIC  X(11).
000397
000398******************************************************************
000399*                TRANSFER DATA                                   *
000400******************************************************************
000401     12  PD-TRANSFERRED-FROM.
000402         16  PD-TRNFROM-CARRIER       PIC  X(01).
000403         16  PD-TRNFROM-GROUPING.
000404             20  PD-TRNFROM-GRP-PREFIX
000405                                      PIC  X(03).
000406             20  PD-TRNFROM-GRP-PRIME PIC  X(03).
000407         16  PD-TRNFROM-STATE         PIC  X(02).
000408         16  PD-TRNFROM-PRODUCER.
000409             20  PD-TRNFROM-PROD-PREFIX
000410                                      PIC  X(04).
000411             20  PD-TRNFROM-PROD-PRIME
000412                                      PIC  X(06).
000413         16  PD-TRNFROM-DATE          PIC  X(02).
000414     12  PD-TRANSFERRED-TO.
000415         16  PD-TRNTO-CARRIER         PIC  X(01).
000416         16  PD-TRNTO-GROUPING.
000417             20  PD-TRNTO-GRP-PREFIX  PIC  X(03).
000418             20  PD-TRNTO-GRP-PRIME   PIC  X(03).
000419         16  PD-TRNTO-STATE           PIC  X(02).
000420         16  PD-TRNTO-PRODUCER.
000421             20  PD-TRNTO-PROD-PREFIX PIC  X(04).
000422             20  PD-TRNTO-PROD-PRIME  PIC  X(06).
000423         16  PD-TRNTO-DATE            PIC  X(02).
000424     12  FILLER                       PIC  X(20).
000425
000426******************************************************************
000427*                MORTGAGE PLANS SOLD                             *
000428******************************************************************
000429
000430     12  PD-PLANS-SOLD.
000431         16  PD-PRODUCER-PLANS  OCCURS 40 TIMES
000432                                INDEXED BY PD-PLAN-NDX
000433                                           PD-PLAN-NDX2.
000434             20  PD-INDIVIDUAL-PLAN.
000435                 24  PD-PLAN-CODE     PIC  X(02).
000436                 24  PD-PLAN-REVISION PIC  X(03).
000437             20  PD-IBNR-PERCENT      PIC S9(01)V9(04) COMP-3.
000438     12  FILLER                       PIC  X(54).
000439
000440******************************************************************
000441*                 AGENT AND COMMISSION DATA                      *
000442******************************************************************
000443
000444     12  PD-COMMISSION-INFORMATION.
000445         16  PD-REMIT-TO              PIC S9(03)   COMP-3.
000446         16  PD-RECALCULATION-SW      PIC  X(01).
000447             88  PD-RECALC-DETAIL             VALUE 'Y'.
000448             88  PD-RECALC-NO-DETAIL          VALUE 'I'.
000449             88  PD-IGNORE-RECALC             VALUE 'N'.
000450             88  PD-VALID-RECALCULATION-SW    VALUE 'Y' 'I' 'N'.
000451         16  PD-AGENT-DATA.
000452             20  PD-AGENT-ENTRY       OCCURS 5 TIMES
000453                                    INDEXED BY PD-AGENT-NDX
000454                                               PD-AGENT-NDX2.
000455                 24  PD-AGENT-NUMBER  PIC  X(10).
000456                 24  PD-AGENT-TYPE    PIC  X(01).
000457                     88  PD-AGENT-TYPE-A      VALUE 'C' 'D'.
000458                     88  PD-AGENT-TYPE-G      VALUE 'O' 'R'
000459                                                    'P' 'T'
000460                                                    'W'.
000461                     88  PD-AGENT-GROSS       VALUE 'C'.
000462                     88  PD-AGENT-REINS       VALUE 'R'.
000463                     88  PD-AGENT-GROSS-REINS VALUE 'D'.
000464                     88  PD-OVERWRITE-GROSS   VALUE 'O'.
000465                     88  PD-OVERWRITE-GROSS-REINS
000466                                          VALUE 'P'.
000467                     88  PD-OVERWRITE-REINS   VALUE 'T'.
000468                     88  PD-REINS-ONLY        VALUE 'W'.
000469                     88  PD-VALID-AGENT-TYPE  VALUE 'C' 'R'
000470                                                'D' 'O' 'P'
000471                                                'T' 'W'.
000472                 24  PD-COMMISSION-BILLED-PAID
000473                                      PIC  X(01).
000474                     88  PD-AGENT-BILLED      VALUE 'B'.
000475                     88  PD-AGENT-PAID        VALUE 'P'.
000476                 24  PD-COMP-RECALC-FLAG
000477                                      PIC  X(01).
000478                     88  PD-BYPASS-RECALC     VALUE 'N'.
000479                     88  PD-VALID-RECALC-FLAG VALUE ' ' 'N'.
000480     12  FILLER                       PIC  X(55).
000481
000482******************************************************************
000483*                BANK DATA                                       *
000484******************************************************************
000485
000486     12  PD-BANK-ACCOUNT-NUMBER       PIC  X(20).
000487     12  PD-BANK-TRANSIT-NUMBER.
000488         16  PD-FEDERAL-NUMBER        PIC  X(04).
000489         16  PD-BANK-NUMBER           PIC  X(04).
000490     12  PD-CHARGE-CARD-EXP-DT        PIC  X(02).
000491     12  PD-CHARGE-CARD-TYPE          PIC  X(02).
000492         88  PD-AMERICAN-EXPRESS                 VALUE 'AE'.
000493         88  PD-CARTE-BLANCHE                    VALUE 'CB'.
000494         88  PD-DINERS-CLUB                      VALUE 'DN'.
000495         88  PD-DISCOVER                         VALUE 'DS'.
000496         88  PD-MASTER-CARD                      VALUE 'MC'.
000497         88  PD-VISA                             VALUE 'VI'.
000498     12  PD-SIGNATURE-NAME            PIC  X(25).
000499     12  PD-AUTHORIZATION-SW          PIC  X(01).
000500******************************************************************
000501*                GENERIC FILLER                                  *
000502******************************************************************
000503
000504     12  PD-DATE-TEST                 PIC S9(08) COMP.
000505     12  FILLER                       PIC  X(62).
000506
000507******************************************************************
      *<<((file: MPCPROD))
001001                                 EJECT
001002*                                COPY MPCPLCY.
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
001003                                 EJECT
001004*                                COPY MPCPLAN.
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
001005                                 EJECT
001006*                                COPY ELCNAPS.
      *>>((file: ELCNAPS))
000001******************************************************************
000002*                                                                *
000003*                            ELCNAPS                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*        FILE DESCRIPTION = NAPERSOFT LETTER FILE                *
000008*                                                                *
000009*        FILE TYPE= VSAM,KSDS                                    *
000010*        RECORD SIZE = 150    RECFORM = FIXED                    *
000011*                                                                *
000012*        BASE CLUSTER = ELNAPS        RKP=2,LEN=28               *
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
000025* 033110  CR2009122800001  AJRA  NEW FILE FOR NAPERSOFT.
000026******************************************************************
000027
000028 01  NAPERSOFT-FILE.
000029     12  NA-RECORD-ID                PIC  XX.
000030         88  VALID-NA-ID                  VALUE 'NA'.
000031
000032     12  NA-CONTROL-PRIMARY.
000033         16  NA-COMPANY-CD           PIC X.
000034         16  NA-CARRIER              PIC X.
000035         16  NA-CLAIM-NO             PIC X(7).
000036         16  NA-CERT-NO.
000037             20  NA-CERT-PRIME       PIC X(10).
000038             20  NA-CERT-SFX         PIC X.
000039         16  NA-ARCHIVE-NO           PIC 9(8).
000040
000041     12  NA-LETTER-INFORMATION.
000042         16  NA-LETTER-ID            PIC X(4).
000043         16  NA-PROCESSOR-ID         PIC X(4).
000044         16  NA-CREATION-DT          PIC X(2).
000045         16  NA-INITIAL-PRINT-DT     PIC X(2).
000046         16  NA-FOLLOW-UP-DT         PIC X(2).
000047         16  NA-RESEND-DT            PIC X(2).
000048         16  NA-RESEND-LETTER-ID     PIC X(4).
000049         16  NA-NO-OF-COPIES         PIC 9(2).
000050         16  NA-ADDRESS-TYPE         PIC X(2).
000051         16  NA-CORR-TRLR-SEQ        PIC 9(4).
000052         16  NA-RESEND-PRINT-DT      PIC X(2).
000053         16  NA-1ST-LTR-PRINT-DT     PIC X(2).
000054         16  NA-NEXT-DUE-DT          PIC X(2).
000055         16  NA-AUTOPYDT             PIC X(2).
000056         16  NA-ENCLOSURE-CD         PIC X(3).
000057         16  NA-CREATED-IN-NAPERSOFT PIC X(1).
000058         16  NA-ORIG-ARCHIVE-NO      PIC 9(9).
000059         16  NA-RESEND-PROMPT-IND    PIC X(1).
000060         16  NA-ORIG-ENCLOSURE-CD    PIC X(3).
000061         16  FILLER                  PIC X(67).
000062******************************************************************
      *<<((file: ELCNAPS))
001007                                 EJECT
001008*  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
001009*  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA
001010
001011 01  SYSTEM-VARIABLES            PIC  X(3340).
001012 01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
001013     12  SYS-VAR-CODE            PIC  X(04).
001014     12  SYS-VAR-LEN             PIC  9(02).
001015     12  SYS-VAR-DATA            PIC  X(100).
001016
001017                                 EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1523' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
001018 VCOBOL-DUMMY-PROCEDURE.
001019
001020*    SERVICE RELOAD PARMLIST.
001021
001022     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
001023
001024     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
001025     MOVE '5'                    TO DC-OPTION-CODE.
001026     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
001027     MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
001028     MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.
001029
001030     MOVE W-SAVE-BIN-DATE        TO W-CURRENT-SAVE.
001031
001032     
      * EXEC CICS HANDLE CONDITION
001033*        PGMIDERR (9600-PGMID-ERROR)
001034*        ERROR    (9990-ABEND)
001035*    END-EXEC.
      *    MOVE '"$L.                  ! " #00006724' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303036373234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001036
001037
001038     MOVE ZEROS                  TO PI-ERROR-CODE
001039                                    W-PI-ADDR-SEQ
001040                                    W-ADDR-SEQ
001041                                    W-CURRENT-LINE
001042                                    W-TOTAL-LINES.
001043
001044     
      * EXEC CICS SYNCPOINT
001045*    END-EXEC.
      *    MOVE '6"                    !   #00006736' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303036373336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001046                                 EJECT
001047 0100-PROCESS-REQUEST.
001048
001049     PERFORM 1000-EDIT-ROUTINE THRU 1000-EXIT.
001050
001051     IF  PI-FATAL-ERROR
001052         GO TO 0200-RETURN-TO-CALLING-PGM.
001053
001054     PERFORM 2000-CREATE-LETTER THRU 2999-EXIT.
001055
001056     IF  PI-FATAL-ERROR
001057         GO TO 0200-RETURN-TO-CALLING-PGM.
001058
001059     PERFORM 6000-ARCHIVE-LETTER THRU 6000-EXIT.
001060
001061 0100-EXIT.
001062     EXIT.
001063                                 EJECT
001064 0200-RETURN-TO-CALLING-PGM.
001065
001066     IF  PI-FATAL-ERROR
001067         
      * EXEC CICS SYNCPOINT ROLLBACK
001068*        END-EXEC.
      *    MOVE '6"R                   !   #00006759' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303036373539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001069
001070     MOVE PROGRAM-INTERFACE-BLOCK
001071                                 TO DFHCOMMAREA.
001072
001073     
      * EXEC CICS RETURN
001074*    END-EXEC.
      *    MOVE '.(                    ''   #00006765' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036373635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001075
001076 0200-EXIT.
001077     EXIT.
001078                                 EJECT
001079 1000-EDIT-ROUTINE.
001080
001081     PERFORM 1100-SET-CODES THRU 1100-EXIT.
001082
001083     IF  PI-RESEND-DATE EQUAL SPACES
001084         MOVE LOW-VALUES         TO PI-RESEND-DATE.
001085
001086     IF  PI-FOLLOW-UP-DATE   EQUAL SPACES
001087         MOVE LOW-VALUES         TO PI-FOLLOW-UP-DATE.
001088
001089     IF  PI-ADDR-TYPE GREATER THAN LOW-VALUES
001090         MOVE PI-ADDR-TYPE       TO W-ADDR-TYPE-CD
001091
001092         IF  PI-ADDR-TYPE EQUAL 'I' OR 'B' OR 'A' OR 'P'
001093                               OR 'O' OR 'Q' OR 'E'
001094             NEXT SENTENCE
001095
001096         ELSE
001097             MOVE ER-0176        TO PI-ERROR-CODE
001098             PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001099
001100 1000-EXIT.
001101      EXIT.
001102                                 EJECT
001103 1100-SET-CODES.
001104
001105     MOVE PI-COMPANY-ID          TO W-CNTL-CO.
001106
001107     MOVE PI-COMPANY-CD          TO W-CLM-CO
001108                                    W-TEXT-CO
001109                                    W-ACTV-CO
001110                                    W-CERT-CO
001111                                    W-BENE-COMP-CD
001112                                    W-COMP-COMPANY-CD
001113                                    W-ACCT-CO
001114                                    W-ARCH-CO
001115                                    W-PROD-CO
001116                                    W-PLCY-CO
001117                                    W-PLAN-CO.
001118
001119     MOVE PI-CARRIER             TO W-CLM-CARRIER
001120                                    W-ACTV-CARRIER
001121                                    W-CERT-CARRIER
001122                                    W-ACCT-CARRIER
001123                                    W-PROD-CARRIER
001124                                    W-PLCY-CARRIER
001125                                    W-PLAN-CARRIER.
001126
001127     MOVE PI-CERT-NO             TO W-CLM-CERT-NUM
001128                                    W-ACTV-CERT-NUM
001129                                    W-CERT-CERT-NUM.
001130
001131     MOVE PI-CLAIM-NO            TO W-CLM-CLAIM
001132                                    W-ACTV-CLAIM.
001133
001134     MOVE W-ACTV-KEY             TO W-ACTV-SAVE-KEY.
001135
001136 1100-EXIT.
001137      EXIT.
001138                                 EJECT
001139 2000-CREATE-LETTER.
001140***************************************************************
001141*    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
001142*    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
001143*    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
001144*    WILL BE DISPLAYED ONTO THE SCREEN.                       *
001145*                                                             *
001146***************************************************************
001147
001148     MOVE SPACES                 TO W-RECORD-TABLE.
001149
001150     PERFORM 3000-READ-ADDR THRU 3999-EXIT.
001151
001152     IF  PI-FATAL-ERROR
001153         GO TO 2999-EXIT.
001154
001155     SET W-TB-NDX                TO 7.
001156     MOVE W-TOP-FORM             TO W-REC-TEXT (W-TB-NDX).
001157     SET W-TB-NDX UP BY 1.
001158
001159     MOVE PI-FORM-NUMBER         TO W-TEXT-LETTER.
001160     MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.
001161
001162     
      * EXEC CICS HANDLE CONDITION
001163*         NOTFND     (2120-ENDBR)
001164*         ENDFILE    (2120-ENDBR)
001165*         NOTOPEN    (2900-TEXT-NOT-OPEN)
001166*    END-EXEC
      *    MOVE '"$I''J                 ! # #00006854' TO DFHEIV0
           MOVE X'222449274A20202020202020' &
                X'202020202020202020202120' &
                X'2320233030303036383534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001167
001168     
      * EXEC CICS STARTBR
001169*         DATASET    (W-TEXT-ID)
001170*         RIDFLD     (W-TEXT-KEY)
001171*         GTEQ
001172*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006860' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036383630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001173
001174     MOVE 'Y'                    TO W-TEXT-BROWSE-STARTED.
001175
001176 2110-READ-NEXT.
001177
001178     IF  W-TB-NDX GREATER THAN W-MAX-LINES
001179         MOVE ER-0051            TO W-CURRENT-ERROR
001180         PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
001181         GO TO 2120-ENDBR.
001182
001183     
      * EXEC CICS READNEXT
001184*         DATASET    (W-TEXT-ID)
001185*         SET        (ADDRESS OF TEXT-FILES)
001186*         RIDFLD     (W-TEXT-KEY)
001187*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006875' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001188
001189*    SERVICE RELOAD TEXT-FILES.
001190
001191     IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY
001192         GO TO 2120-ENDBR.
001193
001194     IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
001195         PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT
001196
001197         IF  PI-FATAL-ERROR
001198             GO TO 2999-EXIT
001199
001200         ELSE
001201             GO TO 2110-READ-NEXT.
001202
001203
001204      IF (PI-COMPANY-ID = 'CID')
001205         AND (PI-CARRIER NOT = '8')
001206         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
001207            B1 > +20
001208            IF TX-TEXT-LINE (B1:5) = '@03.1'
001209               GO TO 2110-READ-NEXT
001210            END-IF
001211         END-PERFORM
001212      END-IF
001213      IF PI-AUTO-LETTER-DATE > SPACES
001214         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
001215           B1 > 61
001216           IF TX-TEXT-LINE (B1:10) = '@@AUTOPYDT' or '@@autopydt'
001217               MOVE PI-AUTO-LETTER-DATE TO TX-TEXT-LINE(B1:10)
001218           END-IF
001219         END-PERFORM
001220      END-IF.
001221
001222     MOVE TX-TEXT-LINE           TO W-REC-TEXT (W-TB-NDX).
001223     MOVE TX-PROCESS-CONTROL     TO W-REC-PC (W-TB-NDX)
001224                                    W-NDX-WORK.
001225     SET W-TB-NDX UP BY 1.
001226
001227     IF  W-NDX-WORK = 99
001228         MOVE W-TOP-FORM         TO W-REC-TEXT (W-TB-NDX)
001229         SET W-TB-NDX UP BY 1
001230         GO TO 2110-READ-NEXT
001231
001232     ELSE
001233         SET W-TB-NDX UP BY W-NDX-WORK
001234         GO TO 2110-READ-NEXT.
001235
001236 2120-ENDBR.
001237
001238     IF  W-TEXT-BROWSE-STARTED = 'Y'
001239         MOVE 'N'                TO W-TEXT-BROWSE-STARTED
001240
001241         
      * EXEC CICS ENDBR
001242*             DATASET (W-TEXT-ID)
001243*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006933' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001244
001245     IF  W-TB-NDX = 8
001246         MOVE ER-0006            TO W-CURRENT-ERROR
001247         PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
001248         GO TO 2999-EXIT.
001249
001250     SET W-TB-NDX DOWN BY 1.
001251     SET W-TOTAL-LINES           TO W-TB-NDX.
001252     MOVE 1                      TO W-CURRENT-LINE.
001253
001254     IF  PI-COMPANY-ID EQUAL 'AUK'
001255         MOVE 'Y'                TO W-REVERSE-DATE-SW.
001256
001257     PERFORM 7000-RESOLVE-VARIABLES THRU 7399-EXIT.
001258
001259     IF  PI-COMPANY-ID EQUAL 'AUK'
001260         MOVE SPACES             TO W-REVERSE-DATE-SW.
001261
001262     PERFORM 7800-VARIABLE-SEARCH THRU 7899-EXIT
001263             VARYING
001264         W-TB-NDX FROM 7 BY 1
001265             UNTIL
001266         W-TB-NDX > W-TOTAL-LINES.
001267     GO TO 2999-EXIT.
001268                                 EJECT
001269 2800-PROCESS-Z-CONTROLS.
001270
001271     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
001272
001273     IF  PI-RESEND-DATE EQUAL LOW-VALUES
001274
001275         IF  W-DAYS-TO-RESEND-1 NUMERIC
001276                 AND
001277             W-DAYS-TO-RESEND-1 GREATER THAN ZEROS
001278             MOVE '6'            TO DC-OPTION-CODE
001279             MOVE W-SAVE-BIN-DATE
001280                                 TO DC-BIN-DATE-1
001281             MOVE ZEROS          TO DC-ELAPSED-MONTHS
001282             MOVE W-DAYS-TO-RESEND-1
001283                                 TO DC-ELAPSED-DAYS
001284             PERFORM 9700-DATE-LINK THRU 9700-EXIT
001285
001286             IF  NO-CONVERSION-ERROR
001287                 MOVE DC-BIN-DATE-2
001288                                 TO PI-RESEND-DATE
001289
001290             ELSE
001291                 MOVE ER-3770    TO W-CURRENT-ERROR
001292                 PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001293
001294     IF  PI-FOLLOW-UP-DATE EQUAL LOW-VALUES
001295
001296         IF  W-DAYS-TO-FOLLOW-UP NUMERIC
001297                 AND
001298             W-DAYS-TO-FOLLOW-UP GREATER THAN ZEROS
001299             MOVE '6'            TO DC-OPTION-CODE
001300             MOVE W-SAVE-BIN-DATE
001301                                 TO DC-BIN-DATE-1
001302             MOVE ZEROS          TO DC-ELAPSED-MONTHS
001303             MOVE W-DAYS-TO-FOLLOW-UP
001304                                 TO DC-ELAPSED-DAYS
001305             PERFORM 9700-DATE-LINK THRU 9700-EXIT
001306
001307             IF  NO-CONVERSION-ERROR
001308                 MOVE DC-BIN-DATE-2
001309                                 TO PI-FOLLOW-UP-DATE
001310
001311             ELSE
001312                 MOVE ER-3771    TO W-CURRENT-ERROR
001313                 PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001314
001315     IF  PI-NUMBER-COPIES NOT NUMERIC
001316             OR
001317         PI-NUMBER-COPIES EQUAL ZEROS
001318
001319         IF  W-NUMBER-OF-COPIES NUMERIC
001320                 AND
001321             W-NUMBER-OF-COPIES NOT EQUAL ZEROS
001322             MOVE W-NUMBER-OF-COPIES
001323                                 TO PI-NUMBER-COPIES
001324
001325         ELSE
001326             MOVE +1             TO PI-NUMBER-COPIES.
001327
001328     IF W-FORM-TO-RESEND > SPACES
001329         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
001330     ELSE
001331         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
001332     END-IF.
001333
001334     MOVE W-PROMPT-LETTER               TO PI-PROMPT-LETTER.
001335     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD.
001336     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
001337     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
001338
001339 2800-EXIT.
001340     EXIT.
001341
001342 2900-TEXT-NOT-OPEN.
001343
001344     MOVE ER-0013                TO W-CURRENT-ERROR.
001345     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001346
001347 2999-EXIT.
001348     EXIT.
001349                                 EJECT
001350 3000-READ-ADDR.
001351
001352     
      * EXEC CICS HANDLE CONDITION
001353*         NOTOPEN    (3920-CLM-NOT-OPEN)
001354*         NOTFND     (3900-CLAIM-NOT-FOUND)
001355*    END-EXEC.
      *    MOVE '"$JI                  ! $ #00007044' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303037303434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001356
001357     
      * EXEC CICS READ
001358*         DATASET    (W-CLM-ID)
001359*         SET        (ADDRESS OF CLAIM-MASTER)
001360*         RIDFLD     (W-CLM-KEY)
001361*    END-EXEC.
      *    MOVE '&"S        E          (   #00007049' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037303439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CLM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001362
001363*    SERVICE RELOAD CLAIM-MASTER.
001364
001365     IF  PI-ADDR-TYPE NOT GREATER THAN SPACES
001366         GO TO 3999-EXIT.
001367
001368     PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.
001369
001370     MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.
001371
001372     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
001373         GO TO 3200-READ-PRODUCER.
001374
001375 3100-READ-ACCT.
001376
001377     MOVE CL-CERT-GROUPING       TO W-ACCT-GROUPING.
001378     MOVE CL-CERT-STATE          TO W-ACCT-STATE.
001379     MOVE CL-CERT-ACCOUNT        TO W-ACCT-ACCOUNT.
001380     MOVE CL-CERT-EFF-DT         TO W-ACCT-EXP-DATE.
001381
001382     
      * EXEC CICS HANDLE CONDITION
001383*         NOTOPEN    (3930-ACCT-NOT-OPEN)
001384*         NOTFND     (3910-ACCT-NOT-FOUND)
001385*         END-EXEC.
      *    MOVE '"$JI                  ! % #00007074' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303037303734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001386
001387     PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.
001388
001389     MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
001390
001391 3110-READNEXT.
001392
001393     PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.
001394
001395     IF  W-ACCT-PARTIAL-KEY NOT = W-ACCT-SAVE-KEY
001396
001397         IF  W-SAVE-ACCT-RECORD EQUAL SPACES
001398             GO TO 3910-ACCT-NOT-FOUND
001399
001400         ELSE
001401             MOVE 'Y'            TO W-ACCT-READ-SW
001402             MOVE AM-CONTROL-PRIMARY
001403                                 TO W-ACCT-KEY
001404             MOVE W-SAVE-ACCT-RECORD
001405                                 TO ACCOUNT-MASTER
001406             GO TO 3120-CONTINUE-BUILD-ADDR.
001407
001408     IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES
001409         NEXT SENTENCE
001410
001411     ELSE
001412         MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD
001413         GO TO 3110-READNEXT.
001414
001415     MOVE AM-CONTROL-PRIMARY     TO W-ACCT-KEY.
001416     MOVE 'Y'                    TO W-ACCT-READ-SW.
001417
001418
001419 3120-CONTINUE-BUILD-ADDR.
001420
001421     MOVE SPACES                 TO W-SAVE-ACCT-RECORD.
001422
001423     IF  W-ADDR-TYPE-CD EQUAL 'A0'
001424         MOVE ZEROS              TO W-PI-ADDR-SEQ
001425
001426     ELSE
001427         GO TO 3350-CHECK-BENE-ADDR.
001428
001429     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
001430     MOVE AM-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
001431     MOVE AM-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).
001432     MOVE AM-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).
001433     MOVE AM-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).
001434     MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).
001435
001436     MOVE SPACES                 TO W-ZIP-CODE.
001437
001438     IF  AM-CANADIAN-POST-CODE
001439         MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1
001440         MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2
001441
001442     ELSE
001443         MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE
001444
001445         IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS
001446             MOVE '-'            TO W-AM-ZIP-DASH
001447             MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
001448
001449     MOVE W-ZIP-CODE             TO W-REC-TEXT (6)
001450                                    W-LABEL-LINES (06).
001451
001452     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
001453
001454     MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
001455     MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
001456     MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
001457     MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
001458     MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
001459     MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.
001460
001461     GO TO 3999-EXIT.
001462
001463     EJECT
001464 3200-READ-PRODUCER.
001465
001466     MOVE CL-CERT-GROUPING       TO W-PROD-GROUPING.
001467     MOVE CL-CERT-STATE          TO W-PROD-STATE.
001468     MOVE CL-CERT-ACCOUNT        TO W-PROD-PRODUCER.
001469     MOVE CL-CERT-EFF-DT         TO W-PROD-EXP-DATE.
001470
001471     
      * EXEC CICS HANDLE CONDITION
001472*         NOTOPEN    (3960-PROD-NOT-OPEN)
001473*         NOTFND     (3950-PROD-NOT-FOUND)
001474*         END-EXEC.
      *    MOVE '"$JI                  ! & #00007163' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303037313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001475
001476     PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.
001477
001478     MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.
001479
001480 3210-READNEXT.
001481
001482     PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.
001483
001484     IF  W-PROD-PARTIAL-KEY NOT = W-PROD-SAVE-KEY
001485
001486         IF  W-SAVE-PROD-RECORD EQUAL SPACES
001487             GO TO 3950-PROD-NOT-FOUND
001488
001489         ELSE
001490             MOVE 'Y'            TO W-PROD-READ-SW
001491             MOVE PD-CONTROL-PRIMARY
001492                                 TO W-PROD-KEY
001493             MOVE W-SAVE-PROD-RECORD
001494                                 TO PRODUCER-MASTER
001495             GO TO 3220-CONTINUE-BUILD-ADDR.
001496
001497     IF  PD-EXPIRE-DATE EQUAL HIGH-VALUES
001498         NEXT SENTENCE
001499
001500     ELSE
001501         MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD
001502         GO TO 3210-READNEXT.
001503
001504     MOVE PD-CONTROL-PRIMARY     TO W-PROD-KEY.
001505     MOVE 'Y'                    TO W-PROD-READ-SW.
001506
001507 3220-CONTINUE-BUILD-ADDR.
001508
001509     MOVE SPACES                 TO W-SAVE-PROD-RECORD.
001510
001511     IF  W-ADDR-TYPE-CD EQUAL 'A0'
001512         MOVE ZEROS              TO W-PI-ADDR-SEQ
001513
001514     ELSE
001515         GO TO 3350-CHECK-BENE-ADDR.
001516
001517     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
001518     MOVE PD-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
001519     MOVE PD-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).
001520     MOVE PD-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).
001521     MOVE PD-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).
001522     MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).
001523
001524     MOVE SPACES                 TO W-ZIP-CODE.
001525
001526     MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.
001527
001528     IF  PD-ZIP-PLUS4 NOT = SPACES AND  ZEROS
001529         MOVE '-'                TO W-AM-ZIP-DASH
001530         MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.
001531
001532     MOVE W-ZIP-CODE             TO W-REC-TEXT (6)
001533                                    W-LABEL-LINES (06).
001534
001535     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
001536
001537     MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
001538     MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
001539     MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
001540     MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
001541     MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
001542     MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.
001543
001544     GO TO 3999-EXIT.
001545
001546     EJECT
001547 3350-CHECK-BENE-ADDR.
001548
001549     IF  W-ADDR-TYPE-CD NOT = 'B0'
001550         GO TO 3400-CONTINUE-BUILD-ADDR.
001551
001552     IF  CL-BENEFICIARY = SPACES
001553         GO TO 3450-ACTV-NOT-FOUND.
001554
001555     
      * EXEC CICS HANDLE CONDITION
001556*         NOTFND (3450-ACTV-NOT-FOUND)
001557*    END-EXEC.
      *    MOVE '"$I                   ! '' #00007247' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303037323437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001558
001559     MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
001560     MOVE 'B'                    TO W-BENE-REC-TYPE.
001561     MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.
001562
001563     
      * EXEC CICS READ
001564*         DATASET    (W-BENE-ID)
001565*         SET        (ADDRESS OF BENEFICIARY-MASTER)
001566*         RIDFLD     (W-BENE-KEY)
001567*         END-EXEC.
      *    MOVE '&"S        E          (   #00007255' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037323535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001568
001569*    SERVICE RELOAD BENEFICIARY-MASTER.
001570
001571     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
001572     MOVE BE-MAIL-TO-NAME        TO W-REC-TEXT (1)
001573                                    W-LABEL-LINES (01).
001574     MOVE BE-ADDRESS-LINE-1      TO W-REC-TEXT (02)
001575                                    W-LABEL-LINES (02).
001576     MOVE BE-ADDRESS-LINE-2      TO W-REC-TEXT (03)
001577                                    W-LABEL-LINES (03).
001578     MOVE BE-CITY-STATE          TO W-REC-TEXT (04)
001579                                    W-LABEL-LINES (04).
001580
001581     MOVE SPACES                 TO W-ZIP-CODE.
001582
001583     IF  BE-CANADIAN-POST-CODE
001584         MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1
001585         MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2
001586
001587     ELSE
001588         MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE
001589
001590         IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS
001591             MOVE '-'            TO W-AM-ZIP-DASH
001592             MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
001593
001594     MOVE W-ZIP-CODE             TO W-REC-TEXT (05)
001595                                    W-LABEL-LINES (05).
001596
001597     MOVE SPACES                 TO W-REC-TEXT (6).
001598
001599     GO TO 3420-SET-ADDR.
001600
001601 3400-CONTINUE-BUILD-ADDR.
001602
001603     IF  W-ACTV-SEQ = ZEROS
001604         GO TO 3450-ACTV-NOT-FOUND.
001605
001606     MOVE W-ACTV-SEQ             TO W-PI-ADDR-SEQ.
001607
001608     
      * EXEC CICS HANDLE CONDITION
001609*         NOTOPEN    (3940-ACTV-NOT-OPEN)
001610*         NOTFND     (3450-ACTV-NOT-FOUND)
001611*    END-EXEC.
      *    MOVE '"$JI                  ! ( #00007300' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303037333030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001612
001613     
      * EXEC CICS READ
001614*         DATASET    (W-ACTV-ID)
001615*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
001616*         RIDFLD     (W-ACTV-KEY)
001617*    END-EXEC.
      *    MOVE '&"S        E          (   #00007305' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037333035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001618
001619*    SERVICE RELOAD ACTIVITY-TRAILERS.
001620
001621     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
001622     MOVE AT-MAIL-TO-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
001623     MOVE AT-ADDRESS-LINE-1 TO W-REC-TEXT (02) W-LABEL-LINES (02).
001624     MOVE AT-ADDRESS-LINE-2 TO W-REC-TEXT (03) W-LABEL-LINES (03).
001625     MOVE AT-CITY-STATE     TO W-REC-TEXT (04) W-LABEL-LINES (04).
001626
001627     MOVE SPACES                 TO W-ZIP-CODE.
001628
001629     IF  AT-CANADIAN-POST-CODE
001630         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
001631         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
001632
001633     ELSE
001634         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
001635         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
001636             MOVE '-'            TO W-AM-ZIP-DASH
001637             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
001638
001639     MOVE W-ZIP-CODE             TO W-REC-TEXT (05)
001640                                    W-LABEL-LINES (05).
001641
001642     MOVE SPACES                 TO W-REC-TEXT (6).
001643
001644 3420-SET-ADDR.
001645
001646     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
001647
001648     MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
001649     MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
001650     MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
001651     MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
001652     MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
001653     MOVE SPACES                 TO W-REC-TEXT (06) SS05-6D.
001654     GO TO 3999-EXIT.
001655
001656 3450-ACTV-NOT-FOUND.
001657
001658     IF  W-ACTV-SEQ EQUAL +29
001659         NEXT SENTENCE
001660
001661     ELSE
001662         GO TO 3480-CONTINUE-ACTV-ERROR.
001663
001664     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
001665         GO TO 3999-EXIT.
001666
001667     IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
001668         MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL
001669         GO TO 3480-CONTINUE-ACTV-ERROR.
001670
001671     IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
001672         GO TO 3480-CONTINUE-ACTV-ERROR.
001673
001674     MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD.
001675     MOVE AM-CARRIER             TO W-COMP-CARRIER.
001676     MOVE AM-GROUPING            TO W-COMP-GROUPING.
001677     MOVE 'A'                    TO W-COMP-TYPE.
001678     MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
001679                                 TO W-COMP-RESP-NO.
001680
001681     IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO
001682         IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR
001683                                           'G' OR 'B' or 'S'
001684             MOVE 'G'            TO W-COMP-TYPE
001685             MOVE LOW-VALUES     TO W-COMP-ACCOUNT
001686         ELSE
001687             MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
001688                                 TO W-COMP-ACCOUNT
001689     ELSE
001690         MOVE 'G'                TO W-COMP-TYPE
001691         MOVE LOW-VALUES         TO W-COMP-ACCOUNT.
001692
001693     IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
001694         MOVE ZEROS              TO W-COMP-CARRIER.
001695
001696     IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
001697         MOVE ZEROS              TO W-COMP-GROUPING.
001698
001699     
      * EXEC CICS HANDLE CONDITION
001700*         NOTFND    (3480-CONTINUE-ACTV-ERROR)
001701*         NOTOPEN   (3479-COMP-NOT-OPEN)
001702*    END-EXEC.
      *    MOVE '"$IJ                  ! ) #00007391' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303037333931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001703
001704     
      * EXEC CICS  READ
001705*         SET      (ADDRESS OF COMPENSATION-MASTER)
001706*         DATASET  ('ERCOMP')
001707*         RIDFLD   (W-COMP-KEY)
001708*    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007396' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037333936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001709
001710*    SERVICE RELOAD COMPENSATION-MASTER.
001711
001712     MOVE 'Y'                    TO W-COMP-READ-SW.
001713     MOVE SPACES                 TO W-LABEL-HOLD-AREA
001714     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)
001715
001716     IF  CO-ACCT-NAME EQUAL SPACES
001717         MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).
001718
001719     MOVE CO-ADDR-1              TO W-LABEL-LINES (02).
001720     MOVE CO-ADDR-2              TO W-LABEL-LINES (03).
001721     MOVE CO-ADDR-3              TO W-LABEL-LINES (04).
001722
001723     MOVE SPACES                 TO W-ZIP-CODE.
001724
001725     IF  CO-CANADIAN-POST-CODE
001726         MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1
001727         MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2
001728
001729     ELSE
001730         MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE
001731
001732         IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS
001733             MOVE '-'            TO W-AM-ZIP-DASH
001734             MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
001735
001736     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
001737
001738     MOVE ZEROS                  TO W-PHONE-IN.
001739     MOVE CO-AREA-CODE           TO W-PO-AREA.
001740     MOVE CO-PREFIX              TO W-PO-PFX.
001741     MOVE CO-PHONE               TO W-PO-SFX.
001742     MOVE W-PHONE-OUT            TO SS53-6D.
001743
001744     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
001745
001746     MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (1) SS52D.
001747     MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS53-1D.
001748     MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS53-2D.
001749     MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS53-3D.
001750     MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS53-4D.
001751     MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (6) SS53-5D.
001752
001753     GO TO 3999-EXIT.
001754
001755 3479-COMP-NOT-OPEN.
001756
001757     MOVE ER-2055                TO W-CURRENT-ERROR.
001758     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001759     GO TO 3999-EXIT.
001760                                 EJECT
001761 3480-CONTINUE-ACTV-ERROR.
001762
001763     MOVE ER-0178                TO W-CURRENT-ERROR.
001764     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001765*    MOVE SPACES                 TO PI-ADDR-TYPE.
001766     MOVE SPACES                 TO W-REC-TEXT (01)
001767                                    W-REC-TEXT (02)
001768                                    W-REC-TEXT (03)
001769                                    W-REC-TEXT (04)
001770                                    W-REC-TEXT (05)
001771                                    W-REC-TEXT (06).
001772     GO TO 3999-EXIT.
001773
001774 3900-CLAIM-NOT-FOUND.
001775
001776     MOVE ER-0186                TO W-CURRENT-ERROR.
001777     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001778     GO TO 3999-EXIT.
001779
001780 3910-ACCT-NOT-FOUND.
001781
001782     MOVE ER-0179                TO W-CURRENT-ERROR.
001783     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001784     GO TO 3999-EXIT.
001785
001786 3920-CLM-NOT-OPEN.
001787
001788     MOVE ER-0154                TO W-CURRENT-ERROR.
001789     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001790     GO TO 3999-EXIT.
001791
001792 3930-ACCT-NOT-OPEN.
001793
001794     MOVE ER-0168                TO W-CURRENT-ERROR.
001795     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001796     GO TO 3999-EXIT.
001797
001798 3940-ACTV-NOT-OPEN.
001799
001800     MOVE ER-0172                TO W-CURRENT-ERROR.
001801     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001802     GO TO 3999-EXIT.
001803
001804 3950-PROD-NOT-FOUND.
001805     MOVE ER-9887                TO W-CURRENT-ERROR.
001806     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001807     GO TO 3999-EXIT.
001808
001809 3960-PROD-NOT-OPEN.
001810     MOVE ER-9886                TO W-CURRENT-ERROR.
001811     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001812     GO TO 3999-EXIT.
001813
001814 3999-EXIT.
001815      EXIT.
001816                                 EJECT
001817 6000-ARCHIVE-LETTER.
001818***************************************************************
001819*    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
001820*    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *
001821*    THE ARCHIVE FILE.                                        *
001822*    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
001823*        1. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *
001824*        2. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
001825*        3. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *
001826*        4. BUILD A CORRESPONDENCE TRAILER.                   *
001827*        5. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *
001828*           ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.      *
001829*        6. RESET ALL CONTROL FIELDS AND RETURN THE           *
001830*           ARCHIVE NUMBER USED TO FILE THE RECORDS.          *
001831***************************************************************
001832
001833     MOVE +0                     TO TALLY.
001834     INSPECT W-RECORD-TABLE TALLYING TALLY
001835                                 FOR CHARACTERS BEFORE '@'.
001836
001837     IF  TALLY LESS THAN +21900
001838         COMPUTE W-CURRENT-LINE = TALLY / 73
001839         MOVE ER-0191            TO W-CURRENT-ERROR
001840         PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001841
001842     MOVE '1'                    TO W-CNTL-RECORD-TYPE.
001843     MOVE ZEROS                  TO W-CNTL-SEQ.
001844     MOVE SPACES                 TO W-CNTL-GENL.
001845
001846     
      * EXEC CICS HANDLE CONDITION
001847*         NOTFND  (6100-NOT-FOUND)
001848*         NOTOPEN (6110-CNTL-NOT-OPEN)
001849*    END-EXEC.
      *    MOVE '"$IJ                  ! * #00007538' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303037353338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001850
001851     
      * EXEC CICS READ
001852*         DATASET  (W-CNTL-ID)
001853*         SET      (ADDRESS OF CONTROL-FILE)
001854*         RIDFLD   (W-CNTL-KEY)
001855*         UPDATE
001856*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007543' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037353433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001857
001858*    SERVICE RELOAD CONTROL-FILE.
001859     ADD 1                       TO CF-CO-ARCHIVE-COUNTER.
001860     MOVE CF-CO-ARCHIVE-COUNTER  TO W-ARCH-NUMBER
001861                                    PI-ARCHIVE-NUMBER.
001862     MOVE CF-PRINT-ADDRESS-LABELS
001863                                 TO W-LABELS-SW.
001864
001865     
      * EXEC CICS REWRITE
001866*         FROM      (CONTROL-FILE)
001867*         DATASET   (W-CNTL-ID)
001868*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007557' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037353537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001869
001870     PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
001871
001872     
      * EXEC CICS HANDLE CONDITION
001873*         NOTOPEN (6120-ARCH-NOT-OPEN)
001874*    END-EXEC.
      *    MOVE '"$J                   ! + #00007564' TO DFHEIV0
           MOVE X'22244A202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303037353634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001875
001876     IF  PI-ARCH-POINTER GREATER THAN ZEROS
001877         MOVE PI-ARCH-POINTER    TO LCP-WS-ADDR-COMP
001878         SET ADDRESS OF LETTER-ARCHIVE TO LCP-WS-ADDR-PNTR
001879*        SERVICE RELOAD LETTER-ARCHIVE
001880
001881     ELSE
001882         
      * EXEC CICS GETMAIN
001883*             SET      (ADDRESS OF LETTER-ARCHIVE)
001884*             LENGTH   (W-ARCH-LENGTH)
001885*        END-EXEC
      *    MOVE '," L                  $   #00007574' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037353734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001886         SET LCP-WS-ADDR-PNTR TO ADDRESS OF LETTER-ARCHIVE
001887
001888*        SERVICE RELOAD LETTER-ARCHIVE
001889         MOVE LCP-WS-ADDR-COMP TO PI-ARCH-POINTER.
001890
001891     MOVE SPACES                 TO LETTER-ARCHIVE.
001892     MOVE 'LA'                   TO LA-RECORD-ID.
001893     MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
001894                                    LA-ARCHIVE-NO-A1.
001895     MOVE '1'                    TO LA-RECORD-TYPE
001896                                    LA-RECORD-TYPE-A1.
001897     MOVE ZEROS                  TO LA-LINE-SEQ-NO
001898                                    LA-LINE-SEQ-NO-A1.
001899     MOVE W-ARCH-CO              TO LA-COMPANY-CD
001900                                    LA-COMPANY-CD-A1.
001901     MOVE W-CLM-CARRIER          TO LA-CARRIER.
001902     MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.
001903     MOVE PI-CERT-NO             TO LA-CERT-NO.
001904
001905     IF  PI-NUMBER-COPIES NUMERIC
001906             AND
001907         PI-NUMBER-COPIES GREATER THAN ZEROS
001908         MOVE PI-NUMBER-COPIES
001909                                 TO LA-NO-OF-COPIES
001910
001911     ELSE
001912         MOVE  1                 TO LA-NO-OF-COPIES.
001913
001914     MOVE PI-RESEND-DATE         TO LA-RESEND-DATE.
001915
001916     MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
001917     MOVE W-CURRENT-SAVE         TO LA-CREATION-DT.
001918     MOVE LOW-VALUES             TO LA-INITIAL-PRINT-DATE
001919                                    LA-RESEND-PRINT-DATE.
001920     MOVE W-CORR-TRLR-SEQ        TO LA-CORR-TRLR-SEQ.
001921     PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
001922
001923     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
001924         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
001925     END-IF.
001926
001927     IF  W-LABELS-SW EQUAL       TO 'N'
001928         NEXT SENTENCE
001929
001930     ELSE
001931         SET W-TB-NDX            TO 1
001932         MOVE ZEROS              TO W-SEQ-COUNTER
001933         PERFORM 6300-FORMAT-ADDRESS THRU 6300-EXIT
001934                 VARYING
001935             W-TB-NDX FROM 1 BY 1
001936                 UNTIL
001937             W-TB-NDX GREATER THAN 6.
001938
001939     MOVE ZEROS                  TO W-SEQ-COUNTER.
001940     PERFORM 6200-FORMAT-TEXT THRU 6200-EXIT
001941             VARYING
001942         W-TB-NDX FROM 8 BY 1
001943             UNTIL
001944         W-TB-NDX GREATER THAN W-TOTAL-LINES.
001945
001946 6000-EXIT.
001947     EXIT.
001948
001949 6100-NOT-FOUND.
001950
001951     MOVE ER-0281                TO W-CURRENT-ERROR.
001952     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001953     GO TO 6000-EXIT.
001954
001955 6110-CNTL-NOT-OPEN.
001956
001957     MOVE ER-0042                TO W-CURRENT-ERROR.
001958     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001959     GO TO 6000-EXIT.
001960
001961 6120-ARCH-NOT-OPEN.
001962
001963     MOVE ER-0332                TO W-CURRENT-ERROR.
001964     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
001965     GO TO 6000-EXIT.
001966
001967                                 EJECT
001968 6200-FORMAT-TEXT.
001969
001970     MOVE SPACES                 TO LETTER-ARCHIVE.
001971     MOVE '3'                    TO LA-RECORD-TYPE
001972                                    LA-RECORD-TYPE-A1.
001973     MOVE 'LA'                   TO LA-RECORD-ID.
001974     MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
001975                                    LA-ARCHIVE-NO-A1.
001976     MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
001977                                    LA-LINE-SEQ-NO-A1.
001978     MOVE W-ARCH-CO              TO LA-COMPANY-CD
001979                                    LA-COMPANY-CD-A1.
001980     MOVE W-REC-TEXT (W-TB-NDX)  TO LA-TEXT-LINE.
001981     SET W-TB-NDX1               TO W-TB-NDX.
001982     SET W-TB-NDX1 UP BY 1.
001983     MOVE ZEROS                  TO W-NDX-WORK.
001984
001985 6200-LOOP.
001986
001987     IF  W-TB-NDX1 LESS THAN W-TOTAL-LINES
001988             AND
001989         W-REC-TEXT (W-TB-NDX1) = SPACES
001990         SET W-TB-NDX1 UP BY 1
001991         ADD 1                   TO W-NDX-WORK
001992         GO TO 6200-LOOP.
001993
001994     IF  W-REC-TEXT (W-TB-NDX1) = W-TOP-FORM
001995         MOVE '99'               TO LA-SKIP-CONTROL
001996         SET W-TB-NDX1 UP BY 1
001997
001998     ELSE
001999         MOVE W-NDX-WORK         TO LA-SKIP-CONTROL.
002000
002001     SET W-TB-NDX                TO W-TB-NDX1.
002002     SET W-TB-NDX DOWN BY 1.
002003     PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
002004     ADD 1 TO W-SEQ-COUNTER.
002005
002006 6200-EXIT.
002007      EXIT.
002008                                 EJECT
002009 6300-FORMAT-ADDRESS.
002010
002011     MOVE SPACES                 TO LETTER-ARCHIVE.
002012     MOVE '2'                    TO LA-RECORD-TYPE
002013                                    LA-RECORD-TYPE-A1.
002014     MOVE 'LA'                   TO LA-RECORD-ID.
002015     MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
002016                                    LA-ARCHIVE-NO-A1.
002017     MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
002018                                    LA-LINE-SEQ-NO-A1.
002019     MOVE W-ARCH-CO              TO LA-COMPANY-CD
002020                                    LA-COMPANY-CD-A1.
002021     MOVE W-REC-TEXT (W-TB-NDX)  TO LA-ADDRESS-LINE.
002022     PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
002023     ADD 1 TO W-SEQ-COUNTER.
002024
002025 6300-EXIT.
002026      EXIT.
002027                                 EJECT
002028 6400-WRITE-ARCHIVE.
002029
002030     
      * EXEC CICS HANDLE CONDITION
002031*        DUPREC    (6420-ARCH-DUPREC)
002032*        NOTOPEN   (6410-ARCH-NOT-OPEN)
002033*        NOSPACE   (6425-ARCH-NOSPACE)
002034*    END-EXEC.
      *    MOVE '"$%JE                 ! , #00007722' TO DFHEIV0
           MOVE X'2224254A4520202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303037373232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002035
002036     
      * EXEC CICS WRITE
002037*         DATASET   (W-ARCH-ID)
002038*         FROM      (LETTER-ARCHIVE)
002039*         RIDFLD    (LA-CONTROL-PRIMARY)
002040*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007728' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037373238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002041
002042 6400-EXIT.
002043     EXIT.
002044
002045 6410-ARCH-NOT-OPEN.
002046
002047     MOVE ER-0332                TO W-CURRENT-ERROR.
002048     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002049     GO TO 6400-EXIT.
002050
002051 6420-ARCH-DUPREC.
002052
002053     MOVE ER-3766                TO W-CURRENT-ERROR.
002054     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002055     GO TO 6400-EXIT.
002056
002057 6425-ARCH-NOSPACE.
002058
002059     MOVE ER-3699                TO W-CURRENT-ERROR.
002060     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002061     GO TO 6400-EXIT.
002062                                 EJECT
002063 6500-BUILD-CORRESPOND.
002064***************************************************************
002065*    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
002066*    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
002067*    USING THE NEW SEQUENCE NUMBER.                           *
002068*    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
002069*    TRAILER RECORD.                                          *
002070***************************************************************
002071
002072     MOVE PI-CLAIM-NO            TO W-CLM-CLAIM.
002073
002074     
      * EXEC CICS READ
002075*         DATASET    (W-CLM-ID)
002076*         SET        (ADDRESS OF CLAIM-MASTER)
002077*         RIDFLD     (W-CLM-KEY)
002078*         UPDATE
002079*         END-EXEC.
      *    MOVE '&"S        EU         (   #00007766' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CLM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002080
002081*    SERVICE RELOAD CLAIM-MASTER.
002082
002083     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
002084
002085     IF  PI-FOLLOW-UP-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT
002086         MOVE PI-FOLLOW-UP-DATE  TO CL-NEXT-FOLLOWUP-DT.
002087
002088     IF  PI-RESEND-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT
002089         MOVE PI-RESEND-DATE     TO CL-NEXT-FOLLOWUP-DT.
002090
002091     MOVE '2'                    TO CL-LAST-MAINT-TYPE.
002092
002093     IF  PI-ACTV-POINTER GREATER THAN ZEROS
002094         MOVE PI-ACTV-POINTER    TO LCP-WS-ADDR-COMP
002095         SET ADDRESS OF ACTIVITY-TRAILERS TO LCP-WS-ADDR-PNTR
002096*        SERVICE RELOAD ACTIVITY-TRAILERS
002097
002098     ELSE
002099         
      * EXEC CICS GETMAIN
002100*             SET      (ADDRESS OF ACTIVITY-TRAILERS)
002101*             INITIMG  (W-GETMAINSPACE)
002102*             LENGTH   (W-ACTV-LENGTH)
002103*        END-EXEC
      *    MOVE ',"IL                  $   #00007791' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037373931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ACTV-LENGTH, 
                 W-GETMAINSPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002104         SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACTIVITY-TRAILERS
002105
002106*        SERVICE RELOAD ACTIVITY-TRAILERS
002107         MOVE LCP-WS-ADDR-COMP TO PI-ACTV-POINTER.
002108
002109     MOVE 'AT'                   TO AT-RECORD-ID.
002110     MOVE  4                     TO AT-TRAILER-TYPE.
002111     MOVE W-CURRENT-SAVE         TO AT-RECORDED-DT
002112                                    CL-LAST-MAINT-DT
002113                                    AT-CORR-LAST-MAINT-DT
002114     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
002115                                    CL-LAST-MAINT-USER
002116                                    AT-CORR-LAST-UPDATED-BY
002117     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
002118                                    CL-LAST-MAINT-HHMMSS.
002119     MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.
002120*    MOVE W-ACTV-KEY             TO AT-CONTROL-PRIMARY.
002121*    MOVE W-ACTV-SAVE-KEY        TO AT-CONTROL-PRIMARY.
002122     MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
002123     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
002124                                    W-CORR-TRLR-SEQ.
002125     MOVE W-CURRENT-SAVE         TO AT-LETTER-SENT-DT.
002126     MOVE PI-FOLLOW-UP-DATE      TO AT-RECEIPT-FOLLOW-UP.
002127     MOVE PI-RESEND-DATE         TO AT-AUTO-RE-SEND-DT.
002128     MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
002129                                    AT-LETTER-PURGED-DT.
002130     MOVE W-ARCH-NUMBER          TO AT-LETTER-ARCHIVE-NO.
002131     MOVE '1'                    TO AT-LETTER-ORIGIN.
002132
002133     MOVE PI-FORM-NUMBER         TO AT-STD-LETTER-FORM
002134     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
002135     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
002136     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
002137
002138     IF  PI-REASON GREATER THAN LOW-VALUES
002139         MOVE PI-REASON          TO AT-REASON-TEXT
002140
002141     ELSE
002142         MOVE SPACES             TO AT-REASON-TEXT.
002143
002144     MOVE W-PI-ADDR-SEQ          TO AT-ADDRESS-REC-SEQ-NO.
002145
002146     IF  PI-ADDR-TYPE = LOW-VALUES
002147         MOVE SPACES             TO AT-ADDRESEE-TYPE
002148
002149     ELSE
002150         MOVE PI-ADDR-TYPE       TO AT-ADDRESEE-TYPE.
002151
002152     MOVE W-REC-TEXT (1)         TO AT-ADDRESSEE-NAME.
002153
002154     MOVE LOW-VALUES             TO AT-INITIAL-PRINT-DATE
002155                                    AT-RESEND-PRINT-DATE.
002156
002157     
      * EXEC CICS HANDLE CONDITION
002158*        DUPREC    (6596-ACTV-DUPREC)
002159*        NOSPACE   (6597-ACTV-NOSPACE)
002160*    END-EXEC.
      *    MOVE '"$%E                  ! - #00007849' TO DFHEIV0
           MOVE X'222425452020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303037383439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002161
002162     
      * EXEC CICS WRITE
002163*         DATASET    (W-ACTV-ID)
002164*         FROM       (ACTIVITY-TRAILERS)
002165*         RIDFLD     (AT-CONTROL-PRIMARY)
002166*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007854' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037383534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002167
002168     
      * EXEC CICS HANDLE CONDITION
002169*        DUPREC      (6598-REWRITE-CLAIM)
002170*    END-EXEC.
      *    MOVE '"$%                   ! . #00007860' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303037383630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002171
002172     
      * EXEC CICS REWRITE
002173*         DATASET    (W-CLM-ID)
002174*         FROM       (CLAIM-MASTER)
002175*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007864' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037383634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002176
002177     GO TO 6599-EXIT.
002178
002179 6596-ACTV-DUPREC.
002180
002181     MOVE ER-3697                TO W-CURRENT-ERROR.
002182     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002183     GO TO 6599-EXIT.
002184
002185 6597-ACTV-NOSPACE.
002186
002187     MOVE ER-3698                TO W-CURRENT-ERROR.
002188     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002189     GO TO 6599-EXIT.
002190
002191 6598-REWRITE-CLAIM.
002192
002193 6599-EXIT.
002194      EXIT.
002195                                 EJECT
002196 6600-SET-ADDR-SEQ.
002197
002198     MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.
002199
002200     IF  W-ADDR-SEQ NOT NUMERIC
002201         GO TO 6699-EXIT.
002202
002203     MOVE ZEROS                  TO W-PI-ADDR-SEQ.
002204
002205     IF  PI-ADDR-TYPE EQUAL 'I'
002206         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002207
002208     ELSE
002209     IF  PI-ADDR-TYPE EQUAL 'A'
002210         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002211         ADD +20                 TO W-ACTV-SEQ
002212
002213     ELSE
002214     IF  PI-ADDR-TYPE EQUAL 'B'
002215         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002216         ADD +10                 TO W-ACTV-SEQ
002217
002218     ELSE
002219     IF  PI-ADDR-TYPE EQUAL 'P'
002220         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002221         ADD +30                 TO W-ACTV-SEQ
002222
002223     ELSE
002224     IF  PI-ADDR-TYPE EQUAL 'E'
002225         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002226         ADD +40                 TO W-ACTV-SEQ
002227
002228     ELSE
002229     IF  PI-ADDR-TYPE EQUAL 'O'
002230         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002231         ADD +50                 TO W-ACTV-SEQ
002232
002233     ELSE
002234     IF  PI-ADDR-TYPE EQUAL 'Q'
002235         MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
002236         ADD +60                 TO W-ACTV-SEQ.
002237
002238 6699-EXIT.
002239      EXIT.
002240
002241 6700-BUILD-NAPERSOFT.
002242
002243     
      * EXEC CICS GETMAIN
002244*         SET      (ADDRESS OF NAPERSOFT-FILE)
002245*         LENGTH   (W-NAPS-LENGTH)
002246*    END-EXEC.
      *    MOVE '," L                  $   #00007935' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037393335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-NAPS-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002247
002248     MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
002249     MOVE 'NA'                  TO  NA-RECORD-ID.
002250     MOVE PI-COMPANY-CD         TO  NA-COMPANY-CD.
002251     MOVE PI-CARRIER            TO  NA-CARRIER.
002252     MOVE PI-CLAIM-NO           TO  NA-CLAIM-NO.
002253     MOVE PI-CERT-NO            TO  NA-CERT-NO.
002254     MOVE W-ARCH-NUMBER         TO  NA-ARCHIVE-NO.
002255     MOVE PI-FORM-NUMBER        TO  NA-LETTER-ID.
002256     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
002257     MOVE W-CURRENT-SAVE        TO  NA-CREATION-DT.
002258     MOVE LOW-VALUES            TO  NA-INITIAL-PRINT-DT
002259     MOVE PI-FOLLOW-UP-DATE     TO  NA-FOLLOW-UP-DT.
002260     MOVE PI-RESEND-DATE        TO  NA-RESEND-DT
002261     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
002262     IF PI-NUMBER-COPIES NOT = ZEROS
002263         MOVE PI-NUMBER-COPIES  TO  NA-NO-OF-COPIES
002264     ELSE
002265         MOVE 1                 TO  NA-NO-OF-COPIES
002266     END-IF.
002267     IF PI-ADDR-TYPE = ' 0'
002268         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
002269     ELSE
002270         MOVE PI-ADDR-TYPE      TO  NA-ADDRESS-TYPE
002271     END-IF.
002272     MOVE W-CORR-TRLR-SEQ       TO  NA-CORR-TRLR-SEQ.
002273     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
002274     IF PI-AUTO-LETTER-DATE > SPACES
002275        MOVE PI-AUTO-LETTER-DATE TO DC-GREG-DATE-A-EDIT
002276        MOVE DC-EDITA-MONTH     TO DC-EDIT1-MONTH
002277        MOVE SLASHA-1           TO SLASH1-1
002278        MOVE DC-EDITA-DAY       TO DC-EDIT1-DAY
002279        MOVE SLASHA-2           TO SLASH1-2
002280        MOVE DC-EDITA-YEAR      TO DC-EDIT1-YEAR
002281        MOVE '2'                TO  DC-OPTION-CODE
002282        PERFORM 9700-DATE-LINK  THRU  9700-EXIT
002283        IF DATE-CONVERSION-ERROR
002284            MOVE LOW-VALUES     TO  NA-AUTOPYDT
002285        ELSE
002286            MOVE DC-BIN-DATE-1  TO  NA-AUTOPYDT
002287     ELSE
002288        MOVE LOW-VALUES         TO  NA-AUTOPYDT
002289     END-IF.
002290
002291     
      * EXEC CICS WRITE
002292*         DATASET    (W-NAPS-ID)
002293*         FROM       (NAPERSOFT-FILE)
002294*         RIDFLD     (NA-CONTROL-PRIMARY)
002295*    END-EXEC.
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007983' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037393833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NAPS-ID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 NA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002296
002297 6799-EXIT.
002298      EXIT.
002299                                 EJECT
002300 7000-RESOLVE-VARIABLES.
002301***************************************************************
002302*    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
002303*    WITH DATA PERTAINING        TO THE DESIGNATED CLAIM.     *
002304*    THIS ROUTINE IS PERFORM THRU 7399-EXIT IN ORDER TO       *
002305*    RESOLVE ALL OF THE SYMBOLS.                              *
002306***************************************************************
002307
002308     MOVE PI-CLAIM-NO            TO SS34D.
002309
002310
002311     EVALUATE TRUE
002312
002313     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
002314        MOVE PI-AH-OVERRIDE-L6   TO SS12D
002315
002316     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
002317        MOVE PI-LIFE-OVERRIDE-L6 TO SS12D
002318
002319     WHEN CL-CLAIM-TYPE = 'I'
002320        MOVE '  IU  '            TO SS12D
002321
002322     WHEN CL-CLAIM-TYPE = 'G'
002323        MOVE ' GAP  '            TO SS12D
002324
002325     WHEN CL-CLAIM-TYPE = 'B'
002326        MOVE ' BRV  '            TO SS12D
002327
002328     WHEN CL-CLAIM-TYPE = 'H'
002329        MOVE ' HOSP '            TO SS12D
002330
002331     WHEN CL-CLAIM-TYPE = 'O'
002332        MOVE ' OTH  '            TO SS12D
002333
002334     END-EVALUATE
002335
002336     MOVE CL-LAST-PMT-AMT        TO SS16D.
002337     MOVE CL-TOTAL-PAID-AMT      TO SS18D.
002338     MOVE CL-CAUSE-CD            TO SS19-1D.
002339     MOVE CL-ASSOC-CERT-SEQU     TO SS54D.
002340     MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.
002341
002342     MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
002343     MOVE SPACES                 TO DC-OPTION-CODE.
002344     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002345
002346     IF  NO-CONVERSION-ERROR
002347         MOVE DC-GREG-DATE-1-EDIT
002348                                 TO SS13D.
002349
002350     MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.
002351     MOVE SPACES                 TO DC-OPTION-CODE.
002352     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002353
002354     IF  NO-CONVERSION-ERROR
002355         MOVE DC-GREG-DATE-1-EDIT
002356                                 TO SS14D.
002357
002358     MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.
002359     MOVE SPACES                 TO DC-OPTION-CODE.
002360     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002361
002362     IF  NO-CONVERSION-ERROR
002363         MOVE DC-GREG-DATE-1-EDIT
002364                                 TO SS15D.
002365
002366     IF  NOT PI-USES-PAID-TO
002367         MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
002368         MOVE SPACES             TO DC-OPTION-CODE
002369         PERFORM 9700-DATE-LINK THRU 9700-EXIT
002370
002371         IF  NO-CONVERSION-ERROR
002372             MOVE DC-GREG-DATE-1-EDIT
002373                                 TO SS17D
002374
002375         ELSE
002376             MOVE SPACES         TO SS17D
002377
002378     ELSE
002379         MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
002380         MOVE '6'                TO DC-OPTION-CODE
002381         MOVE +1                 TO DC-ELAPSED-DAYS
002382         MOVE +0                 TO DC-ELAPSED-MONTHS
002383         PERFORM 9700-DATE-LINK THRU 9700-EXIT
002384
002385         IF  NO-CONVERSION-ERROR
002386             MOVE DC-GREG-DATE-1-EDIT
002387                                 TO SS17D.
002388
002389     MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.
002390     MOVE SPACES                 TO DC-OPTION-CODE.
002391     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002392
002393     IF  NO-CONVERSION-ERROR
002394         MOVE DC-GREG-DATE-1-EDIT
002395                                 TO SS45D
002396
002397     ELSE
002398         MOVE '@@DOB'            TO SS45D.
002399
002400     IF  CL-SSN-STATE = CL-CERT-STATE
002401
002402         IF  CL-SSN-ACCOUNT EQUAL CL-CERT-ACCOUNT-PRIME
002403             NEXT SENTENCE
002404
002405         ELSE
002406             MOVE CL-SOC-SEC-NO  TO SS46D
002407
002408     ELSE
002409         MOVE CL-SOC-SEC-NO      TO SS46D.
002410
002411     MOVE CL-CERT-GROUPING       TO W-CERT-GROUPING
002412                                    W-ACCT-GROUPING
002413                                    W-PROD-GROUPING
002414                                    W-PLCY-GROUPING
002415                                    W-PLAN-GROUPING.
002416     MOVE CL-CERT-STATE          TO W-CERT-STATE
002417                                    W-ACCT-STATE
002418                                    W-PROD-STATE
002419                                    W-PLCY-STATE
002420                                    W-PLAN-STATE.
002421     MOVE CL-CERT-ACCOUNT        TO W-CERT-ACCOUNT
002422                                    W-ACCT-ACCOUNT
002423                                    W-PROD-PRODUCER
002424                                    W-PLCY-PRODUCER
002425                                    W-PLAN-PRODUCER.
002426     MOVE CL-CERT-EFF-DT         TO W-CERT-EFF-DT
002427                                    W-PLCY-EFF-DT.
002428     MOVE CL-CV-REFERENCE-NO     TO W-PLCY-REFERENCE-NO.
002429
002430     PERFORM 7400-MOVE-NAME THRU 7400-EXIT.
002431
002432     IF  LOWER-CASE-LETTERS-USED
002433         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002434         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002435         MOVE W-TEMP-AREA2       TO SS10D
002436
002437     ELSE
002438         MOVE W-NAME-WORK        TO SS10D.
002439
002440     MOVE CL-INSURED-1ST-NAME    TO W-FIRST-NAME.
002441     MOVE CL-INSURED-LAST-NAME   TO W-LAST-NAME.
002442     MOVE CL-INSURED-MID-INIT    TO W-MIDDLE-NAME.
002443     PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
002444
002445     IF  LOWER-CASE-LETTERS-USED
002446         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002447         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002448         MOVE W-TEMP-AREA2       TO SS39D
002449
002450     ELSE
002451         MOVE W-NAME-WORK        TO SS39D.
002452
002453     MOVE CL-INSURED-LAST-NAME   TO SS40D.
002454
002455     IF  INSURED-IS-FEMALE
002456         MOVE 'MS.'              TO SS41D
002457
002458     ELSE
002459         MOVE 'MR.'              TO SS41D.
002460
002461     IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
002462         PERFORM 7370-RESOLVE-CREDITOR THRU 7370-EXIT.
002463
002464     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
002465         GO TO 7010-READ-POLICY-RECORD.
002466                                 EJECT
002467     
      * EXEC CICS HANDLE CONDITION
002468*         NOTOPEN   (7022-CERT-NOT-OPEN)
002469*         NOTFND    (7024-CERT-NOT-FOUND)
002470*    END-EXEC.
      *    MOVE '"$JI                  ! / #00008159' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303038313539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002471
002472     
      * EXEC CICS READ
002473*         DATASET   (W-CERT-ID)
002474*         SET       (ADDRESS OF CERTIFICATE-MASTER)
002475*         RIDFLD    (W-CERT-KEY)
002476*    END-EXEC.
      *    MOVE '&"S        E          (   #00008164' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038313634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002477
002478*    SERVICE RELOAD CERTIFICATE-MASTER.
002479
002480     MOVE CM-CERT-NO             TO SS26D.
002481     MOVE CM-INSURED-ISSUE-AGE   TO SS33D.
002482     MOVE CM-LOAN-BALANCE        TO SS37D.
002483     MOVE CM-MEMBER-NO           TO SS38D.
002484
002485     IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
002486         MOVE CL-CURRENT-CARRIER TO SS23D
002487         MOVE CL-CURRENT-GROUPING
002488                                 TO SS24D
002489         MOVE CL-CURRENT-ACCOUNT TO SS25D
002490         MOVE CM-MEMBER-NO       TO SS36D
002491         MOVE CM-GROUPING        TO W-GROUPING
002492
002493         IF  W-GROUP-3 EQUAL 'C01' OR 'C02'
002494             MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO
002495             MOVE CM-LOAN-NUMBER TO W-LOAN-NUMBER
002496             MOVE W-CREDIT-CARD-LOAN-NO
002497                                 TO SS36-1D
002498
002499         ELSE
002500             MOVE CM-MEMBER-NO   TO SS36-1D
002501
002502     ELSE
002503         MOVE CM-CARRIER         TO SS23D
002504         MOVE CM-GROUPING        TO SS24D
002505         MOVE CM-ACCOUNT         TO SS25D
002506         MOVE CM-LOAN-NUMBER     TO SS36D.
002507
002508     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
002509     MOVE SPACES                 TO DC-OPTION-CODE.
002510     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002511
002512     IF  NO-CONVERSION-ERROR
002513         MOVE DC-GREG-DATE-1-EDIT TO SS27D.
002514
002515     MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
002516     MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.
002517     MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.
002518     PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
002519
002520     IF  LOWER-CASE-LETTERS-USED
002521         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002522         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002523         MOVE W-TEMP-AREA2       TO SS57D
002524
002525     ELSE
002526         MOVE W-NAME-WORK        TO SS57D.
002527
002528     MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.
002529     MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.
002530     MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.
002531     PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
002532
002533     IF  LOWER-CASE-LETTERS-USED
002534         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002535         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002536         MOVE W-TEMP-AREA2       TO SS58D
002537
002538     ELSE
002539         MOVE W-NAME-WORK        TO SS58D.
002540
002541     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
002542         MOVE CM-LF-BENEFIT-CD   TO W-BEN-HOLD
002543         MOVE CM-LF-ORIG-TERM    TO SS29D
002544         MOVE CM-LF-BENEFIT-AMT  TO SS30D
002545         MOVE CM-POLICY-FORM-NO  TO SS32D
002546         MOVE CM-LF-CANCEL-DT    TO DC-BIN-DATE-1
002547
002548     ELSE
002549         MOVE CM-AH-BENEFIT-CD   TO W-BEN-HOLD
002550         MOVE CM-AH-ORIG-TERM    TO SS29D
002551         MOVE CM-AH-BENEFIT-AMT  TO SS30D
002552         MOVE CM-POLICY-FORM-NO  TO SS32D
002553         MOVE CM-AH-CANCEL-DT    TO DC-BIN-DATE-1.
002554
002555     MOVE ZEROS                  TO W-WORK-AMOUNT.
002556     COMPUTE W-WORK-AMOUNT =
002557        (CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT).
002558     MOVE W-WORK-AMOUNT          TO SS51D.
002559
002560     MOVE SPACES                 TO DC-OPTION-CODE.
002561     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002562     IF  NO-CONVERSION-ERROR
002563         MOVE DC-GREG-DATE-1-EDIT TO SS31D.
002564
002565     MOVE ' '                    TO DC-OPTION-CODE.
002566     MOVE +0                     TO DC-ELAPSED-MONTHS
002567                                          DC-ELAPSED-DAYS.
002568
002569     IF  CL-CLAIM-TYPE EQUAL     TO PI-LIFE-OVERRIDE-L1 OR 'O'
002570         MOVE CM-LF-LOAN-EXPIRE-DT
002571                                 TO DC-BIN-DATE-1
002572         PERFORM 9700-DATE-LINK THRU 9700-EXIT
002573
002574         IF  NO-CONVERSION-ERROR
002575             MOVE DC-GREG-DATE-1-EDIT TO SS28D
002576
002577         ELSE
002578             NEXT SENTENCE
002579
002580     ELSE
002581         MOVE CM-AH-LOAN-EXPIRE-DT
002582                                 TO DC-BIN-DATE-1
002583         PERFORM 9700-DATE-LINK THRU 9700-EXIT
002584
002585         IF  NO-CONVERSION-ERROR
002586             MOVE DC-GREG-DATE-1-EDIT
002587                                 TO SS28D.
002588
002589     GO TO 7015-READ-DIAGNOSIS-TRAILER.
002590
002591     EJECT
002592 7010-READ-POLICY-RECORD.
002593
002594     
      * EXEC CICS HANDLE CONDITION
002595*         NOTOPEN    (7026-PLCY-NOT-OPEN)
002596*         NOTFND     (7028-PLCY-NOT-FOUND)
002597*    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00008286' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303038323836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002598
002599     
      * EXEC CICS READ
002600*         DATASET  (W-PLCY-ID)
002601*         SET      (ADDRESS OF POLICY-MASTER)
002602*         RIDFLD   (W-PLCY-KEY)
002603*    END-EXEC.
      *    MOVE '&"S        E          (   #00008291' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038323931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PLCY-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002604
002605*    SERVICE RELOAD POLICY-MASTER.
002606
002607     MOVE PM-REFERENCE-NUMBER    TO SS59D.
002608     MOVE PM-INSURED-ISSUE-AGE   TO SS33D.
002609     MOVE PM-LOAN-BALC           TO SS37D.
002610     MOVE PM-CARRIER             TO SS23D.
002611     MOVE PM-GROUPING            TO SS24D.
002612     MOVE PM-PRODUCER            TO SS25D.
002613     MOVE PM-LOAN-NUMBER         TO SS36-1D.
002614
002615     MOVE PM-POLICY-EFF-DT       TO DC-BIN-DATE-1.
002616     MOVE SPACES                 TO DC-OPTION-CODE.
002617     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002618
002619     IF NO-CONVERSION-ERROR
002620         MOVE DC-GREG-DATE-1-EDIT TO SS27D.
002621
002622     MOVE PM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
002623     MOVE PM-INSURED-LAST-NAME   TO W-LAST-NAME.
002624     MOVE PM-INSURED-MIDDLE-INIT TO W-MIDDLE-NAME.
002625     PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
002626
002627     IF LOWER-CASE-LETTERS-USED
002628         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002629         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002630         MOVE W-TEMP-AREA2       TO SS57D
002631     ELSE
002632         MOVE W-NAME-WORK        TO SS57D.
002633
002634     MOVE PM-JOINT-FIRST-NAME    TO W-FIRST-NAME.
002635     MOVE PM-JOINT-LAST-NAME     TO W-LAST-NAME.
002636     MOVE PM-JOINT-MIDDLE-INIT   TO W-MIDDLE-NAME.
002637     PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
002638
002639     IF LOWER-CASE-LETTERS-USED
002640         MOVE W-NAME-WORK        TO W-TEMP-AREA2
002641         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
002642         MOVE W-TEMP-AREA2       TO SS58D
002643     ELSE
002644         MOVE W-NAME-WORK        TO SS58D.
002645
002646     MOVE PM-INS-PLAN-CD         TO W-BEN-HOLD.
002647     MOVE PM-LOAN-TERM           TO SS29D.
002648     MOVE PM-INS-POLICY-FORM     TO SS32D.
002649
002650     IF PM-AH-MORT-PLAN
002651         MOVE PM-INS-MONTH-BENEFIT TO SS30D
002652         MOVE ZEROS                TO W-WORK-AMOUNT
002653         COMPUTE W-WORK-AMOUNT =
002654             (PM-INS-MONTH-BENEFIT * PM-LOAN-TERM)
002655         MOVE W-WORK-AMOUNT        TO SS51D
002656     ELSE
002657         MOVE PM-INS-TOTAL-BENEFIT TO SS30D.
002658
002659     MOVE PM-CANCEL-DT           TO DC-BIN-DATE-1.
002660     MOVE SPACES                 TO DC-OPTION-CODE.
002661     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002662
002663     IF NO-CONVERSION-ERROR
002664         MOVE DC-GREG-DATE-1-EDIT TO SS31D.
002665
002666     MOVE PM-INS-TERMINATION-DT  TO DC-BIN-DATE-1.
002667     MOVE SPACES                 TO DC-OPTION-CODE.
002668     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
002669
002670     IF NO-CONVERSION-ERROR
002671         MOVE DC-GREG-DATE-1-EDIT TO SS28D.
002672
002673     EJECT
002674 7015-READ-DIAGNOSIS-TRAILER.
002675     
      * EXEC CICS HANDLE CONDITION
002676*         NOTOPEN    (7020-ACTV-NOT-OPEN)
002677*         NOTFND     (7040-READ-BENEFICIARY)
002678*    END-EXEC.
      *    MOVE '"$JI                  ! 1 #00008367' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303038333637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002679
002680     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-KEY.
002681     MOVE +90                    TO W-ACTV-SEQ.
002682
002683     
      * EXEC CICS READ
002684*         DATASET  (W-ACTV-ID)
002685*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
002686*         RIDFLD   (W-ACTV-KEY)
002687*    END-EXEC.
      *    MOVE '&"S        E          (   #00008375' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038333735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002688
002689*    SERVICE RELOAD ACTIVITY-TRAILERS
002690
002691     IF  AT-TRAILER-TYPE EQUAL '6'
002692         MOVE AT-INFO-LINE-1     TO SS19D.
002693
002694     GO TO 7040-READ-BENEFICIARY.
002695                                 EJECT
002696 7020-ACTV-NOT-OPEN.
002697
002698     MOVE ER-0172                TO W-CURRENT-ERROR.
002699     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002700     GO TO 7399-EXIT.
002701
002702 7022-CERT-NOT-OPEN.
002703
002704     MOVE ER-0169                TO W-CURRENT-ERROR.
002705     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002706     GO TO 7399-EXIT.
002707
002708 7024-CERT-NOT-FOUND.
002709
002710     MOVE ER-0206                TO W-CURRENT-ERROR.
002711     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002712     GO TO 7399-EXIT.
002713
002714 7026-PLCY-NOT-OPEN.
002715
002716     MOVE ER-9883                TO W-CURRENT-ERROR.
002717     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002718     GO TO 7399-EXIT.
002719
002720 7028-PLCY-NOT-FOUND.
002721
002722     MOVE ER-9483                TO W-CURRENT-ERROR.
002723     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
002724     GO TO 7399-EXIT.
002725                                 EJECT
002726
002727 7040-READ-BENEFICIARY.
002728
002729     IF  CL-BENIF-ADDR-CNT EQUAL +0
002730             AND
002731         CL-BENEFICIARY = SPACES
002732         GO TO 7100-READ-PHYSICIAN-ADDR.
002733
002734     MOVE CL-BENIF-ADDR-CNT      TO W-ACTV-SEQ.
002735     ADD +10                     TO W-ACTV-SEQ.
002736
002737     
      * EXEC CICS HANDLE CONDITION
002738*         NOTOPEN    (7380-BENE-NOT-OPEN)
002739*         NOTFND     (7100-READ-PHYSICIAN-ADDR)
002740*    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00008429' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303038343239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002741
002742     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
002743
002744     IF  W-ACTV-SEQ NOT EQUAL +10
002745         GO TO 7060-GET-FROM-ACTIVITY.
002746
002747     MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
002748     MOVE 'B'                    TO W-BENE-REC-TYPE.
002749     MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.
002750
002751     
      * EXEC CICS READ
002752*         DATASET    (W-BENE-ID)
002753*         SET        (ADDRESS OF BENEFICIARY-MASTER)
002754*         RIDFLD     (W-BENE-KEY)
002755*    END-EXEC.
      *    MOVE '&"S        E          (   #00008443' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038343433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002756
002757*    SERVICE RELOAD BENEFICIARY-MASTER.
002758
002759     MOVE BE-MAIL-TO-NAME        TO W-LABEL-LINES (01).
002760     MOVE BE-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
002761     MOVE BE-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
002762     MOVE BE-CITY-STATE          TO W-LABEL-LINES (04).
002763
002764     MOVE SPACES                 TO W-ZIP-CODE.
002765
002766     IF  BE-CANADIAN-POST-CODE
002767         MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1
002768         MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2
002769
002770     ELSE
002771         MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE
002772
002773         IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS
002774             MOVE '-'            TO W-AM-ZIP-DASH
002775             MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
002776
002777     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
002778
002779     MOVE BE-PHONE-NO            TO W-PHONE-IN.
002780
002781     GO TO 7080-SET-PHONE.
002782
002783 7060-GET-FROM-ACTIVITY.
002784
002785     
      * EXEC CICS HANDLE CONDITION
002786*         NOTOPEN    (7390-ACTV-NOT-OPEN)
002787*         NOTFND     (7100-READ-PHYSICIAN-ADDR)
002788*    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00008477' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303038343737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002789
002790     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
002791
002792     
      * EXEC CICS READ
002793*         DATASET  (W-ACTV-ID)
002794*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
002795*         RIDFLD   (W-ACTV-KEY)
002796*    END-EXEC.
      *    MOVE '&"S        E          (   #00008484' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038343834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002797
002798*    SERVICE RELOAD ACTIVITY-TRAILERS.
002799
002800     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
002801     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
002802     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
002803     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
002804
002805     MOVE SPACES                 TO W-ZIP-CODE.
002806
002807     IF  AT-CANADIAN-POST-CODE
002808         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
002809         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
002810
002811     ELSE
002812         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
002813
002814         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
002815             MOVE '-'            TO W-AM-ZIP-DASH
002816             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
002817
002818     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
002819
002820     MOVE AT-PHONE-NO            TO W-PHONE-IN.
002821
002822 7080-SET-PHONE.
002823
002824     MOVE W-PI-AREA              TO W-PO-AREA.
002825     MOVE W-PI-PFX               TO W-PO-PFX.
002826     MOVE W-PI-SFX               TO W-PO-SFX.
002827     MOVE W-PHONE-OUT            TO SS44-5D.
002828
002829     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
002830
002831     MOVE W-LABEL-LINES (01)     TO SS43D.
002832     MOVE W-LABEL-LINES (02)     TO SS44-1D.
002833     MOVE W-LABEL-LINES (03)     TO SS44-2D.
002834     MOVE W-LABEL-LINES (04)     TO SS44-3D.
002835     MOVE W-LABEL-LINES (05)     TO SS44-4D.
002836                                 EJECT
002837 7100-READ-PHYSICIAN-ADDR.
002838
002839     MOVE CL-DOCTOR-ADDR-CNT     TO W-ACTV-SEQ.
002840     ADD +30                     TO W-ACTV-SEQ.
002841
002842     IF  W-ACTV-SEQ EQUAL +30
002843         GO TO 7120-READ-EMPLOYER-ADDR.
002844
002845     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
002846
002847     
      * EXEC CICS HANDLE CONDITION
002848*         NOTOPEN    (7390-ACTV-NOT-OPEN)
002849*         NOTFND     (7120-READ-EMPLOYER-ADDR)
002850*    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00008539' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303038353339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002851
002852     
      * EXEC CICS READ
002853*         DATASET    (W-ACTV-ID)
002854*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
002855*         RIDFLD     (W-ACTV-KEY)
002856*    END-EXEC.
      *    MOVE '&"S        E          (   #00008544' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038353434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002857
002858*    SERVICE RELOAD ACTIVITY-TRAILERS.
002859
002860     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
002861     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
002862     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
002863     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
002864     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
002865
002866     MOVE SPACES                 TO W-ZIP-CODE.
002867
002868     IF  AT-CANADIAN-POST-CODE
002869         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
002870         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
002871
002872     ELSE
002873         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
002874
002875         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
002876             MOVE '-'            TO W-AM-ZIP-DASH
002877             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
002878
002879     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
002880
002881     MOVE ZEROS                  TO W-PHONE-IN.
002882     MOVE AT-PHONE-NO            TO W-PHONE-IN.
002883     MOVE W-PI-AREA              TO W-PO-AREA.
002884     MOVE W-PI-PFX               TO W-PO-PFX.
002885     MOVE W-PI-SFX               TO W-PO-SFX.
002886     MOVE W-PHONE-OUT            TO SS47-5D.
002887
002888     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
002889
002890     MOVE W-LABEL-LINES (01)     TO SS47D.
002891     MOVE W-LABEL-LINES (02)     TO SS47-1D.
002892     MOVE W-LABEL-LINES (03)     TO SS47-2D.
002893     MOVE W-LABEL-LINES (04)     TO SS47-3D.
002894     MOVE W-LABEL-LINES (05)     TO SS47-4D.
002895     GO TO 7120-READ-EMPLOYER-ADDR.
002896                                 EJECT
002897
002898 7120-READ-EMPLOYER-ADDR.
002899
002900     MOVE CL-EMPLOYER-ADDR-CNT   TO W-ACTV-SEQ.
002901     ADD +40                     TO W-ACTV-SEQ.
002902
002903     IF  W-ACTV-SEQ EQUAL +40
002904         GO TO 7140-READ-INSURED-ADDR.
002905
002906     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
002907
002908     
      * EXEC CICS HANDLE CONDITION
002909*         NOTOPEN    (7390-ACTV-NOT-OPEN)
002910*         NOTFND     (7140-READ-INSURED-ADDR)
002911*    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00008600' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303038363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002912
002913     
      * EXEC CICS READ
002914*         DATASET    (W-ACTV-ID)
002915*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
002916*         RIDFLD     (W-ACTV-KEY)
002917*    END-EXEC.
      *    MOVE '&"S        E          (   #00008605' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002918
002919*    SERVICE RELOAD ACTIVITY-TRAILERS.
002920
002921     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
002922     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
002923     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
002924     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
002925     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
002926
002927     MOVE SPACES                 TO W-ZIP-CODE.
002928
002929     IF  AT-CANADIAN-POST-CODE
002930         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
002931         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
002932
002933     ELSE
002934         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
002935
002936         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
002937             MOVE '-'            TO W-AM-ZIP-DASH
002938             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
002939
002940     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
002941
002942     MOVE ZEROS                  TO W-PHONE-IN.
002943     MOVE AT-PHONE-NO            TO W-PHONE-IN.
002944     MOVE W-PI-AREA              TO W-PO-AREA.
002945     MOVE W-PI-PFX               TO W-PO-PFX.
002946     MOVE W-PI-SFX               TO W-PO-SFX.
002947     MOVE W-PHONE-OUT            TO SS48-5D.
002948
002949     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
002950
002951     MOVE W-LABEL-LINES (01)     TO SS48D.
002952     MOVE W-LABEL-LINES (02)     TO SS48-1D.
002953     MOVE W-LABEL-LINES (03)     TO SS48-2D.
002954     MOVE W-LABEL-LINES (04)     TO SS48-3D.
002955     MOVE W-LABEL-LINES (05)     TO SS48-4D.
002956
002957                                 EJECT
002958 7140-READ-INSURED-ADDR.
002959
002960     MOVE CL-INSURED-ADDR-CNT    TO W-ACTV-SEQ.
002961
002962     IF  W-ACTV-SEQ EQUAL +0
002963         GO TO 7160-READ-OTHER1-ADDR.
002964
002965     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
002966
002967     
      * EXEC CICS HANDLE CONDITION
002968*         NOTOPEN   (7390-ACTV-NOT-OPEN)
002969*         NOTFND    (7160-READ-OTHER1-ADDR)
002970*         END-EXEC.
      *    MOVE '"$JI                  ! 6 #00008659' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303038363539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002971
002972     
      * EXEC CICS READ
002973*         DATASET   (W-ACTV-ID)
002974*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
002975*         RIDFLD    (W-ACTV-KEY)
002976*         END-EXEC.
      *    MOVE '&"S        E          (   #00008664' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038363634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002977
002978*    SERVICE RELOAD ACTIVITY-TRAILERS.
002979
002980     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
002981     MOVE SS10D                  TO W-LABEL-LINES (01).
002982     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
002983     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
002984     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
002985
002986     MOVE SPACES                 TO W-ZIP-CODE.
002987
002988     IF  AT-CANADIAN-POST-CODE
002989         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
002990         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
002991
002992     ELSE
002993         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
002994
002995         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
002996             MOVE '-'            TO W-AM-ZIP-DASH
002997             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
002998
002999     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003000
003001     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003002
003003     MOVE W-LABEL-LINES (01)     TO SS10D.
003004     MOVE W-LABEL-LINES (02)     TO SS11-1D.
003005     MOVE W-LABEL-LINES (03)     TO SS11-2D.
003006     MOVE W-LABEL-LINES (04)     TO SS11-3D.
003007     MOVE W-LABEL-LINES (05)     TO SS11-4D.
003008     MOVE AT-MAIL-TO-NAME        TO SS11-5D.
003009
003010     MOVE ZEROS                  TO W-PHONE-IN.
003011     MOVE AT-PHONE-NO            TO W-PHONE-IN.
003012     MOVE W-PI-AREA              TO W-PO-AREA.
003013     MOVE W-PI-PFX               TO W-PO-PFX.
003014     MOVE W-PI-SFX               TO W-PO-SFX.
003015     MOVE W-PHONE-OUT            TO SS11-6D.
003016
003017                                 EJECT
003018 7160-READ-OTHER1-ADDR.
003019
003020     MOVE CL-OTHER-1-ADDR-CNT    TO W-ACTV-SEQ.
003021     ADD +50                     TO W-ACTV-SEQ.
003022
003023     IF  W-ACTV-SEQ EQUAL +50
003024         GO TO 7180-READ-OTHER2-ADDR.
003025
003026     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
003027
003028     
      * EXEC CICS HANDLE CONDITION
003029*         NOTOPEN    (7390-ACTV-NOT-OPEN)
003030*         NOTFND     (7180-READ-OTHER2-ADDR)
003031*         END-EXEC.
      *    MOVE '"$JI                  ! 7 #00008720' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303038373230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003032
003033     
      * EXEC CICS READ
003034*         DATASET    (W-ACTV-ID)
003035*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
003036*         RIDFLD     (W-ACTV-KEY)
003037*         END-EXEC.
      *    MOVE '&"S        E          (   #00008725' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003038
003039*    SERVICE RELOAD ACTIVITY-TRAILERS.
003040
003041     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003042     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
003043     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
003044     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
003045     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
003046
003047     MOVE SPACES                 TO W-ZIP-CODE.
003048
003049     IF  AT-CANADIAN-POST-CODE
003050         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003051         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003052
003053     ELSE
003054         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
003055
003056         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003057             MOVE '-'            TO W-AM-ZIP-DASH
003058             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003059
003060     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003061
003062     MOVE ZEROS                  TO W-PHONE-IN.
003063     MOVE AT-PHONE-NO            TO W-PHONE-IN.
003064     MOVE W-PI-AREA              TO W-PO-AREA.
003065     MOVE W-PI-PFX               TO W-PO-PFX.
003066     MOVE W-PI-SFX               TO W-PO-SFX.
003067     MOVE W-PHONE-OUT            TO SS49-5D.
003068
003069     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003070
003071     MOVE W-LABEL-LINES (01)     TO SS49D.
003072     MOVE W-LABEL-LINES (02)     TO SS49-1D.
003073     MOVE W-LABEL-LINES (03)     TO SS49-2D.
003074     MOVE W-LABEL-LINES (04)     TO SS49-3D.
003075     MOVE W-LABEL-LINES (05)     TO SS49-4D.
003076                                 EJECT
003077 7180-READ-OTHER2-ADDR.
003078
003079     MOVE CL-OTHER-2-ADDR-CNT    TO W-ACTV-SEQ.
003080     ADD +60                     TO W-ACTV-SEQ.
003081
003082     IF  W-ACTV-SEQ EQUAL +60
003083         GO TO 7200-NOT-FOUND.
003084
003085     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
003086
003087     
      * EXEC CICS HANDLE CONDITION
003088*         NOTOPEN    (7390-ACTV-NOT-OPEN)
003089*         NOTFND     (7200-NOT-FOUND)
003090*         END-EXEC.
      *    MOVE '"$JI                  ! 8 #00008779' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303038373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003091
003092     
      * EXEC CICS READ
003093*         DATASET    (W-ACTV-ID)
003094*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
003095*         RIDFLD     (W-ACTV-KEY)
003096*         END-EXEC.
      *    MOVE '&"S        E          (   #00008784' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003097
003098*    SERVICE RELOAD ACTIVITY-TRAILERS.
003099
003100     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003101     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
003102     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
003103     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
003104     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
003105
003106     MOVE SPACES                 TO W-ZIP-CODE.
003107
003108     IF  AT-CANADIAN-POST-CODE
003109         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003110         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003111
003112     ELSE
003113         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
003114
003115         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003116             MOVE '-'            TO W-AM-ZIP-DASH
003117             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003118
003119     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003120
003121     MOVE ZEROS                  TO W-PHONE-IN.
003122     MOVE AT-PHONE-NO            TO W-PHONE-IN.
003123     MOVE W-PI-AREA              TO W-PO-AREA.
003124     MOVE W-PI-PFX               TO W-PO-PFX.
003125     MOVE W-PI-SFX               TO W-PO-SFX.
003126     MOVE W-PHONE-OUT            TO SS50-5D.
003127
003128     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003129
003130     MOVE W-LABEL-LINES (01)     TO SS50D.
003131     MOVE W-LABEL-LINES (02)     TO SS50-1D.
003132     MOVE W-LABEL-LINES (03)     TO SS50-2D.
003133     MOVE W-LABEL-LINES (04)     TO SS50-3D.
003134     MOVE W-LABEL-LINES (05)     TO SS50-4D.
003135                                 EJECT
003136 7200-NOT-FOUND.
003137
003138     IF  ACCOUNT-IS-ONLINE
003139         GO TO 7220-READ-ACCOUNT.
003140
003141     MOVE CL-ACCOUNT-ADDR-CNT    TO W-ACTV-SEQ.
003142     ADD +20                     TO W-ACTV-SEQ.
003143
003144     IF  W-ACTV-SEQ EQUAL +20
003145         GO TO 7220-READ-ACCOUNT.
003146
003147     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
003148
003149     
      * EXEC CICS HANDLE CONDITION
003150*         NOTOPEN    (7390-ACTV-NOT-OPEN)
003151*         NOTFND     (7220-READ-ACCOUNT)
003152*         END-EXEC.
      *    MOVE '"$JI                  ! 9 #00008841' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303038383431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003153
003154     
      * EXEC CICS READ
003155*         DATASET    (W-ACTV-ID)
003156*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
003157*         RIDFLD     (W-ACTV-KEY)
003158*         END-EXEC.
      *    MOVE '&"S        E          (   #00008846' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038383436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003159
003160*    SERVICE RELOAD ACTIVITY-TRAILERS.
003161
003162     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003163     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
003164     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
003165     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
003166     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
003167
003168     MOVE SPACES                 TO W-ZIP-CODE.
003169
003170     IF  AT-CANADIAN-POST-CODE
003171         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003172         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003173
003174     ELSE
003175         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
003176
003177         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003178             MOVE '-'            TO W-AM-ZIP-DASH
003179             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003180
003181     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003182
003183     MOVE ZEROS                  TO W-PHONE-IN.
003184     MOVE AT-PHONE-NO            TO W-PHONE-IN.
003185     MOVE W-PI-AREA              TO W-PO-AREA.
003186     MOVE W-PI-PFX               TO W-PO-PFX.
003187     MOVE W-PI-SFX               TO W-PO-SFX.
003188     MOVE W-PHONE-OUT            TO SS07-6D.
003189
003190     IF  PI-COMPANY-ID EQUAL     TO 'FLA'
003191         NEXT SENTENCE
003192
003193     ELSE
003194         PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003195
003196     MOVE W-LABEL-LINES (01)     TO SS06D.
003197     MOVE W-LABEL-LINES (02)     TO SS07-1D.
003198     MOVE W-LABEL-LINES (03)     TO SS07-2D.
003199     MOVE W-LABEL-LINES (04)     TO SS07-3D.
003200     MOVE W-LABEL-LINES (05)     TO SS07-4D.
003201     MOVE W-LABEL-LINES (06)     TO SS07-5D.
003202                                 EJECT
003203 7220-READ-ACCOUNT.
003204
003205     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
003206         GO TO 7230-READ-PRODUCER.
003207
003208     IF  W-ACCT-READ-SW EQUAL 'Y'
003209         GO TO 7228-BUILD-ACCT-ADDR.
003210
003211     MOVE CM-CERT-EFF-DT         TO W-ACCT-EXP-DATE.
003212
003213     
      * EXEC CICS HANDLE CONDITION
003214*         NOTOPEN   (7375-ACCT-NOT-OPEN)
003215*         NOTFND    (7240-READ-3RD-PARTY)
003216*         END-EXEC.
      *    MOVE '"$JI                  ! : #00008905' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303038393035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003217
003218 7222-STARTBR-ACCOUNT.
003219
003220     MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
003221     PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.
003222
003223 7226-READNEXT-ACCOUNT.
003224
003225     PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.
003226
003227     IF  W-ACCT-PARTIAL-KEY IS NOT EQUAL W-ACCT-SAVE-KEY
003228
003229         IF  W-SAVE-ACCT-RECORD EQUAL SPACES
003230             GO TO 7240-READ-3RD-PARTY
003231
003232         ELSE
003233             MOVE AM-CONTROL-PRIMARY  TO W-ACCT-KEY
003234             MOVE W-SAVE-ACCT-RECORD TO ACCOUNT-MASTER
003235             GO TO 7228-BUILD-ACCT-ADDR.
003236
003237     IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES
003238         MOVE AM-CONTROL-PRIMARY TO W-ACCT-KEY
003239
003240     ELSE
003241         MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD
003242         GO TO 7226-READNEXT-ACCOUNT.
003243
003244 7228-BUILD-ACCT-ADDR.
003245
003246     MOVE SPACES                 TO W-SAVE-ACCT-RECORD.
003247
003248     IF  NOT ACCOUNT-IS-ONLINE
003249         GO TO 7240-READ-3RD-PARTY.
003250
003251     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003252     MOVE AM-NAME                TO W-LABEL-LINES (01).
003253     MOVE AM-PERSON              TO W-LABEL-LINES (02).
003254     MOVE AM-ADDRS               TO W-LABEL-LINES (03).
003255     MOVE AM-CITY                TO W-LABEL-LINES (04).
003256
003257     MOVE SPACES                 TO W-ZIP-CODE.
003258
003259     IF  AM-CANADIAN-POST-CODE
003260         MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003261         MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003262
003263     ELSE
003264         MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE
003265
003266         IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003267             MOVE '-'            TO W-AM-ZIP-DASH
003268             MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003269
003270     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003271
003272     MOVE ZEROS                  TO W-PHONE-IN.
003273     MOVE AM-AREA-CODE           TO W-PO-AREA.
003274     MOVE AM-TEL-PRE             TO W-PO-PFX.
003275     MOVE AM-TEL-NBR             TO W-PO-SFX.
003276     MOVE W-PHONE-OUT            TO SS07-6D.
003277
003278     IF  PI-COMPANY-ID EQUAL     TO 'FLA'
003279         NEXT SENTENCE
003280
003281     ELSE
003282         PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003283
003284     MOVE W-LABEL-LINES (01)     TO SS06D.
003285     MOVE W-LABEL-LINES (02)     TO SS07-1D.
003286     MOVE W-LABEL-LINES (03)     TO SS07-2D.
003287     MOVE W-LABEL-LINES (04)     TO SS07-3D.
003288     MOVE W-LABEL-LINES (05)     TO SS07-4D.
003289     MOVE W-LABEL-LINES (06)     TO SS07-5D.
003290
003291     GO TO 7240-READ-3RD-PARTY.
003292                                 EJECT
003293 7230-READ-PRODUCER.
003294
003295     IF W-PROD-READ-SW IS EQUAL TO 'Y'
003296         GO TO 7238-BUILD-PROD-ADDR.
003297
003298     MOVE PM-POLICY-EFF-DT       TO W-PROD-EXP-DATE.
003299
003300     
      * EXEC CICS HANDLE CONDITION
003301*         NOTOPEN   (7395-PROD-NOT-OPEN)
003302*         NOTFND    (7300-READ-DENIAL)
003303*    END-EXEC.
      *    MOVE '"$JI                  ! ; #00008992' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303038393932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003304
003305 7232-STARTBR-PRODUCER.
003306
003307     MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.
003308     PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.
003309
003310 7236-READNEXT-PRODUCER.
003311
003312     PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.
003313
003314     IF W-PROD-PARTIAL-KEY IS NOT EQUAL W-PROD-SAVE-KEY
003315         IF W-SAVE-PROD-RECORD IS EQUAL TO SPACES
003316             GO TO 7300-READ-DENIAL
003317         ELSE
003318             MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY
003319             MOVE W-SAVE-PROD-RECORD TO PRODUCER-MASTER
003320             GO TO 7238-BUILD-PROD-ADDR.
003321
003322     IF PD-EXPIRE-DATE IS EQUAL TO HIGH-VALUES
003323         MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY
003324     ELSE
003325         MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD
003326         GO TO 7236-READNEXT-PRODUCER.
003327
003328 7238-BUILD-PROD-ADDR.
003329
003330     MOVE SPACES                 TO W-SAVE-PROD-RECORD.
003331
003332     IF NOT ACCOUNT-IS-ONLINE
003333         GO TO 7300-READ-DENIAL.
003334
003335     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003336     MOVE PD-NAME                TO W-LABEL-LINES (01).
003337     MOVE PD-PERSON              TO W-LABEL-LINES (02).
003338     MOVE PD-ADDRS               TO W-LABEL-LINES (03).
003339     MOVE PD-CITY                TO W-LABEL-LINES (04).
003340
003341     MOVE SPACES                 TO W-ZIP-CODE.
003342
003343     MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.
003344     IF (PD-ZIP-PLUS4 IS NOT EQUAL TO SPACES AND ZEROS)
003345         MOVE '-'                TO W-AM-ZIP-DASH
003346         MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.
003347
003348     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003349
003350     MOVE ZEROS                  TO W-PHONE-IN.
003351     MOVE PD-AREA-CODE           TO W-PO-AREA.
003352     MOVE PD-TEL-PRE             TO W-PO-PFX.
003353     MOVE PD-TEL-NBR             TO W-PO-SFX.
003354     MOVE W-PHONE-OUT            TO SS07-6D.
003355
003356     IF PI-COMPANY-ID IS EQUAL TO 'FLA'
003357         NEXT SENTENCE
003358     ELSE
003359         PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003360
003361     MOVE W-LABEL-LINES (01)     TO SS06D.
003362     MOVE W-LABEL-LINES (02)     TO SS07-1D.
003363     MOVE W-LABEL-LINES (03)     TO SS07-2D.
003364     MOVE W-LABEL-LINES (04)     TO SS07-3D.
003365     MOVE W-LABEL-LINES (05)     TO SS07-4D.
003366     MOVE W-LABEL-LINES (06)     TO SS07-5D.
003367
003368     GO TO 7300-READ-DENIAL.
003369                                 EJECT
003370 7240-READ-3RD-PARTY.
003371
003372     MOVE +29                    TO W-ACTV-SEQ.
003373
003374     
      * EXEC CICS HANDLE CONDITION
003375*         NOTOPEN    (7390-ACTV-NOT-OPEN)
003376*         NOTFND     (7260-READ-COMP)
003377*    END-EXEC.
      *    MOVE '"$JI                  ! < #00009066' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303039303636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003378
003379     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
003380
003381     
      * EXEC CICS READ
003382*         DATASET    (W-ACTV-ID)
003383*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
003384*         RIDFLD     (W-ACTV-KEY)
003385*    END-EXEC.
      *    MOVE '&"S        E          (   #00009073' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039303733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003386
003387*    SERVICE RELOAD ACTIVITY-TRAILERS.
003388
003389     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003390     MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
003391     MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
003392     MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
003393     MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
003394
003395     MOVE SPACES                 TO W-ZIP-CODE.
003396
003397     IF  AT-CANADIAN-POST-CODE
003398         MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003399         MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003400
003401     ELSE
003402         MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
003403
003404         IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003405             MOVE '-'            TO W-AM-ZIP-DASH
003406             MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003407
003408     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003409
003410     MOVE ZEROS                  TO W-PHONE-IN.
003411     MOVE AT-PHONE-NO            TO W-PHONE-IN.
003412     MOVE W-PI-AREA              TO W-PO-AREA.
003413     MOVE W-PI-PFX               TO W-PO-PFX.
003414     MOVE W-PI-SFX               TO W-PO-SFX.
003415     MOVE W-PHONE-OUT            TO SS53-6D.
003416
003417     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003418
003419     MOVE W-LABEL-LINES (01)     TO SS52D.
003420     MOVE W-LABEL-LINES (02)     TO SS53-1D.
003421     MOVE W-LABEL-LINES (03)     TO SS53-2D.
003422     MOVE W-LABEL-LINES (04)     TO SS53-3D.
003423     MOVE W-LABEL-LINES (05)     TO SS53-4D.
003424     MOVE W-LABEL-LINES (06)     TO SS53-5D.
003425
003426     GO TO 7300-READ-DENIAL.
003427                                 EJECT
003428 7260-READ-COMP.
003429
003430     IF  W-COMP-READ-SW EQUAL 'Y'
003431         GO TO 7280-BUILD-COMP-ADDR.
003432
003433     IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
003434         MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL
003435         GO TO 7300-READ-DENIAL.
003436
003437     IF  AM-3RD-PARTY-NOTIF-LEVEL GREATER THAN 00 AND
003438             LESS THAN 11
003439         NEXT SENTENCE
003440
003441     ELSE
003442         GO TO 7300-READ-DENIAL.
003443
003444     IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
003445         GO TO 7300-READ-DENIAL.
003446
003447     MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD
003448     MOVE AM-CARRIER             TO W-COMP-CARRIER
003449     MOVE AM-GROUPING            TO W-COMP-GROUPING
003450     MOVE 'A'                    TO W-COMP-TYPE
003451     MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
003452                                 TO W-COMP-RESP-NO.
003453
003454     IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO
003455         IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR
003456                                           'G' OR 'B' or 'S'
003457             MOVE 'G'            TO W-COMP-TYPE
003458             MOVE LOW-VALUES     TO W-COMP-ACCOUNT
003459         ELSE
003460             MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
003461                                 TO W-COMP-ACCOUNT
003462     ELSE
003463         MOVE 'G'                TO W-COMP-TYPE
003464         MOVE LOW-VALUES         TO W-COMP-ACCOUNT.
003465
003466     IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
003467         MOVE ZEROS              TO W-COMP-CARRIER.
003468
003469     IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
003470         MOVE ZEROS              TO W-COMP-GROUPING.
003471
003472     
      * EXEC CICS HANDLE CONDITION
003473*         NOTFND    (7300-READ-DENIAL)
003474*    END-EXEC.
      *    MOVE '"$I                   ! = #00009164' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303039313634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003475
003476     
      * EXEC CICS  READ
003477*         SET      (ADDRESS OF COMPENSATION-MASTER)
003478*         DATASET  ('ERCOMP')
003479*         RIDFLD   (W-COMP-KEY)
003480*    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009168' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039313638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003481
003482*    SERVICE RELOAD COMPENSATION-MASTER.
003483
003484 7280-BUILD-COMP-ADDR.
003485
003486     MOVE SPACES                 TO W-LABEL-HOLD-AREA
003487     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)
003488
003489     IF  CO-ACCT-NAME EQUAL SPACES
003490         MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).
003491
003492     MOVE CO-ADDR-1              TO W-LABEL-LINES (02).
003493     MOVE CO-ADDR-2              TO W-LABEL-LINES (03).
003494     MOVE CO-ADDR-3              TO W-LABEL-LINES (04).
003495
003496     MOVE SPACES                 TO W-ZIP-CODE.
003497
003498     IF  CO-CANADIAN-POST-CODE
003499         MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003500         MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003501
003502     ELSE
003503         MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE
003504
003505         IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003506             MOVE '-'            TO W-AM-ZIP-DASH
003507             MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003508
003509     MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
003510
003511     MOVE ZEROS                  TO W-PHONE-IN.
003512     MOVE CO-AREA-CODE           TO W-PO-AREA.
003513     MOVE CO-PREFIX              TO W-PO-PFX.
003514     MOVE CO-PHONE               TO W-PO-SFX.
003515     MOVE W-PHONE-OUT            TO SS53-6D.
003516
003517     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003518
003519     MOVE W-LABEL-LINES (01)     TO SS52D.
003520     MOVE W-LABEL-LINES (02)     TO SS53-1D.
003521     MOVE W-LABEL-LINES (03)     TO SS53-2D.
003522     MOVE W-LABEL-LINES (04)     TO SS53-3D.
003523     MOVE W-LABEL-LINES (05)     TO SS53-4D.
003524     MOVE W-LABEL-LINES (06)     TO SS53-5D.
003525                                 EJECT
003526 7300-READ-DENIAL.
003527
003528     MOVE +90                    TO W-ACTV-SEQ.
003529
003530     MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
003531
003532     
      * EXEC CICS HANDLE CONDITION
003533*         NOTOPEN    (7390-ACTV-NOT-OPEN)
003534*         NOTFND     (7340-READ-CNTL1)
003535*         ENDFILE    (7340-READ-CNTL1)
003536*         END-EXEC.
      *    MOVE '"$JI''                 ! > #00009224' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'3E20233030303039323234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003537
003538     
      * EXEC CICS STARTBR
003539*         DATASET    (W-ACTV-ID)
003540*         RIDFLD     (W-ACTV-KEY)
003541*         GTEQ
003542*         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009230' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039323330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003543
003544     MOVE 'Y'                    TO W-ACTV-BROWSE-STARTED.
003545     MOVE W-ACTV-PARTIAL-KEY     TO W-ACTV-SAVE-KEY.
003546
003547 7304-READ-NEXT.
003548
003549     
      * EXEC CICS READNEXT
003550*         SET     (ADDRESS OF ACTIVITY-TRAILERS)
003551*         RIDFLD  (W-ACTV-KEY)
003552*         DATASET (W-ACTV-ID)
003553*         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009241' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039323431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003554
003555*    SERVICE RELOAD ACTIVITY-TRAILERS.
003556
003557     IF  W-ACTV-PARTIAL-KEY NOT = W-ACTV-SAVE-KEY
003558         GO TO 7340-READ-CNTL1.
003559
003560     IF  AT-TRAILER-TYPE NOT = '8'
003561         GO TO 7304-READ-NEXT.
003562
003563     MOVE AT-DENIAL-INFO-1       TO SS35-1D.
003564     MOVE AT-DENIAL-INFO-2       TO SS35-2D.
003565                                 EJECT
003566 7340-READ-CNTL1.
003567
003568     IF  W-ACTV-BROWSE-STARTED = 'Y'
003569         MOVE 'N'                TO W-ACTV-BROWSE-STARTED
003570         
      * EXEC CICS ENDBR
003571*             DATASET    (W-ACTV-ID)
003572*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009262' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039323632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003573
003574     IF  SS35-1D = ALL '*'
003575         MOVE '@@DENIAL1'        TO SS35-1D.
003576
003577     IF  SS35-2D = ALL '*'
003578         MOVE '@@DENIAL2'        TO SS35-2D.
003579
003580     MOVE '1'                    TO W-CNTL-RECORD-TYPE.
003581     MOVE ZEROS                  TO W-CNTL-SEQ.
003582
003583     
      * EXEC CICS HANDLE CONDITION
003584*         NOTOPEN   (7385-CNTL-NOT-OPEN)
003585*         NOTFND    (7340-READ-CNTL2)
003586*    END-EXEC.
      *    MOVE '"$JI                  ! ? #00009275' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3F20233030303039323735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003587
003588     
      * EXEC CICS READ
003589*         DATASET   (W-CNTL-ID)
003590*         SET       (ADDRESS OF CONTROL-FILE)
003591*         RIDFLD    (W-CNTL-KEY)
003592*    END-EXEC.
      *    MOVE '&"S        E          (   #00009280' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039323830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003593
003594*    SERVICE RELOAD CONTROL-FILE.
003595
003596     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003597     MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (01).
003598     MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (02).
003599     MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (03).
003600     MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (04).
003601     MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (05).
003602
003603     IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC
003604         MOVE ZEROS              TO CF-CL-ZIP-CODE-NUM.
003605
003606     IF  CF-CL-ZIP-CODE-NUM NOT = ZEROS
003607         MOVE CF-CL-ZIP-CODE-NUM TO W-ZIP-NUMERIC
003608         MOVE W-ZIP-NONNUM       TO CF-CL-ZIP-CODE.
003609
003610     MOVE SPACES                 TO W-ZIP-CODE.
003611
003612     IF  CF-CL-CAN-POST-CODE
003613         MOVE CF-CL-CAN-POSTAL-1 TO W-CAN-POSTAL-1
003614         MOVE CF-CL-CAN-POSTAL-2 TO W-CAN-POSTAL-2
003615
003616     ELSE
003617         MOVE CF-CL-ZIP-PRIME    TO W-AM-ZIP-CODE
003618
003619         IF  CF-CL-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003620             MOVE '-'            TO W-AM-ZIP-DASH
003621             MOVE CF-CL-ZIP-PLUS4
003622                                 TO W-AM-ZIP-PLUS4.
003623
003624     MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).
003625
003626     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003627
003628     MOVE W-LABEL-LINES (01)     TO SS01D.
003629     MOVE W-LABEL-LINES (02)     TO SS02-1D.
003630     MOVE W-LABEL-LINES (03)     TO SS02-2D.
003631     MOVE W-LABEL-LINES (04)     TO SS02-3D.
003632     MOVE W-LABEL-LINES (05)     TO SS02-4D.
003633     MOVE W-LABEL-LINES (06)     TO SS02-5D.
003634
003635 7340-READ-CNTL2.
003636
003637     IF  PI-PROCESSOR-ID =  'LGXX'
003638         GO TO 7350-READ-CNTL4.
003639
003640     MOVE '2'                    TO W-CNTL-RECORD-TYPE.
003641     MOVE PI-PROCESSOR-ID        TO W-CNTL-GENL.
003642     MOVE ZEROS                  TO W-CNTL-SEQ.
003643
003644     
      * EXEC CICS HANDLE CONDITION
003645*         NOTOPEN    (7385-CNTL-NOT-OPEN)
003646*         NOTFND     (7350-READ-CNTL4)
003647*    END-EXEC.
      *    MOVE '"$JI                  ! @ #00009336' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4020233030303039333336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003648
003649     
      * EXEC CICS READ
003650*         DATASET    (W-CNTL-ID)
003651*         SET        (ADDRESS OF CONTROL-FILE)
003652*         RIDFLD     (W-CNTL-KEY)
003653*     END-EXEC.
      *    MOVE '&"S        E          (   #00009341' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039333431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003654
003655*    SERVICE RELOAD CONTROL-FILE.
003656
003657     MOVE CF-PROCESSOR-NAME      TO SS08D.
003658     MOVE CF-PROCESSOR-TITLE     TO SS09D.
003659                                 EJECT
003660 7350-READ-CNTL4.
003661
003662     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
003663         GO TO 7350-READ-EMPLAN.
003664
003665     IF  W-BEN-HOLD = ZEROS
003666         GO TO 7350-READ-CNTL6.
003667
003668     MOVE W-BEN-HOLD             TO W-CNTL-GEN2.
003669     MOVE SPACES                 TO W-CNTL-GEN1.
003670
003671     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
003672         MOVE '4'                TO W-CNTL-RECORD-TYPE
003673
003674     ELSE
003675         MOVE '5'                TO W-CNTL-RECORD-TYPE.
003676
003677     MOVE ZEROS                  TO W-CNTL-SEQ.
003678
003679     
      * EXEC CICS HANDLE CONDITION
003680*         NOTOPEN    (7385-CNTL-NOT-OPEN)
003681*         NOTFND     (7350-READ-CNTL6)
003682*    END-EXEC.
      *    MOVE '"$JI                  ! A #00009371' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4120233030303039333731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003683
003684     
      * EXEC CICS READ
003685*         DATASET    (W-CNTL-ID)
003686*         SET        (ADDRESS OF CONTROL-FILE)
003687*         RIDFLD     (W-CNTL-KEY)
003688*         GTEQ
003689*     END-EXEC.
      *    MOVE '&"S        G          (   #00009376' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039333736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003690
003691*    SERVICE RELOAD CONTROL-FILE.
003692
003693     MOVE 1                      TO W-SUB.
003694
003695 7350-LOOP.
003696
003697     IF  W-SUB = 9
003698         GO TO 7350-READ-CNTL6.
003699
003700     IF  CF-BENEFIT-CODE (W-SUB) LESS THAN W-BEN-HOLD
003701         ADD 1                   TO W-SUB
003702         GO TO 7350-LOOP.
003703
003704     IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-SUB)
003705         MOVE CF-BENEFIT-DESCRIP (W-SUB) TO SS22D
003706
003707         IF  CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
003708                                               OR 'B' OR 'H'
003709             MOVE CF-BENEFIT-ALPHA (W-SUB)
003710                                 TO W-BENEFIT-WORK
003711             MOVE W-ELIM-DAYS    TO SS42D.
003712
003713     GO TO 7350-READ-CNTL6.
003714                                 EJECT
003715 7350-READ-EMPLAN.
003716
003717     IF W-BEN-HOLD IS EQUAL TO ZEROS
003718         GO TO 7350-READ-CNTL6.
003719
003720     MOVE PM-GROUPING            TO W-PLAN-GROUPING.
003721     MOVE PM-STATE               TO W-PLAN-STATE.
003722     MOVE PM-PRODUCER            TO W-PLAN-PRODUCER.
003723     MOVE PM-INS-PLAN-CD         TO W-PLAN-CODE.
003724     MOVE PM-INS-PLAN-REVISION   TO W-PLAN-REV-NO.
003725
003726     
      * EXEC CICS HANDLE CONDITION
003727*         NOTOPEN  (7397-PLAN-NOT-OPEN)
003728*         NOTFND   (7350-READ-CNTL6)
003729*    END-EXEC.
      *    MOVE '"$JI                  ! B #00009418' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4220233030303039343138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003730
003731     
      * EXEC CICS READ
003732*         DATASET   (W-PLAN-ID)
003733*         SET       (ADDRESS OF PRODUCER-PLANS)
003734*         RIDFLD    (W-PLAN-KEY)
003735*    END-EXEC.
      *    MOVE '&"S        E          (   #00009423' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039343233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PLAN-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003736
003737*    SERVICE RELOAD PRODUCER-PLANS.
003738
003739     MOVE PP-PLAN-DESCRIPTION    TO SS22D.
003740                                 EJECT
003741 7350-READ-CNTL6.
003742
003743     MOVE '6'                    TO W-CNTL-RECORD-TYPE.
003744     MOVE SPACES                 TO W-CNTL-GENL.
003745
003746     IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
003747         MOVE CL-CURRENT-CARRIER TO W-CNTL-GEN4
003748
003749     ELSE
003750         MOVE PI-CARRIER         TO W-CNTL-GEN4.
003751
003752     MOVE ZEROS                  TO W-CNTL-SEQ.
003753
003754     
      * EXEC CICS HANDLE CONDITION
003755*         NOTOPEN    (7385-CNTL-NOT-OPEN)
003756*         NOTFND     (7360-SET-DATE)
003757*    END-EXEC.
      *    MOVE '"$JI                  ! C #00009446' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4320233030303039343436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003758
003759     
      * EXEC CICS READ
003760*         DATASET    (W-CNTL-ID)
003761*         SET        (ADDRESS OF CONTROL-FILE)
003762*         RIDFLD     (W-CNTL-KEY)
003763*    END-EXEC.
      *    MOVE '&"S        E          (   #00009451' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039343531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003764
003765*    SERVICE RELOAD CONTROL-FILE.
003766
003767     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003768     MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (01).
003769     MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (02).
003770     MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (03).
003771     MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (04).
003772     MOVE CF-CITY-STATE          TO W-LABEL-LINES (05).
003773
003774     IF  CF-ZIP-CODE-NUM NOT NUMERIC
003775         MOVE ZEROS              TO CF-ZIP-CODE-NUM.
003776
003777     IF  CF-ZIP-CODE-NUM NOT = ZEROS
003778         MOVE CF-ZIP-CODE-NUM    TO W-ZIP-NUMERIC
003779         MOVE W-ZIP-NONNUM       TO CF-ZIP-CODE.
003780
003781     MOVE SPACES                 TO W-ZIP-CODE.
003782
003783     IF  CF-CANADIAN-POST-CODE
003784         MOVE CF-CAN-POSTAL-1    TO W-CAN-POSTAL-1
003785         MOVE CF-CAN-POSTAL-2    TO W-CAN-POSTAL-2
003786
003787     ELSE
003788         MOVE CF-ZIP-PRIME       TO W-AM-ZIP-CODE
003789
003790         IF  CF-ZIP-PLUS4 NOT = SPACES AND  ZEROS
003791             MOVE '-'            TO W-AM-ZIP-DASH
003792             MOVE CF-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
003793
003794     MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).
003795
003796     MOVE ZEROS                  TO W-PHONE-IN.
003797     MOVE CF-PHONE-NO            TO W-PHONE-IN.
003798     MOVE W-PI-AREA              TO W-PO-AREA.
003799     MOVE W-PI-PFX               TO W-PO-PFX.
003800     MOVE W-PI-SFX               TO W-PO-SFX.
003801     MOVE W-PHONE-OUT            TO SS04-6D.
003802
003803     PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
003804
003805     MOVE W-LABEL-LINES (01)     TO SS03D.
003806     IF PI-CARRIER = '8'
003807        MOVE 'as Administrator for Investors Heritage Life Insuran
003808-     'ce Company'               TO SS03-1D
003809     END-IF
003810     MOVE W-LABEL-LINES (02)     TO SS04-1D.
003811     MOVE W-LABEL-LINES (03)     TO SS04-2D.
003812     MOVE W-LABEL-LINES (04)     TO SS04-3D.
003813     MOVE W-LABEL-LINES (05)     TO SS04-4D.
003814     MOVE W-LABEL-LINES (06)     TO SS04-5D.
003815                                 EJECT
003816 7360-SET-DATE.
003817
003818     MOVE EIBDATE                TO W-DATE-WORK.
003819     MOVE W-DT-WORK              TO DC-JULIAN-YYDDD.
003820     MOVE '5'                    TO DC-OPTION-CODE.
003821     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
003822     MOVE DC-GREG-DATE-1-EDIT    TO SS20D.
003823     MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.
003824
003825     IF  LOWER-CASE-LETTERS-USED
003826         NEXT SENTENCE
003827
003828     ELSE
003829         GO TO 7399-EXIT.
003830
003831     INSPECT SS19D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
003832     INSPECT SS35-1D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
003833     INSPECT SS35-2D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
003834     INSPECT SS22D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
003835
003836     MOVE SS08D                  TO W-TEMP-AREA2
003837     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003838     MOVE W-TEMP-AREA2           TO SS08D
003839
003840     MOVE SS09D                  TO W-TEMP-AREA2
003841     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003842     MOVE W-TEMP-AREA2           TO SS09D
003843
003844     MOVE SS11-5D                TO W-TEMP-AREA2
003845     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003846     MOVE W-TEMP-AREA2           TO SS11-5D
003847
003848     MOVE SS21D                  TO W-TEMP-AREA2
003849     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003850     MOVE W-TEMP-AREA2           TO SS21D
003851
003852     MOVE SS40D                  TO W-TEMP-AREA2
003853     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003854     MOVE W-TEMP-AREA2           TO SS40D
003855
003856     MOVE SS41D                  TO W-TEMP-AREA2
003857     PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003858     MOVE W-TEMP-AREA2           TO SS41D.
003859     GO TO 7399-EXIT.
003860                                 EJECT
003861 7370-RESOLVE-CREDITOR.
003862
003863     
      * EXEC CICS HANDLE CONDITION
003864*         NOTOPEN    (7380-BENE-NOT-OPEN)
003865*         NOTFND     (7370-EXIT)
003866*    END-EXEC.
      *    MOVE '"$JI                  ! D #00009555' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4420233030303039353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003867
003868     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
003869
003870     MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
003871     MOVE 'B'                    TO W-BENE-REC-TYPE.
003872     MOVE SPACES                 TO W-BENE-NUMBER.
003873     MOVE CL-CURRENT-GROUPING    TO W-BENE-CREDITOR.
003874
003875     
      * EXEC CICS READ
003876*         DATASET    (W-BENE-ID)
003877*         SET        (ADDRESS OF BENEFICIARY-MASTER)
003878*         RIDFLD     (W-BENE-KEY)
003879*    END-EXEC.
      *    MOVE '&"S        E          (   #00009567' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039353637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003880
003881*    SERVICE RELOAD BENEFICIARY-MASTER.
003882
003883     IF  LOWER-CASE-LETTERS-USED
003884         MOVE BE-MAIL-TO-NAME    TO W-TEMP-AREA2
003885         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
003886         MOVE W-TEMP-AREA2       TO SS56D
003887
003888     ELSE
003889         MOVE BE-MAIL-TO-NAME    TO SS56D.
003890
003891 7370-EXIT.
003892     EXIT.
003893
003894 7375-ACCT-NOT-OPEN.
003895
003896     MOVE ER-0168                TO W-CURRENT-ERROR.
003897     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003898     GO TO 7399-EXIT.
003899
003900 7380-BENE-NOT-OPEN.
003901
003902     MOVE ER-7675                TO W-CURRENT-ERROR.
003903     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003904     GO TO 7399-EXIT.
003905
003906
003907 7385-CNTL-NOT-OPEN.
003908
003909     MOVE ER-0042                TO W-CURRENT-ERROR.
003910     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003911     GO TO 7399-EXIT.
003912
003913 7390-ACTV-NOT-OPEN.
003914
003915     MOVE ER-0172                TO W-CURRENT-ERROR.
003916     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003917     GO TO 7399-EXIT.
003918
003919 7395-PROD-NOT-OPEN.
003920
003921     MOVE ER-9106                TO W-CURRENT-ERROR.
003922     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003923     GO TO 7399-EXIT.
003924
003925 7397-PLAN-NOT-OPEN.
003926
003927     MOVE ER-9808                TO W-CURRENT-ERROR.
003928     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
003929     GO TO 7399-EXIT.
003930
003931 7399-EXIT.
003932     EXIT.
003933                                 EJECT
003934 7400-MOVE-NAME.
003935
003936     MOVE SPACES                 TO W-NAME-WORK
003937                                    W-NAME-WORK2.
003938     MOVE ZERO                   TO W-NAME-SW.
003939     SET W-NWA-NDX               TO +1.
003940
003941     IF  CL-INSURED-1ST-NAME = SPACES
003942             AND
003943         CL-INSURED-MID-INIT = SPACES
003944         MOVE +1                 TO W-NAME-SW.
003945
003946     MOVE CL-INSURED-LAST-NAME   TO W-NAME-WORK2.
003947     PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
003948
003949     MOVE CL-INSURED-1ST-NAME    TO W-NAME-WORK2.
003950     PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
003951
003952     SET W-NWA-NDX UP BY +1.
003953     MOVE CL-INSURED-MID-INIT    TO W-NAME-WORK2.
003954     PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
003955
003956 7400-EXIT.
003957     EXIT.
003958                                 EJECT
003959 7420-MOVE-NAME SECTION.
003960
003961     IF  W-NAME-SW GREATER THAN +1
003962         GO TO 7429-EXIT.
003963
003964     IF  W-NAME-WORK2 = SPACES
003965         GO TO 7429-EXIT.
003966
003967     SET W-NWA-NDX2              TO +1.
003968     SET W-NWA-NDX3              TO +2.
003969
003970 7422-MOVE-NAME.
003971
003972     MOVE W-NW2 (W-NWA-NDX2)   TO W-NW (W-NWA-NDX).
003973
003974     IF  W-NWA-NDX LESS THAN +30
003975         SET W-NWA-NDX UP BY +1
003976
003977     ELSE
003978         ADD +2                  TO W-NAME-SW
003979         GO TO 7429-EXIT.
003980
003981     IF  W-NWA-NDX2 LESS THAN +20
003982         SET W-NWA-NDX3 UP BY +1
003983         SET W-NWA-NDX2 UP BY +1.
003984
003985     IF  W-NW2 (W-NWA-NDX2) = SPACES
003986             AND
003987         W-NW2 (W-NWA-NDX3) = SPACES
003988
003989         IF  W-NAME-SW = ZERO
003990             MOVE ','            TO W-NW (W-NWA-NDX)
003991             SET W-NWA-NDX UP BY +2
003992             MOVE +1             TO W-NAME-SW
003993             GO TO 7429-EXIT
003994
003995         ELSE
003996             GO TO 7429-EXIT.
003997
003998     GO TO 7422-MOVE-NAME.
003999
004000 7429-EXIT.
004001     EXIT.
004002                                 EJECT
004003 7500-MOVE-NAME.
004004
004005     MOVE SPACES                 TO W-NAME-WORK
004006                                    W-NAME-WORK2.
004007     MOVE ZERO                   TO W-NAME-SW.
004008     SET W-NWA-NDX               TO +1.
004009
004010     IF  W-FIRST-NAME = SPACES
004011             AND
004012         W-MIDDLE-NAME = SPACES
004013         MOVE W-LAST-NAME
004014                                 TO W-NAME-WORK
004015         GO TO 7500-EXIT.
004016
004017     MOVE W-FIRST-NAME           TO W-NAME-WORK2.
004018     PERFORM 7520-MOVE-NAME THRU 7529-EXIT.
004019
004020     SET W-NWA-NDX UP BY +1.
004021
004022     IF  W-MIDDLE-NAME NOT = SPACES
004023         MOVE W-MIDDLE-NAME      TO W-NW (W-NWA-NDX)
004024         SET W-NWA-NDX UP BY +1
004025         MOVE '.'                TO W-NW (W-NWA-NDX)
004026         SET W-NWA-NDX UP BY +2.
004027
004028     MOVE W-LAST-NAME            TO W-NAME-WORK2.
004029     PERFORM 7520-MOVE-NAME THRU 7529-EXIT.
004030
004031 7500-EXIT.
004032     EXIT.
004033                                 EJECT
004034 7520-MOVE-NAME SECTION.
004035
004036     IF  W-NAME-SW GREATER THAN +1
004037         GO TO 7529-EXIT.
004038
004039     IF  W-NAME-WORK2 = SPACES
004040         GO TO 7529-EXIT.
004041
004042     SET W-NWA-NDX2              TO +1.
004043     SET W-NWA-NDX3              TO +2.
004044
004045 7522-MOVE-NAME.
004046
004047     MOVE W-NW2 (W-NWA-NDX2)     TO W-NW (W-NWA-NDX).
004048
004049     IF  W-NWA-NDX LESS THAN +30
004050         SET W-NWA-NDX UP BY +1
004051
004052     ELSE
004053         ADD +2                 TO W-NAME-SW
004054         GO TO 7529-EXIT.
004055
004056     IF  W-NWA-NDX2 LESS THAN +20
004057         SET W-NWA-NDX2 UP BY +1
004058         SET W-NWA-NDX3 UP BY +1.
004059
004060     IF  W-NW2 (W-NWA-NDX2) = SPACES
004061             AND
004062         W-NW2 (W-NWA-NDX3) = SPACES
004063         GO TO 7529-EXIT.
004064
004065     GO TO 7522-MOVE-NAME.
004066
004067 7529-EXIT.
004068     EXIT.
004069                                 EJECT
004070 7600-LABEL-MOVE.
004071
004072     IF  W-LABEL-HOLD-AREA = SPACES
004073         GO TO 7600-EXIT.
004074
004075     PERFORM 7700-TRANSLATE-LOWER THRU 7700-EXIT.
004076
004077     IF  W-LABEL-LINES (1) = SPACES
004078         MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
004079         MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
004080         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
004081         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
004082         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
004083         MOVE SPACES             TO W-LABEL-LINES (6)
004084         GO TO 7600-LABEL-MOVE.
004085
004086     IF  W-LABEL-LINES (2) = SPACES AND
004087         W-LABEL-LINES (3) = SPACES AND
004088         W-LABEL-LINES (4) = SPACES AND
004089         W-LABEL-LINES (5) = SPACES AND
004090         W-LABEL-LINES (6) = SPACES
004091         SET W-NDX               TO 1
004092         GO TO 7600-MOVE-ZIP.
004093
004094     IF  W-LABEL-LINES (2) = SPACES
004095         MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
004096         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
004097         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
004098         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
004099         MOVE SPACES             TO W-LABEL-LINES (6)
004100         GO TO 7600-LABEL-MOVE.
004101
004102     IF  W-LABEL-LINES (3) = SPACES AND
004103         W-LABEL-LINES (4) = SPACES AND
004104         W-LABEL-LINES (5) = SPACES AND
004105         W-LABEL-LINES (6) = SPACES
004106         SET W-NDX               TO 2
004107         GO TO 7600-MOVE-ZIP.
004108
004109     IF  W-LABEL-LINES (3) = SPACES
004110         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
004111         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
004112         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
004113         MOVE SPACES             TO W-LABEL-LINES (6)
004114         GO TO 7600-LABEL-MOVE.
004115
004116     IF  W-LABEL-LINES (4) = SPACES AND
004117         W-LABEL-LINES (5) = SPACES AND
004118         W-LABEL-LINES (6) = SPACES
004119         SET W-NDX               TO 3
004120         GO TO 7600-MOVE-ZIP.
004121
004122     IF  W-LABEL-LINES (4) = SPACES
004123         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
004124         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
004125         MOVE SPACES             TO W-LABEL-LINES (6)
004126         GO TO 7600-LABEL-MOVE.
004127
004128     IF  W-LABEL-LINES (5) = SPACES AND
004129         W-LABEL-LINES (6) = SPACES
004130         SET W-NDX               TO 4
004131         GO TO 7600-MOVE-ZIP.
004132
004133     IF  W-LABEL-LINES (5) = SPACES
004134         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
004135         MOVE SPACES             TO W-LABEL-LINES (6)
004136         SET W-NDX               TO 5
004137         GO TO 7600-MOVE-ZIP
004138
004139     ELSE
004140         IF  W-LABEL-LINES (6) = SPACES
004141             SET W-NDX           TO 5
004142             GO TO 7600-MOVE-ZIP
004143
004144         ELSE
004145             SET W-NDX           TO 6.
004146
004147 7600-MOVE-ZIP.
004148
004149     SET W-NDX2                  TO W-NDX.
004150     SET W-NDX2 DOWN BY +1.
004151
004152     IF  W-LAST-ZIP (W-NDX2) = SPACES
004153             AND
004154         W-LAST-DIGIT (W-NDX2) EQUAL SPACES
004155*****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
004156
004157         IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC
004158
004159             IF  PI-COMPANY-ID NOT = 'FLA'
004160                 MOVE W-LABEL-ZIP (W-NDX)
004161                                 TO W-LAST-ZIP (W-NDX2)
004162                 MOVE SPACES     TO W-LABEL-LINES (W-NDX).
004163
004164 7600-EXIT.
004165     EXIT.
004166                                 EJECT
004167 7700-TRANSLATE-LOWER.
004168
004169     IF  LOWER-CASE-LETTERS-USED
004170         NEXT SENTENCE
004171
004172     ELSE
004173         GO TO 7700-EXIT.
004174
004175     IF  W-LABEL-LINES (1) NOT = SPACES
004176         MOVE W-LABEL-LINES (1)  TO W-TEMP-AREA2
004177         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
004178         MOVE W-TEMP-AREA2       TO W-LABEL-LINES (1).
004179
004180     IF  W-LABEL-LINES (2) NOT = SPACES
004181         MOVE W-LABEL-LINES (2)  TO W-TEMP-AREA2
004182         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
004183         MOVE W-TEMP-AREA2       TO W-LABEL-LINES (2).
004184
004185     IF  W-LABEL-LINES (3) NOT = SPACES
004186         MOVE W-LABEL-LINES (3)  TO W-TEMP-AREA2
004187         PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
004188         MOVE W-TEMP-AREA2       TO W-LABEL-LINES (3).
004189
004190*****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING
004191*****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.
004192
004193     MOVE 'N'                    TO W-STATE-LINE.
004194
004195     IF  W-LABEL-LINES (4) NOT = SPACES
004196        IF  W-LABEL-LINES (6) = SPACES
004197           MOVE 'Y'              TO W-STATE-LINE
004198           MOVE W-LABEL-LINES (4)
004199                                 TO W-TEMP-AREA2
004200           PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
004201           MOVE W-TEMP-AREA2     TO W-LABEL-LINES (4).
004202
004203     IF  W-LABEL-LINES (5) NOT = SPACES
004204        IF  W-LABEL-LINES (6) NOT = SPACES
004205           MOVE 'Y'              TO W-STATE-LINE
004206           MOVE W-LABEL-LINES (5)
004207                                 TO W-TEMP-AREA2
004208           PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
004209           MOVE W-TEMP-AREA2     TO W-LABEL-LINES (5).
004210
004211 7700-EXIT.
004212     EXIT.
004213                                 EJECT
004214
004215 7750-SEARCH-AND-TRANSLATE.
004216
004217     SET W-TA1                   TO +1.
004218
004219 7750-FIND-FIRST-NON-BLANK.
004220
004221     IF  W-TEMP-2 (W-TA1) = SPACES
004222         SET W-TA1 UP BY 1
004223         GO TO 7750-FIND-FIRST-NON-BLANK.
004224
004225*****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.
004226     SET W-TA2                   TO W-TA1
004227     SET W-TA2 UP BY 1
004228     SET W-TA21                  TO W-TA2
004229     SET W-TA21 UP BY 1.
004230
004231     MOVE 'N'                    TO W-DATA-FOUND-SW.
004232     PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.
004233
004234 7750-EXIT.
004235     EXIT.
004236     EJECT
004237
004238 7282-FIND-NEXT-BLANK.
004239
004240     IF  W-TA21 GREATER THAN 31
004241         GO TO 7282-EXIT.
004242
004243     IF  W-TA21 EQUAL 31
004244
004245         IF  NO-CHARACTERS-FOUND
004246             GO TO 7282-EXIT
004247
004248         ELSE
004249             SET W-TA1           TO +1
004250             MOVE SPACES         TO W-TEMP-AREA1
004251
004252             PERFORM 7283-MOVE
004253                     VARYING
004254                 W-MOVE-NDX FROM W-TA2 BY 1
004255                     UNTIL
004256                 W-MOVE-NDX EQUAL W-TA21
004257
004258             PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT
004259             SET W-TA1           TO +1
004260
004261             PERFORM 7284-MOVE-BACK
004262                     VARYING
004263                 W-MOVE-NDX FROM W-TA2 BY 1
004264                     UNTIL
004265                 W-MOVE-NDX EQUAL W-TA21
004266
004267             GO TO 7282-EXIT.
004268
004269     IF  W-TEMP-2 (W-TA2) = SPACE OR ',' OR '/'
004270         GO TO 7282-NEXT-GROUP-SEARCH.
004271
004272     IF  W-TEMP-2 (W-TA21) = SPACES OR ',' OR '/'
004273         NEXT SENTENCE
004274
004275     ELSE
004276         MOVE 'Y'                TO W-DATA-FOUND-SW
004277         SET W-TA21 UP BY +1
004278         GO TO 7282-FIND-NEXT-BLANK.
004279
004280     SET W-TA1                   TO +1.
004281     MOVE SPACES                 TO W-TEMP-AREA1.
004282
004283     PERFORM 7283-MOVE
004284             VARYING
004285         W-MOVE-NDX FROM W-TA2 BY 1
004286             UNTIL
004287         W-MOVE-NDX EQUAL W-TA21.
004288
004289     PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.
004290     SET W-TA1                   TO +1.
004291
004292     PERFORM 7284-MOVE-BACK
004293             VARYING
004294         W-MOVE-NDX FROM W-TA2 BY 1
004295             UNTIL
004296         W-MOVE-NDX EQUAL W-TA21.
004297
004298     SET W-TA2                  TO W-TA21.
004299     SET W-TA21 UP BY 1.
004300     MOVE 'N'                   TO W-DATA-FOUND-SW.
004301
004302 7282-NEXT-GROUP-SEARCH.
004303
004304     IF  W-TEMP-2 (W-TA2) EQUAL SPACES OR ',' OR '/'
004305         SET W-TA2 UP BY 1
004306
004307         IF  W-TA2 = 30
004308             GO TO 7282-EXIT
004309
004310         ELSE
004311             GO TO 7282-NEXT-GROUP-SEARCH.
004312
004313     SET W-TA2  UP BY 1.
004314     SET W-TA21                  TO W-TA2.
004315     SET W-TA21 UP BY 1.
004316     GO TO 7282-FIND-NEXT-BLANK.
004317
004318 7282-EXIT.
004319     EXIT.
004320
004321 7283-MOVE.
004322
004323     MOVE W-TEMP-2 (W-MOVE-NDX)  TO W-TEMP-1 (W-TA1).
004324     SET W-TA1 UP BY +1.
004325
004326 7284-MOVE-BACK.
004327
004328     MOVE W-TEMP-1 (W-TA1)       TO W-TEMP-2 (W-MOVE-NDX).
004329     SET W-TA1 UP BY +1.
004330
004331 7285-TEST-AND-TRANSLATE.
004332***BYPASS IF  THE AREA MAY BE A PO BOX, OR RR NUMBER
004333
004334     IF  W-TEMP-AREA1 = '.O.' OR 'RR'
004335         GO TO 7285-EXIT.
004336
004337***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07
004338***AND IT APPEARS THAT IT MAY BE A ABREVIATION.
004339
004340     SET W-POSITION2             TO W-TA2.
004341     SET W-POSITION21            TO W-TA21.
004342     COMPUTE W-WORD-LENGTH = W-POSITION21 - W-POSITION2.
004343
004344     IF  W-WORD-LENGTH LESS THAN 3
004345             AND
004346         W-STATE-LINE = 'Y'
004347             AND
004348         W-TA2 GREATER THAN 07
004349         GO TO 7285-EXIT.
004350
004351     INSPECT W-TEMP-AREA1 CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
004352
004353 7285-EXIT.
004354     EXIT.
004355
004356 7800-VARIABLE-SEARCH.
004357***************************************************************
004358*    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *
004359*    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
004360*    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
004361*    DATA THAT WAS GENERATED IN PARAGRAPHS 7000-7299.         *
004362*                                                             *
004363*    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *
004364*    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *
004365*    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *
004366***************************************************************
004367
004368     MOVE W-REC-TEXT (W-TB-NDX)  TO W-SINGLE-LINE.
004369     SET NDX1                    TO 1.
004370
004371 7801-LOOP.
004372
004373     IF  NDX1 GREATER 70
004374         MOVE W-SINGLE-LINE      TO W-REC-TEXT (W-TB-NDX)
004375         GO TO 7899-EXIT.
004376
004377     IF  W-ONE-CHAR (NDX1) NOT = '@'
004378         SET NDX1 UP BY 1
004379         GO TO 7801-LOOP.
004380
004381     SET NDX2                    TO NDX1.
004382     SET NDX2 UP BY 1.
004383
004384     IF  W-ONE-CHAR (NDX2) = '@'
004385         SET NDX1 UP BY 2
004386         GO TO 7801-LOOP.
004387
004388     MOVE W-ONE-CHAR (NDX2)      TO W-V1.
004389     SET NDX2 UP BY 1.
004390     MOVE W-ONE-CHAR (NDX2)      TO W-V2.
004391     SET NDX2 UP BY 1.
004392     MOVE W-ONE-CHAR (NDX2)      TO W-V3.
004393     SET NDX2 UP BY 1.
004394     MOVE W-ONE-CHAR (NDX2)      TO W-V4.
004395
004396     IF  W-V-NUM NOT NUMERIC
004397         GO TO 7830-VAR-ERROR.
004398
004399     IF  W-V-PERIOD NOT = '.'
004400         MOVE '.'                TO W-V-PERIOD
004401         MOVE ZERO               TO W-V-DECIMAL
004402         GO TO 7840-TABLE-SEARCH.
004403
004404     IF  W-V-DECIMAL NUMERIC
004405         GO TO 7840-TABLE-SEARCH.
004406
004407 7830-VAR-ERROR.
004408
004409     MOVE ER-0180                TO W-CURRENT-ERROR.
004410     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
004411     SET NDX1 UP BY 1.
004412     GO TO 7801-LOOP.
004413
004414 7840-TABLE-SEARCH.
004415
004416     IF  W-NO-GETMAIN-DONE-YET
004417
004418         IF  PI-VAR-POINTER GREATER THAN ZEROS
004419             MOVE PI-VAR-POINTER TO LCP-WS-ADDR-COMP
004420             SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
004421*            SERVICE RELOAD SYSTEM-VARIABLES
004422             MOVE SYSTEM-SUPPORTED-VARIABLES
004423                                 TO SYSTEM-VARIABLES
004424             MOVE 1              TO W-GETMAIN-SW
004425             MOVE 1              TO SS-COUNTER
004426
004427         ELSE
004428             
      * EXEC CICS GETMAIN
004429*                 SET     (ADDRESS OF SYSTEM-VARIABLES)
004430*                 LENGTH  (SS-WORK-AREA-LENGTH)
004431*            END-EXEC
      *    MOVE '," L                  $   #00010120' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130313230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 SS-WORK-AREA-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF SYSTEM-VARIABLES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004432
004433*            SERVICE RELOAD SYSTEM-VARIABLES
004434
004435             MOVE 1              TO W-GETMAIN-SW
004436             SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
004437             MOVE LCP-WS-ADDR-COMP
004438                                 TO PI-VAR-POINTER
004439             MOVE SYSTEM-SUPPORTED-VARIABLES
004440                                 TO SYSTEM-VARIABLES
004441             MOVE 1              TO SS-COUNTER
004442
004443     ELSE
004444         MOVE 1                  TO SS-COUNTER
004445         MOVE PI-VAR-POINTER     TO LCP-WS-ADDR-COMP
004446         SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR.
004447
004448 7850-TABLE-LOOP.
004449
004450*    SERVICE RELOAD SYSTEM-VARIABLES.
004451
004452     IF  SS-COUNTER GREATER THAN  SS-NUM-ENTRIES
004453         GO TO 7830-VAR-ERROR.
004454
004455     IF  SYS-VAR-CODE NOT = W-VAR-HOLD
004456         SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
004457         ADD SYS-VAR-LEN         TO LCP-WS-ADDR-COMP
004458         SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
004459         ADD 1                   TO SS-COUNTER
004460         GO TO 7850-TABLE-LOOP.
004461
004462     MOVE SYS-VAR-ENTRY          TO W-VARIABLE-WORK-AREA.
004463     SET W-NDXV                  TO 1.
004464     SUBTRACT 6                  FROM W-VAR-LEN.
004465     PERFORM 7900-MOVE-VAR-DATA THRU 7900-EXIT
004466         W-VAR-LEN TIMES.
004467     GO TO 7801-LOOP.
004468
004469 7899-EXIT.
004470      EXIT.
004471
004472 7900-MOVE-VAR-DATA.
004473
004474     IF  NDX1 GREATER 70
004475         MOVE ER-0181            TO W-CURRENT-ERROR
004476         PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
004477         GO TO 7801-LOOP.
004478
004479     MOVE W-VAR-W-ONE-CHAR (W-NDXV) TO W-ONE-CHAR (NDX1).
004480     SET W-NDXV UP BY 1.
004481     SET NDX1 UP BY 1.
004482
004483 7900-EXIT.
004484      EXIT.
004485                                 EJECT
004486 8000-STARTBR-ERACCT.
004487
004488     IF  W-ACCT-READ-SW EQUAL    TO 'Y'
004489         NEXT SENTENCE
004490
004491     ELSE
004492         IF  PI-ACCT-POINTER GREATER THAN ZEROS
004493             MOVE PI-ACCT-POINTER
004494                                 TO LCP-WS-ADDR-COMP
004495             SET ADDRESS OF ACCOUNT-MASTER TO LCP-WS-ADDR-PNTR
004496*            SERVICE RELOAD ACCOUNT-MASTER
004497
004498         ELSE
004499             
      * EXEC CICS GETMAIN
004500*                 SET      (ADDRESS OF ACCOUNT-MASTER)
004501*                 LENGTH   (W-ACCT-LENGTH)
004502*            END-EXEC
      *    MOVE '," L                  $   #00010191' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130313931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ACCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004503             SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACCOUNT-MASTER
004504
004505*            SERVICE RELOAD ACCOUNT-MASTER
004506             MOVE LCP-WS-ADDR-COMP TO PI-ACCT-POINTER.
004507
004508     
      * EXEC CICS STARTBR
004509*        RIDFLD      (W-ACCT-KEY)
004510*        DATASET     (W-ACCT-ID)
004511*        KEYLENGTH   (20)
004512*        GENERIC
004513*    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00010200' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303130323030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-ID, 
                 W-ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004514
004515     MOVE 'Y'                   TO W-ACCT-BROWSE-STARTED.
004516
004517 8000-EXIT.
004518     EXIT.
004519
004520 8010-READNEXT-ERACCT.
004521
004522     
      * EXEC CICS READNEXT
004523*        DATASET   (W-ACCT-ID)
004524*        INTO      (ACCOUNT-MASTER)
004525*        RIDFLD    (W-ACCT-KEY)
004526*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010214' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130323134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004527
004528 8010-EXIT.
004529     EXIT.
004530
004531                                 EJECT
004532 8050-STARTBR-EMPROD.
004533
004534     IF  W-PROD-READ-SW EQUAL    TO 'Y'
004535         NEXT SENTENCE
004536
004537     ELSE
004538         IF  PI-PROD-POINTER GREATER THAN ZEROS
004539             MOVE PI-PROD-POINTER
004540                                 TO LCP-WS-ADDR-COMP
004541             SET ADDRESS OF PRODUCER-MASTER TO LCP-WS-ADDR-PNTR
004542*            SERVICE RELOAD PRODUCER-MASTER
004543
004544         ELSE
004545             
      * EXEC CICS GETMAIN
004546*                 SET      (ADDRESS OF PRODUCER-MASTER)
004547*                 LENGTH   (W-PROD-LENGTH)
004548*            END-EXEC
      *    MOVE '," L                  $   #00010237' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130323337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-PROD-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004549             SET LCP-WS-ADDR-PNTR TO ADDRESS OF PRODUCER-MASTER
004550
004551*            SERVICE RELOAD PRODUCER-MASTER
004552             MOVE LCP-WS-ADDR-COMP TO PI-PROD-POINTER.
004553
004554     
      * EXEC CICS STARTBR
004555*        RIDFLD      (W-PROD-KEY)
004556*        DATASET     (W-PROD-ID)
004557*        KEYLENGTH   (20)
004558*        GENERIC
004559*    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00010246' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303130323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PROD-ID, 
                 W-PROD-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004560
004561     MOVE 'Y'                   TO W-PROD-BROWSE-STARTED.
004562
004563 8050-EXIT.
004564     EXIT.
004565
004566 8060-READNEXT-EMPROD.
004567
004568     
      * EXEC CICS READNEXT
004569*        DATASET   (W-PROD-ID)
004570*        INTO      (PRODUCER-MASTER)
004571*        RIDFLD    (W-PROD-KEY)
004572*    END-EXEC.
           MOVE LENGTH OF
            PRODUCER-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010260' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130323630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PROD-ID, 
                 PRODUCER-MASTER, 
                 DFHEIV12, 
                 W-PROD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004573
004574 8060-EXIT.
004575     EXIT.
004576
004577                                EJECT
004578 9600-PGMID-ERROR.
004579
004580     MOVE 9999                   TO W-CURRENT-ERROR.
004581     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
004582     GO TO 0200-RETURN-TO-CALLING-PGM.
004583
004584 9700-DATE-LINK.
004585
004586     MOVE W-LINK-ELDATCV         TO W-PGM-NAME.
004587
004588     
      * EXEC CICS LINK
004589*        PROGRAM   (W-PGM-NAME)
004590*        COMMAREA  (DATE-CONVERSION-DATA)
004591*        LENGTH    (DC-COMM-LENGTH)
004592*    END-EXEC.
      *    MOVE '."C                   (   #00010280' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303130323830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004593
004594     IF  NO-CONVERSION-ERROR
004595             AND
004596         W-REVERSE-DATE
004597             AND
004598         PI-COMPANY-ID EQUAL 'AUK'
004599         MOVE DC-GREG-DATE-1-EDIT
004600                                 TO W-EDIT-DATE-1
004601         MOVE W-ED1-MM           TO W-ED2-MM
004602         MOVE W-ED1-DD           TO W-ED2-DD
004603         MOVE W-ED1-YY           TO W-ED2-YY
004604         MOVE W-EDIT-DATE-2      TO DC-GREG-DATE-1-EDIT.
004605
004606 9700-EXIT.
004607      EXIT.
004608
004609 9900-ERROR-PROCESS.
004610
004611     MOVE W-CURRENT-ERROR        TO PI-ERROR-CODE.
004612
004613     IF  PI-FATAL-ERROR
004614         GO TO 0200-RETURN-TO-CALLING-PGM.
004615
004616 9900-EXIT.
004617     EXIT.
004618
004619 9990-ABEND.
004620
004621     MOVE 9999                   TO W-CURRENT-ERROR.
004622     PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
004623     GO TO 0200-RETURN-TO-CALLING-PGM.
004624
004625 9990-EXIT.
004626     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1523' TO DFHEIV1
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
               GO TO 2120-ENDBR,
                     2120-ENDBR,
                     2900-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3920-CLM-NOT-OPEN,
                     3900-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3930-ACCT-NOT-OPEN,
                     3910-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3960-PROD-NOT-OPEN,
                     3950-PROD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3450-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3940-ACTV-NOT-OPEN,
                     3450-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3480-CONTINUE-ACTV-ERROR,
                     3479-COMP-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6100-NOT-FOUND,
                     6110-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6120-ARCH-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 6420-ARCH-DUPREC,
                     6410-ARCH-NOT-OPEN,
                     6425-ARCH-NOSPACE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6596-ACTV-DUPREC,
                     6597-ACTV-NOSPACE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6598-REWRITE-CLAIM
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7022-CERT-NOT-OPEN,
                     7024-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7026-PLCY-NOT-OPEN,
                     7028-PLCY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7020-ACTV-NOT-OPEN,
                     7040-READ-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7380-BENE-NOT-OPEN,
                     7100-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 7390-ACTV-NOT-OPEN,
                     7100-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 7390-ACTV-NOT-OPEN,
                     7120-READ-EMPLOYER-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 7390-ACTV-NOT-OPEN,
                     7140-READ-INSURED-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 7390-ACTV-NOT-OPEN,
                     7160-READ-OTHER1-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 7390-ACTV-NOT-OPEN,
                     7180-READ-OTHER2-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 7390-ACTV-NOT-OPEN,
                     7200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7390-ACTV-NOT-OPEN,
                     7220-READ-ACCOUNT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7375-ACCT-NOT-OPEN,
                     7240-READ-3RD-PARTY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7395-PROD-NOT-OPEN,
                     7300-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7390-ACTV-NOT-OPEN,
                     7260-READ-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7300-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7390-ACTV-NOT-OPEN,
                     7340-READ-CNTL1,
                     7340-READ-CNTL1
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 7385-CNTL-NOT-OPEN,
                     7340-READ-CNTL2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 7385-CNTL-NOT-OPEN,
                     7350-READ-CNTL4
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 7385-CNTL-NOT-OPEN,
                     7350-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 7397-PLAN-NOT-OPEN,
                     7350-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 7385-CNTL-NOT-OPEN,
                     7360-SET-DATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 7380-BENE-NOT-OPEN,
                     7370-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1523' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
