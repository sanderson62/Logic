      *((program: EL152.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL152 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 06/14/94 08:03:40.
000007*                            VMOD=2.055.
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*REMARKS.    TRANSACTION - EX27 - CLAIMS LETTER WRITER.
000013
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000023*                                REMOVE OBSOLETE CODE
000024* 102703                   SMVA  MOD COPY MEMBER ELCNAMET TO LEAVE
000025*                                CERTIFICATION DESIGNATION ON
000026*                                PROCESSOR NAME LINE IN CAPS
000027* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
000028* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
000029* 042605    2005042100002  PEMA  FIX TEMP STORAGE PROBLEMS
000030* 010407    2006111300003  PEMA  ADD PROCESSING FOR CARRIER 8
000031* 060109  CR2008102800002  PEMA  ADD VAR 16.1
000032* 033110  CR2009122800001  AJRA  NAPERSOFT
000033* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
000034* 011212  IR2012011100002  AJRA  FIX DCC PROMPT LETTER
000035* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000036* 041513  CR2013011500003  AJRA  VALIDATE ENC CODE AGAINST ELENCC
000037* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
000038* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000039* 080322  CR2021100800003  TANA  Add B and H claim types
000040******************************************************************
000041 ENVIRONMENT DIVISION.
000042
000043     EJECT
000044 DATA DIVISION.
000045 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000046 77  FILLER  PIC X(32)  VALUE '********************************'.
000047 77  FILLER  PIC X(32)  VALUE '*    EL152 WORKING STORAGE     *'.
000048 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'.
000049
000050*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
000051*77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
000052*                                  USAGE POINTER.
000053 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
000054 77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
000055                                   USAGE POINTER.
000056 77  B1                          PIC S9(5) COMP-3 VALUE +0.
000057 77  S1                          PIC S9 VALUE +0.
000058 77  S2                          PIC S9 VALUE +0.
000059
000060
000061 01  P pointer.
000062 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000063 01  var-ptr pointer.
000064 01  env-var-len                 pic 9(4)  binary.
000065 01  rc                          pic 9(9)  binary.
000066
000067 01  WS-KIXSYS.
000068     05  WS-KIX-FIL1             PIC X(10).
000069     05  WS-KIX-APPS             PIC X(10).
000070     05  WS-KIX-ENV              PIC X(10).
000071     05  WS-KIX-MYENV            PIC X(10).
000072     05  WS-KIX-SYS              PIC X(10).
000073
000074 01  WS-HOLDING-ZIPS.
000075     05  FILLER              PIC X(10)  VALUE '--AM-ZIP--'.
000076     05  WS-AM-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
000077     05  FILLER              PIC X(10)  VALUE '--AT-ZIP--'.
000078     05  WS-AT-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
000079     05  FILLER              PIC X(10)  VALUE '--BE-ZIP--'.
000080     05  WS-BE-ZIP-CODE      PIC X(9)   VALUE 'XXXXXXXXX'.
000081     05  FILLER              PIC X(10)  VALUE '--CO-ZIP--'.
000082     05  WS-CO-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
000083     05  FILLER              PIC X(17)  VALUE '--AM-ZIP-BEFORE--'.
000084     05  WS-AM-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
000085     05  FILLER              PIC X(17)  VALUE '--AT-ZIP-BEFORE--'.
000086     05  WS-AT-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
000087     05  FILLER              PIC X(17)  VALUE '--BE-ZIP-BEFORE--'.
000088     05  WS-BE-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
000089     05  FILLER              PIC X(17)  VALUE '--CO-ZIP-BEFORE--'.
000090     05  WS-CO-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
000091     05  FILLER              PIC X(17)  VALUE '-----------------'.
000092
000093*                            COPY ELCSCTM.
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
000094
000095*                            COPY ELCSCRTY.
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
000096
000097 01  WS-DATE-AREA.
000098     12  SAVE-DATE               PIC X(08)    VALUE SPACES.
000099     12  SAVE-BIN-DATE           PIC XX       VALUE SPACES.
000100     12  W-REVERSE-DATE-SW       PIC X        VALUE SPACES.
000101         88  W-REVERSE-DATE                   VALUE 'Y'.
000102     12  W-EDIT-DATE-1.
000103         16  W-ED1-MM            PIC XX.
000104         16  FILLER              PIC X        VALUE '/'.
000105         16  W-ED1-DD            PIC XX.
000106         16  FILLER              PIC X        VALUE '/'.
000107         16  W-ED1-YY            PIC XX.
000108     12  W-EDIT-DATE-2.
000109         16  W-ED2-DD            PIC XX.
000110         16  FILLER              PIC X        VALUE '/'.
000111         16  W-ED2-MM            PIC XX.
000112         16  FILLER              PIC X        VALUE '/'.
000113         16  W-ED2-YY            PIC XX.
000114 01  FILLER.
000115     05  WS-WORK-INT-RATE        PIC 99.999.
000116     05  WS-WORK-INT REDEFINES WS-WORK-INT-RATE
000117                                 PIC X(6).
000118 01  TRAN-DATA-LINE1             PIC X(80)    VALUE
000119     'BEGINJOB mode=''MVS'''.
000120 01  TRAN-DATA-LINE2.
000121*     05  FILLER                  PIC X(39)    VALUE
000122*     '"smtp -f EL152cl2 -t pema,kmsb,jmsb -s '.
000123     05  FILLER                  PIC X(29)    VALUE
000124     '"smtp -f EL152cl2 -t ajra -s '.
000125     05  TRAN-DETAIL.
000126         10  FILLER              PIC XX       VALUE
000127         ''' '.
000128         10  TRAN-DL2-USER       PIC X(05)    VALUE SPACES.
000129         10  TRAN-DL2-ARCHNO     PIC ZZZZ999  VALUE ZEROS.
000130         10  FILLER              PIC X        VALUE SPACES.
000131         10  TRAN-DL2-CCC        PIC X(22)    VALUE SPACES.
000132     05  FILLER                  PIC XX       VALUE '''"'.
000133     05  FILLER                  PIC X(30)    VALUE SPACES.
000134 01  TRAN-DATA-LINE3             PIC X(80)    VALUE
000135     'ENDJOB            '.
000136
000137 01  WS-PRINTER-ID               PIC X(4)    VALUE SPACES.
000138 01  STANDARD-AREAS.
000139     12  WS-ACCT-READ-SW         PIC X       VALUE ' '.
000140     12  WS-COMP-READ-SW         PIC X       VALUE ' '.
000141     12  WS-PROD-READ-SW         PIC X       VALUE ' '.
000142     12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.
000143     12  MAP-NAME.
000144         16  MAP-PREFIX          PIC XX      VALUE 'EL'.
000145         16  MAP-NUMBER          PIC X(4)    VALUE '152A'.
000146         16  MAP-FILLER          PIC XX      VALUE SPACES.
000147     12  GETMAIN-SPACE           PIC X       VALUE SPACE.
000148     12  MAPSET-NAME             PIC X(8)    VALUE 'EL152S'.
000149     12  TRANS-ID                PIC X(4)    VALUE 'EX27'.
000150     12  PRINT-TRANS             PIC X(4)    VALUE 'EX57'.
000151     12  LGXX-ID                 PIC X(4)    VALUE 'LGXX'.
000152     12  PGM-NAME                PIC X(8).
000153     12  TIME-IN                 PIC S9(7).
000154     12  TIME-OUT-R  REDEFINES TIME-IN.
000155         16  FILLER              PIC X.
000156         16  TIME-OUT            PIC 99V99.
000157         16  FILLER              PIC XX.
000158     12  XCTL-005                PIC X(5)    VALUE 'EL005'.
000159     12  XCTL-010                PIC X(5)    VALUE 'EL010'.
000160     12  XCTL-126                PIC X(5)    VALUE 'EL126'.
000161     12  LINK-001                PIC X(5)    VALUE 'EL001'.
000162     12  LINK-004                PIC X(5)    VALUE 'EL004'.
000163     12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.
000164     12  THIS-PGM                PIC X(8)    VALUE 'EL152'.
000165     12  LINK-EL1522             PIC X(8)    VALUE 'EL1522'.
000166     12  PGM-EL126               PIC X(8)    VALUE 'EL126'.
000167     12  PGM-EL150               PIC X(8)    VALUE 'EL150'.
000168     12  PGM-EL141               PIC X(8)    VALUE 'EL141'.
000169     12  PGM-EL1042              PIC X(8)    VALUE 'EL1042'.
000170     12  SUB                     PIC 99.
000171     12  WS-LABELS-SW            PIC X       VALUE SPACE.
000172
000173     12  WS-PI-QID.
000174         16  QID-TERM            PIC X(4)    VALUE SPACES.
000175         16  FILLER              PIC X(4)    VALUE '152A'.
000176
000177     12  W-LETTER-ADDRESS-TYPE.
000178         16  W-LETTER-ADDR-TYPE  PIC  X VALUE SPACES.
000179         16  W-LETTER-ADDR-SEQ   PIC  9 VALUE ZEROS.
000180
000181     12  W-ADDRESS-SELECTION.
000182         16  W-ACCOUNT           PIC  9 VALUE ZEROS.
000183         16  W-BENEFICIARY       PIC  9 VALUE ZEROS.
000184         16  W-EMPLOYER          PIC  9 VALUE ZEROS.
000185         16  W-INSURED           PIC  9 VALUE ZEROS.
000186         16  W-OTHER-1           PIC  9 VALUE ZEROS.
000187         16  W-OTHER-2           PIC  9 VALUE ZEROS.
000188         16  W-PHYSICIAN         PIC  9 VALUE ZEROS.
000189
000190     12  W-Z-CONTROL-DATA.
000191         16  W-NUMBER-OF-COPIES  PIC  9.
000192         16  FILLER              PIC  X.
000193         16  W-DAYS-TO-FOLLOW-UP PIC  999.
000194         16  FILLER              PIC  X.
000195         16  W-DAYS-TO-RESEND-1  PIC  999.
000196         16  FILLER              PIC  X.
000197         16  W-FORM-TO-RESEND    PIC  X(4).
000198         16  FILLER              PIC  X(1).
000199         16  W-PROMPT-LETTER     PIC  X(1).
000200         16  FILLER              PIC  X(1).
000201         16  W-ENCLOSURE-CD      PIC  X(3).
000202         16  FILLER              PIC  X(1).
000203         16  W-AUTO-CLOSE-IND    PIC  X(1).
000204         16  FILLER              PIC  X(1).
000205         16  W-LETTER-TO-BENE    PIC  X(1).
000206
000207     12  W-CREDIT-CARD-LOAN-NO.
000208         16  W-LOAN-NO           PIC  X(08).
000209         16  W-CURRENT-LOAN-NO   PIC  X(12).
000210
000211     12  W-GROUPING.
000212         16  W-GROUP-3           PIC  XXX.
000213         16  FILLER              PIC  XXX.
000214
000215     12  W-NAME.
000216         16  W-FIRST-NAME        PIC  X(12).
000217         16  W-MIDDLE-NAME       PIC  X(12).
000218         16  W-LAST-NAME         PIC  X(15).
000219
000220     12  WS-PHONE-IN             PIC 9(11)   VALUE ZEROS.
000221     12  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
000222         16  FILLER              PIC 9.
000223         16  WSPI-AREA           PIC 9(3).
000224         16  WSPI-PFX            PIC 9(3).
000225         16  WSPI-SFX            PIC 9(4).
000226     12  WS-PHONE-OUT.
000227         16  WSPO-AREA           PIC X(3).
000228         16  FILLER              PIC X       VALUE '-'.
000229         16  WSPO-PFX            PIC X(3).
000230         16  FILLER              PIC X       VALUE '-'.
000231         16  WSPO-SFX            PIC X(4).
000232
000233     12  WS-ZIP-NUMERIC          PIC 9(9).
000234     12  WS-ZIP-NONNUM  REDEFINES  WS-ZIP-NUMERIC
000235                                 PIC X(9).
000236
000237     12  WS-ZIP-CODE.
000238         16  WS-AM-ZIP-CODE      PIC X(5).
000239         16  WS-AM-ZIP-DASH      PIC X.
000240         16  WS-AM-ZIP-PLUS4     PIC X(4).
000241     12  WS-ZIP-CODE-CANADIAN  REDEFINES  WS-ZIP-CODE.
000242         16  WS-CAN-POSTAL-1     PIC XXX.
000243         16  FILLER              PIC X.
000244         16  WS-CAN-POSTAL-2     PIC XXX.
000245         16  FILLER              PIC XXX.
000246
000247     12  WS-LABEL-HOLD-AREA.
000248         16  WS-LABEL-LINES OCCURS 6 TIMES
000249                            INDEXED BY WS-NDX  WS-NDX2.
000250             20  WS-LABEL-ZIP.
000251                 24  WS-LABEL-1ST-ZIP  PIC X(5).
000252                 24  FILLER            PIC X.
000253                 24  WS-LABEL-2ND-ZIP  PIC X(4).
000254             20  FILLER                PIC X(9).
000255             20  WS-LAST-DIGIT         PIC X.
000256             20  WS-LAST-ZIP.
000257                 24  WS-LAST-1ST-ZIP   PIC X(5).
000258                 24  FILLER            PIC X.
000259                 24  WS-LAST-2ND-ZIP   PIC X(4).
000260
000261     12  WS-DATA-FOUND-SW            PIC X.
000262         88  NO-CHARACTERS-FOUND           VALUE 'N'.
000263
000264     12  WS-SKIP-EMAIL           PIC X       VALUE 'N'.
000265     12  WS-STATE-LINE           PIC X       VALUE 'N'.
000266     12  WS-PROCESSOR-LINE       PIC X(01)   VALUE 'N'.
000267     12  WS-CAPS-SW              PIC X(01)   VALUE 'N'.
000268         88  THE-REST-R-CAPS                 VALUE 'Y'.
000269     12  WS-POSITION2            PIC S9(4)   COMP.
000270     12  WS-POSITION21           PIC S9(4)   COMP.
000271     12  WS-WORD-LENGTH          PIC S9(4)   COMP-3.
000272
000273     12  WS-TEMP-AREA1.
000274         16  WS-TEMP-1           PIC X OCCURS 29
000275                                       INDEXED BY TA1.
000276     12  WS-TEMP-AREA2.
000277         16  WS-TEMP-2           PIC X OCCURS 30
000278                                       INDEXED BY TA2
000279                                                  TA21
000280                                                  MOVE-INDX.
000281
000282     12  WS-SAVE-TEMP-AREA2      PIC X(30)   VALUE SPACES.
000283
000284     12  ACCT-BROWSE-STARTED     PIC X       VALUE 'N'.
000285     12  TEXT-BROWSE-STARTED     PIC X       VALUE 'N'.
000286     12  ARCH-BROWSE-STARTED     PIC X       VALUE 'N'.
000287     12  ACTV-BROWSE-STARTED     PIC X       VALUE 'N'.
000288     12  PROD-BROWSE-STARTED     PIC X       VALUE 'N'.
000289     12  INDX-WORK               PIC 99.
000290     12  TEMP-CURR-LINE          PIC S9(3)    COMP-3.
000291
000292     12  DATE-WORK               PIC 9(7).
000293     12  DT-REDEF REDEFINES DATE-WORK.
000294         16  FILLER              PIC XX.
000295         16  DT-WORK             PIC 9(5).
000296
000297     12  BEN-HOLD                PIC XX.
000298
000299     12  BENEFIT-WORK            PIC XXX.
000300     12  BEN-R REDEFINES BENEFIT-WORK.
000301         16  ELIM-DAYS           PIC XX.
000302         16  FILLER              PIC X.
000303
000304     12  GETMAIN-SWITCH          PIC 9        VALUE 0.
000305         88  NO-GETMAIN-DONE-YET              VALUE 0.
000306         88  REFRESH-GETMAIN-AREA             VALUE 1.
000307
000308     12  WORK-AMOUNT             PIC S9(9)V99 VALUE +0.
000309     12  CURRENT-SAVE            PIC XX.
000310     12  CURRENT-PLUS3-SAVE      PIC XX.
000311     12  RESEND-SAVE             PIC XX    VALUE LOW-VALUE.
000312     12  FOLLOW-UP-SAVE          PIC XX    VALUE LOW-VALUE.
000313     12  SEQ-COUNTER             PIC S9(4) COMP.
000314     12  CORR-TRLR-SEQ           PIC S9(4) COMP.
000315     12  DEEDIT-FIELD            PIC X(15).
000316     12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
000317
000318     12  ARCH-SUPPRESS           PIC ZZZZZZZZ99.
000319     12  ARCH-EDIT REDEFINES ARCH-SUPPRESS    PIC X(10).
000320
000321     12  LOWER-CASE PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
000322     12  UPPER-CASE PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
000323
000324     12  WS-CCN-12               PIC X(12).
000325
000326     12  WS-ADDR-TYPE-CD.
000327         16  WS-ADDR-TYPE        PIC X.
000328         16  WS-ADDR-SEQ         PIC X.
000329         16  WS-ADDR-SEQ-NUM REDEFINES
000330             WS-ADDR-SEQ         PIC 9.
000331
000332     12  WS-DMD-CERT-STATE       PIC XX.
000333     12  WS-DMD-CERT-GROUPING    PIC XX.
000334     12  WS-DMD-BEN-CODE         PIC XX.
000335     12  WS-DMD-CORR-TRLR-SEQ    PIC S9(4) COMP.
000336     12  WS-DMD-LETTER-FORM      PIC X(4).
000337     12  WS-DMD-RES-ST           PIC XX.
000338
000339     12  WS-DMD-UND-STATEMENT    PIC X(50)  VALUE
000340          'Central States provides plan administration for'.
000341
000342     12  WS-DMD-UND-COMPANYA     PIC X(50)  VALUE
000343          'Central States Health and Life Co.'.
000344
000345     12  WS-DMD-UND-COMPANYB     PIC X(50)  VALUE
000346          'Central States Indemnity Co.'.
000347
000348* DLO023
000349 01  DL23-COMM-LENGTH            PIC S9(4) COMP VALUE +132.
000350 01  WS-DLO-CODES-TABLE.
000351     12  DL23-SYSTEM-ID          PIC XX.
000352     12  DL23-RECORD-TYPE        PIC XX.
000353     12  DL23-RECORD-KEY         PIC X(6).
000354     12  DL23-RETURN-CODE        PIC XX.
000355     12  DL23-CODE-DESC          PIC X(60).
000356     12  DL23-GEN-DESC-1         PIC X(20).
000357     12  DL23-GEN-DESC-2         PIC X(20).
000358     12  DL23-GEN-DESC-3         PIC X(20).
000359     12  WS-LETTER-STATUS        PIC X        VALUE ' '.
000360     12  WS-BLANK                PIC X        VALUE ' '.
000361
000362 01  WS-SAVE-ACCT-RECORD         PIC X(2000)  VALUE SPACES.
000363 01  WS-SAVE-PRODUCER-RECORD     PIC X(2000)  VALUE SPACES.
000364
000365     EJECT
000366 01  ACCESS-KEYS-AND-FILE-IDS.
000367     12  CNTL-ID                  PIC X(8)    VALUE 'ELCNTL'.
000368     12  ACCT-ID                  PIC X(8)    VALUE 'ERACCT'.
000369     12  CERT-ID                  PIC X(8)    VALUE 'ELCERT'.
000370     12  ARCH-ID                  PIC X(8)    VALUE 'ELARCH'.
000371     12  ARCT-ID                  PIC X(8)    VALUE 'ELARCT'.
000372     12  CLAM-ID                  PIC X(8)    VALUE 'ELMSTR'.
000373     12  TEXT-ID                  PIC X(8)    VALUE 'ELLETR'.
000374     12  ACTV-ID                  PIC X(8)    VALUE 'ELTRLR'.
000375     12  BENE-ID                  PIC X(8)    VALUE 'ELBENE'.
000376     12  PROD-ID                  PIC X(8)    VALUE 'MPPROD'.
000377     12  PLCY-ID                  PIC X(8)    VALUE 'MPPLCY'.
000378     12  PLAN-ID                  PIC X(8)    VALUE 'MPPLAN'.
000379     12  NAPS-ID                  PIC X(8)    VALUE 'ELNAPS'.
000380     12  ENCC-ID                  PIC X(8)    VALUE 'ELENCC'.
000381
000382     12  CNTL-KEY.
000383         16  CNTL-CO              PIC X(3).
000384         16  CNTL-RECORD-TYPE     PIC X       VALUE '1'.
000385         16  CNTL-GENL.
000386           18 CNTL-GEN1           PIC XX      VALUE SPACES.
000387           18 CNTL-GEN2.
000388             20 CNTL-GEN3         PIC X       VALUE SPACES.
000389             20 CNTL-GEN4         PIC X       VALUE SPACES.
000390         16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.
000391
000392      12  WS-ERCOMP-KEY.
000393          16  WS-ERCOMP-COMPANY-CD PIC X.
000394          16  WS-ERCOMP-CARRIER    PIC X.
000395          16  WS-ERCOMP-GROUPING   PIC X(6).
000396          16  WS-ERCOMP-RESP-NO    PIC X(10).
000397          16  WS-ERCOMP-ACCOUNT    PIC X(10).
000398          16  WS-ERCOMP-TYPE       PIC X.
000399
000400     12  CLAM-KEY.
000401         16  CLAM-CO              PIC X.
000402         16  CLAM-CARRIER         PIC X.
000403         16  CLAM-CLAIM           PIC X(7).
000404         16  CLAM-CERT-NUM        PIC X(11).
000405
000406     12  TEXT-KEY.
000407         16  TEXT-PARTIAL-KEY.
000408             20  TEXT-CO          PIC X.
000409             20  TEXT-LETTER      PIC X(4).
000410         16  TEXT-FILLER          PIC X(8)    VALUE SPACES.
000411         16  TEXT-SEQ             PIC S9(4)   VALUE +0    COMP.
000412
000413     12  ACTV-KEY.
000414         16  ACTV-PARTIAL-KEY.
000415             20  ACTV-CO          PIC X.
000416             20  ACTV-CARRIER     PIC X.
000417             20  ACTV-CLAIM       PIC X(7).
000418             20  ACTV-CERT-NUM    PIC X(11).
000419         16  ACTV-SEQ             PIC S9(4)   VALUE +0    COMP.
000420
000421     12  CERT-KEY.
000422         16  CERT-CO              PIC X.
000423         16  CERT-CARRIER         PIC X.
000424         16  CERT-GROUPING        PIC X(6).
000425         16  CERT-STATE           PIC XX.
000426         16  CERT-ACCOUNT         PIC X(10).
000427         16  CERT-EFF-DT          PIC XX.
000428         16  CERT-CERT-NUM        PIC X(11).
000429
000430     12  ACCT-KEY.
000431         16  ACCT-PARTIAL-KEY.
000432             20  ACCT-CO              PIC X.
000433             20  ACCT-CARRIER         PIC X.
000434             20  ACCT-GROUPING        PIC X(6).
000435             20  ACCT-STATE           PIC XX.
000436             20  ACCT-ACCOUNT         PIC X(10).
000437         16  ACCT-EXP-DATE            PIC XX.
000438
000439     12  ARCH-KEY.
000440         16  ARCH-PARTIAL-KEY.
000441             20  ARCH-CO          PIC X.
000442             20  ARCH-NUMBER      PIC S9(8)      COMP.
000443         16  ARCH-REC-TYPE        PIC X.
000444         16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.
000445
000446     12  BENE-KEY.
000447         16  BENE-COMP-CD         PIC X.
000448         16  BENE-REC-TYPE        PIC X.
000449         16  BENE-NUMBER.
000450             20  BENE-CREDITOR    PIC XXX.
000451             20  FILLER           PIC X(7).
000452
000453     12  PROD-KEY.
000454         16  PROD-PARTIAL-KEY.
000455             20  PROD-CO          PIC X.
000456             20  PROD-CARRIER     PIC X.
000457             20  PROD-GROUPING    PIC X(6).
000458             20  PROD-STATE       PIC XX.
000459             20  PROD-PRODUCER    PIC X(10).
000460         16  PROD-EXP-DATE        PIC XX.
000461
000462     12  PLCY-KEY.
000463         16  PLCY-CO              PIC X.
000464         16  PLCY-CARRIER         PIC X.
000465         16  PLCY-GROUPING        PIC X(06).
000466         16  PLCY-STATE           PIC XX.
000467         16  PLCY-PRODUCER        PIC X(10).
000468         16  PLCY-EFF-DT          PIC XX.
000469         16  PLCY-REFERENCE-NO    PIC X(20).
000470
000471     12  PLAN-KEY.
000472         16  PLAN-CO              PIC X.
000473         16  PLAN-CARRIER         PIC X.
000474         16  PLAN-GROUPING        PIC X(6).
000475         16  PLAN-STATE           PIC XX.
000476         16  PLAN-PRODUCER        PIC X(10).
000477         16  PLAN-CODE            PIC XX.
000478         16  PLAN-REV-NO          PIC 999.
000479
000480     12  ELENCC-KEY.
000481         16  ELENCC-COMPANY-CD    PIC X.
000482         16  ELENCC-REC-TYPE      PIC X.
000483         16  ELENCC-ENC-CODE      PIC X(5).
000484         16  F                    PIC X(09).
000485
000486     EJECT
000487     12  ARCH-SAVE-KEY           PIC X(5).
000488     12  ACTV-SAVE-KEY           PIC X(20).
000489     12  ACCT-SAVE-KEY           PIC X(20).
000490     12  PROD-SAVE-KEY           PIC X(20).
000491     12  ACCT-LENGTH             PIC S9(4)  COMP  VALUE +2000.
000492     12  ARCH-LENGTH             PIC S9(4)  COMP  VALUE +90.
000493     12  ARCT-LENGTH             PIC S9(4)  COMP  VALUE +90.
000494     12  ACTV-LENGTH             PIC S9(4)  COMP  VALUE +200.
000495     12  PROD-LENGTH             PIC S9(4)  COMP  VALUE +2000.
000496     12  NAPS-LENGTH             PIC S9(4)  COMP  VALUE +150.
000497     12  ENCC-LENGTH             PIC S9(4)  COMP  VALUE +400.
000498     12  TEXT-SAVE-KEY           PIC X(5).
000499     12  SAVE-VARIABLE-POINTER   PIC S9(8)   COMP.
000500     12  VAR-HOLD.
000501         16  V1                  PIC X.
000502         16  V2                  PIC X.
000503         16  V3                  PIC X.
000504         16  V4                  PIC X.
000505     12  V-HOLD REDEFINES VAR-HOLD.
000506         16  V-NUM               PIC 99.
000507         16  V-PERIOD            PIC X.
000508         16  V-DECIMAL           PIC 9.
000509
000510     12  MAX-LINES               PIC 999     VALUE 300.
000511     12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 13.
000512     12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.
000513     12  TS-GROUP-WORK           PIC 9(5)    VALUE 0     COMP-3.
000514     12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.
000515     12  TS-ITEM                 PIC S9(4)   VALUE +0    COMP.
000516     12  TS-MAP-LENGTH           PIC S9(4)   VALUE +1343 COMP.
000517     12  ROLL-COUNTER            PIC S999    VALUE +0    COMP-3.
000518     12  TS-NAME-TEXT.
000519         16  TS-ID-TEXT          PIC X(4)    VALUE '104A'.
000520         16  TS-ID-TIME REDEFINES TS-ID-TEXT  PIC S9(7) COMP-3.
000521         16  TS-TERM-TEXT.
000522          17 TS-TERM-PREFIX      PIC XX.
000523          17 FILLER              PIC XX.
000524     12  TS-NAME-SCREEN.
000525         16  FILLER              PIC X(4)    VALUE '152X'.
000526         16  TS-TERM-SCREEN      PIC X(4).
000527
000528     12  LINE-NUM.
000529         16  LINE1               PIC X.
000530         16  LINE23              PIC 99.
000531     12  LIN-NUM REDEFINES LINE-NUM  PIC 999.
000532     12  TOP-FORM                PIC X(70)
000533         VALUE '*****TOP OF FORM *****'.
000534     12  SINGLE-LINE             PIC X(70).
000535     12  SINGLE-LINE-BY-1 REDEFINES SINGLE-LINE.
000536         16  ONE-CHAR OCCURS 70 TIMES INDEXED BY INDX1 INDX2
000537                                 PIC X.
000538     12  NAPERSOFT-LETTER        PIC X(16)
000539         VALUE 'Napersoft letter'.
000540
000541     12  WS-RESPONSE             PIC S9(8)   COMP.
000542         88  RESP-NORMAL              VALUE +00.
000543         88  RESP-ERROR               VALUE +01.
000544         88  RESP-NOTFND              VALUE +13.
000545         88  RESP-NOTOPEN             VALUE +19.
000546         88  RESP-ENDFILE             VALUE +20.
000547
000548 01  HAN-LETTER-REASON-DATA.
000549     12  WS-REASON-TEXT.
000550         16  WS-RE-NDX           PIC 99.
000551         16  FILLER              PIC X(68).
000552
000553     12  HAN-REASON-TABLE.
000554         16  FILLER              PIC X(50) VALUE
000555           'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.
000556         16  FILLER              PIC X(50) VALUE
000557           'CHECKING PRE-EXISTING CONDITION                   '.
000558         16  FILLER              PIC X(50) VALUE
000559           'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.
000560         16  FILLER              PIC X(50) VALUE
000561           'LETTER TO INSURED                                 '.
000562         16  FILLER              PIC X(50) VALUE
000563           'LETTER TO CREDITOR                                '.
000564         16  FILLER              PIC X(50) VALUE
000565           'LETTER TO EMPLOYER                                '.
000566         16  FILLER              PIC X(50) VALUE
000567           'LETTER TO INSURED / 2ND REQUEST                   '.
000568         16  FILLER              PIC X(50) VALUE
000569           'LETTER TO CREDITOR / 2ND REQUEST                  '.
000570         16  FILLER              PIC X(50) VALUE
000571           'LETTER TO EMPLOYER / 2ND REQUEST                  '.
000572         16  FILLER              PIC X(50) VALUE
000573           'AWAITING INITIAL CLAIM FORM                       '.
000574         16  FILLER              PIC X(50) VALUE
000575           'AWAITING SUPPLEMENTAL INFORMATION                 '.
000576         16  FILLER              PIC X(50) VALUE
000577           'DENIED / PRE-EXISTING CONDITION                   '.
000578         16  FILLER              PIC X(50) VALUE
000579           'DENIED / WAITING PERIOD NOT MET                   '.
000580         16  FILLER              PIC X(50) VALUE
000581           'DENIED / NORMAL PREGNANCY                         '.
000582         16  FILLER              PIC X(50) VALUE
000583           'DENIED / ACT OF WAR                               '.
000584         16  FILLER              PIC X(50) VALUE
000585           'DENIED / NOT TOTALLY DISABLED                     '.
000586         16  FILLER              PIC X(50) VALUE
000587           'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.
000588         16  FILLER              PIC X(50) VALUE
000589           'DENIED / NO COVERAGE INFORCE                      '.
000590         16  FILLER              PIC X(50) VALUE
000591           'DENIED / DISABLED ON DATE OF LOAN                 '.
000592         16  FILLER              PIC X(50) VALUE
000593           'DENIED / OVER MAXIMUM AGE                         '.
000594         16  FILLER              PIC X(50) VALUE
000595           'CLOSED / CLAIM INFO NOT PROVIDED                  '.
000596         16  FILLER              PIC X(50) VALUE
000597           'PHYSICIAN INFORMATION INCOMPLETE                  '.
000598         16  FILLER              PIC X(50) VALUE
000599           'ACKNOWLEDGEMENT LETTER TO INSURED                 '.
000600         16  FILLER              PIC X(50) VALUE
000601           'DENIED/SUICIDE EXCLUSION                          '.
000602         16  FILLER              PIC X(50) VALUE
000603           'DENIED/LOAN EFFECTIVE BEFORE POLICY EFFECTIVE DATE'.
000604         16  FILLER              PIC X(50) VALUE
000605           'DENIED/JOINT DEBTORS NOT COVERED                  '.
000606         16  FILLER              PIC X(50) VALUE
000607           'DENIED/GROUP POLICY LAPSED                        '.
000608         16  FILLER              PIC X(50) VALUE
000609           'DENIED/DECEASED PRIOR TO POLICY EFFECTIVE DATE    '.
000610         16  FILLER              PIC X(50) VALUE
000611           'DENIED/LOAN TERM IN EXCESS OF MAXIMUM LOAN TERM   '.
000612         16  FILLER              PIC X(50) VALUE
000613           'DENIED/INSURED NOT PERMANENTLY & TOTALLY DISABLED '.
000614         16  FILLER              PIC X(50) VALUE
000615           'DENIED/IME EXAM DOES NOT SUPPORT CONT. DISABILITY '.
000616         16  FILLER              PIC X(50) VALUE
000617           'DENIED/INSURED DID NOT APPEAR FOR IME EXAM        '.
000618
000619     12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.
000620         16  HAN-TABLE-ENTRIES  OCCURS  32  TIMES.
000621             20  HAN-REASON-TEXT PIC X(50).
000622
000623 01  ERROR-MESSAGES.
000624     12  ER-0000                 PIC X(4)  VALUE '0000'.
000625     12  ER-0004                 PIC X(4)  VALUE '0004'.
000626     12  ER-0006                 PIC X(4)  VALUE '0006'.
000627     12  ER-0008                 PIC X(4)  VALUE '0008'.
000628     12  ER-0013                 PIC X(4)  VALUE '0013'.
000629     12  ER-0023                 PIC X(4)  VALUE '0023'.
000630     12  ER-0029                 PIC X(4)  VALUE '0029'.
000631     12  ER-0033                 PIC X(4)  VALUE '0033'.
000632     12  ER-0042                 PIC X(4)  VALUE '0042'.
000633     12  ER-0047                 PIC X(4)  VALUE '0047'.
000634     12  ER-0051                 PIC X(4)  VALUE '0051'.
000635     12  ER-0066                 PIC X(4)  VALUE '0066'.
000636     12  ER-0067                 PIC X(4)  VALUE '0067'.
000637     12  ER-0070                 PIC X(4)  VALUE '0070'.
000638     12  ER-0133                 PIC X(4)  VALUE '0133'.
000639     12  ER-0154                 PIC X(4)  VALUE '0154'.
000640     12  ER-0168                 PIC X(4)  VALUE '0168'.
000641     12  ER-0169                 PIC X(4)  VALUE '0169'.
000642     12  ER-0172                 PIC X(4)  VALUE '0172'.
000643     12  ER-0174                 PIC X(4)  VALUE '0174'.
000644     12  ER-0175                 PIC X(4)  VALUE '0175'.
000645     12  ER-0176                 PIC X(4)  VALUE '0176'.
000646     12  ER-0177                 PIC X(4)  VALUE '0177'.
000647     12  ER-0178                 PIC X(4)  VALUE '0178'.
000648     12  ER-0179                 PIC X(4)  VALUE '0179'.
000649     12  ER-0180                 PIC X(4)  VALUE '0180'.
000650     12  ER-0181                 PIC X(4)  VALUE '0181'.
000651     12  ER-0182                 PIC X(4)  VALUE '0182'.
000652     12  ER-0183                 PIC X(4)  VALUE '0183'.
000653     12  ER-0184                 PIC X(4)  VALUE '0184'.
000654     12  ER-0185                 PIC X(4)  VALUE '0185'.
000655     12  ER-0186                 PIC X(4)  VALUE '0186'.
000656     12  ER-0187                 PIC X(4)  VALUE '0187'.
000657     12  ER-0188                 PIC X(4)  VALUE '0188'.
000658     12  ER-0189                 PIC X(4)  VALUE '0189'.
000659     12  ER-0190                 PIC X(4)  VALUE '0190'.
000660     12  ER-0191                 PIC X(4)  VALUE '0191'.
000661     12  ER-0206                 PIC X(4)  VALUE '0206'.
000662     12  ER-0211                 PIC X(4)  VALUE '0211'.
000663     12  ER-0279                 PIC X(4)  VALUE '0279'.
000664     12  ER-0280                 PIC X(4)  VALUE '0280'.
000665     12  ER-0281                 PIC X(4)  VALUE '0281'.
000666     12  ER-0332                 PIC X(4)  VALUE '0332'.
000667     12  ER-0343                 PIC X(4)  VALUE '0343'.
000668     12  ER-0373                 PIC X(4)  VALUE '0373'.
000669     12  ER-0374                 PIC X(4)  VALUE '0374'.
000670     12  ER-0412                 PIC X(4)  VALUE '0412'.
000671     12  ER-0413                 PIC X(4)  VALUE '0413'.
000672     12  ER-0533                 PIC X(4)  VALUE '0533'.
000673     12  ER-0537                 PIC X(4)  VALUE '0537'.
000674     12  ER-0716                 PIC X(4)  VALUE '0716'.
000675     12  ER-0861                 PIC X(4)  VALUE '0861'.
000676     12  ER-0894                 PIC X(4)  VALUE '0894'.
000677     12  ER-0900                 PIC X(4)  VALUE '0900'.
000678     12  ER-0909                 PIC X(4)  VALUE '0909'.
000679     12  ER-0911                 PIC X(4)  VALUE '0911'.
000680     12  ER-0912                 PIC X(4)  VALUE '0912'.
000681     12  ER-1236                 PIC X(4)  VALUE '1236'.
000682     12  ER-1560                 PIC X(4)  VALUE '1560'.
000683     12  ER-2055                 PIC X(4)  VALUE '2055'.
000684     12  ER-2397                 PIC X(4)  VALUE '2397'.
000685     12  ER-2398                 PIC X(4)  VALUE '2398'.
000686     12  ER-3547                 PIC X(4)  VALUE '3547'.
000687     12  ER-3770                 PIC X(4)  VALUE '3770'.
000688     12  ER-3771                 PIC X(4)  VALUE '3771'.
000689     12  ER-3772                 PIC X(4)  VALUE '3772'.
000690     12  ER-7675                 PIC X(4)  VALUE '7675'.
000691     12  ER-7840                 PIC X(4)  VALUE '7840'.
000692     12  ER-7842                 PIC X(4)  VALUE '7842'.
000693     12  ER-7843                 PIC X(4)  VALUE '7843'.
000694     12  ER-8158                 PIC X(4)  VALUE '8158'.
000695     12  ER-9106                 PIC X(4)  VALUE '9106'.
000696     12  ER-9483                 PIC X(4)  VALUE '9483'.
000697     12  ER-9808                 PIC X(4)  VALUE '9808'.
000698     12  ER-9883                 PIC X(4)  VALUE '9883'.
000699     12  ER-9887                 PIC X(4)  VALUE '9887'.
000700
000701     EJECT
000702****************************************************
000703*       WHEN ADDING OR DELETING ENTRIES TO         *
000704*       THE SYSTEM-SUPPORTED-VARIABLES THE         *
000705*       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
000706*       TO MATCH THE NUMBER OF ENTRIES IN THE      *
000707*       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
000708*       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
000709*       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
000710*                                                  *
000711*   3 FIELDS TO CHANGE:                            *
000712*      1 - SS-NUM-ENTRIES........(NO.OF ENTRIES)   *
000713*      2 - SS-WORK-AREA-LENGTH...(TOTAL LENGTH)    *
000714*      3 - SYSTEM-VARIABLES......( "      "   )    *
000715****************************************************
000716
000717*  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
000718*  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
000719
000720     12  SS-NUM-ENTRIES          PIC 999    VALUE 130      COMP-3.
000721     12  SS-COUNTER              PIC 999                   COMP-3.
000722     12  SS-WORK-AREA-LENGTH     PIC S9(4)  VALUE +3865    COMP.
000723
000724 01  VARIABLE-WORK-AREA.
000725     12  VAR-CODE                PIC X(4).
000726     12  VAR-LEN                 PIC 99.
000727     12  VAR-DATA                PIC X(100).
000728     12  VAR-DATA-R REDEFINES VAR-DATA.
000729       16  VAR-ONE-CHAR OCCURS 100 TIMES INDEXED BY INDXV PIC X.
000730
000731 01  SYSTEM-SUPPORTED-VARIABLES.
000732*****COMPANY NAME
000733     12  SS01                    PIC X(4)  VALUE     '01.0'.
000734     12  SS01L                   PIC 99    VALUE 36.
000735     12  SS01D                   PIC X(30) VALUE ALL '*'.
000736*****FULL COMPANY ADDRESS
000737     12  SS02-1                  PIC X(4)  VALUE     '02.1'.
000738     12  SS02-1L                 PIC 99    VALUE 36.
000739     12  SS02-1D                 PIC X(30) VALUE ALL '*'.
000740     12  SS02-2                  PIC X(4)  VALUE     '02.2'.
000741     12  SS02-2L                 PIC 99    VALUE 36.
000742     12  SS02-2D                 PIC X(30) VALUE ALL '*'.
000743     12  SS02-3                  PIC X(4)  VALUE     '02.3'.
000744     12  SS02-3L                 PIC 99    VALUE 36.
000745     12  SS02-3D                 PIC X(30) VALUE ALL '*'.
000746     12  SS02-4                  PIC X(4)  VALUE     '02.4'.
000747     12  SS02-4L                 PIC 99    VALUE 36.
000748     12  SS02-4D                 PIC X(30) VALUE ALL '*'.
000749     12  SS02-5                  PIC X(4)  VALUE     '02.5'.
000750     12  SS02-5L                 PIC 99    VALUE 36.
000751     12  SS02-5D                 PIC X(30) VALUE ALL '*'.
000752*****CARRIER NAME
000753     12  SS03                    PIC X(4)  VALUE     '03.0'.
000754     12  SS03L                   PIC 99    VALUE 36.
000755     12  SS03D                   PIC X(30) VALUE ALL '*'.
000756*****INVESTORS HERITAGE
000757* as Administrator for Investors Heritage Life Insurance Company
000758     12  SS03-1                  PIC X(4)  VALUE     '03.1'.
000759     12  SS03-1L                 PIC 99    VALUE 68.
000760     12  SS03-1D                 PIC X(62) VALUE ALL '*'.
000761*****FULL CARRIER ADDRESS
000762     12  SS04-1                  PIC X(4)  VALUE     '04.1'.
000763     12  SS04-1L                 PIC 99    VALUE 36.
000764     12  SS04-1D                 PIC X(30) VALUE ALL '*'.
000765     12  SS04-2                  PIC X(4)  VALUE     '04.2'.
000766     12  SS04-2L                 PIC 99    VALUE 36.
000767     12  SS04-2D                 PIC X(30) VALUE ALL '*'.
000768     12  SS04-3                  PIC X(4)  VALUE     '04.3'.
000769     12  SS04-3L                 PIC 99    VALUE 36.
000770     12  SS04-3D                 PIC X(30) VALUE ALL '*'.
000771     12  SS04-4                  PIC X(4)  VALUE     '04.4'.
000772     12  SS04-4L                 PIC 99    VALUE 36.
000773     12  SS04-4D                 PIC X(30) VALUE ALL '*'.
000774     12  SS04-5                  PIC X(4)  VALUE     '04.5'.
000775     12  SS04-5L                 PIC 99    VALUE 36.
000776     12  SS04-5D                 PIC X(30) VALUE ALL '*'.
000777*****CARRIER PHONE NUMBER
000778     12  SS04-6                  PIC X(4)  VALUE     '04.6'.
000779     12  SS04-6L                 PIC 99    VALUE 18.
000780     12  SS04-6D                 PIC X(12) VALUE ALL '*'.
000781*****FULL ADDRESEE LABEL
000782     12  SS05-1                  PIC X(4)  VALUE     '05.1'.
000783     12  SS05-1L                 PIC 99    VALUE 36.
000784     12  SS05-1D                 PIC X(30) VALUE ALL '*'.
000785     12  SS05-2                  PIC X(4)  VALUE     '05.2'.
000786     12  SS05-2L                 PIC 99    VALUE 36.
000787     12  SS05-2D                 PIC X(30) VALUE ALL '*'.
000788     12  SS05-3                  PIC X(4)  VALUE     '05.3'.
000789     12  SS05-3L                 PIC 99    VALUE 36.
000790     12  SS05-3D                 PIC X(30) VALUE ALL '*'.
000791     12  SS05-4                  PIC X(4)  VALUE     '05.4'.
000792     12  SS05-4L                 PIC 99    VALUE 36.
000793     12  SS05-4D                 PIC X(30) VALUE ALL '*'.
000794     12  SS05-5                  PIC X(4)  VALUE     '05.5'.
000795     12  SS05-5L                 PIC 99    VALUE 36.
000796     12  SS05-5D                 PIC X(30) VALUE ALL '*'.
000797     12  SS05-6                  PIC X(4)  VALUE     '05.6'.
000798     12  SS05-6L                 PIC 99    VALUE 36.
000799     12  SS05-6D                 PIC X(30) VALUE ALL '*'.
000800*****ACCOUNT NAME
000801     12  SS06                    PIC X(4)  VALUE     '06.0'.
000802     12  SS06L                   PIC 99    VALUE 36.
000803     12  SS06D                   PIC X(30) VALUE ALL '*'.
000804*****FULL ACCOUNT ADDRESS
000805     12  SS07-1                  PIC X(4)  VALUE     '07.1'.
000806     12  SS07-1L                 PIC 99    VALUE 36.
000807     12  SS07-1D                 PIC X(30) VALUE ALL '*'.
000808     12  SS07-2                  PIC X(4)  VALUE     '07.2'.
000809     12  SS07-2L                 PIC 99    VALUE 36.
000810     12  SS07-2D                 PIC X(30) VALUE ALL '*'.
000811     12  SS07-3                  PIC X(4)  VALUE     '07.3'.
000812     12  SS07-3L                 PIC 99    VALUE 36.
000813     12  SS07-3D                 PIC X(30) VALUE ALL '*'.
000814     12  SS07-4                  PIC X(4)  VALUE     '07.4'.
000815     12  SS07-4L                 PIC 99    VALUE 36.
000816     12  SS07-4D                 PIC X(30) VALUE ALL '*'.
000817     12  SS07-5                  PIC X(4)  VALUE     '07.5'.
000818     12  SS07-5L                 PIC 99    VALUE 36.
000819     12  SS07-5D                 PIC X(30) VALUE ALL '*'.
000820*****ACCOUNT PHONE NUMBER
000821     12  SS07-6                  PIC X(4)  VALUE     '07.6'.
000822     12  SS07-6L                 PIC 99    VALUE 18.
000823     12  SS07-6D                 PIC X(12) VALUE ALL '*'.
000824*****EXECUTING PROCESSOR NAME
000825     12  SS08                    PIC X(4)  VALUE     '08.0'.
000826     12  SS08L                   PIC 99    VALUE 36.
000827     12  SS08D                   PIC X(30) VALUE ALL '*'.
000828*****PROCESSOR TITLE
000829     12  SS09                    PIC X(4)  VALUE     '09.0'.
000830     12  SS09L                   PIC 99    VALUE 32.
000831     12  SS09D                   PIC X(26) VALUE ALL '*'.
000832*****INSUREDS NAME
000833     12  SS10                    PIC X(4)  VALUE     '10.0'.
000834     12  SS10L                   PIC 99    VALUE 36.
000835     12  SS10D                   PIC X(30) VALUE ALL '*'.
000836*****INSUREDS ADDRESS
000837     12  SS11-1                  PIC X(4)  VALUE     '11.1'.
000838     12  SS11-1L                 PIC 99    VALUE 36.
000839     12  SS11-1D                 PIC X(30) VALUE ALL '*'.
000840     12  SS11-2                  PIC X(4)  VALUE     '11.2'.
000841     12  SS11-2L                 PIC 99    VALUE 36.
000842     12  SS11-2D                 PIC X(30) VALUE ALL '*'.
000843     12  SS11-3                  PIC X(4)  VALUE     '11.3'.
000844     12  SS11-3L                 PIC 99    VALUE 36.
000845     12  SS11-3D                 PIC X(30) VALUE ALL '*'.
000846     12  SS11-4                  PIC X(4)  VALUE     '11.4'.
000847     12  SS11-4L                 PIC 99    VALUE 36.
000848     12  SS11-4D                 PIC X(30) VALUE ALL '*'.
000849*****INSUREDS NAME FROM ADDR TRAILER
000850     12  SS11-5                  PIC X(4)  VALUE     '11.5'.
000851     12  SS11-5L                 PIC 99    VALUE 36.
000852     12  SS11-5D                 PIC X(30) VALUE ALL '*'.
000853*****INSUREDS PHONE NUMBER FROM ADDR TRAILER
000854     12  SS11-6                  PIC X(4)  VALUE     '11.6'.
000855     12  SS11-6L                 PIC 99    VALUE 18.
000856     12  SS11-6D                 PIC X(12) VALUE ALL '*'.
000857*****CLAIM TYPE NAME
000858     12  SS12                    PIC X(4)  VALUE     '12.0'.
000859     12  SS12L                   PIC 99    VALUE 12.
000860     12  SS12D                   PIC X(6)  VALUE ALL '*'.
000861*****CLAIM INCURRED DATE
000862     12  SS13                    PIC X(4)  VALUE     '13.0'.
000863     12  SS13L                   PIC 99    VALUE 14.
000864     12  SS13D                   PIC X(8)  VALUE ALL '*'.
000865*****CLAIM REPORTED DATE
000866     12  SS14                    PIC X(4)  VALUE     '14.0'.
000867     12  SS14L                   PIC 99    VALUE 14.
000868     12  SS14D                   PIC X(8)  VALUE ALL '*'.
000869*****LAST PAYMENT DATE
000870     12  SS15                    PIC X(4)  VALUE     '15.0'.
000871     12  SS15L                   PIC 99    VALUE 14.
000872     12  SS15D                   PIC X(8)  VALUE ALL '*'.
000873*****LAST PAYMENT AMOUNT
000874     12  SS16                    PIC X(4)  VALUE     '16.0'.
000875     12  SS16L                   PIC 99    VALUE 17.
000876     12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.
000877*****CLAIM INTEREST RATE
000878     12  SS16-1                  PIC X(4)  VALUE     '16.1'.
000879     12  SS16-1L                 PIC 99    VALUE 13.
000880     12  SS16-1D                 PIC X(7)        VALUE SPACE.
000881*****CLAIM PAID THRU/TO DATE
000882     12  SS17                    PIC X(4)  VALUE     '17.0'.
000883     12  SS17L                   PIC 99    VALUE 14.
000884     12  SS17D                   PIC X(8)  VALUE ALL '*'.
000885*****TOTAL PAID TO DATE
000886     12  SS18                    PIC X(4)  VALUE     '18.0'.
000887     12  SS18L                   PIC 99    VALUE 17.
000888     12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.
000889*****DIAGNOSIS OR CAUSE
000890     12  SS19                    PIC X(4)  VALUE     '19.0'.
000891     12  SS19L                   PIC 99    VALUE 32.
000892     12  SS19D                   PIC X(26) VALUE ALL '*'.
000893*****CAUSE CODE
000894     12  SS19-1                  PIC X(4)  VALUE     '19.1'.
000895     12  SS19-1L                 PIC 99    VALUE 12.
000896     12  SS19-1D                 PIC X(6)  VALUE ALL '*'.
000897*****CID LOAN NUMBER
000898     12  SS19-2                  PIC X(4)  VALUE     '19.2'.
000899     12  SS19-2L                 PIC 99    VALUE 31.
000900     12  SS19-2D                 PIC X(25) VALUE ALL '*'.
000901*****CURRENT DATE
000902     12  SS20                    PIC X(4)  VALUE     '20.0'.
000903     12  SS20L                   PIC 99    VALUE 14.
000904     12  SS20D                   PIC X(8)  VALUE ALL '*'.
000905*****FULL CURRENT DATE
000906     12  SS21                    PIC X(4)  VALUE     '21.0'.
000907     12  SS21L                   PIC 99    VALUE 24.
000908     12  SS21D                   PIC X(18) VALUE ALL '*'.
000909*****BENEFIT DESCRIPTION
000910     12  SS22                    PIC X(4)  VALUE     '22.0'.
000911     12  SS22L                   PIC 99    VALUE 16.
000912     12  SS22D                   PIC X(10) VALUE ALL '*'.
000913*****CARRIER CODE IN CERT
000914     12  SS23                    PIC X(4)  VALUE     '23.0'.
000915     12  SS23L                   PIC 99    VALUE 9.
000916     12  SS23D                   PIC XXX   VALUE ALL '*'.
000917*****GROUPING CODE IN CERT
000918     12  SS24                    PIC X(4)  VALUE     '24.0'.
000919     12  SS24L                   PIC 99    VALUE 12.
000920     12  SS24D                   PIC X(6)  VALUE ALL '*'.
000921*****ACCOUNT NUMBER IN CERT
000922     12  SS25                    PIC X(4)  VALUE     '25.0'.
000923     12  SS25L                   PIC 99    VALUE 16.
000924     12  SS25D                   PIC X(10) VALUE ALL '*'.
000925*****CERTIFICATE NUMBER
000926     12  SS26                    PIC X(4)  VALUE     '26.0'.
000927     12  SS26L                   PIC 99    VALUE 17.
000928     12  SS26D                   PIC X(11) VALUE ALL '*'.
000929*****CERT EFFECTIVE DATE
000930     12  SS27                    PIC X(4)  VALUE     '27.0'.
000931     12  SS27L                   PIC 99    VALUE 14.
000932     12  SS27D                   PIC X(8)  VALUE ALL '*'.
000933*****CERT EXPIRATION DATE
000934     12  SS28                    PIC X(4)  VALUE     '28.0'.
000935     12  SS28L                   PIC 99    VALUE 14.
000936     12  SS28D                   PIC X(8)  VALUE ALL '*'.
000937*****APPLICABLE COVERAGE TERM
000938     12  SS29                    PIC X(4)  VALUE     '29.0'.
000939     12  SS29L                   PIC 99    VALUE 9.
000940     12  SS29D                   PIC XXX   VALUE ALL '*'.
000941*****APPLICABLE COVERAGE AMOUNT
000942     12  SS30                    PIC X(4)  VALUE     '30.0'.
000943     12  SS30L                   PIC 99    VALUE 18.
000944     12  SS30D                   PIC $$$$$,$$$.99  VALUE ZEROS.
000945*****APPLICABLE COVERAGE CANCEL DATE
000946     12  SS31                    PIC X(4)  VALUE     '31.0'.
000947     12  SS31L                   PIC 99    VALUE 14.
000948     12  SS31D                   PIC X(8)  VALUE ALL '*'.
000949*****APPLICABLE COVERAGE FORM NUMBER
000950     12  SS32                    PIC X(4)  VALUE     '32.0'.
000951     12  SS32L                   PIC 99    VALUE 18.
000952     12  SS32D                   PIC X(12) VALUE ALL '*'.
000953*****INSURES AGE AT POLICY ISSUE
000954     12  SS33                    PIC X(4)  VALUE     '33.0'.
000955     12  SS33L                   PIC 99    VALUE 9.
000956     12  SS33D                   PIC XXX   VALUE ALL '*'.
000957*****CLAIM NUMBER
000958     12  SS34                    PIC X(4)  VALUE     '34.0'.
000959     12  SS34L                   PIC 99    VALUE 13.
000960     12  SS34D                   PIC X(7)  VALUE ALL '*'.
000961*****LAST DENIAL TEXT
000962     12  SS35-1                  PIC X(4)  VALUE     '35.1'.
000963     12  SS35-1L                 PIC 99    VALUE 66.
000964     12  SS35-1D                 PIC X(60) VALUE ALL '*'.
000965     12  SS35-2                  PIC X(4)  VALUE     '35.2'.
000966     12  SS35-2L                 PIC 99    VALUE 66.
000967     12  SS35-2D                 PIC X(60) VALUE ALL '*'.
000968*****LOAN NUMBER
000969     12  SS36                    PIC X(4)  VALUE     '36.0'.
000970     12  SS36L                   PIC 99    VALUE 14.
000971     12  SS36D                   PIC X(8)  VALUE ALL '*'.
000972*****CURRENT LOAN NUMBER
000973     12  SS36-1                  PIC X(4)  VALUE     '36.1'.
000974     12  SS36-1L                 PIC 99    VALUE 26.
000975     12  SS36-1D                 PIC X(20) VALUE ALL '*'.
000976*****LOAN BALANCE
000977     12  SS37                    PIC X(4)  VALUE     '37.0'.
000978     12  SS37L                   PIC 99    VALUE 18.
000979     12  SS37D                   PIC $$$$$,$$$.99  VALUE ZEROS.
000980*****MEMBER NUMBER
000981     12  SS38                    PIC X(4)  VALUE     '38.0'.
000982     12  SS38L                   PIC 99    VALUE 18.
000983     12  SS38D                   PIC X(12) VALUE ALL '*'.
000984*****INSURED NAME (FIRST M LAST)
000985     12  SS39                    PIC X(4)  VALUE     '39.0'.
000986     12  SS39L                   PIC 99    VALUE 36.
000987     12  SS39D                   PIC X(30) VALUE ALL '*'.
000988*****INSURED LAST NAME ONLY
000989     12  SS40                    PIC X(4)  VALUE     '40.0'.
000990     12  SS40L                   PIC 99    VALUE 21.
000991     12  SS40D                   PIC X(15) VALUE ALL '*'.
000992*****TITLE (MR/MS)
000993     12  SS41                    PIC X(4)  VALUE     '41.0'.
000994     12  SS41L                   PIC 99    VALUE 9.
000995     12  SS41D                   PIC X(3)  VALUE ALL '*'.
000996*****ELIMINATION PERIOD
000997     12  SS42                    PIC X(4)  VALUE     '42.0'.
000998     12  SS42L                   PIC 99    VALUE 9.
000999     12  SS42D                   PIC X(3)  VALUE ALL '*'.
001000*****BENEFICIARY NAME
001001     12  SS43                    PIC X(4)  VALUE     '43.0'.
001002     12  SS43L                   PIC 99    VALUE 36.
001003     12  SS43D                   PIC X(30) VALUE ALL '*'.
001004*****BENEFICIARY ADDRESS
001005     12  SS44-1                  PIC X(4)  VALUE     '44.1'.
001006     12  SS44-1L                 PIC 99    VALUE 36.
001007     12  SS44-1D                 PIC X(30) VALUE ALL '*'.
001008     12  SS44-2                  PIC X(4)  VALUE     '44.2'.
001009     12  SS44-2L                 PIC 99    VALUE 36.
001010     12  SS44-2D                 PIC X(30) VALUE ALL '*'.
001011     12  SS44-3                  PIC X(4)  VALUE     '44.3'.
001012     12  SS44-3L                 PIC 99    VALUE 36.
001013     12  SS44-3D                 PIC X(30) VALUE ALL '*'.
001014     12  SS44-4                  PIC X(4)  VALUE     '44.4'.
001015     12  SS44-4L                 PIC 99    VALUE 36.
001016     12  SS44-4D                 PIC X(30) VALUE ALL '*'.
001017     12  SS44-5                  PIC X(4)  VALUE     '44.5'.
001018     12  SS44-5L                 PIC 99    VALUE 18.
001019     12  SS44-5D                 PIC X(12) VALUE ALL '*'.
001020     12  SS44-6                  PIC X(4)  VALUE     '44.6'.
001021     12  SS44-6L                 PIC 99    VALUE 36.
001022     12  SS44-6D                 PIC X(30) VALUE ALL '*'.
001023*****INSUREDS DATE OF BIRTH
001024     12  SS45                    PIC X(4)  VALUE     '45.0'.
001025     12  SS45L                   PIC 99    VALUE 14.
001026     12  SS45D                   PIC X(8)  VALUE ALL '*'.
001027*****INSUREDS SOC SEC NUMBER
001028     12  SS46                    PIC X(4)  VALUE     '46.0'.
001029     12  SS46L                   PIC 99    VALUE 17.
001030     12  SS46D                   PIC X(11) VALUE ALL '*'.
001031*****PHYSICIANS  NAME
001032     12  SS47                    PIC X(4)  VALUE     '47.0'.
001033     12  SS47L                   PIC 99    VALUE 36.
001034     12  SS47D                   PIC X(30) VALUE ALL '*'.
001035*****PHYSICIANS  ADDRESS
001036     12  SS47-1                  PIC X(4)  VALUE     '47.1'.
001037     12  SS47-1L                 PIC 99    VALUE 36.
001038     12  SS47-1D                 PIC X(30) VALUE ALL '*'.
001039     12  SS47-2                  PIC X(4)  VALUE     '47.2'.
001040     12  SS47-2L                 PIC 99    VALUE 36.
001041     12  SS47-2D                 PIC X(30) VALUE ALL '*'.
001042     12  SS47-3                  PIC X(4)  VALUE     '47.3'.
001043     12  SS47-3L                 PIC 99    VALUE 36.
001044     12  SS47-3D                 PIC X(30) VALUE ALL '*'.
001045     12  SS47-4                  PIC X(4)  VALUE     '47.4'.
001046     12  SS47-4L                 PIC 99    VALUE 36.
001047     12  SS47-4D                 PIC X(30) VALUE ALL '*'.
001048     12  SS47-5                  PIC X(4)  VALUE     '47.5'.
001049     12  SS47-5L                 PIC 99    VALUE 18.
001050     12  SS47-5D                 PIC X(12) VALUE ALL '*'.
001051*****EMPLOYERS   NAME
001052     12  SS48                    PIC X(4)  VALUE     '48.0'.
001053     12  SS48L                   PIC 99    VALUE 36.
001054     12  SS48D                   PIC X(30) VALUE ALL '*'.
001055*****EMPLOYERS   ADDRESS
001056     12  SS48-1                  PIC X(4)  VALUE     '48.1'.
001057     12  SS48-1L                 PIC 99    VALUE 36.
001058     12  SS48-1D                 PIC X(30) VALUE ALL '*'.
001059     12  SS48-2                  PIC X(4)  VALUE     '48.2'.
001060     12  SS48-2L                 PIC 99    VALUE 36.
001061     12  SS48-2D                 PIC X(30) VALUE ALL '*'.
001062     12  SS48-3                  PIC X(4)  VALUE     '48.3'.
001063     12  SS48-3L                 PIC 99    VALUE 36.
001064     12  SS48-3D                 PIC X(30) VALUE ALL '*'.
001065     12  SS48-4                  PIC X(4)  VALUE     '48.4'.
001066     12  SS48-4L                 PIC 99    VALUE 36.
001067     12  SS48-4D                 PIC X(30) VALUE ALL '*'.
001068     12  SS48-5                  PIC X(4)  VALUE     '48.5'.
001069     12  SS48-5L                 PIC 99    VALUE 18.
001070     12  SS48-5D                 PIC X(12) VALUE ALL '*'.
001071*****OTHER1      NAME
001072     12  SS49                    PIC X(4)  VALUE     '49.0'.
001073     12  SS49L                   PIC 99    VALUE 36.
001074     12  SS49D                   PIC X(30) VALUE ALL '*'.
001075*****OTHER1      ADDRESS
001076     12  SS49-1                  PIC X(4)  VALUE     '49.1'.
001077     12  SS49-1L                 PIC 99    VALUE 36.
001078     12  SS49-1D                 PIC X(30) VALUE ALL '*'.
001079     12  SS49-2                  PIC X(4)  VALUE     '49.2'.
001080     12  SS49-2L                 PIC 99    VALUE 36.
001081     12  SS49-2D                 PIC X(30) VALUE ALL '*'.
001082     12  SS49-3                  PIC X(4)  VALUE     '49.3'.
001083     12  SS49-3L                 PIC 99    VALUE 36.
001084     12  SS49-3D                 PIC X(30) VALUE ALL '*'.
001085     12  SS49-4                  PIC X(4)  VALUE     '49.4'.
001086     12  SS49-4L                 PIC 99    VALUE 36.
001087     12  SS49-4D                 PIC X(30) VALUE ALL '*'.
001088     12  SS49-5                  PIC X(4)  VALUE     '49.5'.
001089     12  SS49-5L                 PIC 99    VALUE 18.
001090     12  SS49-5D                 PIC X(12) VALUE ALL '*'.
001091*****OTHER2      NAME
001092     12  SS50                    PIC X(4)  VALUE     '50.0'.
001093     12  SS50L                   PIC 99    VALUE 36.
001094     12  SS50D                   PIC X(30) VALUE ALL '*'.
001095*****OTHER2      ADDRESS
001096     12  SS50-1                  PIC X(4)  VALUE     '50.1'.
001097     12  SS50-1L                 PIC 99    VALUE 36.
001098     12  SS50-1D                 PIC X(30) VALUE ALL '*'.
001099     12  SS50-2                  PIC X(4)  VALUE     '50.2'.
001100     12  SS50-2L                 PIC 99    VALUE 36.
001101     12  SS50-2D                 PIC X(30) VALUE ALL '*'.
001102     12  SS50-3                  PIC X(4)  VALUE     '50.3'.
001103     12  SS50-3L                 PIC 99    VALUE 36.
001104     12  SS50-3D                 PIC X(30) VALUE ALL '*'.
001105     12  SS50-4                  PIC X(4)  VALUE     '50.4'.
001106     12  SS50-4L                 PIC 99    VALUE 36.
001107     12  SS50-4D                 PIC X(30) VALUE ALL '*'.
001108     12  SS50-5                  PIC X(4)  VALUE     '50.5'.
001109     12  SS50-5L                 PIC 99    VALUE 18.
001110     12  SS50-5D                 PIC X(12) VALUE ALL '*'.
001111*****A&H TERM TIMES MON. BEN.
001112     12  SS51                    PIC X(4)  VALUE     '51.0'.
001113     12  SS51L                   PIC 99    VALUE 17.
001114     12  SS51D                   PIC $$$$,$$$.99 VALUE ZEROS.
001115*****THIRD PARTY NAME
001116     12  SS52                    PIC X(4)  VALUE     '52.0'.
001117     12  SS52L                   PIC 99    VALUE 36.
001118     12  SS52D                   PIC X(30) VALUE ALL '*'.
001119*****THIRD PARTY ADDRESS
001120     12  SS53-1                  PIC X(4)  VALUE     '53.1'.
001121     12  SS53-1L                 PIC 99    VALUE 36.
001122     12  SS53-1D                 PIC X(30) VALUE ALL '*'.
001123     12  SS53-2                  PIC X(4)  VALUE     '53.2'.
001124     12  SS53-2L                 PIC 99    VALUE 36.
001125     12  SS53-2D                 PIC X(30) VALUE ALL '*'.
001126     12  SS53-3                  PIC X(4)  VALUE     '53.3'.
001127     12  SS53-3L                 PIC 99    VALUE 36.
001128     12  SS53-3D                 PIC X(30) VALUE ALL '*'.
001129     12  SS53-4                  PIC X(4)  VALUE     '53.4'.
001130     12  SS53-4L                 PIC 99    VALUE 36.
001131     12  SS53-4D                 PIC X(30) VALUE ALL '*'.
001132     12  SS53-5                  PIC X(4)  VALUE     '53.5'.
001133     12  SS53-5L                 PIC 99    VALUE 36.
001134     12  SS53-5D                 PIC X(30) VALUE ALL '*'.
001135*****THIRD PARTY PHONE NUMBER
001136     12  SS53-6                  PIC X(4)  VALUE     '53.6'.
001137     12  SS53-6L                 PIC 99    VALUE 18.
001138     12  SS53-6D                 PIC X(12) VALUE ALL '*'.
001139*****CERTIFICATE SEQUENCE
001140     12  SS54                    PIC X(4)  VALUE     '54.0'.
001141     12  SS54L                   PIC 99    VALUE 09.
001142     12  SS54D                   PIC XXX VALUE ALL '*'.
001143*****CERTIFICATE TOTAL  E
001144     12  SS55                    PIC X(4)  VALUE     '55.0'.
001145     12  SS55L                   PIC 99    VALUE 09.
001146     12  SS55D                   PIC XXX VALUE ALL '*'.
001147*****CREDITOR ID
001148     12  SS56                    PIC  X(04) VALUE    '56.0'.
001149     12  SS56L                   PIC  99 VALUE 36.
001150     12  SS56D                   PIC  X(30) VALUE ALL '*'.
001151*****INSUREDS NAME (CERTIFICATE)
001152     12  SS57                    PIC X(4)  VALUE     '57.0'.
001153     12  SS57L                   PIC 99    VALUE 36.
001154     12  SS57D                   PIC X(30) VALUE ALL '*'.
001155*****JOINT NAME (CERTIFICATE)
001156     12  SS58                    PIC X(4)  VALUE     '58.0'.
001157     12  SS58L                   PIC 99    VALUE 36.
001158     12  SS58D                   PIC X(30) VALUE ALL '*'.
001159*****POLICY REFERENCE NUMBER
001160     12  SS59                    PIC X(4)  VALUE     '59.0'.
001161     12  SS59L                   PIC 99    VALUE 26.
001162     12  SS59D                   PIC X(20) VALUE ALL '*'.
001163*****CREDIT CARD NUMBER (CLAIMS)
001164     12  SS60                    PIC X(4)  VALUE     '60.0'.
001165     12  SS60L                   PIC 99    VALUE 22.
001166     12  SS60D                   PIC X(16) VALUE ALL '*'.
001167*****CORRESPONDENCE BENEFICIARY NAME
001168     12  SS61                    PIC X(4)  VALUE     '61.0'.
001169     12  SS61L                   PIC 99    VALUE 36.
001170     12  SS61D                   PIC X(30) VALUE ALL '*'.
001171*****CORRESPONDENCE BENEFICIARY ADDRESS
001172     12  SS61-1                  PIC X(4)  VALUE     '61.1'.
001173     12  SS61-1L                 PIC 99    VALUE 36.
001174     12  SS61-1D                 PIC X(30) VALUE ALL '*'.
001175     12  SS61-2                  PIC X(4)  VALUE     '61.2'.
001176     12  SS61-2L                 PIC 99    VALUE 36.
001177     12  SS61-2D                 PIC X(30) VALUE ALL '*'.
001178     12  SS61-3                  PIC X(4)  VALUE     '61.3'.
001179     12  SS61-3L                 PIC 99    VALUE 36.
001180     12  SS61-3D                 PIC X(30) VALUE ALL '*'.
001181     12  SS61-4                  PIC X(4)  VALUE     '61.4'.
001182     12  SS61-4L                 PIC 99    VALUE 36.
001183     12  SS61-4D                 PIC X(30) VALUE ALL '*'.
001184     12  SS61-5                  PIC X(4)  VALUE     '61.5'.
001185     12  SS61-5L                 PIC 99    VALUE 36.
001186     12  SS61-5D                 PIC X(30) VALUE ALL '*'.
001187     12  SS61-6                  PIC X(4)  VALUE     '61.6'.
001188     12  SS61-6L                 PIC 99    VALUE 18.
001189     12  SS61-6D                 PIC X(12) VALUE ALL '*'.
001190
001191*****DMD UNDERWRITER STATEMENT
001192     12  SS62                    PIC X(4)  VALUE     '62.0'.
001193     12  SS62L                   PIC 99    VALUE 56.
001194     12  SS62D                   PIC X(50) VALUE ALL '*'.
001195
001196*****DMD UNDERWRITER NAME
001197     12  SS63                    PIC X(4)  VALUE     '63.0'.
001198     12  SS63L                   PIC 99    VALUE 66.
001199     12  SS63D                   PIC X(60) VALUE ALL '*'.
001200
001201*****DMD UNDERWRITER NAME
001202     12  SS64                    PIC X(4)  VALUE     '64.0'.
001203     12  SS64L                   PIC 99    VALUE 66.
001204     12  SS64D                   PIC X(60) VALUE ALL '*'.
001205
001206****************************************************
001207*       WHEN ADDING OR DELETING ENTRIES TO         *
001208*       THE SYSTEM-SUPPORTED-VARIABLES THE         *
001209*       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
001210*       TO MATCH THE NUMBER OF ENTRIES IN THE      *
001211*       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
001212*       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
001213*       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
001214****************************************************
001215
001216
001217*  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
001218*  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
001219
001220 01  SYSTEM-VARIABLES            PIC X(3865).
001221 01  SYS-VAR-ENTRY.
001222     12  SYS-VAR-CODE            PIC X(4).
001223     12  SYS-VAR-LEN             PIC 99.
001224     12  SYS-VAR-DATA            PIC X(100).
001225
001226
001227
001228*                                COPY ELCDATE.
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
001229
001230
001231*                                COPY ELCLOGOF.
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
001232
001233     EJECT
001234*                                COPY ELCNWA.
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
001235     EJECT
001236*                                COPY ELCATTR.
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
001237     EJECT
001238*                                COPY ELCEMIB.
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
001239 01  EMI-SAVE-AREA               PIC X(400).
001240     EJECT
001241*                                COPY ELCINTF.
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
001242     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
001243*        COPY ELC1042.
      *>>((file: ELC1042))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                           ELC1042                              *
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*    NOTE                                                        *
000008*        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
000009*        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
000010*        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
000011*        TO THESE PROGRAMS.                                      *
000012*                                                                *
000013*    NOTE                                                        *
000014*        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
000015*        USE ONLY!                                               *
000016*                                                                *
000017******************************************************************
000018
000019         16  PI-1042-WA.
000020             20  PI-ACTION       PIC  X(01).
000021                 88 PI-SHOW-MODE           VALUE '1'.
000022                 88 PI-CLEAR-MODE          VALUE '2'.
000023                 88 PI-CREATE-MODE         VALUE '3'.
000024             20  PI-COMM-CONTROL PIC  X(12).
000025             20  PI-CURRENT-LINE PIC S9(03) COMP-3.
000026             20  PI-EOF-SW       PIC  X(01).
000027                 88  PI-FILE-EOF           VALUE 'Y'.
000028             20  PI-FILETYP      PIC  X(01).
000029             20  PI-FORM-SQUEEZE-CONTROL
000030                                 PIC  X(01).
000031                 88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
000032                 88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
000033             20  PI-LAST-CONTROL PIC  X(12).
000034             20  PI-TEMP-STOR-ITEMS
000035                                 PIC S9(04) COMP.
000036             20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
000037             20  PI-UPDATE-SW    PIC  9(01).
000038                 88 ANY-UPDATES            VALUE 1.
000039             20  PI-104-SCREEN-SENT-IND
000040                                 PIC  X(01).
000041                 88  PI-104-SCREEN-SENT    VALUE 'Y'.
000042                 88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
000043             20  PI-1042-SCREEN-SENT-IND
000044                                 PIC  X(01).
000045                 88  PI-1042-SCREEN-SENT    VALUE 'Y'.
000046                 88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
000047             20  PI-1042-ARCHIVE-IND
000048                                 PIC  X(01).
000049                 88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
000050             20  FILLER          PIC  X(29).
      *<<((file: ELC1042))
001244**********************************************************
001245*    NOTE                                                *
001246*        THE WORK AREA IS USED BY EL152 AND EL1522       *
001247*        AND CANNOT BE REARRANGED WITHOUT COMPILING      *
001248*        BOTH PROGRAMS.                                  *
001249*                                                        *
001250**********************************************************
001251         16  PI-EL152-WA.
001252             20  PI-ALT-PRINTER-ID        PIC X(4).
001253             20  PI-ARCHIVE-NUMBER        PIC 9(8).
001254             20  PI-FORM-NUMBER           PIC X(4).
001255             20  PI-ADDR-TYPE             PIC XX.
001256             20  PI-TEMP-STOR-ID          PIC X(8).
001257             20  PI-NUM-PRINT-COPIES      PIC 9.
001258             20  PI-ADDR-SEQ              PIC S9(4)   COMP.
001259             20  PI-PRINT-SW              PIC X.
001260                 88 PRINT-PERFORMED  VALUE '1'.
001261             20  PI-LETTER-ADDRESS-TYPE   PIC XX.
001262             20  PI-RESEND-FORM-NUMBER    PIC X(4).
001263             20  PI-PROMPT-LETTER         PIC X.
001264             20  PI-ENCLOSURE-CD          PIC X(3).
001265             20  PI-AUTO-CLOSE-IND        PIC X(1).
001266             20  PI-LETTER-TO-BENE        PIC X(1).
001267             20  PI-FILLER                PIC X(528).
001268             20  PI-FORCE-7840            PIC X.
001269                 88 FORCE-7840       VALUE '1'.
001270
001271             20  PI-BSR-LETTER-IND        PIC XX.
001272                 88  PI-BSR-AUTOMATED VALUE 'BA'.
001273             20  FILLER REDEFINES PI-BSR-LETTER-IND.
001274                 22  PI-ELLETR-BSR        PIC X.
001275                 22  PI-ELBENE-BSR        PIC X.
001276
001277     EJECT
001278*                            COPY DFHBMSCA.
      *>>((file: DFHBMSCA))
000001*****************************************************************
000002*                                                               *
000003* Copyright (c) 2016-2020 NTT DATA, Inc.                        *
000004* All rights reserved.                                          *
000005*                                                               *
000006*****************************************************************
000007 01    DFHBMSCA.
000008
000009*        VARIABLE  PROTECTION    INTENSITY    MODIFIED
000010*                                             DATA TAG
000011*
000012*        DFHBMUNP  Unprotected   Normal       Off
000013     02  DFHBMUNP PIC X VALUE SPACE.
000014
000015*        DFHBMUNN  Numeric       Normal       Off
000016     02  DFHBMUNN PIC X VALUE "&".
000017
000018*        DFHBMPRO  Protected     Normal       Off
000019     02  DFHBMPRO PIC X VALUE "-".
000020
000021*        DFHBMASK  Autoskip      Normal       Off
000022     02  DFHBMASK PIC X VALUE "0".
000023
000024*************************************************
000025
000026*        DFHBMBRY  Unprotected   Bright       Off
000027     02  DFHBMBRY PIC X VALUE "H".
000028
000029*        DFHPROTI  Protected     Bright       Off
000030     02  DFHPROTI PIC X VALUE "Y".
000031
000032*        DFHBMASB  Autoskip      Bright       Off
000033     02  DFHBMASB PIC X VALUE "8".
000034*************************************************
000035
000036*        DFHBMDAR  Unprotected   Non-Display  Off
000037     02  DFHBMDAR PIC X VALUE "<".
000038
000039*        DFHPROTN  Protected     Non-Display  Off
000040     02  DFHPROTN PIC X VALUE "%".
000041*************************************************
000042
000043*        DFHBMFSE  Unprotected   Normal       On
000044     02  DFHBMFSE PIC X VALUE "A".
000045
000046*        DFHBMASF  Autoskip      Normal       On
000047     02  DFHBMASF  PIC X VALUE "1".
000048
000049*        DFHUNNUM  Numeric       Normal       On
000050     02  DFHUNNUM PIC X VALUE "J".
000051
000052*        DFHBMPRF  Protected     Normal       On
000053     02  DFHBMPRF PIC X VALUE "/".
000054*************************************************
000055
000056*        DFHUNIMD  Unprotected   Bright       On
000057     02  DFHUNIMD PIC X VALUE "I".
000058
000059*        DFHUNINT  Numeric       Bright       On
000060     02  DFHUNINT PIC X VALUE "R".
000061*************************************************
000062
000063*        DFHUNNOD  Unprotected   Non-Display  On
000064     02  DFHUNNOD PIC X VALUE "(".
000065
000066*        DFHUNNON  Numeric       Non-Display  On
000067     02  DFHUNNON PIC X VALUE ")".
000068*************************************************
000069
000070*
000071*     COLOURS
000072*
000073
000074     02  DFHDFCOL PIC X VALUE X"00".
000075     02  DFHBLUE  PIC X VALUE "1".
000076     02  DFHRED   PIC X VALUE "2".
000077     02  DFHPINK  PIC X VALUE "3".
000078     02  DFHGREEN PIC X VALUE "4".
000079     02  DFHTURQ  PIC X VALUE "5".
000080     02  DFHYELLO PIC X VALUE "6".
000081     02  DFHNEUTR PIC X VALUE "7".
000082
000083     02  DFH3270  PIC X VALUE "{".
000084     02  DFHALL   PIC X VALUE LOW-VALUE.
000085     02  DFHBASE  PIC X VALUE LOW-VALUE.
000086     02  DFHBLINK PIC X VALUE "1".
000087     02  DFHBMDET PIC X VALUE LOW-VALUE.
000088     02  DFHBMEOF PIC X VALUE X"80".
000089     02  DFHBMPEM PIC X VALUE X"19".
000090* Newline, DFHBMPNL, is changed to be an ASCII 0x0A
000091     02  DFHBMPNL PIC X VALUE X"0A".
000092     02  DFHCOLOR PIC X VALUE "!".
000093     02  DFHDFHI  PIC X VALUE LOW-VALUE.
000094     02  DFHDFT   PIC X VALUE X"FF".
000095     02  DFHERROR PIC X VALUE X"1A".
000096     02  DFHHLT   PIC X VALUE " ".
000097     02  DFHMENT  PIC X VALUE X"02".
000098     02  DFHBMCUR  PIC X VALUE X"02".
000099     02  DFHBMFLG  PIC X.
000100         88 DFHCURSR VALUES ARE X"02" , X"82".
000101     02  DFHMET   PIC X VALUE X"03".
000102     02  DFHMFE   PIC X VALUE X"06".
000103     02  DFHMFET  PIC X VALUE LOW-VALUE.
000104     02  DFHMFIL  PIC X VALUE X"1C".
000105     02  DFHMFT   PIC X VALUE X"09".
000106     02  DFHMT    PIC X VALUE X"01".
000107     02  DFHPS    PIC X VALUE '"'.
000108     02  DFHREVRS PIC X VALUE "2".
000109     02  DFHSA    PIC X VALUE X"08".
000110     02  DFHUNDLN PIC X VALUE "4".
000111     02  DFHVAL   PIC X VALUE "A".
000112
000113     02  DFHLEFT  PIC X VALUE X"97".
000114     02  DFHOVER  PIC X VALUE X"9C".
000115     02  DFHRIGHT PIC X VALUE X"02".
000116     02  DFHUNDER PIC X VALUE X"01".
000117     02  DFHBOX-BIN  PIC 9(4) COMP VALUE 15.
000118     02  FILLER REDEFINES DFHBOX-BIN.
000119         03  FILLER PIC X.
000120         03  DFHBOX PIC X.
000121     02  DFHSOSI  PIC X VALUE X"01".
000122     02  DFHBMPSO-BIN    PIC 9(4) COMP VALUE 3599.
000123     02  FILLER REDEFINES DFHBMPSO-BIN.
000124         03 DFHBMPSO  PIC X.
000125         03 DFHBMPSI  PIC X.
000126     02  DFHBMEC  PIC X VALUE X"82".
000127*    02  DFHDFFR  PIC X VALUE X"00".
000128*    02  DFHOPAQ  PIC X VALUE X"00".
000129*    02  DFHOUTLN PIC X VALUE X"00".
000130*    02  DFHTRANS PIC X VALUE X"00".
000131*    02  DFHBKTRN PIC X VALUE X"00".
      *<<((file: DFHBMSCA))
001279
001280*                            COPY ELCAID.
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
001281 01  FILLER    REDEFINES DFHAID.
001282     12  FILLER              PIC X(8).
001283     12  PF-VALUES           PIC X       OCCURS 2.
001284     EJECT
001285*                            COPY EL152S.
      *>>((file: EL152S))
000001 01  EL152AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  DATEAL PIC S9(0004) COMP.
000005     05  DATEAF PIC  X(0001).
000006     05  FILLER REDEFINES DATEAF.
000007         10  DATEAA PIC  X(0001).
000008     05  DATEAI PIC  X(0008).
000009*    -------------------------------
000010     05  TIMEAL PIC S9(0004) COMP.
000011     05  TIMEAF PIC  X(0001).
000012     05  FILLER REDEFINES TIMEAF.
000013         10  TIMEAA PIC  X(0001).
000014     05  TIMEAI PIC  X(0005).
000015*    -------------------------------
000016     05  CARRL PIC S9(0004) COMP.
000017     05  CARRF PIC  X(0001).
000018     05  FILLER REDEFINES CARRF.
000019         10  CARRA PIC  X(0001).
000020     05  CARRI PIC  X(0001).
000021*    -------------------------------
000022     05  CLMNOL PIC S9(0004) COMP.
000023     05  CLMNOF PIC  X(0001).
000024     05  FILLER REDEFINES CLMNOF.
000025         10  CLMNOA PIC  X(0001).
000026     05  CLMNOI PIC  X(0007).
000027*    -------------------------------
000028     05  CRTNOL PIC S9(0004) COMP.
000029     05  CRTNOF PIC  X(0001).
000030     05  FILLER REDEFINES CRTNOF.
000031         10  CRTNOA PIC  X(0001).
000032     05  CRTNOI PIC  X(0011).
000033*    -------------------------------
000034     05  PROCL PIC S9(0004) COMP.
000035     05  PROCF PIC  X(0001).
000036     05  FILLER REDEFINES PROCF.
000037         10  PROCA PIC  X(0001).
000038     05  PROCI PIC  X(0004).
000039*    -------------------------------
000040     05  COMPIDL PIC S9(0004) COMP.
000041     05  COMPIDF PIC  X(0001).
000042     05  FILLER REDEFINES COMPIDF.
000043         10  COMPIDA PIC  X(0001).
000044     05  COMPIDI PIC  X(0003).
000045*    -------------------------------
000046     05  CLEANL PIC S9(0004) COMP.
000047     05  CLEANF PIC  X(0001).
000048     05  FILLER REDEFINES CLEANF.
000049         10  CLEANA PIC  X(0001).
000050     05  CLEANI PIC  X(0001).
000051*    -------------------------------
000052     05  SYSL PIC S9(0004) COMP.
000053     05  SYSF PIC  X(0001).
000054     05  FILLER REDEFINES SYSF.
000055         10  SYSA PIC  X(0001).
000056     05  SYSI PIC  X(0008).
000057*    -------------------------------
000058     05  MAINTL PIC S9(0004) COMP.
000059     05  MAINTF PIC  X(0001).
000060     05  FILLER REDEFINES MAINTF.
000061         10  MAINTA PIC  X(0001).
000062     05  MAINTI PIC  X(0001).
000063*    -------------------------------
000064     05  ARCHNUML PIC S9(0004) COMP.
000065     05  ARCHNUMF PIC  X(0001).
000066     05  FILLER REDEFINES ARCHNUMF.
000067         10  ARCHNUMA PIC  X(0001).
000068     05  ARCHNUMI PIC  99999999.
000069*    -------------------------------
000070     05  FORML PIC S9(0004) COMP.
000071     05  FORMF PIC  X(0001).
000072     05  FILLER REDEFINES FORMF.
000073         10  FORMA PIC  X(0001).
000074     05  FORMI PIC  X(0004).
000075*    -------------------------------
000076     05  FOLLOWL PIC S9(0004) COMP.
000077     05  FOLLOWF PIC  X(0001).
000078     05  FILLER REDEFINES FOLLOWF.
000079         10  FOLLOWA PIC  X(0001).
000080     05  FOLLOWI PIC  X(0008).
000081*    -------------------------------
000082     05  RESENDL PIC S9(0004) COMP.
000083     05  RESENDF PIC  X(0001).
000084     05  FILLER REDEFINES RESENDF.
000085         10  RESENDA PIC  X(0001).
000086     05  RESENDI PIC  X(0008).
000087*    -------------------------------
000088     05  PRINTL PIC S9(0004) COMP.
000089     05  PRINTF PIC  X(0001).
000090     05  FILLER REDEFINES PRINTF.
000091         10  PRINTA PIC  X(0001).
000092     05  PRINTI PIC  X(0001).
000093*    -------------------------------
000094     05  COPIESL PIC S9(0004) COMP.
000095     05  COPIESF PIC  X(0001).
000096     05  FILLER REDEFINES COPIESF.
000097         10  COPIESA PIC  X(0001).
000098     05  COPIESI PIC  X(0001).
000099*    -------------------------------
000100     05  ENCL PIC S9(0004) COMP.
000101     05  ENCF PIC  X(0001).
000102     05  FILLER REDEFINES ENCF.
000103         10  ENCA PIC  X(0001).
000104     05  ENCI PIC  X(0003).
000105*    -------------------------------
000106     05  ADDRL PIC S9(0004) COMP.
000107     05  ADDRF PIC  X(0001).
000108     05  FILLER REDEFINES ADDRF.
000109         10  ADDRA PIC  X(0001).
000110     05  ADDRI PIC  X(0002).
000111*    -------------------------------
000112     05  ACTL PIC S9(0004) COMP.
000113     05  ACTF PIC  X(0001).
000114     05  FILLER REDEFINES ACTF.
000115         10  ACTA PIC  X(0001).
000116     05  ACTI PIC  X(0001).
000117*    -------------------------------
000118     05  BENL PIC S9(0004) COMP.
000119     05  BENF PIC  X(0001).
000120     05  FILLER REDEFINES BENF.
000121         10  BENA PIC  X(0001).
000122     05  BENI PIC  X(0001).
000123*    -------------------------------
000124     05  EMPL PIC S9(0004) COMP.
000125     05  EMPF PIC  X(0001).
000126     05  FILLER REDEFINES EMPF.
000127         10  EMPA PIC  X(0001).
000128     05  EMPI PIC  X(0001).
000129*    -------------------------------
000130     05  INSL PIC S9(0004) COMP.
000131     05  INSF PIC  X(0001).
000132     05  FILLER REDEFINES INSF.
000133         10  INSA PIC  X(0001).
000134     05  INSI PIC  X(0001).
000135*    -------------------------------
000136     05  PHYSL PIC S9(0004) COMP.
000137     05  PHYSF PIC  X(0001).
000138     05  FILLER REDEFINES PHYSF.
000139         10  PHYSA PIC  X(0001).
000140     05  PHYSI PIC  X(0001).
000141*    -------------------------------
000142     05  OTHR1L PIC S9(0004) COMP.
000143     05  OTHR1F PIC  X(0001).
000144     05  FILLER REDEFINES OTHR1F.
000145         10  OTHR1A PIC  X(0001).
000146     05  OTHR1I PIC  X(0001).
000147*    -------------------------------
000148     05  OTHR2L PIC S9(0004) COMP.
000149     05  OTHR2F PIC  X(0001).
000150     05  FILLER REDEFINES OTHR2F.
000151         10  OTHR2A PIC  X(0001).
000152     05  OTHR2I PIC  X(0001).
000153*    -------------------------------
000154     05  REL PIC S9(0004) COMP.
000155     05  REF PIC  X(0001).
000156     05  FILLER REDEFINES REF.
000157         10  REA PIC  X(0001).
000158     05  REI PIC  X(0070).
000159*    -------------------------------
000160     05  L1L PIC S9(0004) COMP.
000161     05  L1F PIC  X(0001).
000162     05  FILLER REDEFINES L1F.
000163         10  L1A PIC  X(0001).
000164     05  L1I PIC  X(0003).
000165*    -------------------------------
000166     05  TEXT1L PIC S9(0004) COMP.
000167     05  TEXT1F PIC  X(0001).
000168     05  FILLER REDEFINES TEXT1F.
000169         10  TEXT1A PIC  X(0001).
000170     05  TEXT1I PIC  X(0070).
000171*    -------------------------------
000172     05  L2L PIC S9(0004) COMP.
000173     05  L2F PIC  X(0001).
000174     05  FILLER REDEFINES L2F.
000175         10  L2A PIC  X(0001).
000176     05  L2I PIC  X(0003).
000177*    -------------------------------
000178     05  TEXT2L PIC S9(0004) COMP.
000179     05  TEXT2F PIC  X(0001).
000180     05  FILLER REDEFINES TEXT2F.
000181         10  TEXT2A PIC  X(0001).
000182     05  TEXT2I PIC  X(0070).
000183*    -------------------------------
000184     05  L3L PIC S9(0004) COMP.
000185     05  L3F PIC  X(0001).
000186     05  FILLER REDEFINES L3F.
000187         10  L3A PIC  X(0001).
000188     05  L3I PIC  X(0003).
000189*    -------------------------------
000190     05  TEXT3L PIC S9(0004) COMP.
000191     05  TEXT3F PIC  X(0001).
000192     05  FILLER REDEFINES TEXT3F.
000193         10  TEXT3A PIC  X(0001).
000194     05  TEXT3I PIC  X(0070).
000195*    -------------------------------
000196     05  L4L PIC S9(0004) COMP.
000197     05  L4F PIC  X(0001).
000198     05  FILLER REDEFINES L4F.
000199         10  L4A PIC  X(0001).
000200     05  L4I PIC  X(0003).
000201*    -------------------------------
000202     05  TEXT4L PIC S9(0004) COMP.
000203     05  TEXT4F PIC  X(0001).
000204     05  FILLER REDEFINES TEXT4F.
000205         10  TEXT4A PIC  X(0001).
000206     05  TEXT4I PIC  X(0070).
000207*    -------------------------------
000208     05  L5L PIC S9(0004) COMP.
000209     05  L5F PIC  X(0001).
000210     05  FILLER REDEFINES L5F.
000211         10  L5A PIC  X(0001).
000212     05  L5I PIC  X(0003).
000213*    -------------------------------
000214     05  TEXT5L PIC S9(0004) COMP.
000215     05  TEXT5F PIC  X(0001).
000216     05  FILLER REDEFINES TEXT5F.
000217         10  TEXT5A PIC  X(0001).
000218     05  TEXT5I PIC  X(0070).
000219*    -------------------------------
000220     05  L6L PIC S9(0004) COMP.
000221     05  L6F PIC  X(0001).
000222     05  FILLER REDEFINES L6F.
000223         10  L6A PIC  X(0001).
000224     05  L6I PIC  X(0003).
000225*    -------------------------------
000226     05  TEXT6L PIC S9(0004) COMP.
000227     05  TEXT6F PIC  X(0001).
000228     05  FILLER REDEFINES TEXT6F.
000229         10  TEXT6A PIC  X(0001).
000230     05  TEXT6I PIC  X(0070).
000231*    -------------------------------
000232     05  L7L PIC S9(0004) COMP.
000233     05  L7F PIC  X(0001).
000234     05  FILLER REDEFINES L7F.
000235         10  L7A PIC  X(0001).
000236     05  L7I PIC  X(0003).
000237*    -------------------------------
000238     05  TEXT7L PIC S9(0004) COMP.
000239     05  TEXT7F PIC  X(0001).
000240     05  FILLER REDEFINES TEXT7F.
000241         10  TEXT7A PIC  X(0001).
000242     05  TEXT7I PIC  X(0070).
000243*    -------------------------------
000244     05  L8L PIC S9(0004) COMP.
000245     05  L8F PIC  X(0001).
000246     05  FILLER REDEFINES L8F.
000247         10  L8A PIC  X(0001).
000248     05  L8I PIC  X(0003).
000249*    -------------------------------
000250     05  TEXT8L PIC S9(0004) COMP.
000251     05  TEXT8F PIC  X(0001).
000252     05  FILLER REDEFINES TEXT8F.
000253         10  TEXT8A PIC  X(0001).
000254     05  TEXT8I PIC  X(0070).
000255*    -------------------------------
000256     05  L9L PIC S9(0004) COMP.
000257     05  L9F PIC  X(0001).
000258     05  FILLER REDEFINES L9F.
000259         10  L9A PIC  X(0001).
000260     05  L9I PIC  X(0003).
000261*    -------------------------------
000262     05  TEXT9L PIC S9(0004) COMP.
000263     05  TEXT9F PIC  X(0001).
000264     05  FILLER REDEFINES TEXT9F.
000265         10  TEXT9A PIC  X(0001).
000266     05  TEXT9I PIC  X(0070).
000267*    -------------------------------
000268     05  L10L PIC S9(0004) COMP.
000269     05  L10F PIC  X(0001).
000270     05  FILLER REDEFINES L10F.
000271         10  L10A PIC  X(0001).
000272     05  L10I PIC  X(0003).
000273*    -------------------------------
000274     05  TEXT10L PIC S9(0004) COMP.
000275     05  TEXT10F PIC  X(0001).
000276     05  FILLER REDEFINES TEXT10F.
000277         10  TEXT10A PIC  X(0001).
000278     05  TEXT10I PIC  X(0070).
000279*    -------------------------------
000280     05  L11L PIC S9(0004) COMP.
000281     05  L11F PIC  X(0001).
000282     05  FILLER REDEFINES L11F.
000283         10  L11A PIC  X(0001).
000284     05  L11I PIC  X(0003).
000285*    -------------------------------
000286     05  TEXT11L PIC S9(0004) COMP.
000287     05  TEXT11F PIC  X(0001).
000288     05  FILLER REDEFINES TEXT11F.
000289         10  TEXT11A PIC  X(0001).
000290     05  TEXT11I PIC  X(0070).
000291*    -------------------------------
000292     05  L12L PIC S9(0004) COMP.
000293     05  L12F PIC  X(0001).
000294     05  FILLER REDEFINES L12F.
000295         10  L12A PIC  X(0001).
000296     05  L12I PIC  X(0003).
000297*    -------------------------------
000298     05  TEXT12L PIC S9(0004) COMP.
000299     05  TEXT12F PIC  X(0001).
000300     05  FILLER REDEFINES TEXT12F.
000301         10  TEXT12A PIC  X(0001).
000302     05  TEXT12I PIC  X(0070).
000303*    -------------------------------
000304     05  L13L PIC S9(0004) COMP.
000305     05  L13F PIC  X(0001).
000306     05  FILLER REDEFINES L13F.
000307         10  L13A PIC  X(0001).
000308     05  L13I PIC  X(0003).
000309*    -------------------------------
000310     05  TEXT13L PIC S9(0004) COMP.
000311     05  TEXT13F PIC  X(0001).
000312     05  FILLER REDEFINES TEXT13F.
000313         10  TEXT13A PIC  X(0001).
000314     05  TEXT13I PIC  X(0070).
000315*    -------------------------------
000316     05  ERRMSGL PIC S9(0004) COMP.
000317     05  ERRMSGF PIC  X(0001).
000318     05  FILLER REDEFINES ERRMSGF.
000319         10  ERRMSGA PIC  X(0001).
000320     05  ERRMSGI PIC  X(0072).
000321*    -------------------------------
000322     05  ENTERPFL PIC S9(0004) COMP.
000323     05  ENTERPFF PIC  X(0001).
000324     05  FILLER REDEFINES ENTERPFF.
000325         10  ENTERPFA PIC  X(0001).
000326     05  ENTERPFI PIC  99.
000327*    -------------------------------
000328     05  PRINTERL PIC S9(0004) COMP.
000329     05  PRINTERF PIC  X(0001).
000330     05  FILLER REDEFINES PRINTERF.
000331         10  PRINTERA PIC  X(0001).
000332     05  PRINTERI PIC  X(0004).
000333*    -------------------------------
000334     05  PFKEY9L PIC S9(0004) COMP.
000335     05  PFKEY9F PIC  X(0001).
000336     05  FILLER REDEFINES PFKEY9F.
000337         10  PFKEY9A PIC  X(0001).
000338     05  PFKEY9I PIC  X(0014).
000339 01  EL152AO REDEFINES EL152AI.
000340     05  FILLER            PIC  X(0012).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  DATEAO PIC  X(0008).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  TIMEAO PIC  99.99.
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  CARRO PIC  X(0001).
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  CLMNOO PIC  X(0007).
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  CRTNOO PIC  X(0011).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  PROCO PIC  X(0004).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  COMPIDO PIC  X(0003).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  CLEANO PIC  X(0001).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  SYSO PIC  X(0008).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  MAINTO PIC  X(0001).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  ARCHNUMO PIC  X(0008).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  FORMO PIC  X(0004).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  FOLLOWO PIC  X(0008).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  RESENDO PIC  X(0008).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  PRINTO PIC  X(0001).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  COPIESO PIC  X(0001).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  ENCO PIC  X(0003).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  ADDRO PIC  X(0002).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  ACTO PIC  X(0001).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  BENO PIC  X(0001).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  EMPO PIC  X(0001).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  INSO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  PHYSO PIC  X(0001).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  OTHR1O PIC  X(0001).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  OTHR2O PIC  X(0001).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  REO PIC  X(0070).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  L1O PIC  X(0003).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  TEXT1O PIC  X(0070).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  L2O PIC  X(0003).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  TEXT2O PIC  X(0070).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  L3O PIC  X(0003).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  TEXT3O PIC  X(0070).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  L4O PIC  X(0003).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  TEXT4O PIC  X(0070).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  L5O PIC  X(0003).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  TEXT5O PIC  X(0070).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  L6O PIC  X(0003).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  TEXT6O PIC  X(0070).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  L7O PIC  X(0003).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  TEXT7O PIC  X(0070).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  L8O PIC  X(0003).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  TEXT8O PIC  X(0070).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  L9O PIC  X(0003).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  TEXT9O PIC  X(0070).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  L10O PIC  X(0003).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  TEXT10O PIC  X(0070).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  L11O PIC  X(0003).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  TEXT11O PIC  X(0070).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  L12O PIC  X(0003).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  TEXT12O PIC  X(0070).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  L13O PIC  X(0003).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  TEXT13O PIC  X(0070).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  ERRMSGO PIC  X(0072).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  ENTERPFO PIC  X(0002).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  PRINTERO PIC  X(0004).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  PFKEY9O PIC  X(0014).
000509*    -------------------------------
      *<<((file: EL152S))
001286 01  MAP-REDEF REDEFINES EL152AI.
001287     12  FILLER              PIC X(251).
001288*    12  FILLER              PIC X(189).
001289     12  EL152RI.
001290       14  TEXT-LINES OCCURS 13 TIMES INDEXED BY SC-INDX.
001291         16  SC-LINEL        PIC S9(4)  COMP.
001292         16  SC-LINEA        PIC X.
001293         16  SC-LINE         PIC XXX.
001294         16  SC-TEXTL        PIC S9(4)  COMP.
001295         16  SC-TEXTA        PIC X.
001296         16  SC-TEXT         PIC X(70).
001297     EJECT
001298 01  RECORD-TABLE            PIC X(21900) VALUE SPACES.
001299
001300 01  REC-TABLE REDEFINES RECORD-TABLE.
001301     12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX
001302                             PIC X(3650).
001303
001304 01  REC-ENTRIES REDEFINES RECORD-TABLE.
001305     12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
001306         16  REC-TEXT        PIC X(70).
001307         16  REC-PC          PIC 99.
001308         16  FILLER          PIC X.
001309
001310 01  TS-WORK-AREA            PIC X(3650).
001311     EJECT
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
001313
001314 01  DFHCOMMAREA                 PIC X(1024).
001315
001316
001317 01  var  pic x(30).
001318
001319
001320     EJECT
001321*                                COPY ELCMSTR.
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
001322
001323     EJECT
001324*                                COPY ELCCNTL.
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
001325
001326     EJECT
001327*                                COPY ELCARCH.
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
001328
001329     EJECT
001330*                                COPY ELCARCT.
      *>>((file: ELCARCT))
000001******************************************************************
000002*                                                                *
000003*                            ELCARCT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*   FILE DESCRIPTION = TEMPORARY LETTER ARCHIVE FILE             *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 090  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELARCT                        RKP=2,LEN=8     *
000013*       ALTERNATE PATH1 = ELARCT2  (RECORD TYPE) RKP=10,LEN=8    *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCARCT                          *
000019******************************************************************
000020 01  LETTER-ARCHIVE-TEMP.
000021     12  LT-RECORD-ID                PIC XX.
000022         88  VALID-LT-ID                VALUE 'LT'.
000023
000024     12  LT-CONTROL-PRIMARY.
000025         16  LT-COMPANY-CD           PIC X.
000026         16  LT-ARCHIVE-NO           PIC S9(8)     COMP.
000027         16  LT-RECORD-TYPE          PIC X.
000028             88  LT-HEADER-DATA         VALUE '1'.
000029             88  LT-ADDRESS-DATA        VALUE '2'.
000030             88  LT-TEXT-DATA           VALUE '3'.
000031             88  LT-FORM-CONTROL-HDR    VALUE '4'.
000032         16  LT-LINE-SEQ-NO          PIC S9(4)     COMP.
000033
000034     12  LT-CONTROL-BY-TYPE.
000035         16  LT-COMPANY-CD-A1        PIC X.
000036         16  LT-RECORD-TYPE-A1       PIC X.
000037         16  LT-ARCHIVE-NO-A1        PIC S9(8)     COMP.
000038         16  LT-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
000039
000040     12  LT-TEXT-RECORD.
000041         16  LT-SKIP-CONTROL         PIC XX.
000042         16  LT-TEXT-LINE            PIC X(70).
000043
000044     12  LT-ADDRESS-RECORD  REDEFINES  LT-TEXT-RECORD.
000045         16  FILLER                  PIC XX.
000046         16  LT-ADDRESS-LINE         PIC X(30).
000047         16  FILLER                  PIC X(40).
000048
000049     12  LT-HEADER-RECORD  REDEFINES  LT-TEXT-RECORD.
000050         16  FILLER                  PIC XX.
000051         16  LT-CARRIER              PIC X.
000052         16  LT-CLAIM-NO             PIC X(7).
000053         16  LT-CERT-NO.
000054             20  LT-CERT-PRIME       PIC X(10).
000055             20  LT-CERT-SFX         PIC X.
000056         16  LT-NO-OF-COPIES         PIC S9.
000057         16  LT-RESEND-DATE          PIC XX.
000058         16  LT-PROCESSOR-CD         PIC X(4).
000059         16  LT-CREATION-DT          PIC XX.
000060         16  LT-INITIAL-PRINT-DATE   PIC XX.
000061         16  LT-RESEND-PRINT-DATE    PIC XX.
000062         16  LT-CORR-TRLR-SEQ        PIC S9(4)    COMP.
000063         16  LT-1ST-RESEND-PRINT-DT  PIC XX.
000064         16  LT-DMD-ADDITIONAL-FIELDS.
000065             20  LT-DMD-LETTER-FORM      PIC X(4).
000066             20  LT-DMD-PROD-CODE        PIC XX.
000067             20  LT-DMD-RES-ST           PIC XX.
000068             20  LT-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
000069             20  LT-DMD-LETTER-STATUS    PIC X.
000070                 88  LT-DMD-LETTER-ONLINE   VALUE '1'.
000071                 88  LT-DMD-LETTER-PURGED   VALUE '2'.
000072                 88  LT-DMD-LETTER-RELOADED VALUE '3'.
000073             20  LT-DMD-LETTER-PURGE-DT  PIC XX.
000074             20  LT-DMD-LETTER-RELOAD-DT PIC XX.
000075             20  LT-DMD-UND-CODE         PIC XX.
000076             20  LT-DMD-BEN-CODE         PIC XX.
000077         16  FILLER                  PIC X(15).
000078
000079     12  LT-FORM-CONTROL-HEADER REDEFINES  LT-TEXT-RECORD.
000080         16  FILLER                  PIC XX.
000081         16  LT4-CARRIER             PIC X.
000082         16  LT4-CLAIM-NO            PIC X(7).
000083         16  LT4-CERT-NO.
000084             20  LT4-CERT-PRIME      PIC X(10).
000085             20  LT4-CERT-SFX        PIC X.
000086         16  LT4-NO-OF-COPIES        PIC S9.
000087         16  LT4-RESEND-DATE         PIC XX.
000088         16  LT4-PROCESSOR-CD        PIC X(4).
000089         16  LT4-CREATION-DT         PIC XX.
000090         16  LT4-INITIAL-PRINT-DATE  PIC XX.
000091         16  LT4-RESEND-PRINT-DATE   PIC XX.
000092         16  LT4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
000093         16  LT4-FORM-TYPE           PIC X.
000094             88  LT4-INITIAL-FORM    VALUE '1'.
000095             88  LT4-PROGRESS-FORM   VALUE '2'.
000096         16  LT4-FORM-REM-PRINT-DT   PIC XX.
000097         16  LT4-STATE               PIC XX.
000098         16  FILLER                  PIC X(31).
000099******************************************************************
      *<<((file: ELCARCT))
001331
001332     EJECT
001333*                                COPY ELCCERT.
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
001334
001335     EJECT
001336*                                COPY ERCACCT.
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
001337
001338     EJECT
001339*                                COPY ELCTEXT.
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
001340
001341     EJECT
001342*                                COPY ELCTRLR.
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
001343
001344     EJECT
001345*                                COPY ELCBENE.
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
001346
001347     EJECT
001348*                                COPY ERCCOMP.
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
001349
001350     EJECT
001351*                                COPY MPCPROD.
001352*    EJECT
001353*                                COPY MPCPLCY.
001354*    EJECT
001355*                                COPY MPCPLAN.
001356     EJECT
001357*                                COPY ELCNAPS.
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
001358     EJECT
001359*                                COPY ELCENCC.
      *>>((file: ELCENCC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCENCC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
000010*   A NAPERSOFT DOCUMENT                                         *
000011*                                                                *
000012*   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 400   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
000018*                                                                *
000019*   LOG = NO                                                     *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021*                                                                *
000022*                                                                *
000023******************************************************************
000024*                   C H A N G E   L O G
000025*
000026* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000027*-----------------------------------------------------------------
000028*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000029* EFFECTIVE    NUMBER
000030*-----------------------------------------------------------------
000031* 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
000033******************************************************************
000034
000035 01  ENCLOSURE-CODES.
000036     12  NC-RECORD-ID                      PIC XX.
000037         88  VALID-NC-ID                      VALUE 'NC'.
000038
000039     12  NC-CONTROL-PRIMARY.
000040         16  NC-COMPANY-CD                 PIC X.
000041         16  NC-REC-TYPE                   PIC X.
000042             88  NC-CLAIMS                   VALUE '1'.
000043             88  NC-ADMIN                    VALUE '2'.
000044         16  NC-ENC-CODE                   PIC X(5).
000045         16  FILLER                        PIC X(09).
000046
000047     12  NC-MAINT-INFORMATION.
000048         16  NC-LAST-MAINT-DT              PIC XX.
000049         16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000050         16  NC-LAST-MAINT-USER            PIC X(4).
000051         16  FILLER                        PIC XX.
000052
000053     12  NC-OUTPUT-STACK                   PIC XXX.
000054     12  NC-ENCLOSURE-LINE                 PIC X(100).
000055     12  NC-ATTACHMENTS                    PIC X(255).
000056     12  NC-FUTURE                         PIC X(12).
000057******************************************************************
      *<<((file: ELCENCC))
001360     EJECT
001361*  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
001362*  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
001363
001364*01  SYSTEM-VARIABLES            PIC X(3851).
001365*01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
001366*    12  SYS-VAR-CODE            PIC X(4).
001367*    12  SYS-VAR-LEN             PIC 99.
001368*    12  SYS-VAR-DATA            PIC X(100).
001369
001370     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL152' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
001371 VCOBOL-DUMMY-PROCEDURE.
001372
001373     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
001374     MOVE '5'                   TO DC-OPTION-CODE.
001375     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
001376     MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.
001377     MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.
001378
001379     MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.
001380     MOVE 1                     TO EMI-NUMBER-OF-LINES.
001381     MOVE ERROR-MESSAGE-INTERFACE-BLOCK
001382                                TO EMI-SAVE-AREA.
001383
001384     MOVE EIBTRMID              TO TS-TERM-TEXT
001385                                   TS-TERM-SCREEN
001386                                   QID-TERM.
001387
001388     MOVE SAVE-BIN-DATE       TO CURRENT-SAVE.
001389
001390*    IF PI-COMPANY-ID = 'DMD'
001391*       MOVE CURRENT-SAVE           TO DC-BIN-DATE-1
001392*       MOVE '6'                    TO DC-OPTION-CODE
001393*       MOVE +3                     TO DC-ELAPSED-MONTHS
001394*       MOVE +0                     TO DC-ELAPSED-DAYS
001395*       PERFORM 9700-DATE-LINK THRU 9700-EXIT
001396*       MOVE DC-BIN-DATE-2          TO CURRENT-PLUS3-SAVE.
001397
001398     
      * EXEC CICS HANDLE CONDITION
001399*        PGMIDERR (9600-PGMID-ERROR)
001400*        MAPFAIL  (0325-MAPFAIL)
001401*        ERROR    (9990-ABEND)
001402*    END-EXEC.
      *    MOVE '"$L?.                 ! " #00006989' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' &
                X'202020202020202020202120' &
                X'2220233030303036393839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001403
001404     IF PI-PROCESSOR-ID NOT = 'LGXX'
001405         
      * EXEC CICS READQ TS
001406*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
001407*            INTO    (SECURITY-CONTROL)
001408*            LENGTH  (SC-COMM-LENGTH)
001409*            ITEM    (SC-ITEM)
001410*        END-EXEC
      *    MOVE '*$II   L              ''   #00006996' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036393936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001411         MOVE SC-CLAIMS-DISPLAY (7)  TO  PI-DISPLAY-CAP
001412         MOVE SC-CLAIMS-UPDATE  (7)  TO  PI-MODIFY-CAP.
001413
001414
001415
001416     set P to address of KIXSYS
001417     CALL "getenv" using by value P returning var-ptr
001418     if var-ptr = null then
001419        display ' kixsys not set '
001420     else
001421        set address of var to var-ptr
001422        move 0 to env-var-len
001423        inspect var tallying env-var-len
001424          for characters before X'00'
001425        unstring var (1:env-var-len) delimited by '/'
001426           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
001427              WS-KIX-SYS
001428        end-unstring
001429     end-if
001430
001431     IF EIBCALEN = 0
001432         GO TO 8800-UNAUTHORIZED-ACCESS.
001433
001434     IF PI-CALLING-PROGRAM NOT = THIS-PGM
001435         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
001436             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
001437             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
001438             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
001439             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
001440             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
001441             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
001442             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
001443             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
001444             PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT
001445             PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT
001446             PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT
001447             MOVE SPACES              TO PI-WA
001448                                         PI-FORCE-7840
001449             MOVE ZEROS               TO PI-TOTAL-LINES
001450                                         PI-CURRENT-LINE
001451                                         PI-TEMP-STOR-ITEMS
001452                                         PI-UPDATE-SW
001453                                         PI-ADDR-SEQ
001454             MOVE '2'                 TO PI-ACTION
001455             MOVE LOW-VALUES          TO EL152AO
001456                                         PI-ADDR-TYPE
001457             GO TO 8100-SEND-INITIAL-MAP
001458         ELSE
001459             MOVE PI-CALLING-PROGRAM   TO PGM-NAME
001460             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
001461             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
001462             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
001463             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
001464             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
001465             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
001466             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
001467             MOVE SPACES               TO PI-SAVED-PROGRAM-6
001468             PERFORM 7500-READ-TS           THRU 7599-EXIT
001469             PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT
001470             IF PGM-NAME = PGM-EL141
001471                PERFORM 500-RECOVER-PI-TEMP-STORAGE THRU 599-EXIT
001472                SET TB-INDX TO PI-CURRENT-LINE
001473                PERFORM 400-SET-CODES       THRU 499-EXIT
001474                PERFORM 7000-READ-ADDR      THRU 7099-EXIT
001475                PERFORM 7170-FORMAT-SCREEN  THRU 7170-EXIT
001476                        VARYING SC-INDX FROM 1 BY 1 UNTIL
001477                        SC-INDX > NUM-LINES-PER-SCREEN
001478                MOVE PI-ALT-PRINTER-ID  TO PRINTERO
001479                MOVE AL-UANON           TO PRINTERA
001480                GO TO 8100-SEND-INITIAL-MAP
001481             ELSE
001482                SET TB-INDX TO PI-CURRENT-LINE
001483                PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT
001484                        VARYING SC-INDX FROM 1 BY 1 UNTIL
001485                        SC-INDX > NUM-LINES-PER-SCREEN
001486                MOVE PI-ALT-PRINTER-ID  TO PRINTERO
001487                MOVE AL-UANON           TO PRINTERA
001488                GO TO 8100-SEND-INITIAL-MAP.
001489
001490*0100-PA.
001491*    MOVE ER-0008                TO EMI-ERROR.
001492*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001493*    MOVE -1                     TO MAINTL.
001494*    GO TO 8200-SEND-DATAONLY.
001495
001496     EJECT
001497 0200-RECEIVE.
001498     MOVE TRANS-ID         TO EIBTRNID.
001499     MOVE EMI-SAVE-AREA    TO ERROR-MESSAGE-INTERFACE-BLOCK.
001500     MOVE LOW-VALUES       TO EL152AI.
001501     MOVE '104A'           TO TS-ID-TEXT.
001502     MOVE EIBTRMID         TO TS-TERM-TEXT
001503                              TS-TERM-SCREEN
001504                              QID-TERM.
001505
001506     
      * EXEC CICS HANDLE AID
001507*        CLEAR    (9400-CLEAR)
001508*        PA1      (0100-PA)
001509*        PA2      (0100-PA)
001510*        PA3      (0100-PA)
001511*    END-EXEC.
      *    MOVE '"&=!"#               V! # #00007097' TO DFHEIV0
           MOVE X'22263D212223202020202020' &
                X'202020202020202020562120' &
                X'2320233030303037303937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001512
001513     
      * EXEC CICS HANDLE CONDITION
001514*        MAPFAIL  (0325-MAPFAIL)
001515*    END-EXEC.
      *    MOVE '"$?                   ! $ #00007104' TO DFHEIV0
           MOVE X'22243F202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303037313034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001516
001517     
      * EXEC CICS SYNCPOINT
001518*    END-EXEC.
      *    MOVE '6"                    !   #00007108' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303037313038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001519
001520     IF LOWER-CASE-LETTERS-USED
001521        
      * EXEC CICS RECEIVE
001522*            MAP      (MAP-NAME)
001523*            MAPSET   (MAPSET-NAME)
001524*            INTO     (EL152AI)
001525*            ASIS
001526*       END-EXEC
           MOVE LENGTH OF
            EL152AI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00007112' TO DFHEIV0
           MOVE X'382254414920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037313132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001527      ELSE
001528        
      * EXEC CICS RECEIVE
001529*            MAP      (MAP-NAME)
001530*            MAPSET   (MAPSET-NAME)
001531*            INTO     (EL152AI)
001532*       END-EXEC.
           MOVE LENGTH OF
            EL152AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00007119' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037313139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001533
001534     IF NOT DISPLAY-CAP AND PI-PROCESSOR-ID NOT = 'LGXX'
001535         MOVE 'READ'             TO  SM-READ
001536         PERFORM 9995-SECURITY-VIOLATION
001537         MOVE ER-0070            TO  EMI-ERROR
001538         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001539         GO TO 8100-SEND-INITIAL-MAP.
001540
001541     INSPECT MAINTI CONVERTING LOWER-CASE TO UPPER-CASE.
001542     MOVE 'N'                    TO CLEANI
001543
001544     IF ENTERPFL = 0
001545         GO TO 0300-CHECK-PFKEYS.
001546     IF EIBAID NOT = DFHENTER
001547         MOVE ER-0004            TO EMI-ERROR
001548         GO TO 0320-INPUT-ERROR.
001549     IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
001550         MOVE PF-VALUES (ENTERPFI) TO EIBAID
001551     ELSE
001552         MOVE ER-0029            TO EMI-ERROR
001553         GO TO 0320-INPUT-ERROR.
001554
001555 0300-CHECK-PFKEYS.
001556     IF EIBAID = DFHPF23
001557         GO TO 8810-PF23.
001558     IF EIBAID = DFHPF24
001559         GO TO 9200-RETURN-MAIN-MENU.
001560*    IF EIBAID = DFHPF12
001561*        GO TO 9500-PF12.
001562     IF EIBAID = DFHPF1
001563         MOVE 7                  TO ROLL-COUNTER
001564         GO TO 7900-ROLL-PAGE.
001565     IF EIBAID = DFHPF2
001566         MOVE -7                 TO ROLL-COUNTER
001567         GO TO 7900-ROLL-PAGE.
001568     IF EIBAID = DFHPF3
001569         GO TO 6100-ADDR-MAINT.
001570     IF EIBAID = DFHPF4
001571         IF MODIFY-CAP
001572             GO TO 6200-EDIT-MODE
001573         ELSE
001574             MOVE 'UPDATE'       TO  SM-READ
001575             PERFORM 9995-SECURITY-VIOLATION
001576             MOVE ER-0070        TO  EMI-ERROR
001577             GO TO 0320-INPUT-ERROR.
001578
001579     IF EIBAID = DFHPF5
001580         GO TO 0330-FUNCTION-CHECK.
001581
001582     IF EIBAID = DFHPF6
001583         GO TO 0330-FUNCTION-CHECK.
001584
001585     IF EIBAID = DFHPF7
001586        COMPUTE ROLL-COUNTER = ((PI-TOTAL-LINES - 1) * -1)
001587        GO TO 7900-ROLL-PAGE.
001588
001589     IF EIBAID = DFHPF8
001590        MOVE PI-TOTAL-LINES TO ROLL-COUNTER
001591        GO TO 7900-ROLL-PAGE.
001592
001593*    IF EIBAID = DFHPF9 AND
001594*       PI-COMPANY-ID = 'DMD'
001595*        MOVE '1'           TO PI-FORCE-7840
001596*        GO TO 0330-FUNCTION-CHECK.
001597
001598     IF EIBAID = DFHENTER
001599         GO TO 0330-FUNCTION-CHECK.
001600
001601     PERFORM 7500-READ-TS        THRU 7599-EXIT
001602
001603     PERFORM 7950-SET-INDX THRU 7950-EXIT.
001604     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
001605             VARYING SC-INDX FROM 1 BY 1
001606             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
001607
001608     MOVE ER-0029                TO EMI-ERROR.
001609 0320-INPUT-ERROR.
001610     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001611     MOVE AL-UNBON               TO ENTERPFA.
001612     IF ENTERPFL = 0
001613         MOVE -1                 TO MAINTL
001614     ELSE
001615         MOVE -1                 TO ENTERPFL.
001616
001617     GO TO 8200-SEND-DATAONLY.
001618
001619
001620 0100-PA.
001621     MOVE ER-0008                TO EMI-ERROR.
001622     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001623     MOVE -1                     TO MAINTL.
001624     GO TO 8200-SEND-DATAONLY.
001625
001626
001627 0325-MAPFAIL.
001628***********************************************************
001629*      ROUTINE SHOULD ONLY BE PERFORMED WHEN PRINTING     *
001630*      LETTERS ON A 3275 PRINTER.                         *
001631***********************************************************
001632
001633     PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT.
001634
001635     SET TB-INDX         TO PI-CURRENT-LINE.
001636     PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT
001637                  VARYING SC-INDX FROM 1 BY 1 UNTIL
001638                  SC-INDX > NUM-LINES-PER-SCREEN.
001639
001640     GO TO 8100-SEND-INITIAL-MAP.
001641
001642     EJECT
001643 0330-FUNCTION-CHECK.
001644     IF NOT MODIFY-CAP
001645         IF MAINTI = 'S'
001646             NEXT SENTENCE
001647         ELSE
001648             MOVE 'UPDATE'           TO  SM-READ
001649             PERFORM 9995-SECURITY-VIOLATION
001650             MOVE ER-0070            TO  EMI-ERROR
001651             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001652             GO TO 8100-SEND-INITIAL-MAP.
001653
001654     PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
001655
001656     IF EIBAID = DFHPF5
001657      IF PI-PROMPT-LETTER = 'Y'
001658         MOVE ER-0894            TO EMI-ERROR
001659         MOVE -1             TO MAINTL
001660         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001661         MOVE AL-UABON       TO MAINTA
001662         GO TO 8200-SEND-DATAONLY
001663      ELSE
001664         IF MAINTI NOT = 'C'
001665             MOVE ER-0716        TO EMI-ERROR
001666             MOVE -1             TO MAINTL
001667             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001668             MOVE AL-UABON       TO MAINTA
001669             GO TO 8200-SEND-DATAONLY
001670         ELSE
001671             IF MODIFY-CAP
001672                 GO TO 6400-LETTER-RELEASE
001673             ELSE
001674                 IF PRINT-PERFORMED
001675                     GO TO 6400-LETTER-RELEASE
001676                 ELSE
001677                     MOVE AL-UNBON TO ENTERPFA
001678                     MOVE -1       TO ENTERPFL
001679                     MOVE ER-2398 TO EMI-ERROR
001680                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001681                     GO TO 8200-SEND-DATAONLY.
001682
001683     IF EIBAID = DFHPF6
001684      IF PI-PROMPT-LETTER = 'Y'
001685         MOVE ER-0894            TO EMI-ERROR
001686         MOVE -1             TO MAINTL
001687         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001688         MOVE AL-UABON       TO MAINTA
001689         GO TO 8200-SEND-DATAONLY
001690      ELSE
001691         GO TO 7800-PRINT-LETTER-NOW.
001692
001693     IF MAINTI = 'S'
001694         GO TO 1000-SHOW.
001695
001696     IF MAINTI = 'C'
001697         GO TO 2000-CREATE.
001698
001699     IF MAINTI = 'R'
001700        IF NOT MODIFY-CAP
001701           MOVE ER-0070          TO EMI-ERROR
001702           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001703           MOVE LOW-VALUES       TO EL152AO
001704           GO TO 8100-SEND-INITIAL-MAP
001705          ELSE
001706           GO TO 3000-RECORD.
001707
001708     PERFORM 7500-READ-TS        THRU 7599-EXIT
001709     PERFORM 7950-SET-INDX       THRU 7950-EXIT
001710     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN
001711                                 THRU 7960-EXIT
001712        VARYING SC-INDX FROM 1 BY 1 UNTIL
001713        SC-INDX > NUM-LINES-PER-SCREEN
001714
001715     MOVE ER-0023                TO EMI-ERROR.
001716     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001717     MOVE -1                     TO MAINTL.
001718     MOVE AL-UABON               TO MAINTA.
001719     GO TO 8200-SEND-DATAONLY.
001720
001721     EJECT
001722 0350-EDIT-ROUTINE.
001723     PERFORM 400-SET-CODES THRU 499-EXIT.
001724     MOVE PI-CLAIM-NO    TO CLAM-CLAIM
001725                            ACTV-CLAIM.
001726
001727     IF ADDRL NOT = +0
001728        INSPECT ADDRI CONVERTING LOWER-CASE TO UPPER-CASE
001729        MOVE ADDRI           TO WS-ADDR-TYPE-CD
001730        IF WS-ADDR-TYPE = 'I' OR 'B' OR 'A' OR 'P' OR
001731                          'O' OR 'Q' OR 'E'
001732           MOVE AL-UANON         TO ADDRA
001733        ELSE
001734           MOVE -1               TO ADDRL
001735           MOVE AL-UABON         TO ADDRA
001736           MOVE ER-0176          TO EMI-ERROR
001737           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001738*     ELSE
001739*       IF PI-COMPANY-ID = 'DMD'
001740*        IF MAINTI = 'C' OR 'R'
001741*          MOVE -1               TO ADDRL
001742*          MOVE AL-UABON         TO ADDRA
001743*          MOVE ER-0861          TO EMI-ERROR
001744*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001745
001746     IF ADDRL NOT = +0
001747        IF WS-ADDR-SEQ NOT NUMERIC
001748           MOVE -1               TO ADDRL
001749           MOVE AL-UABON         TO ADDRA
001750           MOVE ER-0176          TO EMI-ERROR
001751           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001752
001753*    IF ADDRL NOT = +0
001754*       IF PI-COMPANY-ID = 'DMD'
001755*          IF WS-ADDR-TYPE = 'B' AND
001756*             WS-ADDR-SEQ NOT = '9'
001757*                MOVE -1               TO ADDRL
001758*                MOVE AL-UABON         TO ADDRA
001759*                MOVE ER-7842          TO EMI-ERROR
001760*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001761
001762     IF FORML NOT = ZEROS
001763        INSPECT FORMI CONVERTING LOWER-CASE TO UPPER-CASE.
001764
001765     IF MAINTI = 'C' AND FORML = ZEROS
001766        MOVE -1                  TO FORML
001767        MOVE ER-0177             TO EMI-ERROR
001768        MOVE AL-UABON            TO FORMA
001769        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001770
001771     IF FOLLOWL NOT = ZEROS
001772        MOVE FOLLOWI             TO DEEDIT-FIELD
001773        PERFORM 8600-DEEDIT
001774        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
001775        MOVE '4'                 TO DC-OPTION-CODE
001776        PERFORM 9700-DATE-LINK  THRU  9700-EXIT
001777        IF DATE-CONVERSION-ERROR
001778           MOVE ER-0182          TO EMI-ERROR
001779           MOVE -1               TO FOLLOWL
001780           MOVE AL-UABON         TO FOLLOWA
001781           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001782        ELSE
001783*          IF (PI-COMPANY-ID = 'DMD') AND
001784*             (NOT FORCE-7840)        AND
001785*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
001786*              MOVE 'PF9=FORCE 7840' TO PFKEY9O
001787*              MOVE AL-SABON         TO PFKEY9A
001788*              MOVE ER-7840          TO EMI-ERROR
001789*              MOVE -1               TO FOLLOWL
001790*              MOVE AL-UABON         TO FOLLOWA
001791*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001792*          ELSE
001793             MOVE SPACES                TO PFKEY9O
001794             MOVE AL-SADON              TO PFKEY9A
001795             MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWO
001796             MOVE AL-UANON              TO FOLLOWA
001797             MOVE DC-BIN-DATE-1         TO FOLLOW-UP-SAVE
001798     ELSE
001799        MOVE LOW-VALUES                 TO FOLLOW-UP-SAVE.
001800
001801     IF FOLLOW-UP-SAVE NOT = LOW-VALUES
001802        IF FOLLOW-UP-SAVE NOT > CURRENT-SAVE
001803           MOVE ER-0533          TO EMI-ERROR
001804           MOVE AL-UABON         TO FOLLOWA
001805           MOVE -1               TO FOLLOWL
001806           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001807
001808     IF RESENDL NOT = ZEROS
001809        MOVE RESENDI             TO DEEDIT-FIELD
001810        PERFORM 8600-DEEDIT
001811        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
001812        MOVE '4'                 TO DC-OPTION-CODE
001813        PERFORM 9700-DATE-LINK  THRU  9700-EXIT
001814        IF DATE-CONVERSION-ERROR
001815           MOVE ER-0185          TO EMI-ERROR
001816           MOVE -1               TO RESENDL
001817           MOVE AL-UABON         TO RESENDA
001818           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001819         ELSE
001820*          IF (PI-COMPANY-ID = 'DMD') AND
001821*             (NOT FORCE-7840)        AND
001822*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
001823*              MOVE 'PF9=FORCE 7840' TO PFKEY9O
001824*              MOVE AL-SABON         TO PFKEY9A
001825*              MOVE ER-7840          TO EMI-ERROR
001826*              MOVE -1               TO RESENDL
001827*              MOVE AL-UABON         TO RESENDA
001828*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001829*          ELSE
001830             MOVE SPACES                 TO PFKEY9O
001831             MOVE AL-SADON               TO PFKEY9A
001832             MOVE DC-GREG-DATE-1-EDIT    TO RESENDO
001833             MOVE AL-UANON               TO RESENDA
001834             MOVE DC-BIN-DATE-1          TO RESEND-SAVE
001835     ELSE
001836        MOVE LOW-VALUE                   TO RESEND-SAVE.
001837
001838     IF RESEND-SAVE NOT = LOW-VALUES
001839        IF RESEND-SAVE NOT > CURRENT-SAVE
001840           MOVE ER-0537          TO EMI-ERROR
001841           MOVE AL-UABON         TO RESENDA
001842           MOVE -1               TO RESENDL
001843           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001844
001845     IF PRINTL NOT = ZEROS
001846        INSPECT PRINTI CONVERTING LOWER-CASE TO UPPER-CASE
001847        IF PRINTI = 'Y' OR = ' '
001848           MOVE AL-UANON         TO PRINTA
001849        ELSE
001850           MOVE ER-0183          TO EMI-ERROR
001851           MOVE -1               TO PRINTL
001852           MOVE AL-UABON         TO PRINTA
001853           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001854
001855     MOVE 'Y'                    TO CLEANO
001856     IF ENCL <= ZEROS
001857        MOVE 'N'                 TO CLEANO
001858     END-IF
001859
001860     IF ENCL > ZEROS
001861        MOVE FUNCTION UPPER-CASE(ENCI) TO ENCI
001862                                          PI-ENCLOSURE-CD
001863        MOVE SPACES             TO ELENCC-KEY
001864        MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
001865        MOVE '1'                TO ELENCC-REC-TYPE
001866        MOVE ENCI               TO ELENCC-ENC-CODE
001867
001868        
      * EXEC CICS READ
001869*           DATASET    (ENCC-ID)
001870*           SET        (ADDRESS OF ENCLOSURE-CODES)
001871*           RIDFLD     (ELENCC-KEY)
001872*           RESP       (WS-RESPONSE)
001873*       END-EXEC
      *    MOVE '&"S        E          (  N#00007459' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037343539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ENCC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001874
001875        IF RESP-NORMAL
001876           MOVE ENCI             TO PI-ENCLOSURE-CD
001877                                    W-ENCLOSURE-CD
001878           MOVE AL-UANON         TO ENCA
001879        ELSE
001880           MOVE 'N'              TO CLEANO
001881           MOVE ER-1560          TO EMI-ERROR
001882           MOVE -1               TO ENCL
001883           MOVE AL-UABON         TO ENCA
001884           PERFORM 9900-ERROR-FORMAT
001885                                 THRU 9900-EXIT
001886        END-IF
001887     END-IF
001888
001889     IF COPIESL NOT = ZEROS
001890        IF COPIESI NOT NUMERIC OR
001891           COPIESI = '0'
001892           MOVE ER-0184          TO EMI-ERROR
001893           MOVE -1               TO COPIESL
001894           MOVE AL-UABON         TO COPIESA
001895           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001896        ELSE
001897           MOVE AL-UANON         TO COPIESA.
001898
001899     IF PI-PROCESSOR-PRINTER NOT = SPACES
001900         MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINTER-ID.
001901
001902     MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
001903
001904     IF PRINTERL > ZERO
001905        INSPECT PRINTERI CONVERTING LOWER-CASE TO UPPER-CASE
001906        MOVE AL-UANON            TO PRINTERA
001907        MOVE PRINTERI            TO PI-ALT-PRINTER-ID
001908                                    PI-ALT-DMD-PRT-ID
001909     ELSE
001910        IF (PI-NO-CARRIER-SECURITY AND
001911            PI-NO-ACCOUNT-SECURITY) OR
001912            PI-PROCESSOR-PRINTER NOT = SPACES
001913             NEXT SENTENCE
001914        ELSE
001915            MOVE AL-UABON       TO PRINTERA
001916            MOVE -1             TO PRINTERL
001917            MOVE ER-2397        TO EMI-ERROR
001918            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001919
001920     IF ACTL NOT = +0
001921         MOVE ZEROS              TO W-ACCOUNT
001922         IF ACTI > SPACES
001923             IF ACTI NUMERIC
001924                     AND
001925                ACTI NOT < 1
001926                     AND
001927                ACTI NOT > 9
001928                 MOVE AL-UANON   TO ACTA
001929                 MOVE ACTI       TO W-ACCOUNT
001930                                    W-LETTER-ADDR-SEQ
001931                 MOVE 'A'        TO W-LETTER-ADDR-TYPE
001932             ELSE
001933                 MOVE -1         TO ACTL
001934                 MOVE AL-UABON   TO ACTA
001935                 MOVE ER-3547    TO EMI-ERROR
001936                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001937
001938     IF BENL NOT = +0
001939         MOVE ZEROS              TO W-BENEFICIARY
001940         IF BENI > SPACES
001941             IF BENI NUMERIC
001942                     AND
001943                BENI NOT < 1
001944                     AND
001945                BENI NOT > 9
001946                 MOVE AL-UANON   TO BENA
001947                 MOVE BENI       TO W-BENEFICIARY
001948                                    W-LETTER-ADDR-SEQ
001949                 MOVE 'B'        TO W-LETTER-ADDR-TYPE
001950             ELSE
001951                 MOVE -1         TO BENL
001952                 MOVE AL-UABON   TO BENA
001953                 MOVE ER-3547    TO EMI-ERROR
001954                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001955
001956     IF EMPL NOT = +0
001957         MOVE ZEROS              TO W-EMPLOYER
001958         IF EMPI > SPACES
001959             IF EMPI NUMERIC
001960                     AND
001961                EMPI NOT < 1
001962                     AND
001963                EMPI NOT > 9
001964                 MOVE AL-UANON   TO EMPA
001965                 MOVE EMPI       TO W-EMPLOYER
001966                                    W-LETTER-ADDR-SEQ
001967                 MOVE 'E'        TO W-LETTER-ADDR-TYPE
001968             ELSE
001969                 MOVE -1         TO EMPL
001970                 MOVE AL-UABON   TO EMPA
001971                 MOVE ER-3547    TO EMI-ERROR
001972                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001973
001974     IF INSL NOT = +0
001975         MOVE ZEROS              TO W-INSURED
001976         IF INSI > SPACES
001977             IF INSI NUMERIC
001978                     AND
001979                INSI NOT < 1
001980                     AND
001981                INSI NOT > 9
001982                 MOVE AL-UANON   TO INSA
001983                 MOVE INSI       TO W-INSURED
001984                                    W-LETTER-ADDR-SEQ
001985                 MOVE 'I'        TO W-LETTER-ADDR-TYPE
001986             ELSE
001987                 MOVE -1         TO INSL
001988                 MOVE AL-UABON   TO INSA
001989                 MOVE ER-3547    TO EMI-ERROR
001990                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001991
001992     IF PHYSL NOT = +0
001993         MOVE ZEROS              TO W-PHYSICIAN
001994         IF PHYSI > SPACES
001995             IF PHYSI NUMERIC
001996                     AND
001997                PHYSI NOT < 1
001998                     AND
001999                PHYSI NOT > 9
002000                 MOVE AL-UANON   TO PHYSA
002001                 MOVE PHYSI      TO W-PHYSICIAN
002002                                    W-LETTER-ADDR-SEQ
002003                 MOVE 'P'        TO W-LETTER-ADDR-TYPE
002004             ELSE
002005                 MOVE -1         TO PHYSL
002006                 MOVE AL-UABON   TO PHYSA
002007                 MOVE ER-3547    TO EMI-ERROR
002008                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002009
002010     IF OTHR1L NOT = +0
002011         MOVE ZEROS              TO W-OTHER-1
002012         IF OTHR1I > SPACES
002013             IF OTHR1I NUMERIC
002014                     AND
002015                OTHR1I NOT < 1
002016                     AND
002017                OTHR1I NOT > 9
002018                 MOVE AL-UANON   TO OTHR1A
002019                 MOVE OTHR1I     TO W-OTHER-1
002020                                    W-LETTER-ADDR-SEQ
002021                 MOVE 'O'        TO W-LETTER-ADDR-TYPE
002022             ELSE
002023                 MOVE -1         TO OTHR1L
002024                 MOVE AL-UABON   TO OTHR1A
002025                 MOVE ER-3547    TO EMI-ERROR
002026                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002027
002028     IF OTHR2L NOT = +0
002029         MOVE ZEROS              TO W-OTHER-2
002030         IF OTHR2I > SPACES
002031             IF OTHR2I NUMERIC
002032                     AND
002033                OTHR2I NOT < 1
002034                     AND
002035                OTHR2I NOT > 9
002036                 MOVE AL-UANON   TO OTHR2A
002037                 MOVE OTHR2I     TO W-OTHER-2
002038                                    W-LETTER-ADDR-SEQ
002039                 MOVE 'Q'        TO W-LETTER-ADDR-TYPE
002040             ELSE
002041                 MOVE -1         TO OTHR2L
002042                 MOVE AL-UABON   TO OTHR2A
002043                 MOVE ER-3547    TO EMI-ERROR
002044                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002045
002046     IF NOT EMI-NO-ERRORS
002047        GO TO 8200-SEND-DATAONLY.
002048
002049 0350-EXIT.
002050      EXIT.
002051     EJECT
002052 400-SET-CODES.
002053     MOVE PI-COMPANY-ID          TO CNTL-CO.
002054     MOVE PI-COMPANY-CD          TO CLAM-CO
002055                                    TEXT-CO
002056                                    ACTV-CO
002057                                    CERT-CO
002058                                    ACCT-CO
002059                                    ARCH-CO
002060                                    PROD-CO
002061                                    PLCY-CO
002062                                    PLAN-CO.
002063
002064     MOVE PI-CARRIER             TO CLAM-CARRIER
002065                                    ACTV-CARRIER
002066                                    CERT-CARRIER
002067                                    ACCT-CARRIER
002068                                    PROD-CARRIER
002069                                    PLCY-CARRIER
002070                                    PLAN-CARRIER.
002071
002072     MOVE PI-CERT-NO             TO CLAM-CERT-NUM
002073                                    ACTV-CERT-NUM
002074                                    CERT-CERT-NUM.
002075 499-EXIT.
002076      EXIT.
002077
002078 500-RECOVER-PI-TEMP-STORAGE.
002079
002080     
      * EXEC CICS HANDLE CONDITION
002081*        QIDERR   (590-QIDERR)
002082*    END-EXEC.
      *    MOVE '"$N                   ! % #00007671' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303037363731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002083
002084     
      * EXEC CICS READQ TS
002085*        QUEUE    (WS-PI-QID)
002086*        INTO     (PROGRAM-INTERFACE-BLOCK)
002087*        LENGTH   (PI-COMM-LENGTH)
002088*    END-EXEC.
      *    MOVE '*$I    L              ''   #00007675' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037363735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002089
002090     PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT.
002091
002092     GO TO 599-EXIT.
002093
002094 590-QIDERR.
002095
002096     IF EIBTRNID = TRANS-ID
002097         MOVE ER-0033                   TO  EMI-ERROR
002098         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002099         GO TO 8200-SEND-DATAONLY.
002100
002101 599-EXIT.
002102     EXIT.
002103
002104     EJECT
002105 1000-SHOW.
002106***************************************************************
002107*     THIS ROUTINE WILL BROWSE THE ARCHIVE FILE WITH THE      *
002108*     ARCHIVE NUMBER SPECIFIED FROM THE SCREEN. THE TEXT      *
002109*     WILL BE INSERTED INTO THE TS-TABLE AND DISPLAYED TO     *
002110*     OPERATOR.                                               *
002111***************************************************************
002112
002113     MOVE SPACES                 TO RECORD-TABLE.
002114     IF ARCHNUML = ZEROS
002115        MOVE -1                  TO ARCHNUML
002116        MOVE AL-UNBON            TO ARCHNUMA
002117        MOVE ER-0174             TO EMI-ERROR
002118        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002119        GO TO 8200-SEND-DATAONLY.
002120
002121     IF ARCHNUMI NOT NUMERIC
002122        MOVE -1                  TO ARCHNUML
002123        MOVE AL-UNBON            TO ARCHNUMA
002124        MOVE ER-0175             TO EMI-ERROR
002125        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002126        GO TO 8200-SEND-DATAONLY.
002127
002128     IF PI-SHOW-MODE
002129*       IF PI-ARCHIVE-NUMBER = ARCHNUMI
002130*          MOVE -1               TO MAINTL
002131*          GO TO 8200-SEND-DATAONLY
002132*       ELSE
002133           PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
002134
002135     MOVE ARCHNUMI               TO ARCH-NUMBER
002136                                    PI-ARCHIVE-NUMBER.
002137     MOVE ' '                    TO ARCH-REC-TYPE.
002138     MOVE '1'                    TO PI-ACTION.
002139     MOVE ZEROS                  TO PI-ADDR-SEQ.
002140     MOVE LOW-VALUES             TO PI-ADDR-TYPE.
002141     MOVE SPACES                 TO PI-PRINT-SW
002142                                    PI-FORM-NUMBER.
002143     SET TB-INDX TO 1.
002144
002145     MOVE SPACE                  TO WS-LETTER-STATUS.
002146
002147     
      * EXEC CICS HANDLE CONDITION
002148*         NOTOPEN    (8850-ARCH-NOT-OPEN)
002149*         NOTFND     (1020-ENDBR)
002150*         ENDFILE    (1020-ENDBR)
002151*    END-EXEC.
      *    MOVE '"$JI''                 ! & #00007738' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2620233030303037373338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002152
002153     MOVE ARCH-PARTIAL-KEY       TO ARCH-SAVE-KEY.
002154
002155     
      * EXEC CICS STARTBR
002156*         DATASET    (ARCH-ID)
002157*         RIDFLD     (ARCH-KEY)
002158*         GTEQ
002159*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007746' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303037373436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002160
002161     MOVE 'Y'                    TO ARCH-BROWSE-STARTED.
002162
002163 1010-READ-NEXT.
002164     
      * EXEC CICS READNEXT
002165*         SET       (ADDRESS OF LETTER-ARCHIVE)
002166*         DATASET   (ARCH-ID)
002167*         RIDFLD    (ARCH-KEY)
002168*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007755' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037373535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002169
002170     IF LA-FORM-CONTROL-HDR
002171        GO TO 1020-ENDBR.
002172
002173
002174     IF NOT LA-HEADER-DATA
002175         NEXT SENTENCE
002176     ELSE
002177         IF NOT LA-CSO-LETTER-PURGED
002178             NEXT SENTENCE
002179         ELSE
002180             MOVE 'P'            TO WS-LETTER-STATUS
002181             GO TO 1020-ENDBR.
002182
002183     IF ARCH-PARTIAL-KEY = ARCH-SAVE-KEY
002184        IF LA-HEADER-DATA
002185           PERFORM 1040-FORMAT-RESEND
002186           MOVE LA-NO-OF-COPIES    TO COPIESO
002187           PERFORM 1050-GET-CORRESPOND THRU 1059-EXIT
002188           GO TO 1010-READ-NEXT
002189        ELSE
002190           IF LA-ADDRESS-DATA
002191              MOVE LA-ADDRESS-LINE   TO REC-TEXT (TB-INDX)
002192              MOVE ZEROS             TO REC-PC (TB-INDX)
002193              SET TB-INDX UP BY 1
002194              GO TO 1010-READ-NEXT
002195           ELSE
002196              PERFORM 1030-TEXT-BUILD
002197              GO TO 1010-READ-NEXT.
002198
002199 1020-ENDBR.
002200     IF ARCH-BROWSE-STARTED = 'Y'
002201        MOVE 'N'                TO ARCH-BROWSE-STARTED
002202        
      * EXEC CICS ENDBR
002203*            DATASET   (ARCH-ID)
002204*       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007793' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037373933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002205     END-IF.
002206
002207*    IF PI-COMPANY-ID = 'CID'
002208         IF TB-INDX = 1
002209             IF WS-LETTER-STATUS EQUAL 'P'
002210                 MOVE 9021           TO EMI-ERROR
002211                 MOVE -1             TO ARCHNUML
002212                 MOVE AL-UNBON       TO ARCHNUMA
002213                 PERFORM 9900-ERROR-FORMAT THRU
002214                         9900-EXIT
002215                 GO TO 8200-SEND-DATAONLY
002216             ELSE
002217                 MOVE ER-0006             TO EMI-ERROR
002218                 MOVE -1                  TO ARCHNUML
002219                 MOVE AL-UNBON            TO ARCHNUMA
002220                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002221                 GO TO 8200-SEND-DATAONLY
002222             END-IF
002223         END-IF
002224*    ELSE
002225*        IF TB-INDX = 1
002226*            MOVE ER-0006             TO EMI-ERROR
002227*            MOVE -1                  TO ARCHNUML
002228*            MOVE AL-UNBON            TO ARCHNUMA
002229*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002230*            GO TO 8200-SEND-DATAONLY
002231*        END-IF
002232*    END-IF
002233
002234
002235     SET TB-INDX DOWN BY 1.
002236     SET PI-TOTAL-LINES          TO TB-INDX.
002237     MOVE 1                      TO PI-CURRENT-LINE.
002238     SET TB-INDX TO 1.
002239
002240     PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
002241             VARYING SC-INDX FROM 1 BY 1
002242               UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002243
002244     MOVE SPACES                 TO PI-ADDR-TYPE
002245                                    MAINTI.
002246     MOVE -1                     TO MAINTL.
002247
002248     GO TO 8100-SEND-INITIAL-MAP.
002249
002250 1030-TEXT-BUILD.
002251***** IF THERE ARE NO ADDRESS LINES OR LESS THAN 6 LINES, SET TO
002252***** TOP OF FORM.
002253     IF TB-INDX < 7
002254        SET TB-INDX   TO 7.
002255
002256     IF TB-INDX = 7
002257        MOVE TOP-FORM            TO REC-TEXT (TB-INDX)
002258        SET TB-INDX UP BY 1.
002259
002260     MOVE LA-TEXT-LINE           TO REC-TEXT (TB-INDX).
002261     MOVE LA-SKIP-CONTROL        TO REC-PC (TB-INDX)
002262                                    INDX-WORK.
002263
002264     IF INDX-WORK NOT = 99
002265        SET TB-INDX UP BY 1
002266        SET TB-INDX UP BY INDX-WORK
002267     ELSE
002268        SET TB-INDX UP BY 1
002269        MOVE TOP-FORM            TO REC-TEXT (TB-INDX)
002270        SET TB-INDX UP BY 1.
002271
002272 1040-FORMAT-RESEND.
002273     IF LA-RESEND-DATE = LOW-VALUES
002274        MOVE SPACES                 TO RESENDI
002275       ELSE
002276        MOVE LA-RESEND-DATE         TO DC-BIN-DATE-1
002277        MOVE ' '                    TO DC-OPTION-CODE
002278        PERFORM 9700-DATE-LINK  THRU 9700-EXIT
002279        MOVE DC-GREG-DATE-1-EDIT    TO RESENDO.
002280
002281 1050-GET-CORRESPOND.
002282     MOVE LA-CARRIER             TO ACTV-CARRIER.
002283     MOVE LA-CLAIM-NO            TO ACTV-CLAIM.
002284     MOVE LA-CERT-NO             TO ACTV-CERT-NUM.
002285     MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ.
002286
002287     
      * EXEC CICS READ
002288*         DATASET     (ACTV-ID)
002289*         RIDFLD      (ACTV-KEY)
002290*         SET         (ADDRESS OF ACTIVITY-TRAILERS)
002291*    END-EXEC.
      *    MOVE '&"S        E          (   #00007878' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037383738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002292
002293     IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES
002294        MOVE SPACES                TO FOLLOWI
002295     ELSE
002296        MOVE SPACES                TO DC-OPTION-CODE
002297        MOVE AT-RECEIPT-FOLLOW-UP  TO DC-BIN-DATE-1
002298        PERFORM 9700-DATE-LINK THRU 9700-EXIT
002299        MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWI.
002300
002301     MOVE SPACES                 TO ADDRI
002302     MOVE AT-STD-LETTER-FORM     TO FORMI
002303     MOVE AT-REASON-TEXT         TO REI.
002304
002305 1059-EXIT.
002306      EXIT.
002307     EJECT
002308 2000-CREATE.
002309***************************************************************
002310*    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
002311*    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
002312*    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
002313*    WILL BE DISPLAYED ONTO THE SCREEN.                       *
002314*                                                             *
002315***************************************************************
002316
002317***************************************************************
002318*    CHECK TO SEE IF SAME REQUEST OR NOT.                     *
002319*    IF NEW REQUEST AND A LETTER WAS PRINTED, FORCE AN ERROR  *
002320***************************************************************
002321
002322     IF PI-CREATE-MODE
002323        IF PI-FORM-NUMBER = FORMI AND PI-ADDR-TYPE = ADDRI AND
002324           PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE
002325           PERFORM 7500-READ-TS  THRU 7599-EXIT
002326           PERFORM 7950-SET-INDX THRU 7950-EXIT
002327           PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002328             VARYING SC-INDX FROM 1 BY 1
002329             UNTIL SC-INDX > NUM-LINES-PER-SCREEN
002330           MOVE -1               TO MAINTL
002331*          MOVE 'Y'              TO CLEANO
002332           GO TO 8200-SEND-DATAONLY
002333        ELSE
002334           IF PRINT-PERFORMED AND
002335              PI-RETURN-TO-PROGRAM  =  PGM-EL150 AND
002336              FORMI NOT = 9999
002337              MOVE ER-0279           TO EMI-ERROR
002338              MOVE -1                TO MAINTL
002339              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002340              GO TO 8200-SEND-DATAONLY.
002341
002342***************************************************************
002343*   IF THE FORM NUMBER IS THE SAME BUT THE ADDRESS TYPE       *
002344*   HAS CHANGED, THEN SAVE THE CURRENT SCREEN AND REBUILD     *
002345*   THE ADDRESS DATA.                                         *
002346***************************************************************
002347
002348     IF PI-FORM-NUMBER = FORMI AND
002349        PI-ADDR-TYPE  =  ADDRI AND
002350        PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE
002351           MOVE -1                  TO MAINTL
002352           GO TO 8200-SEND-DATAONLY.
002353
002354     IF PI-LETTER-ADDRESS-TYPE NOT = W-LETTER-ADDRESS-TYPE
002355         MOVE 0                  TO GETMAIN-SWITCH
002356         MOVE 'N'                TO WS-ACCT-READ-SW
002357                                    WS-PROD-READ-SW
002358                                    WS-COMP-READ-SW.
002359
002360     MOVE SPACES                 TO RECORD-TABLE.
002361
002362     PERFORM 7000-READ-ADDR THRU 7099-EXIT.
002363
002364     SET TB-INDX TO 1
002365     MOVE TOP-FORM               TO REC-TEXT (TB-INDX).
002366     SET TB-INDX UP BY 1.
002367
002368***************************************************************
002369*    IF A NEW LETTER IS BEING CREATED FROM SCRATCH, SAVE      *
002370*    THE EXISTING SCREEN AND PASS CONTROL TO THE TEXT EDITOR  *
002371***************************************************************
002372
002373     IF FORMI = '9999'
002374        MOVE 16                  TO PI-TOTAL-LINES
002375        MOVE 1                   TO PI-CURRENT-LINE
002376        PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
002377        PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT
002378        MOVE '3'                 TO PI-ACTION
002379        MOVE FORMI               TO PI-FORM-NUMBER
002380                                    PI-COMM-CONTROL
002381        MOVE ZEROS               TO PI-UPDATE-SW
002382        MOVE PGM-EL1042          TO PGM-NAME
002383        GO TO 9300-XCTL.
002384
002385     MOVE FORMI                  TO TEXT-LETTER.
002386     MOVE TEXT-PARTIAL-KEY       TO TEXT-SAVE-KEY.
002387
002388     
      * EXEC CICS HANDLE CONDITION
002389*         NOTFND     (2120-ENDBR)
002390*         ENDFILE    (2120-ENDBR)
002391*         NOTOPEN    (8890-TEXT-NOT-OPEN)
002392*    END-EXEC
      *    MOVE '"$I''J                 ! '' #00007979' TO DFHEIV0
           MOVE X'222449274A20202020202020' &
                X'202020202020202020202120' &
                X'2720233030303037393739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002393
002394     
      * EXEC CICS STARTBR
002395*         DATASET    (TEXT-ID)
002396*         RIDFLD     (TEXT-KEY)
002397*         GTEQ
002398*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007985' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303037393835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002399
002400     MOVE 'Y'                    TO TEXT-BROWSE-STARTED.
002401
002402 2110-READ-NEXT.
002403     IF TB-INDX > MAX-LINES
002404        MOVE ER-0051             TO EMI-ERROR
002405        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002406        GO TO 2120-ENDBR.
002407
002408     
      * EXEC CICS READNEXT
002409*         DATASET    (TEXT-ID)
002410*         SET        (ADDRESS OF TEXT-FILES)
002411*         RIDFLD     (TEXT-KEY)
002412*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007999' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037393939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002413
002414     IF TEXT-PARTIAL-KEY NOT = TEXT-SAVE-KEY
002415        IF TB-INDX = +2
002416           MOVE SPACES           TO PI-FORM-NUMBER
002417           MOVE -1               TO FORML
002418           MOVE AL-UABON         TO FORMA
002419           MOVE ER-1236          TO EMI-ERROR
002420           MOVE 'N'              TO CLEANO
002421           PERFORM 9900-ERROR-FORMAT
002422                                 THRU 9900-EXIT
002423        END-IF
002424        GO TO 2120-ENDBR
002425     END-IF
002426
002427
002428     MOVE FORMI                  TO PI-FORM-NUMBER.
002429
002430     IF TX-LINE-SQUEEZE-CONTROL = 'Z'
002431         PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT
002432         GO TO 2110-READ-NEXT.
002433
002434      MOVE TX-BSR-CODE           TO PI-ELLETR-BSR.
002435
002436      IF (PI-COMPANY-ID = 'CID' OR 'AHL')
002437         AND (PI-CARRIER NOT = '8')
002438         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
002439            B1 > +20
002440            IF TX-TEXT-LINE (B1:5) = '@03.1'
002441               GO TO 2110-READ-NEXT
002442            END-IF
002443         END-PERFORM
002444      END-IF
002445      MOVE TX-TEXT-LINE          TO REC-TEXT (TB-INDX).
002446      MOVE TX-PROCESS-CONTROL    TO REC-PC (TB-INDX)
002447                                    INDX-WORK.
002448      SET TB-INDX UP BY 1.
002449      IF INDX-WORK = 99
002450         MOVE TOP-FORM           TO REC-TEXT (TB-INDX)
002451         SET TB-INDX UP BY 1
002452         GO TO 2110-READ-NEXT
002453      ELSE
002454         SET TB-INDX UP BY INDX-WORK
002455         GO TO 2110-READ-NEXT.
002456
002457 2120-ENDBR.
002458     IF TEXT-BROWSE-STARTED = 'Y'
002459        MOVE 'N'                 TO TEXT-BROWSE-STARTED
002460        
      * EXEC CICS ENDBR
002461*            DATASET     (TEXT-ID)
002462*       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008051' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038303531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002463
002464*02160      IF TB-INDX = 8
002465*02161         MOVE ER-0006             TO EMI-ERROR
002466*02162         MOVE -1                  TO FORML
002467*02163         MOVE AL-UABON            TO FORMA
002468*02164         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002469*02165         GO TO 8200-SEND-DATAONLY-ERASEAUP.
002470
002471     SET TB-INDX DOWN BY 1.
002472     SET PI-TOTAL-LINES          TO TB-INDX.
002473     MOVE 1                      TO PI-CURRENT-LINE.
002474
002475***************************************************************
002476*    IF IT IS A DESIGNATED CLAIM, THEN RESOLVE ALL            *
002477*    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *
002478***************************************************************
002479
002480     IF PI-RETURN-TO-PROGRAM = PGM-EL150
002481        PERFORM 7200-RESOLVE-VARIABLES THRU 7269-EXIT
002482        MOVE SPACES              TO W-REVERSE-DATE-SW
002483        PERFORM 7300-VARIABLE-SEARCH   THRU 7399-EXIT
002484                VARYING TB-INDX FROM 7 BY 1 UNTIL
002485                TB-INDX > PI-TOTAL-LINES
002486     ELSE
002487        MOVE ER-0373             TO EMI-ERROR
002488        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002489
002490     SET TB-INDX TO 1.
002491     PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
002492             VARYING SC-INDX FROM 1 BY 1 UNTIL
002493             SC-INDX > NUM-LINES-PER-SCREEN.
002494
002495     MOVE '3'                    TO PI-ACTION.
002496*    MOVE FORMI                  TO PI-FORM-NUMBER.
002497     MOVE W-LETTER-ADDRESS-TYPE  TO PI-LETTER-ADDRESS-TYPE.
002498     MOVE -1                     TO MAINTL.
002499     GO TO 8200-SEND-DATAONLY-ERASEAUP.
002500                                 EJECT
002501 2800-PROCESS-Z-CONTROLS.
002502
002503     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
002504
002505     IF RESEND-SAVE = LOW-VALUES
002506         IF W-DAYS-TO-RESEND-1 NUMERIC
002507             IF W-DAYS-TO-RESEND-1 > ZEROS
002508                 MOVE '6'                    TO DC-OPTION-CODE
002509                 MOVE SAVE-BIN-DATE          TO DC-BIN-DATE-1
002510                 MOVE ZEROS                  TO DC-ELAPSED-MONTHS
002511                 MOVE W-DAYS-TO-RESEND-1     TO DC-ELAPSED-DAYS
002512                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
002513                 IF NO-CONVERSION-ERROR
002514                     MOVE DC-BIN-DATE-2       TO RESEND-SAVE
002515                     MOVE DC-GREG-DATE-1-EDIT TO RESENDO
002516                     MOVE AL-UANON            TO RESENDA
002517                     MOVE +8                  TO RESENDL
002518                 ELSE
002519                     MOVE ER-3770             TO EMI-ERROR
002520                     MOVE -1                  TO MAINTL
002521                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002522                     GO TO 8200-SEND-DATAONLY.
002523
002524     IF FOLLOW-UP-SAVE = LOW-VALUES
002525         IF W-DAYS-TO-FOLLOW-UP NUMERIC
002526             IF W-DAYS-TO-FOLLOW-UP > ZEROS
002527                 MOVE '6'                 TO DC-OPTION-CODE
002528                 MOVE SAVE-BIN-DATE       TO DC-BIN-DATE-1
002529                 MOVE ZEROS               TO DC-ELAPSED-MONTHS
002530                 MOVE W-DAYS-TO-FOLLOW-UP TO DC-ELAPSED-DAYS
002531                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
002532                 IF NO-CONVERSION-ERROR
002533                     MOVE DC-BIN-DATE-2       TO FOLLOW-UP-SAVE
002534                     MOVE DC-GREG-DATE-1-EDIT TO FOLLOWO
002535                     MOVE AL-UANON            TO FOLLOWA
002536                     MOVE +8                  TO FOLLOWL
002537                 ELSE
002538                     MOVE ER-3771             TO EMI-ERROR
002539                     MOVE -1                  TO MAINTL
002540                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002541                     GO TO 8200-SEND-DATAONLY.
002542
002543     IF COPIESI NOT NUMERIC
002544         IF W-NUMBER-OF-COPIES NUMERIC
002545             IF W-NUMBER-OF-COPIES > ZEROS
002546                 MOVE W-NUMBER-OF-COPIES  TO COPIESI
002547                 MOVE AL-UNNON            TO COPIESA
002548                 MOVE +1                  TO COPIESL
002549             ELSE
002550                 MOVE +1                  TO COPIESI
002551                 MOVE AL-UNNON            TO COPIESA
002552                 MOVE +1                  TO COPIESL.
002553
002554     IF W-FORM-TO-RESEND > SPACES
002555         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
002556     ELSE
002557         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
002558     END-IF.
002559
002560     IF W-PROMPT-LETTER = 'Y'
002561         MOVE W-PROMPT-LETTER           TO PI-PROMPT-LETTER
002562         MOVE ER-0894                   TO EMI-ERROR
002563         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002564     ELSE
002565         MOVE 'N'                       TO PI-PROMPT-LETTER
002566     END-IF.
002567
002568     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD
002569                                           ENCO
002570     MOVE AL-UANON                      TO ENCA
002571     MOVE +3                            TO ENCL
002572     MOVE 'N'                           TO CLEANO
002573
002574     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
002575     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
002576
002577 2800-EXIT.
002578     EXIT.
002579     EJECT
002580 3000-RECORD.
002581***************************************************************
002582*    THIS ROUTINE WILL SAVE THE LETTER TEXT (IF ANY) AND      *
002583*    BUILD A CORRESPONDENCE TRAILER WITH THE DATA FROM        *
002584*    THE SCREEN.                                              *
002585***************************************************************
002586
002587     IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
002588        MOVE ER-0211             TO EMI-ERROR
002589        MOVE -1                  TO MAINTL
002590        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002591        GO TO 8200-SEND-DATAONLY.
002592
002593     
      * EXEC CICS HANDLE CONDITION
002594*         NOTOPEN    (8860-CLAM-NOT-OPEN)
002595*         NOTFND     (3010-NOT-FOUND)
002596*    END-EXEC.
      *    MOVE '"$JI                  ! ( #00008184' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303038313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002597
002598     PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
002599     PERFORM 7950-SET-INDX     THRU 7950-EXIT.
002600
002601     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002602             VARYING SC-INDX FROM 1 BY 1
002603             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002604
002605     PERFORM 7000-READ-ADDR THRU 7099-EXIT.
002606
002607     IF NOT EMI-NO-ERRORS
002608        GO TO 8200-SEND-DATAONLY.
002609
002610     MOVE ZEROS               TO ARCH-NUMBER.
002611     PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
002612     MOVE ER-0000             TO EMI-ERROR.
002613     MOVE -1                  TO MAINTL.
002614     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002615     GO TO 8200-SEND-DATAONLY.
002616
002617 3010-NOT-FOUND.
002618     MOVE ER-0133             TO EMI-ERROR.
002619     MOVE -1                  TO MAINTL.
002620     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002621     GO TO 8200-SEND-DATAONLY.
002622
002623     EJECT
002624*4000-SET-BSR. Remove as dead code
002625*4001-READ-NEXT. Remove as dead code
002626*4002-ENDBR. Remove as dead code
002627*4000-EXIT. Remove as dead code
002628
002629     EJECT
002630 5000-MOVE-NAME.
002631*    COPY ELCMNS.
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
002632     EJECT
002633 5200-MOVE-NAME.
002634*    NOTE *******************************************************
002635*         *           M O V E   N A M E   R O U T I N E         *
002636*         *                                                     *
002637*         *      THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
002638*         *  NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
002639*         *  NO EMBEDDED BLANKS.                                *
002640*         *                                                     *
002641*         *        FIELD               VALUE                    *
002642*         *                                                     *
002643*         *      LAST NAME (CL15)      SMITH                    *
002644*         *      1ST NAME  (CL12)      JOHN                     *
002645*         *      MID NAME  (CL1)       A                        *
002646*         *                                                     *
002647*         *      AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
002648*         *                                                     *
002649*         *              JOHN A. SMITH                          *
002650*         *                                                     *
002651*         *      TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
002652*         *  STORAGE COPYBOOK:                                  *
002653*         *                                                     *
002654*         *      01  WS-NAME-WORK-AREA COPY ELCNWA.             *
002655*         *                                                     *
002656*         *******************************************************.
002657
002658     MOVE SPACES                 TO  WS-NAME-WORK-AREA.
002659     MOVE ZERO                   TO  WS-NAME-SW.
002660     SET NWA-INDEX TO +1.
002661
002662     IF W-FIRST-NAME = SPACES
002663             AND
002664        W-MIDDLE-NAME = SPACES
002665         MOVE W-LAST-NAME        TO WS-NAME-WORK
002666         GO TO 5200-EXIT.
002667
002668     MOVE W-FIRST-NAME           TO  WS-NAME-WORK2.
002669     PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
002670
002671     SET NWA-INDEX UP BY +1.
002672
002673     IF W-MIDDLE-NAME NOT = SPACES
002674        MOVE W-MIDDLE-NAME       TO  WS-NW (NWA-INDEX)
002675        SET NWA-INDEX UP BY +1
002676        MOVE '.'                 TO  WS-NW (NWA-INDEX)
002677        SET NWA-INDEX UP BY +2.
002678
002679     MOVE W-LAST-NAME            TO  WS-NAME-WORK2.
002680     PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
002681
002682 5200-EXIT.
002683     EXIT.
002684
002685     EJECT
002686 5300-MOVE-NAME.
002687     IF WS-NAME-SW > +1
002688         GO TO 5390-EXIT.
002689
002690     IF WS-NAME-WORK2 = SPACES
002691         GO TO 5390-EXIT.
002692
002693     SET NWA-INDEX2 TO +1.
002694     SET NWA-INDEX3 TO +2.
002695
002696 5310-MOVE-NAME.
002697     MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
002698
002699     IF NWA-INDEX < +30
002700        SET NWA-INDEX UP BY +1
002701     ELSE
002702        ADD +2  TO  WS-NAME-SW
002703        GO TO 5390-EXIT.
002704
002705     IF NWA-INDEX2 < +20
002706         SET NWA-INDEX2 UP BY +1
002707         SET NWA-INDEX3 UP BY +1.
002708
002709     IF WS-NW2 (NWA-INDEX2) = SPACES AND
002710        WS-NW2 (NWA-INDEX3) = SPACES
002711        GO TO 5390-EXIT.
002712
002713     GO TO 5310-MOVE-NAME.
002714
002715 5390-EXIT.
002716     EXIT.
002717
002718     EJECT
002719 6100-ADDR-MAINT.
002720***************************************************************
002721*    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
002722*    AND THE TS-TABLE OF LETTER TEXT.                         *
002723*    IT WILL THEN XCTL TO THE ADDRESS MAINT PROGRAM.          *
002724***************************************************************
002725
002726
002727     PERFORM 7500-READ-TS        THRU 7599-EXIT
002728
002729     PERFORM 7950-SET-INDX THRU 7950-EXIT.
002730     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002731             VARYING SC-INDX FROM 1 BY 1
002732             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002733
002734     IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
002735        MOVE ER-0343             TO EMI-ERROR
002736        MOVE -1                  TO MAINTL
002737        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002738        GO TO 8200-SEND-DATAONLY.
002739
002740
002741*    PERFORM 7500-READ-TS        THRU 7599-EXIT
002742
002743*    PERFORM 7950-SET-INDX THRU 7950-EXIT.
002744*    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002745*            VARYING SC-INDX FROM 1 BY 1
002746*            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002747
002748     PERFORM 7700-PUT-TEMP-STOR    THRU 7749-EXIT.
002749     PERFORM 7790-WRITE-SCREEN-TS  THRU 7790-EXIT.
002750     PERFORM 7795-WRITE-PI-AREA-TS THRU 7795-EXIT.
002751     MOVE PGM-EL141 TO PGM-NAME.
002752
002753     GO TO 9300-XCTL.
002754     EJECT
002755 6200-EDIT-MODE.
002756***************************************************************
002757*    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
002758*    AND THE TS-TABLE OF LETTER TEXT.                         *
002759*    IT WILL THEN XCTL TO THE TEXT-EDITOR PROGRAM.            *
002760***************************************************************
002761     IF PI-SHOW-MODE
002762        MOVE ER-0188             TO EMI-ERROR
002763        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002764        MOVE -1                  TO MAINTL
002765        MOVE AL-UABON            TO MAINTA
002766        GO TO 8200-SEND-DATAONLY
002767     END-IF
002768
002769*    IF PI-COMPANY-ID = 'DMD'
002770*        IF PI-BSR-AUTOMATED
002771*            MOVE ER-0912        TO EMI-ERROR
002772*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002773*            MOVE -1             TO MAINTL
002774*            MOVE AL-UABON       TO MAINTA
002775*            GO TO 8200-SEND-DATAONLY.
002776
002777     IF PI-CURRENT-LINE = ZEROS
002778        MOVE ER-0187             TO EMI-ERROR
002779        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002780        MOVE -1                  TO MAINTL
002781        GO TO 8200-SEND-DATAONLY.
002782
002783
002784     PERFORM 7500-READ-TS        THRU 7599-EXIT
002785     PERFORM 7750-DELETE-TEMP-STOR
002786                                 THRU 7750-EXIT
002787
002788     PERFORM 7950-SET-INDX THRU 7950-EXIT.
002789     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002790             VARYING SC-INDX FROM 1 BY 1
002791             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002792
002793     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT.
002794     PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.
002795
002796     MOVE PI-FORM-NUMBER         TO PI-COMM-CONTROL.
002797     MOVE ZEROS                  TO PI-UPDATE-SW.
002798     MOVE '1'                    TO PI-ENTRY-CD-1.
002799     MOVE PGM-EL1042             TO PGM-NAME.
002800     GO TO 9300-XCTL.
002801
002802     EJECT
002803 6400-LETTER-RELEASE.
002804***************************************************************
002805*    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
002806*    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *
002807*    THE ARCHIVE FILE.                                        *
002808*    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
002809*        1. CHECK SECURITY AND IF IT IS A DESIGNATED LETTER.  *
002810*        2. RE-EDIT DATA AND UPDATE TS-TABLE WITH CHANGES     *
002811*        3. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *
002812*        4. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
002813*        5. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *
002814*        7. BUILD A CORRESPONDENCE TRAILER                    *
002815*        8. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *
002816*                 ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.*
002817*        4. RESET ALL CONTROL FIELDS AND RETURN THE           *
002818*                 ARCHIVE NUMBER USED TO FILE THE RECORDS.    *
002819***************************************************************
002820
002821
002822     PERFORM 7500-READ-TS        THRU 7599-EXIT
002823
002824     PERFORM 7950-SET-INDX THRU 7950-EXIT.
002825     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002826             VARYING SC-INDX FROM 1 BY 1
002827             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002828
002829
002830     IF NOT MODIFY-CAP
002831        MOVE ER-0070             TO EMI-ERROR
002832        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002833        MOVE -1                  TO MAINTL
002834        GO TO 8200-SEND-DATAONLY.
002835
002836     IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
002837        MOVE ER-0211             TO EMI-ERROR
002838        MOVE -1                  TO MAINTL
002839        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002840        GO TO 8200-SEND-DATAONLY.
002841
002842
002843*    PERFORM 7500-READ-TS        THRU 7599-EXIT
002844
002845*    PERFORM 7950-SET-INDX THRU 7950-EXIT.
002846*    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
002847*            VARYING SC-INDX FROM 1 BY 1
002848*            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
002849
002850     IF PI-ADDR-TYPE  NOT =  ADDRI
002851        PERFORM 7000-READ-ADDR THRU 7099-EXIT.
002852
002853     IF NOT EMI-NO-ERRORS
002854        GO TO 8200-SEND-DATAONLY.
002855
002856     MOVE +0                     TO TALLY.
002857     INSPECT RECORD-TABLE TALLYING TALLY
002858                                 FOR CHARACTERS BEFORE '@'.
002859
002860     IF TALLY < +21900
002861        COMPUTE PI-CURRENT-LINE = TALLY / 73
002862        MOVE ZEROS               TO ROLL-COUNTER
002863        MOVE ER-0191             TO EMI-ERROR
002864        MOVE -1                  TO MAINTL
002865        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002866        GO TO 7900-ROLL-PAGE.
002867
002868     MOVE '1'                    TO CNTL-RECORD-TYPE.
002869     MOVE ZEROS                  TO CNTL-SEQ.
002870     MOVE SPACES                 TO CNTL-GENL.
002871     
      * EXEC CICS READ
002872*         DATASET    (CNTL-ID)
002873*         SET        (ADDRESS OF CONTROL-FILE)
002874*         RIDFLD     (CNTL-KEY)
002875*         UPDATE
002876*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008554' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038353534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002877
002878     ADD 1                       TO CF-CO-ARCHIVE-COUNTER.
002879     MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.
002880     MOVE CF-PRINT-ADDRESS-LABELS   TO  WS-LABELS-SW.
002881
002882     
      * EXEC CICS REWRITE
002883*         FROM      (CONTROL-FILE)
002884*         DATASET   (CNTL-ID)
002885*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008565' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038353635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002886
002887     PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
002888
002889     
      * EXEC CICS HANDLE CONDITION
002890*         NOTOPEN   (9990-ABEND)
002891*    END-EXEC.
      *    MOVE '"$J                   ! ) #00008572' TO DFHEIV0
           MOVE X'22244A202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303038353732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002892
002893     
      * EXEC CICS GETMAIN
002894*         SET      (ADDRESS OF LETTER-ARCHIVE)
002895*         LENGTH   (ARCH-LENGTH)
002896*    END-EXEC.
      *    MOVE '," L                  $   #00008576' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038353736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002897
002898     MOVE SPACES                 TO LETTER-ARCHIVE.
002899     MOVE 'LA'                   TO LA-RECORD-ID.
002900     MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
002901                                    LA-ARCHIVE-NO-A1.
002902     MOVE '1'                    TO LA-RECORD-TYPE
002903                                    LA-RECORD-TYPE-A1.
002904     MOVE ZEROS                  TO LA-LINE-SEQ-NO
002905                                    LA-LINE-SEQ-NO-A1.
002906     MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
002907                                    LA-COMPANY-CD-A1.
002908     MOVE CLAM-CARRIER           TO LA-CARRIER.
002909     MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.
002910     MOVE PI-CERT-NO             TO LA-CERT-NO.
002911
002912*    IF PI-COMPANY-ID = 'DMD'
002913*        MOVE WS-DMD-CERT-STATE    TO LA-DMD-UND-CODE
002914*        MOVE WS-DMD-CERT-GROUPING TO LA-DMD-PROD-CODE
002915*        MOVE WS-DMD-BEN-CODE      TO LA-DMD-BEN-CODE
002916*        MOVE WS-DMD-CORR-TRLR-SEQ TO LA-DMD-CORR-TRLR-SEQ
002917*        MOVE WS-DMD-LETTER-FORM   TO LA-DMD-LETTER-FORM
002918*        MOVE WS-DMD-RES-ST        TO LA-DMD-RES-ST
002919*        MOVE LOW-VALUES           TO LA-DMD-LETTER-PURGE-DT
002920*                                     LA-DMD-LETTER-RELOAD-DT
002921*        MOVE '1'                  TO LA-DMD-LETTER-STATUS
002922*        EXEC CICS GETMAIN
002923*            SET    (ADDRESS OF LETTER-ARCHIVE-TEMP)
002924*            LENGTH (ARCT-LENGTH)
002925*        END-EXEC.
002926
002927     IF COPIESL NOT = ZEROS
002928        MOVE COPIESI             TO LA-NO-OF-COPIES
002929     ELSE
002930        MOVE  1                  TO LA-NO-OF-COPIES.
002931
002932     IF RESENDL NOT = ZEROS
002933        MOVE RESEND-SAVE         TO LA-RESEND-DATE
002934     ELSE
002935        MOVE LOW-VALUES          TO LA-RESEND-DATE.
002936
002937     MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
002938     MOVE CURRENT-SAVE           TO LA-CREATION-DT.
002939
002940     IF PRINT-PERFORMED
002941        MOVE CURRENT-SAVE        TO LA-INITIAL-PRINT-DATE
002942     ELSE
002943        MOVE LOW-VALUES          TO LA-INITIAL-PRINT-DATE.
002944
002945     MOVE LOW-VALUES             TO LA-RESEND-PRINT-DATE.
002946     MOVE CORR-TRLR-SEQ          TO LA-CORR-TRLR-SEQ.
002947     PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
002948
002949     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
002950         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
002951     END-IF.
002952
002953     IF WS-LABELS-SW = 'N'
002954         NEXT SENTENCE
002955     ELSE
002956         SET TB-INDX TO 1
002957         MOVE ZEROS                  TO SEQ-COUNTER
002958         PERFORM 6480-FORMAT-ADDRESS
002959               VARYING TB-INDX FROM 1 BY 1 UNTIL
002960               TB-INDX > 6.
002961
002962     MOVE ZEROS                  TO SEQ-COUNTER.
002963     MOVE 'N'                    TO WS-SKIP-EMAIL
002964     PERFORM 6470-FORMAT-TEXT THRU 6479-EXIT
002965             VARYING TB-INDX FROM 8 BY 1
002966             UNTIL TB-INDX > PI-TOTAL-LINES.
002967     MOVE 'Y'                    TO WS-SKIP-EMAIL.
002968     IF SEQ-COUNTER < +15
002969      AND WS-SKIP-EMAIL EQUAL 'N'
002970        MOVE ARCH-NUMBER         TO TRAN-DL2-ARCHNO
002971        MOVE PI-PROCESSOR-ID     TO TRAN-DL2-USER
002972        MOVE SPACES              TO TRAN-DL2-CCC
002973        STRING PI-CARRIER ' ' PI-CLAIM-NO ' ' PI-CERT-NO
002974           DELIMITED BY SIZE
002975           INTO TRAN-DL2-CCC
002976        END-STRING
002977        
      * EXEC CICS WRITEQ TD
002978*          QUEUE ('BTCH')
002979*          FROM (TRAN-DATA-LINE1)
002980*          LENGTH (80)
002981*       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008660' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303038363630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002982        
      * EXEC CICS WRITEQ TD
002983*          QUEUE ('BTCH')
002984*          FROM (TRAN-DATA-LINE2)
002985*          LENGTH (80)
002986*       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008665' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303038363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE2, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002987        
      * EXEC CICS WRITEQ TD
002988*          QUEUE ('BTCH')
002989*          FROM (TRAN-DATA-LINE3)
002990*          LENGTH (80)
002991*       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008670' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303038363730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE3, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002992     END-IF
002993
002994     MOVE ER-0280                TO EMI-ERROR.
002995
002996     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002997     MOVE ARCH-NUMBER            TO ARCH-SUPPRESS
002998     MOVE ARCH-EDIT              TO EMI-TEXT-VARIABLE (1).
002999
003000     GO TO 9400-CLEAR.
003001
003002     EJECT
003003 6470-FORMAT-TEXT.
003004
003005     IF REC-TEXT (TB-INDX) (1:16) = NAPERSOFT-LETTER
003006         MOVE 'Y'                TO WS-SKIP-EMAIL
003007     END-IF.
003008
003009     MOVE SPACES                 TO LETTER-ARCHIVE.
003010     MOVE '3'                    TO LA-RECORD-TYPE
003011                                    LA-RECORD-TYPE-A1.
003012     MOVE 'LA'                   TO LA-RECORD-ID.
003013     MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
003014                                    LA-ARCHIVE-NO-A1.
003015     MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO
003016                                    LA-LINE-SEQ-NO-A1.
003017     MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
003018                                    LA-COMPANY-CD-A1.
003019     MOVE REC-TEXT (TB-INDX)     TO LA-TEXT-LINE.
003020     SET TB-INDX1 TO TB-INDX.
003021     SET TB-INDX1 UP BY 1.
003022     MOVE ZEROS                  TO INDX-WORK.
003023
003024 6472-LOOP.
003025     IF TB-INDX1 < PI-TOTAL-LINES AND
003026        REC-TEXT (TB-INDX1) = SPACES
003027           SET TB-INDX1 UP BY 1
003028           ADD 1                 TO INDX-WORK
003029           GO TO 6472-LOOP.
003030
003031     IF REC-TEXT (TB-INDX1) = TOP-FORM
003032        MOVE '99'                TO LA-SKIP-CONTROL
003033        SET TB-INDX1 UP BY 1
003034     ELSE
003035        MOVE INDX-WORK           TO LA-SKIP-CONTROL.
003036
003037     SET TB-INDX TO TB-INDX1.
003038     SET TB-INDX DOWN BY 1.
003039     PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
003040     ADD 1 TO SEQ-COUNTER.
003041
003042 6479-EXIT.
003043      EXIT.
003044
003045     EJECT
003046 6480-FORMAT-ADDRESS.
003047     MOVE SPACES                 TO LETTER-ARCHIVE.
003048     MOVE '2'                    TO LA-RECORD-TYPE
003049                                    LA-RECORD-TYPE-A1.
003050     MOVE 'LA'                   TO LA-RECORD-ID.
003051     MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
003052                                    LA-ARCHIVE-NO-A1.
003053     MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO
003054                                    LA-LINE-SEQ-NO-A1.
003055     MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
003056                                    LA-COMPANY-CD-A1.
003057     MOVE REC-TEXT (TB-INDX)     TO LA-ADDRESS-LINE.
003058
003059     PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
003060     ADD 1 TO SEQ-COUNTER.
003061
003062     EJECT
003063 6490-WRITE-ARCHIVE.
003064     
      * EXEC CICS HANDLE CONDITION
003065*        DUPKEY    (6499-EXIT)
003066*    END-EXEC.
      *    MOVE '"$$                   ! * #00008747' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303038373437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003067
003068*    IF PI-COMPANY-ID = 'DMD'
003069*        MOVE LETTER-ARCHIVE     TO LETTER-ARCHIVE-TEMP
003070*        MOVE 'LT'               TO LT-RECORD-ID
003071*        EXEC CICS WRITE
003072*            DATASET   (ARCT-ID)
003073*            FROM      (LETTER-ARCHIVE-TEMP)
003074*            RIDFLD    (LT-CONTROL-PRIMARY)
003075*        END-EXEC.
003076
003077     
      * EXEC CICS WRITE
003078*         DATASET   (ARCH-ID)
003079*         FROM      (LETTER-ARCHIVE)
003080*         RIDFLD    (LA-CONTROL-PRIMARY)
003081*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008760' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038373630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003082
003083 6499-EXIT.
003084     EXIT.
003085
003086     EJECT
003087 6500-BUILD-CORRESPOND.
003088***************************************************************
003089*    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
003090*    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
003091*    USING THE NEW SEQUENCE NUMBER.                           *
003092*    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
003093*    TRAILER RECORD.                                          *
003094***************************************************************
003095
003096     MOVE PI-CLAIM-NO            TO CLAM-CLAIM.
003097
003098     
      * EXEC CICS READ
003099*         DATASET    (CLAM-ID)
003100*         SET        (ADDRESS OF CLAIM-MASTER)
003101*         RIDFLD     (CLAM-KEY)
003102*         UPDATE
003103*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008781' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038373831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003104
003105     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
003106
003107     IF FOLLOW-UP-SAVE > CL-NEXT-FOLLOWUP-DT
003108        MOVE FOLLOW-UP-SAVE      TO CL-NEXT-FOLLOWUP-DT.
003109
003110     IF RESEND-SAVE > CL-NEXT-FOLLOWUP-DT
003111        MOVE RESEND-SAVE         TO CL-NEXT-FOLLOWUP-DT.
003112
003113     IF MAINTI = 'C'
003114        MOVE '2'                 TO CL-LAST-MAINT-TYPE.
003115
003116     IF MAINTI = 'R'
003117        PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT
003118        MOVE ACTV-SEQ            TO PI-ADDR-SEQ.
003119
003120*    IF PI-COMPANY-ID = 'DMD'
003121*        MOVE 04                 TO CL-ACTIVITY-CODE.
003122
003123     
      * EXEC CICS GETMAIN
003124*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
003125*         INITIMG   (GETMAIN-SPACE)
003126*         LENGTH    (ACTV-LENGTH)
003127*    END-EXEC.
      *    MOVE ',"IL                  $   #00008806' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038383036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTV-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003128
003129     MOVE 'AT'                   TO AT-RECORD-ID.
003130     MOVE  4                     TO AT-TRAILER-TYPE.
003131     MOVE CURRENT-SAVE           TO AT-RECORDED-DT
003132                                    CL-LAST-MAINT-DT
003133                                    AT-CORR-LAST-MAINT-DT
003134     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
003135                                    CL-LAST-MAINT-USER
003136                                    AT-CORR-LAST-UPDATED-BY
003137     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
003138                                    CL-LAST-MAINT-HHMMSS.
003139     MOVE ACTV-KEY               TO AT-CONTROL-PRIMARY.
003140     MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
003141     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
003142                                    CORR-TRLR-SEQ.
003143     MOVE CURRENT-SAVE           TO AT-LETTER-SENT-DT.
003144     MOVE FOLLOW-UP-SAVE         TO AT-RECEIPT-FOLLOW-UP.
003145     MOVE RESEND-SAVE            TO AT-AUTO-RE-SEND-DT.
003146     MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
003147                                    AT-LETTER-PURGED-DT.
003148     MOVE ARCH-NUMBER            TO AT-LETTER-ARCHIVE-NO.
003149     MOVE '1'                    TO AT-LETTER-ORIGIN.
003150     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
003151     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
003152     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
003153
003154*    IF PI-COMPANY-ID = 'DMD'
003155*        MOVE '1'                TO AT-DMD-LETTER-STATUS
003156*        IF PI-BSR-AUTOMATED
003157*            MOVE 'A'            TO AT-DMD-BSR-CODE.
003158*
003159     IF FORML  NOT = ZEROS
003160        MOVE FORMI               TO AT-STD-LETTER-FORM
003161     ELSE
003162        MOVE SPACES              TO AT-STD-LETTER-FORM.
003163
003164     IF REL NOT = ZEROS
003165        MOVE REI                 TO AT-REASON-TEXT
003166     ELSE
003167        MOVE SPACES              TO AT-REASON-TEXT.
003168
003169*    IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
003170*        MOVE AT-REASON-TEXT     TO WS-REASON-TEXT
003171*        IF WS-RE-NDX NUMERIC  AND
003172*           WS-RE-NDX > ZERO   AND
003173*           WS-RE-NDX < 33
003174*            MOVE HAN-REASON-TEXT (WS-RE-NDX)
003175*                                TO AT-REASON-TEXT.
003176
003177     MOVE PI-ADDR-SEQ            TO AT-ADDRESS-REC-SEQ-NO.
003178
003179     IF PI-ADDR-TYPE > SPACES
003180         MOVE PI-ADDR-TYPE          TO AT-ADDRESEE-TYPE
003181         MOVE REC-TEXT (1)          TO AT-ADDRESSEE-NAME
003182     ELSE
003183     IF W-ACCOUNT > ZEROS
003184         MOVE 'A'                   TO AT-ADDRESEE-TYPE
003185         MOVE SS06D                 TO AT-ADDRESSEE-NAME
003186     ELSE
003187     IF W-BENEFICIARY > ZEROS
003188          MOVE 'B'                  TO AT-ADDRESEE-TYPE
003189          IF W-BENEFICIARY = 9
003190              MOVE SS61D            TO AT-ADDRESSEE-NAME
003191            ELSE
003192              MOVE SS43D            TO AT-ADDRESSEE-NAME
003193     ELSE
003194     IF W-EMPLOYER > ZEROS
003195          MOVE 'E'                  TO AT-ADDRESEE-TYPE
003196          MOVE SS48D                TO AT-ADDRESSEE-NAME
003197     ELSE
003198     IF W-INSURED > ZEROS
003199          MOVE 'I'                  TO AT-ADDRESEE-TYPE
003200          MOVE SS57D                TO AT-ADDRESSEE-NAME
003201     ELSE
003202     IF W-PHYSICIAN > ZEROS
003203          MOVE 'P'                  TO AT-ADDRESEE-TYPE
003204          MOVE SS47D                TO AT-ADDRESSEE-NAME
003205     ELSE
003206     IF W-OTHER-1 > ZEROS
003207          MOVE 'O'                  TO AT-ADDRESEE-TYPE
003208          MOVE SS49D                TO AT-ADDRESSEE-NAME
003209     ELSE
003210     IF W-OTHER-2 > ZEROS
003211          MOVE 'Q'                  TO AT-ADDRESEE-TYPE
003212          MOVE SS50D                TO AT-ADDRESSEE-NAME
003213     ELSE
003214          MOVE SPACES               TO AT-ADDRESEE-TYPE
003215                                       AT-ADDRESSEE-NAME.
003216     IF PRINT-PERFORMED
003217        MOVE CURRENT-SAVE        TO AT-INITIAL-PRINT-DATE
003218     ELSE
003219        MOVE LOW-VALUES          TO AT-INITIAL-PRINT-DATE.
003220
003221     MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE.
003222
003223*    IF PI-COMPANY-ID = 'DMD'
003224*        MOVE CL-CERT-STATE          TO WS-DMD-CERT-STATE
003225*        MOVE CL-CERT-GROUPING (5:2) TO WS-DMD-CERT-GROUPING
003226*        MOVE BEN-HOLD               TO WS-DMD-BEN-CODE
003227*        MOVE AT-SEQUENCE-NO         TO WS-DMD-CORR-TRLR-SEQ
003228*        MOVE AT-STD-LETTER-FORM     TO WS-DMD-LETTER-FORM.
003229
003230     
      * EXEC CICS WRITE
003231*         DATASET    (ACTV-ID)
003232*         FROM       (ACTIVITY-TRAILERS)
003233*         RIDFLD     (AT-CONTROL-PRIMARY)
003234*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008913' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038393133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003235
003236     
      * EXEC CICS HANDLE CONDITION
003237*        DUPKEY      (6599-EXIT)
003238*    END-EXEC.
      *    MOVE '"$$                   ! + #00008919' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303038393139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003239
003240     
      * EXEC CICS REWRITE
003241*         DATASET    (CLAM-ID)
003242*         FROM       (CLAIM-MASTER)
003243*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008923' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038393233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003244
003245 6599-EXIT.
003246      EXIT.
003247     EJECT
003248 6600-SET-ADDR-SEQ.
003249     IF ADDRL = ZEROS
003250        GO TO 6699-EXIT.
003251
003252     MOVE ADDRI                  TO PI-ADDR-TYPE
003253                                    WS-ADDR-TYPE-CD.
003254     IF WS-ADDR-SEQ NOT NUMERIC
003255        GO TO 6699-EXIT.
003256
003257     MOVE ZEROS                  TO PI-ADDR-SEQ.
003258
003259     IF WS-ADDR-TYPE = 'I'
003260        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003261     ELSE
003262     IF WS-ADDR-TYPE = 'A'
003263        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003264        ADD +20                  TO ACTV-SEQ
003265     ELSE
003266     IF WS-ADDR-TYPE = 'B'
003267        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003268        ADD +10                  TO ACTV-SEQ
003269     ELSE
003270     IF WS-ADDR-TYPE = 'P'
003271        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003272        ADD +30                  TO ACTV-SEQ
003273     ELSE
003274     IF WS-ADDR-TYPE = 'E'
003275        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003276        ADD +40                  TO ACTV-SEQ
003277     ELSE
003278     IF WS-ADDR-TYPE = 'O'
003279        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003280        ADD +50                  TO ACTV-SEQ
003281     ELSE
003282     IF WS-ADDR-TYPE = 'Q'
003283        MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
003284        ADD +60                  TO ACTV-SEQ.
003285
003286 6699-EXIT.
003287      EXIT.
003288
003289 6700-BUILD-NAPERSOFT.
003290
003291     
      * EXEC CICS GETMAIN
003292*         SET      (ADDRESS OF NAPERSOFT-FILE)
003293*         LENGTH   (NAPS-LENGTH)
003294*    END-EXEC.
      *    MOVE '," L                  $   #00008974' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038393734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAPS-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003295
003296     MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
003297     MOVE 'NA'                  TO  NA-RECORD-ID.
003298     MOVE PI-COMPANY-CD         TO  NA-COMPANY-CD.
003299     MOVE PI-CARRIER            TO  NA-CARRIER.
003300     MOVE PI-CLAIM-NO           TO  NA-CLAIM-NO.
003301     MOVE PI-CERT-NO            TO  NA-CERT-NO.
003302     MOVE ARCH-NUMBER           TO  NA-ARCHIVE-NO.
003303     MOVE FORMI                 TO  NA-LETTER-ID.
003304     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
003305     MOVE CURRENT-SAVE          TO  NA-CREATION-DT.
003306     IF PRINT-PERFORMED
003307         MOVE CURRENT-SAVE      TO  NA-INITIAL-PRINT-DT
003308     ELSE
003309         MOVE LOW-VALUES        TO  NA-INITIAL-PRINT-DT
003310     END-IF.
003311     MOVE FOLLOW-UP-SAVE        TO  NA-FOLLOW-UP-DT.
003312     MOVE RESEND-SAVE           TO  NA-RESEND-DT
003313     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
003314     IF COPIESL NOT = ZEROS
003315         MOVE COPIESI           TO  NA-NO-OF-COPIES
003316     ELSE
003317         MOVE 1                 TO  NA-NO-OF-COPIES
003318     END-IF.
003319     IF PI-LETTER-ADDRESS-TYPE = ' 0'
003320         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
003321     ELSE
003322         MOVE PI-LETTER-ADDRESS-TYPE TO NA-ADDRESS-TYPE
003323     END-IF.
003324     MOVE CORR-TRLR-SEQ         TO  NA-CORR-TRLR-SEQ.
003325     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
003326
003327     
      * EXEC CICS WRITE
003328*         DATASET    (NAPS-ID)
003329*         FROM       (NAPERSOFT-FILE)
003330*         RIDFLD     (NA-CONTROL-PRIMARY)
003331*    END-EXEC.
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009010' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039303130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAPS-ID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 NA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003332
003333 6799-EXIT.
003334      EXIT.
003335
003336     EJECT
003337 7000-READ-ADDR.
003338     IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
003339        IF ADDRL = ZEROS
003340           GO TO 7099-EXIT
003341        ELSE
003342           MOVE ER-0374             TO EMI-ERROR
003343           MOVE -1                  TO ADDRL
003344           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003345           GO TO 7099-EXIT.
003346
003347     MOVE PI-CLAIM-NO            TO CLAM-CLAIM
003348                                    ACTV-CLAIM.
003349
003350     
      * EXEC CICS HANDLE CONDITION
003351*         NOTOPEN    (8860-CLAM-NOT-OPEN)
003352*         NOTFND     (7090-CLAIM-NOT-FOUND)
003353*     END-EXEC.
      *    MOVE '"$JI                  ! , #00009033' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303039303333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003354
003355     
      * EXEC CICS READ
003356*         DATASET    (CLAM-ID)
003357*         SET        (ADDRESS OF CLAIM-MASTER)
003358*         RIDFLD     (CLAM-KEY)
003359*    END-EXEC.
      *    MOVE '&"S        E          (   #00009038' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039303338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003360
003361*    IF PI-COMPANY-ID = 'DMD'
003362*            AND
003363*       NOT SYSTEM-MODIFY-CAP
003364*            AND
003365*        HIGHEST-PRIORITY
003366*        IF MAINTI = 'R'
003367*            MOVE ER-0909        TO EMI-ERROR
003368*            MOVE -1             TO ADDRL
003369*            MOVE AL-UABON       TO ADDRA
003370*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003371*            GO TO 8200-SEND-DATAONLY
003372*        ELSE
003373*            MOVE ER-0900        TO EMI-ERROR
003374*            MOVE -1             TO ADDRL
003375*            MOVE AL-UABON       TO ADDRA
003376*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003377*            GO TO 8200-SEND-DATAONLY.
003378
003379     IF ADDRL = ZEROS
003380        GO TO 7099-EXIT.
003381
003382*    IF PI-COMPANY-ID = 'DMD'
003383*        IF MAINTI = 'C'
003384*           IF CL-BENEFICIARY = '0000B0CA10' OR '0000B0CAN0' OR
003385*                               '0000B0CAS0' OR '0000B0CA50' OR
003386*                               '0000B0CAA0' OR '0000B0CAB0' OR
003387*                               '0000B0CAC0' OR '0000B0CAD0' OR
003388*                               '0000B0CO20' OR '0000B0MN10' OR
003389*                               '0000B0MN20' OR '0000B0MN30' OR
003390*                               '0000B0MN40' OR '0000B0MN50' OR
003391*                               '0000B0MN60' OR '0000B0MN70'
003392*           IF ADDRI(1:1) = 'B'
003393*              MOVE ER-8158     TO EMI-ERROR
003394*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003395*              MOVE -1          TO MAINTL
003396*              GO TO 8200-SEND-DATAONLY.
003397*
003398*    IF PI-COMPANY-ID = 'DMD'
003399*      PERFORM 4000-SET-BSR THRU 4000-EXIT
003400*      IF PI-BSR-AUTOMATED
003401*        IF MAINTI = 'C'
003402*           MOVE ER-7843          TO EMI-ERROR
003403*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003404*           MOVE -1               TO MAINTL
003405*           GO TO 8200-SEND-DATAONLY.
003406
003407     PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.
003408
003409     MOVE ADDRI TO WS-ADDR-TYPE-CD.
003410
003411*    IF CL-SYSTEM-IDENTIFIER = 'CV'
003412*        GO TO 7040-READ-PRODUCER.
003413
003414 7010-READ-ACCT.
003415
003416     MOVE CL-CERT-GROUPING       TO ACCT-GROUPING.
003417     MOVE CL-CERT-STATE          TO ACCT-STATE.
003418     MOVE CL-CERT-ACCOUNT        TO ACCT-ACCOUNT.
003419     MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DATE.
003420
003421     
      * EXEC CICS HANDLE CONDITION
003422*         NOTOPEN    (8880-ACCT-NOT-OPEN)
003423*         NOTFND     (7080-ACCT-NOT-FOUND)
003424*    END-EXEC.
      *    MOVE '"$JI                  ! - #00009104' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303039313034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003425
003426     PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.
003427
003428     MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.
003429
003430 7020-READNEXT.
003431
003432     PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.
003433
003434     IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
003435        IF WS-SAVE-ACCT-RECORD = SPACES
003436            GO TO 7080-ACCT-NOT-FOUND
003437        ELSE
003438            MOVE 'Y'                 TO  WS-ACCT-READ-SW
003439            MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY
003440            MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER
003441            GO TO 7030-CONTINUE-BUILD-ADDR.
003442
003443     IF AM-EXPIRATION-DT = HIGH-VALUES
003444         NEXT SENTENCE
003445     ELSE
003446         MOVE ACCOUNT-MASTER         TO  WS-SAVE-ACCT-RECORD
003447         GO TO 7020-READNEXT.
003448
003449     MOVE AM-CONTROL-PRIMARY TO ACCT-KEY.
003450     MOVE 'Y'                TO WS-ACCT-READ-SW.
003451
003452 7030-CONTINUE-BUILD-ADDR.
003453
003454     MOVE SPACES             TO  WS-SAVE-ACCT-RECORD.
003455
003456     IF WS-ADDR-TYPE-CD = 'A0'
003457        MOVE ZEROS               TO PI-ADDR-SEQ
003458     ELSE
003459        GO TO 7067-CHECK-BENE-ADDR.
003460
003461     MOVE SPACES             TO               WS-LABEL-HOLD-AREA.
003462     MOVE AM-NAME            TO  REC-TEXT (1) WS-LABEL-LINES (1).
003463     MOVE AM-PERSON          TO  REC-TEXT (2) WS-LABEL-LINES (2).
003464     MOVE AM-ADDRS           TO  REC-TEXT (3) WS-LABEL-LINES (3).
003465*    MOVE AM-CITY            TO  REC-TEXT (4) WS-LABEL-LINES (4).
003466     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
003467        INTO WS-LABEL-LINES (4)
003468     END-STRING
003469     MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
003470     MOVE SPACES             TO  REC-TEXT (5) WS-LABEL-LINES (5).
003471
003472     MOVE SPACES             TO  WS-ZIP-CODE.
003473
003474     IF AM-CANADIAN-POST-CODE
003475         MOVE AM-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
003476         MOVE AM-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
003477     ELSE
003478         MOVE AM-ZIP-PRIME       TO  WS-AM-ZIP-CODE
003479         IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
003480             MOVE '-'            TO  WS-AM-ZIP-DASH
003481             MOVE AM-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
003482
003483     MOVE WS-ZIP-CODE        TO  REC-TEXT (6) WS-LABEL-LINES (6).
003484
003485     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
003486
003487     MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.
003488     MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.
003489     MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.
003490     MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.
003491     MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.
003492     MOVE WS-LABEL-LINES (6) TO  REC-TEXT (6)    SS05-6D.
003493
003494     GO TO 7099-EXIT.
003495
003496     EJECT
003497*7040-READ-PRODUCER. Remove as dead code
003498*7050-READNEXT. Remove as dead code
003499*7060-CONTINUE-BUILD-ADDR. Remove as dead code
003500
003501 7067-CHECK-BENE-ADDR.
003502
003503*    IF PI-COMPANY-ID = 'DMD'
003504*        IF WS-ADDR-TYPE-CD = 'B9'
003505*            GO TO 7067-CHECK-BENE-ADDR-9.
003506
003507     IF WS-ADDR-TYPE-CD NOT = 'B0'
003508         GO TO 7068-CONTINUE-BUILD-ADDR.
003509
003510     IF CL-BENEFICIARY = SPACES
003511         GO TO 7070-ACTV-NOT-FOUND.
003512
003513     
      * EXEC CICS HANDLE CONDITION
003514*         NOTFND     (7070-ACTV-NOT-FOUND)
003515*    END-EXEC.
      *    MOVE '"$I                   ! . #00009196' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303039313936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003516
003517     MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
003518     MOVE 'B'                    TO BENE-REC-TYPE.
003519     MOVE CL-BENEFICIARY         TO BENE-NUMBER.
003520
003521     
      * EXEC CICS READ
003522*         DATASET    (BENE-ID)
003523*         SET        (ADDRESS OF BENEFICIARY-MASTER)
003524*         RIDFLD     (BENE-KEY)
003525*    END-EXEC.
      *    MOVE '&"S        E          (   #00009204' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039323034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003526
003527     MOVE SPACES              TO  WS-LABEL-HOLD-AREA.
003528     MOVE BE-MAIL-TO-NAME     TO  REC-TEXT (1) WS-LABEL-LINES (1).
003529     MOVE BE-ADDRESS-LINE-1   TO  REC-TEXT (2) WS-LABEL-LINES (2).
003530     MOVE BE-ADDRESS-LINE-2   TO  REC-TEXT (3) WS-LABEL-LINES (3).
003531     MOVE BE-ADDRESS-LINE-3   TO  REC-TEXT (4) WS-LABEL-LINES (4).
003532*    MOVE BE-CITY-STATE       TO  REC-TEXT (5) WS-LABEL-LINES (5).
003533     STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
003534        INTO WS-LABEL-LINES (5)
003535     END-STRING
003536     MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
003537
003538     MOVE SPACES              TO  WS-ZIP-CODE.
003539     IF BE-CANADIAN-POST-CODE
003540         MOVE BE-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
003541         MOVE BE-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
003542     ELSE
003543         MOVE BE-ZIP-PRIME       TO  WS-AM-ZIP-CODE
003544         IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
003545             MOVE '-'            TO  WS-AM-ZIP-DASH
003546             MOVE BE-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
003547
003548     MOVE WS-ZIP-CODE         TO  REC-TEXT (6) WS-LABEL-LINES (6).
003549
003550     GO TO 7069-SET-ADDR.
003551
003552 7067-CHECK-BENE-ADDR-9.
003553
003554     IF CL-BENEFICIARY = SPACES
003555         GO TO 7070-ACTV-NOT-FOUND.
003556
003557     
      * EXEC CICS HANDLE CONDITION
003558*         NOTFND     (7070-ACTV-NOT-FOUND)
003559*    END-EXEC.
      *    MOVE '"$I                   ! / #00009240' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303039323430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003560
003561     MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
003562     MOVE 'B'                    TO BENE-REC-TYPE.
003563     MOVE CL-BENEFICIARY         TO BENE-NUMBER.
003564
003565     
      * EXEC CICS READ
003566*         DATASET    (BENE-ID)
003567*         SET        (ADDRESS OF BENEFICIARY-MASTER)
003568*         RIDFLD     (BENE-KEY)
003569*    END-EXEC.
      *    MOVE '&"S        E          (   #00009248' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003570
003571     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
003572     MOVE BE-MAIL-TO-NAME2       TO REC-TEXT (1)
003573                                    WS-LABEL-LINES (1).
003574     MOVE BE-ADDRESS-LINE-12     TO REC-TEXT (2)
003575                                    WS-LABEL-LINES (2).
003576     MOVE BE-ADDRESS-LINE-22     TO REC-TEXT (3)
003577                                    WS-LABEL-LINES (3).
003578     MOVE BE-ADDRESS-LINE-32     TO REC-TEXT (4)
003579                                    WS-LABEL-LINES (4).
003580*    MOVE BE-CITY-STATE2         TO REC-TEXT (5)
003581*                                   WS-LABEL-LINES (5).
003582     STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
003583        INTO WS-LABEL-LINES (5)
003584     END-STRING
003585     MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
003586
003587     MOVE SPACES                 TO WS-ZIP-CODE.
003588
003589     IF BE-CANADIAN-POST-CODE2
003590         MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1
003591         MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2
003592     ELSE
003593         MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE
003594         IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS
003595             MOVE '-'            TO WS-AM-ZIP-DASH
003596             MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.
003597
003598     MOVE WS-ZIP-CODE            TO REC-TEXT (6)
003599                                    WS-LABEL-LINES (6).
003600
003601     GO TO 7069-SET-ADDR.
003602
003603 7068-CONTINUE-BUILD-ADDR.
003604
003605     IF ACTV-SEQ = ZEROS
003606        GO TO 7070-ACTV-NOT-FOUND.
003607
003608     MOVE ACTV-SEQ               TO PI-ADDR-SEQ.
003609
003610     
      * EXEC CICS HANDLE CONDITION
003611*         NOTOPEN    (8870-ACTV-NOT-OPEN)
003612*         NOTFND     (7070-ACTV-NOT-FOUND)
003613*    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00009293' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303039323933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003614
003615     
      * EXEC CICS READ
003616*         DATASET    (ACTV-ID)
003617*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
003618*         RIDFLD     (ACTV-KEY)
003619*    END-EXEC.
      *    MOVE '&"S        E          (   #00009298' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039323938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003620
003621     MOVE SPACES            TO              WS-LABEL-HOLD-AREA.
003622     MOVE AT-MAIL-TO-NAME   TO REC-TEXT (1) WS-LABEL-LINES (1).
003623     MOVE AT-ADDRESS-LINE-1 TO REC-TEXT (2) WS-LABEL-LINES (2).
003624     MOVE AT-ADDRESS-LINE-2 TO REC-TEXT (3) WS-LABEL-LINES (3).
003625*    MOVE AT-CITY-STATE     TO REC-TEXT (4) WS-LABEL-LINES (4).
003626     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
003627        INTO WS-LABEL-LINES (4)
003628     END-STRING
003629     MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
003630
003631     MOVE SPACES            TO  WS-ZIP-CODE.
003632     IF AT-CANADIAN-POST-CODE
003633         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
003634         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
003635     ELSE
003636         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
003637         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
003638             MOVE '-'            TO  WS-AM-ZIP-DASH
003639             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
003640
003641     MOVE WS-ZIP-CODE       TO  REC-TEXT (5) WS-LABEL-LINES (5).
003642
003643     MOVE SPACES            TO  REC-TEXT (6).
003644
003645 7069-SET-ADDR.
003646     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
003647
003648     MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.
003649     MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.
003650     MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.
003651     MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.
003652     MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.
003653     MOVE SPACES             TO  REC-TEXT (6)    SS05-6D.
003654
003655     GO TO 7099-EXIT.
003656
003657 7070-ACTV-NOT-FOUND.
003658
003659     IF ACTV-SEQ NOT = +29
003660        GO TO 7075-CONTINUE-ACTV-ERROR.
003661
003662*    IF CL-SYSTEM-IDENTIFIER = 'CV'
003663*        GO TO 7099-EXIT.
003664
003665     IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
003666        MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL
003667        GO TO 7075-CONTINUE-ACTV-ERROR.
003668
003669     IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS
003670        GO TO 7075-CONTINUE-ACTV-ERROR.
003671
003672     MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD
003673     MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER
003674     MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING
003675     MOVE 'A'             TO WS-ERCOMP-TYPE
003676     MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
003677                          TO WS-ERCOMP-RESP-NO.
003678
003679     IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
003680         IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
003681                                       'G' OR 'B' or 'S'
003682             MOVE 'G'            TO WS-ERCOMP-TYPE
003683             MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT
003684         ELSE
003685             MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
003686                                 TO WS-ERCOMP-ACCOUNT
003687     ELSE
003688         MOVE 'G'                TO WS-ERCOMP-TYPE
003689         MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
003690
003691     IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
003692        MOVE ZEROS TO WS-ERCOMP-CARRIER.
003693
003694     IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
003695        MOVE ZEROS TO WS-ERCOMP-GROUPING.
003696
003697     
      * EXEC CICS HANDLE CONDITION
003698*         NOTFND    (7075-CONTINUE-ACTV-ERROR)
003699*    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009380' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303039333830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003700
003701     
      * EXEC CICS  READ
003702*         SET      (ADDRESS OF COMPENSATION-MASTER)
003703*         DATASET  ('ERCOMP')
003704*         RIDFLD   (WS-ERCOMP-KEY)
003705*    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009384' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039333834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003706
003707     MOVE 'Y'                   TO WS-COMP-READ-SW.
003708     MOVE SPACES                TO WS-LABEL-HOLD-AREA.
003709     MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).
003710
003711     IF CO-ACCT-NAME = SPACES
003712        MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).
003713
003714     MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).
003715     MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).
003716     MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).
003717
003718     MOVE SPACES                TO  WS-ZIP-CODE.
003719     IF CO-CANADIAN-POST-CODE
003720         MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
003721         MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
003722     ELSE
003723         MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE
003724         IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
003725             MOVE '-'           TO  WS-AM-ZIP-DASH
003726             MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
003727
003728     MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
003729
003730     MOVE ZEROS                 TO WS-PHONE-IN.
003731     MOVE CO-AREA-CODE          TO WSPO-AREA.
003732     MOVE CO-PREFIX             TO WSPO-PFX.
003733     MOVE CO-PHONE              TO WSPO-SFX.
003734     MOVE WS-PHONE-OUT          TO SS53-6D.
003735
003736     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
003737
003738     MOVE WS-LABEL-LINES (1)     TO REC-TEXT (1)    SS52D.
003739     MOVE WS-LABEL-LINES (2)     TO REC-TEXT (2)    SS53-1D.
003740     MOVE WS-LABEL-LINES (3)     TO REC-TEXT (3)    SS53-2D.
003741     MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)    SS53-3D.
003742     MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)    SS53-4D.
003743     MOVE WS-LABEL-LINES (6)     TO REC-TEXT (6)    SS53-5D.
003744
003745     GO TO 7099-EXIT.
003746     EJECT
003747
003748 7075-CONTINUE-ACTV-ERROR.
003749     MOVE ER-0178                TO EMI-ERROR.
003750     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003751     MOVE -1                     TO ADDRL.
003752     MOVE AL-UABON               TO ADDRA.
003753     MOVE SPACES                 TO PI-ADDR-TYPE.
003754     MOVE SPACES                 TO REC-TEXT (1)
003755                                    REC-TEXT (2)
003756                                    REC-TEXT (3)
003757                                    REC-TEXT (4)
003758                                    REC-TEXT (5)
003759                                    REC-TEXT (6).
003760     GO TO 7099-EXIT.
003761
003762 7080-ACCT-NOT-FOUND.
003763     MOVE ER-0179                TO EMI-ERROR.
003764     MOVE -1                     TO ADDRL.
003765     MOVE AL-UABON               TO ADDRA.
003766     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003767     GO TO 8200-SEND-DATAONLY.
003768
003769 7090-CLAIM-NOT-FOUND.
003770     MOVE ER-0186                TO EMI-ERROR.
003771     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003772     GO TO 8200-SEND-DATAONLY.
003773
003774*7095-PROD-NOT-FOUND. Remove as dead code
003775
003776 7099-EXIT.
003777      EXIT.
003778     EJECT
003779
003780 7170-FORMAT-SCREEN.
003781     IF MAINTI = 'S'
003782        MOVE AL-PANOF            TO SC-TEXTA (SC-INDX)
003783       ELSE
003784        MOVE AL-UANOF            TO SC-TEXTA (SC-INDX).
003785
003786     IF TB-INDX < 7
003787        SET LINE23 TO TB-INDX
003788        MOVE 'A'                 TO LINE1
003789       ELSE
003790        IF TB-INDX = 7
003791           MOVE ZEROS            TO LINE-NUM
003792        ELSE
003793           SET LIN-NUM TO TB-INDX
003794           SUBTRACT 7            FROM LIN-NUM.
003795
003796     MOVE LINE-NUM               TO SC-LINE (SC-INDX).
003797     MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).
003798     SET TB-INDX UP BY 1.
003799
003800 7170-EXIT.
003801      EXIT.
003802
003803     EJECT
003804 7200-RESOLVE-VARIABLES.
003805***************************************************************
003806*    THIS ROUTINE WILL FORMAT THE SYSTEM-DEFINED SYMBOLS      *
003807*    WITH DATA PERTAINING TO THE DESIGNATED CLAIM.            *
003808*    THIS ROUTINE IS PERFORMED THRU 7269-EXIT TO              *
003809*    RESOLVE ALL OF THE SYMBOLS.                              *
003810***************************************************************
003811
003812*    IF PI-COMPANY-ID = 'AUK'
003813*        MOVE 'Y'                TO W-REVERSE-DATE-SW.
003814
003815     MOVE PI-CLAIM-NO            TO SS34D.
003816
003817     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
003818        MOVE PI-LIFE-OVERRIDE-L6 TO SS12D
003819     ELSE
003820        MOVE PI-AH-OVERRIDE-L6   TO SS12D.
003821
003822     MOVE CL-LAST-PMT-AMT        TO SS16D.
003823     MOVE CL-TOTAL-PAID-AMT      TO SS18D.
003824     MOVE CL-CAUSE-CD            TO SS19-1D.
003825     MOVE CL-ASSOC-CERT-SEQU     TO SS54D.
003826     MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.
003827
003828*    IF PI-COMPANY-ID NOT = 'DMD'
003829*        GO TO 7200-CONTINUE.
003830
003831     IF CL-CCN-PREFIX-A5 = '1111' OR '3333' OR '6666' OR
003832                           '8888' OR '9876'
003833         MOVE CL-CCN-PRIME-A5    TO WS-CCN-12
003834         MOVE WS-CCN-12          TO CL-CCN.
003835
003836     IF CL-CERT-STATE NOT = '08' AND '09'
003837         MOVE SPACES             TO SS62D
003838                                    SS63D
003839         GO TO 7200-CONTINUE.
003840
003841     MOVE WS-DMD-UND-STATEMENT   TO SS62D.
003842
003843     MOVE 'CL'                   TO DL23-SYSTEM-ID.
003844     MOVE 'UN'                   TO DL23-RECORD-TYPE.
003845     MOVE CL-CERT-STATE          TO DL23-RECORD-KEY.
003846
003847     
      * EXEC CICS LINK
003848*        PROGRAM    ('DLO023')
003849*        COMMAREA   (WS-DLO-CODES-TABLE)
003850*        LENGTH     (DL23-COMM-LENGTH)
003851*    END-EXEC.
           MOVE 'DLO023' TO DFHEIV1
      *    MOVE '."C                   (   #00009530' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039353330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-CODES-TABLE, 
                 DL23-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003852
003853     IF DL23-RETURN-CODE = 'OK'
003854         MOVE DL23-CODE-DESC     TO SS63D
003855       ELSE
003856         MOVE SPACES             TO SS63D.
003857
003858 7200-CONTINUE.
003859     MOVE CL-CCN                 TO SS60D.
003860
003861     MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
003862     MOVE SPACES                 TO DC-OPTION-CODE.
003863     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
003864     IF NO-CONVERSION-ERROR
003865        MOVE DC-GREG-DATE-1-EDIT TO SS13D.
003866
003867     MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.
003868     MOVE SPACES                 TO DC-OPTION-CODE.
003869     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
003870     IF NO-CONVERSION-ERROR
003871        MOVE DC-GREG-DATE-1-EDIT TO SS14D.
003872
003873     MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.
003874     MOVE SPACES                 TO DC-OPTION-CODE.
003875     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
003876     IF NO-CONVERSION-ERROR
003877        MOVE DC-GREG-DATE-1-EDIT TO SS15D.
003878
003879     IF NOT PI-USES-PAID-TO
003880        MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
003881        MOVE SPACES                 TO DC-OPTION-CODE
003882        PERFORM 9700-DATE-LINK THRU 9700-EXIT
003883        IF NO-CONVERSION-ERROR
003884           MOVE DC-GREG-DATE-1-EDIT TO SS17D
003885        ELSE
003886           MOVE SPACES TO SS17D
003887     ELSE
003888        MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
003889        MOVE '6'                    TO DC-OPTION-CODE
003890        MOVE +1                     TO DC-ELAPSED-DAYS
003891        MOVE +0                     TO DC-ELAPSED-MONTHS
003892        PERFORM 9700-DATE-LINK THRU 9700-EXIT
003893        IF NO-CONVERSION-ERROR
003894           MOVE DC-GREG-DATE-1-EDIT TO SS17D.
003895
003896     MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.
003897     MOVE SPACES                 TO DC-OPTION-CODE.
003898     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
003899     IF NO-CONVERSION-ERROR
003900        MOVE DC-GREG-DATE-1-EDIT TO SS45D
003901     ELSE
003902        MOVE '@@DOB'             TO SS45D.
003903
003904     IF CL-SSN-STATE   = CL-CERT-STATE  AND
003905        CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
003906             NEXT SENTENCE
003907         ELSE
003908             MOVE CL-SOC-SEC-NO  TO SS46D.
003909
003910     MOVE CL-CERT-GROUPING       TO CERT-GROUPING
003911                                    ACCT-GROUPING
003912                                    PROD-GROUPING
003913                                    PLCY-GROUPING
003914                                    PLAN-GROUPING.
003915     MOVE CL-CERT-STATE          TO CERT-STATE
003916                                    ACCT-STATE
003917                                    PROD-STATE
003918                                    PLCY-STATE
003919                                    PLAN-STATE.
003920     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
003921                                    ACCT-ACCOUNT
003922                                    PROD-PRODUCER
003923                                    PLCY-PRODUCER
003924                                    PLAN-PRODUCER.
003925     MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT
003926                                    PLCY-EFF-DT.
003927     MOVE CL-CV-REFERENCE-NO     TO PLCY-REFERENCE-NO.
003928
003929     PERFORM 5000-MOVE-NAME THRU 5000-EXIT.
003930
003931*    IF PI-COMPANY-ID = 'DMD'
003932*       MOVE WS-NAME-WORK           TO SS10D
003933*       GO TO 7200-CONTINUE-2.
003934
003935     IF LOWER-CASE-LETTERS-USED
003936        MOVE 'N'                    TO WS-STATE-LINE
003937        MOVE WS-NAME-WORK           TO WS-TEMP-AREA2
003938        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
003939        MOVE WS-TEMP-AREA2          TO SS10D
003940     ELSE
003941        MOVE WS-NAME-WORK           TO SS10D.
003942
003943 7200-CONTINUE-2.
003944     MOVE CL-INSURED-1ST-NAME     TO W-FIRST-NAME.
003945     MOVE CL-INSURED-LAST-NAME    TO W-LAST-NAME.
003946     MOVE CL-INSURED-MID-INIT     TO W-MIDDLE-NAME.
003947     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
003948
003949     IF LOWER-CASE-LETTERS-USED
003950        MOVE 'N'                    TO WS-STATE-LINE
003951        MOVE WS-NAME-WORK           TO WS-TEMP-AREA2
003952        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
003953        MOVE WS-TEMP-AREA2          TO SS39D
003954     ELSE
003955        MOVE WS-NAME-WORK           TO SS39D.
003956
003957     MOVE CL-INSURED-LAST-NAME   TO SS40D.
003958
003959     IF INSURED-IS-FEMALE
003960        MOVE 'MS.'               TO SS41D
003961       ELSE
003962        MOVE 'MR.'               TO SS41D.
003963
003964*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
003965*        PERFORM 7290-RESOLVE-CREDITOR THRU 7290-EXIT.
003966
003967*    IF CL-SYSTEM-IDENTIFIER = 'CV'
003968*        GO TO 7200-READ-EMPLCY.
003969     EJECT
003970     
      * EXEC CICS HANDLE CONDITION
003971*         NOTOPEN   (8900-CERT-NOT-OPEN)
003972*         NOTFND    (8910-CERT-NOT-FOUND)
003973*    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00009653' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303039363533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003974
003975     
      * EXEC CICS READ
003976*         DATASET   (CERT-ID)
003977*         SET       (ADDRESS OF CERTIFICATE-MASTER)
003978*         RIDFLD    (CERT-KEY)
003979*    END-EXEC.
      *    MOVE '&"S        E          (   #00009658' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039363538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003980
003981*    IF PI-COMPANY-ID = 'DMD'
003982*        MOVE CM-RESIDENT-STATE  TO WS-DMD-RES-ST
003983*        IF CM-POLICY-FORM-NO(6:1) = '1'
003984*            MOVE WS-DMD-UND-COMPANYA    TO SS64D
003985*          ELSE
003986*        IF CM-POLICY-FORM-NO(6:1) = '2'
003987*            MOVE WS-DMD-UND-COMPANYB    TO SS64D
003988*          ELSE
003989*            MOVE SPACES                 TO SS64D.
003990
003991     MOVE CM-CERT-NO             TO SS26D.
003992     MOVE CM-INSURED-ISSUE-AGE   TO SS33D.
003993
003994*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
003995*        MOVE CL-CURRENT-CARRIER TO SS23D
003996*        MOVE CL-CURRENT-GROUPING
003997*                                TO SS24D
003998*        MOVE CL-CURRENT-ACCOUNT TO SS25D
003999*        MOVE CM-MEMBER-NO       TO SS36D
004000*        MOVE CM-GROUPING        TO W-GROUPING
004001*        IF W-GROUP-3 = 'C01' OR 'C02'
004002*            MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO
004003*            MOVE CM-LOAN-NUMBER TO W-LOAN-NO
004004*            MOVE W-CREDIT-CARD-LOAN-NO
004005*                                TO SS36-1D
004006*        ELSE
004007*            MOVE CM-MEMBER-NO   TO SS36-1D
004008*    ELSE
004009         MOVE CM-CARRIER         TO SS23D.
004010         MOVE CM-GROUPING        TO SS24D.
004011         MOVE CM-ACCOUNT         TO SS25D.
004012         MOVE CM-LOAN-NUMBER     TO SS36D.
004013
004014     MOVE CM-LOAN-BALANCE        TO SS37D.
004015     MOVE CM-MEMBER-NO           TO SS38D.
004016
004017     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
004018     MOVE SPACES                 TO DC-OPTION-CODE.
004019     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
004020     IF NO-CONVERSION-ERROR
004021        MOVE DC-GREG-DATE-1-EDIT TO SS27D.
004022
004023     MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
004024     MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.
004025     MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.
004026     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
004027
004028     IF LOWER-CASE-LETTERS-USED
004029         MOVE 'N'                TO WS-STATE-LINE
004030         MOVE WS-NAME-WORK       TO WS-TEMP-AREA2
004031         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
004032         MOVE WS-TEMP-AREA2      TO SS57D
004033     ELSE
004034         MOVE WS-NAME-WORK       TO SS57D.
004035
004036     MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.
004037     MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.
004038     MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.
004039     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
004040
004041     IF LOWER-CASE-LETTERS-USED
004042         MOVE 'N'                TO WS-STATE-LINE
004043         MOVE WS-NAME-WORK       TO WS-TEMP-AREA2
004044         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
004045         MOVE WS-TEMP-AREA2      TO SS58D
004046     ELSE
004047         MOVE WS-NAME-WORK       TO SS58D.
004048
004049     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
004050        MOVE CM-LF-BENEFIT-CD    TO BEN-HOLD
004051        MOVE CM-LF-ORIG-TERM     TO SS29D
004052        MOVE CM-LF-BENEFIT-AMT   TO SS30D
004053        MOVE CM-POLICY-FORM-NO   TO SS32D
004054        MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
004055     ELSE
004056        MOVE CM-AH-BENEFIT-CD    TO BEN-HOLD
004057        MOVE CM-AH-ORIG-TERM     TO SS29D
004058        MOVE CM-AH-BENEFIT-AMT   TO SS30D
004059        MOVE CM-POLICY-FORM-NO   TO SS32D
004060        MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1.
004061
004062     COMPUTE WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.
004063
004064     MOVE WORK-AMOUNT            TO SS51D.
004065
004066     MOVE SPACES                 TO DC-OPTION-CODE.
004067     PERFORM 9700-DATE-LINK THRU 9700-EXIT.
004068     IF NO-CONVERSION-ERROR
004069        MOVE DC-GREG-DATE-1-EDIT TO SS31D.
004070
004071     MOVE ' '                         TO  DC-OPTION-CODE.
004072     MOVE +0                          TO  DC-ELAPSED-MONTHS
004073                                          DC-ELAPSED-DAYS.
004074
004075     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
004076         MOVE CM-LF-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
004077         PERFORM 9700-DATE-LINK THRU 9700-EXIT
004078         IF NO-CONVERSION-ERROR
004079             MOVE DC-GREG-DATE-1-EDIT TO  SS28D
004080         ELSE
004081             NEXT SENTENCE
004082     ELSE
004083         MOVE CM-AH-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
004084         PERFORM 9700-DATE-LINK THRU 9700-EXIT
004085         IF NO-CONVERSION-ERROR
004086             MOVE DC-GREG-DATE-1-EDIT TO  SS28D.
004087
004088*    GO TO 7200-READ-DIAGNOSIS.
004089
004090     EJECT
004091*7200-READ-EMPLCY. Remove as dead code
004092 7200-READ-DIAGNOSIS.
004093     
      * EXEC CICS HANDLE CONDITION
004094*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004095*         NOTFND     (7200-READ-LOAN-NUMBER)
004096*    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00009776' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303039373736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004097
004098     MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.
004099     MOVE +90                   TO ACTV-SEQ.
004100
004101     
      * EXEC CICS READ
004102*         DATASET  (ACTV-ID)
004103*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
004104*         RIDFLD   (ACTV-KEY)
004105*    END-EXEC.
      *    MOVE '&"S        E          (   #00009784' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039373834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004106
004107     IF AT-TRAILER-TYPE = '6'
004108        MOVE AT-INFO-LINE-1         TO SS19D.
004109
004110 7200-READ-LOAN-NUMBER.
004111
004112     
      * EXEC CICS HANDLE CONDITION
004113*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004114*         NOTFND     (7201-READ-BENEFICIARY)
004115*    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00009795' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303039373935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004116
004117     MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.
004118     MOVE +91                   TO ACTV-SEQ.
004119
004120     
      * EXEC CICS READ
004121*         DATASET  (ACTV-ID)
004122*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
004123*         RIDFLD   (ACTV-KEY)
004124*    END-EXEC.
      *    MOVE '&"S        E          (   #00009803' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004125
004126     IF AT-TRAILER-TYPE = '6'
004127        MOVE AT-INFO-LINE-1         TO SS19-2D.
004128
004129 7201-READ-BENEFICIARY.
004130     IF CL-BENIF-ADDR-CNT = +0  AND
004131        CL-BENEFICIARY = SPACES
004132          GO TO 7205-READ-PHYSICIAN-ADDR.
004133
004134     IF W-BENEFICIARY > ZERO
004135         COMPUTE ACTV-SEQ = W-BENEFICIARY + 10
004136     ELSE
004137         MOVE CL-BENIF-ADDR-CNT  TO ACTV-SEQ
004138         ADD +10                 TO ACTV-SEQ.
004139
004140     
      * EXEC CICS HANDLE CONDITION
004141*         NOTOPEN    (7290-BENE-NOT-OPEN)
004142*         NOTFND     (7205-READ-PHYSICIAN-ADDR)
004143*    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00009823' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303039383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004144
004145     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004146
004147     IF ACTV-SEQ = +19
004148         NEXT SENTENCE
004149     ELSE
004150         IF ACTV-SEQ NOT = +10
004151             GO TO 7202-GET-FROM-ACTIVITY.
004152
004153     MOVE PI-COMPANY-CD        TO BENE-COMP-CD.
004154     MOVE 'B'                  TO BENE-REC-TYPE.
004155     MOVE CL-BENEFICIARY       TO BENE-NUMBER.
004156
004157     
      * EXEC CICS READ
004158*         DATASET    (BENE-ID)
004159*         SET        (ADDRESS OF BENEFICIARY-MASTER)
004160*         RIDFLD     (BENE-KEY)
004161*    END-EXEC.
      *    MOVE '&"S        E          (   #00009840' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004162
004163*    IF PI-COMPANY-ID = 'DMD'
004164*        MOVE BE-BSR             TO PI-ELBENE-BSR.
004165
004166 7201-CONT.
004167     MOVE BE-MAIL-TO-NAME2       TO WS-LABEL-LINES (1).
004168     MOVE BE-ADDRESS-LINE-12     TO WS-LABEL-LINES (2).
004169     MOVE BE-ADDRESS-LINE-22     TO WS-LABEL-LINES (3).
004170     MOVE BE-ADDRESS-LINE-32     TO WS-LABEL-LINES (4).
004171*    MOVE BE-CITY-STATE2         TO WS-LABEL-LINES (5).
004172     STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
004173        INTO WS-LABEL-LINES (5)
004174     END-STRING
004175
004176     MOVE SPACES                 TO WS-ZIP-CODE.
004177
004178     IF BE-CANADIAN-POST-CODE2
004179         MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1
004180         MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2
004181     ELSE
004182         MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE
004183         IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS
004184             MOVE '-'            TO WS-AM-ZIP-DASH
004185             MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.
004186
004187     MOVE WS-ZIP-CODE            TO WS-LABEL-LINES (6).
004188
004189     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004190
004191     MOVE WS-LABEL-LINES (1)     TO SS61D.
004192     MOVE WS-LABEL-LINES (2)     TO SS61-1D.
004193     MOVE WS-LABEL-LINES (3)     TO SS61-2D.
004194     MOVE WS-LABEL-LINES (4)     TO SS61-3D.
004195     MOVE WS-LABEL-LINES (5)     TO SS61-4D.
004196     MOVE WS-LABEL-LINES (6)     TO SS61-5D.
004197
004198     MOVE BE-PHONE-NO2           TO WS-PHONE-IN.
004199     MOVE WSPI-AREA              TO WSPO-AREA.
004200     MOVE WSPI-PFX               TO WSPO-PFX.
004201     MOVE WSPI-SFX               TO WSPO-SFX.
004202     MOVE WS-PHONE-OUT           TO SS61-6D.
004203
004204 7201-CONT-PROCESS.
004205     MOVE BE-MAIL-TO-NAME      TO  WS-LABEL-LINES (1).
004206     MOVE BE-ADDRESS-LINE-1    TO  WS-LABEL-LINES (2).
004207     MOVE BE-ADDRESS-LINE-2    TO  WS-LABEL-LINES (3).
004208     MOVE BE-ADDRESS-LINE-3    TO  WS-LABEL-LINES (4).
004209*    MOVE BE-CITY-STATE        TO  WS-LABEL-LINES (5).
004210     STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
004211        INTO WS-LABEL-LINES (5)
004212     END-STRING
004213
004214     MOVE SPACES               TO  WS-ZIP-CODE.
004215     IF BE-CANADIAN-POST-CODE
004216         MOVE BE-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1
004217         MOVE BE-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2
004218     ELSE
004219         MOVE BE-ZIP-PRIME     TO  WS-AM-ZIP-CODE
004220         IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004221             MOVE '-'          TO  WS-AM-ZIP-DASH
004222             MOVE BE-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.
004223
004224     MOVE WS-ZIP-CODE          TO  WS-LABEL-LINES (6).
004225
004226     MOVE BE-PHONE-NO          TO  WS-PHONE-IN.
004227
004228     GO TO 7204-SET-PHONE.
004229
004230 7202-GET-FROM-ACTIVITY.
004231     
      * EXEC CICS READ
004232*         DATASET  (ACTV-ID)
004233*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
004234*         RIDFLD   (ACTV-KEY)
004235*    END-EXEC.
      *    MOVE '&"S        E          (   #00009914' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039393134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004236
004237     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004238     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004239     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004240*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004241     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004242        INTO WS-LABEL-LINES (4)
004243     END-STRING
004244
004245     MOVE SPACES                 TO  WS-ZIP-CODE.
004246     IF AT-CANADIAN-POST-CODE
004247         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004248         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004249     ELSE
004250         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004251         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004252             MOVE '-'            TO  WS-AM-ZIP-DASH
004253             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004254
004255     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004256
004257     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004258
004259 7204-SET-PHONE.
004260     MOVE WSPI-AREA              TO WSPO-AREA.
004261     MOVE WSPI-PFX               TO WSPO-PFX.
004262     MOVE WSPI-SFX               TO WSPO-SFX.
004263     MOVE WS-PHONE-OUT           TO SS44-5D.
004264
004265     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004266
004267     MOVE WS-LABEL-LINES (1)     TO SS43D.
004268     MOVE WS-LABEL-LINES (2)     TO SS44-1D.
004269     MOVE WS-LABEL-LINES (3)     TO SS44-2D.
004270     MOVE WS-LABEL-LINES (4)     TO SS44-3D.
004271     MOVE WS-LABEL-LINES (5)     TO SS44-4D.
004272     MOVE WS-LABEL-LINES (6)     TO SS44-6D.
004273
004274     EJECT
004275 7205-READ-PHYSICIAN-ADDR.
004276     IF W-PHYSICIAN > ZERO
004277         COMPUTE ACTV-SEQ = W-PHYSICIAN + 30
004278     ELSE
004279         MOVE CL-DOCTOR-ADDR-CNT TO ACTV-SEQ
004280         ADD +30                 TO ACTV-SEQ.
004281
004282     IF ACTV-SEQ = +30
004283        GO TO 7210-READ-EMPLOYER-ADDR.
004284
004285     
      * EXEC CICS HANDLE CONDITION
004286*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004287*         NOTFND     (7210-READ-EMPLOYER-ADDR)
004288*    END-EXEC.
      *    MOVE '"$JI                  ! 6 #00009968' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303039393638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004289
004290     
      * EXEC CICS READ
004291*         DATASET    (ACTV-ID)
004292*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004293*         RIDFLD     (ACTV-KEY)
004294*    END-EXEC.
      *    MOVE '&"S        E          (   #00009973' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039393733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004295
004296     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004297     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004298     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004299     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004300*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004301     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004302        INTO WS-LABEL-LINES (4)
004303     END-STRING
004304
004305     MOVE SPACES                 TO  WS-ZIP-CODE.
004306     IF AT-CANADIAN-POST-CODE
004307         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004308         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004309     ELSE
004310         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004311         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004312             MOVE '-'            TO  WS-AM-ZIP-DASH
004313             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004314
004315     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004316
004317     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004318     MOVE WSPI-AREA              TO WSPO-AREA.
004319     MOVE WSPI-PFX               TO WSPO-PFX.
004320     MOVE WSPI-SFX               TO WSPO-SFX.
004321     MOVE WS-PHONE-OUT           TO SS47-5D.
004322
004323     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004324
004325     MOVE WS-LABEL-LINES (1)     TO SS47D.
004326     MOVE WS-LABEL-LINES (2)     TO SS47-1D.
004327     MOVE WS-LABEL-LINES (3)     TO SS47-2D.
004328     MOVE WS-LABEL-LINES (4)     TO SS47-3D.
004329     MOVE WS-LABEL-LINES (5)     TO SS47-4D.
004330
004331     EJECT
004332
004333 7210-READ-EMPLOYER-ADDR.
004334     IF W-EMPLOYER > ZERO
004335         COMPUTE ACTV-SEQ = W-EMPLOYER + 40
004336     ELSE
004337         MOVE CL-EMPLOYER-ADDR-CNT
004338                                 TO ACTV-SEQ
004339         ADD +40                 TO ACTV-SEQ.
004340
004341     IF ACTV-SEQ = +40
004342        GO TO 7220-READ-INSURED-ADDR.
004343
004344     
      * EXEC CICS HANDLE CONDITION
004345*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004346*         NOTFND     (7220-READ-INSURED-ADDR)
004347*    END-EXEC.
      *    MOVE '"$JI                  ! 7 #00010027' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303130303237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004348
004349     
      * EXEC CICS READ
004350*         DATASET    (ACTV-ID)
004351*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004352*         RIDFLD     (ACTV-KEY)
004353*    END-EXEC.
      *    MOVE '&"S        E          (   #00010032' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130303332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004354
004355     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004356     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004357     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004358     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004359*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004360     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004361        INTO WS-LABEL-LINES (4)
004362     END-STRING
004363
004364     MOVE SPACES                 TO  WS-ZIP-CODE.
004365     IF AT-CANADIAN-POST-CODE
004366         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004367         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004368     ELSE
004369         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004370         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004371             MOVE '-'            TO  WS-AM-ZIP-DASH
004372             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004373
004374     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004375
004376     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004377     MOVE WSPI-AREA              TO WSPO-AREA.
004378     MOVE WSPI-PFX               TO WSPO-PFX.
004379     MOVE WSPI-SFX               TO WSPO-SFX.
004380     MOVE WS-PHONE-OUT           TO SS48-5D.
004381
004382     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004383
004384     MOVE WS-LABEL-LINES (1)     TO SS48D.
004385     MOVE WS-LABEL-LINES (2)     TO SS48-1D.
004386     MOVE WS-LABEL-LINES (3)     TO SS48-2D.
004387     MOVE WS-LABEL-LINES (4)     TO SS48-3D.
004388     MOVE WS-LABEL-LINES (5)     TO SS48-4D.
004389
004390     EJECT
004391 7220-READ-INSURED-ADDR.
004392
004393     IF W-INSURED > ZERO
004394         COMPUTE ACTV-SEQ = W-INSURED
004395     ELSE
004396         MOVE CL-INSURED-ADDR-CNT TO ACTV-SEQ.
004397
004398     IF ACTV-SEQ = +0
004399        GO TO 7225-READ-OTHER1-ADDR.
004400
004401     
      * EXEC CICS HANDLE CONDITION
004402*         NOTOPEN   (8870-ACTV-NOT-OPEN)
004403*         NOTFND    (7225-READ-OTHER1-ADDR)
004404*    END-EXEC.
      *    MOVE '"$JI                  ! 8 #00010084' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303130303834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004405
004406     
      * EXEC CICS READ
004407*         DATASET   (ACTV-ID)
004408*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
004409*         RIDFLD    (ACTV-KEY)
004410*    END-EXEC.
      *    MOVE '&"S        E          (   #00010089' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130303839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004411
004412     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004413     MOVE SS10D                  TO WS-LABEL-LINES (1).
004414     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004415     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004416*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004417     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004418        INTO WS-LABEL-LINES (4)
004419     END-STRING
004420
004421     MOVE SPACES                 TO  WS-ZIP-CODE.
004422     IF AT-CANADIAN-POST-CODE
004423         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004424         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004425     ELSE
004426         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004427         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004428             MOVE '-'            TO  WS-AM-ZIP-DASH
004429             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004430
004431     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004432
004433     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004434
004435     MOVE WS-LABEL-LINES (1)     TO SS10D.
004436     MOVE WS-LABEL-LINES (2)     TO SS11-1D.
004437     MOVE WS-LABEL-LINES (3)     TO SS11-2D.
004438     MOVE WS-LABEL-LINES (4)     TO SS11-3D.
004439     MOVE WS-LABEL-LINES (5)     TO SS11-4D.
004440     MOVE AT-MAIL-TO-NAME        TO SS11-5D.
004441
004442     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004443     MOVE WSPI-AREA              TO WSPO-AREA.
004444     MOVE WSPI-PFX               TO WSPO-PFX.
004445     MOVE WSPI-SFX               TO WSPO-SFX.
004446     MOVE WS-PHONE-OUT           TO SS11-6D.
004447
004448     EJECT
004449
004450 7225-READ-OTHER1-ADDR.
004451
004452     IF W-OTHER-1 > ZERO
004453         COMPUTE ACTV-SEQ = W-OTHER-1 + 50
004454     ELSE
004455         MOVE CL-OTHER-1-ADDR-CNT TO ACTV-SEQ
004456         ADD +50                  TO ACTV-SEQ.
004457
004458     IF ACTV-SEQ = +50
004459        GO TO 7230-READ-OTHER2-ADDR.
004460
004461     
      * EXEC CICS HANDLE CONDITION
004462*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004463*         NOTFND     (7230-READ-OTHER2-ADDR)
004464*    END-EXEC.
      *    MOVE '"$JI                  ! 9 #00010144' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303130313434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004465
004466     
      * EXEC CICS READ
004467*         DATASET    (ACTV-ID)
004468*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004469*         RIDFLD     (ACTV-KEY)
004470*    END-EXEC.
      *    MOVE '&"S        E          (   #00010149' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130313439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004471
004472     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004473     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004474     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004475     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004476*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004477     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004478        INTO WS-LABEL-LINES (4)
004479     END-STRING
004480
004481     MOVE SPACES                 TO  WS-ZIP-CODE.
004482     IF AT-CANADIAN-POST-CODE
004483         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004484         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004485     ELSE
004486         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004487         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004488             MOVE '-'            TO  WS-AM-ZIP-DASH
004489             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004490
004491     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004492
004493     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004494     MOVE WSPI-AREA              TO WSPO-AREA.
004495     MOVE WSPI-PFX               TO WSPO-PFX.
004496     MOVE WSPI-SFX               TO WSPO-SFX.
004497     MOVE WS-PHONE-OUT           TO SS49-5D.
004498
004499     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004500
004501     MOVE WS-LABEL-LINES (1)     TO SS49D.
004502     MOVE WS-LABEL-LINES (2)     TO SS49-1D.
004503     MOVE WS-LABEL-LINES (3)     TO SS49-2D.
004504     MOVE WS-LABEL-LINES (4)     TO SS49-3D.
004505     MOVE WS-LABEL-LINES (5)     TO SS49-4D.
004506
004507     EJECT
004508 7230-READ-OTHER2-ADDR.
004509
004510     IF W-OTHER-2 > ZERO
004511         COMPUTE ACTV-SEQ = W-OTHER-2 + 60
004512     ELSE
004513         MOVE CL-OTHER-2-ADDR-CNT TO ACTV-SEQ
004514         ADD +60                  TO ACTV-SEQ.
004515
004516     IF ACTV-SEQ = +60
004517        GO TO 7240-NOT-FOUND.
004518
004519     
      * EXEC CICS HANDLE CONDITION
004520*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004521*         NOTFND     (7240-NOT-FOUND)
004522*    END-EXEC.
      *    MOVE '"$JI                  ! : #00010202' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303130323032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004523
004524     
      * EXEC CICS READ
004525*         DATASET    (ACTV-ID)
004526*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004527*         RIDFLD     (ACTV-KEY)
004528*    END-EXEC.
      *    MOVE '&"S        E          (   #00010207' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004529
004530     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004531     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004532     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004533     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004534*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004535     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004536        INTO WS-LABEL-LINES (4)
004537     END-STRING
004538
004539     MOVE SPACES                 TO  WS-ZIP-CODE.
004540     IF AT-CANADIAN-POST-CODE
004541         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004542         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004543     ELSE
004544         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004545         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004546             MOVE '-'            TO  WS-AM-ZIP-DASH
004547             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004548
004549     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004550
004551     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004552     MOVE WSPI-AREA              TO WSPO-AREA.
004553     MOVE WSPI-PFX               TO WSPO-PFX.
004554     MOVE WSPI-SFX               TO WSPO-SFX.
004555     MOVE WS-PHONE-OUT           TO SS50-5D.
004556
004557     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004558
004559     MOVE WS-LABEL-LINES (1)     TO SS50D.
004560     MOVE WS-LABEL-LINES (2)     TO SS50-1D.
004561     MOVE WS-LABEL-LINES (3)     TO SS50-2D.
004562     MOVE WS-LABEL-LINES (4)     TO SS50-3D.
004563     MOVE WS-LABEL-LINES (5)     TO SS50-4D.
004564
004565     EJECT
004566 7240-NOT-FOUND.
004567     IF ACCOUNT-IS-ONLINE
004568        GO TO 7250-READ-ACCOUNT.
004569
004570     IF W-ACCOUNT > ZERO
004571         COMPUTE ACTV-SEQ = W-ACCOUNT + 20
004572     ELSE
004573         MOVE CL-ACCOUNT-ADDR-CNT TO ACTV-SEQ
004574         ADD +20                  TO ACTV-SEQ.
004575
004576     IF ACTV-SEQ = +20
004577        GO TO 7250-READ-ACCOUNT.
004578
004579     
      * EXEC CICS HANDLE CONDITION
004580*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004581*         NOTFND     (7250-READ-ACCOUNT)
004582*    END-EXEC.
      *    MOVE '"$JI                  ! ; #00010262' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303130323632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004583
004584     
      * EXEC CICS READ
004585*         DATASET    (ACTV-ID)
004586*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004587*         RIDFLD     (ACTV-KEY)
004588*    END-EXEC.
      *    MOVE '&"S        E          (   #00010267' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130323637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004589
004590     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004591     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004592     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004593     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004594*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004595     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004596        INTO WS-LABEL-LINES (4)
004597     END-STRING
004598
004599     MOVE SPACES                 TO  WS-ZIP-CODE.
004600     IF AT-CANADIAN-POST-CODE
004601         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004602         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004603     ELSE
004604         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004605         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004606             MOVE '-'            TO  WS-AM-ZIP-DASH
004607             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004608
004609     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004610
004611     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004612     MOVE WSPI-AREA              TO WSPO-AREA.
004613     MOVE WSPI-PFX               TO WSPO-PFX.
004614     MOVE WSPI-SFX               TO WSPO-SFX.
004615     MOVE WS-PHONE-OUT           TO SS07-6D.
004616
004617*    IF PI-COMPANY-ID NOT = 'FLA'
004618*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004619
004620     MOVE WS-LABEL-LINES (1)     TO SS06D.
004621     MOVE WS-LABEL-LINES (2)     TO SS07-1D.
004622     MOVE WS-LABEL-LINES (3)     TO SS07-2D.
004623     MOVE WS-LABEL-LINES (4)     TO SS07-3D.
004624     MOVE WS-LABEL-LINES (5)     TO SS07-4D.
004625     MOVE WS-LABEL-LINES (6)     TO SS07-5D.
004626
004627     EJECT
004628 7250-READ-ACCOUNT.
004629*    IF CL-SYSTEM-IDENTIFIER = 'CV'
004630*        GO TO 7250-READ-PRODUCER.
004631
004632     IF WS-ACCT-READ-SW = 'Y'
004633        GO TO 7250-BUILD-ACCT-ADDR.
004634
004635     MOVE CM-CERT-EFF-DT TO ACCT-EXP-DATE.
004636
004637     
      * EXEC CICS HANDLE CONDITION
004638*         NOTOPEN   (8880-ACCT-NOT-OPEN)
004639*         NOTFND    (7251-READ-3RD-PARTY)
004640*    END-EXEC.
      *    MOVE '"$JI                  ! < #00010320' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303130333230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004641
004642 7250-STARTBR-ACCOUNT.
004643     MOVE ACCT-PARTIAL-KEY       TO  ACCT-SAVE-KEY.
004644     PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.
004645
004646 7250-READNEXT-ACCOUNT.
004647     PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.
004648
004649     IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
004650         IF WS-SAVE-ACCT-RECORD = SPACES
004651             GO TO 7251-READ-3RD-PARTY
004652         ELSE
004653             MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY
004654             MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER
004655             GO TO 7250-BUILD-ACCT-ADDR.
004656
004657     IF AM-EXPIRATION-DT = HIGH-VALUES
004658         MOVE AM-CONTROL-PRIMARY      TO  ACCT-KEY
004659     ELSE
004660         MOVE ACCOUNT-MASTER          TO  WS-SAVE-ACCT-RECORD
004661         GO TO 7250-READNEXT-ACCOUNT.
004662
004663 7250-BUILD-ACCT-ADDR.
004664     MOVE SPACES                TO  WS-SAVE-ACCT-RECORD.
004665
004666     IF NOT ACCOUNT-IS-ONLINE
004667        GO TO 7251-READ-3RD-PARTY.
004668
004669     MOVE SPACES                TO WS-LABEL-HOLD-AREA.
004670     MOVE AM-NAME               TO WS-LABEL-LINES (1).
004671     MOVE AM-PERSON             TO WS-LABEL-LINES (2).
004672     MOVE AM-ADDRS              TO WS-LABEL-LINES (3).
004673*    MOVE AM-CITY               TO WS-LABEL-LINES (4).
004674     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
004675        INTO WS-LABEL-LINES (4)
004676     END-STRING
004677
004678     MOVE SPACES                TO  WS-ZIP-CODE.
004679     IF AM-CANADIAN-POST-CODE
004680         MOVE AM-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
004681         MOVE AM-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
004682     ELSE
004683         MOVE AM-ZIP-PRIME      TO  WS-AM-ZIP-CODE
004684         IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004685             MOVE '-'           TO  WS-AM-ZIP-DASH
004686             MOVE AM-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
004687
004688     MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
004689
004690     MOVE ZEROS                 TO WS-PHONE-IN.
004691     MOVE AM-AREA-CODE          TO WSPO-AREA.
004692     MOVE AM-TEL-PRE            TO WSPO-PFX.
004693     MOVE AM-TEL-NBR            TO WSPO-SFX.
004694     MOVE WS-PHONE-OUT          TO SS07-6D.
004695
004696*    IF PI-COMPANY-ID NOT = 'FLA'
004697*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004698
004699     MOVE WS-LABEL-LINES (1)     TO SS06D.
004700     MOVE WS-LABEL-LINES (2)     TO SS07-1D.
004701     MOVE WS-LABEL-LINES (3)     TO SS07-2D.
004702     MOVE WS-LABEL-LINES (4)     TO SS07-3D.
004703     MOVE WS-LABEL-LINES (5)     TO SS07-4D.
004704     MOVE WS-LABEL-LINES (6)     TO SS07-5D.
004705
004706*    GO TO 7251-READ-3RD-PARTY.
004707
004708     EJECT
004709*7250-READ-PRODUCER. Remove as dead code
004710*7250-STARTBR-PRODUCER. Remove as dead code
004711*7250-READNEXT-PRODUCER. Remove as dead code
004712*7250-BUILD-PROD-ADDR. Remove as dead code
004713
004714 7251-READ-3RD-PARTY.
004715
004716     MOVE +29                    TO ACTV-SEQ.
004717
004718     
      * EXEC CICS HANDLE CONDITION
004719*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004720*         NOTFND     (7252-READ-COMP)
004721*    END-EXEC.
      *    MOVE '"$JI                  ! = #00010401' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303130343031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004722
004723     
      * EXEC CICS READ
004724*         DATASET    (ACTV-ID)
004725*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
004726*         RIDFLD     (ACTV-KEY)
004727*    END-EXEC.
      *    MOVE '&"S        E          (   #00010406' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130343036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004728
004729     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004730     MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
004731     MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
004732     MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
004733*    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
004734     STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
004735        INTO WS-LABEL-LINES (4)
004736     END-STRING
004737
004738     MOVE SPACES                 TO  WS-ZIP-CODE.
004739     IF AT-CANADIAN-POST-CODE
004740         MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
004741         MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
004742     ELSE
004743         MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
004744         IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004745             MOVE '-'            TO  WS-AM-ZIP-DASH
004746             MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
004747
004748     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
004749
004750     MOVE AT-PHONE-NO            TO WS-PHONE-IN.
004751     MOVE WSPI-AREA              TO WSPO-AREA.
004752     MOVE WSPI-PFX               TO WSPO-PFX.
004753     MOVE WSPI-SFX               TO WSPO-SFX.
004754     MOVE WS-PHONE-OUT           TO SS53-6D.
004755
004756     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004757
004758     MOVE WS-LABEL-LINES (1)     TO SS52D.
004759     MOVE WS-LABEL-LINES (2)     TO SS53-1D.
004760     MOVE WS-LABEL-LINES (3)     TO SS53-2D.
004761     MOVE WS-LABEL-LINES (4)     TO SS53-3D.
004762     MOVE WS-LABEL-LINES (5)     TO SS53-4D.
004763     MOVE WS-LABEL-LINES (6)     TO SS53-5D.
004764
004765     GO TO 7260-READ-DENIAL.
004766     EJECT
004767 7252-READ-COMP.
004768
004769     IF WS-COMP-READ-SW = 'Y'
004770        GO TO 7255-BUILD-COMP-ADDR.
004771
004772     IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
004773        MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL
004774        GO TO 7260-READ-DENIAL.
004775
004776     IF AM-3RD-PARTY-NOTIF-LEVEL > 00 AND < 11
004777        NEXT SENTENCE
004778     ELSE
004779        GO TO 7260-READ-DENIAL.
004780
004781     IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS
004782        GO TO 7260-READ-DENIAL.
004783
004784     MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.
004785     MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.
004786     MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.
004787     MOVE 'A'             TO WS-ERCOMP-TYPE.
004788     MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
004789                          TO WS-ERCOMP-RESP-NO.
004790
004791     IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
004792         IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
004793                                       'G' OR 'B' or 'S'
004794             MOVE 'G'            TO WS-ERCOMP-TYPE
004795             MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT
004796         ELSE
004797             MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
004798                                 TO WS-ERCOMP-ACCOUNT
004799     ELSE
004800         MOVE 'G'                TO WS-ERCOMP-TYPE
004801         MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
004802
004803     IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
004804        MOVE ZEROS TO WS-ERCOMP-CARRIER.
004805
004806     IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
004807        MOVE ZEROS TO WS-ERCOMP-GROUPING.
004808
004809     
      * EXEC CICS HANDLE CONDITION
004810*         NOTFND    (7260-READ-DENIAL)
004811*    END-EXEC.
      *    MOVE '"$I                   ! > #00010492' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3E20233030303130343932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004812
004813     
      * EXEC CICS  READ
004814*         SET      (ADDRESS OF COMPENSATION-MASTER)
004815*         DATASET  ('ERCOMP')
004816*         RIDFLD   (WS-ERCOMP-KEY)
004817*    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010496' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130343936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004818
004819 7255-BUILD-COMP-ADDR.
004820
004821     MOVE SPACES                TO WS-LABEL-HOLD-AREA.
004822     MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).
004823     IF CO-ACCT-NAME = SPACES
004824        MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).
004825     MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).
004826     MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).
004827     MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).
004828
004829     MOVE SPACES                TO  WS-ZIP-CODE.
004830     IF CO-CANADIAN-POST-CODE
004831         MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
004832         MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
004833     ELSE
004834         MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE
004835         IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004836             MOVE '-'           TO  WS-AM-ZIP-DASH
004837             MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
004838
004839     MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
004840
004841     MOVE ZEROS                 TO WS-PHONE-IN.
004842     MOVE CO-AREA-CODE          TO WSPO-AREA.
004843     MOVE CO-PREFIX             TO WSPO-PFX.
004844     MOVE CO-PHONE              TO WSPO-SFX.
004845     MOVE WS-PHONE-OUT          TO SS53-6D.
004846
004847     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004848
004849     MOVE WS-LABEL-LINES (1)     TO SS52D.
004850     MOVE WS-LABEL-LINES (2)     TO SS53-1D.
004851     MOVE WS-LABEL-LINES (3)     TO SS53-2D.
004852     MOVE WS-LABEL-LINES (4)     TO SS53-3D.
004853     MOVE WS-LABEL-LINES (5)     TO SS53-4D.
004854     MOVE WS-LABEL-LINES (6)     TO SS53-5D.
004855
004856     EJECT
004857
004858 7260-READ-DENIAL.
004859
004860     MOVE +93                    TO ACTV-SEQ
004861
004862     
      * EXEC CICS HANDLE CONDITION
004863*         NOTOPEN    (8870-ACTV-NOT-OPEN)
004864*         NOTFND     (7265-READ-CNTL1)
004865*         ENDFILE    (7265-READ-CNTL1)
004866*    END-EXEC.
      *    MOVE '"$JI''                 ! ? #00010545' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'3F20233030303130353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004867
004868     
      * EXEC CICS STARTBR
004869*         DATASET    (ACTV-ID)
004870*         RIDFLD     (ACTV-KEY)
004871*         GTEQ
004872*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010551' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303130353531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004873
004874     MOVE 'Y'                    TO ACTV-BROWSE-STARTED.
004875     MOVE ACTV-PARTIAL-KEY       TO ACTV-SAVE-KEY.
004876
004877 7262-READ-NEXT.
004878
004879     
      * EXEC CICS READNEXT
004880*         SET     (ADDRESS OF ACTIVITY-TRAILERS)
004881*         RIDFLD  (ACTV-KEY)
004882*         DATASET (ACTV-ID)
004883*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010562' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130353632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004884
004885     IF ACTV-PARTIAL-KEY  NOT = ACTV-SAVE-KEY
004886        GO TO 7265-READ-CNTL1.
004887
004888     IF AT-TRAILER-TYPE = '2'
004889        AND AT-PAYMENT-TYPE = 'I'
004890        AND AT-INT-RATE NUMERIC
004891        AND AT-INT-RATE NOT = ZEROS
004892        COMPUTE WS-WORK-INT-RATE = AT-INT-RATE * 100
004893        MOVE +1                  TO S2
004894        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
004895           (S1 > +6)
004896           IF WS-WORK-INT (S1:1) NOT = '0'
004897              MOVE WS-WORK-INT (S1:1)
004898                                 TO SS16-1D (S2:1)
004899              ADD +1             TO S2
004900           END-IF
004901        END-PERFORM
004902        MOVE '%'                 TO SS16-1D (S2:1)
004903        GO TO 7262-READ-NEXT
004904     END-IF
004905
004906     IF AT-TRAILER-TYPE = '8'
004907        MOVE AT-DENIAL-INFO-1    TO SS35-1D
004908        MOVE AT-DENIAL-INFO-2    TO SS35-2D
004909        IF (CL-TOTAL-INT-PAID = ZEROS)
004910           OR (CL-CLAIM-TYPE NOT = (PI-LIFE-OVERRIDE-L1 OR 'O'))
004911           GO TO 7265-READ-CNTL1
004912        END-IF
004913     END-IF
004914
004915     GO TO 7262-READ-NEXT
004916
004917     .
004918 7265-READ-CNTL1.
004919     IF ACTV-BROWSE-STARTED = 'Y'
004920        MOVE 'N'                  TO ACTV-BROWSE-STARTED
004921        
      * EXEC CICS ENDBR
004922*            DATASET    (ACTV-ID)
004923*       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010604' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130363034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004924
004925     IF SS35-1D = ALL '*'
004926         MOVE '@@DENIAL1'        TO SS35-1D.
004927
004928     IF SS35-2D = ALL '*'
004929         MOVE '@@DENIAL2'        TO SS35-2D.
004930
004931     MOVE '1'                    TO CNTL-RECORD-TYPE.
004932     MOVE ZEROS                  TO CNTL-SEQ.
004933     
      * EXEC CICS HANDLE CONDITION
004934*         NOTOPEN   (8840-CNTL-NOT-OPEN)
004935*         NOTFND    (7266-READ-CNTL2)
004936*    END-EXEC.
      *    MOVE '"$JI                  ! @ #00010616' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4020233030303130363136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004937
004938     
      * EXEC CICS READ
004939*         DATASET   (CNTL-ID)
004940*         SET       (ADDRESS OF CONTROL-FILE)
004941*         RIDFLD    (CNTL-KEY)
004942*    END-EXEC.
      *    MOVE '&"S        E          (   #00010621' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130363231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004943
004944     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
004945     MOVE CF-CL-MAIL-TO-NAME     TO WS-LABEL-LINES (1).
004946     MOVE CF-CL-IN-CARE-OF       TO WS-LABEL-LINES (2).
004947     MOVE CF-CL-ADDR-LINE-1      TO WS-LABEL-LINES (3).
004948     MOVE CF-CL-ADDR-LINE-2      TO WS-LABEL-LINES (4).
004949     MOVE CF-CL-CITY-STATE       TO WS-LABEL-LINES (5).
004950
004951     IF CF-CL-ZIP-CODE-NUM NOT NUMERIC
004952         MOVE ZEROS              TO  CF-CL-ZIP-CODE-NUM.
004953     IF CF-CL-ZIP-CODE-NUM NOT = ZEROS
004954         MOVE CF-CL-ZIP-CODE-NUM TO  WS-ZIP-NUMERIC
004955         MOVE WS-ZIP-NONNUM      TO  CF-CL-ZIP-CODE.
004956
004957     MOVE SPACES                 TO  WS-ZIP-CODE.
004958     IF CF-CL-CAN-POST-CODE
004959         MOVE CF-CL-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
004960         MOVE CF-CL-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
004961     ELSE
004962         MOVE CF-CL-ZIP-PRIME      TO  WS-AM-ZIP-CODE
004963         IF CF-CL-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
004964             MOVE '-'              TO  WS-AM-ZIP-DASH
004965             MOVE CF-CL-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
004966
004967     MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (6).
004968
004969     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
004970
004971     MOVE WS-LABEL-LINES (1)     TO SS01D.
004972     MOVE WS-LABEL-LINES (2)     TO SS02-1D.
004973     MOVE WS-LABEL-LINES (3)     TO SS02-2D.
004974     MOVE WS-LABEL-LINES (4)     TO SS02-3D.
004975     MOVE WS-LABEL-LINES (5)     TO SS02-4D.
004976     MOVE WS-LABEL-LINES (6)     TO SS02-5D.
004977
004978 7266-READ-CNTL2.
004979     IF PI-PROCESSOR-ID  =  LGXX-ID
004980        GO TO 7267-READ-CNTL4.
004981
004982     MOVE '2'                    TO CNTL-RECORD-TYPE.
004983     MOVE PI-PROCESSOR-ID        TO CNTL-GENL.
004984     MOVE ZEROS                  TO CNTL-SEQ.
004985
004986     
      * EXEC CICS HANDLE CONDITION
004987*         NOTOPEN    (8840-CNTL-NOT-OPEN)
004988*         NOTFND     (7267-READ-CNTL4)
004989*    END-EXEC.
      *    MOVE '"$JI                  ! A #00010669' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4120233030303130363639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004990
004991     
      * EXEC CICS READ
004992*         DATASET    (CNTL-ID)
004993*         SET        (ADDRESS OF CONTROL-FILE)
004994*         RIDFLD     (CNTL-KEY)
004995*    END-EXEC.
      *    MOVE '&"S        E          (   #00010674' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130363734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004996
004997     MOVE CF-PROCESSOR-NAME      TO SS08D.
004998     MOVE CF-PROCESSOR-TITLE     TO SS09D.
004999
005000     EJECT
005001 7267-READ-CNTL4.
005002*    IF CL-SYSTEM-IDENTIFIER = 'CV'
005003*        GO TO 7267-READ-EMPLAN.
005004
005005     IF BEN-HOLD = ZEROS
005006        GO TO 7267-READ-CNTL6.
005007
005008     MOVE BEN-HOLD               TO CNTL-GEN2.
005009     MOVE SPACES                 TO CNTL-GEN1.
005010
005011     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
005012        MOVE '4'                 TO CNTL-RECORD-TYPE
005013     ELSE
005014        MOVE '5'                 TO CNTL-RECORD-TYPE.
005015
005016     MOVE ZEROS                  TO CNTL-SEQ.
005017
005018     
      * EXEC CICS HANDLE CONDITION
005019*         NOTOPEN    (8840-CNTL-NOT-OPEN)
005020*         NOTFND     (7267-READ-CNTL6)
005021*    END-EXEC.
      *    MOVE '"$JI                  ! B #00010701' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4220233030303130373031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005022
005023     
      * EXEC CICS READ
005024*         DATASET    (CNTL-ID)
005025*         SET        (ADDRESS OF CONTROL-FILE)
005026*         RIDFLD     (CNTL-KEY)
005027*         GTEQ
005028*    END-EXEC.
      *    MOVE '&"S        G          (   #00010706' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303130373036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005029
005030     MOVE 1                      TO SUB.
005031 7267-LOOP.
005032     IF SUB = 9
005033        GO TO 7267-READ-CNTL6.
005034
005035     IF CF-BENEFIT-CODE (SUB) < BEN-HOLD
005036        ADD 1 TO SUB
005037        GO TO 7267-LOOP.
005038
005039     IF BEN-HOLD = CF-BENEFIT-CODE (SUB)
005040        MOVE CF-BENEFIT-DESCRIP (SUB) TO SS22D
005041        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
005042           OR CL-CLAIM-TYPE = 'I'
005043           OR CL-CLAIM-TYPE = 'G'
005044           OR CL-CLAIM-TYPE = 'F'
005045           OR CL-CLAIM-TYPE = 'B'
005046           OR CL-CLAIM-TYPE = 'H'
005047           MOVE CF-BENEFIT-ALPHA (SUB)   TO BENEFIT-WORK
005048           MOVE ELIM-DAYS                TO SS42D.
005049
005050     GO TO 7267-READ-CNTL6.
005051
005052     EJECT
005053*7267-READ-EMPLAN. Remove as dead code
005054
005055 7267-READ-CNTL6.
005056     MOVE '6'                    TO CNTL-RECORD-TYPE.
005057     MOVE SPACES                 TO CNTL-GENL.
005058
005059*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
005060*        MOVE CL-CURRENT-CARRIER TO CNTL-GEN4
005061*    ELSE
005062         MOVE PI-CARRIER         TO CNTL-GEN4.
005063
005064     MOVE ZEROS                  TO CNTL-SEQ.
005065
005066     
      * EXEC CICS HANDLE CONDITION
005067*         NOTOPEN    (8840-CNTL-NOT-OPEN)
005068*         NOTFND     (7268-SET-DATE)
005069*    END-EXEC.
      *    MOVE '"$JI                  ! C #00010749' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4320233030303130373439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005070
005071     
      * EXEC CICS READ
005072*         DATASET    (CNTL-ID)
005073*         SET        (ADDRESS OF CONTROL-FILE)
005074*         RIDFLD     (CNTL-KEY)
005075*    END-EXEC.
      *    MOVE '&"S        E          (   #00010754' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130373534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005076
005077     MOVE SPACES              TO WS-LABEL-HOLD-AREA.
005078     MOVE CF-MAIL-TO-NAME     TO WS-LABEL-LINES (1).
005079     MOVE CF-IN-CARE-OF       TO WS-LABEL-LINES (2).
005080     MOVE CF-ADDRESS-LINE-1   TO WS-LABEL-LINES (3).
005081     MOVE CF-ADDRESS-LINE-2   TO WS-LABEL-LINES (4).
005082     MOVE CF-CITY-STATE       TO WS-LABEL-LINES (5).
005083
005084     IF CF-ZIP-CODE-NUM NOT NUMERIC
005085         MOVE ZEROS              TO  CF-ZIP-CODE-NUM.
005086     IF CF-ZIP-CODE-NUM NOT = ZEROS
005087         MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-NUMERIC
005088         MOVE WS-ZIP-NONNUM      TO  CF-ZIP-CODE.
005089
005090     MOVE SPACES              TO  WS-ZIP-CODE.
005091     IF CF-CANADIAN-POST-CODE
005092         MOVE CF-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1
005093         MOVE CF-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2
005094     ELSE
005095         MOVE CF-ZIP-PRIME     TO  WS-AM-ZIP-CODE
005096         IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
005097             MOVE '-'          TO  WS-AM-ZIP-DASH
005098             MOVE CF-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.
005099
005100     MOVE WS-ZIP-CODE         TO  WS-LABEL-LINES (6).
005101
005102     MOVE CF-PHONE-NO         TO WS-PHONE-IN.
005103     MOVE WSPI-AREA           TO WSPO-AREA.
005104     MOVE WSPI-PFX            TO WSPO-PFX.
005105     MOVE WSPI-SFX            TO WSPO-SFX.
005106     MOVE WS-PHONE-OUT        TO SS04-6D.
005107
005108     PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
005109
005110     MOVE WS-LABEL-LINES (1)  TO SS03D
005111     IF PI-CARRIER = '8'
005112        MOVE 'as Administrator for Investors Heritage Life Insuran
005113-     'ce Company'               TO SS03-1D
005114     END-IF
005115     MOVE WS-LABEL-LINES (2)  TO SS04-1D.
005116     MOVE WS-LABEL-LINES (3)  TO SS04-2D.
005117     MOVE WS-LABEL-LINES (4)  TO SS04-3D.
005118     MOVE WS-LABEL-LINES (5)  TO SS04-4D.
005119     MOVE WS-LABEL-LINES (6)  TO SS04-5D.
005120
005121     EJECT
005122 7268-SET-DATE.
005123     MOVE EIBDATE                TO DATE-WORK.
005124     MOVE DT-WORK                TO DC-JULIAN-YYDDD.
005125     MOVE '5'                    TO DC-OPTION-CODE.
005126     PERFORM 9700-DATE-LINK  THRU  9700-EXIT.
005127     MOVE DC-GREG-DATE-1-EDIT    TO SS20D.
005128     MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.
005129
005130     IF NOT LOWER-CASE-LETTERS-USED
005131        GO TO 7269-EXIT.
005132
005133*    IF PI-COMPANY-ID = 'DMD'
005134*        MOVE SS21D              TO WS-TEMP-AREA2
005135*        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
005136*        IF WS-TEMP-AREA2 (1:1) = SPACE
005137*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
005138*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
005139*        END-IF
005140*        IF WS-TEMP-AREA2 (1:1) = SPACE
005141*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
005142*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
005143*        END-IF
005144*        IF WS-TEMP-AREA2 (1:1) = SPACE
005145*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
005146*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
005147*        END-IF
005148*        IF WS-TEMP-AREA2 (1:1) = SPACE
005149*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
005150*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
005151*        END-IF
005152*        IF WS-TEMP-AREA2 (1:1) = SPACE
005153*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
005154*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
005155*        END-IF
005156*        MOVE WS-TEMP-AREA2      TO SS21D
005157*        GO TO 7269-EXIT.
005158*
005159     INSPECT SS19D   CONVERTING UPPER-CASE TO LOWER-CASE.
005160     INSPECT SS35-1D CONVERTING UPPER-CASE TO LOWER-CASE.
005161     INSPECT SS35-2D CONVERTING UPPER-CASE TO LOWER-CASE.
005162     INSPECT SS22D   CONVERTING UPPER-CASE TO LOWER-CASE.
005163
005164     MOVE 'N'                    TO WS-STATE-LINE.
005165
005166     MOVE 'Y'                    TO WS-PROCESSOR-LINE.
005167     MOVE SS08D                  TO WS-TEMP-AREA2.
005168     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005169     MOVE WS-TEMP-AREA2          TO SS08D.
005170     MOVE 'N'                    TO WS-PROCESSOR-LINE.
005171     MOVE 'N'                    TO WS-CAPS-SW.
005172
005173     MOVE SS09D                  TO WS-TEMP-AREA2.
005174     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005175     MOVE WS-TEMP-AREA2          TO SS09D.
005176
005177     MOVE SS11-5D                TO WS-TEMP-AREA2.
005178     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005179     MOVE WS-TEMP-AREA2          TO SS11-5D.
005180
005181     MOVE SS21D                  TO WS-TEMP-AREA2.
005182     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005183     MOVE WS-TEMP-AREA2          TO SS21D.
005184
005185     MOVE SS40D                  TO WS-TEMP-AREA2.
005186     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005187     MOVE WS-TEMP-AREA2          TO SS40D.
005188
005189     MOVE SS41D                  TO WS-TEMP-AREA2.
005190     PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
005191     MOVE WS-TEMP-AREA2          TO SS41D.
005192
005193 7269-EXIT.
005194     EXIT.
005195
005196     EJECT
005197*            COPY ELCNAMET.
      *>>((file: ELCNAMET))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCNAMET                           *
000005*                            VMOD=2.003                         *
000006*                                                               *
000007*            TRANSLATION ROUTINE USED MAINLY FOR CONVERTING     *
000008*            NAMES AND ADDRESSES AND OTHER VARIABLES TO         *
000009*            LOWER CASE.  THE FIRST CHARACTER OF EACH WORD      *
000010*            IS LEFT AS UPPER AND ALL OTHER LOWER CASE.         *
000011*                                                               *
000012*                                                               *
000013*****************************************************************.
000014*                   C H A N G E   L O G
000015*
000016* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000017*-----------------------------------------------------------------
000018*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000019* EFFECTIVE    NUMBER
000020*-----------------------------------------------------------------
000021* 102703                   SMVA  LEAVE CERTIFICATION DESIGNATION O
000022*                                PROCESSOR NAME LINE IN CAPS
000023******************************************************************
000024
000025 7270-LABEL-MOVE.
000026     IF WS-LABEL-HOLD-AREA = SPACES
000027        GO TO 7279-EXIT.
000028
000029     IF PI-COMPANY-ID NOT = 'DMD'
000030         PERFORM 7280-TRANSLATE-LOWER THRU 7280-EXIT.
000031
000032 7270-LABEL-MOVE-1.
000033
000034     IF WS-LABEL-LINES (1) = SPACES
000035        MOVE WS-LABEL-LINES (2)  TO WS-LABEL-LINES (1)
000036        MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
000037        MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
000038        MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
000039        MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
000040        MOVE SPACES              TO WS-LABEL-LINES (6)
000041        GO TO 7270-LABEL-MOVE-1.
000042
000043     IF WS-LABEL-LINES (2) = SPACES AND
000044        WS-LABEL-LINES (3) = SPACES AND
000045        WS-LABEL-LINES (4) = SPACES AND
000046        WS-LABEL-LINES (5) = SPACES AND
000047        WS-LABEL-LINES (6) = SPACES
000048        SET WS-NDX               TO 1
000049        GO TO 7275-MOVE-ZIP.
000050
000051 7270-LABEL-MOVE-2.
000052
000053     IF WS-LABEL-LINES (2) = SPACES
000054        MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
000055        MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
000056        MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
000057        MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
000058        MOVE SPACES              TO WS-LABEL-LINES (6)
000059        GO TO 7270-LABEL-MOVE-2.
000060
000061     IF WS-LABEL-LINES (3) = SPACES AND
000062        WS-LABEL-LINES (4) = SPACES AND
000063        WS-LABEL-LINES (5) = SPACES AND
000064        WS-LABEL-LINES (6) = SPACES
000065        SET WS-NDX               TO 2
000066        GO TO 7275-MOVE-ZIP.
000067
000068 7270-LABEL-MOVE-3.
000069
000070     IF WS-LABEL-LINES (3) = SPACES
000071        MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
000072        MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
000073        MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
000074        MOVE SPACES              TO WS-LABEL-LINES (6)
000075        GO TO 7270-LABEL-MOVE-3.
000076
000077     IF WS-LABEL-LINES (4) = SPACES AND
000078        WS-LABEL-LINES (5) = SPACES AND
000079        WS-LABEL-LINES (6) = SPACES
000080        SET WS-NDX               TO 3
000081        GO TO 7275-MOVE-ZIP.
000082
000083 7270-LABEL-MOVE-4.
000084
000085     IF WS-LABEL-LINES (4) = SPACES
000086        MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
000087        MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
000088        MOVE SPACES              TO WS-LABEL-LINES (6)
000089        GO TO 7270-LABEL-MOVE-4.
000090
000091     IF WS-LABEL-LINES (5) = SPACES AND
000092        WS-LABEL-LINES (6) = SPACES
000093        SET WS-NDX               TO 4
000094        GO TO 7275-MOVE-ZIP.
000095
000096 7270-LABEL-MOVE-5.
000097
000098     IF WS-LABEL-LINES (5) = SPACES
000099        MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
000100        MOVE SPACES              TO WS-LABEL-LINES (6)
000101        SET WS-NDX               TO 5
000102        GO TO 7275-MOVE-ZIP
000103
000104     ELSE
000105        IF WS-LABEL-LINES (6) = SPACES
000106           SET WS-NDX            TO 5
000107           GO TO 7275-MOVE-ZIP
000108
000109        ELSE
000110           SET WS-NDX            TO 6.
000111
000112 7275-MOVE-ZIP.
000113
000114     SET WS-NDX2                 TO WS-NDX.
000115     SET WS-NDX2 DOWN BY +1.
000116
000117     IF  WS-LAST-ZIP (WS-NDX2) = SPACES
000118*****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
000119         IF WS-LABEL-1ST-ZIP (WS-NDX) NUMERIC
000120          IF PI-COMPANY-ID NOT = 'FLA'
000121            MOVE WS-LABEL-ZIP (WS-NDX)
000122                                     TO WS-LAST-ZIP (WS-NDX2)
000123            MOVE SPACES              TO WS-LABEL-LINES (WS-NDX).
000124
000125 7279-EXIT.
000126     EXIT.
000127     EJECT
000128
000129 7280-TRANSLATE-LOWER.
000130     IF LOWER-CASE-LETTERS-USED
000131        MOVE 'N'                     TO WS-STATE-LINE
000132     ELSE
000133        GO TO 7280-EXIT.
000134
000135     IF WS-LABEL-LINES (1) NOT = SPACES
000136        MOVE WS-LABEL-LINES (1)      TO WS-TEMP-AREA2
000137        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000138        MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (1).
000139
000140     IF WS-LABEL-LINES (2) NOT = SPACES
000141        MOVE WS-LABEL-LINES (2)      TO WS-TEMP-AREA2
000142        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000143        MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (2).
000144
000145     IF WS-LABEL-LINES (3) NOT = SPACES
000146        MOVE WS-LABEL-LINES (3)      TO WS-TEMP-AREA2
000147        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000148        MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (3).
000149
000150*****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING
000151*****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.
000152
000153
000154     IF WS-LABEL-LINES (4) NOT = SPACES
000155        IF (WS-LABEL-LINES (6) = SPACES  OR
000156           WS-LABEL-LINES (5) = SPACES)
000157           MOVE 'Y'                     TO WS-STATE-LINE
000158           MOVE WS-LABEL-LINES (4)      TO WS-TEMP-AREA2
000159           PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000160           MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (4)
000161        ELSE
000162           MOVE WS-LABEL-LINES (4)      TO WS-TEMP-AREA2
000163           PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000164           MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (4).
000165
000166     IF WS-LABEL-LINES (5) NOT = SPACES
000167        IF WS-LABEL-LINES (6) NOT = SPACES
000168           MOVE 'Y'                     TO WS-STATE-LINE
000169           MOVE WS-LABEL-LINES (5)      TO WS-TEMP-AREA2
000170           PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
000171           MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (5).
000172
000173 7280-EXIT.
000174     EXIT.
000175     EJECT
000176
000177 7281-SEARCH-AND-TRANSLATE.
000178     SET TA1  TO +1.
000179
000180 7281-FIND-FIRST-NON-BLANK.
000181     IF WS-TEMP-2 (TA1) = SPACES
000182        SET TA1 UP BY 1
000183        GO TO 7281-FIND-FIRST-NON-BLANK.
000184
000185*****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.
000186     SET TA2   TO TA1
000187     SET TA2   UP BY 1
000188     SET TA21  TO TA2
000189     SET TA21  UP BY 1.
000190
000191     MOVE 'N'                   TO WS-DATA-FOUND-SW.
000192     PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.
000193
000194 7281-EXIT.
000195     EXIT.
000196     EJECT
000197
000198 7282-FIND-NEXT-BLANK.
000199     IF TA21 GREATER THAN 31
000200        GO TO 7282-EXIT.
000201
000202     IF TA21 EQUAL TO 31
000203        IF NO-CHARACTERS-FOUND
000204           GO TO 7282-EXIT
000205           ELSE
000206           SET TA1   TO +1
000207           MOVE SPACES             TO WS-TEMP-AREA1
000208           PERFORM 7283-MOVE VARYING MOVE-INDX FROM TA2 BY 1
000209                   UNTIL MOVE-INDX EQUAL TO TA21
000210           PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT
000211           SET TA1   TO +1
000212           PERFORM 7284-MOVE-BACK VARYING MOVE-INDX FROM TA2 BY 1
000213                   UNTIL MOVE-INDX EQUAL TO TA21
000214           GO TO 7282-EXIT.
000215
000216     IF WS-TEMP-2 (TA2)  = SPACE OR ',' OR '/'
000217         GO TO 7282-NEXT-GROUP-SEARCH.
000218
000219     IF WS-TEMP-2 (TA21) = SPACES OR ',' OR '/'
000220         IF WS-TEMP-2 (TA21)  = ','
000221            AND WS-PROCESSOR-LINE = 'Y'
000222             SET THE-REST-R-CAPS TO TRUE
000223         ELSE
000224             CONTINUE
000225     ELSE
000226         MOVE 'Y'                  TO WS-DATA-FOUND-SW
000227         SET TA21 UP BY +1
000228         GO TO 7282-FIND-NEXT-BLANK.
000229
000230     SET TA1  TO +1
000231     MOVE SPACES                   TO WS-TEMP-AREA1.
000232     PERFORM 7283-MOVE VARYING MOVE-INDX FROM TA2 BY 1
000233             UNTIL MOVE-INDX EQUAL TO TA21
000234     PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.
000235     SET TA1  TO +1
000236     PERFORM 7284-MOVE-BACK VARYING MOVE-INDX FROM TA2 BY 1
000237             UNTIL MOVE-INDX EQUAL TO TA21
000238
000239     IF THE-REST-R-CAPS
000240         GO TO 7282-EXIT.
000241
000242     SET TA2   TO TA21
000243     SET TA21  UP BY 1
000244     MOVE 'N'                   TO WS-DATA-FOUND-SW.
000245 7282-NEXT-GROUP-SEARCH.
000246     IF WS-TEMP-2 (TA2) EQUAL TO SPACES OR ',' OR '/'
000247        SET TA2   UP BY 1
000248        IF TA2 = 30
000249           GO TO 7282-EXIT
000250           ELSE
000251           GO TO 7282-NEXT-GROUP-SEARCH.
000252
000253     SET TA2    UP BY 1
000254     SET TA21   TO TA2
000255     SET TA21   UP BY 1
000256     GO TO 7282-FIND-NEXT-BLANK.
000257
000258 7282-EXIT.
000259     EXIT.
000260
000261
000262 7283-MOVE.
000263     MOVE WS-TEMP-2 (MOVE-INDX) TO WS-TEMP-1 (TA1)
000264     SET TA1 UP BY +1.
000265
000266
000267 7284-MOVE-BACK.
000268     MOVE WS-TEMP-1 (TA1)       TO WS-TEMP-2 (MOVE-INDX).
000269     SET TA1 UP BY +1.
000270
000271 7285-TEST-AND-TRANSLATE.
000272***BYPASS IF THE AREA MAY BE A PO BOX, OR RR NUMBER
000273     IF WS-TEMP-AREA1 = '.O.' OR 'RR' OR 'O'
000274        GO TO 7285-EXIT.
000275
000276***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07
000277***AND IT APPEARS THAT IT MAY BE A ABREVIATION.
000278
000279     SET WS-POSITION2   TO TA2
000280     SET WS-POSITION21  TO TA21
000281     COMPUTE WS-WORD-LENGTH = WS-POSITION21 - WS-POSITION2
000282     IF WS-WORD-LENGTH LESS THAN 3 AND
000283        WS-STATE-LINE = 'Y' AND
000284        TA2 GREATER THAN 07
000285        GO TO 7285-EXIT.
000286
000287     INSPECT WS-TEMP-AREA1 CONVERTING UPPER-CASE TO LOWER-CASE.
000288
000289 7285-EXIT.
000290     EXIT.
000291
      *<<((file: ELCNAMET))
005198                                 EJECT
005199 7290-RESOLVE-CREDITOR.
005200
005201     
      * EXEC CICS HANDLE CONDITION
005202*         NOTOPEN    (7290-BENE-NOT-OPEN)
005203*         NOTFND     (7290-EXIT)
005204*    END-EXEC.
      *    MOVE '"$JI                  ! D #00011177' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4420233030303131313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005205
005206     MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
005207
005208     MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
005209     MOVE 'B'                    TO BENE-REC-TYPE.
005210     MOVE SPACES                 TO BENE-NUMBER.
005211     MOVE CL-CURRENT-GROUPING    TO BENE-CREDITOR.
005212
005213     
      * EXEC CICS READ
005214*         DATASET    (BENE-ID)
005215*         SET        (ADDRESS OF BENEFICIARY-MASTER)
005216*         RIDFLD     (BENE-KEY)
005217*    END-EXEC.
      *    MOVE '&"S        E          (   #00011189' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303131313839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005218
005219     IF LOWER-CASE-LETTERS-USED
005220         MOVE BE-MAIL-TO-NAME    TO WS-TEMP-AREA2
005221         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
005222         MOVE WS-TEMP-AREA2      TO SS56D
005223     ELSE
005224         MOVE BE-MAIL-TO-NAME    TO SS56D.
005225
005226     GO TO 7290-EXIT.
005227
005228 7290-BENE-NOT-OPEN.
005229
005230     MOVE ER-7675                TO EMI-ERROR.
005231     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005232     MOVE -1                     TO MAINTL.
005233     GO TO 8200-SEND-DATAONLY.
005234
005235 7290-EXIT.
005236     EXIT.
005237
005238     EJECT
005239 7300-VARIABLE-SEARCH.
005240***************************************************************
005241*    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *
005242*    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
005243*    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
005244*    DATA THAT WAS GENERATED IN PARAGRAPHS 7200-7299.         *
005245*                                                             *
005246*    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *
005247*    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *
005248*    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *
005249***************************************************************
005250
005251     MOVE REC-TEXT (TB-INDX)     TO SINGLE-LINE.
005252
005253     SET INDX1 TO 1.
005254 7301-LOOP.
005255     IF INDX1 > 70
005256        MOVE SINGLE-LINE         TO REC-TEXT (TB-INDX)
005257        GO TO 7399-EXIT.
005258
005259     IF ONE-CHAR (INDX1) NOT = '@'
005260        SET INDX1 UP BY 1
005261        GO TO 7301-LOOP.
005262
005263     SET INDX2 TO INDX1.
005264     SET INDX2 UP BY 1.
005265
005266     IF ONE-CHAR (INDX2) = '@'
005267        SET INDX1 UP BY 2
005268        GO TO 7301-LOOP.
005269
005270     MOVE ONE-CHAR (INDX2)       TO V1.
005271     SET INDX2 UP BY 1.
005272     MOVE ONE-CHAR (INDX2)       TO V2.
005273     SET INDX2 UP BY 1.
005274     MOVE ONE-CHAR (INDX2)       TO V3.
005275     SET INDX2 UP BY 1.
005276     MOVE ONE-CHAR (INDX2)       TO V4.
005277
005278     IF V-NUM NOT NUMERIC
005279        GO TO 7330-VAR-ERROR.
005280
005281     IF V-PERIOD NOT = '.'
005282        MOVE '.'                 TO V-PERIOD
005283        MOVE ZERO                TO V-DECIMAL
005284        GO TO 7340-TABLE-SEARCH.
005285
005286     IF V-DECIMAL NUMERIC
005287        GO TO 7340-TABLE-SEARCH.
005288
005289 7330-VAR-ERROR.
005290     MOVE ER-0180                TO EMI-ERROR.
005291     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005292     SET INDX1 UP BY 1.
005293     GO TO 7301-LOOP.
005294
005295 7340-TABLE-SEARCH.
005296
005297     IF REFRESH-GETMAIN-AREA
005298*        MOVE SAVE-VARIABLE-POINTER      TO LCP-WS-ADDR-COMP
005299*        SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
005300         MOVE SYSTEM-SUPPORTED-VARIABLES TO SYSTEM-VARIABLES
005301         MOVE 2                          TO GETMAIN-SWITCH
005302         MOVE 1                          TO SS-COUNTER
005303                                            B1
005304     ELSE
005305         IF NO-GETMAIN-DONE-YET
005306*            EXEC CICS GETMAIN
005307*                 SET       (ADDRESS OF SYSTEM-VARIABLES)
005308*                 LENGTH    (SS-WORK-AREA-LENGTH)
005309*            END-EXEC
005310             MOVE 2              TO GETMAIN-SWITCH
005311*            SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
005312*            MOVE LCP-WS-ADDR-COMP
005313*                                TO SAVE-VARIABLE-POINTER
005314             MOVE SYSTEM-SUPPORTED-VARIABLES
005315                                 TO SYSTEM-VARIABLES
005316             MOVE 1              TO SS-COUNTER
005317                                    B1
005318         ELSE
005319             MOVE 1              TO SS-COUNTER
005320                                    B1
005321*            MOVE SAVE-VARIABLE-POINTER
005322*                                TO LCP-WS-ADDR-COMP
005323*            SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
005324         END-IF
005325     END-IF
005326     .
005327 7350-TABLE-LOOP.
005328
005329     IF SS-COUNTER  >  SS-NUM-ENTRIES
005330        GO TO 7330-VAR-ERROR.
005331
005332     MOVE SYSTEM-VARIABLES (B1:106)
005333                                 TO SYS-VAR-ENTRY
005334     IF SYS-VAR-CODE NOT = VAR-HOLD
005335        ADD SYS-VAR-LEN          TO B1
005336*       SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
005337*       ADD SYS-VAR-LEN          TO LCP-WS-ADDR-COMP
005338*       SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
005339        ADD 1                    TO SS-COUNTER
005340        GO TO 7350-TABLE-LOOP
005341     END-IF
005342
005343     MOVE SYS-VAR-ENTRY          TO VARIABLE-WORK-AREA.
005344     SET INDXV TO 1.
005345     SUBTRACT 6                  FROM VAR-LEN.
005346     PERFORM 7400-MOVE-VAR-DATA VAR-LEN TIMES.
005347     GO TO 7301-LOOP.
005348
005349 7399-EXIT.
005350      EXIT.
005351
005352 7400-MOVE-VAR-DATA.
005353     IF INDX1 > 70
005354        MOVE ER-0181             TO EMI-ERROR
005355        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005356        GO TO 7301-LOOP.
005357
005358     MOVE VAR-ONE-CHAR (INDXV)   TO ONE-CHAR (INDX1).
005359     SET INDXV UP BY 1.
005360     SET INDX1 UP BY 1.
005361
005362     EJECT
005363 7500-READ-TS.
005364     
      * EXEC CICS HANDLE CONDITION
005365*         QIDERR     (7590-TS-QIDERR)
005366*         ITEMERR    (7585-TS-ITEMERR)
005367*    END-EXEC.
      *    MOVE '"$N<                  ! E #00011340' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'4520233030303131333430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005368
005369     SET TS-INDX TO 1.
005370     MOVE 1                      TO TS-ITEM.
005371
005372 7501-LOOP.
005373     
      * EXEC CICS READQ TS
005374*         INTO     (TS-WORK-AREA)
005375*         QUEUE    (TS-NAME-TEXT)
005376*         LENGTH   (TS-LENGTH)
005377*         ITEM     (TS-ITEM)
005378*    END-EXEC.
      *    MOVE '*$II   L              ''   #00011349' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303131333439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005379
005380     MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).
005381     SET TS-INDX UP BY 1.
005382     ADD 1                       TO TS-ITEM.
005383     GO TO 7501-LOOP.
005384
005385 7585-TS-ITEMERR.
005386     IF EIBTRNID NOT = TRANS-ID
005387        SUBTRACT 1               FROM TS-ITEM
005388        MOVE TS-ITEM             TO PI-TEMP-STOR-ITEMS.
005389
005390     GO TO 7599-EXIT.
005391
005392 7590-TS-QIDERR.
005393     IF EIBTRNID = TRANS-ID
005394        MOVE ER-0033             TO EMI-ERROR
005395        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005396        GO TO 8200-SEND-DATAONLY.
005397
005398 7599-EXIT.
005399      EXIT.
005400
005401     EJECT
005402 7600-READ-SCREEN-TS.
005403     
      * EXEC CICS HANDLE CONDITION
005404*         QIDERR     (7690-TS-QIDERR)
005405*         ITEMERR    (7685-TS-ITEMERR)
005406*    END-EXEC.
      *    MOVE '"$N<                  ! F #00011379' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'4620233030303131333739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005407
005408     MOVE 1                      TO TS-ITEM.
005409
005410     
      * EXEC CICS READQ TS
005411*         INTO    (EL152AO)
005412*         QUEUE   (TS-NAME-SCREEN)
005413*         LENGTH  (TS-MAP-LENGTH)
005414*         ITEM    (TS-ITEM)
005415*    END-EXEC.
      *    MOVE '*$II   L              ''   #00011386' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303131333836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 EL152AO, 
                 TS-MAP-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005416
005417     IF MAINTL NOT = ZEROS
005418        MOVE AL-UANON            TO MAINTA.
005419     IF ARCHNUML NOT = ZEROS
005420        MOVE AL-UNNON            TO ARCHNUMA.
005421     IF FORML NOT = ZEROS
005422        MOVE AL-UANON            TO FORMA.
005423     IF FOLLOWL NOT = ZEROS
005424        MOVE AL-UANON            TO FOLLOWA.
005425     IF RESENDL NOT = ZEROS
005426        MOVE AL-UANON            TO RESENDA.
005427     IF PRINTL NOT = ZEROS
005428        MOVE AL-UANON            TO PRINTA.
005429     IF COPIESL NOT = ZEROS
005430        MOVE AL-UNNON            TO COPIESA.
005431     IF ADDRL NOT = ZEROS
005432        MOVE AL-UANON            TO ADDRA.
005433     IF REL NOT = ZEROS
005434        MOVE AL-UANON            TO REA.
005435
005436     GO TO 7699-EXIT.
005437
005438 7685-TS-ITEMERR.
005439     GO TO 7699-EXIT.
005440
005441 7690-TS-QIDERR.
005442     IF EIBTRNID = TRANS-ID
005443        MOVE ER-0033             TO EMI-ERROR
005444        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005445        GO TO 8200-SEND-DATAONLY.
005446
005447 7699-EXIT.
005448      EXIT.
005449     EJECT
005450
005451 7700-PUT-TEMP-STOR.
005452     PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
005453     PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT.
005454     SET TS-INDX TO 1.
005455     MOVE 0                      TO PI-TEMP-STOR-ITEMS.
005456     PERFORM 7780-WRITE-TS THRU 7780-EXIT
005457             VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
005458             UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES.
005459
005460 7749-EXIT.
005461      EXIT.
005462
005463 7750-DELETE-TEMP-STOR.
005464     
      * EXEC CICS HANDLE CONDITION
005465*         QIDERR      (7750-EXIT)
005466*    END-EXEC.
      *    MOVE '"$N                   ! G #00011440' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'4720233030303131343430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005467
005468     
      * EXEC CICS DELETEQ TS
005469*         QUEUE       (TS-NAME-TEXT)
005470*    END-EXEC.
      *    MOVE '*&                    #   #00011444' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131343434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005471
005472 7750-EXIT.
005473     EXIT.
005474
005475 7760-DELETE-TEMP-STOR-SCREEN.
005476     
      * EXEC CICS HANDLE CONDITION
005477*         QIDERR     (7760-EXIT)
005478*    END-EXEC.
      *    MOVE '"$N                   ! H #00011452' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'4820233030303131343532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005479
005480     
      * EXEC CICS DELETEQ TS
005481*         QUEUE      (TS-NAME-SCREEN)
005482*    END-EXEC.
      *    MOVE '*&                    #   #00011456' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131343536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005483
005484 7760-EXIT.
005485     EXIT.
005486
005487 7770-DELETE-TEMP-STOR-PI-AREA.
005488     
      * EXEC CICS HANDLE CONDITION
005489*        QIDERR   (7770-EXIT)
005490*    END-EXEC.
      *    MOVE '"$N                   ! I #00011464' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'4920233030303131343634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005491
005492     
      * EXEC CICS DELETEQ TS
005493*        QUEUE    (WS-PI-QID)
005494*    END-EXEC.
      *    MOVE '*&                    #   #00011468' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131343638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005495
005496 7770-EXIT.
005497     EXIT.
005498
005499     EJECT
005500 7780-WRITE-TS.
005501     MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.
005502     SET TS-INDX UP BY 1.
005503     ADD 1                       TO PI-TEMP-STOR-ITEMS.
005504
005505     
      * EXEC CICS WRITEQ TS
005506*         FROM    (TS-WORK-AREA)
005507*         QUEUE   (TS-NAME-TEXT)
005508*         LENGTH  (TS-LENGTH)
005509*         ITEM    (PI-TEMP-STOR-ITEMS)
005510*    END-EXEC.
      *    MOVE '*" I   L              ''   #00011481' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303131343831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005511
005512 7780-EXIT.
005513     EXIT.
005514
005515     EJECT
005516 7790-WRITE-SCREEN-TS.
005517     MOVE 1                      TO TS-ITEM.
005518
005519     
      * EXEC CICS WRITEQ TS
005520*         FROM    (EL152AI)
005521*         QUEUE   (TS-NAME-SCREEN)
005522*         LENGTH  (TS-MAP-LENGTH)
005523*         ITEM    (TS-ITEM)
005524*    END-EXEC.
      *    MOVE '*" I   L              ''   #00011495' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303131343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 EL152AI, 
                 TS-MAP-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005525
005526 7790-EXIT.
005527     EXIT.
005528
005529 7795-WRITE-PI-AREA-TS.
005530     
      * EXEC CICS WRITEQ TS
005531*        QUEUE    (WS-PI-QID)
005532*        FROM     (PROGRAM-INTERFACE-BLOCK)
005533*        LENGTH   (PI-COMM-LENGTH)
005534*    END-EXEC.
      *    MOVE '*"     L              ''   #00011506' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303131353036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005535
005536 7795-EXIT.
005537     EXIT.
005538
005539     EJECT
005540 7800-PRINT-LETTER-NOW.
005541***************************************************************
005542*     THIS ROUTINE WILL CAUSE THE CURRENTLY CREATED LETTER    *
005543*     TO BE PRINTED ON A HARDCOPY PRINTER.                    *
005544*        THE TEXT IS EDITED FOR ANY UNRESOLVED SYMBOLS,       *
005545*        THE PRINTER ID IS OBTAINED FROM THE CONTROL FILE,    *
005546*        THE LETTER IS SAVED IN TEMP-STORAGE,                 *
005547*        AND A START IS ISSUED FOR THE PRINT TRANSACTION.     *
005548***************************************************************
005549
005550     PERFORM 400-SET-CODES THRU 499-EXIT.
005551
005552     PERFORM 7500-READ-TS        THRU 7599-EXIT
005553
005554     PERFORM 7950-SET-INDX THRU 7950-EXIT.
005555     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
005556             VARYING SC-INDX FROM 1 BY 1
005557             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
005558
005559     IF PI-TOTAL-LINES = 0
005560        MOVE ER-0187             TO EMI-ERROR
005561        MOVE -1                  TO MAINTL
005562        MOVE AL-UABON            TO MAINTA
005563        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005564        GO TO 8200-SEND-DATAONLY.
005565
005566     IF COPIESL NOT = ZEROS
005567        IF COPIESI NOT NUMERIC
005568           MOVE ER-0184          TO EMI-ERROR
005569           MOVE -1               TO COPIESL
005570           MOVE AL-UNBON         TO COPIESA
005571           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005572           GO TO 8200-SEND-DATAONLY
005573         ELSE
005574           MOVE COPIESI          TO PI-NUM-PRINT-COPIES
005575       ELSE
005576        MOVE 1                   TO PI-NUM-PRINT-COPIES.
005577
005578*    PERFORM 7500-READ-TS        THRU 7599-EXIT
005579
005580*    PERFORM 7950-SET-INDX THRU 7950-EXIT.
005581*    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
005582*            VARYING SC-INDX FROM 1 BY 1
005583*            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
005584
005585     MOVE +0                     TO TALLY.
005586     INSPECT RECORD-TABLE TALLYING TALLY
005587                                 FOR CHARACTERS BEFORE '@'.
005588
005589     IF TALLY < +21900
005590        COMPUTE PI-CURRENT-LINE = TALLY / 73
005591        MOVE ZEROS               TO ROLL-COUNTER
005592        MOVE ER-0191             TO EMI-ERROR
005593        MOVE -1                  TO MAINTL
005594        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005595        GO TO 7900-ROLL-PAGE.
005596
005597 7800-UPDATE-CLAIM.
005598
005599     IF PI-RETURN-TO-PROGRAM = PGM-EL126
005600         GO TO 7800-GET-PRINTER.
005601
005602     MOVE PI-COMPANY-CD          TO  CLAM-CO.
005603     MOVE PI-CARRIER             TO  CLAM-CARRIER.
005604     MOVE PI-CLAIM-NO            TO  CLAM-CLAIM.
005605     MOVE PI-CERT-NO             TO  CLAM-CERT-NUM.
005606
005607     
      * EXEC CICS HANDLE CONDITION
005608*        NOTOPEN   (8860-CLAM-NOT-OPEN)
005609*        NOTFND    (7800-CLAIM-NOT-FOUND)
005610*    END-EXEC.
      *    MOVE '"$JI                  ! J #00011583' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4A20233030303131353833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005611
005612     
      * EXEC CICS READ
005613*        DATASET   (CLAM-ID)
005614*        RIDFLD    (CLAM-KEY)
005615*        SET       (ADDRESS OF CLAIM-MASTER)
005616*        UPDATE
005617*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011588' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131353838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005618
005619     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
005620     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
005621     MOVE CURRENT-SAVE           TO  CL-LAST-MAINT-DT.
005622     MOVE '2'                    TO  CL-LAST-MAINT-TYPE.
005623
005624     
      * EXEC CICS HANDLE CONDITION
005625*        DUPKEY    (7800-GET-PRINTER)
005626*    END-EXEC.
      *    MOVE '"$$                   ! K #00011600' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'4B20233030303131363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005627
005628     
      * EXEC CICS REWRITE
005629*        DATASET   (CLAM-ID)
005630*        FROM      (CLAIM-MASTER)
005631*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011604' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131363034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005632
005633     GO TO 7800-GET-PRINTER.
005634
005635 7800-CLAIM-NOT-FOUND.
005636
005637     MOVE ER-0133                TO  EMI-ERROR.
005638     MOVE -1                     TO  MAINTL.
005639     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005640     GO TO 8200-SEND-DATAONLY.
005641
005642 7800-GET-PRINTER.
005643
005644     IF PI-ALT-PRINTER-ID NOT = SPACES
005645        GO TO 7800-CNTL-BYPASS.
005646
005647     MOVE '1'                    TO CNTL-RECORD-TYPE.
005648     MOVE SPACES                 TO CNTL-GENL.
005649     MOVE ZEROS                  TO CNTL-SEQ.
005650
005651     
      * EXEC CICS HANDLE CONDITION
005652*         NOTOPEN    (8840-CNTL-NOT-OPEN)
005653*         NOTFND     (7890-NOT-FOUND)
005654*    END-EXEC.
      *    MOVE '"$JI                  ! L #00011627' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'4C20233030303131363237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005655
005656     
      * EXEC CICS READ
005657*         DATASET    (CNTL-ID)
005658*         SET        (ADDRESS OF CONTROL-FILE)
005659*         RIDFLD     (CNTL-KEY)
005660*    END-EXEC.
      *    MOVE '&"S        E          (   #00011632' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303131363332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005661
005662*    MOVE CF-FORMS-PRINTER-ID       TO TS-TERM-TEXT.
005663     MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID
005664     GO TO 7800-SET-UP-TS.
005665
005666 7800-CNTL-BYPASS.
005667*    MOVE PI-ALT-PRINTER-ID         TO TS-TERM-TEXT.
005668     MOVE PI-ALT-PRINTER-ID      TO WS-PRINTER-ID.
005669
005670 7800-SET-UP-TS.
005671
005672***********************************************************
005673*      CHECK TO SEE IF IT IS A PRINT REQUEST FOR PRINTING *
005674*      LETTERS ON A 3275 PRINTER. IF SO, SAVE THE SCREEN  *
005675***********************************************************
005676
005677     IF TS-TERM-PREFIX = 'DU'
005678        PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT
005679        PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.
005680
005681     SET TS-INDX TO 1.
005682
005683     
      * EXEC CICS ASKTIME
005684*    END-EXEC.
      *    MOVE '0"                    "   #00011659' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303131363539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005685
005686     MOVE EIBTIME                TO TS-ID-TIME.
005687*    MOVE '152A'                 TO TS-ID-TEXT
005688     MOVE TS-NAME-TEXT           TO PI-TEMP-STOR-ID.
005689     MOVE 0                      TO PI-TEMP-STOR-ITEMS.
005690
005691     PERFORM 7780-WRITE-TS THRU 7780-EXIT
005692             VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
005693             UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES
005694
005695     IF NOT PI-SHOW-MODE
005696        MOVE '1'                 TO PI-PRINT-SW.
005697
005698     
      * EXEC CICS HANDLE CONDITION
005699*         TERMIDERR  (8820-TERM-ERROR)
005700*         TRANSIDERR (8830-TRAN-ERROR)
005701*    END-EXEC.
      *    MOVE '"$[\                  ! M #00011674' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' &
                X'202020202020202020202120' &
                X'4D20233030303131363734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005702
005703*    IF PI-COMPANY-ID = 'DMD' OR 'XXX'
005704*        MOVE EIBTRMID    TO TS-TERM-TEXT
005705*        EXEC CICS START
005706*             INTERVAL    (0)
005707*             TRANSID     (PRINT-TRANS)
005708*             FROM        (PROGRAM-INTERFACE-BLOCK)
005709*             LENGTH      (PI-COMM-LENGTH)
005710*             TERMID      (TS-TERM-TEXT)
005711*        END-EXEC
005712*    ELSE
005713         
      * EXEC CICS START
005714*             INTERVAL    (0)
005715*             TRANSID     (PRINT-TRANS)
005716*             FROM        (PROGRAM-INTERFACE-BLOCK)
005717*             LENGTH      (PI-COMM-LENGTH)
005718*             TERMID      (TS-TERM-TEXT)
005719*             TERMID      (WS-PRINTER-ID)
005720*        END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                1   #00011689' TO DFHEIV0
           MOVE X'3028494C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303131363839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 PRINT-TRANS, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 WS-PRINTER-ID, 
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
           
005721
005722     MOVE '104A'                 TO TS-ID-TEXT
005723     MOVE ER-0189                TO EMI-ERROR.
005724     MOVE -1                     TO MAINTL.
005725     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
005726     MOVE ' PRINT TRANS STARTED ' TO EMI-MESSAGE-AREA (1)
005727     MOVE PI-TEMP-STOR-ID TO EMI-MESSAGE-AREA (1) (25:10)
005728
005729     GO TO 8200-SEND-DATAONLY.
005730
005731 7890-NOT-FOUND.
005732     MOVE ER-0190                TO EMI-ERROR.
005733     MOVE -1                     TO MAINTL.
005734     PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
005735     GO TO 8200-SEND-DATAONLY.
005736
005737     EJECT
005738 7900-ROLL-PAGE.
005739     IF ENTERPFL NOT = ZEROS
005740        MOVE -1                  TO ENTERPFL
005741       ELSE
005742        MOVE -1                  TO MAINTL.
005743
005744     PERFORM 7500-READ-TS     THRU 7599-EXIT
005745     PERFORM 7950-SET-INDX    THRU 7950-EXIT
005746     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
005747        VARYING SC-INDX FROM 1 BY 1 UNTIL
005748        SC-INDX > NUM-LINES-PER-SCREEN
005749
005750     IF PI-TOTAL-LINES = 0
005751        MOVE ER-0047             TO EMI-ERROR
005752        MOVE -1                  TO MAINTL
005753        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005754        GO TO 8200-SEND-DATAONLY.
005755
005756     COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.
005757
005758     IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
005759        MOVE ER-0067             TO EMI-ERROR
005760        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005761        MOVE 1                   TO TEMP-CURR-LINE.
005762
005763     IF TEMP-CURR-LINE > PI-TOTAL-LINES
005764        MOVE ER-0066             TO EMI-ERROR
005765        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005766        COMPUTE TEMP-CURR-LINE
005767            = PI-TOTAL-LINES + 1 - NUM-LINES-PER-SCREEN
005768        IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
005769            MOVE 1 TO TEMP-CURR-LINE.
005770
005771*    IF EMI-ERROR NOT = ER-0191
005772*       PERFORM 7500-READ-TS     THRU 7599-EXIT
005773*       PERFORM 7950-SET-INDX    THRU 7950-EXIT
005774*       PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
005775*                VARYING SC-INDX FROM 1 BY 1 UNTIL
005776*                SC-INDX > NUM-LINES-PER-SCREEN
005777*    END-IF
005778
005779     IF EMI-ERROR = ER-0066 OR ER-0067 OR ER-0000 OR ER-0191
005780        NEXT SENTENCE
005781     ELSE
005782        GO TO 8200-SEND-DATAONLY.
005783
005784
005785*    PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
005786
005787     MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.
005788     SET TB-INDX                 TO PI-CURRENT-LINE.
005789     MOVE LOW-VALUES             TO EL152RI.
005790
005791*    PERFORM 7500-READ-TS        THRU 7599-EXIT
005792
005793     PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
005794             VARYING SC-INDX FROM 1 BY 1
005795             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
005796
005797     GO TO 8200-SEND-DATAONLY.
005798
005799     EJECT
005800 7950-SET-INDX.
005801     IF PI-CURRENT-LINE = 0
005802        SET TB-INDX TO 1
005803       ELSE
005804        SET TB-INDX              TO PI-CURRENT-LINE.
005805
005806 7950-EXIT.
005807      EXIT.
005808
005809     EJECT
005810 7960-UPDATE-TABLE-FROM-SCREEN.
005811     IF SC-TEXTL (SC-INDX) NOT = ZEROS
005812        IF TB-INDX > PI-TOTAL-LINES
005813           IF PI-TOTAL-LINES = MAX-LINES
005814              MOVE ER-0051       TO EMI-ERROR
005815              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005816              GO TO 8200-SEND-DATAONLY
005817           ELSE
005818              MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX)
005819              ADD 1              TO PI-TOTAL-LINES
005820        ELSE
005821           MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX).
005822
005823     SET TB-INDX UP BY 1.
005824
005825 7960-EXIT.
005826      EXIT.
005827
005828     EJECT
005829 8000-STARTBR-ERACCT.
005830
005831     IF WS-ACCT-READ-SW = 'Y'
005832         NEXT SENTENCE
005833     ELSE
005834         
      * EXEC CICS GETMAIN
005835*             SET       (ADDRESS OF ACCOUNT-MASTER)
005836*             LENGTH    (ACCT-LENGTH)
005837*        END-EXEC.
      *    MOVE '," L                  $   #00011810' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131383130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005838
005839     
      * EXEC CICS STARTBR
005840*        RIDFLD      (ACCT-KEY)
005841*        DATASET     (ACCT-ID)
005842*        KEYLENGTH   (20)
005843*        GENERIC
005844*    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00011815' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303131383135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005845
005846     MOVE 'Y'         TO ACCT-BROWSE-STARTED.
005847 8000-STARTBR-EXIT.
005848     EXIT.
005849
005850 8000-READNEXT-ERACCT.
005851
005852     
      * EXEC CICS READNEXT
005853*        DATASET   (ACCT-ID)
005854*        INTO      (ACCOUNT-MASTER)
005855*        RIDFLD    (ACCT-KEY)
005856*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011828' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303131383238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005857
005858 8000-READNEXT-EXIT.
005859     EXIT.
005860
005861     EJECT
005862*8050-STARTBR-EMPROD. Remove as dead code
005863*8050-EXIT. Remove as dead code
005864*8060-READNEXT-EMPROD. Remove as dead code
005865*8060-EXIT. Remove as dead code
005866
005867     EJECT
005868 8100-SEND-INITIAL-MAP.
005869
005870     PERFORM 7700-PUT-TEMP-STOR  THRU 7749-EXIT
005871
005872     MOVE SAVE-DATE              TO DATEAO.
005873     MOVE EIBTIME                TO TIME-IN.
005874     MOVE TIME-OUT               TO TIMEAO.
005875
005876     MOVE PI-CARRIER             TO CARRO
005877     MOVE PI-CLAIM-NO            TO CLMNOO
005878     MOVE PI-CERT-NO             TO CRTNOO
005879     MOVE PI-PROCESSOR-ID        TO PROCO
005880     MOVE PI-COMPANY-ID          TO COMPIDO
005881     MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
005882                                 TO SYSO
005883
005884     MOVE PI-ENCLOSURE-CD        TO ENCO
005885
005886     IF NOT EMI-NO-ERRORS
005887         SET EMI-INDX TO 1
005888         MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGO
005889     ELSE
005890         MOVE SPACES             TO ERRMSGO.
005891
005892     MOVE -1                     TO MAINTL.
005893
005894     
      * EXEC CICS SEND
005895*        MAP      (MAP-NAME)
005896*        MAPSET   (MAPSET-NAME)
005897*        FROM     (EL152AO)
005898*        ERASE
005899*        CURSOR
005900*    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00011870' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303131383730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
005901
005902     PERFORM 8210-ENDBR  THRU 8210-EXIT.
005903     PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
005904
005905     GO TO 9000-RETURN-TRANS.
005906*    GO TO 0200-RECEIVE.
005907
005908 8200-SEND-DATAONLY.
005909
005910     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
005911
005912     MOVE PI-ENCLOSURE-CD        TO ENCO
005913
005914     MOVE SAVE-DATE              TO DATEAO.
005915     MOVE EIBTIME                TO TIME-IN.
005916     MOVE TIME-OUT               TO TIMEAO.
005917     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
005918
005919*    IF PI-COMPANY-ID = 'DMD'
005920*       IF FORCE-7840
005921*          MOVE '7840 FORCED' TO PFKEY9O
005922*          MOVE AL-SABON      TO PFKEY9A.
005923
005924     
      * EXEC CICS SEND
005925*        MAP      (MAP-NAME)
005926*        MAPSET   (MAPSET-NAME)
005927*        FROM     (EL152AO)
005928*        DATAONLY
005929*        CURSOR
005930*    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00011900' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303131393030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
005931
005932     PERFORM 8210-ENDBR  THRU 8210-EXIT.
005933     PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
005934
005935     GO TO 9000-RETURN-TRANS.
005936*    GO TO 0200-RECEIVE.
005937
005938 8200-SEND-DATAONLY-ERASEAUP.
005939
005940     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
005941
005942     MOVE PI-ENCLOSURE-CD        TO ENCO
005943
005944     MOVE SAVE-DATE              TO DATEAO.
005945     MOVE EIBTIME                TO TIME-IN.
005946     MOVE TIME-OUT               TO TIMEAO.
005947     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
005948     
      * EXEC CICS SEND
005949*        MAP      (MAP-NAME)
005950*        MAPSET   (MAPSET-NAME)
005951*        FROM     (EL152AO)
005952*        DATAONLY
005953*        ERASEAUP
005954*        CURSOR
005955*    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00011924' TO DFHEIV0
           MOVE X'382444202020204354202041' &
                X'2020202048204C2046202C20' &
                X'2020233030303131393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
005956
005957     PERFORM 8210-ENDBR  THRU 8210-EXIT.
005958     PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
005959
005960     GO TO 9000-RETURN-TRANS.
005961*    GO TO 0200-RECEIVE.
005962
005963 8210-ENDBR.
005964     IF ACCT-BROWSE-STARTED = 'Y'
005965        MOVE 'N'                  TO ACCT-BROWSE-STARTED
005966        
      * EXEC CICS ENDBR
005967*            DATASET    (ACCT-ID)
005968*       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011942' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131393432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005969
005970 8210-EXIT.
005971     EXIT.
005972
005973 8220-ENDBR-EMPROD.
005974     IF PROD-BROWSE-STARTED = 'Y'
005975        MOVE 'N'                  TO PROD-BROWSE-STARTED
005976        
      * EXEC CICS ENDBR
005977*            DATASET    (PROD-ID)
005978*       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011952' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131393532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROD-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005979
005980 8220-EXIT.
005981     EXIT.
005982
005983 8300-SEND-TEXT.
005984     
      * EXEC CICS SEND TEXT
005985*        FROM    (LOGOFF-TEXT)
005986*        LENGTH  (LOGOFF-LENGTH)
005987*        ERASE
005988*        FREEKB
005989*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00011960' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303131393630' TO DFHEIV0
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
           
005990
005991     
      * EXEC CICS RETURN
005992*    END-EXEC.
      *    MOVE '.(                    ''   #00011967' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131393637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005993
005994 8600-DEEDIT.
005995     
      * EXEC CICS BIF DEEDIT
005996*         FIELD    (DEEDIT-FIELD)
005997*         LENGTH   (15)
005998*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00011971' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131393731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005999
006000     EJECT
006001 8800-UNAUTHORIZED-ACCESS.
006002     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
006003     GO TO 8300-SEND-TEXT.
006004
006005 8810-PF23.
006006     PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
006007     MOVE ZEROS                  TO PI-TOTAL-LINES.
006008     MOVE ZEROS                  TO PI-CURRENT-LINE.
006009     MOVE EIBAID                 TO PI-ENTRY-CD-1.
006010     MOVE XCTL-005               TO PGM-NAME.
006011     GO TO 9300-XCTL.
006012
006013 8820-TERM-ERROR.
006014     MOVE ER-0412                TO EMI-ERROR.
006015     MOVE SPACES                 TO PI-PRINT-SW.
006016     GO TO 8999-OPEN-ERROR.
006017
006018 8830-TRAN-ERROR.
006019     MOVE ER-0413                TO EMI-ERROR.
006020     MOVE SPACES                 TO PI-PRINT-SW.
006021     GO TO 8999-OPEN-ERROR.
006022
006023 8840-CNTL-NOT-OPEN.
006024     MOVE ER-0042                TO EMI-ERROR.
006025     GO TO 8999-OPEN-ERROR.
006026
006027 8850-ARCH-NOT-OPEN.
006028     MOVE ER-0332                TO EMI-ERROR.
006029     GO TO 8999-OPEN-ERROR.
006030
006031 8860-CLAM-NOT-OPEN.
006032     MOVE ER-0154                TO EMI-ERROR.
006033     GO TO 8999-OPEN-ERROR.
006034
006035 8870-ACTV-NOT-OPEN.
006036     MOVE ER-0172                TO EMI-ERROR.
006037     GO TO 8999-OPEN-ERROR.
006038
006039 8880-ACCT-NOT-OPEN.
006040     MOVE ER-0168                TO EMI-ERROR.
006041     GO TO 8999-OPEN-ERROR.
006042
006043 8890-TEXT-NOT-OPEN.
006044     MOVE ER-0013                TO EMI-ERROR.
006045     GO TO 8999-OPEN-ERROR.
006046
006047 8900-CERT-NOT-OPEN.
006048     MOVE ER-0169                TO EMI-ERROR.
006049     GO TO 8999-OPEN-ERROR.
006050
006051 8910-CERT-NOT-FOUND.
006052     MOVE ER-0206                TO EMI-ERROR.
006053     GO TO 8999-OPEN-ERROR.
006054
006055 8915-PROD-NOT-OPEN.
006056     MOVE ER-9106                TO EMI-ERROR.
006057     GO TO 8999-OPEN-ERROR.
006058
006059 8920-PLCY-NOT-OPEN.
006060     MOVE ER-9883                TO EMI-ERROR.
006061     GO TO 8999-OPEN-ERROR.
006062
006063 8925-PLAN-NOT-OPEN.
006064     MOVE ER-9808                TO EMI-ERROR.
006065     GO TO 8999-OPEN-ERROR.
006066
006067 8930-PLCY-NOT-FOUND.
006068     MOVE ER-9483                TO EMI-ERROR.
006069     GO TO 8999-OPEN-ERROR.
006070
006071 8999-OPEN-ERROR.
006072     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006073     MOVE -1                     TO MAINTL.
006074     GO TO 8200-SEND-DATAONLY.
006075
006076 9000-RETURN-TRANS.
006077
006078     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
006079
006080     
      * EXEC CICS RETURN
006081*        TRANSID    (TRANS-ID)
006082*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
006083*        LENGTH     (PI-COMM-LENGTH)
006084*    END-EXEC.
      *    MOVE '.(CT                  ''   #00012056' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132303536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006085
006086
006087 9000-EXIT.
006088     EXIT.
006089
006090 9200-RETURN-MAIN-MENU.
006091     PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
006092
006093     MOVE ZEROS                  TO PI-TOTAL-LINES
006094                                    PI-CURRENT-LINE.
006095     MOVE XCTL-126               TO PGM-NAME.
006096
006097     GO TO 9300-XCTL.
006098
006099 9300-XCTL.
006100     
      * EXEC CICS XCTL
006101*        PROGRAM    (PGM-NAME)
006102*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
006103*        LENGTH     (PI-COMM-LENGTH)
006104*    END-EXEC.
      *    MOVE '.$C                   %   #00012076' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132303736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006105
006106 9400-CLEAR.
006107     IF PI-CLEAR-MODE
006108         MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME
006109         GO TO 9300-XCTL
006110     ELSE
006111         PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT
006112         PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT
006113         PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT
006114         MOVE SPACES             TO PI-WA
006115                                    RECORD-TABLE
006116                                    TS-WORK-AREA
006117         MOVE ZEROS              TO PI-TOTAL-LINES
006118                                    PI-CURRENT-LINE
006119                                    PI-TEMP-STOR-ITEMS
006120                                    PI-UPDATE-SW
006121                                    PI-ADDR-SEQ
006122         MOVE '2'                TO PI-ACTION
006123         MOVE LOW-VALUES         TO EL152AO
006124                                    PI-ADDR-TYPE
006125         SET TS-INDX
006126             TA1
006127             TA2
006128             TA21
006129             TB-INDX
006130             TB-INDX1
006131             MOVE-INDX
006132             SC-INDX             TO +1
006133         IF GETMAIN-SWITCH = '2'
006134             MOVE 1              TO GETMAIN-SWITCH
006135             GO TO 8100-SEND-INITIAL-MAP
006136         ELSE
006137             GO TO 8100-SEND-INITIAL-MAP.
006138
006139 9600-PGMID-ERROR.
006140     
      * EXEC CICS HANDLE CONDITION
006141*        PGMIDERR    (8300-SEND-TEXT)
006142*    END-EXEC.
      *    MOVE '"$L                   ! N #00012116' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'4E20233030303132313136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006143
006144     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
006145     MOVE ' '                    TO PI-ENTRY-CD-1.
006146     MOVE XCTL-005               TO PGM-NAME.
006147     MOVE PGM-NAME               TO LOGOFF-PGM.
006148     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
006149     GO TO 9300-XCTL.
006150
006151 9700-DATE-LINK.
006152     MOVE LINK-ELDATCV           TO PGM-NAME.
006153     
      * EXEC CICS LINK
006154*        PROGRAM   (PGM-NAME)
006155*        COMMAREA  (DATE-CONVERSION-DATA)
006156*        LENGTH    (DC-COMM-LENGTH)
006157*    END-EXEC.
      *    MOVE '."C                   (   #00012129' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006158
006159*    IF NO-CONVERSION-ERROR
006160*            AND
006161*       W-REVERSE-DATE
006162*            AND
006163*       PI-COMPANY-ID = 'AUK'
006164*           MOVE DC-GREG-DATE-1-EDIT TO W-EDIT-DATE-1
006165*           MOVE W-ED1-MM            TO W-ED2-MM
006166*           MOVE W-ED1-DD            TO W-ED2-DD
006167*           MOVE W-ED1-YY            TO W-ED2-YY
006168*           MOVE W-EDIT-DATE-2       TO DC-GREG-DATE-1-EDIT.
006169
006170 9700-EXIT.
006171      EXIT.
006172
006173 9900-ERROR-FORMAT.
006174     IF NOT EMI-ERRORS-COMPLETE
006175         MOVE LINK-001           TO PGM-NAME
006176         
      * EXEC CICS LINK
006177*            PROGRAM   (PGM-NAME)
006178*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
006179*            LENGTH    (EMI-COMM-LENGTH)
006180*        END-EXEC.
      *    MOVE '."C                   (   #00012152' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132313532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006181
006182 9900-EXIT.
006183     EXIT.
006184
006185 9990-ABEND.
006186     MOVE LINK-004               TO PGM-NAME.
006187     MOVE DFHEIBLK               TO EMI-LINE1.
006188     
      * EXEC CICS LINK
006189*        PROGRAM   (PGM-NAME)
006190*        COMMAREA  (EMI-LINE1)
006191*        LENGTH    (72)
006192*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00012164' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132313634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006193
006194     GO TO 8200-SEND-DATAONLY.
006195
006196 9995-SECURITY-VIOLATION.
006197*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00012191' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132313931' TO DFHEIV0
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
006198

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL152' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     0325-MAPFAIL,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9400-CLEAR,
                     0100-PA,
                     0100-PA,
                     0100-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0325-MAPFAIL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 590-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8850-ARCH-NOT-OPEN,
                     1020-ENDBR,
                     1020-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2120-ENDBR,
                     2120-ENDBR,
                     8890-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8860-CLAM-NOT-OPEN,
                     3010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6599-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8860-CLAM-NOT-OPEN,
                     7090-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8880-ACCT-NOT-OPEN,
                     7080-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8870-ACTV-NOT-OPEN,
                     7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7075-CONTINUE-ACTV-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 8900-CERT-NOT-OPEN,
                     8910-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8870-ACTV-NOT-OPEN,
                     7200-READ-LOAN-NUMBER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8870-ACTV-NOT-OPEN,
                     7201-READ-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 7290-BENE-NOT-OPEN,
                     7205-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 8870-ACTV-NOT-OPEN,
                     7210-READ-EMPLOYER-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8870-ACTV-NOT-OPEN,
                     7220-READ-INSURED-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 8870-ACTV-NOT-OPEN,
                     7225-READ-OTHER1-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 8870-ACTV-NOT-OPEN,
                     7230-READ-OTHER2-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 8870-ACTV-NOT-OPEN,
                     7240-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 8870-ACTV-NOT-OPEN,
                     7250-READ-ACCOUNT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 8880-ACCT-NOT-OPEN,
                     7251-READ-3RD-PARTY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8870-ACTV-NOT-OPEN,
                     7252-READ-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7260-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 8870-ACTV-NOT-OPEN,
                     7265-READ-CNTL1,
                     7265-READ-CNTL1
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 8840-CNTL-NOT-OPEN,
                     7266-READ-CNTL2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 8840-CNTL-NOT-OPEN,
                     7267-READ-CNTL4
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 8840-CNTL-NOT-OPEN,
                     7267-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 8840-CNTL-NOT-OPEN,
                     7268-SET-DATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 7290-BENE-NOT-OPEN,
                     7290-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 7590-TS-QIDERR,
                     7585-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7690-TS-QIDERR,
                     7685-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7750-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7760-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7770-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 8860-CLAM-NOT-OPEN,
                     7800-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 7800-GET-PRINTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 44
               GO TO 8840-CNTL-NOT-OPEN,
                     7890-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 45
               GO TO 8820-TERM-ERROR,
                     8830-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 46
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL152' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
