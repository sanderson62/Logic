      *((program: EL176.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL176 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 07/13/94 11:11:10.
000007*                            VMOD=2.060
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
000020*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
000021*            *                                                   *
000022*            *****************************************************
000023
000024*REMARKS.
000025*        IN ORDER TO START THE PRINTING OF CHECKS, THIS FUNCTION
000026*    IS USED TO QUALIFY THE CONTROL BATCHES TO BE PRINTED AND
000027*    SPECIFY A PRINT TIME.  THE INDIVIDUAL CHECK WRITER PROGRAM
000028*    IS STARTED BY THIS PROGRAM.
000029
000030*    SCREENS     - EL176A - CHECK WRITER
000031
000032*    ENTERED BY  - EL171  - REPORT MENU
000033
000034*    EXIT TO     - EL171  - RESULT OF CLEAR
000035
000036*    INPUT FILES - ELCHKQ - CHECK QUEUE
000037*                  ELTRLR - ACTIVITY TRAILERS
000038*                  ELCNTL - CONTROL FILE
000039*                  ELMSTR - CLAIM MASTER
000040*                  ELCERT - CERTIFICATE MASTER
000041*                  ELBENE - BENEFICIARY MASTER
000042*                  ERACCT - ACCOUNT MASTER
000043*                  ERCOMP - COMPENSATION MASTER
000044*                  MPPLAN - CONVENIENCE PRODUCER PLAN MASTER
000045*                  MPPLCY - CONVENIENCE POLICY MASTER
000046*                  MPPROD - CONVENIENCE PRODUCER MASTER
000047
000048*    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS
000049*                   ELCHKQ - CHECK QUEUE
000050
000051*    COMMAREA    - PASSED.  PRINT TIME PASSED TO CHECK WRITER AS
000052*                  A 2 BYTE COMP NUMBER IN THE FIRST TWO BYTES OF
000053*                  THE WORK AREA.  FORM TYPE IS PASSED AS
000054*                  ENTRY-CD-1.
000055
000056*
000057******************************************************************
000058*                   C H A N G E   L O G
000059*
000060* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000061*-----------------------------------------------------------------
000062*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000063* EFFECTIVE    NUMBER
000064*-----------------------------------------------------------------
000065*                - JWBA 7/94 PER IR94025-8010 TO ADD HANDLE
000066*   LGC123         CONDITIONS FOR CLOSED/DISABLED MICRFLAG AN
000067*                  MICR.DRAFTS FILE.
000068*
000069*                - JWBA 4/95 PER IR94196-8912 - ADD OF "NOTFN
000070*   LGC134         HANDLE CONDITION;
000071*
000072*                - DJNA 4/01/2000 CR#2000030100009
000073*   CSODJN         DRAFT NUMBER EXPANSION.
000074* 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
000075* 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
000076* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
000077* 042704    2004042700002  PEMA  REMOVE NOTIFY NAME AND ADDRESS
000078* 070104    IR             PEMA  MODIFY LENGTH OF MICRDRFT
000079* 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
000080* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000081* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000082* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
000083* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000084* 080322  CR2021100800003  TANA  Add B and H claim types
000085******************************************************************
000086     EJECT
000087 ENVIRONMENT DIVISION.
000088
000089 DATA DIVISION.
000090
000091 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000092 77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
000093 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
000094                                   USAGE POINTER.
000095
000096 01  WS-MOD-FIELDS.
000097     05  WS-FLAG-KEY                 PIC X(8) VALUE SPACES.
000098     05  MICR-CHK-SW                 PIC X VALUE 'N'.
000099
000100     05  CSO-DRAFT-420C.
000101         10  CSO-DRAFT-KEY           PIC X(19).
000102         10  FILLER                  PIC X(1162).
000103
000104     05  WS-MICR-KEY                 PIC X(19) VALUE SPACES.
000105*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1181.
000106*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1184.
000107     05  REC-LGTH                    PIC S9(4) COMP VALUE +1254.
000108
000109 77  FILLER  PIC X(32)  VALUE '********************************'.
000110 77  FILLER  PIC X(32)  VALUE '*    EL176 WORKING STORAGE     *'.
000111 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.060 **********'.
000112
000113*                            COPY ELCSCTM.
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
000114
000115*                            COPY ELCSCRTY.
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
000116
000117     EJECT
000118 01  WS-DATE-AREA.
000119     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000120     05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
000121     05  WS-RESPONSE         PIC S9(8)   COMP.
000122         88  WS-RESP-NORMAL              VALUE +00.
000123         88  WS-RESP-NOTFND              VALUE +13.
000124
000125
000126 01  FILLER                          COMP-3.
000127     05  WS-NOT-FOUND                PIC S9      VALUE ZERO.
000128     05  WS-PRINTER-STARTED-SW       PIC S9      VALUE ZERO.
000129     05  WS-READNEXT-SW              PIC S9      VALUE ZERO.
000130     05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.
000131     05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.
000132       88  TRANSACTION-SUCCESSFUL                    VALUE +1 +2.
000133       88  CHECKS-WITHOUT-ADDRESSES                  VALUE +2.
000134
000135     05  TIME-IN                     PIC S9(7)   VALUE ZERO.
000136     05  TIME-OUT REDEFINES TIME-IN  PIC S9(3)V9(4).
000137     05  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.
000138
000139     05  WS-ACTIVITY-TRAILERS-BROWSE-SW PIC S9 VALUE ZERO.
000140     05  WS-CHECK-QUEUE-BROWSE-SW       PIC S9 VALUE ZERO.
000141
000142     05  WS-NOT-RELEASED-COUNT       PIC S9(5)    VALUE ZERO.
000143     05  WS-NOT-RELEASED-AMOUNT      PIC S9(9)V99 VALUE ZERO.
000144     05  WS-RELEASED-COUNT           PIC S9(5)    VALUE ZERO.
000145     05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.
000146
000147     EJECT
000148 01  FILLER      COMP SYNC.
000149     05  WS-PAYMENT-NOTE-SEQ-NO      PIC S9(4)  COMP VALUE +0.
000150     05  SC-ITEM                     PIC S9(4)   VALUE +0001.
000151
000152     05  WS-TS-LENGTH                PIC S9(4)   VALUE +1158.
000153
000154     05  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.
000155
000156     05  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.
000157     05  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.
000158
000159     05  WS-LAST-CONTROL-GROUP       PIC S9(8)   VALUE ZERO.
000160     05  WS-TIMES-PRINTED            PIC S9(4)   VALUE ZERO.
000161
000162     05  WS-SEQUENCE-NUMBER          PIC S9(4)   VALUE ZERO.
000163     05  WS-BEGIN-NUMBER             PIC S9(4)   VALUE ZERO.
000164
000165     05  WS-ELCHKQ-POINTER           PIC S9(8)   VALUE ZERO.
000166     05  WS-INDEX                    PIC S9(4)   VALUE ZERO.
000167
000168     EJECT
000169 01  FILLER.
000170     05  WS-ERCOMP-KEY.
000171         10  WS-ERCOMP-COMPANY-CD    PIC X.
000172         10  WS-ERCOMP-CARRIER       PIC X.
000173         10  WS-ERCOMP-GROUPING      PIC X(6).
000174         10  WS-ERCOMP-RESP-NO       PIC X(10).
000175         10  WS-ERCOMP-ACCOUNT       PIC X(10).
000176         10  WS-ERCOMP-TYPE          PIC X.
000177
000178     05  WS-WORK-DATE.
000179         10  WS-WORK-MO              PIC XX.
000180         10  FILLER                  PIC X.
000181         10  WS-WORK-DA              PIC XX.
000182         10  FILLER                  PIC X.
000183         10  WS-WORK-YR              PIC XX.
000184     05  WS-CONTROL-FILE-KEY.
000185         10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
000186         10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
000187         10  FILLER                  PIC XX      VALUE SPACES.
000188         10  WS-CFK-BENEFIT-NO                   VALUE SPACES.
000189             15  FILLER              PIC X.
000190             15  WS-CFK-CARRIER      PIC X.
000191         10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP.
000192
000193     05  WS-ACTIVITY-TRAILERS-KEY.
000194         10  WS-ATK-COMPANY-CD       PIC X.
000195         10  WS-ATK-CARRIER          PIC X.
000196         10  WS-ATK-CLAIM-NO         PIC X(7).
000197         10  WS-ATK-CERT-NO          PIC X(11).
000198         10  WS-ATK-SEQUENCE-NO      PIC S9(4)  COMP.
000199
000200     05  WS-CHECK-QUEUE-KEY.
000201         10  WS-CQK-COMPANY-CD       PIC X.
000202         10  WS-CQK-CONTROL-NUMBER   PIC S9(8) COMP.
000203         10  WS-CQK-SEQUENCE-NUMBER  PIC S9(4) COMP.
000204
000205     05  WS-CHECK-AIX-KEY.
000206         10  WS-CQK-COMPANY-CD-A1        PIC X.
000207         10  WS-CQK-CONTROL-NUMBER-A1    PIC S9(08)  COMP.
000208         10  WS-PAYEE-CARRIER            PIC X.
000209         10  WS-PAYEE-GROUPING           PIC X(06).
000210         10  WS-PAYEE-STATE              PIC XX.
000211         10  WS-PAYEE-BENE-ACCT          PIC X(10).
000212         10  WS-CQK-SEQUENCE-NUMBER-A1   PIC S9(04)   COMP.
000213
000214     05  WS-LAST-CHECK-QUEUE-KEY     PIC X(26) VALUE LOW-VALUE.
000215     05  WS-BROWSE-LENGTH            PIC S9           COMP.
000216
000217     05  WS-CLAIM-MASTER-KEY.
000218         10  WS-CK-COMPANY-CD        PIC X.
000219         10  WS-CK-CARRIER           PIC X.
000220         10  WS-CK-CLAIM-NO          PIC X(7).
000221         10  WS-CK-CERT-NO           PIC X(11).
000222
000223     05  WS-CERTIFICATE-MASTER-KEY.
000224         10  WS-CM-COMPANY-CD        PIC X.
000225         10  WS-CM-CARRIER           PIC X.
000226         10  WS-CM-GROUPING          PIC X(6).
000227         10  WS-CM-STATE             PIC XX.
000228         10  WS-CM-ACCOUNT           PIC X(10).
000229         10  WS-CM-CERT-EFF-DT       PIC XX.
000230         10  WS-CM-CERT-NO           PIC X(11).
000231
000232     05  WS-ACCOUNT-MASTER-KEY.
000233         10  WS-AK-COMPANY-CD        PIC X.
000234         10  WS-AK-CARRIER           PIC X.
000235         10  WS-AK-GROUPING          PIC X(6).
000236         10  WS-AK-STATE             PIC XX.
000237         10  WS-AK-ACCOUNT           PIC X(10).
000238         10  WS-AK-EXPIRATION-DT     PIC XX.
000239
000240     05  WS-ACCOUNT-HOLD-RECORD      PIC X(2000).
000241     05  WS-PRODUCER-HOLD-RECORD     PIC X(2000).
000242
000243     05  WS-BENEFICIARY-KEY.
000244         10  WS-BK-COMPANY-CD        PIC X.
000245         10  WS-BK-RECORD-TYPE       PIC X.
000246         10  WS-BK-BENEFICIARY       PIC X(10).
000247
000248     05  WS-POLICY-MASTER-KEY.
000249         10  WS-PM-COMPANY-CD        PIC X.
000250         10  WS-PM-CARRIER           PIC X.
000251         10  WS-PM-GROUPING          PIC X(06).
000252         10  WS-PM-STATE             PIC XX.
000253         10  WS-PM-PRODUCER          PIC X(10).
000254         10  WS-PM-EFF-DT            PIC XX.
000255         10  WS-PM-REFERENCE-NO      PIC X(20).
000256
000257     05  WS-PRODUCER-MASTER-KEY.
000258         10  WS-PD-COMPANY-CD        PIC X.
000259         10  WS-PD-CARRIER           PIC X.
000260         10  WS-PD-GROUPING          PIC X(06).
000261         10  WS-PD-STATE             PIC XX.
000262         10  WS-PD-PRODUCER          PIC X(10).
000263         10  WS-PD-EXP-DT            PIC XX.
000264
000265     05  WS-PLAN-MASTER-KEY.
000266         10  WS-PP-COMPANY-CD        PIC X.
000267         10  WS-PP-CARRIER           PIC X.
000268         10  WS-PP-GROUPING          PIC X(06).
000269         10  WS-PP-STATE             PIC XX.
000270         10  WS-PP-PRODUCER          PIC X(10).
000271         10  WS-PP-PLAN-CODE         PIC XX.
000272         10  WS-PP-REV-NO            PIC 9(03).
000273
000274     EJECT
000275     05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL176S'.
000276     05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL176A'.
000277
000278     05  FILLER REDEFINES WS-MAP-NAME.
000279         20  FILLER                  PIC XX.
000280         20  WS-MAP-NUMBER           PIC X(6).
000281
000282     05  THIS-PGM                    PIC X(8)  VALUE 'EL176'.
000283
000284     05  WS-LAST-CARRIER             PIC X     VALUE LOW-VALUES.
000285
000286     05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.
000287     05  WS-CHECK-QUEUE-AIX-DSID     PIC X(8) VALUE 'ELCHKQ2'.
000288     05  WS-ENQ-COMPANY-ID           PIC X(3) VALUE ZERO.
000289     05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.
000290     05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
000291     05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.
000292     05  WS-CERTIFICATE-MASTER-DSID  PIC X(8) VALUE 'ELCERT'.
000293     05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.
000294     05  WS-BENEFICIARY-MASTER-DSID  PIC X(8) VALUE 'ELBENE'.
000295     05  WS-POLICY-MASTER-DSID       PIC X(8) VALUE 'MPPLCY'.
000296     05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.
000297     05  WS-PLAN-MASTER-DSID         PIC X(8) VALUE 'MPPLAN'.
000298
000299     05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
000300     05  WS-SPACES                   PIC X VALUE SPACES.
000301
000302     05  WS-CHECK-WRITER-DATE        PIC XX VALUE LOW-VALUES.
000303
000304     05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX46'.
000305     05  WS-CHECK-WRITER-TRANS-ID.
000306         10  WS-CHECK-TRANSID-1      PIC X       VALUE 'Q'.
000307         10  WS-CHECK-TRANSID-2      PIC XXX     VALUE SPACES.
000308
000309     05  WS-BENEFIT-NO               PIC XX      VALUE SPACES.
000310
000311     05  WS-AUTO-PAY-SW              PIC X       VALUE 'N'.
000312
000313     05  WS-EMPROD-GETMAIN-SW        PIC X       VALUE 'Y'.
000314     05  WS-ERACCT-GETMAIN-SW        PIC X       VALUE 'Y'.
000315     05  WS-ELCHKQ-GETMAIN-SW        PIC X       VALUE 'Y'.
000316
000317     05  FILE-SWITCH                 PIC X(04)   VALUE SPACES.
000318
000319     05  WS-AIG-CREDITOR-NAME        PIC X(30)   VALUE SPACES.
000320     05  WS-AIG-INS-CO-NAME          PIC X(30)   VALUE SPACES.
000321
000322     05  WS-DMD-FLAG-KEY             PIC X(8)    VALUE SPACES.
000323     05  WS-DMD-MICR-FLAG            PIC X(18)   VALUE SPACES.
000324
000325     05  WS-DMD-DRFT-KEY             PIC X(19)   VALUE SPACES.
000326     05  WS-DMD-MICR-DRFT            PIC X(705)  VALUE SPACES.
000327
000328     05  WS-DMD-CNTL-GRP-COUNT       PIC S9  COMP  VALUE ZEROS.
000329
000330     05  WS-COV-TYPE                 PIC X.
000331     05  WS-KIND.
000332         10  WS-RETRO-DAYS           PIC 99.
000333         10  WS-RETRO-ELIM           PIC X.
000334
000335     05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)   VALUE +70
000336                                     COMP SYNC.
000337
000338     05  WS-TEXT-MESSAGE             PIC X(70)   VALUE SPACES.
000339
000340     05  WS-TEMP-STORAGE-ITEM        PIC S9(4)   VALUE ZERO
000341                                     COMP SYNC.
000342
000343     05  WS-TEMP-STORAGE-KEY.
000344         10  WS-TSK-TERM-ID          PIC X(4)    VALUE SPACES.
000345         10  WS-TSK-TIME             PIC S9(7)   VALUE ZERO
000346                                     COMP-3.
000347
000348     05  WS-MINUTES                  PIC S99     VALUE ZERO.
000349
000350     05  WS-GREATEST-CHECK-NUMBER    PIC 9(7)    VALUE ZERO.
000351
000352     05  WS-GREATEST-CHECK-NUMBER-X  REDEFINES
000353         WS-GREATEST-CHECK-NUMBER    PIC X(7).
000354
000355     05  WS-CHECK-NUMBER             PIC 9(7)    VALUE ZERO.
000356
000357     05  WS-CHECK-NUMBER-X REDEFINES
000358         WS-CHECK-NUMBER             PIC X(7).
000359
000360     05  WS-ACKNO                    PIC 9(7)    VALUE ZERO.
000361
000362     05  WS-ACKNO-X REDEFINES
000363         WS-ACKNO                    PIC X(7).
000364
000365     05  WS-PAYMENT-TYPE             PIC X       VALUE ZERO.
000366     05  WS-PAYEE-CODE.
000367         10  WS-PAYEE-CD             PIC X.
000368         10  WS-PAYEE-SEQ            PIC X.
000369         10  WS-PAYEE-SEQ-NUM REDEFINES
000370             WS-PAYEE-SEQ            PIC 9.
000371
000372     05  WS-INIT-CONTROL-GROUP.
000373         10  FILLER                  PIC X    VALUE SPACES.
000374         10  FILLER                  PIC X(7) VALUE LOW-VALUES.
000375
000376     05  WS-SSN.
000377         10  WS-SSN-STATE            PIC XX.
000378         10  WS-SSN-ACCOUNT          PIC X(6).
000379         10  WS-SSN-LN3              PIC X(3).
000380
000381     05  WS-MEMBER-NUMBER.
000382         10  WS-MEMBER-STATE         PIC XX.
000383         10  WS-MEMBER-ACCOUNT       PIC X(6).
000384         10  WS-MEMBER-LN4           PIC X(4).
000385
000386     05  WS-GROUP.
000387         10  WS-GRP-1-3              PIC XXX.
000388         10  WS-GRP-4-6              PIC XXX.
000389
000390     05  WS-ZIP-UNPACKED                 PIC 9(9)  VALUE ZEROS.
000391
000392     05  WS-CARRIER-ADDRESS-DATA.
000393         10  WS-CARRIER-MAIL-TO-NAME     PIC X(30).
000394         10  WS-CARRIER-IN-CARE-OF       PIC X(30).
000395         10  WS-CARRIER-ADDRESS-LINE-1   PIC X(30).
000396         10  WS-CARRIER-ADDRESS-LINE-2   PIC X(30).
000397         10  WS-CARRIER-CITY-STATE       PIC X(30).
000398         10  WS-CARRIER-ZIP-CODE         PIC X(9).
000399         10  WS-CARRIER-PHONE-NO         PIC 9(11)     COMP-3.
000400
000401     05  WS-OLD-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.
000402     05  WS-NEW-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.
000403
000404     05  WS-WORK-PHONE               PIC X(10)   VALUE ZEROS.
000405     05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
000406                                     PIC 9(10).
000407
000408     EJECT
000409
000410 01  ERROR-MESSAGES.
000411     12  ER-0002                 PIC X(4)  VALUE '0002'.
000412     12  ER-0004                 PIC X(4)  VALUE '0004'.
000413     12  ER-0008                 PIC X(4)  VALUE '0008'.
000414     12  ER-0029                 PIC X(4)  VALUE '0029'.
000415     12  ER-0042                 PIC X(4)  VALUE '0042'.
000416     12  ER-0070                 PIC X(4)  VALUE '0070'.
000417     12  ER-0154                 PIC X(4)  VALUE '0154'.
000418     12  ER-0168                 PIC X(4)  VALUE '0168'.
000419     12  ER-0169                 PIC X(4)  VALUE '0169'.
000420     12  ER-0172                 PIC X(4)  VALUE '0172'.
000421     12  ER-0330                 PIC X(4)  VALUE '0330'.
000422     12  ER-0361                 PIC X(4)  VALUE '0361'.
000423     12  ER-0362                 PIC X(4)  VALUE '0362'.
000424     12  ER-0364                 PIC X(4)  VALUE '0364'.
000425     12  ER-0365                 PIC X(4)  VALUE '0365'.
000426     12  ER-0366                 PIC X(4)  VALUE '0366'.
000427     12  ER-0367                 PIC X(4)  VALUE '0367'.
000428     12  ER-0368                 PIC X(4)  VALUE '0368'.
000429     12  ER-0369                 PIC X(4)  VALUE '0369'.
000430     12  ER-0370                 PIC X(4)  VALUE '0370'.
000431     12  ER-0371                 PIC X(4)  VALUE '0371'.
000432     12  ER-0379                 PIC X(4)  VALUE '0379'.
000433     12  ER-0380                 PIC X(4)  VALUE '0380'.
000434     12  ER-0381                 PIC X(4)  VALUE '0381'.
000435     12  ER-0382                 PIC X(4)  VALUE '0382'.
000436     12  ER-0383                 PIC X(4)  VALUE '0383'.
000437     12  ER-0385                 PIC X(4)  VALUE '0385'.
000438     12  ER-0387                 PIC X(4)  VALUE '0387'.
000439     12  ER-0389                 PIC X(4)  VALUE '0389'.
000440     12  ER-0390                 PIC X(4)  VALUE '0390'.
000441     12  ER-0391                 PIC X(4)  VALUE '0391'.
000442     12  ER-0392                 PIC X(4)  VALUE '0392'.
000443     12  ER-0393                 PIC X(4)  VALUE '0393'.
000444     12  ER-0394                 PIC X(4)  VALUE '0394'.
000445     12  ER-0395                 PIC X(4)  VALUE '0395'.
000446     12  ER-0490                 PIC X(4)  VALUE '0490'.
000447     12  ER-2055                 PIC X(4)  VALUE '2055'.
000448     12  ER-2370                 PIC X(4)  VALUE '2370'.
000449     12  ER-2936                 PIC X(4)  VALUE '2936'.
000450     12  ER-3027                 PIC X(4)  VALUE '3027'.
000451     12  ER-3028                 PIC X(4)  VALUE '3028'.
000452     12  ER-3130                 PIC X(4)  VALUE '3130'.
000453     12  ER-3776                 PIC X(4)  VALUE '3776'.
000454     12  ER-7675                 PIC X(4)  VALUE '7675'.
000455     12  ER-9808                 PIC X(4)  VALUE '9808'.
000456     12  ER-9883                 PIC X(4)  VALUE '9883'.
000457     12  ER-9886                 PIC X(4)  VALUE '9886'.
000458
000459     EJECT
000460*    COPY ELCINTF.
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
000461
000462*    COPY ELC176PI.
      *>>((file: ELC176PI))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELC176PI.                          *
000005*                            VMOD=2.005                         *
000006*****************************************************************.
000007
000008     12  FILLER                      REDEFINES
000009         PI-PROGRAM-WORK-AREA.
000010
000011         16  PI-TEMP-STORAGE-KEY.
000012             20  PI-TSK-TERM-ID      PIC X(4).
000013             20  PI-TSK-TIME         PIC S9(7)     COMP-3.
000014
000015         16  PI-PROCESSING-SW        PIC S9        COMP-3.
000016
000017         16  PI-NUMBER-OF-ALIGNMENT-CHECKS
000018                                     PIC S9        COMP-3.
000019         16  PI-ALIGNMENT-CONTROL-GROUP
000020                                     PIC S9(8)     COMP-3.
000021         16  PI-ALIGNMENT-SEQUENCE-NO
000022                                     PIC S9(4)     COMP-3.
000023         16  PI-NUMBER-OF-CONTROL-GROUPS
000024                                     PIC S9(4)     COMP-3.
000025
000026         16  PI-CONTROL-GROUPS                     COMP
000027             OCCURS 4 TIMES          INDEXED BY PI-INDEX.
000028             20  PI-CONTROL-GROUP    PIC S9(8).
000029             20  PI-HIGH-SEQUENCE    PIC S9(4).
000030
000031         16  PI-CHECK-PRINTER-ID     PIC X(4).
000032
000033         16  PI-PRINTER-STARTED-SW   PIC S9        COMP-3.
000034
000035         16  PI-ASSIGN-CHECK-NUMBERS PIC X.
000036
000037         16  PI-COMPANY-ADDRESS.
000038             20  PI-COMPANY-NAME             PIC X(30).
000039             20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
000040             20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
000041             20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
000042             20  PI-COMPANY-CITY-ST          PIC X(30).
000043             20  PI-COMPANY-ZIP.
000044                 24  PI-COMPANY-ZIP-CODE.
000045                     28  PI-COMPANY-ZIP-1ST  PIC X.
000046                         88  PI-COMPANY-CAN-POST-CODE
000047                                             VALUE 'A' THRU 'Z'.
000048                     28  FILLER              PIC X(4).
000049                 24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
000050             20  PI-CO-CANADIAN-POSTAL-CODE
000051                     REDEFINES PI-COMPANY-ZIP.
000052                 24  PI-CO-CAN-POSTAL-1      PIC XXX.
000053                 24  PI-CO-CAN-POSTAL-2      PIC XXX.
000054                 24  FILLER                  PIC XXX.
000055             20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
000056
000057         16  PI-MONTH-END-SAVE               PIC XX.
000058
000059         16  PI-VALID-RCD-SW                 PIC X.
000060
000061         16  FILLER                          PIC X(419).
000062
      *<<((file: ELC176PI))
000463            16  FILLER PIC XX.
000464
000465     EJECT
000466*    COPY ELCCPA.
      *>>((file: ELCCPA))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCPA.                             *
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
000008******************************************************************
000009******************************************************************
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 072110    2009122800001  AJRA  CHANGE COMMENT,COMMENT-2 FROM 40
000018*                                SEPARATE CITY AND STATE
000019* 013017  CR2016053100001  PEMA  ACH PROCESSING
000020******************************************************************
000021
000022 01  CHECK-PASS-AREA.
000023     12  CPA-ALIGNMENT               PIC S9(3)    COMP-3.
000024     12  CPA-CARRIER                 PIC X.
000025     12  CPA-GROUP                   PIC X(6).
000026     12  CPA-ACCOUNT                 PIC X(10).
000027     12  CPA-STATE                   PIC XX.
000028     12  CPA-CLAIM-NO                PIC X(7).
000029     12  CPA-CERT-NO                 PIC X(11).
000030     12  CPA-CERT-EFF-DT             PIC XX.
000031     12  CPA-CLAIM-TYPE              PIC X.
000032     12  CPA-PAYEE-TYPE-CD           PIC X.
000033     12  CPA-IND-GRP-TYPE            PIC X.
000034         88  CPA-INDIVIDUAL                     VALUE 'I'.
000035         88  CPA-GROUP-POLICY                   VALUE 'G'.
000036     12  CPA-PAYEE-NAME              PIC X(30).
000037     12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
000038     12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
000039     12  CPA-PAYEE-ADDRESS-LINE3     PIC X(30).
000040     12  CPA-PAYEE-CITY-STATE.
000041         16  CPA-PAYEE-CITY          PIC X(28).
000042         16  CPA-PAYEE-STATE         PIC X(2).
000043     12  CPA-PAYEE-ZIP.
000044         16  CPA-PAYEE-ZIP-CODE.
000045             24  CPA-ZIP-1ST-PAYEE   PIC X.
000046                 88  CPA-CAN-POST-CODE-PAYEE  VALUE 'A' THRU 'Z'.
000047             24  FILLER              PIC X(4).
000048         16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
000049     12  CPA-CANADIAN-POSTAL-CODE-PAYEE REDEFINES CPA-PAYEE-ZIP.
000050         16  CPA-CAN-POSTAL-1-PAYEE  PIC XXX.
000051         16  CPA-CAN-POSTAL-2-PAYEE  PIC XXX.
000052         16  FILLER                  PIC XXX.
000053     12  CPA-INSURED-NAME            PIC X(30).
000054     12  CPA-INSURED-ADDRESS-LINE1   PIC X(30).
000055     12  CPA-INSURED-ADDRESS-LINE2   PIC X(30).
000056     12  CPA-INSURED-ADDRESS-LINE3   PIC X(30).
000057     12  CPA-INSURED-CITY-STATE.
000058         16  CPA-INSURED-CITY        PIC X(28).
000059         16  CPA-INSURED-STATE       PIC X(2).
000060     12  CPA-INSURED-ZIP.
000061         16  CPA-INSURED-ZIP-CODE.
000062             24  CPA-ZIP-1ST-INS     PIC X.
000063                 88  CPA-CAN-POST-CODE-INS    VALUE 'A' THRU 'Z'.
000064             24  FILLER              PIC X(4).
000065         16  CPA-INSURED-ZIP-PLUS4   PIC X(4).
000066     12  CPA-CANADIAN-POSTAL-CODE-INS REDEFINES CPA-INSURED-ZIP.
000067         16  CPA-CAN-POSTAL-1-INS    PIC XXX.
000068         16  CPA-CAN-POSTAL-2-INS    PIC XXX.
000069         16  FILLER                  PIC XXX.
000070     12  CPA-INSURED-AGE             PIC 99.
000071     12  CPA-PAYMENT-TYPE            PIC X.
000072     12  CPA-PAYMENT-BY              PIC X(4).
000073     12  CPA-CHECK-DATE              PIC X(2).
000074     12  CPA-CHECK-NUMBER            PIC X(7).
000075     12  CPA-AMOUNT-PAID             PIC S9(7)V99    COMP-3.
000076     12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(7)V99    COMP-3.
000077     12  CPA-DAYS-PAID               PIC S9(5)       COMP-3.
000078     12  CPA-DAILY-RATE              PIC S9(3)V99    COMP-3.
000079     12  CPA-ELIMINATION-DAYS        PIC S9(3)       COMP-3.
000080     12  CPA-CLAIM-CODE              PIC X.
000081     12  CPA-PAY-CODE                PIC X.
000082     12  CPA-INCURRED-DT             PIC XX.
000083     12  CPA-REPORTED-DT             PIC XX.
000084     12  CPA-PAID-THRU-DT            PIC XX.
000085     12  CPA-PAID-FROM-DT            PIC XX.
000086     12  CPA-PAID-DT                 PIC XX.
000087
000088     12  CPA-ACCOUNT-NAME            PIC X(30).
000089     12  CPA-ACCOUNT-IN-CARE-OF      PIC X(30).
000090     12  CPA-ACCOUNT-ADDRESS-LINE1   PIC X(30).
000091     12  CPA-ACCOUNT-ADDRESS-LINE2   PIC X(30).
000092     12  CPA-ACCOUNT-CITY-ST.
000093         16  CPA-ACCOUNT-CITY        PIC X(28).
000094         16  CPA-ACCOUNT-STATE       PIC X(2).
000095     12  CPA-ACCOUNT-ZIP-CODE.
000096         16  CPA-ACCOUNT-ZIP.
000097             24  CPA-ZIP-1ST-ACCT    PIC X.
000098                 88  CPA-CAN-POST-CODE-ACCT VALUE 'A' THRU 'Z'.
000099             24  FILLER              PIC X(4).
000100         16  CPA-ACCOUNT-ZIP-PLUS4   PIC X(4).
000101     12  CPA-CANADIAN-POSTAL-CODE-ACCT
000102             REDEFINES CPA-ACCOUNT-ZIP-CODE.
000103         16  CPA-CAN-POSTAL-1-ACCT   PIC XXX.
000104         16  CPA-CAN-POSTAL-2-ACCT   PIC XXX.
000105         16  FILLER                  PIC XXX.
000106     12  CPA-ACCOUNT-PHONE-NO        PIC S9(11)     COMP-3.
000107
000108     12  CPA-SOC-SEC-NO              PIC X(11).
000109     12  CPA-MEMBER-NUMBER           PIC X(12).
000110     12  CPA-LOAN-NUMBER             PIC X(8).
000111
000112     12  CPA-BENEFIT-TYPE            PIC X.
000113
000114     12  CPA-NO-OF-PMTS-MADE         PIC S9(3)       COMP-3.
000115
000116     12  CPA-EXPIRE-DT               PIC XX.
000117
000118     12  CPA-MONTHLY-BENEFIT         PIC S9(7)V99    COMP-3.
000119
000120     12  CPA-COMMENT                 PIC X(60).
000121
000122     12  CPA-CLAIM-STATUS            PIC X.
000123*      88  CLAIM-IS-OPEN                             VALUE 'O'.
000124*      88  CLAIM-IS-CLOSED                           VALUE 'C'.
000125
000126     12  CPA-LAST-CLOSE-REASON       PIC X.
000127*      88  FINAL-PAID                                VALUE '1'.
000128*      88  CLAIM-DENIED                              VALUE '2'.
000129*      88  AUTO-CLOSE                                VALUE '3'.
000130
000131     12  CPA-INSURED-ADDR-TRLR-NAME  PIC X(30).
000132
000133     12  CPA-EXAMINER                PIC X(4).
000134     12  CPA-EXPENSE-TYPE            PIC X.
000135
000136     12  CPA-CARRIER-ADDRESS-DATA.
000137         16  CPA-CARRIER-NAME            PIC X(30).
000138         16  CPA-CARRIER-ADDRESS-LINE1   PIC X(30).
000139         16  CPA-CARRIER-ADDRESS-LINE2   PIC X(30).
000140         16  CPA-CARRIER-ADDRESS-LINE3   PIC X(30).
000141         16  CPA-CARRIER-CITY-STATE.
000142             24  CPA-CARRIER-CITY        PIC X(28).
000143             24  CPA-CARRIER-STATE       PIC X(2).
000144         16  CPA-CARRIER-ZIP.
000145             24  CPA-CARRIER-ZIP-CODE.
000146                 28  CPA-ZIP-1ST-CARRIER PIC X.
000147                     88  CPA-CAN-POST-CODE-CARR
000148                                         VALUE 'A' THRU 'Z'.
000149                 28  FILLER              PIC X(4).
000150             24  CPA-CARRIER-ZIP-PLUS4   PIC X(4).
000151         16  CPA-CANADIAN-POSTAL-CODE-CARR
000152                           REDEFINES CPA-CARRIER-ZIP.
000153             24  CPA-CAN-POSTAL-1-CARR   PIC XXX.
000154             24  CPA-CAN-POSTAL-2-CARR   PIC XXX.
000155             24  FILLER                  PIC XXX.
000156
000157     12  CPA-PAYMENT-ORIGIN              PIC X.
000158     12  CPA-BENEFIT-CD                  PIC XX.
000159     12  CPA-COMMENT-2                   PIC X(60).
000160
000161     12  CPA-NOTIFY-ADDRESS-DATA.
000162         16  CPA-NOTIFY-NAME             PIC X(30).
000163         16  CPA-NOTIFY-ADDRESS-LINE1    PIC X(30).
000164         16  CPA-NOTIFY-ADDRESS-LINE2    PIC X(30).
000165         16  CPA-NOTIFY-ADDRESS-LINE3    PIC X(30).
000166         16  CPA-NOTIFY-CITY-STATE.
000167             20  CPA-NOTIFY-CITY         PIC X(28).
000168             20  CPA-NOTIFY-STATE        PIC X(2).
000169         16  CPA-NOTIFY-ZIP.
000170             20  CPA-NOTIFY-ZIP-CODE     PIC X(5).
000171             20  CPA-NOTIFY-ZIP-PLUS4    PIC X(4).
000172     12  CPA-COVERAGE-TYPE               PIC X.
000173         88  CPA-LEVEL-COV                 VALUE 'L'.
000174         88  CPA-REDUCE-COV                VALUE 'R'.
000175         88  CPA-PROP-COV                  VALUE 'P'.
000176         88  CPA-DISAB-COV                 VALUE 'A'.
000177
000178     12  CPA-AUTO-PAY-END-DT             PIC X(02).
000179     12  CPA-COMPANY-NAME                PIC X(30).
000180     12  CPA-TOTAL-BENEFIT               PIC S9(09)V99  COMP-3.
000181     12  CPA-FORM-CTL-SEQ-NO             PIC S9(4)      COMP.
000182     12  CPA-BENEFICIARY                 PIC X(25).
000183     12  CPA-REFERENCE-NO.
000184         16  CPA-REFERENCE-PRIME         PIC X(17).
000185         16  CPA-REFERENCE-SFX           PIC X(02).
000186     12  CPA-ACH-PAYMENT                 PIC X.
000187     12  CPA-CV-LOAN-NUMBER              PIC X(20).
000188     12  CPA-CREDIT-CARD-NO REDEFINES
000189         CPA-CV-LOAN-NUMBER              PIC X(20).
      *<<((file: ELCCPA))
000467
000468     EJECT
000469*    COPY ELCNWA.
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
000470
000471*    COPY EL176S.
      *>>((file: EL176S))
000001 01  EL176AI.
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
000016     05  AOPTIONL PIC S9(0004) COMP.
000017     05  AOPTIONF PIC  X(0001).
000018     05  FILLER REDEFINES AOPTIONF.
000019         10  AOPTIONA PIC  X(0001).
000020     05  AOPTIONI PIC  X(0001).
000021*    -------------------------------
000022     05  ACG01L PIC S9(0004) COMP.
000023     05  ACG01F PIC  X(0001).
000024     05  FILLER REDEFINES ACG01F.
000025         10  ACG01A PIC  X(0001).
000026     05  ACG01I PIC  X(0007).
000027*    -------------------------------
000028     05  ACG02L PIC S9(0004) COMP.
000029     05  ACG02F PIC  X(0001).
000030     05  FILLER REDEFINES ACG02F.
000031         10  ACG02A PIC  X(0001).
000032     05  ACG02I PIC  X(0007).
000033*    -------------------------------
000034     05  ACG03L PIC S9(0004) COMP.
000035     05  ACG03F PIC  X(0001).
000036     05  FILLER REDEFINES ACG03F.
000037         10  ACG03A PIC  X(0001).
000038     05  ACG03I PIC  X(0007).
000039*    -------------------------------
000040     05  ACG04L PIC S9(0004) COMP.
000041     05  ACG04F PIC  X(0001).
000042     05  FILLER REDEFINES ACG04F.
000043         10  ACG04A PIC  X(0001).
000044     05  ACG04I PIC  X(0007).
000045*    -------------------------------
000046     05  AALIGNL PIC S9(0004) COMP.
000047     05  AALIGNF PIC  X(0001).
000048     05  FILLER REDEFINES AALIGNF.
000049         10  AALIGNA PIC  X(0001).
000050     05  AALIGNI PIC  X(0001).
000051*    -------------------------------
000052     05  ACKNOL PIC S9(0004) COMP.
000053     05  ACKNOF PIC  X(0001).
000054     05  FILLER REDEFINES ACKNOF.
000055         10  ACKNOA PIC  X(0001).
000056     05  ACKNOI PIC  X(0007).
000057*    -------------------------------
000058     05  AACNL PIC S9(0004) COMP.
000059     05  AACNF PIC  X(0001).
000060     05  FILLER REDEFINES AACNF.
000061         10  AACNA PIC  X(0001).
000062     05  AACNI PIC  X(0001).
000063*    -------------------------------
000064     05  APRTL PIC S9(0004) COMP.
000065     05  APRTF PIC  X(0001).
000066     05  FILLER REDEFINES APRTF.
000067         10  APRTA PIC  X(0001).
000068     05  APRTI PIC  X(0004).
000069*    -------------------------------
000070     05  AEMSG1L PIC S9(0004) COMP.
000071     05  AEMSG1F PIC  X(0001).
000072     05  FILLER REDEFINES AEMSG1F.
000073         10  AEMSG1A PIC  X(0001).
000074     05  AEMSG1I PIC  X(0079).
000075*    -------------------------------
000076     05  AEMSG2L PIC S9(0004) COMP.
000077     05  AEMSG2F PIC  X(0001).
000078     05  FILLER REDEFINES AEMSG2F.
000079         10  AEMSG2A PIC  X(0001).
000080     05  AEMSG2I PIC  X(0079).
000081*    -------------------------------
000082     05  AEMSG3L PIC S9(0004) COMP.
000083     05  AEMSG3F PIC  X(0001).
000084     05  FILLER REDEFINES AEMSG3F.
000085         10  AEMSG3A PIC  X(0001).
000086     05  AEMSG3I PIC  X(0079).
000087*    -------------------------------
000088     05  APFKL PIC S9(0004) COMP.
000089     05  APFKF PIC  X(0001).
000090     05  FILLER REDEFINES APFKF.
000091         10  APFKA PIC  X(0001).
000092     05  APFKI PIC  9(2).
000093 01  EL176AO REDEFINES EL176AI.
000094     05  FILLER            PIC  X(0012).
000095*    -------------------------------
000096     05  FILLER            PIC  X(0003).
000097     05  ADATEO PIC  X(0008).
000098*    -------------------------------
000099     05  FILLER            PIC  X(0003).
000100     05  ATIMEO PIC  99.99.
000101*    -------------------------------
000102     05  FILLER            PIC  X(0003).
000103     05  AOPTIONO PIC  X(0001).
000104*    -------------------------------
000105     05  FILLER            PIC  X(0003).
000106     05  ACG01O PIC  9999999.
000107*    -------------------------------
000108     05  FILLER            PIC  X(0003).
000109     05  ACG02O PIC  9999999.
000110*    -------------------------------
000111     05  FILLER            PIC  X(0003).
000112     05  ACG03O PIC  9999999.
000113*    -------------------------------
000114     05  FILLER            PIC  X(0003).
000115     05  ACG04O PIC  9999999.
000116*    -------------------------------
000117     05  FILLER            PIC  X(0003).
000118     05  AALIGNO PIC  9.
000119*    -------------------------------
000120     05  FILLER            PIC  X(0003).
000121     05  ACKNOO PIC  9999999.
000122*    -------------------------------
000123     05  FILLER            PIC  X(0003).
000124     05  AACNO PIC  X(0001).
000125*    -------------------------------
000126     05  FILLER            PIC  X(0003).
000127     05  APRTO PIC  X(0004).
000128*    -------------------------------
000129     05  FILLER            PIC  X(0003).
000130     05  AEMSG1O PIC  X(0079).
000131*    -------------------------------
000132     05  FILLER            PIC  X(0003).
000133     05  AEMSG2O PIC  X(0079).
000134*    -------------------------------
000135     05  FILLER            PIC  X(0003).
000136     05  AEMSG3O PIC  X(0079).
000137*    -------------------------------
000138     05  FILLER            PIC  X(0003).
000139     05  APFKO PIC  99.
000140*    -------------------------------
      *<<((file: EL176S))
000472
000473 01  FILLER REDEFINES EL176AI.
000474     05  FILLER                      PIC X(35).
000475
000476     05  FILLER                      OCCURS 4 TIMES
000477                                     INDEXED BY EL176A-INDEX.
000478
000479         10  EL176A-CONTROL-GROUP-LENGTH   PIC S9(4)
000480                                           COMP.
000481         10  EL176A-CONTROL-GROUP-ATTRB    PIC X.
000482         10  EL176A-CONTROL-GROUP          PIC 9(7).
000483         10  EL176A-CONTROL-GROUP-X        REDEFINES
000484             EL176A-CONTROL-GROUP          PIC X(7).
000485
000486     EJECT
000487*    COPY ELCJPFX.
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
000488          PIC X(750).
000489
000490     EJECT
000491*    COPY ELCEMIB.
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
000492
000493     EJECT
000494*    COPY ELCDATE.
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
000495     EJECT
000496*    COPY ELCLOGOF.
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
000497
000498     EJECT
000499*    COPY ELCATTR.
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
000500
000501     EJECT
000502*    COPY ELCAID.
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
000503
000504 01  FILLER REDEFINES DFHAID.
000505     05  FILLER                      PIC X(8).
000506     05  PF-VALUES                   PIC X
000507         OCCURS 24 TIMES.
000508
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
000510
000511 01  DFHCOMMAREA                     PIC X(1024).
000512
000513     EJECT
000514*    COPY ELCCHKQ.
      *>>((file: ELCCHKQ))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCHKQ.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.007                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CHECK QUE FILE                            *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 100  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
000014*       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019 01  CHECK-QUE.
000020     12  CQ-RECORD-ID                PIC XX.
000021         88  VALID-CQ-ID         VALUE 'CQ'.
000022
000023     12  CQ-CONTROL-PRIMARY.
000024         16  CQ-COMPANY-CD           PIC X.
000025         16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
000026         16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
000027
000028     12  CQ-CONTROL-BY-PAYEE.
000029         16  CQ-CONTROL-BY-NUMBER.
000030             20  CQ-COMPANY-CD-A1     PIC X.
000031             20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
000032         16  CQ-PAYEE-CARRIER        PIC X.
000033         16  CQ-PAYEE-GROUPING       PIC X(6).
000034         16  CQ-PAYEE-STATE          PIC XX.
000035         16  CQ-PAYEE-BENE-ACCT      PIC X(10).
000036         16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
000037
000038     12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
000039         16  CQ-DMD-COMPANY-CD-A2    PIC X.
000040         16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
000041         16  CQ-DMD-BENE-CODE-A2     PIC X(10).
000042         16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
000043         16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
000044         16  FILLER                  PIC X(3).
000045
000046     12  CQ-ENTRY-TYPE               PIC X.
000047             88  CHECK-ON-QUE           VALUE 'Q'.
000048             88  ALIGNMENT-CHECK        VALUE 'A'.
000049             88  SPOILED-CHECK          VALUE 'S'.
000050             88  PAYMENT-ABORTED        VALUE 'X'.
000051
000052     12  CQ-CLAIM-MAST-CNTL.
000053         16  CQ-CARRIER              PIC X.
000054         16  CQ-CLAIM-NO             PIC X(7).
000055         16  CQ-CERT-NO.
000056             20  CQ-CERT-PRIME       PIC X(10).
000057             20  CQ-CERT-SFX         PIC X.
000058         16  CQ-CLAIM-TYPE           PIC X.
000059             88  CQ-LIFE-CLAIM          VALUE 'L'.
000060             88  CQ-AH-CLAIM            VALUE 'A'.
000061         16  CQ-CLAIM-SUB-TYPE       PIC X.
000062             88  CQ-FIXED-COVERAGE      VALUE '1'.
000063             88  CQ-O-B-COVERAGE        VALUE '2'.
000064             88  CQ-OPEN-END-COVERAGE   VALUE '3'.
000065
000066     12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
000067     12  CQ-CHECK-NUMBER             PIC X(7).
000068     12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
000069     12  CQ-PAYMENT-TYPE             PIC X.
000070             88  CQ-PARTIAL-PAYMENT        VALUE '1'.
000071             88  CQ-FINAL-PAYMENT          VALUE '2'.
000072             88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
000073             88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
000074             88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
000075             88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
000076             88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
000077             88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
000078     12  CQ-VOID-INDICATOR           PIC X.
000079             88  CHECK-IS-STOPPED          VALUE 'S'.
000080             88  CHECK-IS-VOID             VALUE 'V'.
000081     12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
000082     12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
000083     12  CQ-CHECK-BY-USER            PIC X(4).
000084     12  CQ-PRE-NUMBERING-SW         PIC X.
000085       88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
000086       88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
000087
000088     12  CQ-CHECK-WRITTEN-DT         PIC XX.
000089     12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
000090     12  CQ-LEDGER-FLAG              PIC X(01).
000091     12  CQ-VOID-AFTER-LEDGER        PIC X(01).
000092     12  CQ-LAST-UPDATED-DT          PIC XX.
000093     12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
000094     12  CQ-APPLIED-TO-RCON-DT       PIC XX.
000095
000096     12  FILLER                      PIC X(04).
000097
000098******************************************************************
      *<<((file: ELCCHKQ))
000515
000516     EJECT
000517*    COPY ELCCNTL.
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
000518
000519     EJECT
000520*    COPY ELCTRLR.
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
000521
000522     EJECT
000523*    COPY ELCMSTR.
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
000524
000525     EJECT
000526*    COPY ELCCERT.
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
000527
000528     EJECT
000529*    COPY ERCACCT.
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
000530
000531     EJECT
000532*    COPY ELCBENE.
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
000533
000534     EJECT
000535*    COPY ERCCOMP.
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
000536
000537     EJECT
000538*    COPY MPCPLCY.
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
000539
000540     EJECT
000541*    COPY MPCPROD.
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
000542
000543     EJECT
000544*    COPY MPCPLAN.
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
000545
000546     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL176' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000547 VCOBOL-DUMMY-PROCEDURE.
000548
000549     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000550     MOVE '5'                   TO DC-OPTION-CODE.
000551     PERFORM 8500-DATE-CONVERSION.
000552     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000553     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE
000554                                    WS-CHECK-WRITER-DATE.
000555
000556     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000557
000558*    NOTE *******************************************************
000559*         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
000560*         *  FROM ANOTHER MODULE.                               *
000561*         *******************************************************.
000562
000563     IF EIBCALEN NOT GREATER THAN ZERO
000564         MOVE UNACCESS-MSG       TO  LOGOFF-MSG
000565         GO TO 8300-SEND-TEXT.
000566
000567     
      * EXEC CICS HANDLE CONDITION
000568*        PGMIDERR   (9600-PGMIDERR)
000569*        NOTOPEN    (8800-NOT-OPEN)
000570*        NOTFND     (0180-MAIN-LOGIC)
000571*        ENDFILE    (0190-MAIN-LOGIC)
000572*        TERMIDERR  (0900-TERMIDERR)
000573*        ENQBUSY    (0910-ENQ-BUSY)
000574*        ERROR      (9990-ERROR)
000575*    END-EXEC.
      *    MOVE '"$LJI''[).             ! " #00006807' TO DFHEIV0
           MOVE X'22244C4A49275B292E202020' &
                X'202020202020202020202120' &
                X'2220233030303036383037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000576
000577     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
000578     MOVE +2                     TO  EMI-SWITCH2.
000579
000580     EJECT
000581 0010-MAIN-LOGIC.
000582     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000583         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000584             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
000585             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
000586             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
000587             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
000588             MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
000589             MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
000590             MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
000591             MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
000592           ELSE
000593             MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
000594             MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
000595             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
000596             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
000597             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
000598             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
000599             MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
000600             MOVE SPACES               TO  PI-SAVED-PROGRAM-6
000601       ELSE
000602         GO TO 0040-MAIN-LOGIC.
000603
000604 0015-MAIN-LOGIC.
000605*    NOTE *******************************************************
000606*         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *
000607*         *  INTERFACE BLOCK FOR THIS MODULE.                   *
000608*         *******************************************************.
000609
000610     MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
000611
000612     MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS
000613                                     PI-NUMBER-OF-ALIGNMENT-CHECKS
000614                                     PI-ALIGNMENT-CONTROL-GROUP
000615                                     PI-ALIGNMENT-SEQUENCE-NO
000616                                     PI-PROCESSING-SW
000617                                     PI-CONTROL-GROUP (1)
000618                                     PI-CONTROL-GROUP (2)
000619                                     PI-CONTROL-GROUP (3)
000620                                     PI-CONTROL-GROUP (4)
000621                                     PI-HIGH-SEQUENCE (1)
000622                                     PI-HIGH-SEQUENCE (2)
000623                                     PI-HIGH-SEQUENCE (3)
000624                                     PI-HIGH-SEQUENCE (4)
000625                                     PI-COMPANY-ZIP-CODE
000626                                     PI-COMPANY-PHONE-NUMBER.
000627
000628     MOVE WS-PRINTER-STARTED-SW  TO  PI-PRINTER-STARTED-SW.
000629     MOVE WS-TEMP-STORAGE-KEY    TO  PI-TEMP-STORAGE-KEY.
000630
000631     MOVE LOW-VALUES             TO  EL176AI.
000632     MOVE -1                     TO  AOPTIONL.
000633
000634     PERFORM 8100-SEND-INITIAL-MAP.
000635
000636     MOVE 'N' TO MICR-CHK-SW.
000637
000638     EJECT
000639
000640
000641 0030-CHECK-MICR.
000642**   ----------------------------------------
000643**   --> IS.MICR.DRAFTS HANDLE CONDITIONS
000644**   ----------------------------------------
000645
000646     
      * EXEC CICS HANDLE CONDITION
000647*        NOTFND   (0030-MICR-CHK-SW)
000648*        NOTOPEN  (5000-MICR-CLOSED)
000649*        DISABLED (5000-MICR-CLOSED) END-EXEC.
      *    MOVE '"$IJc                 ! # #00006886' TO DFHEIV0
           MOVE X'2224494A6320202020202020' &
                X'202020202020202020202120' &
                X'2320233030303036383836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000650
000651*    MOVE '420C00001CCCCCCCCCC' TO WS-MICR-KEY.
000652     MOVE 'DCC2000000000000000' TO WS-MICR-KEY.
000653
000654     
      * EXEC CICS READ
000655*        DATASET ('MICRDRFT')
000656*        LENGTH  (REC-LGTH)
000657*        RIDFLD  (WS-MICR-KEY)
000658*        INTO (CSO-DRAFT-420C)
000659*        GTEQ
000660*    END-EXEC.
           MOVE 'MICRDRFT' TO DFHEIV1
      *    MOVE '&"IL       G          (   #00006894' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303036383934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CSO-DRAFT-420C, 
                 REC-LGTH, 
                 WS-MICR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000661
000662 0030-MICR-CHK-SW.
000663
000664     MOVE 'Y' TO  MICR-CHK-SW.
000665
000666 0030-MICR-EXIT.
000667     EXIT.
000668**   ----------------------------------------
000669
000670
000671 0040-MAIN-LOGIC.
000672*    NOTE *******************************************************
000673*         *  AFTER THE FIRST TIME THROUGH THE PROPER ATTENTION  *
000674*         *  KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY         *
000675*         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
000676*         *******************************************************.
000677
000678     IF EIBAID = DFHCLEAR
000679         GO TO 9400-CLEAR.
000680
000681     IF PI-PROCESSOR-ID = 'LGXX'
000682         NEXT SENTENCE
000683     ELSE
000684         
      * EXEC CICS READQ TS
000685*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000686*            INTO    (SECURITY-CONTROL)
000687*            LENGTH  (SC-COMM-LENGTH)
000688*            ITEM    (SC-ITEM)
000689*        END-EXEC
      *    MOVE '*$II   L              ''   #00006924' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000690         MOVE SC-CLAIMS-DISPLAY (13)   TO  PI-DISPLAY-CAP
000691         MOVE SC-CLAIMS-UPDATE  (13)   TO  PI-MODIFY-CAP
000692         IF NOT MODIFY-CAP
000693             MOVE 'UPDATE'             TO  SM-READ
000694             PERFORM 9995-SECURITY-VIOLATION
000695             MOVE ER-0070              TO  EMI-ERROR
000696             GO TO 8100-SEND-INITIAL-MAP.
000697
000698     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000699         MOVE LOW-VALUES         TO  EL176AI
000700         MOVE ER-0008               TO  EMI-ERROR
000701         MOVE -1                 TO  APFKL
000702         PERFORM 8200-SEND-DATAONLY.
000703
000704     
      * EXEC CICS RECEIVE
000705*        INTO   (EL176AI)
000706*        MAPSET (WS-MAPSET-NAME)
000707*        MAP    (WS-MAP-NAME)
000708*    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006944' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303036393434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000709
000710     IF APFKL IS GREATER THAN ZERO
000711         IF EIBAID NOT = DFHENTER
000712             MOVE ER-0004        TO  EMI-ERROR
000713             MOVE AL-UNBOF       TO  APFKA
000714             MOVE -1             TO  APFKL
000715             PERFORM 8200-SEND-DATAONLY
000716           ELSE
000717             IF APFKO IS NUMERIC
000718               AND APFKO IS GREATER THAN ZERO
000719               AND APFKO IS LESS THAN '25'
000720                 MOVE PF-VALUES (APFKI)  TO  EIBAID
000721               ELSE
000722                 MOVE ER-0029        TO  EMI-ERROR
000723                 MOVE AL-UNBOF       TO  APFKA
000724                 MOVE -1             TO  APFKL
000725                 PERFORM 8200-SEND-DATAONLY.
000726
000727     IF EIBAID = DFHPF12
000728         MOVE 'EL010'            TO  THIS-PGM
000729         GO TO 9300-XCTL.
000730
000731     IF EIBAID = DFHPF23
000732         GO TO 9000-RETURN-CICS.
000733
000734     IF EIBAID = DFHPF24
000735         MOVE 'EL126'            TO  THIS-PGM
000736         GO TO 9300-XCTL.
000737
000738     IF EIBAID NOT = DFHENTER
000739         MOVE ER-0008               TO  EMI-ERROR
000740         MOVE -1                 TO  APFKL
000741         PERFORM 8200-SEND-DATAONLY.
000742
000743     IF PI-PROCESSING-SW NOT = ZERO
000744         GO TO 0240-MAIN-LOGIC.
000745
000746     EJECT
000747 0100-MAIN-LOGIC.
000748*    NOTE *******************************************************
000749*         *          SYNTAX CHECK THE MAP FIELDS                *
000750*         *******************************************************.
000751
000752
000753     IF MICR-CHK-SW = 'Y'
000754         CONTINUE
000755     ELSE
000756         PERFORM 0030-CHECK-MICR THRU 0030-MICR-EXIT
000757     END-IF.
000758
000759     IF AOPTIONL NOT GREATER THAN ZERO
000760         MOVE -1                    TO  AOPTIONL
000761         MOVE AL-UNBON              TO  AOPTIONA
000762         MOVE ER-0002               TO  EMI-ERROR
000763         PERFORM 8200-SEND-DATAONLY.
000764
000765     IF (AOPTIONI GREATER THAN ZERO AND
000766         AOPTIONI LESS THAN '4')
000767       OR
000768         (PI-COMPANY-ID = ('POS' OR 'WSL' OR 'MLI') AND
000769          AOPTIONI = ('2' OR '3'))
000770             MOVE AL-UNNON           TO  AOPTIONA
000771           ELSE
000772             MOVE -1                 TO  AOPTIONL
000773             MOVE AL-UNBON           TO  AOPTIONA
000774             MOVE ER-0330            TO  EMI-ERROR
000775             PERFORM 9900-ERROR-FORMAT.
000776
000777     IF AALIGNL GREATER THAN ZERO
000778         IF AALIGNI IS NUMERIC
000779             MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
000780             MOVE AL-UNNON     TO  AALIGNA
000781           ELSE
000782             MOVE ER-0365      TO  EMI-ERROR
000783             MOVE -1           TO  AALIGNL
000784             MOVE AL-UNBON     TO  AALIGNA
000785             PERFORM 9900-ERROR-FORMAT.
000786
000787     IF ACKNOL GREATER THAN ZERO
000788         IF ACKNOI IS NUMERIC
000789             MOVE AL-UNNON       TO  ACKNOA
000790             MOVE ACKNOI         TO  WS-CHECK-NUMBER-X
000791           ELSE
000792             MOVE ER-0366        TO  EMI-ERROR
000793             MOVE -1             TO  ACKNOL
000794             MOVE AL-UNBON       TO  ACKNOA
000795             PERFORM 9900-ERROR-FORMAT
000796     ELSE
000797         MOVE ZEROS              TO  WS-CHECK-NUMBER.
000798
000799     IF AACNL GREATER THAN ZERO
000800         MOVE AACNI              TO  PI-ASSIGN-CHECK-NUMBERS
000801         IF AACNI = 'Y' OR 'N'
000802             MOVE AL-UANON       TO  AACNA
000803             MOVE AACNI          TO  PI-ENTRY-CD-1
000804           ELSE
000805             MOVE AL-UABON       TO  AACNA
000806             MOVE -1             TO  AACNL
000807             MOVE ER-0367        TO  EMI-ERROR
000808             PERFORM 9900-ERROR-FORMAT
000809       ELSE
000810         MOVE AL-UABOF           TO  AACNA
000811         MOVE -1                 TO  AACNL
000812         MOVE ER-0368            TO  EMI-ERROR
000813         PERFORM 9900-ERROR-FORMAT.
000814
000815     IF AACNI = 'Y'
000816       AND ACKNOL NOT GREATER THAN ZERO
000817         MOVE -1                 TO  ACKNOL
000818         MOVE AL-UNBOF           TO  ACKNOA
000819         MOVE ER-0392            TO  EMI-ERROR
000820         PERFORM 9900-ERROR-FORMAT.
000821
000822     IF AACNI = 'N'
000823       AND ACKNOL GREATER THAN ZERO
000824         MOVE -1                 TO  ACKNOL
000825         MOVE AL-UNBON           TO  ACKNOA
000826         MOVE ER-0393            TO  EMI-ERROR
000827         PERFORM 9900-ERROR-FORMAT.
000828
000829     EJECT
000830*    NOTE *******************************************************
000831*         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       *
000832*         *  ENTERED.                                           *
000833*         *******************************************************.
000834
000835     SET EL176A-INDEX
000836         PI-INDEX TO +1.
000837
000838 0120-MAIN-LOGIC.
000839
000840     IF PI-COMPANY-ID = 'DMD'
000841         GO TO 0150-DMD-CNTL-GRP-EDIT.
000842
000843     IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
000844                                 NOT GREATER THAN ZERO
000845         MOVE AL-UNNOF  TO  EL176A-CONTROL-GROUP-ATTRB
000846                                                    (EL176A-INDEX)
000847         GO TO 0190-MAIN-LOGIC.
000848
000849     IF EL176A-CONTROL-GROUP (EL176A-INDEX) IS NOT NUMERIC
000850         MOVE AL-UNBON  TO  EL176A-CONTROL-GROUP-ATTRB
000851                                                    (EL176A-INDEX)
000852         MOVE -1  TO  EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
000853         MOVE ER-0369               TO  EMI-ERROR
000854         PERFORM 9900-ERROR-FORMAT
000855         GO TO 0190-MAIN-LOGIC.
000856
000857     MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)
000858                                 TO  PI-CONTROL-GROUP (PI-INDEX).
000859     SET PI-INDEX UP BY +1.
000860     MOVE AL-UNNON  TO  EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
000861
000862     IF PI-INDEX IS GREATER THAN +2
000863       AND PI-CONTROL-GROUP (PI-INDEX - 2)
000864                     NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)
000865         MOVE ER-0385               TO  EMI-ERROR
000866         PERFORM 9900-ERROR-FORMAT
000867         MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
000868         MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB
000869                                                   (EL176A-INDEX).
000870
000871 0125-MAIN-LOGIC.
000872
000873     MOVE ZERO                   TO  WS-NOT-FOUND.
000874
000875     MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.
000876
000877     MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
000878
000879     IF PI-COMPANY-ID NOT = 'DMD'
000880         MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)
000881                                 TO  WS-CQK-CONTROL-NUMBER-A1
000882         MOVE +5                 TO  WS-BROWSE-LENGTH
000883     ELSE
000884         MOVE +1                 TO  WS-BROWSE-LENGTH.
000885
000886     MOVE 'CHKQ'                 TO  FILE-SWITCH.
000887
000888     IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
000889         
      * EXEC CICS STARTBR
000890*            DATASET   (WS-CHECK-QUEUE-AIX-DSID)
000891*            RIDFLD    (WS-CHECK-AIX-KEY)
000892*            GENERIC   EQUAL
000893*            KEYLENGTH (WS-BROWSE-LENGTH)
000894*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00007129' TO DFHEIV0
           MOVE X'262C2020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303037313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-BROWSE-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000895         MOVE +1                 TO  WS-CHECK-QUEUE-BROWSE-SW
000896       ELSE
000897         
      * EXEC CICS RESETBR
000898*            DATASET   (WS-CHECK-QUEUE-AIX-DSID)
000899*            RIDFLD    (WS-CHECK-AIX-KEY)
000900*            GENERIC   EQUAL
000901*            KEYLENGTH (5)
000902*        END-EXEC.
           MOVE 5
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&4   KG    E          &   #00007137' TO DFHEIV0
           MOVE X'26342020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303037313337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000903
000904 0130-MAIN-LOGIC.
000905     
      * EXEC CICS READNEXT
000906*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
000907*        RIDFLD  (WS-CHECK-AIX-KEY)
000908*        SET     (ADDRESS OF CHECK-QUE)
000909*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007145' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037313435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000910
000911     IF WS-CQK-COMPANY-CD-A1 NOT = PI-COMPANY-CD
000912         GO TO 0170-MAIN-LOGIC.
000913
000914     IF PI-COMPANY-ID NOT = 'DMD'
000915         IF WS-CQK-CONTROL-NUMBER-A1 NOT = EL176A-CONTROL-GROUP
000916                                                    (EL176A-INDEX)
000917             GO TO 0170-MAIN-LOGIC
000918         ELSE
000919             NEXT SENTENCE
000920     ELSE
000921         IF CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(1) AND
000922            CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(2) AND
000923            CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(3) AND
000924            CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(4)
000925               GO TO 0130-MAIN-LOGIC.
000926
000927     IF CQ-ENTRY-TYPE NOT = 'Q'
000928         GO TO 0130-MAIN-LOGIC.
000929
000930     IF NOT PI-NO-CARRIER-SECURITY
000931        IF CQ-CARRIER NOT = PI-CARRIER-SECURITY
000932           MOVE ER-2370 TO EMI-ERROR
000933           PERFORM 9900-ERROR-FORMAT
000934           MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
000935           MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB
000936                                              (EL176A-INDEX)
000937           GO TO 0190-MAIN-LOGIC.
000938
000939     IF PI-COMPANY-ID = 'POS' OR 'TAO' OR 'CSL'
000940         MOVE CQ-CARRIER         TO  PI-CARRIER.
000941
000942     IF AOPTIONI = '2'
000943       AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO
000944         GO TO 0190-MAIN-LOGIC.
000945
000946     IF AOPTIONI = '3'
000947       AND CQ-TIMES-PRINTED GREATER THAN ZERO
000948         GO TO 0190-MAIN-LOGIC.
000949
000950     GO TO 0130-MAIN-LOGIC.
000951
000952     EJECT
000953
000954*    NOTE *******************************************************
000955*         *  DMD  - CHECK CONTROL GROUP REQUESTS                *
000956*         *******************************************************.
000957
000958 0150-DMD-CNTL-GRP-EDIT.
000959
000960     IF AOPTIONI = 1
000961         GO TO 0190-MAIN-LOGIC.
000962*****************************************************************
000963***       IF PROCESSING BY CONTROL GROUP DETERMINE THAT THERE   *
000964***       IS A LEAST ONE CONTROL GROUP ENTERED.                 *
000965*****************************************************************
000966
000967     IF EL176A-CONTROL-GROUP-LENGTH (1)
000968                             NOT GREATER THAN ZERO AND
000969        EL176A-CONTROL-GROUP-LENGTH (2)
000970                             NOT GREATER THAN ZERO AND
000971        EL176A-CONTROL-GROUP-LENGTH (3)
000972                             NOT GREATER THAN ZERO AND
000973        EL176A-CONTROL-GROUP-LENGTH (4)
000974                             NOT GREATER THAN ZERO
000975         MOVE -1                 TO  ACG01L
000976         MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A
000977         MOVE ER-0370            TO  EMI-ERROR
000978         PERFORM 9900-ERROR-FORMAT
000979         GO TO 0190-MAIN-LOGIC.
000980
000981*****************************************************************
000982***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE NUMERIC   *
000983*****************************************************************
000984
000985     IF EL176A-CONTROL-GROUP-LENGTH (1)
000986                             GREATER THAN ZERO AND
000987        EL176A-CONTROL-GROUP (1) NOT NUMERIC
000988             MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (1)
000989             MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (1)
000990             MOVE ER-0369     TO  EMI-ERROR
000991             PERFORM 9900-ERROR-FORMAT
000992             GO TO 0190-MAIN-LOGIC.
000993
000994     IF EL176A-CONTROL-GROUP-LENGTH (2)
000995                             GREATER THAN ZERO AND
000996        EL176A-CONTROL-GROUP (2) NOT NUMERIC
000997             MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (2)
000998             MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (2)
000999             MOVE ER-0369     TO  EMI-ERROR
001000             PERFORM 9900-ERROR-FORMAT
001001             GO TO 0190-MAIN-LOGIC.
001002
001003     IF EL176A-CONTROL-GROUP-LENGTH (3)
001004                             GREATER THAN ZERO AND
001005        EL176A-CONTROL-GROUP (3) NOT NUMERIC
001006             MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (3)
001007             MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (3)
001008             MOVE ER-0369     TO  EMI-ERROR
001009             PERFORM 9900-ERROR-FORMAT
001010             GO TO 0190-MAIN-LOGIC.
001011
001012     IF EL176A-CONTROL-GROUP-LENGTH (4)
001013                             GREATER THAN ZERO AND
001014        EL176A-CONTROL-GROUP (4) NOT NUMERIC
001015             MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (4)
001016             MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (4)
001017             MOVE ER-0369     TO  EMI-ERROR
001018             PERFORM 9900-ERROR-FORMAT
001019             GO TO 0190-MAIN-LOGIC.
001020
001021*****************************************************************
001022***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE IN        *
001023***       SEQUENCE.                                             *
001024*****************************************************************
001025
001026     IF EL176A-CONTROL-GROUP-LENGTH (2)
001027                             GREATER THAN ZERO AND
001028        EL176A-CONTROL-GROUP (2) NOT GREATER THAN
001029                                 EL176A-CONTROL-GROUP (1)
001030         MOVE ER-0385            TO  EMI-ERROR
001031         PERFORM 9900-ERROR-FORMAT
001032         MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (2)
001033         MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (2)
001034         GO TO 0190-MAIN-LOGIC.
001035
001036     IF EL176A-CONTROL-GROUP-LENGTH (3)
001037                             GREATER THAN ZERO AND
001038        EL176A-CONTROL-GROUP (3) NOT GREATER THAN
001039                                 EL176A-CONTROL-GROUP (2)
001040         MOVE ER-0385            TO  EMI-ERROR
001041         PERFORM 9900-ERROR-FORMAT
001042         MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (3)
001043         MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (3)
001044         GO TO 0190-MAIN-LOGIC.
001045
001046     IF EL176A-CONTROL-GROUP-LENGTH (4)
001047                             GREATER THAN ZERO AND
001048        EL176A-CONTROL-GROUP (4) NOT GREATER THAN
001049                                 EL176A-CONTROL-GROUP (3)
001050         MOVE ER-0385            TO  EMI-ERROR
001051         PERFORM 9900-ERROR-FORMAT
001052         MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (4)
001053         MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (4)
001054         GO TO 0190-MAIN-LOGIC.
001055
001056*****************************************************************
001057***       MOVE REQUESTED CONTROL GROUPS TO THE PI AREA WITH     *
001058***       ANY SKIP CONTROL FIELDS COMPRESSED.                   *
001059*****************************************************************
001060
001061     MOVE +0                     TO  WS-DMD-CNTL-GRP-COUNT.
001062     SET EL176A-INDEX TO +1.
001063
001064 0150-CNTL-LOOP.
001065
001066     IF WS-DMD-CNTL-GRP-COUNT GREATER THAN +2
001067         GO TO 0150-END-CNTL-LOOP.
001068
001069     IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
001070                                    NOT GREATER THAN ZERO
001071         MOVE EL176A-CONTROL-GROUP (EL176A-INDEX + 1)
001072                                 TO  EL176A-CONTROL-GROUP
001073                                     (EL176A-INDEX)
001074         MOVE SPACES             TO EL176A-CONTROL-GROUP-X
001075                                     (EL176A-INDEX + 1).
001076     SET EL176A-INDEX UP BY +1.
001077     ADD +1  TO  WS-DMD-CNTL-GRP-COUNT.
001078     GO TO 0150-CNTL-LOOP.
001079
001080 0150-END-CNTL-LOOP.
001081
001082     MOVE EL176A-CONTROL-GROUP (1)  TO  PI-CONTROL-GROUP (1).
001083     MOVE EL176A-CONTROL-GROUP (2)  TO  PI-CONTROL-GROUP (2).
001084     MOVE EL176A-CONTROL-GROUP (3)  TO  PI-CONTROL-GROUP (3).
001085     MOVE EL176A-CONTROL-GROUP (4)  TO  PI-CONTROL-GROUP (4).
001086
001087     GO TO 0125-MAIN-LOGIC.
001088
001089 0170-MAIN-LOGIC.
001090     MOVE ER-0394                   TO  EMI-ERROR.
001091     PERFORM 9900-ERROR-FORMAT.
001092     MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).
001093     MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
001094
001095     GO TO 0190-MAIN-LOGIC.
001096
001097 0180-MAIN-LOGIC.
001098     MOVE ER-0387                   TO  EMI-ERROR.
001099     PERFORM 9900-ERROR-FORMAT.
001100     MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).
001101     MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
001102
001103 0190-MAIN-LOGIC.
001104
001105     IF PI-COMPANY-ID = 'DMD'
001106         NEXT SENTENCE
001107     ELSE
001108         IF EL176A-INDEX LESS THAN +4
001109             SET EL176A-INDEX UP BY +1
001110             GO TO 0120-MAIN-LOGIC.
001111
001112     IF WS-CHECK-QUEUE-BROWSE-SW NOT = ZERO
001113         MOVE ZERO               TO  WS-CHECK-QUEUE-BROWSE-SW
001114         
      * EXEC CICS ENDBR
001115*            DATASET (WS-CHECK-QUEUE-AIX-DSID)
001116*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007354' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037333534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001117
001118     IF EMI-FATAL-CTR GREATER THAN ZERO
001119         PERFORM 8200-SEND-DATAONLY.
001120
001121     IF PI-COMPANY-ID = 'DMD'
001122         GO TO 0200-MAIN-LOGIC.
001123
001124     IF AOPTIONI = ('2' OR '3')
001125       AND PI-INDEX NOT GREATER THAN +1
001126         MOVE -1                 TO  ACG01L
001127         MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A
001128         MOVE ER-0370               TO  EMI-ERROR
001129         PERFORM 9900-ERROR-FORMAT.
001130
001131     IF PI-INDEX GREATER THAN +1
001132         NEXT SENTENCE
001133       ELSE
001134         GO TO 0200-MAIN-LOGIC.
001135
001136     SET PI-INDEX DOWN BY +1.
001137     SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX.
001138     SET PI-INDEX
001139         EL176A-INDEX TO +1.
001140
001141 0195-MAIN-LOGIC.
001142     IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO
001143         MOVE PI-CONTROL-GROUP (PI-INDEX)
001144                     TO  EL176A-CONTROL-GROUP (EL176A-INDEX)
001145         MOVE AL-UNNON TO EL176A-CONTROL-GROUP-ATTRB
001146                                                    (EL176A-INDEX)
001147     ELSE
001148         MOVE SPACES TO  EL176A-CONTROL-GROUP-X (EL176A-INDEX)
001149         MOVE AL-UNNOF TO EL176A-CONTROL-GROUP-ATTRB
001150                                                   (EL176A-INDEX).
001151
001152     IF PI-INDEX LESS THAN +4
001153         SET PI-INDEX
001154             EL176A-INDEX UP BY +1
001155         GO TO 0195-MAIN-LOGIC.
001156
001157     SET EL176A-INDEX
001158         PI-INDEX TO +1.
001159
001160     EJECT
001161 0200-MAIN-LOGIC.
001162*    NOTE *******************************************************
001163*         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL. *
001164*         *  NOW DO THE PRE-EDIT.                               *
001165*         *                                                     *
001166*         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    *
001167*         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
001168*         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
001169*         *  STEPS:                                             *
001170*         *                                                     *
001171*         *  1. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
001172*         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
001173*         *     FILE FOR OVERLAPS AND GAPS.                     *
001174*         *                                                     *
001175*         *  2. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       *
001176*         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       *
001177*         *                                                     *
001178*         *  3. IF PRE-NUMBERING IS NOT USED THAT ALL CHECKS    *
001179*         *     HAVE A CHECK NUMBER ASSIGNED.                   *
001180*         *                                                     *
001181*         *  4. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        *
001182*         *                                                     *
001183*         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A *
001184*         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
001185*         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
001186*         *  STEPS:                                             *
001187*         *                                                     *
001188*         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  *
001189*         *     BEEN PREVIOUSLY PRINTED.                        *
001190*         *                                                     *
001191*         *  2. IF THE PRE-NUMBERING SWITCH IS SET IN ANY RECORD*
001192*         *     IT MUST BE SET IN ALL RECORDS.                  *
001193*         *                                                     *
001194*         *  3. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
001195*         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
001196*         *     FILE FOR OVERLAPS AND GAPS.                     *
001197*         *******************************************************.
001198
001199     MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.
001200
001201     
      * EXEC CICS ENQ
001202*        RESOURCE (WS-CHECK-QUEUE-DSID)
001203*        LENGTH   (11)
001204*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00007441' TO DFHEIV0
           MOVE X'32244C202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037343431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001205
001206     
      * EXEC CICS HANDLE CONDITION
001207*        NOTFND  (0225-MAIN-LOGIC)
001208*        ENDFILE (0230-MAIN-LOGIC)
001209*    END-EXEC.
      *    MOVE '"$I''                  ! $ #00007446' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303037343436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001210
001211     MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY
001212                                     WS-LAST-CHECK-QUEUE-KEY.
001213     MOVE +5                     TO  WS-KEY-LENGTH.
001214
001215     MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
001216     MOVE 'CHKQ'                 TO  FILE-SWITCH.
001217
001218     
      * EXEC CICS STARTBR
001219*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
001220*        RIDFLD  (WS-CHECK-AIX-KEY)
001221*        GTEQ
001222*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007458' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303037343538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001223
001224 0210-MAIN-LOGIC.
001225     MOVE EMI-FATAL-CTR         TO  WS-LAST-ERROR-COUNT.
001226
001227     
      * EXEC CICS READNEXT
001228*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
001229*        RIDFLD  (WS-CHECK-AIX-KEY)
001230*        SET     (ADDRESS OF CHECK-QUE)
001231*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007467' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037343637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001232
001233     IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
001234         GO TO 0230-MAIN-LOGIC.
001235
001236     MOVE '1'                   TO  PI-VALID-RCD-SW.
001237
001238*    NOTE *******************************************************
001239*         *      SAVE THE CHECK NUMBER SO AT THE END OF THE     *
001240*         *  BROWSE YOU CAN CHECK FOR GAPS OR OVERLAPS.         *
001241*         *******************************************************.
001242
001243     IF CQ-CHECK-NUMBER GREATER THAN WS-GREATEST-CHECK-NUMBER-X
001244         MOVE CQ-CHECK-NUMBER    TO  WS-GREATEST-CHECK-NUMBER-X.
001245
001246*    NOTE *******************************************************
001247*         *      IF YOU ARE PROCESSING BY GROUPS BYPASS ALL     *
001248*         *  RECORDS IF NOT IN SPECEFIED GROUPS.  SAVE THE HIGH *
001249*         *  SEQUENCE NUMBER IN EACH GROUP FOR REPRINT.         *
001250*         *******************************************************.
001251
001252     IF AOPTIONI = '2' OR '3'
001253         NEXT SENTENCE
001254       ELSE
001255         GO TO 0213-MAIN-LOGIC.
001256
001257     SET PI-INDEX
001258         EL176A-INDEX TO +1.
001259
001260 0212-MAIN-LOGIC.
001261     IF CQ-CONTROL-NUMBER  = PI-CONTROL-GROUP (PI-INDEX)
001262         IF CQ-SEQUENCE-NUMBER GREATER PI-HIGH-SEQUENCE (PI-INDEX)
001263             MOVE CQ-SEQUENCE-NUMBER
001264                                 TO PI-HIGH-SEQUENCE (PI-INDEX)
001265             GO TO 0215-MAIN-LOGIC
001266           ELSE
001267             GO TO 0215-MAIN-LOGIC.
001268
001269     IF PI-INDEX LESS THAN +4
001270         SET PI-INDEX
001271             EL176A-INDEX UP BY +1
001272         GO TO 0212-MAIN-LOGIC.
001273
001274     GO TO 0210-MAIN-LOGIC.
001275
001276 0213-MAIN-LOGIC.
001277*    NOTE *******************************************************
001278*         *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  *
001279*         *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. *
001280*         *******************************************************.
001281
001282     IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP
001283         MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP
001284         MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED.
001285
001286     IF WS-TIMES-PRINTED GREATER THAN ZERO
001287         GO TO 0210-MAIN-LOGIC.
001288
001289     EJECT
001290 0215-MAIN-LOGIC.
001291     IF AOPTIONI = '1' OR '2'
001292         NEXT SENTENCE
001293       ELSE
001294         GO TO 0220-MAIN-LOGIC.
001295
001296     IF CQ-TIMES-PRINTED GREATER THAN ZERO
001297        MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
001298        MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
001299        MOVE ER-0379                TO  EMI-ERROR
001300        PERFORM 9900-ERROR-FORMAT.
001301
001302     IF AACNI = 'Y'
001303       AND CQ-CHECK-NUMBER NOT = SPACES
001304         MOVE ER-0382            TO  EMI-ERROR
001305         PERFORM 9900-ERROR-FORMAT
001306         MOVE -1                 TO  AACNL
001307         MOVE AL-UABON           TO  AACNA
001308       ELSE
001309     IF AACNI = 'N'
001310       AND CQ-CHECK-NUMBER = SPACES
001311         MOVE -1                 TO  AACNL
001312         MOVE AL-UABON           TO  AACNA
001313         MOVE ER-0383            TO  EMI-ERROR
001314         PERFORM 9900-ERROR-FORMAT.
001315
001316     GO TO 0210-MAIN-LOGIC.
001317
001318 0220-MAIN-LOGIC.
001319     IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO
001320        MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
001321        MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
001322        MOVE ER-0389                TO  EMI-ERROR
001323        PERFORM 9900-ERROR-FORMAT.
001324
001325     IF CQ-PRE-NUMBERING-SW = '1'
001326       AND AACNI = 'N'
001327         MOVE -1                 TO  AACNL
001328         MOVE AL-UABON           TO  AACNA
001329         MOVE ER-0390            TO  EMI-ERROR
001330         PERFORM 9900-ERROR-FORMAT.
001331
001332     IF CQ-PRE-NUMBERING-SW = SPACES
001333       AND AACNI = 'Y'
001334         MOVE -1                 TO  AACNL
001335         MOVE AL-UABON           TO  AACNA
001336         MOVE ER-0391            TO  EMI-ERROR
001337         PERFORM 9900-ERROR-FORMAT.
001338
001339     GO TO 0210-MAIN-LOGIC.
001340
001341 0225-MAIN-LOGIC.
001342     MOVE ER-0490                TO  EMI-ERROR
001343     MOVE -1                     TO  AOPTIONL
001344     PERFORM 8200-SEND-DATAONLY
001345     PERFORM 9100-RETURN-TRAN.
001346
001347     EJECT
001348 0230-MAIN-LOGIC.
001349     
      * EXEC CICS ENDBR
001350*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
001351*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007589' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037353839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001352
001353     IF PI-VALID-RCD-SW NOT = '1'
001354         MOVE ER-2936            TO  EMI-ERROR
001355         MOVE -1                 TO  AOPTIONL
001356         GO TO 0015-MAIN-LOGIC.
001357
001358     IF EMI-FATAL-CTR GREATER THAN ZERO
001359         PERFORM 8200-SEND-DATAONLY.
001360
001361*    NOTE *******************************************************
001362*         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  *
001363*         *  TO GET THE CICS/VS PRINTER TERMINAL ID AND CHECK   *
001364*         *  TO SEE IF THE PRINTER HAS BEEN SPECIFIED.          *
001365*         *******************************************************.
001366
001367     MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
001368     MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
001369     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
001370     MOVE 'CNTL'                 TO  FILE-SWITCH.
001371
001372     
      * EXEC CICS READ
001373*        DATASET (WS-CONTROL-FILE-DSID)
001374*        RIDFLD  (WS-CONTROL-FILE-KEY)
001375*        SET    (ADDRESS OF CONTROL-FILE)
001376*    END-EXEC.
      *    MOVE '&"S        E          (   #00007612' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037363132' TO DFHEIV0
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
           
001377
001378     IF CF-CHECK-PRINTER-ID = SPACES
001379         MOVE ER-0371            TO  EMI-ERROR
001380         MOVE -1                 TO  APFKL
001381         PERFORM 9900-ERROR-FORMAT.
001382
001383     IF APRTL GREATER THAN ZEROS
001384         MOVE AL-UANON           TO  APRTA
001385         MOVE APRTI              TO  PI-CHECK-PRINTER-ID
001386     ELSE
001387         MOVE CF-CHECK-PRINTER-ID
001388                                 TO  PI-CHECK-PRINTER-ID.
001389
001390     MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.
001391     MOVE CF-CURRENT-MONTH-END   TO  PI-MONTH-END-SAVE.
001392
001393     IF PI-COMPANY-ID = 'ADL' OR 'FLB' OR 'ALA' OR 'FND'
001394        NEXT SENTENCE
001395     ELSE
001396        GO TO 0235-MAIN-LOGIC.
001397
001398     MOVE SAVE-BIN-DATE     TO DC-BIN-DATE-1.
001399     MOVE PI-MONTH-END-SAVE TO DC-BIN-DATE-2.
001400     MOVE '1'               TO DC-OPTION-CODE.
001401     PERFORM 8500-DATE-CONVERSION.
001402
001403     IF NO-CONVERSION-ERROR
001404        IF DC-ELAPSED-MONTHS GREATER THAN +0
001405           MOVE DC-GREG-DATE-2-EDIT  TO WS-WORK-DATE
001406           MOVE '01'                 TO WS-WORK-DA
001407           MOVE WS-WORK-DATE         TO DC-GREG-DATE-1-EDIT
001408           MOVE '2'                  TO DC-OPTION-CODE
001409           PERFORM 8500-DATE-CONVERSION
001410           IF NO-CONVERSION-ERROR
001411              MOVE DC-BIN-DATE-1     TO WS-CHECK-WRITER-DATE
001412              MOVE ZEROS             TO DC-ELAPSED-MONTHS
001413              IF DC-DAY-OF-WEEK = +1
001414                 MOVE +1             TO DC-ELAPSED-DAYS
001415                 MOVE '6'            TO DC-OPTION-CODE
001416                 PERFORM 8500-DATE-CONVERSION
001417                 IF NO-CONVERSION-ERROR
001418                    MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE
001419                 ELSE
001420                    NEXT SENTENCE
001421              ELSE
001422              IF DC-DAY-OF-WEEK = +7
001423                 MOVE +2          TO DC-ELAPSED-DAYS
001424                 MOVE '6'         TO DC-OPTION-CODE
001425                 PERFORM 8500-DATE-CONVERSION
001426                 IF NO-CONVERSION-ERROR
001427                    MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE.
001428
001429 0235-MAIN-LOGIC.
001430
001431     IF AACNI = 'Y'
001432         IF WS-GREATEST-CHECK-NUMBER-X NOT LESS THAN ACKNOI
001433             MOVE ER-0380        TO  EMI-ERROR
001434             PERFORM 9900-ERROR-FORMAT
001435             MOVE -1             TO  ACKNOL
001436             MOVE AL-UNBON       TO  ACKNOA
001437           ELSE
001438             SUBTRACT +1 FROM ACKNOO GIVING WS-ACKNO
001439             IF WS-GREATEST-CHECK-NUMBER-X NOT = WS-ACKNO-X
001440                 MOVE ER-0381    TO  EMI-ERROR
001441                 PERFORM 9900-ERROR-FORMAT
001442                 MOVE -1         TO  ACKNOL
001443                 MOVE AL-UNBON   TO  ACKNOA.
001444
001445*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX'
001446*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX' AND 'CID' AND 'CSO'
001447     GO TO 0237-MAIN-LOGIC.
001448
001449     
      * EXEC CICS HANDLE CONDITION
001450*        NOTOPEN    (8800-NOT-OPEN)
001451*        NOTFND     (0236-MAIN-LOGIC)
001452*    END-EXEC.
      *    MOVE '"$JI                  ! % #00007689' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303037363839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001453
001454     MOVE 'MICR'                 TO  WS-DMD-FLAG-KEY.
001455     MOVE 'FLAG'                 TO  FILE-SWITCH.
001456
001457     
      * EXEC CICS READ
001458*        DATASET ('MICRFLAG')
001459*        RIDFLD  (WS-DMD-FLAG-KEY)
001460*        INTO    (WS-DMD-MICR-FLAG)
001461*    END-EXEC.
           MOVE LENGTH OF
            WS-DMD-MICR-FLAG
             TO DFHEIV11
           MOVE 'MICRFLAG' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00007697' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037363937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DMD-MICR-FLAG, 
                 DFHEIV11, 
                 WS-DMD-FLAG-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001462
001463 0236-MAIN-LOGIC.
001464
001465     
      * EXEC CICS HANDLE CONDITION
001466*        NOTOPEN    (8800-NOT-OPEN)
001467*        NOTFND     (0237-MAIN-LOGIC)
001468*    END-EXEC.
      *    MOVE '"$JI                  ! & #00007705' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303037373035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001469
001470     MOVE '420E'                 TO  WS-DMD-DRFT-KEY.
001471     MOVE 'DRFT'                 TO  FILE-SWITCH.
001472
001473     
      * EXEC CICS READ
001474*        DATASET ('MICRDRFT')
001475*        RIDFLD  (WS-DMD-DRFT-KEY)
001476*        INTO    (WS-DMD-MICR-DRFT)
001477*    END-EXEC.
           MOVE LENGTH OF
            WS-DMD-MICR-DRFT
             TO DFHEIV11
           MOVE 'MICRDRFT' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00007713' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037373133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DMD-MICR-DRFT, 
                 DFHEIV11, 
                 WS-DMD-DRFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001478
001479     MOVE '    '                 TO  FILE-SWITCH.
001480
001481 0237-MAIN-LOGIC.
001482
001483     IF EMI-FATAL-CTR GREATER THAN ZERO
001484         PERFORM 8200-SEND-DATAONLY.
001485
001486     MOVE AL-SANON               TO  AOPTIONA
001487                                     ACG01A
001488                                     ACG02A
001489                                     ACG03A
001490                                     ACG04A
001491                                     AACNA
001492                                     ACKNOA
001493                                     APRTA.
001494
001495     MOVE +1                     TO  PI-PROCESSING-SW.
001496     MOVE -1                     TO  AALIGNL.
001497
001498     IF PI-NUMBER-OF-ALIGNMENT-CHECKS GREATER THAN ZERO
001499         MOVE ER-0361               TO  EMI-ERROR
001500       ELSE
001501         MOVE ER-0362               TO  EMI-ERROR.
001502
001503     PERFORM 8200-SEND-DATAONLY.
001504
001505     
      * EXEC CICS HANDLE AID
001506*        CLEAR (9400-CLEAR)
001507*        PA1   (0040-MAIN-LOGIC)
001508*        PA2   (0040-MAIN-LOGIC)
001509*        PA3   (0040-MAIN-LOGIC)
001510*    END-EXEC.
      *    MOVE '"&=!"#               V! '' #00007745' TO DFHEIV0
           MOVE X'22263D212223202020202020' &
                X'202020202020202020562120' &
                X'2720233030303037373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001511
001512     
      * EXEC CICS SYNCPOINT
001513*    END-EXEC.
      *    MOVE '6"                    !   #00007752' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303037373532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001514
001515     GO TO 0040-MAIN-LOGIC.
001516
001517     EJECT
001518 0240-MAIN-LOGIC.
001519     IF AALIGNL GREATER THAN ZERO
001520         IF AALIGNI IS NUMERIC
001521             MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
001522             MOVE AL-UNNON     TO  AALIGNA
001523           ELSE
001524             MOVE ER-0365      TO  EMI-ERROR
001525             MOVE -1           TO  AALIGNL
001526             MOVE AL-UNBON     TO  AALIGNA
001527             PERFORM 8200-SEND-DATAONLY
001528             GO TO 0040-MAIN-LOGIC.
001529
001530     IF PI-NUMBER-OF-ALIGNMENT-CHECKS NOT GREATER THAN ZERO
001531         GO TO 0300-MAIN-LOGIC.
001532
001533     IF PI-ALIGNMENT-CONTROL-GROUP GREATER THAN ZERO
001534         GO TO 0245-MAIN-LOGIC.
001535
001536     MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
001537     MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
001538     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
001539     MOVE 'CNTL'                 TO  FILE-SWITCH.
001540
001541     
      * EXEC CICS READ UPDATE
001542*        DATASET (WS-CONTROL-FILE-DSID)
001543*        RIDFLD  (WS-CONTROL-FILE-KEY)
001544*        SET    (ADDRESS OF CONTROL-FILE)
001545*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007781' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037373831' TO DFHEIV0
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
           
001546
001547     ADD +1  TO  CF-CO-CHECK-QUE-COUNTER.
001548
001549     IF CO-QUE-COUNT-RESET
001550         MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.
001551
001552     MOVE CF-CO-CHECK-QUE-COUNTER TO PI-ALIGNMENT-CONTROL-GROUP.
001553
001554     
      * EXEC CICS REWRITE
001555*        DATASET (WS-CONTROL-FILE-DSID)
001556*        FROM    (CONTROL-FILE)
001557*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007794' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037373934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001558
001559     MOVE EIBTRMID               TO  PI-TSK-TERM-ID.
001560     MOVE EIBTIME                TO  PI-TSK-TIME.
001561     MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.
001562     IF PI-COMPANY-ID = 'COM'
001563         MOVE 'COM'              TO  WS-CHECK-TRANSID-2.
001564
001565     IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'
001566         MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.
001567
001568     IF PI-COMPANY-ID = 'KSM'
001569         MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.
001570
001571     IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'
001572         MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.
001573
001574     IF PI-COMPANY-ID = 'FGL'
001575         MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.
001576
001577     IF PI-COMPANY-ID = 'LAP'
001578         MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.
001579
001580     IF PI-COMPANY-ID = 'TII'
001581         MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.
001582
001583     IF PI-COMPANY-ID = 'CVL'  OR  'CNL'
001584         MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.
001585
001586     IF PI-COMPANY-ID = 'AUK'
001587         MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.
001588
001589     IF PI-COMPANY-ID = 'BPI'
001590         MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.
001591
001592     IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'
001593                              OR 'FRS' OR 'FRT' OR 'FRH'
001594                              OR 'OFI' OR 'OFJ' OR 'CAB'
001595                              OR 'AFL' OR 'AFC' OR 'SRL'
001596                              OR 'RIC'
001597         MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.
001598
001599     IF PI-COMPANY-ID = 'JAL' OR 'JAI'
001600        NEXT SENTENCE
001601     ELSE
001602         IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'
001603             
      * EXEC CICS START
001604*                TRANSID ('EX47')
001605*                FROM    (PROGRAM-INTERFACE-BLOCK)
001606*                LENGTH  (PI-COMM-LENGTH)
001607*                TERMID  ('A199')
001608*            END-EXEC
           MOVE 'EX47' TO DFHEIV5
      *    MOVE '0( LF                 1   #00007843' TO DFHEIV0
           MOVE X'3028204C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303037383433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV5, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001609         ELSE
001610             
      * EXEC CICS START
001611*                TRANSID (WS-CHECK-WRITER-TRANS-ID)
001612*                FROM    (PROGRAM-INTERFACE-BLOCK)
001613*                LENGTH  (PI-COMM-LENGTH)
001614*                TERMID  (PI-CHECK-PRINTER-ID)
001615*            END-EXEC.
      *    MOVE '0( LFT                1   #00007850' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303037383530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-CHECK-PRINTER-ID, 
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
           
001616
001617     MOVE +1                     TO  PI-PRINTER-STARTED-SW.
001618
001619     
      * EXEC CICS GETMAIN
001620*        SET     (ADDRESS OF CHECK-QUE)
001621*        LENGTH  (100)
001622*        INITIMG (WS-SPACES)
001623*    END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007859' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037383539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001624
001625 0245-MAIN-LOGIC.
001626     MOVE SPACES                 TO  CHECK-QUE.
001627
001628     MOVE 'CQ'                   TO  CQ-RECORD-ID.
001629
001630     MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE.
001631     MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD
001632                                     CQ-COMPANY-CD-A1.
001633     MOVE 'A'                    TO  CQ-ENTRY-TYPE.
001634
001635     MOVE PI-ALIGNMENT-CONTROL-GROUP
001636                                 TO  CQ-CONTROL-NUMBER
001637                                     CQ-CONTROL-NUMBER-A1.
001638
001639     MOVE ZERO                   TO  CQ-CHECK-AMOUNT.
001640     MOVE +1                     TO  CQ-TIMES-PRINTED.
001641     MOVE WS-CHECK-WRITER-DATE   TO  CQ-CHECK-WRITTEN-DT.
001642
001643     MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
001644     MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
001645     MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.
001646     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001647     MOVE '5'                    TO  DC-OPTION-CODE.
001648     PERFORM 8500-DATE-CONVERSION.
001649     MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
001650
001651 0250-MAIN-LOGIC.
001652     IF AACNI = 'Y'
001653         MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
001654         ADD +1  TO  WS-CHECK-NUMBER
001655         MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
001656
001657     MOVE PI-ALIGNMENT-SEQUENCE-NO  TO  CQ-SEQUENCE-NUMBER
001658                                 CQ-SEQUENCE-NUMBER-A1.
001659     ADD +1  TO  PI-ALIGNMENT-SEQUENCE-NO.
001660
001661     MOVE SPACES                 TO  CHECK-PASS-AREA.
001662
001663     MOVE +1                     TO  CPA-ALIGNMENT.
001664     MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER.
001665
001666     IF PI-COMPANY-ID = 'CSL'
001667         MOVE PI-CARRIER         TO  CPA-CARRIER.
001668
001669     PERFORM 0800-PRINT-CHECK.
001670
001671     IF AACNI = 'Y'
001672         
      * EXEC CICS WRITE
001673*            DATASET (WS-CHECK-QUEUE-DSID)
001674*            RIDFLD  (CQ-CONTROL-PRIMARY)
001675*            FROM    (CHECK-QUE)
001676*        END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007912' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037393132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001677
001678     SUBTRACT +1 FROM PI-NUMBER-OF-ALIGNMENT-CHECKS.
001679
001680     IF PI-NUMBER-OF-ALIGNMENT-CHECKS IS GREATER THAN ZERO
001681         GO TO 0250-MAIN-LOGIC.
001682
001683     MOVE SPACES                    TO  AALIGNI.
001684     MOVE AL-UNNOF                  TO  AALIGNA.
001685     MOVE -1                        TO  AALIGNL.
001686     MOVE ER-0362                   TO  EMI-ERROR.
001687     PERFORM 8200-SEND-DATAONLY.
001688
001689     
      * EXEC CICS SYNCPOINT
001690*    END-EXEC.
      *    MOVE '6"                    !   #00007929' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303037393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001691
001692     GO TO 0040-MAIN-LOGIC.
001693
001694     EJECT
001695 0300-MAIN-LOGIC.
001696     MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.
001697
001698     IF PI-COMPANY-ID = 'COM'
001699         MOVE 'COM'              TO  WS-CHECK-TRANSID-2.
001700
001701     IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'
001702         MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.
001703
001704
001705     IF PI-COMPANY-ID = 'KSM'
001706         MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.
001707
001708     IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'
001709         MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.
001710
001711     IF PI-COMPANY-ID = 'FGL'
001712         MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.
001713
001714     IF PI-COMPANY-ID = 'LAP'
001715         MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.
001716
001717     IF PI-COMPANY-ID = 'TII'
001718         MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.
001719
001720     IF PI-COMPANY-ID = 'CVL'  OR  'CNL'
001721         MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.
001722
001723     IF PI-COMPANY-ID = 'AUK'
001724         MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.
001725
001726     IF PI-COMPANY-ID = 'BPI'
001727         MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.
001728
001729     IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'
001730                              OR 'FRT' OR 'FRH' OR 'FRS'
001731                              OR 'OFI' OR 'OFJ' OR 'CAB'
001732                              OR 'AFL' OR 'AFC' OR 'SRL'
001733                              OR 'RIC'
001734         MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.
001735
001736*    IF PI-COMPANY-ID = 'CSL'
001737*        MOVE 'E'                TO  WS-CHECK-TRANSID-1
001738*        MOVE 'X47'              TO  WS-CHECK-TRANSID-2.
001739
001740     IF PI-PRINTER-STARTED-SW = ZERO
001741        MOVE +1                     TO  PI-PRINTER-STARTED-SW
001742        MOVE EIBTRMID               TO  PI-TSK-TERM-ID
001743        MOVE EIBTIME                TO  PI-TSK-TIME
001744        IF PI-COMPANY-ID = 'JAL' OR 'JAI'
001745           NEXT SENTENCE
001746        ELSE
001747        IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'
001748             
      * EXEC CICS START
001749*                TRANSID ('EX47')
001750*                FROM    (PROGRAM-INTERFACE-BLOCK)
001751*                LENGTH  (PI-COMM-LENGTH)
001752*                TERMID  ('A199')
001753*            END-EXEC
           MOVE 'EX47' TO DFHEIV5
      *    MOVE '0( LF                 1   #00007988' TO DFHEIV0
           MOVE X'3028204C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303037393838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV5, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001754        ELSE
001755           
      * EXEC CICS START
001756*               TRANSID (WS-CHECK-WRITER-TRANS-ID)
001757*               FROM    (PROGRAM-INTERFACE-BLOCK)
001758*               LENGTH  (PI-COMM-LENGTH)
001759*               TERMID  (PI-CHECK-PRINTER-ID)
001760*          END-EXEC.
      *    MOVE '0( LFT                1   #00007995' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303037393935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-CHECK-PRINTER-ID, 
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
           
001761
001762     SET PI-INDEX TO +1.
001763
001764     MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.
001765     MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
001766
001767     IF PI-COMPANY-ID NOT = 'DMD'
001768         MOVE +5                 TO  WS-KEY-LENGTH
001769     ELSE
001770         MOVE +1                 TO  WS-KEY-LENGTH.
001771
001772 0310-MAIN-LOGIC.
001773     MOVE LOW-VALUES             TO  WS-LAST-CHECK-QUEUE-KEY.
001774     MOVE 'CHKQ'                 TO  FILE-SWITCH.
001775
001776     IF AOPTIONI = '1'
001777         
      * EXEC CICS STARTBR
001778*            DATASET (WS-CHECK-QUEUE-AIX-DSID)
001779*            RIDFLD  (WS-CHECK-AIX-KEY)
001780*            GTEQ
001781*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008017' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038303137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001782     ELSE
001783         IF PI-COMPANY-ID NOT = 'DMD'
001784             MOVE PI-CONTROL-GROUP (PI-INDEX)
001785                                 TO WS-CQK-CONTROL-NUMBER-A1
001786             ADD +1  PI-HIGH-SEQUENCE (PI-INDEX)
001787                                  GIVING WS-SEQUENCE-NUMBER
001788             MOVE ZERO               TO  WS-CQK-SEQUENCE-NUMBER-A1
001789             END-IF
001790             IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
001791                 
      * EXEC CICS STARTBR
001792*                    DATASET   (WS-CHECK-QUEUE-AIX-DSID)
001793*                    RIDFLD    (WS-CHECK-AIX-KEY)
001794*                    KEYLENGTH (WS-KEY-LENGTH)
001795*                    GENERIC   EQUAL
001796*                END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00008031' TO DFHEIV0
           MOVE X'262C2020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303038303331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001797                 MOVE +1         TO  WS-CHECK-QUEUE-BROWSE-SW
001798             ELSE
001799                 
      * EXEC CICS RESETBR
001800*                    DATASET   (WS-CHECK-QUEUE-AIX-DSID)
001801*                    RIDFLD    (WS-CHECK-AIX-KEY)
001802*                    KEYLENGTH (WS-KEY-LENGTH)
001803*                    GENERIC   EQUAL
001804*                END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4   KG    E          &   #00008039' TO DFHEIV0
           MOVE X'26342020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303038303339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001805
001806     EJECT
001807 0320-MAIN-LOGIC.
001808     
      * EXEC CICS HANDLE CONDITION
001809*        NOTFND
001810*        ENDFILE (0390-MAIN-LOGIC)
001811*    END-EXEC.
      *    MOVE '"$¹''                  ! ( #00008048' TO DFHEIV0
           MOVE X'2224B9272020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303038303438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001812
001813 0325-MAIN-LOGIC.
001814     
      * EXEC CICS READNEXT
001815*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
001816*        RIDFLD  (WS-CHECK-AIX-KEY)
001817*        SET     (ADDRESS OF CHECK-QUE)
001818*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008054' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038303534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001819
001820     IF CQ-PRE-NUMBERING-SW NOT = '1'
001821         IF WS-CHECK-AIX-KEY NOT GREATER WS-LAST-CHECK-QUEUE-KEY
001822             GO TO 0325-MAIN-LOGIC.
001823
001824     IF CQ-PRE-NUMBERING-SW NOT = '1'
001825         MOVE WS-CHECK-AIX-KEY   TO  WS-LAST-CHECK-QUEUE-KEY.
001826
001827     IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
001828         GO TO 0390-MAIN-LOGIC.
001829
001830     IF PI-COMPANY-ID NOT = 'DMD'
001831         IF AOPTIONI = '2' OR '3'
001832             IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (PI-INDEX)
001833                 IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
001834                                      PI-HIGH-SEQUENCE (PI-INDEX)
001835                     GO TO 0325-CONT-LOGIC
001836                 ELSE
001837                     GO TO 0325-MAIN-LOGIC
001838             ELSE
001839                 GO TO 0390-MAIN-LOGIC
001840         ELSE
001841             GO TO 0325-CONT-LOGIC.
001842
001843     IF AOPTIONI NOT = '2' AND '3'
001844         GO TO 0325-CONT-LOGIC.
001845
001846     IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (1)
001847         IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
001848                                   PI-HIGH-SEQUENCE (1)
001849             GO TO 0325-CONT-LOGIC
001850         ELSE
001851             GO TO 0325-MAIN-LOGIC
001852     ELSE
001853         IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (2)
001854             IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
001855                                       PI-HIGH-SEQUENCE (2)
001856                 GO TO 0325-CONT-LOGIC
001857             ELSE
001858                 GO TO 0325-MAIN-LOGIC
001859         ELSE
001860             IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (3)
001861                 IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
001862                                       PI-HIGH-SEQUENCE (3)
001863                     GO TO 0325-CONT-LOGIC
001864                 ELSE
001865                     GO TO 0325-MAIN-LOGIC
001866             ELSE
001867                 IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (4)
001868                     IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
001869                                       PI-HIGH-SEQUENCE (4)
001870                         GO TO 0325-CONT-LOGIC
001871                     ELSE
001872                         GO TO 0325-MAIN-LOGIC
001873                 ELSE
001874                     GO TO 0325-MAIN-LOGIC.
001875
001876 0325-CONT-LOGIC.
001877     IF CQ-ENTRY-TYPE NOT = 'Q'
001878         GO TO 0325-MAIN-LOGIC.
001879
001880     IF (AOPTIONI NOT = '3' AND
001881         CQ-TIMES-PRINTED GREATER THAN ZERO)
001882       OR
001883        (AOPTIONI = '3' AND
001884         CQ-TIMES-PRINTED NOT GREATER THAN ZERO)
001885             GO TO 0325-MAIN-LOGIC.
001886
001887     IF AOPTIONI = '1'
001888        IF NOT PI-NO-CARRIER-SECURITY
001889           IF PI-CARRIER-SECURITY NOT = CQ-CARRIER
001890              GO TO 0325-MAIN-LOGIC.
001891
001892     MOVE CQ-CONTROL-PRIMARY         TO  WS-CHECK-QUEUE-KEY.
001893
001894     
      * EXEC CICS ENDBR
001895*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
001896*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008134' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038313334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001897
001898     
      * EXEC CICS READ UPDATE
001899*        DATASET (WS-CHECK-QUEUE-DSID)
001900*        RIDFLD  (WS-CHECK-QUEUE-KEY)
001901*        SET     (ADDRESS OF CHECK-QUE)
001902*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008138' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038313338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001903
001904     MOVE CHECK-QUE              TO  WS-OLD-CHECK-QUEUE-RECORD.
001905
001906     IF AACNI = 'Y'
001907         MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
001908         ADD +1  TO  WS-CHECK-NUMBER
001909         MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
001910
001911*    NOTE *******************************************************
001912*         *                PRINT THE CHECK                      *
001913*         *******************************************************.
001914
001915     MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD
001916                                     WS-CM-COMPANY-CD.
001917     MOVE CQ-CARRIER             TO  WS-CK-CARRIER
001918                                     CPA-CARRIER.
001919     MOVE CQ-CLAIM-NO            TO  WS-CK-CLAIM-NO
001920                                     CPA-CLAIM-NO.
001921     MOVE CQ-CERT-NO             TO  WS-CK-CERT-NO
001922                                     CPA-CERT-NO.
001923     MOVE 'MSTR'                 TO  FILE-SWITCH.
001924
001925     IF CQ-CARRIER NOT = WS-LAST-CARRIER
001926         PERFORM 1000-GET-CARRIER-NAME.
001927
001928*    NOTE *******************************************************
001929*         *            READ THE CLAIM MASTER RECORD             *
001930*         *******************************************************.
001931
001932     
      * EXEC CICS READ
001933*        DATASET (WS-CLAIM-MASTER-DSID)
001934*        RIDFLD  (WS-CLAIM-MASTER-KEY)
001935*        SET     (ADDRESS OF CLAIM-MASTER)
001936*    END-EXEC.
      *    MOVE '&"S        E          (   #00008172' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038313732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001937
001938*    NOTE *******************************************************
001939*         *       READ THE CERTIFICATE MASTER RECORD            *
001940*         *******************************************************.
001941
001942     IF CL-SYSTEM-IDENTIFIER = 'CV'
001943         GO TO 0325-READ-EMPLCY.
001944
001945     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
001946     MOVE CL-CERT-CARRIER        TO  WS-CM-CARRIER.
001947     MOVE CL-CERT-GROUPING       TO  WS-CM-GROUPING.
001948     MOVE CL-CERT-STATE          TO  WS-CM-STATE.
001949     MOVE CL-CERT-ACCOUNT        TO  WS-CM-ACCOUNT.
001950     MOVE CL-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
001951     MOVE CL-CERT-NO             TO  WS-CM-CERT-NO.
001952     MOVE 'CERT'                 TO  FILE-SWITCH.
001953
001954     
      * EXEC CICS READ
001955*        DATASET   (WS-CERTIFICATE-MASTER-DSID)
001956*        RIDFLD    (WS-CERTIFICATE-MASTER-KEY)
001957*        SET       (ADDRESS OF CERTIFICATE-MASTER)
001958*    END-EXEC.
      *    MOVE '&"S        E          (   #00008194' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038313934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001959
001960     GO TO 0328-MAIN-LOGIC.
001961
001962*    NOTE *******************************************************
001963*         *      READ THE CONVENIENCE POLICY MASTER RECORD      *
001964*         *******************************************************.
001965
001966 0325-READ-EMPLCY.
001967
001968     MOVE PI-COMPANY-CD          TO  WS-PM-COMPANY-CD.
001969     MOVE CL-CERT-CARRIER        TO  WS-PM-CARRIER.
001970     MOVE CL-CERT-GROUPING       TO  WS-PM-GROUPING.
001971     MOVE CL-CERT-STATE          TO  WS-PM-STATE.
001972     MOVE CL-CERT-ACCOUNT        TO  WS-PM-PRODUCER.
001973     MOVE CL-CERT-EFF-DT         TO  WS-PM-EFF-DT.
001974     MOVE CL-CV-REFERENCE-NO     TO  WS-PM-REFERENCE-NO.
001975     MOVE 'PLCY'                 TO  FILE-SWITCH.
001976
001977     
      * EXEC CICS READ
001978*        DATASET   (WS-POLICY-MASTER-DSID)
001979*        RIDFLD    (WS-POLICY-MASTER-KEY)
001980*        SET       (ADDRESS OF POLICY-MASTER)
001981*    END-EXEC.
      *    MOVE '&"S        E          (   #00008217' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038323137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-POLICY-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-POLICY-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001982
001983     EJECT
001984 0326-READ-EMPROD.
001985
001986*    NOTE *******************************************************
001987*         *     READ THE CONVENIENCE PRODUCER MASTER RECORD     *
001988*         *******************************************************.
001989
001990     MOVE PI-COMPANY-CD          TO  WS-PD-COMPANY-CD.
001991     MOVE CL-CERT-CARRIER        TO  WS-PD-CARRIER.
001992     MOVE CL-CERT-GROUPING       TO  WS-PD-GROUPING.
001993     MOVE CL-CERT-STATE          TO  WS-PD-STATE.
001994     MOVE CL-CERT-ACCOUNT        TO  WS-PD-PRODUCER.
001995     MOVE CL-CERT-EFF-DT         TO  WS-PD-EXP-DT.
001996     MOVE SPACES                 TO  WS-PRODUCER-HOLD-RECORD.
001997     MOVE 'PROD'                 TO  FILE-SWITCH.
001998
001999 0326-STARTBR-EMPROD.
002000
002001     
      * EXEC CICS HANDLE CONDITION
002002*        NOTFND   (0326-NOTFND-EMPROD)
002003*    END-EXEC.
      *    MOVE '"$I                   ! ) #00008241' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303038323431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002004
002005     
      * EXEC CICS STARTBR
002006*        DATASET   (WS-PRODUCER-MASTER-DSID)
002007*        RIDFLD    (WS-PRODUCER-MASTER-KEY)
002008*        GTEQ
002009*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008245' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038323435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002010
002011     IF WS-EMPROD-GETMAIN-SW = 'Y'
002012         MOVE 'N'                TO  WS-EMPROD-GETMAIN-SW
002013         
      * EXEC CICS GETMAIN
002014*            SET       (ADDRESS OF PRODUCER-MASTER)
002015*            LENGTH    (2000)
002016*            INITIMG   (WS-SPACES)
002017*        END-EXEC.
           MOVE 2000
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008253' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038323533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002018
002019 0326-READNEXT-EMPROD.
002020
002021     
      * EXEC CICS READNEXT
002022*        DATASET   (WS-PRODUCER-MASTER-DSID)
002023*        RIDFLD    (WS-PRODUCER-MASTER-KEY)
002024*        INTO      (PRODUCER-MASTER)
002025*    END-EXEC.
           MOVE LENGTH OF
            PRODUCER-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008261' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038323631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 PRODUCER-MASTER, 
                 DFHEIV12, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002026
002027     IF WS-PD-COMPANY-CD NOT = PI-COMPANY-CD     OR
002028        WS-PD-CARRIER    NOT = CL-CERT-CARRIER   OR
002029        WS-PD-GROUPING   NOT = CL-CERT-GROUPING  OR
002030        WS-PD-STATE      NOT = CL-CERT-STATE     OR
002031        WS-PD-PRODUCER   NOT = CL-CERT-ACCOUNT
002032         IF WS-PRODUCER-HOLD-RECORD = SPACES
002033             GO TO 0326-NOTFND-EMPROD
002034         ELSE
002035             MOVE WS-PRODUCER-HOLD-RECORD    TO  PRODUCER-MASTER
002036             GO TO 0326-ENDBR-EMPROD.
002037
002038     IF WS-PD-EXP-DT = HIGH-VALUES
002039         GO TO 0326-ENDBR-EMPROD
002040     ELSE
002041         MOVE PRODUCER-MASTER    TO  WS-PRODUCER-HOLD-RECORD.
002042
002043     GO TO 0326-READNEXT-EMPROD.
002044
002045 0326-NOTFND-EMPROD.
002046
002047     MOVE SPACES                 TO  PRODUCER-MASTER.
002048
002049     MOVE ZEROS                  TO  PD-ZIP
002050                                     PD-TEL-NO
002051                                     PD-TOL-CLM.
002052
002053     MOVE 'PRODUCER NOT FOUND'   TO  PD-NAME.
002054
002055 0326-ENDBR-EMPROD.
002056
002057     
      * EXEC CICS ENDBR
002058*        DATASET   (WS-PRODUCER-MASTER-DSID)
002059*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008297' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038323937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002060
002061     MOVE SPACES                 TO  CHECK-PASS-AREA.
002062
002063     GO TO 0354-UPDATE-ACTIVITY-TRLRS.
002064
002065     EJECT
002066 0328-MAIN-LOGIC.
002067
002068     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002069         NEXT SENTENCE
002070     ELSE
002071         GO TO 0329-MAIN-LOGIC.
002072
002073*    NOTE *******************************************************
002074*         *     READ THE BENEFICIARY MASTER RECORD              *
002075*         *     1ST READ IS TO GET THE CREDITOR'S NAME          *
002076*         *******************************************************.
002077
002078     MOVE CL-CURRENT-GROUPING        TO  WS-GROUP.
002079     MOVE WS-GRP-1-3                 TO  WS-BK-BENEFICIARY.
002080     MOVE 'BENE'                     TO  FILE-SWITCH.
002081     PERFORM 2000-GET-BENEFICIARY.
002082
002083     MOVE BE-MAIL-TO-NAME            TO  WS-AIG-CREDITOR-NAME.
002084
002085*    NOTE *******************************************************
002086*         *     READ THE BENEFICIARY MASTER RECORD              *
002087*         *     2ND READ IS TO GET THE INSURANCE CO'S NAME      *
002088*         *******************************************************.
002089
002090     MOVE CL-BENEFICIARY             TO  WS-BK-BENEFICIARY.
002091     PERFORM 2000-GET-BENEFICIARY.
002092
002093     MOVE BE-MAIL-TO-NAME            TO  WS-AIG-INS-CO-NAME.
002094
002095 0329-MAIN-LOGIC.
002096
002097*    NOTE *******************************************************
002098*         *          READ THE ACCOUNT MASTER RECORD             *
002099*         *******************************************************.
002100
002101     MOVE PI-COMPANY-CD          TO  WS-AK-COMPANY-CD.
002102
002103     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002104         MOVE CL-CURRENT-CARRIER     TO  WS-AK-CARRIER
002105         MOVE CL-CURRENT-GROUPING    TO  WS-AK-GROUPING
002106         MOVE CL-CURRENT-STATE       TO  WS-AK-STATE
002107         MOVE CL-CURRENT-ACCOUNT     TO  WS-AK-ACCOUNT
002108         MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT
002109     ELSE
002110         MOVE CL-CERT-CARRIER        TO  WS-AK-CARRIER
002111         MOVE CL-CERT-GROUPING       TO  WS-AK-GROUPING
002112         MOVE CL-CERT-STATE          TO  WS-AK-STATE
002113         MOVE CL-CERT-ACCOUNT        TO  WS-AK-ACCOUNT
002114         MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT.
002115
002116     MOVE 'ACCT'                     TO  FILE-SWITCH.
002117     MOVE SPACES                     TO  WS-ACCOUNT-HOLD-RECORD.
002118
002119     
      * EXEC CICS HANDLE CONDITION
002120*        ENDFILE (0340-MAIN-LOGIC)
002121*        NOTFND  (0340-MAIN-LOGIC)
002122*    END-EXEC.
      *    MOVE '"$''I                  ! * #00008359' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303038333539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002123
002124     
      * EXEC CICS STARTBR
002125*        DATASET (WS-ACCOUNT-MASTER-DSID)
002126*        RIDFLD  (WS-ACCOUNT-MASTER-KEY)
002127*        GTEQ
002128*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008364' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038333634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002129
002130     IF WS-ERACCT-GETMAIN-SW = 'Y'
002131         MOVE 'N'                TO  WS-ERACCT-GETMAIN-SW
002132         
      * EXEC CICS GETMAIN
002133*            SET     (ADDRESS OF ACCOUNT-MASTER)
002134*            LENGTH  (2000)
002135*            INITIMG (WS-SPACES)
002136*        END-EXEC.
           MOVE 2000
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008372' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038333732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002137
002138 0330-MAIN-LOGIC.
002139     
      * EXEC CICS READNEXT
002140*        DATASET (WS-ACCOUNT-MASTER-DSID)
002141*        RIDFLD  (WS-ACCOUNT-MASTER-KEY)
002142*        INTO    (ACCOUNT-MASTER)
002143*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008379' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038333739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002144
002145     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002146         IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD        OR
002147            WS-AK-CARRIER    NOT = CL-CURRENT-CARRIER   OR
002148            WS-AK-GROUPING   NOT = CL-CURRENT-GROUPING  OR
002149            WS-AK-STATE      NOT = CL-CURRENT-STATE     OR
002150            WS-AK-ACCOUNT    NOT = CL-CURRENT-ACCOUNT
002151             IF WS-ACCOUNT-HOLD-RECORD = SPACES
002152                 GO TO 0340-MAIN-LOGIC
002153             ELSE
002154                 MOVE WS-ACCOUNT-HOLD-RECORD TO  ACCOUNT-MASTER
002155                 GO TO 0350-MAIN-LOGIC
002156         ELSE
002157             NEXT SENTENCE
002158     ELSE
002159         IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD     OR
002160            WS-AK-CARRIER    NOT = CL-CERT-CARRIER   OR
002161            WS-AK-GROUPING   NOT = CL-CERT-GROUPING  OR
002162            WS-AK-STATE      NOT = CL-CERT-STATE     OR
002163            WS-AK-ACCOUNT    NOT = CL-CERT-ACCOUNT
002164            IF WS-ACCOUNT-HOLD-RECORD = SPACES
002165               GO TO 0340-MAIN-LOGIC
002166           ELSE
002167               MOVE WS-ACCOUNT-HOLD-RECORD TO ACCOUNT-MASTER
002168               GO TO 0350-MAIN-LOGIC.
002169
002170     IF WS-AK-EXPIRATION-DT = HIGH-VALUES
002171         GO TO 0350-MAIN-LOGIC
002172     ELSE
002173         MOVE ACCOUNT-MASTER     TO  WS-ACCOUNT-HOLD-RECORD.
002174
002175     GO TO 0330-MAIN-LOGIC.
002176
002177 0340-MAIN-LOGIC.
002178     MOVE SPACES                 TO  ACCOUNT-MASTER.
002179
002180     MOVE ZERO                   TO  AM-ZIP
002181                                     AM-TEL-NO
002182                                     AM-TOL-PREM
002183                                     AM-TOL-REF
002184                                     AM-TOL-CLM.
002185
002186     MOVE 'ACCOUNT NOT FOUND'    TO  AM-NAME.
002187
002188 0350-MAIN-LOGIC.
002189     
      * EXEC CICS ENDBR
002190*        DATASET (WS-ACCOUNT-MASTER-DSID)
002191*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008429' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038343239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002192
002193     MOVE SPACES                 TO  CHECK-PASS-AREA.
002194
002195     IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
002196        MOVE ZEROS               TO  AM-3RD-PARTY-NOTIF-LEVEL.
002197
002198     IF AM-3RD-PARTY-NOTIF-LEVEL = ZEROS
002199        GO TO 0354-UPDATE-ACTIVITY-TRLRS.
002200
002201     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'
002202        OR 'AHL'
002203         GO TO 0354-UPDATE-ACTIVITY-TRLRS.
002204
002205     
      * EXEC CICS HANDLE CONDITION
002206*         NOTFND (0351-READ-COMP-MASTER)
002207*    END-EXEC.
      *    MOVE '"$I                   ! + #00008445' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303038343435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002208
002209     MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.
002210     MOVE +29                    TO  WS-ATK-SEQUENCE-NO.
002211     MOVE 'TRLR'                 TO  FILE-SWITCH.
002212
002213     
      * EXEC CICS READ
002214*         DATASET  (WS-ACTIVITY-TRAILERS-DSID)
002215*         RIDFLD   (WS-ACTIVITY-TRAILERS-KEY)
002216*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
002217*    END-EXEC.
      *    MOVE '&"S        E          (   #00008453' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038343533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002218
002219     MOVE AT-MAIL-TO-NAME    TO  CPA-NOTIFY-NAME.
002220     MOVE AT-ADDRESS-LINE-1  TO  CPA-NOTIFY-ADDRESS-LINE1.
002221     MOVE AT-ADDRESS-LINE-2  TO  CPA-NOTIFY-ADDRESS-LINE2.
002222     MOVE SPACES             TO  CPA-NOTIFY-ADDRESS-LINE3.
002223     MOVE AT-CITY-STATE      TO  CPA-NOTIFY-CITY-STATE.
002224     MOVE AT-ZIP             TO  CPA-NOTIFY-ZIP.
002225
002226     GO TO 0354-UPDATE-ACTIVITY-TRLRS.
002227
002228 0351-READ-COMP-MASTER.
002229
002230     MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.
002231     MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.
002232     MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.
002233     MOVE 'A'             TO WS-ERCOMP-TYPE.
002234     MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
002235                          TO WS-ERCOMP-RESP-NO.
002236     IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
002237         IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
002238                                       'G' OR 'B' or 'S'
002239             MOVE 'G'            TO  WS-ERCOMP-TYPE
002240             MOVE LOW-VALUES     TO  WS-ERCOMP-ACCOUNT
002241         ELSE
002242             MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
002243                                 TO WS-ERCOMP-ACCOUNT
002244     ELSE
002245         MOVE 'G'                TO WS-ERCOMP-TYPE
002246         MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
002247
002248     IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
002249        MOVE ZEROS TO WS-ERCOMP-CARRIER.
002250
002251     IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
002252        MOVE ZEROS TO WS-ERCOMP-GROUPING.
002253
002254     MOVE 'COMP'                 TO  FILE-SWITCH.
002255
002256     
      * EXEC CICS HANDLE CONDITION
002257*         NOTFND    (0354-UPDATE-ACTIVITY-TRLRS)
002258*    END-EXEC.
      *    MOVE '"$I                   ! , #00008496' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303038343936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002259
002260     
      * EXEC CICS  READ
002261*         SET      (ADDRESS OF COMPENSATION-MASTER)
002262*         DATASET  ('ERCOMP')
002263*         RIDFLD   (WS-ERCOMP-KEY)
002264*    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008500' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038353030' TO DFHEIV0
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
           
002265
002266     MOVE CO-ACCT-NAME          TO CPA-NOTIFY-NAME
002267     IF CO-ACCT-NAME = SPACES
002268        MOVE CO-MAIL-NAME       TO CPA-NOTIFY-NAME.
002269
002270     MOVE CO-ADDR-1             TO CPA-NOTIFY-ADDRESS-LINE1.
002271     MOVE CO-ADDR-2             TO CPA-NOTIFY-ADDRESS-LINE2.
002272     MOVE CO-ADDR-3             TO CPA-NOTIFY-CITY-STATE.
002273     MOVE CO-ZIP                TO CPA-NOTIFY-ZIP.
002274
002275 0354-UPDATE-ACTIVITY-TRLRS.
002276
002277     MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.
002278     MOVE CQ-PMT-TRLR-SEQUENCE   TO  WS-ATK-SEQUENCE-NO.
002279     MOVE 'TRLR'                 TO  FILE-SWITCH.
002280
002281     
      * EXEC CICS READ UPDATE
002282*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
002283*        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
002284*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
002285*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008521' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038353231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002286
002287     MOVE ZERO                   TO  CPA-ALIGNMENT.
002288
002289     IF CL-SYSTEM-IDENTIFIER = 'CV'
002290         MOVE PM-CARRIER                 TO  CPA-CARRIER
002291         MOVE PM-GROUPING                TO  CPA-GROUP
002292         MOVE PM-PRODUCER                TO  CPA-ACCOUNT
002293         MOVE PM-STATE                   TO  CPA-STATE
002294         MOVE PM-REFERENCE-NUMBER        TO  CPA-REFERENCE-NO
002295         MOVE PM-INS-TYPE                TO  CPA-IND-GRP-TYPE
002296         MOVE PM-POLICY-EFF-DT           TO  CPA-CERT-EFF-DT
002297     ELSE
002298         IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002299             MOVE CL-CURRENT-CARRIER     TO  CPA-CARRIER
002300             MOVE CL-CURRENT-GROUPING    TO  CPA-GROUP
002301             MOVE CL-CURRENT-STATE       TO  CPA-STATE
002302             MOVE CL-CURRENT-ACCOUNT     TO  CPA-ACCOUNT
002303             MOVE CM-CERT-NO             TO  CPA-CERT-NO
002304             MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE
002305             MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT
002306         ELSE
002307             MOVE CM-CARRIER             TO  CPA-CARRIER
002308             MOVE CM-GROUPING            TO  CPA-GROUP
002309             MOVE CM-ACCOUNT             TO  CPA-ACCOUNT
002310             MOVE CM-STATE               TO  CPA-STATE
002311             MOVE CM-CERT-NO             TO  CPA-CERT-NO
002312             MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE
002313             MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT.
002314
002315     MOVE CL-CLAIM-NO                    TO  CPA-CLAIM-NO.
002316     MOVE CL-CLAIM-STATUS                TO  CPA-CLAIM-STATUS.
002317     MOVE CL-LAST-CLOSE-REASON           TO  CPA-LAST-CLOSE-REASON
002318
002319     PERFORM 5000-MOVE-NAME.
002320     MOVE WS-NAME-WORK           TO  CPA-INSURED-NAME.
002321
002322     MOVE CL-CLAIM-TYPE          TO  CPA-CLAIM-TYPE.
002323     MOVE AT-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.
002324     MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.
002325
002326     MOVE AT-PAYMENT-NOTE-SEQ-NO TO WS-PAYMENT-NOTE-SEQ-NO.
002327
002328     IF PI-COMPANY-ID = 'LAP' OR 'RMC'
002329         MOVE AT-FORM-CTL-SEQ-NO TO CPA-FORM-CTL-SEQ-NO.
002330
002331     MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER
002332                                     AT-CHECK-NO.
002333
002334     MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.
002335     MOVE CL-TOTAL-PAID-AMT      TO  CPA-AMOUNT-PAID-TO-DATE.
002336     MOVE AT-DAYS-IN-PERIOD      TO  CPA-DAYS-PAID.
002337     MOVE AT-DAILY-RATE          TO  CPA-DAILY-RATE.
002338     MOVE AT-ELIMINATION-DAYS    TO  CPA-ELIMINATION-DAYS.
002339     MOVE AT-BENEFIT-TYPE        TO  CPA-BENEFIT-TYPE.
002340     MOVE AT-EXPENSE-TYPE        TO  CPA-EXPENSE-TYPE.
002341     MOVE CL-NO-OF-PMTS-MADE     TO  CPA-NO-OF-PMTS-MADE.
002342     MOVE CL-PROCESSOR-ID        TO  CPA-EXAMINER.
002343     MOVE AT-PAYMENT-ORIGIN      TO  CPA-PAYMENT-ORIGIN.
002344     MOVE CL-CCN-A5              TO  CPA-CREDIT-CARD-NO.
002345
002346     IF AT-PAYMENT-ORIGIN = '2'
002347         MOVE 'Y'                TO  WS-AUTO-PAY-SW
002348     ELSE
002349         MOVE 'N'                TO  WS-AUTO-PAY-SW.
002350
002351     IF (PI-COMPANY-ID = 'FIM' OR 'FMK' OR 'HER') AND
002352        (AT-PAYMENT-ORIGIN = '2')
002353         MOVE 'AUTO'             TO  CPA-EXAMINER.
002354
002355*    NOTE *******************************************************
002356*         *      CLAIM TYPE      MEANING                        *
002357*         *          1         DEATH CLAIM (INDIVIDUAL)         *
002358*         *          2         DISABILITY CLAIM (INDIVIDUAL)    *
002359*         *          3         OUTSTANDING BALANCE (DEATH)      *
002360*         *          4         OUTSTANDING BALANCE (DISABILITY) *
002361*         *******************************************************.
002362
002363     IF CL-CLAIM-TYPE = 'L' OR 'O'
002364         IF CL-CLAIM-PREM-TYPE = '2'
002365             MOVE '3'            TO  CPA-CLAIM-CODE
002366           ELSE
002367             MOVE '1'            TO  CPA-CLAIM-CODE
002368       ELSE
002369         IF CL-CLAIM-PREM-TYPE = '2'
002370             MOVE '4'            TO  CPA-CLAIM-CODE
002371           ELSE
002372             MOVE '2'            TO  CPA-CLAIM-CODE.
002373
002374*    NOTE *******************************************************
002375*         *      PAY CODE       MEANING                         *
002376*         *         A        ADDITIONAL DEATH CLAIM             *
002377*         *         P        PARTIAL PAYMENT                    *
002378*         *         F        FINAL PAYMENT                      *
002379*         *         S        LUMP SUM DISABILITY                *
002380*         *******************************************************.
002381
002382     MOVE AT-PAYMENT-TYPE        TO  CPA-PAY-CODE
002383                                     WS-PAYMENT-TYPE.
002384     INSPECT CPA-PAY-CODE CONVERTING '123456789' TO 'PFSA     '.
002385
002386     MOVE CL-INCURRED-DT         TO  CPA-INCURRED-DT.
002387     MOVE CL-REPORTED-DT         TO  CPA-REPORTED-DT.
002388
002389     IF NOT PI-USES-PAID-TO
002390        MOVE AT-PAID-THRU-DT     TO  CPA-PAID-THRU-DT
002391     ELSE
002392        MOVE AT-PAID-THRU-DT     TO  DC-BIN-DATE-1
002393        MOVE +1                  TO  DC-ELAPSED-DAYS
002394        MOVE +0                  TO  DC-ELAPSED-MONTHS
002395        MOVE '6'                 TO  DC-OPTION-CODE
002396        PERFORM 8500-DATE-CONVERSION
002397        IF NO-CONVERSION-ERROR
002398           MOVE DC-BIN-DATE-2    TO  CPA-PAID-THRU-DT.
002399
002400     MOVE AT-PAID-FROM-DT        TO  CPA-PAID-FROM-DT.
002401     MOVE CL-LAST-PMT-DT         TO  CPA-PAID-DT.
002402
002403     MOVE WS-CARRIER-ADDRESS-DATA  TO  CPA-CARRIER-ADDRESS-DATA.
002404
002405     IF CL-SYSTEM-IDENTIFIER = 'CV'
002406         GO TO 0354-PROCESS-CV-CLAIM.
002407
002408     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002409         MOVE WS-AIG-INS-CO-NAME TO  CPA-COMPANY-NAME.
002410
002411     MOVE AM-NAME                TO  CPA-ACCOUNT-NAME.
002412     MOVE AM-PERSON              TO  CPA-ACCOUNT-IN-CARE-OF.
002413     MOVE AM-ADDRS               TO  CPA-ACCOUNT-ADDRESS-LINE1.
002414     MOVE SPACES                 TO  CPA-ACCOUNT-ADDRESS-LINE2.
002415     MOVE AM-CITY                TO  CPA-ACCOUNT-CITY-ST.
002416     MOVE AM-ZIP                 TO  CPA-ACCOUNT-ZIP-CODE.
002417     MOVE AM-TEL-NO              TO  WS-WORK-PHONE.
002418     INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.
002419     MOVE WS-NUMERIC-PHONE       TO  CPA-ACCOUNT-PHONE-NO.
002420
002421     IF CM-SSN-STATE   = CM-STATE AND
002422        CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
002423         MOVE SPACES             TO  CPA-SOC-SEC-NO
002424     ELSE
002425         MOVE CM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.
002426
002427     MOVE CM-STATE               TO  WS-SSN-STATE
002428                                     WS-MEMBER-STATE.
002429     MOVE CM-ACCOUNT             TO  WS-SSN-ACCOUNT
002430                                     WS-MEMBER-ACCOUNT.
002431     MOVE CM-INSURED-LAST-NAME   TO  WS-SSN-LN3
002432                                     WS-MEMBER-LN4.
002433
002434     IF CM-MEMB-STATE   = CM-STATE AND
002435        CM-MEMB-ACCOUNT = CM-ACCOUNT-PRIME
002436         MOVE SPACES             TO  CPA-MEMBER-NUMBER
002437     ELSE
002438         MOVE CM-MEMBER-NO       TO  CPA-MEMBER-NUMBER.
002439
002440     IF PI-COMPANY-ID = 'DMD'
002441         MOVE CM-POLICY-FORM-NO  TO  CPA-MEMBER-NUMBER.
002442
002443*    MOVE CM-LOAN-NUMBER         TO  CPA-LOAN-NUMBER
002444*    MOVE CM-BENEFICIARY         TO  CPA-BENEFICIARY
002445
002446     GO TO 0354-CONT.
002447
002448 0354-PROCESS-CV-CLAIM.
002449
002450     MOVE PD-NAME               TO  CPA-ACCOUNT-NAME.
002451     MOVE PD-PERSON             TO  CPA-ACCOUNT-IN-CARE-OF.
002452     MOVE PD-ADDRS              TO  CPA-ACCOUNT-ADDRESS-LINE1.
002453     MOVE SPACES                TO  CPA-ACCOUNT-ADDRESS-LINE2.
002454     MOVE PD-CITY               TO  CPA-ACCOUNT-CITY-ST.
002455     MOVE PD-ZIP                TO  CPA-ACCOUNT-ZIP-CODE.
002456     MOVE PD-TEL-NO             TO  WS-WORK-PHONE.
002457     INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.
002458     MOVE WS-NUMERIC-PHONE      TO  CPA-ACCOUNT-PHONE-NO.
002459
002460     IF PM-SSN-STATE    = PM-STATE AND
002461        PM-SSN-PRODUCER = PM-PRODUCER-PRIME
002462         MOVE SPACES             TO  CPA-SOC-SEC-NO
002463     ELSE
002464         MOVE PM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.
002465
002466     MOVE PM-LOAN-NUMBER         TO  CPA-CV-LOAN-NUMBER.
002467
002468 0354-CONT.
002469
002470     MOVE AT-PAYEE-TYPE-CD       TO  WS-PAYEE-CODE
002471                                     CPA-PAYEE-TYPE-CD.
002472
002473     MOVE WS-CHECK-WRITER-DATE   TO  AT-CHECK-WRITTEN-DT
002474                                     CQ-CHECK-WRITTEN-DT
002475                                     CPA-CHECK-DATE.
002476
002477     IF AOPTIONI NOT = '3'
002478         MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.
002479
002480     IF AOPTIONI = '3'
002481       AND CQ-PRE-NUMBERING-SW = '1'
002482         MOVE WS-SEQUENCE-NUMBER TO  AT-CHECK-QUE-SEQUENCE.
002483
002484     MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
002485     MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
002486     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
002487     MOVE '5'                    TO  DC-OPTION-CODE.
002488     PERFORM 8500-DATE-CONVERSION.
002489     MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
002490
002491     
      * EXEC CICS REWRITE
002492*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
002493*        FROM    (ACTIVITY-TRAILERS)
002494*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008731' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038373331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002495
002496     IF WS-PAYMENT-NOTE-SEQ-NO GREATER THAN +0 AND
002497        WS-PAYMENT-NOTE-SEQ-NO LESS THAN +4096
002498        MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY
002499        MOVE WS-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO
002500        MOVE 'TRLR'                 TO  FILE-SWITCH
002501        
      * EXEC CICS HANDLE CONDITION
002502*            NOTFND  (0354-CHECK-FOR-AUTO-PAY)
002503*       END-EXEC
      *    MOVE '"$I                   ! - #00008741' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303038373431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002504        
      * EXEC CICS READ
002505*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
002506*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
002507*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
002508*       END-EXEC
      *    MOVE '&"S        E          (   #00008744' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002509        IF AT-TRAILER-TYPE = '6'
002510           MOVE AT-INFO-LINE-1 TO CPA-COMMENT
002511           MOVE AT-INFO-LINE-2 TO CPA-COMMENT-2.
002512
002513 0354-CHECK-FOR-AUTO-PAY.
002514
002515     IF WS-AUTO-PAY-SW = 'N'
002516         GO TO 0355-GET-ADDRESS.
002517
002518     
      * EXEC CICS HANDLE CONDITION
002519*        NOTFND   (0355-GET-ADDRESS)
002520*    END-EXEC.
      *    MOVE '"$I                   ! . #00008758' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303038373538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002521
002522     MOVE WS-CLAIM-MASTER-KEY        TO  WS-ACTIVITY-TRAILERS-KEY.
002523     MOVE CL-AUTO-PAY-SEQ            TO  WS-ATK-SEQUENCE-NO.
002524     MOVE 'TRLR'                     TO  FILE-SWITCH.
002525
002526     
      * EXEC CICS READ
002527*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
002528*        RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)
002529*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
002530*    END-EXEC.
      *    MOVE '&"S        E          (   #00008766' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002531
002532     IF AT-TRAILER-TYPE = '3'
002533         NEXT SENTENCE
002534     ELSE
002535         GO TO 0355-GET-ADDRESS.
002536
002537     MOVE AT-SCHEDULE-END-DT         TO  CPA-AUTO-PAY-END-DT.
002538
002539 0355-GET-ADDRESS.
002540
002541     IF CL-ACCOUNT-ADDR-CNT NOT = ZERO
002542        MOVE CL-ACCOUNT-ADDR-CNT TO WS-ATK-SEQUENCE-NO
002543        ADD +20    TO WS-ATK-SEQUENCE-NO
002544        PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT
002545        MOVE CPA-PAYEE-NAME          TO  CPA-ACCOUNT-NAME
002546        MOVE CPA-PAYEE-ADDRESS-LINE2 TO  CPA-ACCOUNT-ADDRESS-LINE1
002547        MOVE CPA-PAYEE-ADDRESS-LINE3 TO  CPA-ACCOUNT-ADDRESS-LINE2
002548        MOVE CPA-PAYEE-CITY-STATE    TO  CPA-ACCOUNT-CITY-ST
002549        MOVE CPA-PAYEE-ZIP           TO  CPA-ACCOUNT-ZIP-CODE.
002550
002551     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL'
002552        MOVE +91                 TO WS-ATK-SEQUENCE-NO
002553        
      * EXEC CICS READ
002554*          DATASET (WS-ACTIVITY-TRAILERS-DSID)
002555*          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
002556*          SET     (ADDRESS OF ACTIVITY-TRAILERS)
002557*          RESP    (WS-RESPONSE)
002558*       END-EXEC
      *    MOVE '&"S        E          (  N#00008793' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038373933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002559        IF WS-RESP-NORMAL
002560           MOVE AT-INFO-LINE-1   TO CPA-BENEFICIARY
002561        ELSE
002562           MOVE SPACES           TO CPA-BENEFICIARY
002563        END-IF
002564     END-IF
002565
002566     MOVE CL-INSURED-ADDR-CNT TO WS-ATK-SEQUENCE-NO.
002567
002568     PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT.
002569
002570     MOVE CPA-PAYEE-NAME           TO  CPA-INSURED-ADDR-TRLR-NAME.
002571     MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CPA-INSURED-ADDRESS-LINE1.
002572     MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CPA-INSURED-ADDRESS-LINE2.
002573     MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CPA-INSURED-ADDRESS-LINE3.
002574     MOVE CPA-PAYEE-CITY-STATE     TO  CPA-INSURED-CITY-STATE.
002575     MOVE CPA-PAYEE-ZIP            TO  CPA-INSURED-ZIP.
002576
002577     IF WS-PAYEE-CD = 'I'
002578        GO TO 0370-MAIN-LOGIC.
002579
002580     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002581       IF WS-PAYEE-CD = 'B'
002582         IF WS-PAYEE-SEQ-NUM = 0
002583           MOVE CL-BENEFICIARY           TO  WS-BK-BENEFICIARY
002584           PERFORM 2000-GET-BENEFICIARY
002585           GO TO 0370-MAIN-LOGIC
002586         ELSE
002587           IF WS-PAYEE-SEQ-NUM = 9
002588             MOVE WS-AIG-CREDITOR-NAME   TO  CPA-PAYEE-NAME
002589             GO TO 0370-MAIN-LOGIC
002590           ELSE
002591             MOVE WS-PAYEE-SEQ-NUM       TO  WS-ATK-SEQUENCE-NO
002592             ADD +10                     TO  WS-ATK-SEQUENCE-NO
002593             GO TO 0360-MAIN-LOGIC.
002594
002595     IF WS-PAYEE-CD = 'B'
002596        IF (CL-BENIF-ADDR-CNT = +0) OR
002597           (WS-PAYEE-SEQ-NUM = 0 OR 9)
002598            MOVE CL-BENEFICIARY        TO  WS-BK-BENEFICIARY
002599            PERFORM 2000-GET-BENEFICIARY
002600            GO TO 0370-MAIN-LOGIC
002601        ELSE
002602            MOVE WS-PAYEE-SEQ-NUM   TO  WS-ATK-SEQUENCE-NO
002603            ADD +10                 TO  WS-ATK-SEQUENCE-NO
002604            GO TO 0360-MAIN-LOGIC.
002605
002606     IF WS-PAYEE-CD = 'O'
002607        MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
002608        ADD +50                    TO  WS-ATK-SEQUENCE-NO
002609        GO TO 0360-MAIN-LOGIC.
002610
002611     IF WS-PAYEE-CD = 'Q'
002612        MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
002613        ADD +60                    TO  WS-ATK-SEQUENCE-NO
002614        GO TO 0360-MAIN-LOGIC.
002615
002616     IF WS-PAYEE-CD = 'P'
002617        MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
002618        ADD +30                    TO  WS-ATK-SEQUENCE-NO
002619        GO TO 0360-MAIN-LOGIC.
002620
002621     IF WS-PAYEE-CD = 'E'
002622        MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
002623        ADD +40                    TO  WS-ATK-SEQUENCE-NO
002624        GO TO 0360-MAIN-LOGIC.
002625
002626     IF (WS-PAYEE-CD = 'A') AND
002627        (CL-ACCOUNT-ADDR-CNT NOT = +0) AND
002628        (WS-PAYEE-SEQ-NUM GREATER THAN 0)
002629        MOVE WS-PAYEE-SEQ-NUM     TO  WS-ATK-SEQUENCE-NO
002630        ADD +20                   TO  WS-ATK-SEQUENCE-NO
002631        GO TO 0360-MAIN-LOGIC.
002632
002633     IF CL-SYSTEM-IDENTIFIER = 'CV'
002634         MOVE PD-NAME           TO  CPA-PAYEE-NAME
002635         MOVE PD-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1
002636         MOVE PD-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2
002637         MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3
002638         MOVE PD-CITY           TO  CPA-PAYEE-CITY-STATE
002639         MOVE PD-ZIP            TO  CPA-PAYEE-ZIP
002640     ELSE
002641         MOVE AM-NAME           TO  CPA-PAYEE-NAME
002642         MOVE AM-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1
002643         MOVE AM-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2
002644         MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3
002645         MOVE AM-CITY           TO  CPA-PAYEE-CITY-STATE
002646         MOVE AM-ZIP            TO  CPA-PAYEE-ZIP.
002647
002648     GO TO 0370-MAIN-LOGIC.
002649
002650 0360-MAIN-LOGIC.
002651     
      * EXEC CICS HANDLE CONDITION
002652*        NOTFND (0360-NO-TRLR)
002653*    END-EXEC.
      *    MOVE '"$I                   ! / #00008891' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303038383931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002654
002655     IF WS-ATK-SEQUENCE-NO = +0
002656        GO TO 0360-NO-TRLR
002657     ELSE
002658        
      * EXEC CICS READ
002659*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
002660*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
002661*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
002662*       END-EXEC
      *    MOVE '&"S        E          (   #00008898' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038383938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002663        MOVE AT-MAIL-TO-NAME    TO  CPA-PAYEE-NAME
002664        MOVE AT-ADDRESS-LINE-1  TO  CPA-PAYEE-ADDRESS-LINE2
002665        MOVE AT-ADDRESS-LINE-2  TO  CPA-PAYEE-ADDRESS-LINE3
002666        MOVE AT-CITY-STATE      TO  CPA-PAYEE-CITY-STATE
002667        MOVE AT-ZIP             TO  CPA-PAYEE-ZIP.
002668
002669     GO TO 0360-EXIT.
002670
002671 0360-NO-TRLR.
002672     MOVE WS-NAME-WORK           TO  CPA-PAYEE-NAME.
002673     MOVE ZERO                   TO  CPA-PAYEE-ZIP.
002674     MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE2
002675                                     CPA-PAYEE-ADDRESS-LINE3
002676                                     CPA-PAYEE-CITY-STATE.
002677     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
002678 0360-EXIT.
002679     EXIT.
002680
002681 0370-MAIN-LOGIC.
002682     IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
002683         MOVE EIBDATE            TO  DC-JULIAN-YYDDD
002684         MOVE '5'                TO  DC-OPTION-CODE
002685         PERFORM 8500-DATE-CONVERSION
002686         MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-2
002687         MOVE '1'                TO  DC-OPTION-CODE
002688         PERFORM 8500-DATE-CONVERSION
002689         DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING CPA-INSURED-AGE
002690       ELSE
002691         MOVE ZERO               TO  CPA-INSURED-AGE.
002692
002693     IF CL-SYSTEM-IDENTIFIER = 'CV'
002694         GO TO 0372-READ-EMPLAN.
002695
002696     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
002697                            OR 'B' OR 'H'
002698         MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
002699         MOVE '5'                TO  WS-CFK-RECORD-TYPE
002700         MOVE CM-AH-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO
002701                                     WS-BENEFIT-NO
002702         PERFORM 8700-LOCATE-BENEFIT
002703         IF WS-KIND NOT = SPACES
002704            MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE
002705            MOVE WS-RETRO-ELIM      TO  CPA-BENEFIT-TYPE
002706            MOVE WS-RETRO-DAYS      TO  CPA-ELIMINATION-DAYS
002707            MOVE CM-AH-BENEFIT-AMT  TO  CPA-MONTHLY-BENEFIT
002708          ELSE
002709            MOVE ZERO               TO  CPA-ELIMINATION-DAYS
002710                                        CPA-MONTHLY-BENEFIT
002711       ELSE
002712         MOVE ZERO               TO  CPA-ELIMINATION-DAYS
002713                                     CPA-MONTHLY-BENEFIT
002714         MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
002715         MOVE '4'                TO  WS-CFK-RECORD-TYPE
002716         MOVE CM-LF-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO
002717                                     WS-BENEFIT-NO
002718         PERFORM 8700-LOCATE-BENEFIT
002719         IF WS-KIND NOT = SPACES
002720            MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE.
002721
002722     IF CM-CERT-EFF-DT = LOW-VALUES
002723         GO TO 0375-MAIN-LOGIC.
002724
002725     MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
002726
002727     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
002728                            OR 'B' OR 'H'
002729         MOVE CM-AH-ORIG-TERM    TO  DC-ELAPSED-MONTHS
002730         MOVE CM-AH-BENEFIT-CD   TO  CPA-BENEFIT-CD
002731       ELSE
002732         MOVE CM-LF-BENEFIT-AMT  TO  CPA-TOTAL-BENEFIT
002733         MOVE CM-LF-BENEFIT-CD   TO  CPA-BENEFIT-CD
002734         MOVE CM-LF-ORIG-TERM    TO  DC-ELAPSED-MONTHS.
002735
002736     MOVE ZERO                   TO  DC-ELAPSED-DAYS
002737     MOVE '6'                    TO  DC-OPTION-CODE
002738     PERFORM 8500-DATE-CONVERSION
002739
002740     MOVE DC-BIN-DATE-2          TO  CPA-EXPIRE-DT.
002741
002742     GO TO 0375-MAIN-LOGIC.
002743
002744     EJECT
002745 0372-READ-EMPLAN.
002746
002747     
      * EXEC CICS HANDLE CONDITION
002748*        NOTFND   (0375-MAIN-LOGIC)
002749*    END-EXEC.
      *    MOVE '"$I                   ! 0 #00008987' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303038393837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002750
002751     MOVE PM-COMPANY-CD          TO  WS-PP-COMPANY-CD.
002752     MOVE PM-CARRIER             TO  WS-PP-CARRIER.
002753     MOVE PM-GROUPING            TO  WS-PP-GROUPING.
002754     MOVE PM-STATE               TO  WS-PP-STATE.
002755     MOVE PM-PRODUCER            TO  WS-PP-PRODUCER.
002756     MOVE PM-INS-PLAN-CD         TO  WS-PP-PLAN-CODE.
002757     MOVE PM-INS-PLAN-REVISION   TO  WS-PP-REV-NO.
002758     MOVE 'PLAN'                 TO  FILE-SWITCH.
002759
002760     
      * EXEC CICS READ
002761*        DATASET   (WS-PLAN-MASTER-DSID)
002762*        RIDFLD    (WS-PLAN-MASTER-KEY)
002763*        SET       (ADDRESS OF PRODUCER-PLANS)
002764*    END-EXEC.
      *    MOVE '&"S        E          (   #00009000' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039303030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PLAN-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002765
002766     MOVE PP-BENEFIT-TYPE            TO  CPA-COVERAGE-TYPE.
002767
002768     IF PM-AH-MORT-PLAN
002769         MOVE PM-INS-MONTH-BENEFIT   TO  CPA-MONTHLY-BENEFIT
002770     ELSE
002771         MOVE PM-INS-TOTAL-BENEFIT   TO  CPA-TOTAL-BENEFIT.
002772
002773     MOVE PM-INS-PLAN-CD             TO  CPA-BENEFIT-CD.
002774
002775     IF PM-POLICY-EFF-DT = LOW-VALUES
002776         GO TO 0375-MAIN-LOGIC.
002777
002778     MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.
002779     MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS.
002780     MOVE +0                         TO  DC-ELAPSED-DAYS.
002781     MOVE '6'                        TO  DC-OPTION-CODE.
002782     PERFORM 8500-DATE-CONVERSION.
002783     MOVE DC-BIN-DATE-2              TO  CPA-EXPIRE-DT.
002784     EJECT
002785 0375-MAIN-LOGIC.
002786     ADD +1  TO  CQ-TIMES-PRINTED.
002787
002788     IF AOPTIONI = '3'
002789       AND CQ-PRE-NUMBERING-SW = '1'
002790         MOVE CHECK-QUE          TO  WS-NEW-CHECK-QUEUE-RECORD
002791         MOVE WS-OLD-CHECK-QUEUE-RECORD  TO  CHECK-QUE
002792         MOVE SPACES             TO  CQ-PAYEE-BENE-ACCT
002793         MOVE 'S'                TO  CQ-ENTRY-TYPE.
002794         MOVE LOW-VALUES         TO  CQ-APPLIED-TO-RCON-DT.
002795
002796     MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
002797     MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
002798     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
002799     MOVE '5'                    TO  DC-OPTION-CODE.
002800     PERFORM 8500-DATE-CONVERSION.
002801     MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
002802
002803     
      * EXEC CICS REWRITE
002804*        DATASET (WS-CHECK-QUEUE-DSID)
002805*        FROM    (CHECK-QUE)
002806*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009043' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039303433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002807
002808     IF AOPTIONI NOT = '3'
002809         GO TO 0380-MAIN-LOGIC.
002810
002811     IF WS-ELCHKQ-GETMAIN-SW = 'Y'
002812         MOVE 'N'                TO  WS-ELCHKQ-GETMAIN-SW
002813         
      * EXEC CICS GETMAIN
002814*            SET    (WS-ELCHKQ-POINTER)
002815*            LENGTH (100)
002816*        END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE '," L                  $   #00009053' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039303533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCHKQ-POINTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002817
002818     MOVE WS-ELCHKQ-POINTER             TO LCP-WS-ADDR-COMP
002819     SET ADDRESS OF CHECK-QUE TO LCP-WS-ADDR-PNTR.
002820
002821     MOVE WS-NEW-CHECK-QUEUE-RECORD  TO  CHECK-QUE
002822
002823     IF CQ-PRE-NUMBERING-SW NOT = '1'
002824         GO TO 0380-MAIN-LOGIC.
002825
002826     MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
002827
002828     MOVE SPACE                  TO  CQ-LEDGER-FLAG.
002829
002830     MOVE WS-SEQUENCE-NUMBER     TO  CQ-SEQUENCE-NUMBER
002831                                     CQ-SEQUENCE-NUMBER-A1.
002832
002833     ADD +1  TO  WS-SEQUENCE-NUMBER.
002834
002835     MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
002836     MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
002837     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
002838     MOVE '5'                    TO  DC-OPTION-CODE.
002839     PERFORM 8500-DATE-CONVERSION.
002840     MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
002841     MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.
002842
002843     
      * EXEC CICS WRITE
002844*        DATASET (WS-CHECK-QUEUE-DSID)
002845*        RIDFLD  (CQ-CONTROL-PRIMARY)
002846*        FROM    (CHECK-QUE)
002847*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009083' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039303833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002848
002849 0380-MAIN-LOGIC.
002850     PERFORM 0800-PRINT-CHECK.
002851
002852     
      * EXEC CICS STARTBR
002853*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
002854*        RIDFLD  (WS-CHECK-AIX-KEY)
002855*        GTEQ
002856*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009092' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039303932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002857
002858     GO TO 0320-MAIN-LOGIC.
002859
002860     EJECT
002861 0390-MAIN-LOGIC.
002862     IF AOPTIONI NOT = '1'
002863       AND PI-INDEX LESS THAN PI-NUMBER-OF-CONTROL-GROUPS
002864         SET PI-INDEX UP BY +1
002865         GO TO 0310-MAIN-LOGIC.
002866
002867     
      * EXEC CICS ENDBR
002868*        DATASET (WS-CHECK-QUEUE-AIX-DSID)
002869*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009107' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039313037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002870
002871     IF PI-CHECK-PRINTER-ID NOT = 'R2T7'
002872         PERFORM 0700-END-PRINT.
002873
002874     MOVE PI-PRINTER-STARTED-SW  TO  WS-PRINTER-STARTED-SW.
002875     MOVE PI-TEMP-STORAGE-KEY    TO  WS-TEMP-STORAGE-KEY.
002876     ADD +1  TO  WS-COMPLETED-SUCCESSFUL.
002877     GO TO 0015-MAIN-LOGIC.
002878
002879 0700-END-PRINT SECTION.
002880     MOVE HIGH-VALUES            TO  CHECK-PASS-AREA.
002881     MOVE +1                     TO  WS-TS-LENGTH.
002882
002883     PERFORM 0800-PRINT-CHECK.
002884
002885     MOVE ZERO                   TO  PI-PRINTER-STARTED-SW.
002886
002887 0700-EXIT.
002888     EXIT.
002889
002890 0800-PRINT-CHECK SECTION.
002891
002892     IF PI-COMPANY-ID = 'JAL' OR 'JAI'
002893        NEXT SENTENCE
002894     ELSE
002895        
      * EXEC CICS WRITEQ TS
002896*            QUEUE  (PI-TEMP-STORAGE-KEY)
002897*            ITEM   (WS-TEMP-STORAGE-ITEM)
002898*            FROM   (CHECK-PASS-AREA)
002899*            LENGTH (WS-TS-LENGTH)
002900*       END-EXEC.
      *    MOVE '*" I   L              ''   #00009135' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039313335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 CHECK-PASS-AREA, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002901
002902 0800-EXIT.
002903     EXIT.
002904
002905     EJECT
002906 0900-TERMIDERR SECTION.
002907     
      * EXEC CICS SYNCPOINT
002908*        ROLLBACK
002909*    END-EXEC.
      *    MOVE '6"R                   !   #00009147' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303039313437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002910
002911     MOVE ER-0371                TO  EMI-ERROR.
002912     MOVE -1                     TO  APFKL.
002913     PERFORM 8200-SEND-DATAONLY.
002914     PERFORM 9100-RETURN-TRAN.
002915
002916 0910-ENQ-BUSY.
002917     MOVE ER-0395                TO  EMI-ERROR.
002918     MOVE -1                     TO  AOPTIONL.
002919     PERFORM 8200-SEND-DATAONLY.
002920     PERFORM 9100-RETURN-TRAN.
002921
002922     EJECT
002923 1000-GET-CARRIER-NAME SECTION.
002924     
      * EXEC CICS HANDLE CONDITION
002925*        NOTFND (1020-CARRIER-NOT-FOUND)
002926*    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009164' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303039313634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002927
002928     MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
002929     MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
002930     MOVE CQ-CARRIER             TO  WS-CFK-CARRIER.
002931     MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
002932     MOVE 'CNTL'                 TO  FILE-SWITCH.
002933
002934     
      * EXEC CICS READ
002935*        DATASET (WS-CONTROL-FILE-DSID)
002936*        RIDFLD  (WS-CONTROL-FILE-KEY)
002937*        SET     (ADDRESS OF CONTROL-FILE)
002938*    END-EXEC.
      *    MOVE '&"S        E          (   #00009174' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039313734' TO DFHEIV0
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
           
002939
002940     MOVE CF-MAIL-TO-NAME        TO  WS-CARRIER-MAIL-TO-NAME.
002941     MOVE CF-IN-CARE-OF          TO  WS-CARRIER-IN-CARE-OF.
002942     MOVE CF-ADDRESS-LINE-1      TO  WS-CARRIER-ADDRESS-LINE-1.
002943     MOVE CF-ADDRESS-LINE-2      TO  WS-CARRIER-ADDRESS-LINE-2.
002944     MOVE CF-CITY-STATE          TO  WS-CARRIER-CITY-STATE.
002945
002946     IF CF-ZIP-CODE-NUM NOT NUMERIC
002947         MOVE ZEROS              TO CF-ZIP-CODE-NUM.
002948     IF CF-ZIP-CODE-NUM NOT = ZEROS
002949         MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-UNPACKED
002950         MOVE WS-ZIP-UNPACKED    TO  WS-CARRIER-ZIP-CODE
002951     ELSE
002952         MOVE CF-ZIP-CODE        TO  WS-CARRIER-ZIP-CODE.
002953
002954     MOVE CF-PHONE-NO            TO  WS-CARRIER-PHONE-NO.
002955     GO TO 1090-EXIT.
002956
002957 1020-CARRIER-NOT-FOUND.
002958     MOVE SPACES                 TO  WS-CARRIER-ADDRESS-DATA.
002959
002960 1090-EXIT.
002961     EXIT.
002962
002963     EJECT
002964 2000-GET-BENEFICIARY SECTION.
002965     
      * EXEC CICS HANDLE CONDITION
002966*        NOTFND (2080-GET-BENEFICIARY)
002967*    END-EXEC.
      *    MOVE '"$I                   ! 2 #00009205' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303039323035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002968
002969     MOVE PI-COMPANY-CD          TO  WS-BK-COMPANY-CD.
002970     MOVE 'B'                    TO  WS-BK-RECORD-TYPE.
002971
002972     
      * EXEC CICS READ
002973*        DATASET (WS-BENEFICIARY-MASTER-DSID)
002974*        RIDFLD  (WS-BENEFICIARY-KEY)
002975*        SET     (ADDRESS OF BENEFICIARY-MASTER)
002976*    END-EXEC.
      *    MOVE '&"S        E          (   #00009212' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039323132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BENEFICIARY-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-BENEFICIARY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002977
002978     MOVE BE-MAIL-TO-NAME        TO  CPA-PAYEE-NAME.
002979     MOVE BE-ADDRESS-LINE-1      TO  CPA-PAYEE-ADDRESS-LINE1.
002980     MOVE BE-ADDRESS-LINE-2      TO  CPA-PAYEE-ADDRESS-LINE2.
002981     MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE3.
002982     MOVE BE-CITY-STATE          TO  CPA-PAYEE-CITY-STATE.
002983     MOVE BE-ZIP-CODE            TO  CPA-PAYEE-ZIP.
002984
002985     GO TO 2090-EXIT.
002986
002987 2080-GET-BENEFICIARY.
002988     MOVE SPACES                 TO  CPA-PAYEE-NAME
002989                                     CPA-PAYEE-ADDRESS-LINE1
002990                                     CPA-PAYEE-ADDRESS-LINE2
002991                                     CPA-PAYEE-CITY-STATE.
002992     MOVE ZERO                   TO  CPA-PAYEE-ZIP.
002993
002994 2090-EXIT.
002995     EXIT.
002996
002997     EJECT
002998 5000-MOVE-NAME SECTION.
      *                        COPY ELCMNS.
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
002999
003000
003001 5000-MICR-CLOSED SECTION.
003002
003003     MOVE +9836 TO EMI-ERROR.
003004     MOVE ZERO TO PI-PROCESSING-SW.
003005     PERFORM 8200-SEND-DATAONLY.
003006
003007 5000-MICR-EXIT.
003008
003009*5000-FLAG-CLOSED SECTION.
003010*
003011*    MOVE +9837 TO EMI-ERROR.
003012*    MOVE ZERO TO PI-PROCESSING-SW.
003013*    PERFORM 8200-SEND-DATAONLY.
003014*
003015*5000-FLAG-EXIT.
003016
003017     EJECT
003018 8100-SEND-INITIAL-MAP SECTION.
003019     IF EMI-ERROR NOT = ZERO
003020         PERFORM 9900-ERROR-FORMAT
003021       ELSE
003022         IF TRANSACTION-SUCCESSFUL
003023             PERFORM 9900-ERROR-FORMAT
003024             IF CHECKS-WITHOUT-ADDRESSES
003025                 MOVE ER-0364       TO  EMI-ERROR
003026                 PERFORM 9900-ERROR-FORMAT.
003027
003028     MOVE EIBTIME                TO  TIME-IN.
003029
003030     MOVE SAVE-DATE              TO  ADATEO.
003031     MOVE TIME-OUT               TO  ATIMEO.
003032     MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
003033     MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
003034     MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
003035
003036     
      * EXEC CICS SEND
003037*        FROM   (EL176AI)
003038*        MAPSET (WS-MAPSET-NAME)
003039*        MAP    (WS-MAP-NAME)
003040*        CURSOR ERASE
003041*    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009369' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303039333639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
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
           
003042
003043     PERFORM 9100-RETURN-TRAN.
003044
003045 8100-EXIT.
003046     EXIT.
003047
003048     EJECT
003049 8200-SEND-DATAONLY SECTION.
003050     IF EMI-ERROR NOT = 3130
003051         PERFORM 9900-ERROR-FORMAT
003052       ELSE
003053         IF TRANSACTION-SUCCESSFUL
003054             PERFORM 9900-ERROR-FORMAT
003055             IF CHECKS-WITHOUT-ADDRESSES
003056                 MOVE ER-0364       TO  EMI-ERROR
003057                 PERFORM 9900-ERROR-FORMAT.
003058
003059     MOVE EIBTIME                TO  TIME-IN.
003060
003061     MOVE SAVE-DATE              TO  ADATEO.
003062     MOVE TIME-OUT               TO  ATIMEO.
003063     MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
003064     MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
003065     MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
003066
003067     
      * EXEC CICS SEND DATAONLY
003068*        FROM   (EL176AI)
003069*        MAPSET (WS-MAPSET-NAME)
003070*        MAP    (WS-MAP-NAME)
003071*        CURSOR
003072*    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009400' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303039343030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
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
           
003073
003074     IF PI-PROCESSING-SW = ZERO
003075         PERFORM 9100-RETURN-TRAN.
003076
003077     MOVE ZERO                   TO  EMI-SUB
003078                                     EMI-NOTE-CTR
003079                                     EMI-WARNING-CTR
003080                                     EMI-FORCABLE-CTR
003081                                     EMI-FATAL-CTR.
003082
003083     MOVE '1'                    TO  EMI-SWITCH-AREA-1
003084                                     EMI-SWITCH-AREA-2.
003085
003086     MOVE SPACES                    TO  EMI-ERROR-LINES.
003087
003088 8200-EXIT.
003089     EXIT.
003090
003091     EJECT
003092 8300-SEND-TEXT SECTION.
003093     IF PI-PRINTER-STARTED-SW NOT = ZERO
003094         PERFORM 0700-END-PRINT.
003095
003096     
      * EXEC CICS SEND TEXT
003097*        FROM   (LOGOFF-TEXT)
003098*        LENGTH (LOGOFF-LENGTH)
003099*        ERASE  FREEKB
003100*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009429' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303039343239' TO DFHEIV0
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
           
003101
003102     
      * EXEC CICS RETURN
003103*    END-EXEC.
      *    MOVE '.(                    ''   #00009435' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039343335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003104
003105 8300-EXIT.
003106     EXIT.
003107
003108     EJECT
003109 8500-DATE-CONVERSION SECTION.
003110     
      * EXEC CICS LINK
003111*        PROGRAM  ('ELDATCV')
003112*        COMMAREA (DATE-CONVERSION-DATA)
003113*        LENGTH   (DC-COMM-LENGTH)
003114*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00009443' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039343433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003115
003116 8500-EXIT.
003117     EXIT.
003118
003119     EJECT
003120 8700-LOCATE-BENEFIT SECTION.
003121     
      * EXEC CICS HANDLE CONDITION
003122*        NOTFND (8700-EXIT)
003123*    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009454' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303039343534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003124
003125     MOVE SPACES                 TO  WS-KIND.
003126
003127     MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
003128     MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
003129     MOVE 'CNTL'                 TO  FILE-SWITCH.
003130
003131     
      * EXEC CICS READ
003132*        DATASET (WS-CONTROL-FILE-DSID)
003133*        RIDFLD  (WS-CONTROL-FILE-KEY)
003134*        SET     (ADDRESS OF CONTROL-FILE)
003135*        GTEQ
003136*    END-EXEC.
      *    MOVE '&"S        G          (   #00009464' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039343634' TO DFHEIV0
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
           
003137
003138     IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID
003139       OR WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
003140         GO TO 8700-EXIT.
003141
003142     MOVE +1                     TO  WS-INDEX.
003143
003144 8700-LOOKUP-BENEFIT.
003145     IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
003146         MOVE CF-BENEFIT-ALPHA (WS-INDEX)    TO WS-KIND
003147         MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO WS-COV-TYPE
003148         GO TO 8700-EXIT.
003149
003150     IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
003151         GO TO 8700-EXIT.
003152
003153     IF WS-INDEX LESS THAN +8
003154         ADD +1  TO  WS-INDEX
003155         GO TO 8700-LOOKUP-BENEFIT.
003156
003157 8700-EXIT.
003158     EXIT.
003159
003160     EJECT
003161 8800-NOT-OPEN    SECTION.
003162
003163     IF FILE-SWITCH = 'BENE'
003164         MOVE ER-7675            TO  EMI-ERROR.
003165
003166     IF FILE-SWITCH = 'ACCT'
003167         MOVE ER-0168            TO  EMI-ERROR.
003168
003169     IF FILE-SWITCH = 'CHKQ'
003170         MOVE ER-3776            TO  EMI-ERROR.
003171
003172     IF FILE-SWITCH = 'MSTR'
003173         MOVE ER-0154            TO  EMI-ERROR.
003174
003175     IF FILE-SWITCH = 'CERT'
003176         MOVE ER-0169            TO  EMI-ERROR.
003177
003178     IF FILE-SWITCH = 'PLCY'
003179         MOVE ER-9883            TO  EMI-ERROR.
003180
003181     IF FILE-SWITCH = 'PROD'
003182         MOVE ER-9886            TO  EMI-ERROR.
003183
003184     IF FILE-SWITCH = 'TRLR'
003185         MOVE ER-0172            TO  EMI-ERROR.
003186
003187     IF FILE-SWITCH = 'COMP'
003188         MOVE ER-2055            TO  EMI-ERROR.
003189
003190     IF FILE-SWITCH = 'PLAN'
003191         MOVE ER-9808            TO  EMI-ERROR.
003192
003193     IF FILE-SWITCH = 'FLAG'
003194         MOVE ER-3027            TO  EMI-ERROR.
003195
003196     IF FILE-SWITCH = 'DRFT'
003197         MOVE ER-3028            TO  EMI-ERROR.
003198
003199     MOVE ZERO                   TO  PI-PROCESSING-SW.
003200     MOVE -1                     TO  AOPTIONL.
003201     GO TO 8200-SEND-DATAONLY.
003202     EJECT
003203 9000-RETURN-CICS SECTION.
003204     MOVE 'EL005'                TO  THIS-PGM.
003205     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
003206     PERFORM 9300-XCTL.
003207
003208 9000-EXIT.
003209     EXIT.
003210
003211 9100-RETURN-TRAN SECTION.
003212     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
003213     MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
003214
003215     
      * EXEC CICS RETURN
003216*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
003217*        LENGTH   (PI-COMM-LENGTH)
003218*        TRANSID  (WS-TRANS-ID)
003219*    END-EXEC.
      *    MOVE '.(CT                  ''   #00009548' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039353438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003220
003221 9100-EXIT.
003222     EXIT.
003223
003224 9300-XCTL SECTION.
003225     IF PI-PRINTER-STARTED-SW NOT = ZERO
003226         PERFORM 0700-END-PRINT.
003227
003228     MOVE DFHENTER               TO  EIBAID.
003229
003230     
      * EXEC CICS XCTL
003231*        PROGRAM  (THIS-PGM)
003232*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
003233*        LENGTH   (PI-COMM-LENGTH)
003234*    END-EXEC.
      *    MOVE '.$C                   %   #00009563' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039353633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003235
003236 9300-EXIT.
003237     EXIT.
003238
003239     EJECT
003240 9400-CLEAR SECTION.
003241     MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
003242     PERFORM 9300-XCTL.
003243
003244 9400-EXIT.
003245     EXIT.
003246
003247 9600-PGMIDERR SECTION.
003248     
      * EXEC CICS HANDLE CONDITION
003249*        PGMIDERR (8300-SEND-TEXT)
003250*    END-EXEC.
      *    MOVE '"$L                   ! 4 #00009581' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303039353831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003251
003252     MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
003253
003254     MOVE 'EL005'                TO  THIS-PGM
003255                                     LOGOFF-PGM.
003256     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
003257     MOVE SPACES                 TO  PI-ENTRY-CD-1.
003258     PERFORM 9300-XCTL.
003259
003260 9600-EXIT.
003261     EXIT.
003262
003263 9900-ERROR-FORMAT SECTION.
003264     IF EMI-ERRORS-COMPLETE
003265         MOVE ER-3130               TO  EMI-ERROR
003266         GO TO 9900-EXIT.
003267
003268     
      * EXEC CICS LINK
003269*        PROGRAM  ('EL001')
003270*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
003271*        LENGTH   (EMI-COMM-LENGTH)
003272*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00009601' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039363031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003273
003274     MOVE ER-3130                   TO  EMI-ERROR.
003275
003276 9900-EXIT.
003277     EXIT.
003278
003279     EJECT
003280 9990-ERROR SECTION.
003281     MOVE DFHEIBLK TO EMI-LINE1.
003282     
      * EXEC CICS LINK
003283*        PROGRAM  ('EL004')
003284*        COMMAREA (EMI-LINE1)
003285*        LENGTH   (72)
003286*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009615' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039363135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003287
003288     PERFORM 8200-SEND-DATAONLY.
003289     GO TO 9100-RETURN-TRAN.
003290
003291 9990-EXIT.
003292     EXIT.
003293
003294 9995-SECURITY-VIOLATION.
003295*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00009646' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039363436' TO DFHEIV0
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
003296
003297 9995-EXIT.
003298     EXIT.
003299
003300 9999-LAST-PARAGRAPH SECTION.
003301     MOVE ZEROS TO RETURN-CODE.
003302     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8800-NOT-OPEN,
                     0180-MAIN-LOGIC,
                     0190-MAIN-LOGIC,
                     0900-TERMIDERR,
                     0910-ENQ-BUSY,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0030-MICR-CHK-SW,
                     5000-MICR-CLOSED,
                     5000-MICR-CLOSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0225-MAIN-LOGIC,
                     0230-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8800-NOT-OPEN,
                     0236-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8800-NOT-OPEN,
                     0237-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 9400-CLEAR,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 9999-DFHBACK,
                     0390-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 0326-NOTFND-EMPROD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 0340-MAIN-LOGIC,
                     0340-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 0351-READ-COMP-MASTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 0354-UPDATE-ACTIVITY-TRLRS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 0354-CHECK-FOR-AUTO-PAY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 0355-GET-ADDRESS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 0360-NO-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 0375-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 1020-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 2080-GET-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
