      *((program: EL142.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003
000004 PROGRAM-ID. EL142 .
000005*              PROGRAM CONVERTED BY
000006*              COBOL CONVERSION AID PO 5785-ABJ
000007*              CONVERSION DATE 06/29/95 10:53:47.
000008*                            VMOD=2.033
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
000021*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000022*            *                                                   *
000023*            *****************************************************
000024
000025*REMARKS.
000026*        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE THRU AND
000027*    PERFORM MAINTENANCE ON ACTIVITY TRAILERS.  THE TRAILERS ARE
000028*    CREATED BY OTHER VARIOUS FUNCTIONS.
000029
000030*    TRANS ID = EX25
000031
000032*    SCREENS     - EL142A - BROWSE QUALIFICATION
000033*                  EL142B - PAYMENTS
000034*                  EL142C - AUTOMATIC PAYMENTS
000035*                  EL142D - LETTERS
000036*                  EL142E - GENERAL INFORMATION
000037*                  EL142F - REMINDERS
000038*                  EL142G - DENIAL RECORD
000039*                  EL142H - RESERVES & EXPENSES
000040*                  EL142I - INCURED CHANGE INFORMATION
000041*                  EL142J - FORM INFORMATION
000042
000043*    ENTERED BY  - EL131  - CLAIM MAINTENANCE
000044*                  EL150  - STATUS
000045*                  EL1622 - CLAIM AUDIT
000046
000047*    EXIT TO     - CALLING PROGRAM
000048
000049*    INPUT FILES - ELTRLR - ACTIVITY TRAILERS
000050*                  ELMSTR - CLAIM MASTER
000051
000052*    OUTPUT FILES - ELTRLR - ACTIVITY TRAILERS
000053*                   ELMSTR - CLAIM MASTER
000054
000055*    COMMAREA    - PASSED.  WHEN CALLED BY EL150, A 4 BYTE
000056*                  CHARACTER, TRAILER SEQUENCE NUMBER, IS PASSED
000057*                  IN THE FIRST 4 BYTES OF THE PROGRAM-WORK-AREA
000058*                  OF THE INTERFACE BLOCK.
000059
000060*    NARRATIVE   - ON FIRST ENTRY, THE BROWSE QUALIFICATION SCREEN
000061*                  IS SENT.  IF THE ENTRY CAME FROM EL150, THE
000062*                  PASSED SEQUENCE NUMBER IS PLACED IN THE "START-
000063*                  ING SEQUENCE NUMBER FIELD" SO THAT THE OPERATOR
000064*                  HAS THE OPTION OF STARTING AT THE SAME POINT AS
000065*                  THE STATUS DISPLAY.
000066
000067*                  VIA THE QUALIFICATION SCREEN THE OPERATOR
000068*                  INDICATES WHAT TYPE OF TRAILERS ARE TO BE
000069*                  VIEWED, THE EARLIEST ACTIVITY AND A STARTING
000070*                  SEQUENCE NUMBER.  THE DATE IS OPTIONAL, SPACE
000071*                  IMPLIES ALL DATES.  IF THE SEQUENCE NUMBER IS
000072*                  NOT GIVEN, ZERO IS ASSUMED.  THE TRAILER FILE
000073*                  IS ALWAYS READ FORWARD, SO THAT ACTIVITY WILL
000074*                  BE SHOWN AS MOST RECENT FIRST.  THE QUALIFICA-
000075*                  TIONS ARE SAVED IN THE PROGRAM WORK AREA OF THE
000076*                  INTERFACE BLOCK AND THE FIRST QUALIFYING
000077*                  TRAILER IS READ.
000078
000079*                  THE DISPLAY AND MAINTENANCE TYPE DEPENDS ON THE
000080*                  TYPE OF TRAILER RECORD READ.  SCREENS EL142B
000081*                  THRU EL142H ARE USED DEPENDING ON THE RECORD
000082*                  TYPE.
000083******************************************************************
000084*                   C H A N G E   L O G
000085*
000086* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000087*-----------------------------------------------------------------
000088*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000089* EFFECTIVE    NUMBER
000090*-----------------------------------------------------------------
000091* 062602    2002030700006  PEMA  Add note type of 'S'
000092*                                  (special review)
000093* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
000094* 052506    2006030600001  AJRA  ADD PROOF DT TO PAYMENT & DENIAL
000095* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
000096* 101807    2007100100007  PEMA  INCREASE CLM RESERVE FIELDS
000097* 031808    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
000098* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
000099* 033010  CR2008100900001  PEMA  ADD DENIAL CODE EDITS
000100* 042110  CR2008100900001  PEMA  ADD DELETE PROCESSING FOR MAPG
000101* 050110    2009122800001  AJRA  ADD NAPERSOFT
000102* 100610    2009122800001  AJRA  LOCK RECV DT W/ RESEND PRINTED DT
000103* 102510    2009122800001  AJRA  UPDATE ELNAPS WHEN RESEND DT CHGD
000104* 102510    2009122800001  AJRA  ADD STOP DATE TO LETTER TRAILER
000105* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000106* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
000107* 041613    2013040400004  AJRA  ADD ENC CODE FOR SPECIAL HANDLING
000108* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000109* 102413    2013100800001  AJRA  ADD SPEC RELEASE, FIX ENX ENC COD
000110* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL
000111* 021114    2014012100002  AJRA  ADD CHECK CASHED DATE
000112* 081214  IR2014081100001  PEMA  CORRECT INT CHK CASHED DT PROCESS
000113* 091714  IR2014090800003  PEMA  ADD "manual" table to query
000114* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
000115* 051215  IR2015051100002  PEMA  Correct bogus cashed date
000116* 060315  IR2015060200004  PEMA  Correct cut-off date
000117* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000118* 013017  CR2017022000001  PEMA  DRAFTS TO CHECKS - DCC
000119* 120718  CR2018120400002  PEMA  REMOVE USER HARDCODING
000120* 040819  IR2019030400004  TANA  PROTECT HOLD UNTIL DATE FIELD
000121* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < I
000122* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000123* 011822  CR2019012500003  PEMA  Convert to SQLSERVER 2016
000124******************************************************************
000125
000126     EJECT
000127 ENVIRONMENT DIVISION.
000128
000129 DATA DIVISION.
000130
000131 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000132 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
000133 77  ws-sql-code                 pic s9(7) value zeros.
000134 77  ws-dis-sql-code             pic -9999999 value zeros.
000135 01  LCP-CURRENT-DATE-68.
000136     05  LCP-MONTH                 PIC X(2).
000137     05  FILLER                    PIC X VALUE '/'.
000138     05  LCP-DAY1                  PIC X(2).
000139     05  FILLER                    PIC X VALUE '/'.
000140     05  LCP-YEAR                  PIC X(2).
000141 01  LCP-CICS-DATE                 PIC 9(15).
000142
000143 77  FILLER  PIC X(32)  VALUE '********************************'.
000144 77  FILLER  PIC X(32)  VALUE '*   EL142  WORKING STORAGE     *'.
000145 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.033 *********'.
000146
000147*EXEC SQL
000148*   INCLUDE SQLDA
000149*END-EXEC
000150
000153*EXEC SQL
      *   INCLUDE SQLCA
      *END-EXEC
      *>>((file: SQLCA))
000001****************************************************************<*
000002* Copyright (c) 2016-2021 NTT DATA, Inc. All rights reserved.  *<*
000003* Users of NTT DATA Enterprise COBOL may freely                *<*
000004* redistribute this copybook.                                  *<*
000005****************************************************************<*
000006
000007 01  SQLCA GLOBAL.
000008     05  SQLCAID                PIC X(8).
000009     05  SQLCABC                PIC S9(9) COMP-5.
000010     05  SQLCODE                PIC S9(9) COMP-5.
000011     05  SQLERRM.
000012         49  SQLERRML           PIC S9(4) COMP-5.
000013         49  SQLERRMC           PIC X(254).
000014     05  SQLERRP                PIC X(8).
000015     05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
000016     05  SQLWARN.
000017         10 SQLWARN0            PIC X(1).
000018         10 SQLWARN1            PIC X(1).
000019         10 SQLWARN2            PIC X(1).
000020         10 SQLWARN3            PIC X(1).
000021         10 SQLWARN4            PIC X(1).
000022         10 SQLWARN5            PIC X(1).
000023         10 SQLWARN6            PIC X(1).
000024         10 SQLWARN7            PIC X(1).
000025     05  SQLSTATE               PIC X(5).
000026     05  SQLEXT                 PIC S9(5) COMP-3 VALUE 1.
      *<<((file: SQLCA))
000154
000156 EXEC SQL
          BEGIN DECLARE SECTION
000157 END-EXEC
000158
000159 01  SQLCMD                      PIC X(1024).
000160 01  SVR                         PIC X(32).
000161 01  USR                         PIC X(32).
000162 01  PASS                        PIC X(32).
000163 01  USR-PASS                    PIC X(64).
000164
000165 01  ws-sql-manual-data.
000166     05  ws-check-number-man     pic x(7).
000167     05  ws-check-company-man    pic xxx.
000168
000169 01  WS-SQL-DATA.
000170     05  ws-draft-or-check       pic x.
000171     05  WS-CHECK-TYPE           PIC X.
000172     05  WS-CHECK-NUMBER         PIC X(10).
000173     05  ws-claim-number         pic x(7).
000174     05  WS-CHECK-AMOUNT         PIC X(10).
000175     05  WS-CHECK-COMPANY        PIC X(5).
000176     05  WS-CHECK-CASHED-DT      PIC X(20).
000177
000178 01  ws-ach-data.
000179     05  ws-carrier              pic x.
000180     05  ws-state                pic xx.
000181     05  ws-account-no           pic x(10).
000182     05  ws-cert-no              pic x(11).
000183     05  ws-claim-no             pic x(7).
000184     05  ws-check-no             pic x(10).
000185     05  ws-cashed-date          pic x(10).
000186
000188 EXEC SQL
          END DECLARE SECTION
000189 END-EXEC
000190
000191
000192*                                    COPY ELCSCTM.
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
000193
000194*                                    COPY ELCSCRTY.
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
000195
000196*                                    COPY ELCDCTB.
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
000197
000198*                                    COPY ELCNWA.
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
000199
000200 01  WS-ELDENY-KEY.
000201     05  ELDENY-COMPANY-CD   PIC X.
000202     05  ELDENY-DENIAL-CODE  PIC X(4).
000203     05  FILLER              PIC X(10).
000204
000205 01  WS-RESPONSE             PIC S9(8)   COMP.
000206     88  RESP-NORMAL              VALUE +00.
000207     88  RESP-ERROR               VALUE +01.
000208     88  RESP-NOTFND              VALUE +13.
000209     88  RESP-NOTOPEN             VALUE +19.
000210     88  RESP-ENDFILE             VALUE +20.
000211
000212 01  FILLER                          COMP-3.
000213     05  WS-RECORD-COUNT             PIC S9(5)       VALUE ZERO.
000214     05  WS-RECORD-REMAINDER         PIC S9(5)       VALUE ZERO.
000215     05  WS-RECORD-DIV               PIC S9(5)       VALUE ZERO.
000216     05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
000217     05  WS-NOT-FOUND                PIC S9          VALUE ZERO.
000218     05  WS-ERROR-COUNT              PIC S9(3)       VALUE ZERO.
000219     05  WS-UPDATE-SW                PIC S9          VALUE ZERO.
000220     05  WS-COMPLETED-SUCCESSFUL     PIC S9          VALUE ZERO.
000221       88  TRANSACTION-SUCCESSFUL                    VALUE +1.
000222
000223     05  TIME-IN                     PIC S9(7)       VALUE ZERO.
000224     05  TIME-OUT                    REDEFINES
000225         TIME-IN                     PIC S9(3)V9(4).
000226
000227     05  WS-HPCTCDTI                 PIC S9(3)V99 VALUE ZERO.
000228     05  WS-HMANAMTI                 PIC S9(7)V99 VALUE ZERO.
000229     05  WS-HEXPAMTI                 PIC S9(5)V99 VALUE ZERO.
000230
000231 01  FILLER                          COMP  SYNC.
000232     05  SC-ITEM                     PIC S9(4)       VALUE +0001.
000233     05  WS-INDEX                    PIC S9(4)       VALUE ZERO.
000234
000235     05  WS-ACTIVITY-TRAILERS-LENGTH PIC S9(4)       VALUE +200.
000236     05  WS-CHECK-QUEUE-LENGTH       PIC S9(4)       VALUE +100.
000237     05  WS-DEEDIT-LENGTH            PIC S9(4)       VALUE +8.
000238     05  WS-HPCTCDT-LENGTH           PIC S9(4)       VALUE +7.
000239     05  WS-HMANAMT-LENGTH           PIC S9(4)       VALUE +12.
000240     05  WS-HEXPAMT-LENGTH           PIC S9(4)       VALUE +7.
000241     05  WS-CHECK-QUE                PIC S9(8)       VALUE ZERO.
000242     05  WS-CHECK-QUE-SEQ            PIC S9(4)       VALUE ZERO.
000243     05  WS-DMO-LENGTH               PIC S9(4)       VALUE +108.
000244     05  WS-DCT-LENGTH               PIC S9(4)       VALUE +53.
000245
000246 01  FILLER.
000247     05  WS-CL-CERT-KEY-DATA.
000248         10  WS-CL-CERT-CARRIER  PIC X.
000249         10  WS-CL-CERT-GROUPING PIC X(6).
000250         10  WS-CL-CERT-STATE    PIC XX.
000251         10  WS-CL-CERT-ACCOUNT  PIC X(10).
000252         10  WS-CL-CERT-EFF-DT   PIC XX.
000253     05  WS-CL-CERT-NO           PIC X(11).
000254     05  WS-CL-BENEFICIARY       PIC X(10).
000255     05  WS-CL-CCN               PIC X(16).
000256     05  WS-CL-CLAIM-NO          PIC X(7).
000257     05  WS-CL-CLAIM-TYPE        PIC X.
000258     05  WS-CL-INSURED-LAST-NAME PIC X(15).
000259     05  WS-CL-INSURED-NAME.
000260         10  WS-CL-INSURED-1ST-NAME PIC X(12).
000261         10  WS-CL-INSURED-MID-INIT PIC X.
000262     05  W-NAME-LAST             PIC  X(15).
000263     05  W-NAME-FIRST            PIC  X(15).
000264     05  W-NAME-MIDDLE.
000265         10  FILLER              PIC  X.
000266         10  W-NAME-MIDDLE-2     PIC  X.
000267         10  FILLER              PIC  X(13).
000268     05  WS-CL-NO-OF-PMTS-MADE   PIC S9(3) COMP-3.
000269
000270     05  NOTE-KEY.
000271         10  NOTE-COMP-CD            PIC X.
000272         10  NOTE-CERT-KEY.
000273             15  NOTE-CARRIER        PIC X.
000274             15  NOTE-GROUP          PIC X(6).
000275             15  NOTE-STATE          PIC XX.
000276             15  NOTE-ACCOUNT        PIC X(10).
000277             15  NOTE-DATE           PIC XX.
000278             15  NOTE-CERT-NO        PIC X(11).
000279
000280     05  WS-ACTIVITY-TRAILERS-KEY.
000281         10  WS-ATK-COMPANY-CODE     PIC X.
000282         10  WS-ATK-CARRIER          PIC X.
000283         10  WS-ATK-CLAIM-NO         PIC X(7).
000284         10  WS-ATK-CERT-NO.
000285             15  WS-ATK-CERT-NO-PRIME  PIC X(10).
000286             15  WS-ATK-CERT-NO-SUFX   PIC X.
000287         10  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.
000288
000289     05  WS-CLAIM-KEY.
000290         10  WS-CK-COMPANY-CODE     PIC X.
000291         10  WS-CK-CARRIER          PIC X.
000292         10  WS-CK-CLAIM-NO         PIC X(7).
000293         10  WS-CK-CERT-NO.
000294             15  WS-CK-CERT-NO-PRIME   PIC X(10).
000295             15  WS-CK-CERT-NO-SUFX    PIC X.
000296
000297     05  WS-ACCOUNT-MASTER-KEY.
000298         10  WS-AM-COMPANY-CD        PIC X.
000299         10  WS-AM-CARRIER           PIC X.
000300         10  WS-AM-GROUPING          PIC X(6).
000301         10  WS-AM-STATE             PIC XX.
000302         10  WS-AM-ACCOUNT           PIC X(10).
000303         10  WS-AM-EXPIRATION-DT     PIC XX.
000304         10  FILLER                  PIC X(4).
000305
000306     05  WS-PRODUCER-MASTER-KEY.
000307         10  WS-PD-COMPANY-CD        PIC X.
000308         10  WS-PD-CARRIER           PIC X.
000309         10  WS-PD-GROUPING          PIC X(6).
000310         10  WS-PD-STATE             PIC XX.
000311         10  WS-PD-PRODUCER          PIC X(10).
000312         10  WS-PD-EXPIRATION-DT     PIC XX.
000313
000314     05  WS-LETTER-ARCHIVE-KEY.
000315         10  WS-LA-COMPANY-CD        PIC X.
000316         10  WS-LA-ARCHIVE-NO        PIC S9(8)       COMP.
000317         10  WS-LA-RECORD-TYPE       PIC X.
000318*          88  HEADER-DATA                           VALUE '1'.
000319*          88  ADDRESS-DATA                          VALUE '2'.
000320*          88  TEXT-DATA                             VALUE '3'.
000321         10  WS-LA-LINE-SEQ-NO       PIC S9(4)       COMP.
000322
000323     05  WS-NAPERSOFT-KEY.
000324         10  WS-NA-COMPANY-CD        PIC X.
000325         10  WS-NA-CARRIER           PIC X.
000326         10  WS-NA-CLAIM-NO          PIC X(7).
000327         10  WS-NA-CERT-NO           PIC X(11).
000328         10  WS-NA-ARCHIVE-NO        PIC 9(8).
000329
000330     05  WS-ELENCC-KEY.
000331         10  WS-ELENCC-COMPANY-CD    PIC X.
000332         10  WS-ELENCC-REC-TYPE      PIC X.
000333         10  WS-ELENCC-ENC-CODE      PIC X(5).
000334         10  F                       PIC X(09).
000335
000336     05  WS-LETTER-ARCHIVE-ALT-KEY.
000337         10  WS-LA-ALT-COMPANY-CD    PIC X.
000338         10  WS-LA-ALT-RECORD-TYPE   PIC X.
000339         10  WS-LA-ALT-ARCHIVE-NO    PIC S9(8)       COMP.
000340         10  WS-LA-ATL-LINE-SEQ-NO   PIC S9(4)       COMP.
000341
000342     05  WS-CHECK-QUEUE-KEY.
000343         10  WS-CQ-COMPANY-CD        PIC X.
000344         10  WS-CQ-CONTROL-NUMBER    PIC S9(9)       COMP.
000345         10  WS-CQ-SEQUENCE-NUMBER   PIC S9(4)       COMP.
000346
000347     05  WS-CONTROL-FILE-KEY.
000348         16  CNTL-CO             PIC X(3).
000349         16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.
000350         16  CNTL-GENL           PIC X(4)    VALUE SPACES.
000351         16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.
000352
000353     05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL142S'.
000354
000355     05  EL142A                      PIC X(8) VALUE 'EL142A'.
000356     05  EL142B                      PIC X(8) VALUE 'EL142B'.
000357     05  EL142B2                     PIC X(8) VALUE 'EL142B2'.
000358     05  EL142C                      PIC X(8) VALUE 'EL142C'.
000359     05  EL142D                      PIC X(8) VALUE 'EL142D'.
000360     05  EL142D2                     PIC X(8) VALUE 'EL142D2'.
000361     05  EL142E                      PIC X(8) VALUE 'EL142E'.
000362     05  EL142F                      PIC X(8) VALUE 'EL142F'.
000363     05  EL142G                      PIC X(8) VALUE 'EL142G'.
000364     05  EL142H                      PIC X(8) VALUE 'EL142H'.
000365     05  EL142I                      PIC X(8) VALUE 'EL142I'.
000366     05  EL142J                      PIC X(8) VALUE 'EL142J'.
000367
000368     05  THIS-PGM                    PIC X(8)      VALUE 'EL142'.
000369     05  XCTL-PGM                    PIC X(8).
000370
000371     05  PGM-EL1501                  PIC X(8)      VALUE 'EL1501'.
000372
000373     05  EL001                       PIC X(8)      VALUE 'EL001'.
000374     05  EL004                       PIC X(8)      VALUE 'EL004'.
000375     05  EL005                       PIC X(8)      VALUE 'EL005'.
000376     05  EL010                       PIC X(8)      VALUE 'EL010'.
000377     05  EL126                       PIC X(8)      VALUE 'EL126'.
000378     05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.
000379
000380     05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.
000381     05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.
000382     05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.
000383     05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.
000384     05  WS-LETTER-ARCHIVE-DSID      PIC X(8) VALUE 'ELARCH'.
000385     05  WS-ELARCT-FILE-ID           PIC X(8) VALUE 'ELARCT'.
000386     05  WS-LETTER-ARCHIVE-DSID2     PIC X(8) VALUE 'ELARCH2'.
000387     05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.
000388     05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
000389     05  WS-NOTE-FILE-DSID           PIC X(8) VALUE 'ERNOTE'.
000390     05  WS-NAPERSOFT-DSID           PIC X(8) VALUE 'ELNAPS'.
000391     05  WS-ELENCC-FILE-DSID         PIC X(8) VALUE 'ELENCC'.
000392
000393     05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX25'.
000394
000395     05  WS-CLAIM-TYPE               PIC X           VALUE SPACES.
000396
000397     05  WS-PI-EL142-PRIORITY        PIC X           VALUE SPACES.
000398
000399     05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
000400                                     COMP SYNC.
000401
000402     05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
000403
000404     05  WS-DEEDIT-FIELD             PIC X(8)        VALUE ZERO.
000405
000406     05  WS-DEEDIT-FIELD-V0          REDEFINES
000407         WS-DEEDIT-FIELD             PIC S9(8).
000408
000409     05  WS-RESEND-DATE              PIC XX VALUE LOW-VALUES.
000410     05  WS-SEND-ON-DATE             PIC XX VALUE LOW-VALUES.
000411     05  WS-FOLLOW-UP-DATE           PIC XX VALUE LOW-VALUES.
000412     05  WS-RECEIVED-DATE            PIC XX VALUE LOW-VALUES.
000413     05  WS-RECEIVED-PHY-DATE        PIC XX VALUE LOW-VALUES.
000414     05  WS-RECEIVED-EMP-DATE        PIC XX VALUE LOW-VALUES.
000415     05  WS-START-DATE               PIC XX VALUE LOW-VALUES.
000416     05  WS-END-DATE                 PIC XX VALUE LOW-VALUES.
000417     05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.
000418     05  SAVE-DATE-CCYYMMDD.
000419         10  SAVE-DATE-CC            PIC XX VALUE SPACES.
000420         10  SAVE-DATE-YMD.
000421             15  SAVE-DATE-YY        PIC XX VALUE SPACES.
000422             15  FILLER              PIC X(4) VALUE SPACES.
000423     05  WS-DATE-SENT                PIC XX VALUE LOW-VALUES.
000424     05  WS-IN-PRINT-DATE            PIC XX VALUE LOW-VALUES.
000425     05  WS-REPRINTED-DATE           PIC XX VALUE LOW-VALUES.
000426     05  WS-ARCHIVE-NUMBER           PIC 9(8)  VALUE ZEROS.
000427     05  WS-RESEND-FORM-NUMBER       PIC X(4) VALUE LOW-VALUES.
000428     05  WS-STOP-LETTER-DATE         PIC XX VALUE LOW-VALUES.
000429     05  WS-ENCLOSURE-CODE           PIC X(3) VALUE SPACES.
000430     05  WS-TEMP-ENCCODE             PIC X(3) VALUE SPACES.
000431     05  WS-CHECK-AMT-TMP            PIC Z(7).99.
000432     05  WS-CHECK-AMT-TMPX REDEFINES
000433         WS-CHECK-AMT-TMP            PIC X(10).
000434
000435
000436     05  WS-CRSEL                    PIC XX VALUE LOW-VALUES.
000437     05  WS-VOIDSD                   PIC XX VALUE LOW-VALUES.
000438     05  WS-FORM-NUMBER              PIC X(4)    VALUE SPACES.
000439     05  WS-TEMP-DT                  PIC 99B99B99.
000440
000441     05  SLASH                       PIC X           VALUE '/'.
000442
000443     05  WS-SPACES                   PIC X           VALUE SPACES.
000444     05  WS-ZIP.
000445         10  WS-ZIP-CODE         PIC X(5).
000446         10  WS-DASH             PIC X     VALUE '-'.
000447         10  WS-ZIP-PLUS4        PIC X(4).
000448     05  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.
000449         10  WS-CAN-POSTAL-CD-1  PIC X(3).
000450         10  WS-DASH-CAN         PIC X.
000451         10  WS-CAN-POSTAL-CD-2  PIC X(3).
000452         10  WS-CAN-FILLER       PIC X(3).
000453     05  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.
000454     05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
000455                                 PIC 9(10).
000456
000457     05  WS-PRF-DT               PIC X(2)    VALUE LOW-VALUES.
000458
000459     EJECT
000460     05  ER-ZERO                 PIC 9(4)        VALUE 0000.
000461     05  ER-0000                 PIC 9(4)        VALUE 0000.
000462     05  ER-0004                 PIC 9(4)        VALUE 0004.
000463     05  ER-0006                 PIC 9(4)        VALUE 0006.
000464     05  ER-0008                 PIC 9(4)        VALUE 0008.
000465     05  ER-0021                 PIC 9(4)        VALUE 0021.
000466     05  ER-0023                 PIC 9(4)        VALUE 0023.
000467     05  ER-0029                 PIC 9(4)        VALUE 0029.
000468     05  ER-0068                 PIC 9(4)        VALUE 0068.
000469     05  ER-0070                 PIC 9(4)        VALUE 0070.
000470     05  ER-0105                 PIC 9(4)        VALUE 0105.
000471     05  ER-0106                 PIC 9(4)        VALUE 0106.
000472     05  ER-0107                 PIC 9(4)        VALUE 0107.
000473     05  ER-0109                 PIC 9(4)        VALUE 0109.
000474     05  ER-0111                 PIC 9(4)        VALUE 0111.
000475     05  ER-0175                 PIC 9(4)        VALUE 0175.
000476     05  ER-0198                 PIC 9(4)        VALUE 0198.
000477     05  ER-0285                 PIC 9(4)        VALUE 0285.
000478     05  ER-0286                 PIC 9(4)        VALUE 0286.
000479     05  ER-0287                 PIC 9(4)        VALUE 0287.
000480     05  ER-0288                 PIC 9(4)        VALUE 0288.
000481     05  ER-0289                 PIC 9(4)        VALUE 0289.
000482     05  ER-0290                 PIC 9(4)        VALUE 0290.
000483     05  ER-0291                 PIC 9(4)        VALUE 0291.
000484     05  ER-0292                 PIC 9(4)        VALUE 0292.
000485     05  ER-0293                 PIC 9(4)        VALUE 0293.
000486     05  ER-0294                 PIC 9(4)        VALUE 0294.
000487     05  ER-0295                 PIC 9(4)        VALUE 0295.
000488     05  ER-0296                 PIC 9(4)        VALUE 0296.
000489     05  ER-0297                 PIC 9(4)        VALUE 0297.
000490     05  ER-0298                 PIC 9(4)        VALUE 0298.
000491     05  ER-0299                 PIC 9(4)        VALUE 0299.
000492     05  ER-0300                 PIC 9(4)        VALUE 0300.
000493     05  ER-0303                 PIC 9(4)        VALUE 0303.
000494     05  ER-0310                 PIC 9(4)        VALUE 0310.
000495     05  ER-0324                 PIC 9(4)        VALUE 0324.
000496     05  ER-0325                 PIC 9(4)        VALUE 0325.
000497     05  ER-0327                 PIC 9(4)        VALUE 0327.
000498     05  ER-0328                 PIC 9(4)        VALUE 0328.
000499     05  ER-0329                 PIC 9(4)        VALUE 0329.
000500     05  ER-0341                 PIC 9(4)        VALUE 0341.
000501     05  ER-0342                 PIC 9(4)        VALUE 0342.
000502     05  ER-0344                 PIC 9(4)        VALUE 0344.
000503     05  ER-0384                 PIC 9(4)        VALUE 0384.
000504     05  ER-0388                 PIC 9(4)        VALUE 0388.
000505     05  ER-0532                 PIC 9(4)        VALUE 0532.
000506     05  ER-0538                 PIC 9(4)        VALUE 0538.
000507     05  ER-0550                 PIC 9(4)        VALUE 0550.
000508     05  ER-0551                 PIC 9(4)        VALUE 0551.
000509     05  ER-0564                 PIC 9(4)        VALUE 0564.
000510     05  ER-0570                 PIC 9(4)        VALUE 0570.
000511     05  ER-0571                 PIC 9(4)        VALUE 0571.
000512     05  ER-0574                 PIC 9(4)        VALUE 0574.
000513     05  ER-0575                 PIC 9(4)        VALUE 0575.
000514     05  ER-0576                 PIC 9(4)        VALUE 0576.
000515     05  ER-0577                 PIC 9(4)        VALUE 0577.
000516     05  ER-0578                 PIC 9(4)        VALUE 0578.
000517     05  ER-0579                 PIC 9(4)        VALUE 0579.
000518     05  ER-0580                 PIC 9(4)        VALUE 0580.
000519     05  ER-0581                 PIC 9(4)        VALUE 0581.
000520     05  ER-0641                 PIC 9(4)        VALUE 0641.
000521     05  ER-0642                 PIC 9(4)        VALUE 0642.
000522     05  ER-0643                 PIC 9(4)        VALUE 0643.
000523     05  ER-0755                 PIC 9(4)        VALUE 0755.
000524     05  ER-0830                 PIC 9(4)        VALUE 0830.
000525     05  ER-0849                 PIC 9(4)        VALUE 0849.
000526     05  ER-0873                 PIC 9(4)        VALUE 0873.
000527     05  ER-0884                 PIC 9(4)        VALUE 0884.
000528     05  ER-0897                 PIC 9(4)        VALUE 0897.
000529     05  ER-0919                 PIC 9(4)        VALUE 0919.
000530     05  ER-0921                 PIC 9(4)        VALUE 0921.
000531     05  ER-0937                 PIC 9(4)        VALUE 0937.
000532     05  ER-0946                 PIC 9(4)        VALUE 0946.
000533     05  ER-0947                 PIC 9(4)        VALUE 0947.
000534     05  ER-0948                 PIC 9(4)        VALUE 0948.
000535     05  ER-0949                 PIC 9(4)        VALUE 0949.
000536     05  ER-0950                 PIC 9(4)        VALUE 0950.
000537     05  ER-0951                 PIC 9(4)        VALUE 0951.
000538     05  ER-0954                 PIC 9(4)        VALUE 0954.
000539     05  ER-0969                 PIC 9(4)        VALUE 0969.
000540     05  ER-0974                 PIC 9(4)        VALUE 0974.
000541     05  ER-0975                 PIC 9(4)        VALUE 0975.
000542     05  ER-1560                 PIC 9(4)        VALUE 1560.
000543     05  ER-1561                 PIC 9(4)        VALUE 1561.
000544     05  ER-1566                 PIC 9(4)        VALUE 1566.
000545     05  ER-1567                 PIC 9(4)        VALUE 1567.
000546     05  ER-1568                 PIC 9(4)        VALUE 1568.
000547     05  ER-1569                 PIC 9(4)        VALUE 1569.
000548     05  ER-2466                 PIC 9(4)        VALUE 2466.
000549     05  ER-8003                 PIC 9(4)        VALUE 8003.
000550     05  ER-8004                 PIC 9(4)        VALUE 8004.
000551     05  ER-8051                 PIC 9(4)        VALUE 8051.
000552     05  ER-8052                 PIC 9(4)        VALUE 8052.
000553     05  ER-8053                 PIC 9(4)        VALUE 8053.
000554     05  ER-8054                 PIC 9(4)        VALUE 8054.
000555     05  ER-8055                 PIC 9(4)        VALUE 8055.
000556     05  ER-8056                 PIC 9(4)        VALUE 8056.
000557     05  ER-8057                 PIC 9(4)        VALUE 8057.
000558     05  ER-8058                 PIC 9(4)        VALUE 8058.
000559     05  ER-8059                 PIC 9(4)        VALUE 8059.
000560     05  ER-8060                 PIC 9(4)        VALUE 8060.
000561     05  ER-8061                 PIC 9(4)        VALUE 8061.
000562     05  ER-8062                 PIC 9(4)        VALUE 8062.
000563     05  ER-8063                 PIC 9(4)        VALUE 8063.
000564     05  ER-8064                 PIC 9(4)        VALUE 8064.
000565     05  ER-8065                 PIC 9(4)        VALUE 8065.
000566     05  ER-8066                 PIC 9(4)        VALUE 8066.
000567     05  ER-8152                 PIC 9(4)        VALUE 8152.
000568     05  ER-8153                 PIC 9(4)        VALUE 8153.
000569     05  ER-8154                 PIC 9(4)        VALUE 8154.
000570     05  ER-8155                 PIC 9(4)        VALUE 8155.
000571     05  ER-9616                 PIC 9(4)        VALUE 9616.
000572
000573 01  HAN-PAYMENT-NOTE-DATA.
000574     12  WS-HAN-PAYMENT-NOTE.
000575         16  WS-HAN-PMT-CODE     PIC X.
000576         16  WS-HAN-PMT-TEXT     PIC X(59).
000577
000578 01  HAN-LETTER-REASON-DATA.
000579     12  WS-REASON-TEXT.
000580         16  WS-RE-NDX           PIC 99.
000581         16  FILLER              PIC X(68).
000582
000583     12  HAN-REASON-TABLE.
000584         16  FILLER              PIC X(50) VALUE
000585           'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.
000586         16  FILLER              PIC X(50) VALUE
000587           'CHECKING PRE-EXISTING CONDITION                   '.
000588         16  FILLER              PIC X(50) VALUE
000589           'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.
000590         16  FILLER              PIC X(50) VALUE
000591           'LETTER TO INSURED                                 '.
000592         16  FILLER              PIC X(50) VALUE
000593           'LETTER TO CREDITOR                                '.
000594         16  FILLER              PIC X(50) VALUE
000595           'LETTER TO EMPLOYER                                '.
000596         16  FILLER              PIC X(50) VALUE
000597           'LETTER TO INSURED / 2ND REQUEST                   '.
000598         16  FILLER              PIC X(50) VALUE
000599           'LETTER TO CREDITOR / 2ND REQUEST                  '.
000600         16  FILLER              PIC X(50) VALUE
000601           'LETTER TO EMPLOYER / 2ND REQUEST                  '.
000602         16  FILLER              PIC X(50) VALUE
000603           'AWAITING INITIAL CLAIM FORM                       '.
000604         16  FILLER              PIC X(50) VALUE
000605           'AWAITING SUPPLEMENTAL INFORMATION                 '.
000606         16  FILLER              PIC X(50) VALUE
000607           'DENIED / PRE-EXISTING CONDITION                   '.
000608         16  FILLER              PIC X(50) VALUE
000609           'DENIED / WAITING PERIOD NOT MET                   '.
000610         16  FILLER              PIC X(50) VALUE
000611           'DENIED / NORMAL PREGNANCY                         '.
000612         16  FILLER              PIC X(50) VALUE
000613           'DENIED / ACT OF WAR                               '.
000614         16  FILLER              PIC X(50) VALUE
000615           'DENIED / NOT TOTALLY DISABLED                     '.
000616         16  FILLER              PIC X(50) VALUE
000617           'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.
000618         16  FILLER              PIC X(50) VALUE
000619           'DENIED / NO COVERAGE INFORCE                      '.
000620         16  FILLER              PIC X(50) VALUE
000621           'DENIED / DISABLED ON DATE OF LOAN                 '.
000622         16  FILLER              PIC X(50) VALUE
000623           'DENIED / OVER MAXIMUM AGE                         '.
000624         16  FILLER              PIC X(50) VALUE
000625           'CLOSED / CLAIM INFO NOT PROVIDED                  '.
000626         16  FILLER              PIC X(50) VALUE
000627           'PHYSICIAN INFORMATION INCOMPLETE                  '.
000628         16  FILLER              PIC X(50) VALUE
000629           'ACKNOWLEDGEMENT LETTER TO INSURED                 '.
000630
000631     12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.
000632         16  HAN-TABLE-ENTRIES  OCCURS  23  TIMES.
000633             20  HAN-REASON-TEXT PIC X(50).
000634
000635     EJECT
000636*                                COPY ELCINTF.
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
000637
000638     12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
000639         16  PI-TRAILER-NUMBER       PIC 9(4) COMP.
000640         16  PI-EL142-PRIORITY       PIC X.
000641         16  FILLER                  PIC X.
000642
000643         16  PI-MAP-NAME             PIC X(8).
000644
000645         16  FILLER                  REDEFINES
000646             PI-MAP-NAME.
000647             20  FILLER              PIC XX.
000648             20  PI-MAP-NUMBER       PIC X(6).
000649
000650         16  PI-QUALIFICATION-SWITCHES    COMP-3.
000651             20  PI-REMINDERS-SW     PIC S9.
000652             20  PI-LETTERS-SW       PIC S9.
000653             20  PI-PAYMENTS-SW      PIC S9.
000654             20  PI-AUTO-PAY-SW      PIC S9.
000655             20  PI-NOTES-SW         PIC S9.
000656             20  PI-RES-EXP-SW       PIC S9.
000657             20  PI-DENIALS-SW       PIC S9.
000658             20  PI-INCURRED-DATE-SW PIC S9.
000659             20  PI-FORMS-SW         PIC S9.
000660
000661         16  PI-AFTER-DATE           PIC XX.
000662         16  PI-AFTER-DATE-2         PIC XX.
000663         16  PI-AFTER-DATE-3         PIC XX.
000664         16  PI-HOLD-UNTIL-DATE      PIC XX.
000665
000666         16  PI-ACTIVITY-TRAILERS-KEY.
000667             20  PI-ATK-COMPANY-CODE PIC X.
000668             20  PI-ATK-CARRIER      PIC X.
000669             20  PI-ATK-CLAIM-NO     PIC X(7).
000670             20  PI-ATK-CERT-NO.
000671                 24  PI-ATK-CERT-NO-PRIME  PIC X(10).
000672                 24  PI-ATK-CERT-NO-SUFX   PIC X.
000673             20  PI-ATK-SEQUENCE-NO  PIC S9(4) COMP.
000674
000675         16  PI-PREV-ACTIVITY-TRAILERS-KEY.
000676             20  PI-PREV-ATK-COMPANY-CODE PIC X.
000677             20  PI-PREV-ATK-CARRIER      PIC X.
000678             20  PI-PREV-ATK-CLAIM-NO     PIC X(7).
000679             20  PI-PREV-ATK-CERT-NO.
000680                 24  PI-PREV-ATK-CERT-NO-PRIME PIC X(10).
000681                 24  PI-PREV-ATK-CERT-NO-SUFX  PIC X.
000682             20  PI-PREV-ATK-SEQUENCE-NO  PIC S9(4) COMP.
000683
000684         16  PI-SAVE-KEY.
000685             20  PI-SAVE-ATK-COMPANY-CODE PIC X.
000686             20  PI-SAVE-ATK-CARRIER      PIC X.
000687             20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).
000688             20  PI-SAVE-ATK-CERT-NO.
000689                 24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X(10).
000690                 24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X.
000691             20  PI-SAVE-ATK-SEQUENCE-NO        PIC S9(4) COMP.
000692
000693         16  PI-PREV-AID             PIC X.
000694
000695         16  PI-RECORD-COUNT         PIC S9  COMP-3.
000696         16  PI-END-OF-FILE          PIC S9  COMP-3.
000697         16  PI-DENIAL-REASON-CODE   PIC X(4).
000698         16  PI-MAPG-DELETE-CNT      PIC 9.
000699         16  FILLER                  PIC X.
000700
000701         16  PI-FIRST-TIME-SW        PIC X.
000702             88  FIRST-TIME                       VALUE 'Y'.
000703
000704         16  PI-SAVE-LAST-MAINT-DT   PIC XX.
000705         16  PI-SAVE-LAST-UPD-BY     PIC X(4).
000706         16  PI-APPROVAL-LEVEL       PIC X.
000707         16  PI-ENC-CODE             PIC X(3).
000708         16  PI-CREATED-IN-NAPERSOFT PIC X.
000709         16  pi-den-recorded-dt      pic xx.
000710         16  pi-incurred-dt          pic xx.
000711
000712         16  FILLER                  PIC X(520).
000713
000714     EJECT
000715*                                    COPY EL142S.
      *>>((file: EL142S))
000001 01  EL142BI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  BDATEL PIC S9(0004) COMP.
000005     05  BDATEF PIC  X(0001).
000006     05  FILLER REDEFINES BDATEF.
000007         10  BDATEA PIC  X(0001).
000008     05  BDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  BTIMEL PIC S9(0004) COMP.
000011     05  BTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES BTIMEF.
000013         10  BTIMEA PIC  X(0001).
000014     05  BTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  BTYPEL PIC S9(0004) COMP.
000017     05  BTYPEF PIC  X(0001).
000018     05  FILLER REDEFINES BTYPEF.
000019         10  BTYPEA PIC  X(0001).
000020     05  BTYPEI PIC  X(0010).
000021*    -------------------------------
000022     05  BRECDTEL PIC S9(0004) COMP.
000023     05  BRECDTEF PIC  X(0001).
000024     05  FILLER REDEFINES BRECDTEF.
000025         10  BRECDTEA PIC  X(0001).
000026     05  BRECDTEI PIC  X(0008).
000027*    -------------------------------
000028     05  BBYL PIC S9(0004) COMP.
000029     05  BBYF PIC  X(0001).
000030     05  FILLER REDEFINES BBYF.
000031         10  BBYA PIC  X(0001).
000032     05  BBYI PIC  X(0004).
000033*    -------------------------------
000034     05  BTLRTYPL PIC S9(0004) COMP.
000035     05  BTLRTYPF PIC  X(0001).
000036     05  FILLER REDEFINES BTLRTYPF.
000037         10  BTLRTYPA PIC  X(0001).
000038     05  BTLRTYPI PIC  X(0001).
000039*    -------------------------------
000040     05  BSEQL PIC S9(0004) COMP.
000041     05  BSEQF PIC  X(0001).
000042     05  FILLER REDEFINES BSEQF.
000043         10  BSEQA PIC  X(0001).
000044     05  BSEQI PIC  X(0004).
000045*    -------------------------------
000046     05  BMANTBYL PIC S9(0004) COMP.
000047     05  BMANTBYF PIC  X(0001).
000048     05  FILLER REDEFINES BMANTBYF.
000049         10  BMANTBYA PIC  X(0001).
000050     05  BMANTBYI PIC  X(0004).
000051*    -------------------------------
000052     05  BMANTONL PIC S9(0004) COMP.
000053     05  BMANTONF PIC  X(0001).
000054     05  FILLER REDEFINES BMANTONF.
000055         10  BMANTONA PIC  X(0001).
000056     05  BMANTONI PIC  X(0008).
000057*    -------------------------------
000058     05  BMANTATL PIC S9(0004) COMP.
000059     05  BMANTATF PIC  X(0001).
000060     05  FILLER REDEFINES BMANTATF.
000061         10  BMANTATA PIC  X(0001).
000062     05  BMANTATI PIC  X(0005).
000063*    -------------------------------
000064     05  BMAINTL PIC S9(0004) COMP.
000065     05  BMAINTF PIC  X(0001).
000066     05  FILLER REDEFINES BMAINTF.
000067         10  BMAINTA PIC  X(0001).
000068     05  BMAINTI PIC  X(0001).
000069*    -------------------------------
000070     05  BEOBYNL PIC S9(0004) COMP.
000071     05  BEOBYNF PIC  X(0001).
000072     05  FILLER REDEFINES BEOBYNF.
000073         10  BEOBYNA PIC  X(0001).
000074     05  BEOBYNI PIC  X(0001).
000075*    -------------------------------
000076     05  BCLMYNL PIC S9(0004) COMP.
000077     05  BCLMYNF PIC  X(0001).
000078     05  FILLER REDEFINES BCLMYNF.
000079         10  BCLMYNA PIC  X(0001).
000080     05  BCLMYNI PIC  X(0001).
000081*    -------------------------------
000082     05  BSRVYNL PIC S9(0004) COMP.
000083     05  BSRVYNF PIC  X(0001).
000084     05  FILLER REDEFINES BSRVYNF.
000085         10  BSRVYNA PIC  X(0001).
000086     05  BSRVYNI PIC  X(0001).
000087*    -------------------------------
000088     05  BACHPMTL PIC S9(0004) COMP.
000089     05  BACHPMTF PIC  X(0001).
000090     05  FILLER REDEFINES BACHPMTF.
000091         10  BACHPMTA PIC  X(0001).
000092     05  BACHPMTI PIC  X(0003).
000093*    -------------------------------
000094     05  BPRFDTL PIC S9(0004) COMP.
000095     05  BPRFDTF PIC  X(0001).
000096     05  FILLER REDEFINES BPRFDTF.
000097         10  BPRFDTA PIC  X(0001).
000098     05  BPRFDTI PIC  X(0008).
000099*    -------------------------------
000100     05  BSPRELL PIC S9(0004) COMP.
000101     05  BSPRELF PIC  X(0001).
000102     05  FILLER REDEFINES BSPRELF.
000103         10  BSPRELA PIC  X(0001).
000104     05  BSPRELI PIC  X(0001).
000105*    -------------------------------
000106     05  BCKNOL PIC S9(0004) COMP.
000107     05  BCKNOF PIC  X(0001).
000108     05  FILLER REDEFINES BCKNOF.
000109         10  BCKNOA PIC  X(0001).
000110     05  BCKNOI PIC  X(0007).
000111*    -------------------------------
000112     05  BPAYEEL PIC S9(0004) COMP.
000113     05  BPAYEEF PIC  X(0001).
000114     05  FILLER REDEFINES BPAYEEF.
000115         10  BPAYEEA PIC  X(0001).
000116     05  BPAYEEI PIC  X(0011).
000117*    -------------------------------
000118     05  BDTWRITL PIC S9(0004) COMP.
000119     05  BDTWRITF PIC  X(0001).
000120     05  FILLER REDEFINES BDTWRITF.
000121         10  BDTWRITA PIC  X(0001).
000122     05  BDTWRITI PIC  X(0008).
000123*    -------------------------------
000124     05  BPNAMEL PIC S9(0004) COMP.
000125     05  BPNAMEF PIC  X(0001).
000126     05  FILLER REDEFINES BPNAMEF.
000127         10  BPNAMEA PIC  X(0001).
000128     05  BPNAMEI PIC  X(0030).
000129*    -------------------------------
000130     05  BWRITBYL PIC S9(0004) COMP.
000131     05  BWRITBYF PIC  X(0001).
000132     05  FILLER REDEFINES BWRITBYF.
000133         10  BWRITBYA PIC  X(0001).
000134     05  BWRITBYI PIC  X(0004).
000135*    -------------------------------
000136     05  BAPPVBYL PIC S9(0004) COMP.
000137     05  BAPPVBYF PIC  X(0001).
000138     05  FILLER REDEFINES BAPPVBYF.
000139         10  BAPPVBYA PIC  X(0001).
000140     05  BAPPVBYI PIC  X(0004).
000141*    -------------------------------
000142     05  BAMTL PIC S9(0004) COMP.
000143     05  BAMTF PIC  X(0001).
000144     05  FILLER REDEFINES BAMTF.
000145         10  BAMTA PIC  X(0001).
000146     05  BAMTI PIC  X(0013).
000147*    -------------------------------
000148     05  BRESERVL PIC S9(0004) COMP.
000149     05  BRESERVF PIC  X(0001).
000150     05  FILLER REDEFINES BRESERVF.
000151         10  BRESERVA PIC  X(0001).
000152     05  BRESERVI PIC  X(0010).
000153*    -------------------------------
000154     05  BTHRUHDL PIC S9(0004) COMP.
000155     05  BTHRUHDF PIC  X(0001).
000156     05  FILLER REDEFINES BTHRUHDF.
000157         10  BTHRUHDA PIC  X(0001).
000158     05  BTHRUHDI PIC  X(0014).
000159*    -------------------------------
000160     05  BPDTHRUL PIC S9(0004) COMP.
000161     05  BPDTHRUF PIC  X(0001).
000162     05  FILLER REDEFINES BPDTHRUF.
000163         10  BPDTHRUA PIC  X(0001).
000164     05  BPDTHRUI PIC  X(0008).
000165*    -------------------------------
000166     05  BEXPL PIC S9(0004) COMP.
000167     05  BEXPF PIC  X(0001).
000168     05  FILLER REDEFINES BEXPF.
000169         10  BEXPA PIC  X(0001).
000170     05  BEXPI PIC  X(0010).
000171*    -------------------------------
000172     05  BDAYSPDL PIC S9(0004) COMP.
000173     05  BDAYSPDF PIC  X(0001).
000174     05  FILLER REDEFINES BDAYSPDF.
000175         10  BDAYSPDA PIC  X(0001).
000176     05  BDAYSPDI PIC  X(0007).
000177*    -------------------------------
000178     05  BRATHDL PIC S9(0004) COMP.
000179     05  BRATHDF PIC  X(0001).
000180     05  FILLER REDEFINES BRATHDF.
000181         10  BRATHDA PIC  X(0001).
000182     05  BRATHDI PIC  X(0014).
000183*    -------------------------------
000184     05  BDAYRATL PIC S9(0004) COMP.
000185     05  BDAYRATF PIC  X(0001).
000186     05  FILLER REDEFINES BDAYRATF.
000187         10  BDAYRATA PIC  X(0001).
000188     05  BDAYRATI PIC  X(0010).
000189*    -------------------------------
000190     05  BPAYTYPL PIC S9(0004) COMP.
000191     05  BPAYTYPF PIC  X(0001).
000192     05  FILLER REDEFINES BPAYTYPF.
000193         10  BPAYTYPA PIC  X(0001).
000194     05  BPAYTYPI PIC  X(0022).
000195*    -------------------------------
000196     05  BCRSELL PIC S9(0004) COMP.
000197     05  BCRSELF PIC  X(0001).
000198     05  FILLER REDEFINES BCRSELF.
000199         10  BCRSELA PIC  X(0001).
000200     05  BCRSELI PIC  X(0008).
000201*    -------------------------------
000202     05  BFORCEDL PIC S9(0004) COMP.
000203     05  BFORCEDF PIC  X(0001).
000204     05  FILLER REDEFINES BFORCEDF.
000205         10  BFORCEDA PIC  X(0001).
000206     05  BFORCEDI PIC  X(0001).
000207*    -------------------------------
000208     05  BVOIDSDL PIC S9(0004) COMP.
000209     05  BVOIDSDF PIC  X(0001).
000210     05  FILLER REDEFINES BVOIDSDF.
000211         10  BVOIDSDA PIC  X(0001).
000212     05  BVOIDSDI PIC  X(0008).
000213*    -------------------------------
000214     05  BVOIDDTL PIC S9(0004) COMP.
000215     05  BVOIDDTF PIC  X(0001).
000216     05  FILLER REDEFINES BVOIDDTF.
000217         10  BVOIDDTA PIC  X(0001).
000218     05  BVOIDDTI PIC  X(0008).
000219*    -------------------------------
000220     05  BCKQUEL PIC S9(0004) COMP.
000221     05  BCKQUEF PIC  X(0001).
000222     05  FILLER REDEFINES BCKQUEF.
000223         10  BCKQUEA PIC  X(0001).
000224     05  BCKQUEI PIC  S9(8).
000225*    -------------------------------
000226     05  BCKSEQL PIC S9(0004) COMP.
000227     05  BCKSEQF PIC  X(0001).
000228     05  FILLER REDEFINES BCKSEQF.
000229         10  BCKSEQA PIC  X(0001).
000230     05  BCKSEQI PIC  S9(4).
000231*    -------------------------------
000232     05  BNOTE1L PIC S9(0004) COMP.
000233     05  BNOTE1F PIC  X(0001).
000234     05  FILLER REDEFINES BNOTE1F.
000235         10  BNOTE1A PIC  X(0001).
000236     05  BNOTE1I PIC  X(0060).
000237*    -------------------------------
000238     05  BNOTE2L PIC S9(0004) COMP.
000239     05  BNOTE2F PIC  X(0001).
000240     05  FILLER REDEFINES BNOTE2F.
000241         10  BNOTE2A PIC  X(0001).
000242     05  BNOTE2I PIC  X(0060).
000243*    -------------------------------
000244     05  BORIGINL PIC S9(0004) COMP.
000245     05  BORIGINF PIC  X(0001).
000246     05  FILLER REDEFINES BORIGINF.
000247         10  BORIGINA PIC  X(0001).
000248     05  BORIGINI PIC  X(0010).
000249*    -------------------------------
000250     05  BPMTORGL PIC S9(0004) COMP.
000251     05  BPMTORGF PIC  X(0001).
000252     05  FILLER REDEFINES BPMTORGF.
000253         10  BPMTORGA PIC  X(0001).
000254     05  BPMTORGI PIC  X(0001).
000255*    -------------------------------
000256     05  BCASHEDL PIC S9(0004) COMP.
000257     05  BCASHEDF PIC  X(0001).
000258     05  FILLER REDEFINES BCASHEDF.
000259         10  BCASHEDA PIC  X(0001).
000260     05  BCASHEDI PIC  X(0008).
000261*    -------------------------------
000262     05  BEXPHDGL PIC S9(0004) COMP.
000263     05  BEXPHDGF PIC  X(0001).
000264     05  FILLER REDEFINES BEXPHDGF.
000265         10  BEXPHDGA PIC  X(0001).
000266     05  BEXPHDGI PIC  X(0014).
000267*    -------------------------------
000268     05  BEXPTYPL PIC S9(0004) COMP.
000269     05  BEXPTYPF PIC  X(0001).
000270     05  FILLER REDEFINES BEXPTYPF.
000271         10  BEXPTYPA PIC  X(0001).
000272     05  BEXPTYPI PIC  X(0001).
000273*    -------------------------------
000274     05  BCRACPL PIC S9(0004) COMP.
000275     05  BCRACPF PIC  X(0001).
000276     05  FILLER REDEFINES BCRACPF.
000277         10  BCRACPA PIC  X(0001).
000278     05  BCRACPI PIC  X(0008).
000279*    -------------------------------
000280     05  BHOLDATL PIC S9(0004) COMP.
000281     05  BHOLDATF PIC  X(0001).
000282     05  FILLER REDEFINES BHOLDATF.
000283         10  BHOLDATA PIC  X(0001).
000284     05  BHOLDATI PIC  X(0008).
000285*    -------------------------------
000286     05  BVOIDACL PIC S9(0004) COMP.
000287     05  BVOIDACF PIC  X(0001).
000288     05  FILLER REDEFINES BVOIDACF.
000289         10  BVOIDACA PIC  X(0001).
000290     05  BVOIDACI PIC  X(0008).
000291*    -------------------------------
000292     05  BEMSG1L PIC S9(0004) COMP.
000293     05  BEMSG1F PIC  X(0001).
000294     05  FILLER REDEFINES BEMSG1F.
000295         10  BEMSG1A PIC  X(0001).
000296     05  BEMSG1I PIC  X(0079).
000297*    -------------------------------
000298     05  BPFKL PIC S9(0004) COMP.
000299     05  BPFKF PIC  X(0001).
000300     05  FILLER REDEFINES BPFKF.
000301         10  BPFKA PIC  X(0001).
000302     05  BPFKI PIC  9(2).
000303 01  EL142BO REDEFINES EL142BI.
000304     05  FILLER            PIC  X(0012).
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  BDATEO PIC  X(0008).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  BTIMEO PIC  99.99.
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  BTYPEO PIC  X(0010).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  BRECDTEO PIC  X(0008).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  BBYO PIC  X(0004).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  BTLRTYPO PIC  X(0001).
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  BSEQO PIC  9999.
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  BMANTBYO PIC  X(0004).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  BMANTONO PIC  X(0008).
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  BMANTATO PIC  99.99.
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  BMAINTO PIC  X(0001).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  BEOBYNO PIC  X(0001).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  BCLMYNO PIC  X(0001).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  BSRVYNO PIC  X(0001).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  BACHPMTO PIC  X(0003).
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  BPRFDTO PIC  99B99B99.
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  BSPRELO PIC  X(0001).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  BCKNOO PIC  X(0007).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  BPAYEEO PIC  X(0011).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  BDTWRITO PIC  X(0008).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  BPNAMEO PIC  X(0030).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  BWRITBYO PIC  X(0004).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  BAPPVBYO PIC  X(0004).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  BAMTO PIC  Z,ZZZ,ZZ9.99-.
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  BRESERVO PIC  ZZ,ZZ9.99-.
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  BTHRUHDO PIC  X(0014).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  BPDTHRUO PIC  X(0008).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  BEXPO PIC  ZZ,ZZ9.99-.
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  BDAYSPDO PIC  ZZ,ZZ9-.
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  BRATHDO PIC  X(0014).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  BDAYRATO PIC  ZZ9.99999-.
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  BPAYTYPO PIC  X(0022).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  BCRSELO PIC  X(0008).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  BFORCEDO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  BVOIDSDO PIC  X(0008).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  BVOIDDTO PIC  X(0008).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  BCKQUEO PIC  Z(7)9.
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  BCKSEQO PIC  Z(3)9.
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  BNOTE1O PIC  X(0060).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  BNOTE2O PIC  X(0060).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  BORIGINO PIC  X(0010).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  BPMTORGO PIC  X(0001).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  BCASHEDO PIC  X(0008).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  BEXPHDGO PIC  X(0014).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  BEXPTYPO PIC  X(0001).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  BCRACPO PIC  X(0008).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  BHOLDATO PIC  X(0008).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  BVOIDACO PIC  X(0008).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  BEMSG1O PIC  X(0079).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  BPFKO PIC  99.
000455*    -------------------------------
000456 01  EL142HI REDEFINES EL142BI.
000457     05  FILLER            PIC  X(0012).
000458*    -------------------------------
000459     05  HDATEL PIC S9(0004) COMP.
000460     05  HDATEF PIC  X(0001).
000461     05  FILLER REDEFINES HDATEF.
000462         10  HDATEA PIC  X(0001).
000463     05  HDATEI PIC  X(0008).
000464*    -------------------------------
000465     05  HTIMEL PIC S9(0004) COMP.
000466     05  HTIMEF PIC  X(0001).
000467     05  FILLER REDEFINES HTIMEF.
000468         10  HTIMEA PIC  X(0001).
000469     05  HTIMEI PIC  X(0005).
000470*    -------------------------------
000471     05  HTYPEL PIC S9(0004) COMP.
000472     05  HTYPEF PIC  X(0001).
000473     05  FILLER REDEFINES HTYPEF.
000474         10  HTYPEA PIC  X(0001).
000475     05  HTYPEI PIC  X(0010).
000476*    -------------------------------
000477     05  HRECDTEL PIC S9(0004) COMP.
000478     05  HRECDTEF PIC  X(0001).
000479     05  FILLER REDEFINES HRECDTEF.
000480         10  HRECDTEA PIC  X(0001).
000481     05  HRECDTEI PIC  X(0008).
000482*    -------------------------------
000483     05  HBYL PIC S9(0004) COMP.
000484     05  HBYF PIC  X(0001).
000485     05  FILLER REDEFINES HBYF.
000486         10  HBYA PIC  X(0001).
000487     05  HBYI PIC  X(0004).
000488*    -------------------------------
000489     05  HTLRTYPL PIC S9(0004) COMP.
000490     05  HTLRTYPF PIC  X(0001).
000491     05  FILLER REDEFINES HTLRTYPF.
000492         10  HTLRTYPA PIC  X(0001).
000493     05  HTLRTYPI PIC  X(0001).
000494*    -------------------------------
000495     05  HSEQL PIC S9(0004) COMP.
000496     05  HSEQF PIC  X(0001).
000497     05  FILLER REDEFINES HSEQF.
000498         10  HSEQA PIC  X(0001).
000499     05  HSEQI PIC  X(0004).
000500*    -------------------------------
000501     05  HMANTBYL PIC S9(0004) COMP.
000502     05  HMANTBYF PIC  X(0001).
000503     05  FILLER REDEFINES HMANTBYF.
000504         10  HMANTBYA PIC  X(0001).
000505     05  HMANTBYI PIC  X(0004).
000506*    -------------------------------
000507     05  HMANTONL PIC S9(0004) COMP.
000508     05  HMANTONF PIC  X(0001).
000509     05  FILLER REDEFINES HMANTONF.
000510         10  HMANTONA PIC  X(0001).
000511     05  HMANTONI PIC  X(0008).
000512*    -------------------------------
000513     05  HMANTATL PIC S9(0004) COMP.
000514     05  HMANTATF PIC  X(0001).
000515     05  FILLER REDEFINES HMANTATF.
000516         10  HMANTATA PIC  X(0001).
000517     05  HMANTATI PIC  X(0005).
000518*    -------------------------------
000519     05  HMAINTL PIC S9(0004) COMP.
000520     05  HMAINTF PIC  X(0001).
000521     05  FILLER REDEFINES HMAINTF.
000522         10  HMAINTA PIC  X(0001).
000523     05  HMAINTI PIC  X(0001).
000524*    -------------------------------
000525     05  HRESMANL PIC S9(0004) COMP.
000526     05  HRESMANF PIC  X(0001).
000527     05  FILLER REDEFINES HRESMANF.
000528         10  HRESMANA PIC  X(0001).
000529     05  HRESMANI PIC  X(0001).
000530*    -------------------------------
000531     05  HMANAMTL PIC S9(0004) COMP.
000532     05  HMANAMTF PIC  X(0001).
000533     05  FILLER REDEFINES HMANAMTF.
000534         10  HMANAMTA PIC  X(0001).
000535     05  HMANAMTI PIC  S9(10)V99.
000536*    -------------------------------
000537     05  HITDCL PIC S9(0004) COMP.
000538     05  HITDCF PIC  X(0001).
000539     05  FILLER REDEFINES HITDCF.
000540         10  HITDCA PIC  X(0001).
000541     05  HITDCI PIC  X(0010).
000542*    -------------------------------
000543     05  HRESFUTL PIC S9(0004) COMP.
000544     05  HRESFUTF PIC  X(0001).
000545     05  FILLER REDEFINES HRESFUTF.
000546         10  HRESFUTA PIC  X(0001).
000547     05  HRESFUTI PIC  X(0001).
000548*    -------------------------------
000549     05  HFUTAMTL PIC S9(0004) COMP.
000550     05  HFUTAMTF PIC  X(0001).
000551     05  FILLER REDEFINES HFUTAMTF.
000552         10  HFUTAMTA PIC  X(0001).
000553     05  HFUTAMTI PIC  X(0012).
000554*    -------------------------------
000555     05  HITDNCL PIC S9(0004) COMP.
000556     05  HITDNCF PIC  X(0001).
000557     05  FILLER REDEFINES HITDNCF.
000558         10  HITDNCA PIC  X(0001).
000559     05  HITDNCI PIC  X(0010).
000560*    -------------------------------
000561     05  HRESIBNL PIC S9(0004) COMP.
000562     05  HRESIBNF PIC  X(0001).
000563     05  FILLER REDEFINES HRESIBNF.
000564         10  HRESIBNA PIC  X(0001).
000565     05  HRESIBNI PIC  X(0001).
000566*    -------------------------------
000567     05  HIBNAMTL PIC S9(0004) COMP.
000568     05  HIBNAMTF PIC  X(0001).
000569     05  FILLER REDEFINES HIBNAMTF.
000570         10  HIBNAMTA PIC  X(0001).
000571     05  HIBNAMTI PIC  X(0012).
000572*    -------------------------------
000573     05  HLSTCLOL PIC S9(0004) COMP.
000574     05  HLSTCLOF PIC  X(0001).
000575     05  FILLER REDEFINES HLSTCLOF.
000576         10  HLSTCLOA PIC  X(0001).
000577     05  HLSTCLOI PIC  X(0008).
000578*    -------------------------------
000579     05  HRESLFPL PIC S9(0004) COMP.
000580     05  HRESLFPF PIC  X(0001).
000581     05  FILLER REDEFINES HRESLFPF.
000582         10  HRESLFPA PIC  X(0001).
000583     05  HRESLFPI PIC  X(0001).
000584*    -------------------------------
000585     05  HPTCAMTL PIC S9(0004) COMP.
000586     05  HPTCAMTF PIC  X(0001).
000587     05  FILLER REDEFINES HPTCAMTF.
000588         10  HPTCAMTA PIC  X(0001).
000589     05  HPTCAMTI PIC  X(0012).
000590*    -------------------------------
000591     05  HLSTOPEL PIC S9(0004) COMP.
000592     05  HLSTOPEF PIC  X(0001).
000593     05  FILLER REDEFINES HLSTOPEF.
000594         10  HLSTOPEA PIC  X(0001).
000595     05  HLSTOPEI PIC  X(0008).
000596*    -------------------------------
000597     05  HRESAHPL PIC S9(0004) COMP.
000598     05  HRESAHPF PIC  X(0001).
000599     05  FILLER REDEFINES HRESAHPF.
000600         10  HRESAHPA PIC  X(0001).
000601     05  HRESAHPI PIC  X(0001).
000602*    -------------------------------
000603     05  HCDTAML PIC S9(0004) COMP.
000604     05  HCDTAMF PIC  X(0001).
000605     05  FILLER REDEFINES HCDTAMF.
000606         10  HCDTAMA PIC  X(0001).
000607     05  HCDTAMI PIC  X(0001).
000608*    -------------------------------
000609     05  HPCTCDTL PIC S9(0004) COMP.
000610     05  HPCTCDTF PIC  X(0001).
000611     05  FILLER REDEFINES HPCTCDTF.
000612         10  HPCTCDTA PIC  X(0001).
000613     05  HPCTCDTI PIC  S9(5)V99.
000614*    -------------------------------
000615     05  HEXPL PIC S9(0004) COMP.
000616     05  HEXPF PIC  X(0001).
000617     05  FILLER REDEFINES HEXPF.
000618         10  HEXPA PIC  X(0001).
000619     05  HEXPI PIC  X(0001).
000620*    -------------------------------
000621     05  HEXPAMTL PIC S9(0004) COMP.
000622     05  HEXPAMTF PIC  X(0001).
000623     05  FILLER REDEFINES HEXPAMTF.
000624         10  HEXPAMTA PIC  X(0001).
000625     05  HEXPAMTI PIC  S9(5)V99.
000626*    -------------------------------
000627     05  HOCDT01L PIC S9(0004) COMP.
000628     05  HOCDT01F PIC  X(0001).
000629     05  FILLER REDEFINES HOCDT01F.
000630         10  HOCDT01A PIC  X(0001).
000631     05  HOCDT01I PIC  X(0008).
000632*    -------------------------------
000633     05  HOC01L PIC S9(0004) COMP.
000634     05  HOC01F PIC  X(0001).
000635     05  FILLER REDEFINES HOC01F.
000636         10  HOC01A PIC  X(0001).
000637     05  HOC01I PIC  X(0001).
000638*    -------------------------------
000639     05  HOCAS01L PIC S9(0004) COMP.
000640     05  HOCAS01F PIC  X(0001).
000641     05  FILLER REDEFINES HOCAS01F.
000642         10  HOCAS01A PIC  X(0001).
000643     05  HOCAS01I PIC  X(0005).
000644*    -------------------------------
000645     05  HOCDT02L PIC S9(0004) COMP.
000646     05  HOCDT02F PIC  X(0001).
000647     05  FILLER REDEFINES HOCDT02F.
000648         10  HOCDT02A PIC  X(0001).
000649     05  HOCDT02I PIC  X(0008).
000650*    -------------------------------
000651     05  HOC02L PIC S9(0004) COMP.
000652     05  HOC02F PIC  X(0001).
000653     05  FILLER REDEFINES HOC02F.
000654         10  HOC02A PIC  X(0001).
000655     05  HOC02I PIC  X(0001).
000656*    -------------------------------
000657     05  HOCAS02L PIC S9(0004) COMP.
000658     05  HOCAS02F PIC  X(0001).
000659     05  FILLER REDEFINES HOCAS02F.
000660         10  HOCAS02A PIC  X(0001).
000661     05  HOCAS02I PIC  X(0005).
000662*    -------------------------------
000663     05  HOCDT03L PIC S9(0004) COMP.
000664     05  HOCDT03F PIC  X(0001).
000665     05  FILLER REDEFINES HOCDT03F.
000666         10  HOCDT03A PIC  X(0001).
000667     05  HOCDT03I PIC  X(0008).
000668*    -------------------------------
000669     05  HCO03L PIC S9(0004) COMP.
000670     05  HCO03F PIC  X(0001).
000671     05  FILLER REDEFINES HCO03F.
000672         10  HCO03A PIC  X(0001).
000673     05  HCO03I PIC  X(0001).
000674*    -------------------------------
000675     05  HCAS03L PIC S9(0004) COMP.
000676     05  HCAS03F PIC  X(0001).
000677     05  FILLER REDEFINES HCAS03F.
000678         10  HCAS03A PIC  X(0001).
000679     05  HCAS03I PIC  X(0005).
000680*    -------------------------------
000681     05  HOCDT04L PIC S9(0004) COMP.
000682     05  HOCDT04F PIC  X(0001).
000683     05  FILLER REDEFINES HOCDT04F.
000684         10  HOCDT04A PIC  X(0001).
000685     05  HOCDT04I PIC  X(0008).
000686*    -------------------------------
000687     05  HCO04L PIC S9(0004) COMP.
000688     05  HCO04F PIC  X(0001).
000689     05  FILLER REDEFINES HCO04F.
000690         10  HCO04A PIC  X(0001).
000691     05  HCO04I PIC  X(0001).
000692*    -------------------------------
000693     05  HCAS04L PIC S9(0004) COMP.
000694     05  HCAS04F PIC  X(0001).
000695     05  FILLER REDEFINES HCAS04F.
000696         10  HCAS04A PIC  X(0001).
000697     05  HCAS04I PIC  X(0005).
000698*    -------------------------------
000699     05  HCDT05L PIC S9(0004) COMP.
000700     05  HCDT05F PIC  X(0001).
000701     05  FILLER REDEFINES HCDT05F.
000702         10  HCDT05A PIC  X(0001).
000703     05  HCDT05I PIC  X(0008).
000704*    -------------------------------
000705     05  HCO05L PIC S9(0004) COMP.
000706     05  HCO05F PIC  X(0001).
000707     05  FILLER REDEFINES HCO05F.
000708         10  HCO05A PIC  X(0001).
000709     05  HCO05I PIC  X(0001).
000710*    -------------------------------
000711     05  HCAS05L PIC S9(0004) COMP.
000712     05  HCAS05F PIC  X(0001).
000713     05  FILLER REDEFINES HCAS05F.
000714         10  HCAS05A PIC  X(0001).
000715     05  HCAS05I PIC  X(0005).
000716*    -------------------------------
000717     05  HCDT06L PIC S9(0004) COMP.
000718     05  HCDT06F PIC  X(0001).
000719     05  FILLER REDEFINES HCDT06F.
000720         10  HCDT06A PIC  X(0001).
000721     05  HCDT06I PIC  X(0008).
000722*    -------------------------------
000723     05  HCO06L PIC S9(0004) COMP.
000724     05  HCO06F PIC  X(0001).
000725     05  FILLER REDEFINES HCO06F.
000726         10  HCO06A PIC  X(0001).
000727     05  HCO06I PIC  X(0001).
000728*    -------------------------------
000729     05  HCAS06L PIC S9(0004) COMP.
000730     05  HCAS06F PIC  X(0001).
000731     05  FILLER REDEFINES HCAS06F.
000732         10  HCAS06A PIC  X(0001).
000733     05  HCAS06I PIC  X(0005).
000734*    -------------------------------
000735     05  HCDT07L PIC S9(0004) COMP.
000736     05  HCDT07F PIC  X(0001).
000737     05  FILLER REDEFINES HCDT07F.
000738         10  HCDT07A PIC  X(0001).
000739     05  HCDT07I PIC  X(0008).
000740*    -------------------------------
000741     05  HCO07L PIC S9(0004) COMP.
000742     05  HCO07F PIC  X(0001).
000743     05  FILLER REDEFINES HCO07F.
000744         10  HCO07A PIC  X(0001).
000745     05  HCO07I PIC  X(0001).
000746*    -------------------------------
000747     05  HCAS07L PIC S9(0004) COMP.
000748     05  HCAS07F PIC  X(0001).
000749     05  FILLER REDEFINES HCAS07F.
000750         10  HCAS07A PIC  X(0001).
000751     05  HCAS07I PIC  X(0005).
000752*    -------------------------------
000753     05  HCDT08L PIC S9(0004) COMP.
000754     05  HCDT08F PIC  X(0001).
000755     05  FILLER REDEFINES HCDT08F.
000756         10  HCDT08A PIC  X(0001).
000757     05  HCDT08I PIC  X(0008).
000758*    -------------------------------
000759     05  HCO08L PIC S9(0004) COMP.
000760     05  HCO08F PIC  X(0001).
000761     05  FILLER REDEFINES HCO08F.
000762         10  HCO08A PIC  X(0001).
000763     05  HCO08I PIC  X(0001).
000764*    -------------------------------
000765     05  HCAS08L PIC S9(0004) COMP.
000766     05  HCAS08F PIC  X(0001).
000767     05  FILLER REDEFINES HCAS08F.
000768         10  HCAS08A PIC  X(0001).
000769     05  HCAS08I PIC  X(0005).
000770*    -------------------------------
000771     05  HCDT09L PIC S9(0004) COMP.
000772     05  HCDT09F PIC  X(0001).
000773     05  FILLER REDEFINES HCDT09F.
000774         10  HCDT09A PIC  X(0001).
000775     05  HCDT09I PIC  X(0008).
000776*    -------------------------------
000777     05  HCO09L PIC S9(0004) COMP.
000778     05  HCO09F PIC  X(0001).
000779     05  FILLER REDEFINES HCO09F.
000780         10  HCO09A PIC  X(0001).
000781     05  HCO09I PIC  X(0001).
000782*    -------------------------------
000783     05  HCAS09L PIC S9(0004) COMP.
000784     05  HCAS09F PIC  X(0001).
000785     05  FILLER REDEFINES HCAS09F.
000786         10  HCAS09A PIC  X(0001).
000787     05  HCAS09I PIC  X(0005).
000788*    -------------------------------
000789     05  HCDT10L PIC S9(0004) COMP.
000790     05  HCDT10F PIC  X(0001).
000791     05  FILLER REDEFINES HCDT10F.
000792         10  HCDT10A PIC  X(0001).
000793     05  HCDT10I PIC  X(0008).
000794*    -------------------------------
000795     05  HCO10L PIC S9(0004) COMP.
000796     05  HCO10F PIC  X(0001).
000797     05  FILLER REDEFINES HCO10F.
000798         10  HCO10A PIC  X(0001).
000799     05  HCO10I PIC  X(0001).
000800*    -------------------------------
000801     05  HCAS10L PIC S9(0004) COMP.
000802     05  HCAS10F PIC  X(0001).
000803     05  FILLER REDEFINES HCAS10F.
000804         10  HCAS10A PIC  X(0001).
000805     05  HCAS10I PIC  X(0005).
000806*    -------------------------------
000807     05  HEMSG1L PIC S9(0004) COMP.
000808     05  HEMSG1F PIC  X(0001).
000809     05  FILLER REDEFINES HEMSG1F.
000810         10  HEMSG1A PIC  X(0001).
000811     05  HEMSG1I PIC  X(0079).
000812*    -------------------------------
000813     05  HPFKL PIC S9(0004) COMP.
000814     05  HPFKF PIC  X(0001).
000815     05  FILLER REDEFINES HPFKF.
000816         10  HPFKA PIC  X(0001).
000817     05  HPFKI PIC  9(2).
000818 01  EL142HO REDEFINES EL142BI.
000819     05  FILLER            PIC  X(0012).
000820*    -------------------------------
000821     05  FILLER            PIC  X(0003).
000822     05  HDATEO PIC  X(0008).
000823*    -------------------------------
000824     05  FILLER            PIC  X(0003).
000825     05  HTIMEO PIC  99.99.
000826*    -------------------------------
000827     05  FILLER            PIC  X(0003).
000828     05  HTYPEO PIC  X(0010).
000829*    -------------------------------
000830     05  FILLER            PIC  X(0003).
000831     05  HRECDTEO PIC  X(0008).
000832*    -------------------------------
000833     05  FILLER            PIC  X(0003).
000834     05  HBYO PIC  X(0004).
000835*    -------------------------------
000836     05  FILLER            PIC  X(0003).
000837     05  HTLRTYPO PIC  X(0001).
000838*    -------------------------------
000839     05  FILLER            PIC  X(0003).
000840     05  HSEQO PIC  9999.
000841*    -------------------------------
000842     05  FILLER            PIC  X(0003).
000843     05  HMANTBYO PIC  X(0004).
000844*    -------------------------------
000845     05  FILLER            PIC  X(0003).
000846     05  HMANTONO PIC  X(0008).
000847*    -------------------------------
000848     05  FILLER            PIC  X(0003).
000849     05  HMANTATO PIC  99.99.
000850*    -------------------------------
000851     05  FILLER            PIC  X(0003).
000852     05  HMAINTO PIC  X(0001).
000853*    -------------------------------
000854     05  FILLER            PIC  X(0003).
000855     05  HRESMANO PIC  X(0001).
000856*    -------------------------------
000857     05  FILLER            PIC  X(0003).
000858     05  HMANAMTO PIC  ZZZZ,ZZ9.99-.
000859*    -------------------------------
000860     05  FILLER            PIC  X(0003).
000861     05  HITDCO PIC  ZZ,ZZ9.99-.
000862*    -------------------------------
000863     05  FILLER            PIC  X(0003).
000864     05  HRESFUTO PIC  X(0001).
000865*    -------------------------------
000866     05  FILLER            PIC  X(0003).
000867     05  HFUTAMTO PIC  ZZZZ,ZZ9.99-.
000868*    -------------------------------
000869     05  FILLER            PIC  X(0003).
000870     05  HITDNCO PIC  ZZ,ZZ9.99-.
000871*    -------------------------------
000872     05  FILLER            PIC  X(0003).
000873     05  HRESIBNO PIC  X(0001).
000874*    -------------------------------
000875     05  FILLER            PIC  X(0003).
000876     05  HIBNAMTO PIC  ZZZZ,ZZ9.99-.
000877*    -------------------------------
000878     05  FILLER            PIC  X(0003).
000879     05  HLSTCLOO PIC  X(0008).
000880*    -------------------------------
000881     05  FILLER            PIC  X(0003).
000882     05  HRESLFPO PIC  X(0001).
000883*    -------------------------------
000884     05  FILLER            PIC  X(0003).
000885     05  HPTCAMTO PIC  ZZZZ,ZZ9.99-.
000886*    -------------------------------
000887     05  FILLER            PIC  X(0003).
000888     05  HLSTOPEO PIC  X(0008).
000889*    -------------------------------
000890     05  FILLER            PIC  X(0003).
000891     05  HRESAHPO PIC  X(0001).
000892*    -------------------------------
000893     05  FILLER            PIC  X(0003).
000894     05  HCDTAMO PIC  X(0001).
000895*    -------------------------------
000896     05  FILLER            PIC  X(0003).
000897     05  HPCTCDTO PIC  ZZ9.99-.
000898*    -------------------------------
000899     05  FILLER            PIC  X(0003).
000900     05  HEXPO PIC  X(0001).
000901*    -------------------------------
000902     05  FILLER            PIC  X(0003).
000903     05  HEXPAMTO PIC  ZZZ.99-.
000904*    -------------------------------
000905     05  FILLER            PIC  X(0003).
000906     05  HOCDT01O PIC  X(0008).
000907*    -------------------------------
000908     05  FILLER            PIC  X(0003).
000909     05  HOC01O PIC  X(0001).
000910*    -------------------------------
000911     05  FILLER            PIC  X(0003).
000912     05  HOCAS01O PIC  X(0005).
000913*    -------------------------------
000914     05  FILLER            PIC  X(0003).
000915     05  HOCDT02O PIC  X(0008).
000916*    -------------------------------
000917     05  FILLER            PIC  X(0003).
000918     05  HOC02O PIC  X(0001).
000919*    -------------------------------
000920     05  FILLER            PIC  X(0003).
000921     05  HOCAS02O PIC  X(0005).
000922*    -------------------------------
000923     05  FILLER            PIC  X(0003).
000924     05  HOCDT03O PIC  X(0008).
000925*    -------------------------------
000926     05  FILLER            PIC  X(0003).
000927     05  HCO03O PIC  X(0001).
000928*    -------------------------------
000929     05  FILLER            PIC  X(0003).
000930     05  HCAS03O PIC  X(0005).
000931*    -------------------------------
000932     05  FILLER            PIC  X(0003).
000933     05  HOCDT04O PIC  X(0008).
000934*    -------------------------------
000935     05  FILLER            PIC  X(0003).
000936     05  HCO04O PIC  X(0001).
000937*    -------------------------------
000938     05  FILLER            PIC  X(0003).
000939     05  HCAS04O PIC  X(0005).
000940*    -------------------------------
000941     05  FILLER            PIC  X(0003).
000942     05  HCDT05O PIC  X(0008).
000943*    -------------------------------
000944     05  FILLER            PIC  X(0003).
000945     05  HCO05O PIC  X(0001).
000946*    -------------------------------
000947     05  FILLER            PIC  X(0003).
000948     05  HCAS05O PIC  X(0005).
000949*    -------------------------------
000950     05  FILLER            PIC  X(0003).
000951     05  HCDT06O PIC  X(0008).
000952*    -------------------------------
000953     05  FILLER            PIC  X(0003).
000954     05  HCO06O PIC  X(0001).
000955*    -------------------------------
000956     05  FILLER            PIC  X(0003).
000957     05  HCAS06O PIC  X(0005).
000958*    -------------------------------
000959     05  FILLER            PIC  X(0003).
000960     05  HCDT07O PIC  X(0008).
000961*    -------------------------------
000962     05  FILLER            PIC  X(0003).
000963     05  HCO07O PIC  X(0001).
000964*    -------------------------------
000965     05  FILLER            PIC  X(0003).
000966     05  HCAS07O PIC  X(0005).
000967*    -------------------------------
000968     05  FILLER            PIC  X(0003).
000969     05  HCDT08O PIC  X(0008).
000970*    -------------------------------
000971     05  FILLER            PIC  X(0003).
000972     05  HCO08O PIC  X(0001).
000973*    -------------------------------
000974     05  FILLER            PIC  X(0003).
000975     05  HCAS08O PIC  X(0005).
000976*    -------------------------------
000977     05  FILLER            PIC  X(0003).
000978     05  HCDT09O PIC  X(0008).
000979*    -------------------------------
000980     05  FILLER            PIC  X(0003).
000981     05  HCO09O PIC  X(0001).
000982*    -------------------------------
000983     05  FILLER            PIC  X(0003).
000984     05  HCAS09O PIC  X(0005).
000985*    -------------------------------
000986     05  FILLER            PIC  X(0003).
000987     05  HCDT10O PIC  X(0008).
000988*    -------------------------------
000989     05  FILLER            PIC  X(0003).
000990     05  HCO10O PIC  X(0001).
000991*    -------------------------------
000992     05  FILLER            PIC  X(0003).
000993     05  HCAS10O PIC  X(0005).
000994*    -------------------------------
000995     05  FILLER            PIC  X(0003).
000996     05  HEMSG1O PIC  X(0079).
000997*    -------------------------------
000998     05  FILLER            PIC  X(0003).
000999     05  HPFKO PIC  99.
001000*    -------------------------------
001001 01  EL142DI REDEFINES EL142BI.
001002     05  FILLER            PIC  X(0012).
001003*    -------------------------------
001004     05  DDATEL PIC S9(0004) COMP.
001005     05  DDATEF PIC  X(0001).
001006     05  FILLER REDEFINES DDATEF.
001007         10  DDATEA PIC  X(0001).
001008     05  DDATEI PIC  X(0008).
001009*    -------------------------------
001010     05  DTIMEL PIC S9(0004) COMP.
001011     05  DTIMEF PIC  X(0001).
001012     05  FILLER REDEFINES DTIMEF.
001013         10  DTIMEA PIC  X(0001).
001014     05  DTIMEI PIC  X(0005).
001015*    -------------------------------
001016     05  DTYPEL PIC S9(0004) COMP.
001017     05  DTYPEF PIC  X(0001).
001018     05  FILLER REDEFINES DTYPEF.
001019         10  DTYPEA PIC  X(0001).
001020     05  DTYPEI PIC  X(0010).
001021*    -------------------------------
001022     05  DRECDTEL PIC S9(0004) COMP.
001023     05  DRECDTEF PIC  X(0001).
001024     05  FILLER REDEFINES DRECDTEF.
001025         10  DRECDTEA PIC  X(0001).
001026     05  DRECDTEI PIC  X(0008).
001027*    -------------------------------
001028     05  DBYL PIC S9(0004) COMP.
001029     05  DBYF PIC  X(0001).
001030     05  FILLER REDEFINES DBYF.
001031         10  DBYA PIC  X(0001).
001032     05  DBYI PIC  X(0004).
001033*    -------------------------------
001034     05  DTLRTYPL PIC S9(0004) COMP.
001035     05  DTLRTYPF PIC  X(0001).
001036     05  FILLER REDEFINES DTLRTYPF.
001037         10  DTLRTYPA PIC  X(0001).
001038     05  DTLRTYPI PIC  X(0001).
001039*    -------------------------------
001040     05  DSEQL PIC S9(0004) COMP.
001041     05  DSEQF PIC  X(0001).
001042     05  FILLER REDEFINES DSEQF.
001043         10  DSEQA PIC  X(0001).
001044     05  DSEQI PIC  X(0004).
001045*    -------------------------------
001046     05  DMANTBYL PIC S9(0004) COMP.
001047     05  DMANTBYF PIC  X(0001).
001048     05  FILLER REDEFINES DMANTBYF.
001049         10  DMANTBYA PIC  X(0001).
001050     05  DMANTBYI PIC  X(0004).
001051*    -------------------------------
001052     05  DMANTONL PIC S9(0004) COMP.
001053     05  DMANTONF PIC  X(0001).
001054     05  FILLER REDEFINES DMANTONF.
001055         10  DMANTONA PIC  X(0001).
001056     05  DMANTONI PIC  X(0008).
001057*    -------------------------------
001058     05  DMANTATL PIC S9(0004) COMP.
001059     05  DMANTATF PIC  X(0001).
001060     05  FILLER REDEFINES DMANTATF.
001061         10  DMANTATA PIC  X(0001).
001062     05  DMANTATI PIC  X(0005).
001063*    -------------------------------
001064     05  DMAINTL PIC S9(0004) COMP.
001065     05  DMAINTF PIC  X(0001).
001066     05  FILLER REDEFINES DMAINTF.
001067         10  DMAINTA PIC  X(0001).
001068     05  DMAINTI PIC  X(0001).
001069*    -------------------------------
001070     05  DENCCODL PIC S9(0004) COMP.
001071     05  DENCCODF PIC  X(0001).
001072     05  FILLER REDEFINES DENCCODF.
001073         10  DENCCODA PIC  X(0001).
001074     05  DENCCODI PIC  X(0003).
001075*    -------------------------------
001076     05  DFORMNOL PIC S9(0004) COMP.
001077     05  DFORMNOF PIC  X(0001).
001078     05  FILLER REDEFINES DFORMNOF.
001079         10  DFORMNOA PIC  X(0001).
001080     05  DFORMNOI PIC  X(0004).
001081*    -------------------------------
001082     05  DPURGHDL PIC S9(0004) COMP.
001083     05  DPURGHDF PIC  X(0001).
001084     05  FILLER REDEFINES DPURGHDF.
001085         10  DPURGHDA PIC  X(0001).
001086     05  DPURGHDI PIC  X(0017).
001087*    -------------------------------
001088     05  DPURGDTL PIC S9(0004) COMP.
001089     05  DPURGDTF PIC  X(0001).
001090     05  FILLER REDEFINES DPURGDTF.
001091         10  DPURGDTA PIC  X(0001).
001092     05  DPURGDTI PIC  X(0008).
001093*    -------------------------------
001094     05  DARCHNOL PIC S9(0004) COMP.
001095     05  DARCHNOF PIC  X(0001).
001096     05  FILLER REDEFINES DARCHNOF.
001097         10  DARCHNOA PIC  X(0001).
001098     05  DARCHNOI PIC  X(0008).
001099*    -------------------------------
001100     05  DBENLETL PIC S9(0004) COMP.
001101     05  DBENLETF PIC  X(0001).
001102     05  FILLER REDEFINES DBENLETF.
001103         10  DBENLETA PIC  X(0001).
001104     05  DBENLETI PIC  X(0040).
001105*    -------------------------------
001106     05  DDTSENTL PIC S9(0004) COMP.
001107     05  DDTSENTF PIC  X(0001).
001108     05  FILLER REDEFINES DDTSENTF.
001109         10  DDTSENTA PIC  X(0001).
001110     05  DDTSENTI PIC  X(0008).
001111*    -------------------------------
001112     05  DINPRNTL PIC S9(0004) COMP.
001113     05  DINPRNTF PIC  X(0001).
001114     05  FILLER REDEFINES DINPRNTF.
001115         10  DINPRNTA PIC  X(0001).
001116     05  DINPRNTI PIC  X(0008).
001117*    -------------------------------
001118     05  DRESENDL PIC S9(0004) COMP.
001119     05  DRESENDF PIC  X(0001).
001120     05  FILLER REDEFINES DRESENDF.
001121         10  DRESENDA PIC  X(0001).
001122     05  DRESENDI PIC  X(0008).
001123*    -------------------------------
001124     05  DREPRNTL PIC S9(0004) COMP.
001125     05  DREPRNTF PIC  X(0001).
001126     05  FILLER REDEFINES DREPRNTF.
001127         10  DREPRNTA PIC  X(0001).
001128     05  DREPRNTI PIC  X(0008).
001129*    -------------------------------
001130     05  DREPLYL PIC S9(0004) COMP.
001131     05  DREPLYF PIC  X(0001).
001132     05  FILLER REDEFINES DREPLYF.
001133         10  DREPLYA PIC  X(0001).
001134     05  DREPLYI PIC  X(0008).
001135*    -------------------------------
001136     05  DRESFRML PIC S9(0004) COMP.
001137     05  DRESFRMF PIC  X(0001).
001138     05  FILLER REDEFINES DRESFRMF.
001139         10  DRESFRMA PIC  X(0001).
001140     05  DRESFRMI PIC  X(0004).
001141*    -------------------------------
001142     05  DRECEVEL PIC S9(0004) COMP.
001143     05  DRECEVEF PIC  X(0001).
001144     05  FILLER REDEFINES DRECEVEF.
001145         10  DRECEVEA PIC  X(0001).
001146     05  DRECEVEI PIC  X(0008).
001147*    -------------------------------
001148     05  DAUTOCLL PIC S9(0004) COMP.
001149     05  DAUTOCLF PIC  X(0001).
001150     05  FILLER REDEFINES DAUTOCLF.
001151         10  DAUTOCLA PIC  X(0001).
001152     05  DAUTOCLI PIC  X(0001).
001153*    -------------------------------
001154     05  DWRITENL PIC S9(0004) COMP.
001155     05  DWRITENF PIC  X(0001).
001156     05  FILLER REDEFINES DWRITENF.
001157         10  DWRITENA PIC  X(0001).
001158     05  DWRITENI PIC  X(0001).
001159*    -------------------------------
001160     05  DSTOPLTL PIC S9(0004) COMP.
001161     05  DSTOPLTF PIC  X(0001).
001162     05  FILLER REDEFINES DSTOPLTF.
001163         10  DSTOPLTA PIC  X(0001).
001164     05  DSTOPLTI PIC  X(0008).
001165*    -------------------------------
001166     05  DREASONL PIC S9(0004) COMP.
001167     05  DREASONF PIC  X(0001).
001168     05  FILLER REDEFINES DREASONF.
001169         10  DREASONA PIC  X(0001).
001170     05  DREASONI PIC  X(0070).
001171*    -------------------------------
001172     05  DMAILTOL PIC S9(0004) COMP.
001173     05  DMAILTOF PIC  X(0001).
001174     05  FILLER REDEFINES DMAILTOF.
001175         10  DMAILTOA PIC  X(0001).
001176     05  DMAILTOI PIC  X(0030).
001177*    -------------------------------
001178     05  DADDR1L PIC S9(0004) COMP.
001179     05  DADDR1F PIC  X(0001).
001180     05  FILLER REDEFINES DADDR1F.
001181         10  DADDR1A PIC  X(0001).
001182     05  DADDR1I PIC  X(0030).
001183*    -------------------------------
001184     05  DADDR2L PIC S9(0004) COMP.
001185     05  DADDR2F PIC  X(0001).
001186     05  FILLER REDEFINES DADDR2F.
001187         10  DADDR2A PIC  X(0001).
001188     05  DADDR2I PIC  X(0030).
001189*    -------------------------------
001190     05  DCITYSTL PIC S9(0004) COMP.
001191     05  DCITYSTF PIC  X(0001).
001192     05  FILLER REDEFINES DCITYSTF.
001193         10  DCITYSTA PIC  X(0001).
001194     05  DCITYSTI PIC  X(0030).
001195*    -------------------------------
001196     05  DZIPL PIC S9(0004) COMP.
001197     05  DZIPF PIC  X(0001).
001198     05  FILLER REDEFINES DZIPF.
001199         10  DZIPA PIC  X(0001).
001200     05  DZIPI PIC  X(0010).
001201*    -------------------------------
001202     05  DPHONEL PIC S9(0004) COMP.
001203     05  DPHONEF PIC  X(0001).
001204     05  FILLER REDEFINES DPHONEF.
001205         10  DPHONEA PIC  X(0001).
001206     05  DPHONEI PIC  X(0012).
001207*    -------------------------------
001208     05  DEMSG1L PIC S9(0004) COMP.
001209     05  DEMSG1F PIC  X(0001).
001210     05  FILLER REDEFINES DEMSG1F.
001211         10  DEMSG1A PIC  X(0001).
001212     05  DEMSG1I PIC  X(0079).
001213*    -------------------------------
001214     05  DPFKL PIC S9(0004) COMP.
001215     05  DPFKF PIC  X(0001).
001216     05  FILLER REDEFINES DPFKF.
001217         10  DPFKA PIC  X(0001).
001218     05  DPFKI PIC  9(2).
001219 01  EL142DO REDEFINES EL142BI.
001220     05  FILLER            PIC  X(0012).
001221*    -------------------------------
001222     05  FILLER            PIC  X(0003).
001223     05  DDATEO PIC  X(0008).
001224*    -------------------------------
001225     05  FILLER            PIC  X(0003).
001226     05  DTIMEO PIC  99.99.
001227*    -------------------------------
001228     05  FILLER            PIC  X(0003).
001229     05  DTYPEO PIC  X(0010).
001230*    -------------------------------
001231     05  FILLER            PIC  X(0003).
001232     05  DRECDTEO PIC  X(0008).
001233*    -------------------------------
001234     05  FILLER            PIC  X(0003).
001235     05  DBYO PIC  X(0004).
001236*    -------------------------------
001237     05  FILLER            PIC  X(0003).
001238     05  DTLRTYPO PIC  X(0001).
001239*    -------------------------------
001240     05  FILLER            PIC  X(0003).
001241     05  DSEQO PIC  9999.
001242*    -------------------------------
001243     05  FILLER            PIC  X(0003).
001244     05  DMANTBYO PIC  X(0004).
001245*    -------------------------------
001246     05  FILLER            PIC  X(0003).
001247     05  DMANTONO PIC  X(0008).
001248*    -------------------------------
001249     05  FILLER            PIC  X(0003).
001250     05  DMANTATO PIC  99.99.
001251*    -------------------------------
001252     05  FILLER            PIC  X(0003).
001253     05  DMAINTO PIC  X(0001).
001254*    -------------------------------
001255     05  FILLER            PIC  X(0003).
001256     05  DENCCODO PIC  X(0003).
001257*    -------------------------------
001258     05  FILLER            PIC  X(0003).
001259     05  DFORMNOO PIC  X(0004).
001260*    -------------------------------
001261     05  FILLER            PIC  X(0003).
001262     05  DPURGHDO PIC  X(0017).
001263*    -------------------------------
001264     05  FILLER            PIC  X(0003).
001265     05  DPURGDTO PIC  X(0008).
001266*    -------------------------------
001267     05  FILLER            PIC  X(0003).
001268     05  DARCHNOO PIC  Z(7)9.
001269*    -------------------------------
001270     05  FILLER            PIC  X(0003).
001271     05  DBENLETO PIC  X(0040).
001272*    -------------------------------
001273     05  FILLER            PIC  X(0003).
001274     05  DDTSENTO PIC  99B99B99.
001275*    -------------------------------
001276     05  FILLER            PIC  X(0003).
001277     05  DINPRNTO PIC  99B99B99.
001278*    -------------------------------
001279     05  FILLER            PIC  X(0003).
001280     05  DRESENDO PIC  99B99B99.
001281*    -------------------------------
001282     05  FILLER            PIC  X(0003).
001283     05  DREPRNTO PIC  99B99B99.
001284*    -------------------------------
001285     05  FILLER            PIC  X(0003).
001286     05  DREPLYO PIC  99B99B99.
001287*    -------------------------------
001288     05  FILLER            PIC  X(0003).
001289     05  DRESFRMO PIC  X(0004).
001290*    -------------------------------
001291     05  FILLER            PIC  X(0003).
001292     05  DRECEVEO PIC  99B99B99.
001293*    -------------------------------
001294     05  FILLER            PIC  X(0003).
001295     05  DAUTOCLO PIC  X(0001).
001296*    -------------------------------
001297     05  FILLER            PIC  X(0003).
001298     05  DWRITENO PIC  X(0001).
001299*    -------------------------------
001300     05  FILLER            PIC  X(0003).
001301     05  DSTOPLTO PIC  99B99B99.
001302*    -------------------------------
001303     05  FILLER            PIC  X(0003).
001304     05  DREASONO PIC  X(0070).
001305*    -------------------------------
001306     05  FILLER            PIC  X(0003).
001307     05  DMAILTOO PIC  X(0030).
001308*    -------------------------------
001309     05  FILLER            PIC  X(0003).
001310     05  DADDR1O PIC  X(0030).
001311*    -------------------------------
001312     05  FILLER            PIC  X(0003).
001313     05  DADDR2O PIC  X(0030).
001314*    -------------------------------
001315     05  FILLER            PIC  X(0003).
001316     05  DCITYSTO PIC  X(0030).
001317*    -------------------------------
001318     05  FILLER            PIC  X(0003).
001319     05  DZIPO PIC  X(0010).
001320*    -------------------------------
001321     05  FILLER            PIC  X(0003).
001322     05  DPHONEO PIC  999B999B9999.
001323*    -------------------------------
001324     05  FILLER            PIC  X(0003).
001325     05  DEMSG1O PIC  X(0079).
001326*    -------------------------------
001327     05  FILLER            PIC  X(0003).
001328     05  DPFKO PIC  99.
001329*    -------------------------------
001330 01  EL142JI REDEFINES EL142BI.
001331     05  FILLER            PIC  X(0012).
001332*    -------------------------------
001333     05  JDATEL PIC S9(0004) COMP.
001334     05  JDATEF PIC  X(0001).
001335     05  FILLER REDEFINES JDATEF.
001336         10  JDATEA PIC  X(0001).
001337     05  JDATEI PIC  X(0008).
001338*    -------------------------------
001339     05  JTIMEL PIC S9(0004) COMP.
001340     05  JTIMEF PIC  X(0001).
001341     05  FILLER REDEFINES JTIMEF.
001342         10  JTIMEA PIC  X(0001).
001343     05  JTIMEI PIC  X(0005).
001344*    -------------------------------
001345     05  JTYPEL PIC S9(0004) COMP.
001346     05  JTYPEF PIC  X(0001).
001347     05  FILLER REDEFINES JTYPEF.
001348         10  JTYPEA PIC  X(0001).
001349     05  JTYPEI PIC  X(0010).
001350*    -------------------------------
001351     05  JRECDTEL PIC S9(0004) COMP.
001352     05  JRECDTEF PIC  X(0001).
001353     05  FILLER REDEFINES JRECDTEF.
001354         10  JRECDTEA PIC  X(0001).
001355     05  JRECDTEI PIC  X(0008).
001356*    -------------------------------
001357     05  JBYL PIC S9(0004) COMP.
001358     05  JBYF PIC  X(0001).
001359     05  FILLER REDEFINES JBYF.
001360         10  JBYA PIC  X(0001).
001361     05  JBYI PIC  X(0004).
001362*    -------------------------------
001363     05  JTLRTYPL PIC S9(0004) COMP.
001364     05  JTLRTYPF PIC  X(0001).
001365     05  FILLER REDEFINES JTLRTYPF.
001366         10  JTLRTYPA PIC  X(0001).
001367     05  JTLRTYPI PIC  X(0001).
001368*    -------------------------------
001369     05  JSEQL PIC S9(0004) COMP.
001370     05  JSEQF PIC  X(0001).
001371     05  FILLER REDEFINES JSEQF.
001372         10  JSEQA PIC  X(0001).
001373     05  JSEQI PIC  X(0004).
001374*    -------------------------------
001375     05  JMANTBYL PIC S9(0004) COMP.
001376     05  JMANTBYF PIC  X(0001).
001377     05  FILLER REDEFINES JMANTBYF.
001378         10  JMANTBYA PIC  X(0001).
001379     05  JMANTBYI PIC  X(0004).
001380*    -------------------------------
001381     05  JMANTONL PIC S9(0004) COMP.
001382     05  JMANTONF PIC  X(0001).
001383     05  FILLER REDEFINES JMANTONF.
001384         10  JMANTONA PIC  X(0001).
001385     05  JMANTONI PIC  X(0008).
001386*    -------------------------------
001387     05  JMANTATL PIC S9(0004) COMP.
001388     05  JMANTATF PIC  X(0001).
001389     05  FILLER REDEFINES JMANTATF.
001390         10  JMANTATA PIC  X(0001).
001391     05  JMANTATI PIC  X(0005).
001392*    -------------------------------
001393     05  JMAINTL PIC S9(0004) COMP.
001394     05  JMAINTF PIC  X(0001).
001395     05  FILLER REDEFINES JMAINTF.
001396         10  JMAINTA PIC  X(0001).
001397     05  JMAINTI PIC  X(0001).
001398*    -------------------------------
001399     05  JDTSENTL PIC S9(0004) COMP.
001400     05  JDTSENTF PIC  X(0001).
001401     05  FILLER REDEFINES JDTSENTF.
001402         10  JDTSENTA PIC  X(0001).
001403     05  JDTSENTI PIC  X(0008).
001404*    -------------------------------
001405     05  JRESENDL PIC S9(0004) COMP.
001406     05  JRESENDF PIC  X(0001).
001407     05  FILLER REDEFINES JRESENDF.
001408         10  JRESENDA PIC  X(0001).
001409     05  JRESENDI PIC  X(0008).
001410*    -------------------------------
001411     05  JSI1L PIC S9(0004) COMP.
001412     05  JSI1F PIC  X(0001).
001413     05  FILLER REDEFINES JSI1F.
001414         10  JSI1A PIC  X(0001).
001415     05  JSI1I PIC  X(0028).
001416*    -------------------------------
001417     05  JREPLYL PIC S9(0004) COMP.
001418     05  JREPLYF PIC  X(0001).
001419     05  FILLER REDEFINES JREPLYF.
001420         10  JREPLYA PIC  X(0001).
001421     05  JREPLYI PIC  X(0008).
001422*    -------------------------------
001423     05  JSI2L PIC S9(0004) COMP.
001424     05  JSI2F PIC  X(0001).
001425     05  FILLER REDEFINES JSI2F.
001426         10  JSI2A PIC  X(0001).
001427     05  JSI2I PIC  X(0028).
001428*    -------------------------------
001429     05  JRECEVEL PIC S9(0004) COMP.
001430     05  JRECEVEF PIC  X(0001).
001431     05  FILLER REDEFINES JRECEVEF.
001432         10  JRECEVEA PIC  X(0001).
001433     05  JRECEVEI PIC  X(0008).
001434*    -------------------------------
001435     05  JSI3L PIC S9(0004) COMP.
001436     05  JSI3F PIC  X(0001).
001437     05  FILLER REDEFINES JSI3F.
001438         10  JSI3A PIC  X(0001).
001439     05  JSI3I PIC  X(0028).
001440*    -------------------------------
001441     05  JPHYRECL PIC S9(0004) COMP.
001442     05  JPHYRECF PIC  X(0001).
001443     05  FILLER REDEFINES JPHYRECF.
001444         10  JPHYRECA PIC  X(0001).
001445     05  JPHYRECI PIC  X(0008).
001446*    -------------------------------
001447     05  JEMPRECL PIC S9(0004) COMP.
001448     05  JEMPRECF PIC  X(0001).
001449     05  FILLER REDEFINES JEMPRECF.
001450         10  JEMPRECA PIC  X(0001).
001451     05  JEMPRECI PIC  X(0008).
001452*    -------------------------------
001453     05  JREMDTL PIC S9(0004) COMP.
001454     05  JREMDTF PIC  X(0001).
001455     05  FILLER REDEFINES JREMDTF.
001456         10  JREMDTA PIC  X(0001).
001457     05  JREMDTI PIC  X(0008).
001458*    -------------------------------
001459     05  JFORML PIC S9(0004) COMP.
001460     05  JFORMF PIC  X(0001).
001461     05  FILLER REDEFINES JFORMF.
001462         10  JFORMA PIC  X(0001).
001463     05  JFORMI PIC  X(0008).
001464*    -------------------------------
001465     05  JCARR1L PIC S9(0004) COMP.
001466     05  JCARR1F PIC  X(0001).
001467     05  FILLER REDEFINES JCARR1F.
001468         10  JCARR1A PIC  X(0001).
001469     05  JCARR1I PIC  X(0001).
001470*    -------------------------------
001471     05  JCLAIM1L PIC S9(0004) COMP.
001472     05  JCLAIM1F PIC  X(0001).
001473     05  FILLER REDEFINES JCLAIM1F.
001474         10  JCLAIM1A PIC  X(0001).
001475     05  JCLAIM1I PIC  X(0007).
001476*    -------------------------------
001477     05  JCERT1L PIC S9(0004) COMP.
001478     05  JCERT1F PIC  X(0001).
001479     05  FILLER REDEFINES JCERT1F.
001480         10  JCERT1A PIC  X(0001).
001481     05  JCERT1I PIC  X(0011).
001482*    -------------------------------
001483     05  JCARR2L PIC S9(0004) COMP.
001484     05  JCARR2F PIC  X(0001).
001485     05  FILLER REDEFINES JCARR2F.
001486         10  JCARR2A PIC  X(0001).
001487     05  JCARR2I PIC  X(0001).
001488*    -------------------------------
001489     05  JCLAIM2L PIC S9(0004) COMP.
001490     05  JCLAIM2F PIC  X(0001).
001491     05  FILLER REDEFINES JCLAIM2F.
001492         10  JCLAIM2A PIC  X(0001).
001493     05  JCLAIM2I PIC  X(0007).
001494*    -------------------------------
001495     05  JCERT2L PIC S9(0004) COMP.
001496     05  JCERT2F PIC  X(0001).
001497     05  FILLER REDEFINES JCERT2F.
001498         10  JCERT2A PIC  X(0001).
001499     05  JCERT2I PIC  X(0011).
001500*    -------------------------------
001501     05  JMAILTOL PIC S9(0004) COMP.
001502     05  JMAILTOF PIC  X(0001).
001503     05  FILLER REDEFINES JMAILTOF.
001504         10  JMAILTOA PIC  X(0001).
001505     05  JMAILTOI PIC  X(0030).
001506*    -------------------------------
001507     05  JADDR1L PIC S9(0004) COMP.
001508     05  JADDR1F PIC  X(0001).
001509     05  FILLER REDEFINES JADDR1F.
001510         10  JADDR1A PIC  X(0001).
001511     05  JADDR1I PIC  X(0030).
001512*    -------------------------------
001513     05  JADDR2L PIC S9(0004) COMP.
001514     05  JADDR2F PIC  X(0001).
001515     05  FILLER REDEFINES JADDR2F.
001516         10  JADDR2A PIC  X(0001).
001517     05  JADDR2I PIC  X(0030).
001518*    -------------------------------
001519     05  JCITYSTL PIC S9(0004) COMP.
001520     05  JCITYSTF PIC  X(0001).
001521     05  FILLER REDEFINES JCITYSTF.
001522         10  JCITYSTA PIC  X(0001).
001523     05  JCITYSTI PIC  X(0030).
001524*    -------------------------------
001525     05  JZIPL PIC S9(0004) COMP.
001526     05  JZIPF PIC  X(0001).
001527     05  FILLER REDEFINES JZIPF.
001528         10  JZIPA PIC  X(0001).
001529     05  JZIPI PIC  X(0010).
001530*    -------------------------------
001531     05  JPHONEL PIC S9(0004) COMP.
001532     05  JPHONEF PIC  X(0001).
001533     05  FILLER REDEFINES JPHONEF.
001534         10  JPHONEA PIC  X(0001).
001535     05  JPHONEI PIC  X(0012).
001536*    -------------------------------
001537     05  JEMSG1L PIC S9(0004) COMP.
001538     05  JEMSG1F PIC  X(0001).
001539     05  FILLER REDEFINES JEMSG1F.
001540         10  JEMSG1A PIC  X(0001).
001541     05  JEMSG1I PIC  X(0079).
001542*    -------------------------------
001543     05  JPFKL PIC S9(0004) COMP.
001544     05  JPFKF PIC  X(0001).
001545     05  FILLER REDEFINES JPFKF.
001546         10  JPFKA PIC  X(0001).
001547     05  JPFKI PIC  9(2).
001548 01  EL142JO REDEFINES EL142BI.
001549     05  FILLER            PIC  X(0012).
001550*    -------------------------------
001551     05  FILLER            PIC  X(0003).
001552     05  JDATEO PIC  X(0008).
001553*    -------------------------------
001554     05  FILLER            PIC  X(0003).
001555     05  JTIMEO PIC  99.99.
001556*    -------------------------------
001557     05  FILLER            PIC  X(0003).
001558     05  JTYPEO PIC  X(0010).
001559*    -------------------------------
001560     05  FILLER            PIC  X(0003).
001561     05  JRECDTEO PIC  X(0008).
001562*    -------------------------------
001563     05  FILLER            PIC  X(0003).
001564     05  JBYO PIC  X(0004).
001565*    -------------------------------
001566     05  FILLER            PIC  X(0003).
001567     05  JTLRTYPO PIC  X(0001).
001568*    -------------------------------
001569     05  FILLER            PIC  X(0003).
001570     05  JSEQO PIC  9999.
001571*    -------------------------------
001572     05  FILLER            PIC  X(0003).
001573     05  JMANTBYO PIC  X(0004).
001574*    -------------------------------
001575     05  FILLER            PIC  X(0003).
001576     05  JMANTONO PIC  X(0008).
001577*    -------------------------------
001578     05  FILLER            PIC  X(0003).
001579     05  JMANTATO PIC  99.99.
001580*    -------------------------------
001581     05  FILLER            PIC  X(0003).
001582     05  JMAINTO PIC  X(0001).
001583*    -------------------------------
001584     05  FILLER            PIC  X(0003).
001585     05  JDTSENTO PIC  99B99B99.
001586*    -------------------------------
001587     05  FILLER            PIC  X(0003).
001588     05  JRESENDO PIC  99B99B99.
001589*    -------------------------------
001590     05  FILLER            PIC  X(0003).
001591     05  JSI1O PIC  X(0028).
001592*    -------------------------------
001593     05  FILLER            PIC  X(0003).
001594     05  JREPLYO PIC  99B99B99.
001595*    -------------------------------
001596     05  FILLER            PIC  X(0003).
001597     05  JSI2O PIC  X(0028).
001598*    -------------------------------
001599     05  FILLER            PIC  X(0003).
001600     05  JRECEVEO PIC  99B99B99.
001601*    -------------------------------
001602     05  FILLER            PIC  X(0003).
001603     05  JSI3O PIC  X(0028).
001604*    -------------------------------
001605     05  FILLER            PIC  X(0003).
001606     05  JPHYRECO PIC  99B99B99.
001607*    -------------------------------
001608     05  FILLER            PIC  X(0003).
001609     05  JEMPRECO PIC  99B99B99.
001610*    -------------------------------
001611     05  FILLER            PIC  X(0003).
001612     05  JREMDTO PIC  99B99B99.
001613*    -------------------------------
001614     05  FILLER            PIC  X(0003).
001615     05  JFORMO PIC  X(0008).
001616*    -------------------------------
001617     05  FILLER            PIC  X(0003).
001618     05  JCARR1O PIC  X(0001).
001619*    -------------------------------
001620     05  FILLER            PIC  X(0003).
001621     05  JCLAIM1O PIC  X(0007).
001622*    -------------------------------
001623     05  FILLER            PIC  X(0003).
001624     05  JCERT1O PIC  X(0011).
001625*    -------------------------------
001626     05  FILLER            PIC  X(0003).
001627     05  JCARR2O PIC  X(0001).
001628*    -------------------------------
001629     05  FILLER            PIC  X(0003).
001630     05  JCLAIM2O PIC  X(0007).
001631*    -------------------------------
001632     05  FILLER            PIC  X(0003).
001633     05  JCERT2O PIC  X(0011).
001634*    -------------------------------
001635     05  FILLER            PIC  X(0003).
001636     05  JMAILTOO PIC  X(0030).
001637*    -------------------------------
001638     05  FILLER            PIC  X(0003).
001639     05  JADDR1O PIC  X(0030).
001640*    -------------------------------
001641     05  FILLER            PIC  X(0003).
001642     05  JADDR2O PIC  X(0030).
001643*    -------------------------------
001644     05  FILLER            PIC  X(0003).
001645     05  JCITYSTO PIC  X(0030).
001646*    -------------------------------
001647     05  FILLER            PIC  X(0003).
001648     05  JZIPO PIC  X(0010).
001649*    -------------------------------
001650     05  FILLER            PIC  X(0003).
001651     05  JPHONEO PIC  999B999B9999.
001652*    -------------------------------
001653     05  FILLER            PIC  X(0003).
001654     05  JEMSG1O PIC  X(0079).
001655*    -------------------------------
001656     05  FILLER            PIC  X(0003).
001657     05  JPFKO PIC  99.
001658*    -------------------------------
001659 01  EL142AI REDEFINES EL142BI.
001660     05  FILLER            PIC  X(0012).
001661*    -------------------------------
001662     05  ADATEL PIC S9(0004) COMP.
001663     05  ADATEF PIC  X(0001).
001664     05  FILLER REDEFINES ADATEF.
001665         10  ADATEA PIC  X(0001).
001666     05  ADATEI PIC  X(0008).
001667*    -------------------------------
001668     05  ATIMEL PIC S9(0004) COMP.
001669     05  ATIMEF PIC  X(0001).
001670     05  FILLER REDEFINES ATIMEF.
001671         10  ATIMEA PIC  X(0001).
001672     05  ATIMEI PIC  X(0005).
001673*    -------------------------------
001674     05  ARECDTEL PIC S9(0004) COMP.
001675     05  ARECDTEF PIC  X(0001).
001676     05  FILLER REDEFINES ARECDTEF.
001677         10  ARECDTEA PIC  X(0001).
001678     05  ARECDTEI PIC  X(0008).
001679*    -------------------------------
001680     05  AREMINDL PIC S9(0004) COMP.
001681     05  AREMINDF PIC  X(0001).
001682     05  FILLER REDEFINES AREMINDF.
001683         10  AREMINDA PIC  X(0001).
001684     05  AREMINDI PIC  X(0001).
001685*    -------------------------------
001686     05  ALETTERL PIC S9(0004) COMP.
001687     05  ALETTERF PIC  X(0001).
001688     05  FILLER REDEFINES ALETTERF.
001689         10  ALETTERA PIC  X(0001).
001690     05  ALETTERI PIC  X(0001).
001691*    -------------------------------
001692     05  APAYMNTL PIC S9(0004) COMP.
001693     05  APAYMNTF PIC  X(0001).
001694     05  FILLER REDEFINES APAYMNTF.
001695         10  APAYMNTA PIC  X(0001).
001696     05  APAYMNTI PIC  X(0001).
001697*    -------------------------------
001698     05  AAUTOPAL PIC S9(0004) COMP.
001699     05  AAUTOPAF PIC  X(0001).
001700     05  FILLER REDEFINES AAUTOPAF.
001701         10  AAUTOPAA PIC  X(0001).
001702     05  AAUTOPAI PIC  X(0001).
001703*    -------------------------------
001704     05  ANOTESL PIC S9(0004) COMP.
001705     05  ANOTESF PIC  X(0001).
001706     05  FILLER REDEFINES ANOTESF.
001707         10  ANOTESA PIC  X(0001).
001708     05  ANOTESI PIC  X(0001).
001709*    -------------------------------
001710     05  ARESEXPL PIC S9(0004) COMP.
001711     05  ARESEXPF PIC  X(0001).
001712     05  FILLER REDEFINES ARESEXPF.
001713         10  ARESEXPA PIC  X(0001).
001714     05  ARESEXPI PIC  X(0001).
001715*    -------------------------------
001716     05  ADENIALL PIC S9(0004) COMP.
001717     05  ADENIALF PIC  X(0001).
001718     05  FILLER REDEFINES ADENIALF.
001719         10  ADENIALA PIC  X(0001).
001720     05  ADENIALI PIC  X(0001).
001721*    -------------------------------
001722     05  AIDCL PIC S9(0004) COMP.
001723     05  AIDCF PIC  X(0001).
001724     05  FILLER REDEFINES AIDCF.
001725         10  AIDCA PIC  X(0001).
001726     05  AIDCI PIC  X(0001).
001727*    -------------------------------
001728     05  AFORMSL PIC S9(0004) COMP.
001729     05  AFORMSF PIC  X(0001).
001730     05  FILLER REDEFINES AFORMSF.
001731         10  AFORMSA PIC  X(0001).
001732     05  AFORMSI PIC  X(0001).
001733*    -------------------------------
001734     05  ASEQL PIC S9(0004) COMP.
001735     05  ASEQF PIC  X(0001).
001736     05  FILLER REDEFINES ASEQF.
001737         10  ASEQA PIC  X(0001).
001738     05  ASEQI PIC  X(0004).
001739*    -------------------------------
001740     05  AEMSG1L PIC S9(0004) COMP.
001741     05  AEMSG1F PIC  X(0001).
001742     05  FILLER REDEFINES AEMSG1F.
001743         10  AEMSG1A PIC  X(0001).
001744     05  AEMSG1I PIC  X(0079).
001745*    -------------------------------
001746     05  AEMSG2L PIC S9(0004) COMP.
001747     05  AEMSG2F PIC  X(0001).
001748     05  FILLER REDEFINES AEMSG2F.
001749         10  AEMSG2A PIC  X(0001).
001750     05  AEMSG2I PIC  X(0079).
001751*    -------------------------------
001752     05  AEMSG3L PIC S9(0004) COMP.
001753     05  AEMSG3F PIC  X(0001).
001754     05  FILLER REDEFINES AEMSG3F.
001755         10  AEMSG3A PIC  X(0001).
001756     05  AEMSG3I PIC  X(0079).
001757 01  EL142AO REDEFINES EL142BI.
001758     05  FILLER            PIC  X(0012).
001759*    -------------------------------
001760     05  FILLER            PIC  X(0003).
001761     05  ADATEO PIC  X(0008).
001762*    -------------------------------
001763     05  FILLER            PIC  X(0003).
001764     05  ATIMEO PIC  99.99.
001765*    -------------------------------
001766     05  FILLER            PIC  X(0003).
001767     05  ARECDTEO PIC  99B99B99.
001768*    -------------------------------
001769     05  FILLER            PIC  X(0003).
001770     05  AREMINDO PIC  X(0001).
001771*    -------------------------------
001772     05  FILLER            PIC  X(0003).
001773     05  ALETTERO PIC  X(0001).
001774*    -------------------------------
001775     05  FILLER            PIC  X(0003).
001776     05  APAYMNTO PIC  X(0001).
001777*    -------------------------------
001778     05  FILLER            PIC  X(0003).
001779     05  AAUTOPAO PIC  X(0001).
001780*    -------------------------------
001781     05  FILLER            PIC  X(0003).
001782     05  ANOTESO PIC  X(0001).
001783*    -------------------------------
001784     05  FILLER            PIC  X(0003).
001785     05  ARESEXPO PIC  X(0001).
001786*    -------------------------------
001787     05  FILLER            PIC  X(0003).
001788     05  ADENIALO PIC  X(0001).
001789*    -------------------------------
001790     05  FILLER            PIC  X(0003).
001791     05  AIDCO PIC  X(0001).
001792*    -------------------------------
001793     05  FILLER            PIC  X(0003).
001794     05  AFORMSO PIC  X(0001).
001795*    -------------------------------
001796     05  FILLER            PIC  X(0003).
001797     05  ASEQO PIC  9999.
001798*    -------------------------------
001799     05  FILLER            PIC  X(0003).
001800     05  AEMSG1O PIC  X(0079).
001801*    -------------------------------
001802     05  FILLER            PIC  X(0003).
001803     05  AEMSG2O PIC  X(0079).
001804*    -------------------------------
001805     05  FILLER            PIC  X(0003).
001806     05  AEMSG3O PIC  X(0079).
001807*    -------------------------------
001808 01  EL142B2I REDEFINES EL142BI.
001809     05  FILLER            PIC  X(0012).
001810*    -------------------------------
001811     05  KDATEL PIC S9(0004) COMP.
001812     05  KDATEF PIC  X(0001).
001813     05  FILLER REDEFINES KDATEF.
001814         10  KDATEA PIC  X(0001).
001815     05  KDATEI PIC  X(0008).
001816*    -------------------------------
001817     05  KTIMEL PIC S9(0004) COMP.
001818     05  KTIMEF PIC  X(0001).
001819     05  FILLER REDEFINES KTIMEF.
001820         10  KTIMEA PIC  X(0001).
001821     05  KTIMEI PIC  X(0005).
001822*    -------------------------------
001823     05  KMAINTL PIC S9(0004) COMP.
001824     05  KMAINTF PIC  X(0001).
001825     05  FILLER REDEFINES KMAINTF.
001826         10  KMAINTA PIC  X(0001).
001827     05  KMAINTI PIC  X(0001).
001828*    -------------------------------
001829     05  KCONTRLL PIC S9(0004) COMP.
001830     05  KCONTRLF PIC  X(0001).
001831     05  FILLER REDEFINES KCONTRLF.
001832         10  KCONTRLA PIC  X(0001).
001833     05  KCONTRLI PIC  9(8).
001834*    -------------------------------
001835     05  KSEQL PIC S9(0004) COMP.
001836     05  KSEQF PIC  X(0001).
001837     05  FILLER REDEFINES KSEQF.
001838         10  KSEQA PIC  X(0001).
001839     05  KSEQI PIC  9(4).
001840*    -------------------------------
001841     05  KTYPEL PIC S9(0004) COMP.
001842     05  KTYPEF PIC  X(0001).
001843     05  FILLER REDEFINES KTYPEF.
001844         10  KTYPEA PIC  X(0001).
001845     05  KTYPEI PIC  X(0001).
001846*    -------------------------------
001847     05  KTSEQL PIC S9(0004) COMP.
001848     05  KTSEQF PIC  X(0001).
001849     05  FILLER REDEFINES KTSEQF.
001850         10  KTSEQA PIC  X(0001).
001851     05  KTSEQI PIC  9(4).
001852*    -------------------------------
001853     05  KCARRL PIC S9(0004) COMP.
001854     05  KCARRF PIC  X(0001).
001855     05  FILLER REDEFINES KCARRF.
001856         10  KCARRA PIC  X(0001).
001857     05  KCARRI PIC  X(0001).
001858*    -------------------------------
001859     05  KCTYPEL PIC S9(0004) COMP.
001860     05  KCTYPEF PIC  X(0001).
001861     05  FILLER REDEFINES KCTYPEF.
001862         10  KCTYPEA PIC  X(0001).
001863     05  KCTYPEI PIC  X(0001).
001864*    -------------------------------
001865     05  KCLMNOL PIC S9(0004) COMP.
001866     05  KCLMNOF PIC  X(0001).
001867     05  FILLER REDEFINES KCLMNOF.
001868         10  KCLMNOA PIC  X(0001).
001869     05  KCLMNOI PIC  X(0007).
001870*    -------------------------------
001871     05  KCOVERL PIC S9(0004) COMP.
001872     05  KCOVERF PIC  X(0001).
001873     05  FILLER REDEFINES KCOVERF.
001874         10  KCOVERA PIC  X(0001).
001875     05  KCOVERI PIC  X(0002).
001876*    -------------------------------
001877     05  KCERTNOL PIC S9(0004) COMP.
001878     05  KCERTNOF PIC  X(0001).
001879     05  FILLER REDEFINES KCERTNOF.
001880         10  KCERTNOA PIC  X(0001).
001881     05  KCERTNOI PIC  X(0011).
001882*    -------------------------------
001883     05  KCKNOL PIC S9(0004) COMP.
001884     05  KCKNOF PIC  X(0001).
001885     05  FILLER REDEFINES KCKNOF.
001886         10  KCKNOA PIC  X(0001).
001887     05  KCKNOI PIC  X(0007).
001888*    -------------------------------
001889     05  KCKNO2L PIC S9(0004) COMP.
001890     05  KCKNO2F PIC  X(0001).
001891     05  FILLER REDEFINES KCKNO2F.
001892         10  KCKNO2A PIC  X(0001).
001893     05  KCKNO2I PIC  X(0007).
001894*    -------------------------------
001895     05  KPAYTYPL PIC S9(0004) COMP.
001896     05  KPAYTYPF PIC  X(0001).
001897     05  FILLER REDEFINES KPAYTYPF.
001898         10  KPAYTYPA PIC  X(0001).
001899     05  KPAYTYPI PIC  X(0017).
001900*    -------------------------------
001901     05  KCKAMTL PIC S9(0004) COMP.
001902     05  KCKAMTF PIC  X(0001).
001903     05  FILLER REDEFINES KCKAMTF.
001904         10  KCKAMTA PIC  X(0001).
001905     05  KCKAMTI PIC  S9(11)V9(2).
001906*    -------------------------------
001907     05  KBYL PIC S9(0004) COMP.
001908     05  KBYF PIC  X(0001).
001909     05  FILLER REDEFINES KBYF.
001910         10  KBYA PIC  X(0001).
001911     05  KBYI PIC  X(0004).
001912*    -------------------------------
001913     05  KCKDATEL PIC S9(0004) COMP.
001914     05  KCKDATEF PIC  X(0001).
001915     05  FILLER REDEFINES KCKDATEF.
001916         10  KCKDATEA PIC  X(0001).
001917     05  KCKDATEI PIC  X(0008).
001918*    -------------------------------
001919     05  KVOIDL PIC S9(0004) COMP.
001920     05  KVOIDF PIC  X(0001).
001921     05  FILLER REDEFINES KVOIDF.
001922         10  KVOIDA PIC  X(0001).
001923     05  KVOIDI PIC  X(0003).
001924*    -------------------------------
001925     05  KTIMPRTL PIC S9(0004) COMP.
001926     05  KTIMPRTF PIC  X(0001).
001927     05  FILLER REDEFINES KTIMPRTF.
001928         10  KTIMPRTA PIC  X(0001).
001929     05  KTIMPRTI PIC  9(4).
001930*    -------------------------------
001931     05  KPRENOL PIC S9(0004) COMP.
001932     05  KPRENOF PIC  X(0001).
001933     05  FILLER REDEFINES KPRENOF.
001934         10  KPRENOA PIC  X(0001).
001935     05  KPRENOI PIC  X(0003).
001936*    -------------------------------
001937     05  KEMSG1L PIC S9(0004) COMP.
001938     05  KEMSG1F PIC  X(0001).
001939     05  FILLER REDEFINES KEMSG1F.
001940         10  KEMSG1A PIC  X(0001).
001941     05  KEMSG1I PIC  X(0079).
001942*    -------------------------------
001943     05  KEMSG2L PIC S9(0004) COMP.
001944     05  KEMSG2F PIC  X(0001).
001945     05  FILLER REDEFINES KEMSG2F.
001946         10  KEMSG2A PIC  X(0001).
001947     05  KEMSG2I PIC  X(0079).
001948*    -------------------------------
001949     05  KEMSG3L PIC S9(0004) COMP.
001950     05  KEMSG3F PIC  X(0001).
001951     05  FILLER REDEFINES KEMSG3F.
001952         10  KEMSG3A PIC  X(0001).
001953     05  KEMSG3I PIC  X(0079).
001954*    -------------------------------
001955     05  KPFKL PIC S9(0004) COMP.
001956     05  KPFKF PIC  X(0001).
001957     05  FILLER REDEFINES KPFKF.
001958         10  KPFKA PIC  X(0001).
001959     05  KPFKI PIC  9(2).
001960 01  EL142B2O REDEFINES EL142BI.
001961     05  FILLER            PIC  X(0012).
001962*    -------------------------------
001963     05  FILLER            PIC  X(0003).
001964     05  KDATEO PIC  X(0008).
001965*    -------------------------------
001966     05  FILLER            PIC  X(0003).
001967     05  KTIMEO PIC  99.99.
001968*    -------------------------------
001969     05  FILLER            PIC  X(0003).
001970     05  KMAINTO PIC  X(0001).
001971*    -------------------------------
001972     05  FILLER            PIC  X(0003).
001973     05  KCONTRLO PIC  ZZZZZZZ9.
001974*    -------------------------------
001975     05  FILLER            PIC  X(0003).
001976     05  KSEQO PIC  ZZZ9.
001977*    -------------------------------
001978     05  FILLER            PIC  X(0003).
001979     05  KTYPEO PIC  X(0001).
001980*    -------------------------------
001981     05  FILLER            PIC  X(0003).
001982     05  KTSEQO PIC  ZZZ9.
001983*    -------------------------------
001984     05  FILLER            PIC  X(0003).
001985     05  KCARRO PIC  X(0001).
001986*    -------------------------------
001987     05  FILLER            PIC  X(0003).
001988     05  KCTYPEO PIC  X(0001).
001989*    -------------------------------
001990     05  FILLER            PIC  X(0003).
001991     05  KCLMNOO PIC  X(0007).
001992*    -------------------------------
001993     05  FILLER            PIC  X(0003).
001994     05  KCOVERO PIC  X(0002).
001995*    -------------------------------
001996     05  FILLER            PIC  X(0003).
001997     05  KCERTNOO PIC  X(0011).
001998*    -------------------------------
001999     05  FILLER            PIC  X(0003).
002000     05  KCKNOO PIC  X(0007).
002001*    -------------------------------
002002     05  FILLER            PIC  X(0003).
002003     05  KCKNO2O PIC  X(0007).
002004*    -------------------------------
002005     05  FILLER            PIC  X(0003).
002006     05  KPAYTYPO PIC  X(0017).
002007*    -------------------------------
002008     05  FILLER            PIC  X(0003).
002009     05  KCKAMTO PIC  Z,ZZZ,ZZ9.99-.
002010*    -------------------------------
002011     05  FILLER            PIC  X(0003).
002012     05  KBYO PIC  X(0004).
002013*    -------------------------------
002014     05  FILLER            PIC  X(0003).
002015     05  KCKDATEO PIC  X(0008).
002016*    -------------------------------
002017     05  FILLER            PIC  X(0003).
002018     05  KVOIDO PIC  X(0003).
002019*    -------------------------------
002020     05  FILLER            PIC  X(0003).
002021     05  KTIMPRTO PIC  ZZZ9.
002022*    -------------------------------
002023     05  FILLER            PIC  X(0003).
002024     05  KPRENOO PIC  X(0003).
002025*    -------------------------------
002026     05  FILLER            PIC  X(0003).
002027     05  KEMSG1O PIC  X(0079).
002028*    -------------------------------
002029     05  FILLER            PIC  X(0003).
002030     05  KEMSG2O PIC  X(0079).
002031*    -------------------------------
002032     05  FILLER            PIC  X(0003).
002033     05  KEMSG3O PIC  X(0079).
002034*    -------------------------------
002035     05  FILLER            PIC  X(0003).
002036     05  KPFKO PIC  99.
002037*    -------------------------------
002038 01  EL142CI REDEFINES EL142BI.
002039     05  FILLER            PIC  X(0012).
002040*    -------------------------------
002041     05  CDATEL PIC S9(0004) COMP.
002042     05  CDATEF PIC  X(0001).
002043     05  FILLER REDEFINES CDATEF.
002044         10  CDATEA PIC  X(0001).
002045     05  CDATEI PIC  X(0008).
002046*    -------------------------------
002047     05  CTIMEL PIC S9(0004) COMP.
002048     05  CTIMEF PIC  X(0001).
002049     05  FILLER REDEFINES CTIMEF.
002050         10  CTIMEA PIC  X(0001).
002051     05  CTIMEI PIC  X(0005).
002052*    -------------------------------
002053     05  CTYPEL PIC S9(0004) COMP.
002054     05  CTYPEF PIC  X(0001).
002055     05  FILLER REDEFINES CTYPEF.
002056         10  CTYPEA PIC  X(0001).
002057     05  CTYPEI PIC  X(0010).
002058*    -------------------------------
002059     05  CRECDTEL PIC S9(0004) COMP.
002060     05  CRECDTEF PIC  X(0001).
002061     05  FILLER REDEFINES CRECDTEF.
002062         10  CRECDTEA PIC  X(0001).
002063     05  CRECDTEI PIC  X(0008).
002064*    -------------------------------
002065     05  CBYL PIC S9(0004) COMP.
002066     05  CBYF PIC  X(0001).
002067     05  FILLER REDEFINES CBYF.
002068         10  CBYA PIC  X(0001).
002069     05  CBYI PIC  X(0004).
002070*    -------------------------------
002071     05  CTLRTYPL PIC S9(0004) COMP.
002072     05  CTLRTYPF PIC  X(0001).
002073     05  FILLER REDEFINES CTLRTYPF.
002074         10  CTLRTYPA PIC  X(0001).
002075     05  CTLRTYPI PIC  X(0001).
002076*    -------------------------------
002077     05  CSEQL PIC S9(0004) COMP.
002078     05  CSEQF PIC  X(0001).
002079     05  FILLER REDEFINES CSEQF.
002080         10  CSEQA PIC  X(0001).
002081     05  CSEQI PIC  X(0004).
002082*    -------------------------------
002083     05  CMANTBYL PIC S9(0004) COMP.
002084     05  CMANTBYF PIC  X(0001).
002085     05  FILLER REDEFINES CMANTBYF.
002086         10  CMANTBYA PIC  X(0001).
002087     05  CMANTBYI PIC  X(0004).
002088*    -------------------------------
002089     05  CMANTONL PIC S9(0004) COMP.
002090     05  CMANTONF PIC  X(0001).
002091     05  FILLER REDEFINES CMANTONF.
002092         10  CMANTONA PIC  X(0001).
002093     05  CMANTONI PIC  X(0008).
002094*    -------------------------------
002095     05  CMANTATL PIC S9(0004) COMP.
002096     05  CMANTATF PIC  X(0001).
002097     05  FILLER REDEFINES CMANTATF.
002098         10  CMANTATA PIC  X(0001).
002099     05  CMANTATI PIC  X(0005).
002100*    -------------------------------
002101     05  CMAINTL PIC S9(0004) COMP.
002102     05  CMAINTF PIC  X(0001).
002103     05  FILLER REDEFINES CMAINTF.
002104         10  CMAINTA PIC  X(0001).
002105     05  CMAINTI PIC  X(0001).
002106*    -------------------------------
002107     05  CEFFDTEL PIC S9(0004) COMP.
002108     05  CEFFDTEF PIC  X(0001).
002109     05  FILLER REDEFINES CEFFDTEF.
002110         10  CEFFDTEA PIC  X(0001).
002111     05  CEFFDTEI PIC  X(0008).
002112*    -------------------------------
002113     05  CREPDTEL PIC S9(0004) COMP.
002114     05  CREPDTEF PIC  X(0001).
002115     05  FILLER REDEFINES CREPDTEF.
002116         10  CREPDTEA PIC  X(0001).
002117     05  CREPDTEI PIC  X(0008).
002118*    -------------------------------
002119     05  C1STPAL PIC S9(0004) COMP.
002120     05  C1STPAF PIC  X(0001).
002121     05  FILLER REDEFINES C1STPAF.
002122         10  C1STPAA PIC  X(0001).
002123     05  C1STPAI PIC  X(0013).
002124*    -------------------------------
002125     05  CREGPAL PIC S9(0004) COMP.
002126     05  CREGPAF PIC  X(0001).
002127     05  FILLER REDEFINES CREGPAF.
002128         10  CREGPAA PIC  X(0001).
002129     05  CREGPAI PIC  X(0013).
002130*    -------------------------------
002131     05  C1STPSL PIC S9(0004) COMP.
002132     05  C1STPSF PIC  X(0001).
002133     05  FILLER REDEFINES C1STPSF.
002134         10  C1STPSA PIC  X(0001).
002135     05  C1STPSI PIC  X(0008).
002136*    -------------------------------
002137     05  CLSTPSL PIC S9(0004) COMP.
002138     05  CLSTPSF PIC  X(0001).
002139     05  FILLER REDEFINES CLSTPSF.
002140         10  CLSTPSA PIC  X(0001).
002141     05  CLSTPSI PIC  X(0008).
002142*    -------------------------------
002143     05  CDIFPL PIC S9(0004) COMP.
002144     05  CDIFPF PIC  X(0001).
002145     05  FILLER REDEFINES CDIFPF.
002146         10  CDIFPA PIC  X(0001).
002147     05  CDIFPI PIC  X(0006).
002148*    -------------------------------
002149     05  CMBPAYL PIC S9(0004) COMP.
002150     05  CMBPAYF PIC  X(0001).
002151     05  FILLER REDEFINES CMBPAYF.
002152         10  CMBPAYA PIC  X(0001).
002153     05  CMBPAYI PIC  X(0006).
002154*    -------------------------------
002155     05  CLSTPATL PIC S9(0004) COMP.
002156     05  CLSTPATF PIC  X(0001).
002157     05  FILLER REDEFINES CLSTPATF.
002158         10  CLSTPATA PIC  X(0001).
002159     05  CLSTPATI PIC  X(0007).
002160*    -------------------------------
002161     05  CPAYEEL PIC S9(0004) COMP.
002162     05  CPAYEEF PIC  X(0001).
002163     05  FILLER REDEFINES CPAYEEF.
002164         10  CPAYEEA PIC  X(0001).
002165     05  CPAYEEI PIC  X(0011).
002166*    -------------------------------
002167     05  CENDLETL PIC S9(0004) COMP.
002168     05  CENDLETF PIC  X(0001).
002169     05  FILLER REDEFINES CENDLETF.
002170         10  CENDLETA PIC  X(0001).
002171     05  CENDLETI PIC  X(0004).
002172*    -------------------------------
002173     05  CEMSG1L PIC S9(0004) COMP.
002174     05  CEMSG1F PIC  X(0001).
002175     05  FILLER REDEFINES CEMSG1F.
002176         10  CEMSG1A PIC  X(0001).
002177     05  CEMSG1I PIC  X(0079).
002178*    -------------------------------
002179     05  CEMSG2L PIC S9(0004) COMP.
002180     05  CEMSG2F PIC  X(0001).
002181     05  FILLER REDEFINES CEMSG2F.
002182         10  CEMSG2A PIC  X(0001).
002183     05  CEMSG2I PIC  X(0079).
002184*    -------------------------------
002185     05  CPFKL PIC S9(0004) COMP.
002186     05  CPFKF PIC  X(0001).
002187     05  FILLER REDEFINES CPFKF.
002188         10  CPFKA PIC  X(0001).
002189     05  CPFKI PIC  9(2).
002190 01  EL142CO REDEFINES EL142BI.
002191     05  FILLER            PIC  X(0012).
002192*    -------------------------------
002193     05  FILLER            PIC  X(0003).
002194     05  CDATEO PIC  X(0008).
002195*    -------------------------------
002196     05  FILLER            PIC  X(0003).
002197     05  CTIMEO PIC  99.99.
002198*    -------------------------------
002199     05  FILLER            PIC  X(0003).
002200     05  CTYPEO PIC  X(0010).
002201*    -------------------------------
002202     05  FILLER            PIC  X(0003).
002203     05  CRECDTEO PIC  X(0008).
002204*    -------------------------------
002205     05  FILLER            PIC  X(0003).
002206     05  CBYO PIC  X(0004).
002207*    -------------------------------
002208     05  FILLER            PIC  X(0003).
002209     05  CTLRTYPO PIC  X(0001).
002210*    -------------------------------
002211     05  FILLER            PIC  X(0003).
002212     05  CSEQO PIC  9999.
002213*    -------------------------------
002214     05  FILLER            PIC  X(0003).
002215     05  CMANTBYO PIC  X(0004).
002216*    -------------------------------
002217     05  FILLER            PIC  X(0003).
002218     05  CMANTONO PIC  X(0008).
002219*    -------------------------------
002220     05  FILLER            PIC  X(0003).
002221     05  CMANTATO PIC  99.99.
002222*    -------------------------------
002223     05  FILLER            PIC  X(0003).
002224     05  CMAINTO PIC  X(0001).
002225*    -------------------------------
002226     05  FILLER            PIC  X(0003).
002227     05  CEFFDTEO PIC  X(0008).
002228*    -------------------------------
002229     05  FILLER            PIC  X(0003).
002230     05  CREPDTEO PIC  X(0008).
002231*    -------------------------------
002232     05  FILLER            PIC  X(0003).
002233     05  C1STPAO PIC  Z,ZZZ,ZZ9.99-.
002234*    -------------------------------
002235     05  FILLER            PIC  X(0003).
002236     05  CREGPAO PIC  Z,ZZZ,ZZ9.99-.
002237*    -------------------------------
002238     05  FILLER            PIC  X(0003).
002239     05  C1STPSO PIC  X(0008).
002240*    -------------------------------
002241     05  FILLER            PIC  X(0003).
002242     05  CLSTPSO PIC  X(0008).
002243*    -------------------------------
002244     05  FILLER            PIC  X(0003).
002245     05  CDIFPO PIC  Z,ZZ9-.
002246*    -------------------------------
002247     05  FILLER            PIC  X(0003).
002248     05  CMBPAYO PIC  Z,ZZ9-.
002249*    -------------------------------
002250     05  FILLER            PIC  X(0003).
002251     05  CLSTPATO PIC  X(0007).
002252*    -------------------------------
002253     05  FILLER            PIC  X(0003).
002254     05  CPAYEEO PIC  X(0011).
002255*    -------------------------------
002256     05  FILLER            PIC  X(0003).
002257     05  CENDLETO PIC  X(0004).
002258*    -------------------------------
002259     05  FILLER            PIC  X(0003).
002260     05  CEMSG1O PIC  X(0079).
002261*    -------------------------------
002262     05  FILLER            PIC  X(0003).
002263     05  CEMSG2O PIC  X(0079).
002264*    -------------------------------
002265     05  FILLER            PIC  X(0003).
002266     05  CPFKO PIC  99.
002267*    -------------------------------
002268 01  EL142D2I REDEFINES EL142BI.
002269     05  FILLER            PIC  X(0012).
002270*    -------------------------------
002271     05  LDATEL PIC S9(0004) COMP.
002272     05  LDATEF PIC  X(0001).
002273     05  FILLER REDEFINES LDATEF.
002274         10  LDATEA PIC  X(0001).
002275     05  LDATEI PIC  X(0008).
002276*    -------------------------------
002277     05  LTIMEL PIC S9(0004) COMP.
002278     05  LTIMEF PIC  X(0001).
002279     05  FILLER REDEFINES LTIMEF.
002280         10  LTIMEA PIC  X(0001).
002281     05  LTIMEI PIC  X(0005).
002282*    -------------------------------
002283     05  LARCHNOL PIC S9(0004) COMP.
002284     05  LARCHNOF PIC  X(0001).
002285     05  FILLER REDEFINES LARCHNOF.
002286         10  LARCHNOA PIC  X(0001).
002287     05  LARCHNOI PIC  X(0008).
002288*    -------------------------------
002289     05  LNXTDUEL PIC S9(0004) COMP.
002290     05  LNXTDUEF PIC  X(0001).
002291     05  FILLER REDEFINES LNXTDUEF.
002292         10  LNXTDUEA PIC  X(0001).
002293     05  LNXTDUEI PIC  X(0008).
002294*    -------------------------------
002295     05  LCARRL PIC S9(0004) COMP.
002296     05  LCARRF PIC  X(0001).
002297     05  FILLER REDEFINES LCARRF.
002298         10  LCARRA PIC  X(0001).
002299     05  LCARRI PIC  X(0001).
002300*    -------------------------------
002301     05  LAUTOPYL PIC S9(0004) COMP.
002302     05  LAUTOPYF PIC  X(0001).
002303     05  FILLER REDEFINES LAUTOPYF.
002304         10  LAUTOPYA PIC  X(0001).
002305     05  LAUTOPYI PIC  X(0008).
002306*    -------------------------------
002307     05  LCLMNOL PIC S9(0004) COMP.
002308     05  LCLMNOF PIC  X(0001).
002309     05  FILLER REDEFINES LCLMNOF.
002310         10  LCLMNOA PIC  X(0001).
002311     05  LCLMNOI PIC  X(0007).
002312*    -------------------------------
002313     05  LCREATNL PIC S9(0004) COMP.
002314     05  LCREATNF PIC  X(0001).
002315     05  FILLER REDEFINES LCREATNF.
002316         10  LCREATNA PIC  X(0001).
002317     05  LCREATNI PIC  X(0001).
002318*    -------------------------------
002319     05  LCRTNOL PIC S9(0004) COMP.
002320     05  LCRTNOF PIC  X(0001).
002321     05  FILLER REDEFINES LCRTNOF.
002322         10  LCRTNOA PIC  X(0001).
002323     05  LCRTNOI PIC  X(0011).
002324*    -------------------------------
002325     05  LORIGARL PIC S9(0004) COMP.
002326     05  LORIGARF PIC  X(0001).
002327     05  FILLER REDEFINES LORIGARF.
002328         10  LORIGARA PIC  X(0001).
002329     05  LORIGARI PIC  X(0008).
002330*    -------------------------------
002331     05  LLETRIDL PIC S9(0004) COMP.
002332     05  LLETRIDF PIC  X(0001).
002333     05  FILLER REDEFINES LLETRIDF.
002334         10  LLETRIDA PIC  X(0001).
002335     05  LLETRIDI PIC  X(0004).
002336*    -------------------------------
002337     05  LPROMPTL PIC S9(0004) COMP.
002338     05  LPROMPTF PIC  X(0001).
002339     05  FILLER REDEFINES LPROMPTF.
002340         10  LPROMPTA PIC  X(0001).
002341     05  LPROMPTI PIC  X(0001).
002342*    -------------------------------
002343     05  LPROCL PIC S9(0004) COMP.
002344     05  LPROCF PIC  X(0001).
002345     05  FILLER REDEFINES LPROCF.
002346         10  LPROCA PIC  X(0001).
002347     05  LPROCI PIC  X(0004).
002348*    -------------------------------
002349     05  LORIGENL PIC S9(0004) COMP.
002350     05  LORIGENF PIC  X(0001).
002351     05  FILLER REDEFINES LORIGENF.
002352         10  LORIGENA PIC  X(0001).
002353     05  LORIGENI PIC  X(0001).
002354*    -------------------------------
002355     05  LCREDTEL PIC S9(0004) COMP.
002356     05  LCREDTEF PIC  X(0001).
002357     05  FILLER REDEFINES LCREDTEF.
002358         10  LCREDTEA PIC  X(0001).
002359     05  LCREDTEI PIC  X(0008).
002360*    -------------------------------
002361     05  LNOCPYSL PIC S9(0004) COMP.
002362     05  LNOCPYSF PIC  X(0001).
002363     05  FILLER REDEFINES LNOCPYSF.
002364         10  LNOCPYSA PIC  X(0001).
002365     05  LNOCPYSI PIC  X(0003).
002366*    -------------------------------
002367     05  LINPRNTL PIC S9(0004) COMP.
002368     05  LINPRNTF PIC  X(0001).
002369     05  FILLER REDEFINES LINPRNTF.
002370         10  LINPRNTA PIC  X(0001).
002371     05  LINPRNTI PIC  X(0008).
002372*    -------------------------------
002373     05  LENCCODL PIC S9(0004) COMP.
002374     05  LENCCODF PIC  X(0001).
002375     05  FILLER REDEFINES LENCCODF.
002376         10  LENCCODA PIC  X(0001).
002377     05  LENCCODI PIC  X(0003).
002378*    -------------------------------
002379     05  LFUPDTEL PIC S9(0004) COMP.
002380     05  LFUPDTEF PIC  X(0001).
002381     05  FILLER REDEFINES LFUPDTEF.
002382         10  LFUPDTEA PIC  X(0001).
002383     05  LFUPDTEI PIC  X(0008).
002384*    -------------------------------
002385     05  LRESDTEL PIC S9(0004) COMP.
002386     05  LRESDTEF PIC  X(0001).
002387     05  FILLER REDEFINES LRESDTEF.
002388         10  LRESDTEA PIC  X(0001).
002389     05  LRESDTEI PIC  X(0008).
002390*    -------------------------------
002391     05  LRSLTIDL PIC S9(0004) COMP.
002392     05  LRSLTIDF PIC  X(0001).
002393     05  FILLER REDEFINES LRSLTIDF.
002394         10  LRSLTIDA PIC  X(0001).
002395     05  LRSLTIDI PIC  X(0004).
002396*    -------------------------------
002397     05  LREPRNTL PIC S9(0004) COMP.
002398     05  LREPRNTF PIC  X(0001).
002399     05  FILLER REDEFINES LREPRNTF.
002400         10  LREPRNTA PIC  X(0001).
002401     05  LREPRNTI PIC  X(0008).
002402*    -------------------------------
002403     05  LADDTYPL PIC S9(0004) COMP.
002404     05  LADDTYPF PIC  X(0001).
002405     05  FILLER REDEFINES LADDTYPF.
002406         10  LADDTYPA PIC  X(0001).
002407     05  LADDTYPI PIC  X(0001).
002408*    -------------------------------
002409     05  LCORSEQL PIC S9(0004) COMP.
002410     05  LCORSEQF PIC  X(0001).
002411     05  FILLER REDEFINES LCORSEQF.
002412         10  LCORSEQA PIC  X(0001).
002413     05  LCORSEQI PIC  X(0006).
002414*    -------------------------------
002415     05  L1STPRTL PIC S9(0004) COMP.
002416     05  L1STPRTF PIC  X(0001).
002417     05  FILLER REDEFINES L1STPRTF.
002418         10  L1STPRTA PIC  X(0001).
002419     05  L1STPRTI PIC  X(0008).
002420*    -------------------------------
002421     05  LEMSG1L PIC S9(0004) COMP.
002422     05  LEMSG1F PIC  X(0001).
002423     05  FILLER REDEFINES LEMSG1F.
002424         10  LEMSG1A PIC  X(0001).
002425     05  LEMSG1I PIC  X(0079).
002426*    -------------------------------
002427     05  LEMSG2L PIC S9(0004) COMP.
002428     05  LEMSG2F PIC  X(0001).
002429     05  FILLER REDEFINES LEMSG2F.
002430         10  LEMSG2A PIC  X(0001).
002431     05  LEMSG2I PIC  X(0079).
002432*    -------------------------------
002433     05  LEMSG3L PIC S9(0004) COMP.
002434     05  LEMSG3F PIC  X(0001).
002435     05  FILLER REDEFINES LEMSG3F.
002436         10  LEMSG3A PIC  X(0001).
002437     05  LEMSG3I PIC  X(0079).
002438*    -------------------------------
002439     05  LPFKL PIC S9(0004) COMP.
002440     05  LPFKF PIC  X(0001).
002441     05  FILLER REDEFINES LPFKF.
002442         10  LPFKA PIC  X(0001).
002443     05  LPFKI PIC  9(2).
002444 01  EL142D2O REDEFINES EL142BI.
002445     05  FILLER            PIC  X(0012).
002446*    -------------------------------
002447     05  FILLER            PIC  X(0003).
002448     05  LDATEO PIC  X(0008).
002449*    -------------------------------
002450     05  FILLER            PIC  X(0003).
002451     05  LTIMEO PIC  99.99.
002452*    -------------------------------
002453     05  FILLER            PIC  X(0003).
002454     05  LARCHNOO PIC  99999999.
002455*    -------------------------------
002456     05  FILLER            PIC  X(0003).
002457     05  LNXTDUEO PIC  X(0008).
002458*    -------------------------------
002459     05  FILLER            PIC  X(0003).
002460     05  LCARRO PIC  X(0001).
002461*    -------------------------------
002462     05  FILLER            PIC  X(0003).
002463     05  LAUTOPYO PIC  X(0008).
002464*    -------------------------------
002465     05  FILLER            PIC  X(0003).
002466     05  LCLMNOO PIC  X(0007).
002467*    -------------------------------
002468     05  FILLER            PIC  X(0003).
002469     05  LCREATNO PIC  X(0001).
002470*    -------------------------------
002471     05  FILLER            PIC  X(0003).
002472     05  LCRTNOO PIC  X(0011).
002473*    -------------------------------
002474     05  FILLER            PIC  X(0003).
002475     05  LORIGARO PIC  99999999.
002476*    -------------------------------
002477     05  FILLER            PIC  X(0003).
002478     05  LLETRIDO PIC  X(0004).
002479*    -------------------------------
002480     05  FILLER            PIC  X(0003).
002481     05  LPROMPTO PIC  X(0001).
002482*    -------------------------------
002483     05  FILLER            PIC  X(0003).
002484     05  LPROCO PIC  X(0004).
002485*    -------------------------------
002486     05  FILLER            PIC  X(0003).
002487     05  LORIGENO PIC  X(0001).
002488*    -------------------------------
002489     05  FILLER            PIC  X(0003).
002490     05  LCREDTEO PIC  X(0008).
002491*    -------------------------------
002492     05  FILLER            PIC  X(0003).
002493     05  LNOCPYSO PIC  999.
002494*    -------------------------------
002495     05  FILLER            PIC  X(0003).
002496     05  LINPRNTO PIC  X(0008).
002497*    -------------------------------
002498     05  FILLER            PIC  X(0003).
002499     05  LENCCODO PIC  X(0003).
002500*    -------------------------------
002501     05  FILLER            PIC  X(0003).
002502     05  LFUPDTEO PIC  X(0008).
002503*    -------------------------------
002504     05  FILLER            PIC  X(0003).
002505     05  LRESDTEO PIC  X(0008).
002506*    -------------------------------
002507     05  FILLER            PIC  X(0003).
002508     05  LRSLTIDO PIC  X(0004).
002509*    -------------------------------
002510     05  FILLER            PIC  X(0003).
002511     05  LREPRNTO PIC  X(0008).
002512*    -------------------------------
002513     05  FILLER            PIC  X(0003).
002514     05  LADDTYPO PIC  X(0001).
002515*    -------------------------------
002516     05  FILLER            PIC  X(0003).
002517     05  LCORSEQO PIC  999999.
002518*    -------------------------------
002519     05  FILLER            PIC  X(0003).
002520     05  L1STPRTO PIC  X(0008).
002521*    -------------------------------
002522     05  FILLER            PIC  X(0003).
002523     05  LEMSG1O PIC  X(0079).
002524*    -------------------------------
002525     05  FILLER            PIC  X(0003).
002526     05  LEMSG2O PIC  X(0079).
002527*    -------------------------------
002528     05  FILLER            PIC  X(0003).
002529     05  LEMSG3O PIC  X(0079).
002530*    -------------------------------
002531     05  FILLER            PIC  X(0003).
002532     05  LPFKO PIC  99.
002533*    -------------------------------
002534 01  EL142EI REDEFINES EL142BI.
002535     05  FILLER            PIC  X(0012).
002536*    -------------------------------
002537     05  EDATEL PIC S9(0004) COMP.
002538     05  EDATEF PIC  X(0001).
002539     05  FILLER REDEFINES EDATEF.
002540         10  EDATEA PIC  X(0001).
002541     05  EDATEI PIC  X(0008).
002542*    -------------------------------
002543     05  ETIMEL PIC S9(0004) COMP.
002544     05  ETIMEF PIC  X(0001).
002545     05  FILLER REDEFINES ETIMEF.
002546         10  ETIMEA PIC  X(0001).
002547     05  ETIMEI PIC  X(0005).
002548*    -------------------------------
002549     05  ETYPEL PIC S9(0004) COMP.
002550     05  ETYPEF PIC  X(0001).
002551     05  FILLER REDEFINES ETYPEF.
002552         10  ETYPEA PIC  X(0001).
002553     05  ETYPEI PIC  X(0010).
002554*    -------------------------------
002555     05  ERECDTEL PIC S9(0004) COMP.
002556     05  ERECDTEF PIC  X(0001).
002557     05  FILLER REDEFINES ERECDTEF.
002558         10  ERECDTEA PIC  X(0001).
002559     05  ERECDTEI PIC  X(0008).
002560*    -------------------------------
002561     05  EBYL PIC S9(0004) COMP.
002562     05  EBYF PIC  X(0001).
002563     05  FILLER REDEFINES EBYF.
002564         10  EBYA PIC  X(0001).
002565     05  EBYI PIC  X(0004).
002566*    -------------------------------
002567     05  ETLRTYPL PIC S9(0004) COMP.
002568     05  ETLRTYPF PIC  X(0001).
002569     05  FILLER REDEFINES ETLRTYPF.
002570         10  ETLRTYPA PIC  X(0001).
002571     05  ETLRTYPI PIC  X(0001).
002572*    -------------------------------
002573     05  ESEQL PIC S9(0004) COMP.
002574     05  ESEQF PIC  X(0001).
002575     05  FILLER REDEFINES ESEQF.
002576         10  ESEQA PIC  X(0001).
002577     05  ESEQI PIC  X(0004).
002578*    -------------------------------
002579     05  EMANTBYL PIC S9(0004) COMP.
002580     05  EMANTBYF PIC  X(0001).
002581     05  FILLER REDEFINES EMANTBYF.
002582         10  EMANTBYA PIC  X(0001).
002583     05  EMANTBYI PIC  X(0004).
002584*    -------------------------------
002585     05  EMANTONL PIC S9(0004) COMP.
002586     05  EMANTONF PIC  X(0001).
002587     05  FILLER REDEFINES EMANTONF.
002588         10  EMANTONA PIC  X(0001).
002589     05  EMANTONI PIC  X(0008).
002590*    -------------------------------
002591     05  EMANTATL PIC S9(0004) COMP.
002592     05  EMANTATF PIC  X(0001).
002593     05  FILLER REDEFINES EMANTATF.
002594         10  EMANTATA PIC  X(0001).
002595     05  EMANTATI PIC  X(0005).
002596*    -------------------------------
002597     05  EMAINTL PIC S9(0004) COMP.
002598     05  EMAINTF PIC  X(0001).
002599     05  FILLER REDEFINES EMAINTF.
002600         10  EMAINTA PIC  X(0001).
002601     05  EMAINTI PIC  X(0001).
002602*    -------------------------------
002603     05  ETYPENL PIC S9(0004) COMP.
002604     05  ETYPENF PIC  X(0001).
002605     05  FILLER REDEFINES ETYPENF.
002606         10  ETYPENA PIC  X(0001).
002607     05  ETYPENI PIC  X(0001).
002608*    -------------------------------
002609     05  ECALLTL PIC S9(0004) COMP.
002610     05  ECALLTF PIC  X(0001).
002611     05  FILLER REDEFINES ECALLTF.
002612         10  ECALLTA PIC  X(0001).
002613     05  ECALLTI PIC  X(0010).
002614*    -------------------------------
002615     05  ECALLL PIC S9(0004) COMP.
002616     05  ECALLF PIC  X(0001).
002617     05  FILLER REDEFINES ECALLF.
002618         10  ECALLA PIC  X(0001).
002619     05  ECALLI PIC  X(0001).
002620*    -------------------------------
002621     05  ELINE1L PIC S9(0004) COMP.
002622     05  ELINE1F PIC  X(0001).
002623     05  FILLER REDEFINES ELINE1F.
002624         10  ELINE1A PIC  X(0001).
002625     05  ELINE1I PIC  X(0070).
002626*    -------------------------------
002627     05  ELINE2L PIC S9(0004) COMP.
002628     05  ELINE2F PIC  X(0001).
002629     05  FILLER REDEFINES ELINE2F.
002630         10  ELINE2A PIC  X(0001).
002631     05  ELINE2I PIC  X(0070).
002632*    -------------------------------
002633     05  EEMSG1L PIC S9(0004) COMP.
002634     05  EEMSG1F PIC  X(0001).
002635     05  FILLER REDEFINES EEMSG1F.
002636         10  EEMSG1A PIC  X(0001).
002637     05  EEMSG1I PIC  X(0079).
002638*    -------------------------------
002639     05  EEMSG2L PIC S9(0004) COMP.
002640     05  EEMSG2F PIC  X(0001).
002641     05  FILLER REDEFINES EEMSG2F.
002642         10  EEMSG2A PIC  X(0001).
002643     05  EEMSG2I PIC  X(0079).
002644*    -------------------------------
002645     05  EPFKL PIC S9(0004) COMP.
002646     05  EPFKF PIC  X(0001).
002647     05  FILLER REDEFINES EPFKF.
002648         10  EPFKA PIC  X(0001).
002649     05  EPFKI PIC  9(2).
002650 01  EL142EO REDEFINES EL142BI.
002651     05  FILLER            PIC  X(0012).
002652*    -------------------------------
002653     05  FILLER            PIC  X(0003).
002654     05  EDATEO PIC  X(0008).
002655*    -------------------------------
002656     05  FILLER            PIC  X(0003).
002657     05  ETIMEO PIC  99.99.
002658*    -------------------------------
002659     05  FILLER            PIC  X(0003).
002660     05  ETYPEO PIC  X(0010).
002661*    -------------------------------
002662     05  FILLER            PIC  X(0003).
002663     05  ERECDTEO PIC  X(0008).
002664*    -------------------------------
002665     05  FILLER            PIC  X(0003).
002666     05  EBYO PIC  X(0004).
002667*    -------------------------------
002668     05  FILLER            PIC  X(0003).
002669     05  ETLRTYPO PIC  X(0001).
002670*    -------------------------------
002671     05  FILLER            PIC  X(0003).
002672     05  ESEQO PIC  9999.
002673*    -------------------------------
002674     05  FILLER            PIC  X(0003).
002675     05  EMANTBYO PIC  X(0004).
002676*    -------------------------------
002677     05  FILLER            PIC  X(0003).
002678     05  EMANTONO PIC  X(0008).
002679*    -------------------------------
002680     05  FILLER            PIC  X(0003).
002681     05  EMANTATO PIC  99.99.
002682*    -------------------------------
002683     05  FILLER            PIC  X(0003).
002684     05  EMAINTO PIC  X(0001).
002685*    -------------------------------
002686     05  FILLER            PIC  X(0003).
002687     05  ETYPENO PIC  X(0001).
002688*    -------------------------------
002689     05  FILLER            PIC  X(0003).
002690     05  ECALLTO PIC  X(0010).
002691*    -------------------------------
002692     05  FILLER            PIC  X(0003).
002693     05  ECALLO PIC  X(0001).
002694*    -------------------------------
002695     05  FILLER            PIC  X(0003).
002696     05  ELINE1O PIC  X(0070).
002697*    -------------------------------
002698     05  FILLER            PIC  X(0003).
002699     05  ELINE2O PIC  X(0070).
002700*    -------------------------------
002701     05  FILLER            PIC  X(0003).
002702     05  EEMSG1O PIC  X(0079).
002703*    -------------------------------
002704     05  FILLER            PIC  X(0003).
002705     05  EEMSG2O PIC  X(0079).
002706*    -------------------------------
002707     05  FILLER            PIC  X(0003).
002708     05  EPFKO PIC  99.
002709*    -------------------------------
002710 01  EL142FI REDEFINES EL142BI.
002711     05  FILLER            PIC  X(0012).
002712*    -------------------------------
002713     05  FDATEL PIC S9(0004) COMP.
002714     05  FDATEF PIC  X(0001).
002715     05  FILLER REDEFINES FDATEF.
002716         10  FDATEA PIC  X(0001).
002717     05  FDATEI PIC  X(0008).
002718*    -------------------------------
002719     05  FTIMEL PIC S9(0004) COMP.
002720     05  FTIMEF PIC  X(0001).
002721     05  FILLER REDEFINES FTIMEF.
002722         10  FTIMEA PIC  X(0001).
002723     05  FTIMEI PIC  X(0005).
002724*    -------------------------------
002725     05  FTYPEL PIC S9(0004) COMP.
002726     05  FTYPEF PIC  X(0001).
002727     05  FILLER REDEFINES FTYPEF.
002728         10  FTYPEA PIC  X(0001).
002729     05  FTYPEI PIC  X(0010).
002730*    -------------------------------
002731     05  FRECDTEL PIC S9(0004) COMP.
002732     05  FRECDTEF PIC  X(0001).
002733     05  FILLER REDEFINES FRECDTEF.
002734         10  FRECDTEA PIC  X(0001).
002735     05  FRECDTEI PIC  X(0008).
002736*    -------------------------------
002737     05  FBYL PIC S9(0004) COMP.
002738     05  FBYF PIC  X(0001).
002739     05  FILLER REDEFINES FBYF.
002740         10  FBYA PIC  X(0001).
002741     05  FBYI PIC  X(0004).
002742*    -------------------------------
002743     05  FTLRTYPL PIC S9(0004) COMP.
002744     05  FTLRTYPF PIC  X(0001).
002745     05  FILLER REDEFINES FTLRTYPF.
002746         10  FTLRTYPA PIC  X(0001).
002747     05  FTLRTYPI PIC  X(0001).
002748*    -------------------------------
002749     05  FSEQL PIC S9(0004) COMP.
002750     05  FSEQF PIC  X(0001).
002751     05  FILLER REDEFINES FSEQF.
002752         10  FSEQA PIC  X(0001).
002753     05  FSEQI PIC  X(0004).
002754*    -------------------------------
002755     05  FMANTBYL PIC S9(0004) COMP.
002756     05  FMANTBYF PIC  X(0001).
002757     05  FILLER REDEFINES FMANTBYF.
002758         10  FMANTBYA PIC  X(0001).
002759     05  FMANTBYI PIC  X(0004).
002760*    -------------------------------
002761     05  FMANTONL PIC S9(0004) COMP.
002762     05  FMANTONF PIC  X(0001).
002763     05  FILLER REDEFINES FMANTONF.
002764         10  FMANTONA PIC  X(0001).
002765     05  FMANTONI PIC  X(0008).
002766*    -------------------------------
002767     05  FMANTATL PIC S9(0004) COMP.
002768     05  FMANTATF PIC  X(0001).
002769     05  FILLER REDEFINES FMANTATF.
002770         10  FMANTATA PIC  X(0001).
002771     05  FMANTATI PIC  X(0005).
002772*    -------------------------------
002773     05  FMAINTL PIC S9(0004) COMP.
002774     05  FMAINTF PIC  X(0001).
002775     05  FILLER REDEFINES FMAINTF.
002776         10  FMAINTA PIC  X(0001).
002777     05  FMAINTI PIC  X(0001).
002778*    -------------------------------
002779     05  FSNOTIFL PIC S9(0004) COMP.
002780     05  FSNOTIFF PIC  X(0001).
002781     05  FILLER REDEFINES FSNOTIFF.
002782         10  FSNOTIFA PIC  X(0001).
002783     05  FSNOTIFI PIC  X(0008).
002784*    -------------------------------
002785     05  FENOTIFL PIC S9(0004) COMP.
002786     05  FENOTIFF PIC  X(0001).
002787     05  FILLER REDEFINES FENOTIFF.
002788         10  FENOTIFA PIC  X(0001).
002789     05  FENOTIFI PIC  X(0008).
002790*    -------------------------------
002791     05  FLINE1L PIC S9(0004) COMP.
002792     05  FLINE1F PIC  X(0001).
002793     05  FILLER REDEFINES FLINE1F.
002794         10  FLINE1A PIC  X(0001).
002795     05  FLINE1I PIC  X(0070).
002796*    -------------------------------
002797     05  FLINE2L PIC S9(0004) COMP.
002798     05  FLINE2F PIC  X(0001).
002799     05  FILLER REDEFINES FLINE2F.
002800         10  FLINE2A PIC  X(0001).
002801     05  FLINE2I PIC  X(0070).
002802*    -------------------------------
002803     05  FEMSG1L PIC S9(0004) COMP.
002804     05  FEMSG1F PIC  X(0001).
002805     05  FILLER REDEFINES FEMSG1F.
002806         10  FEMSG1A PIC  X(0001).
002807     05  FEMSG1I PIC  X(0079).
002808*    -------------------------------
002809     05  FEMSG2L PIC S9(0004) COMP.
002810     05  FEMSG2F PIC  X(0001).
002811     05  FILLER REDEFINES FEMSG2F.
002812         10  FEMSG2A PIC  X(0001).
002813     05  FEMSG2I PIC  X(0079).
002814*    -------------------------------
002815     05  FPFKL PIC S9(0004) COMP.
002816     05  FPFKF PIC  X(0001).
002817     05  FILLER REDEFINES FPFKF.
002818         10  FPFKA PIC  X(0001).
002819     05  FPFKI PIC  9(2).
002820 01  EL142FO REDEFINES EL142BI.
002821     05  FILLER            PIC  X(0012).
002822*    -------------------------------
002823     05  FILLER            PIC  X(0003).
002824     05  FDATEO PIC  X(0008).
002825*    -------------------------------
002826     05  FILLER            PIC  X(0003).
002827     05  FTIMEO PIC  99.99.
002828*    -------------------------------
002829     05  FILLER            PIC  X(0003).
002830     05  FTYPEO PIC  X(0010).
002831*    -------------------------------
002832     05  FILLER            PIC  X(0003).
002833     05  FRECDTEO PIC  X(0008).
002834*    -------------------------------
002835     05  FILLER            PIC  X(0003).
002836     05  FBYO PIC  X(0004).
002837*    -------------------------------
002838     05  FILLER            PIC  X(0003).
002839     05  FTLRTYPO PIC  X(0001).
002840*    -------------------------------
002841     05  FILLER            PIC  X(0003).
002842     05  FSEQO PIC  9999.
002843*    -------------------------------
002844     05  FILLER            PIC  X(0003).
002845     05  FMANTBYO PIC  X(0004).
002846*    -------------------------------
002847     05  FILLER            PIC  X(0003).
002848     05  FMANTONO PIC  X(0008).
002849*    -------------------------------
002850     05  FILLER            PIC  X(0003).
002851     05  FMANTATO PIC  99.99.
002852*    -------------------------------
002853     05  FILLER            PIC  X(0003).
002854     05  FMAINTO PIC  X(0001).
002855*    -------------------------------
002856     05  FILLER            PIC  X(0003).
002857     05  FSNOTIFO PIC  99B99B99.
002858*    -------------------------------
002859     05  FILLER            PIC  X(0003).
002860     05  FENOTIFO PIC  99B99B99.
002861*    -------------------------------
002862     05  FILLER            PIC  X(0003).
002863     05  FLINE1O PIC  X(0070).
002864*    -------------------------------
002865     05  FILLER            PIC  X(0003).
002866     05  FLINE2O PIC  X(0070).
002867*    -------------------------------
002868     05  FILLER            PIC  X(0003).
002869     05  FEMSG1O PIC  X(0079).
002870*    -------------------------------
002871     05  FILLER            PIC  X(0003).
002872     05  FEMSG2O PIC  X(0079).
002873*    -------------------------------
002874     05  FILLER            PIC  X(0003).
002875     05  FPFKO PIC  99.
002876*    -------------------------------
002877 01  EL142GI REDEFINES EL142BI.
002878     05  FILLER            PIC  X(0012).
002879*    -------------------------------
002880     05  GDATEL PIC S9(0004) COMP.
002881     05  GDATEF PIC  X(0001).
002882     05  FILLER REDEFINES GDATEF.
002883         10  GDATEA PIC  X(0001).
002884     05  GDATEI PIC  X(0008).
002885*    -------------------------------
002886     05  GTIMEL PIC S9(0004) COMP.
002887     05  GTIMEF PIC  X(0001).
002888     05  FILLER REDEFINES GTIMEF.
002889         10  GTIMEA PIC  X(0001).
002890     05  GTIMEI PIC  X(0005).
002891*    -------------------------------
002892     05  GTYPEL PIC S9(0004) COMP.
002893     05  GTYPEF PIC  X(0001).
002894     05  FILLER REDEFINES GTYPEF.
002895         10  GTYPEA PIC  X(0001).
002896     05  GTYPEI PIC  X(0010).
002897*    -------------------------------
002898     05  GRECDTEL PIC S9(0004) COMP.
002899     05  GRECDTEF PIC  X(0001).
002900     05  FILLER REDEFINES GRECDTEF.
002901         10  GRECDTEA PIC  X(0001).
002902     05  GRECDTEI PIC  X(0008).
002903*    -------------------------------
002904     05  GBYL PIC S9(0004) COMP.
002905     05  GBYF PIC  X(0001).
002906     05  FILLER REDEFINES GBYF.
002907         10  GBYA PIC  X(0001).
002908     05  GBYI PIC  X(0004).
002909*    -------------------------------
002910     05  GTLRTYPL PIC S9(0004) COMP.
002911     05  GTLRTYPF PIC  X(0001).
002912     05  FILLER REDEFINES GTLRTYPF.
002913         10  GTLRTYPA PIC  X(0001).
002914     05  GTLRTYPI PIC  X(0001).
002915*    -------------------------------
002916     05  GSEQL PIC S9(0004) COMP.
002917     05  GSEQF PIC  X(0001).
002918     05  FILLER REDEFINES GSEQF.
002919         10  GSEQA PIC  X(0001).
002920     05  GSEQI PIC  X(0004).
002921*    -------------------------------
002922     05  GMANTBYL PIC S9(0004) COMP.
002923     05  GMANTBYF PIC  X(0001).
002924     05  FILLER REDEFINES GMANTBYF.
002925         10  GMANTBYA PIC  X(0001).
002926     05  GMANTBYI PIC  X(0004).
002927*    -------------------------------
002928     05  GMANTONL PIC S9(0004) COMP.
002929     05  GMANTONF PIC  X(0001).
002930     05  FILLER REDEFINES GMANTONF.
002931         10  GMANTONA PIC  X(0001).
002932     05  GMANTONI PIC  X(0008).
002933*    -------------------------------
002934     05  GMANTATL PIC S9(0004) COMP.
002935     05  GMANTATF PIC  X(0001).
002936     05  FILLER REDEFINES GMANTATF.
002937         10  GMANTATA PIC  X(0001).
002938     05  GMANTATI PIC  X(0005).
002939*    -------------------------------
002940     05  GMAINTL PIC S9(0004) COMP.
002941     05  GMAINTF PIC  X(0001).
002942     05  FILLER REDEFINES GMAINTF.
002943         10  GMAINTA PIC  X(0001).
002944     05  GMAINTI PIC  X(0001).
002945*    -------------------------------
002946     05  GLINE1L PIC S9(0004) COMP.
002947     05  GLINE1F PIC  X(0001).
002948     05  FILLER REDEFINES GLINE1F.
002949         10  GLINE1A PIC  X(0001).
002950     05  GLINE1I PIC  X(0070).
002951*    -------------------------------
002952     05  GLINE2L PIC S9(0004) COMP.
002953     05  GLINE2F PIC  X(0001).
002954     05  FILLER REDEFINES GLINE2F.
002955         10  GLINE2A PIC  X(0001).
002956     05  GLINE2I PIC  X(0070).
002957*    -------------------------------
002958     05  GRECONSL PIC S9(0004) COMP.
002959     05  GRECONSF PIC  X(0001).
002960     05  FILLER REDEFINES GRECONSF.
002961         10  GRECONSA PIC  X(0001).
002962     05  GRECONSI PIC  X(0008).
002963*    -------------------------------
002964     05  GRSNCDL PIC S9(0004) COMP.
002965     05  GRSNCDF PIC  X(0001).
002966     05  FILLER REDEFINES GRSNCDF.
002967         10  GRSNCDA PIC  X(0001).
002968     05  GRSNCDI PIC  X(0004).
002969*    -------------------------------
002970     05  GPRFDTL PIC S9(0004) COMP.
002971     05  GPRFDTF PIC  X(0001).
002972     05  FILLER REDEFINES GPRFDTF.
002973         10  GPRFDTA PIC  X(0001).
002974     05  GPRFDTI PIC  X(0008).
002975*    -------------------------------
002976     05  GEMSG1L PIC S9(0004) COMP.
002977     05  GEMSG1F PIC  X(0001).
002978     05  FILLER REDEFINES GEMSG1F.
002979         10  GEMSG1A PIC  X(0001).
002980     05  GEMSG1I PIC  X(0079).
002981*    -------------------------------
002982     05  GEMSG2L PIC S9(0004) COMP.
002983     05  GEMSG2F PIC  X(0001).
002984     05  FILLER REDEFINES GEMSG2F.
002985         10  GEMSG2A PIC  X(0001).
002986     05  GEMSG2I PIC  X(0079).
002987*    -------------------------------
002988     05  GPFKL PIC S9(0004) COMP.
002989     05  GPFKF PIC  X(0001).
002990     05  FILLER REDEFINES GPFKF.
002991         10  GPFKA PIC  X(0001).
002992     05  GPFKI PIC  9(2).
002993 01  EL142GO REDEFINES EL142BI.
002994     05  FILLER            PIC  X(0012).
002995*    -------------------------------
002996     05  FILLER            PIC  X(0003).
002997     05  GDATEO PIC  X(0008).
002998*    -------------------------------
002999     05  FILLER            PIC  X(0003).
003000     05  GTIMEO PIC  99.99.
003001*    -------------------------------
003002     05  FILLER            PIC  X(0003).
003003     05  GTYPEO PIC  X(0010).
003004*    -------------------------------
003005     05  FILLER            PIC  X(0003).
003006     05  GRECDTEO PIC  X(0008).
003007*    -------------------------------
003008     05  FILLER            PIC  X(0003).
003009     05  GBYO PIC  X(0004).
003010*    -------------------------------
003011     05  FILLER            PIC  X(0003).
003012     05  GTLRTYPO PIC  X(0001).
003013*    -------------------------------
003014     05  FILLER            PIC  X(0003).
003015     05  GSEQO PIC  9999.
003016*    -------------------------------
003017     05  FILLER            PIC  X(0003).
003018     05  GMANTBYO PIC  X(0004).
003019*    -------------------------------
003020     05  FILLER            PIC  X(0003).
003021     05  GMANTONO PIC  X(0008).
003022*    -------------------------------
003023     05  FILLER            PIC  X(0003).
003024     05  GMANTATO PIC  99.99.
003025*    -------------------------------
003026     05  FILLER            PIC  X(0003).
003027     05  GMAINTO PIC  X(0001).
003028*    -------------------------------
003029     05  FILLER            PIC  X(0003).
003030     05  GLINE1O PIC  X(0070).
003031*    -------------------------------
003032     05  FILLER            PIC  X(0003).
003033     05  GLINE2O PIC  X(0070).
003034*    -------------------------------
003035     05  FILLER            PIC  X(0003).
003036     05  GRECONSO PIC  99B99B99.
003037*    -------------------------------
003038     05  FILLER            PIC  X(0003).
003039     05  GRSNCDO PIC  X(0004).
003040*    -------------------------------
003041     05  FILLER            PIC  X(0003).
003042     05  GPRFDTO PIC  99B99B99.
003043*    -------------------------------
003044     05  FILLER            PIC  X(0003).
003045     05  GEMSG1O PIC  X(0079).
003046*    -------------------------------
003047     05  FILLER            PIC  X(0003).
003048     05  GEMSG2O PIC  X(0079).
003049*    -------------------------------
003050     05  FILLER            PIC  X(0003).
003051     05  GPFKO PIC  99.
003052*    -------------------------------
003053 01  EL142II REDEFINES EL142BI.
003054     05  FILLER            PIC  X(0012).
003055*    -------------------------------
003056     05  IDATEL PIC S9(0004) COMP.
003057     05  IDATEF PIC  X(0001).
003058     05  FILLER REDEFINES IDATEF.
003059         10  IDATEA PIC  X(0001).
003060     05  IDATEI PIC  X(0008).
003061*    -------------------------------
003062     05  ITIMEL PIC S9(0004) COMP.
003063     05  ITIMEF PIC  X(0001).
003064     05  FILLER REDEFINES ITIMEF.
003065         10  ITIMEA PIC  X(0001).
003066     05  ITIMEI PIC  X(0005).
003067*    -------------------------------
003068     05  ITYPEL PIC S9(0004) COMP.
003069     05  ITYPEF PIC  X(0001).
003070     05  FILLER REDEFINES ITYPEF.
003071         10  ITYPEA PIC  X(0001).
003072     05  ITYPEI PIC  X(0010).
003073*    -------------------------------
003074     05  IRECDTEL PIC S9(0004) COMP.
003075     05  IRECDTEF PIC  X(0001).
003076     05  FILLER REDEFINES IRECDTEF.
003077         10  IRECDTEA PIC  X(0001).
003078     05  IRECDTEI PIC  X(0008).
003079*    -------------------------------
003080     05  IBYL PIC S9(0004) COMP.
003081     05  IBYF PIC  X(0001).
003082     05  FILLER REDEFINES IBYF.
003083         10  IBYA PIC  X(0001).
003084     05  IBYI PIC  X(0004).
003085*    -------------------------------
003086     05  ITLRTYPL PIC S9(0004) COMP.
003087     05  ITLRTYPF PIC  X(0001).
003088     05  FILLER REDEFINES ITLRTYPF.
003089         10  ITLRTYPA PIC  X(0001).
003090     05  ITLRTYPI PIC  X(0001).
003091*    -------------------------------
003092     05  ISEQL PIC S9(0004) COMP.
003093     05  ISEQF PIC  X(0001).
003094     05  FILLER REDEFINES ISEQF.
003095         10  ISEQA PIC  X(0001).
003096     05  ISEQI PIC  X(0004).
003097*    -------------------------------
003098     05  IINCDTL PIC S9(0004) COMP.
003099     05  IINCDTF PIC  X(0001).
003100     05  FILLER REDEFINES IINCDTF.
003101         10  IINCDTA PIC  X(0001).
003102     05  IINCDTI PIC  X(0008).
003103*    -------------------------------
003104     05  ITAPDL PIC S9(0004) COMP.
003105     05  ITAPDF PIC  X(0001).
003106     05  FILLER REDEFINES ITAPDF.
003107         10  ITAPDA PIC  X(0001).
003108     05  ITAPDI PIC  X(0013).
003109*    -------------------------------
003110     05  IREPDTL PIC S9(0004) COMP.
003111     05  IREPDTF PIC  X(0001).
003112     05  FILLER REDEFINES IREPDTF.
003113         10  IREPDTA PIC  X(0001).
003114     05  IREPDTI PIC  X(0008).
003115*    -------------------------------
003116     05  ITDPDL PIC S9(0004) COMP.
003117     05  ITDPDF PIC  X(0001).
003118     05  FILLER REDEFINES ITDPDF.
003119         10  ITDPDA PIC  X(0001).
003120     05  ITDPDI PIC  X(0006).
003121*    -------------------------------
003122     05  IESTDTL PIC S9(0004) COMP.
003123     05  IESTDTF PIC  X(0001).
003124     05  FILLER REDEFINES IESTDTF.
003125         10  IESTDTA PIC  X(0001).
003126     05  IESTDTI PIC  X(0008).
003127*    -------------------------------
003128     05  INOPMTL PIC S9(0004) COMP.
003129     05  INOPMTF PIC  X(0001).
003130     05  FILLER REDEFINES INOPMTF.
003131         10  INOPMTA PIC  X(0001).
003132     05  INOPMTI PIC  X(0006).
003133*    -------------------------------
003134     05  ITHRUHDL PIC S9(0004) COMP.
003135     05  ITHRUHDF PIC  X(0001).
003136     05  FILLER REDEFINES ITHRUHDF.
003137         10  ITHRUHDA PIC  X(0001).
003138     05  ITHRUHDI PIC  X(0014).
003139*    -------------------------------
003140     05  IPDTHRUL PIC S9(0004) COMP.
003141     05  IPDTHRUF PIC  X(0001).
003142     05  FILLER REDEFINES IPDTHRUF.
003143         10  IPDTHRUA PIC  X(0001).
003144     05  IPDTHRUI PIC  X(0008).
003145*    -------------------------------
003146     05  ITLRCNTL PIC S9(0004) COMP.
003147     05  ITLRCNTF PIC  X(0001).
003148     05  FILLER REDEFINES ITLRCNTF.
003149         10  ITLRCNTA PIC  X(0001).
003150     05  ITLRCNTI PIC  X(0004).
003151*    -------------------------------
003152     05  ILSTPDTL PIC S9(0004) COMP.
003153     05  ILSTPDTF PIC  X(0001).
003154     05  FILLER REDEFINES ILSTPDTF.
003155         10  ILSTPDTA PIC  X(0001).
003156     05  ILSTPDTI PIC  X(0008).
003157*    -------------------------------
003158     05  IMANRESL PIC S9(0004) COMP.
003159     05  IMANRESF PIC  X(0001).
003160     05  FILLER REDEFINES IMANRESF.
003161         10  IMANRESA PIC  X(0001).
003162     05  IMANRESI PIC  X(0013).
003163*    -------------------------------
003164     05  ITEXPDL PIC S9(0004) COMP.
003165     05  ITEXPDF PIC  X(0001).
003166     05  FILLER REDEFINES ITEXPDF.
003167         10  ITEXPDA PIC  X(0001).
003168     05  ITEXPDI PIC  X(0010).
003169*    -------------------------------
003170     05  ICURRESL PIC S9(0004) COMP.
003171     05  ICURRESF PIC  X(0001).
003172     05  FILLER REDEFINES ICURRESF.
003173         10  ICURRESA PIC  X(0001).
003174     05  ICURRESI PIC  X(0013).
003175*    -------------------------------
003176     05  ICHGEXPL PIC S9(0004) COMP.
003177     05  ICHGEXPF PIC  X(0001).
003178     05  FILLER REDEFINES ICHGEXPF.
003179         10  ICHGEXPA PIC  X(0001).
003180     05  ICHGEXPI PIC  X(0010).
003181*    -------------------------------
003182     05  IADDRESL PIC S9(0004) COMP.
003183     05  IADDRESF PIC  X(0001).
003184     05  FILLER REDEFINES IADDRESF.
003185         10  IADDRESA PIC  X(0001).
003186     05  IADDRESI PIC  X(0013).
003187*    -------------------------------
003188     05  ICAUSCDL PIC S9(0004) COMP.
003189     05  ICAUSCDF PIC  X(0001).
003190     05  FILLER REDEFINES ICAUSCDF.
003191         10  ICAUSCDA PIC  X(0001).
003192     05  ICAUSCDI PIC  X(0006).
003193*    -------------------------------
003194     05  IDIAGL PIC S9(0004) COMP.
003195     05  IDIAGF PIC  X(0001).
003196     05  FILLER REDEFINES IDIAGF.
003197         10  IDIAGA PIC  X(0001).
003198     05  IDIAGI PIC  X(0060).
003199*    -------------------------------
003200     05  IEMSG1L PIC S9(0004) COMP.
003201     05  IEMSG1F PIC  X(0001).
003202     05  FILLER REDEFINES IEMSG1F.
003203         10  IEMSG1A PIC  X(0001).
003204     05  IEMSG1I PIC  X(0079).
003205*    -------------------------------
003206     05  IPFKL PIC S9(0004) COMP.
003207     05  IPFKF PIC  X(0001).
003208     05  FILLER REDEFINES IPFKF.
003209         10  IPFKA PIC  X(0001).
003210     05  IPFKI PIC  9(2).
003211 01  EL142IO REDEFINES EL142BI.
003212     05  FILLER            PIC  X(0012).
003213*    -------------------------------
003214     05  FILLER            PIC  X(0003).
003215     05  IDATEO PIC  X(0008).
003216*    -------------------------------
003217     05  FILLER            PIC  X(0003).
003218     05  ITIMEO PIC  99.99.
003219*    -------------------------------
003220     05  FILLER            PIC  X(0003).
003221     05  ITYPEO PIC  X(0010).
003222*    -------------------------------
003223     05  FILLER            PIC  X(0003).
003224     05  IRECDTEO PIC  X(0008).
003225*    -------------------------------
003226     05  FILLER            PIC  X(0003).
003227     05  IBYO PIC  X(0004).
003228*    -------------------------------
003229     05  FILLER            PIC  X(0003).
003230     05  ITLRTYPO PIC  X(0001).
003231*    -------------------------------
003232     05  FILLER            PIC  X(0003).
003233     05  ISEQO PIC  9999.
003234*    -------------------------------
003235     05  FILLER            PIC  X(0003).
003236     05  IINCDTO PIC  X(0008).
003237*    -------------------------------
003238     05  FILLER            PIC  X(0003).
003239     05  ITAPDO PIC  Z,ZZZ,ZZ9.99-.
003240*    -------------------------------
003241     05  FILLER            PIC  X(0003).
003242     05  IREPDTO PIC  X(0008).
003243*    -------------------------------
003244     05  FILLER            PIC  X(0003).
003245     05  ITDPDO PIC  Z,ZZ9-.
003246*    -------------------------------
003247     05  FILLER            PIC  X(0003).
003248     05  IESTDTO PIC  X(0008).
003249*    -------------------------------
003250     05  FILLER            PIC  X(0003).
003251     05  INOPMTO PIC  Z,ZZ9-.
003252*    -------------------------------
003253     05  FILLER            PIC  X(0003).
003254     05  ITHRUHDO PIC  X(0014).
003255*    -------------------------------
003256     05  FILLER            PIC  X(0003).
003257     05  IPDTHRUO PIC  X(0008).
003258*    -------------------------------
003259     05  FILLER            PIC  X(0003).
003260     05  ITLRCNTO PIC  9999.
003261*    -------------------------------
003262     05  FILLER            PIC  X(0003).
003263     05  ILSTPDTO PIC  X(0008).
003264*    -------------------------------
003265     05  FILLER            PIC  X(0003).
003266     05  IMANRESO PIC  Z,ZZZ,ZZ9.99-.
003267*    -------------------------------
003268     05  FILLER            PIC  X(0003).
003269     05  ITEXPDO PIC  ZZ,ZZ9.99-.
003270*    -------------------------------
003271     05  FILLER            PIC  X(0003).
003272     05  ICURRESO PIC  Z,ZZZ,ZZ9.99-.
003273*    -------------------------------
003274     05  FILLER            PIC  X(0003).
003275     05  ICHGEXPO PIC  ZZ,ZZ9.99-.
003276*    -------------------------------
003277     05  FILLER            PIC  X(0003).
003278     05  IADDRESO PIC  Z,ZZZ,ZZ9.99-.
003279*    -------------------------------
003280     05  FILLER            PIC  X(0003).
003281     05  ICAUSCDO PIC  X(0006).
003282*    -------------------------------
003283     05  FILLER            PIC  X(0003).
003284     05  IDIAGO PIC  X(0060).
003285*    -------------------------------
003286     05  FILLER            PIC  X(0003).
003287     05  IEMSG1O PIC  X(0079).
003288*    -------------------------------
003289     05  FILLER            PIC  X(0003).
003290     05  IPFKO PIC  99.
003291*    -------------------------------
      *<<((file: EL142S))
000716 01  FILLER REDEFINES EL142HI.
000717
000718     05  FILLER                      PIC X(259).
000719
000720     05  FILLER  OCCURS 5  INDEXED BY EL142H-INDEX1.
000721         10  EL142H-MAP-LINE  OCCURS 2 INDEXED BY EL142H-INDEX2.
000722
000723             15  EL142H-DATE-LENGTH  PIC S9(4)  COMP.
000724             15  EL142H-DATE-ATTRB   PIC X.
000725             15  EL142H-DATE         PIC X(8).
000726
000727             15  EL142H-OC-LENGTH    PIC S9(4)  COMP.
000728             15  EL142H-OC-ATTRB     PIC X.
000729             15  EL142H-OC           PIC X.
000730
000731             15  EL142H-CAUSE-LENGTH PIC S9(4)  COMP.
000732             15  EL142H-CAUSE-ATTRB  PIC X.
000733             15  EL142H-CAUSE        PIC X(5).
000734
000735     EJECT
000736*                                COPY ELCEMIB.
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
000737     EJECT
000738*                                COPY ELCDATE.
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
000739     EJECT
000740*                                COPY ELCLOGOF.
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
000741     EJECT
000742*                                COPY ELCATTR.
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
000743     EJECT
000744*                                COPY ELCAID.
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
000745 01  FILLER REDEFINES DFHAID.
000746     05  FILLER                      PIC X(8).
000747     05  PF-VALUES                   PIC X  OCCURS 24.
000748
000749     EJECT
000750
000751*    COPY ELCDMO.
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
000752
000753 01  sqlconnect-parms.
000754     05  p-sql-server            PIC X(30).
000755     05  p-sql-database          PIC X(30).
000756     05  p-connect-return-code   pic s9(5) comp-5.
000757     05  p-sql-return-message    pic x(256).
000758
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
000760
000761 01  DFHCOMMAREA                     PIC X(1024).
000762
000763*01 DFHBLLDS                         COMP SYNC.
000764*    05  BLLCBAR                     PIC S9(9).
000765*    05  ELTRLR-POINTER              PIC S9(9).
000766*    05  ELMSTR-POINTER              PIC S9(9).
000767*    05  ERACCT-POINTER              PIC S9(9).
000768*    05  ELARCH-POINTER              PIC S9(9).
000769*    05  ELCHKQ-POINTER              PIC S9(9).
000770*    05  ELCNTL-POINTER              PIC S9(9).
000771*    05  EMPROD-POINTER              PIC S9(9).
000772
000773     EJECT
000774*                                COPY ELCTRLR.
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
000775     EJECT
000776*                                COPY ELCMSTR.
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
000777     EJECT
000778*                                COPY ERCACCT.
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
000779     EJECT
000780*                                COPY ELCARCH.
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
000781     EJECT
000782*                                COPY ELCARCT.
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
000783     EJECT
000784*                                COPY ELCCHKQ.
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
000785     EJECT
000786*                                COPY ELCCNTL.
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
000787
000788*                                COPY ELCDENY.
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
000789
000790*                                COPY ELCNAPS.
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
000791
000792*                                COPY ELCENCC.
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
000793
000794*                                COPY MPCPROD.
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
000795     EJECT
000796*                                COPY ERCDMDNT.
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
000797     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL142' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000798 VCOBOL-DUMMY-PROCEDURE.
000799
000800     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000801
000802*    NOTE *******************************************************
000803*         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
000804*         *  FROM ANOTHER MODULE.                               *
000805*         *******************************************************.
000806
000807     MOVE PI-EL142-PRIORITY      TO WS-PI-EL142-PRIORITY.
000808
000809     IF EIBCALEN NOT GREATER ZERO
000810         MOVE UNACCESS-MSG       TO  LOGOFF-MSG
000811         GO TO 8300-SEND-TEXT.
000812
000813     
      * EXEC CICS HANDLE CONDITION
000814*        PGMIDERR (9600-PGMIDERR)
000815*        NOTFND   (0130-MAIN-LOGIC)
000816*        ENDFILE  (6000-END-OF-FILE)
000817*        ERROR    (9990-ERROR)
000818*    END-EXEC.
      *    MOVE '"$LI''.                ! " #00009144' TO DFHEIV0
           MOVE X'22244C49272E202020202020' &
                X'202020202020202020202120' &
                X'2220233030303039313434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000819
000820     MOVE EIBDATE               TO  DC-JULIAN-YYDDD.
000821     MOVE '5'                   TO  DC-OPTION-CODE.
000822     PERFORM 8500-DATE-CONVERSION.
000823     MOVE DC-BIN-DATE-1         TO  WS-CURRENT-DATE.
000824     MOVE DC-GREG-DATE-1-YMD    TO  SAVE-DATE-YMD.
000825
000826     IF SAVE-DATE-YY GREATER 70
000827         MOVE 19                TO  SAVE-DATE-CC
000828       ELSE
000829         MOVE 20                TO  SAVE-DATE-CC.
000830
000831     EJECT
000832 0010-MAIN-LOGIC.
000833     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000834         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000835             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
000836             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
000837             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
000838             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
000839             MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
000840             MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
000841             MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
000842             MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
000843           ELSE
000844             MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
000845             MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
000846             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
000847             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
000848             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
000849             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
000850             MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
000851             MOVE SPACES               TO  PI-SAVED-PROGRAM-6
000852       ELSE
000853         GO TO 0020-MAIN-LOGIC.
000854
000855 0015-MAIN-LOGIC.
000856*    NOTE *******************************************************
000857*         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
000858*         *  INTERFACE BLOCK FOR THIS MODULE.                   *
000859*         *******************************************************.
000860
000861     IF PI-RETURN-TO-PROGRAM = PGM-EL1501
000862         NEXT SENTENCE
000863     ELSE
000864         MOVE +1                 TO  PI-REMINDERS-SW
000865                                     PI-LETTERS-SW
000866                                     PI-PAYMENTS-SW
000867                                     PI-AUTO-PAY-SW
000868                                     PI-NOTES-SW
000869                                     PI-RES-EXP-SW
000870                                     PI-DENIALS-SW
000871                                     PI-INCURRED-DATE-SW
000872                                     PI-FORMS-SW.
000873
000874     MOVE ZERO                   TO  PI-END-OF-FILE
000875                                     PI-RECORD-COUNT
000876                                     PI-TRAILER-NUMBER
000877                                     PI-MAPG-DELETE-CNT
000878
000879     MOVE LOW-VALUES             TO  PI-AFTER-DATE
000880                                     PI-AFTER-DATE-2
000881                                     PI-AFTER-DATE-3.
000882
000883     MOVE DFHENTER               TO  PI-PREV-AID.
000884
000885     MOVE '1'                    TO PI-APPROVAL-LEVEL
000886     MOVE PI-COMPANY-ID          TO CNTL-CO
000887     MOVE '2'                    TO CNTL-RECORD-TYPE
000888     MOVE +0                     TO CNTL-SEQ
000889     MOVE PI-PROCESSOR-ID        TO CNTL-GENL
000890
000891     
      * EXEC CICS READ
000892*        DATASET  ('ELCNTL')
000893*        SET      (ADDRESS OF CONTROL-FILE)
000894*        RIDFLD   (WS-CONTROL-FILE-KEY)
000895*        RESP     (WS-RESPONSE)
000896*    END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009222' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039323232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000897     IF RESP-NORMAL
000898        MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL
000899     END-IF
000900
000901     IF FIRST-TIME
000902         MOVE 'N'                TO  PI-FIRST-TIME-SW
000903         IF PI-RETURN-TO-PROGRAM = PGM-EL1501
000904             GO TO 4000-READ-TRAILER-FILE.
000905
000906     MOVE LOW-VALUES             TO  EL142AO.
000907
000908     MOVE PI-TRAILER-NUMBER      TO  ASEQO.
000909     MOVE AL-UNNON               TO  ASEQA.
000910     MOVE -1                     TO  ARECDTEL.
000911
000912     MOVE EL142A                 TO  PI-MAP-NAME.
000913
000914     PERFORM 8100-SEND-INITIAL-MAP.
000915
000916     GO TO 9100-RETURN-TRAN.
000917
000918     EJECT
000919 0020-MAIN-LOGIC.
000920*    NOTE *******************************************************
000921*         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
000922*         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
000923*         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
000924*         *******************************************************.
000925
000926     IF EIBAID = DFHCLEAR
000927         GO TO 9400-CLEAR.
000928
000929     IF PI-PROCESSOR-ID = 'LGXX'
000930         NEXT SENTENCE
000931     ELSE
000932         
      * EXEC CICS  READQ TS
000933*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000934*            INTO    (SECURITY-CONTROL)
000935*            LENGTH  (SC-COMM-LENGTH)
000936*            ITEM    (SC-ITEM)
000937*        END-EXEC
      *    MOVE '*$II   L              ''   #00009263' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039323633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000938         MOVE SC-CLAIMS-DISPLAY (15)  TO  PI-DISPLAY-CAP
000939         MOVE SC-CLAIMS-UPDATE  (15)  TO  PI-MODIFY-CAP
000940         IF NOT DISPLAY-CAP
000941             MOVE 'READ'          TO SM-READ
000942             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000943             MOVE ER-0070         TO  EMI-ERROR
000944             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000945             GO TO 8100-SEND-INITIAL-MAP.
000946
000947     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000948         MOVE ER-0008            TO  EMI-ERROR
000949         IF PI-MAP-NAME = EL142A
000950             MOVE -1             TO  ARECDTEL
000951             PERFORM 8200-SEND-DATAONLY
000952           ELSE
000953         IF PI-MAP-NAME = EL142B
000954             MOVE -1             TO  BPFKL
000955             PERFORM 8200-SEND-DATAONLY
000956           ELSE
000957         IF PI-MAP-NAME = EL142B2
000958             MOVE -1             TO  KPFKL
000959             PERFORM 8200-SEND-DATAONLY
000960           ELSE
000961         IF PI-MAP-NAME = EL142C
000962             MOVE -1             TO  CPFKL
000963             PERFORM 8200-SEND-DATAONLY
000964           ELSE
000965         IF PI-MAP-NAME = EL142D
000966             MOVE -1             TO  DPFKL
000967             PERFORM 8200-SEND-DATAONLY
000968           ELSE
000969         IF PI-MAP-NAME = EL142D2
000970             MOVE -1             TO  LPFKL
000971             PERFORM 8200-SEND-DATAONLY
000972           ELSE
000973         IF PI-MAP-NAME = EL142E
000974             MOVE -1             TO  EPFKL
000975             PERFORM 8200-SEND-DATAONLY
000976           ELSE
000977         IF PI-MAP-NAME = EL142F
000978             MOVE -1             TO  FPFKL
000979             PERFORM 8200-SEND-DATAONLY
000980           ELSE
000981         IF PI-MAP-NAME = EL142G
000982             MOVE -1             TO  GPFKL
000983             PERFORM 8200-SEND-DATAONLY
000984           ELSE
000985         IF PI-MAP-NAME = EL142H
000986             MOVE -1             TO  HPFKL
000987             PERFORM 8200-SEND-DATAONLY
000988           ELSE
000989         IF PI-MAP-NAME = EL142I
000990             MOVE -1             TO  IPFKL
000991             PERFORM 8200-SEND-DATAONLY
000992           ELSE
000993         IF PI-MAP-NAME = EL142J
000994             MOVE -1             TO  JPFKL
000995             PERFORM 8200-SEND-DATAONLY
000996           ELSE
000997             PERFORM 8200-SEND-DATAONLY.
000998
000999     
      * EXEC CICS RECEIVE
001000*        INTO   (EL142DI)
001001*        MAPSET (WS-MAPSET-NAME)
001002*        MAP    (PI-MAP-NAME)
001003*    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00009330' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039333330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001004
001005     IF PI-MAP-NAME = EL142B
001006         IF BPFKL GREATER ZERO
001007             IF EIBAID NOT = DFHENTER
001008                 MOVE ER-0004    TO  EMI-ERROR
001009                 MOVE AL-UNBOF   TO  BPFKA
001010                 MOVE -1         TO  BPFKL
001011                 PERFORM 8200-SEND-DATAONLY
001012               ELSE
001013                 IF (BPFKO NUMERIC) AND
001014                    (BPFKO GREATER ZERO AND LESS 25)
001015                     MOVE PF-VALUES (BPFKI)  TO  EIBAID
001016                   ELSE
001017                     MOVE ER-0029  TO  EMI-ERROR
001018                     MOVE AL-UNBON TO  BPFKA
001019                     MOVE -1       TO  BPFKL
001020                     PERFORM 8200-SEND-DATAONLY
001021           ELSE
001022             NEXT SENTENCE
001023       ELSE
001024     IF PI-MAP-NAME = EL142B2
001025        IF KPFKL GREATER ZERO
001026           IF EIBAID NOT = DFHENTER
001027              MOVE ER-0004       TO  EMI-ERROR
001028              MOVE AL-UNBOF      TO  KPFKA
001029              MOVE -1            TO  KPFKL
001030              PERFORM 8200-SEND-DATAONLY
001031           ELSE
001032              IF (KPFKO NUMERIC) AND
001033                 (KPFKO GREATER ZERO AND LESS 25)
001034                  MOVE PF-VALUES (KPFKI)  TO  EIBAID
001035              ELSE
001036                  MOVE ER-0029    TO  EMI-ERROR
001037                  MOVE AL-UNBOF   TO  KPFKA
001038                  MOVE -1         TO  KPFKL
001039                  PERFORM 8200-SEND-DATAONLY
001040        ELSE
001041           NEXT SENTENCE
001042     ELSE
001043     IF PI-MAP-NAME = EL142C
001044         IF CPFKL GREATER ZERO
001045             IF EIBAID NOT = DFHENTER
001046                 MOVE ER-0004    TO  EMI-ERROR
001047                 MOVE AL-UNBOF   TO  CPFKA
001048                 MOVE -1         TO  CPFKL
001049                 PERFORM 8200-SEND-DATAONLY
001050               ELSE
001051                 IF (CPFKO NUMERIC) AND
001052                    (CPFKO GREATER ZERO AND LESS 25)
001053                     MOVE PF-VALUES (CPFKI)  TO  EIBAID
001054                   ELSE
001055                     MOVE ER-0029  TO  EMI-ERROR
001056                     MOVE AL-UNBOF TO  CPFKA
001057                     MOVE -1       TO  CPFKL
001058                     PERFORM 8200-SEND-DATAONLY
001059           ELSE
001060             NEXT SENTENCE
001061       ELSE
001062     IF PI-MAP-NAME = EL142D
001063         IF DPFKL GREATER ZERO
001064             IF EIBAID NOT = DFHENTER
001065                 MOVE ER-0004    TO  EMI-ERROR
001066                 MOVE AL-UNBOF   TO  DPFKA
001067                 MOVE -1         TO  DPFKL
001068                 PERFORM 8200-SEND-DATAONLY
001069             ELSE
001070                 IF (DPFKO NUMERIC) AND
001071                    (DPFKO GREATER ZERO AND LESS 25)
001072                     MOVE PF-VALUES (DPFKI)  TO  EIBAID
001073                 ELSE
001074                     MOVE ER-0029  TO  EMI-ERROR
001075                     MOVE AL-UNBOF TO  DPFKA
001076                     MOVE -1       TO  DPFKL
001077                     PERFORM 8200-SEND-DATAONLY
001078         ELSE
001079             NEXT SENTENCE
001080     ELSE
001081     IF PI-MAP-NAME = EL142D2
001082        IF LPFKL GREATER ZERO
001083           IF EIBAID NOT = DFHENTER
001084              MOVE ER-0004       TO  EMI-ERROR
001085              MOVE AL-UNBOF      TO  LPFKA
001086              MOVE -1            TO  LPFKL
001087              PERFORM 8200-SEND-DATAONLY
001088           ELSE
001089              IF (LPFKO NUMERIC) AND
001090                 (LPFKO GREATER ZERO AND LESS 25)
001091                 MOVE PF-VALUES (LPFKI)  TO  EIBAID
001092              ELSE
001093                 MOVE ER-0029    TO  EMI-ERROR
001094                 MOVE AL-UNBON   TO  LPFKA
001095                 MOVE -1         TO  LPFKL
001096                 PERFORM 8200-SEND-DATAONLY
001097        ELSE
001098           NEXT SENTENCE
001099     ELSE
001100     IF PI-MAP-NAME = EL142E
001101         IF EPFKL GREATER ZERO
001102             IF EIBAID NOT = DFHENTER
001103                 MOVE ER-0004    TO  EMI-ERROR
001104                 MOVE AL-UNBOF   TO  EPFKA
001105                 MOVE -1         TO  EPFKL
001106                 PERFORM 8200-SEND-DATAONLY
001107               ELSE
001108                 IF (EPFKO NUMERIC) AND
001109                    (EPFKO GREATER ZERO AND LESS 25)
001110                     MOVE PF-VALUES (EPFKI)  TO  EIBAID
001111                   ELSE
001112                     MOVE ER-0029  TO  EMI-ERROR
001113                     MOVE AL-UNBOF TO  EPFKA
001114                     MOVE -1       TO  EPFKL
001115                     PERFORM 8200-SEND-DATAONLY
001116           ELSE
001117             NEXT SENTENCE
001118       ELSE
001119     IF PI-MAP-NAME = EL142F
001120         IF FPFKL GREATER ZERO
001121             IF EIBAID NOT = DFHENTER
001122                 MOVE ER-0004    TO  EMI-ERROR
001123                 MOVE AL-UNBOF   TO  FPFKA
001124                 MOVE -1         TO  FPFKL
001125                 PERFORM 8200-SEND-DATAONLY
001126               ELSE
001127                 IF (FPFKO NUMERIC) AND
001128                    (FPFKO GREATER ZERO AND LESS 25)
001129                     MOVE PF-VALUES (FPFKI)  TO  EIBAID
001130                   ELSE
001131                     MOVE ER-0029 TO  EMI-ERROR
001132                     MOVE AL-UNBOF TO  FPFKA
001133                     MOVE -1      TO  FPFKL
001134                     PERFORM 8200-SEND-DATAONLY
001135           ELSE
001136             NEXT SENTENCE
001137       ELSE
001138     IF PI-MAP-NAME = EL142G
001139         IF GPFKL GREATER ZERO
001140             IF EIBAID NOT = DFHENTER
001141                 MOVE ER-0004    TO  EMI-ERROR
001142                 MOVE AL-UNBOF   TO  GPFKA
001143                 MOVE -1         TO  GPFKL
001144                 PERFORM 8200-SEND-DATAONLY
001145               ELSE
001146                 IF (GPFKO NUMERIC) AND
001147                    (GPFKO GREATER ZERO AND LESS 25)
001148                     MOVE PF-VALUES (GPFKI)  TO  EIBAID
001149                   ELSE
001150                     MOVE ER-0029  TO  EMI-ERROR
001151                     MOVE AL-UNBOF TO  GPFKA
001152                     MOVE -1       TO  GPFKL
001153                     PERFORM 8200-SEND-DATAONLY
001154           ELSE
001155             NEXT SENTENCE
001156       ELSE
001157     IF PI-MAP-NAME = EL142H
001158         IF HPFKL GREATER ZERO
001159             IF EIBAID NOT = DFHENTER
001160                 MOVE ER-0004    TO  EMI-ERROR
001161                 MOVE AL-UNBOF   TO  HPFKA
001162                 MOVE -1         TO  HPFKL
001163                 PERFORM 8200-SEND-DATAONLY
001164               ELSE
001165                 IF (HPFKO NUMERIC) AND
001166                    (HPFKO GREATER ZERO AND LESS 25)
001167                     MOVE PF-VALUES (HPFKI)  TO  EIBAID
001168                   ELSE
001169                     MOVE ER-0029  TO  EMI-ERROR
001170                     MOVE AL-UNBOF TO  HPFKA
001171                     MOVE -1       TO  HPFKL
001172                     PERFORM 8200-SEND-DATAONLY
001173           ELSE
001174             NEXT SENTENCE
001175       ELSE
001176     IF PI-MAP-NAME = EL142I
001177         IF IPFKL GREATER ZERO
001178             IF EIBAID NOT = DFHENTER
001179                 MOVE ER-0004    TO  EMI-ERROR
001180                 MOVE AL-UNBOF   TO  IPFKA
001181                 MOVE -1         TO  IPFKL
001182                 PERFORM 8200-SEND-DATAONLY
001183               ELSE
001184                 IF (IPFKO NUMERIC) AND
001185                    (IPFKO GREATER ZERO AND LESS 25)
001186                     MOVE PF-VALUES (IPFKI)  TO  EIBAID
001187                   ELSE
001188                     MOVE ER-0029  TO  EMI-ERROR
001189                     MOVE AL-UNBOF TO  IPFKA
001190                     MOVE -1       TO  IPFKL
001191                     PERFORM 8200-SEND-DATAONLY
001192           ELSE
001193             NEXT SENTENCE
001194       ELSE
001195     IF PI-MAP-NAME = EL142J
001196         IF JPFKL GREATER ZERO
001197             IF EIBAID NOT = DFHENTER
001198                 MOVE ER-0004    TO  EMI-ERROR
001199                 MOVE AL-UNBOF   TO  JPFKA
001200                 MOVE -1         TO  JPFKL
001201                 PERFORM 8200-SEND-DATAONLY
001202               ELSE
001203                 IF (JPFKO NUMERIC) AND
001204                    (JPFKO GREATER ZERO AND LESS 25)
001205                     MOVE PF-VALUES (JPFKI)  TO  EIBAID
001206                   ELSE
001207                     MOVE ER-0029  TO  EMI-ERROR
001208                     MOVE AL-UNBOF TO  JPFKA
001209                     MOVE -1       TO  JPFKL
001210                     PERFORM 8200-SEND-DATAONLY.
001211
001212     IF EIBAID = DFHPF12
001213         MOVE EL010              TO  XCTL-PGM
001214         GO TO 9300-XCTL.
001215
001216     IF EIBAID = DFHPF23
001217         GO TO 9000-RETURN-CICS.
001218
001219     IF EIBAID = DFHPF24
001220         MOVE EL126              TO  XCTL-PGM
001221         GO TO 9300-XCTL.
001222
001223     IF EIBAID = DFHENTER OR
001224       (EIBAID = DFHPF1 OR DFHPF2 OR DFHPF5) AND
001225       (PI-MAP-NAME NOT = EL142A AND EL142B2 AND EL142D2)
001226                   OR
001227       (EIBAID = DFHPF6 AND
001228        PI-PROCESSOR-ID = 'PEMA' AND
001229        PI-MAP-NAME = EL142B OR EL142D)
001230           GO TO 0040-MAIN-LOGIC.
001231
001232     MOVE ER-0008                TO  EMI-ERROR.
001233
001234     IF PI-MAP-NAME = EL142A
001235         MOVE -1                 TO  ARECDTEL
001236       ELSE
001237     IF PI-MAP-NAME = EL142B
001238         MOVE -1                 TO  BPFKL
001239       ELSE
001240     IF PI-MAP-NAME = EL142B2
001241         MOVE -1                 TO  KPFKL
001242       ELSE
001243     IF PI-MAP-NAME = EL142C
001244         MOVE -1                 TO  CPFKL
001245       ELSE
001246     IF PI-MAP-NAME = EL142D
001247         MOVE -1                 TO  DPFKL
001248       ELSE
001249     IF PI-MAP-NAME = EL142D2
001250         MOVE -1                 TO  LPFKL
001251       ELSE
001252     IF PI-MAP-NAME = EL142E
001253         MOVE -1                 TO  EPFKL
001254       ELSE
001255     IF PI-MAP-NAME = EL142F
001256         MOVE -1                 TO  FPFKL
001257       ELSE
001258     IF PI-MAP-NAME = EL142G
001259         MOVE -1                 TO  GPFKL
001260       ELSE
001261     IF PI-MAP-NAME = EL142H
001262         MOVE -1                 TO  HPFKL
001263       ELSE
001264     IF PI-MAP-NAME = EL142I
001265         MOVE -1                 TO  IPFKL
001266       ELSE
001267     IF PI-MAP-NAME = EL142J
001268         MOVE -1                 TO  JPFKL.
001269
001270     PERFORM 8200-SEND-DATAONLY.
001271
001272 0040-MAIN-LOGIC.
001273     IF EIBAID = DFHPF1 OR DFHPF2
001274         PERFORM 4000-READ-TRAILER-FILE.
001275
001276     IF EIBAID = DFHPF5
001277         GO TO 0015-MAIN-LOGIC.
001278
001279     EJECT
001280 0100-MAIN-LOGIC.
001281     IF PI-MAP-NAME NOT = EL142A
001282         GO TO 0200-MAIN-LOGIC.
001283
001284     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
001285     MOVE +2                     TO  EMI-SWITCH2.
001286
001287     IF ARECDTEL NOT GREATER ZERO
001288         MOVE LOW-VALUES         TO  PI-AFTER-DATE
001289     ELSE
001290         MOVE ARECDTEI           TO  WS-DEEDIT-FIELD
001291         PERFORM 8600-DEEDIT
001292         MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001293         MOVE '4'                TO  DC-OPTION-CODE
001294         PERFORM 8500-DATE-CONVERSION
001295         IF DC-ERROR-CODE NOT = SPACE
001296             MOVE ER-0285        TO  EMI-ERROR
001297             PERFORM 9900-ERROR-FORMAT
001298             MOVE -1             TO  ARECDTEL
001299             MOVE AL-UNBON       TO  ARECDTEA
001300         ELSE
001301             MOVE +1             TO  WS-UPDATE-SW
001302             MOVE DC-BIN-DATE-1  TO  PI-AFTER-DATE
001303             MOVE AL-UNNON       TO  ARECDTEA
001304             MOVE WS-DEEDIT-FIELD-V0  TO  ARECDTEO
001305             INSPECT ARECDTEI CONVERTING SPACES TO SLASH.
001306
001307     IF AREMINDL GREATER ZERO
001308         IF AREMINDI = 'Y'
001309             MOVE +1             TO  PI-REMINDERS-SW
001310                                     WS-UPDATE-SW
001311             MOVE AL-UANON       TO  AREMINDA
001312         ELSE
001313             IF AREMINDI = 'N'
001314                 MOVE ZERO       TO  PI-REMINDERS-SW
001315                 MOVE AL-UANON   TO  AREMINDA
001316             ELSE
001317                 MOVE ER-0286    TO  EMI-ERROR
001318                 PERFORM 9900-ERROR-FORMAT
001319                 MOVE -1         TO  AREMINDL
001320                 MOVE AL-UABON   TO  AREMINDA
001321     ELSE
001322         MOVE ER-0286            TO  EMI-ERROR
001323         PERFORM 9900-ERROR-FORMAT
001324         MOVE -1                 TO  AREMINDL
001325         MOVE AL-UABON           TO  AREMINDA.
001326
001327     IF ALETTERL GREATER ZERO
001328         IF ALETTERI = 'Y'
001329             MOVE +1             TO  PI-LETTERS-SW
001330                                     WS-UPDATE-SW
001331             MOVE AL-UANON       TO  ALETTERA
001332         ELSE
001333             IF ALETTERI = 'N'
001334                 MOVE ZERO       TO  PI-LETTERS-SW
001335                 MOVE AL-UANON   TO  ALETTERA
001336             ELSE
001337                 MOVE ER-0287    TO  EMI-ERROR
001338                 PERFORM 9900-ERROR-FORMAT
001339                 MOVE -1         TO  ALETTERL
001340                 MOVE AL-UABON   TO  ALETTERA
001341     ELSE
001342         MOVE ER-0287            TO  EMI-ERROR
001343         PERFORM 9900-ERROR-FORMAT
001344         MOVE -1                 TO  ALETTERL
001345         MOVE AL-UABON           TO  ALETTERA.
001346
001347     IF APAYMNTL GREATER ZERO
001348         IF APAYMNTI = 'Y'
001349             MOVE +1             TO  PI-PAYMENTS-SW
001350                                     WS-UPDATE-SW
001351             MOVE AL-UANON       TO  APAYMNTA
001352         ELSE
001353             IF APAYMNTI = 'N'
001354                 MOVE ZERO       TO  PI-PAYMENTS-SW
001355                 MOVE AL-UANON   TO  APAYMNTA
001356             ELSE
001357                 MOVE ER-0288       TO  EMI-ERROR
001358                 PERFORM 9900-ERROR-FORMAT
001359                 MOVE -1         TO  APAYMNTL
001360                 MOVE AL-UABON   TO  APAYMNTA
001361     ELSE
001362         MOVE ER-0288            TO  EMI-ERROR
001363         PERFORM 9900-ERROR-FORMAT
001364         MOVE -1                 TO  APAYMNTL
001365         MOVE AL-UABON           TO  APAYMNTA.
001366
001367     IF AAUTOPAL GREATER ZERO
001368         IF AAUTOPAI = 'Y'
001369             MOVE +1             TO  PI-AUTO-PAY-SW
001370                                     WS-UPDATE-SW
001371             MOVE AL-UANON       TO  AAUTOPAA
001372         ELSE
001373             IF AAUTOPAI = 'N'
001374                 MOVE ZERO       TO  PI-AUTO-PAY-SW
001375                 MOVE AL-UANON   TO  AAUTOPAA
001376             ELSE
001377                 MOVE ER-0289       TO  EMI-ERROR
001378                 PERFORM 9900-ERROR-FORMAT
001379                 MOVE -1         TO  AAUTOPAL
001380                 MOVE AL-UABON   TO  AAUTOPAA
001381     ELSE
001382         MOVE ER-0289            TO  EMI-ERROR
001383         PERFORM 9900-ERROR-FORMAT
001384         MOVE -1                 TO  AAUTOPAL
001385         MOVE AL-UABON           TO  AAUTOPAA.
001386
001387     IF ANOTESL GREATER ZERO
001388         IF ANOTESI = 'Y'
001389             MOVE +1             TO  PI-NOTES-SW
001390                                     WS-UPDATE-SW
001391             MOVE AL-UANON       TO  ANOTESA
001392         ELSE
001393             IF ANOTESI = 'N'
001394                 MOVE ZERO       TO  PI-NOTES-SW
001395                 MOVE AL-UANON   TO  ANOTESA
001396             ELSE
001397                 MOVE ER-0290       TO  EMI-ERROR
001398                 PERFORM 9900-ERROR-FORMAT
001399                 MOVE -1         TO  ANOTESL
001400                 MOVE AL-UABON   TO  ANOTESA
001401     ELSE
001402         MOVE ER-0290            TO  EMI-ERROR
001403         PERFORM 9900-ERROR-FORMAT
001404         MOVE -1                 TO  ANOTESL
001405         MOVE AL-UABON           TO  ANOTESA.
001406
001407     IF ARESEXPL GREATER ZERO
001408         IF ARESEXPI = 'Y'
001409             MOVE +1             TO  PI-RES-EXP-SW
001410                                     WS-UPDATE-SW
001411             MOVE AL-UANON       TO  ARESEXPA
001412         ELSE
001413             IF ARESEXPI = 'N'
001414                 MOVE ZERO       TO  PI-RES-EXP-SW
001415                 MOVE AL-UANON   TO  ARESEXPA
001416             ELSE
001417                 MOVE ER-0291    TO  EMI-ERROR
001418                 PERFORM 9900-ERROR-FORMAT
001419                 MOVE -1         TO  ARESEXPL
001420                 MOVE AL-UABON   TO  ARESEXPA
001421     ELSE
001422         MOVE ER-0291            TO  EMI-ERROR
001423         PERFORM 9900-ERROR-FORMAT
001424         MOVE -1                 TO  ARESEXPL
001425         MOVE AL-UABON           TO  ARESEXPA.
001426
001427     IF ADENIALL GREATER ZERO
001428         IF ADENIALI = 'Y'
001429             MOVE +1             TO  PI-DENIALS-SW
001430                                     WS-UPDATE-SW
001431             MOVE AL-UANON       TO  ADENIALA
001432         ELSE
001433             IF ADENIALI = 'N'
001434                 MOVE ZERO       TO  PI-DENIALS-SW
001435             MOVE AL-UANON       TO  ADENIALA
001436             ELSE
001437                 MOVE ER-0292    TO  EMI-ERROR
001438                 PERFORM 9900-ERROR-FORMAT
001439                 MOVE -1         TO  ADENIALL
001440                 MOVE AL-UABON   TO  ADENIALA
001441     ELSE
001442         MOVE ER-0292            TO  EMI-ERROR
001443         PERFORM 9900-ERROR-FORMAT
001444         MOVE -1                 TO  ADENIALL
001445         MOVE AL-UABON           TO  ADENIALA.
001446
001447     IF AIDCL GREATER ZERO
001448         IF AIDCI = 'Y'
001449             MOVE +1             TO  PI-INCURRED-DATE-SW
001450                                     WS-UPDATE-SW
001451             MOVE AL-UANON       TO  AIDCA
001452         ELSE
001453             IF AIDCI = 'N'
001454                 MOVE ZERO       TO  PI-INCURRED-DATE-SW
001455                 MOVE AL-UANON   TO  AIDCA
001456             ELSE
001457                 MOVE ER-0341    TO  EMI-ERROR
001458                 PERFORM 9900-ERROR-FORMAT
001459                 MOVE -1         TO  AIDCL
001460                 MOVE AL-UABON   TO  AIDCA
001461     ELSE
001462         MOVE ER-0341            TO  EMI-ERROR
001463         PERFORM 9900-ERROR-FORMAT
001464         MOVE -1                 TO  AIDCL
001465         MOVE AL-UABON           TO  AIDCA.
001466
001467     IF AFORMSL GREATER ZERO
001468         IF AFORMSI = 'Y'
001469             MOVE +1             TO  PI-FORMS-SW
001470                                     WS-UPDATE-SW
001471             MOVE AL-UANON       TO  AFORMSA
001472         ELSE
001473             IF AFORMSI = 'N'
001474                 MOVE ZERO       TO  PI-FORMS-SW
001475                 MOVE AL-UANON   TO  AFORMSA
001476             ELSE
001477                 MOVE ER-0538    TO  EMI-ERROR
001478                 PERFORM 9900-ERROR-FORMAT
001479                 MOVE -1         TO  AFORMSL
001480                 MOVE AL-UABON   TO  AFORMSA
001481     ELSE
001482         MOVE ER-0538            TO  EMI-ERROR
001483         PERFORM 9900-ERROR-FORMAT
001484         MOVE -1                 TO  AFORMSL
001485         MOVE AL-UABON           TO  AFORMSA.
001486
001487     IF ASEQL NOT GREATER ZERO
001488         MOVE ZERO               TO  PI-TRAILER-NUMBER
001489     ELSE
001490         IF ASEQI IS NUMERIC
001491             MOVE ASEQI          TO  PI-TRAILER-NUMBER
001492         ELSE
001493             MOVE ER-0293        TO  EMI-ERROR
001494             PERFORM 9900-ERROR-FORMAT
001495             MOVE -1             TO  ASEQL
001496             MOVE AL-UNBON       TO  ASEQA.
001497
001498     IF WS-UPDATE-SW = ZERO
001499         MOVE ER-0329            TO  EMI-ERROR
001500         MOVE -1                 TO  AREMINDL
001501         PERFORM 9900-ERROR-FORMAT.
001502
001503     IF WS-ERROR-COUNT GREATER ZERO
001504         PERFORM 8200-SEND-DATAONLY.
001505
001506     MOVE SPACES                 TO  PI-ACTIVITY-TRAILERS-KEY.
001507
001508     MOVE PI-COMPANY-CD          TO  PI-ATK-COMPANY-CODE.
001509     MOVE PI-CARRIER             TO  PI-ATK-CARRIER.
001510     MOVE PI-CLAIM-NO            TO  PI-ATK-CLAIM-NO.
001511     MOVE PI-CERT-NO             TO  PI-ATK-CERT-NO.
001512     MOVE PI-TRAILER-NUMBER      TO  PI-ATK-SEQUENCE-NO.
001513
001514     
      * EXEC CICS READ
001515*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
001516*        RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)
001517*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
001518*    END-EXEC
      *    MOVE '&"S        E          (   #00009845' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001519
001520     PERFORM 4000-READ-TRAILER-FILE.
001521
001522 0130-MAIN-LOGIC.
001523     MOVE ER-0342                TO  EMI-ERROR.
001524     MOVE -1                     TO  ARECDTEL.
001525     PERFORM 8200-SEND-DATAONLY.
001526
001527     EJECT
001528 0200-MAIN-LOGIC.
001529     IF PI-MAP-NAME NOT = EL142B
001530         GO TO 0300-MAIN-LOGIC.
001531
001532     IF NOT MODIFY-CAP
001533         IF BMAINTI = 'S'
001534             NEXT SENTENCE
001535         ELSE
001536             MOVE 'UPDATE'       TO SM-READ
001537             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
001538             MOVE ER-0070        TO  EMI-ERROR
001539             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001540             GO TO 8100-SEND-INITIAL-MAP.
001541
001542     IF EIBAID = DFHPF6
001543         PERFORM 5000-DISPLAY-CHECK-QUEUE.
001544
001545     MOVE +2                     TO  EMI-NUMBER-OF-LINES
001546                                     EMI-SWITCH2.
001547
001548     IF BMAINTL NOT GREATER ZERO
001549       OR (BMAINTL GREATER ZERO AND
001550           BMAINTI = 'S')
001551             PERFORM 4000-READ-TRAILER-FILE.
001552
001553     IF BMAINTI NOT = 'C'
001554         MOVE ER-0023            TO  EMI-ERROR
001555         MOVE -1                 TO  BMAINTL
001556         MOVE AL-UABON           TO  BMAINTA
001557         PERFORM 8200-SEND-DATAONLY.
001558
001559     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
001560           or 'FNL'
001561        IF (PI-EL142-PRIORITY = '8')
001562           AND (PI-approval-level <> '5')
001563           MOVE ER-8003          TO EMI-ERROR
001564           MOVE -1               TO BMAINTL
001565           MOVE AL-UABON         TO BMAINTA
001566           GO TO 8200-SEND-DATAONLY.
001567
001568     MOVE AL-UANON               TO  BMAINTA.
001569
001570     IF BCKNOL GREATER ZERO
001571         MOVE +1                 TO  WS-UPDATE-SW.
001572
001573     IF  BPRFDTL > 0
001574         MOVE BPRFDTI             TO WS-DEEDIT-FIELD
001575         PERFORM 8600-DEEDIT
001576         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
001577         MOVE '4'                 TO DC-OPTION-CODE
001578         PERFORM 8500-DATE-CONVERSION
001579         IF DC-ERROR-CODE NOT = SPACES
001580            MOVE ER-0021          TO EMI-ERROR
001581            MOVE -1               TO BPRFDTL
001582            MOVE AL-UABON         TO BPRFDTA
001583            PERFORM 9900-ERROR-FORMAT
001584         ELSE
001585            IF DC-BIN-DATE-1 > WS-CURRENT-DATE
001586                MOVE ER-0873      TO EMI-ERROR
001587                MOVE -1           TO BPRFDTL
001588                MOVE AL-UABON     TO BPRFDTA
001589                PERFORM 9900-ERROR-FORMAT
001590            ELSE
001591                MOVE AL-UANON       TO  BPRFDTA
001592                MOVE +1             TO  WS-UPDATE-SW
001593                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
001594                MOVE WS-DEEDIT-FIELD-V0  TO BPRFDTO
001595                INSPECT BPRFDTI CONVERTING ' ' TO '/'.
001596
001597
001598     IF BPAYEEL GREATER ZERO
001599         IF BPAYEEI = ('I' OR 'B' OR 'A' OR 'O' OR 'Q' OR 'P' OR
001600                              'E'           OR
001601                              'INSURED'     OR
001602                              'BENEFICIARY' OR
001603                              'ACCOUNT'     OR
001604                              'OTHER 1'     OR
001605                              'REM BORR'    OR
001606                              'EMPLOYER')
001607             MOVE +1             TO  WS-UPDATE-SW
001608             MOVE AL-UANON       TO  BPAYEEA
001609         ELSE
001610             MOVE -1             TO  BPAYEEL
001611             MOVE AL-UABON       TO  BPAYEEA
001612             MOVE ER-0294        TO  EMI-ERROR
001613             PERFORM 9900-ERROR-FORMAT.
001614
001615     IF BEXPTYPL GREATER ZERO
001616         MOVE +1                 TO WS-UPDATE-SW
001617         IF BEXPTYPI GREATER 0
001618           OR BEXPTYPI = SPACE
001619             MOVE AL-UANON       TO BEXPTYPA
001620         ELSE
001621             MOVE ER-2466        TO EMI-ERROR
001622             MOVE -1             TO BEXPTYPL
001623             MOVE AL-UABON       TO BEXPTYPA
001624             PERFORM 9900-ERROR-FORMAT.
001625
001626     IF BEOBYNL > +0
001627        MOVE +1               TO WS-UPDATE-SW
001628        IF BEOBYNI NOT = 'Y' AND 'N' AND 'S'
001629           MOVE ER-1561          TO EMI-ERROR
001630           MOVE AL-UABON         TO BEOBYNA
001631           MOVE -1               TO BEOBYNL
001632           PERFORM 9900-ERROR-FORMAT
001633        END-IF
001634     END-IF
001635
001636     IF BCLMYNL > +0
001637        MOVE +1               TO WS-UPDATE-SW
001638        IF BCLMYNI NOT = 'Y' AND 'N'
001639           MOVE ER-1566          TO EMI-ERROR
001640           MOVE AL-UABON         TO BCLMYNA
001641           MOVE -1               TO BCLMYNL
001642           PERFORM 9900-ERROR-FORMAT
001643        END-IF
001644     END-IF
001645
001646     IF BSRVYNL > +0
001647        MOVE +1               TO WS-UPDATE-SW
001648        IF BSRVYNI NOT = 'Y' AND 'N'
001649           MOVE ER-1567          TO EMI-ERROR
001650           MOVE AL-UABON         TO BSRVYNA
001651           MOVE -1               TO BSRVYNL
001652           PERFORM 9900-ERROR-FORMAT
001653        END-IF
001654     END-IF
001655
001656     IF BSPRELL > +0
001657        MOVE +1               TO WS-UPDATE-SW
001658        IF BSPRELI NOT = 'Y' AND 'N'
001659           MOVE ER-1569          TO EMI-ERROR
001660           MOVE AL-UABON         TO BSPRELA
001661           MOVE -1               TO BSPRELL
001662           PERFORM 9900-ERROR-FORMAT
001663        END-IF
001664     END-IF
001665
001666     IF BCRSELL GREATER ZERO
001667         IF BCRSELI = SPACES
001668             MOVE LOW-VALUES     TO  WS-CRSEL
001669             MOVE AL-UNNON       TO  BCRSELA
001670             MOVE +1             TO  WS-UPDATE-SW
001671         ELSE
001672             MOVE BCRSELI        TO  WS-DEEDIT-FIELD
001673             PERFORM 8600-DEEDIT
001674             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001675             MOVE '4'            TO  DC-OPTION-CODE
001676             PERFORM 8500-DATE-CONVERSION
001677             IF DC-ERROR-CODE NOT = SPACE
001678                 MOVE ER-0021    TO  EMI-ERROR
001679                 PERFORM 9900-ERROR-FORMAT
001680                 MOVE -1         TO  BCRSELL
001681                 MOVE AL-UNBON   TO  BCRSELA
001682             ELSE
001683                 MOVE +1                  TO  WS-UPDATE-SW
001684                 MOVE DC-BIN-DATE-1       TO  WS-CRSEL
001685                 MOVE AL-UNNON            TO  BCRSELA
001686                 MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
001687                 INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
001688                 MOVE WS-TEMP-DT          TO BCRSELO.
001689
001690     IF BVOIDSDL GREATER ZERO
001691         IF BVOIDSDI = SPACES
001692             MOVE LOW-VALUES     TO  WS-VOIDSD
001693             MOVE AL-UNNON       TO  BVOIDSDA
001694             MOVE +1             TO  WS-UPDATE-SW
001695         ELSE
001696             MOVE BVOIDSDI       TO  WS-DEEDIT-FIELD
001697             PERFORM 8600-DEEDIT
001698             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001699             MOVE '4'            TO  DC-OPTION-CODE
001700             PERFORM 8500-DATE-CONVERSION
001701             IF DC-ERROR-CODE NOT = SPACE
001702                 MOVE ER-0021    TO  EMI-ERROR
001703                 PERFORM 9900-ERROR-FORMAT
001704                 MOVE -1         TO  BVOIDSDL
001705                 MOVE AL-UNBON   TO  BVOIDSDA
001706             ELSE
001707                 MOVE +1                  TO  WS-UPDATE-SW
001708                 MOVE DC-BIN-DATE-1       TO  WS-VOIDSD
001709                 MOVE AL-UNNON            TO  BVOIDSDA
001710                 MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
001711                 INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
001712                 MOVE WS-TEMP-DT          TO BVOIDSDO.
001713
001714     IF BCRACPL GREATER ZERO
001715         IF BCRACPI = SPACES
001716             MOVE LOW-VALUES     TO  PI-AFTER-DATE-2
001717             MOVE AL-UNNON       TO  BCRACPA
001718             MOVE +1             TO  WS-UPDATE-SW
001719         ELSE
001720             MOVE BCRACPI        TO  WS-DEEDIT-FIELD
001721             PERFORM 8600-DEEDIT
001722             MOVE WS-DEEDIT-FIELD-V0     TO  DC-GREG-DATE-1-MDY
001723             MOVE '4'            TO  DC-OPTION-CODE
001724             PERFORM 8500-DATE-CONVERSION
001725             IF DC-ERROR-CODE NOT = SPACE
001726                 MOVE ER-0021    TO  EMI-ERROR
001727                 PERFORM 9900-ERROR-FORMAT
001728                 MOVE -1         TO  BCRACPL
001729                 MOVE AL-UNBON   TO  BCRACPA
001730             ELSE
001731                 MOVE +1                  TO WS-UPDATE-SW
001732                 MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-2
001733                 MOVE AL-UNNON            TO BCRACPA
001734                 MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
001735                 INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
001736                 MOVE WS-TEMP-DT          TO BCRACPO.
001737
001738     IF BVOIDACL GREATER ZERO
001739         IF BVOIDACI = SPACES
001740             MOVE LOW-VALUES     TO  PI-AFTER-DATE-3
001741             MOVE +1             TO  WS-UPDATE-SW
001742             MOVE AL-UNNON       TO  BVOIDACA
001743         ELSE
001744             MOVE BVOIDACI       TO  WS-DEEDIT-FIELD
001745             PERFORM 8600-DEEDIT
001746             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001747             MOVE '4'            TO  DC-OPTION-CODE
001748             PERFORM 8500-DATE-CONVERSION
001749             IF DC-ERROR-CODE NOT = SPACE
001750                 MOVE ER-0021    TO  EMI-ERROR
001751                 PERFORM 9900-ERROR-FORMAT
001752                 MOVE -1         TO  BVOIDACL
001753                 MOVE AL-UNBON   TO  BVOIDACA
001754             ELSE
001755                 MOVE +1                  TO WS-UPDATE-SW
001756                 MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-3
001757                 MOVE AL-UNNON            TO BVOIDACA
001758                 MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
001759                 INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
001760                 MOVE WS-TEMP-DT          TO BVOIDACO.
001761
001762     IF BHOLDATL GREATER ZERO
001763         IF BHOLDATI = SPACES
001764             MOVE LOW-VALUES     TO  PI-HOLD-UNTIL-DATE
001765             MOVE +1             TO  WS-UPDATE-SW
001766             MOVE AL-UNNON       TO  BHOLDATA
001767         ELSE
001768             MOVE BHOLDATI       TO  WS-DEEDIT-FIELD
001769             PERFORM 8600-DEEDIT
001770             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001771             MOVE '4'            TO  DC-OPTION-CODE
001772             PERFORM 8500-DATE-CONVERSION
001773             IF DC-ERROR-CODE NOT = SPACE
001774                 MOVE ER-0021    TO  EMI-ERROR
001775                 PERFORM 9900-ERROR-FORMAT
001776                 MOVE -1         TO  BHOLDATL
001777                 MOVE AL-UNBON   TO  BHOLDATA
001778             ELSE
001779                 MOVE +1                  TO WS-UPDATE-SW
001780                 MOVE DC-BIN-DATE-1       TO PI-HOLD-UNTIL-DATE
001781                 MOVE AL-UNNON            TO BHOLDATA
001782                 MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
001783                 INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
001784                 MOVE WS-TEMP-DT          TO BHOLDATO.
001785
001786     IF BCKQUEL GREATER ZERO
001787         
      * EXEC CICS BIF DEEDIT
001788*            FIELD   (BCKQUEI)
001789*            LENGTH  (8)
001790*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010118' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130313138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCKQUEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001791         IF BCKQUEI NOT NUMERIC
001792             MOVE ER-0570        TO  EMI-ERROR
001793             PERFORM 9900-ERROR-FORMAT
001794             MOVE -1             TO  BCKQUEL
001795             MOVE AL-UNBON       TO  BCKQUEA
001796         ELSE
001797             MOVE AL-UNNON       TO  BCKQUEA
001798             MOVE BCKQUEI        TO  WS-CHECK-QUE  BCKQUEO
001799             MOVE +1             TO  WS-UPDATE-SW.
001800
001801     IF BCKSEQL GREATER ZERO
001802         
      * EXEC CICS BIF DEEDIT
001803*            FIELD   (BCKSEQI)
001804*            LENGTH  (4)
001805*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010133' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130313333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCKSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001806         IF BCKSEQI NOT NUMERIC
001807             MOVE ER-0571        TO  EMI-ERROR
001808             PERFORM 9900-ERROR-FORMAT
001809             MOVE -1             TO  BCKSEQL
001810             MOVE AL-UNBON       TO  BCKSEQA
001811         ELSE
001812             MOVE AL-UNNON       TO  BCKSEQA
001813             MOVE BCKSEQI        TO  WS-CHECK-QUE-SEQ  BCKSEQO
001814             MOVE +1             TO  WS-UPDATE-SW.
001815
001816     IF (PI-COMPANY-ID = 'RMC' OR 'LAP') OR
001817        (PI-PROCESSOR-ID = 'LGXX')
001818         IF BPMTORGL IS GREATER THAN +0
001819             IF BPMTORGI = '1' OR '2' OR '3'
001820                 MOVE AL-UANON   TO  BPMTORGA
001821                 MOVE +1         TO  WS-UPDATE-SW
001822             ELSE
001823                 MOVE ER-0830    TO  EMI-ERROR
001824                 PERFORM 9900-ERROR-FORMAT
001825                 MOVE AL-UANON   TO  BPMTORGA
001826                 MOVE -1         TO  BPMTORGL.
001827
001828     IF WS-ERROR-COUNT GREATER ZERO
001829         PERFORM 8200-SEND-DATAONLY.
001830
001831     IF WS-UPDATE-SW NOT GREATER ZERO
001832         IF (BNOTE1L IS GREATER THAN 0 OR
001833             BNOTE2L IS GREATER THAN 0)
001834                 GO TO 0200-UPDATE-PMNT-NOTE-TRLR
001835              ELSE
001836                 PERFORM 4000-READ-TRAILER-FILE.
001837
001838     PERFORM 3000-READ-FOR-UPDATE.
001839
001840     IF BPAYEEL GREATER ZERO
001841         IF BPAYEEI EQUAL 'I'
001842             MOVE 'I1'           TO  AT-PAYEE-TYPE-CD
001843         ELSE
001844         IF BPAYEEI EQUAL 'B'
001845             MOVE 'B1'           TO  AT-PAYEE-TYPE-CD
001846         ELSE
001847         IF BPAYEEI EQUAL 'A'
001848             MOVE 'A1'           TO  AT-PAYEE-TYPE-CD
001849         ELSE
001850         IF BPAYEEI EQUAL 'O'
001851             MOVE 'O1'           TO  AT-PAYEE-TYPE-CD
001852         ELSE
001853         IF BPAYEEI EQUAL 'Q' OR 'REM BORR'
001854             MOVE 'Q1'           TO  AT-PAYEE-TYPE-CD
001855           ELSE
001856         IF BPAYEEI = 'P' OR 'D'
001857             MOVE 'P1'           TO  AT-PAYEE-TYPE-CD
001858           ELSE
001859         IF BPAYEEI EQUAL 'E'
001860             MOVE 'E1'           TO  AT-PAYEE-TYPE-CD.
001861
001862     IF BPRFDTL GREATER ZERO
001863         MOVE WS-PRF-DT          TO  AT-PMT-PROOF-DT.
001864
001865     IF BCKNOL GREATER ZERO
001866         MOVE BCKNOI             TO  AT-CHECK-NO.
001867
001868     IF BCRSELL GREATER ZERO
001869        MOVE WS-CRSEL            TO  AT-PMT-SELECT-DT.
001870
001871     IF BVOIDSDL GREATER ZERO
001872        MOVE WS-VOIDSD           TO  AT-VOID-SELECT-DT.
001873
001874     IF BCRACPL GREATER ZERO
001875        MOVE PI-AFTER-DATE-2     TO  AT-PMT-ACCEPT-DT.
001876
001877     IF BEOBYNL > 0
001878        MOVE BEOBYNI             TO AT-PRINT-EOB-WITH-CHECK
001879     END-IF
001880
001881     IF BCLMYNL > 0
001882        MOVE BCLMYNI             TO AT-PRINT-CLM-FORM
001883     END-IF
001884
001885     IF BSRVYNL > 0
001886        MOVE BSRVYNI             TO AT-PRINT-SURVEY
001887     END-IF
001888
001889     IF BSPRELL > 0
001890        MOVE BSPRELI             TO AT-SPECIAL-RELEASE
001891     END-IF
001892
001893     IF BVOIDACL GREATER ZERO
001894        MOVE PI-AFTER-DATE-3     TO  AT-VOID-ACCEPT-DT.
001895
001896     IF BEXPTYPL GREATER ZERO
001897         MOVE BEXPTYPI           TO  AT-EXPENSE-TYPE.
001898
001899     IF BHOLDATL GREATER ZERO
001900         MOVE PI-HOLD-UNTIL-DATE TO  AT-TO-BE-WRITTEN-DT.
001901
001902     IF BCKQUEL GREATER ZERO
001903         MOVE WS-CHECK-QUE       TO  AT-CHECK-QUE-CONTROL.
001904
001905     IF BCKSEQL GREATER ZERO
001906         MOVE WS-CHECK-QUE-SEQ   TO  AT-CHECK-QUE-SEQUENCE.
001907
001908     IF BPMTORGL GREATER ZERO
001909         MOVE BPMTORGI           TO  AT-PAYMENT-ORIGIN.
001910
001911     MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
001912
001913     MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.
001914
001915     PERFORM 3100-REWRITE.
001916     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
001917
001918     MOVE 'S'                    TO  BMAINTO.
001919     MOVE -1                     TO  BMAINTL.
001920     MOVE AL-UANOF               TO  BMAINTA.
001921
001922     IF BNOTE1L IS GREATER THAN 0 OR
001923        BNOTE2L IS GREATER THAN 0
001924             GO TO 0200-UPDATE-PMNT-NOTE-TRLR
001925          ELSE
001926             PERFORM 8200-SEND-DATAONLY.
001927
001928 0200-UPDATE-PMNT-NOTE-TRLR.
001929
001930     
      * EXEC CICS READ
001931*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
001932*        RIDFLD    (PI-SAVE-KEY)
001933*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
001934*    END-EXEC.
      *    MOVE '&"S        E          (   #00010261' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130323631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001935
001936     IF AT-PAYMENT-NOTE-SEQ-NO = 0
001937         GO TO 0200-ADD-PMNT-NOTE-TRLR.
001938
001939     MOVE PI-SAVE-KEY            TO  WS-ACTIVITY-TRAILERS-KEY.
001940     MOVE AT-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO.
001941
001942     
      * EXEC CICS HANDLE CONDITION
001943*        NOTFND   (0200-ADD-PMNT-NOTE-TRLR)
001944*    END-EXEC.
      *    MOVE '"$I                   ! # #00010273' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303130323733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001945
001946     
      * EXEC CICS READ
001947*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
001948*        RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)
001949*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
001950*        UPDATE
001951*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010277' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130323737' TO DFHEIV0
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
           
001952
001953     IF BNOTE1L IS GREATER THAN 0
001954         MOVE BNOTE1I            TO  AT-INFO-LINE-1.
001955
001956     IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
001957       IF BNOTE1L IS GREATER THAN 0
001958         MOVE BNOTE1I            TO  WS-HAN-PAYMENT-NOTE
001959         IF WS-HAN-PMT-CODE = SPACE
001960             MOVE SPACES         TO  WS-HAN-PMT-TEXT
001961         ELSE
001962         IF WS-HAN-PMT-CODE = 'A'
001963             MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'
001964                                 TO  WS-HAN-PMT-TEXT
001965         ELSE
001966         IF WS-HAN-PMT-CODE = 'B'
001967             MOVE ':FINAL PAYMENT / DECEASED'
001968                                 TO  WS-HAN-PMT-TEXT
001969         ELSE
001970         IF WS-HAN-PMT-CODE = 'C'
001971             MOVE ':FINAL PAYMENT / NO LONGER DISABLED'
001972                                 TO  WS-HAN-PMT-TEXT
001973         ELSE
001974         IF WS-HAN-PMT-CODE = 'D'
001975             MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'
001976                                 TO  WS-HAN-PMT-TEXT
001977         ELSE
001978         IF WS-HAN-PMT-CODE = 'E'
001979             MOVE ':FINAL PAYMENT / RETURNED TO WORK'
001980                                 TO  WS-HAN-PMT-TEXT
001981         ELSE
001982         IF WS-HAN-PMT-CODE = 'F'
001983             MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'
001984                                 TO  WS-HAN-PMT-TEXT
001985         ELSE
001986         IF WS-HAN-PMT-CODE = 'P'
001987             MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.
001988
001989     IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
001990       IF BNOTE1L IS GREATER THAN 0
001991         MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I
001992                                       AT-INFO-LINE-1.
001993
001994     IF BNOTE2L IS GREATER THAN 0
001995         MOVE BNOTE2I            TO  AT-INFO-LINE-2.
001996
001997     MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
001998
001999     MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.
002000
002001     PERFORM 3100-REWRITE.
002002
002003     MOVE 'S'                    TO  BMAINTO.
002004     MOVE -1                     TO  BMAINTL.
002005     MOVE AL-UANOF               TO  BMAINTA.
002006
002007     PERFORM 8200-SEND-DATAONLY.
002008
002009 0200-ADD-PMNT-NOTE-TRLR.
002010
002011     PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
002012
002013     PERFORM 3000-READ-FOR-UPDATE.
002014
002015     MOVE CL-TRAILER-SEQ-CNT     TO  AT-PAYMENT-NOTE-SEQ-NO.
002016     SUBTRACT +1 FROM AT-PAYMENT-NOTE-SEQ-NO.
002017
002018     
      * EXEC CICS REWRITE
002019*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
002020*        FROM      (ACTIVITY-TRAILERS)
002021*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010349' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130333439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002022
002023     
      * EXEC CICS GETMAIN
002024*        LENGTH  (WS-ACTIVITY-TRAILERS-LENGTH)
002025*        INITIMG (WS-SPACES)
002026*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
002027*    END-EXEC.
      *    MOVE ',"IL                  $   #00010354' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130333534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ACTIVITY-TRAILERS-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002028
002029     MOVE 'AT'                   TO  AT-RECORD-ID.
002030     MOVE PI-SAVE-KEY            TO  AT-CONTROL-PRIMARY.
002031     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
002032     SUBTRACT +1 FROM AT-SEQUENCE-NO.
002033
002034     MOVE '6'                    TO  AT-TRAILER-TYPE.
002035
002036     MOVE WS-CURRENT-DATE        TO  AT-RECORDED-DT
002037                                     AT-PAYMENT-LAST-MAINT-DT.
002038     MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
002039                                     AT-PAYMENT-LAST-UPDATED-BY.
002040     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
002041
002042     MOVE SPACES                 TO  WS-HAN-PAYMENT-NOTE.
002043
002044     IF BNOTE1L IS GREATER THAN 0
002045         MOVE BNOTE1I            TO  AT-INFO-LINE-1
002046                                     WS-HAN-PAYMENT-NOTE.
002047
002048     IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
002049         IF WS-HAN-PMT-CODE = 'A'
002050             MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'
002051                                 TO  WS-HAN-PMT-TEXT
002052         ELSE
002053         IF WS-HAN-PMT-CODE = 'B'
002054             MOVE ':FINAL PAYMENT / DECEASED'
002055                                 TO  WS-HAN-PMT-TEXT
002056         ELSE
002057         IF WS-HAN-PMT-CODE = 'C'
002058             MOVE ':FINAL PAYMENT / NO LONGER DISABLED'
002059                                 TO  WS-HAN-PMT-TEXT
002060         ELSE
002061         IF WS-HAN-PMT-CODE = 'D'
002062             MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'
002063                                 TO  WS-HAN-PMT-TEXT
002064         ELSE
002065         IF WS-HAN-PMT-CODE = 'E'
002066             MOVE ':FINAL PAYMENT / RETURNED TO WORK'
002067                                 TO  WS-HAN-PMT-TEXT
002068         ELSE
002069         IF WS-HAN-PMT-CODE = 'F'
002070             MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'
002071                                 TO  WS-HAN-PMT-TEXT
002072         ELSE
002073         IF WS-HAN-PMT-CODE = 'P'
002074             MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.
002075
002076     IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
002077         MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I
002078                                       AT-INFO-LINE-1.
002079
002080     IF BNOTE2L IS GREATER THAN 0
002081         MOVE BNOTE2I            TO  AT-INFO-LINE-2.
002082
002083     MOVE 'P'                    TO  AT-INFO-TRAILER-TYPE.
002084
002085     
      * EXEC CICS WRITE
002086*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
002087*        FROM      (ACTIVITY-TRAILERS)
002088*        RIDFLD    (AT-CONTROL-PRIMARY)
002089*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010416' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130343136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002090
002091     SUBTRACT +1 FROM  CL-TRAILER-SEQ-CNT.
002092
002093     PERFORM 3600-REWRITE-ELMSTR.
002094
002095     MOVE 'S'                    TO  BMAINTO.
002096     MOVE -1                     TO  BMAINTL.
002097     MOVE AL-UANOF               TO  BMAINTA.
002098
002099     PERFORM 8200-SEND-DATAONLY.
002100
002101     EJECT
002102 0300-MAIN-LOGIC.
002103     IF PI-MAP-NAME NOT = EL142C
002104         GO TO 0400-MAIN-LOGIC.
002105
002106     PERFORM 4000-READ-TRAILER-FILE.
002107
002108     EJECT
002109 0400-MAIN-LOGIC.
002110     IF PI-MAP-NAME NOT = EL142D
002111         GO TO 0500-MAIN-LOGIC.
002112
002113     IF NOT MODIFY-CAP
002114         IF DMAINTI = 'S'
002115             NEXT SENTENCE
002116         ELSE
002117             MOVE 'UPDATE'       TO SM-READ
002118             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
002119             MOVE ER-0070        TO  EMI-ERROR
002120             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002121             GO TO 8100-SEND-INITIAL-MAP.
002122
002123     IF EIBAID = DFHPF6
002124         PERFORM 6000-DISPLAY-ELNAPS.
002125
002126     MOVE PI-COMPANY-ID          TO CNTL-CO.
002127     MOVE '1'                    TO CNTL-RECORD-TYPE.
002128     MOVE SPACES                 TO CNTL-GENL.
002129     MOVE ZEROS                  TO CNTL-SEQ.
002130
002131     
      * EXEC CICS READ
002132*        DATASET(WS-CONTROL-FILE-DSID)
002133*        SET    (ADDRESS OF CONTROL-FILE)
002134*        RIDFLD (WS-CONTROL-FILE-KEY)
002135*    END-EXEC.
      *    MOVE '&"S        E          (   #00010462' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130343632' TO DFHEIV0
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
           
002136
002137     MOVE +2                     TO  EMI-NUMBER-OF-LINES
002138                                     EMI-SWITCH2.
002139
002140     IF DMAINTL NOT GREATER ZERO
002141       OR (DMAINTL GREATER ZERO AND
002142           DMAINTI = 'S')
002143             PERFORM 4000-READ-TRAILER-FILE.
002144
002145     IF DMAINTI NOT = 'C'
002146         MOVE ER-0023            TO  EMI-ERROR
002147         MOVE -1                 TO  DMAINTL
002148         MOVE AL-UABON           TO  DMAINTA
002149         PERFORM 8200-SEND-DATAONLY.
002150
002151     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
002152           OR 'FNL'
002153        if (pi-el142-priority = '8')
002154           AND (PI-approval-level <> '5')
002155           MOVE ER-8003          TO EMI-ERROR
002156           MOVE -1               TO dMAINTL
002157           MOVE AL-UABON         TO dMAINTA
002158           GO TO 8200-SEND-DATAONLY.
002159
002160     MOVE AL-UANON               TO  DMAINTA
002161
002162*    NOTE *******************************************************
002163*         *                                                     *
002164*         *      THE FOLLOWING CHECK ALLOWS THE LGXX SIGNON     *
002165*         *  TO UPDATE THE ARCHIVE NUMBER, THE DATE SENT FIELD, *
002166*         *  THE INITIAL PRINT DATE FIELD, AND THE RESEND PRINT *
002167*         *  DATE FIELD.  IF NOT LGXX SIGNON, THE FIELDS ARE    *
002168*         *  SET TO ASKIP.                                      *
002169*         *                                                     *
002170*         *******************************************************.
002171
002172     IF PI-PROCESSOR-ID = 'LGXX'
002173         PERFORM 0450-MAIN-LOGIC
002174     ELSE
002175         MOVE AL-SANOF           TO  DARCHNOA
002176                                     DRESFRMA
002177                                     DAUTOCLA
002178                                     DDTSENTA
002179                                     DINPRNTA
002180                                     DREPRNTA.
002181
002182     IF DFORMNOL GREATER ZERO
002183        MOVE DFORMNOI            TO WS-FORM-NUMBER
002184        MOVE +1                  TO WS-UPDATE-SW.
002185
002186     IF DRESENDL GREATER ZERO
002187         IF DRESENDI = SPACES
002188             MOVE AL-UANON       TO  DRESENDA
002189             MOVE +1             TO  WS-UPDATE-SW
002190             MOVE LOW-VALUES     TO  WS-RESEND-DATE
002191         ELSE
002192             MOVE DRESENDI       TO  WS-DEEDIT-FIELD
002193             PERFORM 8600-DEEDIT
002194             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002195                 MOVE WS-DEEDIT-FIELD-V0  TO  DRESENDO
002196                 INSPECT DRESENDI CONVERTING SPACES TO SLASH
002197                 MOVE '4'        TO  DC-OPTION-CODE
002198                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002199                 PERFORM 8500-DATE-CONVERSION
002200                 IF DC-ERROR-CODE NOT = SPACES
002201                     MOVE ER-0295         TO  EMI-ERROR
002202                     MOVE -1              TO  DRESENDL
002203                     MOVE AL-UABON        TO  DRESENDA
002204                     PERFORM 9900-ERROR-FORMAT
002205                 ELSE
002206                     MOVE AL-UANON       TO  DRESENDA
002207                     MOVE +1             TO  WS-UPDATE-SW
002208                     MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE
002209             ELSE
002210                 MOVE ER-0295    TO  EMI-ERROR
002211                 MOVE -1         TO  DRESENDL
002212                 MOVE AL-UABON   TO  DRESENDA
002213                 PERFORM 9900-ERROR-FORMAT.
002214
002215     IF DREPLYL GREATER ZERO
002216         IF DREPLYI = SPACES
002217             MOVE AL-UANON       TO  DREPLYA
002218             MOVE +1             TO  WS-UPDATE-SW
002219             MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE
002220         ELSE
002221             MOVE DREPLYI        TO  WS-DEEDIT-FIELD
002222             PERFORM 8600-DEEDIT
002223             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002224                 MOVE WS-DEEDIT-FIELD-V0  TO  DREPLYO
002225                 INSPECT DREPLYI CONVERTING SPACES TO SLASH
002226                 MOVE '4'                 TO  DC-OPTION-CODE
002227                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002228                 PERFORM 8500-DATE-CONVERSION
002229                 IF DC-ERROR-CODE NOT = SPACES
002230                     MOVE ER-0296           TO  EMI-ERROR
002231                     MOVE -1             TO  DREPLYL
002232                     MOVE AL-UABON       TO  DREPLYA
002233                     PERFORM 9900-ERROR-FORMAT
002234                 ELSE
002235                     MOVE AL-UANON       TO  DREPLYA
002236                     MOVE +1             TO  WS-UPDATE-SW
002237                     MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE
002238             ELSE
002239                 MOVE ER-0296    TO  EMI-ERROR
002240                 MOVE -1         TO  DREPLYL
002241                 MOVE AL-UABON   TO  DREPLYA
002242                 PERFORM 9900-ERROR-FORMAT.
002243
002244     IF DRECEVEL GREATER ZERO
002245         IF DRECEVEI = SPACES
002246             MOVE AL-UANON       TO  DRECEVEA
002247             MOVE +1             TO  WS-UPDATE-SW
002248             MOVE LOW-VALUES     TO  WS-RECEIVED-DATE
002249         ELSE
002250             MOVE DRECEVEI       TO  WS-DEEDIT-FIELD
002251             PERFORM 8600-DEEDIT
002252             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002253                 MOVE WS-DEEDIT-FIELD-V0  TO  DRECEVEO
002254                 INSPECT DRECEVEI CONVERTING SPACES TO SLASH
002255                 MOVE '4'        TO  DC-OPTION-CODE
002256                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
002257                 PERFORM 8500-DATE-CONVERSION
002258                 IF DC-ERROR-CODE NOT = SPACES
002259                     MOVE ER-0297        TO  EMI-ERROR
002260                     MOVE -1             TO  DRECEVEL
002261                     MOVE AL-UABON       TO  DRECEVEA
002262                     PERFORM 9900-ERROR-FORMAT
002263                 ELSE
002264                     MOVE AL-UANON       TO  DRECEVEA
002265                     MOVE +1             TO  WS-UPDATE-SW
002266                     MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE
002267             ELSE
002268                 MOVE ER-0297    TO  EMI-ERROR
002269                 MOVE -1         TO  DRECEVEL
002270                 MOVE AL-UABON   TO  DRECEVEA
002271                 PERFORM 9900-ERROR-FORMAT.
002272
002273     IF DSTOPLTL GREATER ZERO
002274         IF DSTOPLTI = SPACES
002275             MOVE AL-UANON       TO  DSTOPLTA
002276             MOVE +1             TO  WS-UPDATE-SW
002277             MOVE LOW-VALUES     TO  WS-STOP-LETTER-DATE
002278         ELSE
002279             MOVE DSTOPLTI       TO  WS-DEEDIT-FIELD
002280             PERFORM 8600-DEEDIT
002281             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002282                 MOVE WS-DEEDIT-FIELD-V0  TO  DSTOPLTO
002283                 INSPECT DSTOPLTI CONVERTING SPACES TO SLASH
002284                 MOVE '4'        TO  DC-OPTION-CODE
002285                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002286                 PERFORM 8500-DATE-CONVERSION
002287                 IF DC-ERROR-CODE NOT = SPACES
002288                     MOVE ER-0897         TO  EMI-ERROR
002289                     MOVE -1              TO  DSTOPLTL
002290                     MOVE AL-UABON        TO  DSTOPLTA
002291                     PERFORM 9900-ERROR-FORMAT
002292                 ELSE
002293                     MOVE AL-UANON       TO  DSTOPLTA
002294                     MOVE +1             TO  WS-UPDATE-SW
002295                     MOVE DC-BIN-DATE-1  TO  WS-STOP-LETTER-DATE
002296             ELSE
002297                 MOVE ER-0897    TO  EMI-ERROR
002298                 MOVE -1         TO  DSTOPLTL
002299                 MOVE AL-UABON   TO  DSTOPLTA
002300                 PERFORM 9900-ERROR-FORMAT.
002301
002302     IF DREASONL GREATER ZERO
002303         MOVE +1                 TO  WS-UPDATE-SW.
002304
002305     IF DENCCODL GREATER THAN ZERO
002306        IF PI-CREATED-IN-NAPERSOFT = 'Y'
002307            MOVE DENCCODI TO WS-TEMP-ENCCODE
002308            IF WS-TEMP-ENCCODE (3:1) = 'X'
002309                IF WS-TEMP-ENCCODE (1:2) = 'EN'
002310                   MOVE 'V' TO WS-TEMP-ENCCODE (3:1)
002311                ELSE
002312                   MOVE SPACES TO WS-TEMP-ENCCODE (3:1)
002313                END-IF
002314            END-IF
002315            IF WS-TEMP-ENCCODE (2:1) = 'X'
002316                MOVE SPACES TO WS-TEMP-ENCCODE (2:1)
002317            END-IF
002318            IF WS-TEMP-ENCCODE <> PI-ENC-CODE
002319               MOVE ER-1568          TO EMI-ERROR
002320               MOVE -1               TO DENCCODL
002321               MOVE AL-UABON         TO DENCCODA
002322               PERFORM 9900-ERROR-FORMAT
002323            END-IF
002324        END-IF
002325        MOVE SPACES             TO WS-ELENCC-KEY
002326        MOVE PI-COMPANY-CD      TO WS-ELENCC-COMPANY-CD
002327        MOVE '1'                TO WS-ELENCC-REC-TYPE
002328        MOVE DENCCODI           TO WS-ELENCC-ENC-CODE
002329
002330        
      * EXEC CICS READ
002331*           DATASET    (WS-ELENCC-FILE-DSID)
002332*           SET        (ADDRESS OF ENCLOSURE-CODES)
002333*           RIDFLD     (WS-ELENCC-KEY)
002334*           RESP       (WS-RESPONSE)
002335*       END-EXEC
      *    MOVE '&"S        E          (  N#00010661' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303130363631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELENCC-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002336
002337        IF RESP-NORMAL
002338           MOVE DENCCODI         TO WS-ENCLOSURE-CODE
002339           MOVE AL-UANON         TO DENCCODA
002340           MOVE +1               TO WS-UPDATE-SW
002341        ELSE
002342           MOVE ER-1560          TO EMI-ERROR
002343           MOVE -1               TO DENCCODL
002344           MOVE AL-UABON         TO DENCCODA
002345           PERFORM 9900-ERROR-FORMAT
002346        END-IF
002347     END-IF
002348
002349
002350     IF WS-ERROR-COUNT GREATER ZERO
002351         PERFORM 8200-SEND-DATAONLY.
002352
002353     IF WS-UPDATE-SW NOT GREATER ZERO
002354         PERFORM 4000-READ-TRAILER-FILE.
002355
002356     IF DREPLYL GREATER ZERO
002357         PERFORM 3300-UPDATE-CLAIM-MASTER.
002358
002359     PERFORM 3000-READ-FOR-UPDATE.
002360
002361     IF DRESENDL GREATER ZERO AND
002362        AT-LETTER-ARCHIVE-NO LESS THAN CF-STARTING-ARCH-NO
002363          
      * EXEC CICS READ
002364*             UPDATE
002365*             DATASET (WS-CONTROL-FILE-DSID)
002366*             SET     (ADDRESS OF CONTROL-FILE)
002367*             RIDFLD  (WS-CONTROL-FILE-KEY)
002368*         END-EXEC
      *    MOVE '&"S        EU         (   #00010694' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130363934' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002369          MOVE AT-LETTER-ARCHIVE-NO TO CF-STARTING-ARCH-NO
002370          
      * EXEC CICS REWRITE
002371*             DATASET (WS-CONTROL-FILE-DSID)
002372*             FROM    (CONTROL-FILE)
002373*         END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010701' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130373031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002374
002375     IF DRESENDL GREATER ZERO
002376         MOVE WS-RESEND-DATE     TO  AT-AUTO-RE-SEND-DT
002377     END-IF.
002378*02028          MOVE LOW-VALUES         TO  AT-RESEND-PRINT-DATE
002379*02029          MOVE PI-COMPANY-CD      TO  WS-LA-COMPANY-CD
002380*02030          MOVE AT-LETTER-ARCHIVE-NO  TO  WS-LA-ARCHIVE-NO
002381*02031          MOVE '1'                TO  WS-LA-RECORD-TYPE
002382*02032          MOVE ZERO               TO  WS-LA-LINE-SEQ-NO
002383*02033          EXEC CICS READ UPDATE
002384*02034              DATASET (WS-LETTER-ARCHIVE-DSID)
002385*02035              RIDFLD  (WS-LETTER-ARCHIVE-KEY)
002386*02036              SET     (ADDRESS OF LETTER-ARCHIVE)
002387*02037          END-EXEC
002388*02038          MOVE WS-RESEND-DATE     TO  LA-RESEND-DATE
002389*02039          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE
002390*02040          EXEC CICS REWRITE
002391*02041              DATASET (WS-LETTER-ARCHIVE-DSID)
002392*02042              FROM    (LETTER-ARCHIVE)
002393*02043          END-EXEC
002394*02044          IF PI-COMPANY-ID = 'DMD'
002395*02045              EXEC CICS READ UPDATE
002396*02046                  DATASET (WS-ELARCT-FILE-ID)
002397*02047                  RIDFLD  (WS-LETTER-ARCHIVE-KEY)
002398*02048                  SET     (ADDRESS OF LETTER-ARCHIVE-TEMP)
002399*02049              END-EXEC
002400*02050              MOVE WS-RESEND-DATE     TO  LT-RESEND-DATE
002401*02051              MOVE LOW-VALUES         TO  LT-RESEND-PRINT-DA
002402*02052              EXEC CICS REWRITE
002403*02053                  DATASET (WS-ELARCT-FILE-ID)
002404*02054                  FROM    (LETTER-ARCHIVE-TEMP)
002405*02055              END-EXEC.
002406
002407     IF DREPLYL GREATER ZERO
002408         MOVE WS-FOLLOW-UP-DATE  TO  AT-RECEIPT-FOLLOW-UP.
002409
002410     IF DRESENDL GREATER ZERO OR DREPLYL GREATER ZERO
002411        OR DENCCODL GREATER ZERO
002412         MOVE PI-COMPANY-CD      TO  WS-NA-COMPANY-CD
002413         MOVE PI-CARRIER         TO  WS-NA-CARRIER
002414         MOVE PI-CLAIM-NO        TO  WS-NA-CLAIM-NO
002415         MOVE PI-CERT-NO         TO  WS-NA-CERT-NO
002416         MOVE AT-LETTER-ARCHIVE-NO  TO  WS-NA-ARCHIVE-NO
002417         
      * EXEC CICS READ UPDATE
002418*            DATASET (WS-NAPERSOFT-DSID)
002419*            RIDFLD  (WS-NAPERSOFT-KEY)
002420*            SET     (ADDRESS OF NAPERSOFT-FILE)
002421*        END-EXEC
      *    MOVE '&"S        EU         (   #00010748' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130373438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002422         IF DRESENDL GREATER ZERO
002423             MOVE WS-RESEND-DATE    TO  NA-RESEND-DT
002424         END-IF
002425         IF DREPLYL GREATER ZERO
002426             MOVE WS-FOLLOW-UP-DATE TO  NA-FOLLOW-UP-DT
002427         END-IF
002428         IF DENCCODL GREATER ZERO
002429             MOVE WS-ENCLOSURE-CODE TO  NA-ENCLOSURE-CD
002430         END-IF
002431         
      * EXEC CICS REWRITE
002432*            DATASET (WS-NAPERSOFT-DSID)
002433*            FROM    (NAPERSOFT-FILE)
002434*        END-EXEC
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010762' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130373632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002435     END-IF.
002436
002437     IF DRECEVEL GREATER ZERO
002438         MOVE WS-RECEIVED-DATE   TO  AT-LETTER-ANSWERED-DT.
002439
002440     IF DFORMNOL GREATER ZEROS
002441        MOVE WS-FORM-NUMBER      TO AT-STD-LETTER-FORM.
002442
002443     IF DARCHNOL GREATER ZERO
002444        MOVE DARCHNOI            TO  AT-LETTER-ARCHIVE-NO.
002445
002446     IF DDTSENTL GREATER ZERO
002447        MOVE WS-DATE-SENT        TO  AT-LETTER-SENT-DT.
002448
002449     IF DINPRNTL GREATER ZERO
002450        MOVE WS-IN-PRINT-DATE    TO  AT-INITIAL-PRINT-DATE.
002451
002452     IF DREPRNTL GREATER ZERO
002453        MOVE WS-REPRINTED-DATE   TO  AT-RESEND-PRINT-DATE.
002454
002455     IF DRESFRML GREATER ZERO
002456        MOVE DRESFRMI            TO  AT-RESEND-LETTER-FORM.
002457
002458     IF DAUTOCLL GREATER ZERO
002459        MOVE DAUTOCLI            TO  AT-AUTO-CLOSE-IND.
002460
002461     IF DSTOPLTL GREATER ZERO
002462        MOVE WS-STOP-LETTER-DATE TO  AT-STOP-LETTER-DT
002463     END-IF.
002464
002465     IF DREASONL GREATER ZERO
002466        MOVE DREASONI            TO  AT-REASON-TEXT.
002467
002468     IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
002469         MOVE AT-REASON-TEXT     TO  WS-REASON-TEXT
002470         IF WS-RE-NDX NUMERIC  AND
002471            WS-RE-NDX GREATER THAN ZERO  AND
002472            WS-RE-NDX LESS THAN 24
002473             MOVE HAN-REASON-TEXT (WS-RE-NDX)
002474                                 TO  AT-REASON-TEXT
002475                                     DREASONO.
002476
002477     MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.
002478
002479     MOVE WS-CURRENT-DATE        TO  AT-CORR-LAST-MAINT-DT.
002480
002481     PERFORM 3100-REWRITE.
002482     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
002483
002484     MOVE 'S'                    TO  DMAINTO.
002485     MOVE -1                     TO  DMAINTL.
002486     MOVE AL-UANOF               TO  DMAINTA.
002487     PERFORM 8200-SEND-DATAONLY.
002488
002489     EJECT
002490
002491 0450-MAIN-LOGIC.
002492     IF DARCHNOL GREATER ZERO
002493         MOVE DARCHNOI           TO  WS-DEEDIT-FIELD
002494         PERFORM 8600-DEEDIT
002495         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002496             MOVE WS-DEEDIT-FIELD-V0     TO  DARCHNOO
002497             MOVE AL-UNNON       TO  DARCHNOA
002498             MOVE +1             TO  WS-UPDATE-SW
002499         ELSE
002500             MOVE -1             TO  DARCHNOL
002501             MOVE AL-UNBON       TO  DARCHNOA
002502             MOVE ER-0175        TO  EMI-ERROR
002503             PERFORM 9900-ERROR-FORMAT.
002504
002505     IF DDTSENTL GREATER ZERO
002506         IF DDTSENTI = SPACES
002507             MOVE AL-UANON       TO  DDTSENTA
002508             MOVE +1             TO  WS-UPDATE-SW
002509             MOVE LOW-VALUES     TO  WS-DATE-SENT
002510         ELSE
002511             MOVE DDTSENTI       TO  WS-DEEDIT-FIELD
002512             PERFORM 8600-DEEDIT
002513             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002514                 MOVE WS-DEEDIT-FIELD-V0    TO  DDTSENTO
002515                 INSPECT DDTSENTI CONVERTING SPACES TO SLASH
002516                 MOVE '4'                   TO  DC-OPTION-CODE
002517                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
002518                 PERFORM 8500-DATE-CONVERSION
002519                 IF DC-ERROR-CODE NOT = SPACES
002520                     MOVE ER-0641           TO  EMI-ERROR
002521                     MOVE -1                TO  DDTSENTL
002522                     MOVE AL-UABON          TO  DDTSENTA
002523                     PERFORM 9900-ERROR-FORMAT
002524                 ELSE
002525                     MOVE AL-UANON       TO  DDTSENTA
002526                     MOVE +1             TO  WS-UPDATE-SW
002527                     MOVE DC-BIN-DATE-1  TO  WS-DATE-SENT
002528             ELSE
002529                 MOVE ER-0641        TO  EMI-ERROR
002530                 MOVE -1             TO  DDTSENTL
002531                 MOVE AL-UABON       TO  DDTSENTA
002532                 PERFORM 9900-ERROR-FORMAT.
002533
002534     IF DINPRNTL GREATER ZERO
002535         IF DINPRNTI = SPACES
002536             MOVE AL-UANON       TO  DINPRNTA
002537             MOVE +1             TO  WS-UPDATE-SW
002538             MOVE LOW-VALUES     TO  WS-IN-PRINT-DATE
002539         ELSE
002540             MOVE DINPRNTI       TO  WS-DEEDIT-FIELD
002541             PERFORM 8600-DEEDIT
002542             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002543                 MOVE WS-DEEDIT-FIELD-V0  TO  DINPRNTO
002544                 INSPECT DINPRNTI CONVERTING SPACES TO SLASH
002545                 MOVE '4'                 TO  DC-OPTION-CODE
002546                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002547                 PERFORM 8500-DATE-CONVERSION
002548                 IF DC-ERROR-CODE NOT = SPACES
002549                     MOVE ER-0642           TO  EMI-ERROR
002550                     MOVE -1                TO  DINPRNTL
002551                     MOVE AL-UABON          TO  DINPRNTA
002552                     PERFORM 9900-ERROR-FORMAT
002553                 ELSE
002554                     MOVE AL-UANON       TO  DINPRNTA
002555                     MOVE +1             TO  WS-UPDATE-SW
002556                     MOVE DC-BIN-DATE-1  TO  WS-IN-PRINT-DATE
002557             ELSE
002558                 MOVE ER-0642    TO  EMI-ERROR
002559                 MOVE -1         TO  DINPRNTL
002560                 MOVE AL-UABON   TO  DINPRNTA
002561                 PERFORM 9900-ERROR-FORMAT.
002562
002563     IF DREPRNTL GREATER ZERO
002564         IF DREPRNTI = SPACES
002565             MOVE AL-UANON       TO  DREPRNTA
002566             MOVE +1             TO  WS-UPDATE-SW
002567             MOVE LOW-VALUES     TO  WS-REPRINTED-DATE
002568         ELSE
002569             MOVE DREPRNTI       TO  WS-DEEDIT-FIELD
002570             PERFORM 8600-DEEDIT
002571             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002572                 MOVE WS-DEEDIT-FIELD-V0  TO  DREPRNTO
002573                 INSPECT DREPRNTI CONVERTING SPACES TO SLASH
002574                 MOVE '4'                 TO  DC-OPTION-CODE
002575                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
002576                 PERFORM 8500-DATE-CONVERSION
002577                 IF DC-ERROR-CODE NOT = SPACES
002578                     MOVE ER-0643        TO  EMI-ERROR
002579                     MOVE -1             TO  DREPRNTL
002580                     MOVE AL-UABON       TO  DREPRNTA
002581                     PERFORM 9900-ERROR-FORMAT
002582                 ELSE
002583                     MOVE AL-UANON       TO  DREPRNTA
002584                     MOVE +1             TO  WS-UPDATE-SW
002585                     MOVE DC-BIN-DATE-1  TO  WS-REPRINTED-DATE
002586             ELSE
002587                 MOVE ER-0643       TO  EMI-ERROR
002588                 MOVE -1             TO  DREPRNTL
002589                 MOVE AL-UABON       TO  DREPRNTA
002590                 PERFORM 9900-ERROR-FORMAT.
002591
002592     EJECT
002593 0500-MAIN-LOGIC.
002594     IF PI-MAP-NAME NOT = EL142E
002595         GO TO 0600-MAIN-LOGIC.
002596
002597     IF NOT MODIFY-CAP
002598         IF EMAINTI = 'S'
002599             NEXT SENTENCE
002600         ELSE
002601             MOVE 'UPDATE'       TO SM-READ
002602             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
002603             MOVE ER-0070        TO  EMI-ERROR
002604             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002605             GO TO 8100-SEND-INITIAL-MAP.
002606
002607     MOVE +2                     TO  EMI-NUMBER-OF-LINES
002608                                     EMI-SWITCH2
002609
002610     IF EMAINTL NOT GREATER ZERO
002611       OR (EMAINTL GREATER ZERO AND
002612           EMAINTI = 'S')
002613             PERFORM 4000-READ-TRAILER-FILE.
002614
002615     IF EMAINTI = 'C' OR 'D'
002616         NEXT SENTENCE
002617     ELSE
002618         MOVE ER-0023            TO  EMI-ERROR
002619         MOVE -1                 TO  EMAINTL
002620         MOVE AL-UABON           TO  EMAINTA
002621         PERFORM 8200-SEND-DATAONLY.
002622
002623     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
002624           OR 'FNL'
002625        if (pi-el142-priority = '8')
002626           AND (PI-approval-level <> '5')
002627           MOVE ER-8003          TO EMI-ERROR
002628           MOVE -1               TO EMAINTL
002629           MOVE AL-UABON         TO EMAINTA
002630           GO TO 8200-SEND-DATAONLY.
002631
002632     IF PI-COMPANY-ID EQUAL 'DMD'
002633      IF SYSTEM-MODIFY-CAP OR
002634        PI-PROCESSOR-ID = 'LGXX'
002635         NEXT SENTENCE
002636       ELSE
002637         IF (PI-PROCESSOR-ID NOT = PI-SAVE-LAST-UPD-BY)
002638                                  OR
002639            (WS-CURRENT-DATE NOT = PI-SAVE-LAST-MAINT-DT)
002640              MOVE ER-8003        TO EMI-ERROR
002641              MOVE -1             TO EMAINTL
002642              MOVE AL-UABON       TO EMAINTA
002643              GO TO 8200-SEND-DATAONLY.
002644
002645     IF EMAINTI = 'D'
002646         PERFORM 3000-READ-FOR-UPDATE
002647         PERFORM 3200-DELETE
002648         MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
002649         MOVE -1                 TO  EPFKL
002650         MOVE 'S'                TO  EMAINTO
002651         MOVE AL-SANOF           TO  EMAINTA  ELINE1A  ELINE2A
002652         PERFORM 8200-SEND-DATAONLY.
002653
002654     MOVE AL-UANON               TO  EMAINTA.
002655
002656     IF ELINE1L GREATER ZERO OR
002657        ELINE2L GREATER ZERO
002658          NEXT SENTENCE
002659     ELSE
002660          PERFORM 4000-READ-TRAILER-FILE.
002661
002662     PERFORM 3000-READ-FOR-UPDATE.
002663
002664     IF ELINE1L GREATER ZERO
002665         MOVE ELINE1I            TO  AT-INFO-LINE-1.
002666
002667     IF ELINE2L GREATER ZERO
002668         MOVE ELINE2I            TO  AT-INFO-LINE-2.
002669
002670     MOVE PI-PROCESSOR-ID        TO  AT-GEN-INFO-LAST-UPDATED-BY.
002671
002672     MOVE WS-CURRENT-DATE        TO  AT-GEN-INFO-LAST-MAINT-DT.
002673
002674     PERFORM 3100-REWRITE.
002675     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
002676
002677     MOVE 'S'                    TO  EMAINTO.
002678     MOVE -1                     TO  EMAINTL.
002679     MOVE AL-UANOF               TO  EMAINTA.
002680     PERFORM 8200-SEND-DATAONLY.
002681
002682     EJECT
002683 0600-MAIN-LOGIC.
002684     IF PI-MAP-NAME NOT = EL142F
002685         GO TO 0700-MAIN-LOGIC.
002686
002687     IF NOT MODIFY-CAP
002688         IF FMAINTI = 'S'
002689             NEXT SENTENCE
002690         ELSE
002691             MOVE 'UPDATE'       TO SM-READ
002692             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
002693             MOVE ER-0070        TO  EMI-ERROR
002694             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002695             GO TO 8100-SEND-INITIAL-MAP.
002696
002697     MOVE +2                     TO  EMI-NUMBER-OF-LINES
002698                                     EMI-SWITCH2.
002699
002700     IF FMAINTL NOT GREATER ZERO
002701       OR (FMAINTL GREATER ZERO AND
002702           FMAINTI = 'S')
002703             PERFORM 4000-READ-TRAILER-FILE.
002704
002705     IF FMAINTI = 'C' OR 'D'
002706         NEXT SENTENCE
002707     ELSE
002708         MOVE ER-0023            TO  EMI-ERROR
002709         MOVE -1                 TO  FMAINTL
002710         MOVE AL-UABON           TO  FMAINTA
002711         PERFORM 8200-SEND-DATAONLY.
002712
002713     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
002714           OR 'FNL'
002715        if (pi-el142-priority = '8')
002716           AND (PI-approval-level <> '5')
002717           MOVE ER-8003          TO EMI-ERROR
002718           MOVE -1               TO fMAINTL
002719           MOVE AL-UABON         TO fMAINTA
002720           GO TO 8200-SEND-DATAONLY.
002721
002722     IF FMAINTI = 'D'
002723         PERFORM 3000-READ-FOR-UPDATE
002724         PERFORM 3200-DELETE
002725         MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
002726         MOVE -1                 TO  FPFKL
002727         MOVE 'S'                TO  FMAINTO
002728         MOVE AL-SANOF           TO  FMAINTA  FSNOTIFA  FENOTIFA
002729                                     FLINE1A  FLINE2A
002730         PERFORM 8200-SEND-DATAONLY.
002731
002732     MOVE AL-UANON               TO  FMAINTA.
002733
002734     IF FSNOTIFL GREATER ZERO
002735         MOVE FSNOTIFI           TO  WS-DEEDIT-FIELD
002736         PERFORM 8600-DEEDIT
002737         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002738             MOVE WS-DEEDIT-FIELD-V0  TO  FSNOTIFO
002739             INSPECT FSNOTIFI CONVERTING SPACES TO SLASH
002740             MOVE '4'                 TO  DC-OPTION-CODE
002741             MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002742             PERFORM 8500-DATE-CONVERSION
002743             IF DC-ERROR-CODE NOT = SPACES
002744                 MOVE ER-0298        TO  EMI-ERROR
002745                 MOVE -1             TO  FSNOTIFL
002746                 MOVE AL-UABON       TO  FSNOTIFA
002747                 PERFORM 9900-ERROR-FORMAT
002748             ELSE
002749                 MOVE AL-UANON       TO  FSNOTIFA
002750                 MOVE +1             TO  WS-UPDATE-SW
002751                 MOVE DC-BIN-DATE-1  TO  WS-START-DATE
002752         ELSE
002753             MOVE ER-0298        TO  EMI-ERROR
002754             MOVE -1             TO  FSNOTIFL
002755             MOVE AL-UABON       TO  FSNOTIFA
002756             PERFORM 9900-ERROR-FORMAT.
002757
002758     IF FENOTIFL GREATER ZERO
002759         MOVE FENOTIFI           TO  WS-DEEDIT-FIELD
002760         PERFORM 8600-DEEDIT
002761         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002762             MOVE WS-DEEDIT-FIELD-V0  TO  FENOTIFO
002763             INSPECT FENOTIFI CONVERTING SPACES TO SLASH
002764             MOVE '4'                 TO  DC-OPTION-CODE
002765             MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
002766             PERFORM 8500-DATE-CONVERSION
002767             IF DC-ERROR-CODE NOT = SPACES
002768                 MOVE ER-0299           TO  EMI-ERROR
002769                 MOVE -1                TO  FENOTIFL
002770                 MOVE AL-UABON          TO  FENOTIFA
002771                 PERFORM 9900-ERROR-FORMAT
002772             ELSE
002773                 MOVE AL-UANON       TO  FENOTIFA
002774                 MOVE +1             TO  WS-UPDATE-SW
002775                 MOVE DC-BIN-DATE-1  TO  WS-END-DATE
002776         ELSE
002777             MOVE ER-0299        TO  EMI-ERROR
002778             MOVE -1             TO  FENOTIFL
002779             MOVE AL-UABON       TO  FENOTIFA
002780             PERFORM 9900-ERROR-FORMAT.
002781
002782     IF FLINE1L GREATER ZERO
002783       OR FLINE2L GREATER ZERO
002784         MOVE +1                 TO  WS-UPDATE-SW.
002785
002786     IF WS-ERROR-COUNT GREATER ZERO
002787         PERFORM 8200-SEND-DATAONLY.
002788
002789     IF WS-UPDATE-SW NOT GREATER ZERO
002790         PERFORM 4000-READ-TRAILER-FILE.
002791
002792     IF FSNOTIFL GREATER ZERO
002793         PERFORM 3300-UPDATE-CLAIM-MASTER.
002794
002795     PERFORM 3000-READ-FOR-UPDATE.
002796
002797     IF FLINE1L GREATER ZERO
002798         MOVE FLINE1I            TO  AT-PROMPT-LINE-1.
002799
002800     IF FLINE2L GREATER ZERO
002801         MOVE FLINE2I            TO  AT-PROMPT-LINE-2.
002802
002803     IF FSNOTIFL GREATER ZERO
002804         MOVE WS-START-DATE      TO  AT-PROMPT-START-DT.
002805
002806     IF FENOTIFL GREATER ZERO
002807         MOVE WS-END-DATE        TO  AT-PROMPT-END-DT.
002808
002809     MOVE PI-PROCESSOR-ID        TO  AT-PROMPT-LAST-UPDATED-BY.
002810
002811     MOVE WS-CURRENT-DATE        TO  AT-PROMPT-LAST-MAINT-DT.
002812
002813     PERFORM 3100-REWRITE.
002814     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
002815
002816     MOVE 'S'                    TO  FMAINTO.
002817     MOVE -1                     TO  FMAINTL.
002818     MOVE AL-UANOF               TO  FMAINTA.
002819     PERFORM 8200-SEND-DATAONLY.
002820
002821     EJECT
002822 0700-MAIN-LOGIC.
002823     IF PI-MAP-NAME NOT = EL142G
002824         GO TO 0800-MAIN-LOGIC.
002825
002826     IF NOT MODIFY-CAP
002827         IF GMAINTI = 'S'
002828             NEXT SENTENCE
002829         ELSE
002830             MOVE 'UPDATE'       TO SM-READ
002831             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
002832             MOVE ER-0070        TO  EMI-ERROR
002833             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002834             GO TO 8100-SEND-INITIAL-MAP.
002835
002836     MOVE +2                     TO  EMI-NUMBER-OF-LINES
002837                                     EMI-SWITCH2.
002838
002839     IF GMAINTL NOT GREATER ZERO
002840       OR (GMAINTL GREATER ZERO AND
002841           GMAINTI = 'S')
002842             PERFORM 4000-READ-TRAILER-FILE.
002843
002844     IF GMAINTI NOT = 'C' AND 'D'
002845         MOVE ER-0023            TO  EMI-ERROR
002846         MOVE -1                 TO  GMAINTL
002847         MOVE AL-UABON           TO  GMAINTA
002848         PERFORM 8200-SEND-DATAONLY.
002849
002850     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
002851           OR 'FNL'
002852        if (pi-el142-priority = '8')
002853           AND (PI-approval-level <> '5')
002854           MOVE ER-8003          TO EMI-ERROR
002855           MOVE -1               TO gMAINTL
002856           MOVE AL-UABON         TO gMAINTA
002857           GO TO 8200-SEND-DATAONLY.
002858
002859     IF GMAINTI = 'D'
002860        if pi-approval-level <> '5'
002861           MOVE ER-0310            TO  EMI-ERROR
002862           MOVE -1                 TO  GMAINTL
002863           MOVE AL-UABON           TO  GMAINTA
002864           PERFORM 8200-SEND-DATAONLY
002865        ELSE
002866           ADD 1 TO PI-MAPG-DELETE-CNT
002867           IF PI-MAPG-DELETE-CNT > 1
002868              PERFORM 3000-READ-FOR-UPDATE
002869              PERFORM 3200-DELETE
002870              PERFORM 3500-READ-ELMSTR-FOR-UPDATE
002871                                 THRU 3599-EXIT
002872              MOVE SPACES        TO CL-DENIAL-TYPE
002873              MOVE 'O'           TO CL-CLAIM-STATUS
002874              
      * EXEC CICS REWRITE
002875*                DATASET (WS-CLAIM-MASTER-DSID)
002876*                FROM    (CLAIM-MASTER)
002877*             END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011205' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131323035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002878              MOVE 'O'              TO WS-CLAIM-TYPE
002879              PERFORM 0710-UPDATE-ZERO-TRLR THRU 0730-MAIN-LOGIC
002880              MOVE +1               TO WS-COMPLETED-SUCCESSFUL
002881              MOVE -1               TO GPFKL
002882              MOVE 'S'              TO GMAINTO
002883              MOVE AL-SANOF         TO GMAINTA  GLINE1A  GLINE2A
002884              PERFORM 8200-SEND-DATAONLY
002885           ELSE
002886              MOVE ER-0755            TO  EMI-ERROR
002887              MOVE -1                 TO  GMAINTL
002888              MOVE AL-UABOF           TO  GMAINTA
002889              PERFORM 8200-SEND-DATAONLY
002890           END-IF
002891        END-IF
002892     END-IF
002893
002894     MOVE AL-UANON               TO  GMAINTA.
002895
002896     IF GRECONSL GREATER ZERO
002897         IF GRECONSI = SPACES
002898             MOVE AL-UANON       TO  GRECONSA
002899             MOVE +1             TO  WS-UPDATE-SW
002900             MOVE LOW-VALUES     TO  WS-END-DATE
002901             MOVE +4             TO  GRSNCDL
002902         ELSE
002903             MOVE +0             TO  GRSNCDL
002904             MOVE GRECONSI           TO  WS-DEEDIT-FIELD
002905             PERFORM 8600-DEEDIT
002906             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
002907                 MOVE WS-DEEDIT-FIELD-V0  TO  GRECONSO
002908                 INSPECT GRECONSI CONVERTING SPACES TO SLASH
002909                 MOVE '4'                 TO  DC-OPTION-CODE
002910                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
002911                 PERFORM 8500-DATE-CONVERSION
002912                 IF DC-ERROR-CODE NOT = SPACES
002913                     MOVE ER-0300        TO  EMI-ERROR
002914                     MOVE -1             TO  GRECONSL
002915                     MOVE AL-UABON       TO  GRECONSA
002916                     PERFORM 9900-ERROR-FORMAT
002917                 ELSE
002918                     MOVE AL-UANON       TO  GRECONSA
002919                     MOVE +1             TO  WS-UPDATE-SW
002920                     MOVE DC-BIN-DATE-1  TO  WS-END-DATE
002921             ELSE
002922                 MOVE ER-0300        TO  EMI-ERROR
002923                 MOVE -1             TO  GRECONSL
002924                 MOVE AL-UNBON       TO  GRECONSA
002925                 PERFORM 9900-ERROR-FORMAT.
002926
002927     IF WS-END-DATE GREATER THAN WS-CURRENT-DATE
002928        MOVE ER-0300             TO  EMI-ERROR
002929        MOVE -1                  TO  GRECONSL
002930        MOVE AL-UABON            TO  GRECONSA
002931        PERFORM 9900-ERROR-FORMAT.
002932
002933     IF  GPRFDTL > 0
002934         MOVE GPRFDTI             TO WS-DEEDIT-FIELD
002935         PERFORM 8600-DEEDIT
002936         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
002937         MOVE '4'                 TO DC-OPTION-CODE
002938         PERFORM 8500-DATE-CONVERSION
002939         IF DC-ERROR-CODE NOT = SPACES
002940            MOVE ER-0021          TO EMI-ERROR
002941            MOVE -1               TO GPRFDTL
002942            MOVE AL-UABON         TO GPRFDTA
002943            PERFORM 9900-ERROR-FORMAT
002944         ELSE
002945            IF (DC-BIN-DATE-1 > WS-CURRENT-DATE)
002946               or (dc-bin-date-1 > pi-den-recorded-dt)
002947               or (dc-bin-date-1 < pi-incurred-dt)
002948                MOVE ER-0873      TO EMI-ERROR
002949                MOVE -1           TO GPRFDTL
002950                MOVE AL-UABON     TO GPRFDTA
002951                PERFORM 9900-ERROR-FORMAT
002952            ELSE
002953                MOVE AL-UANON       TO  GPRFDTA
002954                MOVE +1             TO  WS-UPDATE-SW
002955                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
002956                MOVE WS-DEEDIT-FIELD-V0  TO GPRFDTO
002957                INSPECT GPRFDTI CONVERTING ' ' TO '/'.
002958
002959     IF GLINE1L GREATER ZERO OR
002960        GLINE2L GREATER ZERO
002961           MOVE +1               TO  WS-UPDATE-SW.
002962
002963     IF WS-ERROR-COUNT GREATER ZERO
002964         PERFORM 8200-SEND-DATAONLY.
002965
002966     IF GRSNCDL GREATER ZERO
002967         MOVE +1 TO WS-UPDATE-SW.
002968
002969     IF WS-UPDATE-SW NOT GREATER ZERO
002970         PERFORM 4000-READ-TRAILER-FILE.
002971
002972     IF GRECONSL NOT GREATER ZERO
002973         GO TO 0740-MAIN-LOGIC.
002974
002975     MOVE PI-SAVE-KEY            TO PI-ACTIVITY-TRAILERS-KEY.
002976
002977     PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
002978
002979     IF PI-COMPANY-ID = 'DMD'
002980         MOVE CL-CERT-KEY-DATA     TO WS-CL-CERT-KEY-DATA
002981         MOVE CL-CERT-NO           TO WS-CL-CERT-NO
002982         MOVE CL-BENEFICIARY       TO WS-CL-BENEFICIARY
002983         MOVE CL-CCN               TO WS-CL-CCN
002984         MOVE CL-CLAIM-NO          TO WS-CL-CLAIM-NO
002985         MOVE CL-CLAIM-TYPE        TO WS-CL-CLAIM-TYPE
002986         MOVE CL-INSURED-LAST-NAME TO WS-CL-INSURED-LAST-NAME
002987         MOVE CL-INSURED-1ST-NAME  TO WS-CL-INSURED-1ST-NAME
002988         MOVE CL-INSURED-MID-INIT  TO WS-CL-INSURED-MID-INIT
002989         MOVE CL-NO-OF-PMTS-MADE   TO WS-CL-NO-OF-PMTS-MADE.
002990
002991     IF WS-END-DATE = LOW-VALUES
002992        IF CLAIM-IS-CLOSED
002993            
      * EXEC CICS UNLOCK
002994*                DATASET   (WS-CLAIM-MASTER-DSID)
002995*           END-EXEC
      *    MOVE '&*                    #   #00011324' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002996            GO TO 0740-MAIN-LOGIC
002997        ELSE
002998           MOVE 'C'              TO CL-CLAIM-STATUS
002999           MOVE WS-CURRENT-DATE  TO CL-LAST-CLOSE-DT
003000     ELSE
003001        IF CLAIM-IS-OPEN
003002            
      * EXEC CICS UNLOCK
003003*                DATASET   (WS-CLAIM-MASTER-DSID)
003004*           END-EXEC
      *    MOVE '&*                    #   #00011333' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131333333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003005            GO TO 0740-MAIN-LOGIC
003006        ELSE
003007           MOVE 'O'              TO CL-CLAIM-STATUS
003008           MOVE '5'              TO CL-DENIAL-TYPE
003009           MOVE WS-CURRENT-DATE  TO CL-LAST-REOPEN-DT.
003010
003011     MOVE CL-CLAIM-STATUS        TO  WS-CLAIM-TYPE.
003012
003013     PERFORM 3600-REWRITE-ELMSTR.
003014
003015 0710-UPDATE-ZERO-TRLR.
003016
003017     MOVE PI-SAVE-KEY              TO  WS-ACTIVITY-TRAILERS-KEY.
003018     MOVE ZERO                     TO  WS-ATK-SEQUENCE-NO.
003019
003020
003021     
      * EXEC CICS READ UPDATE
003022*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
003023*        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
003024*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
003025*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011352' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131333532' TO DFHEIV0
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
           
003026
003027     MOVE +1                     TO  WS-INDEX.
003028
003029 0720-MAIN-LOGIC.
003030
003031     IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = SPACES
003032        IF WS-INDEX GREATER +1
003033           SUBTRACT +1 FROM WS-INDEX
003034           IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = WS-CLAIM-TYPE
003035              MOVE WS-CURRENT-DATE TO
003036                   AT-OPEN-CLOSE-DATE (WS-INDEX)
003037              GO TO 0730-MAIN-LOGIC
003038           ELSE
003039              ADD +1 TO WS-INDEX
003040              MOVE WS-CURRENT-DATE  TO
003041                     AT-OPEN-CLOSE-DATE (WS-INDEX)
003042              MOVE WS-CLAIM-TYPE    TO
003043                     AT-OPEN-CLOSE-TYPE (WS-INDEX)
003044              MOVE 'ALTER'          TO
003045                     AT-OPEN-CLOSE-REASON (WS-INDEX)
003046              GO TO 0730-MAIN-LOGIC.
003047
003048     IF WS-INDEX LESS THAN +6
003049         ADD +1                  TO  WS-INDEX
003050         GO TO 0720-MAIN-LOGIC.
003051
003052     IF AT-OPEN-CLOSE-TYPE (6) = WS-CLAIM-TYPE
003053        MOVE WS-CURRENT-DATE     TO AT-OPEN-CLOSE-DATE (6)
003054        GO TO 0730-MAIN-LOGIC.
003055
003056     MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1).
003057     MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2).
003058     MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3).
003059     MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4).
003060     MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5).
003061
003062     MOVE WS-CURRENT-DATE        TO  AT-OPEN-CLOSE-DATE (6).
003063     MOVE WS-CLAIM-TYPE          TO  AT-OPEN-CLOSE-TYPE (6).
003064     MOVE 'ALTER'                TO  AT-OPEN-CLOSE-REASON (6).
003065
003066 0730-MAIN-LOGIC.
003067
003068     MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY.
003069     MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.
003070
003071     
      * EXEC CICS REWRITE
003072*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
003073*        FROM    (ACTIVITY-TRAILERS)
003074*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011402' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131343032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003075
003076 0740-MAIN-LOGIC.
003077
003078     PERFORM 3000-READ-FOR-UPDATE.
003079
003080     IF GLINE1L GREATER ZERO
003081         MOVE GLINE1I            TO  AT-DENIAL-INFO-1.
003082
003083     IF GLINE2L GREATER ZERO
003084         MOVE GLINE2I            TO  AT-DENIAL-INFO-2.
003085
003086     IF GRSNCDL > +0
003087        MOVE LOW-VALUES          TO WS-ELDENY-KEY
003088        MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
003089        MOVE GRSNCDI             TO ELDENY-DENIAL-CODE
003090        
      * EXEC CICS READ
003091*          DATASET('ELDENY')
003092*          SET    (ADDRESS OF DENIAL-CODES)
003093*          RIDFLD (WS-ELDENY-KEY)
003094*          RESP   (WS-RESPONSE)
003095*       END-EXEC
           MOVE 'ELDENY' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011421' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131343231' TO DFHEIV0
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
003096        IF RESP-NORMAL
003097           MOVE GRSNCDI          TO AT-DENIAL-REASON-CODE
003098           IF AT-DENIAL-REASON-CODE NOT = PI-DENIAL-REASON-CODE
003099              IF GLINE1L > +0
003100                 STRING DN-DESCRIPTION ' ' GLINE1I
003101                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
003102                 END-STRING
003103              ELSE
003104                 STRING AT-DENIAL-INFO-1 ' ' DN-DESCRIPTION
003105                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
003106                 END-STRING
003107              END-IF
003108           END-IF
003109           PERFORM 3500-READ-ELMSTR-FOR-UPDATE
003110                                 THRU 3599-EXIT
003111           MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
003112           
      * EXEC CICS REWRITE
003113*             DATASET (WS-CLAIM-MASTER-DSID)
003114*             FROM    (CLAIM-MASTER)
003115*          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011443' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131343433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003116           MOVE AT-DENIAL-INFO-1 TO GLINE1O
003117           MOVE +1               TO GLINE1L
003118           MOVE AL-UANON         TO GLINE1A
003119        ELSE
003120           MOVE AL-UABON         TO GRSNCDA
003121           MOVE -1               TO GRSNCDL
003122           MOVE ER-0884          TO EMI-ERROR
003123           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003124           
      * EXEC CICS UNLOCK
003125*             DATASET (WS-ACTIVITY-TRAILERS-DSID)
003126*          END-EXEC
      *    MOVE '&*                    #   #00011455' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003127           GO TO 8200-SEND-DATAONLY
003128        END-IF
003129     END-IF
003130
003131     IF GRECONSL GREATER ZERO
003132         MOVE WS-END-DATE        TO  AT-RETRACTION-DT
003133         IF PI-COMPANY-ID = 'DMD'
003134             PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.
003135
003136     IF GPRFDTL GREATER ZERO
003137         MOVE WS-PRF-DT          TO  AT-DENIAL-PROOF-DT.
003138
003139     MOVE PI-PROCESSOR-ID        TO  AT-DENIAL-LAST-UPDATED-BY.
003140
003141     MOVE WS-CURRENT-DATE        TO  AT-DENIAL-LAST-MAINT-DT.
003142
003143     PERFORM 3100-REWRITE.
003144     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
003145
003146     MOVE 'S'                    TO  GMAINTO.
003147     MOVE -1                     TO  GMAINTL.
003148     MOVE AL-UANOF               TO  GMAINTA.
003149     PERFORM 8200-SEND-DATAONLY.
003150
003151     EJECT
003152 0800-MAIN-LOGIC.
003153     IF PI-MAP-NAME NOT = EL142H
003154         GO TO 0900-MAIN-LOGIC.
003155
003156     IF NOT MODIFY-CAP
003157         IF HMAINTI = 'S'
003158             NEXT SENTENCE
003159         ELSE
003160             MOVE 'UPDATE'       TO SM-READ
003161             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
003162             MOVE ER-0070        TO  EMI-ERROR
003163             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003164             GO TO 8100-SEND-INITIAL-MAP.
003165
003166     MOVE +2                     TO  EMI-NUMBER-OF-LINES
003167                                     EMI-SWITCH2.
003168
003169     IF HMAINTL NOT GREATER ZERO
003170       OR (HMAINTL GREATER ZERO AND
003171           HMAINTI = 'S')
003172             PERFORM 4000-READ-TRAILER-FILE.
003173
003174     IF HMAINTI NOT = 'C'
003175         MOVE ER-0023            TO  EMI-ERROR
003176         MOVE -1                 TO  HMAINTL
003177         MOVE AL-UABON           TO  HMAINTA
003178         PERFORM 8200-SEND-DATAONLY.
003179
003180     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
003181           OR 'FNL'
003182        if (pi-el142-priority = '8')
003183           AND (PI-approval-level <> '5')
003184           MOVE ER-8003          TO EMI-ERROR
003185           MOVE -1               TO hMAINTL
003186           MOVE AL-UABON         TO hMAINTA
003187           GO TO 8200-SEND-DATAONLY.
003188
003189     MOVE AL-UANON               TO  HMAINTA.
003190
003191     IF HRESMANL GREATER ZERO
003192         IF HRESMANI = 'Y' OR 'N'
003193             MOVE AL-UANON       TO  HRESMANA
003194         ELSE
003195             MOVE -1             TO  HRESMANL
003196             MOVE AL-UABON       TO  HRESMANA
003197             MOVE ER-0107        TO  EMI-ERROR
003198             PERFORM 9900-ERROR-FORMAT.
003199
003200     IF HRESFUTL GREATER ZERO
003201         IF HRESFUTI = 'Y' OR 'N'
003202             MOVE AL-UANON       TO  HRESFUTA
003203         ELSE
003204             MOVE -1             TO  HRESFUTL
003205             MOVE AL-UABON       TO  HRESFUTA
003206             MOVE ER-0109        TO  EMI-ERROR
003207             PERFORM 9900-ERROR-FORMAT.
003208
003209     IF HRESIBNL GREATER ZERO
003210         IF HRESIBNI = 'Y' OR 'N'
003211             MOVE AL-UANON       TO  HRESIBNA
003212         ELSE
003213             MOVE -1             TO  HRESIBNL
003214             MOVE AL-UABON       TO  HRESIBNA
003215             MOVE ER-0111        TO  EMI-ERROR
003216             PERFORM 9900-ERROR-FORMAT.
003217
003218     IF HRESLFPL GREATER ZERO
003219         IF HRESLFPI = 'Y' OR 'N'
003220             MOVE AL-UANON       TO  HRESLFPA
003221         ELSE
003222             MOVE -1             TO  HRESLFPL
003223             MOVE AL-UABON       TO  HRESLFPA
003224             MOVE ER-0324        TO  EMI-ERROR
003225             PERFORM 9900-ERROR-FORMAT.
003226
003227     IF HRESAHPL GREATER ZERO
003228         IF HRESAHPI = 'Y' OR 'N'
003229             MOVE AL-UANON       TO  HRESAHPA
003230         ELSE
003231             MOVE -1             TO  HRESAHPL
003232             MOVE AL-UABON       TO  HRESAHPA
003233             MOVE ER-0325        TO  EMI-ERROR
003234             PERFORM 9900-ERROR-FORMAT.
003235
003236     IF HCDTAML GREATER ZERO
003237         IF HCDTAMI = '1' OR '2' OR '3'
003238             MOVE AL-UNNON       TO  HCDTAMA
003239         ELSE
003240             MOVE AL-UNBON       TO  HCDTAMA
003241             MOVE -1             TO  HCDTAML
003242             MOVE ER-0105        TO  EMI-ERROR
003243             PERFORM 9900-ERROR-FORMAT.
003244
003245     IF HMANAMTL GREATER ZERO
003246         
      * EXEC CICS BIF DEEDIT
003247*            FIELD  (HMANAMTI)
003248*            LENGTH (WS-HMANAMT-LENGTH)
003249*        END-EXEC
      *    MOVE '@"L                   #   #00011577' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HMANAMTI, 
                 WS-HMANAMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003250         IF HMANAMTI IS NUMERIC
003251             MOVE HMANAMTI       TO  WS-HMANAMTI
003252                                     HMANAMTO
003253             MOVE AL-UNNON       TO  HMANAMTA
003254           ELSE
003255             MOVE -1             TO  HMANAMTL
003256             MOVE AL-UNBON       TO  HMANAMTA
003257             MOVE ER-0107        TO  EMI-ERROR
003258             PERFORM 9900-ERROR-FORMAT.
003259
003260     IF HPCTCDTL GREATER ZERO
003261         
      * EXEC CICS BIF DEEDIT
003262*            FIELD  (HPCTCDTI)
003263*            LENGTH (WS-HPCTCDT-LENGTH)
003264*        END-EXEC
      *    MOVE '@"L                   #   #00011592' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131353932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HPCTCDTI, 
                 WS-HPCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003265         IF HPCTCDTI IS NUMERIC
003266             MOVE HPCTCDTI       TO  WS-HPCTCDTI
003267                                     HPCTCDTO
003268             MOVE AL-UNNON       TO  HPCTCDTA
003269         ELSE
003270             MOVE -1             TO  HPCTCDTL
003271             MOVE AL-UNBON       TO  HPCTCDTA
003272             MOVE ER-0106        TO  EMI-ERROR
003273             PERFORM 9900-ERROR-FORMAT.
003274
003275     IF HEXPL GREATER ZERO
003276         IF HEXPI = '1' OR '2' OR '3'
003277             MOVE AL-UNNON       TO  HEXPA
003278         ELSE
003279             MOVE -1             TO  HEXPL
003280             MOVE AL-UNBON       TO  HEXPA
003281             MOVE ER-0327        TO  EMI-ERROR
003282             PERFORM 9900-ERROR-FORMAT.
003283
003284     IF HEXPAMTL GREATER ZERO
003285         
      * EXEC CICS BIF DEEDIT
003286*            FIELD  (HEXPAMTI)
003287*            LENGTH (WS-HEXPAMT-LENGTH)
003288*        END-EXEC
      *    MOVE '@"L                   #   #00011616' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131363136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HEXPAMTI, 
                 WS-HEXPAMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003289         IF HEXPAMTI IS NUMERIC
003290             MOVE HEXPAMTI       TO  WS-HEXPAMTI
003291                                     HEXPAMTO
003292             MOVE AL-UNNON       TO  HEXPAMTA
003293         ELSE
003294             MOVE -1             TO  HEXPAMTL
003295             MOVE AL-UNBON       TO  HEXPAMTA
003296             MOVE ER-0328        TO  EMI-ERROR
003297             PERFORM 9900-ERROR-FORMAT.
003298
003299     IF WS-ERROR-COUNT GREATER ZERO
003300         PERFORM 8200-SEND-DATAONLY.
003301
003302     PERFORM 3000-READ-FOR-UPDATE.
003303
003304     IF HRESMANL GREATER ZERO
003305         MOVE HRESMANI           TO  AT-MANUAL-SW
003306         INSPECT AT-MANUAL-SW CONVERTING 'NY' TO ' 1'.
003307
003308     IF HMANAMTL GREATER ZERO
003309         MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.
003310
003311     IF HRESFUTL GREATER ZERO
003312         MOVE HRESFUTI           TO  AT-FUTURE-SW
003313         INSPECT AT-FUTURE-SW CONVERTING 'NY' TO ' 1'.
003314
003315     IF HRESIBNL GREATER ZERO
003316         MOVE HRESIBNI           TO  AT-IBNR-SW
003317         INSPECT AT-IBNR-SW CONVERTING 'NY' TO ' 1'.
003318
003319     IF HRESLFPL GREATER ZERO
003320         MOVE HRESLFPI           TO  AT-PTC-LF-SW
003321         INSPECT AT-PTC-LF-SW CONVERTING 'NY' TO ' 1'.
003322
003323     IF HRESAHPL GREATER ZERO
003324         MOVE HRESAHPI           TO  AT-PTC-SW
003325         INSPECT AT-PTC-SW CONVERTING 'NY' TO ' 1'.
003326
003327     IF HCDTAML GREATER ZERO
003328         MOVE HCDTAMI            TO  AT-CDT-ACCESS-METHOD.
003329
003330     IF HMANAMTL GREATER ZERO
003331         MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.
003332
003333     IF HPCTCDTL GREATER ZERO
003334         MOVE WS-HPCTCDTI        TO  AT-PERCENT-OF-CDT.
003335
003336     IF HEXPL GREATER ZERO
003337         MOVE HEXPI              TO  AT-EXPENSE-METHOD.
003338
003339     IF HEXPAMTL GREATER ZERO
003340         IF AT-EXPENSE-METHOD = '2' OR '4'
003341             MOVE WS-HEXPAMTI    TO  AT-EXPENSE-DOLLAR
003342           ELSE
003343             IF AT-EXPENSE-METHOD = '3'
003344                 MOVE WS-HEXPAMTI TO AT-EXPENSE-PERCENT
003345               ELSE
003346                 MOVE ZERO       TO  AT-EXPENSE-DOLLAR
003347                                     AT-EXPENSE-PERCENT.
003348
003349     MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY.
003350
003351     MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.
003352
003353     PERFORM 3100-REWRITE.
003354     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
003355
003356     MOVE 'S'                    TO  HMAINTO.
003357     MOVE -1                     TO  HMAINTL.
003358     MOVE AL-UANOF               TO  HMAINTA.
003359     PERFORM 8200-SEND-DATAONLY.
003360
003361     EJECT
003362 0900-MAIN-LOGIC.
003363     IF PI-MAP-NAME NOT = EL142I
003364         GO TO 1000-MAIN-LOGIC.
003365
003366     PERFORM 4000-READ-TRAILER-FILE.
003367
003368     EJECT
003369 1000-MAIN-LOGIC.
003370     IF PI-MAP-NAME NOT = EL142J
003371         GO TO 1100-MAIN-LOGIC.
003372
003373     IF NOT MODIFY-CAP
003374         IF JMAINTI = 'S'
003375             NEXT SENTENCE
003376         ELSE
003377             MOVE 'UPDATE'       TO SM-READ
003378             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
003379             MOVE ER-0070        TO  EMI-ERROR
003380             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003381             GO TO 8100-SEND-INITIAL-MAP.
003382
003383     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
003384     MOVE +2                     TO  EMI-SWITCH2.
003385
003386     IF JMAINTL NOT GREATER ZERO
003387       OR (JMAINTL GREATER ZERO AND
003388           JMAINTI = 'S')
003389             PERFORM 4000-READ-TRAILER-FILE.
003390
003391     IF JMAINTI = 'C' OR 'D'
003392         NEXT SENTENCE
003393     ELSE
003394         MOVE ER-0023            TO  EMI-ERROR
003395         MOVE -1                 TO  JMAINTL
003396         MOVE AL-UABON           TO  JMAINTA
003397         PERFORM 8200-SEND-DATAONLY.
003398
003399     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
003400           OR 'FNL'
003401        if (pi-el142-priority = '8')
003402           AND (PI-approval-level <> '5')
003403           MOVE ER-8003          TO EMI-ERROR
003404           MOVE -1               TO jMAINTL
003405           MOVE AL-UABON         TO jMAINTA
003406           GO TO 8200-SEND-DATAONLY.
003407
003408     IF FMAINTI NOT = 'D'
003409         GO TO 1050-MAIN-LOGIC.
003410
003411     
      * EXEC CICS READ
003412*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
003413*        RIDFLD  (PI-SAVE-KEY)
003414*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
003415*    END-EXEC.
      *    MOVE '&"S        E          (   #00011742' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303131373432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003416
003417     IF AT-FORM-PRINTED-DT NOT = LOW-VALUES
003418         MOVE ER-0564            TO  EMI-ERROR
003419         MOVE -1                 TO  JMAINTL
003420         MOVE AL-UABON           TO  JMAINTA
003421         PERFORM 8200-SEND-DATAONLY.
003422
003423     PERFORM 3000-READ-FOR-UPDATE.
003424
003425     PERFORM 3400-DELETE-FORM-ARCHIVE.
003426
003427     PERFORM 3200-DELETE.
003428
003429     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
003430     MOVE -1                     TO  JPFKL.
003431     MOVE 'S'                    TO  JMAINTO.
003432
003433     MOVE AL-SANOF               TO  JMAINTA  JDTSENTA  JRESENDA
003434                                     JSI1A    JREPLYA   JSI2A
003435                                     JRECEVEA JSI3A     JFORMA
003436                                     JCARR1A  JCLAIM1A  JCERT1A
003437                                     JCARR2A  JCLAIM2A  JCERT2A
003438                                     JPHYRECA JEMPRECA  JREMDTA.
003439
003440     PERFORM 8200-SEND-DATAONLY.
003441
003442 1050-MAIN-LOGIC.
003443     MOVE AL-UANON               TO  JMAINTA.
003444
003445     IF JDTSENTL GREATER ZERO
003446         MOVE JDTSENTI           TO  WS-DEEDIT-FIELD
003447         PERFORM 8600-DEEDIT
003448         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003449             MOVE WS-DEEDIT-FIELD-V0  TO  JDTSENTO
003450             INSPECT JDTSENTI CONVERTING SPACES TO SLASH
003451             MOVE '4'                 TO  DC-OPTION-CODE
003452             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003453             PERFORM 8500-DATE-CONVERSION
003454             IF DC-ERROR-CODE NOT = SPACES
003455                 MOVE ER-0550    TO  EMI-ERROR
003456                 MOVE -1         TO  JDTSENTL
003457                 MOVE AL-UABON   TO  JDTSENTA
003458                 PERFORM 9900-ERROR-FORMAT
003459             ELSE
003460                 MOVE AL-UANON   TO  JDTSENTA
003461                 MOVE +1         TO  WS-UPDATE-SW
003462                 MOVE DC-BIN-DATE-1  TO  WS-SEND-ON-DATE
003463                 IF WS-SEND-ON-DATE LESS THAN WS-CURRENT-DATE
003464                     MOVE ER-0551 TO  EMI-ERROR
003465                     MOVE -1      TO  JDTSENTL
003466                     MOVE AL-UABON TO  JDTSENTA
003467                     PERFORM 9900-ERROR-FORMAT
003468                 ELSE
003469                     NEXT SENTENCE
003470         ELSE
003471             MOVE ER-0550        TO  EMI-ERROR
003472             MOVE -1             TO  JDTSENTL
003473             MOVE AL-UABON       TO  JDTSENTA
003474             PERFORM 9900-ERROR-FORMAT.
003475
003476     IF JRESENDL GREATER ZERO
003477         IF JRESENDI = SPACES
003478             MOVE AL-UANON       TO  JRESENDA
003479             MOVE +1             TO  WS-UPDATE-SW
003480             MOVE LOW-VALUES     TO  WS-RESEND-DATE
003481         ELSE
003482             MOVE JRESENDI       TO  WS-DEEDIT-FIELD
003483             PERFORM 8600-DEEDIT
003484             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003485                 MOVE WS-DEEDIT-FIELD-V0  TO  JRESENDO
003486                 INSPECT JRESENDI CONVERTING SPACES TO SLASH
003487                 MOVE '4'                 TO  DC-OPTION-CODE
003488                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003489                 PERFORM 8500-DATE-CONVERSION
003490                 IF DC-ERROR-CODE NOT = SPACES
003491                     MOVE ER-0295           TO  EMI-ERROR
003492                     MOVE -1             TO  JRESENDL
003493                     MOVE AL-UABON       TO  JRESENDA
003494                     PERFORM 9900-ERROR-FORMAT
003495                 ELSE
003496                     MOVE AL-UANON       TO  JRESENDA
003497                     MOVE +1             TO  WS-UPDATE-SW
003498                     MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE
003499             ELSE
003500                 MOVE ER-0295        TO  EMI-ERROR
003501                 MOVE -1             TO  JRESENDL
003502                 MOVE AL-UABON       TO  JRESENDA
003503                 PERFORM 9900-ERROR-FORMAT.
003504
003505     IF JREPLYL GREATER ZERO
003506         IF JREPLYI = SPACES
003507             MOVE AL-UANON       TO  JREPLYA
003508             MOVE +1             TO  WS-UPDATE-SW
003509             MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE
003510         ELSE
003511             MOVE JREPLYI        TO  WS-DEEDIT-FIELD
003512             PERFORM 8600-DEEDIT
003513             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003514                 MOVE WS-DEEDIT-FIELD-V0  TO  JREPLYO
003515                 INSPECT JREPLYI CONVERTING SPACES TO SLASH
003516                 MOVE '4'                 TO  DC-OPTION-CODE
003517                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003518                 PERFORM 8500-DATE-CONVERSION
003519                 IF DC-ERROR-CODE NOT = SPACES
003520                     MOVE ER-0296           TO  EMI-ERROR
003521                     MOVE -1             TO  JREPLYL
003522                     MOVE AL-UABON       TO  JREPLYA
003523                     PERFORM 9900-ERROR-FORMAT
003524                 ELSE
003525                     MOVE AL-UANON       TO  JREPLYA
003526                     MOVE +1             TO  WS-UPDATE-SW
003527                     MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE
003528             ELSE
003529                 MOVE ER-0296        TO  EMI-ERROR
003530                 MOVE -1             TO  JREPLYL
003531                 MOVE AL-UABON       TO  JREPLYA
003532                 PERFORM 9900-ERROR-FORMAT.
003533
003534     IF JRECEVEL GREATER ZERO
003535         IF JRECEVEI = SPACES
003536             MOVE AL-UANON       TO  JRECEVEA
003537             MOVE +1             TO  WS-UPDATE-SW
003538             MOVE LOW-VALUES     TO  WS-RECEIVED-DATE
003539         ELSE
003540             MOVE JRECEVEI       TO  WS-DEEDIT-FIELD
003541             PERFORM 8600-DEEDIT
003542             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003543                 MOVE WS-DEEDIT-FIELD-V0  TO  JRECEVEO
003544                 INSPECT JRECEVEI CONVERTING SPACES TO SLASH
003545                 MOVE '4'                TO  DC-OPTION-CODE
003546                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003547                 PERFORM 8500-DATE-CONVERSION
003548                 IF DC-ERROR-CODE NOT = SPACES
003549                     MOVE ER-0297        TO  EMI-ERROR
003550                     MOVE -1             TO  JRECEVEL
003551                     MOVE AL-UABON       TO  JRECEVEA
003552                     PERFORM 9900-ERROR-FORMAT
003553                 ELSE
003554                     MOVE AL-UANON       TO  JRECEVEA
003555                     MOVE +1             TO  WS-UPDATE-SW
003556                     MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE
003557             ELSE
003558                 MOVE ER-0297        TO  EMI-ERROR
003559                 MOVE -1             TO  JRECEVEL
003560                 MOVE AL-UABON       TO  JRECEVEA
003561                 PERFORM 9900-ERROR-FORMAT.
003562
003563     IF JPHYRECL GREATER ZERO
003564         IF JPHYRECI = SPACES
003565             MOVE AL-UANON       TO  JPHYRECA
003566             MOVE +1             TO  WS-UPDATE-SW
003567             MOVE LOW-VALUES     TO  WS-RECEIVED-PHY-DATE
003568         ELSE
003569             MOVE JPHYRECI       TO  WS-DEEDIT-FIELD
003570             PERFORM 8600-DEEDIT
003571             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003572                 MOVE WS-DEEDIT-FIELD-V0  TO  JPHYRECO
003573                 INSPECT JPHYRECI CONVERTING SPACES TO SLASH
003574                 MOVE '4'                TO  DC-OPTION-CODE
003575                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003576                 PERFORM 8500-DATE-CONVERSION
003577                 IF DC-ERROR-CODE NOT = SPACES
003578                     MOVE ER-0297        TO  EMI-ERROR
003579                     MOVE -1             TO  JPHYRECL
003580                     MOVE AL-UABON       TO  JPHYRECA
003581                     PERFORM 9900-ERROR-FORMAT
003582                 ELSE
003583                     MOVE AL-UANON       TO  JPHYRECA
003584                     MOVE +1             TO  WS-UPDATE-SW
003585                     MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-PHY-DATE
003586             ELSE
003587                 MOVE ER-0297        TO  EMI-ERROR
003588                 MOVE -1             TO  JPHYRECL
003589                 MOVE AL-UABON       TO  JPHYRECA
003590                 PERFORM 9900-ERROR-FORMAT.
003591
003592     IF JEMPRECL GREATER ZERO
003593         IF JEMPRECI = SPACES
003594             MOVE AL-UANON       TO  JEMPRECA
003595             MOVE +1             TO  WS-UPDATE-SW
003596             MOVE LOW-VALUES     TO  WS-RECEIVED-EMP-DATE
003597         ELSE
003598             MOVE JEMPRECI       TO  WS-DEEDIT-FIELD
003599             PERFORM 8600-DEEDIT
003600             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
003601                 MOVE WS-DEEDIT-FIELD-V0  TO  JEMPRECO
003602                 INSPECT JEMPRECI CONVERTING SPACES TO SLASH
003603                 MOVE '4'                TO  DC-OPTION-CODE
003604                 MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
003605                 PERFORM 8500-DATE-CONVERSION
003606                 IF DC-ERROR-CODE NOT = SPACES
003607                     MOVE ER-0297        TO  EMI-ERROR
003608                     MOVE -1             TO  JEMPRECL
003609                     MOVE AL-UABON       TO  JEMPRECA
003610                     PERFORM 9900-ERROR-FORMAT
003611                 ELSE
003612                     MOVE AL-UANON       TO  JEMPRECA
003613                     MOVE +1             TO  WS-UPDATE-SW
003614                     MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-EMP-DATE
003615             ELSE
003616                 MOVE ER-0297        TO  EMI-ERROR
003617                 MOVE -1             TO  JEMPRECL
003618                 MOVE AL-UABON       TO  JEMPRECA
003619                 PERFORM 9900-ERROR-FORMAT.
003620
003621     IF JFORML GREATER ZERO
003622         IF JFORMI = 'INITIAL'  OR 'I' OR
003623                     'PROGRESS' OR 'P'
003624             MOVE AL-UANON       TO  JFORMA
003625         ELSE
003626             MOVE ER-0532        TO  EMI-ERROR
003627             MOVE AL-UABON       TO  JFORMA
003628             MOVE -1             TO  JFORML
003629             PERFORM 9900-ERROR-FORMAT.
003630
003631     IF JSI1L GREATER ZERO         OR
003632        JSI2L GREATER ZERO         OR
003633        JSI3L GREATER ZERO         OR
003634        JCARR1L GREATER ZERO       OR
003635        JCLAIM1L GREATER ZERO      OR
003636        JCERT1L GREATER ZERO       OR
003637        JCARR2L GREATER ZERO       OR
003638        JCLAIM2L GREATER ZERO      OR
003639        JCERT2L GREATER ZERO
003640         MOVE +1                 TO  WS-UPDATE-SW.
003641
003642     IF WS-ERROR-COUNT GREATER ZERO
003643         PERFORM 8200-SEND-DATAONLY.
003644
003645     IF WS-UPDATE-SW NOT GREATER ZERO
003646         PERFORM 4000-READ-TRAILER-FILE.
003647
003648     IF JREPLYL GREATER ZERO
003649         PERFORM 3300-UPDATE-CLAIM-MASTER.
003650
003651     PERFORM 3000-READ-FOR-UPDATE.
003652
003653     IF JDTSENTL GREATER ZERO
003654         MOVE WS-SEND-ON-DATE    TO  AT-FORM-SEND-ON-DT
003655         MOVE LOW-VALUES         TO  AT-FORM-PRINTED-DT.
003656
003657     IF JRESENDL GREATER ZERO
003658         MOVE WS-RESEND-DATE     TO  AT-FORM-RE-SEND-DT.
003659
003660     IF JREPLYL GREATER ZERO
003661         MOVE WS-FOLLOW-UP-DATE  TO  AT-FORM-FOLLOW-UP-DT.
003662
003663     IF JRECEVEL GREATER ZERO
003664         MOVE WS-RECEIVED-DATE   TO  AT-FORM-ANSWERED-DT.
003665
003666     IF JPHYRECL GREATER ZERO
003667         MOVE WS-RECEIVED-PHY-DATE   TO  AT-PHY-FORM-ANSWERED-DT.
003668
003669     IF JEMPRECL GREATER ZERO
003670         MOVE WS-RECEIVED-EMP-DATE   TO  AT-EMP-FORM-ANSWERED-DT.
003671
003672     IF JFORML GREATER ZERO
003673         MOVE JFORMI             TO  AT-FORM-TYPE
003674         INSPECT AT-FORM-TYPE CONVERTING 'IP' TO '12'.
003675
003676     IF JSI1L GREATER ZERO
003677         MOVE JSI1I              TO  AT-INSTRUCT-LN-1.
003678
003679     IF JSI2L GREATER ZERO
003680         MOVE JSI2I              TO  AT-INSTRUCT-LN-2.
003681
003682     IF JSI3L GREATER ZERO
003683         MOVE JSI3I              TO  AT-INSTRUCT-LN-3.
003684
003685     IF AT-INSTRUCT-LN-2 = SPACES
003686         MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2
003687         MOVE SPACES             TO  AT-INSTRUCT-LN-3.
003688
003689     IF AT-INSTRUCT-LN-1 = SPACES
003690         MOVE AT-INSTRUCT-LN-2   TO  AT-INSTRUCT-LN-1
003691         MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2
003692         MOVE SPACES             TO  AT-INSTRUCT-LN-3.
003693
003694     IF JCARR1L GREATER ZERO
003695         MOVE JCARR1I            TO  AT-REL-CARR-1.
003696
003697     IF JCLAIM1L GREATER ZERO
003698         MOVE JCLAIM1I           TO  AT-REL-CLAIM-1.
003699
003700     IF JCERT1L GREATER ZERO
003701         MOVE JCERT1I            TO  AT-REL-CERT-1.
003702
003703     IF JCARR2L GREATER ZERO
003704         MOVE JCARR2I            TO  AT-REL-CARR-2.
003705
003706     IF JCLAIM2L GREATER ZERO
003707         MOVE JCLAIM2I           TO  AT-REL-CLAIM-2.
003708
003709     IF JCERT2L GREATER ZERO
003710         MOVE JCERT2I            TO  AT-REL-CERT-2.
003711
003712     MOVE PI-PROCESSOR-ID        TO  AT-FORM-LAST-UPDATED-BY.
003713
003714     MOVE WS-CURRENT-DATE        TO  AT-FORM-LAST-MAINT-DT.
003715
003716     PERFORM 3100-REWRITE.
003717     MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
003718
003719     MOVE 'S'                    TO  JMAINTO.
003720     MOVE -1                     TO  JMAINTL.
003721     MOVE AL-UANOF               TO  JMAINTA.
003722     PERFORM 8200-SEND-DATAONLY.
003723
003724     EJECT
003725 1100-MAIN-LOGIC.
003726     IF PI-MAP-NAME NOT = EL142B2
003727         GO TO 1200-MAIN-LOGIC.
003728
003729     IF NOT MODIFY-CAP
003730         IF KMAINTI = 'S'
003731             NEXT SENTENCE
003732         ELSE
003733             MOVE 'UPDATE'       TO SM-READ
003734             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
003735             MOVE ER-0070        TO  EMI-ERROR
003736             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003737             GO TO 8100-SEND-INITIAL-MAP.
003738
003739     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
003740     MOVE +2                     TO  EMI-SWITCH2.
003741
003742     IF KMAINTL NOT GREATER ZERO
003743       OR (KMAINTL GREATER ZERO AND
003744           KMAINTI = 'S')
003745             MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY
003746             PERFORM 4000-READ-TRAILER-FILE.
003747
003748
003749     if kmainti = 'C' or 'D' or 'A'
003750        if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
003751           OR 'FNL'
003752           if (pi-el142-priority = '8')
003753           AND (PI-approval-level <> '5')
003754              MOVE ER-8003       TO EMI-ERROR
003755              MOVE -1            TO kMAINTL
003756              MOVE AL-UABON      TO kMAINTA
003757              GO TO 8200-SEND-DATAONLY
003758           end-if
003759        end-if
003760     end-if
003761
003762     IF KMAINTI NOT = 'D'
003763         GO TO 1120-MAIN-LOGIC.
003764
003765     PERFORM 3000-READ-FOR-UPDATE.
003766
003767     MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
003768     MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
003769     MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
003770
003771     
      * EXEC CICS READ UPDATE
003772*        DATASET (WS-CHECK-QUEUE-DSID)
003773*        RIDFLD  (WS-CHECK-QUEUE-KEY)
003774*        SET     (ADDRESS OF CHECK-QUE)
003775*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012102' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132313032' TO DFHEIV0
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
           
003776
003777     
      * EXEC CICS DELETE
003778*        DATASET (WS-CHECK-QUEUE-DSID)
003779*    END-EXEC.
      *    MOVE '&(                    &   #00012108' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132313038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003780
003781     MOVE +99999999              TO  AT-CHECK-QUE-CONTROL.
003782     MOVE ZERO                   TO  AT-CHECK-QUE-SEQUENCE.
003783
003784     PERFORM 3100-REWRITE.
003785
003786     MOVE ER-ZERO                TO  EMI-ERROR.
003787     PERFORM 9900-ERROR-FORMAT.
003788
003789     MOVE PI-SAVE-KEY  TO  PI-ACTIVITY-TRAILERS-KEY.
003790     PERFORM 4000-READ-TRAILER-FILE.
003791
003792 1120-MAIN-LOGIC.
003793     IF KMAINTI = 'A' OR 'C'
003794         NEXT SENTENCE
003795     ELSE
003796         MOVE ER-0023            TO  EMI-ERROR
003797         MOVE -1                 TO  KMAINTL
003798         MOVE AL-UABON           TO  KMAINTA
003799         PERFORM 8200-SEND-DATAONLY.
003800
003801     IF KMAINTI NOT = 'C'
003802         GO TO 1150-MAIN-LOGIC.
003803
003804     IF KTIMPRTL GREATER ZERO
003805         
      * EXEC CICS BIF DEEDIT
003806*            FIELD  (KTIMPRTI)
003807*            LENGTH (4)
003808*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012136' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132313336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003809         IF KTIMPRTI NOT NUMERIC
003810             MOVE -1             TO  KTIMPRTL
003811             MOVE AL-UNBON       TO  KTIMPRTA
003812             MOVE ER-0579        TO  EMI-ERROR
003813             PERFORM 9900-ERROR-FORMAT
003814         ELSE
003815             MOVE KTIMPRTI       TO  KTIMPRTO
003816             MOVE +1             TO  WS-UPDATE-SW.
003817
003818     IF WS-ERROR-COUNT GREATER ZERO
003819         PERFORM 8200-SEND-DATAONLY.
003820
003821     
      * EXEC CICS READ
003822*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
003823*        RIDFLD  (PI-SAVE-KEY)
003824*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
003825*    END-EXEC.
      *    MOVE '&"S        E          (   #00012152' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132313532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003826
003827     MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
003828     MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
003829     MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
003830
003831     
      * EXEC CICS READ UPDATE
003832*        DATASET (WS-CHECK-QUEUE-DSID)
003833*        RIDFLD  (WS-CHECK-QUEUE-KEY)
003834*        SET     (ADDRESS OF CHECK-QUE)
003835*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012162' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132313632' TO DFHEIV0
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
           
003836
003837     IF KTIMPRTL GREATER ZERO
003838         
      * EXEC CICS BIF DEEDIT
003839*            FIELD  (KTIMPRTI)
003840*            LENGTH (4)
003841*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012169' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132313639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003842         MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED
003843                                     KTIMPRTO.
003844
003845     
      * EXEC CICS REWRITE
003846*        DATASET (WS-CHECK-QUEUE-DSID)
003847*        FROM    (CHECK-QUE)
003848*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012176' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132313736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003849
003850     MOVE ER-ZERO                TO  EMI-ERROR.
003851     PERFORM 9900-ERROR-FORMAT.
003852
003853     MOVE -1                     TO  KMAINTL.
003854
003855     PERFORM 8200-SEND-DATAONLY.
003856
003857 1150-MAIN-LOGIC.
003858     IF KCONTRLL GREATER ZERO
003859         
      * EXEC CICS BIF DEEDIT
003860*            FIELD  (KCONTRLI)
003861*            LENGTH (8)
003862*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012190' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132313930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KCONTRLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003863         IF KCONTRLI NOT NUMERIC
003864             MOVE -1             TO  KCONTRLL
003865             MOVE AL-UNBON       TO  KCONTRLA
003866             MOVE ER-0580        TO  EMI-ERROR
003867             PERFORM 9900-ERROR-FORMAT
003868         ELSE
003869             MOVE KCONTRLI       TO  KCONTRLO
003870             MOVE +1             TO  WS-UPDATE-SW.
003871
003872     IF KSEQL GREATER ZERO
003873         
      * EXEC CICS BIF DEEDIT
003874*            FIELD  (KSEQI)
003875*            LENGTH (4)
003876*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012204' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003877         IF KSEQI NOT NUMERIC
003878             MOVE -1             TO  KSEQL
003879             MOVE AL-UNBON       TO  KSEQA
003880             MOVE ER-0581        TO  EMI-ERROR
003881             PERFORM 9900-ERROR-FORMAT
003882         ELSE
003883             MOVE KSEQI          TO  KSEQO
003884             MOVE +1             TO  WS-UPDATE-SW.
003885
003886     IF KTIMPRTL GREATER ZERO
003887         
      * EXEC CICS BIF DEEDIT
003888*            FIELD  (KTIMPRTI)
003889*            LENGTH (4)
003890*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012218' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003891         IF KTIMPRTI NOT NUMERIC
003892             MOVE -1             TO  KTIMPRTL
003893             MOVE AL-UNBON       TO  KTIMPRTA
003894             MOVE ER-0579           TO  EMI-ERROR
003895             PERFORM 9900-ERROR-FORMAT
003896         ELSE
003897             MOVE KTIMPRTI       TO  KTIMPRTO
003898             MOVE +1             TO  WS-UPDATE-SW.
003899
003900     IF WS-ERROR-COUNT GREATER ZERO
003901         PERFORM 8200-SEND-DATAONLY.
003902
003903     IF KCONTRLL GREATER ZERO
003904       OR KSEQL GREATER ZERO
003905         PERFORM 3000-READ-FOR-UPDATE
003906     ELSE
003907         
      * EXEC CICS READ
003908*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
003909*            RIDFLD  (PI-SAVE-KEY)
003910*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
003911*        END-EXEC.
      *    MOVE '&"S        E          (   #00012238' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132323338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003912
003913     MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
003914     MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
003915     MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
003916
003917     
      * EXEC CICS GETMAIN
003918*        LENGTH  (WS-CHECK-QUEUE-LENGTH)
003919*        INITIMG (WS-SPACES)
003920*        SET     (ADDRESS OF CHECK-QUE)
003921*    END-EXEC.
      *    MOVE ',"IL                  $   #00012248' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-CHECK-QUEUE-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003922
003923     MOVE 'CQ'                   TO  CQ-RECORD-ID.
003924     MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD.
003925     MOVE AT-CHECK-QUE-CONTROL   TO  CQ-CONTROL-NUMBER.
003926     MOVE AT-CHECK-QUE-SEQUENCE  TO  CQ-SEQUENCE-NUMBER.
003927
003928     MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
003929     MOVE AT-CARRIER             TO  CQ-CARRIER.
003930     MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.
003931     MOVE AT-CERT-NO             TO  CQ-CERT-NO.
003932     MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.
003933     MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.
003934
003935     MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.
003936     MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.
003937     MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.
003938     MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.
003939
003940     IF AT-VOID-DT NOT = LOW-VALUES
003941         MOVE 'V'                TO  CQ-VOID-INDICATOR.
003942
003943     MOVE ZERO                   TO  CQ-TIMES-PRINTED
003944                                     CQ-PRINT-AT-HHMM.
003945     MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.
003946     MOVE AT-CHECK-WRITTEN-DT    TO  CQ-CHECK-WRITTEN-DT.
003947     MOVE +1420                  TO  CQ-LAST-UPDATED-BY.
003948
003949     IF KCONTRLL GREATER ZERO
003950         
      * EXEC CICS BIF DEEDIT
003951*            FIELD  (KCONTRLI)
003952*            LENGTH (8)
003953*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012281' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KCONTRLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003954         MOVE KCONTRLI           TO  CQ-CONTROL-NUMBER
003955                                     AT-CHECK-QUE-CONTROL
003956                                     KCONTRLO.
003957     IF KSEQL GREATER ZERO
003958         
      * EXEC CICS BIF DEEDIT
003959*            FIELD  (KSEQI)
003960*            LENGTH (4)
003961*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012289' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003962         MOVE KSEQI              TO  CQ-SEQUENCE-NUMBER
003963                                     AT-CHECK-QUE-SEQUENCE
003964                                     KSEQO.
003965     IF KTIMPRTL GREATER ZERO
003966         
      * EXEC CICS BIF DEEDIT
003967*            FIELD  (KTIMPRTI)
003968*            LENGTH (4)
003969*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012297' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003970         MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED
003971                                     KTIMPRTO.
003972
003973     
      * EXEC CICS WRITE
003974*        DATASET (WS-CHECK-QUEUE-DSID)
003975*        FROM    (CHECK-QUE)
003976*        RIDFLD  (CQ-CONTROL-PRIMARY)
003977*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012304' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132333034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003978
003979     IF KCONTRLL GREATER ZERO OR
003980        KSEQL    GREATER ZERO
003981         PERFORM 3100-REWRITE.
003982
003983     MOVE ER-ZERO                TO  EMI-ERROR.
003984     PERFORM 9900-ERROR-FORMAT.
003985
003986     MOVE SPACES                 TO  KMAINTO.
003987     MOVE AL-UANOF               TO  KMAINTA.
003988     MOVE -1                     TO  KMAINTL.
003989
003990     PERFORM 8200-SEND-DATAONLY.
003991
003992     EJECT
003993 1200-MAIN-LOGIC.
003994     IF PI-MAP-NAME = EL142D2
003995*03438          GO TO 0015-MAIN-LOGIC.
003996         MOVE EL142D TO PI-MAP-NAME
003997         IF PI-END-OF-FILE = ZERO
003998             SUBTRACT +1 FROM PI-ATK-SEQUENCE-NO
003999             ADD +1 TO PI-PREV-ATK-SEQUENCE-NO
004000         END-IF
004001         MOVE ZERO TO PI-END-OF-FILE
004002         PERFORM 4000-READ-TRAILER-FILE
004003     END-IF.
004004
004005     MOVE '1200-MAIN-LOGIC REACHED'  TO  LOGOFF-MSG.
004006     PERFORM 8300-SEND-TEXT.
004007
004008     EJECT
004009 3000-READ-FOR-UPDATE SECTION.
004010
004011     
      * EXEC CICS READ UPDATE
004012*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
004013*        RIDFLD  (PI-SAVE-KEY)
004014*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
004015*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012342' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132333432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004016
004017     IF AT-INFO-TRAILER-TYPE = 'M'
004018         MOVE ER-0969        TO EMI-ERROR
004019         MOVE -1             TO EMAINTL
004020         MOVE AL-UABON       TO EMAINTA
004021         
      * EXEC CICS UNLOCK
004022*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004023*        END-EXEC
      *    MOVE '&*                    #   #00012352' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132333532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004024         GO TO 8200-SEND-DATAONLY.
004025
004026     IF AT-RECORDED-BY NOT = PI-UPDATE-BY
004027       OR AT-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
004028         MOVE AL-UABON           TO  HMAINTA
004029         MOVE -1                 TO  HMAINTL
004030         MOVE ER-0068            TO  EMI-ERROR
004031         
      * EXEC CICS UNLOCK
004032*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004033*        END-EXEC
      *    MOVE '&*                    #   #00012362' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132333632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004034         PERFORM 8200-SEND-DATAONLY.
004035
004036 3000-EXIT.
004037     EXIT.
004038
004039     EJECT
004040 3100-REWRITE SECTION.
004041
004042     MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY
004043                                     BMANTBYO.
004044
004045     MOVE AT-RECORDED-DT             TO  DC-BIN-DATE-1.
004046     MOVE ' '                        TO  DC-OPTION-CODE.
004047     PERFORM 8500-DATE-CONVERSION.
004048     IF NO-CONVERSION-ERROR
004049         MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO
004050     ELSE
004051         MOVE SPACES                 TO  BRECDTEO.
004052
004053     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS
004054                                     PI-UPDATE-HHMMSS.
004055
004056     
      * EXEC CICS REWRITE
004057*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
004058*        FROM    (ACTIVITY-TRAILERS)
004059*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012387' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132333837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004060
004061     MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
004062
004063     PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
004064
004065     PERFORM 3600-REWRITE-ELMSTR.
004066
004067 3100-EXIT.
004068     EXIT.
004069
004070     EJECT
004071 3200-DELETE SECTION.
004072
004073     
      * EXEC CICS DELETE
004074*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
004075*    END-EXEC.
      *    MOVE '&(                    &   #00012404' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132343034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004076
004077     MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
004078
004079     
      * EXEC CICS READ
004080*        DATASET (WS-CLAIM-MASTER-DSID)
004081*        RIDFLD  (WS-CLAIM-KEY)
004082*        SET     (ADDRESS OF CLAIM-MASTER)
004083*    END-EXEC.
      *    MOVE '&"S        E          (   #00012410' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004084
004085     IF CL-TRAILER-SEQ-CNT NOT = PI-SAVE-ATK-SEQUENCE-NO
004086         GO TO 3200-EXIT.
004087
004088     PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
004089
004090     
      * EXEC CICS HANDLE CONDITION
004091*        NOTFND (3200-NOT-FOUND)
004092*    END-EXEC.
      *    MOVE '"$I                   ! $ #00012421' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303132343231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004093
004094     
      * EXEC CICS READ
004095*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
004096*        RIDFLD  (PI-SAVE-KEY)
004097*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
004098*        GTEQ
004099*    END-EXEC.
      *    MOVE '&"S        G          (   #00012425' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303132343235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004100
004101     IF PI-SAVE-ATK-COMPANY-CODE = AT-COMPANY-CD    AND
004102        PI-SAVE-ATK-CARRIER      = AT-CARRIER       AND
004103        PI-SAVE-ATK-CLAIM-NO     = AT-CLAIM-NO      AND
004104        PI-SAVE-ATK-CERT-NO      = AT-CERT-NO
004105         MOVE AT-SEQUENCE-NO           TO  CL-TRAILER-SEQ-CNT
004106         GO TO 3200-REWRITE-CLAIM-MASTER.
004107
004108 3200-NOT-FOUND.
004109     MOVE +4095                  TO  CL-TRAILER-SEQ-CNT.
004110
004111 3200-REWRITE-CLAIM-MASTER.
004112     PERFORM 3600-REWRITE-ELMSTR.
004113
004114 3200-EXIT.
004115     EXIT.
004116
004117     EJECT
004118 3300-UPDATE-CLAIM-MASTER SECTION.
004119     MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
004120
004121     
      * EXEC CICS READ
004122*        DATASET (WS-CLAIM-MASTER-DSID)
004123*        RIDFLD  (WS-CLAIM-KEY)
004124*        SET     (ADDRESS OF CLAIM-MASTER)
004125*    END-EXEC.
      *    MOVE '&"S        E          (   #00012452' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132343532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004126
004127     IF WS-FOLLOW-UP-DATE NOT LESS THAN CL-NEXT-FOLLOWUP-DT
004128         GO TO 3300-EXIT.
004129
004130     PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
004131
004132     MOVE WS-FOLLOW-UP-DATE      TO  CL-NEXT-FOLLOWUP-DT.
004133
004134     PERFORM 3600-REWRITE-ELMSTR.
004135
004136 3300-EXIT.
004137     EXIT.
004138
004139     EJECT
004140 3400-DELETE-FORM-ARCHIVE SECTION.
004141     
      * EXEC CICS HANDLE CONDITION
004142*        ENDFILE (3490-DELETE-FORM-ARCHIVE)
004143*        NOTFND  (3499-EXIT)
004144*    END-EXEC
      *    MOVE '"$''I                  ! % #00012472' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303132343732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004145
004146     MOVE LOW-VALUES             TO  WS-LETTER-ARCHIVE-ALT-KEY.
004147
004148     MOVE PI-COMPANY-CD          TO  WS-LA-ALT-COMPANY-CD.
004149     MOVE '4'                    TO  WS-LA-ALT-RECORD-TYPE.
004150
004151     
      * EXEC CICS STARTBR
004152*        DATASET (WS-LETTER-ARCHIVE-DSID2)
004153*        RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)
004154*        GTEQ
004155*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012482' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132343832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 WS-LETTER-ARCHIVE-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004156
004157 3410-DELETE-FORM-ARCHIVE.
004158     
      * EXEC CICS READNEXT
004159*        DATASET (WS-LETTER-ARCHIVE-DSID2)
004160*        RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)
004161*        SET     (ADDRESS OF LETTER-ARCHIVE)
004162*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012489' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132343839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-LETTER-ARCHIVE-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004163
004164     IF LA-COMPANY-CD NOT = PI-COMPANY-CD
004165         GO TO 3490-DELETE-FORM-ARCHIVE.
004166
004167     IF LA-RECORD-TYPE NOT = '4'
004168         GO TO 3490-DELETE-FORM-ARCHIVE.
004169
004170     IF LA-CARRIER  NOT = PI-SAVE-ATK-CARRIER   OR
004171        LA-CLAIM-NO NOT = PI-SAVE-ATK-CLAIM-NO  OR
004172        LA-CERT-NO  NOT = PI-SAVE-ATK-CERT-NO
004173         GO TO 3410-DELETE-FORM-ARCHIVE.
004174
004175     MOVE LA-COMPANY-CD          TO WS-LA-COMPANY-CD.
004176     MOVE LA-ARCHIVE-NO          TO WS-LA-ARCHIVE-NO.
004177     MOVE LA-RECORD-TYPE         TO WS-LA-RECORD-TYPE.
004178     MOVE LA-LINE-SEQ-NO         TO WS-LA-LINE-SEQ-NO.
004179
004180     
      * EXEC CICS ENDBR
004181*        DATASET (WS-LETTER-ARCHIVE-DSID2)
004182*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012511' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132353131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004183
004184     
      * EXEC CICS READ UPDATE
004185*        DATASET (WS-LETTER-ARCHIVE-DSID)
004186*        RIDFLD  (WS-LETTER-ARCHIVE-KEY)
004187*        SET     (ADDRESS OF LETTER-ARCHIVE)
004188*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012515' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132353135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-LETTER-ARCHIVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004189
004190     
      * EXEC CICS DELETE
004191*        DATASET (WS-LETTER-ARCHIVE-DSID)
004192*    END-EXEC.
      *    MOVE '&(                    &   #00012521' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132353231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID, 
                 WS-LETTER-ARCHIVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004193
004194     GO TO 3499-EXIT.
004195
004196 3490-DELETE-FORM-ARCHIVE.
004197     
      * EXEC CICS ENDBR
004198*        DATASET (WS-LETTER-ARCHIVE-DSID2)
004199*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012528' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132353238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004200
004201 3499-EXIT.
004202     EXIT.
004203
004204     EJECT
004205 3500-READ-ELMSTR-FOR-UPDATE SECTION.
004206     MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
004207
004208     
      * EXEC CICS READ UPDATE
004209*        DATASET (WS-CLAIM-MASTER-DSID)
004210*        RIDFLD  (WS-CLAIM-KEY)
004211*        SET     (ADDRESS OF CLAIM-MASTER)
004212*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012539' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132353339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004213
004214 3599-EXIT.
004215     EXIT.
004216
004217 3600-REWRITE-ELMSTR SECTION.
004218     
      * EXEC CICS HANDLE CONDITION
004219*        DUPKEY (3699-EXIT)
004220*    END-EXEC.
      *    MOVE '"$$                   ! & #00012549' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303132353439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004221
004222     MOVE WS-CURRENT-DATE        TO  CL-LAST-MAINT-DT.
004223     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
004224     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
004225     MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
004226
004227     
      * EXEC CICS REWRITE
004228*        DATASET (WS-CLAIM-MASTER-DSID)
004229*        FROM    (CLAIM-MASTER)
004230*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012558' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132353538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004231
004232 3699-EXIT.
004233     EXIT.
004234
004235     EJECT
004236 4000-READ-TRAILER-FILE SECTION.
004237     IF ((EIBAID = (DFHENTER OR DFHPF1) AND
004238         PI-PREV-AID = (DFHENTER OR DFHPF1))
004239       OR
004240        (EIBAID = DFHPF2 AND
004241         PI-PREV-AID = DFHPF2)
004242       OR
004243         PI-RECORD-COUNT = +1)
004244       AND
004245         PI-END-OF-FILE NOT = ZERO
004246             GO TO 0015-MAIN-LOGIC.
004247
004248     IF PI-END-OF-FILE NOT = ZERO
004249         IF EIBAID = DFHPF2
004250             MOVE PI-SAVE-KEY    TO  PI-PREV-ACTIVITY-TRAILERS-KEY
004251         ELSE
004252             MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY.
004253
004254     MOVE LOW-VALUES             TO  EL142DO.
004255
004256     IF EIBAID = DFHPF2
004257         
      * EXEC CICS STARTBR
004258*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004259*            RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)
004260*            EQUAL
004261*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00012588' TO DFHEIV0
           MOVE X'262C20202020202020202045' &
                X'202020202020202020202620' &
                X'2020233030303132353838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 PI-PREV-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004262       ELSE
004263         
      * EXEC CICS STARTBR
004264*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004265*            RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)
004266*            GTEQ
004267*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012594' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132353934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004268
004269     EJECT
004270 4100-READNEXT.
004271     IF EIBAID = DFHPF2
004272         MOVE PI-PREV-ACTIVITY-TRAILERS-KEY
004273                                 TO  PI-ACTIVITY-TRAILERS-KEY
004274         
      * EXEC CICS READPREV
004275*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004276*            RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)
004277*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
004278*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00012605' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132363035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PREV-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004279     ELSE
004280         MOVE PI-ACTIVITY-TRAILERS-KEY
004281                                 TO  PI-PREV-ACTIVITY-TRAILERS-KEY
004282         
      * EXEC CICS READNEXT
004283*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
004284*            RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)
004285*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
004286*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012613' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132363133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004287
004288     IF AT-SEQUENCE-NO = +90 OR +91
004289         GO TO 4100-READNEXT.
004290
004291     IF AT-TRAILER-TYPE = '6'
004292         IF AT-PAYMENT-NOTE
004293             GO TO 4100-READNEXT.
004294
004295     ADD +1  TO  WS-RECORD-COUNT.
004296     IF WS-RECORD-COUNT GREATER +0
004297        DIVIDE WS-RECORD-COUNT BY +10 GIVING WS-RECORD-DIV
004298               REMAINDER WS-RECORD-REMAINDER
004299     END-IF
004300*       IF WS-RECORD-REMAINDER = +0
004301*          EXEC CICS DELAY
004302*               INTERVAL(00001)
004303*          END-EXEC.
004304     EJECT
004305     IF AT-COMPANY-CD NOT = PI-COMPANY-CD OR
004306        AT-CARRIER    NOT = PI-CARRIER    OR
004307        AT-CLAIM-NO   NOT = PI-CLAIM-NO   OR
004308        AT-CERT-NO    NOT = PI-CERT-NO
004309         GO TO 6000-END-OF-FILE.
004310
004311     IF AT-RECORDED-DT LESS THAN PI-AFTER-DATE
004312        GO TO 4100-READNEXT.
004313
004314     IF ((EIBAID NOT = DFHPF2 AND
004315          PI-PREV-AID = DFHPF2)
004316        OR
004317         (EIBAID = DFHPF2 AND
004318          PI-PREV-AID NOT = DFHPF2)
004319        OR
004320          PI-END-OF-FILE = +1)
004321       AND
004322         WS-RECORD-COUNT NOT GREATER +1
004323             GO TO 4100-READNEXT.
004324
004325     MOVE ZERO                   TO  PI-END-OF-FILE.
004326
004327     IF (AT-TRAILER-TYPE = '1' AND
004328         PI-RES-EXP-SW = +1)
004329       OR
004330        (AT-TRAILER-TYPE = '2' AND
004331         PI-PAYMENTS-SW = +1)
004332       OR
004333        (AT-TRAILER-TYPE = '3' AND
004334         PI-AUTO-PAY-SW = +1)
004335       OR
004336        (AT-TRAILER-TYPE = '4' AND
004337         PI-LETTERS-SW = +1)
004338       OR
004339        (AT-TRAILER-TYPE = '6' AND
004340         PI-NOTES-SW = +1)
004341       OR
004342        (AT-TRAILER-TYPE = '7' AND
004343         PI-REMINDERS-SW = +1)
004344       OR
004345        (AT-TRAILER-TYPE = '8' AND
004346         PI-DENIALS-SW = +1)
004347       OR
004348        (AT-TRAILER-TYPE = '9' AND
004349         PI-INCURRED-DATE-SW = +1)
004350       OR
004351        (AT-TRAILER-TYPE = 'A' AND
004352         PI-FORMS-SW = +1)
004353             NEXT SENTENCE
004354           ELSE
004355             GO TO 4100-READNEXT.
004356
004357     ADD 1 TO LCP-ONCTR-01
004358
004359     IF LCP-ONCTR-01 = 2
004360         GO TO 6000-ENDBROWSE.
004361
004362     MOVE AT-RECORDED-BY         TO  PI-UPDATE-BY.
004363     MOVE AT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
004364     MOVE AT-CONTROL-PRIMARY     TO  PI-SAVE-KEY.
004365
004366     ADD +1                      TO  PI-RECORD-COUNT.
004367
004368     EJECT
004369     IF AT-TRAILER-TYPE NOT = '1'
004370         GO TO 4200-PAYMENT-TRAILER.
004371
004372     MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY.
004373
004374     
      * EXEC CICS READ
004375*         DATASET (WS-CLAIM-MASTER-DSID)
004376*         RIDFLD  (WS-CLAIM-KEY)
004377*         SET     (ADDRESS OF CLAIM-MASTER)
004378*    END-EXEC.
      *    MOVE '&"S        E          (   #00012705' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132373035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004379
004380     IF CL-LAST-CLOSE-DT = LOW-VALUES
004381        MOVE SPACES              TO HLSTCLOO
004382     ELSE
004383        MOVE CL-LAST-CLOSE-DT    TO DC-BIN-DATE-1
004384        MOVE SPACES              TO  DC-OPTION-CODE
004385        PERFORM 8500-DATE-CONVERSION
004386        MOVE DC-GREG-DATE-1-EDIT TO HLSTCLOO.
004387
004388     IF CL-LAST-REOPEN-DT = LOW-VALUES
004389        MOVE SPACES TO HLSTOPEO
004390     ELSE
004391        MOVE CL-LAST-REOPEN-DT   TO DC-BIN-DATE-1
004392        MOVE SPACES              TO  DC-OPTION-CODE
004393        PERFORM 8500-DATE-CONVERSION
004394        MOVE DC-GREG-DATE-1-EDIT TO HLSTOPEO.
004395
004396     MOVE EL142H                 TO  PI-MAP-NAME.
004397
004398     MOVE AT-TRAILER-TYPE        TO  HTLRTYPO.
004399     MOVE AT-SEQUENCE-NO         TO  HSEQO.
004400
004401     MOVE SPACES                 TO  DC-OPTION-CODE.
004402     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
004403     PERFORM 8500-DATE-CONVERSION.
004404     MOVE DC-GREG-DATE-1-EDIT    TO  HRECDTEO.
004405
004406     MOVE AT-RECORDED-BY         TO  HBYO.
004407
004408     MOVE AT-RESERVES-LAST-MAINT-DT   TO  DC-BIN-DATE-1.
004409     MOVE ' '                         TO  DC-OPTION-CODE.
004410     PERFORM 8500-DATE-CONVERSION.
004411     IF NO-CONVERSION-ERROR
004412         MOVE DC-GREG-DATE-1-EDIT     TO  HMANTONO
004413     ELSE
004414         MOVE SPACES                  TO  HMANTONO.
004415
004416     MOVE AT-RESERVES-LAST-UPDATED-BY TO  HMANTBYO.
004417
004418     MOVE AT-LAST-MAINT-HHMMSS        TO  TIME-IN.
004419     MOVE TIME-OUT                    TO  HMANTATO.
004420
004421     MOVE AT-MANUAL-SW           TO  HRESMANO.
004422     INSPECT HRESMANO CONVERTING ' 1' TO 'NY'.
004423
004424     MOVE AT-FUTURE-SW           TO  HRESFUTO.
004425     INSPECT HRESFUTO CONVERTING ' 1' TO 'NY'.
004426
004427     MOVE AT-PTC-SW              TO  HRESAHPO.
004428     INSPECT HRESAHPO CONVERTING ' 1' TO 'NY'.
004429
004430     MOVE AT-IBNR-SW             TO  HRESIBNO.
004431     INSPECT HRESIBNO CONVERTING ' 1' TO 'NY'.
004432
004433     MOVE AT-PTC-LF-SW           TO  HRESLFPO.
004434     INSPECT HRESLFPO CONVERTING ' 1' TO 'NY'.
004435
004436     MOVE AT-CURRENT-MANUAL-RESERVE  TO  HMANAMTO.
004437     MOVE AT-FUTURE-RESERVE      TO  HFUTAMTO.
004438     MOVE AT-IBNR-RESERVE        TO  HIBNAMTO.
004439     MOVE AT-PAY-CURRENT-RESERVE TO  HPTCAMTO.
004440
004441     MOVE AT-ITD-CHARGEABLE-EXPENSE TO HITDCO.
004442     MOVE AT-ITD-PAID-EXPENSES   TO  HITDNCO.
004443
004444     MOVE AT-CDT-ACCESS-METHOD   TO  HCDTAMO.
004445     MOVE AT-PERCENT-OF-CDT      TO  HPCTCDTO.
004446
004447     MOVE AT-EXPENSE-METHOD      TO  HEXPO.
004448
004449     IF AT-EXPENSE-METHOD = '2' OR '4'
004450         MOVE AT-EXPENSE-DOLLAR  TO  HEXPAMTO
004451     ELSE
004452         IF AT-EXPENSE-METHOD = '3'
004453             MOVE AT-EXPENSE-PERCENT  TO  HEXPAMTO
004454         ELSE
004455             MOVE ZERO           TO  HEXPAMTO.
004456
004457     MOVE -1                     TO  HMAINTL.
004458
004459     MOVE +1                     TO  WS-INDEX.
004460     SET EL142H-INDEX1
004461         EL142H-INDEX2  TO  +1.
004462
004463 4110-MOVE-CAUSE.
004464     MOVE AT-OPEN-CLOSE-DATE (WS-INDEX)  TO  DC-BIN-DATE-1.
004465
004466     IF DC-BIN-DATE-1 = LOW-VALUES OR SPACES
004467         GO TO 4120-BUMP-INDEX.
004468
004469     MOVE SPACES                 TO  DC-OPTION-CODE.
004470     PERFORM 8500-DATE-CONVERSION.
004471     MOVE DC-GREG-DATE-1-EDIT    TO EL142H-DATE
004472                                    (EL142H-INDEX1  EL142H-INDEX2)
004473
004474     MOVE AT-OPEN-CLOSE-TYPE (WS-INDEX) TO EL142H-OC
004475                                    (EL142H-INDEX1  EL142H-INDEX2)
004476
004477     MOVE AT-OPEN-CLOSE-REASON (WS-INDEX) TO EL142H-CAUSE
004478                                   (EL142H-INDEX1  EL142H-INDEX2).
004479
004480 4120-BUMP-INDEX.
004481     IF WS-INDEX LESS +6
004482         ADD +1                  TO  WS-INDEX
004483     ELSE
004484         GO TO 4100-READNEXT.
004485
004486     IF EL142H-INDEX1 LESS +5
004487         SET EL142H-INDEX1 UP BY +1
004488         GO TO 4110-MOVE-CAUSE.
004489
004490     IF EL142H-INDEX2 LESS +2
004491         SET EL142H-INDEX1 TO +1
004492         SET EL142H-INDEX2 UP BY +1
004493         GO TO 4110-MOVE-CAUSE.
004494
004495     GO TO 4100-READNEXT.
004496
004497     EJECT
004498 4200-PAYMENT-TRAILER.
004499     IF AT-TRAILER-TYPE NOT = '2'
004500         GO TO 4300-AUTO-PAY-TRAILER.
004501
004502     MOVE EL142B                 TO  PI-MAP-NAME.
004503
004504     MOVE AT-TRAILER-TYPE        TO  BTLRTYPO.
004505     MOVE AT-SEQUENCE-NO         TO  BSEQO.
004506
004507     MOVE SPACES                 TO  DC-OPTION-CODE.
004508     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
004509     PERFORM 8500-DATE-CONVERSION.
004510     MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO.
004511
004512     MOVE AT-RECORDED-BY         TO  BBYO.
004513
004514     MOVE AT-PAYMENT-LAST-UPDATED-BY TO  BMANTBYO.
004515     MOVE AT-PAYMENT-LAST-MAINT-DT   TO  DC-BIN-DATE-1.
004516     MOVE ' '                        TO  DC-OPTION-CODE.
004517     PERFORM 8500-DATE-CONVERSION.
004518     IF NO-CONVERSION-ERROR
004519         MOVE DC-GREG-DATE-1-EDIT    TO  BMANTONO
004520     ELSE
004521         MOVE SPACES                 TO  BMANTONO.
004522
004523     MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
004524     MOVE TIME-OUT                   TO  BMANTATO.
004525
004526     MOVE AT-CHECK-NO            TO  BCKNOO.
004527     if at-ach-payment = 'Y'
004528        move 'YES'               to bachpmto
004529        go to 4200-ach-cashed-dt
004530     else
004531        move 'NO '               to bachpmto
004532     end-if
004533
004534**** This routine will connect to the Logic Database on SQL Server
004535**** and call a stored procedure to determine the check cashed dat
004536
004537     PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
004538     IF SQLCODE = 0
004539        PERFORM 7100-GET-CHK-CASHED-DT  THRU 7100-EXIT
004540        if sqlcode = zeros
004541           move ws-check-cashed-dt (3:2)
004542                                 to dc-ymd-year
004543           move ws-check-cashed-dt (6:2)
004544                                 to dc-ymd-month
004545           move ws-check-cashed-dt (9:2)
004546                                 to dc-ymd-day
004547        else
004548           perform 7110-check-manual thru 7110-exit
004549           IF SQLCODE = 0
004550              move ws-check-cashed-dt (7:2)
004551                                 to dc-ymd-year
004552              move ws-check-cashed-dt (1:2)
004553                                 to dc-ymd-month
004554              move ws-check-cashed-dt (4:2)
004555                                 to dc-ymd-day
004556           end-if
004557        end-if
004558        if sqlcode = zeros
004559           move '3' to dc-option-code
004560           perform 8500-date-conversion
004561           if (no-conversion-error)
004562              and (dc-bin-date-1 > at-check-written-dt)
004563              move dc-greg-date-1-edit to bcashedo
004564*             MOVE WS-CHECK-CASHED-DT TO BCASHEDO
004565           else
004566              move spaces        to bcashedo
004567           end-if
004568        END-IF
004569     END-IF
004570     PERFORM 7200-DISCONNECT THRU 7200-EXIT
004571     go to 4200-carry-on-my-waward-son
004572
004573     .
004574 4200-ach-cashed-dt.
004575
004576     MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY
004577
004578     
      * EXEC CICS READ
004579*         DATASET (WS-CLAIM-MASTER-DSID)
004580*         RIDFLD  (WS-CLAIM-KEY)
004581*         SET     (ADDRESS OF CLAIM-MASTER)
004582*    END-EXEC
      *    MOVE '&"S        E          (   #00012909' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132393039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004583
004584     move spaces                 to ws-check-cashed-dt
004585     move zeros                  to sqlcode
004586
004587     PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
004588     IF SQLCODE <> 0
004589        display ' bad connect ' sqlcode
004590        go to 4200-carry-on-my-waward-son
004591     end-if
004592
004593     move at-carrier             to ws-carrier
004594     move cl-cert-state          to ws-state
004595     move cl-cert-account        to ws-account-no
004596     move at-cert-no             to ws-cert-no
004597     move at-claim-no            to ws-claim-no
004598     move zeros                  to ws-check-no
004599     move at-check-no            to ws-check-no (4:7)
004600
004601     if pi-company-id = 'CID'
004603        exec sql
                 SELECT
004604              CASHED_DATE
004605           INTO
004606              :ws-check-cashed-dt
004607           FROM
004608              CLM_PMTS_ACH
004609           WHERE
004610                  CARRIER   = :ws-carrier
004611              and STATE     = :ws-state
004612              and ACCOUNT   = :ws-account-no
004613              and CERT_NO   = :ws-cert-no
004614              and CLAIM_NO  = :ws-claim-no
004615              and CHECK_NO  = :ws-check-no
004616        end-exec
004617     else
004619        exec sql
                 SELECT
004620              CASHED_DATE
004621           INTO
004622              :ws-check-cashed-dt
004623           FROM
004624              DCC_CLM_PMTS_ACH
004625           WHERE
004626                  CARRIER   = :ws-carrier
004627              and STATE     = :ws-state
004628              and ACCOUNT   = :ws-account-no
004629              and CERT_NO   = :ws-cert-no
004630              and CLAIM_NO  = :ws-claim-no
004631              and CHECK_NO  = :ws-check-no
004632        end-exec
004633     end-if
004634
004635     if sqlcode not = 0 and 1
004636        move sqlcode             to ws-sql-code
004637        move ws-sql-code         to ws-dis-sql-code
004638        display ' dis sql code ' ws-dis-sql-code
004639        display "Error: cannot read row "
004640        display ' sql return code ' sqlcode
004641        display ' sql err mess    ' sqlerrmc
004642     else
004643        move ws-check-cashed-dt (3:2)
004644                                 to dc-ymd-year
004645        move ws-check-cashed-dt (6:2)
004646                                 to dc-ymd-month
004647        move ws-check-cashed-dt (9:2)
004648                                 to dc-ymd-day
004649        move '3'                 to dc-option-code
004650        perform 8500-date-conversion
004651        if (no-conversion-error)
004652           and (dc-bin-date-1 > at-check-written-dt)
004653           move dc-greg-date-1-edit
004654                                 to bcashedo
004655        else
004656           move spaces           to bcashedo
004657        end-if
004658     end-if
004659
004660     PERFORM 7200-DISCONNECT THRU 7200-EXIT
004661
004662     .
004663 4200-carry-on-my-waward-son.
004664
004665     IF OFFLINE-PMT
004666        MOVE AL-UANOF            TO BCKNOA
004667       ELSE
004668        MOVE AL-SANOF            TO BCKNOA.
004669
004670     IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
004671         MOVE SPACES             TO  DC-OPTION-CODE
004672         MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1
004673         PERFORM 8500-DATE-CONVERSION
004674         MOVE DC-GREG-DATE-1-EDIT TO  BDTWRITO.
004675
004676     MOVE AT-RECORDED-BY         TO  BWRITBYO.
004677     MOVE AT-PMT-APPROVED-BY     TO  BAPPVBYO.
004678     MOVE AT-AMOUNT-PAID         TO  BAMTO.
004679
004680     IF AT-PAID-THRU-DT NOT = LOW-VALUES
004681        MOVE SPACES             TO  DC-OPTION-CODE
004682        MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1
004683        PERFORM 8500-DATE-CONVERSION
004684        MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO
004685        IF PI-USES-PAID-TO
004686           MOVE '6'                TO  DC-OPTION-CODE
004687           MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1
004688           MOVE +1                 TO  DC-ELAPSED-DAYS
004689           MOVE +0                 TO  DC-ELAPSED-MONTHS
004690           PERFORM 8500-DATE-CONVERSION
004691           MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO.
004692
004693     IF AT-TO-BE-WRITTEN-DT NOT = LOW-VALUES AND SPACES
004694         MOVE SPACES             TO  DC-OPTION-CODE
004695         MOVE AT-TO-BE-WRITTEN-DT TO  DC-BIN-DATE-1
004696         PERFORM 8500-DATE-CONVERSION
004697         MOVE DC-GREG-DATE-1-EDIT TO  BHOLDATO.
004698
004699     MOVE AT-DAYS-IN-PERIOD      TO  BDAYSPDO
004700     IF AT-PRINT-EOB-WITH-CHECK = 'Y' OR 'S'
004701        MOVE AT-PRINT-EOB-WITH-CHECK TO BEOBYNO
004702     ELSE
004703        MOVE 'N'                 TO BEOBYNO
004704     END-IF
004705     IF AT-PRINT-CLM-FORM = 'N'
004706         MOVE 'N'                TO BCLMYNO
004707     ELSE
004708         MOVE 'Y'                TO BCLMYNO
004709     END-IF
004710     IF AT-PRINT-SURVEY = 'N'
004711         MOVE 'N'                TO BSRVYNO
004712     ELSE
004713         MOVE 'Y'                TO BSRVYNO
004714     END-IF
004715     IF AT-SPECIAL-RELEASE = 'Y'
004716         MOVE 'Y'                TO BSPRELO
004717     ELSE
004718         MOVE 'N'                TO BSPRELO
004719     END-IF
004720     IF AT-CHECK-WRITTEN-DT = LOW-VALUES
004721      AND AT-VOID-DT = LOW-VALUES
004722        MOVE AL-UANON            TO BEOBYNA
004723        MOVE AL-UANON            TO BCLMYNA
004724        MOVE AL-UANON            TO BSPRELA
004725        IF PI-APPROVAL-LEVEL = '4' OR '5'
004726           MOVE AL-UANON         TO BSRVYNA
004727        END-IF
004728     END-IF
004729     IF AT-PAYMENT-TYPE = 'I'
004730        IF AT-INT-RATE NOT NUMERIC
004731           MOVE ZEROS            TO AT-INT-RATE
004732        END-IF
004733        MOVE AT-INT-RATE         TO  BDAYRATO
004734        MOVE 'INTEREST RT  -'    TO  BRATHDO
004735     ELSE
004736        MOVE AT-DAILY-RATE       TO  BDAYRATO
004737     END-IF
004738
004739     IF AT-CV-PMT-CODE = ' '
004740         GO TO 4200-DISPLAY-PMT-DESC.
004741
004742     IF AT-CV-PMT-CODE = '1'
004743         MOVE 'FULL DEATH'               TO  BPAYTYPO.
004744
004745     IF AT-CV-PMT-CODE = '2'
004746         MOVE 'HALF DEATH'               TO  BPAYTYPO.
004747
004748     IF AT-CV-PMT-CODE = '3'
004749         MOVE 'FULL AD&D'                TO  BPAYTYPO.
004750
004751     IF AT-CV-PMT-CODE = '4'
004752         MOVE 'HALF AD&D'                TO  BPAYTYPO.
004753
004754     IF AT-CV-PMT-CODE = '5'
004755         MOVE 'FULL RIDER'               TO  BPAYTYPO.
004756
004757     IF AT-CV-PMT-CODE = '6'
004758         MOVE 'HALF RIDER'               TO  BPAYTYPO.
004759
004760     IF AT-CV-PMT-CODE = '7'
004761         MOVE 'NON-CHG EXP'              TO  BPAYTYPO.
004762
004763     IF AT-CV-PMT-CODE = '8'
004764         MOVE 'ADDITIONAL'               TO  BPAYTYPO.
004765
004766     GO TO 4200-PAYMENT-TRAILER-CONT.
004767
004768 4200-DISPLAY-PMT-DESC.
004769
004770     EVALUATE AT-PAYMENT-TYPE
004771        WHEN '1'
004772           MOVE 'PARTIAL PAYMENT'        TO BPAYTYPO
004773        WHEN '2'
004774           MOVE 'FINAL PAYMENT'          TO BPAYTYPO
004775        WHEN '3'
004776           MOVE 'LUMP SUM PAYMENT'       TO BPAYTYPO
004777        WHEN '4'
004778           MOVE 'ADDITIONAL PAYMENT'     TO BPAYTYPO
004779        WHEN '5'
004780           MOVE 'CHARGEABLE PAYMENT'     TO BPAYTYPO
004781        WHEN '6'
004782           MOVE 'NON-CHARGEABLE PAYMENT' TO BPAYTYPO
004783        WHEN '7'
004784           MOVE 'LIFE PREMIUM REFUND'    TO BPAYTYPO
004785        WHEN '8'
004786           MOVE 'A & H PREMIUM REFUND'   TO BPAYTYPO
004787        WHEN 'I'
004788           MOVE 'INTEREST PAYMENT   '    TO BPAYTYPO
004789        WHEN OTHER
004790           MOVE 'ENTRY CORRECTION'       TO BPAYTYPO
004791     END-EVALUATE
004792
004793     .
004794 4200-PAYMENT-TRAILER-CONT.
004795
004796     MOVE AT-FORCE-CONTROL       TO  BFORCEDO.
004797     INSPECT BFORCEDO CONVERTING ' 1' TO 'NY'.
004798
004799     IF AT-VOID-DT NOT = LOW-VALUES
004800         MOVE SPACES              TO  DC-OPTION-CODE
004801         MOVE AT-VOID-DT          TO  DC-BIN-DATE-1
004802         PERFORM 8500-DATE-CONVERSION
004803         MOVE DC-GREG-DATE-1-EDIT TO  BVOIDDTO.
004804
004805     IF AT-PMT-PROOF-DT NOT = LOW-VALUES
004806         MOVE SPACES              TO  DC-OPTION-CODE
004807         MOVE AT-PMT-PROOF-DT     TO  WS-PRF-DT
004808         MOVE AT-PMT-PROOF-DT     TO  DC-BIN-DATE-1
004809         PERFORM 8500-DATE-CONVERSION
004810         MOVE DC-GREG-DATE-1-EDIT TO  BPRFDTI.
004811
004812     IF AT-PAYEE-TYPE EQUAL 'I'
004813         MOVE 'INSURED'          TO  BPAYEEO
004814       ELSE
004815     IF AT-PAYEE-TYPE EQUAL 'B'
004816         MOVE 'BENEFICIARY'      TO  BPAYEEO
004817       ELSE
004818     IF AT-PAYEE-TYPE EQUAL 'A'
004819         MOVE 'ACCOUNT'          TO  BPAYEEO
004820       ELSE
004821     IF AT-PAYEE-TYPE EQUAL 'O'
004822         MOVE 'OTHER 1'          TO  BPAYEEO
004823       ELSE
004824     IF AT-PAYEE-TYPE EQUAL 'Q'
004825         MOVE 'REM BORR   '      TO  BPAYEEO
004826       ELSE
004827     IF AT-PAYEE-TYPE EQUAL 'P'
004828         MOVE 'DOCTOR'           TO  BPAYEEO
004829       ELSE
004830     IF AT-PAYEE-TYPE EQUAL 'E'
004831         MOVE 'EMPLOYER'         TO  BPAYEEO.
004832
004833
004834     MOVE AT-PAYEES-NAME         TO  BPNAMEO.
004835
004836     MOVE AT-ADDL-RESERVE        TO  BRESERVO.
004837     IF AT-PAYMENT-TYPE NOT = 'I'
004838        MOVE AT-EXPENSE-PER-PMT  TO  BEXPO
004839     END-IF
004840
004841     IF AT-PMT-SELECT-DT NOT = LOW-VALUES
004842         MOVE SPACES             TO  DC-OPTION-CODE
004843         MOVE AT-PMT-SELECT-DT   TO  DC-BIN-DATE-1
004844         PERFORM 8500-DATE-CONVERSION
004845         MOVE DC-GREG-DATE-1-EDIT TO  BCRSELO.
004846
004847     IF AT-PMT-ACCEPT-DT NOT = LOW-VALUES
004848         MOVE SPACES             TO  DC-OPTION-CODE
004849         MOVE AT-PMT-ACCEPT-DT   TO  DC-BIN-DATE-1
004850         PERFORM 8500-DATE-CONVERSION
004851         MOVE DC-GREG-DATE-1-EDIT TO  BCRACPO.
004852
004853     IF AT-VOID-SELECT-DT NOT = LOW-VALUES
004854         MOVE SPACES             TO  DC-OPTION-CODE
004855         MOVE AT-VOID-SELECT-DT  TO  DC-BIN-DATE-1
004856         PERFORM 8500-DATE-CONVERSION
004857         MOVE DC-GREG-DATE-1-EDIT TO  BVOIDSDO.
004858
004859     IF AT-VOID-ACCEPT-DT NOT = LOW-VALUES
004860         MOVE SPACES             TO  DC-OPTION-CODE
004861         MOVE AT-VOID-ACCEPT-DT  TO  DC-BIN-DATE-1
004862         PERFORM 8500-DATE-CONVERSION
004863         MOVE DC-GREG-DATE-1-EDIT TO  BVOIDACO.
004864
004865     IF AT-PAYMENT-ORIGIN = '1'
004866         MOVE 'ON-LINE'          TO  BORIGINO
004867       ELSE
004868     IF AT-PAYMENT-ORIGIN = '2'
004869         MOVE 'AUTO PAY'         TO  BORIGINO
004870       ELSE
004871     IF AT-PAYMENT-ORIGIN = '3'
004872         MOVE 'MANUAL'           TO  BORIGINO
004873       ELSE
004874         MOVE AT-PAYMENT-ORIGIN  TO  BORIGINO.
004875
004876     IF AT-EXPENSE-TYPE = SPACE
004877         MOVE AL-SADOF           TO BEXPHDGA
004878         MOVE AL-SANOF           TO BEXPTYPA
004879     ELSE
004880         MOVE AT-EXPENSE-TYPE    TO BEXPTYPO
004881         MOVE AL-SANOF           TO BEXPHDGA
004882         MOVE AL-UANOF           TO BEXPTYPA.
004883
004884     MOVE AT-CHECK-QUE-CONTROL   TO  BCKQUEO.
004885     MOVE AT-CHECK-QUE-SEQUENCE  TO  BCKSEQO.
004886
004887     IF PI-PROCESSOR-ID = 'LGXX'
004888         MOVE AL-UNNOF           TO  BCKQUEA  BCKSEQA
004889                                     BCRSELA  BVOIDSDA
004890         MOVE AL-UANOF           TO  BPAYEEA  BEXPTYPA
004891                                     BCRACPA  BHOLDATA
004892                                     BVOIDACA
004893     ELSE
004894         MOVE AL-SANOF           TO  BHOLDATA
004895         IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
004896          OR AT-VOID-DT NOT = LOW-VALUES
004897             MOVE AL-SANOF       TO  BPAYEEA
004898         ELSE
004899             MOVE AL-UANOF       TO  BPAYEEA.
004900
004901     IF (PI-COMPANY-ID = 'LAP' OR 'RMC') OR
004902        (PI-PROCESSOR-ID = 'LGXX')
004903         MOVE AL-UANOF           TO  BPMTORGA.
004904
004905     MOVE -1                     TO  BMAINTL.
004906
004907     IF AT-PAYMENT-NOTE-SEQ-NO = 0
004908         GO TO 4100-READNEXT.
004909
004910 4200-READ-PAYMENT-NOTE-TRLR.
004911
004912     
      * EXEC CICS HANDLE CONDITION
004913*        NOTFND   (4200-NOTE-TRLR-NOTFND)
004914*    END-EXEC.
      *    MOVE '"$I                   ! '' #00013243' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303133323433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004915
004916     MOVE PI-ACTIVITY-TRAILERS-KEY   TO
004917                                PI-PREV-ACTIVITY-TRAILERS-KEY.
004918
004919     MOVE AT-PAYMENT-NOTE-SEQ-NO TO  PI-ATK-SEQUENCE-NO.
004920
004921     
      * EXEC CICS READ
004922*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
004923*        RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)
004924*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004925*    END-EXEC.
      *    MOVE '&"S        E          (   #00013252' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133323532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004926
004927     IF AT-PAYMENT-NOTE
004928         MOVE AT-INFO-LINE-1     TO  BNOTE1O
004929         MOVE AT-INFO-LINE-2     TO  BNOTE2O
004930         IF AT-EOB-CODES-PRESENT
004931            MOVE AL-SANOF        TO  BNOTE2A
004932         END-IF
004933     ELSE
004934         MOVE SPACES             TO  BNOTE1O
004935                                     BNOTE2O.
004936
004937     MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO
004938                                     PI-ACTIVITY-TRAILERS-KEY.
004939
004940     GO TO 4100-READNEXT.
004941
004942 4200-NOTE-TRLR-NOTFND.
004943
004944     MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO
004945                                     PI-ACTIVITY-TRAILERS-KEY.
004946
004947     GO TO 4100-READNEXT.
004948
004949     EJECT
004950 4300-AUTO-PAY-TRAILER.
004951     IF AT-TRAILER-TYPE NOT = '3'
004952         GO TO 4400-CORRESPONDENCE-TRAILER.
004953
004954     MOVE EL142C                 TO  PI-MAP-NAME.
004955
004956     MOVE AT-TRAILER-TYPE        TO  CTLRTYPO.
004957     MOVE AT-SEQUENCE-NO         TO  CSEQO.
004958
004959     MOVE SPACES                 TO  DC-OPTION-CODE.
004960     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
004961     PERFORM 8500-DATE-CONVERSION.
004962     MOVE DC-GREG-DATE-1-EDIT    TO  CRECDTEO.
004963
004964     MOVE AT-RECORDED-BY         TO  CBYO.
004965
004966     MOVE AT-AUTO-PAY-LAST-MAINT-DT  TO  DC-BIN-DATE-1.
004967     MOVE ' '                        TO  DC-OPTION-CODE.
004968     PERFORM 8500-DATE-CONVERSION.
004969     IF NO-CONVERSION-ERROR
004970         MOVE DC-GREG-DATE-1-EDIT    TO  CMANTONO
004971     ELSE
004972         MOVE SPACES                 TO  CMANTONO.
004973
004974     MOVE AT-AUTO-PAY-LAST-UPDATED-BY    TO  CMANTBYO.
004975
004976     MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
004977     MOVE TIME-OUT                   TO  CMANTATO.
004978
004979     IF AT-SCHEDULE-START-DT NOT = LOW-VALUES
004980         MOVE SPACES             TO  DC-OPTION-CODE
004981         MOVE AT-SCHEDULE-START-DT   TO  DC-BIN-DATE-1
004982         PERFORM 8500-DATE-CONVERSION
004983         MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTEO.
004984
004985     IF AT-TERMINATED-DT NOT = LOW-VALUES
004986         MOVE SPACES             TO  DC-OPTION-CODE
004987         MOVE AT-TERMINATED-DT   TO  DC-BIN-DATE-1
004988         PERFORM 8500-DATE-CONVERSION
004989         MOVE DC-GREG-DATE-1-EDIT TO  CREPDTEO.
004990
004991     MOVE AT-FIRST-PMT-AMT       TO  C1STPAO.
004992     MOVE AT-REGULAR-PMT-AMT     TO  CREGPAO.
004993
004994     IF AT-1ST-PAY-THRU-DT NOT = LOW-VALUES
004995         IF PI-USES-PAID-TO
004996             MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
004997             MOVE '6'                    TO  DC-OPTION-CODE
004998             MOVE +1                     TO  DC-ELAPSED-DAYS
004999             MOVE +0                     TO  DC-ELAPSED-MONTHS
005000             PERFORM 8500-DATE-CONVERSION
005001             IF NO-CONVERSION-ERROR
005002                 MOVE DC-GREG-DATE-1-EDIT TO C1STPSO
005003             ELSE
005004                 MOVE LOW-VALUES         TO  C1STPSO
005005         ELSE
005006             MOVE SPACES                 TO  DC-OPTION-CODE
005007             MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
005008             MOVE +0                     TO  DC-ELAPSED-DAYS
005009                                             DC-ELAPSED-MONTHS
005010             PERFORM 8500-DATE-CONVERSION
005011             IF NO-CONVERSION-ERROR
005012                 MOVE DC-GREG-DATE-1-EDIT TO C1STPSO
005013             ELSE
005014                 MOVE SPACES             TO  C1STPSO.
005015
005016     IF AT-SCHEDULE-END-DT NOT = LOW-VALUES
005017         IF PI-USES-PAID-TO
005018             MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
005019             MOVE '6'                    TO  DC-OPTION-CODE
005020             MOVE +1                     TO  DC-ELAPSED-DAYS
005021             MOVE +0                     TO  DC-ELAPSED-MONTHS
005022             PERFORM 8500-DATE-CONVERSION
005023             IF NO-CONVERSION-ERROR
005024                 MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO
005025             ELSE
005026                 MOVE LOW-VALUES         TO  CLSTPSO
005027         ELSE
005028             MOVE SPACES                 TO  DC-OPTION-CODE
005029             MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
005030             MOVE +0                     TO  DC-ELAPSED-DAYS
005031                                             DC-ELAPSED-MONTHS
005032             PERFORM 8500-DATE-CONVERSION
005033             IF NO-CONVERSION-ERROR
005034                 MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO
005035             ELSE
005036                 MOVE LOW-VALUES         TO  CLSTPSO.
005037
005038     MOVE AT-DAYS-IN-1ST-PMT     TO  CDIFPO.
005039     MOVE AT-INTERVAL-MONTHS     TO  CMBPAYO.
005040
005041     IF AT-LAST-PMT-TYPE = 'P'
005042         MOVE 'PARTIAL'          TO  CLSTPATO
005043     ELSE
005044         IF AT-LAST-PMT-TYPE = 'F'
005045             MOVE 'FINAL  '      TO  CLSTPATO.
005046
005047     IF AT-AUTO-PAYEE-TYPE EQUAL 'I'
005048         MOVE 'INSURED'          TO  CPAYEEO
005049       ELSE
005050     IF AT-AUTO-PAYEE-TYPE EQUAL 'B'
005051         MOVE 'BENEFICIARY'      TO  CPAYEEO
005052       ELSE
005053     IF AT-AUTO-PAYEE-TYPE EQUAL 'A'
005054         MOVE 'ACCOUNT'          TO  CPAYEEO
005055       ELSE
005056     IF AT-AUTO-PAYEE-TYPE EQUAL 'O'
005057         MOVE 'OTHER 1'          TO  CPAYEEO
005058       ELSE
005059         MOVE 'REM BORR'         TO  CPAYEEO.
005060
005061     MOVE AT-AUTO-END-LETTER     TO  CENDLETO.
005062
005063     MOVE -1                     TO  CPFKL.
005064
005065     GO TO 4100-READNEXT.
005066
005067     EJECT
005068 4400-CORRESPONDENCE-TRAILER.
005069     IF AT-TRAILER-TYPE NOT = '4'
005070         GO TO 4500-GENERAL-INFO-TRAILER.
005071
005072     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.
005073     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
005074     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
005075     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
005076     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.
005077
005078     
      * EXEC CICS READ
005079*         DATASET (WS-NAPERSOFT-DSID)
005080*         RIDFLD  (WS-NAPERSOFT-KEY)
005081*         SET     (ADDRESS OF NAPERSOFT-FILE)
005082*         RESP    (WS-RESPONSE)
005083*    END-EXEC.
      *    MOVE '&"S        E          (  N#00013409' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303133343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005084
005085     IF RESP-NORMAL
005086          MOVE NA-ENCLOSURE-CD TO DENCCODO
005087                                  PI-ENC-CODE
005088          MOVE NA-CREATED-IN-NAPERSOFT TO
005089                                  PI-CREATED-IN-NAPERSOFT
005090     ELSE
005091          MOVE SPACES TO DENCCODO
005092                         PI-ENC-CODE
005093                         PI-CREATED-IN-NAPERSOFT
005094     END-IF.
005095
005096     MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.
005097
005098     
      * EXEC CICS READ
005099*        DATASET   (WS-CLAIM-MASTER-DSID)
005100*        RIDFLD    (WS-CLAIM-KEY)
005101*        SET       (ADDRESS OF CLAIM-MASTER)
005102*    END-EXEC.
      *    MOVE '&"S        E          (   #00013429' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133343239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005103
005104     
      * EXEC CICS HANDLE CONDITION
005105*        NOTFND (4410-ADDRESS-NOT-FOUND)
005106*    END-EXEC.
      *    MOVE '"$I                   ! ( #00013435' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303133343335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005107
005108     MOVE EL142D                 TO  PI-MAP-NAME.
005109
005110     MOVE +2                     TO  EMI-NUMBER-OF-LINES
005111                                     EMI-SWITCH2.
005112
005113     MOVE AT-TRAILER-TYPE        TO  DTLRTYPO.
005114     MOVE AT-SEQUENCE-NO         TO  DSEQO.
005115
005116     MOVE SPACES                 TO  DC-OPTION-CODE.
005117     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
005118     PERFORM 8500-DATE-CONVERSION.
005119     MOVE DC-GREG-DATE-1-EDIT    TO  DRECDTEO.
005120
005121     MOVE AT-RECORDED-BY         TO  DBYO.
005122
005123     MOVE AT-CORR-LAST-MAINT-DT      TO  DC-BIN-DATE-1.
005124     MOVE ' '                        TO  DC-OPTION-CODE.
005125     PERFORM 8500-DATE-CONVERSION.
005126     IF NO-CONVERSION-ERROR
005127         MOVE DC-GREG-DATE-1-EDIT    TO  DMANTONO
005128     ELSE
005129         MOVE SPACES                 TO  DMANTONO.
005130
005131     MOVE AT-CORR-LAST-UPDATED-BY    TO  DMANTBYO.
005132
005133     MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
005134     MOVE TIME-OUT               TO  DMANTATO.
005135
005136     MOVE AT-STD-LETTER-FORM     TO  DFORMNOO.
005137     MOVE AT-LETTER-ARCHIVE-NO   TO  DARCHNOO.
005138     MOVE AT-RESEND-LETTER-FORM  TO  DRESFRMO.
005139     MOVE AT-AUTO-CLOSE-IND      TO  DAUTOCLO.
005140     IF AT-LETTER-TO-BENE EQUAL 'Y'
005141         MOVE 'LETTER SENT TO BENEFICIARY' TO DBENLETO
005142     ELSE
005143         MOVE LOW-VALUES TO DBENLETO
005144     END-IF.
005145
005146     IF AT-LETTER-ARCHIVE-NO = ZEROS
005147        MOVE AL-UANON TO DFORMNOA.
005148
005149     IF AT-LETTER-SENT-DT NOT = LOW-VALUES
005150         MOVE SPACES             TO  DC-OPTION-CODE
005151         MOVE AT-LETTER-SENT-DT  TO  DC-BIN-DATE-1
005152         PERFORM 8500-DATE-CONVERSION
005153         MOVE DC-GREG-DATE-1-EDIT TO  DDTSENTI.
005154
005155     IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
005156         MOVE SPACES             TO  DC-OPTION-CODE
005157         MOVE AT-AUTO-RE-SEND-DT TO  DC-BIN-DATE-1
005158         PERFORM 8500-DATE-CONVERSION
005159         MOVE DC-GREG-DATE-1-EDIT TO  DRESENDI.
005160
005161     IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES
005162         MOVE SPACES             TO  DC-OPTION-CODE
005163         MOVE AT-RECEIPT-FOLLOW-UP TO  DC-BIN-DATE-1
005164         PERFORM 8500-DATE-CONVERSION
005165         MOVE DC-GREG-DATE-1-EDIT TO  DREPLYI.
005166
005167     IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
005168         MOVE SPACES             TO  DC-OPTION-CODE
005169         MOVE AT-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1
005170         PERFORM 8500-DATE-CONVERSION
005171         MOVE DC-GREG-DATE-1-EDIT TO  DRECEVEI.
005172
005173     IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES
005174         MOVE SPACES             TO  DC-OPTION-CODE
005175         MOVE AT-INITIAL-PRINT-DATE  TO  DC-BIN-DATE-1
005176         PERFORM 8500-DATE-CONVERSION
005177         MOVE DC-GREG-DATE-1-EDIT TO  DINPRNTI.
005178
005179     IF AT-RESEND-PRINT-DATE  NOT = LOW-VALUES
005180         MOVE SPACES             TO  DC-OPTION-CODE
005181         MOVE AT-RESEND-PRINT-DATE   TO  DC-BIN-DATE-1
005182         PERFORM 8500-DATE-CONVERSION
005183         MOVE DC-GREG-DATE-1-EDIT TO  DREPRNTI.
005184
005185     IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES AND SPACES
005186         MOVE AL-SANOF               TO DENCCODA
005187     END-IF.
005188
005189     IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
005190         MOVE AL-SANOF               TO DRECEVEA
005191                                        DRESENDA
005192                                        DREPLYA
005193                                        DSTOPLTA
005194     END-IF.
005195
005196     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
005197         MOVE SPACES           TO DC-OPTION-CODE
005198         MOVE AT-STOP-LETTER-DT TO DC-BIN-DATE-1
005199         PERFORM 8500-DATE-CONVERSION
005200         MOVE DC-GREG-DATE-1-EDIT TO DSTOPLTI
005201         MOVE AL-SANOF            TO DRECEVEA
005202                                     DRESENDA
005203                                     DREPLYA
005204                                     DENCCODA
005205     END-IF.
005206
005207     IF PI-COMPANY-ID NOT = 'DMD'
005208         GO TO 4400-CONTINUE.
005209
005210** DMD CODE START *****************
005211*    MOVE AT-DMD-LETTER-STATUS       TO DMDLETSI.
005212*
005213*    IF AT-DMD-LETTER-ONLINE
005214*        MOVE 'ONLINE'               TO DMDLETDO
005215*      ELSE
005216*    IF AT-DMD-LETTER-PURGED
005217*        MOVE 'PURGED'               TO DMDLETDO
005218*      ELSE
005219*    IF AT-DMD-LETTER-RELOADED
005220*        MOVE 'RELOADED'             TO DMDLETDO.
005221*
005222*    IF AT-DMD-LETTER-PURGE-DT NOT = LOW-VALUES AND SPACES
005223*        MOVE SPACES                 TO DC-OPTION-CODE
005224*        MOVE AT-DMD-LETTER-PURGE-DT TO DC-BIN-DATE-1
005225*        PERFORM 8500-DATE-CONVERSION
005226*        MOVE DC-GREG-DATE-1-EDIT    TO DMDPURDI.
005227*
005228*    IF AT-DMD-LETTER-RELOAD-DT NOT = LOW-VALUES AND SPACES
005229*        MOVE SPACES                  TO DC-OPTION-CODE
005230*        MOVE AT-DMD-LETTER-RELOAD-DT TO DC-BIN-DATE-1
005231*        PERFORM 8500-DATE-CONVERSION
005232*        MOVE DC-GREG-DATE-1-EDIT     TO DMDRELDI.
005233** DMD CODE END *******************
005234
005235 4400-CONTINUE.
005236     IF AT-LETTER-PURGED-DT NOT = LOW-VALUES AND SPACES
005237         MOVE ' '                    TO  DC-OPTION-CODE
005238         MOVE AT-LETTER-PURGED-DT    TO  DC-BIN-DATE-1
005239         PERFORM 8500-DATE-CONVERSION
005240         MOVE DC-GREG-DATE-1-EDIT    TO  DPURGDTI
005241         MOVE AL-SABON               TO  DPURGHDA
005242     ELSE
005243         MOVE AL-SANON               TO  DPURGHDA.
005244
005245     MOVE AT-LETTER-ORIGIN       TO  DWRITENO.
005246     INSPECT DWRITENO CONVERTING '1234' TO 'NYNY'.
005247
005248     MOVE AT-REASON-TEXT         TO  DREASONO.
005249
005250     MOVE -1                     TO  DMAINTL.
005251
005252     MOVE AT-ADDRESSEE-NAME      TO  DMAILTOO.
005253
005254     IF AT-ADDRESEE-TYPE = SPACE
005255         NEXT SENTENCE
005256      ELSE
005257     IF AT-ADDRESEE-TYPE      = '3'   AND
005258        AT-ADDRESS-REC-SEQ-NO = ZERO  AND
005259        CL-SYSTEM-IDENTIFIER  = 'CR'
005260         MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY
005261         MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD
005262         MOVE PI-CARRIER         TO  WS-AM-CARRIER
005263         MOVE PI-GROUPING        TO  WS-AM-GROUPING
005264         MOVE PI-STATE           TO  WS-AM-STATE
005265         MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT
005266         MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT
005267         PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT
005268      ELSE
005269     IF AT-ADDRESEE-TYPE      = '3'   AND
005270        AT-ADDRESS-REC-SEQ-NO = ZERO  AND
005271        CL-SYSTEM-IDENTIFIER  = 'CV'
005272         MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY
005273         MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD
005274         MOVE PI-CARRIER         TO  WS-PD-CARRIER
005275         MOVE PI-GROUPING        TO  WS-PD-GROUPING
005276         MOVE PI-STATE           TO  WS-PD-STATE
005277         MOVE PI-PRODUCER        TO  WS-PD-PRODUCER
005278         MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT
005279         PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT
005280      ELSE
005281     IF AT-ADDRESS-REC-SEQ-NO NOT = ZERO
005282         MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
005283         MOVE AT-ADDRESS-REC-SEQ-NO    TO  WS-ATK-SEQUENCE-NO
005284         
      * EXEC CICS READ
005285*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
005286*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
005287*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
005288*        END-EXEC
      *    MOVE '&"S        E          (   #00013615' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133363135' TO DFHEIV0
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
005289         IF AT-MAIL-TO-NAME NOT = DMAILTOO
005290             MOVE ER-0384           TO  EMI-ERROR
005291             PERFORM 9900-ERROR-FORMAT
005292         ELSE
005293             MOVE AT-MAIL-TO-NAME        TO  DMAILTOO
005294             MOVE AT-ADDRESS-LINE-1      TO  DADDR1O
005295             MOVE AT-ADDRESS-LINE-2      TO  DADDR2O
005296*            MOVE AT-CITY-STATE          TO  DCITYSTO
005297             STRING AT-CITY ' ' AT-STATE
005298                DELIMITED BY '  ' INTO DCITYSTO
005299             END-STRING
005300             MOVE AT-PHONE-NO            TO  DPHONEO
005301             INSPECT DPHONEI CONVERTING SPACES TO '-'
005302             IF AT-CANADIAN-POST-CODE
005303                 MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
005304                 MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
005305                 MOVE SPACES             TO WS-DASH-CAN
005306                                            WS-CAN-FILLER
005307                 MOVE WS-CANADIAN-POSTAL-CODES
005308                                         TO DZIPO
005309             ELSE
005310                 MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
005311                 IF AT-ZIP-PLUS4 = SPACES OR ZEROS
005312                     MOVE SPACES         TO WS-ZIP-PLUS4
005313                                            WS-DASH
005314                     MOVE WS-ZIP         TO DZIPO
005315                 ELSE
005316                     MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
005317                     MOVE '-'            TO WS-DASH
005318                     MOVE WS-ZIP         TO DZIPO.
005319
005320     GO TO 4100-READNEXT.
005321
005322 4410-ADDRESS-NOT-FOUND.
005323     MOVE ER-0388                TO  EMI-ERROR.
005324     PERFORM 9900-ERROR-FORMAT.
005325
005326     GO TO 4100-READNEXT.
005327
005328     EJECT
005329 4420-FIND-ACCOUNT-MASTER.
005330     MOVE ZERO                   TO  WS-NOT-FOUND.
005331
005332     
      * EXEC CICS HANDLE CONDITION
005333*        NOTFND (4440-NOT-FOUND)
005334*    END-EXEC.
      *    MOVE '"$I                   ! ) #00013663' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303133363633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005335
005336     
      * EXEC CICS STARTBR
005337*        DATASET   (WS-ACCOUNT-MASTER-DSID)
005338*        RIDFLD    (WS-ACCOUNT-MASTER-KEY)
005339*        GENERIC   EQUAL
005340*        KEYLENGTH (13)
005341*    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00013667' TO DFHEIV0
           MOVE X'262C2020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303133363637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005342
005343     MOVE +1                     TO  WS-READNEXT-SW.
005344
005345 4425-READNEXT.
005346     
      * EXEC CICS READNEXT
005347*        DATASET   (WS-ACCOUNT-MASTER-DSID)
005348*        RIDFLD    (WS-ACCOUNT-MASTER-KEY)
005349*        SET       (ADDRESS OF ACCOUNT-MASTER)
005350*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013677' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133363737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005351
005352     IF PI-COMPANY-CD = WS-AM-COMPANY-CD AND
005353        PI-CARRIER    = WS-AM-CARRIER    AND
005354        PI-GROUPING   = WS-AM-GROUPING   AND
005355        PI-STATE      = WS-AM-STATE      AND
005356        PI-ACCOUNT    = WS-AM-ACCOUNT
005357         NEXT SENTENCE
005358       ELSE
005359         GO TO 4440-NOT-FOUND.
005360
005361     IF (PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT
005362       AND PI-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT)
005363         IF AT-TRAILER-TYPE = '4'
005364             MOVE AM-NAME        TO  DMAILTOO
005365             MOVE AM-PERSON      TO  DADDR1O
005366             MOVE AM-ADDRS       TO  DADDR2O
005367*            MOVE AM-CITY        TO  DCITYSTO
005368             STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
005369                DELIMITED BY '  ' INTO DCITYSTO
005370             END-STRING
005371             MOVE AM-TEL-NO      TO WS-WORK-PHONE
005372             INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
005373             MOVE WS-NUMERIC-PHONE TO DPHONEO
005374             INSPECT DPHONEI CONVERTING SPACES TO '-'
005375             IF  AM-CANADIAN-POST-CODE
005376                 MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
005377                 MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
005378                 MOVE SPACES             TO WS-DASH-CAN
005379                                            WS-CAN-FILLER
005380                 MOVE WS-CANADIAN-POSTAL-CODES
005381                                         TO DZIPO
005382                 GO TO 4445-END-OF-BROWSE
005383             ELSE
005384                 MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
005385                 IF AM-ZIP-PLUS4 = SPACES OR ZEROS
005386                     MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
005387                     MOVE WS-ZIP         TO DZIPO
005388                     GO TO 4445-END-OF-BROWSE
005389                 ELSE
005390                     MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
005391                     MOVE '-'            TO WS-DASH
005392                     MOVE WS-ZIP         TO DZIPO
005393                     GO TO 4445-END-OF-BROWSE
005394         ELSE
005395             MOVE AM-NAME        TO  JMAILTOO
005396             MOVE AM-PERSON      TO  JADDR1O
005397             MOVE AM-ADDRS       TO  JADDR2O
005398*            MOVE AM-CITY        TO  JCITYSTO
005399             STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
005400                DELIMITED BY '  ' INTO JCITYSTO
005401             END-STRING
005402             MOVE AM-TEL-NO      TO WS-WORK-PHONE
005403             INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
005404             MOVE WS-NUMERIC-PHONE TO JPHONEO
005405             INSPECT JPHONEI CONVERTING SPACES TO '-'
005406             IF  AM-CANADIAN-POST-CODE
005407                 MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
005408                 MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
005409                 MOVE SPACES             TO WS-DASH-CAN
005410                                            WS-CAN-FILLER
005411                 MOVE WS-CANADIAN-POSTAL-CODES
005412                                         TO JZIPO
005413                 GO TO 4445-END-OF-BROWSE
005414             ELSE
005415                 MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
005416                 IF AM-ZIP-PLUS4 = SPACES OR ZEROS
005417                     MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
005418                     MOVE WS-ZIP         TO JZIPO
005419                     GO TO 4445-END-OF-BROWSE
005420                 ELSE
005421                     MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
005422                     MOVE '-'            TO WS-DASH
005423                     MOVE WS-ZIP         TO JZIPO
005424                     GO TO 4445-END-OF-BROWSE.
005425
005426     GO TO 4425-READNEXT.
005427
005428 4440-NOT-FOUND.
005429     MOVE ER-0198                TO  EMI-ERROR.
005430     PERFORM 9900-ERROR-FORMAT.
005431
005432 4445-END-OF-BROWSE.
005433     IF WS-READNEXT-SW = +1
005434         
      * EXEC CICS ENDBR
005435*            DATASET (WS-ACCOUNT-MASTER-DSID)
005436*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013765' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133373635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005437
005438 4449-EXIT.
005439     EXIT.
005440
005441     EJECT
005442 4450-FIND-PRODUCER-MASTER.
005443     MOVE ZERO                   TO  WS-NOT-FOUND.
005444
005445     
      * EXEC CICS HANDLE CONDITION
005446*        NOTFND (4490-NOT-FOUND)
005447*    END-EXEC.
      *    MOVE '"$I                   ! * #00013776' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303133373736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005448
005449     
      * EXEC CICS STARTBR
005450*        DATASET   (WS-PRODUCER-MASTER-DSID)
005451*        RIDFLD    (WS-PRODUCER-MASTER-KEY)
005452*        GENERIC   EQUAL
005453*        KEYLENGTH (13)
005454*    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00013780' TO DFHEIV0
           MOVE X'262C2020204B472020202045' &
                X'202020202020202020202620' &
                X'2020233030303133373830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005455
005456     MOVE +1                     TO  WS-READNEXT-SW.
005457
005458 4455-READNEXT.
005459     
      * EXEC CICS READNEXT
005460*        DATASET   (WS-PRODUCER-MASTER-DSID)
005461*        RIDFLD    (WS-PRODUCER-MASTER-KEY)
005462*        SET       (ADDRESS OF PRODUCER-MASTER)
005463*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013790' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133373930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005464
005465     IF PI-COMPANY-CD = WS-PD-COMPANY-CD AND
005466        PI-CARRIER    = WS-PD-CARRIER    AND
005467        PI-GROUPING   = WS-PD-GROUPING   AND
005468        PI-STATE      = WS-PD-STATE      AND
005469        PI-PRODUCER   = WS-PD-PRODUCER
005470         NEXT SENTENCE
005471       ELSE
005472         GO TO 4490-NOT-FOUND.
005473
005474     IF (PI-CERT-EFF-DT NOT LESS THAN PD-EFFECT-DT
005475       AND PI-CERT-EFF-DT LESS THAN PD-EXPIRE-DATE)
005476         IF AT-TRAILER-TYPE = '4'
005477             MOVE PD-NAME        TO  DMAILTOO
005478             MOVE PD-PERSON      TO  DADDR1O
005479             MOVE PD-ADDRS       TO  DADDR2O
005480             MOVE PD-CITY        TO  DCITYSTO
005481             MOVE PD-TEL-NO      TO WS-WORK-PHONE
005482             INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
005483             MOVE WS-NUMERIC-PHONE TO DPHONEO
005484             INSPECT DPHONEI CONVERTING SPACES TO '-'
005485             MOVE PD-ZIP-PRIME       TO WS-ZIP-CODE
005486             IF PD-ZIP-PLUS4 = SPACES OR ZEROS
005487                 MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
005488                 MOVE WS-ZIP         TO DZIPO
005489                 GO TO 4495-END-OF-BROWSE
005490             ELSE
005491                 MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4
005492                 MOVE '-'            TO WS-DASH
005493                 MOVE WS-ZIP         TO DZIPO
005494                 GO TO 4495-END-OF-BROWSE
005495         ELSE
005496             MOVE PD-NAME        TO  JMAILTOO
005497             MOVE PD-PERSON      TO  JADDR1O
005498             MOVE PD-ADDRS       TO  JADDR2O
005499             MOVE PD-CITY        TO  JCITYSTO
005500             MOVE PD-TEL-NO      TO WS-WORK-PHONE
005501             INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
005502             MOVE WS-NUMERIC-PHONE TO JPHONEO
005503             INSPECT JPHONEI CONVERTING SPACES TO '-'
005504             IF PD-ZIP-PLUS4 = SPACES OR ZEROS
005505                 MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
005506                 MOVE WS-ZIP         TO JZIPO
005507                 GO TO 4495-END-OF-BROWSE
005508             ELSE
005509                 MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4
005510                 MOVE '-'            TO WS-DASH
005511                 MOVE WS-ZIP         TO JZIPO
005512                 GO TO 4495-END-OF-BROWSE.
005513
005514     GO TO 4455-READNEXT.
005515
005516 4490-NOT-FOUND.
005517     MOVE ER-9616                TO  EMI-ERROR.
005518     PERFORM 9900-ERROR-FORMAT.
005519
005520 4495-END-OF-BROWSE.
005521     IF WS-READNEXT-SW = +1
005522         
      * EXEC CICS ENDBR
005523*            DATASET (WS-PRODUCER-MASTER-DSID)
005524*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013853' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133383533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005525
005526 4499-EXIT.
005527     EXIT.
005528
005529     EJECT
005530 4500-GENERAL-INFO-TRAILER.
005531     IF AT-TRAILER-TYPE NOT = '6'
005532         GO TO 4600-AUTO-PROMPT-TRAILER.
005533
005534     MOVE EL142E                 TO  PI-MAP-NAME.
005535
005536     MOVE AT-TRAILER-TYPE        TO  ETLRTYPO.
005537     MOVE AT-SEQUENCE-NO         TO  ESEQO.
005538
005539     MOVE SPACES                 TO  DC-OPTION-CODE.
005540     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
005541     PERFORM 8500-DATE-CONVERSION.
005542     MOVE DC-GREG-DATE-1-EDIT    TO  ERECDTEO.
005543
005544     MOVE AT-RECORDED-BY         TO  EBYO.
005545
005546     MOVE AT-GEN-INFO-LAST-MAINT-DT    TO  DC-BIN-DATE-1
005547                                           PI-SAVE-LAST-MAINT-DT.
005548     MOVE ' '                          TO  DC-OPTION-CODE.
005549     PERFORM 8500-DATE-CONVERSION.
005550     IF NO-CONVERSION-ERROR
005551         MOVE DC-GREG-DATE-1-EDIT      TO  EMANTONO
005552     ELSE
005553         MOVE SPACES                   TO  EMANTONO.
005554
005555     MOVE AT-GEN-INFO-LAST-UPDATED-BY  TO  EMANTBYO
005556                                           PI-SAVE-LAST-UPD-BY.
005557
005558     MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
005559     MOVE TIME-OUT               TO  EMANTATO.
005560
005561     MOVE AT-INFO-TRAILER-TYPE   TO  ETYPENO.
005562
005563     IF AT-INFO-TRAILER-TYPE = 'C'
005564         MOVE AT-CALL-TYPE       TO  ECALLO
005565       ELSE
005566         MOVE SPACES             TO  ECALLO
005567                                     ECALLTO.
005568
005569     MOVE AT-INFO-LINE-1         TO  ELINE1O.
005570     MOVE AT-INFO-LINE-2         TO  ELINE2O.
005571
005572     MOVE -1                     TO  EMAINTL.
005573
005574     GO TO 4100-READNEXT.
005575
005576     EJECT
005577 4600-AUTO-PROMPT-TRAILER.
005578     IF AT-TRAILER-TYPE NOT = '7'
005579         GO TO 4700-DENIAL-TRAILER.
005580
005581     MOVE EL142F                 TO  PI-MAP-NAME.
005582
005583     MOVE AT-TRAILER-TYPE        TO  FTLRTYPO.
005584     MOVE AT-SEQUENCE-NO         TO  FSEQO.
005585
005586     MOVE SPACES                 TO  DC-OPTION-CODE.
005587     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
005588     PERFORM 8500-DATE-CONVERSION.
005589     MOVE DC-GREG-DATE-1-EDIT    TO  FRECDTEO.
005590
005591     MOVE AT-RECORDED-BY         TO  FBYO.
005592
005593     MOVE AT-PROMPT-LAST-MAINT-DT    TO  DC-BIN-DATE-1.
005594     MOVE ' '                        TO  DC-OPTION-CODE.
005595     PERFORM 8500-DATE-CONVERSION.
005596     IF NO-CONVERSION-ERROR
005597         MOVE DC-GREG-DATE-1-EDIT    TO  FMANTONO
005598     ELSE
005599         MOVE SPACES                 TO  FMANTONO.
005600
005601     MOVE AT-PROMPT-LAST-UPDATED-BY  TO  FMANTBYO.
005602
005603     MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
005604     MOVE TIME-OUT                   TO  FMANTATO.
005605
005606     IF AT-PROMPT-START-DT NOT = LOW-VALUES
005607         MOVE SPACES                 TO  DC-OPTION-CODE
005608         MOVE AT-PROMPT-START-DT     TO  DC-BIN-DATE-1
005609         PERFORM 8500-DATE-CONVERSION
005610         MOVE DC-GREG-DATE-1-EDIT    TO  FSNOTIFI.
005611
005612     IF AT-PROMPT-END-DT NOT = LOW-VALUES
005613         MOVE SPACES                 TO  DC-OPTION-CODE
005614         MOVE AT-PROMPT-END-DT       TO  DC-BIN-DATE-1
005615         PERFORM 8500-DATE-CONVERSION
005616         MOVE DC-GREG-DATE-1-EDIT    TO  FENOTIFI.
005617
005618     MOVE AT-PROMPT-LINE-1       TO  FLINE1O.
005619     MOVE AT-PROMPT-LINE-2       TO  FLINE2O.
005620
005621     MOVE -1                     TO  FMAINTL.
005622
005623     GO TO 4100-READNEXT.
005624
005625     EJECT
005626 4700-DENIAL-TRAILER.
005627     IF AT-TRAILER-TYPE NOT = '8'
005628         GO TO 4800-INCURRED-CHANGE-TRAILER.
005629
005630     MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY
005631
005632     
      * EXEC CICS READ
005633*       DATASET   (WS-CLAIM-MASTER-DSID)
005634*       RIDFLD    (WS-CLAIM-KEY)
005635*       SET       (ADDRESS OF CLAIM-MASTER)
005636*       resp      (ws-response)
005637*    END-EXEC
      *    MOVE '&"S        E          (  N#00013963' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303133393633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005638
005639     if resp-normal
005640        move cl-incurred-dt      to pi-incurred-dt
005641     else
005642        move low-values          to pi-incurred-dt
005643     end-if
005644
005645     MOVE EL142G                 TO  PI-MAP-NAME.
005646
005647     MOVE AT-TRAILER-TYPE        TO  GTLRTYPO.
005648     MOVE AT-SEQUENCE-NO         TO  GSEQO.
005649
005650     MOVE SPACES                 TO  DC-OPTION-CODE.
005651     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1
005652                                     pi-den-recorded-dt
005653     PERFORM 8500-DATE-CONVERSION.
005654     MOVE DC-GREG-DATE-1-EDIT    TO  GRECDTEO.
005655
005656     MOVE AT-RECORDED-BY         TO  GBYO.
005657
005658     MOVE AT-DENIAL-LAST-MAINT-DT    TO  DC-BIN-DATE-1.
005659     MOVE ' '                        TO  DC-OPTION-CODE.
005660     PERFORM 8500-DATE-CONVERSION.
005661     IF NO-CONVERSION-ERROR
005662         MOVE DC-GREG-DATE-1-EDIT    TO  GMANTONO
005663     ELSE
005664         MOVE SPACES                 TO  GMANTONO.
005665
005666     MOVE AT-DENIAL-LAST-UPDATED-BY  TO  GMANTBYO.
005667
005668     MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
005669     MOVE TIME-OUT               TO  GMANTATO.
005670
005671     MOVE AT-DENIAL-INFO-1       TO  GLINE1O.
005672     MOVE AT-DENIAL-INFO-2       TO  GLINE2O.
005673
005674     IF AT-RETRACTION-DT NOT = LOW-VALUES
005675         MOVE SPACES             TO  DC-OPTION-CODE
005676         MOVE AT-RETRACTION-DT   TO  DC-BIN-DATE-1
005677         PERFORM 8500-DATE-CONVERSION
005678         MOVE DC-GREG-DATE-1-EDIT TO  GRECONSI.
005679
005680     IF AT-DENIAL-PROOF-DT NOT = LOW-VALUES
005681         MOVE SPACES             TO DC-OPTION-CODE
005682         MOVE AT-DENIAL-PROOF-DT TO DC-BIN-DATE-1
005683         PERFORM 8500-DATE-CONVERSION
005684         MOVE DC-GREG-DATE-1-EDIT TO GPRFDTI.
005685
005686     MOVE AT-DENIAL-REASON-CODE  TO  GRSNCDI
005687                                     PI-DENIAL-REASON-CODE
005688
005689     MOVE -1                     TO  BMAINTL.
005690
005691     GO TO 4100-READNEXT.
005692
005693     EJECT
005694 4800-INCURRED-CHANGE-TRAILER.
005695     IF AT-TRAILER-TYPE NOT = '9'
005696         GO TO 4900-FORMS-TRAILER.
005697
005698     MOVE EL142I                 TO  PI-MAP-NAME.
005699
005700     MOVE AT-TRAILER-TYPE        TO  ITLRTYPO.
005701     MOVE AT-SEQUENCE-NO         TO  ISEQO.
005702
005703     MOVE SPACES                 TO  DC-OPTION-CODE.
005704     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
005705     PERFORM 8500-DATE-CONVERSION.
005706     MOVE DC-GREG-DATE-1-EDIT    TO  IRECDTEO.
005707
005708     MOVE AT-RECORDED-BY         TO  IBYO.
005709
005710     IF AT-OLD-INCURRED-DT NOT = LOW-VALUES
005711         MOVE SPACES                 TO  DC-OPTION-CODE
005712         MOVE AT-OLD-INCURRED-DT     TO  DC-BIN-DATE-1
005713         PERFORM 8500-DATE-CONVERSION
005714         MOVE DC-GREG-DATE-1-EDIT    TO  IINCDTO.
005715
005716     IF AT-OLD-REPORTED-DT NOT = LOW-VALUES
005717         MOVE SPACES                 TO  DC-OPTION-CODE
005718         MOVE AT-OLD-REPORTED-DT     TO  DC-BIN-DATE-1
005719         PERFORM 8500-DATE-CONVERSION
005720         MOVE DC-GREG-DATE-1-EDIT    TO  IREPDTO.
005721
005722     IF AT-OLD-ESTABLISHED-DT NOT = LOW-VALUES
005723         MOVE SPACES                 TO  DC-OPTION-CODE
005724         MOVE AT-OLD-ESTABLISHED-DT  TO  DC-BIN-DATE-1
005725         PERFORM 8500-DATE-CONVERSION
005726         MOVE DC-GREG-DATE-1-EDIT    TO  IESTDTO.
005727
005728     MOVE AT-TRAILER-CNT-AT-CHG  TO  ITLRCNTO.
005729     MOVE AT-OLD-TOTAL-PAID      TO  ITAPDO.
005730     MOVE AT-OLD-DAYS-PAID       TO  ITDPDO.
005731     MOVE AT-OLD-NO-OF-PMTS      TO  INOPMTO.
005732
005733     IF AT-OLD-PAID-THRU-DT NOT = LOW-VALUES
005734        MOVE SPACES                 TO  DC-OPTION-CODE
005735        MOVE AT-OLD-PAID-THRU-DT    TO  DC-BIN-DATE-1
005736        PERFORM 8500-DATE-CONVERSION
005737        MOVE DC-GREG-DATE-1-EDIT    TO  IPDTHRUO
005738        IF PI-USES-PAID-TO
005739           MOVE '6'                TO  DC-OPTION-CODE
005740           MOVE AT-OLD-PAID-THRU-DT TO  DC-BIN-DATE-1
005741           MOVE +1                 TO  DC-ELAPSED-DAYS
005742           MOVE +0                 TO  DC-ELAPSED-MONTHS
005743           PERFORM 8500-DATE-CONVERSION
005744           MOVE DC-GREG-DATE-1-EDIT TO  IPDTHRUO.
005745
005746     IF AT-LAST-PMT-MADE-DT NOT = LOW-VALUES
005747         MOVE SPACES                 TO  DC-OPTION-CODE
005748         MOVE AT-LAST-PMT-MADE-DT    TO  DC-BIN-DATE-1
005749         PERFORM 8500-DATE-CONVERSION
005750         MOVE DC-GREG-DATE-1-EDIT    TO  ILSTPDTO.
005751
005752     MOVE AT-OLD-INIT-MAN-RESV     TO  IMANRESO.
005753     MOVE AT-OLD-CURRENT-MAN-RESV  TO  ICURRESO.
005754     MOVE AT-OLD-ADDL-MAN-RESV     TO  IADDRESO.
005755
005756     MOVE AT-OLD-ITD-PAID-EXPENSE  TO  ITEXPDO.
005757     MOVE AT-OLD-CHARGABLE-EXPENSE TO  ICHGEXPO.
005758
005759     MOVE AT-OLD-DIAG-CODE         TO  ICAUSCDO.
005760     MOVE AT-OLD-DIAG-DESCRIP      TO  IDIAGO.
005761
005762     MOVE -1                     TO  IPFKL.
005763
005764     GO TO 4100-READNEXT.
005765
005766     EJECT
005767 4900-FORMS-TRAILER.
005768     IF AT-TRAILER-TYPE NOT = 'A'
005769         GO TO 6000-BAD-RECORD-TYPE.
005770
005771     MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.
005772
005773     
      * EXEC CICS READ
005774*        DATASET   (WS-CLAIM-MASTER-DSID)
005775*        RIDFLD    (WS-CLAIM-KEY)
005776*        SET        (ADDRESS OF CLAIM-MASTER)
005777*    END-EXEC.
      *    MOVE '&"S        E          (   #00014104' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134313034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005778
005779     
      * EXEC CICS HANDLE CONDITION
005780*        NOTFND (4910-ADDRESS-NOT-FOUND)
005781*    END-EXEC.
      *    MOVE '"$I                   ! + #00014110' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303134313130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005782
005783     MOVE EL142J                 TO  PI-MAP-NAME.
005784
005785     MOVE +1                     TO  EMI-NUMBER-OF-LINES.
005786     MOVE +2                     TO  EMI-SWITCH2.
005787
005788     MOVE AT-TRAILER-TYPE        TO  JTLRTYPO.
005789     MOVE AT-SEQUENCE-NO         TO  JSEQO.
005790
005791     MOVE SPACES                 TO  DC-OPTION-CODE.
005792     MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
005793     PERFORM 8500-DATE-CONVERSION.
005794     MOVE DC-GREG-DATE-1-EDIT    TO  JRECDTEO.
005795
005796     MOVE AT-RECORDED-BY         TO  JBYO.
005797
005798     MOVE AT-FORM-LAST-MAINT-DT      TO  DC-BIN-DATE-1.
005799     MOVE ' '                        TO  DC-OPTION-CODE.
005800     PERFORM 8500-DATE-CONVERSION.
005801     IF NO-CONVERSION-ERROR
005802         MOVE DC-GREG-DATE-1-EDIT    TO  JMANTONO
005803     ELSE
005804         MOVE SPACES                 TO  JMANTONO.
005805
005806     MOVE AT-FORM-LAST-UPDATED-BY    TO  JMANTBYO.
005807
005808     MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
005809     MOVE TIME-OUT                   TO  JMANTATO.
005810
005811     IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
005812         MOVE SPACES             TO  DC-OPTION-CODE
005813         MOVE AT-FORM-SEND-ON-DT  TO  DC-BIN-DATE-1
005814         PERFORM 8500-DATE-CONVERSION
005815         MOVE DC-GREG-DATE-1-EDIT TO  JDTSENTI.
005816
005817     IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
005818         MOVE SPACES              TO  DC-OPTION-CODE
005819         MOVE AT-FORM-RE-SEND-DT  TO  DC-BIN-DATE-1
005820         PERFORM 8500-DATE-CONVERSION
005821         MOVE DC-GREG-DATE-1-EDIT    TO  JRESENDI
005822         IF AT-FORM-REPRINT-DT NOT = LOW-VALUES
005823             MOVE AL-SANOF       TO  JRESENDA
005824           ELSE
005825             NEXT SENTENCE
005826     ELSE
005827         MOVE AL-SANOF           TO  JRESENDA.
005828
005829     IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
005830         MOVE SPACES             TO  DC-OPTION-CODE
005831         MOVE AT-FORM-FOLLOW-UP-DT TO DC-BIN-DATE-1
005832         PERFORM 8500-DATE-CONVERSION
005833         MOVE DC-GREG-DATE-1-EDIT TO  JREPLYI.
005834
005835     IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
005836         MOVE SPACES             TO  DC-OPTION-CODE
005837         MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
005838         PERFORM 8500-DATE-CONVERSION
005839         MOVE DC-GREG-DATE-1-EDIT TO JRECEVEI.
005840
005841     IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES
005842         MOVE SPACES                  TO  DC-OPTION-CODE
005843         MOVE AT-PHY-FORM-ANSWERED-DT TO  DC-BIN-DATE-1
005844         PERFORM 8500-DATE-CONVERSION
005845         MOVE DC-GREG-DATE-1-EDIT     TO JPHYRECI.
005846
005847     IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES
005848         MOVE SPACES                  TO  DC-OPTION-CODE
005849         MOVE AT-EMP-FORM-ANSWERED-DT TO  DC-BIN-DATE-1
005850         PERFORM 8500-DATE-CONVERSION
005851         MOVE DC-GREG-DATE-1-EDIT     TO JEMPRECI.
005852
005853     IF AT-FORM-REM-PRINT-DT NOT = LOW-VALUES AND SPACES
005854         MOVE SPACES                  TO  DC-OPTION-CODE
005855         MOVE AT-FORM-REM-PRINT-DT    TO  DC-BIN-DATE-1
005856         PERFORM 8500-DATE-CONVERSION
005857         MOVE DC-GREG-DATE-1-EDIT     TO JREMDTI.
005858
005859     IF AT-FORM-TYPE = '1'
005860         MOVE 'INITIAL'          TO  JFORMO
005861     ELSE
005862         IF AT-FORM-TYPE = '2'
005863             MOVE 'PROGRESS'     TO  JFORMO
005864         ELSE
005865             MOVE AT-FORM-TYPE   TO  JFORMO.
005866
005867     MOVE AT-INSTRUCT-LN-1       TO  JSI1O.
005868     MOVE AT-INSTRUCT-LN-2       TO  JSI2O.
005869     MOVE AT-INSTRUCT-LN-3       TO  JSI3O.
005870
005871     MOVE AT-REL-CARR-1          TO  JCARR1O.
005872     MOVE AT-REL-CLAIM-1         TO  JCLAIM1O.
005873     MOVE AT-REL-CERT-1          TO  JCERT1O.
005874
005875     MOVE AT-REL-CARR-2          TO  JCARR2O.
005876     MOVE AT-REL-CLAIM-2         TO  JCLAIM2O.
005877     MOVE AT-REL-CERT-2          TO  JCERT2O.
005878
005879     MOVE -1                     TO  JMAINTL.
005880
005881     IF AT-FORM-ADDRESS = SPACE
005882         NEXT SENTENCE
005883       ELSE
005884     IF AT-FORM-ADDRESS       = '3'    AND
005885        AT-FORM-ADDR-SEQ-NO   = ZERO   AND
005886        CL-SYSTEM-IDENTIFIER  = 'CR'
005887         MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY
005888         MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD
005889         MOVE PI-CARRIER         TO  WS-AM-CARRIER
005890         MOVE PI-GROUPING        TO  WS-AM-GROUPING
005891         MOVE PI-STATE           TO  WS-AM-STATE
005892         MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT
005893         MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT
005894         PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT
005895       ELSE
005896     IF AT-FORM-ADDRESS       = '3'    AND
005897        AT-FORM-ADDR-SEQ-NO   = ZERO   AND
005898        CL-SYSTEM-IDENTIFIER  = 'CV'
005899         MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY
005900         MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD
005901         MOVE PI-CARRIER         TO  WS-PD-CARRIER
005902         MOVE PI-GROUPING        TO  WS-PD-GROUPING
005903         MOVE PI-STATE           TO  WS-PD-STATE
005904         MOVE PI-PRODUCER        TO  WS-PD-PRODUCER
005905         MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT
005906         PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT
005907       ELSE
005908     IF AT-FORM-ADDR-SEQ-NO NOT = ZERO
005909         MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
005910         MOVE AT-FORM-ADDR-SEQ-NO    TO  WS-ATK-SEQUENCE-NO
005911         
      * EXEC CICS READ
005912*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
005913*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
005914*            SET     (ADDRESS OF ACTIVITY-TRAILERS)
005915*        END-EXEC
      *    MOVE '&"S        E          (   #00014242' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134323432' TO DFHEIV0
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
005916         MOVE AT-MAIL-TO-NAME    TO  JMAILTOO
005917         MOVE AT-ADDRESS-LINE-1  TO  JADDR1O
005918         MOVE AT-ADDRESS-LINE-2  TO  JADDR2O
005919         MOVE AT-CITY-STATE      TO  JCITYSTO
005920         STRING AT-CITY ' ' AT-STATE
005921            DELIMITED BY '  ' INTO JCITYSTO
005922         END-STRING
005923         MOVE AT-PHONE-NO        TO  JPHONEO
005924         INSPECT JPHONEI CONVERTING SPACES TO '-'
005925         IF  AT-CANADIAN-POST-CODE
005926             MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
005927             MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
005928             MOVE SPACES             TO WS-DASH-CAN
005929                                        WS-CAN-FILLER
005930             MOVE WS-CANADIAN-POSTAL-CODES
005931                                     TO JZIPO
005932         ELSE
005933             MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
005934             IF  AT-ZIP-PLUS4 = SPACES OR ZEROS
005935                 MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
005936                 MOVE WS-ZIP         TO JZIPO
005937             ELSE
005938                 MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
005939                 MOVE '-'            TO WS-DASH
005940                 MOVE WS-ZIP         TO JZIPO.
005941
005942     GO TO 4100-READNEXT.
005943
005944 4910-ADDRESS-NOT-FOUND.
005945     MOVE ER-0388                TO  EMI-ERROR.
005946     PERFORM 9900-ERROR-FORMAT.
005947
005948     GO TO 4100-READNEXT.
005949
005950     EJECT
005951 6000-BAD-RECORD-TYPE.
005952     MOVE 'BAD TRAILER RECORD TYPE - PROBABLE LOGIC ERROR'
005953                                 TO  LOGOFF-MSG.
005954
005955     PERFORM 8300-SEND-TEXT.
005956
005957 6000-END-OF-FILE.
005958     IF PI-MAP-NAME = EL142A
005959         MOVE ER-0344            TO  EMI-ERROR
005960         MOVE -1                 TO  ARECDTEL
005961         
      * EXEC CICS ENDBR
005962*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
005963*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014292' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134323932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005964         PERFORM 8200-SEND-DATAONLY.
005965
005966     MOVE ER-0303                TO  EMI-ERROR.
005967     MOVE +1                     TO  PI-END-OF-FILE.
005968
005969 6000-ENDBROWSE.
005970     
      * EXEC CICS ENDBR
005971*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
005972*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014301' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134333031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005973
005974     MOVE EIBAID                 TO  PI-PREV-AID.
005975
005976     PERFORM 8100-SEND-INITIAL-MAP.
005977     PERFORM 9100-RETURN-TRAN.
005978
005979 6990-EXIT.
005980     EXIT.
005981
005982     EJECT
005983 5000-DISPLAY-CHECK-QUEUE SECTION.
005984     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
005985     MOVE +2                     TO  EMI-SWITCH2.
005986
005987     MOVE EL142B2                TO  PI-MAP-NAME.
005988
005989     MOVE LOW-VALUES             TO  EL142B2O.
005990
005991     MOVE -1                     TO  KMAINTL.
005992
005993     
      * EXEC CICS READ
005994*        DATASET  (WS-ACTIVITY-TRAILERS-DSID)
005995*        RIDFLD   (PI-SAVE-KEY)
005996*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
005997*    END-EXEC.
      *    MOVE '&"S        E          (   #00014324' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005998
005999     MOVE AT-CLAIM-TYPE          TO  KCTYPEO.
006000
006001     MOVE AT-CARRIER             TO  KCARRO.
006002     MOVE AT-CLAIM-NO            TO  KCLMNOO.
006003     MOVE AT-CERT-NO             TO  KCERTNOO.
006004
006005     MOVE AT-SEQUENCE-NO         TO  KTSEQO.
006006
006007     IF AT-CLAIM-PREM-TYPE = '1'
006008         MOVE 'SP'               TO  KCOVERO
006009       ELSE
006010     IF AT-CLAIM-PREM-TYPE = '2'
006011         MOVE 'OB'               TO  KCOVERO
006012       ELSE
006013     IF AT-CLAIM-PREM-TYPE = '3'
006014         MOVE 'OE'               TO  KCOVERO
006015       ELSE
006016         MOVE AT-CLAIM-PREM-TYPE TO  KCOVERO.
006017
006018     MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
006019     MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER
006020                                     KCONTRLO.
006021     MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER
006022                                     KSEQO.
006023
006024     MOVE AT-CHECK-NO            TO  KCKNOO.
006025     MOVE AT-AMOUNT-PAID         TO  KCKAMTO.
006026
006027     IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
006028         MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1
006029         MOVE SPACES              TO  DC-OPTION-CODE
006030         PERFORM 8500-DATE-CONVERSION
006031         MOVE DC-GREG-DATE-1-EDIT TO  KCKDATEO.
006032
006033     MOVE AT-RECORDED-BY         TO  KBYO.
006034
006035     IF AT-CV-PMT-CODE = ' '
006036         GO TO 5000-DISPLAY-CHECK-DESC.
006037
006038     IF AT-CV-PMT-CODE = '1'
006039         MOVE 'FULL DEATH     '         TO  KPAYTYPO.
006040
006041     IF AT-CV-PMT-CODE = '2'
006042         MOVE 'HALF DEATH     '         TO  KPAYTYPO.
006043
006044     IF AT-CV-PMT-CODE = '3'
006045         MOVE 'FULL AD&D      '         TO  KPAYTYPO.
006046
006047     IF AT-CV-PMT-CODE = '4'
006048         MOVE 'HALF AD&D      '         TO  KPAYTYPO.
006049
006050     IF AT-CV-PMT-CODE = '5'
006051         MOVE 'FULL RIDER     '         TO  KPAYTYPO.
006052
006053     IF AT-CV-PMT-CODE = '6'
006054         MOVE 'HALF RIDER     '         TO  KPAYTYPO.
006055
006056     IF AT-CV-PMT-CODE = '7'
006057         MOVE 'NON-CHG EXP    '         TO  KPAYTYPO.
006058
006059     IF AT-CV-PMT-CODE = '8'
006060         MOVE 'ADDITIONAL     '         TO  KPAYTYPO.
006061
006062     GO TO 5000-DISPLAY-CHECK-QUE-CONT.
006063
006064 5000-DISPLAY-CHECK-DESC.
006065
006066     IF AT-PAYMENT-TYPE = '1'
006067         MOVE 'PARTIAL PAYMENT'         TO  KPAYTYPO
006068       ELSE
006069     IF AT-PAYMENT-TYPE = '2'
006070         MOVE 'FINAL PAYMENT'           TO  KPAYTYPO
006071       ELSE
006072     IF AT-PAYMENT-TYPE = '3'
006073         MOVE 'LUMP SUM PAYMENT'        TO  KPAYTYPO
006074       ELSE
006075     IF AT-PAYMENT-TYPE = '4'
006076         MOVE 'ADDITIONAL PAYMENT'      TO  KPAYTYPO
006077       ELSE
006078     IF AT-PAYMENT-TYPE = '5'
006079         MOVE 'CHARGEABLE PAYMENT'      TO  KPAYTYPO
006080       ELSE
006081     IF AT-PAYMENT-TYPE = '6'
006082         MOVE 'NON-CHARGEABLE PAYMENT'  TO  KPAYTYPO
006083       ELSE
006084     IF AT-PAYMENT-TYPE = '7'
006085         MOVE 'LIFE PREMIUM REFUND'     TO  KPAYTYPO
006086       ELSE
006087     IF AT-PAYMENT-TYPE = '8'
006088         MOVE 'A & H PREMIUM REFUND'    TO  KPAYTYPO
006089       ELSE
006090         MOVE 'ENTRY CORRECTION'        TO  KPAYTYPO.
006091
006092 5000-DISPLAY-CHECK-QUE-CONT.
006093
006094     IF AT-VOID-DT NOT = LOW-VALUES
006095         MOVE 'YES'              TO  KVOIDO
006096       ELSE
006097         MOVE 'NO'               TO  KVOIDO.
006098
006099     
      * EXEC CICS HANDLE CONDITION
006100*        NOTFND (5020-DISPLAY-CHECK-QUEUE)
006101*    END-EXEC.
      *    MOVE '"$I                   ! , #00014430' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303134343330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006102
006103     
      * EXEC CICS READ
006104*        DATASET (WS-CHECK-QUEUE-DSID)
006105*        RIDFLD  (WS-CHECK-QUEUE-KEY)
006106*        SET     (ADDRESS OF CHECK-QUE)
006107*    END-EXEC.
      *    MOVE '&"S        E          (   #00014434' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134343334' TO DFHEIV0
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
           
006108
006109     MOVE CQ-ENTRY-TYPE          TO  KTYPEO.
006110
006111     IF CQ-CHECK-NUMBER NOT = AT-CHECK-NO
006112         MOVE CQ-CHECK-NUMBER    TO  KCKNO2O
006113         MOVE ER-0574            TO  EMI-ERROR
006114         PERFORM 9900-ERROR-FORMAT.
006115
006116     IF CQ-PMT-TRLR-SEQUENCE NOT = AT-SEQUENCE-NO
006117         MOVE ER-0575            TO  EMI-ERROR
006118         PERFORM 9900-ERROR-FORMAT.
006119
006120     IF CQ-CHECK-AMOUNT NOT = AT-AMOUNT-PAID
006121         MOVE ER-0576            TO  EMI-ERROR
006122         PERFORM 9900-ERROR-FORMAT.
006123
006124     IF CQ-CHECK-WRITTEN-DT NOT = AT-CHECK-WRITTEN-DT
006125         MOVE ER-0577            TO  EMI-ERROR
006126         PERFORM 9900-ERROR-FORMAT.
006127
006128     MOVE CQ-TIMES-PRINTED       TO  KTIMPRTO.
006129
006130     IF CHECKS-WERE-PRE-NUMBERED
006131         MOVE 'YES'              TO  KPRENOO
006132     ELSE
006133         MOVE 'NO '              TO  KPRENOO.
006134
006135     GO TO 5080-DISPLAY-CHECK-QUEUE.
006136
006137 5020-DISPLAY-CHECK-QUEUE.
006138     MOVE 'A'                    TO  KMAINTO.
006139     MOVE AL-SANON               TO  KMAINTA.
006140
006141     MOVE -1                     TO  KCONTRLO.
006142
006143     MOVE ER-0578                TO  EMI-ERROR.
006144     PERFORM 9900-ERROR-FORMAT.
006145
006146 5080-DISPLAY-CHECK-QUEUE.
006147     PERFORM 8100-SEND-INITIAL-MAP.
006148
006149 5090-EXIT.
006150     EXIT.
006151
006152     EJECT
006153
006154 6000-DISPLAY-ELNAPS SECTION.
006155     MOVE +3                     TO  EMI-NUMBER-OF-LINES.
006156     MOVE +2                     TO  EMI-SWITCH2.
006157
006158     MOVE EL142D2                TO  PI-MAP-NAME.
006159
006160     MOVE LOW-VALUES             TO  EL142D2O.
006161
006162     MOVE -1                     TO LPFKL.
006163
006164     
      * EXEC CICS READ
006165*        DATASET  (WS-ACTIVITY-TRAILERS-DSID)
006166*        RIDFLD   (PI-SAVE-KEY)
006167*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
006168*    END-EXEC.
      *    MOVE '&"S        E          (   #00014495' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006169
006170     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.
006171     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
006172     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
006173     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
006174     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.
006175
006176     
      * EXEC CICS HANDLE CONDITION
006177*        ENDFILE (6020-ELNAPS-NOTFND)
006178*        NOTFND  (6020-ELNAPS-NOTFND)
006179*    END-EXEC.
      *    MOVE '"$''I                  ! - #00014507' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303134353037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006180
006181     
      * EXEC CICS READ
006182*         DATASET (WS-NAPERSOFT-DSID)
006183*         RIDFLD  (WS-NAPERSOFT-KEY)
006184*         SET     (ADDRESS OF NAPERSOFT-FILE)
006185*    END-EXEC.
      *    MOVE '&"S        E          (   #00014512' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134353132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006186
006187     MOVE NA-CARRIER             TO LCARRO.
006188     MOVE NA-CLAIM-NO            TO LCLMNOO.
006189     MOVE NA-CERT-NO             TO LCRTNOO.
006190     MOVE NA-ARCHIVE-NO          TO LARCHNOO.
006191     MOVE NA-LETTER-ID           TO LLETRIDO.
006192     MOVE NA-PROCESSOR-ID        TO LPROCO.
006193     MOVE NA-RESEND-LETTER-ID    TO LRSLTIDO.
006194     MOVE NA-NO-OF-COPIES        TO LNOCPYSO.
006195     MOVE NA-ADDRESS-TYPE        TO LADDTYPO.
006196     MOVE NA-CORR-TRLR-SEQ       TO LCORSEQO.
006197     MOVE NA-ENCLOSURE-CD        TO LENCCODO.
006198     MOVE NA-CREATED-IN-NAPERSOFT TO LCREATNO.
006199     MOVE NA-ORIG-ARCHIVE-NO     TO LORIGARO.
006200     MOVE NA-RESEND-PROMPT-IND   TO LPROMPTO.
006201     MOVE NA-ORIG-ENCLOSURE-CD   TO LORIGENO.
006202
006203     IF NA-CREATION-DT NOT = LOW-VALUES
006204         MOVE NA-CREATION-DT     TO  DC-BIN-DATE-1
006205         MOVE SPACES             TO  DC-OPTION-CODE
006206         PERFORM 8500-DATE-CONVERSION
006207         MOVE DC-GREG-DATE-1-EDIT TO LCREDTEO.
006208
006209     IF NA-INITIAL-PRINT-DT NOT = LOW-VALUES
006210         MOVE NA-INITIAL-PRINT-DT TO  DC-BIN-DATE-1
006211         MOVE SPACES             TO  DC-OPTION-CODE
006212         PERFORM 8500-DATE-CONVERSION
006213         MOVE DC-GREG-DATE-1-EDIT TO LINPRNTO.
006214
006215     IF NA-FOLLOW-UP-DT NOT = LOW-VALUES
006216         MOVE NA-FOLLOW-UP-DT    TO  DC-BIN-DATE-1
006217         MOVE SPACES             TO  DC-OPTION-CODE
006218         PERFORM 8500-DATE-CONVERSION
006219         MOVE DC-GREG-DATE-1-EDIT TO LFUPDTEO.
006220
006221     IF NA-RESEND-DT NOT = LOW-VALUES
006222         MOVE NA-RESEND-DT       TO  DC-BIN-DATE-1
006223         MOVE SPACES             TO  DC-OPTION-CODE
006224         PERFORM 8500-DATE-CONVERSION
006225         MOVE DC-GREG-DATE-1-EDIT TO LRESDTEO.
006226
006227     IF NA-RESEND-PRINT-DT NOT = LOW-VALUES
006228         MOVE NA-RESEND-PRINT-DT TO  DC-BIN-DATE-1
006229         MOVE SPACES             TO  DC-OPTION-CODE
006230         PERFORM 8500-DATE-CONVERSION
006231         MOVE DC-GREG-DATE-1-EDIT  TO LREPRNTO.
006232
006233     IF NA-1ST-LTR-PRINT-DT NOT = LOW-VALUES
006234         MOVE NA-1ST-LTR-PRINT-DT TO  DC-BIN-DATE-1
006235         MOVE SPACES             TO  DC-OPTION-CODE
006236         PERFORM 8500-DATE-CONVERSION
006237         MOVE DC-GREG-DATE-1-EDIT TO L1STPRTO.
006238
006239     IF NA-NEXT-DUE-DT NOT = LOW-VALUES
006240         MOVE NA-NEXT-DUE-DT     TO  DC-BIN-DATE-1
006241         MOVE SPACES             TO  DC-OPTION-CODE
006242         PERFORM 8500-DATE-CONVERSION
006243         MOVE DC-GREG-DATE-1-EDIT TO LNXTDUEO.
006244
006245     IF NA-AUTOPYDT NOT = LOW-VALUES
006246         MOVE NA-AUTOPYDT        TO  DC-BIN-DATE-1
006247         MOVE SPACES             TO  DC-OPTION-CODE
006248         PERFORM 8500-DATE-CONVERSION
006249         MOVE DC-GREG-DATE-1-EDIT TO LAUTOPYO.
006250
006251     GO TO 8100-SEND-INITIAL-MAP.
006252
006253 6020-ELNAPS-NOTFND.
006254     MOVE ER-0006                TO  EMI-ERROR.
006255     PERFORM 9900-ERROR-FORMAT.
006256
006257     GO TO 8100-SEND-INITIAL-MAP.
006258     EJECT
006259
006260
006261 7000-CONNECT-TO-DB.
006262
006263****  The below code is for when the db has been
006264****  converted to sql server 2016
006265     move '//sdv-db01.cso.local:1433;'
006266                                 to p-sql-server
006267
006268
006269     CALL 'SQLCONNECT' USING sqlconnect-parms
006270     display ' ret code ' p-connect-return-code
006271     move p-connect-return-code  to sqlcode
006272     move p-sql-return-message   to sqlerrmc
006273
006274
006275
006276
006277     IF SQLCODE NOT = 0
006278        DISPLAY "ERROR: CANNOT CONNECT "
006279        DISPLAY SQLCODE
006280        DISPLAY SQLERRMC
006281        GO TO 7000-EXIT
006282     END-IF
006283
006284     .
006285 7000-EXIT.
006286     EXIT.
006287
006288
006289
006290 7100-GET-CHK-CASHED-DT.
006291
006292     move spaces                 to ws-check-cashed-dt
006293     move zeros                  to sqlcode
006294     move '2'                    to ws-draft-or-check  *>  draft
006295     if at-check-written-dt not = low-values and spaces
006296        evaluate true
006297           when (at-check-written-dt > X'ACFE')    *> 04/30/2015
006298              and (pi-company-id not = 'DCC')
006299*          when at-check-written-dt >= X'AC93'    *> 01/19/2015
006300              move '1'           to ws-draft-or-check  *>  check
006301              move '000'         to ws-check-number (1:3)
006302              move at-check-no   to ws-check-number (4:7)
006303           when (at-check-written-dt > X'B016')    *> 05/22/2017
006304              and (pi-company-id = 'DCC')
006305              move '1'           to ws-draft-or-check  *>  check
006306              move '000'         to ws-check-number (1:3)
006307              move at-check-no   to ws-check-number (4:7)
006308           when at-payment-type = 'I'
006309              move '000'         to ws-check-number (1:3)
006310              move at-check-no   to ws-check-number (4:7)
006311           when other
006312              move '0'           to ws-check-number (1:1)
006313              move at-check-no (1:1)
006314                                 to ws-check-number (2:1)
006315              move '00'          to ws-check-number (3:2)
006316              move at-check-no (2:6)
006317                                 to ws-check-number (5:6)
006318        end-evaluate
006319     else   *>  no check written date yet
006320        go to 7100-exit
006321     end-if
006322
006323     evaluate true
006324        when pi-company-id = 'AHL'
006325           move '%AHL%'          to ws-check-company
006326        when pi-company-id = 'DCC'
006327           move '%DCC%'          to ws-check-company
006328        when pi-company-id = 'VPP'
006329           move '%VPP%'          to ws-check-company
006330        when pi-company-id = 'FNL'
006331           move '%FNL%'          to ws-check-company
006332        when other   *>   CID s/b   CSO
006333           move '%CSO%'          to ws-check-company
006334     end-evaluate
006335
006336     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
006337     move at-claim-no            to ws-claim-number
006338     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
006339     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
006340     MOVE SPACES                 TO WS-CHECK-CASHED-DT
006341
006343     EXEC SQL
              CALL LogicPaidBankChkCashedDt
006344         (
006345           @draftorcheck    = :WS-DRAFT-OR-CHECK,
006346           @checktype       = :WS-CHECK-TYPE,
006347           @claimnumber     = :WS-CLAIM-NUMBER,
006348           @checknumber     = :WS-CHECK-NUMBER,
006349           @checkamount     = :WS-CHECK-AMOUNT,
006350           @checkcompany    = :WS-CHECK-COMPANY,
006351           @checkcasheddate = :WS-CHECK-CASHED-DT
006352         )
006353     END-EXEC
006354
006355     IF SQLCODE NOT = 0
006356        MOVE SPACES TO WS-CHECK-CASHED-DT
006357*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
006358*        DISPLAY ' SQL RETURN CODE ' SQLCODE
006359*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
006360        GO TO 7100-EXIT
006361     END-IF
006362
006363     .
006364 7100-EXIT.
006365     EXIT.
006366
006367 7110-CHECK-MANUAL.
006368
006369     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
006370     MOVE AT-CHECK-NO            TO WS-CHECK-NUMBER-man
006371     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
006372     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
006373     MOVE PI-COMPANY-ID          TO WS-CHECK-COMPANY-man
006374     MOVE SPACES                 TO WS-CHECK-CASHED-DT
006375
006377     EXEC SQL
             CALL LogicPaidBankChkCashedDtManual
006378               (
006379                 @checktype = :WS-CHECK-TYPE,
006380                 @checknumber = :WS-CHECK-NUMBER-man,
006381                 @checkamount = :WS-CHECK-AMOUNT,
006382                 @checkcompany = :WS-CHECK-COMPANY-man,
006383                 @checkcasheddate = :WS-CHECK-CASHED-DT
006384               )
006385     END-EXEC
006386
006387     IF SQLCODE NOT = 0
006388        MOVE SPACES TO WS-CHECK-CASHED-DT
006389*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
006390*        DISPLAY ' SQL RETURN CODE ' SQLCODE
006391*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
006392*       GO TO 7100-EXIT
006393     END-IF
006394
006395     .
006396 7110-EXIT.
006397     EXIT.
006398
006399 7200-DISCONNECT.
006400
006402     EXEC SQL
              DISCONNECT
006403     END-EXEC
006404     .
006405 7200-EXIT.
006406     EXIT.
006407
006408
006409 8000-CREATE-DMO-REC.
006410     MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.
006411     MOVE WS-CL-CERT-KEY-DATA    TO NOTE-CERT-KEY.
006412     MOVE WS-CL-CERT-NO          TO NOTE-CERT-NO.
006413
006414     
      * EXEC CICS HANDLE CONDITION
006415*         NOTFND   (8000-NOTE-NOT-FOUND)
006416*    END-EXEC.
      *    MOVE '"$I                   ! . #00014745' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303134373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006417
006418     
      * EXEC CICS READ
006419*         DATASET(WS-NOTE-FILE-DSID)
006420*         SET    (ADDRESS OF CERTIFICATE-NOTE)
006421*         RIDFLD (NOTE-KEY)
006422*    END-EXEC.
      *    MOVE '&"S        E          (   #00014749' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134373439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006423
006424     MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
006425     MOVE WS-CL-BENEFICIARY      TO DCT-LOGIC-BENEFICIARY-ID.
006426     MOVE WS-CL-CCN              TO DCT-CREDIT-CARD-NUMBER.
006427
006428     IF PI-GROUPING (5:2) = ZEROS OR SPACES
006429         MOVE 'CC'               TO DCT-PRODUCT-CODE
006430     ELSE
006431         MOVE PI-GROUPING (5:2)  TO DCT-PRODUCT-CODE.
006432
006433     MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
006434     MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
006435
006436     
      * EXEC CICS LINK
006437*        PROGRAM    ('DLO006')
006438*        COMMAREA   (DCT-COMMUNICATION-AREA)
006439*        LENGTH     (WS-DCT-LENGTH)
006440*    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00014767' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303134373637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006441
006442     IF DCT-RETURN-CODE = 'OK'
006443         GO TO 8000-CONT.
006444
006445     IF DCT-RETURN-CODE = '01' OR '02'
006446         GO TO 8000-EXIT.
006447
006448     IF DCT-RETURN-CODE = '03'
006449         MOVE ER-0951            TO EMI-ERROR
006450         MOVE -1                 TO GMAINTL
006451         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006452         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006453         GO TO 8200-SEND-DATAONLY.
006454
006455     IF DCT-RETURN-CODE = '06'
006456         MOVE ER-0921            TO EMI-ERROR
006457         MOVE -1                 TO GMAINTL
006458         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006459         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006460         GO TO 8200-SEND-DATAONLY.
006461
006462     IF DCT-RETURN-CODE = '07'
006463         MOVE ER-0919            TO EMI-ERROR
006464         MOVE -1                 TO GMAINTL
006465         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006466         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006467         GO TO 8200-SEND-DATAONLY.
006468
006469     IF DCT-RETURN-CODE = '04'
006470         MOVE ER-0946            TO EMI-ERROR
006471         MOVE -1                 TO GMAINTL
006472         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006473         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006474         GO TO 8200-SEND-DATAONLY.
006475
006476     IF DCT-RETURN-CODE = '05'
006477         MOVE ER-0947            TO EMI-ERROR
006478         MOVE -1                 TO GMAINTL
006479         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006480         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006481         GO TO 8200-SEND-DATAONLY.
006482
006483     IF DCT-RETURN-CODE = '08'
006484         MOVE ER-0948            TO EMI-ERROR
006485         MOVE -1                 TO GMAINTL
006486         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006487         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006488         GO TO 8200-SEND-DATAONLY.
006489
006490     IF DCT-RETURN-CODE = 'N1'
006491         MOVE ER-0950            TO EMI-ERROR
006492         MOVE -1                 TO GMAINTL
006493         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006494         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006495         GO TO 8200-SEND-DATAONLY.
006496
006497     IF DCT-RETURN-CODE = 'E1'
006498         MOVE ER-0974            TO EMI-ERROR
006499         MOVE -1                 TO GMAINTL
006500         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006501         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006502         GO TO 8200-SEND-DATAONLY.
006503
006504     IF DCT-RETURN-CODE = 'E2'
006505         MOVE ER-0975            TO EMI-ERROR
006506         MOVE -1                 TO GMAINTL
006507         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006508         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006509         GO TO 8200-SEND-DATAONLY.
006510
006511     IF DCT-RETURN-CODE NOT = 'OK'
006512          MOVE ER-0949            TO EMI-ERROR
006513          MOVE -1                 TO GMAINTL
006514          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006515          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006516          GO TO 8200-SEND-DATAONLY.
006517
006518 8000-CONT.
006519
006520     MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
006521     MOVE 'CS'                   TO DM-RECORD-TYPE.
006522     MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
006523     MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
006524     MOVE WS-CL-CLAIM-NO         TO DM-CLAIM-NO.
006525     MOVE WS-CL-CERT-NO (4:1)    TO DM-CLAIM-TYPE.
006526     MOVE WS-CL-CCN              TO DM-CREDIT-CARD-NUMBER.
006527     MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.
006528
006529     MOVE WS-CL-INSURED-LAST-NAME TO W-NAME-LAST.
006530     MOVE WS-CL-INSURED-1ST-NAME  TO W-NAME-FIRST.
006531     MOVE WS-CL-INSURED-MID-INIT  TO W-NAME-MIDDLE.
006532     PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.
006533     MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
006534
006535     MOVE 'R'                    TO DM-STAT-CHANGE-TYPE.
006536
006537     IF WS-CL-NO-OF-PMTS-MADE = 0
006538         MOVE '1'                TO DM-CLAIM-STATUS
006539      ELSE
006540         MOVE '2'                TO DM-CLAIM-STATUS.
006541
006542     MOVE WS-CL-CERT-CARRIER     TO DM-STAT-CARRIER.
006543
006544     
      * EXEC CICS LINK
006545*        PROGRAM    ('DLO025')
006546*        COMMAREA   (DMO-COMMUNICATION-AREA)
006547*        LENGTH     (WS-DMO-LENGTH)
006548*    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00014875' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303134383735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006549
006550     IF DM-RETURN-CODE = 'OK'
006551         GO TO 8000-EXIT.
006552
006553     IF DM-RETURN-CODE = '01'
006554         MOVE ER-8051            TO EMI-ERROR
006555         MOVE -1                 TO GMAINTL
006556         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006557         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006558         GO TO 8200-SEND-DATAONLY.
006559
006560     IF DM-RETURN-CODE = '02'
006561         MOVE ER-8052            TO EMI-ERROR
006562         MOVE -1                 TO GMAINTL
006563         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006564         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006565         GO TO 8200-SEND-DATAONLY.
006566
006567     IF DM-RETURN-CODE = '03'
006568         MOVE ER-8053            TO EMI-ERROR
006569         MOVE -1                 TO GMAINTL
006570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006571         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006572         GO TO 8200-SEND-DATAONLY.
006573
006574     IF DM-RETURN-CODE = '04'
006575         MOVE ER-8054            TO EMI-ERROR
006576         MOVE -1                 TO GMAINTL
006577         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006578         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006579         GO TO 8200-SEND-DATAONLY.
006580
006581     IF DM-RETURN-CODE = '05'
006582         MOVE ER-8055            TO EMI-ERROR
006583         MOVE -1                 TO GMAINTL
006584         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006585         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006586         GO TO 8200-SEND-DATAONLY.
006587
006588     IF DM-RETURN-CODE = '06'
006589         MOVE ER-8056            TO EMI-ERROR
006590         MOVE -1                 TO GMAINTL
006591         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006592         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006593         GO TO 8200-SEND-DATAONLY.
006594
006595     IF DM-RETURN-CODE = '07'
006596         MOVE ER-8057            TO EMI-ERROR
006597         MOVE -1                 TO GMAINTL
006598         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006599         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006600         GO TO 8200-SEND-DATAONLY.
006601
006602     IF DM-RETURN-CODE = '08'
006603         MOVE ER-8058            TO EMI-ERROR
006604         MOVE -1                 TO GMAINTL
006605         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006606         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006607         GO TO 8200-SEND-DATAONLY.
006608
006609     IF DM-RETURN-CODE = '09'
006610         MOVE ER-8059            TO EMI-ERROR
006611         MOVE -1                 TO GMAINTL
006612         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006613         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006614         GO TO 8200-SEND-DATAONLY.
006615
006616     IF DM-RETURN-CODE = '10'
006617         MOVE ER-8060            TO EMI-ERROR
006618         MOVE -1                 TO GMAINTL
006619         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006620         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006621         GO TO 8200-SEND-DATAONLY.
006622
006623     IF DM-RETURN-CODE = '11'
006624         MOVE ER-8061            TO EMI-ERROR
006625         MOVE -1                 TO GMAINTL
006626         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006627         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006628         GO TO 8200-SEND-DATAONLY.
006629
006630     IF DM-RETURN-CODE = '12'
006631         MOVE ER-8062            TO EMI-ERROR
006632         MOVE -1                 TO GMAINTL
006633         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006634         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006635         GO TO 8200-SEND-DATAONLY.
006636
006637     IF DM-RETURN-CODE = '13'
006638         MOVE ER-8063            TO EMI-ERROR
006639         MOVE -1                 TO GMAINTL
006640         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006641         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006642         GO TO 8200-SEND-DATAONLY.
006643
006644     IF DM-RETURN-CODE = '14'
006645         MOVE ER-8064            TO EMI-ERROR
006646         MOVE -1                 TO GMAINTL
006647         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006648         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006649         GO TO 8200-SEND-DATAONLY.
006650
006651     IF DM-RETURN-CODE = '15'
006652         MOVE ER-8065            TO EMI-ERROR
006653         MOVE -1                 TO GMAINTL
006654         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006655         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006656         GO TO 8200-SEND-DATAONLY.
006657
006658     IF DM-RETURN-CODE = '16'
006659         MOVE ER-8154            TO EMI-ERROR
006660         MOVE -1                 TO GMAINTL
006661         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006662         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006663         GO TO 8200-SEND-DATAONLY.
006664
006665     IF DM-RETURN-CODE = '17'
006666         MOVE ER-8155            TO EMI-ERROR
006667         MOVE -1                 TO GMAINTL
006668         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006669         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006670         GO TO 8200-SEND-DATAONLY.
006671
006672     IF DM-RETURN-CODE = 'N1'
006673         MOVE ER-8152            TO EMI-ERROR
006674         MOVE -1                 TO GMAINTL
006675         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006676         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006677         GO TO 8200-SEND-DATAONLY.
006678
006679     IF DM-RETURN-CODE = 'E1'
006680         MOVE ER-8153            TO EMI-ERROR
006681         MOVE -1                 TO GMAINTL
006682         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006683         PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
006684         GO TO 8200-SEND-DATAONLY.
006685
006686     MOVE ER-8066                TO EMI-ERROR.
006687     MOVE -1                     TO GMAINTL.
006688     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006689     PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.
006690     GO TO 8200-SEND-DATAONLY.
006691
006692 8000-NOTE-NOT-FOUND.
006693     MOVE ER-0954                TO EMI-ERROR.
006694     MOVE -1                     TO GMAINTL.
006695     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006696     PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.
006697     GO TO 8200-SEND-DATAONLY.
006698
006699 8000-EXIT.
006700     EXIT.
006701 EJECT
006702 8050-FORMAT-LAST-NAME-1ST.
006703*****************************************************************
006704*             M O V E   N A M E   R O U T I N E                 *
006705*                                                               *
006706*     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
006707*     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
006708*     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           *
006709*                                                               *
006710*                  FIELD               VALUE                    *
006711*                  -----               -----                    *
006712*           W-NAME-LAST    (CL15)      SMITH                    *
006713*           W-NAME-FIRST   (CL15)      JOHN                     *
006714*           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
006715*                                                               *
006716*     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       *
006717*                SMITH, JOHN ALLEN                              *
006718*                     OR                                        *
006719*                SMITH, JOHN A.                                 *
006720*                                                               *
006721*     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          *
006722*     COPYBOOK, ELCNWA.                                         *
006723*****************************************************************.
006724
006725     MOVE SPACES                 TO WS-NAME-WORK-AREA.
006726     MOVE ZERO                   TO WS-NAME-SW.
006727     SET NWA-INDEX               TO +1.
006728
006729     IF W-NAME-LAST   = SPACES  AND
006730        W-NAME-MIDDLE = SPACES
006731          MOVE +1                TO WS-NAME-SW.
006732
006733     MOVE W-NAME-LAST            TO WS-NAME-WORK2.
006734     PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
006735
006736     MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
006737     PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
006738
006739     SET NWA-INDEX UP BY +1.
006740
006741     IF W-NAME-MIDDLE NOT = SPACES
006742         IF W-NAME-MIDDLE-2 = SPACES
006743             MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
006744             SET NWA-INDEX UP BY +1
006745             MOVE '.'            TO WS-NW (NWA-INDEX)
006746             SET NWA-INDEX UP BY +2
006747         ELSE
006748             MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
006749             PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
006750
006751 8050-EXIT.
006752     EXIT.
006753
006754 EJECT
006755 8060-MOVE-NAME.
006756     IF WS-NAME-SW GREATER THAN +1
006757         GO TO 8060-EXIT.
006758
006759     IF WS-NAME-WORK2 = SPACES
006760         GO TO 8060-EXIT.
006761
006762     SET NWA-INDEX2            TO +1.
006763     SET NWA-INDEX3            TO +2.
006764
006765 8060-MOVE-NAME-CYCLE.
006766     MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
006767
006768     IF NWA-INDEX LESS THAN +30
006769         SET NWA-INDEX UP BY +1
006770     ELSE
006771         ADD +2 TO  WS-NAME-SW
006772         GO TO 8060-EXIT.
006773
006774     IF NWA-INDEX2 LESS THAN +20
006775         SET NWA-INDEX3 UP BY +1
006776         SET NWA-INDEX2 UP BY +1.
006777
006778     IF WS-NW2 (NWA-INDEX2) = SPACES  AND
006779        WS-NW2 (NWA-INDEX3) = SPACES
006780         IF WS-NAME-SW = ZERO
006781             MOVE ','            TO WS-NW (NWA-INDEX)
006782             SET NWA-INDEX UP BY +2
006783             MOVE +1             TO WS-NAME-SW
006784             GO TO 8060-EXIT
006785         ELSE
006786             GO TO 8060-EXIT.
006787
006788     GO TO 8060-MOVE-NAME-CYCLE.
006789
006790 8060-EXIT.
006791     EXIT.
006792
006793     EJECT
006794
006795 8070-UNLOCK-TRLR.
006796     
      * EXEC CICS UNLOCK
006797*        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
006798*    END-EXEC.
      *    MOVE '&*                    #   #00015127' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303135313237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006799
006800 8070-EXIT.
006801      EXIT.
006802
006803     EJECT
006804 8100-SEND-INITIAL-MAP SECTION.
006805     IF EMI-ERROR NOT = ZERO
006806         PERFORM 9900-ERROR-FORMAT
006807       ELSE
006808         IF TRANSACTION-SUCCESSFUL
006809             PERFORM 9900-ERROR-FORMAT.
006810
006811     MOVE EIBTIME                TO  TIME-IN.
006812
006813     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
006814     MOVE '5'                    TO  DC-OPTION-CODE.
006815     PERFORM 8500-DATE-CONVERSION.
006816
006817     IF PI-MAP-NAME = EL142A
006818         MOVE DC-GREG-DATE-1-EDIT  TO ADATEO
006819         MOVE TIME-OUT             TO ATIMEO
006820         MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
006821         MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O
006822         MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O
006823         GO TO 8110-SEND-MAP.
006824
006825     IF PI-MAP-NAME = EL142B
006826         MOVE DC-GREG-DATE-1-EDIT  TO BDATEO
006827         MOVE TIME-OUT             TO BTIMEO
006828         MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
006829         GO TO 8110-SEND-MAP.
006830
006831     IF PI-MAP-NAME = EL142B2
006832         MOVE DC-GREG-DATE-1-EDIT  TO KDATEO
006833         MOVE TIME-OUT             TO KTIMEO
006834         MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O
006835         MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O
006836         MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O
006837         GO TO 8110-SEND-MAP.
006838
006839     IF PI-MAP-NAME = EL142C
006840         MOVE DC-GREG-DATE-1-EDIT  TO CDATEO
006841         MOVE TIME-OUT             TO CTIMEO
006842         MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O
006843         MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O
006844         GO TO 8110-SEND-MAP.
006845
006846     IF PI-MAP-NAME = EL142D
006847         IF PI-PROCESSOR-ID = 'LGXX'
006848             MOVE AL-UNNON       TO  DARCHNOA
006849                                     DRESFRMA
006850                                     DAUTOCLA
006851             MOVE AL-UANON       TO  DDTSENTA
006852                                     DINPRNTA
006853                                     DREPRNTA
006854     ELSE
006855         MOVE AL-SANOF           TO  DARCHNOA
006856                                     DRESFRMA
006857                                     DAUTOCLA
006858                                     DDTSENTA
006859                                     DINPRNTA
006860                                     DREPRNTA.
006861
006862*05834      IF PI-MAP-NAME = EL142D
006863*05835          IF PI-COMPANY-ID = 'DMD'
006864*05836              MOVE AL-SANON       TO  DMDLETPA
006865*05837                                      DMDPURPA
006866*05838                                      DMDRELPA.
006867
006868     IF PI-MAP-NAME = EL142D
006869         MOVE DC-GREG-DATE-1-EDIT  TO DDATEO
006870         MOVE TIME-OUT             TO DTIMEO
006871         MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O
006872         GO TO 8110-SEND-MAP.
006873
006874     IF PI-MAP-NAME = EL142D2
006875         MOVE DC-GREG-DATE-1-EDIT  TO LDATEO
006876         MOVE TIME-OUT             TO LTIMEO
006877         MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O
006878         MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O
006879         MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O
006880         GO TO 8110-SEND-MAP.
006881
006882     IF PI-MAP-NAME = EL142E
006883         MOVE DC-GREG-DATE-1-EDIT  TO EDATEO
006884         MOVE TIME-OUT             TO ETIMEO
006885         MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O
006886         MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O
006887         GO TO 8110-SEND-MAP.
006888
006889     IF PI-MAP-NAME = EL142F
006890         MOVE DC-GREG-DATE-1-EDIT  TO FDATEO
006891         MOVE TIME-OUT             TO FTIMEO
006892         MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O
006893         MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O
006894         GO TO 8110-SEND-MAP.
006895
006896     IF PI-MAP-NAME = EL142G
006897         MOVE DC-GREG-DATE-1-EDIT  TO GDATEO
006898         MOVE TIME-OUT             TO GTIMEO
006899         MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O
006900         MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O
006901         MOVE AL-UANON             TO GRSNCDA
006902         GO TO 8110-SEND-MAP.
006903
006904     IF PI-MAP-NAME = EL142H
006905         MOVE DC-GREG-DATE-1-EDIT  TO HDATEO
006906         MOVE TIME-OUT             TO HTIMEO
006907         MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O
006908         GO TO 8110-SEND-MAP.
006909
006910     IF PI-MAP-NAME = EL142I
006911         MOVE DC-GREG-DATE-1-EDIT  TO IDATEO
006912         MOVE TIME-OUT             TO ITIMEO
006913         MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O
006914         GO TO 8110-SEND-MAP.
006915
006916     IF PI-MAP-NAME = EL142J
006917         MOVE DC-GREG-DATE-1-EDIT  TO JDATEO
006918         MOVE TIME-OUT             TO JTIMEO
006919         MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O
006920         GO TO 8110-SEND-MAP.
006921
006922 8110-SEND-MAP.
006923     IF PI-USES-PAID-TO
006924        IF PI-MAP-NAME = EL142B
006925           MOVE 'PAID  TO     -'    TO BTHRUHDO
006926           ELSE
006927           IF PI-MAP-NAME = EL142I
006928              MOVE 'PAID  TO  DATE' TO ITHRUHDO.
006929
006930     IF PI-COMPANY-ID = 'DMD'
006931         PERFORM  8400-DMD-NOTES-ONLY.
006932
006933     
      * EXEC CICS SEND
006934*        FROM   (EL142DI)
006935*        MAPSET (WS-MAPSET-NAME)
006936*        MAP    (PI-MAP-NAME)
006937*        CURSOR
006938*        ERASE
006939*    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00015264' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303135323634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
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
           
006940
006941     PERFORM 9100-RETURN-TRAN.
006942
006943 8100-EXIT.
006944     EXIT.
006945
006946     EJECT
006947 8200-SEND-DATAONLY SECTION.
006948     IF EMI-ERROR NOT = ZERO
006949         PERFORM 9900-ERROR-FORMAT
006950       ELSE
006951         IF TRANSACTION-SUCCESSFUL
006952             PERFORM 9900-ERROR-FORMAT.
006953
006954     MOVE EIBTIME                TO  TIME-IN.
006955
006956     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
006957     MOVE '5'                    TO  DC-OPTION-CODE.
006958     PERFORM 8500-DATE-CONVERSION.
006959
006960     IF PI-MAP-NAME = EL142A
006961         MOVE DC-GREG-DATE-1-EDIT  TO ADATEO
006962         MOVE TIME-OUT             TO ATIMEO
006963         MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
006964         MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O
006965         MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O
006966         GO TO 8210-SEND-MAP.
006967
006968     IF PI-MAP-NAME = EL142B
006969         MOVE DC-GREG-DATE-1-EDIT  TO BDATEO
006970         MOVE TIME-OUT             TO BTIMEO
006971         MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
006972         GO TO 8210-SEND-MAP.
006973
006974     IF PI-MAP-NAME = EL142B2
006975         MOVE DC-GREG-DATE-1-EDIT  TO KDATEO
006976         MOVE TIME-OUT             TO KTIMEO
006977         MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O
006978         MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O
006979         MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O
006980         GO TO 8210-SEND-MAP.
006981
006982     IF PI-MAP-NAME = EL142C
006983         MOVE DC-GREG-DATE-1-EDIT  TO CDATEO
006984         MOVE TIME-OUT             TO CTIMEO
006985         MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O
006986         MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O
006987         GO TO 8210-SEND-MAP.
006988
006989     IF PI-MAP-NAME = EL142D
006990         MOVE DC-GREG-DATE-1-EDIT  TO DDATEO
006991         MOVE TIME-OUT             TO DTIMEO
006992         MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O
006993         GO TO 8210-SEND-MAP.
006994
006995     IF PI-MAP-NAME = EL142D2
006996         MOVE DC-GREG-DATE-1-EDIT  TO LDATEO
006997         MOVE TIME-OUT             TO LTIMEO
006998         MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O
006999         MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O
007000         MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O
007001         GO TO 8210-SEND-MAP.
007002
007003     IF PI-MAP-NAME = EL142E
007004         MOVE DC-GREG-DATE-1-EDIT  TO EDATEO
007005         MOVE TIME-OUT             TO ETIMEO
007006         MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O
007007         MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O
007008         GO TO 8210-SEND-MAP.
007009
007010     IF PI-MAP-NAME = EL142F
007011         MOVE DC-GREG-DATE-1-EDIT  TO FDATEO
007012         MOVE TIME-OUT             TO FTIMEO
007013         MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O
007014         MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O
007015         GO TO 8210-SEND-MAP.
007016
007017     IF PI-MAP-NAME = EL142G
007018         MOVE DC-GREG-DATE-1-EDIT  TO GDATEO
007019         MOVE TIME-OUT             TO GTIMEO
007020         MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O
007021         MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O
007022         GO TO 8210-SEND-MAP.
007023
007024     IF PI-MAP-NAME = EL142H
007025         MOVE DC-GREG-DATE-1-EDIT  TO HDATEO
007026         MOVE TIME-OUT             TO HTIMEO
007027         MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O
007028         GO TO 8210-SEND-MAP.
007029
007030     IF PI-MAP-NAME = EL142I
007031         MOVE DC-GREG-DATE-1-EDIT  TO IDATEO
007032         MOVE TIME-OUT             TO ITIMEO
007033         MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O
007034         GO TO 8210-SEND-MAP.
007035
007036     IF PI-MAP-NAME = EL142J
007037         
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)
007038*        END-EXEC
      *    MOVE '0"A                   "   #00015368' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303135333638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007039         
      * EXEC CICS FORMATTIME
007040*                  ABSTIME(LCP-CICS-DATE)
007041*                  YYMMDD(LCP-CURRENT-DATE-68)
007042*                  DATESEP('/')
007043*        END-EXEC
           MOVE '/' TO DFHEIV9
      *    MOVE 'j$((   "              $   #00015370' TO DFHEIV0
           MOVE X'6A2428282020202220202020' &
                X'202020202020202020202420' &
                X'2020233030303135333730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE, 
                 LCP-CURRENT-DATE-68, 
                 DFHEIV9
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007044         MOVE LCP-CURRENT-DATE-68  TO JDATEO
007045         MOVE TIME-OUT             TO JTIMEO
007046         MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O
007047         GO TO 8210-SEND-MAP.
007048
007049 8210-SEND-MAP.
007050     IF PI-USES-PAID-TO
007051        IF PI-MAP-NAME = EL142B
007052           MOVE 'PAID  TO     -'    TO BTHRUHDO
007053          ELSE
007054           IF PI-MAP-NAME = EL142I
007055              MOVE 'PAID  TO  DATE' TO ITHRUHDO.
007056
007057     IF PI-COMPANY-ID = 'DMD'
007058         PERFORM  8400-DMD-NOTES-ONLY.
007059
007060     
      * EXEC CICS SEND DATAONLY
007061*        FROM   (EL142DI)
007062*        MAPSET (WS-MAPSET-NAME)
007063*        MAP    (PI-MAP-NAME)
007064*        CURSOR
007065*    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00015391' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303135333931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
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
           
007066
007067     PERFORM 9100-RETURN-TRAN.
007068
007069 8290-EXIT.
007070     EXIT.
007071
007072     EJECT
007073 8300-SEND-TEXT SECTION.
007074
007075     
      * EXEC CICS SEND TEXT
007076*        FROM   (LOGOFF-TEXT)
007077*        LENGTH (LOGOFF-LENGTH)
007078*        ERASE  FREEKB
007079*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015406' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303135343036' TO DFHEIV0
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
           
007080
007081     
      * EXEC CICS RETURN
007082*    END-EXEC.
      *    MOVE '.(                    ''   #00015412' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135343132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007083
007084 8300-EXIT.
007085     EXIT.
007086
007087 8400-DMD-NOTES-ONLY SECTION.
007088
007089     IF NOT SYSTEM-MODIFY-CAP
007090        IF PI-MAP-NAME = EL142A
007091           IF WS-PI-EL142-PRIORITY = '9'
007092             MOVE AL-SANON   TO AREMINDA  ALETTERA  APAYMNTA
007093                                AAUTOPAA  ARESEXPA  ADENIALA
007094                                AIDCA     AFORMSA
007095             MOVE -1         TO ANOTESL.
007096
007097 8400-EXIT.
007098     EXIT.
007099
007100     EJECT
007101 8500-DATE-CONVERSION SECTION.
007102     
      * EXEC CICS LINK
007103*        PROGRAM  (ELDATCV)
007104*        COMMAREA (DATE-CONVERSION-DATA)
007105*        LENGTH   (DC-COMM-LENGTH)
007106*    END-EXEC.
      *    MOVE '."C                   (   #00015433' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135343333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007107
007108
007109 8500-EXIT.
007110     EXIT.
007111
007112 8600-DEEDIT SECTION.
007113     
      * EXEC CICS BIF DEEDIT
007114*        FIELD  (WS-DEEDIT-FIELD)
007115*        LENGTH (WS-DEEDIT-LENGTH)
007116*    END-EXEC.
      *    MOVE '@"L                   #   #00015444' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303135343434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 WS-DEEDIT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007117
007118 8600-EXIT.
007119     EXIT.
007120
007121     EJECT
007122 9000-RETURN-CICS SECTION.
007123     MOVE EL005                  TO  XCTL-PGM.
007124     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
007125     PERFORM 9300-XCTL.
007126
007127 9000-EXIT.
007128     EXIT.
007129
007130 9100-RETURN-TRAN SECTION.
007131     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
007132     MOVE PI-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
007133
007134     
      * EXEC CICS RETURN
007135*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
007136*        LENGTH   (PI-COMM-LENGTH)
007137*        TRANSID  (WS-TRANS-ID)
007138*    END-EXEC.
      *    MOVE '.(CT                  ''   #00015465' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135343635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007139
007140 9100-EXIT.
007141     EXIT.
007142
007143 9300-XCTL SECTION.
007144     MOVE DFHENTER               TO  EIBAID.
007145
007146     
      * EXEC CICS XCTL
007147*        PROGRAM  (XCTL-PGM)
007148*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
007149*        LENGTH   (PI-COMM-LENGTH)
007150*    END-EXEC.
      *    MOVE '.$C                   %   #00015477' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303135343737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007151
007152 9300-EXIT.
007153     EXIT.
007154
007155     EJECT
007156 9400-CLEAR SECTION.
007157     IF PI-MAP-NAME = EL142D2
007158         GO TO 1200-MAIN-LOGIC
007159     END-IF
007160
007161     MOVE PI-RETURN-TO-PROGRAM  TO  XCTL-PGM.
007162     PERFORM 9300-XCTL.
007163
007164 9400-EXIT.
007165     EXIT.
007166
007167 9600-PGMIDERR SECTION.
007168     
      * EXEC CICS HANDLE CONDITION
007169*        PGMIDERR (8300-SEND-TEXT)
007170*    END-EXEC.
      *    MOVE '"$L                   ! / #00015499' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303135343939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007171
007172     MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
007173
007174     MOVE EL005                  TO  XCTL-PGM
007175                                     LOGOFF-PGM.
007176     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
007177     MOVE SPACES                 TO  PI-ENTRY-CD-1.
007178     PERFORM 9300-XCTL.
007179
007180 9600-EXIT.
007181     EXIT.
007182
007183     EJECT
007184 9900-ERROR-FORMAT SECTION.
007185     ADD +1  TO  WS-ERROR-COUNT.
007186
007187     IF EMI-ERRORS-COMPLETE
007188         MOVE ER-ZERO            TO  EMI-ERROR
007189         GO TO 9900-EXIT.
007190
007191     
      * EXEC CICS LINK
007192*        PROGRAM  (EL001)
007193*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
007194*        LENGTH   (EMI-COMM-LENGTH)
007195*    END-EXEC.
      *    MOVE '."C                   (   #00015522' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135353232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007196
007197     MOVE ER-ZERO                TO  EMI-ERROR.
007198
007199 9900-EXIT.
007200     EXIT.
007201
007202     EJECT
007203 9990-ERROR SECTION.
007204     MOVE DFHEIBLK               TO EMI-LINE1.
007205     
      * EXEC CICS LINK
007206*        PROGRAM  (EL004)
007207*        COMMAREA (EMI-LINE1)
007208*        LENGTH   (72)
007209*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015536' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135353336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007210
007211     PERFORM 8200-SEND-DATAONLY.
007212     GO TO 9100-RETURN-TRAN.
007213
007214 9990-EXIT.
007215     EXIT.
007216
007217 9995-SECURITY-VIOLATION.
007218*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015567' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135353637' TO DFHEIV0
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
007219
007220 9995-EXIT.
007221      EXIT.
007222
007223 9999-LAST-PARAGRAPH SECTION.
007224
007225     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0130-MAIN-LOGIC,
                     6000-END-OF-FILE,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0200-ADD-PMNT-NOTE-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3490-DELETE-FORM-ARCHIVE,
                     3499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4200-NOTE-TRLR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4410-ADDRESS-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4440-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4490-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4910-ADDRESS-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5020-DISPLAY-CHECK-QUEUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6020-ELNAPS-NOTFND,
                     6020-ELNAPS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
