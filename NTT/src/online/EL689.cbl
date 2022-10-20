      *((program: EL689.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003
000004 PROGRAM-ID. EL689 .
000005*              PROGRAM CONVERTED BY
000006*              COBOL CONVERSION AID PO 5785-ABJ
000007*              CONVERSION DATE 04/20/94 15:10:07.
000008*                            VMOD=2.037
000009*
000010*AUTHOR.     LOGIC,INC.
000011*            DALLAS, TEXAS.
000012*DATE-COMPILED.
000013*REMARKS.    TRANSACTION - EXH3 - CREDIT LETTER WRITER.
000014*
000015******************************************************************
000016
000017* W-FILE-USED REFERENCE LIST:
000018* (1)  ELCNTL - COMPANY
000019* (2)  ELCNTL - LIFE BENEFIT
000020* (3)  ELCNTL - A&H BENEFIT
000021* (4)  ELCNTL - CARRIER
000022* (5)  MAIL
000023* (6)  ERACCT
000024* (7)  USED FOR SYSTEM VARIABLES ?
000025* (8)  ELCERT
000026* (9)  PENDING BUSINESS
000027* (10) COMPENSATION
000028* (11) ELCNTL - PROCESSOR DATA ?
000029* (12) ERCHEK
000030* (13) ERPYAJ
000031* (14) NOT USED
000032* (15) NOT USED
000033* (16) NOT USED
000034* (17) NOT USED
000035* (18) NOT USED
000036* (19) NOT USED
000037* (20) NOT USED
000038*
000039******************************************************************
000040*                   C H A N G E   L O G
000041*
000042* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000043*-----------------------------------------------------------------
000044*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000045* EFFECTIVE    NUMBER
000046*-----------------------------------------------------------------
000047* 101501    2001100100006  SMVA  ADD USERID to screen header
000048*                                ADJUSTED REDEFINES EL689AI FILLER
000049* 031504    2004020600009  SMVA  PULL MOST CURRENT ACCT ADDRESS
000050* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
000051* 100705    2004072800003  PEMA  ADD NEW VARIABLES
000052* 072308    2008062700001  PEMA  ADD NEW VARIABLES
000053* 092908    2008013100006  PEMA  MOVE VAR 154
000054* 102408  CR2008091000001  PEMA  MOVE PNDB VARS TO CERT VARS
000055* 051209  CR2009021700003  PEMA  ADD CRED BENE VARIABLES
000056* 051209  CR2009042700002  PEMA  ADD ERACCT ACCT NUMBER VARIABLE
000057* 022510  IR2009062300003  PEMA  ADD ENDFILE TEST TO ERACCT READ
000058* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000059* 041811  CR2011010400001  PEMA  DISABLE PF8 FOR CERTAIN USERS
000060* 042011  CR2011010400001  PEMA  FIX CRED BENE ADDRESS LINES
000061* 061411  CR2011061300001  PEMA  ADD ALWA TO PF8 DISABLE LIST
000062* 092911  IR2011092900001  AJRA  CLEAR LABEL LINES BEFORE CRED BEN
000063* 022712  IR2012022700001  AJRA  DON'T COUNT &&&&&& IN LINES
000064* 122011  CR2011022800001  AJRA  NAPERSOFT
000065* 090612  IR2012090600001  AJRA  NAPERSOFT
000066* 091712  IR2012091400001  AJRA  NAPERSOFT CERT ID
000067* 091912  IR2012091900001  AJRA  ALLOW FOLLOW UP DATE TO = CURR DT
000068* 101812  CR2012101700002  AJRA  ADD ENDARCH AND SCREENID
000069* 110512  CR2012101700002  AJRA  EXIT LETTER WRITER AFTER PF7
000070* 120512  CR2012101700002  AJRA  ADD RE ENCL CODE
000071* 011013  CR2012101700002  AJRA  CHECK REQ REASON CODE IND
000072* 012413  CR2013012100001  AJRA  ADD AUTO BILLING NOTE
000073* 121213  CR2013090300001  AJRA  VALIDATE ENC CODE AGAINST ELENCC
000074* 123113  CR2013090300001  AJRA  USE NEXT BUS DT FOR RESEND DT CAL
000075* 010814  CR2013090300001  AJRA  CALL SQL STORED PROC FOR NEXT BUS
000076* 021214  IR2014021200002  AJRA  ADD SQL DISCONNECT
000077* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
000078* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
000079* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
000080* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000081******************************************************************
000082
000083 DATA DIVISION.
000084 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000085 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
000086 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
000087                                   USAGE POINTER.
000088 77  FILLER  PIC X(32) VALUE '********************************'.
000089 77  FILLER  PIC X(32) VALUE '*    EL689 WORKING STORAGE     *'.
000090 77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
000091
000092
000093* EXEC SQL
000094*    INCLUDE SQLDA
000095* END-EXEC
000096
000099*EXEC SQL
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
000100
000102 EXEC SQL
          BEGIN DECLARE SECTION
000103 END-EXEC
000104
000105 01  SQLCMD                      PIC X(1024).
000106 01  SVR                         PIC X(32).
000107 01  USR                         PIC X(32).
000108 01  PASS                        PIC X(32).
000109 01  USR-PASS                    PIC X(64).
000110
000111 01  WS-SQL-DATA.
000112     05  WS-CYCLE-DATE           PIC X(10).
000113     05  WS-NEXT-BUS-DT          PIC X(10).
000114
000116 EXEC SQL
          END DECLARE SECTION
000117 END-EXEC
000118
000119
000120 01  P pointer.
000121 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000122 01  KIXHOST             pic x(9) value Z"HOSTNAME".
000123 01  var-ptr pointer.
000124 01  env-var-len                 pic 9(4)  binary.
000125 01  rc                          pic 9(9)  binary.
000126
000127 01  WS-KIXHOST                  PIC X(10).
000128 01  WS-KIXSYS.
000129     05  WS-KIX-FIL1             PIC X(10).
000130     05  WS-KIX-APPS             PIC X(10).
000131     05  WS-KIX-ENV              PIC X(10).
000132     05  WS-KIX-MYENV            PIC X(10).
000133     05  WS-KIX-SYS              PIC X(10).
000134
000135 01 srch-commarea.
000136*                                copy ELCADLTRSPI.
      *>>((file: ELCADLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 060611    2011022800001  PEMA  NEW COPYBOOK
000010* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
000011* 110612    2012101700002  AJRA  EXPAND PASSED DATA
000012******************************************************************
000013****************************************
000014*  commarea for NaperSoft On Demand Admin services letters
000015*  (business logic input & output)
000016****************************************
000017
000018     03  BL-INPUT.
000019         05  BL-DATA-SRCE        PIC X.
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-CARRIER          PIC X.
000022         05  BL-GROUP            PIC X(6).
000023         05  BL-STATE            PIC XX.
000024         05  BL-ACCOUNT          PIC X(10).
000025         05  BL-EFF-DT           PIC X(10).
000026         05  BL-CERT-NO          PIC X(11).
000027         05  BL-BATCH-NO         PIC X(6).
000028         05  BL-BATCH-SEQ        PIC 9(8).
000029         05  BL-RESP-NO          PIC X(10).
000030         05  BL-NO-OF-COPIES     PIC 99.
000031         05  BL-PROC-ID          PIC XXXX.
000032         05  BL-COMP-ID          PIC XXX.
000033         05  BL-PRINT-NOW-SW     PIC X.
000034         05  BL-ENC-CD           PIC XXX.
000035         05  BL-RESEND-DT        PIC X(10).
000036         05  BL-FOLLOW-UP-DT     PIC X(10).
000037         05  BL-ARCHIVE-NO       PIC 9(8).
000038         05  BL-FUNC             PIC X(8).
000039         05  BL-COMMENTS         PIC X(100).
000040         05  FILLER REDEFINES BL-COMMENTS.
000041             10  BL-REASON-CODE OCCURS 12 PIC X(4).
000042             10  BL-LETTER-TO-ACCT PIC X.
000043             10  BL-LETTER-TO-BENE PIC X.
000044             10  BL-WRITE-ERARCH   PIC X.
000045                 88  ERARCH-QWS      VALUE 'Q'.
000046                 88  ERARCH-BATCH    VALUE 'B'.
000047                 88  ERARCH-TEMP     VALUE 'T'.
000048             10  BL-PROCESS-TYPE PIC X(07).
000049             10  BL-CERT-FORM-ID PIC X(05).
000050             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
000051             10  BL-SOURCE-SCREEN PIC X(8).
000052             10  FILLER          PIC X(25).
000053
000054     03  BL-OUTPUT.
000055         05  BL-STATUS                   PIC X.
000056             88  BL-OK                      VALUE "P".
000057             88  BL-FAIL                  VALUE "F".
000058         05  BL-MESSAGE          PIC X(50).
000059     03  BL-RECORD-PASSED-DATA   PIC X(6200).
000060     03  FILLER                  PIC X(31).
      *<<((file: ELCADLTRSPI))
000137 01  W-WORK-AREAS.
000138     12  FILLER                  PIC  X(18)
000139                                      VALUE 'PROGRAM WORK AREA:'.
000140
000141     12  W-ASKTIME-CTR           PIC S9(04)  COMP   VALUE +0.
000142     12  W-CWA-NDX               PIC S9(04)  COMP   VALUE +0.
000143     12  W-DISPLAY-NDX           PIC S9(04)  COMP   VALUE +0.
000144     12  W-FIRST-BAD-VARIABLE    PIC S9(04)  COMP   VALUE +0.
000145     12  W-NDX                   PIC S9(04)  COMP   VALUE +0.
000146     12  W-NDX2                  PIC S9(04)  COMP   VALUE +0.
000147     12  W-SEQ-CTR               PIC S9(04)  COMP   VALUE +0.
000148     12  W-TS-ITEM               PIC S9(04)  COMP   VALUE +0.
000149     12  W-TOTAL-LINE-LENGTH     PIC S9(04)  COMP   VALUE +0.
000150     12  W-WORK-NDX              PIC S9(04)  COMP   VALUE +0.
000151
000152     12  W-ADJUST-SHORT          PIC S9(03) VALUE +0   COMP-3.
000153     12  W-DIFFERENCE            PIC S9(07)V9(02)
000154                                            VALUE +0   COMP-3.
000155     12  W-INITIAL-COLUMN        PIC S9(03) VALUE +0   COMP-3.
000156     12  W-LAST-COLUMN           PIC S9(03) VALUE +70 COMP-3.
000157     12  W-LAST-SQUEEZED-SPACE   PIC S9(03) VALUE +0   COMP-3.
000158     12  W-LAST-TX-SPACE         PIC S9(03) VALUE +0   COMP-3.
000159     12  W-LAST-WC-SPACE         PIC S9(03) VALUE +0   COMP-3.
000160     12  W-LINE-COUNT            PIC S9(03) VALUE +0   COMP-3.
000161     12  W-LINE-INDENT-1         PIC  9(02) VALUE 0    COMP-3.
000162     12  W-LINE-INDENT-2         PIC  9(02) VALUE 0    COMP-3.
000163     12  W-LINE-INDENT-3         PIC  9(02) VALUE 0    COMP-3.
000164     12  W-LINE-INDENT-4         PIC  9(02) VALUE 0    COMP-3.
000165     12  W-LINE-INDENT-5         PIC  9(02) VALUE 0    COMP-3.
000166     12  W-MAX-LINES-PER-PAGE    PIC  9(02) VALUE 56   COMP-3.
000167     12  W-NEXT-INDENT           PIC  9(02) VALUE 0    COMP-3.
000168     12  W-PAGE                  PIC S9(02) VALUE +0   COMP-3.
000169     12  W-PAGE-LINE             PIC S9(03) VALUE +0   COMP-3.
000170     12  W-PARAGRAPH-INDENT      PIC  9(02) VALUE 0    COMP-3.
000171     12  W-ROLL-COUNTER          PIC S9(03) VALUE +0   COMP-3.
000172     12  W-START-COLUMN          PIC S9(03) VALUE +1   COMP-3.
000173     12  W-TEMP-CURRENT-LINE     PIC S9(03) VALUE +0   COMP-3.
000174     12  W-TOO-FAR               PIC S9(03) VALUE +71 COMP-3.
000175     12  W-TOP-MARGIN            PIC  9(02) VALUE 0    COMP-3.
000176     12  W-TOTAL-TX-LINES        PIC S9(03) VALUE +0   COMP-3.
000177     12  W-TS-GROUP-WORK         PIC  9(05) VALUE 0    COMP-3.
000178     12  W-WORK-INDENT           PIC  9(02) VALUE 0    COMP-3.
000179     12  W-WORK-AMOUNT           PIC S9(09)V9(02)
000180                                            VALUE +0   COMP-3.
000181
000182     12  W-DATA-SOURCE           PIC  9(01) VALUE 0.
000183     12  W-LABEL-SOURCE          PIC  9(01) value 0.
000184         88  W-LABEL-SOURCE-VALID           VALUE 1 THRU 7.
000185     12  W-LABEL-JOINT-NAME      PIC  X(30) VALUE SPACES.
000186     12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
000187     12  W-NUMB-LABEL-LINES      PIC  9(01) VALUE 0.
000188     12  W-PRINT-CONTROL         PIC  9(02) VALUE 0.
000189     12  W-SAVE-REVISION         PIC  9(03) value 0.
000190     12  W-DISPLAY-3             PIC  9(03) value 0.
000191     12  W-DISPLAY-8             PIC  9(08) value 0.
000192     12  W-DISPLAY-7             PIC  9(07) value 0.
000193     12  W-EDIT-2-5-S            PIC  Z9.9(05).
000194     12  W-EDIT-3-0              PIC  ZZ9.
000195     12  W-EDIT-7-2              PIC  $$,$$$,$$9.99.
000196     12  W-EDIT-7-2-NEGATIVE     PIC  $$,$$$,$$9.99-.
000197     12  W-EDIT-9-2              PIC  $$$$,$$$,$$9.99.
000198     12  W-EDIT-12-2             PIC  $$$$,$$$,$$$,$$9.99.
000199
000200     12  WS-ACCT-PKEY-MATCH-SW   PIC  X(01) VALUE SPACE.
000201         88  PRIOR-MATCH-ACCT-PKEY          VALUE 'Y'.
000202
000203     12  W-ARCH-SUPPRESS         PIC 99999999.
000204     12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
000205                                 PIC  X(08).
000206     12  W-COMP-WORK-AREA.
000207         16  W-CWA-CHAR OCCURS 10 TIMES
000208                                 PIC  X(01).
000209     12  W-INCOMING-ARCHIVE      PIC  X(08).
000210     12  W-INCOMING-ARCH-NO REDEFINES W-INCOMING-ARCHIVE
000211                                 PIC  9(08).
000212
000213     12  W-BATCH-BREAKDOWN.
000214         16  W-BATCH-CONTROL     PIC S9(08) COMP.
000215         16  W-BATCH-SEQ         PIC S9(04) COMP.
000216     12  W-BEN-HOLD              PIC  X(02).
000217     12  W-BENEFIT-WORK          PIC  X(03).
000218     12  W-BEN-R REDEFINES W-BENEFIT-WORK.
000219         16  W-ELIM-DAYS         PIC  X(02).
000220         16  FILLER              PIC  X(01).
000221     12  W-CALL-PGM              PIC  X(08).
000222     12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
000223
000224     12  FILLER                  PIC  X(09) VALUE 'LINE CNTL'.
000225     12  W-LINE-CONTROL-RECORD.
000226         16  W-LC-LINE-WIDTH     PIC  9(02).
000227         16  FILLER              PIC  X(01).
000228         16  W-LC-PARAGRAPH-INDENT
000229                                 PIC  9(02).
000230         16  FILLER              PIC  X(01).
000231         16  W-LC-MAX-LINES-PER-PAGE
000232                                 PIC  9(02).
000233         16  FILLER              PIC  X(01).
000234         16  W-LC-LINE-ADJUST    PIC  9(02).
000235         16  FILLER              PIC  X(01).
000236         16  W-LC-LINE-INDENT-1  PIC  9(02).
000237         16  FILLER              PIC  X(01).
000238         16  W-LC-LINE-INDENT-2  PIC  9(02).
000239         16  FILLER              PIC  X(01).
000240         16  W-LC-LINE-INDENT-3  PIC  9(02).
000241         16  FILLER              PIC  X(01).
000242         16  W-LC-LINE-INDENT-4  PIC  9(02).
000243         16  FILLER              PIC  X(01).
000244         16  W-LC-LINE-INDENT-5  PIC  9(02).
000245         16  FILLER              PIC  X(01).
000246         16  W-LC-TOP-MARGIN     PIC  9(02).
000247         16  FILLER              PIC  X(01).
000248         16  W-LC-CASE-IND       PIC  X(01).
000249             88  W-LC-USE-BOTH-CASES        VALUE 'Y'.
000250         16  FILLER              PIC  X(01).
000251         16  W-LC-PAGE-IND       PIC  X(01).
000252             88  W-LC-CREATE-PAGES          VALUE 'Y'.
000253     12  W-DATE-WORK             PIC  9(07).
000254     12  W-DT-REDEF REDEFINES W-DATE-WORK.
000255         16  FILLER              PIC  X(02).
000256         16  W-DT-WORK           PIC  9(05).
000257
000258     12  W-DEEDIT-FIELD          PIC  X(15).
000259     12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
000260
000261     12  W-EDIT-DATE-1.
000262         16  W-ED1-MM            PIC  X(02).
000263         16  FILLER              PIC  X(01) VALUE '/'.
000264         16  W-ED1-DD            PIC  X(02).
000265         16  FILLER              PIC  X(01) VALUE '/'.
000266         16  W-ED1-YY            PIC  X(02).
000267
000268     12  W-EDIT-DATE-2.
000269         16  W-ED2-DD            PIC  X(02).
000270         16  FILLER              PIC  X(01) VALUE '/'.
000271         16  W-ED2-MM            PIC  X(02).
000272         16  FILLER              PIC  X(01) VALUE '/'.
000273         16  W-ED2-YY            PIC  X(02).
000274
000275     12  W-EDIT-A-DATE.
000276         16  W-EDIT-A-MM                  PIC X(2).
000277         16  FILLER                       PIC X(1).
000278         16  W-EDIT-A-DD                  PIC X(2).
000279         16  FILLER                       PIC X(1).
000280         16  W-EDIT-A-CCYY.
000281             20  W-EDIT-A-CC              PIC X(2).
000282             20  W-EDIT-A-YY              PIC X(2).
000283
000284     12  W-LABEL-HOLD-AREA.
000285         16  W-LABEL-LINES OCCURS 6 TIMES.
000286             20  W-LABEL-ZIP.
000287                 24  W-LABEL-1ST-ZIP  PIC  X(05).
000288                 24  W-LABEL-DASH     PIC  X(01).
000289                 24  W-LABEL-2ND-ZIP  PIC  X(04).
000290             20  W-LAB-CAN-POSTAL-CODES REDEFINES W-LABEL-ZIP.
000291                 24  W-LAB-CAN-POSTAL-CD-1
000292                                      PIC  X(03).
000293                 24  W-LAB-CAN-DASH   PIC  X(01).
000294                 24  W-LAB-CAN-POSTAL-CD-2
000295                                      PIC  X(03).
000296                 24  W-LAB-CAN-FILLER PIC  X(03).
000297             20  FILLER               PIC  X(10).
000298             20  W-LAST-ZIP.
000299                 24  W-LAST-1ST-ZIP   PIC  X(05).
000300                 24  W-LABEL-DASH-LAST
000301                                      PIC  X(01).
000302                 24  W-LAST-2ND-ZIP   PIC  X(04).
000303             20  W-LAST-CAN-POSTAL-CODES REDEFINES W-LAST-ZIP.
000304                 24  W-LAST-CAN-POSTAL-CD-1
000305                                      PIC  X(03).
000306                 24  W-LAST-CAN-DASH  PIC  X(01).
000307                 24  W-LAST-CAN-POSTAL-CD-2
000308                                      PIC  X(03).
000309                 24  W-LAST-CAN-FILLER
000310                                      PIC  X(03).
000311
000312     12  W-LAST-CHAR             PIC  X(01).
000313         88  W-LAST-CHAR-PUNC    VALUE '-' '/'.
000314     12  W-LAST-SQ-CHAR          PIC  X(01).
000315
000316     12  W-LINE-NUM.
000317         16  W-LINE1             PIC  X(01).
000318         16  W-LINE23            PIC  9(02).
000319     12  W-LIN-NUM REDEFINES W-LINE-NUM
000320                                 PIC  9(03).
000321
000322     12  W-NAME-LAST             PIC  X(15).
000323     12  W-NAME-FIRST.
000324         16  W-NAME-FIRST-INIT   PIC  X(01).
000325         16  W-NAME-FIRST-REMAIN PIC  X(14).
000326     12  W-NAME-MIDDLE.
000327         16  FILLER              PIC  X(01).
000328         16  W-NAME-MIDDLE-2     PIC  X(01).
000329         16  FILLER              PIC  X(13).
000330
000331     12  W-PAGE-PRT.
000332         16  FILLER              PIC  X(34) VALUE SPACES.
000333         16  W-PAGE-NUMBER       PIC  Z9.
000334         16  FILLER              PIC  X(01) VALUE '.'.
000335         16  FILLER              PIC  X(33) VALUE SPACES.
000336
000337     12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
000338     12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
000339         16  FILLER              PIC  9(01).
000340         16  W-PHI-AREA          PIC  9(03).
000341         16  W-PHI-PFX           PIC  9(03).
000342         16  W-PHI-SFX           PIC  9(04).
000343     12  W-PHONE-OUT.
000344         16  W-PO-AREA           PIC  X(03).
000345         16  FILLER              PIC  X(01) VALUE '-'.
000346         16  W-PO-PFX            PIC  X(03).
000347         16  FILLER              PIC  X(01) VALUE '-'.
000348         16  W-PO-SFX            PIC  X(04).
000349
000350     12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
000351     12  W-SAVE-BIN-NEXT-BUS-DT  PIC  X(02) VALUE SPACES.
000352     12  W-SAVE-NEXT-BUS-DT-EDIT-A PIC X(10)   VALUE SPACES.
000353     12  W-SAVE-CYCLE-DAY-OF-WEEK PIC S9   COMP-3 VALUE +0.
000354     12  W-SAVE-EDIT-A-DATE      PIC  X(10) VALUE SPACES.
000355     12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
000356     12  W-SAVE-PLAN             PIC  X(02) VALUE SPACES.
000357
000358     12  W-SINGLE-LINE           PIC  X(70).
000359     12  W-SINGLE-LINE-BY REDEFINES W-SINGLE-LINE.
000360         16  ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2 NDXA
000361                                 PIC  X(01).
000362
000363     12  W-SQUEEZED-LINE.
000364         16  W-SQ-CHAR OCCURS 70 TIMES
000365                       INDEXED BY W-SQ-NDX
000366                                 PIC  X(01).
000367
000368     12  W-TIME-IN               PIC S9(07).
000369     12  W-TIME-OUT-R REDEFINES W-TIME-IN.
000370         16  FILLER              PIC  X(01).
000371         16  W-TIME-OUT          PIC  99V9(02).
000372         16  FILLER              PIC  X(02).
000373
000374     12  W-TS-NAME-TEXT.
000375         16  W-TS-ID-TEXT        PIC  X(04) VALUE '104A'.
000376         16  W-TS-ID-TIME
000377             REDEFINES W-TS-ID-TEXT
000378                                 PIC S9(07) COMP-3.
000379         16  W-TS-TERM-TEXT.
000380             20 W-TS-TERM-PREFIX PIC  X(02).
000381             20 FILLER           PIC  X(02).
000382     12  W-TS-NAME-SCREEN.
000383         16  FILLER              PIC  X(04) VALUE '689X'.
000384         16  W-TS-TERM-SCREEN    PIC  X(04).
000385
000386     12  W-WORK-LINE.
000387         16  W-WORK-CHAR OCCURS 70 TIMES
000388                       INDEXED BY W-WC-NDX
000389                                  W-WC-NDX2
000390                                  W-WC-NDX3
000391                                 PIC  X(01).
000392
000393     12  W-WORK-ZIP-NUMERIC      PIC  9(09).
000394     12  W-WORK-ZIP REDEFINES W-WORK-ZIP-NUMERIC.
000395         16  W-WORK-ZIP5         PIC  X(05).
000396         16  W-WORK-ZIP4         PIC  X(04).
000397     12  W-CANADIAN-POSTAL-CODES REDEFINES W-WORK-ZIP-NUMERIC.
000398         16  W-CAN-POSTAL-CD-1.
000399             20  FILLER          PIC  X(01).
000400                 88 W-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000401             20  FILLER          PIC  X(02).
000402         16  W-CAN-POSTAL-CD-2   PIC  X(03).
000403         16  W-FILLER            PIC  X(03).
000404
000405**** Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
000406*                   COPY ELCZREC.
      *>>((file: ELCZREC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCZREC.                            *
000005*                                                                *
000006*   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
000007*                                                                *
000008******************************************************************
000009*-----------------------------------------------------------------
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 122011    2011022800001  AJRA  NEW FILE
000018* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
000019* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
000020* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
000021* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
000022*-----------------------------------------------------------------
000023 01  W-Z-CONTROL-DATA.
000024     05  W-NUMBER-OF-COPIES      PIC  9.
000025     05  FILLER                  PIC  X.
000026     05  W-DAYS-TO-FOLLOW-UP     PIC  999.
000027     05  FILLER                  PIC  X.
000028     05  W-DAYS-TO-RESEND        PIC  999.
000029     05  FILLER                  PIC  X.
000030     05  W-FORM-TO-RESEND        PIC  X(4).
000031     05  W-ADD-BAR-CODE          PIC  X.
000032     05  W-PROMPT-LETTER         PIC  X.
000033     05  W-HAS-RETURN-ENV        PIC  X.
000034     05  W-ENCLOSURE-CD          PIC  XXX.
000035     05  W-SIG-FLAG-DEFAULT      PIC  X.
000036     05  W-AUTO-CLOSE-IND        PIC  X.
000037     05  FILLER                  PIC  X.
000038     05  W-LETTER-TO-BENE        PIC  X.
000039     05  FILLER                  PIC  X.
000040     05  W-LETTER-TO-ACCT        PIC  X.
000041     05  FILLER                  PIC  X.
000042     05  W-LETTER-TYPE           PIC  X.
000043     05  FILLER                  PIC  X.
000044     05  W-PRINT-CERTIFICATE     PIC  X.
000045     05  FILLER                  PIC  X.
000046     05  W-REFUND-REQUIRED       PIC  X.
000047     05  FILLER                  PIC  X.
000048     05  W-ONBASE-CODE           PIC  XX.
000049     05  FILLER                  PIC  X.
000050     05  W-ACCT-SUMM             PIC  X.
000051     05  FILLER                  PIC  X.
000052     05  W-CSO-SUMM              PIC  X.
000053     05  FILLER                  PIC  X.
000054     05  W-REASONS-REQUIRED      PIC  X.
000055     05  FILLER                  PIC  X(29).
      *<<((file: ELCZREC))
000407                                 EJECT
000408 01  W-SWITCH-INDICATORS-AREA.
000409     12  FILLER                  PIC  X(16)
000410                                      VALUE 'PROGRAM SWITCHS:'.
000411     12  W-ADDRESS-ONLY-SW       PIC  X(01) VALUE ' '.
000412         88  W-ADDRESS-ONLY      VALUE 'Y'.
000413         88  W-FULL-DATA         VALUE ' '.
000414     12  W-ACCT-BROWSE-STARTED-SW
000415                                 PIC  X(01) VALUE 'N'.
000416         88  W-ACCT-BROWSE-STARTED          VALUE 'Y'.
000417     12  W-ARCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
000418     12  W-CERT-FOUND-SW         PIC  X(01) VALUE 'N'.
000419         88  W-CERT-FOUND                   VALUE 'Y'.
000420     12  W-CHARACTER-TYPE        PIC  X(01).
000421         88  W-END-OF-SENTENCE   VALUE '.' '?' '!'.
000422         88  W-PUNCTUATION       VALUE '.' '?' '!' ',' ';'.
000423         88  W-SPACE             VALUE ' '.
000424     12  W-COMP-BROWSE-SW        PIC  X(01) VALUE 'N'.
000425         88  W-COMP-BROWSE-STARTED          VALUE 'Y'.
000426     12  W-DATA-SHRINK-IND       PIC  X(01) VALUE SPACES.
000427         88  W-DATA-SHRINKING    VALUE 'Y'.
000428     12  W-END-OF-SENTENCE-IND   PIC  X(01) VALUE SPACES.
000429         88  W-END-OF-SENTENCE-WORKING      VALUE 'Y'.
000430     12  W-FIRST-BAD-VARIABLE-IND
000431                                 PIC  X(01) VALUE SPACES.
000432         88  W-FIRST-BAD-VARIABLE-FOUND     VALUE 'Y'.
000433     12  W-FIRST-CHAR-FOUND-IND  PIC  X(01) VALUE SPACES.
000434         88  W-FIRST-CHAR-FOUND             VALUE 'Y'.
000435         88  W-FIRST-CHAR-NOT-FOUND         VALUE SPACES.
000436     12  W-FORM-CHANGED-IND      PIC  X(01) VALUE SPACES.
000437         88  W-FORM-CHANGED                 VALUE 'Y'.
000438     12  W-FORM-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
000439         88  W-FORM-SQUEEZE-ON              VALUE 'Y'.
000440         88  W-FORM-SQUEEZE-OFF             VALUE ' '.
000441     12  W-HOLD-IND              PIC  X(01) VALUE SPACES.
000442         88  W-HOLD-ON                      VALUE 'Y'.
000443         88  W-HOLD-OFF                     VALUE ' '.
000444     12  W-INDIVIDUAL-DATA-SW    PIC  X(01) VALUE ' '.
000445         88  W-INDIVIDUAL-DATA-COMPLETED    VALUE 'Y'.
000446         88  W-INDIV-DATA-NOT-COMPLETED     VALUE ' '.
000447     12  W-KEY-FIELDS-CHANGED-IND
000448                                 PIC  X(01) VALUE ' '.
000449         88  W-KEY-FIELDS-CHANGED           VALUE 'Y'.
000450         88  W-KEY-FIELDS-NOT-CHANGED       VALUE ' '.
000451     12  W-LAST-ONE              PIC  X(01) VALUE HIGH-VALUES.
000452         88  W-LAST-ONE-A-SPACE  VALUE ' '.
000453     12  W-LINE-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
000454         88  W-NEW-PARAGRAPH                VALUE 'P' 'Q' 'R' 'S'
000455                                                   'T' 'U'.
000456         88  W-CONTINUE-PARAGRAPH           VALUE 'C' 'D' 'E' 'F'
000457                                                   'G' 'H'.
000458         88  W-FORM-CONTROL-LINE            VALUE 'K'.
000459         88  W-DO-NOT-ADJUST                VALUE 'N'.
000460         88  W-ADJUST-TO-LINE-LENGTH        VALUE 'A'.
000461         88  W-AS-IS                        VALUE 'A' 'N'.
000462         88  W-CONTINUE-PREVIOUS-PROCESS    VALUE ' ' '1' '2' '3'
000463                                                   '4' '5'.
000464
000465     12  W-PNDB-FOUND-SW         PIC  X(01) VALUE 'N'.
000466         88  W-PNDB-FOUND                   VALUE 'Y'.
000467     12  W-REMAINING-VAR-SW      PIC  X(01) VALUE SPACES.
000468         88  W-REMAINING-VAR-FOUND          VALUE 'Y'.
000469     12  W-REVERSE-DATE-SW       PIC  X(01) VALUE SPACES.
000470         88  W-REVERSE-DATE                 VALUE 'Y'.
000471     12  W-TEXT-BROWSED-SW       PIC  X(01) VALUE 'N'.
000472         88  W-TEXT-BROWSE-STARTED          VALUE 'Y'.
000473         88  W-TEXT-BROWSE-NOT-STARTED      VALUE 'N'.
000474
000475     12  W-VALID-ENDT-SW         PIC  X(01) VALUE 'N'.
000476         88  W-VALID-ENDT                   VALUE 'Y'.
000477     12  W-RESPONSE              PIC S9(8)   COMP.
000478         88  RESP-NORMAL                  VALUE +00.
000479         88  RESP-NOTFND                  VALUE +13.
000480         88  RESP-DUPREC                  VALUE +14.
000481         88  RESP-DUPKEY                  VALUE +15.
000482         88  RESP-NOTOPEN                 VALUE +19.
000483         88  RESP-ENDFILE                 VALUE +20.
000484                                 EJECT
000485 01  WS-SAVE-AREA.
000486
000487     12  WS-SAV-AM-DEFN-1.
000488         16  WS-SAV-AM-AGT-COMMS OCCURS 10 TIMES.
000489             20  WS-SAV-AM-AGT             PIC  X(10).
000490             20  WS-SAV-AM-COM-TYP         PIC  X(01).
000491             20  FILLER                    PIC  X(15).
000492
000493     12  WS-SAV-AM-REMIT-TO                PIC  9(02).
000494     12  WS-SAV-AM-CSR-CODE                PIC  X(04).
000495
000496     12  WS-SAV-AM-CARRIER                 PIC  X(01).
000497     12  WS-SAV-AM-ACCOUNT                 PIC  X(10).
000498     12  WS-SAV-AM-CONTROL-NAME            PIC  X(30).
000499     12  WS-SAV-AM-NAME                    PIC  X(30).
000500     12  WS-SAV-AM-PERSON                  PIC  X(30).
000501     12  WS-SAV-AM-ADDRS                   PIC  X(30).
000502     12  WS-SAV-AM-CITY                    PIC  X(30).
000503     12  WS-SAV-AM-ERACCT-ACCOUNT          PIC  X(10).
000504
000505     12  WS-SAV-AM-ZIP.
000506         16  WS-SAV-AM-ZIP-PRIME.
000507             20  WS-SAV-AM-ZIP-PRI-1ST     PIC  X(01).
000508                 88  SAVE-AM-CANADIAN-POST-CODE       VALUE
000509                     'A' THRU 'Z'.
000510             20  FILLER                    PIC  X(04).
000511         16  FILLER                        PIC  X(04).
000512     12  WS-SAV-AM-CANADIAN-POSTAL-CODE REDEFINES
000513                        WS-SAV-AM-ZIP      PIC  X(09).
000514
000515     12  WS-SAV-AM-TEL-NO.
000516         16  WS-SAV-AM-AREA-CODE           PIC 9(03).
000517         16  WS-SAV-AM-TEL-PRE             PIC 9(03).
000518         16  WS-SAV-AM-TEL-NBR             PIC 9(04).
000519
000520 01  W-KEY-AREAS.
000521     12  FILLER                    PIC  X(13)
000522                                      VALUE 'PROGRAM KEYS:'.
000523
000524     12  W-ACCT-SAVE-KEY           PIC  X(20).
000525     12  W-ACCT-KEY.
000526         16  W-ACCT-PARTIAL-KEY.
000527             20  W-ACCT-COMPANY-CD PIC  X(01).
000528             20  W-ACCT-CARRIER    PIC  X(01).
000529             20  W-ACCT-GROUPING   PIC  X(06).
000530             20  W-ACCT-STATE      PIC  X(02).
000531             20  W-ACCT-ACCOUNT    PIC  X(10).
000532         16  W-ACCT-EXP-DT         PIC  X(02).
000533
000534     12  W-ARCH-SAVE-KEY         PIC  X(05).
000535     12  W-ARCH-KEY.
000536         16  W-ARCH-PARTIAL-KEY.
000537             20  W-ARCH-COMPANY-CD
000538                                 PIC  X(01).
000539             20  W-ARCH-NUMBER   PIC S9(08)      COMP.
000540         16  W-ARCH-SEQ-NO       PIC S9(04)      COMP VALUE +0.
000541
000542     12  W-ARCT-KEY.
000543         16  W-ARCT-PARTIAL-KEY.
000544             20  W-ARCT-COMPANY-CD
000545                                 PIC  X(01).
000546             20  W-ARCT-NUMBER   PIC S9(08)      COMP.
000547         16  W-ARCT-REC-TYPE     PIC  X(01).
000548         16  W-ARCT-SEQ-NO       PIC S9(04)      COMP VALUE +0.
000549
000550     12  W-CERT-KEY.
000551         16  W-CERT-COMPANY-CD   PIC  X(01).
000552         16  W-CERT-CARRIER      PIC  X(01).
000553         16  W-CERT-GROUPING     PIC  X(06).
000554         16  W-CERT-STATE        PIC  X(02).
000555         16  W-CERT-ACCOUNT      PIC  X(10).
000556         16  W-CERT-EFF-DT       PIC  X(02).
000557         16  W-CERT-CERT-NO.
000558             20  W-CERT-CERT-PRIME
000559                                 PIC  X(10).
000560             20  W-CERT-CERT-SFX PIC  X(01).
000561
000562     12  W-CHEK-KEY.
000563         16  W-CHEK-COMPANY-CD   PIC  X(01).
000564         16  W-CHEK-CARRIER      PIC  X(01).
000565         16  W-CHEK-GROUPING     PIC  X(06).
000566         16  W-CHEK-STATE        PIC  X(02).
000567         16  W-CHEK-ACCOUNT      PIC  X(10).
000568         16  W-CHEK-EFF-DT       PIC  X(02).
000569         16  W-CHEK-CERT-NO.
000570             20  W-CHEK-CERT-PRIME
000571                                 PIC  X(10).
000572             20  W-CHEK-CERT-SFX PIC  X(01).
000573         16  W-CHEK-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
000574
000575     12  W-CNTL-KEY.
000576         16  W-CNTL-COMPANY-ID   PIC  X(03).
000577         16  W-CNTL-RECORD-TYPE  PIC  X(01)   VALUE '1'.
000578         16  W-CNTL-GENL.
000579             20  W-CNTL-GEN1     PIC  X(02)   VALUE SPACES.
000580             20  W-CNTL-GEN2.
000581                 24 W-CNTL-GEN3  PIC  X(01)   VALUE SPACES.
000582                 24 W-CNTL-GEN4  PIC  X(01)   VALUE SPACES.
000583         16  W-CNTL-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
000584
000585     12  W-COMP-SAVE-KEY         PIC  X(29).
000586     12  W-COMP-KEY.
000587         16  W-COMP-COMPANY-CD   PIC  X(01).
000588         16  W-COMP-CARRIER      PIC  X(01).
000589         16  W-COMP-GROUPING     PIC  X(06).
000590         16  W-COMP-RESP-PERSON.
000591             20  W-COMP-RP-CHAR OCCURS 10 TIMES
000592                                INDEXED BY W-COMP-NDX
000593                                 PIC  X(01).
000594         16  W-COMP-ACCOUNT      PIC  X(10).
000595         16  W-COMP-TYPE         PIC  X(01).
000596
000597     12  W-DELETE-KEY.
000598         16  W-DELETE-PARTIAL-KEY.
000599             20  W-DELETE-COMPANY-CD
000600                                 PIC  X(01).
000601             20  W-DELETE-NUMBER PIC S9(08)      COMP.
000602         16  W-DELETE-RECORD-TYPE
000603                                 PIC  X(01).
000604         16  W-DELETE-SEQ        PIC S9(04)      COMP VALUE +0.
000605
000606     12  W-MAIL-KEY.
000607         16  W-MAIL-COMPANY-CD   PIC  X(01).
000608         16  W-MAIL-CARRIER      PIC  X(01).
000609         16  W-MAIL-GROUPING     PIC  X(06).
000610         16  W-MAIL-STATE        PIC  X(02).
000611         16  W-MAIL-ACCOUNT      PIC  X(10).
000612         16  W-MAIL-EFF-DT       PIC  X(02).
000613         16  W-MAIL-CERT-NO.
000614             20  W-MAIL-CERT-PRIME
000615                                 PIC  X(10).
000616             20  W-MAIL-CERT-SFX PIC  X(01).
000617
000618     12  W-PNDB-KEY.
000619         16  W-PNDB-COMPANY-CD   PIC  X(01).
000620         16  W-PNDB-ENTRY        PIC  X(06).
000621         16  W-PNDB-SEQ-NO       PIC S9(04)  COMP.
000622         16  W-PNDB-CHG-SEQ-NO   PIC S9(04)  COMP.
000623
000624     12  W-PNDB2-KEY.
000625         16  W-PNDB2-COMPANY-CD  PIC  X(01).
000626         16  W-PNDB2-CARRIER     PIC  X(01).
000627         16  W-PNDB2-GROUPING    PIC  X(06).
000628         16  W-PNDB2-STATE       PIC  X(02).
000629         16  W-PNDB2-ACCOUNT     PIC  X(10).
000630         16  W-PNDB2-EFF-DT      PIC  X(02).
000631         16  W-PNDB2-CERT-NO.
000632             20  W-PNDB2-CERT-PRIME
000633                                 PIC  X(10).
000634             20  W-PNDB2-CERT-SFX
000635                                 PIC  X(01).
000636         16  W-PNDB2-ALT-CHG-SEQ-NO
000637                                 PIC S9(04)  COMP.
000638         16  W-PNDB2-TYPE        PIC  X(01).
000639
000640     12  W-PYAJ-KEY.
000641         16  W-PYAJ-COMPANY-CD   PIC  X(01).
000642         16  W-PYAJ-CARRIER      PIC  X(01).
000643         16  W-PYAJ-GROUPING     PIC  X(06).
000644         16  W-PYAJ-FIN-RESP     PIC  X(10).
000645         16  W-PYAJ-ACCOUNT      PIC  X(10).
000646         16  W-PYAJ-FILE-SEQ-NO  PIC S9(08)    COMP.
000647         16  W-PYAJ-RECORD-TYPE  PIC  X(01).
000648
000649     12  W-SC-QUID-KEY.
000650         16  W-SC-QUID-TERMINAL  PIC  X(04).
000651         16  W-SC-QUID-SYSTEM    PIC  X(04).
000652
000653     12  W-TEXT-SAVE-KEY         PIC  X(05).
000654     12  W-TEXT-KEY.
000655         16  W-TEXT-PARTIAL-KEY.
000656             20  W-TEXT-COMPANY-CD
000657                                 PIC  X(01).
000658             20  W-TEXT-LETTER   PIC  X(04).
000659         16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
000660         16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
000661
000662     12  W-ERENDT-KEY-BY-ARCH.
000663         16  W-ERENDT-COMPANY-CD-A1 PIC X.
000664         16  W-ERENDT-ARCHIVE       PIC 9(8) BINARY.
000665
000666     12  W-ELEOBC-KEY.
000667         16  W-EOBC-COMPANY-CD   PIC X.
000668         16  W-EOBC-REC-TYPE     PIC X.
000669         16  W-EOBC-CODE         PIC X(4).
000670         16  FILLER              PIC X(9).
000671     12  W-ERNOTE-KEY.
000672         16  W-NOTE-COMPANY-CD   PIC X.
000673         16  W-NOTE-CARRIER      PIC X.
000674         16  W-NOTE-GROUPING     PIC X(6).
000675         16  W-NOTE-STATE        PIC XX.
000676         16  W-NOTE-ACCOUNT      PIC X(10).
000677         16  W-NOTE-CERT-EFF-DT  PIC XX.
000678         16  W-NOTE-CERT-PRIME   PIC X(10).
000679         16  W-NOTE-CERT-SFX     PIC X.
000680         16  w-note-record-type  pic x.
000681     12  W-SAVE-KEY.
000682         16  W-SV-COMPANY-CD     PIC X.
000683         16  W-SV-CARRIER        PIC X.
000684         16  W-SV-GROUPING       PIC X(6).
000685         16  W-SV-STATE          PIC XX.
000686         16  W-SV-ACCOUNT        PIC X(10).
000687         16  W-SV-CERT-EFF-DT    PIC XX.
000688         16  W-SV-CERT-PRIME     PIC X(10).
000689         16  W-SV-CERT-SFX       PIC X.
000690     12  W-BILLING-NOTE.
000691         16  W-BN-NOTE           PIC X(25).
000692         16  W-BN-LTRID          PIC X(4).
000693         16  FILLER              PIC X(3).
000694         16  W-BN-DATE           PIC X(8).
000695         16  FILLER              PIC X(3).
000696         16  W-BN-USERID         PIC X(4).
000697         16  FILLER              PIC X(30).
000698     12  W-LEN                   PIC S9(5) COMP-3 VALUE +0.
000699     12  NOTE-SUB                PIC S9(5) COMP-3 VALUE +0.
000700     12  W-CERT-UPDATE-SW        PIC X  VALUE ' '.
000701         88  NO-CERT-RW                 VALUE 'N'.
000702         88  CERT-RW                    VALUE 'Y'.
000703
000704     12  W-ELENCC-KEY.
000705         16  W-ENCC-COMPANY-CD   PIC X.
000706         16  W-ENCC-REC-TYPE     PIC X.
000707         16  W-ENCC-ENC-CODE     PIC X(5).
000708         16  FILLER              PIC X(09).
000709                                 EJECT
000710 01  FILLER                      PIC  X(22)
000711                                 VALUE 'INTERFACE AREA STARTS:'.
000712*    COPY ELCINTF.
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
000713     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
000714*    COPY ELC1042.
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
000715*    COPY ELC689PI.
      *>>((file: ELC689PI))
000001******************************************************************
000002*                                                                *
000003*                            ELC689PI                            *
000004*                            VMOD=2.003                          *
000005*                                                                *
000006*    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
000007*    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
000008*    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
000009*                                                                *
000010*    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
000011*    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
000012*    BETWEEN PROGRAMS.                                           *
000013*                                                                *
000014*    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
000015*                                                                *
000016*               EL631 - EL689  - EL6891 - EL6892                 *
000017*                                                                *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
000027* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
000028* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000029******************************************************************
000030
000031
000032         16  PI-689-WORK-AREA.
000033             20  PI-689-ALT-PRINTER-ID
000034                                 PIC  X(04).
000035             20  PI-689-ARCHIVE-NUMBER
000036                                 PIC  9(08).
000037             20  PI-689-ARCHIVE-SW
000038                                 PIC  X(01).
000039                 88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
000040             20  PI-689-DATA-SOURCE
000041                                 PIC  X(01).
000042                 88  PI-689-SRC-ACCOUNT        VALUE '1'.
000043                 88  PI-689-SRC-CERTIFICATE    VALUE '2'.
000044                 88  PI-689-SRC-COMPENSATION   VALUE '3'.
000045                 88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
000046                 88  PI-689-SRC-CHECKS         VALUE '5'.
000047             20  PI-689-ERROR-IND
000048                                 PIC  X(01).
000049                 88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
000050             20  PI-689-ERROR    PIC  9(04).
000051                 88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
000052                 88  PI-689-FATAL-ERROR
000053                     VALUES 0004 0006 0008 0013 0023 0029 0033
000054                            0042 0047 0051 0066 0067 0070
000055                            0168 0169 0174 0175 0176 0177 0179
000056                            0180 0181 0182 0184 0185 0189 0190
000057                            0191
000058                            0215 0279 0280
000059                            0412 0413 0454
000060                            0533 0537
000061                            2055 2114 2208 2209 2216 2232 2369
000062                            2398 2433 2908 2999
000063                            3000 3770 3771 3775
000064                            7250 7365 7367 7368 7369 7370 7371
000065                            7272 7373 7374 7376 7377 7378 7379
000066                            7381 7388 7390 7393 7395 7396 7398
000067                            9095 9096 9281 9298 9299 9320 9327
000068                            9426 9427.
000069                 88  PI-689-STOP-ERROR
000070                     VALUES 0004 0008 0013 0023 0029 0033
000071                            0042 0047 0066 0067 0070
000072                            0168 0169 0174 0175 0176 0177
000073                            0181 0182 0184 0185 0189 0190
000074                            0279 0280
000075                            0412 0413 0454
000076                            2055 2208 2209 2216 2232
000077                            2398 2999
000078                            3000 3770 3771 3775
000079                            7250 7365 7369 7370 7371
000080                            7272 7373 7374 7376 7377 7378 7379
000081                            7381 7388 7390 7393 7396 7398
000082                            9095 9096 9299 9320 9426.
000083             20  PI-689-FOLLOW-UP-DATE
000084                                 PIC  X(02).
000085             20  PI-689-FORM-NUMBER
000086                                 PIC  X(04).
000087             20  PI-689-LABEL-SOURCE
000088                                 PIC X(01).
000089                 88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
000090                 88  PI-689-SOURCE-CARRIER  VALUE '2'.
000091                 88  PI-689-SOURCE-COMPANY  VALUE '3'.
000092                 88  PI-689-SOURCE-COMP     VALUE '4'.
000093                 88  PI-689-SOURCE-MAIL     VALUE '5'.
000094                 88  PI-689-SOURCE-CHECK    VALUE '6'.
000095                 88  PI-689-SOURCE-VARIABLE VALUE '7'.
000096             20  PI-689-NUMBER-COPIES
000097                                 PIC  9(01).
000098             20  PI-689-NUMBER-LABEL-LINES
000099                                 PIC  9(01).
000100             20  PI-689-NUMBER-TEXT-RECORDS
000101                                 PIC  9(03).
000102             20  PI-689-PRINT-ORDER-SW
000103                                 PIC  X(01).
000104                 88  PI-689-PRINT-FIRST     VALUE '1'.
000105                 88  PI-689-PRINT-SECOND    VALUE '2'.
000106                 88  PI-689-PRINT-LATER     VALUE '3'.
000107                 88  PI-689-PRINT-ONLY      VALUE '4'.
000108             20  PI-689-PRINT-RESTRICTION
000109                                 PIC  X(01).
000110                 88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
000111                 88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
000112                 88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
000113             20  PI-689-PRINT-SW PIC  X(01).
000114                 88  PI-689-PRINT-PERFORMED VALUE '1'.
000115             20  PI-689-RESEND-DATE-1
000116                                 PIC  X(02).
000117             20  PI-689-RESEND-LETR-1
000118                                 PIC X(4).
000119             20  PI-689-TEMP-STOR-ID
000120                                 PIC  X(08).
000121             20  PI-689-USE-SCREEN-IND
000122                                 PIC  X(01).
000123                 88  PI-689-CREATE-NO-SCREENS VALUE '1'.
000124             20  PI-689-ARCH-POINTER
000125                                 PIC S9(08) COMP.
000126                 88  PI-689-GET-ARCH-MAIN     VALUE +0.
000127             20  PI-689-ARCT-POINTER
000128                                 PIC S9(08) COMP.
000129                 88  PI-689-GET-ARCT-MAIN     VALUE +0.
000130             20  PI-689-VARIABLE-DATA-GRP.
000131                 24  PI-689-VARIABLE-DATA-1
000132                                 PIC  X(30).
000133                 24  PI-689-VARIABLE-DATA-2
000134                                 PIC  X(30).
000135                 24  PI-689-VARIABLE-DATA-3
000136                                 PIC  X(30).
000137                 24  PI-689-VARIABLE-DATA-4
000138                                 PIC  X(30).
000139
000140         16  PI-689-KEY-DATA-FIELDS.
000141             20  PI-689-ACCOUNT  PIC  X(10).
000142             20  PI-689-CARRIER  PIC  X(01).
000143             20  PI-689-CERT-NO.
000144                 24  PI-689-CERT-PRIME
000145                                 PIC  X(10).
000146                 24  PI-689-CERT-SFX
000147                                 PIC  X(01).
000148             20  PI-689-CHG-SEQ-NO
000149                                 PIC S9(04)    COMP.
000150             20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
000151                                 PIC  X(02).
000152             20  PI-689-ENTRY-BATCH
000153                                 PIC  X(06).
000154             20  PI-689-EFF-DATE PIC  X(02).
000155             20  PI-689-EXP-DATE PIC  X(02).
000156             20  PI-689-GROUPING PIC  X(06).
000157             20  PI-689-RESP-PERSON
000158                                 PIC  X(10).
000159             20  PI-689-SEQ-NO   PIC S9(08)    COMP.
000160             20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
000161                                 PIC  X(04).
000162             20  PI-689-STATE    PIC  X(02).
000163             20  PI-689-TYPE     PIC  X(01).
000164             20  PI-689-CONTROL  PIC S9(08)    COMP.
000165             20  PI-689-ALT-SEQ-NO
000166                                 PIC S9(04)    COMP.
000167         16  PI-689-DATE-EDIT    PIC  X(08).
000168         16  PI-689-FOLLOW-UP-EDIT
000169                                 PIC  X(08).
000170         16  PI-689-RESEND1-EDIT PIC  X(08).
000171         16  PI-689-SEQ-EDIT     PIC  X(08).
000172         16  PI-689-BCSEQ-EDIT   PIC  X(04).
000173         16  PI-689-LBL-OVERRIDE PIC  X(01).
000174             88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
000175         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
000176         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
      *<<((file: ELC689PI))
000716         16  PI-PROMPT-IND       PIC X(1).
000717         16  PI-CERT-FORM-ID     PIC X(5).
000718         16  PI-PRINT-NOW        PIC X(1).
000719         16  PI-ENCLOSURE-CD     PIC X(3).
000720         16  PI-CERT-REQ-IND     PIC X(1).
000721         16  PI-ENDT-ARCH-NO     PIC 9(8).
000722         16  PI-REASON-REQ-IND   PIC X(1).
000723         16  pi-iss-can-pend-rec pic x.
000724         16  FILLER              PIC X(243).
000725*        16  FILLER                        PIC X(276).
000726*        16  FILLER                        PIC X(280).
000727
000728 01  FILLER                      PIC  X(20)
000729                                 VALUE ':INTERFACE AREA ENDS'.
000730                                 EJECT
000731 01  FILLER                      PIC  X(16)
000732                        VALUE 'MAP AREA STARTS:'.
000733*    COPY EL689S.
      *>>((file: EL689S))
000001 01  EL689AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  RUNDTEL PIC S9(0004) COMP.
000005     05  RUNDTEF PIC  X(0001).
000006     05  FILLER REDEFINES RUNDTEF.
000007         10  RUNDTEA PIC  X(0001).
000008     05  RUNDTEI PIC  X(0008).
000009*    -------------------------------
000010     05  RUNTIMEL PIC S9(0004) COMP.
000011     05  RUNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES RUNTIMEF.
000013         10  RUNTIMEA PIC  X(0001).
000014     05  RUNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  HOSTL PIC S9(0004) COMP.
000017     05  HOSTF PIC  X(0001).
000018     05  FILLER REDEFINES HOSTF.
000019         10  HOSTA PIC  X(0001).
000020     05  HOSTI PIC  X(0010).
000021*    -------------------------------
000022     05  SYSL PIC S9(0004) COMP.
000023     05  SYSF PIC  X(0001).
000024     05  FILLER REDEFINES SYSF.
000025         10  SYSA PIC  X(0001).
000026     05  SYSI PIC  X(0008).
000027*    -------------------------------
000028     05  COMPANYL PIC S9(0004) COMP.
000029     05  COMPANYF PIC  X(0001).
000030     05  FILLER REDEFINES COMPANYF.
000031         10  COMPANYA PIC  X(0001).
000032     05  COMPANYI PIC  X(0003).
000033*    -------------------------------
000034     05  USERIDL PIC S9(0004) COMP.
000035     05  USERIDF PIC  X(0001).
000036     05  FILLER REDEFINES USERIDF.
000037         10  USERIDA PIC  X(0001).
000038     05  USERIDI PIC  X(0004).
000039*    -------------------------------
000040     05  MAINTL PIC S9(0004) COMP.
000041     05  MAINTF PIC  X(0001).
000042     05  FILLER REDEFINES MAINTF.
000043         10  MAINTA PIC  X(0001).
000044     05  MAINTI PIC  X(0001).
000045*    -------------------------------
000046     05  FORML PIC S9(0004) COMP.
000047     05  FORMF PIC  X(0001).
000048     05  FILLER REDEFINES FORMF.
000049         10  FORMA PIC  X(0001).
000050     05  FORMI PIC  X(0004).
000051*    -------------------------------
000052     05  ARCHNUML PIC S9(0004) COMP.
000053     05  ARCHNUMF PIC  X(0001).
000054     05  FILLER REDEFINES ARCHNUMF.
000055         10  ARCHNUMA PIC  X(0001).
000056     05  ARCHNUMI PIC  99999999.
000057*    -------------------------------
000058     05  PRTNOWL PIC S9(0004) COMP.
000059     05  PRTNOWF PIC  X(0001).
000060     05  FILLER REDEFINES PRTNOWF.
000061         10  PRTNOWA PIC  X(0001).
000062     05  PRTNOWI PIC  X(0001).
000063*    -------------------------------
000064     05  CARRIERL PIC S9(0004) COMP.
000065     05  CARRIERF PIC  X(0001).
000066     05  FILLER REDEFINES CARRIERF.
000067         10  CARRIERA PIC  X(0001).
000068     05  CARRIERI PIC  X(0001).
000069*    -------------------------------
000070     05  GROUPL PIC S9(0004) COMP.
000071     05  GROUPF PIC  X(0001).
000072     05  FILLER REDEFINES GROUPF.
000073         10  GROUPA PIC  X(0001).
000074     05  GROUPI PIC  X(0006).
000075*    -------------------------------
000076     05  STATEL PIC S9(0004) COMP.
000077     05  STATEF PIC  X(0001).
000078     05  FILLER REDEFINES STATEF.
000079         10  STATEA PIC  X(0001).
000080     05  STATEI PIC  X(0002).
000081*    -------------------------------
000082     05  ACCTL PIC S9(0004) COMP.
000083     05  ACCTF PIC  X(0001).
000084     05  FILLER REDEFINES ACCTF.
000085         10  ACCTA PIC  X(0001).
000086     05  ACCTI PIC  X(0010).
000087*    -------------------------------
000088     05  CERTL PIC S9(0004) COMP.
000089     05  CERTF PIC  X(0001).
000090     05  FILLER REDEFINES CERTF.
000091         10  CERTA PIC  X(0001).
000092     05  CERTI PIC  X(0010).
000093*    -------------------------------
000094     05  SFXL PIC S9(0004) COMP.
000095     05  SFXF PIC  X(0001).
000096     05  FILLER REDEFINES SFXF.
000097         10  SFXA PIC  X(0001).
000098     05  SFXI PIC  X(0001).
000099*    -------------------------------
000100     05  TYPEL PIC S9(0004) COMP.
000101     05  TYPEF PIC  X(0001).
000102     05  FILLER REDEFINES TYPEF.
000103         10  TYPEA PIC  X(0001).
000104     05  TYPEI PIC  X(0001).
000105*    -------------------------------
000106     05  DATEL PIC S9(0004) COMP.
000107     05  DATEF PIC  X(0001).
000108     05  FILLER REDEFINES DATEF.
000109         10  DATEA PIC  X(0001).
000110     05  DATEI PIC  X(0008).
000111*    -------------------------------
000112     05  FOLLOWL PIC S9(0004) COMP.
000113     05  FOLLOWF PIC  X(0001).
000114     05  FILLER REDEFINES FOLLOWF.
000115         10  FOLLOWA PIC  X(0001).
000116     05  FOLLOWI PIC  X(0008).
000117*    -------------------------------
000118     05  RESEND1L PIC S9(0004) COMP.
000119     05  RESEND1F PIC  X(0001).
000120     05  FILLER REDEFINES RESEND1F.
000121         10  RESEND1A PIC  X(0001).
000122     05  RESEND1I PIC  X(0008).
000123*    -------------------------------
000124     05  ENCL PIC S9(0004) COMP.
000125     05  ENCF PIC  X(0001).
000126     05  FILLER REDEFINES ENCF.
000127         10  ENCA PIC  X(0001).
000128     05  ENCI PIC  X(0003).
000129*    -------------------------------
000130     05  ENDARCHL PIC S9(0004) COMP.
000131     05  ENDARCHF PIC  X(0001).
000132     05  FILLER REDEFINES ENDARCHF.
000133         10  ENDARCHA PIC  X(0001).
000134     05  ENDARCHI PIC  99999999.
000135*    -------------------------------
000136     05  CERTIDL PIC S9(0004) COMP.
000137     05  CERTIDF PIC  X(0001).
000138     05  FILLER REDEFINES CERTIDF.
000139         10  CERTIDA PIC  X(0001).
000140     05  CERTIDI PIC  X(0005).
000141*    -------------------------------
000142     05  ADDRSL PIC S9(0004) COMP.
000143     05  ADDRSF PIC  X(0001).
000144     05  FILLER REDEFINES ADDRSF.
000145         10  ADDRSA PIC  X(0001).
000146     05  ADDRSI PIC  X(0001).
000147*    -------------------------------
000148     05  PRINTERL PIC S9(0004) COMP.
000149     05  PRINTERF PIC  X(0001).
000150     05  FILLER REDEFINES PRINTERF.
000151         10  PRINTERA PIC  X(0001).
000152     05  PRINTERI PIC  X(0004).
000153*    -------------------------------
000154     05  COPIESL PIC S9(0004) COMP.
000155     05  COPIESF PIC  X(0001).
000156     05  FILLER REDEFINES COPIESF.
000157         10  COPIESA PIC  X(0001).
000158     05  COPIESI PIC  X(0001).
000159*    -------------------------------
000160     05  DATASORL PIC S9(0004) COMP.
000161     05  DATASORF PIC  X(0001).
000162     05  FILLER REDEFINES DATASORF.
000163         10  DATASORA PIC  X(0001).
000164     05  DATASORI PIC  X(0001).
000165*    -------------------------------
000166     05  ADDRLBLL PIC S9(0004) COMP.
000167     05  ADDRLBLF PIC  X(0001).
000168     05  FILLER REDEFINES ADDRLBLF.
000169         10  ADDRLBLA PIC  X(0001).
000170     05  ADDRLBLI PIC  X(0001).
000171*    -------------------------------
000172     05  RPERSONL PIC S9(0004) COMP.
000173     05  RPERSONF PIC  X(0001).
000174     05  FILLER REDEFINES RPERSONF.
000175         10  RPERSONA PIC  X(0001).
000176     05  RPERSONI PIC  X(0010).
000177*    -------------------------------
000178     05  BENTRYL PIC S9(0004) COMP.
000179     05  BENTRYF PIC  X(0001).
000180     05  FILLER REDEFINES BENTRYF.
000181         10  BENTRYA PIC  X(0001).
000182     05  BENTRYI PIC  X(0006).
000183*    -------------------------------
000184     05  SEQL PIC S9(0004) COMP.
000185     05  SEQF PIC  X(0001).
000186     05  FILLER REDEFINES SEQF.
000187         10  SEQA PIC  X(0001).
000188     05  SEQI PIC  X(0008).
000189*    -------------------------------
000190     05  BCSEQL PIC S9(0004) COMP.
000191     05  BCSEQF PIC  X(0001).
000192     05  FILLER REDEFINES BCSEQF.
000193         10  BCSEQA PIC  X(0001).
000194     05  BCSEQI PIC  X(0004).
000195*    -------------------------------
000196     05  L1L PIC S9(0004) COMP.
000197     05  L1F PIC  X(0001).
000198     05  FILLER REDEFINES L1F.
000199         10  L1A PIC  X(0001).
000200     05  L1I PIC  X(0003).
000201*    -------------------------------
000202     05  TEXT1L PIC S9(0004) COMP.
000203     05  TEXT1F PIC  X(0001).
000204     05  FILLER REDEFINES TEXT1F.
000205         10  TEXT1A PIC  X(0001).
000206     05  TEXT1I PIC  X(0070).
000207*    -------------------------------
000208     05  L2L PIC S9(0004) COMP.
000209     05  L2F PIC  X(0001).
000210     05  FILLER REDEFINES L2F.
000211         10  L2A PIC  X(0001).
000212     05  L2I PIC  X(0003).
000213*    -------------------------------
000214     05  TEXT2L PIC S9(0004) COMP.
000215     05  TEXT2F PIC  X(0001).
000216     05  FILLER REDEFINES TEXT2F.
000217         10  TEXT2A PIC  X(0001).
000218     05  TEXT2I PIC  X(0070).
000219*    -------------------------------
000220     05  L3L PIC S9(0004) COMP.
000221     05  L3F PIC  X(0001).
000222     05  FILLER REDEFINES L3F.
000223         10  L3A PIC  X(0001).
000224     05  L3I PIC  X(0003).
000225*    -------------------------------
000226     05  TEXT3L PIC S9(0004) COMP.
000227     05  TEXT3F PIC  X(0001).
000228     05  FILLER REDEFINES TEXT3F.
000229         10  TEXT3A PIC  X(0001).
000230     05  TEXT3I PIC  X(0070).
000231*    -------------------------------
000232     05  L4L PIC S9(0004) COMP.
000233     05  L4F PIC  X(0001).
000234     05  FILLER REDEFINES L4F.
000235         10  L4A PIC  X(0001).
000236     05  L4I PIC  X(0003).
000237*    -------------------------------
000238     05  TEXT4L PIC S9(0004) COMP.
000239     05  TEXT4F PIC  X(0001).
000240     05  FILLER REDEFINES TEXT4F.
000241         10  TEXT4A PIC  X(0001).
000242     05  TEXT4I PIC  X(0070).
000243*    -------------------------------
000244     05  L5L PIC S9(0004) COMP.
000245     05  L5F PIC  X(0001).
000246     05  FILLER REDEFINES L5F.
000247         10  L5A PIC  X(0001).
000248     05  L5I PIC  X(0003).
000249*    -------------------------------
000250     05  TEXT5L PIC S9(0004) COMP.
000251     05  TEXT5F PIC  X(0001).
000252     05  FILLER REDEFINES TEXT5F.
000253         10  TEXT5A PIC  X(0001).
000254     05  TEXT5I PIC  X(0070).
000255*    -------------------------------
000256     05  L6L PIC S9(0004) COMP.
000257     05  L6F PIC  X(0001).
000258     05  FILLER REDEFINES L6F.
000259         10  L6A PIC  X(0001).
000260     05  L6I PIC  X(0003).
000261*    -------------------------------
000262     05  TEXT6L PIC S9(0004) COMP.
000263     05  TEXT6F PIC  X(0001).
000264     05  FILLER REDEFINES TEXT6F.
000265         10  TEXT6A PIC  X(0001).
000266     05  TEXT6I PIC  X(0070).
000267*    -------------------------------
000268     05  L7L PIC S9(0004) COMP.
000269     05  L7F PIC  X(0001).
000270     05  FILLER REDEFINES L7F.
000271         10  L7A PIC  X(0001).
000272     05  L7I PIC  X(0003).
000273*    -------------------------------
000274     05  TEXT7L PIC S9(0004) COMP.
000275     05  TEXT7F PIC  X(0001).
000276     05  FILLER REDEFINES TEXT7F.
000277         10  TEXT7A PIC  X(0001).
000278     05  TEXT7I PIC  X(0070).
000279*    -------------------------------
000280     05  L8L PIC S9(0004) COMP.
000281     05  L8F PIC  X(0001).
000282     05  FILLER REDEFINES L8F.
000283         10  L8A PIC  X(0001).
000284     05  L8I PIC  X(0003).
000285*    -------------------------------
000286     05  TEXT8L PIC S9(0004) COMP.
000287     05  TEXT8F PIC  X(0001).
000288     05  FILLER REDEFINES TEXT8F.
000289         10  TEXT8A PIC  X(0001).
000290     05  TEXT8I PIC  X(0070).
000291*    -------------------------------
000292     05  L9L PIC S9(0004) COMP.
000293     05  L9F PIC  X(0001).
000294     05  FILLER REDEFINES L9F.
000295         10  L9A PIC  X(0001).
000296     05  L9I PIC  X(0003).
000297*    -------------------------------
000298     05  TEXT9L PIC S9(0004) COMP.
000299     05  TEXT9F PIC  X(0001).
000300     05  FILLER REDEFINES TEXT9F.
000301         10  TEXT9A PIC  X(0001).
000302     05  TEXT9I PIC  X(0070).
000303*    -------------------------------
000304     05  L10L PIC S9(0004) COMP.
000305     05  L10F PIC  X(0001).
000306     05  FILLER REDEFINES L10F.
000307         10  L10A PIC  X(0001).
000308     05  L10I PIC  X(0003).
000309*    -------------------------------
000310     05  TEXT10L PIC S9(0004) COMP.
000311     05  TEXT10F PIC  X(0001).
000312     05  FILLER REDEFINES TEXT10F.
000313         10  TEXT10A PIC  X(0001).
000314     05  TEXT10I PIC  X(0070).
000315*    -------------------------------
000316     05  L11L PIC S9(0004) COMP.
000317     05  L11F PIC  X(0001).
000318     05  FILLER REDEFINES L11F.
000319         10  L11A PIC  X(0001).
000320     05  L11I PIC  X(0003).
000321*    -------------------------------
000322     05  TEXT11L PIC S9(0004) COMP.
000323     05  TEXT11F PIC  X(0001).
000324     05  FILLER REDEFINES TEXT11F.
000325         10  TEXT11A PIC  X(0001).
000326     05  TEXT11I PIC  X(0070).
000327*    -------------------------------
000328     05  L12L PIC S9(0004) COMP.
000329     05  L12F PIC  X(0001).
000330     05  FILLER REDEFINES L12F.
000331         10  L12A PIC  X(0001).
000332     05  L12I PIC  X(0003).
000333*    -------------------------------
000334     05  TEXT12L PIC S9(0004) COMP.
000335     05  TEXT12F PIC  X(0001).
000336     05  FILLER REDEFINES TEXT12F.
000337         10  TEXT12A PIC  X(0001).
000338     05  TEXT12I PIC  X(0070).
000339*    -------------------------------
000340     05  ERRMSGL PIC S9(0004) COMP.
000341     05  ERRMSGF PIC  X(0001).
000342     05  FILLER REDEFINES ERRMSGF.
000343         10  ERRMSGA PIC  X(0001).
000344     05  ERRMSGI PIC  X(0079).
000345*    -------------------------------
000346     05  ENTERPFL PIC S9(0004) COMP.
000347     05  ENTERPFF PIC  X(0001).
000348     05  FILLER REDEFINES ENTERPFF.
000349         10  ENTERPFA PIC  X(0001).
000350     05  ENTERPFI PIC  99.
000351*    -------------------------------
000352     05  PF8HDL PIC S9(0004) COMP.
000353     05  PF8HDF PIC  X(0001).
000354     05  FILLER REDEFINES PF8HDF.
000355         10  PF8HDA PIC  X(0001).
000356     05  PF8HDI PIC  X(0022).
000357 01  EL689AO REDEFINES EL689AI.
000358     05  FILLER            PIC  X(0012).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  RUNDTEO PIC  X(0008).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  RUNTIMEO PIC  99.99.
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  HOSTO PIC  X(0010).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  SYSO PIC  X(0008).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  COMPANYO PIC  X(0003).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  USERIDO PIC  X(0004).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  MAINTO PIC  X(0001).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  FORMO PIC  X(0004).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  ARCHNUMO PIC  99999999.
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  PRTNOWO PIC  X(0001).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  CARRIERO PIC  X(0001).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  GROUPO PIC  X(0006).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  STATEO PIC  X(0002).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  ACCTO PIC  X(0010).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  CERTO PIC  X(0010).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  SFXO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  TYPEO PIC  X(0001).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  DATEO PIC  X(0008).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  FOLLOWO PIC  X(0008).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  RESEND1O PIC  X(0008).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  ENCO PIC  X(0003).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  ENDARCHO PIC  99999999.
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  CERTIDO PIC  X(0005).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  ADDRSO PIC  X(0001).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  PRINTERO PIC  X(0004).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  COPIESO PIC  X(0001).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  DATASORO PIC  X(0001).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  ADDRLBLO PIC  X(0001).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  RPERSONO PIC  X(0010).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  BENTRYO PIC  X(0006).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  SEQO PIC  X(0008).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  BCSEQO PIC  X(0004).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  L1O PIC  X(0003).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  TEXT1O PIC  X(0070).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  L2O PIC  X(0003).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  TEXT2O PIC  X(0070).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  L3O PIC  X(0003).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  TEXT3O PIC  X(0070).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  L4O PIC  X(0003).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  TEXT4O PIC  X(0070).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  L5O PIC  X(0003).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  TEXT5O PIC  X(0070).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  L6O PIC  X(0003).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  TEXT6O PIC  X(0070).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  L7O PIC  X(0003).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  TEXT7O PIC  X(0070).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  L8O PIC  X(0003).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  TEXT8O PIC  X(0070).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  L9O PIC  X(0003).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  TEXT9O PIC  X(0070).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  L10O PIC  X(0003).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  TEXT10O PIC  X(0070).
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  L11O PIC  X(0003).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  TEXT11O PIC  X(0070).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  L12O PIC  X(0003).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  TEXT12O PIC  X(0070).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  ERRMSGO PIC  X(0079).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  ENTERPFO PIC  X(0002).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  PF8HDO PIC  X(0022).
000536*    -------------------------------
      *<<((file: EL689S))
000734 01  W-MAP-REDEF REDEFINES EL689AI.
000735*    12  FILLER                  PIC X(218).
000736     12  FILLER                  PIC X(267).
000737     12  EL689RI.
000738         16  W-TEXT-LINES OCCURS 12 TIMES INDEXED BY W-SC-NDX.
000739             20  W-SC-LINEL      PIC S9(04) COMP.
000740             20  W-SC-LINEA      PIC  X(01).
000741             20  W-SC-LINE       PIC  X(03).
000742             20  W-SC-TEXTL      PIC S9(04) COMP.
000743             20  W-SC-TEXTA      PIC  X(01).
000744             20  W-SC-TEXT       PIC  X(70).
000745*    12  FILLER                  PIC  X(87).
000746     12  FILLER                  PIC  X(112).
000747                                 EJECT
000748 01  W-CONSTANT-AREA.
000749     12  FILLER                  PIC  X(18)
000750                                 VALUE 'PROGRAM CONSTANTS:'.
000751     12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
000752     12  W-ARCH-LENGTH           PIC S9(04)  COMP  VALUE +250.
000753     12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
000754     12  W-ENDT-LENGTH           PIC S9(04)  COMP  VALUE +579.
000755     12  W-NOTE-LENGTH           PIC S9(04)  COMP  VALUE +825.
000756     12  W-EOBC-LENGTH           PIC S9(04)  COMP  VALUE +350.
000757     12  W-ENCC-LENGTH           PIC S9(04)  COMP  VALUE +400.
000758     12  W-MAX-LINES             PIC S9(03) VALUE +300 COMP-3.
000759     12  W-NUM-LINES-PER-SCREEN  PIC  9(02)        VALUE 12.
000760     12  W-TS-NUM-REC-IN-GROUP   PIC  9(02)        VALUE 50.
000761     12  W-TS-LENGTH             PIC S9(04)  COMP  VALUE +3650.
000762     12  W-TS-MAP-LENGTH         PIC S9(04)  COMP  VALUE +1260.
000763     12  W-ZEROS                 PIC S9(03) VALUE +000 COMP-3.
000764
000765     12  W-ACCT-FILE-ID          PIC  X(08) VALUE 'ERACCT'.
000766     12  W-ACCT2-FILE-ID         PIC  X(08) VALUE 'ERACCT2'.
000767     12  W-ARCH-FILE-ID          PIC  X(08) VALUE 'ERARCH'.
000768     12  W-ARCH2-FILE-ID         PIC  X(08) VALUE 'ERARCH2'.
000769     12  W-ARCH3-FILE-ID         PIC  X(08) VALUE 'ERARCH3'.
000770     12  W-ARCH4-FILE-ID         PIC  X(08) VALUE 'ERARCH4'.
000771     12  W-ARCH5-FILE-ID         PIC  X(08) VALUE 'ERARCH5'.
000772     12  W-ARCT-FILE-ID          PIC  X(08) VALUE 'ERARCT'.
000773     12  W-CERT-FILE-ID          PIC  X(08) VALUE 'ELCERT'.
000774     12  W-CHEK-FILE-ID          PIC  X(08) VALUE 'ERCHEK'.
000775     12  W-CNTL-FILE-ID          PIC  X(08) VALUE 'ELCNTL'.
000776     12  W-COMP-FILE-ID          PIC  X(08) VALUE 'ERCOMP'.
000777     12  W-NOTE-FILE-ID          PIC  X(08) VALUE 'ERNOTE'.
000778     12  W-EOBC-FILE-ID          PIC  X(08) VALUE 'ELEOBC'.
000779     12  W-ENCC-FILE-ID          PIC  X(08) VALUE 'ELENCC'.
000780     12  W-GETMAIN-SPACE         PIC  X(01) VALUE SPACE.
000781     12  W-LGXX-ID               PIC  X(04) VALUE 'LGXX'.
000782     12  W-LINK-001              PIC  X(05) VALUE 'EL001'.
000783     12  W-LINK-004              PIC  X(05) VALUE 'EL004'.
000784     12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.
000785     12  W-LOWER-CASE            PIC  X(26)
000786         VALUE 'abcdefghijklmnopqrstuvwxyz'.
000787     12  W-MAIL-FILE-ID          PIC  X(08) VALUE 'ERMAIL'.
000788     12  W-MAP.
000789         16  W-MAP-PREFIX        PIC  X(02) VALUE 'EL'.
000790         16  W-MAP-NUM           PIC  X(04) VALUE '689A'.
000791         16  W-MAP-FILLER        PIC  X(02) VALUE SPACES.
000792     12  W-MAPSET                PIC  X(08) VALUE 'EL689S'.
000793     12  W-PGM-EL1042            PIC  X(08) VALUE 'EL1042'.
000794     12  W-PGM-EL690             PIC  X(08) VALUE 'EL690'.
000795     12  W-PGM-EL626             PIC  X(08) VALUE 'EL626'.
000796     12  W-PNDB-FILE-ID          PIC  X(08) VALUE 'ERPNDB'.
000797     12  W-PRINT-TRANS           PIC  X(04) VALUE 'EXH5'.
000798     12  W-PYAJ-FILE-ID          PIC  X(08) VALUE 'ERPYAJ'.
000799
000800     12  W-TEXT-FILE-ID          PIC  X(08) VALUE 'ELLETR'.
000801     12  W-THIS-PGM              PIC  X(08) VALUE 'EL689'.
000802     12  W-TOP-FORM              PIC  X(70)
000803                              VALUE '*****TOP OF FORM *****'.
000804     12  W-TRANSACTION           PIC  X(04) VALUE 'EXH3'.
000805     12  W-UPPER-CASE            PIC  X(26)
000806         VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
000807     12  W-XCTL-005              PIC  X(05) VALUE 'EL005'.
000808     12  W-XCTL-010              PIC  X(05) VALUE 'EL010'.
000809     12  W-XCTL-626              PIC  X(05) VALUE 'EL626'.
000810                                 EJECT
000811 01  W-VARIABLE-PROCESS-CNTLS.
000812     12  FILLER                  PIC  X(26)
000813                        VALUE 'VARIABLE WORK AREA STARTS:'.
000814     12  W-NUM-OF-VARIABLES      PIC S9(03) VALUE +189 COMP-3.
000815     12  W-VAR-HOLD.
000816         16  W-V1                PIC  X(01).
000817         16  W-V2                PIC  X(01).
000818         16  W-V3                PIC  X(01).
000819     12  FILLER REDEFINES W-VAR-HOLD.
000820         16  W-VAR-RELATIVE-NUM  PIC  9(03).
000821     12  W-FIELD-SQUEEZE-IND     PIC  X(01).
000822         88  W-SQUEEZE-FIELD          VALUE '#'.
000823
000824 01  W-SUPPORTED-VARIABLES.
000825
000826*****************COMPANY VARIABLES - ELCNTL ********************
000827*****COMPANY NAME
000828     12  FILLER                  PIC  X(03) VALUE '001'.
000829     12  FILLER                  PIC S9(04) COMP VALUE +30.
000830     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000831     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000832     12  FILLER                  PIC S9(04) COMP VALUE +01.
000833
000834*****FULL COMPANY ADDRESS
000835     12  FILLER                  PIC  X(03) VALUE '002'.
000836     12  FILLER                  PIC S9(04) COMP VALUE +30.
000837     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000838     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000839     12  FILLER                  PIC S9(04) COMP VALUE +01.
000840
000841     12  FILLER                  PIC  X(03) VALUE '003'.
000842     12  FILLER                  PIC S9(04) COMP VALUE +30.
000843     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000844     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000845     12  FILLER                  PIC S9(04) COMP VALUE +01.
000846
000847     12  FILLER                  PIC  X(03) VALUE '004'.
000848     12  FILLER                  PIC S9(04) COMP VALUE +30.
000849     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000850     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000851     12  FILLER                  PIC S9(04) COMP VALUE +01.
000852
000853     12  FILLER                  PIC  X(03) VALUE '005'.
000854     12  FILLER                  PIC S9(04) COMP VALUE +30.
000855     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000856     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000857     12  FILLER                  PIC S9(04) COMP VALUE +01.
000858
000859     12  FILLER                  PIC  X(03) VALUE '006'.
000860     12  FILLER                  PIC S9(04) COMP VALUE +30.
000861     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000862     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000863     12  FILLER                  PIC S9(04) COMP VALUE +01.
000864
000865*****REMAINING 4 ARE NOT CURRENTLY USED
000866     12  FILLER                  PIC  X(03) VALUE '007'.
000867     12  FILLER                  PIC S9(04) COMP VALUE +30.
000868     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000869     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000870     12  FILLER                  PIC S9(04) COMP VALUE +01.
000871
000872     12  FILLER                  PIC  X(03) VALUE '008'.
000873     12  FILLER                  PIC S9(04) COMP VALUE +30.
000874     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000875     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000876     12  FILLER                  PIC S9(04) COMP VALUE +01.
000877
000878     12  FILLER                  PIC  X(03) VALUE '009'.
000879     12  FILLER                  PIC S9(04) COMP VALUE +30.
000880     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000881     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000882     12  FILLER                  PIC S9(04) COMP VALUE +01.
000883
000884     12  FILLER                  PIC  X(03) VALUE '010'.
000885     12  FILLER                  PIC S9(04) COMP VALUE +30.
000886     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000887     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000888     12  FILLER                  PIC S9(04) COMP VALUE +01.
000889
000890************ LIFE BENEFIT VARIABLES - ELCNTL *******************
000891*****LIFE BENEFIT DESCRIPTION
000892     12  FILLER                  PIC  X(03) VALUE '011'.
000893     12  FILLER                  PIC S9(04) COMP VALUE +10.
000894     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000895     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000896     12  FILLER                  PIC S9(04) COMP VALUE +02.
000897
000898*****REMAINING 3 ARE NOT CURRENTLY USED
000899     12  FILLER                  PIC  X(03) VALUE '012'.
000900     12  FILLER                  PIC S9(04) COMP VALUE +30.
000901     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000902     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000903     12  FILLER                  PIC S9(04) COMP VALUE +02.
000904
000905     12  FILLER                  PIC  X(03) VALUE '013'.
000906     12  FILLER                  PIC S9(04) COMP VALUE +30.
000907     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000909     12  FILLER                  PIC S9(04) COMP VALUE +02.
000910
000911     12  FILLER                  PIC  X(03) VALUE '014'.
000912     12  FILLER                  PIC S9(04) COMP VALUE +30.
000913     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000914     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000915     12  FILLER                  PIC S9(04) COMP VALUE +02.
000916
000917************* A&H BENEFIT VARIABLES - ELCNTL *******************
000918*****AH BENEFIT DESCRIPTION
000919     12  FILLER                  PIC  X(03) VALUE '015'.
000920     12  FILLER                  PIC S9(04) COMP VALUE +10.
000921     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000922     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000923     12  FILLER                  PIC S9(04) COMP VALUE +03.
000924
000925*****ELIMINATION PERIOD
000926     12  FILLER                  PIC  X(03) VALUE '016'.
000927     12  FILLER                  PIC S9(04) COMP VALUE +2.
000928     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000929     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000930     12  FILLER                  PIC S9(04) COMP VALUE +03.
000931
000932*****REMAINING 3 ARE NOT CURRENTLY USED
000933     12  FILLER                  PIC  X(03) VALUE '017'.
000934     12  FILLER                  PIC S9(04) COMP VALUE +30.
000935     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000936     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000937     12  FILLER                  PIC S9(04) COMP VALUE +03.
000938
000939     12  FILLER                  PIC  X(03) VALUE '018'.
000940     12  FILLER                  PIC S9(04) COMP VALUE +30.
000941     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000942     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000943     12  FILLER                  PIC S9(04) COMP VALUE +03.
000944
000945     12  FILLER                  PIC  X(03) VALUE '019'.
000946     12  FILLER                  PIC S9(04) COMP VALUE +30.
000947     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000948     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000949     12  FILLER                  PIC S9(04) COMP VALUE +03.
000950
000951*****************CARRIER VARIABLES - ELCNTL ********************
000952*****CARRIER NAME
000953     12  FILLER                  PIC  X(03) VALUE '020'.
000954     12  FILLER                  PIC S9(04) COMP VALUE +30.
000955     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000956     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000957     12  FILLER                  PIC S9(04) COMP VALUE +04.
000958
000959*****FULL CARRIER ADDRESS
000960     12  FILLER                  PIC  X(03) VALUE '021'.
000961     12  FILLER                  PIC S9(04) COMP VALUE +30.
000962     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000963     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000964     12  FILLER                  PIC S9(04) COMP VALUE +04.
000965
000966     12  FILLER                  PIC  X(03) VALUE '022'.
000967     12  FILLER                  PIC S9(04) COMP VALUE +30.
000968     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000969     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000970     12  FILLER                  PIC S9(04) COMP VALUE +04.
000971
000972     12  FILLER                  PIC  X(03) VALUE '023'.
000973     12  FILLER                  PIC S9(04) COMP VALUE +30.
000974     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000975     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000976     12  FILLER                  PIC S9(04) COMP VALUE +04.
000977
000978     12  FILLER                  PIC  X(03) VALUE '024'.
000979     12  FILLER                  PIC S9(04) COMP VALUE +30.
000980     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000981     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000982     12  FILLER                  PIC S9(04) COMP VALUE +04.
000983
000984     12  FILLER                  PIC  X(03) VALUE '025'.
000985     12  FILLER                  PIC S9(04) COMP VALUE +30.
000986     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000987     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
000988     12  FILLER                  PIC S9(04) COMP VALUE +04.
000989
000990*****CARRIER PHONE NUMBER
000991     12  FILLER                  PIC  X(03) VALUE '026'.
000992     12  FILLER                  PIC S9(04) COMP VALUE +12.
000993     12  FILLER                  PIC  X(30) VALUE ALL '*'.
000994     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
000995     12  FILLER                  PIC S9(04) COMP VALUE +04.
000996
000997*****REMAINING 4 ARE NOT CURRENTLY USED
000998     12  FILLER                  PIC  X(03) VALUE '027'.
000999     12  FILLER                  PIC S9(04) COMP VALUE +30.
001000     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001001     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001002     12  FILLER                  PIC S9(04) COMP VALUE +04.
001003
001004     12  FILLER                  PIC  X(03) VALUE '028'.
001005     12  FILLER                  PIC S9(04) COMP VALUE +30.
001006     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001007     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001008     12  FILLER                  PIC S9(04) COMP VALUE +04.
001009
001010     12  FILLER                  PIC  X(03) VALUE '029'.
001011     12  FILLER                  PIC S9(04) COMP VALUE +30.
001012     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001013     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001014     12  FILLER                  PIC S9(04) COMP VALUE +04.
001015
001016     12  FILLER                  PIC  X(03) VALUE '030'.
001017     12  FILLER                  PIC S9(04) COMP VALUE +30.
001018     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001019     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001020     12  FILLER                  PIC S9(04) COMP VALUE +04.
001021
001022***************** MAIL VARIABLES - ELMAIL **********************
001023*****FULL MAIL ADDRESS
001024     12  FILLER                  PIC  X(03) VALUE '031'.
001025     12  FILLER                  PIC S9(04) COMP VALUE +30.
001026     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001027     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001028     12  FILLER                  PIC S9(04) COMP VALUE +05.
001029
001030     12  FILLER                  PIC  X(03) VALUE '032'.
001031     12  FILLER                  PIC S9(04) COMP VALUE +30.
001032     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001033     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001034     12  FILLER                  PIC S9(04) COMP VALUE +05.
001035
001036     12  FILLER                  PIC  X(03) VALUE '033'.
001037     12  FILLER                  PIC S9(04) COMP VALUE +30.
001038     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001039     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001040     12  FILLER                  PIC S9(04) COMP VALUE +05.
001041
001042     12  FILLER                  PIC  X(03) VALUE '034'.
001043     12  FILLER                  PIC S9(04) COMP VALUE +30.
001044     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001045     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001046     12  FILLER                  PIC S9(04) COMP VALUE +05.
001047
001048     12  FILLER                  PIC  X(03) VALUE '035'.
001049     12  FILLER                  PIC S9(04) COMP VALUE +30.
001050     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001051     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001052     12  FILLER                  PIC S9(04) COMP VALUE +05.
001053
001054     12  FILLER                  PIC  X(03) VALUE '036'.
001055     12  FILLER                  PIC S9(04) COMP VALUE +30.
001056     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001057     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001058     12  FILLER                  PIC S9(04) COMP VALUE +05.
001059**** CRED BENE NAME FROM ERMAIL
001060     12  FILLER                  PIC  X(03) VALUE '037'.
001061     12  FILLER                  PIC S9(04) COMP VALUE +25.
001062     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001063     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001064     12  FILLER                  PIC S9(04) COMP VALUE +05.
001065
001066*****REMAINING 2 ARE NOT CURRENTLY USED
001067     12  FILLER                  PIC  X(03) VALUE '038'.
001068     12  FILLER                  PIC S9(04) COMP VALUE +30.
001069     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001070     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001071     12  FILLER                  PIC S9(04) COMP VALUE +05.
001072
001073     12  FILLER                  PIC  X(03) VALUE '039'.
001074     12  FILLER                  PIC S9(04) COMP VALUE +30.
001075     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001076     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001077     12  FILLER                  PIC S9(04) COMP VALUE +05.
001078
001079*************** ACCOUNT VARIABLES - ERACCT *********************
001080*****ACCOUNT NAME
001081     12  FILLER                  PIC  X(03) VALUE '040'.
001082     12  FILLER                  PIC S9(04) COMP VALUE +30.
001083     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001084     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001085     12  FILLER                  PIC S9(04) COMP VALUE +06.
001086
001087*****FULL ACCOUNT ADDRESS
001088     12  FILLER                  PIC  X(03) VALUE '041'.
001089     12  FILLER                  PIC S9(04) COMP VALUE +30.
001090     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001091     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001092     12  FILLER                  PIC S9(04) COMP VALUE +06.
001093
001094     12  FILLER                  PIC  X(03) VALUE '042'.
001095     12  FILLER                  PIC S9(04) COMP VALUE +30.
001096     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001097     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001098     12  FILLER                  PIC S9(04) COMP VALUE +06.
001099
001100     12  FILLER                  PIC  X(03) VALUE '043'.
001101     12  FILLER                  PIC S9(04) COMP VALUE +30.
001102     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001103     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001104     12  FILLER                  PIC S9(04) COMP VALUE +06.
001105
001106     12  FILLER                  PIC  X(03) VALUE '044'.
001107     12  FILLER                  PIC S9(04) COMP VALUE +30.
001108     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001109     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001110     12  FILLER                  PIC S9(04) COMP VALUE +06.
001111
001112     12  FILLER                  PIC  X(03) VALUE '045'.
001113     12  FILLER                  PIC S9(04) COMP VALUE +30.
001114     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001115     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001116     12  FILLER                  PIC S9(04) COMP VALUE +06.
001117
001118*****ACCOUNT PHONE NUMBER
001119     12  FILLER                  PIC  X(03) VALUE '046'.
001120     12  FILLER                  PIC S9(04) COMP VALUE +12.
001121     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001122     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001123     12  FILLER                  PIC S9(04) COMP VALUE +06.
001124
001125*****ACCOUNT CONTROL NAME AM-CONTROL-NAME
001126     12  FILLER                  PIC  X(03) VALUE '047'.
001127     12  FILLER                  PIC S9(04) COMP VALUE +30.
001128     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001129     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001130     12  FILLER                  PIC S9(04) COMP VALUE +06.
001131
001132****ACCOUNT CSR CODE
001133     12  FILLER                  PIC  X(03) VALUE '048'.
001134     12  FILLER                  PIC S9(04) COMP VALUE +4..
001135     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001136     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001137     12  FILLER                  PIC S9(04) COMP VALUE +06.
001138
001139****ACCOUNT ACCOUNT NUMBER
001140     12  FILLER                  PIC  X(03) VALUE '049'.
001141     12  FILLER                  PIC S9(04) COMP VALUE +10.
001142     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001143     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001144     12  FILLER                  PIC S9(04) COMP VALUE +06.
001145
001146**** CRED BENE ADDRESS LINE 1 ERMAIL
001147     12  FILLER                  PIC  X(03) VALUE '050'.
001148     12  FILLER                  PIC S9(04) COMP VALUE +30.
001149     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001150     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001151     12  FILLER                  PIC S9(04) COMP VALUE +05.
001152
001153**** CRED BENE ADDRESS LINE 2 ERMAIL
001154     12  FILLER                  PIC  X(03) VALUE '051'.
001155     12  FILLER                  PIC S9(04) COMP VALUE +30.
001156     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001157     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001158     12  FILLER                  PIC S9(04) COMP VALUE +05.
001159
001160**** CRED BENE CITY STATE   ERMAIL
001161     12  FILLER                  PIC  X(03) VALUE '052'.
001162     12  FILLER                  PIC S9(04) COMP VALUE +30.
001163     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001164     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001165     12  FILLER                  PIC S9(04) COMP VALUE +05.
001166
001167**** CRED BENE ZIP  1 ERMAIL
001168     12  FILLER                  PIC  X(03) VALUE '053'.
001169     12  FILLER                  PIC S9(04) COMP VALUE +30.
001170     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001171     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001172     12  FILLER                  PIC S9(04) COMP VALUE +05.
001173
001174*****REMAINING 6 ARE NOT CURRENTLY USED
001175     12  FILLER                  PIC  X(03) VALUE '054'.
001176     12  FILLER                  PIC S9(04) COMP VALUE +30.
001177     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001178     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001179     12  FILLER                  PIC S9(04) COMP VALUE +06.
001180
001181     12  FILLER                  PIC  X(03) VALUE '055'.
001182     12  FILLER                  PIC S9(04) COMP VALUE +30.
001183     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001184     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001185     12  FILLER                  PIC S9(04) COMP VALUE +06.
001186
001187     12  FILLER                  PIC  X(03) VALUE '056'.
001188     12  FILLER                  PIC S9(04) COMP VALUE +30.
001189     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001190     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001191     12  FILLER                  PIC S9(04) COMP VALUE +06.
001192
001193     12  FILLER                  PIC  X(03) VALUE '057'.
001194     12  FILLER                  PIC S9(04) COMP VALUE +30.
001195     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001196     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001197     12  FILLER                  PIC S9(04) COMP VALUE +06.
001198
001199     12  FILLER                  PIC  X(03) VALUE '058'.
001200     12  FILLER                  PIC S9(04) COMP VALUE +30.
001201     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001202     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001203     12  FILLER                  PIC S9(04) COMP VALUE +06.
001204
001205     12  FILLER                  PIC  X(03) VALUE '059'.
001206     12  FILLER                  PIC S9(04) COMP VALUE +30.
001207     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001208     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001209     12  FILLER                  PIC S9(04) COMP VALUE +06.
001210
001211*************** NON FILE VARIABLES *****************************
001212*****CURRENT DATE
001213     12  FILLER                  PIC  X(03) VALUE '060'.
001214     12  FILLER                  PIC S9(04) COMP VALUE +08.
001215     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001216     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001217     12  FILLER                  PIC S9(04) COMP VALUE +07.
001218
001219*****FULL CURRENT DATE
001220     12  FILLER                  PIC  X(03) VALUE '061'.
001221     12  FILLER                  PIC S9(04) COMP VALUE +18.
001222     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001223     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001224     12  FILLER                  PIC S9(04) COMP VALUE +07.
001225
001226*****FORM
001227     12  FILLER                  PIC  X(03) VALUE '062'.
001228     12  FILLER                  PIC S9(04) COMP VALUE +04.
001229     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001230     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001231     12  FILLER                  PIC S9(04) COMP VALUE +07.
001232
001233*****VARIABLE 1
001234     12  FILLER                  PIC  X(03) VALUE '063'.
001235     12  FILLER                  PIC S9(04) COMP VALUE +30.
001236     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001237     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001238     12  FILLER                  PIC S9(04) COMP VALUE +07.
001239
001240*****VARIABLE 2
001241     12  FILLER                  PIC  X(03) VALUE '064'.
001242     12  FILLER                  PIC S9(04) COMP VALUE +30.
001243     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001244     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001245     12  FILLER                  PIC S9(04) COMP VALUE +07.
001246
001247*****VARIABLE 3
001248     12  FILLER                  PIC  X(03) VALUE '065'.
001249     12  FILLER                  PIC S9(04) COMP VALUE +30.
001250     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001251     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001252     12  FILLER                  PIC S9(04) COMP VALUE +07.
001253
001254*****VARIABLE 4
001255     12  FILLER                  PIC  X(03) VALUE '066'.
001256     12  FILLER                  PIC S9(04) COMP VALUE +30.
001257     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001258     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001259     12  FILLER                  PIC S9(04) COMP VALUE +07.
001260
001261*****REMAINING 3 ARE NOT CURRENTLY USED
001262     12  FILLER                  PIC  X(03) VALUE '067'.
001263     12  FILLER                  PIC S9(04) COMP VALUE +30.
001264     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001265     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001266     12  FILLER                  PIC S9(04) COMP VALUE +07.
001267
001268     12  FILLER                  PIC  X(03) VALUE '068'.
001269     12  FILLER                  PIC S9(04) COMP VALUE +30.
001270     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001271     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001272     12  FILLER                  PIC S9(04) COMP VALUE +07.
001273
001274     12  FILLER                  PIC  X(03) VALUE '069'.
001275     12  FILLER                  PIC S9(04) COMP VALUE +30.
001276     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001277     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001278     12  FILLER                  PIC S9(04) COMP VALUE +07.
001279
001280************** CERTIFICATE VARIABLES - ELCERT *****************
001281*****CARRIER CODE IN CERT
001282     12  FILLER                  PIC  X(03) VALUE '070'.
001283     12  FILLER                  PIC S9(04) COMP VALUE +1.
001284     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001285     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001286     12  FILLER                  PIC S9(04) COMP VALUE +08.
001287
001288*****GROUPING CODE IN CERT
001289     12  FILLER                  PIC  X(03) VALUE '071'.
001290     12  FILLER                  PIC S9(04) COMP VALUE +06.
001291     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001292     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001293     12  FILLER                  PIC S9(04) COMP VALUE +08.
001294
001295*****ACCOUNT NUMBER IN CERT
001296     12  FILLER                  PIC  X(03) VALUE '072'.
001297     12  FILLER                  PIC S9(04) COMP VALUE +10.
001298     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001299     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001300     12  FILLER                  PIC S9(04) COMP VALUE +08.
001301
001302*****CERTIFICATE NUMBER
001303     12  FILLER                  PIC  X(03) VALUE '073'.
001304     12  FILLER                  PIC S9(04) COMP VALUE +11.
001305     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001306     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001307     12  FILLER                  PIC S9(04) COMP VALUE +08.
001308
001309*****CERT EFFECTIVE DATE
001310     12  FILLER                  PIC  X(03) VALUE '074'.
001311     12  FILLER                  PIC S9(04) COMP VALUE +08.
001312     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001313     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001314     12  FILLER                  PIC S9(04) COMP VALUE +08.
001315
001316*****CERT EXPIRATION DATE (LIFE)
001317     12  FILLER                  PIC  X(03) VALUE '075'.
001318     12  FILLER                  PIC S9(04) COMP VALUE +08.
001319     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001320     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001321     12  FILLER                  PIC S9(04) COMP VALUE +08.
001322
001323*****CERT EXPIRATION DATE (AH)
001324     12  FILLER                  PIC  X(03) VALUE '076'.
001325     12  FILLER                  PIC S9(04) COMP VALUE +08.
001326     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001327     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001328     12  FILLER                  PIC S9(04) COMP VALUE +08.
001329
001330*****LIFE TERM
001331     12  FILLER                  PIC  X(03) VALUE '077'.
001332     12  FILLER                  PIC S9(04) COMP VALUE +3.
001333     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001334     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001335     12  FILLER                  PIC S9(04) COMP VALUE +08.
001336
001337*****AH  TERM
001338     12  FILLER                  PIC  X(03) VALUE '078'.
001339     12  FILLER                  PIC S9(04) COMP VALUE +3.
001340     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001341     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001342     12  FILLER                  PIC S9(04) COMP VALUE +08.
001343
001344*****LIFE COVERAGE AMOUNT
001345     12  FILLER                  PIC  X(03) VALUE '079'.
001346     12  FILLER                  PIC S9(04) COMP VALUE +15.
001347     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001348     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001349     12  FILLER                  PIC S9(04) COMP VALUE +08.
001350
001351*****AH MONTHLY BENEFIT
001352     12  FILLER                  PIC  X(03) VALUE '080'.
001353     12  FILLER                  PIC S9(04) COMP VALUE +13.
001354     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001355     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001356     12  FILLER                  PIC S9(04) COMP VALUE +08.
001357
001358*****LIFE CANCEL DATE
001359     12  FILLER                  PIC  X(03) VALUE '081'.
001360     12  FILLER                  PIC S9(04) COMP VALUE +08.
001361     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001362     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001363     12  FILLER                  PIC S9(04) COMP VALUE +08.
001364
001365*****AH CANCEL DATE
001366     12  FILLER                  PIC  X(03) VALUE '082'.
001367     12  FILLER                  PIC S9(04) COMP VALUE +08.
001368     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001369     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001370     12  FILLER                  PIC S9(04) COMP VALUE +08.
001371
001372*****LIFE COVERAGE FORM NUMBER
001373     12  FILLER                  PIC  X(03) VALUE '083'.
001374     12  FILLER                  PIC S9(04) COMP VALUE +12.
001375     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001376     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001377     12  FILLER                  PIC S9(04) COMP VALUE +08.
001378
001379*****UNUSED
001380     12  FILLER                  PIC  X(03) VALUE '084'.
001381     12  FILLER                  PIC S9(04) COMP VALUE +12.
001382     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001383     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001384     12  FILLER                  PIC S9(04) COMP VALUE +08.
001385
001386*****INSUREDS AGE AT POLICY ISSUE (NOT USED)
001387     12  FILLER                  PIC  X(03) VALUE '085'.
001388     12  FILLER                  PIC S9(04) COMP VALUE +3.
001389     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001390     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001391     12  FILLER                  PIC S9(04) COMP VALUE +08.
001392
001393*****LOAN NUMBER
001394     12  FILLER                  PIC  X(03) VALUE '086'.
001395     12  FILLER                  PIC S9(04) COMP VALUE +08.
001396     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001397     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001398     12  FILLER                  PIC S9(04) COMP VALUE +08.
001399
001400*****LOAN BALANCE
001401     12  FILLER                  PIC  X(03) VALUE '087'.
001402     12  FILLER                  PIC S9(04) COMP VALUE +13.
001403     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001404     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001405     12  FILLER                  PIC S9(04) COMP VALUE +08.
001406
001407*****MEMBER NUMBER
001408     12  FILLER                  PIC  X(03) VALUE '088'.
001409     12  FILLER                  PIC S9(04) COMP VALUE +12.
001410     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001411     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001412     12  FILLER                  PIC S9(04) COMP VALUE +08.
001413
001414*****INSURED SOC SEC NUMBER
001415     12  FILLER                  PIC  X(03) VALUE '089'.
001416     12  FILLER                  PIC S9(04) COMP VALUE +11.
001417     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001418     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001419     12  FILLER                  PIC S9(04) COMP VALUE +08.
001420
001421*****INSURED INITIALS & LAST NAME (CERTIFICATE)
001422     12  FILLER                  PIC  X(03) VALUE '090'.
001423     12  FILLER                  PIC S9(04) COMP VALUE +15.
001424     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001425     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001426     12  FILLER                  PIC S9(04) COMP VALUE +08.
001427
001428*****INSURED FIRST NAME (CERTIFICATE)
001429     12  FILLER                  PIC  X(03) VALUE '091'.
001430     12  FILLER                  PIC S9(04) COMP VALUE +10.
001431     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001432     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001433     12  FILLER                  PIC S9(04) COMP VALUE +08.
001434
001435*****INSURED MIDDLE INITIAL (CERTIFICATE)
001436     12  FILLER                  PIC  X(03) VALUE '092'.
001437     12  FILLER                  PIC S9(04) COMP VALUE +05.
001438     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001439     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001440     12  FILLER                  PIC S9(04) COMP VALUE +08.
001441
001442*****ORIG TERM * MON BEN
001443     12  FILLER                  PIC  X(03) VALUE '093'.
001444     12  FILLER                  PIC S9(04) COMP VALUE +15.
001445     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001446     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001447     12  FILLER                  PIC S9(04) COMP VALUE +08.
001448
001449*****INSURED'S NAME (LAST, FIRST, INIT)
001450     12  FILLER                  PIC  X(03) VALUE '094'.
001451     12  FILLER                  PIC S9(04) COMP VALUE +30.
001452     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001453     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001454     12  FILLER                  PIC S9(04) COMP VALUE +08.
001455
001456*****INSURED'S NAME (FIRST, INIT, LAST)
001457     12  FILLER                  PIC  X(03) VALUE  '095'.
001458     12  FILLER                  PIC S9(04) COMP VALUE +30.
001459     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001460     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001461     12  FILLER                  PIC S9(04) COMP VALUE +08.
001462
001463*****TITLE (MR/MS)
001464     12  FILLER                  PIC  X(03) VALUE '096'.
001465     12  FILLER                  PIC S9(04) COMP VALUE +3.
001466     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001467     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001468     12  FILLER                  PIC S9(04) COMP VALUE +08.
001469
001470*****LIFE PREMIUM (CERTIFICATE)
001471     12  FILLER                  PIC  X(03) VALUE '097'.
001472     12  FILLER                  PIC S9(04) COMP VALUE +15.
001473     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001474     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001475     12  FILLER                  PIC S9(04) COMP VALUE +08.
001476
001477*****A/H PREMIUM (CERTIFICATE)
001478     12  FILLER                  PIC  X(03) VALUE '098'.
001479     12  FILLER                  PIC S9(04) COMP VALUE +13.
001480     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001481     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001482     12  FILLER                  PIC S9(04) COMP VALUE +08.
001483
001484*****JOINT'S INITIALS & LAST NAME (CERTIFICATE)
001485     12  FILLER                  PIC  X(03) VALUE '099'.
001486     12  FILLER                  PIC S9(04) COMP VALUE +15.
001487     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001488     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001489     12  FILLER                  PIC S9(04) COMP VALUE +08.
001490
001491*****JOINT'S FIRST NAME (CERTIFICATE)
001492     12  FILLER                  PIC  X(03) VALUE '100'.
001493     12  FILLER                  PIC S9(04) COMP VALUE +10.
001494     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001495     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001496     12  FILLER                  PIC S9(04) COMP VALUE +08.
001497
001498*****JOINT'S MIDDLE INITIAL (CERTIFICATE)
001499     12  FILLER                  PIC  X(03) VALUE '101'.
001500     12  FILLER                  PIC S9(04) COMP VALUE +05.
001501     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001502     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001503     12  FILLER                  PIC S9(04) COMP VALUE +08.
001504
001505*****JOINT'S NAME (LAST, FIRST, INIT)
001506     12  FILLER                  PIC  X(03) VALUE '102'.
001507     12  FILLER                  PIC S9(04) COMP VALUE +30.
001508     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001509     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001510     12  FILLER                  PIC S9(04) COMP VALUE +08.
001511
001512*****JOINT'S NAME (FIRST, INIT, LAST)
001513     12  FILLER                  PIC  X(03) VALUE '103'.
001514     12  FILLER                  PIC S9(04) COMP VALUE +30.
001515     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001516     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001517     12  FILLER                  PIC S9(04) COMP VALUE +08.
001518
001519*****INSURED'S FIRST AND LAST NAME
001520     12  FILLER                  PIC  X(03) VALUE '104'.
001521     12  FILLER                  PIC S9(04) COMP VALUE +30.
001522     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001523     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001524     12  FILLER                  PIC S9(04) COMP VALUE +08.
001525
001526*****JOINT'S FIRST AND LAST NAME
001527     12  FILLER                  PIC  X(03) VALUE '105'.
001528     12  FILLER                  PIC S9(04) COMP VALUE +30.
001529     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001530     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001531     12  FILLER                  PIC S9(04) COMP VALUE +08.
001532
001533*****ENTERED LIFE REFUND (CERT)
001534     12  FILLER                  PIC  X(03) VALUE '106'.
001535     12  FILLER                  PIC S9(04) COMP VALUE +13.
001536     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001537     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001538     12  FILLER                  PIC S9(04) COMP VALUE +08.
001539
001540*****ENTERED A/H REFUND (CERT)
001541     12  FILLER                  PIC  X(03) VALUE '107'.
001542     12  FILLER                  PIC S9(04) COMP VALUE +13.
001543     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001544     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001545     12  FILLER                  PIC S9(04) COMP VALUE +08.
001546
001547*****INSURED'S LAST NAME
001548     12  FILLER                  PIC  X(03) VALUE '108'.
001549     12  FILLER                  PIC S9(04) COMP VALUE +15.
001550     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001551     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001552     12  FILLER                  PIC S9(04) COMP VALUE +08.
001553
001554*****BENEFICIARY
001555     12  FILLER                  PIC  X(03) VALUE  '109'.
001556     12  FILLER                  PIC S9(04) COMP VALUE +25.
001557     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001558     12  FILLER                  PIC  X(01) VALUE 'Y'.
001559     12  FILLER                  PIC S9(04) COMP VALUE +08.
001560
001561************** PENDING VARIABLES - ERPNDB *********************
001562*****INSURED DATE OF BIRTH
001563     12  FILLER                  PIC  X(03) VALUE '110'.
001564     12  FILLER                  PIC S9(04) COMP VALUE +08.
001565     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001566     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001567     12  FILLER                  PIC S9(04) COMP VALUE +09.
001568
001569*****ENTERED LIFE PREMIUM (PENDING)
001570     12  FILLER                  PIC  X(03) VALUE '111'.
001571     12  FILLER                  PIC S9(04) COMP VALUE +13.
001572     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001573     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001574     12  FILLER                  PIC S9(04) COMP VALUE +09.
001575
001576*****ENTERED A/H PREMIUM (PENDING)
001577     12  FILLER                  PIC  X(03) VALUE '112'.
001578     12  FILLER                  PIC S9(04) COMP VALUE +13.
001579     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001580     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001581     12  FILLER                  PIC S9(04) COMP VALUE +09.
001582
001583*****CALCULATED LIFE PREMIUM (PENDING)
001584     12  FILLER                  PIC  X(03) VALUE '113'.
001585     12  FILLER                  PIC S9(04) COMP VALUE +13.
001586     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001587     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001588     12  FILLER                  PIC S9(04) COMP VALUE +09.
001589
001590*****CALCULATED A/H PREMIUM (PENDING)
001591     12  FILLER                  PIC  X(03) VALUE '114'.
001592     12  FILLER                  PIC S9(04) COMP VALUE +13.
001593     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001594     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001595     12  FILLER                  PIC S9(04) COMP VALUE +09.
001596
001597*****DIFFERENCE ENTER/COMPUTED LIFE PREMIUM (PENDING)
001598     12  FILLER                  PIC  X(03) VALUE '115'.
001599     12  FILLER                  PIC S9(04) COMP VALUE +13.
001600     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001601     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001602     12  FILLER                  PIC S9(04) COMP VALUE +09.
001603
001604*****DIFFERENCE ENTER/COMPUTED A/H PREMIUM (PENDING)
001605     12  FILLER                  PIC  X(03) VALUE '116'.
001606     12  FILLER                  PIC S9(04) COMP VALUE +13.
001607     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001608     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001609     12  FILLER                  PIC S9(04) COMP VALUE +09.
001610
001611*****PRIOR CANCEL DATE
001612     12  FILLER                  PIC  X(03) VALUE '117'.
001613     12  FILLER                  PIC S9(04) COMP VALUE +08.
001614     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001615     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001616     12  FILLER                  PIC S9(04) COMP VALUE +09.
001617
001618*****ENTERED LIFE REFUND (PENDING)
001619     12  FILLER                  PIC  X(03) VALUE '118'.
001620     12  FILLER                  PIC S9(04) COMP VALUE +13.
001621     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001622     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001623     12  FILLER                  PIC S9(04) COMP VALUE +09.
001624
001625*****ENTERED A/H REFUND (PENDING)
001626     12  FILLER                  PIC  X(03) VALUE '119'.
001627     12  FILLER                  PIC S9(04) COMP VALUE +13.
001628     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001629     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001630     12  FILLER                  PIC S9(04) COMP VALUE +09.
001631
001632*****CALCULATED LIFE REFUND (PENDING)
001633     12  FILLER                  PIC  X(03) VALUE '120'.
001634     12  FILLER                  PIC S9(04) COMP VALUE +13.
001635     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001636     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001637     12  FILLER                  PIC S9(04) COMP VALUE +09.
001638
001639*****CALCULATED A/H REFUND (PENDING)
001640     12  FILLER                  PIC  X(03) VALUE '121'.
001641     12  FILLER                  PIC S9(04) COMP VALUE +13.
001642     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001643     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001644     12  FILLER                  PIC S9(04) COMP VALUE +09.
001645
001646*****DIFFERENCE ENTER/COMPUTED LIFE REFUND (PENDING)
001647     12  FILLER                  PIC  X(03) VALUE '122'.
001648     12  FILLER                  PIC S9(04) COMP VALUE +13.
001649     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001650     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001651     12  FILLER                  PIC S9(04) COMP VALUE +09.
001652
001653*****DIFFERENCE ENTER/COMPUTED A/H REFUND (PENDING)
001654     12  FILLER                  PIC  X(03) VALUE '123'.
001655     12  FILLER                  PIC S9(04) COMP VALUE +13.
001656     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001657     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001658     12  FILLER                  PIC S9(04) COMP VALUE +09.
001659
001660*****INSUREDS AGE
001661     12  FILLER                  PIC  X(03) VALUE '124'.
001662     12  FILLER                  PIC S9(04) COMP VALUE +3.
001663     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001664     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001665     12  FILLER                  PIC S9(04) COMP VALUE +09.
001666
001667*****LIFE BENEFIT (PENDING)
001668     12  FILLER                  PIC  X(03) VALUE '125'.
001669     12  FILLER                  PIC S9(04) COMP VALUE +15.
001670     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001671     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001672     12  FILLER                  PIC S9(04) COMP VALUE +09.
001673
001674*****A/H BENEFIT (PENDING)
001675     12  FILLER                  PIC  X(03) VALUE '126'.
001676     12  FILLER                  PIC S9(04) COMP VALUE +13.
001677     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001678     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001679     12  FILLER                  PIC S9(04) COMP VALUE +09.
001680
001681*****LIFE RATE
001682     12  FILLER                  PIC  X(03) VALUE '127'.
001683     12  FILLER                  PIC S9(04) COMP VALUE +08.
001684     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001685     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001686     12  FILLER                  PIC S9(04) COMP VALUE +09.
001687
001688*****A/H RATE
001689     12  FILLER                  PIC  X(03) VALUE '128'.
001690     12  FILLER                  PIC S9(04) COMP VALUE +08.
001691     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001692     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001693     12  FILLER                  PIC S9(04) COMP VALUE +09.
001694
001695*****TERM (PENDING)
001696     12  FILLER                  PIC  X(03) VALUE '129'.
001697     12  FILLER                  PIC S9(04) COMP VALUE +3.
001698     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001699     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001700     12  FILLER                  PIC S9(04) COMP VALUE +09.
001701
001702*****BATCH NUMBER
001703     12  FILLER                  PIC  X(03) VALUE '130'.
001704     12  FILLER                  PIC S9(04) COMP VALUE +06.
001705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001706     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001707     12  FILLER                  PIC S9(04) COMP VALUE +09.
001708
001709*****TOTAL OF LIFE AND A&H REFUND
001710     12  FILLER                  PIC  X(03) VALUE '131'.
001711     12  FILLER                  PIC S9(04) COMP VALUE +13.
001712     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001713     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001714     12  FILLER                  PIC S9(04) COMP VALUE +08.
001715
001716*****NH INTEREST ON REFUNDS
001717     12  FILLER                  PIC  X(03) VALUE '132'.
001718     12  FILLER                  PIC S9(04) COMP VALUE +13.
001719     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001720     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001721     12  FILLER                  PIC S9(04) COMP VALUE +08.
001722
001723*****GREATER OF THE LIFE AND AH CANCEL DATE
001724     12  FILLER                  PIC  X(03) VALUE '133'.
001725     12  FILLER                  PIC S9(04) COMP VALUE +8.
001726     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001727     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001728     12  FILLER                  PIC S9(04) COMP VALUE +08.
001729
001730*****THE NEXT 6 NOT CURRENTLY USED
001731     12  FILLER                  PIC  X(03) VALUE '134'.
001732     12  FILLER                  PIC S9(04) COMP VALUE +30.
001733     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001734     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001735     12  FILLER                  PIC S9(04) COMP VALUE +09.
001736
001737     12  FILLER                  PIC  X(03) VALUE '135'.
001738     12  FILLER                  PIC S9(04) COMP VALUE +30.
001739     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001740     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001741     12  FILLER                  PIC S9(04) COMP VALUE +09.
001742
001743     12  FILLER                  PIC  X(03) VALUE '136'.
001744     12  FILLER                  PIC S9(04) COMP VALUE +30.
001745     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001746     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001747     12  FILLER                  PIC S9(04) COMP VALUE +09.
001748
001749     12  FILLER                  PIC  X(03) VALUE '137'.
001750     12  FILLER                  PIC S9(04) COMP VALUE +30.
001751     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001752     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001753     12  FILLER                  PIC S9(04) COMP VALUE +09.
001754
001755     12  FILLER                  PIC  X(03) VALUE '138'.
001756     12  FILLER                  PIC S9(04) COMP VALUE +30.
001757     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001758     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001759     12  FILLER                  PIC S9(04) COMP VALUE +09.
001760
001761     12  FILLER                  PIC  X(03) VALUE '139'.
001762     12  FILLER                  PIC S9(04) COMP VALUE +30.
001763     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001764     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001765     12  FILLER                  PIC S9(04) COMP VALUE +09.
001766
001767************** COMPENSATION VARIABLES - ERCOMP ****************
001768*****COMPENSATION ACCT NAME
001769     12  FILLER                  PIC  X(03) VALUE '140'.
001770     12  FILLER                  PIC S9(04) COMP VALUE +30.
001771     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001772     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001773     12  FILLER                  PIC S9(04) COMP VALUE +10.
001774
001775*****FULL COMPENSATION ADDRESS TYPE 'A'
001776     12  FILLER                  PIC  X(03) VALUE '141'.
001777     12  FILLER                  PIC S9(04) COMP VALUE +30.
001778     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001779     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001780     12  FILLER                  PIC S9(04) COMP VALUE +10.
001781
001782     12  FILLER                  PIC  X(03) VALUE '142'.
001783     12  FILLER                  PIC S9(04) COMP VALUE +30.
001784     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001785     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001786     12  FILLER                  PIC S9(04) COMP VALUE +10.
001787
001788     12  FILLER                  PIC  X(03) VALUE '143'.
001789     12  FILLER                  PIC S9(04) COMP VALUE +30.
001790     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001791     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001792     12  FILLER                  PIC S9(04) COMP VALUE +10.
001793
001794     12  FILLER                  PIC  X(03) VALUE '144'.
001795     12  FILLER                  PIC S9(04) COMP VALUE +30.
001796     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001797     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001798     12  FILLER                  PIC S9(04) COMP VALUE +10.
001799
001800     12  FILLER                  PIC  X(03) VALUE '145'.
001801     12  FILLER                  PIC S9(04) COMP VALUE +30.
001802     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001803     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001804     12  FILLER                  PIC S9(04) COMP VALUE +10.
001805
001806     12  FILLER                  PIC  X(03) VALUE '146'.
001807     12  FILLER                  PIC S9(04) COMP VALUE +12.
001808     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001809     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001810     12  FILLER                  PIC S9(04) COMP VALUE +10.
001811
001812*****COMPENSATION PHONE NUMBER
001813     12  FILLER                  PIC  X(03) VALUE '147'.
001814     12  FILLER                  PIC S9(04) COMP VALUE +12.
001815     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001816     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001817     12  FILLER                  PIC S9(04) COMP VALUE +10.
001818
001819*****COMPENSATION CSR NAME
001820     12  FILLER                  PIC  X(03) VALUE '148'.
001821     12  FILLER                  PIC S9(04) COMP VALUE +30.
001822     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001823     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001824     12  FILLER                  PIC S9(04) COMP VALUE +10.
001825
001826*****COMPENSATION LAST STATEMENT DATE
001827     12  FILLER                  PIC  X(03) VALUE '149'.
001828     12  FILLER                  PIC S9(04) COMP VALUE +18.
001829     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001830     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001831     12  FILLER                  PIC S9(04) COMP VALUE +10.
001832
001833*****COMPENSATION ENDING BALANCE
001834     12  FILLER                  PIC  X(03) VALUE '150'.
001835     12  FILLER                  PIC S9(04) COMP VALUE +14.
001836     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001837     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001838     12  FILLER                  PIC S9(04) COMP VALUE +10.
001839
001840******************  PROCESSOR DATA - ELCNTL (2) ****************
001841*****EXECUTING PROCESSOR NAME
001842     12  FILLER                  PIC  X(03) VALUE '151'.
001843     12  FILLER                  PIC S9(04) COMP VALUE +30.
001844     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001845     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001846     12  FILLER                  PIC S9(04) COMP VALUE +11.
001847
001848*****PROCESSOR TITLE
001849     12  FILLER                  PIC  X(03) VALUE '152'.
001850     12  FILLER                  PIC S9(04) COMP VALUE +26.
001851     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001852     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001853     12  FILLER                  PIC S9(04) COMP VALUE +11.
001854
001855*****PROCESSOR
001856     12  FILLER                  PIC  X(03) VALUE '153'.
001857     12  FILLER                  PIC S9(04) COMP VALUE +04.
001858     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001859     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001860     12  FILLER                  PIC S9(04) COMP VALUE +11.
001861
001862*****CSR TITLE
001863     12  FILLER                  PIC  X(03) VALUE '154'.
001864     12  FILLER                  PIC S9(04) COMP VALUE +30.
001865     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001866     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001867     12  FILLER                  PIC S9(04) COMP VALUE +11.
001868
001869*****REMAINING 2 ARE NOT CURRENTLY USED
001870     12  FILLER                  PIC  X(03) VALUE '155'.
001871     12  FILLER                  PIC S9(04) COMP VALUE +30.
001872     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001873     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001874     12  FILLER                  PIC S9(04) COMP VALUE +11.
001875
001876     12  FILLER                  PIC  X(03) VALUE '156'.
001877     12  FILLER                  PIC S9(04) COMP VALUE +30.
001878     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001879     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001880     12  FILLER                  PIC S9(04) COMP VALUE +11.
001881
001882******************  CHECK DATA - ERCHEK    *********************
001883*****CHECK AMOUNT
001884     12  FILLER                  PIC  X(03) VALUE '157'.
001885     12  FILLER                  PIC S9(04) COMP VALUE +13.
001886     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001887     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001888     12  FILLER                  PIC S9(04) COMP VALUE +12.
001889
001890*****CHECK NUMBER
001891     12  FILLER                  PIC  X(03) VALUE '158'.
001892     12  FILLER                  PIC S9(04) COMP VALUE +7.
001893     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001894     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001895     12  FILLER                  PIC S9(04) COMP VALUE +12.
001896
001897*****PAYEE 1 NAME
001898     12  FILLER                  PIC  X(03) VALUE '159'.
001899     12  FILLER                  PIC S9(04) COMP VALUE +30.
001900     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001901     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001902     12  FILLER                  PIC S9(04) COMP VALUE +12.
001903
001904*****PAYEE 2 NAME
001905     12  FILLER                  PIC  X(03) VALUE '160'.
001906     12  FILLER                  PIC S9(04) COMP VALUE +30.
001907     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001908     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001909     12  FILLER                  PIC S9(04) COMP VALUE +12.
001910
001911*****ADDRSS 1
001912     12  FILLER                  PIC  X(03) VALUE '161'.
001913     12  FILLER                  PIC S9(04) COMP VALUE +30.
001914     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001915     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001916     12  FILLER                  PIC S9(04) COMP VALUE +12.
001917
001918*****ADDRESS 2
001919     12  FILLER                  PIC  X(03) VALUE '162'.
001920     12  FILLER                  PIC S9(04) COMP VALUE +30.
001921     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001922     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001923     12  FILLER                  PIC S9(04) COMP VALUE +12.
001924
001925*****PAYEE CITY STATE
001926     12  FILLER                  PIC  X(03) VALUE '163'.
001927     12  FILLER                  PIC S9(04) COMP VALUE +30.
001928     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001929     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001930     12  FILLER                  PIC S9(04) COMP VALUE +12.
001931
001932*****ZIP CODE
001933     12  FILLER                  PIC  X(03) VALUE '164'.
001934     12  FILLER                  PIC S9(04) COMP VALUE +30.
001935     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001936     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001937     12  FILLER                  PIC S9(04) COMP VALUE +12.
001938
001939*****CHECK CONTROL
001940     12  FILLER                  PIC  X(03) VALUE '165'.
001941     12  FILLER                  PIC S9(04) COMP VALUE +08.
001942     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001943     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001944     12  FILLER                  PIC S9(04) COMP VALUE +12.
001945
001946*****REASON FOR CHECK
001947     12  FILLER                  PIC  X(03) VALUE '166'.
001948     12  FILLER                  PIC S9(04) COMP VALUE +25.
001949     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001950     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001951     12  FILLER                  PIC S9(04) COMP VALUE +12.
001952
001953*****REMAINING 3 ARE NOT CURRENTLY USED
001954     12  FILLER                  PIC  X(03) VALUE '167'.
001955     12  FILLER                  PIC S9(04) COMP VALUE +30.
001956     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001957     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001958     12  FILLER                  PIC S9(04) COMP VALUE +12.
001959
001960     12  FILLER                  PIC  X(03) VALUE '168'.
001961     12  FILLER                  PIC S9(04) COMP VALUE +30.
001962     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001963     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001964     12  FILLER                  PIC S9(04) COMP VALUE +12.
001965
001966     12  FILLER                  PIC  X(03) VALUE '169'.
001967     12  FILLER                  PIC S9(04) COMP VALUE +30.
001968     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001969     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
001970     12  FILLER                  PIC S9(04) COMP VALUE +11.
001971
001972
001973*********** PAYMENT AND ADJUSTMENT DATA - ERPYAJ  **************
001974*****CHECK AMOUNT - PYAJ
001975     12  FILLER                  PIC  X(03) VALUE '170'.
001976     12  FILLER                  PIC S9(04) COMP VALUE +13.
001977     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001978     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001979     12  FILLER                  PIC S9(04) COMP VALUE +13.
001980
001981*****CHECK NUMBER - PYAJ
001982     12  FILLER                  PIC  X(03) VALUE '171'.
001983     12  FILLER                  PIC S9(04) COMP VALUE +7.
001984     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001985     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001986     12  FILLER                  PIC S9(04) COMP VALUE +13.
001987
001988*****CHECK CONTROL - PYAJ
001989     12  FILLER                  PIC  X(03) VALUE '172'.
001990     12  FILLER                  PIC S9(04) COMP VALUE +08.
001991     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001992     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
001993     12  FILLER                  PIC S9(04) COMP VALUE +13.
001994
001995***** COMMENT - PYAJ
001996     12  FILLER                  PIC  X(03) VALUE '173'.
001997     12  FILLER                  PIC S9(04) COMP VALUE +30.
001998     12  FILLER                  PIC  X(30) VALUE ALL '*'.
001999     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002000     12  FILLER                  PIC S9(04) COMP VALUE +13.
002001
002002*****NEW FIELDS.
002003*****COMPENSATION FULL ADDRESS TYPE 'G'
002004     12  FILLER                  PIC  X(03) VALUE '174'.
002005     12  FILLER                  PIC S9(04) COMP VALUE +30.
002006     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002007     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002008     12  FILLER                  PIC S9(04) COMP VALUE +10.
002009
002010     12  FILLER                  PIC  X(03) VALUE '175'.
002011     12  FILLER                  PIC S9(04) COMP VALUE +30.
002012     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002013     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002014     12  FILLER                  PIC S9(04) COMP VALUE +10.
002015
002016     12  FILLER                  PIC  X(03) VALUE '176'.
002017     12  FILLER                  PIC S9(04) COMP VALUE +30.
002018     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002019     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002020     12  FILLER                  PIC S9(04) COMP VALUE +10.
002021
002022     12  FILLER                  PIC  X(03) VALUE '177'.
002023     12  FILLER                  PIC S9(04) COMP VALUE +30.
002024     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002025     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002026     12  FILLER                  PIC S9(04) COMP VALUE +10.
002027
002028     12  FILLER                  PIC  X(03) VALUE '178'.
002029     12  FILLER                  PIC S9(04) COMP VALUE +30.
002030     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002031     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002032     12  FILLER                  PIC S9(04) COMP VALUE +10.
002033
002034     12  FILLER                  PIC  X(03) VALUE '179'.
002035     12  FILLER                  PIC S9(04) COMP VALUE +12.
002036     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002037     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
002038     12  FILLER                  PIC S9(04) COMP VALUE +10.
002039
002040*****COMPENSATION FINANCIAL RESPONSIBLE NO.
002041     12  FILLER                  PIC  X(03) VALUE '180'.
002042     12  FILLER                  PIC S9(04) COMP VALUE +10.
002043     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002044     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
002045     12  FILLER                  PIC S9(04) COMP VALUE +10.
002046
002047**** COMPENSATION BILLING SWITCH
002048     12  FILLER                  PIC XXX    VALUE '181'.
002049     12  FILLER                  PIC S9(4)  COMP VALUE +7.
002050     12  FILLER                  PIC X(30)  VALUE ALL '*'.
002051     12  FILLER                  PIC X      VALUE ALL 'N'.
002052     12  FILLER                  PIC S9(4)  COMP VALUE +10.
002053
002054*****COMPENSATION FAX NUMBER
002055     12  FILLER                  PIC  X(03) VALUE '182'.
002056     12  FILLER                  PIC S9(04) COMP VALUE +12.
002057     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002058     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
002059     12  FILLER                  PIC S9(04) COMP VALUE +10.
002060
002061**** COMPENSATION STATUS
002062     12  FILLER                  PIC XXX    VALUE '183'.
002063     12  FILLER                  PIC S9(4)  COMP VALUE +7.
002064     12  FILLER                  PIC X(30)  VALUE ALL '*'.
002065     12  FILLER                  PIC X      VALUE ALL 'N'.
002066     12  FILLER                  PIC S9(4)  COMP VALUE +10.
002067
002068***** THE VARS 184 - 189 WILL TYPICALLY BE USED FOR DCC BANKS
002069*****COMPENSATION FULL ADDRESS TYPE 'B'
002070     12  FILLER                  PIC  X(03) VALUE '184'.
002071     12  FILLER                  PIC S9(04) COMP VALUE +30.
002072     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002073     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002074     12  FILLER                  PIC S9(04) COMP VALUE +10.
002075
002076     12  FILLER                  PIC  X(03) VALUE '185'.
002077     12  FILLER                  PIC S9(04) COMP VALUE +30.
002078     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002079     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002080     12  FILLER                  PIC S9(04) COMP VALUE +10.
002081
002082     12  FILLER                  PIC  X(03) VALUE '186'.
002083     12  FILLER                  PIC S9(04) COMP VALUE +30.
002084     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002085     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002086     12  FILLER                  PIC S9(04) COMP VALUE +10.
002087
002088     12  FILLER                  PIC  X(03) VALUE '187'.
002089     12  FILLER                  PIC S9(04) COMP VALUE +30.
002090     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002091     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002092     12  FILLER                  PIC S9(04) COMP VALUE +10.
002093
002094     12  FILLER                  PIC  X(03) VALUE '188'.
002095     12  FILLER                  PIC S9(04) COMP VALUE +30.
002096     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002097     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
002098     12  FILLER                  PIC S9(04) COMP VALUE +10.
002099
002100     12  FILLER                  PIC  X(03) VALUE '189'.
002101     12  FILLER                  PIC S9(04) COMP VALUE +12.
002102     12  FILLER                  PIC  X(30) VALUE ALL '*'.
002103     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
002104     12  FILLER                  PIC S9(04) COMP VALUE +10.
002105
002106
002107
002108 01  FILLER REDEFINES W-SUPPORTED-VARIABLES.
002109*    12  W-VARIABLE-GRP OCCURS 180 TIMES
002110     12  W-VARIABLE-GRP OCCURS 189 TIMES
002111                        INDEXED BY W-VG-NDX.
002112         16  W-VARIABLE-ID         PIC  X(03).
002113         16  W-VARIABLE-SIZE       PIC S9(04) COMP.
002114         16  W-VG-TEXT.
002115             20  W-VAR-CHAR
002116                        OCCURS 30 TIMES
002117                        INDEXED BY W-VC-NDX
002118                                   PIC  X(01).
002119         16  W-VARIABLE-UPLOW-IND  PIC  X(01).
002120             88  W-USE-UPPER-AND-LOWER-CASE VALUE 'Y'.
002121
002122         16  W-VARIABLE-SOURCE   PIC S9(04) COMP.
002123
002124 01  W-VAR-END                   PIC  X(23)
002125                        VALUE ':VARIABLE WORK AREA END'.
002126
002127 01  W-PROGRAM-TABLE-AREA.
002128     12  FILLER                  PIC  X(15)
002129                                 VALUE 'PROGRAM TABLES:'.
002130
002131     12  W-RECORD-TABLE              VALUE SPACES.
002132         16  W-RC-GRP OCCURS 500 TIMES
002133                       INDEXED BY W-RG-NDX
002134                                  W-RG-NDX1.
002135             20  W-RC-TEXT.
002136                 24  W-RC-CHAR OCCURS 70 TIMES
002137                                INDEXED BY W-RC-NDX
002138                                           W-RC-NDX1
002139                                 PIC  X(01).
002140             20  W-RC-PC         PIC  9(02).
002141             20  W-RC-SC         PIC  X(01).
002142
002143     12  FILLER REDEFINES W-RECORD-TABLE.
002144         16  W-REC-CHAR OCCURS 36500 TIMES
002145                        INDEXED BY W-RVS-NDX
002146                                   W-RVS-NDX2
002147                                 PIC  X(01).
002148     12  FILLER REDEFINES W-RECORD-TABLE.
002149         16  W-TS-GROUP OCCURS 10 TIMES
002150                        INDEXED BY W-TS-NDX
002151                                 PIC X(3650).
002152
002153     12  FILLER                  PIC  X(11)
002154                                 VALUE 'TEXT TABLE:'.
002155     12  W-TX-TABLE                  VALUE SPACES.
002156         16  W-TX-GRP OCCURS 500 TIMES
002157                       INDEXED BY W-TG-NDX
002158                                  W-TG-NDX2.
002159             20  W-TX-TEXT.
002160                 24  W-TX-CHAR OCCURS 70 TIMES
002161                                    INDEXED BY W-TX-NDX
002162                                               W-TX-NDX1
002163                                               W-TX-NDX2
002164                                 PIC  X(01).
002165             20  W-TX-PC         PIC  9(02).
002166             20  W-TX-SC         PIC  X(01).
002167
002168     12  FILLER                  PIC  X(11)
002169                                 VALUE 'FILE TABLE:'.
002170     12  W-FILE-TABLE                VALUE SPACES.
002171         16  W-FILE-USE-IND OCCURS 20 TIMES
002172                       INDEXED BY W-FILE-NDX
002173                                 PIC  X(01).
002174             88  W-FILE-NOT-USED     VALUE SPACE.
002175             88  W-FILE-USED         VALUE 'Y'.
002176
002177     12  FILLER                  PIC  X(14)
002178                                 VALUE 'END OF TABLES:'.
002179                                 EJECT
002180 01  ERROR-MESSAGES.
002181     12  ER-0000                 PIC  X(04) VALUE '0000'.
002182     12  ER-0004                 PIC  X(04) VALUE '0004'.
002183     12  ER-0006                 PIC  X(04) VALUE '0006'.
002184     12  ER-0008                 PIC  X(04) VALUE '0008'.
002185     12  ER-0013                 PIC  X(04) VALUE '0013'.
002186     12  ER-0023                 PIC  X(04) VALUE '0023'.
002187     12  ER-0029                 PIC  X(04) VALUE '0029'.
002188     12  ER-0033                 PIC  X(04) VALUE '0033'.
002189     12  ER-0042                 PIC  X(04) VALUE '0042'.
002190     12  ER-0047                 PIC  X(04) VALUE '0047'.
002191     12  ER-0051                 PIC  X(04) VALUE '0051'.
002192     12  ER-0066                 PIC  X(04) VALUE '0066'.
002193     12  ER-0067                 PIC  X(04) VALUE '0067'.
002194     12  ER-0070                 PIC  X(04) VALUE '0070'.
002195     12  ER-0168                 PIC  X(04) VALUE '0168'.
002196     12  ER-0169                 PIC  X(04) VALUE '0169'.
002197     12  ER-0174                 PIC  X(04) VALUE '0174'.
002198     12  ER-0175                 PIC  X(04) VALUE '0175'.
002199     12  ER-0176                 PIC  X(04) VALUE '0176'.
002200     12  ER-0177                 PIC  X(04) VALUE '0177'.
002201     12  ER-0179                 PIC  X(04) VALUE '0179'.
002202     12  ER-0180                 PIC  X(04) VALUE '0180'.
002203     12  ER-0181                 PIC  X(04) VALUE '0181'.
002204     12  ER-0182                 PIC  X(04) VALUE '0182'.
002205     12  ER-0184                 PIC  X(04) VALUE '0184'.
002206     12  ER-0185                 PIC  X(04) VALUE '0185'.
002207     12  ER-0187                 PIC  X(04) VALUE '0187'.
002208     12  ER-0188                 PIC  X(04) VALUE '0188'.
002209     12  ER-0189                 PIC  X(04) VALUE '0189'.
002210     12  ER-0190                 PIC  X(04) VALUE '0190'.
002211     12  ER-0191                 PIC  X(04) VALUE '0191'.
002212     12  ER-0215                 PIC  X(04) VALUE '0215'.
002213     12  ER-0279                 PIC  X(04) VALUE '0279'.
002214     12  ER-0280                 PIC  X(04) VALUE '0280'.
002215     12  ER-0412                 PIC  X(04) VALUE '0412'.
002216     12  ER-0413                 PIC  X(04) VALUE '0413'.
002217     12  ER-0454                 PIC  X(04) VALUE '0454'.
002218     12  ER-0533                 PIC  X(04) VALUE '0533'.
002219     12  ER-0537                 PIC  X(04) VALUE '0537'.
002220     12  ER-0715                 PIC  X(04) VALUE '0715'.
002221     12  ER-0894                 PIC  X(04) VALUE '0894'.
002222     12  ER-1560                 PIC  X(04) VALUE '1560'.
002223     12  ER-1565                 PIC  X(04) VALUE '1565'.
002224     12  ER-1778                 PIC  X(04) VALUE '1778'.
002225     12  ER-1818                 PIC  X(04) VALUE '1818'.
002226     12  ER-2055                 PIC  X(04) VALUE '2055'.
002227     12  ER-2114                 PIC  X(04) VALUE '2114'.
002228     12  ER-2208                 PIC  X(04) VALUE '2208'.
002229     12  ER-2209                 PIC  X(04) VALUE '2209'.
002230     12  ER-2216                 PIC  X(04) VALUE '2216'.
002231     12  ER-2232                 PIC  X(04) VALUE '2232'.
002232     12  ER-2369                 PIC  X(04) VALUE '2369'.
002233     12  ER-2398                 PIC  X(04) VALUE '2398'.
002234     12  ER-2433                 PIC  X(04) VALUE '2433'.
002235     12  ER-2908                 PIC  X(04) VALUE '2908'.
002236     12  ER-2999                 PIC  X(04) VALUE '2999'.
002237     12  ER-3000                 PIC  X(04) VALUE '3000'.
002238     12  ER-3770                 PIC  X(04) VALUE '3770'.
002239     12  ER-3771                 PIC  X(04) VALUE '3771'.
002240     12  ER-3775                 PIC  X(04) VALUE '3775'.
002241     12  ER-3783                 PIC  X(04) VALUE '3783'.
002242     12  ER-7243                 PIC  X(04) VALUE '7243'.
002243     12  ER-7245                 PIC  X(04) VALUE '7245'.
002244     12  ER-7246                 PIC  X(04) VALUE '7246'.
002245     12  ER-7247                 PIC  X(04) VALUE '7247'.
002246     12  ER-7250                 PIC  X(04) VALUE '7250'.
002247     12  ER-7365                 PIC  X(04) VALUE '7365'.
002248     12  ER-7367                 PIC  X(04) VALUE '7367'.
002249     12  ER-7368                 PIC  X(04) VALUE '7368'.
002250     12  ER-7369                 PIC  X(04) VALUE '7369'.
002251     12  ER-7370                 PIC  X(04) VALUE '7370'.
002252     12  ER-7371                 PIC  X(04) VALUE '7371'.
002253     12  ER-7372                 PIC  X(04) VALUE '7372'.
002254     12  ER-7373                 PIC  X(04) VALUE '7373'.
002255     12  ER-7374                 PIC  X(04) VALUE '7374'.
002256     12  ER-7376                 PIC  X(04) VALUE '7376'.
002257     12  ER-7377                 PIC  X(04) VALUE '7377'.
002258     12  ER-7378                 PIC  X(04) VALUE '7378'.
002259     12  ER-7379                 PIC  X(04) VALUE '7379'.
002260     12  ER-7381                 PIC  X(04) VALUE '7381'.
002261     12  ER-7388                 PIC  X(04) VALUE '7388'.
002262     12  ER-7390                 PIC  X(04) VALUE '7390'.
002263     12  ER-7393                 PIC  X(04) VALUE '7393'.
002264     12  ER-7395                 PIC  X(04) VALUE '7395'.
002265     12  ER-7396                 PIC  X(04) VALUE '7396'.
002266     12  ER-7398                 PIC  X(04) VALUE '7398'.
002267     12  ER-8965                 PIC  X(04) VALUE '8965'.
002268     12  ER-9095                 PIC  X(04) VALUE '9095'.
002269     12  ER-9097                 PIC  X(04) VALUE '9097'.
002270     12  ER-9281                 PIC  X(04) VALUE '9281'.
002271     12  ER-9283                 PIC  X(04) VALUE '9283'.
002272     12  ER-9298                 PIC  X(04) VALUE '9298'.
002273     12  ER-9299                 PIC  X(04) VALUE '9299'.
002274     12  ER-9320                 PIC  X(04) VALUE '9320'.
002275     12  ER-9327                 PIC  X(04) VALUE '9327'.
002276     12  ER-9426                 PIC  X(04) VALUE '9426'.
002277     12  ER-9427                 PIC  X(04) VALUE '9427'.
002278     12  ER-9840                 PIC  X(04) VALUE '9840'.
002279                                 EJECT
002280*    COPY ELCAID.
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
002281 01  FILLER    REDEFINES DFHAID.
002282     12  FILLER                  PIC  X(08).
002283     12  PF-VALUES               PIC  X(01) OCCURS 2.
002284                                 EJECT
002285*    COPY ELCATTR.
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
002286                                 EJECT
002287*    COPY ELCDATE.
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
002288                                 EJECT
002289*    COPY ELCNWA.
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
002290                                 EJECT
002291*    COPY ELCEMIB.
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
002292 01  EMI-SAVE-AREA               PIC X(400).
002293                                 EJECT
002294*    COPY ELCLOGOF.
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
002295                                 EJECT
002296*    COPY ELCSCTM.
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
002297                                 EJECT
002298*    COPY ELCSCRTY.
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
002299                                 EJECT
002300
002301 01  W-TS-WORK-AREA              PIC X(3650).
002302                                 EJECT
002303 01  sqlconnect-parms.
002304     05  p-sql-server            PIC X(30).
002305     05  p-sql-database          PIC X(30).
002306     05  p-connect-return-code   pic s9(5) comp-5.
002307     05  p-sql-return-message    pic x(256).
002308
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
002310 01  DFHCOMMAREA                 PIC X(1024).
002311
002312 01  var  pic x(30).
002313
002314*    COPY ERCACCT.
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
002315                                 EJECT
002316*    COPY ERCARCH.
      *>>((file: ERCARCH))
000001******************************************************************
000002*                                                                *
000003*                            ERCARCH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 250  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
000013*     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
000014*     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
000015*     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
000016*     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
000017*     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
000018*                                                                *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000029* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
000030* 110612    2012101700002  AJRA  ADD NEW FIELDS
000031* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
000032******************************************************************
000033 01  LETTER-ARCHIVE.
000034     12  LA-RECORD-ID                PIC  X(02).
000035         88  LA-VALID-ID                VALUE 'LA'.
000036
000037     12  LA-CONTROL-PRIMARY.
000038         16  LA-COMPANY-CD           PIC  X(01).
000039         16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
000040
000041     12  LA-CONTROL-BY-CERT-RESP.
000042         16  LA-COMPANY-CD-A2        PIC  X(01).
000043         16  LA-CERT-NO-A2.
000044             20  LA-CERT-PRIME-A2    PIC  X(10).
000045             20  LA-CERT-SUFFIX-A2   PIC  X(01).
000046         16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
000047             20  LA-RESP-PERSON-A2   PIC  X(10).
000048             20  LA-TYPE-A2          PIC  X(01).
000049         16  LA-CARRIER-A2           PIC  X(01).
000050         16  LA-GROUPING-A2          PIC  X(06).
000051         16  LA-STATE-A2             PIC  X(02).
000052         16  LA-ACCOUNT-A2           PIC  X(10).
000053         16  LA-EFFECT-DATE-A2       PIC  X(02).
000054         16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
000055
000056     12  LA-CONTROL-BY-FORM.
000057         16  LA-COMPANY-CD-A3        PIC  X(01).
000058         16  LA-FORM-A3              PIC  X(04).
000059         16  LA-CARRIER-A3           PIC  X(01).
000060         16  LA-GROUPING-A3          PIC  X(06).
000061         16  LA-STATE-A3             PIC  X(02).
000062         16  LA-ACCOUNT-A3           PIC  X(10).
000063         16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
000064
000065     12  LA-CONTROL-BY-PROCESSOR.
000066         16  LA-COMPANY-CD-A4        PIC  X(01).
000067         16  LA-PROCESSOR-CD         PIC  X(04).
000068         16  LA-CARRIER-A4           PIC  X(01).
000069         16  LA-GROUPING-A4          PIC  X(06).
000070         16  LA-STATE-A4             PIC  X(02).
000071         16  LA-ACCOUNT-A4           PIC  X(10).
000072         16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
000073
000074     12  LA-CONTROL-BY-KEY-FIELDS.
000075         16  LA-COMPANY-CD-A5        PIC  X(01).
000076         16  LA-CARRIER-A5           PIC  X(01).
000077         16  LA-GROUPING-A5          PIC  X(06).
000078         16  LA-STATE-A5             PIC  X(02).
000079         16  LA-ACCOUNT-A5           PIC  X(10).
000080         16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
000081
000082     12  LA-CONTROL-BY-GROUP-CODE.
000083         16  LA-COMPANY-CD-A6        PIC  X(01).
000084         16  LA-ENTRY-A6.
000085             20  LA-FILLER           PIC  X(02).
000086             20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
000087         16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
000088
000089     12  FILLER                      PIC  X(09).
000090
000091     12  LA-HEADER-RECORD.
000092         16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
000093         16  LA-CREATION-DATE        PIC  X(02).
000094         16  LA-FOLLOW-UP-DATE       PIC  X(02).
000095         16  LA-FINAL-ACT-DATE       REDEFINES
000096               LA-FOLLOW-UP-DATE     PIC  X(02).
000097         16  LA-INITIAL-PRINT-DATE   PIC  X(02).
000098         16  LA-NO-OF-COPIES         PIC S9(01).
000099         16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
000100         16  LA-REPLY-DATE           PIC  X(02).
000101         16  LA-RESEND-DATES.
000102             20  LA-RESEND-DATE      PIC  X(02).
000103             20  LA-SENT-DATE        PIC  X(02).
000104             20  FILLER              PIC  X(08).
000105         16  LA-SOURCE-INFORMATION.
000106             20  LA-DATA-SOURCE      PIC  X(01).
000107             20  LA-ADDR-SOURCE      PIC  X(01).
000108         16  LA-STATUS               PIC  X(01).
000109             88  LA-STATUS-ACTIVE         VALUE 'A'.
000110             88  LA-STATUS-COMPLETED      VALUE 'C'.
000111             88  LA-STATUS-ON-HOLD        VALUE 'H'.
000112             88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
000113             88  LA-STATUS-PURGED         VALUE 'P'.
000114             88  LA-STATUS-VOIDED         VALUE 'V'.
000115         16  LA-LAST-RESENT-PRINT-DATE
000116                                     PIC  X(02).
000117         16  LA-PRINT-RESTRICTION    PIC  X(01).
000118             88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
000119                                          VALUE 'C'.
000120             88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
000121                                          VALUE 'F'.
000122             88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
000123                                          VALUE 'P'.
000124         16  LA-PURGED-DATE          PIC  X(02).
000125         16  LA-VOIDED-DATE          PIC  X(02).
000126         16  LA-RESEND-LETR          PIC  X(4).
000127         16  LA-VOID-ONBASE-YN       PIC  X.
000128         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
000129         16  FILLER                  PIC  X(04).
000130*        16  LA-RESEND-LETR-2        PIC  X(4).
000131*        16  LA-RESEND-LETR-3        PIC  X(4).
000132*        16  FILLER                  PIC  X(59).
000133         16  LA-ARCHIVE-STATUS       PIC  X.
000134             88  LA-TEMP                VALUE 'T'.
000135             88  LA-QWS                 VALUE 'Q'.
000136             88  LA-BATCH               VALUE 'B'.
000137         16  LA-FINAL-ACT-IND        PIC  X(1).
000138         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
000139         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
000140         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
000141                                     PIC X(4).
000142         16  FILLER                  PIC  X(42).
000143*        16  FILLER                  PIC  X(71).
000144         16  LA-LAST-MAINT-DATE      PIC  X(2).
000145         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
000146         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
000147                                     PIC  X(4).
000148         16  LA-LAST-UPDATED-BY      PIC  X(4).
000149
000150******************************************************************
      *<<((file: ERCARCH))
002317                                 EJECT
002318*    COPY ERCARCT.
      *>>((file: ERCARCT))
000001******************************************************************
000002*                                                                *
000003*                            ERCARCT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 1640  RECFORM = FIXED                          *
000011*                                                                *
000012*   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
000013*                                                                *
000014*   LOG = NO                                                     *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017*                   C H A N G E   L O G
000018*
000019* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000020*-----------------------------------------------------------------
000021*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000022* EFFECTIVE    NUMBER
000023*-----------------------------------------------------------------
000024* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
000025******************************************************************
000026 01  LETTER-ARCHIVE-TEXT.
000027     12  LT-RECORD-ID                PIC  X(02).
000028         88  LT-VALID-ID                VALUE 'LT'.
000029
000030     12  LT-CONTROL-PRIMARY.
000031         16  LT-COMPANY-CD           PIC  X(01).
000032         16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
000033         16  LT-RECORD-TYPE          PIC  X(01).
000034             88  LT-ADDRESS-DATA        VALUE '1'.
000035             88  LT-TEXT-DATA           VALUE '2'.
000036             88  LT-COMMENT-DATA        VALUE '3'.
000037         16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
000038
000039     12  FILLER                      PIC  X(28).
000040     12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
000041
000042     12  LT-TEXT-RECORD.
000043         16  LT-LETTER-TEXT OCCURS 20 TIMES
000044                            INDEXED BY LT-NDX.
000045             20  LT-TEXT-LINE        PIC  X(70).
000046             20  LT-SKIP-CONTROL     PIC  X(02).
000047                 88  LT-NO-LINES-SKIPPED             VALUE SPACES.
000048                 88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
000049             20  FILLER              PIC  X(08).
000050
000051     12  LT-COMMENT-RECORD  REDEFINES LT-TEXT-RECORD.
000052         16  LT-LETTER-COMMENT OCCURS 20 TIMES INDEXED BY LC-NDX.
000053             20  LT-COMMENT-LINE     PIC X(69).
000054             20  LT-COMMENT-CHG-DT   PIC X(02).
000055             20  LT-COMMENT-CHG-BY   PIC X(04).
000056             20  FILLER              PIC X(05).
000057
      *<<((file: ERCARCT))
002319                                 EJECT
002320*    COPY ELCCERT.
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
002321                                 EJECT
002322*    COPY ERCCHEK.
      *>>((file: ERCCHEK))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCHEK                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.008                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CHECK RECORDS                             *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 600    RECFORM = FIXED                         *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
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
000026* 021414    2003053000001  PEMA  changes for auto chk request
000027******************************************************************
000028 01  CHECK-RECORDS.
000029     12  CH-RECORD-ID                      PIC XX.
000030         88  VALID-CH-ID                      VALUE 'CH'.
000031
000032     12  CH-CONTROL-PRIMARY.
000033         16  CH-COMPANY-CD                 PIC X.
000034         16  CH-CARRIER                    PIC X.
000035         16  CH-GROUPING                   PIC X(6).
000036         16  CH-STATE                      PIC XX.
000037         16  CH-ACCOUNT                    PIC X(10).
000038         16  CH-CERT-EFF-DT                PIC XX.
000039         16  CH-CERT-NO.
000040             20  CH-CERT-PRIME             PIC X(10).
000041             20  CH-CERT-SFX               PIC X.
000042         16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
000043
000044     12  CH-RECORDED-DT                    PIC XX.
000045     12  CH-RECORDED-BY                    PIC X(4).
000046     12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
000047
000048     12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
000049     12  CH-CHECK-NO                       PIC X(7).
000050     12  CH-REASON-FOR-CHECK               PIC X(25).
000051     12  CH-CHECK-WRITTEN-DT               PIC XX.
000052     12  FILLER                            PIC X.
000053
000054     12  CH-PAYEE-INFO.
000055         16  CH-PAYEE-NAME-1               PIC X(30).
000056         16  CH-PAYEE-NAME-2               PIC X(30).
000057         16  CH-PAYEE-ADDRESS-1            PIC X(30).
000058         16  CH-PAYEE-ADDRESS-2            PIC X(30).
000059         16  CH-PAYEE-CITY-ST.
000060             20  CH-PAYEE-CITY             PIC X(28).
000061             20  CH-PAYEE-STATE            PIC XX.
000062         16  CH-PAYEE-ZIP-CODE.
000063             20  CH-PAYEE-ZIP.
000064                 24  CH-ZIP-PRI-1ST        PIC X.
000065                     88  CH-CANADIAN-POST-CODE
000066                                           VALUES 'A' THRU 'Z'.
000067                 24  FILLER                PIC X(4).
000068             20  CH-PAYEE-ZIP-EXT          PIC X(4).
000069         16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
000070             20  CH-CAN-POSTAL-1           PIC XXX.
000071             20  CH-CAN-POSTAL-2           PIC XXX.
000072             20  FILLER                    PIC XXX.
000073
000074     12  CH-CHECK-STUB-TEXT.
000075         16  CH-STUB-LINE-1                PIC X(30).
000076         16  CH-TEXT-LINE-1                PIC X(50).
000077         16  CH-TEXT-LINE-2                PIC X(50).
000078         16  CH-TEXT-LINE-3                PIC X(40).
000079     12  CH-RETURN-TO                      PIC X(30).
000080
000081     12  CH-COMPENSATION-CONTROL.
000082         16  CH-COMP-CARRIER               PIC X.
000083         16  CH-COMP-GROUPING              PIC X(6).
000084         16  CH-COMP-FIN-RESP              PIC X(10).
000085         16  CH-COMP-ACCOUNT               PIC X(10).
000086
000087     12  CH-CREDIT-SELECT-DT               PIC XX.
000088     12  CH-CREDIT-ACCEPT-DT               PIC XX.
000089     12  CH-PAYEE-CODE                     PIC X(6).
000090
000091     12  CH-VOID-DATA.
000092         20  CH-VOID-DT                    PIC XX.
000093         20  CH-VOID-BY                    PIC X(4).
000094         20  CH-VOID-REASON                PIC X(25).
000095
000096     12  CH-APPROVAL-DATA.
000097         20  CH-APPROVAL-DT                PIC XX.
000098         20  CH-APPROVAL-STATUS            PIC X.
000099             88  CH-IN-LIMBO                  VALUE ' '.
000100             88  CH-APPROV-PENDING            VALUE 'P' '2'.
000101             88  CH-APPROVED                  VALUE 'A'.
000102             88  CH-DENIED                    VALUE 'D'.
000103         20  CH-APPROVED-BY                PIC XXXX.
000104     12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
000105             88  PAYMENT-NOT-QUEUED           VALUE ZERO.
000106     12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
000107
000108     12  ch-released-dt                    pic xx.
000109     12  ch-check-cashed-dt                pic xx.
000110     12  FILLER                            PIC X.
000111*    12  CH-CHECK-REFERENCE                PIC X(12).
000112     12  CH-CHECK-ORIGIN-SW                PIC X.
000113             88  CH-REFUND-CHECK              VALUE 'R'.
000114             88  CH-MAINT-CHECK               VALUE 'M'.
000115
000116     12  CH-CANC-DT                        PIC XX.
000117     12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
000118     12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
000119
000120     12  CH-INSURED-NAME                   PIC X(28).
000121
000122     12  ch-released-by                    pic x(4).
000123     12  ch-csr                            pic x(4).
000124     12  ch-deduct-commission              pic x.
000125         88  ch-deduct-comm                  value 'Y'.
000126         88  ch-do-not-deduct-comm           value 'N'.
000127
000128     12  FILLER                            PIC X(11).
000129*    12  CH-LETTER-TABLE.
000130*        16  CH-LETTERS OCCURS 3 TIMES
000131*                       INDEXED BY CH-LT-NDX
000132*                                          PIC X(04).
000133
000134     12  FILLER                            PIC X(07).
000135
      *<<((file: ERCCHEK))
002323                                 EJECT
002324*    COPY ELCCNTL.
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
002325                                 EJECT
002326*    COPY ERCCOMP.
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
002327                                 EJECT
002328*    COPY ERCMAIL.
      *>>((file: ERCMAIL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCMAIL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
000014*   ALTERNATE PATH    = NOT USED                                 *
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
000026* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
000027* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000028* 111108                   PEMA  ADD CRED BENE ADDR2
000029******************************************************************
000030
000031 01  MAILING-DATA.
000032     12  MA-RECORD-ID                      PIC XX.
000033         88  VALID-MA-ID                       VALUE 'MA'.
000034
000035     12  MA-CONTROL-PRIMARY.
000036         16  MA-COMPANY-CD                 PIC X.
000037         16  MA-CARRIER                    PIC X.
000038         16  MA-GROUPING.
000039             20  MA-GROUPING-PREFIX        PIC XXX.
000040             20  MA-GROUPING-PRIME         PIC XXX.
000041         16  MA-STATE                      PIC XX.
000042         16  MA-ACCOUNT.
000043             20  MA-ACCOUNT-PREFIX         PIC X(4).
000044             20  MA-ACCOUNT-PRIME          PIC X(6).
000045         16  MA-CERT-EFF-DT                PIC XX.
000046         16  MA-CERT-NO.
000047             20  MA-CERT-PRIME             PIC X(10).
000048             20  MA-CERT-SFX               PIC X.
000049
000050     12  FILLER                            PIC XX.
000051
000052     12  MA-ACCESS-CONTROL.
000053         16  MA-SOURCE-SYSTEM              PIC XX.
000054             88  MA-FROM-CREDIT                VALUE 'CR'.
000055             88  MA-FROM-VSI                   VALUE 'VS'.
000056             88  MA-FROM-WARRANTY              VALUE 'WA'.
000057             88  MA-FROM-OTHER                 VALUE 'OT'.
000058         16  MA-RECORD-ADD-DT              PIC XX.
000059         16  MA-RECORD-ADDED-BY            PIC XXXX.
000060         16  MA-LAST-MAINT-DT              PIC XX.
000061         16  MA-LAST-MAINT-BY              PIC XXXX.
000062         16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000063
000064     12  MA-PROFILE-INFO.
000065         16  MA-QUALIFY-CODE-1             PIC XX.
000066         16  MA-QUALIFY-CODE-2             PIC XX.
000067         16  MA-QUALIFY-CODE-3             PIC XX.
000068         16  MA-QUALIFY-CODE-4             PIC XX.
000069         16  MA-QUALIFY-CODE-5             PIC XX.
000070
000071         16  MA-INSURED-LAST-NAME          PIC X(15).
000072         16  MA-INSURED-FIRST-NAME         PIC X(10).
000073         16  MA-INSURED-MIDDLE-INIT        PIC X.
000074         16  MA-INSURED-ISSUE-AGE          PIC 99.
000075         16  MA-INSURED-BIRTH-DT           PIC XX.
000076         16  MA-INSURED-SEX                PIC X.
000077             88  MA-SEX-MALE                   VALUE 'M'.
000078             88  MA-SEX-FEMALE                 VALUE 'F'.
000079         16  MA-INSURED-SOC-SEC-NO         PIC X(11).
000080
000081         16  MA-ADDRESS-CORRECTED          PIC X.
000082         16  MA-JOINT-BIRTH-DT             PIC XX.
000083*        16  FILLER                        PIC X(12).
000084
000085         16  MA-ADDRESS-LINE-1             PIC X(30).
000086         16  MA-ADDRESS-LINE-2             PIC X(30).
000087         16  MA-CITY-STATE.
000088             20  MA-CITY                   PIC X(28).
000089             20  MA-ADDR-STATE             PIC XX.
000090         16  MA-ZIP.
000091             20  MA-ZIP-CODE.
000092                 24  MA-ZIP-CODE-1ST       PIC X(1).
000093                     88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000094                 24  FILLER                PIC X(4).
000095             20  MA-ZIP-PLUS4              PIC X(4).
000096         16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
000097             20  MA-CAN-POSTAL-CODE-1      PIC X(3).
000098             20  MA-CAN-POSTAL-CODE-2      PIC X(3).
000099             20  FILLER                    PIC X(3).
000100
000101         16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
000102
000103         16  FILLER                        PIC XXX.
000104*        16  FILLER                        PIC X(10).
000105
000106
000107     12  MA-CRED-BENE-INFO.
000108         16  MA-CRED-BENE-NAME                 PIC X(25).
000109         16  MA-CRED-BENE-ADDR                 PIC X(30).
000110         16  MA-CRED-BENE-ADDR2                PIC X(30).
000111         16  MA-CRED-BENE-CTYST.
000112             20  MA-CRED-BENE-CITY             PIC X(28).
000113             20  MA-CRED-BENE-STATE            PIC XX.
000114         16  MA-CRED-BENE-ZIP.
000115             20  MA-CB-ZIP-CODE.
000116                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
000117                     88  MA-CB-CANADIAN-POST-CODE
000118                                           VALUE 'A' THRU 'Z'.
000119                 24  FILLER                    PIC X(4).
000120             20  MA-CB-ZIP-PLUS4               PIC X(4).
000121         16  MA-CB-CANADIAN-POSTAL-CODE
000122                            REDEFINES MA-CRED-BENE-ZIP.
000123             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
000124             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
000125             20  FILLER                        PIC X(3).
000126     12  MA-POST-CARD-MAIL-DATA.
000127         16  MA-MAIL-DATA OCCURS 7.
000128             20  MA-MAIL-TYPE              PIC X.
000129                 88  MA-12MO-MAILING           VALUE '1'.
000130                 88  MA-EXP-MAILING            VALUE '2'.
000131             20  MA-MAIL-STATUS            PIC X.
000132                 88  MA-MAIL-ST-MAILED         VALUE '1'.
000133                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
000134                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
000135             20  MA-MAIL-DATE              PIC XX.
000136     12  FILLER                            PIC XX.
000137     12  FILLER                            PIC XX.
000138*    12  FILLER                            PIC X(30).
000139******************************************************************
      *<<((file: ERCMAIL))
002329                                 EJECT
002330*    COPY ERCPNDB.
      *>>((file: ERCPNDB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.025                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
000009*                                                                *
000010******************************************************************
000011*   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
000012*         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
000013******************************************************************
000014*                                                                *
000015*                                                                *
000016*   FILE TYPE = VSAM,KSDS                                        *
000017*   RECORD SIZE = 585  RECFORM = FIXED                           *
000018*                                                                *
000019*   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
000020*       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
000021*                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
000022*                                                 RKP=13,LEN=36  *
000023*       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
000024*                                      AND CHG-SEQ.)             *
000025*                                                RKP=49,LEN=11   *
000026*       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
000027*                                      AND CHG-SEQ.)             *
000028*                                                RKP=60,LEN=15   *
000029*                                                                *
000030*   LOG = NO                                                     *
000031*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000032******************************************************************
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000042* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000043* 011904                   PEMA  ADD TOTAL FEE PROCESSING
000044* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000045* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
000046* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000047* 032306                   PEMA  ADD BOW LOAN NUMBER
000048* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
000049* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
000050* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000051* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000052* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
000053* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
000054* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000055* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
000056* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
000057* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000058* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
000059* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000060* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
000061******************************************************************
000062
000063 01  PENDING-BUSINESS.
000064     12  PB-RECORD-ID                     PIC XX.
000065         88  VALID-PB-ID                        VALUE 'PB'.
000066
000067     12  PB-CONTROL-PRIMARY.
000068         16  PB-COMPANY-CD                PIC X.
000069         16  PB-ENTRY-BATCH               PIC X(6).
000070         16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
000071         16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
000072
000073     12  PB-CONTROL-BY-ACCOUNT.
000074         16  PB-COMPANY-CD-A1             PIC X.
000075         16  PB-CARRIER                   PIC X.
000076         16  PB-GROUPING.
000077             20  PB-GROUPING-PREFIX       PIC XXX.
000078             20  PB-GROUPING-PRIME        PIC XXX.
000079         16  PB-STATE                     PIC XX.
000080         16  PB-ACCOUNT.
000081             20  PB-ACCOUNT-PREFIX        PIC X(4).
000082             20  PB-ACCOUNT-PRIME         PIC X(6).
000083         16  PB-CERT-EFF-DT               PIC XX.
000084         16  PB-CERT-NO.
000085             20  PB-CERT-PRIME            PIC X(10).
000086             20  PB-CERT-SFX              PIC X.
000087         16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
000088
000089         16  PB-RECORD-TYPE               PIC X.
000090             88  PB-MAILING-DATA                VALUE '0'.
000091             88  PB-ISSUE                       VALUE '1'.
000092             88  PB-CANCELLATION                VALUE '2'.
000093             88  PB-BATCH-TRAILER               VALUE '9'.
000094
000095     12  PB-CONTROL-BY-ORIG-BATCH.
000096         16  PB-ORIGINAL-COMPANY-CD       PIC X.
000097         16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
000098         16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
000099         16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
000100
000101     12  PB-CONTROL-BY-CSR.
000102         16  PB-CSR-COMPANY-CD            PIC X.
000103         16  PB-CSR-ID                    PIC X(4).
000104         16  PB-CSR-ENTRY-BATCH           PIC X(6).
000105         16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
000106         16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
000107******************************************************************
000108*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
000109******************************************************************
000110
000111     12  PB-LAST-MAINT-DT                 PIC XX.
000112     12  PB-LAST-MAINT-BY                 PIC X(4).
000113     12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000114
000115     12  PB-RECORD-BODY                   PIC X(375).
000116
000117     12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
000118         16  PB-CERT-ORIGIN               PIC X.
000119             88  CLASIC-CREATED-CERT         VALUE '1'.
000120         16  PB-I-NAME.
000121             20  PB-I-INSURED-LAST-NAME   PIC X(15).
000122             20  PB-I-INSURED-FIRST-NAME.
000123                 24  PB-I-INSURED-1ST-INIT PIC X.
000124                 24  FILLER                PIC X(9).
000125             20  PB-I-INSURED-MIDDLE-INIT PIC X.
000126         16  PB-I-AGE                     PIC S99   COMP-3.
000127         16  PB-I-JOINT-AGE               PIC S99   COMP-3.
000128         16  PB-I-BIRTHDAY                PIC XX.
000129         16  PB-I-INSURED-SEX             PIC X.
000130             88  PB-SEX-MALE     VALUE 'M'.
000131             88  PB-SEX-FEMALE   VALUE 'F'.
000132
000133         16  PB-I-LF-TERM                 PIC S999   COMP-3.
000134         16  PB-I-AH-TERM                 PIC S999   COMP-3.
000135         16  PB-I-LOAN-TERM               PIC S999   COMP-3.
000136         16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
000137         16  PB-I-SKIP-CODE               PIC X.
000138             88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
000139             88  PB-SKIP-JULY              VALUE '1'.
000140             88  PB-SKIP-AUGUST            VALUE '2'.
000141             88  PB-SKIP-SEPTEMBER         VALUE '3'.
000142             88  PB-SKIP-JULY-AUG          VALUE '4'.
000143             88  PB-SKIP-AUG-SEPT          VALUE '5'.
000144             88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
000145             88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
000146             88  PB-SKIP-JUNE              VALUE '8'.
000147             88  PB-SKIP-JUNE-JULY         VALUE '9'.
000148             88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
000149             88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
000150         16  PB-I-TERM-TYPE               PIC X.
000151             88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
000152             88  PB-PAID-WEEKLY            VALUE 'W'.
000153             88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
000154             88  PB-PAID-BI-WEEKLY         VALUE 'B'.
000155             88  PB-PAID-13-YEARLY         VALUE 'T'.
000156         16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
000157         16  PB-I-POLICY-FORM-NO          PIC X(12).
000158         16  PB-I-DATA-ENTRY-SW           PIC X.
000159             88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
000160             88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
000161             88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
000162             88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
000163         16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
000164         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
000165*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
000166         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
000167         16  PB-I-LETTER-REQD             PIC X.
000168
000169         16  PB-I-LIFE-BENEFIT-CD         PIC XX.
000170             88  PB-VALID-LIFE               VALUE '01' THRU '89'.
000171             88  PB-INVALID-LIFE             VALUE '  ' '00'
000172                                                   '90' THRU '99'.
000173         16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
000174                                          PIC XX.
000175         16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000176         16  PB-I-AMOUNT-FINANCED REDEFINES
000177                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
000178         16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
000179         16  PB-I-UNPAID-CASH-PRICE REDEFINES
000180                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
000181         16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000182         16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
000183         16  PB-I-CLP-AMOUNT REDEFINES
000184                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
000185         16  PB-I-LF-CALC-FLAG            PIC X.
000186             88 PB-COMP-LF-PREM               VALUE '?'.
000187         16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
000188         16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
000189         16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
000190         16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
000191         16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
000192         16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
000193         16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
000194         16  PB-I-LF-ABBR                 PIC XXX.
000195         16  PB-I-LF-INPUT-CD             PIC XX.
000196
000197         16  PB-I-AH-BENEFIT-CD           PIC XX.
000198             88  PB-VALID-AH                 VALUE '01' THRU '89'.
000199             88  PB-INVALID-AH               VALUE '  ' '00'
000200                                                   '90' THRU '99'.
000201         16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000202         16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000203         16  PB-I-AH-CALC-FLAG            PIC X.
000204             88 PB-COMP-AH-PREM                  VALUE '?'.
000205         16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
000206         16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
000207         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
000208         16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
000209         16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
000210         16  PB-I-AH-ABBR                 PIC XXX.
000211         16  PB-I-AH-INPUT-CD             PIC XXX.
000212
000213         16  PB-I-SPECIAL-REIN-CODE       PIC X.
000214         16  PB-I-REIN-TABLE              PIC XXX.
000215         16  PB-I-BUSINESS-TYPE           PIC 99.
000216         16  PB-I-INDV-GRP-CD             PIC X.
000217         16  PB-I-MORT-CODE.
000218             20  PB-I-TABLE               PIC X.
000219             20  PB-I-INTEREST            PIC XX.
000220             20  PB-I-MORT-TYP            PIC X.
000221         16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
000222         16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
000223         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
000224         16  PB-I-INDV-GRP-OVRD           PIC X.
000225         16  PB-I-RATE-CLASS-OVRD         PIC XX.
000226         16  PB-I-SIG-SW                  PIC X.
000227             88  PB-POLICY-SIGNED             VALUE 'Y'.
000228         16  PB-I-RATE-CLASS              PIC XX.
000229         16  PB-I-RATE-DEVIATION-LF       PIC XXX.
000230         16  PB-I-RATE-DEVIATION-AH       PIC XXX.
000231         16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
000232         16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
000233         16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
000234         16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
000235         16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
000236         16  PB-I-BENEFIT-TYPE            PIC XXX.
000237         16  PB-I-OB-FLAG                 PIC X.
000238             88  PB-I-OB                      VALUE 'B'.
000239             88  PB-I-SUMMARY                 VALUE 'Z'.
000240         16  PB-I-ENTRY-STATUS            PIC X.
000241             88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
000242                                              'M' '5' '9' '2'.
000243             88  PB-I-NORMAL-ENTRY            VALUE '1'.
000244             88  PB-I-POLICY-PENDING          VALUE '2'.
000245             88  PB-I-CONVERSION-ENTRY        VALUE '4'.
000246             88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
000247             88  PB-I-POLICY-IS-CASH          VALUE 'C'.
000248             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
000249             88  PB-I-REIN-ONLY               VALUE '9'.
000250             88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
000251             88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
000252             88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
000253             88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
000254         16  PB-I-INT-CODE                PIC X.
000255             88  PB-ADD-ON-INTEREST           VALUE 'A'.
000256             88  PB-SIMPLE-INTEREST           VALUE 'S'.
000257         16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000258         16  PB-I-SOC-SEC-NO              PIC X(11).
000259         16  PB-I-MEMBER-NO               PIC X(12).
000260         16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
000261*        16  PB-I-LOAN-OFFICER            PIC XXX.
000262         16  PB-I-OLD-LOF                 PIC XXX.
000263         16  PB-I-LF-EXPIRE-DT            PIC XX.
000264         16  PB-I-AH-EXPIRE-DT            PIC XX.
000265         16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
000266         16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
000267         16  PB-I-LIFE-INDICATOR          PIC X.
000268             88  PB-I-JOINT-COVERAGE         VALUE 'J'.
000269         16  PB-I-LIVES                   PIC S9(7)       COMP-3.
000270         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
000271                                          PIC S9(5)V99    COMP-3.
000272         16  PB-I-MAIL-ADDRS-SW           PIC X.
000273             88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
000274             88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
000275         16  PB-I-1ST-PMT-DT              PIC XX.
000276         16  PB-I-JOINT-INSURED.
000277             20 PB-I-JOINT-LAST-NAME      PIC X(15).
000278             20 PB-I-JOINT-FIRST-NAME.
000279                24  PB-I-JOINT-FIRST-INIT PIC X.
000280                24  FILLER                PIC X(9).
000281             20 PB-I-JOINT-MIDDLE-INIT    PIC X.
000282*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
000283         16  PB-I-BENEFICIARY-NAME.
000284             20  PB-I-BANK-NUMBER         PIC X(10).
000285             20  FILLER                   PIC X(15).
000286         16  PB-I-LAST-ADD-ON-DT          PIC XX.
000287         16  PB-I-REFERENCE               PIC X(12).
000288         16  FILLER REDEFINES PB-I-REFERENCE.
000289             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
000290             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
000291             20  PB-I-CLP-STATE           PIC XX.
000292         16  PB-I-UNDERWRITING-STATUS     PIC X.
000293             88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
000294             88  PB-I-POLICY-DECLINED         VALUE 'D'.
000295             88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
000296         16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
000297         16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
000298         16  PB-I-RESIDENT-STATE          PIC XX.
000299         16  PB-I-RATE-CODE               PIC X(4).
000300         16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
000301         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000302         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000303         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
000304         16  PB-I-BANK-NOCHRGB            PIC 99.
000305         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
000306         16  PB-I-JOINT-BIRTHDAY          PIC XX.
000307
000308     12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
000309         16  PB-C-LF-CANCEL-VOID-SW       PIC X.
000310             88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
000311         16  PB-C-CANCEL-ORIGIN           PIC X.
000312             88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
000313         16  PB-C-LF-CANCEL-DT            PIC XX.
000314         16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000315         16  PB-C-LF-CALC-REQ             PIC X.
000316             88 PB-COMP-LF-CANCEL            VALUE '?'.
000317         16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
000318         16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
000319         16  PB-C-AH-CANCEL-VOID-SW       PIC X.
000320             88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
000321         16  PB-C-AH-CANCEL-DT            PIC XX.
000322         16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000323         16  PB-C-AH-CALC-REQ             PIC X.
000324             88 PB-COMP-AH-CANCEL            VALUE '?'.
000325         16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
000326         16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
000327         16  PB-C-LAST-NAME               PIC X(15).
000328         16  PB-C-REFUND-SW               PIC X.
000329             88  PB-C-REFUND-CREATED          VALUE 'Y'.
000330             88  PB-C-REFUND-REQUESTED        VALUE 'R'.
000331         16  PB-C-LIVES                   PIC S9(3)       COMP-3.
000332         16  PB-C-PAYEE-CODE              PIC X(6).
000333         16  PB-C-LF-REFUND-OVERRIDE      PIC X.
000334         16  PB-C-AH-REFUND-OVERRIDE      PIC X.
000335         16  PB-C-LF-COMM-CHARGEBACK      PIC X.
000336         16  PB-C-AH-COMM-CHARGEBACK      PIC X.
000337         16  PB-C-REFERENCE               PIC X(12).
000338         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000339         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000340         16  PB-C-POST-CARD-IND           PIC X.
000341         16  PB-C-CANCEL-REASON           PIC X.
000342         16  PB-C-REF-INTERFACE-SW        PIC X.
000343         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
000344         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
000345         16  FILLER                       PIC X(01).
000346*        16  FILLER                       PIC X(18).
000347         16  PB-C-POLICY-FORM-NO          PIC X(12).
000348*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
000349         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
000350         16  PB-CANCELED-CERT-DATA.
000351             20  PB-CI-INSURED-NAME.
000352                 24  PB-CI-LAST-NAME      PIC X(15).
000353                 24  PB-CI-INITIALS       PIC XX.
000354             20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
000355             20  PB-CI-INSURED-SEX        PIC X.
000356             20  PB-CI-LF-TERM            PIC S999        COMP-3.
000357             20  PB-CI-LF-BENEFIT-CD      PIC XX.
000358             20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
000359             20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000360             20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000361             20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
000362             20  PB-CI-AH-TERM            PIC S999        COMP-3.
000363             20  PB-CI-AH-BENEFIT-CD      PIC XX.
000364             20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000365             20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000366             20  PB-CI-RATE-CLASS         PIC XX.
000367             20  PB-CI-RATE-DEV-LF        PIC XXX.
000368             20  PB-CI-RATE-DEV-AH        PIC XXX.
000369             20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
000370             20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
000371             20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
000372             20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
000373             20  PB-CI-LF-ABBR            PIC X(3).
000374             20  PB-CI-AH-ABBR            PIC X(3).
000375             20  PB-CI-OB-FLAG            PIC X.
000376                 88  PB-CI-OB                VALUE 'B'.
000377             20  PB-CI-LF-POLICY-STATUS   PIC X.
000378                 88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
000379                                           'M' '4' '5' '9' '2'.
000380                 88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
000381                 88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
000382                 88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
000383                 88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
000384                 88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
000385                 88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
000386                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000387                 88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
000388                 88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000389                 88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
000390                 88  PB-CI-LF-REIN-ONLY              VALUE '9'.
000391                 88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
000392                 88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
000393             20  PB-CI-AH-POLICY-STATUS   PIC X.
000394                 88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
000395                                           'M' '4' '5' '9' '2'.
000396                 88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
000397                 88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
000398                 88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
000399                 88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
000400                 88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
000401                 88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
000402                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000403                 88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
000404                 88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000405                 88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
000406                 88  PB-CI-AH-REIN-ONLY              VALUE '9'.
000407                 88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
000408                 88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
000409             20  PB-CI-PAY-FREQUENCY      PIC 99.
000410             20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000411             20  PB-CI-SOC-SEC-NO         PIC X(11).
000412             20  PB-CI-MEMBER-NO          PIC X(12).
000413             20  PB-CI-INT-CODE           PIC X.
000414                 88  PB-CI-ADD-ON                  VALUE 'A'.
000415                 88  PB-CI-SIMPLE                  VALUE 'S'.
000416             20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
000417             20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
000418             20  PB-CI-COMP-EXCP-SW       PIC X.
000419                 88  PB-CI-NO-COMP-EXCP            VALUE ' '.
000420                 88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
000421             20  PB-CI-ENTRY-STATUS       PIC X.
000422             20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
000423             20  PB-CI-AH-PAID-THRU-DT    PIC XX.
000424             20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
000425             20  PB-CI-DEATH-DT           PIC XX.
000426             20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
000427             20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
000428             20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000429             20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000430             20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
000431             20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
000432             20  PB-CI-ENTRY-DT              PIC XX.
000433             20  PB-CI-ENTRY-BATCH           PIC X(6).
000434             20  PB-CI-LF-EXPIRE-DT          PIC XX.
000435             20  PB-CI-AH-EXPIRE-DT          PIC XX.
000436             20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
000437             20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
000438             20  PB-CI-OLD-LOF               PIC XXX.
000439*            20  PB-CI-LOAN-OFFICER          PIC XXX.
000440             20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
000441             20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
000442             20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
000443             20  PB-CI-INDV-GRP-CD           PIC X.
000444             20  PB-CI-BENEFICIARY-NAME.
000445                 24  PB-CI-BANK-NUMBER       PIC X(10).
000446                 24  FILLER                  PIC X(15).
000447             20  PB-CI-NOTE-SW               PIC X.
000448             20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
000449             20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
000450             20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
000451             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
000452             20  PB-CI-LOAN-OFFICER          PIC X(5).
000453             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
000454             20  PB-CI-FIRST-NAME            PIC X(10).
000455             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
000456
000457         16  FILLER                       PIC X(13).
000458*032306  16  FILLER                       PIC X(27).
000459*        16  FILLER                       PIC X(46).
000460
000461     12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
000462         16  FILLER                       PIC X(10).
000463         16  PB-M-INSURED-LAST-NAME       PIC X(15).
000464         16  PB-M-INSURED-FIRST-NAME      PIC X(10).
000465         16  PB-M-INSURED-MID-INIT        PIC X.
000466         16  PB-M-INSURED-AGE             PIC 99.
000467         16  PB-M-INSURED-BIRTHDAY        PIC XX.
000468         16  PB-M-INSURED-SEX             PIC X.
000469         16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
000470         16  PB-M-INSURED-ADDRESS-1       PIC X(30).
000471         16  PB-M-INSURED-ADDRESS-2       PIC X(30).
000472         16  PB-M-INSURED-CITY-STATE.
000473             20  PB-M-INSURED-CITY        PIC X(28).
000474             20  PB-M-INSURED-STATE       PIC XX.
000475         16  PB-M-INSURED-ZIP-CODE.
000476             20  PB-M-INSURED-ZIP-PRIME.
000477                 24  PB-M-INSURED-ZIP-1   PIC X.
000478                     88  PB-M-CANADIAN-POST-CODE
000479                                             VALUE 'A' THRU 'Z'.
000480                 24  FILLER               PIC X(4).
000481             20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
000482         16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
000483                                        PB-M-INSURED-ZIP-CODE.
000484             20  PM-M-INS-CAN-POST1       PIC XXX.
000485             20  PM-M-INS-CAN-POST2       PIC XXX.
000486             20  FILLER                   PIC XXX.
000487         16  PB-M-INSURED-PHONE-NO        PIC 9(10).
000488         16  PB-M-JOINT-BIRTHDAY          PIC XX.
000489         16  PB-M-CRED-BENE-NAME          PIC X(30).
000490         16  PB-M-CRED-BENE-ADDR1         PIC X(30).
000491         16  PB-M-CRED-BENE-ADDR2         PIC X(30).
000492         16  PB-M-CRED-BENE-CITYST.
000493             20  PB-M-CRED-BENE-CITY      PIC X(28).
000494             20  PB-M-CRED-BENE-STATE     PIC XX.
000495
000496         16  FILLER                       PIC X(92).
000497
000498     12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
000499         16  FILLER                       PIC X(10).
000500         16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000501         16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000502         16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000503         16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000504         16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000505         16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000506         16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000507         16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000508         16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000509         16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000510         16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000511         16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000512         16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
000513         16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
000514         16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
000515         16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
000516         16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
000517         16  PB-ACCOUNT-NAME              PIC X(30).
000518         16  PB-PREM-REF-RPT-FLAG         PIC X.
000519         16  PB-REFERENCE                 PIC X(12).
000520         16  PB-B-RECEIVED-DT             PIC XX.
000521         16  FILLER                       PIC X(234).
000522
000523     12  PB-RECORD-STATUS.
000524         16  PB-CREDIT-SELECT-DT          PIC XX.
000525         16  PB-CREDIT-ACCEPT-DT          PIC XX.
000526         16  PB-BILLED-DT                 PIC XX.
000527         16  PB-BILLING-STATUS            PIC X.
000528             88  PB-ENTRY-REVERSED            VALUE 'R'.
000529             88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
000530             88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
000531         16  PB-RECORD-BILL               PIC X.
000532             88  PB-RECORD-ON-HOLD            VALUE 'H'.
000533             88  PB-RECORD-RETURNED           VALUE 'R'.
000534             88  PB-RECORD-ENDORSED           VALUE 'E'.
000535             88  PB-OVERRIDE-LIFE             VALUE 'L'.
000536             88  PB-OVERRIDE-AH               VALUE 'A'.
000537             88  PB-OVERRIDE-BOTH             VALUE 'B'.
000538         16  PB-BATCH-ENTRY               PIC X.
000539             88  PB-POLICY-IS-DECLINED        VALUE 'D'.
000540             88  PB-REIN-ONLY-CERT            VALUE 'R'.
000541             88  PB-REISSUED-CERT             VALUE 'E'.
000542             88  PB-CASH-CERT                 VALUE 'C'.
000543             88  PB-MONTHLY-CERT              VALUE 'M'.
000544             88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
000545             88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
000546             88  PB-POLICY-IS-VOIDED          VALUE 'V'.
000547         16  PB-FORCE-CODE                PIC X.
000548             88  PB-FORCE-OFF                 VALUE ' ' '0'.
000549             88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
000550             88  PB-CANCEL-FORCE              VALUE '8'.
000551             88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
000552             88  PB-ALL-CANCEL-FORCED         VALUE '8'.
000553             88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
000554             88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
000555             88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
000556             88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
000557             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
000558             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
000559         16  PB-FATAL-FLAG                PIC X.
000560             88  PB-FATAL-ERRORS              VALUE 'X'.
000561         16  PB-FORCE-ER-CD               PIC X.
000562             88  PB-FORCE-ERRORS              VALUE 'F'.
000563             88  PB-UNFORCED-ERRORS           VALUE 'X'.
000564         16  PB-WARN-ER-CD                PIC X.
000565             88  PB-WARNING-ERRORS            VALUE 'W'.
000566         16  FILLER                       PIC X.
000567         16  PB-OUT-BAL-CD                PIC X.
000568             88  PB-OUT-OF-BAL                VALUE 'O'.
000569         16  PB-LIFE-OVERRIDE-L1          PIC X.
000570         16  PB-AH-OVERRIDE-L1            PIC X.
000571         16  PB-INPUT-DT                  PIC XX.
000572         16  PB-INPUT-BY                  PIC X(4).
000573         16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
000574         16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
000575         16  PB-TOLERANCE-REJECT-SW       PIC X.
000576         16  PB-LF-EARNING-METHOD         PIC X.
000577         16  PB-AH-EARNING-METHOD         PIC X.
000578         16  PB-LF-TERM-CALC-METHOD       PIC X.
000579         16  PB-AH-TERM-CALC-METHOD       PIC X.
000580         16  PB-REIN-CD                   PIC XXX.
000581         16  PB-LF-REFUND-TYPE            PIC X.
000582         16  PB-AH-REFUND-TYPE            PIC X.
000583         16  PB-ACCT-EFF-DT               PIC XX.
000584         16  PB-ACCT-EXP-DT               PIC XX.
000585         16  PB-COMPANY-ID                PIC X(3).
000586         16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000587         16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000588         16  PB-SV-CARRIER                PIC X.
000589         16  PB-SV-GROUPING               PIC X(6).
000590         16  PB-SV-STATE                  PIC XX.
000591         16  PB-CONFIRMATION-REPT-DT      PIC XX.
000592         16  PB-GA-BILLING-INFO.
000593             20  PB-GA-BILL-DT OCCURS 5 TIMES
000594                                          PIC XX.
000595         16  PB-SV-REMIT-TO  REDEFINES
000596             PB-GA-BILLING-INFO           PIC X(10).
000597         16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
000598         16  PB-I-LOAN-OFFICER            PIC X(5).
000599         16  PB-I-VIN                     PIC X(17).
000600
000601         16  FILLER                       PIC X(04).
000602         16  IMNET-BYPASS-SW              PIC X.
000603
000604******************************************************************
000605*                COMMON EDIT ERRORS                              *
000606******************************************************************
000607
000608     12  PB-COMMON-ERRORS.
000609         16  PB-COMMON-ERROR    OCCURS 10 TIMES
000610                                           PIC S9(4)     COMP.
000611
000612******************************************************************
      *<<((file: ERCPNDB))
002331                                 EJECT
002332*    COPY ERCPYAJ.
      *>>((file: ERCPYAJ))
000001******************************************************************
000002*                                                                *
000003*                            ERCPYAJ                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.015                          *
000006*                                                                *
000007*   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
000008*                                                                *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 200  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
000014*       ALTERNATE PATHS = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
000028* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
000029******************************************************************
000030
000031 01  PENDING-PAY-ADJ.
000032     12  PY-RECORD-ID                     PIC XX.
000033         88  VALID-PY-ID                        VALUE 'PY'.
000034
000035     12  PY-CONTROL-PRIMARY.
000036         16  PY-COMPANY-CD                PIC X.
000037         16  PY-CARRIER                   PIC X.
000038         16  PY-GROUPING                  PIC X(6).
000039         16  PY-FIN-RESP                  PIC X(10).
000040         16  PY-ACCOUNT                   PIC X(10).
000041         16  PY-PRODUCER REDEFINES PY-ACCOUNT
000042                                          PIC X(10).
000043         16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
000044         16  PY-RECORD-TYPE               PIC X.
000045             88  PY-REMIT-RECEIVED            VALUE 'R'.
000046             88  PY-DEPOSIT                   VALUE 'D'.
000047             88  PY-CHARGE-TO-AGENT           VALUE 'C'.
000048             88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
000049             88  PY-ADJ-DEPOSIT               VALUE 'T'.
000050             88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
000051             88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
000052             88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
000053             88  PY-ADD-TO-BALANCE            VALUE 'Z'.
000054             88  PY-FICA-ENTRY                VALUE 'F'.
000055             88  PY-REMIT-IND-GROUPING        VALUE 'G'.
000056             88  PY-POLICY-FEE                VALUE 'W'.
000057             88  PY-DUE-PREM-ADJ              VALUE 'P'.
000058
000059     12  PY-PYMT-TYPE                     PIC X.
000060             88  PY-NEW-BUS-PYMT              VALUE 'B'.
000061             88  PY-REINS-PYMT                VALUE 'R'.
000062             88  PY-EXP-PYMT                  VALUE 'E'.
000063
000064     12  PY-BIL-INV                       PIC X(6).
000065     12  PY-REF-NO                        PIC X(12).
000066
000067     12  PY-LAST-MAINT-DT                 PIC XX.
000068     12  PY-LAST-MAINT-BY                 PIC X(4).
000069     12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000070
000071     12  PY-PYADJ-RECORD.
000072         16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
000073         16  PY-ENTRY-COMMENT             PIC X(30).
000074         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
000075             20  PY-GL-ACCOUNT            PIC X(10).
000076             20  PY-GL-STATE              PIC X(02).
000077             20  PY-GL-CANC-SW            PIC X(01).
000078                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
000079                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
000080             20  PY-GL-COMMENT            PIC X(10).
000081             20  FILLER      REDEFINES PY-GL-COMMENT.
000082                 24  PY-GL-CHECK-NO       PIC 9(06).
000083                 24  FILLER               PIC X(04).
000084             20  FILLER                   PIC X(07).
000085         16  PY-SAVE-ACCOUNT              PIC X(10).
000086         16  PY-SAVE-TYPE                 PIC X(01).
000087
000088         16  PY-LETTERS.
000089             20  PY-LETTER OCCURS 3 TIMES
000090                           INDEXED BY PY-LET-NDX
000091                                          PIC X(04).
000092
000093         16  PY-ERCOMP-TYPE               PIC X.
000094             88  PY-ACCOUNT-TYPE              VALUE 'A'.
000095             88  PY-GA-TYPE                   VALUE 'G'.
000096             88  PY-BANK-TYPE                 VALUE 'B'.
000097         16  FILLER                       PIC X(05).
000098
000099     12  PY-RECORD-STATUS.
000100         16  PY-CREDIT-SELECT-DT          PIC XX.
000101         16  PY-CREDIT-ACCEPT-DT          PIC XX.
000102         16  PY-BILLED-DATE               PIC XX.
000103         16  PY-REPORTED-DT               PIC XX.
000104         16  PY-PMT-APPLIED               PIC X.
000105             88  PY-ACCOUNT-PMT               VALUE 'A'.
000106             88  PY-GA-PMT                    VALUE 'G'.
000107             88  PY-OVWRITE-PMT               VALUE 'O'.
000108             88  PY-NON-AR-PMT                VALUE 'N'.
000109         16  FILLER                       PIC X(5).
000110         16  PY-INPUT-DT                  PIC XX.
000111         16  PY-CHECK-NUMBER              PIC X(6).
000112         16  PY-VOID-SW                   PIC X.
000113             88  PY-CHECK-VOIDED              VALUE 'V'.
000114         16  PY-CHECK-ORIGIN-SW           PIC X.
000115             88  PY-BILLING-CHECK             VALUE 'B'.
000116             88  PY-REFUND-CHECK              VALUE 'R'.
000117             88  PY-GA-CHECK                  VALUE 'G'.
000118             88  PY-CHECK-WRITTEN             VALUE 'W'.
000119             88  PY-CHECK-REVERSAL            VALUE 'V'.
000120         16  PY-CHECK-WRITTEN-DT          PIC XX.
000121         16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
000122         16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
000123         16  PY-BILL-FLAG                 PIC X.
000124             88  PY-BILLED                    VALUE 'B'.
000125         16  PY-AR-FLAG                   PIC X.
000126             88  PY-AR-CYCLE                  VALUE 'C'.
000127             88  PY-AR-MONTH-END              VALUE 'M'.
000128         16  PY-AR-DATE                   PIC XX.
000129
000130     12  PY-GL-CODES.
000131         16  PY-GL-DB                     PIC X(14).
000132         16  PY-GL-CR                     PIC X(14).
000133         16  PY-GL-FLAG                   PIC X.
000134         16  PY-GL-DATE                   PIC XX.
000135
000136     12  PY-CANCEL-FEE-FLAG               PIC X(2).
000137     12  FILLER                           PIC X(3).
000138******************************************************************
000139
      *<<((file: ERCPYAJ))
002333                                 EJECT
002334*    COPY ELCTEXT.
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
002335                                 EJECT
002336*    COPY ERCENDT.
      *>>((file: ERCENDT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCENDT.                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ENDORSEMENT FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 579  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
000013*       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*-----------------------------------------------------------------
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
000027*                                WITH OPEN CLAIMS
000028* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
000029* 110612    2012101700002  AJRA  ADD NEW FIELDS
000030* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000031* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
000032*-----------------------------------------------------------------
000033
000034 01  ENDORSEMENT-RECORD.
000035     12  EN-RECORD-ID                PIC XX.
000036         88  VALID-EN-ID                VALUE 'EN'.
000037
000038     12  EN-CONTROL-PRIMARY.
000039         16  EN-COMPANY-CD           PIC X.
000040         16  EN-CARRIER              PIC X.
000041         16  EN-GROUPING             PIC X(6).
000042         16  EN-STATE                PIC XX.
000043         16  EN-ACCOUNT              PIC X(10).
000044         16  EN-CERT-EFF-DT          PIC XX.
000045         16  EN-CERT-NO.
000046             20  EN-CERT-PRIME       PIC X(10).
000047             20  EN-CERT-SFX         PIC X.
000048         16  EN-REC-TYPE             PIC X.
000049             88  EN-ISSUE               VALUE 'I'.
000050             88  EN-CANCELLATION        VALUE 'C'.
000051         16  EN-SEQ-NO               PIC 9(04) BINARY.
000052
000053     12  EN-CONTROL-BY-ARCH-NO.
000054         16  EN-COMPANY-CD-A1              PIC X.
000055         16  EN-ARCHIVE-NO                 PIC 9(8) BINARY.
000056
000057     12  EN-LAST-MAINT-DT            PIC XX.
000058     12  EN-LAST-MAINT-BY            PIC X(4).
000059     12  EN-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000060
000061     12  EN-ENDORSEMENT-RECORD       PIC X(329).
000062
000063     12  EN-ISSUE-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000064         16  EN-INS-ORIG-LAST-NAME   PIC X(15).
000065         16  EN-INS-ORIG-FIRST-NAME  PIC X(10).
000066         16  EN-INS-ORIG-MIDDLE-INIT PIC X.
000067         16  EN-INS-ORIG-AGE         PIC S999     COMP-3.
000068         16  EN-JNT-ORIG-LAST-NAME   PIC X(15).
000069         16  EN-JNT-ORIG-FIRST-NAME  PIC X(10).
000070         16  EN-JNT-ORIG-MIDDLE-INIT PIC X.
000071         16  EN-JNT-ORIG-AGE         PIC S999     COMP-3.
000072         16  EN-LF-ORIG-BENCD        PIC XX.
000073         16  EN-LF-ORIG-TERM         PIC S999      COMP-3.
000074         16  EN-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000075         16  EN-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000076         16  EN-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
000077         16  EN-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
000078         16  EN-LF-ORIG-EXP-DT       PIC XX.
000079*        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
000080         16  EN-ORIG-CRED-BENE       PIC X(25).
000081         16  EN-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000082         16  FILLER                  PIC X.
000083         16  EN-AH-ORIG-BENCD        PIC XX.
000084         16  EN-AH-ORIG-TERM         PIC S999      COMP-3.
000085         16  EN-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000086         16  EN-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000087         16  EN-AH-ORIG-EXP-DT       PIC XX.
000088*        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
000089*        16  EN-AH-ORIG-WAIT-PER     PIC 99.
000090         16  EN-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000091         16  F                       PIC X(09).
000092         16  EN-AH-ORIG-CP           PIC 99.
000093
000094         16  EN-INS-NEW-LAST-NAME    PIC X(15).
000095         16  EN-INS-NEW-FIRST-NAME   PIC X(10).
000096         16  EN-INS-NEW-MIDDLE-INIT  PIC X.
000097         16  EN-INS-NEW-AGE          PIC S999     COMP-3.
000098         16  EN-JNT-NEW-LAST-NAME    PIC X(15).
000099         16  EN-JNT-NEW-FIRST-NAME   PIC X(10).
000100         16  EN-JNT-NEW-MIDDLE-INIT  PIC X.
000101         16  EN-JNT-NEW-AGE          PIC S999     COMP-3.
000102         16  EN-LF-NEW-BENCD         PIC XX.
000103         16  EN-LF-NEW-TERM          PIC S999      COMP-3.
000104         16  EN-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000105         16  EN-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000106         16  EN-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
000107         16  EN-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
000108         16  EN-LF-NEW-EXP-DT        PIC XX.
000109         16  EN-NEW-CRED-BENE        PIC X(25).
000110         16  EN-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000111         16  FILLER                  PIC X.
000112         16  EN-AH-NEW-BENCD         PIC XX.
000113         16  EN-AH-NEW-TERM          PIC S999      COMP-3.
000114         16  EN-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000115         16  EN-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000116         16  EN-AH-NEW-EXP-DT        PIC XX.
000117*        16  EN-AH-NEW-COV-TYPE      PIC X(10).
000118*        16  EN-AH-NEW-WAIT-PER      PIC 99.
000119*        16  F                       PIC X(12).
000120         16  EN-AH-NEW-CP            PIC 99.
000121         16  EN-SIG-SW               PIC X.
000122         16  EN-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000123         16  EN-INS-ORIG-AGE-DEF-FLAG PIC X.
000124         16  EN-JNT-ORIG-AGE-DEF-FLAG PIC X.
000125         16  EN-INS-NEW-AGE-DEF-FLAG PIC X.
000126         16  EN-JNT-NEW-AGE-DEF-FLAG PIC X.
000127         16  FILLER                  PIC X(33).
000128     12  EN-CANCEL-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000129         16  EN-LF-ORIG-REF-DT       PIC XX.
000130         16  EN-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000131         16  EN-AH-ORIG-REF-DT       PIC XX.
000132         16  EN-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000133         16  EN-LF-NEW-REF-DT        PIC XX.
000134         16  EN-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000135         16  EN-AH-NEW-REF-DT        PIC XX.
000136         16  EN-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000137         16  EN-FLAG-CERT            PIC X.
000138         16  EN-INS-LAST-NAME        PIC X(15).
000139         16  EN-INS-FIRST-NAME       PIC X(10).
000140         16  EN-INS-MIDDLE-INIT      PIC X.
000141         16  EN-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000142         16  EN-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000143         16  EN-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000144         16  EN-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000145         16  FILLER                  PIC X(262).
000146
000147     12  EN-MONEY-SW             PIC X.
000148     12  EN-HEALTH-APP           PIC X.
000149     12  EN-VOUCHER-SW           PIC X.
000150     12  EN-PAYEE                PIC X(14).
000151     12  EN-INPUT-DT             PIC XX.
000152     12  EN-PROCESS-DT           PIC XX.
000153     12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
000154     12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
000155
000156     12  EN-REASON-CODES.
000157         16  F OCCURS 12.
000158             20  EN-REASON-CODE  PIC X(4).
000159     12  EN-TEMPLATE-USED        PIC X(8).
000160     12  EN-DOCU-TYPE            PIC X.
000161         88  EN-VERI-DOCU          VALUE 'V'.
000162         88  EN-GCE-DOCU           VALUE 'G'.
000163         88  EN-CANC-DOCU          VALUE 'C'.
000164     12  EN-COMMENTS1            PIC X(13).
000165     12  EN-COMMENTS2            PIC X(70).
000166     12  EN-COMM-CHGBK           PIC X.
000167         88  EN-DO-NOT-CHG-ACCT    VALUE 'N'.
000168         88  EN-CHG-ACCT           VALUE 'Y'.
000169     12  EN-CSO-PORTION          PIC S9(5)V99  COMP-3.
000170     12  EN-ACCT-PORTION         PIC S9(5)V99  COMP-3.
000171     12  EN-BATCH-NUMBER         PIC X(6).
000172     12  EN-ACCT-SUMM            PIC X.
000173     12  EN-CSO-SUMM             PIC X.
000174     12  en-check-type           pic x.
000175     12  FILLER                  PIC X(12).
000176
000177******************************************************************
000178
      *<<((file: ERCENDT))
002337                                 EJECT
002338*    COPY ERCNOTE.
      *>>((file: ERCNOTE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCNOTE                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
000009*                                                                *
000010*        FILE TYPE= VSAM,KSDS                                    *
000011*        RECORD SIZE = 825    RECFORM = FIXED                    *
000012*                                                                *
000013*        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
000014*                                                                *
000015*        LOG = YES                                               *
000016*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000017*                                                                *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
000027*                                FILE WILL CONTAIN BILLING NOTES O
000028* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000029******************************************************************
000030
000031 01  CERTIFICATE-NOTE.
000032     12  CN-RECORD-ID                PIC  XX.
000033         88  VALID-CN-ID                  VALUE 'CN'.
000034
000035     12  CN-CONTROL-PRIMARY.
000036         16  CN-COMPANY-CD           PIC X.
000037         16  CN-CARRIER              PIC X.
000038         16  CN-GROUPING.
000039             20 CN-GROUPING-PREFIX   PIC XXX.
000040             20 CN-GROUPING-PRIME    PIC XXX.
000041         16  CN-STATE                PIC XX.
000042         16  CN-ACCOUNT.
000043             20 CN-ACCOUNT-PREFIX    PIC X(4).
000044             20 CN-ACCOUNT-PRIME     PIC X(6).
000045         16  CN-CERT-EFF-DT          PIC XX.
000046         16  CN-CERT-NO.
000047             20  CN-CERT-PRIME       PIC X(10).
000048             20  CN-CERT-SFX         PIC X.
000049         16  CN-RECORD-TYPE          PIC X.
000050             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
000051             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
000052     12  CN-BILLING-START-LINE-NO    PIC 99.
000053     12  CN-BILLING-END-LINE-NO      PIC 99.
000054
000055     12  CN-LINES.
000056         16  CN-LINE OCCURS 10       PIC X(77).
000057
000058     12  CN-LAST-MAINT-DT            PIC XX.
000059     12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000060     12  CN-LAST-MAINT-USER          PIC X(4).
000061     12  FILLER                      PIC X(5).
000062******************************************************************
      *<<((file: ERCNOTE))
002339*    COPY ELCEOBC.
      *>>((file: ELCEOBC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEOBC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM EOB CODE TABLE                                  *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000010*   VSAM EOB CODE TABLE                                          *
000011*                                                                *
000012*   FILE DESCRIPTION = EOB CODE TABLE                            *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
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
000031* 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
000033* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
000034******************************************************************
000035
000036 01  EOB-CODES.
000037     12  EO-RECORD-ID                      PIC XX.
000038         88  VALID-DN-ID                      VALUE 'EO'.
000039
000040     12  EO-CONTROL-PRIMARY.
000041         16  EO-COMPANY-CD                 PIC X.
000042         16  EO-RECORD-TYPE                PIC X.
000043             88  EO-EOB-RECS                  VALUE '1'.
000044             88  EO-VERIF-RECS                VALUE '2'.
000045             88  EO-GCE-RECS                  VALUE '3'.
000046             88  EO-CANC-RECS                 VALUE '4'.
000047             88  EO-BILL-NOTE-RECS            VALUE '5'.
000048         16  EO-EOB-CODE                   PIC X(4).
000049         16  FILLER                        PIC X(9).
000050
000051     12  EO-MAINT-INFORMATION.
000052         16  EO-LAST-MAINT-DT              PIC XX.
000053         16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000054         16  EO-LAST-MAINT-USER            PIC X(4).
000055         16  FILLER                        PIC XX.
000056
000057     12  EO-DESCRIPTION                    PIC X(275).
000058     12  FILLER                            PIC X(46).
000059******************************************************************
      *<<((file: ELCEOBC))
002340                                 EJECT
002341*    COPY ELCENCC.
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
002342                                 EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL689' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
002343 VCOBOL-DUMMY-PROCEDURE.
002344     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
002345
002346     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
002347     MOVE '5'                    TO DC-OPTION-CODE.
002348     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
002349     MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
002350     MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE
002351                                    W-CURRENT-SAVE.
002352     MOVE DC-GREG-DATE-A-EDIT    TO W-SAVE-EDIT-A-DATE
002353     MOVE DC-DAY-OF-WEEK         TO W-SAVE-CYCLE-DAY-OF-WEEK
002354
002355
002356     MOVE 2                      TO EMI-NUMBER-OF-LINES.
002357     MOVE ERROR-MESSAGE-INTERFACE-BLOCK
002358                                 TO EMI-SAVE-AREA.
002359
002360     MOVE EIBTRMID               TO W-TS-TERM-TEXT
002361                                    W-TS-TERM-SCREEN.
002362
002363     IF  EIBCALEN EQUAL 0
002364         MOVE UNACCESS-MSG       TO LOGOFF-MSG
002365         GO TO 8300-SEND-TEXT
002366     END-IF.
002367
002368     set P to address of KIXSYS
002369     CALL "getenv" using by value P returning var-ptr
002370     if var-ptr = null then
002371        display ' kixsys not set '
002372     else
002373        set address of var to var-ptr
002374        move 0 to env-var-len
002375        inspect var tallying env-var-len
002376          for characters before X'00'
002377*       DISPLAY '  KIXSYS = ' var (1:env-var-len)
002378        unstring var (1:env-var-len) delimited by '/'
002379           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
002380              WS-KIX-SYS
002381        end-unstring
002382*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
002383*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
002384     end-if
002385
002386     set P to address of KIXHOST
002387     CALL "getenv" using by value P returning var-ptr
002388     if var-ptr = null then
002389        display ' kixhost not set '
002390     else
002391        set address of var to var-ptr
002392        move 0 to env-var-len
002393        inspect var tallying env-var-len
002394          for characters before X'00'
002395*       DISPLAY '  KIXHOST = ' var (1:env-var-len)
002396        MOVE var(1:env-var-len)  to ws-kixhost
002397*       unstring var (1:env-var-len) delimited by '/'
002398*          into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
002399*             WS-KIX-SYS
002400*       end-unstring
002401        DISPLAY ' WS KIX HOST ' WS-KIXSYS
002402*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
002403     end-if
002404
002405
002406**** This routine will connect to the Logic Database on SQL Server
002407**** and call a stored procedure to determine the next business da
002408
002409     PERFORM 4500-CONNECT-TO-DB  THRU 4500-EXIT
002410     IF SQLCODE = 0
002411        PERFORM 4600-GET-NEXT-BUS-DT  THRU 4600-EXIT
002412        IF SQLCODE = 0
002413            MOVE WS-NEXT-BUS-DT TO W-SAVE-NEXT-BUS-DT-EDIT-A
002414                                   W-EDIT-A-DATE
002415            MOVE W-EDIT-A-YY    TO DC-YMD-YEAR
002416            MOVE W-EDIT-A-MM    TO DC-YMD-MONTH
002417            MOVE W-EDIT-A-DD    TO DC-YMD-DAY
002418            MOVE '3'            TO DC-OPTION-CODE
002419            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
002420            IF NO-CONVERSION-ERROR
002421               MOVE DC-BIN-DATE-1 TO W-SAVE-BIN-NEXT-BUS-DT
002422            ELSE
002423               MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
002424               MOVE '6'         TO DC-OPTION-CODE
002425               MOVE ZEROS       TO DC-ELAPSED-MONTHS
002426               IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
002427                  MOVE 3        TO DC-ELAPSED-DAYS
002428               ELSE
002429                 IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
002430                    MOVE 2     TO DC-ELAPSED-DAYS
002431                 ELSE
002432                    MOVE 1     TO DC-ELAPSED-DAYS
002433                 END-IF
002434               END-IF
002435               PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
002436               IF NO-CONVERSION-ERROR
002437                   MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
002438                   MOVE DC-GREG-DATE-A-EDIT TO
002439                                W-SAVE-NEXT-BUS-DT-EDIT-A
002440               ELSE
002441                   MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
002442                   MOVE W-SAVE-EDIT-A-DATE TO
002443                                W-SAVE-NEXT-BUS-DT-EDIT-A
002444               END-IF
002445            END-IF
002446        ELSE
002447            MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
002448            MOVE '6'         TO DC-OPTION-CODE
002449            MOVE ZEROS       TO DC-ELAPSED-MONTHS
002450            IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
002451               MOVE 3        TO DC-ELAPSED-DAYS
002452            ELSE
002453              IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
002454                 MOVE 2     TO DC-ELAPSED-DAYS
002455              ELSE
002456                 MOVE 1     TO DC-ELAPSED-DAYS
002457              END-IF
002458            END-IF
002459            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
002460            IF NO-CONVERSION-ERROR
002461                MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
002462                MOVE DC-GREG-DATE-A-EDIT TO
002463                                W-SAVE-NEXT-BUS-DT-EDIT-A
002464            ELSE
002465                MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
002466                MOVE W-SAVE-EDIT-A-DATE TO
002467                                W-SAVE-NEXT-BUS-DT-EDIT-A
002468            END-IF
002469        END-IF
002470     ELSE
002471        MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
002472        MOVE '6'         TO DC-OPTION-CODE
002473        MOVE ZEROS       TO DC-ELAPSED-MONTHS
002474        IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
002475           MOVE 3        TO DC-ELAPSED-DAYS
002476        ELSE
002477          IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
002478             MOVE 2     TO DC-ELAPSED-DAYS
002479          ELSE
002480             MOVE 1     TO DC-ELAPSED-DAYS
002481          END-IF
002482        END-IF
002483        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
002484        IF NO-CONVERSION-ERROR
002485            MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
002486            MOVE DC-GREG-DATE-A-EDIT TO
002487                                W-SAVE-NEXT-BUS-DT-EDIT-A
002488        ELSE
002489            MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
002490            MOVE W-SAVE-EDIT-A-DATE TO
002491                                W-SAVE-NEXT-BUS-DT-EDIT-A
002492        END-IF
002493     END-IF
002494     PERFORM 4700-DISCONNECT THRU 4700-EXIT
002495
002496     IF  PI-689-CREATE-NO-SCREENS
002497         GO TO 0600-BYPASS-SCREEN-CNTL
002498     END-IF.
002499
002500     IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
002501         IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
002502             MOVE PI-SAVED-PROGRAM-5
002503                                 TO PI-SAVED-PROGRAM-6
002504             MOVE PI-SAVED-PROGRAM-4
002505                                 TO PI-SAVED-PROGRAM-5
002506             MOVE PI-SAVED-PROGRAM-3
002507                                 TO PI-SAVED-PROGRAM-4
002508             MOVE PI-SAVED-PROGRAM-2
002509                                 TO PI-SAVED-PROGRAM-3
002510             MOVE PI-SAVED-PROGRAM-1
002511                                 TO PI-SAVED-PROGRAM-2
002512             MOVE PI-RETURN-TO-PROGRAM
002513                                 TO PI-SAVED-PROGRAM-1
002514             MOVE PI-CALLING-PROGRAM
002515                                 TO PI-RETURN-TO-PROGRAM
002516             MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
002517             PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
002518             MOVE LOW-VALUES     TO EL689AO
002519             PERFORM 9905-INITIALIZE-SECURITY THRU 9905-EXIT
002520
002521             IF PI-689-ARCHIVE-NUMBER NOT NUMERIC
002522                MOVE ZEROS       TO PI-689-ARCHIVE-NUMBEr
002523             END-IF
002524
002525             IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6311'
002526                     AND
002527                 PI-689-KEY-DATA-FIELDS GREATER THAN LOW-VALUES
002528                 GO TO 0400-EL6311-INCOMING
002529             ELSE
002530                 MOVE PI-689-ARCHIVE-NUMBER
002531                                 TO W-INCOMING-ARCHIVE
002532                 MOVE LOW-VALUES TO PI-1042-WA
002533                                    PI-689-WORK-AREA
002534                 MOVE ZEROS      TO PI-CURRENT-LINE
002535                                    PI-TEMP-STOR-ITEMS
002536                                    PI-TOTAL-LINES
002537                                    PI-UPDATE-SW
002538                                    PI-689-NUMBER-LABEL-LINES
002539                                    PI-689-NUMBER-TEXT-RECORDS
002540                                    PI-689-ERROR
002541                 MOVE SPACES     TO PI-689-PRINT-SW
002542                                    PI-689-ALT-PRINTER-ID
002543                                    PI-689-LBL-OVERRIDE
002544                                    PI-COMM-CONTROL
002545                                    PI-689-FORM-NUMBER
002546                                    PI-689-TEMP-STOR-ID
002547                                    PI-689-LABEL-SOURCE
002548                                    PI-689-USE-SCREEN-IND
002549                 MOVE '2'        TO PI-ACTION
002550                 MOVE -1         TO MAINTL
002551                 MOVE PI-LOWER-CASE-LETTERS
002552                                 TO W-LC-CASE-IND
002553                 IF  W-INCOMING-ARCHIVE NUMERIC
002554                         AND
002555                     W-INCOMING-ARCH-NO GREATER THAN ZEROS
002556                     MOVE 'S'    TO MAINTI
002557                     MOVE +1     TO MAINTL
002558                     MOVE AL-UANON
002559                                 TO MAINTA
002560                     MOVE W-INCOMING-ARCH-NO
002561                                 TO ARCHNUMI
002562                     MOVE +8     TO ARCHNUML
002563                     MOVE AL-UNNON
002564                                 TO ARCHNUMA
002565                     GO TO 1000-SHOW
002566                 ELSE
002567                     GO TO 0450-SPECIAL-INPUT-PROCESS
002568                 END-IF
002569             END-IF
002570         ELSE
002571             MOVE PI-CALLING-PROGRAM TO W-CALL-PGM
002572             MOVE PI-RETURN-TO-PROGRAM
002573                                     TO PI-CALLING-PROGRAM
002574             MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM
002575             MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1
002576             MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2
002577             MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3
002578             MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4
002579             MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5
002580             MOVE SPACES             TO PI-SAVED-PROGRAM-6
002581             PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
002582             MOVE LOW-VALUES         TO EL689AO
002583             PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
002584             SET W-RG-NDX            TO PI-CURRENT-LINE
002585             PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
002586                 VARYING
002587                     W-SC-NDX FROM 1 BY 1
002588                 UNTIL
002589                     W-SC-NDX GREATER THAN
002590                     W-NUM-LINES-PER-SCREEN
002591             MOVE PI-689-ALT-PRINTER-ID
002592                                     TO PRINTERO
002593             MOVE AL-UANON           TO PRINTERA
002594             MOVE -1                 TO PRINTERL
002595             GO TO 8100-SEND-INITIAL-MAP
002596         END-IF
002597     END-IF.
002598
002599                                 EJECT
002600 0200-RECEIVE.
002601
002602     MOVE PI-689-FATAL-CTR       TO EMI-FATAL-CTR
002603     MOVE PI-689-FORCABLE-CTR    TO EMI-FORCABLE-CTR
002604
002605     IF  EMI-FATAL-CTR GREATER THAN ZEROS
002606             OR
002607         EMI-FORCABLE-CTR GREATER THAN ZEROS
002608         MOVE 'Y'                TO W-HOLD-IND.
002609
002610     MOVE W-TRANSACTION          TO EIBTRNID.
002611     MOVE EMI-SAVE-AREA          TO ERROR-MESSAGE-INTERFACE-BLOCK.
002612     MOVE ZEROS                  TO W-LAST-ERROR.
002613     MOVE LOW-VALUES             TO EL689AI.
002614     MOVE '104A'                 TO W-TS-ID-TEXT.
002615     MOVE EIBTRMID               TO W-TS-TERM-TEXT
002616                                    W-TS-TERM-SCREEN.
002617
002618     
      * EXEC CICS HANDLE AID
002619*        CLEAR    (9300-DFHCLEAR)
002620*        PA1      (9200-PA)
002621*        PA2      (9200-PA)
002622*        PA3      (9200-PA)
002623*    END-EXEC.
      *    MOVE '"&=!"#               V! " #00008768' TO DFHEIV0
           MOVE X'22263D212223202020202020' &
                X'202020202020202020562120' &
                X'2220233030303038373638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002624
002625     
      * EXEC CICS HANDLE CONDITION
002626*        PGMIDERR (9700-PGMID-ERROR)
002627*        ERROR    (9800-ABEND)
002628*        MAPFAIL  (0325-MAPFAIL)
002629*    END-EXEC.
      *    MOVE '"$L.?                 ! # #00008775' TO DFHEIV0
           MOVE X'22244C2E3F20202020202020' &
                X'202020202020202020202120' &
                X'2320233030303038373735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002630
002631     
      * EXEC CICS SYNCPOINT
002632*    END-EXEC.
      *    MOVE '6"                    !   #00008781' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303038373831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002633
002634     IF PI-LOWER-CASE-LETTERS = 'Y'
002635*    IF  W-LC-USE-BOTH-CASES
002636         
      * EXEC CICS RECEIVE
002637*            MAP      (W-MAP)
002638*            MAPSET   (W-MAPSET)
002639*            INTO     (EL689AI)
002640*            ASIS
002641*        END-EXEC
           MOVE LENGTH OF
            EL689AI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00008786' TO DFHEIV0
           MOVE X'382254414920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038373836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002642     ELSE
002643         
      * EXEC CICS RECEIVE
002644*            MAP      (W-MAP)
002645*            MAPSET   (W-MAPSET)
002646*            INTO     (EL689AI)
002647*        END-EXEC.
           MOVE LENGTH OF
            EL689AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00008793' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038373933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002648
002649     IF  NOT DISPLAY-CAP
002650         MOVE 'READ'             TO SM-READ
002651         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
002652         MOVE ER-9097            TO EMI-ERROR
002653         MOVE -1                 TO MAINTL
002654         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002655         GO TO 8100-SEND-INITIAL-MAP.
002656
002657     INSPECT MAINTI CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
002658
002659     IF  ENTERPFL EQUAL 0
002660         GO TO 0300-CHECK-PFKEYS.
002661
002662     IF  EIBAID NOT EQUAL DFHENTER
002663         MOVE ER-0004            TO EMI-ERROR
002664         GO TO 0320-INPUT-ERROR.
002665
002666     IF  ENTERPFI NUMERIC
002667             AND
002668         ENTERPFI GREATER THAN 0
002669             AND
002670         ENTERPFI LESS THAN 25
002671         MOVE PF-VALUES (ENTERPFI)
002672                                 TO EIBAID
002673     ELSE
002674         MOVE ER-0029            TO EMI-ERROR
002675         GO TO 0320-INPUT-ERROR.
002676
002677 0300-CHECK-PFKEYS.
002678
002679     IF  EIBAID EQUAL DFHPF23
002680         PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
002681         MOVE ZEROS              TO PI-TOTAL-LINES
002682                                    PI-CURRENT-LINE
002683         MOVE EIBAID             TO PI-ENTRY-CD-1
002684         MOVE W-XCTL-005         TO W-CALL-PGM
002685         GO TO 9400-XCTL.
002686
002687     IF  EIBAID EQUAL DFHPF24
002688         PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
002689         MOVE ZEROS              TO PI-TOTAL-LINES
002690                                    PI-CURRENT-LINE
002691         MOVE W-XCTL-626         TO W-CALL-PGM
002692         GO TO 9400-XCTL.
002693
002694     MOVE SPACES                 TO W-KEY-FIELDS-CHANGED-IND
002695                                    W-FORM-CHANGED-IND.
002696
002697     IF  PI-689-KEY-DATA-FIELDS EQUAL LOW-VALUES OR SPACES
002698             OR
002699         PI-SHOW-MODE
002700         NEXT SENTENCE
002701     ELSE
002702         IF  PI-689-FORM-NUMBER GREATER THAN SPACES
002703                 AND
002704             FORML NOT EQUAL ZEROS
002705                 AND
002706             PI-689-FORM-NUMBER NOT EQUAL FORMI
002707             MOVE FORMI          TO PI-689-FORM-NUMBER
002708             MOVE LOW-VALUES     TO EL689AI
002709                                    PI-1042-WA
002710                                    PI-689-FOLLOW-UP-DATE
002711                                    PI-689-PRINT-RESTRICTION
002712                                    PI-689-RESEND-DATE-1
002713                                    PI-689-RESEND1-EDIT
002714                                    PI-689-FOLLOW-UP-DATE
002715                                    PI-689-FOLLOW-UP-EDIT
002716                                    PI-689-ERROR-IND
002717             MOVE ZEROS          TO PI-689-ERROR
002718                                    PI-689-NUMBER-COPIES
002719                                    PI-689-NUMBER-LABEL-LINES
002720                                    PI-689-NUMBER-TEXT-RECORDS
002721                                    PI-CURRENT-LINE
002722                                    PI-TEMP-STOR-ITEMS
002723                                    PI-TOTAL-LINES
002724                                    PI-UPDATE-SW
002725             MOVE SPACES         TO PI-689-PRINT-SW
002726                                    PI-689-ALT-PRINTER-ID
002727                                    PI-COMM-CONTROL
002728                                    PI-689-TEMP-STOR-ID
002729                                    PI-689-USE-SCREEN-IND
002730                                    PI-689-RESEND-LETR-1
002731                                    PI-CERT-FORM-ID
002732                                    PI-CERT-REQ-IND
002733                                    PI-REASON-REQ-IND
002734             PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
002735             MOVE DFHENTER       TO EIBAID
002736             MOVE 'Y'            TO W-FORM-CHANGED-IND
002737             GO TO 0330-FUNCTION-CHECK
002738         ELSE
002739             IF  PI-689-DATA-SOURCE NOT EQUAL DATASORI
002740                     OR
002741                 (PI-689-LABEL-SOURCE NOT EQUAL ADDRSI
002742                         AND
002743                     (PI-689-LABEL-SOURCE NOT EQUAL SPACES
002744                             OR
002745                         ADDRSI NOT EQUAL LOW-VALUES))
002746                     OR
002747                 PI-689-CARRIER NOT EQUAL CARRIERI
002748                     OR
002749                 PI-689-GROUPING NOT EQUAL GROUPI
002750                     OR
002751                 PI-689-STATE NOT EQUAL STATEI
002752                     OR
002753                 PI-689-ACCOUNT NOT EQUAL ACCTI
002754                     OR
002755                 PI-689-CERT-PRIME NOT EQUAL CERTI
002756                     OR
002757                 PI-689-CERT-SFX NOT EQUAL SFXI
002758                     OR
002759                 PI-689-TYPE NOT EQUAL TYPEI
002760                     OR
002761                 PI-689-DATE-EDIT NOT EQUAL DATEI
002762                     OR
002763                 PI-689-RESP-PERSON NOT EQUAL RPERSONI
002764                     OR
002765                 PI-689-SEQ-EDIT NOT EQUAL SEQI
002766                     OR
002767                 PI-689-ENTRY-BATCH NOT EQUAL BENTRYI
002768                     OR
002769                 PI-689-BCSEQ-EDIT NOT EQUAL BCSEQI
002770                 MOVE 'Y'        TO W-KEY-FIELDS-CHANGED-IND
002771                 MOVE DFHENTER   TO EIBAID
002772                 GO TO 0330-FUNCTION-CHECK.
002773
002774*    IF  EIBAID EQUAL DFHPF1
002775*        MOVE +7                 TO W-ROLL-COUNTER
002776*        GO TO 4000-ROLL-PAGE.
002777
002778*    IF  EIBAID EQUAL DFHPF2
002779*        MOVE -7                 TO W-ROLL-COUNTER
002780*        GO TO 4000-ROLL-PAGE.
002781
002782*    IF  EIBAID EQUAL DFHPF3
002783*        IF  W-HOLD-ON
002784*            MOVE ER-7245        TO EMI-ERROR
002785*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002786*            MOVE -1             TO MAINTL
002787*            MOVE AL-UABON       TO MAINTA
002788*            GO TO 8200-SEND-DATAONLY
002789*        ELSE
002790*            GO TO 0330-FUNCTION-CHECK.
002791
002792*    IF (EIBAID = DFHPF8)
002793*       AND (PI-PROCESSOR-ID = 'LMLC' OR 'SPJA' OR 'KAWA' OR
002794*         'CAGB' OR 'DLVA' OR 'ECCA' OR 'KRHA' OR 'ALWA')
002795*       MOVE ER-0029             TO EMI-ERROR
002796*       GO TO 0320-INPUT-ERROR
002797*    END-IF
002798
002799*    IF EIBAID = DFHPF4 OR DFHPF8
002800*        IF  W-HOLD-ON
002801*            MOVE ER-7245        TO EMI-ERROR
002802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002803*            MOVE -1             TO MAINTL
002804*            MOVE AL-UABON       TO MAINTA
002805*            GO TO 8200-SEND-DATAONLY
002806*        ELSE
002807*            GO TO 0330-FUNCTION-CHECK.
002808
002809*    IF  EIBAID = DFHPF5
002810*        COMPUTE W-ROLL-COUNTER = ((PI-TOTAL-LINES - 1) * -1)
002811*        GO TO 4000-ROLL-PAGE.
002812
002813*    IF  EIBAID = DFHPF6
002814*        MOVE PI-TOTAL-LINES     TO W-ROLL-COUNTER
002815*        GO TO 4000-ROLL-PAGE.
002816
002817     IF  EIBAID EQUAL DFHPF7
002818         IF PI-PROMPT-IND EQUAL 'Y'
002819             MOVE ER-0894        TO EMI-ERROR
002820             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002821             MOVE -1             TO MAINTL
002822             MOVE AL-UABON       TO MAINTA
002823             GO TO 8200-SEND-DATAONLY
002824         END-IF
002825     END-IF.
002826
002827     IF  EIBAID EQUAL DFHPF7 or dfhpf8
002828         IF  W-HOLD-ON
002829             MOVE ER-7245        TO EMI-ERROR
002830             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002831             MOVE -1             TO MAINTL
002832             MOVE AL-UABON       TO MAINTA
002833             GO TO 8200-SEND-DATAONLY
002834         ELSE
002835             GO TO 0330-FUNCTION-CHECK.
002836
002837     IF  EIBAID EQUAL DFHENTER
002838         GO TO 0330-FUNCTION-CHECK.
002839
002840     MOVE ER-0029                TO EMI-ERROR.
002841
002842 0320-INPUT-ERROR.
002843
002844     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002845
002846     IF  ENTERPFL EQUAL 0
002847         MOVE -1                 TO MAINTL
002848         MOVE AL-UABON           TO MAINTA
002849     ELSE
002850         MOVE AL-UNBON           TO ENTERPFA
002851         MOVE -1                 TO ENTERPFL.
002852
002853     GO TO 8200-SEND-DATAONLY.
002854
002855 0325-MAPFAIL.
002856***********************************************************
002857*      ROUTINE SHOULD ONLY BE PERFORMED WHEN PRINTING     *
002858*      LETTERS ON A 3275 PRINTER.                         *
002859***********************************************************
002860
002861     PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
002862
002863     SET W-RG-NDX                TO PI-CURRENT-LINE.
002864     PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
002865             VARYING
002866         W-SC-NDX FROM 1 BY 1
002867             UNTIL
002868         W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
002869
002870     MOVE -1                     TO MAINTL.
002871     GO TO 8100-SEND-INITIAL-MAP.
002872                                 EJECT
002873 0330-FUNCTION-CHECK.
002874
002875     IF  MAINTI EQUAL 'S'
002876*        IF EIBAID = DFHPF4 OR DFHPF8
002877*            GO TO 7900-PRINT-LETTER-NOW
002878*        ELSE
002879             GO TO 1000-SHOW.
002880
002881     IF  NOT MODIFY-CAP
002882         MOVE 'UPDATE'           TO SM-READ
002883         PERFORM 9995-SECURITY-VIOLATION
002884         MOVE ER-0070            TO EMI-ERROR
002885         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002886         MOVE -1                 TO MAINTL
002887         GO TO 8100-SEND-INITIAL-MAP.
002888
002889     IF  W-FORM-CHANGED
002890             OR
002891         W-KEY-FIELDS-CHANGED
002892         GO TO 0330-EDIT.
002893
002894     PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
002895
002896
002897*    IF  EIBAID EQUAL DFHPF3
002898*        IF  PI-NO-CARRIER-SECURITY
002899*                AND
002900*            PI-NO-ACCOUNT-SECURITY
002901*            GO TO 5000-EDIT-MODE.
002902
002903     IF  EIBAID EQUAL DFHPF7 or dfhpf8
002904         IF  PI-NO-CARRIER-SECURITY
002905                 AND
002906             PI-NO-ACCOUNT-SECURITY
002907             GO TO 5400-LETTER-RELEASE
002908         ELSE
002909             IF  PI-689-PRINT-PERFORMED
002910                 GO TO 5400-LETTER-RELEASE
002911             ELSE
002912                 MOVE AL-UNBON TO ENTERPFA
002913                 MOVE -1       TO ENTERPFL
002914                 MOVE ER-2398  TO EMI-ERROR
002915                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002916                 GO TO 8200-SEND-DATAONLY.
002917
002918*    IF  EIBAID = DFHPF4 OR DFHPF8
002919*        GO TO 7900-PRINT-LETTER-NOW.
002920
002921 0330-EDIT.
002922
002923     IF  W-SC-TEXTL (1) NOT EQUAL ZEROS
002924             OR
002925         W-SC-TEXTL (2) NOT EQUAL ZEROS
002926             OR
002927         W-SC-TEXTL (3) NOT EQUAL ZEROS
002928             OR
002929         W-SC-TEXTL (4) NOT EQUAL ZEROS
002930             OR
002931         W-SC-TEXTL (5) NOT EQUAL ZEROS
002932             OR
002933         W-SC-TEXTL (6) NOT EQUAL ZEROS
002934             OR
002935         W-SC-TEXTL (7) NOT EQUAL ZEROS
002936             OR
002937         W-SC-TEXTL (8) NOT EQUAL ZEROS
002938             OR
002939         W-SC-TEXTL (9) NOT EQUAL ZEROS
002940             OR
002941         W-SC-TEXTL (10) NOT EQUAL ZEROS
002942             OR
002943         W-SC-TEXTL (11) NOT EQUAL ZEROS
002944         IF  W-KEY-FIELDS-CHANGED
002945                 OR
002946             W-FORM-CHANGED
002947             MOVE ER-7250        TO EMI-ERROR
002948             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002949             MOVE -1             TO MAINTL
002950             MOVE AL-UABON       TO MAINTA
002951         ELSE
002952             PERFORM 4100-SET-NDX THRU 4100-EXIT
002953             PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
002954                     VARYING
002955                 W-SC-NDX FROM 1 BY 1
002956                     UNTIL
002957                 W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
002958             MOVE ER-7398        TO EMI-ERROR
002959             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002960             MOVE -1             TO MAINTL
002961             MOVE AL-UABON       TO MAINTA
002962             GO TO 8200-SEND-DATAONLY.
002963
002964     PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
002965
002966     IF  W-KEY-FIELDS-CHANGED
002967             OR
002968         PI-689-KEY-DATA-FIELDS EQUAL LOW-VALUES OR SPACES
002969         PERFORM 0800-EDIT-KEY-FIELDS THRU 0800-EXIT
002970         IF  MAINTI EQUAL 'C'
002971             GO TO 2000-CREATE
002972         ELSE
002973             NEXT SENTENCE
002974     ELSE
002975         IF  W-FORM-CHANGED
002976             PERFORM 0800-EDIT-KEY-FIELDS THRU 0800-EXIT
002977             IF  MAINTI EQUAL 'C'
002978                 GO TO 2000-CREATE
002979             ELSE
002980                 NEXT SENTENCE
002981         ELSE
002982             MOVE -1             TO MAINTL
002983             GO TO 8200-SEND-DATAONLY.
002984
002985     MOVE ER-0023                TO EMI-ERROR.
002986     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002987     MOVE -1                     TO MAINTL.
002988     MOVE AL-UABON               TO MAINTA.
002989     GO TO 8200-SEND-DATAONLY.
002990                                 EJECT
002991 0350-EDIT-ROUTINE.
002992
002993     IF  MAINTI EQUAL 'C'
002994             AND
002995         FORML EQUAL ZEROS
002996         MOVE -1                 TO FORML
002997         MOVE ER-0177            TO EMI-ERROR
002998         MOVE AL-UABON           TO FORMA
002999         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003000
003001     IF PRTNOWL > ZEROS
003002        IF PRTNOWI = 'Y' OR 'N' OR 'P' OR ' '
003003           MOVE PRTNOWI          TO PI-PRINT-NOW
003004           MOVE AL-UANON         TO PRTNOWA
003005        END-IF
003006     END-IF
003007
003008     IF ENDARCHL > ZEROS
003009        IF ENDARCHI NUMERIC
003010            MOVE ENDARCHI       TO PI-ENDT-ARCH-NO
003011            MOVE AL-UNNON       TO ENDARCHA
003012            MOVE ENDARCHI       TO W-ERENDT-ARCHIVE
003013            MOVE PI-COMPANY-CD  TO W-ERENDT-COMPANY-CD-A1
003014            MOVE 'N'            TO W-VALID-ENDT-SW
003015            PERFORM 0355-CHECK-ERENDT THRU 0355-EXIT
003016            IF NOT W-VALID-ENDT
003017                MOVE -1         TO ENDARCHL
003018                MOVE ER-1565    TO EMI-ERROR
003019                MOVE AL-UNBON   TO ENDARCHA
003020                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003021            END-IF
003022        END-IF
003023     ELSE
003024        MOVE ZEROS              TO PI-ENDT-ARCH-NO
003025     END-IF
003026
003027     MOVE SPACES                 TO PI-689-PRINT-RESTRICTION
003028*    IF  PRTRESTL NOT EQUAL ZEROS
003029*        PERFORM 0360-EDIT-PRINT-RESTRICTIONS THRU 0360-EXIT.
003030
003031     IF  FOLLOWL NOT EQUAL ZEROS
003032         MOVE FOLLOWI            TO W-DEEDIT-FIELD
003033         PERFORM 8600-DEEDIT
003034         MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
003035         MOVE '4'                TO DC-OPTION-CODE
003036         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
003037         IF  DATE-CONVERSION-ERROR
003038             MOVE ER-0182        TO EMI-ERROR
003039             MOVE -1             TO FOLLOWL
003040             MOVE AL-UABON       TO FOLLOWA
003041             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003042         ELSE
003043             MOVE DC-GREG-DATE-1-EDIT
003044                                 TO FOLLOWO
003045                                    PI-689-FOLLOW-UP-EDIT
003046             MOVE AL-UANON       TO FOLLOWA
003047             MOVE DC-BIN-DATE-1  TO PI-689-FOLLOW-UP-DATE
003048     ELSE
003049         MOVE LOW-VALUES         TO PI-689-FOLLOW-UP-DATE.
003050
003051     IF  PI-689-FOLLOW-UP-DATE NOT EQUAL LOW-VALUES
003052         IF  PI-689-FOLLOW-UP-DATE LESS THAN
003053                 W-SAVE-BIN-DATE
003054             MOVE ER-0533        TO EMI-ERROR
003055             MOVE AL-UABON       TO FOLLOWA
003056             MOVE -1             TO FOLLOWL
003057             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003058
003059     IF  RESEND1L NOT EQUAL ZEROS
003060         MOVE RESEND1I           TO W-DEEDIT-FIELD
003061         PERFORM 8600-DEEDIT
003062         MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
003063         MOVE '4'                TO DC-OPTION-CODE
003064         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
003065         IF  DATE-CONVERSION-ERROR
003066             MOVE ER-0185        TO EMI-ERROR
003067             MOVE -1             TO RESEND1L
003068             MOVE AL-UABON       TO RESEND1A
003069             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003070         ELSE
003071             MOVE DC-GREG-DATE-1-EDIT
003072                                 TO RESEND1O
003073                                    PI-689-RESEND1-EDIT
003074             MOVE AL-UANON       TO RESEND1A
003075             MOVE DC-BIN-DATE-1  TO PI-689-RESEND-DATE-1
003076     ELSE
003077         MOVE LOW-VALUES         TO PI-689-RESEND-DATE-1.
003078
003079     IF  PI-689-RESEND-DATE-1 NOT EQUAL LOW-VALUES
003080         IF  PI-689-RESEND-DATE-1 NOT GREATER THAN
003081                 W-SAVE-BIN-DATE
003082             MOVE ER-0537        TO EMI-ERROR
003083             MOVE AL-UABON       TO RESEND1A
003084             MOVE -1             TO RESEND1L
003085             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003086
003087     IF ENCL > ZEROS
003088        MOVE FUNCTION UPPER-CASE(ENCI) TO ENCI
003089                                          PI-ENCLOSURE-CD
003090        MOVE SPACES             TO W-ELENCC-KEY
003091        MOVE PI-COMPANY-CD      TO W-ENCC-COMPANY-CD
003092        MOVE '2'                TO W-ENCC-REC-TYPE
003093        MOVE ENCI               TO W-ENCC-ENC-CODE
003094
003095        
      * EXEC CICS READ
003096*           DATASET    (W-ENCC-FILE-ID)
003097*           SET        (ADDRESS OF ENCLOSURE-CODES)
003098*           RIDFLD     (W-ELENCC-KEY)
003099*           RESP       (W-RESPONSE)
003100*       END-EXEC
      *    MOVE '&"S        E          (  N#00009245' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039323435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ENCC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003101
003102        IF RESP-NORMAL
003103           MOVE ENCI             TO PI-ENCLOSURE-CD
003104                                    W-ENCLOSURE-CD
003105           MOVE AL-UANON         TO ENCA
003106        ELSE
003107           MOVE ER-1560          TO EMI-ERROR
003108           MOVE -1               TO ENCL
003109           MOVE AL-UABON         TO ENCA
003110           PERFORM 9900-ERROR-FORMAT
003111                                 THRU 9900-EXIT
003112        END-IF
003113     END-IF
003114
003115
003116     IF NOT PI-CREATE-LABELS
003117         MOVE PI-LABEL-CONTROL   TO ADDRLBLI
003118         MOVE AL-PANON           TO ADDRLBLA
003119         GO TO 0350-BYPASS-OVERRIDE.
003120
003121     IF  ADDRLBLL GREATER THAN ZEROS
003122          INSPECT ADDRLBLI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
003123          IF ADDRLBLI = 'N' OR 'Y'
003124              MOVE ADDRLBLI      TO PI-689-LBL-OVERRIDE
003125              MOVE AL-UANON      TO ADDRLBLA
003126          ELSE
003127             MOVE ER-3783        TO EMI-ERROR
003128             MOVE -1             TO ADDRLBLL
003129             MOVE AL-UABON       TO ADDRLBLA
003130             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003131
003132 0350-BYPASS-OVERRIDE.
003133
003134     IF  PI-BYPASS-LABELS
003135             OR
003136         FORMI EQUAL '9999'
003137             OR
003138         PI-689-LABELS-OVERRIDEN
003139         MOVE SPACES             TO PI-689-LABEL-SOURCE
003140         MOVE LOW-VALUES         TO ADDRSI
003141         MOVE AL-UANOF           TO ADDRSA
003142         MOVE ZEROS              TO ADDRSL
003143     ELSE
003144         IF  ADDRSL GREATER THAN ZEROS
003145             MOVE ADDRSI         TO W-LABEL-SOURCE
003146             IF  W-LABEL-SOURCE-VALID
003147                 MOVE ADDRSI     TO PI-689-LABEL-SOURCE
003148             ELSE
003149                 MOVE -1         TO ADDRSL
003150                 MOVE ER-0176    TO EMI-ERROR
003151                 MOVE AL-UABON   TO ADDRSA
003152                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003153         ELSE
003154             IF  MAINTI EQUAL 'C'
003155                 MOVE -1         TO ADDRSL
003156                 MOVE ER-0176    TO EMI-ERROR
003157                 MOVE AL-UABON   TO ADDRSA
003158                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003159
003160     MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
003161     IF  PRINTERL GREATER ZERO
003162         INSPECT PRINTERI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
003163         MOVE AL-UANON           TO PRINTERA
003164         MOVE PRINTERI           TO PI-689-ALT-PRINTER-ID
003165                                    PI-ALT-DMD-PRT-ID
003166     ELSE
003167         IF  PI-NO-CARRIER-SECURITY
003168                 AND
003169             PI-NO-ACCOUNT-SECURITY
003170             IF  PI-PROCESSOR-PRINTER GREATER THAN SPACES
003171                 MOVE PI-PROCESSOR-PRINTER
003172                                 TO PI-689-ALT-PRINTER-ID
003173                                    PRINTERI
003174                 MOVE AL-UABON   TO PRINTERA
003175             ELSE
003176                 MOVE PI-COMPANY-ID
003177                                 TO W-CNTL-COMPANY-ID
003178                 MOVE '1'        TO W-CNTL-RECORD-TYPE
003179                 MOVE SPACES     TO W-CNTL-GENL
003180                 MOVE ZEROS      TO W-CNTL-SEQ-NO
003181                 
      * EXEC CICS HANDLE CONDITION
003182*                     NOTOPEN    (8040-CNTL-NOT-OPEN)
003183*                     NOTFND     (7945-NOT-FOUND)
003184*                END-EXEC
      *    MOVE '"$JI                  ! $ #00009331' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303039333331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003185                 
      * EXEC CICS READ
003186*                     DATASET    (W-CNTL-FILE-ID)
003187*                     SET        (ADDRESS OF CONTROL-FILE)
003188*                     RIDFLD     (W-CNTL-KEY)
003189*                END-EXEC
      *    MOVE '&"S        E          (   #00009335' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039333335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003190                 MOVE CF-FORMS-PRINTER-ID
003191                                 TO PI-689-ALT-PRINTER-ID
003192                                    PRINTERI
003193                                    W-TS-TERM-TEXT
003194                 MOVE AL-UABON   TO PRINTERA
003195         ELSE
003196             MOVE AL-UABON       TO PRINTERA
003197             MOVE -1             TO PRINTERL
003198             MOVE ER-9283        TO EMI-ERROR
003199             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003200
003201     IF  COPIESL NOT EQUAL ZEROS
003202         IF  COPIESI NOT NUMERIC
003203                 OR
003204             COPIESI EQUAL '0'
003205             MOVE ER-0184        TO EMI-ERROR
003206             MOVE -1             TO COPIESL
003207             MOVE AL-UABON       TO COPIESA
003208             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003209         ELSE
003210             MOVE COPIESI        TO PI-689-NUMBER-COPIES
003211             MOVE AL-UANON       TO COPIESA
003212     ELSE
003213         IF FORMI EQUAL '9999'
003214             MOVE 1              TO PI-689-NUMBER-COPIES
003215                                    COPIESL
003216                                    COPIESO.
003217
003218     IF  FORML NOT EQUAL ZEROS
003219         INSPECT FORMI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
003220         MOVE FORMI              TO PI-689-FORM-NUMBER.
003221
003222     MOVE SPACES                 TO W-HOLD-IND.
003223
003224     IF PI-CERT-REQ-IND = 'Y'
003225       IF  CERTIDL NOT EQUAL ZEROS
003226         IF CERTIDI NUMERIC
003227             MOVE AL-UANON      TO CERTIDA
003228             MOVE CERTIDI       TO PI-CERT-FORM-ID
003229         ELSE
003230             MOVE ER-1778       TO EMI-ERROR
003231             MOVE -1            TO CERTIDL
003232             MOVE AL-UABON      TO CERTIDA
003233             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003234         END-IF
003235       ELSE
003236          MOVE ER-0715       TO EMI-ERROR
003237          MOVE -1            TO CERTIDL
003238          MOVE AL-UABON      TO CERTIDA
003239          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003240       END-IF
003241     END-IF.
003242
003243     IF PI-REASON-REQ-IND = 'Y'
003244        IF PI-ENDT-ARCH-NO NOT > ZERO
003245          MOVE ER-9840       TO EMI-ERROR
003246          MOVE -1            TO FORML
003247          MOVE AL-UABON      TO FORMA
003248          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003249        END-IF
003250     END-IF.
003251
003252     IF  NOT EMI-NO-ERRORS
003253         IF  EMI-ERROR EQUAL ER-7250
003254             GO TO 0350-EXIT
003255         ELSE
003256            MOVE -1             TO MAINTL
003257            GO TO 8200-SEND-DATAONLY.
003258
003259 0350-EXIT.
003260      EXIT.
003261                                 EJECT
003262 0355-CHECK-ERENDT.
003263
003264*     EXEC CICS GETMAIN
003265*          SET      (ADDRESS OF ENDORSEMENT-RECORD)
003266*          LENGTH   (W-ENDT-LENGTH)
003267*     END-EXEC
003268
003269     
      * EXEC CICS READ
003270*         DATASET    ('ERENDT2')
003271*         SET        (ADDRESS OF ENDORSEMENT-RECORD)
003272*         RIDFLD     (W-ERENDT-KEY-BY-ARCH)
003273*         RESP       (W-RESPONSE)
003274*    END-EXEC
           MOVE 'ERENDT2' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009419' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039343139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ERENDT-KEY-BY-ARCH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENDORSEMENT-RECORD TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003275
003276     IF (RESP-NORMAL)
003277         IF PI-RETURN-TO-PROGRAM EQUAL 'EL6311'  AND
003278            EN-COMPANY-CD  =  PI-COMPANY-CD      AND
003279            EN-CARRIER     =  PI-689-CARRIER     AND
003280            EN-GROUPING    =  PI-689-GROUPING    AND
003281            EN-STATE       =  PI-689-STATE       AND
003282            EN-ACCOUNT     =  PI-689-ACCOUNT     AND
003283            EN-CERT-EFF-DT =  PI-689-EFF-DATE    AND
003284            EN-CERT-NO     =  PI-689-CERT-NO
003285               SET W-VALID-ENDT TO TRUE
003286         END-IF
003287         IF PI-RETURN-TO-PROGRAM EQUAL 'EL1273'  AND
003288            EN-COMPANY-CD  =  PI-COMPANY-CD      AND
003289            EN-CARRIER     =  PI-CARRIER         AND
003290            EN-GROUPING    =  PI-GROUPING        AND
003291            EN-STATE       =  PI-STATE           AND
003292            EN-ACCOUNT     =  PI-ACCOUNT         AND
003293            EN-CERT-EFF-DT =  PI-CERT-EFF-DT     AND
003294            EN-CERT-NO     =  PI-CERT-NO
003295               SET W-VALID-ENDT TO TRUE
003296         END-IF
003297     END-IF
003298      .
003299
003300 0355-EXIT.
003301      EXIT.
003302                                 EJECT
003303 0360-EDIT-PRINT-RESTRICTIONS.
003304
003305*    INSPECT PRTRESTI CONVERTING W-LOWER-CASE
003306*                                TO W-UPPER-CASE.
003307*
003308*    IF  (PI-689-PRINT-RESTRICTION EQUAL ' ' OR 'F'
003309*                                     OR 'P' OR LOW-VALUES)
003310*            AND
003311*        PRTRESTI EQUAL 'C'
003312*            AND
003313*        PI-689-CONTROL NOT GREATER THAN ZEROS
003314*        MOVE ER-7243            TO EMI-ERROR
003315*        MOVE -1                 TO PRTRESTL
003316*        MOVE AL-UABON           TO PRTRESTA
003317*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003318*        GO TO 0360-EXIT.
003319*
003320*    IF  PRTRESTL NOT EQUAL ZEROS
003321*        IF PRTRESTI = 'C' OR 'F' OR 'P'
003322*            MOVE PRTRESTI       TO PI-689-PRINT-RESTRICTION
003323*            MOVE AL-UANON       TO PRTRESTA
003324*        ELSE
003325*            IF  PRTRESTI EQUAL ' '
003326*                MOVE SPACES     TO PI-689-PRINT-RESTRICTION
003327*                MOVE AL-UANON   TO PRTRESTA
003328*            ELSE
003329*                MOVE ER-7393    TO EMI-ERROR
003330*                MOVE -1         TO PRTRESTL
003331*                MOVE AL-UABON   TO PRTRESTA
003332*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003333*
003334*    IF  PI-689-PRINT-RESTRICTION EQUAL 'C'
003335*            AND
003336*        W-HOLD-ON
003337*            AND
003338*        PI-689-CONTROL NOT GREATER THAN ZEROS
003339*        MOVE ER-7243            TO EMI-ERROR
003340*        MOVE -1                 TO PRTRESTL
003341*        MOVE AL-UABON           TO PRTRESTA
003342*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003343
003344     .
003345 0360-EXIT.
003346      EXIT.
003347                                 EJECT
003348 0400-EL6311-INCOMING.
003349
003350     MOVE ZEROS                  TO PI-CURRENT-LINE
003351                                    PI-TEMP-STOR-ITEMS
003352                                    PI-TOTAL-LINES
003353                                    PI-UPDATE-SW
003354                                    PI-689-NUMBER-LABEL-LINES
003355                                    PI-689-ERROR
003356                                    PI-689-NUMBER-TEXT-RECORDS.
003357     MOVE SPACES                 TO PI-689-PRINT-SW
003358                                    PI-689-ALT-PRINTER-ID
003359                                    PI-COMM-CONTROL
003360                                    PI-689-FORM-NUMBER
003361                                    PI-689-TEMP-STOR-ID
003362                                    PI-689-LABEL-SOURCE
003363                                    PI-689-USE-SCREEN-IND.
003364
003365     IF PI-CREATE-LABELS
003366         MOVE  AL-UANON          TO ADDRLBLA
003367         MOVE  +1                TO ADDRLBLL.
003368
003369     MOVE PI-LABEL-CONTROL       TO ADDRLBLI
003370                                    PI-689-LBL-OVERRIDE.
003371
003372     MOVE PI-LOWER-CASE-LETTERS  TO W-LC-CASE-IND.
003373     MOVE '2'                    TO PI-ACTION.
003374     MOVE -1                     TO MAINTL.
003375
003376     MOVE AL-UANON               TO PRTNOWA.
003377     MOVE +1                     TO PRTNOWL.
003378     MOVE 'N'                    TO PRTNOWI
003379                                    PI-PRINT-NOW.
003380
003381     IF  PI-689-CARRIER NOT EQUAL LOW-VALUES
003382         MOVE AL-UANON           TO CARRIERA
003383         MOVE +1                 TO CARRIERL
003384         MOVE PI-689-CARRIER     TO CARRIERI.
003385
003386     IF  PI-689-GROUPING NOT EQUAL LOW-VALUES
003387         MOVE +6                 TO GROUPL
003388         MOVE AL-UANON           TO GROUPA
003389         MOVE PI-689-GROUPING    TO GROUPI.
003390
003391     IF  PI-689-STATE NOT EQUAL LOW-VALUES
003392         MOVE +2                 TO STATEL
003393         MOVE AL-UANON           TO STATEA
003394         MOVE PI-689-STATE       TO STATEI.
003395
003396     IF  PI-689-ACCOUNT NOT EQUAL LOW-VALUES
003397         MOVE +10                TO ACCTL
003398         MOVE AL-UANON           TO ACCTA
003399         MOVE PI-689-ACCOUNT     TO ACCTI.
003400
003401     IF  PI-689-CERT-PRIME NOT EQUAL LOW-VALUES
003402         MOVE +10                TO CERTL
003403         MOVE AL-UANON           TO CERTA
003404         MOVE PI-689-CERT-PRIME  TO CERTI.
003405
003406     IF  PI-689-CERT-SFX NOT EQUAL LOW-VALUES
003407         MOVE +1                 TO SFXL
003408         MOVE AL-UANON           TO SFXA
003409         MOVE PI-689-CERT-SFX    TO SFXI.
003410
003411     IF  PI-689-ENTRY-BATCH NOT EQUAL LOW-VALUES
003412         MOVE +6                 TO BENTRYL
003413         MOVE AL-UANON           TO BENTRYA
003414         MOVE PI-689-ENTRY-BATCH TO BENTRYI.
003415
003416     MOVE +4                     TO BCSEQL.
003417     MOVE AL-UANON               TO BCSEQA.
003418     MOVE PI-689-CHG-SEQ-NO      TO BCSEQI.
003419
003420     IF  PI-689-SEQ-NO NOT EQUAL ZEROS
003421         MOVE +8                 TO SEQL
003422         MOVE AL-UANON           TO SEQA
003423         MOVE PI-689-SEQ-NO      TO SEQI.
003424
003425     IF  PI-689-EFF-DATE NOT EQUAL LOW-VALUES
003426         MOVE PI-689-EFF-DATE    TO DC-BIN-DATE-1
003427         MOVE ' '                TO DC-OPTION-CODE
003428         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
003429         MOVE +8                 TO DATEL
003430         MOVE DC-GREG-DATE-1-EDIT
003431                                 TO DATEI
003432         MOVE AL-UANON           TO DATEA.
003433
003434     MOVE -1                     TO MAINTL.
003435     MOVE '4'                    TO DATASORI
003436                                    PI-689-DATA-SOURCE.
003437     MOVE +1                     TO DATASORL.
003438     MOVE AL-UANON               TO DATASORA.
003439
003440     GO TO 8100-SEND-INITIAL-MAP.
003441
003442 0400-EXIT.
003443      EXIT.
003444                                 EJECT
003445 0450-SPECIAL-INPUT-PROCESS.
003446
003447     IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6501'
003448             AND
003449         PI-CARRIER GREATER THAN LOW-VALUES
003450         MOVE '1'                TO DATASORI
003451                                    PI-689-DATA-SOURCE
003452         MOVE PI-CARRIER         TO CARRIERI
003453         MOVE PI-GROUPING        TO GROUPI
003454         MOVE PI-STATE           TO STATEI
003455         MOVE PI-ACCOUNT         TO ACCTI
003456         MOVE PI-CERT-EFF-DT     TO DC-BIN-DATE-1
003457         MOVE ' '                TO DC-OPTION-CODE
003458         PERFORM 9500-LINK-DATE-CONVERT
003459             THRU 9500-EXIT
003460         MOVE DC-GREG-DATE-1-EDIT
003461                                 TO DATEI
003462         MOVE 'N'                TO PRTNOWI
003463                                    PI-PRINT-NOW
003464         MOVE AL-UANON           TO ACCTA
003465                                    CARRIERA
003466                                    DATEA
003467                                    DATASORA
003468                                    GROUPA
003469                                    STATEA
003470                                    PRTNOWA
003471         MOVE +1                 TO CARRIERL
003472                                    DATASORL
003473                                    PRTNOWL
003474         MOVE -1                 TO MAINTL
003475         MOVE +6                 TO GROUPL
003476         MOVE +2                 TO STATEL
003477         MOVE +8                 TO DATEL
003478         MOVE +10                TO ACCTL
003479     ELSE
003480         IF  PI-RETURN-TO-PROGRAM EQUAL 'EL652'
003481                 AND
003482             PI-CARRIER GREATER THAN LOW-VALUES
003483             MOVE '3'            TO DATASORI
003484                                    PI-689-DATA-SOURCE
003485             MOVE PI-CR-CARRIER  TO CARRIERI
003486             MOVE PI-CR-GROUPING TO GROUPI
003487             MOVE PI-CR-ACCOUNT  TO ACCTI
003488             MOVE PI-CR-FIN-RESP TO RPERSONI
003489             MOVE PI-CR-TYPE     TO TYPEI
003490             MOVE 'N'            TO PRTNOWI
003491                                    PI-PRINT-NOW
003492             MOVE AL-UANON       TO CARRIERA
003493                                    GROUPA
003494                                    ACCTA
003495                                    DATASORA
003496                                    RPERSONA
003497                                    TYPEA
003498                                    PRTNOWA
003499             MOVE +1             TO CARRIERL
003500                                    DATASORL
003501                                    TYPEL
003502                                    PRTNOWL
003503             MOVE -1             TO MAINTL
003504             MOVE +6             TO GROUPL
003505             MOVE +10            TO ACCTL
003506                                    RPERSONL
003507         ELSE
003508             IF  PI-RETURN-TO-PROGRAM EQUAL 'EL1273'
003509                     AND
003510                 PI-CARRIER GREATER THAN LOW-VALUES
003511                 MOVE '2'        TO DATASORI
003512                                    PI-689-DATA-SOURCE
003513                 MOVE PI-CARRIER TO CARRIERI
003514                 MOVE PI-GROUPING
003515                                 TO GROUPI
003516                 MOVE PI-STATE   TO STATEI
003517                 MOVE PI-ACCOUNT TO ACCTI
003518                 MOVE PI-CERT-PRIME
003519                                 TO CERTI
003520                 MOVE PI-CERT-SFX
003521                                 TO SFXI
003522                 MOVE PI-CERT-EFF-DT
003523                                 TO DC-BIN-DATE-1
003524                 MOVE ' '        TO DC-OPTION-CODE
003525                 PERFORM 9500-LINK-DATE-CONVERT
003526                     THRU 9500-EXIT
003527                 MOVE DC-GREG-DATE-1-EDIT
003528                                 TO DATEI
003529                 MOVE 'N'        TO PRTNOWI
003530                                    PI-PRINT-NOW
003531                 MOVE AL-UANON   TO CARRIERA
003532                                    GROUPA
003533                                    STATEA
003534                                    ACCTA
003535                                    CERTA
003536                                    SFXA
003537                                    DATEA
003538                                    ADDRSA
003539                                    DATASORA
003540                                    PRTNOWA
003541                 MOVE +1         TO CARRIERL
003542                                    SFXL
003543                                    DATASORL
003544                                    ADDRSL
003545                                    PRTNOWL
003546                 MOVE -1         TO MAINTL
003547                 MOVE +6         TO GROUPL
003548                 MOVE +2         TO STATEL
003549                 MOVE +8         TO DATEL
003550                 MOVE +10        TO ACCTL
003551                                    CERTL.
003552
003553     IF PI-CREATE-LABELS
003554         MOVE  AL-UANON          TO ADDRLBLA
003555         MOVE  +1                TO ADDRLBLL.
003556
003557     MOVE PI-LABEL-CONTROL       TO ADDRLBLI
003558                                    PI-689-LBL-OVERRIDE.
003559
003560     MOVE LOW-VALUES             TO PI-689-KEY-DATA-FIELDS.
003561     GO TO 8100-SEND-INITIAL-MAP.
003562
003563 0450-EXIT.
003564      EXIT.
003565                                 EJECT
003566 0600-BYPASS-SCREEN-CNTL.
003567***************************************************************
003568*     THIS ROUTINE WILL PROCESS A NEW LETTER REQUIREMENT      *
003569*     WITH THE INFORMATION FOUND IN THE PI AREA WITHOUT       *
003570*     PERFORMING ANY SCREEN IO.                               *
003571***************************************************************
003572
003573     
      * EXEC CICS HANDLE CONDITION
003574*        PGMIDERR (9700-PGMID-ERROR)
003575*        MAPFAIL  (0325-MAPFAIL)
003576*        ERROR    (9800-ABEND)
003577*    END-EXEC.
      *    MOVE '"$L?.                 ! % #00009723' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' &
                X'202020202020202020202120' &
                X'2520233030303039373233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003578
003579     MOVE LOW-VALUES             TO EL689AI.
003580
003581     MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM
003582
003583     MOVE PI-689-FORM-NUMBER     TO FORMI.
003584     MOVE +4                     TO FORML.
003585
003586     IF  PI-689-NUMBER-COPIES GREATER THAN ZERO
003587         MOVE PI-689-NUMBER-COPIES
003588                                 TO COPIESI
003589         MOVE +1                 TO COPIESL.
003590
003591     MOVE PI-689-LABEL-SOURCE    TO ADDRSI.
003592     MOVE +1                     TO ADDRSL.
003593     MOVE PI-689-DATA-SOURCE     TO DATASORI.
003594     MOVE +1                     TO DATASORL.
003595
003596*    IF  PI-689-PRINT-RESTRICTION = 'C' OR 'F' OR 'P'
003597*        MOVE PI-689-PRINT-RESTRICTION
003598*                                TO PRTRESTI
003599*        MOVE +1                 TO PRTRESTL.
003600
003601     MOVE SPACES                 TO PI-689-PRINT-SW
003602                                    PI-689-LBL-OVERRIDE.
003603     MOVE ZEROS                  TO PI-TOTAL-LINES
003604                                    PI-CURRENT-LINE
003605                                    PI-TEMP-STOR-ITEMS
003606                                    PI-UPDATE-SW
003607                                    PI-689-ERROR
003608                                    PI-689-NUMBER-LABEL-LINES
003609                                    PI-689-NUMBER-TEXT-RECORDS.
003610
003611     IF  PI-689-ALT-PRINTER-ID NOT EQUAL LOW-VALUES
003612         MOVE +4                 TO PRINTERL
003613         MOVE PI-689-ALT-PRINTER-ID
003614                                 TO PRINTERI PI-ALT-DMD-PRT-ID.
003615
003616     MOVE SPACES                 TO PI-689-ALT-PRINTER-ID.
003617
003618     IF  PI-689-SOURCE-VARIABLE
003619         PERFORM 0700-GET-VARIABLE-LABEL THRU 0700-EXIT.
003620
003621     MOVE SPACES                 TO W-CNTL-KEY
003622     MOVE LOW-VALUES             TO W-ACCT-KEY
003623                                    W-CERT-KEY
003624                                    W-CHEK-KEY
003625                                    W-COMP-KEY
003626                                    W-MAIL-KEY
003627                                    W-PNDB-KEY
003628                                    W-PNDB2-KEY
003629                                    W-PYAJ-KEY.
003630
003631     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
003632     MOVE PI-COMPANY-CD          TO W-ACCT-COMPANY-CD
003633                                    W-CERT-COMPANY-CD
003634                                    W-CHEK-COMPANY-CD
003635                                    W-COMP-COMPANY-CD
003636                                    W-MAIL-COMPANY-CD
003637                                    W-PNDB-COMPANY-CD
003638                                    W-PNDB2-COMPANY-CD
003639                                    W-PYAJ-COMPANY-CD.
003640
003641     IF  PI-689-CARRIER GREATER THAN LOW-VALUES
003642         MOVE PI-689-CARRIER     TO W-ACCT-CARRIER
003643                                    W-CERT-CARRIER
003644                                    W-MAIL-CARRIER
003645                                    W-CERT-CARRIER
003646                                    W-COMP-CARRIER
003647                                    W-CHEK-CARRIER
003648                                    W-PNDB2-CARRIER
003649                                    W-PYAJ-CARRIER.
003650
003651     IF  PI-689-GROUPING GREATER THAN LOW-VALUES
003652         MOVE PI-689-GROUPING    TO W-ACCT-GROUPING
003653                                    W-CERT-GROUPING
003654                                    W-CHEK-GROUPING
003655                                    W-COMP-GROUPING
003656                                    W-MAIL-GROUPING
003657                                    W-PNDB2-GROUPING
003658                                    W-PYAJ-GROUPING.
003659
003660     IF  PI-689-STATE GREATER THAN LOW-VALUES
003661         MOVE PI-689-STATE       TO W-ACCT-STATE
003662                                    W-CERT-STATE
003663                                    W-CHEK-STATE
003664                                    W-MAIL-STATE
003665                                    W-PNDB2-STATE.
003666
003667     IF  PI-689-ACCOUNT GREATER THAN LOW-VALUES
003668         IF PI-689-DATA-SOURCE = '3' OR '6'
003669             MOVE PI-689-ACCOUNT TO W-COMP-ACCOUNT
003670                                    W-PYAJ-ACCOUNT
003671         ELSE
003672             MOVE PI-689-ACCOUNT TO W-ACCT-ACCOUNT
003673                                    W-CERT-ACCOUNT
003674                                    W-CHEK-ACCOUNT
003675                                    W-MAIL-ACCOUNT
003676                                    W-PNDB2-ACCOUNT.
003677
003678     IF  PI-689-EFF-DATE GREATER THAN LOW-VALUES
003679         MOVE PI-689-EFF-DATE    TO W-CERT-EFF-DT
003680                                    W-CHEK-EFF-DT
003681                                    W-MAIL-EFF-DT
003682                                    W-PNDB2-EFF-DT.
003683
003684     IF  PI-689-CERT-PRIME GREATER THAN LOW-VALUES
003685         MOVE PI-689-CERT-PRIME  TO W-CERT-CERT-PRIME
003686                                    W-CHEK-CERT-PRIME
003687                                    W-MAIL-CERT-PRIME
003688                                    W-PNDB2-CERT-PRIME
003689
003690         IF  PI-689-CERT-SFX GREATER THAN LOW-VALUES
003691             MOVE PI-689-CERT-SFX
003692                                 TO W-CERT-CERT-SFX
003693                                    W-CHEK-CERT-SFX
003694                                    W-MAIL-CERT-SFX
003695                                    W-PNDB2-CERT-SFX
003696         ELSE
003697             MOVE SPACES         TO W-CERT-CERT-SFX
003698                                    W-CHEK-CERT-SFX
003699                                    W-MAIL-CERT-SFX
003700                                    W-PNDB2-CERT-SFX.
003701
003702     IF  PI-689-RESP-PERSON GREATER THAN LOW-VALUES
003703         IF PI-689-DATA-SOURCE = '3' OR '6'
003704             MOVE PI-689-RESP-PERSON
003705                                 TO W-COMP-RESP-PERSON
003706                                    W-PYAJ-FIN-RESP.
003707
003708     IF  PI-689-TYPE GREATER THAN LOW-VALUES
003709         IF PI-689-DATA-SOURCE = '3' OR '6'
003710             MOVE PI-689-TYPE    TO W-COMP-TYPE
003711                                    W-PYAJ-RECORD-TYPE
003712         ELSE
003713             MOVE PI-689-TYPE    TO W-PNDB2-TYPE.
003714
003715     IF  PI-689-SEQ-NO GREATER THAN ZEROS
003716         MOVE PI-689-SEQ-NO      TO W-PNDB-SEQ-NO
003717                                    W-PYAJ-FILE-SEQ-NO
003718                                    W-CHEK-SEQ-NO.
003719
003720     IF  PI-689-CHG-SEQ-NO GREATER THAN ZEROS
003721         MOVE PI-689-CHG-SEQ-NO  TO W-PNDB-CHG-SEQ-NO
003722                                    W-PNDB2-ALT-CHG-SEQ-NO.
003723
003724     IF  PI-689-ENTRY-BATCH GREATER THAN LOW-VALUES
003725         MOVE PI-689-ENTRY-BATCH TO W-PNDB-ENTRY.
003726
003727     IF  PI-689-NO-ERRORS-DETECTED
003728         PERFORM 2000-CREATE THRU 2000-EXIT
003729         IF  NOT PI-689-FATAL-ERROR
003730                 AND
003731             NOT PI-689-ERR-DETECTED-PREV
003732             IF  PI-689-PRINT-ONLY
003733                 PERFORM 7900-PRINT-LETTER-NOW THRU 7900-EXIT
003734             ELSE
003735                 IF  PI-689-PRINT-FIRST
003736                     PERFORM 7900-PRINT-LETTER-NOW THRU 7900-EXIT
003737                     IF  NOT PI-689-FATAL-ERROR
003738                         IF  PI-689-ARCHIVE-LETTER
003739                             PERFORM 5400-LETTER-RELEASE
003740                                 THRU 5400-EXIT
003741                         ELSE
003742                             MOVE ER-7390
003743                                 TO PI-689-ERROR
003744                     ELSE
003745                         NEXT SENTENCE
003746                 ELSE
003747                     IF  NOT PI-689-ARCHIVE-LETTER
003748                         MOVE ER-7390
003749                                 TO PI-689-ERROR
003750                     ELSE
003751                         PERFORM 5400-LETTER-RELEASE
003752                             THRU 5400-EXIT
003753                         IF  NOT PI-689-FATAL-ERROR
003754                             IF  PI-689-PRINT-SECOND
003755                                 PERFORM 7900-PRINT-LETTER-NOW
003756                                     THRU 7900-EXIT.
003757
003758 0600-EXIT.
003759     EXIT.
003760
003761 0620-RETURN-TO-CALLER.
003762
003763     MOVE PROGRAM-INTERFACE-BLOCK
003764                                 TO DFHCOMMAREA.
003765     
      * EXEC CICS RETURN
003766*    END-EXEC.
      *    MOVE '.(                    ''   #00009915' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039393135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003767
003768 0700-GET-VARIABLE-LABEL.
003769
003770     IF  PI-689-VARIABLE-DATA-1 NOT GREATER THAN SPACES
003771         MOVE '(VARIABLE NOT PROVIDED)'
003772                                 TO W-RC-TEXT (1)
003773                                    W-VG-TEXT (48)
003774         GO TO 0700-EXIT.
003775
003776     MOVE PI-689-VARIABLE-DATA-1 TO W-RC-TEXT (1)
003777                                    W-VG-TEXT (48).
003778     MOVE PI-689-VARIABLE-DATA-2 TO W-RC-TEXT (2)
003779                                    W-VG-TEXT (49).
003780     MOVE PI-689-VARIABLE-DATA-3 TO W-RC-TEXT (3)
003781                                    W-VG-TEXT (50).
003782     MOVE PI-689-VARIABLE-DATA-4 TO W-RC-TEXT (4)
003783                                    W-VG-TEXT (51).
003784     SET W-RG-NDX                TO +5.
003785
003786 0700-EXIT.
003787     EXIT.
003788                                 EJECT
003789 0800-EDIT-KEY-FIELDS.
003790***************************************************************
003791*     THIS ROUTINE WILL DECIDE WHAT FIELDS ARE NECESSARY      *
003792*     FROM THE DATA SOURCE FIELD.  IF A NECESSARY FIELD       *
003793*     IS NOT PROVIDED AN ERROR WILL PREVENT FURTHER           *
003794*     PROCESSING.                                             *
003795***************************************************************
003796     MOVE SPACES                 TO W-CNTL-KEY
003797     MOVE LOW-VALUES             TO W-ACCT-KEY
003798                                    W-CERT-KEY
003799                                    W-CHEK-KEY
003800                                    W-COMP-KEY
003801                                    W-MAIL-KEY
003802                                    W-PNDB-KEY
003803                                    W-PNDB2-KEY
003804                                    W-PYAJ-KEY.
003805
003806     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
003807     MOVE PI-COMPANY-CD          TO W-ACCT-COMPANY-CD
003808                                    W-CERT-COMPANY-CD
003809                                    W-CHEK-COMPANY-CD
003810                                    W-COMP-COMPANY-CD
003811                                    W-MAIL-COMPANY-CD
003812                                    W-PNDB-COMPANY-CD
003813                                    W-PNDB2-COMPANY-CD
003814                                    W-PYAJ-COMPANY-CD.
003815
003816     PERFORM 0900-EDIT-CARRIER THRU 0900-EXIT.
003817     PERFORM 0905-EDIT-GROUPING THRU 0905-EXIT.
003818     PERFORM 0910-EDIT-STATE THRU 0910-EXIT.
003819     PERFORM 0915-EDIT-ACCOUNT THRU 0915-EXIT.
003820     PERFORM 0920-EDIT-EXP-DATE THRU 0920-EXIT.
003821     PERFORM 0925-EDIT-EFF-DATE THRU 0925-EXIT.
003822     PERFORM 0930-EDIT-CERTIFICATE THRU 0930-EXIT.
003823     PERFORM 0935-EDIT-RESP-PERSON THRU 0935-EXIT.
003824     PERFORM 0940-EDIT-ACCOUNT THRU 0940-EXIT.
003825     PERFORM 0945-EDIT-TYPE THRU 0945-EXIT.
003826     PERFORM 0950-EDIT-ENTRY THRU 0950-EXIT.
003827     PERFORM 0955-EDIT-SEQUENCE THRU 0955-EXIT.
003828     PERFORM 0960-EDIT-BATCH-SEQ THRU 0960-EXIT.
003829     PERFORM 0965-EDIT-TYPE THRU 0965-EXIT.
003830     PERFORM 0970-EDIT-CSO THRU 0970-EXIT.
003831
003832     IF  DATASORI NUMERIC
003833         MOVE DATASORI           TO W-DATA-SOURCE
003834                                    PI-689-DATA-SOURCE
003835         GO TO 0800-SOURCE-ACCOUNT
003836               0800-SOURCE-CERTIFICATE
003837               0800-SOURCE-COMPENSATION
003838               0800-SOURCE-PENDING
003839               0800-SOURCE-CHEK
003840               0800-SOURCE-PAYMENTS-ADJS
003841                                 DEPENDING ON W-DATA-SOURCE.
003842
003843     IF  FORMI NOT EQUAL '9999'
003844         MOVE ER-7365            TO EMI-ERROR
003845         MOVE -1                 TO MAINTL
003846         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003847         GO TO 8200-SEND-DATAONLY
003848
003849     ELSE
003850         GO TO 0800-EXIT.
003851
003852 0800-SOURCE-ACCOUNT.
003853
003854     IF  W-ACCT-COMPANY-CD EQUAL LOW-VALUES
003855             OR
003856         W-ACCT-CARRIER EQUAL LOW-VALUES
003857             OR
003858         W-ACCT-GROUPING EQUAL LOW-VALUES
003859             OR
003860         W-ACCT-STATE EQUAL LOW-VALUES
003861             OR
003862         W-ACCT-ACCOUNT EQUAL LOW-VALUES
003863             OR
003864         W-ACCT-EXP-DT EQUAL LOW-VALUES
003865         MOVE ER-7376            TO EMI-ERROR
003866         MOVE -1                 TO MAINTL
003867         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003868
003869     GO TO 0800-ERROR-TEST.
003870
003871 0800-SOURCE-CERTIFICATE.
003872
003873     IF  W-CERT-COMPANY-CD EQUAL LOW-VALUES
003874             OR
003875         W-CERT-CARRIER EQUAL LOW-VALUES
003876             OR
003877         W-CERT-GROUPING EQUAL LOW-VALUES
003878             OR
003879         W-CERT-STATE EQUAL LOW-VALUES
003880             OR
003881         W-CERT-ACCOUNT EQUAL LOW-VALUES
003882             OR
003883         W-CERT-EFF-DT EQUAL LOW-VALUES
003884             OR
003885         W-CERT-CERT-NO EQUAL LOW-VALUES
003886         MOVE ER-7377            TO EMI-ERROR
003887         MOVE -1                 TO MAINTL
003888         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003889
003890     GO TO 0800-ERROR-TEST.
003891
003892 0800-SOURCE-COMPENSATION.
003893
003894     IF  W-COMP-COMPANY-CD EQUAL LOW-VALUES
003895             OR
003896         W-COMP-CARRIER EQUAL LOW-VALUES
003897             OR
003898         W-COMP-GROUPING EQUAL LOW-VALUES
003899             OR
003900         W-COMP-RESP-PERSON EQUAL LOW-VALUES
003901             OR
003902         W-COMP-TYPE EQUAL LOW-VALUES
003903             OR
003904         (W-COMP-TYPE EQUAL 'A'
003905                 AND
003906             W-COMP-ACCOUNT EQUAL LOW-VALUES)
003907         MOVE ER-7378            TO EMI-ERROR
003908         MOVE -1                 TO MAINTL
003909         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003910
003911     GO TO 0800-ERROR-TEST.
003912
003913 0800-SOURCE-PENDING.
003914
003915     IF  BENTRYL EQUAL ZEROS
003916         IF  W-PNDB2-COMPANY-CD EQUAL LOW-VALUES
003917                 OR
003918             W-PNDB2-CARRIER EQUAL LOW-VALUES
003919                 OR
003920             W-PNDB2-GROUPING EQUAL LOW-VALUES
003921                 OR
003922             W-PNDB2-STATE EQUAL LOW-VALUES
003923                 OR
003924             W-PNDB2-ACCOUNT EQUAL LOW-VALUES
003925                 OR
003926             W-PNDB2-EFF-DT EQUAL LOW-VALUES
003927                 OR
003928             W-PNDB2-CERT-NO EQUAL LOW-VALUES
003929                 OR
003930             W-PNDB2-TYPE EQUAL LOW-VALUES
003931                 OR
003932             W-PNDB2-ALT-CHG-SEQ-NO NOT = ZEROS
003933             MOVE ER-7379        TO EMI-ERROR
003934             MOVE -1             TO MAINTL
003935             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003936         ELSE
003937             NEXT SENTENCE
003938     ELSE
003939         IF  W-PNDB-COMPANY-CD EQUAL LOW-VALUES
003940                 OR
003941             W-PNDB-ENTRY EQUAL LOW-VALUES
003942                 OR
003943             W-PNDB-SEQ-NO EQUAL ZEROS
003944             MOVE ER-7379        TO EMI-ERROR
003945             MOVE -1             TO MAINTL
003946             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003947
003948     GO TO 0800-ERROR-TEST.
003949
003950 0800-SOURCE-CHEK.
003951
003952     IF  W-CHEK-COMPANY-CD EQUAL LOW-VALUES
003953             OR
003954         W-CHEK-CARRIER EQUAL LOW-VALUES
003955             OR
003956         W-CHEK-GROUPING EQUAL LOW-VALUES
003957             OR
003958         W-CHEK-STATE EQUAL LOW-VALUES
003959             OR
003960         W-CHEK-ACCOUNT EQUAL LOW-VALUES
003961             OR
003962         W-CHEK-EFF-DT EQUAL LOW-VALUES
003963             OR
003964         W-CHEK-CERT-NO EQUAL LOW-VALUES
003965         MOVE ER-7381            TO EMI-ERROR
003966         MOVE -1                 TO MAINTL
003967         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003968
003969     GO TO 0800-ERROR-TEST.
003970
003971 0800-SOURCE-PAYMENTS-ADJS.
003972
003973     IF  W-PYAJ-COMPANY-CD EQUAL LOW-VALUES
003974             OR
003975         W-PYAJ-CARRIER EQUAL LOW-VALUES
003976             OR
003977         W-PYAJ-GROUPING EQUAL LOW-VALUES
003978             OR
003979         W-PYAJ-FIN-RESP EQUAL LOW-VALUES
003980             OR
003981         W-PYAJ-ACCOUNT EQUAL LOW-VALUES
003982             OR
003983         W-PYAJ-RECORD-TYPE EQUAL LOW-VALUES
003984         MOVE ER-7396            TO EMI-ERROR
003985         MOVE -1                 TO MAINTL
003986         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003987
003988     GO TO 0800-ERROR-TEST.
003989
003990 0800-ERROR-TEST.
003991
003992     IF  NOT EMI-NO-ERRORS
003993         IF  EMI-ERROR EQUAL ER-7250
003994             GO TO 0800-EXIT
003995         ELSE
003996             GO TO 8200-SEND-DATAONLY.
003997
003998 0800-EXIT.
003999     EXIT.
004000                                 EJECT
004001 0900-EDIT-CARRIER.
004002
004003     IF  CARRIERL EQUAL ZEROES
004004         MOVE LOW-VALUES         TO PI-689-CARRIER
004005                                    W-ACCT-CARRIER
004006                                    W-CERT-CARRIER
004007                                    W-COMP-CARRIER
004008                                    W-CHEK-CARRIER
004009                                    W-MAIL-CARRIER
004010                                    W-PNDB2-CARRIER
004011                                    W-PYAJ-CARRIER
004012     ELSE
004013         PERFORM 0990-CARRIER-VALIDITY-CHECK THRU 0990-EXIT.
004014
004015 0900-EXIT.
004016     EXIT.
004017                                 EJECT
004018 0905-EDIT-GROUPING.
004019
004020     IF  GROUPL EQUAL ZEROES
004021         MOVE LOW-VALUES         TO PI-689-GROUPING
004022                                    W-ACCT-GROUPING
004023                                    W-CERT-GROUPING
004024                                    W-CHEK-GROUPING
004025                                    W-COMP-GROUPING
004026                                    W-MAIL-GROUPING
004027                                    W-PNDB2-GROUPING
004028                                    W-PYAJ-GROUPING
004029     ELSE
004030         MOVE AL-UANON           TO GROUPA
004031         MOVE GROUPI             TO PI-689-GROUPING
004032                                    W-ACCT-GROUPING
004033                                    W-CERT-GROUPING
004034                                    W-CHEK-GROUPING
004035                                    W-COMP-GROUPING
004036                                    W-MAIL-GROUPING
004037                                    W-PNDB2-GROUPING
004038                                    W-PYAJ-GROUPING.
004039
004040 0905-EXIT.
004041     EXIT.
004042                                 EJECT
004043 0910-EDIT-STATE.
004044
004045     IF  STATEL EQUAL ZEROES
004046         MOVE LOW-VALUES         TO PI-689-STATE
004047                                    W-ACCT-STATE
004048                                    W-CERT-STATE
004049                                    W-CHEK-STATE
004050                                    W-MAIL-STATE
004051                                    W-PNDB2-STATE
004052     ELSE
004053         PERFORM 0994-STATE-VALIDITY-CHECK THRU 0994-EXIT.
004054
004055 0910-EXIT.
004056     EXIT.
004057                                 EJECT
004058 0915-EDIT-ACCOUNT.
004059
004060     IF  ACCTL EQUAL ZEROES
004061         MOVE LOW-VALUES         TO PI-689-ACCOUNT
004062                                    W-ACCT-ACCOUNT
004063                                    W-CERT-ACCOUNT
004064                                    W-CHEK-ACCOUNT
004065                                    W-COMP-ACCOUNT
004066                                    W-MAIL-ACCOUNT
004067                                    W-PNDB2-ACCOUNT
004068                                    W-PYAJ-ACCOUNT
004069     ELSE
004070         MOVE AL-UANON           TO ACCTA
004071         MOVE ACCTI              TO PI-689-ACCOUNT
004072                                    W-ACCT-ACCOUNT
004073                                    W-CERT-ACCOUNT
004074                                    W-CHEK-ACCOUNT
004075                                    W-COMP-ACCOUNT
004076                                    W-MAIL-ACCOUNT
004077                                    W-PYAJ-ACCOUNT
004078                                    W-PNDB2-ACCOUNT.
004079
004080 0915-EXIT.
004081     EXIT.
004082                                 EJECT
004083 0920-EDIT-EXP-DATE.
004084
004085     IF  DATEL NOT EQUAL ZEROS
004086         IF  DATEI EQUAL '99/99/99' OR '999999'
004087             MOVE '99/99/99'     TO DATEO
004088                                    PI-689-DATE-EDIT
004089             MOVE AL-UANON       TO DATEA
004090             MOVE HIGH-VALUES    TO PI-689-EXP-DATE
004091                                    W-ACCT-EXP-DT
004092         ELSE
004093             MOVE DATEI          TO W-DEEDIT-FIELD
004094             PERFORM 8600-DEEDIT
004095             MOVE W-DEEDIT-FIELD-V0
004096                                 TO DC-GREG-DATE-1-MDY
004097             MOVE '4'            TO DC-OPTION-CODE
004098             PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
004099             IF  DATE-CONVERSION-ERROR
004100                 MOVE ER-0454    TO EMI-ERROR
004101                 MOVE -1         TO DATEL
004102                 MOVE AL-UABON   TO DATEA
004103                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004104             ELSE
004105                 MOVE DC-GREG-DATE-1-EDIT
004106                                 TO DATEO
004107                                    PI-689-DATE-EDIT
004108                 MOVE AL-UANON   TO DATEA
004109                 MOVE DC-BIN-DATE-1
004110                                 TO PI-689-EXP-DATE
004111                                    W-ACCT-EXP-DT
004112     ELSE
004113         MOVE LOW-VALUES         TO PI-689-EXP-DATE
004114                                    W-ACCT-EXP-DT
004115                                    PI-689-DATE-EDIT.
004116
004117 0920-EXIT.
004118     EXIT.
004119                                 EJECT
004120 0925-EDIT-EFF-DATE.
004121
004122     IF  CERTL EQUAL ZERO
004123             AND
004124         DATASORI NOT EQUAL '2'
004125         GO TO 0925-EXIT.
004126
004127     IF  DATEL NOT EQUAL ZEROS
004128         MOVE DATEI              TO W-DEEDIT-FIELD
004129         PERFORM 8600-DEEDIT
004130         MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
004131         MOVE '4'                TO DC-OPTION-CODE
004132         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
004133
004134         IF  DATE-CONVERSION-ERROR
004135             MOVE ER-0215        TO EMI-ERROR
004136             MOVE -1             TO DATEL
004137             MOVE AL-UABON       TO DATEA
004138             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004139         ELSE
004140             MOVE DC-GREG-DATE-1-EDIT
004141                                 TO DATEO
004142                                    PI-689-DATE-EDIT
004143             MOVE AL-UANON       TO DATEA
004144             MOVE DC-BIN-DATE-1  TO PI-689-EFF-DATE
004145                                    W-CERT-EFF-DT
004146                                    W-CHEK-EFF-DT
004147                                    W-MAIL-EFF-DT
004148                                    W-PNDB2-EFF-DT
004149     ELSE
004150         MOVE LOW-VALUES         TO PI-689-EFF-DATE
004151                                    W-PNDB2-EFF-DT
004152                                    W-CHEK-EFF-DT
004153                                    W-MAIL-EFF-DT
004154                                    W-CERT-EFF-DT
004155                                    PI-689-DATE-EDIT.
004156
004157 0925-EXIT.
004158     EXIT.
004159                                 EJECT
004160 0930-EDIT-CERTIFICATE.
004161
004162     IF  CERTL EQUAL ZEROES
004163         MOVE LOW-VALUES         TO PI-689-CERT-PRIME
004164                                    W-CERT-CERT-PRIME
004165                                    W-CHEK-CERT-PRIME
004166                                    W-MAIL-CERT-PRIME
004167                                    W-PNDB2-CERT-PRIME
004168                                    PI-689-CERT-SFX
004169                                    W-CERT-CERT-SFX
004170                                    W-CHEK-CERT-SFX
004171                                    W-MAIL-CERT-SFX
004172                                    W-PNDB2-CERT-SFX
004173     ELSE
004174         MOVE AL-UANON           TO CERTA
004175         MOVE CERTI              TO PI-689-CERT-PRIME
004176                                    W-CERT-CERT-PRIME
004177                                    W-CHEK-CERT-PRIME
004178                                    W-MAIL-CERT-PRIME
004179                                    W-PNDB2-CERT-PRIME
004180         IF  SFXL EQUAL ZEROES
004181                 OR
004182             SFXI EQUAL LOW-VALUES
004183             MOVE SPACES         TO PI-689-CERT-SFX
004184                                    W-CERT-CERT-SFX
004185                                    W-CHEK-CERT-SFX
004186                                    W-MAIL-CERT-SFX
004187                                    W-PNDB2-CERT-SFX
004188         ELSE
004189             MOVE AL-UANON       TO SFXA
004190             MOVE SFXI           TO PI-689-CERT-SFX
004191                                    W-CERT-CERT-SFX
004192                                    W-CHEK-CERT-SFX
004193                                    W-MAIL-CERT-SFX
004194                                    W-PNDB2-CERT-SFX.
004195
004196 0930-EXIT.
004197     EXIT.
004198                                 EJECT
004199 0935-EDIT-RESP-PERSON.
004200
004201     IF  RPERSONL EQUAL ZEROES
004202         MOVE LOW-VALUES         TO PI-689-RESP-PERSON
004203                                    W-COMP-RESP-PERSON
004204                                    W-PYAJ-FIN-RESP
004205     ELSE
004206         MOVE AL-UANON           TO RPERSONA
004207         MOVE RPERSONI           TO PI-689-RESP-PERSON
004208                                    W-PYAJ-FIN-RESP
004209                                    W-COMP-RESP-PERSON.
004210
004211 0935-EXIT.
004212     EXIT.
004213                                 EJECT
004214 0940-EDIT-ACCOUNT.
004215
004216     IF  ACCTL EQUAL ZEROES
004217         MOVE LOW-VALUES         TO PI-689-ACCOUNT
004218                                    W-COMP-ACCOUNT
004219                                    W-PYAJ-ACCOUNT
004220     ELSE
004221         MOVE AL-UANON           TO ACCTA
004222         MOVE ACCTI              TO PI-689-ACCOUNT
004223                                    W-PYAJ-ACCOUNT
004224                                    W-COMP-ACCOUNT.
004225
004226 0940-EXIT.
004227     EXIT.
004228                                 EJECT
004229 0945-EDIT-TYPE.
004230
004231     IF  TYPEL EQUAL ZEROES
004232         MOVE LOW-VALUES         TO PI-689-TYPE
004233                                    W-COMP-TYPE
004234                                    W-PYAJ-RECORD-TYPE
004235     ELSE
004236         INSPECT TYPEI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
004237         IF TYPEI = 'C' OR 'G' OR 'A'
004238             MOVE AL-UANON       TO TYPEA
004239             MOVE TYPEI          TO PI-689-TYPE
004240                                    W-COMP-TYPE
004241                                    W-PYAJ-RECORD-TYPE
004242         ELSE
004243             MOVE ER-7368        TO EMI-ERROR
004244             MOVE -1             TO TYPEL
004245             MOVE AL-UABON       TO TYPEA
004246             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004247
004248 0945-EXIT.
004249     EXIT.
004250                                 EJECT
004251 0950-EDIT-ENTRY.
004252
004253     IF  BENTRYL EQUAL ZEROES
004254         MOVE LOW-VALUES         TO PI-689-ENTRY-BATCH
004255                                    W-PNDB-ENTRY
004256     ELSE
004257         MOVE AL-UANON           TO BENTRYA
004258         MOVE BENTRYI            TO PI-689-ENTRY-BATCH
004259                                    W-PNDB-ENTRY.
004260
004261 0950-EXIT.
004262     EXIT.
004263                                 EJECT
004264 0955-EDIT-SEQUENCE.
004265
004266     IF  SEQL NOT EQUAL ZEROS
004267         IF  SEQI NOT NUMERIC
004268                 OR
004269             SEQI EQUAL '0'
004270             MOVE ER-7367        TO EMI-ERROR
004271             MOVE -1             TO SEQL
004272             MOVE AL-UABON       TO SEQA
004273             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004274         ELSE
004275             MOVE AL-UANON       TO SEQA
004276             MOVE SEQI           TO PI-689-SEQ-NO
004277                                    W-CHEK-SEQ-NO
004278                                    W-PYAJ-FILE-SEQ-NO
004279                                    W-PNDB-SEQ-NO
004280                                    PI-689-SEQ-EDIT
004281     ELSE
004282         MOVE LOW-VALUES         TO PI-689-SEQ-EDIT.
004283
004284 0955-EXIT.
004285     EXIT.
004286                                 EJECT
004287 0960-EDIT-BATCH-SEQ.
004288
004289     IF  BCSEQL NOT EQUAL ZEROS
004290         IF  BCSEQI NOT NUMERIC
004291             MOVE ER-7368        TO EMI-ERROR
004292             MOVE -1             TO BCSEQL
004293             MOVE AL-UABON       TO BCSEQA
004294             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004295         ELSE
004296             MOVE AL-UANON       TO BCSEQA
004297             MOVE BCSEQI         TO PI-689-CHG-SEQ-NO
004298                                    W-PNDB-CHG-SEQ-NO
004299                                    W-PNDB2-ALT-CHG-SEQ-NO
004300                                    PI-689-BCSEQ-EDIT
004301     ELSE
004302         MOVE LOW-VALUES         TO PI-689-BCSEQ-EDIT.
004303
004304 0960-EXIT.
004305     EXIT.
004306                                 EJECT
004307 0965-EDIT-TYPE.
004308
004309     IF  TYPEL EQUAL ZEROES
004310         MOVE LOW-VALUES         TO PI-689-TYPE
004311                                    W-PNDB2-TYPE
004312     ELSE
004313         IF  DATASORI NOT EQUAL '4'
004314             GO TO 0965-EXIT
004315         ELSE
004316             IF TYPEI = '0' OR '1' OR '2' OR '3'
004317                 MOVE AL-UANON   TO TYPEA
004318                 MOVE TYPEI      TO PI-689-TYPE
004319                                    W-PNDB2-TYPE
004320             ELSE
004321                 MOVE ER-7370    TO EMI-ERROR
004322                 MOVE -1         TO TYPEL
004323                 MOVE AL-UABON   TO TYPEA
004324                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004325
004326 0965-EXIT.
004327     EXIT.
004328
004329 0970-EDIT-CSO.
004330
004331     IF ENDARCHL > ZEROS
004332        IF ENDARCHI NUMERIC
004333            MOVE ENDARCHI       TO PI-ENDT-ARCH-NO
004334            MOVE AL-UNNON       TO ENDARCHA
004335            MOVE ENDARCHI       TO W-ERENDT-ARCHIVE
004336            MOVE PI-COMPANY-CD  TO W-ERENDT-COMPANY-CD-A1
004337            MOVE 'N'            TO W-VALID-ENDT-SW
004338            PERFORM 0355-CHECK-ERENDT THRU 0355-EXIT
004339            IF NOT W-VALID-ENDT
004340                MOVE -1         TO ENDARCHL
004341                MOVE ER-1565    TO EMI-ERROR
004342                MOVE AL-UNBON   TO ENDARCHA
004343                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004344            END-IF
004345        END-IF
004346     ELSE
004347        MOVE ZEROS              TO PI-ENDT-ARCH-NO
004348     END-IF
004349
004350     IF PI-CERT-REQ-IND = 'Y'
004351       IF  CERTIDL NOT EQUAL ZEROS
004352         IF CERTIDI NUMERIC
004353             MOVE AL-UANON      TO CERTIDA
004354             MOVE CERTIDI       TO PI-CERT-FORM-ID
004355         ELSE
004356             MOVE ER-1778       TO EMI-ERROR
004357             MOVE -1            TO CERTIDL
004358             MOVE AL-UABON      TO CERTIDA
004359             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004360         END-IF
004361       ELSE
004362          MOVE ER-0715       TO EMI-ERROR
004363          MOVE -1            TO CERTIDL
004364          MOVE AL-UABON      TO CERTIDA
004365          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004366       END-IF
004367     END-IF.
004368
004369 0970-EXIT.
004370      EXIT.
004371                                 EJECT
004372 0990-CARRIER-VALIDITY-CHECK.
004373
004374     IF  CARRIERI EQUAL SPACES
004375         GO TO 0990-EXIT.
004376
004377     MOVE SPACES                 TO W-CNTL-KEY.
004378     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
004379     MOVE '6'                    TO W-CNTL-RECORD-TYPE.
004380     MOVE CARRIERI               TO W-CNTL-GEN4.
004381     MOVE +0                     TO W-CNTL-SEQ-NO.
004382
004383     
      * EXEC CICS HANDLE CONDITION
004384*        NOTFND  (0990-CARRIER-NOT-FOUND)
004385*        NOTOPEN (8040-CNTL-NOT-OPEN)
004386*    END-EXEC.
      *    MOVE '"$IJ                  ! & #00010533' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303130353333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004387
004388     
      * EXEC CICS READ
004389*        DATASET (W-CNTL-FILE-ID)
004390*        SET     (ADDRESS OF CONTROL-FILE)
004391*        RIDFLD  (W-CNTL-KEY)
004392*    END-EXEC.
      *    MOVE '&"S        E          (   #00010538' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130353338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004393
004394     IF  PI-NO-CARRIER-SECURITY
004395             OR
004396         PI-CARRIER-SECURITY EQUAL CARRIERI
004397         MOVE AL-UANON           TO CARRIERA
004398         MOVE CARRIERI           TO PI-689-CARRIER
004399                                    W-ACCT-CARRIER
004400                                    W-CERT-CARRIER
004401                                    W-CHEK-CARRIER
004402                                    W-COMP-CARRIER
004403                                    W-MAIL-CARRIER
004404                                    W-PNDB2-CARRIER
004405                                    W-PYAJ-CARRIER
004406     ELSE
004407         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
004408         MOVE ER-9095            TO EMI-ERROR
004409         MOVE -1                 TO MAINTL
004410         MOVE AL-UANON           TO MAINTA
004411         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004412
004413     GO TO 0990-EXIT.
004414
004415 0990-CARRIER-NOT-FOUND.
004416
004417     MOVE -1                     TO CARRIERL.
004418     MOVE AL-UABON               TO CARRIERA.
004419     MOVE ER-2208                TO EMI-ERROR.
004420     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004421
004422 0990-EXIT.
004423     EXIT.
004424                                 EJECT
004425 0994-STATE-VALIDITY-CHECK.
004426
004427     IF  STATEI EQUAL SPACES
004428         GO TO 0994-EXIT.
004429
004430     MOVE SPACES                 TO W-CNTL-KEY.
004431     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
004432     MOVE '3'                    TO W-CNTL-RECORD-TYPE.
004433     MOVE STATEI                 TO W-CNTL-GEN1.
004434     MOVE +0                     TO W-CNTL-SEQ-NO.
004435
004436     
      * EXEC CICS HANDLE CONDITION
004437*        NOTFND  (0994-STATE-NOT-FOUND)
004438*        NOTOPEN (8040-CNTL-NOT-OPEN)
004439*    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00010586' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303130353836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004440
004441     
      * EXEC CICS READ
004442*        DATASET (W-CNTL-FILE-ID)
004443*        SET     (ADDRESS OF CONTROL-FILE)
004444*        RIDFLD  (W-CNTL-KEY)
004445*    END-EXEC.
      *    MOVE '&"S        E          (   #00010591' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130353931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004446
004447     MOVE AL-UANON               TO STATEA.
004448     MOVE STATEI                 TO PI-689-STATE
004449                                    W-ACCT-STATE
004450                                    W-CERT-STATE
004451                                    W-CHEK-STATE
004452                                    W-MAIL-STATE
004453                                    W-PNDB2-STATE.
004454
004455     GO TO 0994-EXIT.
004456
004457 0994-STATE-NOT-FOUND.
004458
004459     MOVE -1                     TO STATEL.
004460     MOVE AL-UABON               TO STATEA.
004461     MOVE ER-2209                TO EMI-ERROR.
004462     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004463
004464 0994-EXIT.
004465     EXIT.
004466                                 EJECT
004467 1000-SHOW.
004468***************************************************************
004469*     THIS ROUTINE WILL BROWSE THE ARCHIVE FILE WITH THE      *
004470*     ARCHIVE NUMBER SPECIFIED FROM THE SCREEN.  IF THE       *
004471*     HEADER RECORD IS FOUND THE TEXT IS THEN READ AND        *
004472*     WILL BE INSERTED INTO THE W-TS-TABLE AND DISPLAYED.     *
004473***************************************************************
004474
004475     MOVE SPACES                 TO W-RECORD-TABLE.
004476
004477     IF  PI-689-LBL-OVERRIDE GREATER THAN SPACES
004478          MOVE PI-689-LBL-OVERRIDE
004479                                 TO ADDRLBLI
004480          MOVE +1                TO ADDRLBLL
004481          MOVE AL-UANON          TO ADDRLBLA
004482     ELSE
004483          MOVE PI-LABEL-CONTROL  TO ADDRLBLI
004484          MOVE +1                TO ADDRLBLL
004485          MOVE AL-UANON          TO ADDRLBLA.
004486
004487     IF  ARCHNUML EQUAL ZEROS
004488         MOVE -1                 TO ARCHNUML
004489         MOVE AL-UNBON           TO ARCHNUMA
004490         MOVE ER-0174            TO EMI-ERROR
004491         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004492         GO TO 8200-SEND-DATAONLY.
004493
004494     IF  ARCHNUMI NOT NUMERIC
004495         MOVE -1                 TO ARCHNUML
004496         MOVE AL-UNBON           TO ARCHNUMA
004497         MOVE ER-0175            TO EMI-ERROR
004498         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004499         GO TO 8200-SEND-DATAONLY.
004500
004501     IF  PI-SHOW-MODE
004502
004503         IF  PI-689-ARCHIVE-NUMBER EQUAL ARCHNUMI
004504             MOVE -1             TO MAINTL
004505             GO TO 8200-SEND-DATAONLY
004506         ELSE
004507             PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT.
004508
004509     MOVE +0                     TO W-ARCH-SEQ-NO
004510                                    W-ARCT-SEQ-NO.
004511     MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD
004512                                    W-ARCT-COMPANY-CD.
004513     MOVE ARCHNUMI               TO W-ARCH-NUMBER
004514                                    W-ARCT-NUMBER
004515                                    PI-689-ARCHIVE-NUMBER.
004516     MOVE ' '                    TO W-ARCT-REC-TYPE.
004517     MOVE '1'                    TO PI-ACTION.
004518     MOVE SPACES                 TO PI-689-PRINT-SW
004519                                    PI-689-FORM-NUMBER
004520                                    W-DELETE-KEY.
004521     SET W-RG-NDX                TO W-ZEROS.
004522
004523     
      * EXEC CICS HANDLE CONDITION
004524*         NOTOPEN    (8010-ARCH-NOT-OPEN)
004525*         NOTFND     (1070-ARCH-NOT-FOUND)
004526*         ENDFILE    (1070-ARCH-NOT-FOUND)
004527*    END-EXEC.
      *    MOVE '"$JI''                 ! ( #00010673' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2820233030303130363733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004528
004529     
      * EXEC CICS READ
004530*        DATASET (W-ARCH-FILE-ID)
004531*        SET     (ADDRESS OF LETTER-ARCHIVE)
004532*        RIDFLD  (W-ARCH-KEY)
004533*    END-EXEC.
      *    MOVE '&"S        E          (   #00010679' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130363739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004534
004535     PERFORM 1040-FORMAT-HEADER-DATA THRU 1040-EXIT.
004536
004537*    EXEC CICS HANDLE CONDITION
004538*         NOTOPEN    (8015-ARCT-NOT-OPEN)
004539*         NOTFND     (1020-ARCT-ENDBR)
004540*         ENDFILE    (1020-ARCT-ENDBR)
004541*    END-EXEC.
004542*
004543*1005-START-BROWSE.
004544*
004545*    EXEC CICS STARTBR
004546*         DATASET    (W-ARCT-FILE-ID)
004547*         RIDFLD     (W-ARCT-KEY)
004548*         GTEQ
004549*    END-EXEC.
004550*
004551*    MOVE 'Y'                    TO W-ARCT-BROWSE-STARTED.
004552*
004553*1010-READ-NEXT.
004554*
004555*    EXEC CICS READNEXT
004556*         SET       (ADDRESS OF LETTER-ARCHIVE-TEXT)
004557*         DATASET   (W-ARCT-FILE-ID)
004558*         RIDFLD    (W-ARCT-KEY)
004559*    END-EXEC.
004560*
004561*    IF  W-ARCH-PARTIAL-KEY EQUAL W-ARCT-PARTIAL-KEY
004562*        IF  LT-ADDRESS-DATA
004563*            IF  PI-CREATE-LABELS  AND
004564*                NOT PI-689-LABELS-OVERRIDEN
004565*                PERFORM 1050-MOVE-LABELS THRU 1050-EXIT
004566*                        VARYING
004567*                    LT-NDX FROM 1 BY 1
004568*                        UNTIL
004569*                    LT-NDX GREATER THAN PI-689-NUMBER-LABEL-LINES
004570*                GO TO 1010-READ-NEXT
004571*            ELSE
004572*                MOVE LT-CONTROL-PRIMARY
004573*                                TO W-DELETE-KEY
004574*                GO TO 1010-READ-NEXT
004575*        ELSE
004576*            PERFORM 1030-TEXT-BUILD THRU 1030-EXIT
004577*                    VARYING
004578*                LT-NDX FROM 1 BY 1
004579*                    UNTIL
004580*                LT-NDX GREATER THAN LT-NUM-LINES-ON-RECORD
004581*                    OR
004582*                LT-NDX GREATER THAN +20
004583*            GO TO 1010-READ-NEXT.
004584*
004585*1020-ARCT-ENDBR.
004586*
004587*    IF  W-ARCT-BROWSE-STARTED EQUAL 'Y'
004588*        EXEC CICS ENDBR
004589*             DATASET   (W-ARCT-FILE-ID)
004590*        END-EXEC.
004591*
004592*    IF  W-RG-NDX EQUAL ZEROS
004593*        GO TO 1080-ARCHIVE-TEXT-NOT-FOUND.
004594*
004595*    IF  PI-689-NUMBER-LABEL-LINES GREATER THAN +0
004596*            AND
004597*        PI-BYPASS-LABELS
004598*        MOVE W-DELETE-KEY       TO W-ARCT-KEY
004599*        PERFORM 1060-DELETE-ADDRESS-RECORD THRU 1060-EXIT
004600*        MOVE +0                 TO PI-689-NUMBER-LABEL-LINES.
004601*
004602*    SET PI-TOTAL-LINES          TO W-RG-NDX.
004603*    MOVE 1                      TO PI-CURRENT-LINE.
004604*    SET W-RG-NDX                TO +1.
004605*
004606*    PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
004607*            VARYING
004608*        W-SC-NDX FROM 1 BY 1
004609*            UNTIL
004610*        W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
004611
004612     MOVE -1                     TO MAINTL.
004613     GO TO 8100-SEND-INITIAL-MAP.
004614
004615 1020-EXIT.
004616     EXIT.
004617
004618 1030-TEXT-BUILD.
004619
004620     SET W-RG-NDX UP BY 1.
004621     MOVE LT-SKIP-CONTROL (LT-NDX)
004622                                 TO W-PRINT-CONTROL.
004623
004624     IF  W-PRINT-CONTROL EQUAL 99
004625         MOVE W-TOP-FORM         TO W-RC-TEXT (W-RG-NDX)
004626         SET W-RG-NDX UP BY 1
004627     ELSE
004628         SET W-RG-NDX UP BY W-PRINT-CONTROL.
004629
004630     MOVE LT-TEXT-LINE (LT-NDX)  TO W-RC-TEXT (W-RG-NDX).
004631     MOVE LT-SKIP-CONTROL (LT-NDX)
004632                                 TO W-RC-PC (W-RG-NDX).
004633     MOVE SPACES                 TO W-RC-SC (W-RG-NDX).
004634
004635 1030-EXIT.
004636     EXIT.
004637
004638 1040-FORMAT-HEADER-DATA.
004639
004640     MOVE LA-ACCOUNT-A2          TO ACCTO.
004641     MOVE LA-ADDR-SOURCE         TO ADDRSO.
004642     MOVE LA-CARRIER-A2          TO CARRIERO.
004643     MOVE LA-CERT-NO-A2          TO CERTO.
004644     MOVE LA-CERT-SUFFIX-A2      TO SFXO.
004645     MOVE LA-DATA-SOURCE         TO DATASORO.
004646
004647     IF  LA-EFFECT-DATE-A2 EQUAL LOW-VALUES
004648         MOVE ZEROES             TO DATEO
004649
004650     ELSE
004651         MOVE LA-EFFECT-DATE-A2  TO DC-BIN-DATE-1
004652         MOVE ' '                TO DC-OPTION-CODE
004653         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
004654         MOVE DC-GREG-DATE-1-EDIT
004655                                 TO DATEO.
004656
004657     IF  LA-FOLLOW-UP-DATE EQUAL LOW-VALUES
004658         MOVE ZEROES             TO FOLLOWO
004659     ELSE
004660         MOVE LA-FOLLOW-UP-DATE  TO DC-BIN-DATE-1
004661         MOVE ' '                TO DC-OPTION-CODE
004662         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
004663         MOVE DC-GREG-DATE-1-EDIT
004664                                 TO FOLLOWO.
004665
004666     MOVE LA-FORM-A3             TO FORMO.
004667     MOVE LA-GROUPING-A2         TO GROUPO.
004668     MOVE LA-NO-OF-COPIES        TO COPIESO
004669                                    PI-689-NUMBER-COPIES.
004670
004671*    IF LA-PRINT-RESTRICTION = 'C' OR 'F' OR 'P'
004672*        MOVE LA-PRINT-RESTRICTION
004673*                                TO PRTRESTO
004674*    ELSE
004675*        MOVE SPACES             TO PRTRESTO.
004676
004677     IF  LA-RESEND-DATE = LOW-VALUES
004678         MOVE ZEROES             TO RESEND1O
004679     ELSE
004680         MOVE LA-RESEND-DATE     TO DC-BIN-DATE-1
004681         MOVE ' '                TO DC-OPTION-CODE
004682         PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
004683         MOVE DC-GREG-DATE-1-EDIT
004684                                 TO RESEND1O.
004685
004686     MOVE LA-STATE-A2            TO STATEO.
004687
004688     MOVE LA-NUMBER-LABEL-LINES  TO PI-689-NUMBER-LABEL-LINES.
004689
004690 1040-EXIT.
004691     EXIT.
004692                                 EJECT
004693 1050-MOVE-LABELS.
004694
004695     SET W-RG-NDX                TO LT-NDX.
004696     MOVE ZEROS                  TO W-RC-PC (W-RG-NDX).
004697     MOVE SPACES                 TO W-RC-SC (W-RG-NDX).
004698     MOVE LT-TEXT-LINE (LT-NDX)  TO W-RC-TEXT (W-RG-NDX).
004699
004700 1050-EXIT.
004701     EXIT.
004702                                 EJECT
004703 1060-DELETE-ADDRESS-RECORD.
004704
004705     
      * EXEC CICS DELETE
004706*        DATASET (W-ARCT-FILE-ID)
004707*        RIDFLD  (W-DELETE-KEY)
004708*    END-EXEC.
      *    MOVE '&(  R                 &   #00010855' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'2020233030303130383535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 W-DELETE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004709
004710     
      * EXEC CICS READ
004711*        DATASET (W-ARCH-FILE-ID)
004712*        SET     (ADDRESS OF LETTER-ARCHIVE)
004713*        RIDFLD  (W-ARCH-KEY)
004714*        UPDATE
004715*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010860' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130383630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004716
004717     MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES.
004718
004719     
      * EXEC CICS REWRITE
004720*         FROM      (LETTER-ARCHIVE)
004721*         DATASET   (W-ARCH-FILE-ID)
004722*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010869' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130383639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004723
004724 1060-EXIT.
004725     EXIT.
004726                                 EJECT
004727 1070-ARCH-NOT-FOUND.
004728
004729     MOVE ER-7371                TO EMI-ERROR.
004730     MOVE -1                     TO ARCHNUML.
004731     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004732     GO TO 8100-SEND-INITIAL-MAP.
004733
004734 1080-ARCHIVE-TEXT-NOT-FOUND.
004735
004736     MOVE ER-7372                TO EMI-ERROR.
004737     MOVE -1                     TO ARCHNUML.
004738     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004739     GO TO 8200-SEND-DATAONLY.
004740                                 EJECT
004741 2000-CREATE.
004742***************************************************************
004743*    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
004744*    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
004745*    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
004746*    WILL BE DISPLAYED ON THE SCREEN.                         *
004747*                                                             *
004748***************************************************************
004749
004750***************************************************************
004751*    CHECK TO SEE IF IT IS THE SAME REQUEST OR NOT.           *
004752*    IF IT IS A NEW REQUEST AND A LETTER WAS PRINTED          *
004753*    THEN FORCE AN ERROR.                                     *
004754***************************************************************
004755
004756     IF  PI-689-CREATE-NO-SCREENS
004757         GO TO 2000-GET-TEXT.
004758
004759     IF  PI-CREATE-MODE
004760         IF  PI-689-PRINT-PERFORMED
004761                 AND
004762             FORMI NOT EQUAL 9999
004763             MOVE ER-0279    TO EMI-ERROR
004764             MOVE -1         TO MAINTL
004765             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004766             GO TO 8200-SEND-DATAONLY.
004767
004768     IF  ARCHNUML GREATER THAN ZEROS
004769         MOVE ER-9320            TO EMI-ERROR
004770         MOVE -1                 TO ARCHNUML
004771         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004772         GO TO 8200-SEND-DATAONLY.
004773
004774***************************************************************
004775*    IF  A NEW LETTER IS BEING CREATED FROM SCRATCH, SAVE     *
004776*    THE EXISTING SCREEN AND PASS CONTROL TO THE TEXT EDITOR  *
004777***************************************************************
004778
004779     IF  FORMI EQUAL '9999'
004780         MOVE 16                 TO PI-TOTAL-LINES
004781         MOVE 1                  TO PI-CURRENT-LINE
004782         MOVE SPACES             TO W-FILE-TABLE
004783                                    W-LINE-CONTROL-RECORD
004784                                    W-RECORD-TABLE
004785                                    W-SINGLE-LINE
004786                                    W-SQUEEZED-LINE
004787                                    W-TS-WORK-AREA
004788                                    W-TX-TABLE
004789         PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT
004790         MOVE '3'                TO PI-ACTION
004791         MOVE FORMI              TO PI-689-FORM-NUMBER
004792                                    PI-COMM-CONTROL
004793         MOVE ZEROS              TO PI-UPDATE-SW
004794         MOVE W-PGM-EL1042       TO W-CALL-PGM
004795         GO TO 9400-XCTL.
004796
004797 2000-GET-TEXT.
004798
004799     MOVE SPACES                 TO W-FILE-TABLE
004800                                    W-LINE-CONTROL-RECORD
004801                                    W-RECORD-TABLE
004802                                    W-SINGLE-LINE
004803                                    W-SQUEEZED-LINE
004804                                    W-TS-WORK-AREA
004805                                    W-TX-TABLE.
004806     SET W-TG-NDX                TO +1
004807     MOVE ZEROS                  TO W-INITIAL-COLUMN
004808                                    W-LINE-COUNT
004809                                    W-LINE-INDENT-1
004810                                    W-LINE-INDENT-2
004811                                    W-LINE-INDENT-3
004812                                    W-LINE-INDENT-4
004813                                    W-LINE-INDENT-5
004814                                    W-PAGE
004815                                    W-PARAGRAPH-INDENT
004816                                    W-TOP-MARGIN
004817                                    W-WORK-INDENT.
004818     MOVE PI-LOWER-CASE-LETTERS  TO W-LC-CASE-IND.
004819     MOVE +70                    TO W-LAST-COLUMN.
004820     MOVE +56                    TO W-MAX-LINES-PER-PAGE.
004821     MOVE +1                     TO W-START-COLUMN.
004822     MOVE +71                    TO W-TOO-FAR.
004823
004824     MOVE PI-COMPANY-CD          TO W-TEXT-COMPANY-CD.
004825     MOVE FORMI                  TO W-TEXT-LETTER.
004826     MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.
004827     MOVE 'N'                    TO W-TEXT-BROWSED-SW.
004828
004829     
      * EXEC CICS HANDLE CONDITION
004830*         NOTFND     (2001-NOT-FOUND)
004831*         ENDFILE    (2001-NOT-FOUND)
004832*         NOTOPEN    (8050-TEXT-NOT-OPEN)
004833*    END-EXEC.
      *    MOVE '"$I''J                 ! ) #00010979' TO DFHEIV0
           MOVE X'222449274A20202020202020' &
                X'202020202020202020202120' &
                X'2920233030303130393739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004834
004835     
      * EXEC CICS STARTBR
004836*         DATASET    (W-TEXT-FILE-ID)
004837*         RIDFLD     (W-TEXT-KEY)
004838*         GTEQ
004839*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010985' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303130393835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004840
004841     
      * EXEC CICS HANDLE CONDITION
004842*         ENDFILE    (2000-ENDBR)
004843*    END-EXEC.
      *    MOVE '"$''                   ! * #00010991' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303130393931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004844
004845 2000-READ-NEXT.
004846
004847     IF  W-TG-NDX GREATER THAN W-MAX-LINES
004848         MOVE ER-0051            TO EMI-ERROR
004849         MOVE -1                 TO MAINTL
004850         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004851         GO TO 2000-ENDBR.
004852
004853     
      * EXEC CICS READNEXT
004854*         DATASET    (W-TEXT-FILE-ID)
004855*         SET        (ADDRESS OF TEXT-FILES)
004856*         RIDFLD     (W-TEXT-KEY)
004857*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00011003' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303131303033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
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
           
004858
004859     IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY
004860         GO TO 2000-ENDBR.
004861
004862     MOVE 'Y'                    TO W-TEXT-BROWSED-SW.
004863
004864     MOVE TX-FORM-SQUEEZE-CONTROL
004865                                 TO W-FORM-SQUEEZE-IND.
004866
004867     IF  PI-689-ARCHIVE-SW NOT GREATER THAN SPACES
004868         MOVE TX-ARCHIVE-SW      TO PI-689-ARCHIVE-SW.
004869
004870     IF  TX-FORM-CONTROL-LINE
004871         PERFORM 2100-PROCESS-FORM-CONTROL-LINE THRU 2100-EXIT
004872         GO TO 2000-READ-NEXT.
004873
004874     IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
004875         PERFORM 2150-PROCESS-Z-CONTROLS THRU 2150-EXIT
004876         GO TO 2000-READ-NEXT.
004877
004878     MOVE TX-TEXT-LINE           TO W-TX-TEXT (W-TG-NDX).
004879     MOVE TX-PROCESS-CONTROL     TO W-TX-PC (W-TG-NDX).
004880     PERFORM 2200-CHECK-FOR-VARIABLE THRU 2200-EXIT.
004881     MOVE TX-LINE-SQUEEZE-CONTROL
004882                                 TO W-TX-SC (W-TG-NDX).
004883
004884     SET W-TG-NDX UP BY 1.
004885     GO TO 2000-READ-NEXT.
004886
004887 2000-ENDBR.
004888
004889     
      * EXEC CICS ENDBR
004890*        DATASET     (W-TEXT-FILE-ID)
004891*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011039' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131303339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004892
004893     IF  W-TEXT-BROWSE-NOT-STARTED
004894         MOVE ER-0006            TO EMI-ERROR
004895         MOVE -1                 TO FORML
004896         MOVE AL-UABON           TO FORMA
004897         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004898         GO TO 8350-SEND-DATAONLY-ERASEAUP.
004899
004900     SET  W-TOTAL-TX-LINES       TO W-TG-NDX.
004901     SUBTRACT +1 FROM W-TOTAL-TX-LINES.
004902     MOVE 1                      TO PI-CURRENT-LINE.
004903
004904     IF  COPIESL NOT GREATER THAN ZEROS
004905         MOVE +1                 TO COPIESI
004906                                    PI-689-NUMBER-COPIES
004907         MOVE +1                 TO COPIESL
004908         MOVE AL-UNNON           TO COPIESA.
004909
004910***************************************************************
004911*    IF IT IS A FORM WITH VARIABLES, THEN RESOLVE ALL OF      *
004912*    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *
004913***************************************************************
004914
004915     PERFORM 6000-RESOLVE-VARIABLES THRU 6000-EXIT.
004916
004917*    IF  PI-689-PRINT-RESTRICTION EQUAL 'C'
004918*        IF  PI-689-CONTROL NOT GREATER THAN ZEROS
004919*            MOVE ER-7243        TO EMI-ERROR
004920*            MOVE -1             TO PRTRESTL
004921*            MOVE AL-UABON       TO PRTRESTA
004922*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004923
004924     IF  PI-CREATE-LABELS AND
004925         NOT  PI-689-LABELS-OVERRIDEN
004926         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
004927         SET W-RG-NDX UP BY +1
004928     ELSE
004929         SET W-RG-NDX            TO +1.
004930
004931     MOVE W-TOP-FORM             TO W-RC-TEXT (W-RG-NDX).
004932     SET W-RG-NDX UP BY W-TOP-MARGIN.
004933
004934     PERFORM 7400-CREATE-LETTER THRU 7400-EXIT.
004935
004936     SET PI-TOTAL-LINES          TO W-RG-NDX.
004937
004938     IF  W-FIRST-BAD-VARIABLE-FOUND
004939         SET W-RG-NDX            TO W-FIRST-BAD-VARIABLE
004940         MOVE W-FIRST-BAD-VARIABLE
004941                                 TO PI-CURRENT-LINE
004942     ELSE
004943         SET W-RG-NDX            TO 1.
004944
004945     PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
004946             VARYING
004947         W-SC-NDX FROM 1 BY 1
004948             UNTIL
004949         W-SC-NDX GREATER W-NUM-LINES-PER-SCREEN.
004950     IF  PI-689-CREATE-NO-SCREENS
004951         GO TO 2000-EXIT
004952     ELSE
004953         IF  W-FORM-CHANGED
004954             MOVE '3'            TO PI-ACTION
004955             MOVE -1             TO MAINTL
004956             GO TO 8100-SEND-INITIAL-MAP
004957         ELSE
004958             MOVE '3'            TO PI-ACTION
004959             MOVE -1             TO MAINTL
004960             GO TO 8350-SEND-DATAONLY-ERASEAUP.
004961
004962 2000-EXIT.
004963     EXIT.
004964
004965 2001-NOT-FOUND.
004966
004967     MOVE ER-0006                TO EMI-ERROR.
004968     MOVE -1                     TO FORML.
004969     MOVE AL-UABON               TO FORMA.
004970     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004971     GO TO 8350-SEND-DATAONLY-ERASEAUP.
004972                                 EJECT
004973 2100-PROCESS-FORM-CONTROL-LINE.
004974
004975     MOVE TX-TEXT-LINE           TO W-LINE-CONTROL-RECORD.
004976
004977     IF  W-LC-LINE-WIDTH NUMERIC
004978             AND
004979         W-LC-LINE-WIDTH GREATER THAN ZEROS
004980         COMPUTE W-INITIAL-COLUMN
004981             = (70 - W-LC-LINE-WIDTH) / 2
004982         COMPUTE W-START-COLUMN = W-INITIAL-COLUMN + 1
004983         COMPUTE W-LAST-COLUMN ROUNDED
004984             = W-INITIAL-COLUMN + W-LC-LINE-WIDTH
004985         COMPUTE W-TOO-FAR = W-LAST-COLUMN + 1
004986     ELSE
004987         MOVE 70                 TO W-LC-LINE-WIDTH.
004988
004989     IF  W-LC-PARAGRAPH-INDENT NUMERIC
004990             AND
004991         W-LC-PARAGRAPH-INDENT GREATER THAN ZEROS
004992         MOVE W-LC-PARAGRAPH-INDENT
004993                                 TO W-PARAGRAPH-INDENT.
004994
004995     IF  W-LC-MAX-LINES-PER-PAGE NUMERIC
004996             AND
004997         W-LC-MAX-LINES-PER-PAGE GREATER THAN ZEROS
004998         MOVE W-LC-MAX-LINES-PER-PAGE
004999                                 TO W-MAX-LINES-PER-PAGE.
005000
005001     IF  W-LC-LINE-ADJUST NUMERIC
005002             AND
005003         W-LC-LINE-ADJUST GREATER THAN ZEROS
005004         COMPUTE W-TOTAL-LINE-LENGTH
005005             = W-LC-LINE-WIDTH + (W-LC-LINE-ADJUST * 2)
005006         IF  W-TOTAL-LINE-LENGTH LESS THAN +71
005007             ADD W-LC-LINE-ADJUST
005008                                 TO W-INITIAL-COLUMN
005009                                    W-LAST-COLUMN
005010                                    W-START-COLUMN
005011                                    W-TOO-FAR.
005012
005013     IF  W-LC-LINE-INDENT-1 NUMERIC
005014             AND
005015         W-LC-LINE-INDENT-1 LESS THAN W-LC-LINE-WIDTH
005016         MOVE W-LC-LINE-INDENT-1 TO W-LINE-INDENT-1
005017     ELSE
005018         MOVE +0                 TO W-LINE-INDENT-1.
005019
005020     IF  W-LC-LINE-INDENT-2 NUMERIC
005021             AND
005022         W-LC-LINE-INDENT-2 LESS THAN W-LC-LINE-WIDTH
005023         MOVE W-LC-LINE-INDENT-2 TO W-LINE-INDENT-2
005024     ELSE
005025         MOVE +0                 TO W-LINE-INDENT-2.
005026
005027     IF  W-LC-LINE-INDENT-3 NUMERIC
005028             AND
005029         W-LC-LINE-INDENT-3 LESS THAN W-LC-LINE-WIDTH
005030         MOVE W-LC-LINE-INDENT-3 TO W-LINE-INDENT-3
005031     ELSE
005032         MOVE +0                 TO W-LINE-INDENT-3.
005033
005034     IF  W-LC-LINE-INDENT-4 NUMERIC
005035             AND
005036         W-LC-LINE-INDENT-4 LESS THAN W-LC-LINE-WIDTH
005037         MOVE W-LC-LINE-INDENT-4 TO W-LINE-INDENT-4
005038     ELSE
005039         MOVE +0                 TO W-LINE-INDENT-4.
005040
005041     IF  W-LC-LINE-INDENT-5 NUMERIC
005042             AND
005043         W-LC-LINE-INDENT-5 LESS THAN W-LC-LINE-WIDTH
005044         MOVE W-LC-LINE-INDENT-5 TO W-LINE-INDENT-5
005045
005046     ELSE
005047         MOVE +0                 TO W-LINE-INDENT-5.
005048
005049     IF  W-LC-TOP-MARGIN NUMERIC
005050         MOVE W-LC-TOP-MARGIN    TO W-TOP-MARGIN
005051     ELSE
005052         MOVE +0                 TO W-TOP-MARGIN.
005053
005054     IF W-LC-CASE-IND = 'Y' OR 'N'
005055         NEXT SENTENCE
005056     ELSE
005057         MOVE PI-LOWER-CASE-LETTERS
005058                                 TO W-LC-CASE-IND.
005059
005060 2100-EXIT.
005061     EXIT.
005062                                 EJECT
005063 2150-PROCESS-Z-CONTROLS.
005064
005065     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
005066
005067     IF PI-689-RESEND-DATE-1 = LOW-VALUES
005068        IF W-DAYS-TO-RESEND NUMERIC
005069           IF W-DAYS-TO-RESEND > ZEROS
005070              MOVE '6'           TO DC-OPTION-CODE
005071              MOVE W-SAVE-BIN-NEXT-BUS-DT
005072                                 TO DC-BIN-DATE-1
005073              MOVE ZEROS         TO DC-ELAPSED-MONTHS
005074              MOVE W-DAYS-TO-RESEND
005075                                 TO DC-ELAPSED-DAYS
005076              PERFORM 9500-LINK-DATE-CONVERT
005077                                 THRU 9500-EXIT
005078              IF NO-CONVERSION-ERROR
005079                 MOVE DC-BIN-DATE-2
005080                                 TO PI-689-RESEND-DATE-1
005081                 MOVE DC-GREG-DATE-1-EDIT
005082                                 TO RESEND1O
005083                                    PI-689-RESEND1-EDIT
005084                 MOVE AL-UANON
005085                                 TO RESEND1A
005086                 MOVE +8         TO RESEND1L
005087              ELSE
005088                 MOVE ER-3770
005089                                 TO EMI-ERROR
005090                 MOVE -1         TO MAINTL
005091                 PERFORM 9900-ERROR-FORMAT
005092                                 THRU 9900-EXIT
005093                 GO TO 8200-SEND-DATAONLY
005094              END-IF
005095           END-IF
005096        END-IF
005097     END-IF
005098
005099     IF W-FORM-TO-RESEND NOT = SPACES AND ZEROS
005100        MOVE W-FORM-TO-RESEND    TO PI-689-RESEND-LETR-1
005101     END-IF
005102
005103     IF  PI-689-FOLLOW-UP-DATE EQUAL LOW-VALUES
005104         IF  W-DAYS-TO-FOLLOW-UP NUMERIC
005105            IF  W-DAYS-TO-FOLLOW-UP >= ZEROS
005106                 MOVE '6'        TO DC-OPTION-CODE
005107                 MOVE W-SAVE-BIN-DATE
005108                                 TO DC-BIN-DATE-1
005109                 MOVE ZEROS      TO DC-ELAPSED-MONTHS
005110                 MOVE W-DAYS-TO-FOLLOW-UP
005111                                 TO DC-ELAPSED-DAYS
005112                 PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
005113                 IF  NO-CONVERSION-ERROR
005114                     MOVE DC-BIN-DATE-2
005115                                 TO PI-689-FOLLOW-UP-DATE
005116                     MOVE DC-GREG-DATE-1-EDIT
005117                                 TO FOLLOWO
005118                     MOVE AL-UANON
005119                                 TO FOLLOWA
005120                     MOVE +8     TO FOLLOWL
005121                 ELSE
005122                     MOVE ER-3771
005123                                 TO EMI-ERROR
005124                     MOVE -1     TO MAINTL
005125                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005126                     GO TO 8200-SEND-DATAONLY.
005127
005128     IF  COPIESI NOT NUMERIC
005129             OR
005130         COPIESI NOT GREATER THAN ZEROS
005131         IF  W-NUMBER-OF-COPIES NUMERIC
005132             IF  W-NUMBER-OF-COPIES GREATER THAN ZEROS
005133                 MOVE W-NUMBER-OF-COPIES
005134                                 TO COPIESI
005135                                    PI-689-NUMBER-COPIES
005136                 MOVE AL-UNNON   TO COPIESA
005137                 MOVE +1         TO COPIESL
005138             ELSE
005139                 MOVE +1         TO COPIESI
005140                                    PI-689-NUMBER-COPIES
005141                 MOVE AL-UNNON   TO COPIESA
005142                 MOVE +1         TO COPIESL
005143         ELSE
005144             MOVE +1             TO COPIESI
005145                                    PI-689-NUMBER-COPIES
005146             MOVE AL-UNNON       TO COPIESA
005147             MOVE +1             TO COPIESL.
005148
005149     IF W-PROMPT-LETTER EQUAL 'Y'
005150         MOVE 'Y'                TO PI-PROMPT-IND
005151         MOVE ER-0894            TO EMI-ERROR
005152         MOVE -1                 TO MAINTL
005153         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005154     END-IF.
005155
005156     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD
005157                                           ENCO
005158     MOVE AL-UANON                      TO ENCA
005159     MOVE +3                            TO ENCL
005160
005161     IF W-PRINT-CERTIFICATE = 'Y'
005162        MOVE 'Y' TO PI-CERT-REQ-IND
005163        IF CERTIDL > ZEROS
005164            IF CERTIDI NUMERIC
005165              MOVE AL-UANON      TO CERTIDA
005166              MOVE CERTIDI       TO PI-CERT-FORM-ID
005167            ELSE
005168              MOVE ER-1778       TO EMI-ERROR
005169              MOVE -1            TO CERTIDL
005170              MOVE AL-UABON      TO CERTIDA
005171              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005172           END-IF
005173        ELSE
005174              MOVE ER-0715       TO EMI-ERROR
005175              MOVE -1            TO CERTIDL
005176              MOVE AL-UABON      TO CERTIDA
005177              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005178        END-IF
005179     END-IF.
005180
005181     IF W-REASONS-REQUIRED = 'Y'
005182        MOVE 'Y' TO PI-REASON-REQ-IND
005183        IF PI-ENDT-ARCH-NO NOT > ZERO
005184          MOVE ER-9840       TO EMI-ERROR
005185          MOVE -1            TO FORML
005186          MOVE AL-UABON      TO FORMA
005187          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005188        END-IF
005189     END-IF.
005190
005191*    IF  PI-689-PRINT-RESTRICTION NOT = 'C' AND 'F' AND 'P' AND
005192*                                       SPACES
005193*        IF  W-PRINT-RESTRICTED
005194*            MOVE W-PRINT-RESTRICTED-IND
005195*                                TO PRTRESTI
005196*                                   PI-689-PRINT-RESTRICTION
005197*            MOVE AL-UANON       TO PRTRESTA
005198*            MOVE +1             TO PRTRESTL.
005199
005200     .
005201 2150-EXIT.
005202     EXIT.
005203                                 EJECT
005204 2200-CHECK-FOR-VARIABLE.
005205
005206     IF  W-TX-TEXT (W-TG-NDX) EQUAL SPACES
005207         GO TO 2200-EXIT.
005208
005209     SET W-TX-NDX                TO +1.
005210
005211 2200-CONTINUE.
005212
005213     SEARCH W-TX-CHAR
005214         VARYING
005215             W-TX-NDX
005216         AT END
005217             GO TO 2200-EXIT
005218         WHEN
005219             W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
005220             NEXT SENTENCE.
005221
005222     SET W-TX-NDX UP BY +1.
005223
005224     IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
005225         PERFORM 2220-BUILD-VARIABLE-NUMBER THRU 2220-EXIT
005226         GO TO 2200-CONTINUE.
005227
005228 2200-EXIT.
005229     EXIT.
005230                                 EJECT
005231 2220-BUILD-VARIABLE-NUMBER.
005232
005233     SET W-TX-NDX UP BY +1.
005234     MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V1.
005235     SET W-TX-NDX UP BY +1.
005236     MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V2.
005237     SET W-TX-NDX UP BY +1.
005238     MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V3.
005239
005240     IF  W-VAR-RELATIVE-NUM NUMERIC
005241         SET W-FILE-NDX TO W-VARIABLE-SOURCE (W-VAR-RELATIVE-NUM)
005242         MOVE 'Y' TO W-FILE-USE-IND (W-FILE-NDX).
005243
005244 2220-EXIT.
005245     EXIT.
005246                                 EJECT
005247 4000-ROLL-PAGE.
005248
005249     IF  ENTERPFL NOT EQUAL ZEROS
005250         MOVE -1                 TO ENTERPFL
005251     ELSE
005252         MOVE -1                 TO MAINTL.
005253
005254     IF  PI-TOTAL-LINES EQUAL 0
005255         MOVE ER-0047            TO EMI-ERROR
005256         MOVE -1                 TO MAINTL
005257         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005258         GO TO 8200-SEND-DATAONLY.
005259
005260     COMPUTE W-TEMP-CURRENT-LINE EQUAL
005261         PI-CURRENT-LINE + W-ROLL-COUNTER.
005262
005263     IF  W-TEMP-CURRENT-LINE NEGATIVE
005264             OR
005265         W-TEMP-CURRENT-LINE EQUAL ZEROS
005266         MOVE ER-0067            TO EMI-ERROR
005267         MOVE -1                 TO MAINTL
005268         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005269         MOVE 1                  TO W-TEMP-CURRENT-LINE.
005270
005271*    IF  W-TEMP-CURRENT-LINE GREATER THAN PI-TOTAL-LINES
005272*            AND
005273*        (EIBAID EQUAL DFHPF1 OR DFHPF6)
005274*        MOVE ER-0066            TO EMI-ERROR
005275*        MOVE -1                 TO MAINTL
005276*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005277*        COMPUTE W-TEMP-CURRENT-LINE
005278*            = PI-TOTAL-LINES + 1 - W-NUM-LINES-PER-SCREEN
005279*        IF  W-TEMP-CURRENT-LINE NEGATIVE
005280*                OR
005281*            W-TEMP-CURRENT-LINE = ZEROS
005282*            MOVE 1 TO W-TEMP-CURRENT-LINE.
005283
005284     IF  EMI-ERROR NOT EQUAL ER-0191
005285             AND
005286         MAINTI NOT EQUAL 'S'
005287         PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
005288         PERFORM 4100-SET-NDX THRU 4100-EXIT
005289         PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
005290                 VARYING
005291             W-SC-NDX FROM 1 BY 1
005292                 UNTIL
005293             W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
005294
005295     IF  EMI-ERROR EQUAL ER-0066 OR ER-0067 OR ER-0000 OR
005296                         ER-0191
005297         NEXT SENTENCE
005298     ELSE
005299         GO TO 8200-SEND-DATAONLY.
005300
005301     MOVE W-TEMP-CURRENT-LINE    TO PI-CURRENT-LINE
005302     SET W-RG-NDX                TO PI-CURRENT-LINE
005303     MOVE LOW-VALUES             TO EL689RI
005304
005305*    PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
005306     PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
005307     SET W-RG-NDX                TO PI-CURRENT-LINE
005308
005309     PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
005310             VARYING
005311         W-SC-NDX FROM 1 BY 1
005312             UNTIL
005313         W-SC-NDX GREATER W-NUM-LINES-PER-SCREEN.
005314
005315     GO TO 8200-SEND-DATAONLY.
005316                                 EJECT
005317 4100-SET-NDX.
005318
005319     IF  PI-CURRENT-LINE EQUAL 0
005320         SET W-RG-NDX            TO 1
005321     ELSE
005322         SET W-RG-NDX            TO PI-CURRENT-LINE.
005323
005324 4100-EXIT.
005325      EXIT.
005326                                 EJECT
005327 4200-UPDATE-TABLE-FROM-SCREEN.
005328
005329     IF  W-SC-TEXTL (W-SC-NDX) NOT EQUAL ZEROS
005330         MOVE ZEROS              TO W-SC-TEXTL (W-SC-NDX)
005331         IF  W-RG-NDX GREATER THAN PI-TOTAL-LINES
005332             IF  PI-TOTAL-LINES EQUAL W-MAX-LINES
005333                 MOVE ER-0051    TO EMI-ERROR
005334                 MOVE -1         TO MAINTL
005335                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005336                 GO TO 8200-SEND-DATAONLY
005337             ELSE
005338                 MOVE W-SC-TEXT (W-SC-NDX)
005339                                 TO W-RC-TEXT (W-RG-NDX)
005340                 ADD 1           TO PI-TOTAL-LINES
005341         ELSE
005342             MOVE W-SC-TEXT (W-SC-NDX)
005343                                 TO W-RC-TEXT (W-RG-NDX).
005344
005345     SET W-RG-NDX UP BY 1.
005346
005347 4200-EXIT.
005348      EXIT.
005349                                 EJECT
005350
005351
005352 4500-CONNECT-TO-DB.
005353
005354****  The below code is for when the db has been
005355****  converted to sql server 2016
005356     evaluate ws-kix-myenv
005357        when 'cid1p'
005358           move '//sdv-db01.cso.local:1433;'
005359                                 to p-sql-server
005360        when 'mdoff'
005361           move '//hov-tstdb01.cso.local:55330;'
005362                                 to p-sql-server
005363        when other
005364           move '//hov-tstdb01.cso.local:1433;'
005365                                 to p-sql-server
005366     end-evaluate
005367
005368
005369     move 'Logic'                to p-sql-database
005370
005371     CALL 'SQLCONNECT' USING sqlconnect-parms
005372     display ' ret code ' p-connect-return-code
005373     move p-connect-return-code  to sqlcode
005374     move p-sql-return-message   to sqlerrmc
005375
005376
005377*
005378*     EXEC SQL
005379**       CONNECT TO :SVR USER :USR-PASS
005380*        CONNECT TO :SVR
005381*          USER     :USR
005382*          USING    :PASS
005383*     END-EXEC
005384
005385     IF SQLCODE NOT = 0
005386        DISPLAY "ERROR: CANNOT CONNECT "
005387        DISPLAY SQLCODE
005388        DISPLAY SQLERRMC
005389        GO TO 4500-EXIT
005390     END-IF
005391
005392     .
005393 4500-EXIT.
005394     EXIT.
005395
005396
005397
005398 4600-GET-NEXT-BUS-DT.
005399
005400     MOVE W-SAVE-EDIT-A-DATE     TO WS-CYCLE-DATE
005401     MOVE SPACES                 TO WS-NEXT-BUS-DT
005402
005403     IF WS-KIXHOST = 'logictest'
005405        EXEC SQL
                CALL NaperTestCalcNextBusDt
005406            (
005407              @cycledate = :WS-CYCLE-DATE,
005408              @nextbusdate = :WS-NEXT-BUS-DT
005409            )
005410        END-EXEC
005411     ELSE
005413        EXEC SQL
                CALL NaperProdCalcNextBusDt
005414            (
005415              @cycledate = :WS-CYCLE-DATE,
005416              @nextbusdate = :WS-NEXT-BUS-DT
005417            )
005418        END-EXEC
005419     END-IF
005420
005421     IF SQLCODE NOT = 0
005422        DISPLAY "ERROR: DID NOT RETURN NEXT BUS DT "
005423        DISPLAY ' SQL RETURN CODE ' SQLCODE
005424        DISPLAY ' SQL ERR MESS    ' SQLERRMC
005425        GO TO 4600-EXIT
005426     END-IF
005427
005428     .
005429 4600-EXIT.
005430     EXIT.
005431
005432
005433
005434 4700-DISCONNECT.
005435
005437     EXEC SQL
              DISCONNECT
005438     END-EXEC
005439     .
005440 4700-EXIT.
005441     EXIT.
005442
005443 5000-EDIT-MODE.
005444***************************************************************
005445*    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
005446*    AND THE W-TS-TABLE OF LETTER TEXT.  IT WILL THEN         *
005447*    TRANSFER TO THE W-TEXT-EDITOR PROGRAM.                   *
005448***************************************************************
005449
005450     IF  PI-SHOW-MODE
005451         MOVE ER-0188            TO EMI-ERROR
005452         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005453         MOVE -1                 TO MAINTL
005454         MOVE AL-UABON           TO MAINTA
005455         GO TO 8200-SEND-DATAONLY.
005456
005457     IF  PI-CURRENT-LINE EQUAL ZEROS
005458         MOVE ER-0187            TO EMI-ERROR
005459         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005460         MOVE -1                 TO MAINTL
005461         GO TO 8200-SEND-DATAONLY.
005462
005463
005464     PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
005465
005466     PERFORM 4100-SET-NDX THRU 4100-EXIT.
005467     PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
005468             VARYING
005469         W-SC-NDX FROM 1 BY 1
005470             UNTIL
005471         W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
005472
005473     PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT.
005474
005475     MOVE PI-689-FORM-NUMBER     TO PI-COMM-CONTROL.
005476     MOVE ZEROS                  TO PI-UPDATE-SW.
005477     MOVE '1'                    TO PI-ENTRY-CD-1.
005478     MOVE W-PGM-EL1042           TO W-CALL-PGM.
005479     GO TO 9400-XCTL.
005480                                 EJECT
005481 5400-LETTER-RELEASE.
005482***************************************************************
005483*    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
005484*    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS IN       *
005485*    THE ARCHIVE FILE.                                        *
005486*    THE FUNCTIONS BELOW WILL BE PERFORMED.                   *
005487*        1. CHECK SECURITY.                                   *
005488*        2. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS.        *
005489*        3. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
005490*        4. WRITE THE NEW ARCHIVE RECORDS FROM W-TS-TABLE.    *
005491*        5. RESET INSURED'S CONTROL FIELDS AND RETURN THE     *
005492*           ARCHIVE NUMBER USED  TO FILE THE RECORDS.         *
005493***************************************************************
005494
005495     IF  NOT MODIFY-CAP
005496         MOVE ER-0070            TO EMI-ERROR
005497         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005498         MOVE -1                 TO MAINTL
005499         GO TO 8200-SEND-DATAONLY.
005500
005501     IF  PI-SHOW-MODE
005502         MOVE ER-0188            TO EMI-ERROR
005503         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005504         MOVE -1                 TO MAINTL
005505         MOVE AL-UABON           TO MAINTA
005506         GO TO 8200-SEND-DATAONLY.
005507
005508      IF  ARCHNUML GREATER THAN ZEROS
005509          MOVE ER-9320            TO EMI-ERROR
005510          MOVE -1                 TO ARCHNUML
005511          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005512          GO TO 8200-SEND-DATAONLY.
005513
005514*     IF  PI-CURRENT-LINE EQUAL ZEROS
005515*        MOVE ER-0187            TO EMI-ERROR
005516*        MOVE -1                 TO MAINTL
005517*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005518*        GO TO 8200-SEND-DATAONLY.
005519
005520
005521*    PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
005522*
005523*    IF  NOT PI-689-CREATE-NO-SCREENS
005524*        PERFORM 4100-SET-NDX THRU 4100-EXIT
005525*        PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
005526*                VARYING
005527*            W-SC-NDX FROM 1 BY 1
005528*                UNTIL
005529*            W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
005530*        IF  EMI-FATAL-CTR GREATER THAN ZEROS
005531*                OR
005532*            PI-689-FATAL-ERROR
005533*            GO TO 8200-SEND-DATAONLY.
005534*
005535*    MOVE SPACES                 TO W-REMAINING-VAR-SW.
005536*    PERFORM 7915-SEARCH-REMAINING-VARS THRU 7915-EXIT
005537*            VARYING
005538*        W-RVS-NDX FROM +1 BY +1
005539*            UNTIL
005540*        W-RVS-NDX > +36500
005541*            OR
005542*        W-REMAINING-VAR-FOUND.
005543*
005544*    IF  NOT PI-689-CREATE-NO-SCREENS
005545*        IF  W-REMAINING-VAR-FOUND
005546*            GO TO 4000-ROLL-PAGE.
005547
005548*    MOVE SPACES                 TO W-CNTL-KEY.
005549*    MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
005550*    MOVE '1'                    TO W-CNTL-RECORD-TYPE.
005551*    MOVE ZEROS                  TO W-CNTL-SEQ-NO.
005552*
005553*    EXEC CICS HANDLE CONDITION
005554*         NOTOPEN    (8040-CNTL-NOT-OPEN)
005555*         NOTFND     (5470-CNTL-NOT-FOUND)
005556*    END-EXEC.
005557*
005558*    EXEC CICS READ
005559*         DATASET    (W-CNTL-FILE-ID)
005560*         SET        (ADDRESS OF CONTROL-FILE)
005561*         RIDFLD     (W-CNTL-KEY)
005562*         UPDATE
005563*    END-EXEC.
005564*
005565*    IF  CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
005566*        MOVE ZEROS              TO CF-CREDIT-LAST-ARCH-NUM
005567*                                   CF-CREDIT-START-ARCH-NUM.
005568*
005569*    ADD 1                       TO CF-CREDIT-LAST-ARCH-NUM.
005570*    MOVE CF-CREDIT-LAST-ARCH-NUM
005571*                                TO W-ARCH-NUMBER
005572*                                   W-ARCT-NUMBER.
005573*
005574*    EXEC CICS REWRITE
005575*         FROM      (CONTROL-FILE)
005576*         DATASET   (W-CNTL-FILE-ID)
005577*    END-EXEC
005578
005579*    EXEC CICS HANDLE CONDITION
005580*         NOTOPEN   (8010-ARCH-NOT-OPEN)
005581*    END-EXEC.
005582*
005583*    EXEC CICS GETMAIN
005584*       SET      (ADDRESS OF LETTER-ARCHIVE)
005585*       LENGTH   (W-ARCH-LENGTH)
005586*    END-EXEC
005587*
005588*    MOVE SPACES                 TO LETTER-ARCHIVE
005589*    MOVE 'LA'                   TO LA-RECORD-ID.
005590*    MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
005591*                                   LA-ARCHIVE-NO-A2
005592*                                   LA-ARCHIVE-NO-A3
005593*                                   LA-ARCHIVE-NO-A4
005594*                                   LA-ARCHIVE-NO-A5
005595*                                   LA-ARCHIVE-NO-A6
005596*                                   PI-689-ARCHIVE-NUMBER.
005597*    MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
005598*                                   LA-COMPANY-CD-A2
005599*                                   LA-COMPANY-CD-A3
005600*                                   LA-COMPANY-CD-A4
005601*                                   LA-COMPANY-CD-A5
005602*                                   LA-COMPANY-CD-A6.
005603*    MOVE PI-689-CARRIER         TO LA-CARRIER-A2
005604*                                   LA-CARRIER-A3
005605*                                   LA-CARRIER-A4
005606*                                   LA-CARRIER-A5.
005607*    MOVE PI-689-EFF-DATE        TO LA-EFFECT-DATE-A2.
005608*    MOVE PI-689-GROUPING        TO LA-GROUPING-A2
005609*                                   LA-GROUPING-A3
005610*                                   LA-GROUPING-A4
005611*                                   LA-GROUPING-A5.
005612*    MOVE PI-689-ACCOUNT         TO LA-ACCOUNT-A2
005613*                                   LA-ACCOUNT-A3
005614*                                   LA-ACCOUNT-A4
005615*                                   LA-ACCOUNT-A5.
005616*    MOVE PI-689-STATE           TO LA-STATE-A2
005617*                                   LA-STATE-A3
005618*                                   LA-STATE-A4
005619*                                   LA-STATE-A5.
005620*
005621*    IF  PI-689-DATA-SOURCE EQUAL '3'
005622*            AND
005623*        PI-689-RESP-PERSON GREATER THAN SPACES
005624*        MOVE PI-689-RESP-PERSON
005625*                                TO LA-RESP-PERSON-A2
005626*        MOVE PI-689-TYPE        TO LA-TYPE-A2
005627*    ELSE
005628*        IF  PI-689-CERT-PRIME GREATER THAN SPACES
005629*            MOVE PI-689-CERT-PRIME
005630*                                TO LA-CERT-PRIME-A2
005631*            MOVE PI-689-CERT-SFX
005632*                                TO LA-CERT-SUFFIX-A2
005633*        ELSE
005634*            MOVE LOW-VALUE      TO LA-RESP-PERSON-A2
005635*                                   LA-TYPE-A2.
005636*
005637*    IF  PI-689-ENTRY-BATCH GREATER THAN SPACES
005638*        MOVE PI-689-ENTRY-BATCH TO LA-ENTRY-A6
005639*        IF  PI-689-CONTROL GREATER THAN ZEROS
005640*                AND
005641*            PI-689-PRINT-RESTRICTION EQUAL 'C'
005642*            MOVE PI-689-CONTROL TO LA-QUE-CONTROL-A6
005643*            MOVE 'CK'           TO LA-FILLER
005644*        ELSE
005645*            NEXT SENTENCE
005646*    ELSE
005647*        IF  PI-689-CONTROL GREATER THAN ZEROS
005648*            MOVE PI-689-CONTROL TO LA-QUE-CONTROL-A6
005649*            MOVE 'CK'           TO LA-FILLER
005650*        ELSE
005651*            MOVE LOW-VALUES     TO LA-ENTRY-A6.
005652*
005653*    MOVE ZEROS                  TO W-SEQ-CTR.
005654*
005655*    MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
005656*
005657*    MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
005658*                                   LA-INITIAL-PRINT-DATE
005659*                                   LA-SENT-DATE
005660*                                   LA-REPLY-DATE.
005661*    MOVE 'A'                    TO LA-STATUS.
005662*
005663*    IF  PI-689-NUMBER-COPIES NOT NUMERIC
005664*            OR
005665*        PI-689-NUMBER-COPIES EQUAL ZEROS
005666*        MOVE  1                 TO LA-NO-OF-COPIES
005667*    ELSE
005668*        MOVE PI-689-NUMBER-COPIES
005669*                                TO LA-NO-OF-COPIES.
005670*
005671*    MOVE PI-689-FORM-NUMBER     TO LA-FORM-A3.
005672*    MOVE PI-689-FOLLOW-UP-DATE  TO LA-FOLLOW-UP-DATE.
005673*    MOVE PI-689-PRINT-RESTRICTION
005674*                                TO LA-PRINT-RESTRICTION
005675*    MOVE PI-689-LABEL-SOURCE    TO LA-ADDR-SOURCE.
005676*    MOVE PI-689-DATA-SOURCE     TO LA-DATA-SOURCE.
005677*    MOVE PI-689-RESEND-DATE-1   TO LA-RESEND-DATE
005678*    MOVE PI-689-RESEND-LETR-1   TO LA-RESEND-LETR
005679*    MOVE W-CURRENT-SAVE         TO LA-CREATION-DATE.
005680*
005681*    IF  PI-689-PRINT-PERFORMED
005682*        MOVE SPACES             TO PI-689-PRINT-SW
005683*        MOVE W-CURRENT-SAVE     TO LA-INITIAL-PRINT-DATE
005684*        IF  LA-RESEND-DATE = LOW-VALUES
005685*            MOVE 'C'            TO LA-STATUS.
005686*
005687*    MOVE PI-689-NUMBER-LABEL-LINES
005688*                                TO LA-NUMBER-LABEL-LINES.
005689*    MOVE ZEROS                  TO PI-689-NUMBER-TEXT-RECORDS.
005690*
005691*    PERFORM 5410-CREATE-TEXT-RECORDS THRU 5410-EXIT.
005692*
005693*    MOVE PI-689-NUMBER-TEXT-RECORDS
005694*    MOVE ZEROS                  TO LA-NO-OF-TEXT-RECORDS.
005695*
005696*    EXEC CICS HANDLE CONDITION
005697*        DUPREC    (5460-DUPLICATE-ARCHIVE-RCD)
005698*    END-EXEC.
005699*
005700*    PERFORM 5650-WRITE-ARCHIVE THRU 5650-EXIT.
005701
005702*    MOVE ER-0280                TO EMI-ERROR.
005703*    MOVE -1                     TO MAINTL.
005704*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005705*    MOVE W-ARCH-NUMBER          TO W-ARCH-SUPPRESS
005706*    MOVE W-ARCH-EDIT            TO EMI-TEXT-VARIABLE (1).
005707
005708     .
005709 5400-END.
005710
005711     DISPLAY ' MADE IT TO 5400 '
005712
005713     MOVE SPACES                 TO BL-INPUT
005714     MOVE ZEROS                  TO BL-ARCHIVE-NO
005715     MOVE PI-689-DATA-SOURCE     TO BL-DATA-SRCE
005716     MOVE PI-689-CARRIER         TO BL-CARRIER
005717     MOVE PI-689-GROUPING        TO BL-GROUP
005718     MOVE PI-689-STATE           TO BL-STATE
005719     MOVE PI-689-ACCOUNT         TO BL-ACCOUNT
005720     IF PI-689-EFF-DATE NOT = LOW-VALUES
005721        MOVE PI-689-EFF-DATE     TO DC-BIN-DATE-1
005722        MOVE ' '                 TO DC-OPTION-CODE
005723        PERFORM 9500-LINK-DATE-CONVERT
005724                                 THRU 9500-EXIT
005725        IF NO-CONVERSION-ERROR
005726           MOVE DC-GREG-DATE-1-EDIT
005727                                 TO BL-EFF-DT
005728        END-IF
005729     END-IF
005730     MOVE PI-689-CERT-NO         TO BL-CERT-NO
005731     MOVE PI-689-ENTRY-BATCH     TO BL-BATCH-NO
005732     MOVE PI-689-SEQ-NO          TO BL-BATCH-SEQ
005733     MOVE PI-689-RESP-PERSON     TO BL-RESP-NO
005734     MOVE PI-689-FORM-NUMBER     TO BL-LETTER-ID
005735     MOVE PI-689-NUMBER-COPIES   TO BL-NO-OF-COPIES
005736     IF PI-ENDT-ARCH-NO > ZERO
005737         MOVE PI-ENDT-ARCH-NO    TO BL-ENDT-ARCH-NO
005738     END-IF
005739     MOVE 'LETTER  '             TO BL-SOURCE-SCREEN
005740     MOVE PI-PROCESSOR-ID        TO BL-PROC-ID
005741*    MOVE 'ALWA'                 TO BL-PROC-ID
005742     MOVE PI-COMPANY-ID          TO BL-COMP-ID
005743*    MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
005744     MOVE PI-ENCLOSURE-CD        TO BL-ENC-CD
005745     IF PI-689-RESEND-DATE-1 NOT = LOW-VALUES
005746        MOVE PI-689-RESEND-DATE-1 TO DC-BIN-DATE-1
005747        MOVE ' '                 TO DC-OPTION-CODE
005748        PERFORM 9500-LINK-DATE-CONVERT
005749                                 THRU 9500-EXIT
005750        IF NO-CONVERSION-ERROR
005751           MOVE DC-GREG-DATE-B-EDIT
005752                                 TO BL-RESEND-DT
005753        END-IF
005754     END-IF
005755     IF PI-689-FOLLOW-UP-DATE NOT = LOW-VALUES
005756        MOVE PI-689-FOLLOW-UP-DATE TO DC-BIN-DATE-1
005757        MOVE ' '                 TO DC-OPTION-CODE
005758        PERFORM 9500-LINK-DATE-CONVERT
005759                                 THRU 9500-EXIT
005760        IF NO-CONVERSION-ERROR
005761           MOVE DC-GREG-DATE-B-EDIT
005762                                 TO BL-FOLLOW-UP-DT
005763        END-IF
005764     END-IF
005765*    MOVE IFF-COMMENTS           TO BL-COMMENTS
005766*    MOVE 'TEST COMMENTS'        TO BL-COMMENTS
005767     MOVE PI-CERT-FORM-ID        TO BL-CERT-FORM-ID
005768     if eibaid = dfhpf7
005769        move 'B'                 TO BL-WRITE-ERARCH
005770     else
005771        move 'T'                 to bl-write-erarch
005772     end-if
005773
005774    DISPLAY ' ABOUT TO LINK TO NSRASBL '
005775*****************************************
005776* Invoke the LETTER business logic
005777*****************************************
005778
005779     
      * exec cics link
005780*       program('NSRASBL')
005781*       commarea(srch-commarea)
005782*    end-exec.
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'NSRASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00011929' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303131393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005783
005784     DISPLAY ' MADE IT BACK FROM NSRASBL ' bl-status ' '
005785        bl-message ' ' bl-archive-no
005786     move bl-archive-no to w-arch-number
005787
005788
005789     if eibaid = dfhpf7
005790        PERFORM 5440-ADD-BILLING-NOTE THRU 5440-EXIT
005791        MOVE ER-0280                TO EMI-ERROR
005792        MOVE -1                     TO MAINTL
005793        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005794        MOVE W-ARCH-NUMBER          TO W-ARCH-SUPPRESS
005795        MOVE W-ARCH-EDIT            TO EMI-TEXT-VARIABLE (1)
005796        MOVE '2'                    TO PI-ACTION
005797        MOVE ZEROS                  TO PI-TOTAL-LINES
005798                                       PI-CURRENT-LINE
005799        MOVE SPACES                 TO PI-689-PRINT-SW
005800                                       PI-689-FORM-NUMBER
005801        GO TO 9300-DFHCLEAR
005802     else
005803        move w-arch-number          to archnumo
005804        MOVE ER-1818                TO EMI-ERROR
005805        MOVE -1                     TO MAINTL
005806        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005807     end-if
005808
005809     MOVE -1                     TO MAINTL
005810     GO TO 8200-SEND-DATAONLY
005811
005812     .
005813 5400-EXIT.
005814     EXIT.
005815                                 EJECT
005816 5410-CREATE-TEXT-RECORDS.
005817
005818*    EXEC CICS HANDLE CONDITION
005819*         NOTOPEN   (8015-ARCT-NOT-OPEN)
005820*         DUPREC    (5465-DUPLICATE-ARCH-TEXT)
005821*    END-EXEC.
005822*
005823*    IF  PI-689-CREATE-NO-SCREENS
005824*        IF  PI-689-GET-ARCT-MAIN
005825*            EXEC CICS GETMAIN
005826*                 SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
005827*                 LENGTH   (W-ARCT-LENGTH)
005828*            END-EXEC
005829*            SET LCP-WS-ADDR-PNTR TO ADDRESS OF
005830*                                    LETTER-ARCHIVE-TEXT
005831*            MOVE LCP-WS-ADDR-COMP
005832*                                TO PI-689-ARCT-POINTER
005833*        ELSE
005834*            MOVE PI-689-ARCT-POINTER
005835*                                TO LCP-WS-ADDR-COMP
005836*            SET ADDRESS OF LETTER-ARCHIVE-TEXT
005837*                                TO LCP-WS-ADDR-PNTR
005838*    ELSE
005839*        EXEC CICS GETMAIN
005840*             SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
005841*             LENGTH   (W-ARCT-LENGTH)
005842*        END-EXEC.
005843*
005844*    PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
005845*
005846*    MOVE ZEROS                  TO W-SEQ-CTR
005847*
005848*    IF  PI-CREATE-LABELS AND
005849*        NOT PI-689-LABELS-OVERRIDEN
005850*        MOVE '1'                TO LT-RECORD-TYPE
005851*        MOVE +0                 TO LT-LINE-SEQ-NO
005852*        PERFORM 5600-FORMAT-ADDRESS-LINE THRU 5600-EXIT
005853*                VARYING
005854*            W-RG-NDX FROM 1 BY 1
005855*                UNTIL
005856*            W-RG-NDX GREATER THAN PI-689-NUMBER-LABEL-LINES
005857*        PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
005858*        PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT
005859*    ELSE
005860*        SET W-RG-NDX            TO +1.
005861*
005862*    SET LT-NDX                  TO W-ZEROS.
005863*
005864*    PERFORM 5500-FORMAT-TEXT THRU 5500-EXIT
005865*            VARYING
005866*        W-RG-NDX FROM W-RG-NDX BY +1
005867*            UNTIL
005868*        W-RG-NDX GREATER THAN PI-TOTAL-LINES.
005869
005870 5410-EXIT.
005871     EXIT.
005872                                 EJECT
005873 5415-INITIALIZE-TEXT.
005874
005875     MOVE SPACES                 TO LETTER-ARCHIVE-TEXT.
005876     MOVE 'LT'                   TO LT-RECORD-ID.
005877
005878     MOVE LA-ARCHIVE-NO          TO LT-ARCHIVE-NO.
005879     MOVE PI-COMPANY-CD          TO LT-COMPANY-CD.
005880
005881     MOVE ZEROS                  TO LT-LINE-SEQ-NO
005882                                    LT-NUM-LINES-ON-RECORD.
005883
005884 5415-EXIT.
005885     EXIT.
005886
005887 5440-ADD-BILLING-NOTE.
005888
005889     MOVE LOW-VALUES             TO W-ELEOBC-KEY
005890     MOVE PI-COMPANY-CD          TO W-EOBC-COMPANY-CD
005891     MOVE '5'                    TO W-EOBC-REC-TYPE
005892
005893     
      * EXEC CICS STARTBR
005894*        DATASET   ('ELEOBC')
005895*        RIDFLD    (W-ELEOBC-KEY)
005896*        GTEQ
005897*        RESP      (W-RESPONSE)
005898*    END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00012043' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303132303433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 W-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005899
005900     IF NOT RESP-NORMAL
005901        GO TO 5440-EXIT
005902     END-IF
005903      .
005904 5440-READNEXT-ELEOBC.
005905
005906     
      * EXEC CICS READNEXT
005907*       INTO    (EOB-CODES)
005908*       DATASET ('ELEOBC')
005909*       RIDFLD  (W-ELEOBC-KEY)
005910*       RESP    (W-RESPONSE)
005911*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00012056' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303132303536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 W-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005912
005913     IF RESP-NORMAL
005914         IF EO-RECORD-TYPE NOT = '5'
005915             GO TO 5440-EXIT
005916         END-IF
005917     ELSE
005918         GO TO 5440-EXIT
005919     END-IF
005920
005921     IF EO-RECORD-TYPE = '5' AND
005922        EO-EOB-CODE = PI-689-FORM-NUMBER
005923           CONTINUE
005924     ELSE
005925         GO TO 5440-READNEXT-ELEOBC
005926     END-IF
005927
005928     MOVE SPACES TO W-BILLING-NOTE
005929     MOVE EO-DESCRIPTION TO W-BN-NOTE
005930     MOVE PI-689-FORM-NUMBER TO W-BN-LTRID
005931     MOVE W-SAVE-DATE TO W-BN-DATE
005932     MOVE PI-PROCESSOR-ID TO W-BN-USERID
005933     MOVE +25 TO W-LEN
005934
005935     PERFORM 5441-UPDATE-BILLING-NOTE THRU 5441-EXIT
005936     .
005937 5440-EXIT.
005938     EXIT.
005939
005940 5441-UPDATE-BILLING-NOTE.
005941
005942     MOVE PI-COMPANY-CD          TO W-SV-COMPANY-CD
005943     MOVE PI-689-CARRIER         TO W-SV-CARRIER
005944     MOVE PI-689-GROUPING        TO W-SV-GROUPING
005945     MOVE PI-689-STATE           TO W-SV-STATE
005946     MOVE PI-689-ACCOUNT         TO W-SV-ACCOUNT
005947     MOVE PI-689-EFF-DATE        TO W-SV-CERT-EFF-DT
005948     MOVE PI-689-CERT-PRIME      TO W-SV-CERT-PRIME
005949     MOVE PI-689-CERT-SFX        TO W-SV-CERT-SFX
005950     MOVE W-SAVE-KEY             TO W-ERNOTE-KEY
005951     if pi-iss-can-pend-rec = '2'
005952        move '2'              to cn-record-type
005953     else
005954        move '1'              to cn-record-type
005955     end-if
005956
005957     move cn-record-type      to w-note-record-type
005958
005959     
      * EXEC CICS READ
005960*       DATASET    (W-NOTE-FILE-ID)
005961*       RIDFLD     (W-ERNOTE-KEY)
005962*       INTO       (CERTIFICATE-NOTE)
005963*       RESP       (W-RESPONSE)
005964*       UPDATE
005965*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00012109' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303132313039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 W-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005966
005967     IF RESP-NORMAL
005968       IF CN-BILLING-START-LINE-NO NOT NUMERIC
005969          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
005970       END-IF
005971       IF CN-BILLING-END-LINE-NO NOT NUMERIC
005972          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
005973       END-IF
005974       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
005975           (NOTE-SUB > +10) OR
005976           (CN-LINE (NOTE-SUB) (1:W-LEN) =
005977                             W-BILLING-NOTE (1:W-LEN))
005978       END-PERFORM
005979       IF CN-LINE (NOTE-SUB) (1:W-LEN) =
005980                              W-BILLING-NOTE (1:W-LEN)
005981         
      * EXEC CICS UNLOCK
005982*           DATASET    (W-NOTE-FILE-ID)
005983*        END-EXEC
      *    MOVE '&*                    #   #00012131' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132313331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005984       ELSE
005985         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
005986           (NOTE-SUB > +10) OR
005987           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES)
005988         END-PERFORM
005989         IF (NOTE-SUB < +11)
005990           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
005991              NOTE-SUB <= CN-BILLING-END-LINE-NO
005992                MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB)
005993           ELSE
005994             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
005995              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
005996                MOVE W-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
005997                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
005998             ELSE
005999               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
006000                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
006001                     MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB)
006002                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
006003               ELSE
006004                 IF (CN-BILLING-END-LINE-NO = ZEROS)
006005                   MOVE W-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
006006                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
006007                                       CN-BILLING-START-LINE-NO
006008                 ELSE
006009                    PERFORM 5442-SQUEEZE-IT-IN THRU 5442-EXIT
006010                 END-IF
006011               END-IF
006012             END-IF
006013           END-IF
006014           MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
006015           MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
006016           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
006017           
      * EXEC CICS REWRITE
006018*             DATASET    (W-NOTE-FILE-ID)
006019*             FROM       (CERTIFICATE-NOTE)
006020*             RESP       (W-RESPONSE)
006021*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00012167' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303132313637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006022           PERFORM 5445-CERTIFICATE-UPDATE THRU 5445-EXIT
006023         END-IF
006024       END-IF
006025     ELSE
006026        MOVE SPACES              TO CERTIFICATE-NOTE
006027        MOVE 'CN'                TO CN-RECORD-ID
006028        MOVE W-SAVE-KEY          TO CN-CONTROL-PRIMARY
006029                                    W-ERNOTE-KEY
006030        if pi-iss-can-pend-rec = '2'
006031           move '2'              to cn-record-type
006032        else
006033           move '1'              to cn-record-type
006034        end-if
006035
006036        move cn-record-type      to w-note-record-type
006037
006038        MOVE 01                  TO CN-BILLING-START-LINE-NO
006039                                    CN-BILLING-END-LINE-NO
006040        MOVE W-BILLING-NOTE      TO CN-LINE (01)
006041        MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
006042        MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
006043        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
006044        
      * EXEC CICS WRITE
006045*          DATASET    (W-NOTE-FILE-ID)
006046*          FROM       (CERTIFICATE-NOTE)
006047*          RIDFLD     (cn-control-primary)
006048*          RESP       (W-RESPONSE)
006049*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00012194' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303132313934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 cn-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006050
006051        PERFORM 5445-CERTIFICATE-UPDATE THRU 5445-EXIT
006052     END-IF
006053
006054     .
006055 5441-EXIT.
006056     EXIT.
006057
006058
006059 5442-SQUEEZE-IT-IN.
006060
006061     IF NOTE-SUB < CN-BILLING-START-LINE-NO
006062        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
006063           NOTE-SUB = +10
006064           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
006065           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
006066             MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
006067             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
006068             MOVE +9 TO NOTE-SUB
006069           END-IF
006070        END-PERFORM
006071     ELSE
006072        IF NOTE-SUB > CN-BILLING-END-LINE-NO
006073           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1
006074             UNTIL NOTE-SUB = +1
006075             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
006076             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
006077                MOVE W-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
006078                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
006079                MOVE +2          TO NOTE-SUB
006080             END-IF
006081           END-PERFORM
006082        END-IF
006083     END-IF
006084
006085     .
006086 5442-EXIT.
006087     EXIT.
006088
006089 5445-CERTIFICATE-UPDATE.
006090
006091     MOVE W-SAVE-KEY         TO W-CERT-KEY
006092
006093     
      * EXEC CICS READ
006094*        DATASET  (W-CERT-FILE-ID)
006095*        RIDFLD   (W-CERT-KEY)
006096*        SET      (ADDRESS OF CERTIFICATE-MASTER)
006097*        RESP     (W-RESPONSE)
006098*        UPDATE
006099*    END-EXEC
      *    MOVE '&"S        EU         (  N#00012243' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303132323433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006100
006101     IF RESP-NORMAL
006102        EVALUATE CM-NOTE-SW
006103           WHEN '2'
006104           WHEN '3'
006105           WHEN '6'
006106           WHEN '7'
006107              SET NO-CERT-RW     TO TRUE
006108           WHEN ' '
006109              MOVE '2'           TO CM-NOTE-SW
006110           WHEN '1'
006111              MOVE '3'           TO CM-NOTE-SW
006112           WHEN '4'
006113              MOVE '6'           TO CM-NOTE-SW
006114           WHEN '5'
006115              MOVE '7'           TO CM-NOTE-SW
006116        END-EVALUATE
006117     END-IF
006118     IF NOT NO-CERT-RW
006119        
      * EXEC CICS REWRITE
006120*          FROM     (CERTIFICATE-MASTER)
006121*          DATASET  (W-CERT-FILE-ID)
006122*          RESP     (W-RESPONSE)
006123*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00012269' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303132323639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006124     ELSE
006125        
      * EXEC CICS UNLOCK
006126*          DATASET  (W-CERT-FILE-ID)
006127*       END-EXEC
      *    MOVE '&*                    #   #00012275' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132323735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006128     END-IF
006129
006130     .
006131 5445-EXIT.
006132     EXIT.
006133
006134                                 EJECT
006135 5460-DUPLICATE-ARCHIVE-RCD.
006136
006137     IF  NOT PI-689-CREATE-NO-SCREENS
006138         
      * EXEC CICS SYNCPOINT ROLLBACK
006139*        END-EXEC.
      *    MOVE '6"R                   !   #00012288' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303132323838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006140
006141     MOVE ER-9426                TO EMI-ERROR.
006142     MOVE -1                     TO MAINTL.
006143     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006144     GO TO 8200-SEND-DATAONLY.
006145
006146 5465-DUPLICATE-ARCH-TEXT.
006147
006148     IF  NOT PI-689-CREATE-NO-SCREENS
006149         
      * EXEC CICS SYNCPOINT ROLLBACK
006150*        END-EXEC.
      *    MOVE '6"R                   !   #00012299' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303132323939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006151
006152     MOVE ER-7369                TO EMI-ERROR.
006153     MOVE -1                     TO MAINTL.
006154     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006155     GO TO 8200-SEND-DATAONLY.
006156
006157 5470-CNTL-NOT-FOUND.
006158
006159     MOVE ER-9299                TO EMI-ERROR.
006160     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006161     MOVE -1                     TO MAINTL.
006162     MOVE AL-UANON               TO MAINTA.
006163     GO TO 8200-SEND-DATAONLY.
006164                                 EJECT
006165 5500-FORMAT-TEXT.
006166
006167     SET LT-NDX UP BY +1.
006168
006169     IF  W-RC-TEXT (W-RG-NDX) EQUAL W-TOP-FORM
006170         SET W-RG-NDX UP BY 1
006171         MOVE 99                 TO LT-SKIP-CONTROL (LT-NDX)
006172         MOVE W-RC-TEXT (W-RG-NDX)
006173                                 TO LT-TEXT-LINE (LT-NDX)
006174     ELSE
006175         IF  W-RC-TEXT (W-RG-NDX) EQUAL SPACES
006176             MOVE ZEROS          TO W-PRINT-CONTROL
006177             PERFORM 5505-FIND-USED-LINE THRU 5505-EXIT
006178                     VARYING
006179                 W-RG-NDX FROM W-RG-NDX BY 1
006180                     UNTIL
006181                 W-RG-NDX GREATER THAN PI-TOTAL-LINES
006182                     OR
006183                 W-RC-TEXT (W-RG-NDX) GREATER THAN SPACES
006184             IF  W-RG-NDX GREATER THAN PI-TOTAL-LINES
006185                 GO TO 5500-WRITE-TEST
006186             ELSE
006187                 MOVE W-PRINT-CONTROL
006188                                 TO LT-SKIP-CONTROL (LT-NDX)
006189                 MOVE W-RC-TEXT (W-RG-NDX)
006190                                 TO LT-TEXT-LINE (LT-NDX)
006191         ELSE
006192             MOVE W-RC-TEXT (W-RG-NDX)
006193                                 TO LT-TEXT-LINE (LT-NDX)
006194             MOVE ZEROS          TO W-PRINT-CONTROL.
006195
006196     ADD +1                      TO LT-NUM-LINES-ON-RECORD.
006197
006198 5500-WRITE-TEST.
006199
006200     IF  W-RG-NDX NOT LESS THAN PI-TOTAL-LINES
006201         IF  LT-TEXT-RECORD NOT EQUAL SPACES
006202             ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
006203             MOVE '2'            TO LT-RECORD-TYPE
006204             ADD +1              TO W-SEQ-CTR
006205             MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
006206             PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
006207         ELSE
006208             NEXT SENTENCE
006209     ELSE
006210         IF  LT-NDX EQUAL +20
006211             ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
006212             MOVE '2'            TO LT-RECORD-TYPE
006213             ADD +1              TO W-SEQ-CTR
006214             MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
006215             PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
006216             SET LT-NDX          TO W-ZEROS
006217             PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
006218
006219 5500-EXIT.
006220      EXIT.
006221                                 EJECT
006222 5505-FIND-USED-LINE.
006223
006224     ADD +1                      TO W-PRINT-CONTROL.
006225
006226 5505-EXIT.
006227      EXIT.
006228                                 EJECT
006229 5600-FORMAT-ADDRESS-LINE.
006230
006231     SET LT-NDX                  TO W-RG-NDX.
006232     MOVE W-RC-TEXT (W-RG-NDX)   TO LT-TEXT-LINE (LT-NDX).
006233
006234 5600-EXIT.
006235     EXIT.
006236                                 EJECT
006237 5650-WRITE-ARCHIVE.
006238
006239     
      * EXEC CICS WRITE
006240*         DATASET   (W-ARCH-FILE-ID)
006241*         FROM      (LETTER-ARCHIVE)
006242*         RIDFLD    (LA-CONTROL-PRIMARY)
006243*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012389' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132333839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006244
006245 5650-EXIT.
006246     EXIT.
006247                                 EJECT
006248 5655-WRITE-ARCHIVE-TEXT.
006249
006250     
      * EXEC CICS WRITE
006251*         DATASET   (W-ARCT-FILE-ID)
006252*         FROM      (LETTER-ARCHIVE-TEXT)
006253*         RIDFLD    (LT-CONTROL-PRIMARY)
006254*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012400' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132343030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 LT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006255
006256 5655-EXIT.
006257     EXIT.
006258                                 EJECT
006259 6000-RESOLVE-VARIABLES.
006260***************************************************************
006261*    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
006262*    WITH DATA PERTAINING TO THE FORM WITH VARIABLES.         *
006263*    THIS ROUTINE IS PERFORM THRU 6000-EXIT IN ORDER TO       *
006264*    RESOLVE ALL OF THE SYMBOLS.                              *
006265*                                                             *
006266***************************************************************
006267
006268     PERFORM 6200-GET-COMPANY-DATA THRU 6200-EXIT.
006269     PERFORM 6250-GET-CNTL2-DATA THRU 6250-EXIT.
006270     PERFORM 6400-GET-PENDING-DATA THRU 6400-EXIT.
006271     PERFORM 6450-GET-CERT-DATA THRU 6450-EXIT.
006272     PERFORM 6500-GET-ACCOUNT-DATA THRU 6500-EXIT
006273*    PERFORM 6350-GET-CSR-DATA   THRU 6350-EXIT
006274     PERFORM 6600-GET-COMPENSATION-DATA THRU 6600-EXIT.
006275     PERFORM 6700-GET-MAIL-DATA THRU 6700-EXIT.
006276     PERFORM 6300-GET-CARRIER-DATA THRU 6300-EXIT.
006277     PERFORM 6750-GET-LIFE-BENEFIT-DATA THRU 6750-EXIT.
006278     PERFORM 6800-GET-A-H-BENEFIT-DATA THRU 6800-EXIT.
006279     PERFORM 6850-GET-CHECK-DATA THRU 6850-EXIT.
006280     PERFORM 6900-GET-PYAJ-DATA THRU 6900-EXIT.
006281     PERFORM 6950-MOVE-SYSTEM-DATA THRU 6950-EXIT.
006282
006283     MOVE SPACES                 TO W-REVERSE-DATE-SW.
006284
006285 6000-EXIT.
006286     EXIT.
006287                                 EJECT
006288 6200-GET-COMPANY-DATA.
006289
006290     IF  ADDRSI EQUAL '3'
006291             AND
006292         PI-CREATE-LABELS
006293             AND
006294         NOT PI-689-LABELS-OVERRIDEN
006295         NEXT SENTENCE
006296     ELSE
006297         IF  W-FILE-NOT-USED (1)
006298             GO TO 6200-EXIT.
006299
006300     MOVE SPACES                 TO W-CNTL-KEY.
006301     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
006302     MOVE +0                     TO W-CNTL-SEQ-NO.
006303     MOVE '1'                    TO W-CNTL-RECORD-TYPE.
006304
006305     
      * EXEC CICS HANDLE CONDITION
006306*         NOTOPEN   (8040-CNTL-NOT-OPEN)
006307*         NOTFND    (6200-CNTL1-NOT-FOUND)
006308*    END-EXEC.
      *    MOVE '"$JI                  ! + #00012455' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303132343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006309
006310     
      * EXEC CICS READ
006311*         DATASET   (W-CNTL-FILE-ID)
006312*         SET       (ADDRESS OF CONTROL-FILE)
006313*         RIDFLD    (W-CNTL-KEY)
006314*    END-EXEC.
      *    MOVE '&"S        E          (   #00012460' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132343630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006315
006316     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
006317     MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (1)
006318                                    W-VG-TEXT (25).
006319     MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (2).
006320     MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (3).
006321     MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (4).
006322     MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (5).
006323
006324     IF  CF-CL-CAN-POST-CODE
006325         MOVE CF-CL-CANADIAN-POSTAL-CODE
006326                                 TO W-CANADIAN-POSTAL-CODES
006327         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
006328         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
006329         MOVE SPACES             TO W-LAB-CAN-DASH (6)
006330                                    W-LAB-CAN-FILLER (6)
006331         GO TO 6200-CONTINUE.
006332
006333     IF CF-CL-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
006334         MOVE CF-CL-ZIP-CODE     TO W-WORK-ZIP
006335         MOVE SPACES             TO W-LABEL-ZIP (6)
006336         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
006337
006338         IF  W-WORK-ZIP4 GREATER THAN '0000'
006339             MOVE '-'            TO W-LABEL-DASH (6)
006340             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
006341             GO TO 6200-CONTINUE
006342         ELSE
006343             MOVE SPACES         TO W-LABEL-DASH (6)
006344                                    W-LABEL-2ND-ZIP (6)
006345             GO TO 6200-CONTINUE.
006346
006347     IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC
006348         GO TO 6200-CONTINUE.
006349
006350     MOVE CF-CL-ZIP-CODE-NUM     TO W-WORK-ZIP-NUMERIC.
006351
006352     IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
006353         COMPUTE W-WORK-ZIP-NUMERIC
006354             = W-WORK-ZIP-NUMERIC * 10000.
006355
006356     MOVE SPACES                 TO W-LABEL-ZIP (6).
006357     MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
006358
006359     IF  W-WORK-ZIP4 GREATER THAN '0000'
006360         MOVE '-'                TO W-LABEL-DASH (6)
006361         MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
006362         GO TO 6200-CONTINUE
006363     ELSE
006364         MOVE SPACES             TO W-LABEL-DASH (6)
006365                                    W-LABEL-2ND-ZIP (6)
006366         GO TO 6200-CONTINUE.
006367
006368 6200-CONTINUE.
006369
006370     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
006371
006372     IF  ADDRSI EQUAL '3'
006373             AND
006374         PI-CREATE-LABELS
006375             AND
006376         NOT PI-689-LABELS-OVERRIDEN
006377         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1) W-VG-TEXT (1)
006378         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2) W-VG-TEXT (2)
006379         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3) W-VG-TEXT (3)
006380         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4) W-VG-TEXT (04)
006381         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5) W-VG-TEXT (5)
006382         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6) W-VG-TEXT (6)
006383         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
006384         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
006385         SET W-RG-NDX UP BY +1
006386     ELSE
006387         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (1)
006388         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (2)
006389         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (3)
006390         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (4)
006391         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (5)
006392         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (6).
006393
006394     GO TO 6200-EXIT.
006395
006396 6200-CNTL1-NOT-FOUND.
006397
006398     MOVE ER-9299                TO EMI-ERROR.
006399     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006400     MOVE -1                     TO ADDRSL.
006401     MOVE AL-UANON               TO ADDRSA.
006402     GO TO 8200-SEND-DATAONLY.
006403
006404 6200-EXIT.
006405     EXIT.
006406                                 EJECT
006407 6250-GET-CNTL2-DATA.
006408
006409     IF  PI-PROCESSOR-ID EQUAL W-LGXX-ID
006410         GO TO 6250-EXIT.
006411
006412     IF  W-FILE-NOT-USED (11)
006413         GO TO 6250-EXIT.
006414
006415     MOVE SPACES                 TO W-CNTL-KEY.
006416     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
006417     MOVE +0                     TO W-CNTL-SEQ-NO.
006418     MOVE '2'                    TO W-CNTL-RECORD-TYPE.
006419     MOVE PI-PROCESSOR-ID        TO W-CNTL-GENL.
006420
006421     
      * EXEC CICS HANDLE CONDITION
006422*         NOTOPEN    (8040-CNTL-NOT-OPEN)
006423*         NOTFND     (6250-CNTL2-NOT-FOUND)
006424*    END-EXEC.
      *    MOVE '"$JI                  ! , #00012571' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303132353731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006425
006426     
      * EXEC CICS READ
006427*         DATASET    (W-CNTL-FILE-ID)
006428*         SET        (ADDRESS OF CONTROL-FILE)
006429*         RIDFLD     (W-CNTL-KEY)
006430*    END-EXEC.
      *    MOVE '&"S        E          (   #00012576' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132353736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006431
006432     MOVE CF-PROCESSOR           TO W-VG-TEXT (153).
006433     MOVE CF-PROCESSOR-NAME      TO W-VG-TEXT (151).
006434     MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (152).
006435     GO TO 6250-EXIT.
006436
006437 6250-CNTL2-NOT-FOUND.
006438
006439     MOVE ER-9299                TO EMI-ERROR.
006440     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006441     MOVE -1                     TO MAINTL.
006442     MOVE AL-UANON               TO MAINTA.
006443     GO TO 8200-SEND-DATAONLY.
006444
006445 6250-EXIT.
006446     EXIT.
006447                                 EJECT
006448 6300-GET-CARRIER-DATA.
006449
006450     IF  ADDRSI EQUAL '2'
006451             AND
006452         PI-CREATE-LABELS
006453             AND
006454         NOT PI-689-LABELS-OVERRIDEN
006455         NEXT SENTENCE
006456     ELSE
006457         IF  W-FILE-NOT-USED (4)
006458             GO TO 6300-EXIT.
006459
006460     IF  PI-689-CARRIER GREATER THAN LOW-VALUES
006461         MOVE SPACES             TO W-CNTL-KEY
006462         MOVE PI-COMPANY-ID      TO W-CNTL-COMPANY-ID
006463         MOVE +0                 TO W-CNTL-SEQ-NO
006464         MOVE '6'                TO W-CNTL-RECORD-TYPE
006465         MOVE PI-689-CARRIER     TO W-CNTL-GEN4
006466     ELSE
006467         MOVE ER-9327            TO EMI-ERROR
006468         MOVE -1                 TO CARRIERL
006469         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006470         GO TO 6300-EXIT.
006471
006472     
      * EXEC CICS HANDLE CONDITION
006473*         NOTOPEN    (8040-CNTL-NOT-OPEN)
006474*         NOTFND     (6300-CNTL6-NOT-FOUND)
006475*    END-EXEC.
      *    MOVE '"$JI                  ! - #00012622' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303132363232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006476
006477     
      * EXEC CICS READ
006478*         DATASET    (W-CNTL-FILE-ID)
006479*         SET        (ADDRESS OF CONTROL-FILE)
006480*         RIDFLD     (W-CNTL-KEY)
006481*    END-EXEC.
      *    MOVE '&"S        E          (   #00012627' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132363237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006482
006483     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
006484     MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (1).
006485     MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (2).
006486     MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
006487     MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
006488     MOVE CF-CITY-STATE          TO W-LABEL-LINES (5).
006489
006490     IF  CF-CANADIAN-POST-CODE
006491         MOVE CF-CANADIAN-POSTAL-CODE
006492                                 TO W-CANADIAN-POSTAL-CODES
006493         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
006494         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
006495         MOVE SPACES             TO W-LAB-CAN-DASH (6)
006496                                    W-LAB-CAN-FILLER (6)
006497         GO TO 6300-CONTINUE.
006498
006499     IF CF-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
006500         MOVE CF-ZIP-CODE        TO W-WORK-ZIP
006501         MOVE SPACES             TO W-LABEL-ZIP (6)
006502         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
006503         IF  W-WORK-ZIP4 GREATER THAN '0000'
006504             MOVE '-'            TO W-LABEL-DASH (6)
006505             MOVE W-WORK-ZIP4
006506                                 TO W-LABEL-2ND-ZIP (6)
006507             GO TO 6300-CONTINUE
006508         ELSE
006509             MOVE SPACES         TO W-LABEL-DASH (6)
006510                                    W-LABEL-2ND-ZIP (6)
006511             GO TO 6300-CONTINUE.
006512
006513     IF  CF-ZIP-CODE-NUM NOT NUMERIC
006514         GO TO 6300-CONTINUE.
006515
006516     MOVE CF-ZIP-CODE-NUM TO W-WORK-ZIP-NUMERIC.
006517
006518     IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
006519         COMPUTE W-WORK-ZIP-NUMERIC
006520             = W-WORK-ZIP-NUMERIC * 10000.
006521
006522     MOVE SPACES                 TO W-LABEL-ZIP (6).
006523     MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
006524
006525     IF  W-WORK-ZIP4 GREATER THAN '0000'
006526         MOVE '-'                TO W-LABEL-DASH (6)
006527         MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
006528         GO TO 6300-CONTINUE
006529     ELSE
006530         MOVE SPACES             TO W-LABEL-DASH (6)
006531                                    W-LABEL-2ND-ZIP (6)
006532         GO TO 6300-CONTINUE.
006533
006534 6300-CONTINUE.
006535
006536     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
006537
006538     IF  ADDRSI EQUAL '2'
006539             AND
006540         PI-CREATE-LABELS
006541             AND
006542         NOT PI-689-LABELS-OVERRIDEN
006543         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1) W-VG-TEXT (20)
006544         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2) W-VG-TEXT (21)
006545         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3) W-VG-TEXT (22)
006546         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4) W-VG-TEXT (23)
006547         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5) W-VG-TEXT (24)
006548         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6) W-VG-TEXT (25)
006549         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
006550         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
006551         SET W-RG-NDX UP BY +1
006552     ELSE
006553         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (20)
006554         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (21)
006555         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (22)
006556         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (23)
006557         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (24)
006558         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (25).
006559
006560     IF  W-FULL-DATA
006561         MOVE ZEROS              TO W-PHONE-IN
006562         MOVE CF-PHONE-NO        TO W-PHONE-IN
006563         MOVE W-PHI-AREA         TO W-PO-AREA
006564         MOVE W-PHI-PFX          TO W-PO-PFX
006565         MOVE W-PHI-SFX          TO W-PO-SFX
006566         MOVE W-PHONE-OUT        TO W-VG-TEXT (26).
006567
006568     GO TO 6300-EXIT.
006569
006570 6300-CNTL6-NOT-FOUND.
006571
006572     MOVE ER-9298                TO EMI-ERROR.
006573     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006574     MOVE -1                     TO ADDRSL.
006575     MOVE AL-UANON               TO ADDRSA.
006576     GO TO 8200-SEND-DATAONLY.
006577
006578 6300-EXIT.
006579     EXIT.
006580                                 EJECT
006581 6350-GET-CSR-DATA.
006582
006583     IF W-VG-TEXT (148) (1:4) = '****' OR SPACES
006584        GO TO 6350-EXIT
006585     END-IF
006586
006587*    IF W-FILE-NOT-USED (11)
006588*       GO TO 6350-EXIT
006589*    END-IF
006590
006591     MOVE SPACES                 TO W-CNTL-KEY
006592     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID
006593     MOVE +0                     TO W-CNTL-SEQ-NO
006594     MOVE '2'                    TO W-CNTL-RECORD-TYPE
006595     MOVE CO-CSR-CODE            TO W-CNTL-GENL
006596
006597     
      * EXEC CICS HANDLE CONDITION
006598*         NOTOPEN    (8040-CNTL-NOT-OPEN)
006599*         NOTFND     (6350-CNTL2-NOT-FOUND)
006600*    END-EXEC
      *    MOVE '"$JI                  ! . #00012747' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303132373437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006601
006602     
      * EXEC CICS READ
006603*         DATASET    (W-CNTL-FILE-ID)
006604*         SET        (ADDRESS OF CONTROL-FILE)
006605*         RIDFLD     (W-CNTL-KEY)
006606*    END-EXEC
      *    MOVE '&"S        E          (   #00012752' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132373532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006607
006608     MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (154)
006609     GO TO 6350-EXIT
006610
006611     .
006612 6350-CNTL2-NOT-FOUND.
006613
006614     MOVE ER-9299                TO EMI-ERROR.
006615     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006616     MOVE -1                     TO MAINTL.
006617     MOVE AL-UANON               TO MAINTA.
006618     GO TO 8200-SEND-DATAONLY.
006619
006620 6350-EXIT.
006621     EXIT.
006622
006623 6400-GET-PENDING-DATA.
006624
006625     IF (ADDRSI EQUAL '7')
006626        AND (PI-CREATE-LABELS)
006627        AND (NOT PI-689-LABELS-OVERRIDEN)
006628        CONTINUE
006629     ELSE
006630        IF PI-689-DATA-SOURCE = '4'
006631           CONTINUE
006632        ELSE
006633           IF W-FILE-NOT-USED (9)
006634              GO TO 6400-EXIT
006635           END-IF
006636        END-IF
006637     END-IF
006638
006639     IF  W-PNDB-ENTRY EQUAL LOW-VALUES
006640         MOVE ER-9327            TO EMI-ERROR
006641         MOVE -1                 TO MAINTL
006642         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006643         GO TO 6400-EXIT.
006644
006645 6400-READ-PNDB-ONLY.
006646
006647     
      * EXEC CICS HANDLE CONDITION
006648*         NOTOPEN (8060-PNDB-NOT-OPEN)
006649*         NOTFND  (8070-PNDB-NOT-FOUND)
006650*    END-EXEC.
      *    MOVE '"$JI                  ! / #00012797' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303132373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006651
006652     
      * EXEC CICS READ
006653*         DATASET (W-PNDB-FILE-ID)
006654*         SET     (ADDRESS OF PENDING-BUSINESS)
006655*         RIDFLD  (W-PNDB-KEY)
006656*    END-EXEC.
      *    MOVE '&"S        E          (   #00012802' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132383032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006657
006658     MOVE 'Y'                    TO W-PNDB-FOUND-SW.
006659     move pb-record-type         to pi-iss-can-pend-rec
006660
006661     IF  W-CERT-CERT-PRIME EQUAL LOW-VALUES
006662         MOVE PB-SV-CARRIER      TO PB-CARRIER
006663                                    PI-689-CARRIER
006664         MOVE PB-SV-GROUPING     TO PB-GROUPING
006665                                    PI-689-GROUPING
006666         MOVE PB-SV-STATE        TO PB-STATE
006667                                    PI-689-STATE
006668         MOVE PB-CONTROL-BY-ACCOUNT
006669                                 TO W-CERT-KEY
006670                                    W-MAIL-KEY
006671                                    W-ACCT-KEY
006672         MOVE PB-ACCOUNT         TO PI-689-ACCOUNT
006673         MOVE PB-CERT-EFF-DT     TO PI-689-EFF-DATE
006674         MOVE PB-CERT-NO         TO PI-689-CERT-NO.
006675
006676 6400-ONLY-STOP.
006677
006678     MOVE PB-ENTRY-BATCH         TO W-VG-TEXT (130).
006679
006680     IF  NOT PB-ISSUE
006681         GO TO 6400-CANCEL.
006682
006683     MOVE PB-I-BIRTHDAY          TO DC-BIN-DATE-1.
006684     MOVE SPACES                 TO DC-OPTION-CODE.
006685     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006686
006687     IF  NO-CONVERSION-ERROR
006688         MOVE DC-GREG-DATE-1-EDIT
006689                                 TO W-VG-TEXT (110).
006690
006691     MOVE PB-I-AGE               TO W-EDIT-3-0.
006692     MOVE W-EDIT-3-0             TO W-VG-TEXT (124).
006693     MOVE PB-I-LF-PREMIUM-AMT    TO W-EDIT-7-2.
006694     MOVE W-EDIT-7-2             TO W-VG-TEXT (111).
006695     MOVE PB-I-AH-PREMIUM-AMT    TO W-EDIT-7-2.
006696     MOVE W-EDIT-7-2             TO W-VG-TEXT (112).
006697     MOVE PB-I-LF-PREM-CALC      TO W-EDIT-7-2.
006698     MOVE W-EDIT-7-2             TO W-VG-TEXT (113).
006699     MOVE PB-I-AH-PREM-CALC      TO W-EDIT-7-2.
006700     MOVE W-EDIT-7-2             TO W-VG-TEXT (114).
006701     MOVE PB-I-LF-BENEFIT-AMT    TO W-EDIT-9-2.
006702     MOVE W-EDIT-9-2             TO W-VG-TEXT (125).
006703     MOVE PB-I-AH-BENEFIT-AMT    TO W-EDIT-7-2.
006704     MOVE W-EDIT-7-2             TO W-VG-TEXT (126).
006705     MOVE PB-I-LF-RATE           TO W-EDIT-2-5-S.
006706     MOVE W-EDIT-2-5-S           TO W-VG-TEXT (127).
006707     MOVE PB-I-AH-RATE           TO W-EDIT-2-5-S.
006708     MOVE W-EDIT-2-5-S           TO W-VG-TEXT (128).
006709     MOVE PB-I-LOAN-TERM         TO W-EDIT-3-0.
006710     MOVE W-EDIT-3-0             TO W-VG-TEXT (129).
006711
006712     COMPUTE W-DIFFERENCE
006713         = PB-I-LF-PREM-CALC - PB-I-LF-PREMIUM-AMT.
006714
006715     MOVE W-DIFFERENCE           TO W-EDIT-7-2.
006716     MOVE W-EDIT-7-2             TO W-VG-TEXT (115).
006717
006718     COMPUTE W-DIFFERENCE
006719         = PB-I-AH-PREM-CALC - PB-I-AH-PREMIUM-AMT.
006720
006721     MOVE W-DIFFERENCE           TO W-EDIT-7-2.
006722     MOVE W-EDIT-7-2             TO W-VG-TEXT (116).
006723
006724 6400-CANCEL.
006725
006726     IF  NOT PB-CANCELLATION
006727         GO TO 6400-EXIT.
006728
006729     MOVE PB-CI-AH-PRIOR-CANCEL-DT
006730                                 TO DC-BIN-DATE-1.
006731     MOVE SPACES                 TO DC-OPTION-CODE.
006732     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006733
006734     IF  NO-CONVERSION-ERROR
006735         MOVE DC-GREG-DATE-1-EDIT
006736                                 TO W-VG-TEXT (117).
006737
006738     MOVE PB-C-LF-CANCEL-AMT     TO W-EDIT-7-2.
006739     MOVE W-EDIT-7-2             TO W-VG-TEXT (118).
006740     MOVE PB-C-AH-CANCEL-AMT     TO W-EDIT-7-2.
006741     MOVE W-EDIT-7-2             TO W-VG-TEXT (119).
006742     MOVE PB-C-LF-REF-CALC       TO W-EDIT-7-2.
006743     MOVE W-EDIT-7-2             TO W-VG-TEXT (120).
006744     MOVE PB-C-AH-REF-CALC       TO W-EDIT-7-2.
006745     MOVE W-EDIT-7-2             TO W-VG-TEXT (121).
006746
006747     COMPUTE W-EDIT-7-2 = PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT
006748     MOVE W-EDIT-7-2             TO W-VG-TEXT (131)
006749
006750     IF PB-C-INT-ON-REFS NOT NUMERIC
006751        MOVE ZEROS               TO PB-C-INT-ON-REFS
006752     END-IF
006753
006754     MOVE PB-C-INT-ON-REFS       TO W-EDIT-7-2
006755     MOVE W-EDIT-7-2             TO W-VG-TEXT (132)
006756
006757     MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
006758     IF PB-C-AH-CANCEL-DT > DC-BIN-DATE-1
006759        MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
006760     END-IF
006761     MOVE SPACES                 TO DC-OPTION-CODE
006762     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
006763
006764     IF NO-CONVERSION-ERROR
006765        MOVE DC-GREG-DATE-1-EDIT TO W-VG-TEXT (133)
006766     END-IF
006767
006768     COMPUTE W-DIFFERENCE = PB-C-LF-REF-CALC
006769                           - PB-C-LF-CANCEL-AMT.
006770
006771     MOVE W-DIFFERENCE           TO W-EDIT-7-2.
006772     MOVE W-EDIT-7-2             TO W-VG-TEXT (122).
006773
006774     COMPUTE W-DIFFERENCE = PB-C-AH-REF-CALC
006775                           - PB-C-AH-CANCEL-AMT.
006776
006777     MOVE W-DIFFERENCE           TO W-EDIT-7-2.
006778     MOVE W-EDIT-7-2             TO W-VG-TEXT (123).
006779
006780 6400-EXIT.
006781     EXIT.
006782                                 EJECT
006783 6450-GET-CERT-DATA.
006784     IF  W-FILE-NOT-USED (8)
006785         GO TO 6450-EXIT.
006786
006787     IF  W-CERT-CERT-PRIME EQUAL LOW-VALUES
006788         MOVE ER-9327            TO EMI-ERROR
006789         MOVE -1                 TO MAINTL
006790         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006791         GO TO 6450-EXIT.
006792
006793 6450-READ-CERT-ONLY.
006794
006795     
      * EXEC CICS HANDLE CONDITION
006796*         NOTOPEN (8030-CERT-NOT-OPEN)
006797*         NOTFND  (8035-CERT-NOT-FOUND)
006798*    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00012945' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303132393435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006799
006800     
      * EXEC CICS READ
006801*         DATASET (W-CERT-FILE-ID)
006802*         SET     (ADDRESS OF CERTIFICATE-MASTER)
006803*         RIDFLD  (W-CERT-KEY)
006804*    END-EXEC.
      *    MOVE '&"S        E          (   #00012950' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132393530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006805
006806     MOVE 'Y'                    TO W-CERT-FOUND-SW.
006807
006808 6450-ONLY-STOP.
006809
006810     MOVE CM-INSURED-LAST-NAME   TO W-VG-TEXT (108).
006811
006812     MOVE CM-BENEFICIARY         TO W-VG-TEXT (109).
006813
006814     MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
006815     MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
006816     MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
006817     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006818     MOVE WS-NAME-WORK           TO W-VG-TEXT (95).
006819
006820     MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
006821     MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
006822     MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
006823     PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
006824     MOVE WS-NAME-WORK           TO W-VG-TEXT (94).
006825
006826     MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
006827     MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
006828     MOVE '.'                    TO W-NAME-FIRST-REMAIN.
006829     MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
006830     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006831     MOVE WS-NAME-WORK           TO W-VG-TEXT (90).
006832
006833     MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
006834     MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
006835     MOVE SPACES                 TO W-NAME-MIDDLE.
006836     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006837     MOVE WS-NAME-WORK           TO W-VG-TEXT (104).
006838
006839     MOVE CM-INSURED-FIRST-NAME  TO W-VG-TEXT (91).
006840     MOVE CM-INSURED-INITIAL2    TO W-VG-TEXT (92).
006841
006842     MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
006843     MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
006844     MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
006845     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006846     MOVE WS-NAME-WORK           TO W-VG-TEXT (103)
006847                                    W-LABEL-JOINT-NAME.
006848
006849     MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
006850     MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
006851     MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
006852     PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
006853     MOVE WS-NAME-WORK           TO W-VG-TEXT (102).
006854
006855     MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
006856     MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
006857     MOVE '.'                    TO W-NAME-FIRST-REMAIN.
006858     MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
006859     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006860     MOVE WS-NAME-WORK           TO W-VG-TEXT (99).
006861
006862     MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
006863     MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
006864     MOVE SPACES                 TO W-NAME-MIDDLE.
006865     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
006866     MOVE WS-NAME-WORK           TO W-VG-TEXT (105).
006867
006868     MOVE CM-JT-FIRST-NAME       TO W-VG-TEXT (100).
006869     MOVE CM-JT-INITIAL          TO W-VG-TEXT (101).
006870
006871     COMPUTE W-WORK-AMOUNT
006872         = CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT.
006873     MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
006874     MOVE W-EDIT-9-2             TO W-VG-TEXT (97).
006875     MOVE CM-AH-PREMIUM-AMT      TO W-EDIT-7-2.
006876     MOVE W-EDIT-7-2             TO W-VG-TEXT (98).
006877
006878     IF  CM-SEX-FEMAL
006879         MOVE 'MS.'              TO W-VG-TEXT (96)
006880
006881     ELSE
006882         MOVE 'MR.'              TO W-VG-TEXT (96).
006883
006884     MOVE CM-CARRIER OF CERTIFICATE-MASTER
006885                                 TO W-VG-TEXT (70).
006886     MOVE CM-GROUPING            TO W-VG-TEXT (71).
006887     MOVE CM-ACCOUNT             TO W-VG-TEXT (72).
006888     MOVE CM-CERT-NO             TO W-VG-TEXT (73).
006889     MOVE CM-INSURED-ISSUE-AGE   TO W-DISPLAY-3.
006890     MOVE W-DISPLAY-3            TO W-VG-TEXT (83).
006891     MOVE CM-LOAN-NUMBER         TO W-VG-TEXT (86).
006892     MOVE CM-LOAN-BALANCE        TO W-EDIT-7-2.
006893     MOVE W-EDIT-7-2             TO W-VG-TEXT (87).
006894     MOVE CM-MEMBER-NO           TO W-VG-TEXT (88).
006895     MOVE CM-SOC-SEC-NO          TO W-VG-TEXT (89).
006896     MOVE CM-POLICY-FORM-NO      TO W-VG-TEXT (83).
006897     MOVE CM-LF-ORIG-TERM        TO W-EDIT-3-0.
006898     MOVE W-EDIT-3-0             TO W-VG-TEXT (77).
006899     MOVE CM-LF-BENEFIT-AMT      TO W-EDIT-9-2.
006900     MOVE W-EDIT-9-2             TO W-VG-TEXT (79).
006901     MOVE CM-AH-ORIG-TERM        TO W-EDIT-3-0.
006902     MOVE W-EDIT-3-0             TO W-VG-TEXT (78).
006903     MOVE CM-AH-BENEFIT-AMT      TO W-EDIT-7-2.
006904     MOVE W-EDIT-7-2             TO W-VG-TEXT (80).
006905     MOVE CM-AH-ITD-CANCEL-AMT   TO W-EDIT-7-2.
006906     MOVE W-EDIT-7-2             TO W-VG-TEXT (107).
006907     MOVE CM-LF-ITD-CANCEL-AMT   TO W-EDIT-7-2.
006908     MOVE W-EDIT-7-2             TO W-VG-TEXT (106).
006909
006910     IF PI-689-DATA-SOURCE NOT = '4'
006911
006912     COMPUTE W-EDIT-7-2 = CM-LF-ITD-CANCEL-AMT
006913        + CM-AH-ITD-CANCEL-AMT
006914     MOVE W-EDIT-7-2             TO W-VG-TEXT (131)
006915
006916     IF CM-INT-ON-REFS NOT NUMERIC
006917        MOVE ZEROS               TO CM-INT-ON-REFS
006918     END-IF
006919
006920     MOVE CM-INT-ON-REFS         TO W-EDIT-7-2
006921     MOVE W-EDIT-7-2             TO W-VG-TEXT (132)
006922
006923     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
006924     IF CM-AH-CANCEL-DT > DC-BIN-DATE-1
006925        MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
006926     END-IF
006927     MOVE SPACES                 TO DC-OPTION-CODE
006928     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
006929
006930     IF NO-CONVERSION-ERROR
006931        MOVE DC-GREG-DATE-1-EDIT TO W-VG-TEXT (133)
006932     END-IF
006933
006934     END-IF
006935
006936     COMPUTE W-WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.
006937     MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
006938     MOVE W-EDIT-9-2             TO W-VG-TEXT (93).
006939
006940     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1.
006941     MOVE SPACES                 TO DC-OPTION-CODE.
006942     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006943
006944     IF  NO-CONVERSION-ERROR
006945         MOVE DC-GREG-DATE-1-EDIT
006946                                 TO W-VG-TEXT (81).
006947
006948     MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1.
006949     MOVE SPACES                 TO DC-OPTION-CODE.
006950     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006951
006952     IF  NO-CONVERSION-ERROR
006953         MOVE DC-GREG-DATE-1-EDIT
006954                                 TO W-VG-TEXT (82).
006955
006956     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
006957     MOVE SPACES                 TO DC-OPTION-CODE.
006958     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006959
006960     IF  NO-CONVERSION-ERROR
006961         MOVE DC-GREG-DATE-1-EDIT
006962                                 TO W-VG-TEXT (74).
006963
006964     MOVE CM-LF-ORIG-TERM        TO DC-ELAPSED-MONTHS.
006965
006966     IF  CM-PMT-EXTENSION-DAYS NUMERIC
006967         MOVE CM-PMT-EXTENSION-DAYS
006968                                 TO DC-ELAPSED-DAYS
006969     ELSE
006970         MOVE ZEROS              TO DC-ELAPSED-DAYS.
006971
006972     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
006973     MOVE '6'                    TO DC-OPTION-CODE.
006974     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006975
006976     IF  NO-CONVERSION-ERROR
006977         MOVE DC-GREG-DATE-1-EDIT
006978                                 TO W-VG-TEXT (75).
006979
006980     MOVE CM-AH-ORIG-TERM        TO DC-ELAPSED-MONTHS.
006981     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
006982     MOVE '6'                    TO DC-OPTION-CODE.
006983     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
006984
006985     IF  NO-CONVERSION-ERROR
006986         MOVE DC-GREG-DATE-1-EDIT
006987                                 TO W-VG-TEXT (76).
006988
006989 6450-EXIT.
006990     EXIT.
006991                                 EJECT
006992 6500-GET-ACCOUNT-DATA.
006993
006994     IF  ADDRSI EQUAL '1'
006995             AND
006996         PI-CREATE-LABELS
006997             AND
006998         NOT PI-689-LABELS-OVERRIDEN
006999         NEXT SENTENCE
007000     ELSE
007001         IF  W-FILE-NOT-USED (6)
007002             GO TO 6500-EXIT
007003         END-IF
007004     END-IF.
007005
007006 6500-READ-ACCT-ONLY.
007007
007008     IF  W-ACCT-ACCOUNT EQUAL LOW-VALUES
007009         MOVE ER-9327            TO EMI-ERROR
007010         MOVE -1                 TO MAINTL
007011         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007012         GO TO 6500-EXIT.
007013
007014     
      * EXEC CICS HANDLE CONDITION
007015*         NOTOPEN    (8020-ACCT-NOT-OPEN)
007016*         NOTFND     (6500-ACCT-NOT-FOUND)
007017*         ENDFILE    (6500-ONLY-STOP)
007018*    END-EXEC.
      *    MOVE '"$JI''                 ! 1 #00013164' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'3120233030303133313634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007019
007020     
      * EXEC CICS STARTBR
007021*         RIDFLD     (W-ACCT-KEY)
007022*         DATASET    (W-ACCT-FILE-ID)
007023*         KEYLENGTH  (20)
007024*         GENERIC
007025*    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00013170' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303133313730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 W-ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007026
007027     MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
007028     MOVE 'Y'                    TO W-ACCT-BROWSE-STARTED-SW.
007029
007030 6500-READNEXT.
007031
007032     
      * EXEC CICS READNEXT
007033*         DATASET    (W-ACCT-FILE-ID)
007034*         SET        (ADDRESS OF ACCOUNT-MASTER)
007035*         RIDFLD     (W-ACCT-KEY)
007036*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013182' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133313832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007037
007038     IF  W-ACCT-PARTIAL-KEY NOT EQUAL W-ACCT-SAVE-KEY
007039         IF  PRIOR-MATCH-ACCT-PKEY
007040             CONTINUE
007041*            GO TO 6500-ONLY-STOP
007042         ELSE
007043             IF  W-FILE-NOT-USED (6)
007044                 PERFORM 6500-ENDBR
007045                 GO TO 6500-EXIT
007046             ELSE
007047                 GO TO 6500-ACCT-NOT-FOUND
007048             END-IF
007049         END-IF
007050     ELSE
007051         SET PRIOR-MATCH-ACCT-PKEY TO TRUE
007052         MOVE AM-CARRIER           TO WS-SAV-AM-CARRIER
007053         MOVE AM-ACCOUNT           TO WS-SAV-AM-ACCOUNT
007054         MOVE AM-DEFN-1            TO WS-SAV-AM-DEFN-1
007055         MOVE AM-REMIT-TO          TO WS-SAV-AM-REMIT-TO
007056         MOVE AM-NAME              TO WS-SAV-AM-NAME
007057         MOVE AM-PERSON            TO WS-SAV-AM-PERSON
007058         MOVE AM-ADDRS             TO WS-SAV-AM-ADDRS
007059         MOVE SPACES               TO WS-SAV-AM-CITY
007060*        MOVE AM-CITY              TO WS-SAV-AM-CITY
007061         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
007062            DELIMITED BY '  ' INTO WS-SAV-AM-CITY
007063         END-STRING
007064         MOVE AM-ZIP               TO WS-SAV-AM-ZIP
007065         MOVE AM-TEL-NO            TO WS-SAV-AM-TEL-NO
007066         MOVE AM-CONTROL-NAME      TO WS-SAV-AM-CONTROL-NAME
007067         MOVE AM-CSR-CODE          TO WS-SAV-AM-CSR-CODE
007068         MOVE AM-ACCOUNT           TO WS-SAV-AM-ERACCT-ACCOUNT
007069         GO TO 6500-READNEXT
007070     END-IF
007071
007072     .
007073 6500-ONLY-STOP.
007074
007075     IF  NOT PI-NO-CARRIER-SECURITY
007076             AND
007077         PI-CARRIER-SECURITY NOT = WS-SAV-AM-CARRIER
007078         MOVE -1                 TO CARRIERL
007079         MOVE AL-UANON           TO CARRIERA
007080         MOVE ER-9095            TO EMI-ERROR
007081         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007082         PERFORM 6500-ENDBR
007083         GO TO 8200-SEND-DATAONLY.
007084
007085     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007086     MOVE WS-SAV-AM-NAME         TO W-LABEL-LINES (1).
007087     MOVE WS-SAV-AM-PERSON       TO W-LABEL-LINES (2).
007088     MOVE WS-SAV-AM-ADDRS        TO W-LABEL-LINES (3).
007089     MOVE WS-SAV-AM-CITY         TO W-LABEL-LINES (4).
007090
007091     MOVE WS-SAV-AM-ZIP          TO W-WORK-ZIP.
007092     MOVE SPACES                 TO W-LABEL-ZIP (5).
007093
007094     IF  SAVE-AM-CANADIAN-POST-CODE
007095         MOVE WS-SAV-AM-CANADIAN-POSTAL-CODE
007096                                 TO W-CANADIAN-POSTAL-CODES
007097         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
007098         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
007099         MOVE SPACES             TO W-LAB-CAN-DASH (5)
007100                                    W-LAB-CAN-FILLER (5)
007101     ELSE
007102         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
007103
007104         IF  W-WORK-ZIP4 GREATER THAN '0000'
007105             MOVE '-'            TO W-LABEL-DASH (5)
007106             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
007107         ELSE
007108             MOVE SPACES         TO W-LABEL-DASH (5)
007109                                    W-LABEL-2ND-ZIP (5).
007110
007111     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
007112
007113     IF  ADDRSI EQUAL '1'
007114             AND
007115         PI-CREATE-LABELS
007116             AND
007117         NOT PI-689-LABELS-OVERRIDEN
007118         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
007119                                    W-VG-TEXT (40)
007120         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
007121                                    W-VG-TEXT (41)
007122         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
007123                                    W-VG-TEXT (42)
007124         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
007125                                    W-VG-TEXT (43)
007126         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
007127                                    W-VG-TEXT (44)
007128         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
007129                                    W-VG-TEXT (45)
007130         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
007131         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
007132         SET W-RG-NDX UP BY +1
007133     ELSE
007134         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (40)
007135         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (41)
007136         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (42)
007137         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (43)
007138         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (44)
007139         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (45).
007140
007141     IF  WS-SAV-AM-TEL-NO NOT NUMERIC
007142             OR
007143         WS-SAV-AM-TEL-NO EQUAL ZEROS
007144         MOVE SPACES             TO W-VG-TEXT (46)
007145     ELSE
007146         MOVE ZEROS                  TO W-PHONE-IN
007147         MOVE WS-SAV-AM-AREA-CODE    TO W-PO-AREA
007148         MOVE WS-SAV-AM-TEL-PRE      TO W-PO-PFX
007149         MOVE WS-SAV-AM-TEL-NBR      TO W-PO-SFX
007150         MOVE W-PHONE-OUT            TO W-VG-TEXT (46).
007151
007152     IF WS-SAV-AM-CONTROL-NAME (1:1) NOT = LOW-VALUES
007153        MOVE WS-SAV-AM-CONTROL-NAME  TO W-VG-TEXT (47)
007154     ELSE
007155        MOVE SPACES                   TO W-VG-TEXT (47)
007156     END-IF.
007157
007158     MOVE WS-SAV-AM-CSR-CODE     TO W-VG-TEXT (48)
007159     MOVE WS-SAV-AM-ERACCT-ACCOUNT
007160                                 TO W-VG-TEXT (49)
007161
007162     MOVE WS-SAV-AM-ACCOUNT      TO W-VG-TEXT (72)
007163
007164     PERFORM 6500-ENDBR.
007165
007166     GO TO 6500-EXIT.
007167
007168 6500-ENDBR.
007169
007170     IF  W-ACCT-BROWSE-STARTED
007171         MOVE 'N'                TO W-ACCT-BROWSE-STARTED-SW
007172         
      * EXEC CICS ENDBR
007173*            DATASET (W-ACCT-FILE-ID)
007174*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013322' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133333232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007175     END-IF.
007176
007177 6500-ACCT-NOT-FOUND.
007178
007179     PERFORM 6500-ENDBR.
007180
007181     IF  DATASORI NOT EQUAL '1'
007182         MOVE ER-0179            TO EMI-ERROR
007183         GO TO 6500-EXIT.
007184
007185     MOVE ER-0179                TO EMI-ERROR.
007186     MOVE -1                     TO MAINTL.
007187     MOVE AL-UNBON               TO MAINTA.
007188     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007189     GO TO 8200-SEND-DATAONLY.
007190
007191 6500-EXIT.
007192     EXIT.
007193                                 EJECT
007194 6600-GET-COMPENSATION-DATA.
007195
007196     IF  ADDRSI EQUAL '4'
007197             AND
007198         PI-CREATE-LABELS
007199             AND
007200         NOT PI-689-LABELS-OVERRIDEN
007201         NEXT SENTENCE
007202     ELSE
007203         IF  W-FILE-NOT-USED (10)
007204             GO TO 6600-EXIT.
007205
007206     IF  W-COMP-RESP-PERSON GREATER THAN LOW-VALUES
007207         MOVE W-COMP-KEY         TO W-COMP-SAVE-KEY
007208         GO TO 6600-START.
007209
007210     IF  PI-ZERO-CARRIER
007211             OR
007212         PI-ZERO-CAR-GROUP
007213         MOVE ZEROS              TO W-COMP-CARRIER.
007214
007215     IF  PI-ZERO-GROUPING
007216             OR
007217         PI-ZERO-CAR-GROUP
007218         MOVE ZEROS              TO W-COMP-GROUPING.
007219
007220     IF  W-FILE-NOT-USED (6)
007221         PERFORM 6500-READ-ACCT-ONLY THRU 6500-READNEXT
007222         IF  EMI-ERROR EQUAL ER-0179
007223             MOVE ER-9327        TO EMI-ERROR
007224             MOVE -1             TO MAINTL
007225             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007226             GO TO 6600-EXIT.
007227
007228     MOVE  'A'                   TO W-COMP-TYPE.
007229
007230     IF  WS-SAV-AM-REMIT-TO GREATER THAN ZEROS
007231         MOVE WS-SAV-AM-AGT (WS-SAV-AM-REMIT-TO)
007232                                 TO W-COMP-RESP-PERSON
007233     ELSE
007234         MOVE PI-689-ACCOUNT     TO W-COMP-ACCOUNT.
007235
007236     PERFORM VARYING W-NDX FROM +1 BY +1 UNTIL
007237        (W-NDX > +10)
007238            OR
007239         (WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D' OR 'F')
007240     END-PERFORM
007241
007242     IF  WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D' OR 'F'
007243         MOVE WS-SAV-AM-AGT (W-NDX)  TO W-COMP-ACCOUNT
007244     ELSE
007245         MOVE PI-689-ACCOUNT     TO W-COMP-ACCOUNT.
007246
007247     PERFORM 6500-ENDBR.
007248     MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY.
007249
007250     IF  W-COMP-RESP-PERSON EQUAL LOW-VALUES
007251         MOVE ER-9327            TO EMI-ERROR
007252         MOVE -1                 TO MAINTL
007253         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007254         GO TO 6600-EXIT.
007255
007256 6600-START.
007257
007258     
      * EXEC CICS HANDLE CONDITION
007259*         NOTOPEN (8080-COMP-NOT-OPEN)
007260*         NOTFND  (6600-COMP-NOT-FOUND)
007261*    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00013408' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303133343038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007262
007263     
      * EXEC CICS STARTBR
007264*         RIDFLD  (W-COMP-KEY)
007265*         DATASET (W-COMP-FILE-ID)
007266*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013413' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303133343133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007267
007268     MOVE 'Y'                    TO W-COMP-BROWSE-SW.
007269
007270 6600-READNEXT.
007271
007272     
      * EXEC CICS READNEXT
007273*         DATASET (W-COMP-FILE-ID)
007274*         SET     (ADDRESS OF COMPENSATION-MASTER)
007275*         RIDFLD  (W-COMP-KEY)
007276*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013422' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133343232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007277
007278     IF CO-CONTROL-PRIMARY
007279           NOT = W-COMP-SAVE-KEY
007280        GO TO 6600-COMP-NOT-FOUND.
007281
007282     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007283     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1)
007284                                    W-VG-TEXT (140).
007285     MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
007286     MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
007287     MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
007288*    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
007289     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
007290        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
007291     END-STRING
007292
007293     MOVE CO-ZIP                 TO W-WORK-ZIP.
007294
007295     IF  CO-CANADIAN-POST-CODE
007296         MOVE CO-CANADIAN-POSTAL-CODE
007297                                 TO W-CANADIAN-POSTAL-CODES
007298         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
007299         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
007300         MOVE SPACES             TO W-LAB-CAN-DASH (6)
007301                                    W-LAB-CAN-FILLER (6)
007302     ELSE
007303         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
007304
007305         IF  W-WORK-ZIP4 GREATER THAN '0000'
007306             MOVE '-'            TO W-LABEL-DASH (6)
007307             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
007308         ELSE
007309             MOVE SPACES         TO W-LABEL-DASH (6)
007310                                    W-LABEL-2ND-ZIP (6).
007311
007312     IF  CO-TELEPHONE NOT NUMERIC
007313             OR
007314         CO-TELEPHONE = ZEROS
007315         MOVE SPACES             TO W-VG-TEXT (147)
007316     ELSE
007317         MOVE ZEROS              TO W-PHONE-IN
007318         MOVE CO-AREA-CODE       TO W-PO-AREA
007319         MOVE CO-PREFIX          TO W-PO-PFX
007320         MOVE CO-PHONE           TO W-PO-SFX
007321         MOVE W-PHONE-OUT        TO W-VG-TEXT (147).
007322
007323     IF (CO-FAXNO NOT NUMERIC)
007324        OR (CO-FAXNO = ZEROS)
007325        MOVE SPACES              TO W-VG-TEXT (182)
007326     ELSE
007327        MOVE ZEROS               TO W-PHONE-IN
007328        MOVE CO-FAX-AREA-CODE    TO W-PO-AREA
007329        MOVE CO-FAX-PREFIX       TO W-PO-PFX
007330        MOVE CO-FAX-PHONE        TO W-PO-SFX
007331        MOVE W-PHONE-OUT         TO W-VG-TEXT (182)
007332     END-IF
007333
007334     IF CO-GA-STATUS-CODE NOT = 'A' AND 'I' AND 'P'
007335        MOVE SPACES              TO W-VG-TEXT (183)
007336     ELSE
007337        EVALUATE CO-GA-STATUS-CODE
007338           WHEN 'A'
007339              MOVE 'ACTIVE'      TO W-VG-TEXT (183)
007340           WHEN 'I'
007341              MOVE 'INACTIVE'    TO W-VG-TEXT (183)
007342           WHEN 'P'
007343              MOVE 'PENDING'     TO W-VG-TEXT (183)
007344        END-EVALUATE
007345     END-IF
007346
007347     IF CO-BILL-SW NOT = 'B' AND 'R' AND 'T' AND 'S'
007348        MOVE SPACES              TO W-VG-TEXT (181)
007349     ELSE
007350        EVALUATE CO-BILL-SW
007351           WHEN 'B'
007352              MOVE 'BILLED'      TO W-VG-TEXT (181)
007353           WHEN 'R'
007354              MOVE 'REMIT'       TO W-VG-TEXT (181)
007355           WHEN 'T'
007356              MOVE 'T'           TO W-VG-TEXT (181)
007357           WHEN 'S'
007358              MOVE 'S'           TO W-VG-TEXT (181)
007359        END-EVALUATE
007360     END-IF
007361
007362     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
007363
007364     IF  ADDRSI EQUAL '4'
007365             AND
007366         PI-CREATE-LABELS
007367             AND
007368         NOT PI-689-LABELS-OVERRIDEN
007369         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
007370                                    W-VG-TEXT (141)
007371         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
007372                                    W-VG-TEXT (142)
007373         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
007374                                    W-VG-TEXT (143)
007375         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
007376                                    W-VG-TEXT (144)
007377         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
007378                                    W-VG-TEXT (145)
007379         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
007380                                    W-VG-TEXT (146)
007381         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
007382         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
007383         SET W-RG-NDX UP BY +1
007384     ELSE
007385         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (141)
007386         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (142)
007387         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (143)
007388         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (144)
007389         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (145)
007390         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (146).
007391
007392     PERFORM 6630-GET-CSR-NAME THRU 6630-EXIT.
007393
007394     MOVE CO-RESP-NO             TO W-VG-TEXT (180).
007395     MOVE CO-END-BAL             TO W-EDIT-7-2-NEGATIVE.
007396     MOVE W-EDIT-7-2-NEGATIVE    TO W-VG-TEXT (150).
007397
007398     MOVE CO-LAST-STMT-DT        TO DC-GREG-DATE-1-YMD.
007399     MOVE '3'                    TO DC-OPTION-CODE.
007400     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
007401
007402     IF  NO-CONVERSION-ERROR
007403         MOVE DC-GREG-DATE-1-ALPHA
007404                                 TO W-VG-TEXT (149).
007405
007406     PERFORM 6620-GET-G-DATA THRU 6620-EXIT.
007407     PERFORM 6626-GET-B-DATA     THRU 6626-EXIT
007408
007409     PERFORM 6600-ENDBR.
007410     GO TO 6600-EXIT.
007411
007412 6600-ENDBR.
007413
007414     IF  W-COMP-BROWSE-STARTED
007415         MOVE 'N'                      TO W-COMP-BROWSE-SW
007416         
      * EXEC CICS ENDBR
007417*             DATASET(W-COMP-FILE-ID)
007418*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013566' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133353636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007419
007420 6600-COMP-NOT-FOUND.
007421
007422     PERFORM 6600-ENDBR.
007423
007424     IF ADDRSL GREATER ZERO
007425         IF ADDRSI = '4'
007426            IF PI-689-TYPE EQUAL LOW-VALUES OR SPACES
007427                MOVE ER-7378                TO EMI-ERROR
007428                MOVE -1                     TO TYPEL
007429                MOVE AL-UNBON               TO TYPEA
007430                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007431                GO TO 8200-SEND-DATAONLY
007432            ELSE
007433                MOVE ER-2114                TO EMI-ERROR
007434                MOVE -1                     TO ADDRSL
007435                MOVE AL-UNBON               TO ADDRSA
007436                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007437                GO TO 8200-SEND-DATAONLY.
007438
007439 6600-EXIT.
007440     EXIT.
007441
007442 6620-GET-G-DATA.
007443
007444     MOVE W-COMP-SAVE-KEY        TO W-COMP-KEY.
007445
007446     IF  CO-RPTCD2 GREATER THAN SPACES
007447         MOVE SPACES             TO W-COMP-RESP-PERSON
007448         MOVE CO-RPTCD2          TO W-COMP-WORK-AREA
007449         MOVE +10                TO W-CWA-NDX
007450         PERFORM 6622-FIND-LAST-NON-SPACE
007451         SET W-COMP-NDX          TO +10
007452         PERFORM 6625-FILL-IN-RESP-PERSON.
007453         INSPECT W-COMP-RESP-PERSON CONVERTING SPACES TO ZEROS.
007454
007455     MOVE LOW-VALUES             TO W-COMP-ACCOUNT.
007456     MOVE 'G'                    TO W-COMP-TYPE.
007457     MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY.
007458
007459     
      * EXEC CICS RESETBR
007460*         RIDFLD  (W-COMP-KEY)
007461*         DATASET (W-COMP-FILE-ID)
007462*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00013609' TO DFHEIV0
           MOVE X'263420202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303133363039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007463
007464     
      * EXEC CICS READNEXT
007465*         DATASET (W-COMP-FILE-ID)
007466*         SET     (ADDRESS OF COMPENSATION-MASTER)
007467*         RIDFLD  (W-COMP-KEY)
007468*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013614' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133363134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007469
007470     IF  CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
007471         GO TO 6620-EXIT.
007472
007473     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007474     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1).
007475     MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
007476     MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
007477     MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
007478*    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
007479     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
007480        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
007481     END-STRING
007482
007483     MOVE CO-ZIP                 TO W-WORK-ZIP.
007484
007485     IF  CO-CANADIAN-POST-CODE
007486         MOVE CO-CANADIAN-POSTAL-CODE
007487                                 TO W-CANADIAN-POSTAL-CODES
007488         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
007489         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
007490         MOVE SPACES             TO W-LAB-CAN-DASH (6)
007491                                    W-LAB-CAN-FILLER (6)
007492     ELSE
007493         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
007494         IF  W-WORK-ZIP4 GREATER THAN '0000'
007495             MOVE '-'            TO W-LABEL-DASH (6)
007496             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
007497         ELSE
007498             MOVE SPACES         TO W-LABEL-DASH (6)
007499                                    W-LABEL-2ND-ZIP (6).
007500
007501     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
007502
007503     MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (174).
007504     MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (175).
007505     MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (176).
007506     MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (177).
007507     MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (178).
007508     MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (179).
007509
007510 6620-EXIT.
007511     EXIT.
007512                                 EJECT
007513 6622-FIND-LAST-NON-SPACE.
007514
007515     IF  W-CWA-CHAR (W-CWA-NDX) EQUAL SPACES
007516         SUBTRACT +1 FROM W-CWA-NDX
007517         GO TO 6622-FIND-LAST-NON-SPACE.
007518
007519 6622-EXIT.
007520     EXIT.
007521                                 EJECT
007522 6625-FILL-IN-RESP-PERSON.
007523
007524     MOVE W-CWA-CHAR (W-CWA-NDX) TO W-COMP-RP-CHAR (W-COMP-NDX)
007525     SET W-COMP-NDX DOWN BY +1.
007526     SUBTRACT +1 FROM W-CWA-NDX.
007527
007528     IF  W-CWA-NDX GREATER THAN ZEROS
007529         GO TO 6625-FILL-IN-RESP-PERSON.
007530
007531 6625-EXIT.
007532     EXIT.
007533
007534 6626-GET-B-DATA.
007535
007536     MOVE W-COMP-SAVE-KEY        TO W-COMP-KEY
007537
007538     IF W-FILE-USED (8)
007539        PERFORM 6450-READ-CERT-ONLY
007540        IF W-CERT-FOUND
007541           MOVE CM-BENEFICIARY (1:10)
007542                                 TO W-COMP-RESP-PERSON
007543        ELSE
007544           MOVE SPACES           TO W-COMP-RESP-PERSON
007545           GO TO 6626-EXIT
007546        END-IF
007547     ELSE
007548        MOVE CM-BENEFICIARY (1:10)
007549                                 TO W-COMP-RESP-PERSON
007550     END-IF
007551
007552     MOVE LOW-VALUES             TO W-COMP-ACCOUNT
007553     MOVE 'B'                    TO W-COMP-TYPE
007554     MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY
007555
007556     
      * EXEC CICS RESETBR
007557*         RIDFLD  (W-COMP-KEY)
007558*         DATASET (W-COMP-FILE-ID)
007559*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00013706' TO DFHEIV0
           MOVE X'263420202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303133373036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007560
007561     
      * EXEC CICS READNEXT
007562*         DATASET (W-COMP-FILE-ID)
007563*         SET     (ADDRESS OF COMPENSATION-MASTER)
007564*         RIDFLD  (W-COMP-KEY)
007565*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013711' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007566
007567     IF  CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
007568         GO TO 6626-EXIT.
007569
007570     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007571     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1).
007572     MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
007573     MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
007574     MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
007575*    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
007576     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
007577        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
007578     END-STRING
007579
007580     MOVE CO-ZIP                 TO W-WORK-ZIP.
007581
007582     IF  CO-CANADIAN-POST-CODE
007583         MOVE CO-CANADIAN-POSTAL-CODE
007584                                 TO W-CANADIAN-POSTAL-CODES
007585         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
007586         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
007587         MOVE SPACES             TO W-LAB-CAN-DASH (6)
007588                                    W-LAB-CAN-FILLER (6)
007589     ELSE
007590         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
007591         IF  W-WORK-ZIP4 GREATER THAN '0000'
007592             MOVE '-'            TO W-LABEL-DASH (6)
007593             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
007594         ELSE
007595             MOVE SPACES         TO W-LABEL-DASH (6)
007596                                    W-LABEL-2ND-ZIP (6).
007597
007598     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
007599
007600     MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (184).
007601     MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (185).
007602     MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (186).
007603     MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (187).
007604     MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (188).
007605     MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (189).
007606
007607 6626-EXIT.
007608     EXIT.
007609
007610 6630-GET-CSR-NAME.
007611
007612     MOVE SPACES                 TO W-CNTL-KEY.
007613     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
007614     MOVE +0                     TO W-CNTL-SEQ-NO.
007615     MOVE '2'                    TO W-CNTL-RECORD-TYPE.
007616     MOVE CO-CSR-CODE            TO W-CNTL-GENL.
007617
007618     
      * EXEC CICS HANDLE CONDITION
007619*         NOTOPEN    (8040-CNTL-NOT-OPEN)
007620*         NOTFND     (6630-CNTL2-NOT-FOUND)
007621*    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00013768' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303133373638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007622
007623     
      * EXEC CICS READ
007624*         DATASET    (W-CNTL-FILE-ID)
007625*         SET        (ADDRESS OF CONTROL-FILE)
007626*         RIDFLD     (W-CNTL-KEY)
007627*    END-EXEC.
      *    MOVE '&"S        E          (   #00013773' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133373733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007628
007629     MOVE CF-PROCESSOR-NAME      TO W-VG-TEXT (148).
007630     MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (154)
007631     GO TO 6630-EXIT.
007632
007633 6630-CNTL2-NOT-FOUND.
007634
007635*    MOVE ER-8965                TO EMI-ERROR.
007636*    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007637*    MOVE -1                     TO MAINTL.
007638*    MOVE AL-UANON               TO MAINTA.
007639*    GO TO 8200-SEND-DATAONLY
007640     .
007641 6630-EXIT.
007642     EXIT.
007643                                 EJECT
007644 6700-GET-MAIL-DATA.
007645
007646     IF  ADDRSI EQUAL '5'
007647             AND
007648         PI-CREATE-LABELS
007649             AND
007650         NOT PI-689-LABELS-OVERRIDEN
007651         NEXT SENTENCE
007652     ELSE
007653         IF  W-FILE-NOT-USED (5)
007654             GO TO 6700-EXIT.
007655
007656     IF  W-MAIL-CERT-NO EQUAL LOW-VALUES
007657         MOVE ER-9327            TO EMI-ERROR
007658         MOVE -1                 TO MAINTL
007659         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007660         GO TO 6700-EXIT.
007661
007662     
      * EXEC CICS HANDLE CONDITION
007663*         NOTOPEN (8090-MAIL-NOT-OPEN)
007664*         NOTFND  (6700-MAIL-NOT-FOUND)
007665*    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00013812' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303133383132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007666
007667     
      * EXEC CICS READ
007668*         DATASET(W-MAIL-FILE-ID)
007669*         SET    (ADDRESS OF MAILING-DATA)
007670*         RIDFLD (W-MAIL-KEY)
007671*    END-EXEC.
      *    MOVE '&"S        E          (   #00013817' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133383137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-MAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007672
007673     MOVE MA-INSURED-LAST-NAME   TO W-NAME-LAST.
007674     MOVE MA-INSURED-FIRST-NAME  TO W-NAME-FIRST.
007675     MOVE MA-INSURED-MIDDLE-INIT TO W-NAME-MIDDLE.
007676     PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
007677
007678     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007679     MOVE WS-NAME-WORK           TO W-LABEL-LINES (1).
007680     MOVE W-LABEL-JOINT-NAME     TO W-LABEL-LINES (2).
007681     MOVE MA-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
007682     MOVE MA-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
007683
007684*    MOVE MA-CITY-STATE          TO W-LABEL-LINES (5).
007685
007686     STRING MA-CITY ' ' MA-ADDR-STATE
007687        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
007688     END-STRING
007689
007690     MOVE MA-ZIP                 TO W-WORK-ZIP.
007691     MOVE SPACES                 TO W-LABEL-ZIP (6).
007692
007693     IF  MA-CANADIAN-POST-CODE
007694         MOVE MA-CANADIAN-POSTAL-CODE
007695                                 TO W-CANADIAN-POSTAL-CODES
007696         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
007697         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
007698         MOVE SPACES             TO W-LAB-CAN-DASH (6)
007699                                    W-LAB-CAN-FILLER (6)
007700     ELSE
007701         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
007702         IF  W-WORK-ZIP4 GREATER THAN '0000'
007703             MOVE '-'            TO W-LABEL-DASH (6)
007704             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
007705         ELSE
007706             MOVE SPACES         TO W-LABEL-DASH (6)
007707                                    W-LABEL-2ND-ZIP (6).
007708
007709     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
007710
007711     IF  ADDRSI EQUAL '5'
007712             AND
007713         PI-CREATE-LABELS
007714             AND
007715         NOT PI-689-LABELS-OVERRIDEN
007716         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
007717                                    W-VG-TEXT (31)
007718         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
007719                                    W-VG-TEXT (32)
007720         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
007721                                    W-VG-TEXT (33)
007722         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
007723                                    W-VG-TEXT (34)
007724         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
007725                                    W-VG-TEXT (35)
007726         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
007727                                    W-VG-TEXT (36)
007728         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
007729         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
007730         SET W-RG-NDX UP BY +1
007731     ELSE
007732         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (31)
007733         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (32)
007734         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (33)
007735         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (34)
007736         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (35)
007737         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (36).
007738
007739     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
007740     MOVE MA-CRED-BENE-NAME      TO W-LABEL-LINES (1)
007741     MOVE MA-CRED-BENE-ADDR      TO W-LABEL-LINES (2)
007742     MOVE MA-CRED-BENE-ADDR2     TO W-LABEL-LINES (3)
007743
007744     STRING MA-CRED-BENE-CITY ' ' MA-CRED-BENE-STATE
007745        DELIMITED BY '  ' INTO W-LABEL-LINES (4)
007746     END-STRING
007747
007748     MOVE MA-CRED-BENE-ZIP       TO W-WORK-ZIP
007749     MOVE SPACES                 TO W-LABEL-ZIP (5)
007750
007751     IF  MA-CB-CANADIAN-POST-CODE
007752         MOVE MA-CB-CANADIAN-POSTAL-CODE
007753                                 TO W-CANADIAN-POSTAL-CODES
007754         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
007755         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
007756         MOVE SPACES             TO W-LAB-CAN-DASH (5)
007757                                    W-LAB-CAN-FILLER (5)
007758     ELSE
007759         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
007760         IF  W-WORK-ZIP4 GREATER THAN '0000'
007761             MOVE '-'            TO W-LABEL-DASH (5)
007762             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
007763         ELSE
007764             MOVE SPACES         TO W-LABEL-DASH (5)
007765                                    W-LABEL-2ND-ZIP (5)
007766         END-IF
007767     END-IF
007768
007769     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT
007770
007771     MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (37)
007772     MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (50)
007773     MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (51)
007774     MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (52)
007775     MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (53)
007776
007777     GO TO 6700-EXIT.
007778
007779 6700-MAIL-NOT-FOUND.
007780
007781     IF  ADDRSL GREATER ZERO
007782         IF ADDRSI = '5'
007783            MOVE ER-3000         TO EMI-ERROR
007784            MOVE -1              TO ADDRSL
007785            MOVE AL-UNBON        TO ADDRSA
007786            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007787            GO TO 8200-SEND-DATAONLY.
007788
007789 6700-EXIT.
007790     EXIT.
007791                                 EJECT
007792 6750-GET-LIFE-BENEFIT-DATA.
007793
007794     IF  W-FILE-NOT-USED (2)
007795         GO TO 6750-EXIT.
007796
007797     MOVE SPACES                 TO W-CNTL-GEN2.
007798
007799     IF  DATASORI EQUAL '2'
007800         IF  W-FILE-NOT-USED (8)
007801             PERFORM 6450-READ-CERT-ONLY
007802             IF  W-CERT-FOUND
007803                 MOVE CM-LF-BENEFIT-CD
007804                                 TO W-BEN-HOLD
007805                                    W-CNTL-GEN2
007806             ELSE
007807                 NEXT SENTENCE
007808         ELSE
007809             MOVE CM-LF-BENEFIT-CD
007810                                 TO W-BEN-HOLD
007811                                    W-CNTL-GEN2
007812     ELSE
007813         IF  DATASORI EQUAL '4'
007814             IF  W-FILE-NOT-USED (9)
007815                 PERFORM 6400-READ-PNDB-ONLY
007816                 IF  W-PNDB-FOUND
007817                     IF  PI-689-TYPE = '1'
007818                         MOVE PB-I-LF-BENEFIT-CD
007819                                 TO W-BEN-HOLD
007820                                    W-CNTL-GEN2
007821                     ELSE
007822                         MOVE PB-CI-LF-BENEFIT-CD
007823                                 TO W-BEN-HOLD
007824                                    W-CNTL-GEN2
007825             ELSE
007826                 NEXT SENTENCE
007827         ELSE
007828             IF  W-FILE-USED (8)
007829                 MOVE CM-LF-BENEFIT-CD
007830                                 TO W-BEN-HOLD
007831                                    W-CNTL-GEN2
007832             ELSE
007833                 IF  W-FILE-USED (9)
007834                     IF  PI-689-TYPE = '1'
007835                         MOVE PB-I-LF-BENEFIT-CD
007836                                 TO W-BEN-HOLD
007837                                    W-CNTL-GEN2
007838                     ELSE
007839                         MOVE PB-CI-LF-BENEFIT-CD
007840                                 TO W-BEN-HOLD
007841                                    W-CNTL-GEN2.
007842
007843     IF  W-CNTL-GEN2 EQUAL SPACES
007844         MOVE ER-9327            TO EMI-ERROR
007845         MOVE -1                 TO MAINTL
007846         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007847         GO TO 6750-EXIT.
007848
007849     MOVE SPACES                 TO W-CNTL-GEN1.
007850     MOVE '4'                    TO W-CNTL-RECORD-TYPE.
007851     MOVE ZEROS                  TO W-CNTL-SEQ-NO.
007852
007853     
      * EXEC CICS HANDLE CONDITION
007854*         NOTOPEN (8040-CNTL-NOT-OPEN)
007855*         NOTFND  (6750-EXIT)
007856*    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00014003' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303134303033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007857
007858     
      * EXEC CICS READ
007859*         DATASET (W-CNTL-FILE-ID)
007860*         SET     (ADDRESS OF CONTROL-FILE)
007861*         GTEQ
007862*         RIDFLD  (W-CNTL-KEY)
007863*    END-EXEC.
      *    MOVE '&"S        G          (   #00014008' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303134303038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007864
007865     MOVE 1                      TO W-NDX.
007866
007867 6750-LOOP.
007868
007869     IF  W-NDX = 9
007870         GO TO 6750-EXIT.
007871
007872     IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
007873         ADD 1                   TO W-NDX
007874         GO TO 6750-LOOP.
007875
007876     IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
007877         MOVE CF-BENEFIT-DESCRIP (W-NDX)
007878                                 TO W-VG-TEXT (11).
007879
007880 6750-EXIT.
007881     EXIT.
007882                                 EJECT
007883 6800-GET-A-H-BENEFIT-DATA.
007884
007885     IF  W-FILE-NOT-USED (3)
007886         GO TO 6800-EXIT.
007887
007888     MOVE SPACES                 TO W-CNTL-GEN2.
007889
007890     IF  DATASORI EQUAL '2'
007891         IF  W-FILE-NOT-USED (8)
007892             PERFORM 6450-READ-CERT-ONLY
007893             IF  W-CERT-FOUND
007894                 MOVE CM-AH-BENEFIT-CD
007895                                 TO W-BEN-HOLD
007896                                    W-CNTL-GEN2
007897             ELSE
007898                 NEXT SENTENCE
007899         ELSE
007900             MOVE CM-AH-BENEFIT-CD
007901                                 TO W-BEN-HOLD
007902                                    W-CNTL-GEN2
007903     ELSE
007904         IF  DATASORI EQUAL '4'
007905             IF  W-FILE-NOT-USED (9)
007906                 PERFORM 6400-READ-PNDB-ONLY
007907                 IF  W-PNDB-FOUND
007908                     IF  PI-689-TYPE = '1'
007909                         MOVE PB-I-AH-BENEFIT-CD
007910                                 TO W-BEN-HOLD
007911                                    W-CNTL-GEN2
007912                     ELSE
007913                         MOVE PB-CI-AH-BENEFIT-CD
007914                                 TO W-BEN-HOLD
007915                                    W-CNTL-GEN2
007916             ELSE
007917                 NEXT SENTENCE
007918         ELSE
007919             IF  W-FILE-USED (8)
007920                 MOVE CM-AH-BENEFIT-CD
007921                                 TO W-BEN-HOLD
007922                                    W-CNTL-GEN2
007923             ELSE
007924                 IF  W-FILE-USED (9)
007925                     IF  PI-689-TYPE = '1'
007926                         MOVE PB-I-AH-BENEFIT-CD
007927                                 TO W-BEN-HOLD
007928                                    W-CNTL-GEN2
007929                     ELSE
007930                         MOVE PB-CI-AH-BENEFIT-CD
007931                                 TO W-BEN-HOLD
007932                                    W-CNTL-GEN2.
007933
007934     IF  W-CNTL-GEN2 EQUAL SPACES
007935         MOVE ER-9327            TO EMI-ERROR
007936         MOVE -1                 TO MAINTL
007937         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007938         GO TO 6800-EXIT.
007939
007940     MOVE SPACES                 TO W-CNTL-GEN1.
007941     MOVE '5'                    TO W-CNTL-RECORD-TYPE.
007942     MOVE ZEROS                  TO W-CNTL-SEQ-NO.
007943
007944     
      * EXEC CICS HANDLE CONDITION
007945*         NOTOPEN (8040-CNTL-NOT-OPEN)
007946*         NOTFND  (6800-EXIT)
007947*    END-EXEC.
      *    MOVE '"$JI                  ! 6 #00014094' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303134303934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007948
007949     
      * EXEC CICS READ
007950*         DATASET (W-CNTL-FILE-ID)
007951*         SET     (ADDRESS OF CONTROL-FILE)
007952*         RIDFLD  (W-CNTL-KEY)
007953*         GTEQ
007954*    END-EXEC.
      *    MOVE '&"S        G          (   #00014099' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303134303939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007955
007956     MOVE 1                      TO W-NDX.
007957
007958 6800-LOOP-AH.
007959
007960     IF  W-NDX = 9
007961         GO TO 6800-EXIT.
007962
007963     IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
007964         ADD 1                   TO W-NDX
007965         GO TO 6800-LOOP-AH.
007966
007967     IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
007968         MOVE CF-BENEFIT-DESCRIP (W-NDX)
007969                                 TO W-VG-TEXT (15)
007970         MOVE CF-BENEFIT-ALPHA (W-NDX)
007971                                 TO W-BENEFIT-WORK
007972         MOVE W-ELIM-DAYS        TO W-VG-TEXT (16).
007973
007974 6800-EXIT.
007975     EXIT.
007976                                 EJECT
007977 6850-GET-CHECK-DATA.
007978
007979     IF  ADDRSI EQUAL '6'
007980             AND
007981         PI-CREATE-LABELS
007982             AND
007983         NOT PI-689-LABELS-OVERRIDEN
007984         NEXT SENTENCE
007985     ELSE
007986         IF  W-FILE-NOT-USED (12)
007987             GO TO 6850-EXIT.
007988
007989     IF  W-CHEK-CERT-NO EQUAL LOW-VALUES
007990         MOVE ER-9327            TO EMI-ERROR
007991         MOVE -1                 TO MAINTL
007992         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007993         GO TO 6850-EXIT.
007994
007995     
      * EXEC CICS HANDLE CONDITION
007996*         NOTOPEN (8045-CHEK-NOT-OPEN)
007997*         NOTFND  (6850-CHEK-NOT-FOUND)
007998*    END-EXEC.
      *    MOVE '"$JI                  ! 7 #00014145' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303134313435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007999
008000     
      * EXEC CICS READ
008001*         DATASET (W-CHEK-FILE-ID)
008002*         SET     (ADDRESS OF CHECK-RECORDS)
008003*         RIDFLD  (W-CHEK-KEY)
008004*    END-EXEC.
      *    MOVE '&"S        E          (   #00014150' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134313530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008005
008006     MOVE CH-CHECK-NO            TO W-VG-TEXT (158).
008007     MOVE CH-AMOUNT-PAID         TO W-EDIT-7-2.
008008     MOVE W-EDIT-7-2             TO W-VG-TEXT (157).
008009     MOVE CH-CHECK-QUE-CONTROL   TO W-DISPLAY-8
008010                                    PI-689-CONTROL.
008011     MOVE W-DISPLAY-8            TO W-VG-TEXT (165).
008012     MOVE CH-REASON-FOR-CHECK    TO W-VG-TEXT (166).
008013
008014     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
008015     MOVE CH-PAYEE-NAME-1        TO W-LABEL-LINES (1).
008016     MOVE CH-PAYEE-NAME-2        TO W-LABEL-LINES (2).
008017     MOVE CH-PAYEE-ADDRESS-1     TO W-LABEL-LINES (3).
008018     MOVE CH-PAYEE-ADDRESS-2     TO W-LABEL-LINES (4).
008019     MOVE CH-PAYEE-CITY-ST       TO W-LABEL-LINES (5).
008020
008021     MOVE CH-PAYEE-ZIP           TO W-WORK-ZIP.
008022     MOVE SPACES                 TO W-LABEL-ZIP (6).
008023
008024     IF  CH-CANADIAN-POST-CODE
008025         MOVE CH-CANADIAN-POSTAL-CODE
008026                                 TO W-CANADIAN-POSTAL-CODES
008027         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
008028         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
008029         MOVE SPACES             TO W-LAB-CAN-DASH (6)
008030                                    W-LAB-CAN-FILLER (6)
008031     ELSE
008032         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
008033         IF  W-WORK-ZIP4 GREATER THAN '0000'
008034             MOVE '-'            TO W-LABEL-DASH (6)
008035             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
008036         ELSE
008037             MOVE SPACES         TO W-LABEL-DASH (6)
008038                                    W-LABEL-2ND-ZIP (6).
008039
008040     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
008041
008042     IF  ADDRSI EQUAL '6'
008043             AND
008044         PI-CREATE-LABELS
008045             AND
008046         NOT PI-689-LABELS-OVERRIDEN
008047         MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
008048                                    W-VG-TEXT (159)
008049         MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
008050                                    W-VG-TEXT (160)
008051         MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
008052                                    W-VG-TEXT (161)
008053         MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
008054                                    W-VG-TEXT (162)
008055         MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
008056                                    W-VG-TEXT (163)
008057         MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
008058                                    W-VG-TEXT (164)
008059         MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
008060         SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
008061         SET W-RG-NDX UP BY +1
008062     ELSE
008063         MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (159)
008064         MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (160)
008065         MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (161)
008066         MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (162)
008067         MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (163)
008068         MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (164).
008069
008070     GO TO 6850-EXIT.
008071
008072 6850-CHEK-NOT-FOUND.
008073
008074     IF  ADDRSL GREATER ZERO
008075         IF ADDRSI = '6'
008076            MOVE ER-2908         TO EMI-ERROR
008077            MOVE -1              TO ADDRSL
008078            MOVE AL-UNBON        TO ADDRSA
008079            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008080            GO TO 8200-SEND-DATAONLY.
008081
008082 6850-EXIT.
008083     EXIT.
008084                                 EJECT
008085 6900-GET-PYAJ-DATA.
008086
008087     IF  W-FILE-NOT-USED (13)
008088         GO TO 6900-EXIT.
008089
008090     IF  W-PYAJ-FIN-RESP EQUAL LOW-VALUES
008091         MOVE ER-9327            TO EMI-ERROR
008092         MOVE -1                 TO MAINTL
008093         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008094         GO TO 6900-EXIT.
008095
008096     
      * EXEC CICS HANDLE CONDITION
008097*         NOTOPEN (8085-PYAJ-NOT-OPEN)
008098*         NOTFND  (6900-PYAJ-NOT-FOUND)
008099*    END-EXEC.
      *    MOVE '"$JI                  ! 8 #00014246' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303134323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008100
008101 6900-READ-NEXT.
008102
008103     
      * EXEC CICS READ
008104*         DATASET (W-PYAJ-FILE-ID)
008105*         SET     (ADDRESS OF PENDING-PAY-ADJ)
008106*         RIDFLD  (W-PYAJ-KEY)
008107*         GTEQ
008108*    END-EXEC.
      *    MOVE '&"S        G          (   #00014253' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303134323533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008109
008110     IF  PY-COMPANY-CD NOT EQUAL W-PYAJ-COMPANY-CD
008111             OR
008112         PY-CARRIER NOT EQUAL W-PYAJ-CARRIER
008113             OR
008114         PY-GROUPING NOT EQUAL W-PYAJ-GROUPING
008115             OR
008116         PY-FIN-RESP NOT EQUAL W-PYAJ-FIN-RESP
008117             OR
008118         PY-ACCOUNT NOT EQUAL W-PYAJ-ACCOUNT
008119         GO TO 6900-PYAJ-NOT-FOUND.
008120
008121     IF  PI-689-SEQ-NO GREATER THAN ZEROS
008122             AND
008123         PY-FILE-SEQ-NO NOT EQUAL PI-689-SEQ-NO
008124         GO TO 6900-PYAJ-NOT-FOUND.
008125
008126     IF  PY-RECORD-TYPE NOT EQUAL W-PYAJ-RECORD-TYPE
008127         MOVE PY-CONTROL-PRIMARY TO W-PYAJ-KEY
008128         ADD +1                  TO W-PYAJ-FILE-SEQ-NO
008129         GO TO 6900-READ-NEXT.
008130
008131     MOVE PY-CHECK-NUMBER        TO W-DISPLAY-7.
008132     MOVE W-DISPLAY-7            TO W-VG-TEXT (171).
008133
008134     MOVE PY-ENTRY-AMT           TO W-EDIT-7-2.
008135     MOVE W-EDIT-7-2             TO W-VG-TEXT (170).
008136
008137     MOVE PY-CHECK-QUE-CONTROL   TO W-DISPLAY-8
008138                                    PI-689-CONTROL.
008139     MOVE W-DISPLAY-8            TO W-VG-TEXT (172).
008140     MOVE PY-ENTRY-COMMENT       TO W-VG-TEXT (173).
008141
008142     GO TO 6900-EXIT.
008143
008144 6900-PYAJ-NOT-FOUND.
008145
008146     MOVE ER-7395                TO EMI-ERROR
008147     MOVE -1                     TO DATASORL
008148     MOVE AL-UNBON               TO DATASORA
008149     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008150     GO TO 8200-SEND-DATAONLY.
008151
008152 6900-EXIT.
008153     EXIT.
008154                                 EJECT
008155 6950-MOVE-SYSTEM-DATA.
008156
008157     IF  W-FILE-NOT-USED (7)
008158         GO TO 6950-EXIT.
008159
008160     MOVE EIBDATE                TO W-DATE-WORK.
008161     MOVE W-DT-WORK              TO DC-JULIAN-YYDDD.
008162     MOVE '5'                    TO DC-OPTION-CODE.
008163     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
008164     MOVE DC-GREG-DATE-1-EDIT    TO W-VG-TEXT (60).
008165     MOVE DC-GREG-DATE-1-ALPHA   TO W-VG-TEXT (61).
008166
008167     MOVE FORMI                  TO W-VG-TEXT (62).
008168
008169     IF  PI-689-VARIABLE-DATA-1 GREATER THAN SPACES
008170         MOVE PI-689-VARIABLE-DATA-1
008171                                 TO W-VG-TEXT (63)
008172         MOVE PI-689-VARIABLE-DATA-2
008173                                 TO W-VG-TEXT (64)
008174         MOVE PI-689-VARIABLE-DATA-3
008175                                 TO W-VG-TEXT (65)
008176         MOVE PI-689-VARIABLE-DATA-4
008177                                 TO W-VG-TEXT (66)
008178     ELSE
008179         MOVE '(VARIABLE NOT PROVIDED)'
008180                                 TO W-VG-TEXT (63).
008181
008182 6950-EXIT.
008183     EXIT.
008184                                 EJECT
008185 7000-FORMAT-SCREEN.
008186
008187     IF  MAINTI EQUAL 'S'
008188             OR
008189         PI-SHOW-MODE
008190         MOVE AL-PANOF           TO W-SC-TEXTA (W-SC-NDX)
008191     ELSE
008192         MOVE AL-UANOF           TO W-SC-TEXTA (W-SC-NDX).
008193
008194     IF  W-RG-NDX NOT GREATER THAN PI-689-NUMBER-LABEL-LINES
008195         SET W-LINE23            TO W-RG-NDX
008196         MOVE 'A'                TO W-LINE1
008197     ELSE
008198         SET W-LIN-NUM           TO W-RG-NDX
008199         COMPUTE W-LIN-NUM
008200             = W-LIN-NUM - PI-689-NUMBER-LABEL-LINES - 1.
008201
008202     MOVE W-LINE-NUM             TO W-SC-LINE (W-SC-NDX).
008203     MOVE W-RC-TEXT (W-RG-NDX)   TO W-SC-TEXT (W-SC-NDX).
008204     SET W-RG-NDX UP BY 1.
008205
008206 7000-EXIT.
008207      EXIT.
008208                                 EJECT
008209 7100-FORMAT-LAST-NAME-1ST.
008210*****************************************************************
008211*             M O V E   N A M E   R O U T I N E                 *
008212*     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
008213*     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
008214*     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
008215*                  FIELD                   VALUE                *
008216*           W-NAME-LAST    (CL15)      SMITH                    *
008217*           W-NAME-FIRST   (CL15)      JOHN                     *
008218*           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
008219*     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
008220*     CONTAIN                                                   *
008221*                SMITH, JOHN ALLEN                              *
008222*     OR                                                        *
008223*                SMITH, JOHN A.                                 *
008224*     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
008225*     COPYBOOK, ELCNWA.                                         *
008226*****************************************************************.
008227
008228     MOVE SPACES                 TO WS-NAME-WORK-AREA.
008229     MOVE ZERO                   TO WS-NAME-SW.
008230     SET NWA-INDEX               TO +1.
008231
008232     IF  W-NAME-LAST EQUAL SPACES
008233             AND
008234         W-NAME-MIDDLE EQUAL SPACES
008235         MOVE +1                 TO WS-NAME-SW.
008236
008237     MOVE W-NAME-LAST            TO WS-NAME-WORK2.
008238     PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
008239
008240     MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
008241     PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
008242
008243     SET NWA-INDEX UP BY +1.
008244
008245     IF  W-NAME-MIDDLE NOT EQUAL SPACES
008246         IF  W-NAME-MIDDLE-2 EQUAL SPACES
008247             MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
008248             SET NWA-INDEX UP BY +1
008249             MOVE '.'            TO WS-NW (NWA-INDEX)
008250             SET NWA-INDEX UP BY +2
008251         ELSE
008252             MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
008253             PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
008254
008255 7100-EXIT.
008256     EXIT.
008257                                 EJECT
008258 7110-MOVE-NAME.
008259
008260     IF  WS-NAME-SW GREATER THAN +1
008261         GO TO 7110-EXIT.
008262
008263     IF  WS-NAME-WORK2 = SPACES
008264         GO TO 7110-EXIT.
008265
008266     SET NWA-INDEX2            TO +1.
008267     SET NWA-INDEX3            TO +2.
008268
008269 7110-MOVE-NAME-CYCLE.
008270
008271     MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
008272
008273     IF  NWA-INDEX LESS THAN +30
008274         SET NWA-INDEX UP BY +1
008275     ELSE
008276         ADD +2                TO WS-NAME-SW
008277         GO TO 7110-EXIT.
008278
008279     IF  NWA-INDEX2 LESS THAN +20
008280         SET NWA-INDEX3 UP BY +1
008281         SET NWA-INDEX2 UP BY +1.
008282
008283     IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
008284             AND
008285         WS-NW2 (NWA-INDEX3) EQUAL SPACES
008286         IF  WS-NAME-SW EQUAL ZERO
008287             MOVE ','            TO WS-NW (NWA-INDEX)
008288             SET NWA-INDEX UP BY +2
008289             MOVE +1             TO WS-NAME-SW
008290             GO TO 7110-EXIT
008291         ELSE
008292             GO TO 7110-EXIT.
008293
008294     GO TO 7110-MOVE-NAME-CYCLE.
008295
008296 7110-EXIT.
008297     EXIT.
008298                                 EJECT
008299 7200-FORMAT-NAME-STRAIGHT.
008300*****************************************************************
008301*           M O V E   N A M E   R O U T I N E                   *
008302*     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
008303*     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
008304*     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
008305*                  FIELD                   VALUE                *
008306*           W-NAME-LAST    (CL15)      SMITH                    *
008307*           W-NAME-FIRST   (CL15)      JOHN                     *
008308*           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
008309*     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
008310*     CONTAIN                                                   *
008311*              JOHN A. SMITH                                    *
008312*     OR                                                        *
008313*              JOHN ALLEN SMITH                                 *
008314*     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
008315*     COPYBOOK, ELCNWA.                                         *
008316*****************************************************************
008317
008318     MOVE SPACES                 TO WS-NAME-WORK-AREA.
008319     MOVE ZERO                   TO WS-NAME-SW.
008320     SET NWA-INDEX               TO +1.
008321
008322     IF  W-NAME-FIRST EQUAL SPACES
008323             AND
008324         W-NAME-MIDDLE EQUAL SPACES
008325         MOVE W-NAME-LAST        TO WS-NAME-WORK
008326         GO TO 7200-EXIT.
008327
008328     MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
008329     PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
008330
008331     IF  W-NAME-MIDDLE NOT EQUAL SPACES
008332         SET NWA-INDEX UP BY +1
008333
008334         IF  W-NAME-MIDDLE-2 EQUAL SPACES
008335             MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
008336             SET NWA-INDEX UP BY +1
008337             MOVE '.'            TO WS-NW (NWA-INDEX)
008338             SET NWA-INDEX UP BY +1
008339
008340         ELSE
008341             MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
008342             PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
008343
008344     SET NWA-INDEX UP BY +1
008345     MOVE W-NAME-LAST            TO WS-NAME-WORK2.
008346     PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
008347
008348 7200-EXIT.
008349     EXIT.
008350                                 EJECT
008351 7290-MOVE-NAME.
008352
008353     IF  WS-NAME-SW GREATER THAN +1
008354         GO TO 7290-EXIT.
008355
008356     IF  WS-NAME-WORK2 EQUAL SPACES
008357         GO TO 7290-EXIT.
008358
008359     SET NWA-INDEX2            TO +1.
008360     SET NWA-INDEX3            TO +2.
008361
008362 7290-MOVE-NAME-CYCLE.
008363
008364     MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
008365
008366     IF  NWA-INDEX LESS THAN +30
008367         SET NWA-INDEX UP BY +1
008368     ELSE
008369         ADD +2                TO WS-NAME-SW
008370         GO TO 7290-EXIT.
008371
008372     IF  NWA-INDEX2 LESS THAN +20
008373         SET NWA-INDEX2 UP BY +1
008374         SET NWA-INDEX3 UP BY +1.
008375
008376     IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
008377             AND
008378         WS-NW2 (NWA-INDEX3) EQUAL SPACES
008379         GO TO 7290-EXIT.
008380
008381     GO TO 7290-MOVE-NAME-CYCLE.
008382
008383 7290-EXIT.
008384     EXIT.
008385                                 EJECT
008386 7300-LABEL-MOVE.
008387
008388     IF  W-LABEL-HOLD-AREA = SPACES
008389         MOVE +0                 TO W-NUMB-LABEL-LINES
008390         GO TO 7300-EXIT.
008391
008392     IF  W-LABEL-LINES (1) = SPACES
008393         MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
008394         MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
008395         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
008396         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
008397         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
008398         MOVE SPACES             TO W-LABEL-LINES (6)
008399         GO TO 7300-LABEL-MOVE.
008400
008401     IF  W-LABEL-LINES (2) = SPACES
008402             AND
008403         W-LABEL-LINES (3) = SPACES
008404             AND
008405         W-LABEL-LINES (4) = SPACES
008406             AND
008407         W-LABEL-LINES (5) = SPACES
008408             AND
008409         W-LABEL-LINES (6) = SPACES
008410         MOVE 1                  TO W-NDX
008411                                    W-NUMB-LABEL-LINES
008412         GO TO 7300-EXIT.
008413
008414 7300-TRY-2.
008415
008416     IF  W-LABEL-LINES (2) = SPACES
008417         MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
008418         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
008419         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
008420         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
008421         MOVE SPACES             TO W-LABEL-LINES (6)
008422         GO TO 7300-TRY-2.
008423
008424     IF  W-LABEL-LINES (3) = SPACES
008425             AND
008426         W-LABEL-LINES (4) = SPACES
008427             AND
008428         W-LABEL-LINES (5) = SPACES
008429             AND
008430         W-LABEL-LINES (6) = SPACES
008431         MOVE 2                  TO W-NDX
008432         GO TO 7300-MOVE-ZIP.
008433
008434 7300-TRY-3.
008435
008436     IF  W-LABEL-LINES (3) = SPACES
008437         MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
008438         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
008439         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
008440         MOVE SPACES             TO W-LABEL-LINES (6)
008441         GO TO 7300-TRY-3.
008442
008443     IF  W-LABEL-LINES (4) = SPACES
008444             AND
008445         W-LABEL-LINES (5) = SPACES
008446             AND
008447         W-LABEL-LINES (6) = SPACES
008448         MOVE 3                   TO W-NDX
008449         GO TO 7300-MOVE-ZIP.
008450
008451 7300-TRY-4.
008452
008453     IF  W-LABEL-LINES (4) = SPACES
008454         MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
008455         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
008456         MOVE SPACES             TO W-LABEL-LINES (6)
008457         GO TO 7300-TRY-4.
008458
008459     IF  W-LABEL-LINES (5) = SPACES
008460             AND
008461         W-LABEL-LINES (6) = SPACES
008462         MOVE 4                  TO W-NDX
008463         GO TO 7300-MOVE-ZIP.
008464
008465 7300-TRY-5.
008466
008467     IF  W-LABEL-LINES (5) = SPACES
008468         MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
008469         MOVE SPACES             TO W-LABEL-LINES (6)
008470         GO TO 7300-TRY-5
008471     ELSE
008472         IF  W-LABEL-LINES (6) = SPACES
008473             MOVE 5              TO W-NDX
008474             GO TO 7300-MOVE-ZIP
008475         ELSE
008476             MOVE 6              TO W-NDX.
008477
008478 7300-MOVE-ZIP.
008479
008480     COMPUTE W-NDX2 = W-NDX - 1.
008481
008482     IF  W-LAST-ZIP (W-NDX2) = SPACES
008483*****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
008484
008485         IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC
008486                 MOVE W-LABEL-ZIP (W-NDX)
008487                                 TO W-LAST-ZIP (W-NDX2)
008488                 MOVE SPACES     TO W-LABEL-LINES (W-NDX)
008489                 MOVE W-NDX2     TO W-NUMB-LABEL-LINES
008490                 GO TO 7300-EXIT.
008491
008492     MOVE W-NDX                  TO W-NUMB-LABEL-LINES.
008493
008494 7300-EXIT.
008495     EXIT.
008496                                 EJECT
008497 7400-CREATE-LETTER.
008498****************************************************************
008499*    THIS AREA CONTROLS THE PROCESSING OF INDIVIDUAL INPUT     *
008500*    RECORDS.                                                  *
008501****************************************************************
008502
008503     SET W-SQ-NDX                TO W-START-COLUMN.
008504     MOVE SPACES                 TO W-SQUEEZED-LINE
008505                                    W-WORK-LINE.
008506     MOVE W-START-COLUMN         TO W-LAST-SQUEEZED-SPACE
008507     MOVE +1                     TO W-LAST-WC-SPACE.
008508     MOVE HIGH-VALUES            TO W-LAST-SQ-CHAR.
008509
008510     SET W-TG-NDX                TO +1.
008511
008512     PERFORM 7410-TEXT-LINE-PROCESS THRU 7410-EXIT
008513            VARYING
008514         W-TG-NDX FROM W-TG-NDX BY +1
008515            UNTIL
008516         W-TG-NDX GREATER THAN W-TOTAL-TX-LINES.
008517
008518     IF  W-FORM-SQUEEZE-ON
008519             AND
008520         W-SQUEEZED-LINE GREATER THAN SPACES
008521         PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT.
008522
008523     PERFORM 7640-PAGING THRU 7640-EXIT.
008524
008525 7400-EXIT.
008526     EXIT.
008527                                 EJECT
008528 7410-TEXT-LINE-PROCESS.
008529***************************************************************
008530*    THIS ROUTINE SEARCHES THE TEXT WORK AREA FOR ANY         *
008531*    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
008532*    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
008533*    DATA THAT WAS GENERATED PREVIOUSLY.  WHILE PERFORMING    *
008534*    THIS FUNCTION THE ROUTINE MOVES EACH CHARACTER OR ITS    *
008535*    VARIABLE REPLACEMENT TO A WORK LINE.                     *
008536***************************************************************
008537
008538
008539     SET W-WC-NDX                TO W-ZEROS.
008540     MOVE HIGH-VALUES            TO W-LAST-CHAR.
008541
008542     MOVE W-TX-SC (W-TG-NDX)     TO W-LINE-SQUEEZE-IND.
008543
008544     PERFORM 7420-TEXT-CHAR-PROCESS THRU 7420-EXIT
008545             VARYING
008546         W-TX-NDX FROM +1 BY +1
008547             UNTIL
008548         W-TX-NDX GREATER THAN +70.
008549
008550****************************************************************
008551*    IF SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS    *
008552*    EACH LINE OF TEXT IS PROCESSED ACCORDING TO THE INDICATED *
008553*    LINE SQUEEZE CONTROL.                                     *
008554****************************************************************
008555     IF  W-FORM-SQUEEZE-ON
008556
008557         IF  W-WORK-LINE GREATER THAN SPACES
008558                 OR
008559             W-AS-IS
008560             PERFORM 7450-SQUEEZING THRU 7450-EXIT
008561
008562         ELSE
008563             NEXT SENTENCE
008564****************************************************************
008565*    IF NO SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS *
008566*    EACH LINE OF TEXT IS PRINT AS IS EXCEPT FOR VARIABLES     *
008567****************************************************************
008568     ELSE
008569         IF  W-WORK-LINE GREATER THAN SPACES
008570             MOVE W-TX-PC (W-TG-NDX)
008571                                 TO W-PRINT-CONTROL
008572             MOVE W-WORK-LINE    TO W-SQUEEZED-LINE
008573             PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008574             MOVE SPACES         TO W-WORK-LINE.
008575
008576 7410-EXIT.
008577     EXIT.
008578                                 EJECT
008579 7420-TEXT-CHAR-PROCESS.
008580****************************************************************
008581*    LOOKING FOR VARIABLE INDICATORS (**XXX) THIS SECTION      *
008582*    CONTROLS THE CONVERSION OF THESE INDICATORS INTO THEIR    *
008583*    ACTUAL VALUES.  IT MOVES THESE REPLACEMENTS AND THE       *
008584*    REMAINING CHARACTERS TO THE WORK LINE.                    *
008585****************************************************************
008586
008587     IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
008588             AND
008589         W-LAST-CHAR EQUAL '@'
008590         PERFORM 7430-PROCESS-VARIABLE THRU 7430-EXIT.
008591
008592     SET W-WC-NDX UP BY +1.
008593
008594     IF  W-WC-NDX GREATER THAN +70
008595         IF  W-FORM-SQUEEZE-ON
008596                 AND
008597             NOT W-AS-IS
008598             PERFORM 7440-BACK-TO-NEAREST-WORD THRU 7440-EXIT
008599                     VARYING
008600                 W-WC-NDX FROM W-LAST-WC-SPACE BY +1
008601                     UNTIL
008602                 W-WC-NDX EQUAL +71
008603             SET W-TX-NDX        TO W-LAST-TX-SPACE
008604
008605             IF  W-WORK-LINE GREATER THAN SPACES
008606                 PERFORM 7450-SQUEEZING THRU 7450-EXIT
008607                 MOVE SPACES     TO W-WORK-LINE
008608                                    W-LINE-SQUEEZE-IND
008609                 SET W-WC-NDX    TO +1
008610             ELSE
008611                 SET W-WC-NDX    TO +1
008612         ELSE
008613             PERFORM 7460-REMAIN-CHAR-CHECK THRU 7460-EXIT.
008614
008615     MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008616                                 TO W-CHARACTER-TYPE.
008617
008618     SET W-WC-NDX2               TO W-WC-NDX.
008619     SET W-WC-NDX2 DOWN BY +1.
008620
008621     IF  W-PUNCTUATION
008622             AND
008623         W-WC-NDX2 GREATER THAN W-ZEROS
008624             AND
008625         W-WORK-CHAR (W-WC-NDX2) EQUAL SPACES
008626         SET W-WC-NDX DOWN BY +1.
008627
008628     MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008629                                 TO W-LAST-CHAR
008630                                    W-WORK-CHAR (W-WC-NDX).
008631
008632     IF  W-LAST-CHAR EQUAL SPACES
008633         SET W-LAST-WC-SPACE     TO W-WC-NDX
008634         SET W-LAST-TX-SPACE     TO W-TX-NDX.
008635
008636 7420-EXIT.
008637     EXIT.
008638                                EJECT
008639 7430-PROCESS-VARIABLE.
008640******************************************************************
008641*    THIS SECTION CHECKS ON ALL '@@XXX' FORMATED CHARACTER       *
008642*    GROUPS.  IF IN PROPER FORMAT, EACH GROUP IS REPLACED WITH   *
008643*    ITS CORRESPONDING VALUES.                                   *
008644******************************************************************
008645
008646     SET W-TX-NDX UP BY +1.
008647
008648     IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
008649         MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008650                                 TO W-V1
008651         SET W-TX-NDX UP BY +1
008652
008653         IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
008654             MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008655                                 TO W-V2
008656             SET W-TX-NDX UP BY +1
008657             IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
008658                 MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008659                                 TO W-V3
008660                 SET W-TX-NDX UP BY +1
008661                 MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
008662                                 TO W-FIELD-SQUEEZE-IND
008663                 GO TO 7430-CONTINUE
008664             ELSE
008665                 SET W-TX-NDX DOWN BY +3
008666                 MOVE ER-0180    TO EMI-ERROR
008667                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008668                 MOVE -1         TO MAINTL
008669                 MOVE HIGH-VALUES
008670                                 TO W-LAST-CHAR
008671         ELSE
008672             SET W-TX-NDX DOWN BY +2
008673             MOVE ER-0180        TO EMI-ERROR
008674             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008675             MOVE -1             TO MAINTL
008676             MOVE HIGH-VALUES    TO W-LAST-CHAR
008677     ELSE
008678         SET W-TX-NDX DOWN BY +1
008679         MOVE ER-7246            TO EMI-ERROR
008680         MOVE -1                 TO MAINTL
008681         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008682         MOVE HIGH-VALUES        TO W-LAST-CHAR.
008683
008684****************************************************************
008685*    WHEN THE PROPER FORM IS NOT FOUND THE CHARACTERS INVOLVED *
008686*    ARE TRANSFERRED INTACT AND A NOTE MADE INDICATING THIS    *
008687*    ACTION.                                                   *
008688****************************************************************
008689
008690     IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
008691         MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
008692         GO TO 7430-EXIT
008693     ELSE
008694         GO TO 7430-EXIT.
008695
008696 7430-CONTINUE.
008697
008698     IF  W-VAR-RELATIVE-NUM LESS THAN 001
008699             OR
008700         W-VAR-RELATIVE-NUM GREATER THAN W-NUM-OF-VARIABLES
008701         SET W-TX-NDX DOWN BY +4
008702         MOVE ER-0180            TO EMI-ERROR
008703         MOVE -1                 TO MAINTL
008704         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008705         MOVE HIGH-VALUES        TO W-LAST-CHAR
008706         IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
008707             MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
008708             GO TO 7430-EXIT
008709         ELSE
008710             GO TO 7430-EXIT.
008711
008712     SET W-VG-NDX                TO W-VAR-RELATIVE-NUM.
008713
008714     SET W-WC-NDX DOWN BY +1.
008715
008716     IF  (W-FORM-SQUEEZE-OFF
008717                 AND
008718             NOT W-SQUEEZE-FIELD)
008719             OR
008720         (W-FORM-SQUEEZE-ON
008721                 AND
008722             W-AS-IS)
008723         PERFORM 7435-MOVE-VARIABLE-DIRECT THRU 7435-EXIT
008724                 VARYING
008725             W-VC-NDX FROM +1 BY +1
008726                 UNTIL
008727             W-VC-NDX GREATER THAN
008728                 W-VARIABLE-SIZE (W-VG-NDX)
008729
008730         IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +5
008731             SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
008732             SET W-TX-NDX DOWN BY +5
008733             IF  W-TX-NDX GREATER THAN +70
008734                 SET W-TX-NDX    TO +70
008735                 GO TO 7430-EXIT
008736             ELSE
008737                 GO TO 7430-EXIT
008738         ELSE
008739             COMPUTE W-ADJUST-SHORT
008740                 = 5 - W-VARIABLE-SIZE (W-VG-NDX)
008741             SET W-WC-NDX UP BY W-ADJUST-SHORT
008742             GO TO 7430-EXIT.
008743
008744     IF  W-SQUEEZE-FIELD
008745         SET W-TX-NDX UP BY +1.
008746
008747     MOVE SPACES                 TO W-FIRST-CHAR-FOUND-IND.
008748
008749     PERFORM 7470-MOVE-CHAR THRU 7470-EXIT
008750             VARYING
008751         W-VC-NDX FROM +1 BY +1
008752             UNTIL
008753         W-VC-NDX GREATER THAN
008754             W-VARIABLE-SIZE (W-VG-NDX).
008755
008756     MOVE HIGH-VALUES            TO W-LAST-CHAR.
008757
008758     IF  W-FORM-SQUEEZE-ON
008759         GO TO 7430-EXIT.
008760
008761     IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +6
008762         SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
008763         SET W-TX-NDX DOWN BY +6
008764         IF  W-TX-NDX GREATER THAN +70
008765             SET W-TX-NDX        TO +70.
008766
008767     IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
008768         SET W-WC-NDX DOWN BY +1.
008769
008770 7430-EXIT.
008771     EXIT.
008772                                 EJECT
008773 7435-MOVE-VARIABLE-DIRECT.
008774
008775     IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
008776             AND
008777         W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
008778             AND
008779         W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
008780             AND
008781         (W-VC-NDX GREATER THAN +1
008782                 AND
008783             W-LAST-CHAR NOT EQUAL SPACE
008784                 AND
008785             NOT W-LAST-CHAR-PUNC)
008786         IF  W-LC-USE-BOTH-CASES
008787             INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
008788                 CONVERTING W-UPPER-CASE TO W-LOWER-CASE
008789         ELSE
008790             INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
008791                 CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
008792
008793     SET W-WC-NDX UP BY +1.
008794     MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
008795                                 TO W-WORK-CHAR (W-WC-NDX)
008796                                    W-LAST-CHAR.
008797
008798 7435-EXIT.
008799     EXIT.
008800                                 EJECT
008801 7437-FIND-NEXT-CHARACTER.
008802*DELIBERATELY LEFT BLANK.
008803 7437-EXIT.
008804     EXIT.
008805                                 EJECT
008806 7440-BACK-TO-NEAREST-WORD.
008807
008808     MOVE SPACES                 TO W-WORK-CHAR (W-WC-NDX).
008809
008810 7440-EXIT.
008811     EXIT.
008812                                 EJECT
008813 7450-SQUEEZING.
008814
008815     IF  W-NEW-PARAGRAPH
008816         PERFORM 7600-SET-PARA-INDENT THRU 7600-EXIT
008817         MOVE SPACES             TO W-LINE-SQUEEZE-IND
008818         IF  W-SQUEEZED-LINE GREATER THAN SPACES
008819             PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008820             MOVE W-TX-PC (W-TG-NDX)
008821                                 TO W-PRINT-CONTROL
008822             MOVE SPACES         TO W-SQUEEZED-LINE
008823             SET W-SQ-NDX        TO W-INITIAL-COLUMN
008824             SET W-SQ-NDX UP BY W-WORK-INDENT
008825         ELSE
008826             MOVE W-TX-PC (W-TG-NDX)
008827                                 TO W-PRINT-CONTROL
008828             SET W-SQ-NDX        TO W-INITIAL-COLUMN
008829             SET W-SQ-NDX UP BY W-WORK-INDENT
008830     ELSE
008831         IF  W-DO-NOT-ADJUST
008832             MOVE SPACES         TO W-LINE-SQUEEZE-IND
008833             IF  W-SQUEEZED-LINE GREATER THAN SPACES
008834                 PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008835                 MOVE W-WORK-LINE
008836                                 TO W-SQUEEZED-LINE
008837                 MOVE W-TX-PC (W-TG-NDX)
008838                                 TO W-PRINT-CONTROL
008839                 PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008840                 SET W-SQ-NDX    TO W-INITIAL-COLUMN
008841                 MOVE SPACES     TO W-SQUEEZED-LINE
008842                 GO TO 7450-EXIT
008843             ELSE
008844                 MOVE W-WORK-LINE
008845                                 TO W-SQUEEZED-LINE
008846                 MOVE W-TX-PC (W-TG-NDX)
008847                                 TO W-PRINT-CONTROL
008848                 PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008849                 MOVE SPACES     TO W-SQUEEZED-LINE
008850                                    W-WORK-LINE
008851                 SET W-SQ-NDX    TO W-INITIAL-COLUMN
008852                 GO TO 7450-EXIT
008853         ELSE
008854             IF  W-ADJUST-TO-LINE-LENGTH
008855                 MOVE SPACES     TO W-LINE-SQUEEZE-IND
008856
008857                 IF  W-SQUEEZED-LINE GREATER THAN SPACES
008858                     PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008859                     SET W-SQ-NDX
008860                                 TO W-INITIAL-COLUMN
008861                     PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
008862                             VARYING
008863                         W-WC-NDX FROM 1 BY 1
008864                             UNTIL
008865                         W-SQ-NDX EQUAL W-TOO-FAR
008866                     MOVE W-TX-PC (W-TG-NDX)
008867                                 TO W-PRINT-CONTROL
008868                     PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008869                     SET W-SQ-NDX
008870                                 TO W-INITIAL-COLUMN
008871                     MOVE SPACES TO W-SQUEEZED-LINE
008872                                    W-WORK-LINE
008873                     GO TO 7450-EXIT
008874                 ELSE
008875                     SET W-SQ-NDX
008876                                 TO W-INITIAL-COLUMN
008877                     PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
008878                             VARYING
008879                         W-WC-NDX FROM 1 BY 1
008880                             UNTIL
008881                         W-SQ-NDX EQUAL W-TOO-FAR
008882                     MOVE W-TX-PC (W-TG-NDX)
008883                                 TO W-PRINT-CONTROL
008884                     PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008885                     MOVE SPACES TO W-SQUEEZED-LINE
008886                                    W-WORK-LINE
008887                     SET W-SQ-NDX
008888                                 TO W-INITIAL-COLUMN
008889                     GO TO 7450-EXIT
008890             ELSE
008891                 IF  W-CONTINUE-PARAGRAPH
008892                     IF  W-SQUEEZED-LINE GREATER THAN SPACES
008893                         PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008894                         PERFORM 7610-SET-CONP-INDENT
008895                             THRU 7610-EXIT
008896                         MOVE SPACES
008897                                 TO W-LINE-SQUEEZE-IND
008898                         MOVE W-TX-PC (W-TG-NDX)
008899                                 TO W-PRINT-CONTROL
008900                         MOVE SPACES
008901                                 TO W-SQUEEZED-LINE
008902                         SET W-SQ-NDX
008903                                 TO W-INITIAL-COLUMN
008904                         SET W-SQ-NDX UP BY W-WORK-INDENT
008905                     ELSE
008906                         PERFORM 7610-SET-CONP-INDENT
008907                             THRU 7610-EXIT
008908                         MOVE SPACES
008909                                 TO W-LINE-SQUEEZE-IND
008910                         MOVE W-TX-PC (W-TG-NDX)
008911                                 TO W-PRINT-CONTROL
008912                         MOVE SPACES
008913                                 TO W-SQUEEZED-LINE
008914                         SET W-SQ-NDX
008915                                 TO W-INITIAL-COLUMN
008916                         SET W-SQ-NDX UP BY W-WORK-INDENT.
008917
008918
008919     PERFORM 7480-UPDATE-SQUEEZED-LINE THRU 7480-EXIT
008920             VARYING
008921         W-WC-NDX FROM +1 BY +1
008922             UNTIL
008923         W-WC-NDX GREATER THAN +70.
008924
008925 7450-EXIT.
008926     EXIT.
008927                                 EJECT
008928 7460-REMAIN-CHAR-CHECK.
008929
008930     IF  W-TX-NDX EQUAL W-WC-NDX
008931         MOVE W-TX-PC (W-TG-NDX)
008932                                 TO W-PRINT-CONTROL
008933         MOVE W-WORK-LINE        TO W-SQUEEZED-LINE
008934         PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008935         MOVE SPACES             TO W-WORK-LINE
008936         SET W-WC-NDX            TO +1
008937     ELSE
008938         SET W-TX-NDX1           TO W-TX-NDX
008939         SEARCH W-TX-CHAR
008940             VARYING W-TX-NDX1
008941             AT END
008942                 MOVE W-TX-PC (W-TG-NDX)
008943                                 TO W-PRINT-CONTROL
008944                 MOVE W-WORK-LINE
008945                                 TO W-SQUEEZED-LINE
008946                 PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008947                 MOVE SPACES     TO W-WORK-LINE
008948                 SET W-WC-NDX    TO +1
008949                 SET W-TX-NDX    TO +1
008950                 SET W-TG-NDX UP BY +1
008951             WHEN
008952                 W-TX-CHAR (W-TG-NDX W-TX-NDX1) NOT EQUAL SPACES
008953                 MOVE ER-9427    TO EMI-ERROR
008954                 MOVE -1         TO MAINTL
008955                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008956                 MOVE W-TX-PC (W-TG-NDX)
008957                                 TO W-PRINT-CONTROL
008958                 MOVE W-WORK-LINE
008959                                 TO W-SQUEEZED-LINE
008960                 PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
008961                 MOVE SPACES     TO W-WORK-LINE
008962                 SET W-WC-NDX    TO +1.
008963
008964 7460-EXIT.
008965     EXIT.
008966                                 EJECT
008967 7470-MOVE-CHAR.
008968
008969     IF  W-FIRST-CHAR-NOT-FOUND
008970             AND
008971         W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
008972         MOVE SPACES             TO W-LAST-CHAR
008973         GO TO 7470-EXIT.
008974
008975     MOVE 'Y'                    TO W-FIRST-CHAR-FOUND-IND.
008976
008977     IF  W-SQUEEZE-FIELD
008978             AND
008979         W-LAST-CHAR EQUAL SPACES
008980             AND
008981         W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
008982         GO TO 7470-EXIT.
008983
008984     SET W-WC-NDX UP BY +1.
008985
008986     IF  W-WC-NDX GREATER THAN +70
008987         IF  W-FORM-SQUEEZE-ON
008988             GO TO 7470-EXIT
008989         ELSE
008990             MOVE ER-9427        TO EMI-ERROR
008991             MOVE -1             TO MAINTL
008992             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008993             GO TO 7470-EXIT.
008994
008995     IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
008996             AND
008997         W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
008998             AND
008999         W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
009000             AND
009001         (W-VC-NDX GREATER THAN +1
009002                 AND
009003             W-LAST-CHAR NOT EQUAL SPACE
009004                 AND
009005             NOT W-LAST-CHAR-PUNC)
009006         IF  W-LC-USE-BOTH-CASES
009007             INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
009008                 CONVERTING W-UPPER-CASE TO W-LOWER-CASE
009009         ELSE
009010             INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
009011                 CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
009012
009013     MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
009014                                 TO W-WORK-CHAR (W-WC-NDX)
009015                                    W-LAST-CHAR.
009016
009017     IF  W-LAST-CHAR EQUAL SPACES
009018         SET W-LAST-WC-SPACE     TO W-WC-NDX.
009019
009020 7470-EXIT.
009021     EXIT.
009022                                 EJECT
009023 7480-UPDATE-SQUEEZED-LINE.
009024****************************************************************
009025*    THIS SECTION CONTROLS THE MOVEMENT FROM THE WORK LINE TO *
009026*    THE SQUEEZED LINE.  DURING THIS PROCESS ALL EXTRA SPACES  *
009027*    (DEFINED AS TWO OR MORE SPACES NOT FOLLOWING A END OF     *
009028*    SENTENCE INDICATOR) ARE REMOVED.  IF THE LAST CHARACTER   *
009029*    IS NOT A SPACE OR PUNCTUATION MARK AN UNFINISHED WORD IS  *
009030*    ASSUMED.  EVERYTHING TO THE PREVIOUS SPACE IS REMOVED     *
009031****************************************************************
009032
009033     SET W-SQ-NDX UP BY +1.
009034
009035     IF  W-SQ-NDX GREATER THAN W-LAST-COLUMN
009036         MOVE W-SQ-CHAR (W-LAST-COLUMN)
009037                                 TO W-CHARACTER-TYPE
009038         IF  W-PUNCTUATION
009039                 OR
009040             W-SPACE
009041             PERFORM 7485-FIND-NEXT-CHARACTER THRU 7485-EXIT
009042             PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
009043             MOVE SPACES         TO W-SQUEEZED-LINE
009044             SET W-SQ-NDX        TO W-START-COLUMN
009045             SET W-SQ-NDX UP BY W-NEXT-INDENT
009046             SET W-LAST-SQUEEZED-SPACE
009047                                 TO W-SQ-NDX
009048         ELSE
009049             PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
009050             PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
009051             MOVE SPACES         TO W-SQUEEZED-LINE
009052             SET W-SQ-NDX        TO W-START-COLUMN
009053             SET W-SQ-NDX UP BY W-NEXT-INDENT
009054             SET W-LAST-SQUEEZED-SPACE
009055                                 TO W-SQ-NDX.
009056
009057     IF  W-WC-NDX GREATER THAN +70
009058         GO TO 7480-EXIT.
009059
009060     MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
009061
009062     IF  W-END-OF-SENTENCE
009063         MOVE 'Y'                TO W-END-OF-SENTENCE-IND
009064         SET W-WORK-NDX          TO W-SQ-NDX
009065         COMPUTE W-DISPLAY-NDX = W-WORK-NDX - 1
009066         IF  W-DISPLAY-NDX EQUAL W-LAST-SQUEEZED-SPACE
009067             SET W-SQ-NDX        TO W-LAST-SQUEEZED-SPACE
009068         ELSE
009069             NEXT SENTENCE
009070     ELSE
009071         IF  W-PUNCTUATION
009072                 AND
009073             W-LAST-SQ-CHAR EQUAL SPACES
009074             SET W-SQ-NDX DOWN BY +1
009075             MOVE SPACES         TO W-END-OF-SENTENCE-IND
009076         ELSE
009077             IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
009078                 IF  W-LAST-SQ-CHAR EQUAL SPACES
009079                     IF  W-END-OF-SENTENCE-WORKING
009080                         MOVE SPACES
009081                                 TO W-END-OF-SENTENCE-IND
009082                         SET W-LAST-SQUEEZED-SPACE
009083                                 TO W-SQ-NDX
009084                     ELSE
009085                         SET W-SQ-NDX DOWN BY +1
009086                         GO TO 7480-EXIT
009087                 ELSE
009088                     SET W-LAST-SQUEEZED-SPACE
009089                                 TO W-SQ-NDX
009090             ELSE
009091                 MOVE SPACES     TO W-END-OF-SENTENCE-IND.
009092
009093     MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX)
009094                                    W-LAST-SQ-CHAR.
009095
009096     IF  W-WC-NDX EQUAL +70
009097             AND
009098         W-WORK-CHAR (W-WC-NDX) GREATER THAN SPACES
009099             AND
009100         W-SQ-NDX NOT EQUAL W-LAST-COLUMN
009101         SET W-SQ-NDX UP BY +1
009102         SET W-LAST-SQUEEZED-SPACE
009103                                 TO W-SQ-NDX
009104         MOVE SPACES             TO W-SQ-CHAR (W-SQ-NDX)
009105                                    W-LAST-SQ-CHAR.
009106
009107 7480-EXIT.
009108     EXIT.
009109                                 EJECT
009110 7485-FIND-NEXT-CHARACTER.
009111
009112     MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
009113
009114     IF  W-PUNCTUATION
009115         PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
009116     ELSE
009117         SET W-WC-NDX            TO W-WC-NDX
009118         SEARCH W-WORK-CHAR
009119             VARYING W-WC-NDX
009120             AT END
009121                 GO TO 7485-EXIT
009122             WHEN
009123                 W-WORK-CHAR (W-WC-NDX) NOT EQUAL SPACES
009124                 GO TO 7485-EXIT.
009125
009126 7485-EXIT.
009127     EXIT.
009128                                 EJECT
009129 7490-REMOVE-LAST-WORD.
009130
009131*********  NOTE: W-WORK-LINE ALWAYS STARTS WITH A FULL WORD*******
009132     IF  W-WC-NDX EQUAL +1
009133         GO TO 7490-EXIT.
009134
009135     ADD +1                      TO W-LAST-SQUEEZED-SPACE.
009136
009137     PERFORM 7500-CLEAR-SQUEEZED-EXCESS THRU 7500-EXIT
009138             VARYING
009139         W-SQ-NDX FROM W-LAST-SQUEEZED-SPACE BY +1
009140             UNTIL
009141         W-SQ-NDX EQUAL W-TOO-FAR.
009142
009143     SET W-WORK-NDX              TO W-WC-NDX.
009144     COMPUTE W-LAST-WC-SPACE
009145         = W-WORK-NDX - W-TOO-FAR + W-LAST-SQUEEZED-SPACE.
009146     SET W-WC-NDX                TO W-LAST-WC-SPACE.
009147
009148 7490-EXIT.
009149     EXIT.
009150                                 EJECT
009151 7500-CLEAR-SQUEEZED-EXCESS.
009152
009153     MOVE SPACES                 TO W-SQ-CHAR (W-SQ-NDX).
009154
009155 7500-EXIT.
009156     EXIT.
009157
009158 7505-MOVE-STRAIGHT.
009159
009160     SET W-SQ-NDX UP BY +1.
009161
009162     MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX).
009163
009164 7505-EXIT.
009165     EXIT.
009166                                 EJECT
009167 7510-MOVE-TO-TABLE.
009168
009169     IF  W-PRINT-CONTROL NOT NUMERIC
009170         MOVE 00                 TO W-PRINT-CONTROL
009171         ADD +1                  TO W-LINE-COUNT
009172     ELSE
009173         IF  W-PRINT-CONTROL EQUAL 99
009174             PERFORM 7640-PAGING THRU 7640-EXIT
009175             MOVE 00             TO W-PRINT-CONTROL
009176             SET W-RG-NDX UP BY +1
009177             MOVE W-TOP-FORM     TO W-RC-TEXT (W-RG-NDX)
009178             MOVE +1             TO W-LINE-COUNT
009179             SET W-RG-NDX UP BY W-TOP-MARGIN
009180         ELSE
009181           IF W-SQUEEZED-LINE (1:6) NOT EQUAL '&&&&&&'
009182             COMPUTE W-LINE-COUNT
009183                 = W-LINE-COUNT + W-PRINT-CONTROL + 1.
009184
009185     IF  W-LINE-COUNT GREATER THAN W-MAX-LINES-PER-PAGE
009186         PERFORM 7640-PAGING THRU 7640-EXIT
009187         MOVE 00                 TO W-PRINT-CONTROL
009188         SET W-RG-NDX UP BY +1
009189         MOVE W-TOP-FORM         TO W-RC-TEXT (W-RG-NDX)
009190         SET W-RG-NDX UP BY W-TOP-MARGIN
009191         MOVE +1                 TO W-LINE-COUNT.
009192
009193     SET W-RG-NDX UP BY W-PRINT-CONTROL.
009194     SET W-RG-NDX UP BY +1.
009195     MOVE W-SQUEEZED-LINE        TO W-RC-TEXT (W-RG-NDX).
009196     MOVE 00                     TO W-PRINT-CONTROL.
009197
009198     IF  W-FIRST-BAD-VARIABLE-FOUND
009199             AND
009200         W-FIRST-BAD-VARIABLE EQUAL ZEROS
009201         SET W-FIRST-BAD-VARIABLE
009202                                 TO W-RG-NDX.
009203
009204 7510-EXIT.
009205     EXIT.
009206                                 EJECT
009207 7600-SET-PARA-INDENT.
009208
009209     IF  W-LINE-SQUEEZE-IND EQUAL 'P'
009210         MOVE W-PARAGRAPH-INDENT TO W-WORK-INDENT
009211     ELSE
009212     IF  W-LINE-SQUEEZE-IND EQUAL 'Q'
009213         MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
009214         ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
009215     ELSE
009216     IF  W-LINE-SQUEEZE-IND EQUAL 'R'
009217         MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
009218         ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
009219     ELSE
009220     IF  W-LINE-SQUEEZE-IND EQUAL 'S'
009221         MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
009222         ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
009223     ELSE
009224     IF  W-LINE-SQUEEZE-IND EQUAL 'T'
009225         MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
009226         ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
009227     ELSE
009228     IF  W-LINE-SQUEEZE-IND EQUAL 'U'
009229         MOVE W-LINE-INDENT-5    TO W-WORK-INDENT
009230         ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT.
009231
009232     SET W-TG-NDX2               TO W-TG-NDX.
009233     SET W-TG-NDX2 UP BY +1.
009234
009235     IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
009236         MOVE W-WORK-INDENT      TO W-NEXT-INDENT
009237     ELSE
009238         PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
009239
009240 7600-EXIT.
009241     EXIT.
009242                                 EJECT
009243 7610-SET-CONP-INDENT.
009244
009245     IF  W-LINE-SQUEEZE-IND EQUAL 'C'
009246         MOVE +0                 TO W-WORK-INDENT
009247     ELSE
009248     IF  W-LINE-SQUEEZE-IND EQUAL 'D'
009249         MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
009250     ELSE
009251     IF  W-LINE-SQUEEZE-IND EQUAL 'E'
009252         MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
009253     ELSE
009254     IF  W-LINE-SQUEEZE-IND EQUAL 'F'
009255         MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
009256     ELSE
009257     IF  W-LINE-SQUEEZE-IND EQUAL 'G'
009258         MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
009259     ELSE
009260     IF  W-LINE-SQUEEZE-IND EQUAL 'H'
009261         MOVE W-LINE-INDENT-5    TO W-WORK-INDENT.
009262
009263     SET W-TG-NDX2               TO W-TG-NDX.
009264     SET W-TG-NDX2 UP BY +1.
009265
009266     IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
009267         MOVE W-WORK-INDENT      TO W-NEXT-INDENT
009268     ELSE
009269         PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
009270
009271 7610-EXIT.
009272     EXIT.
009273                                 EJECT
009274 7630-NEXT-LINE.
009275
009276     IF  W-TX-SC (W-TG-NDX2) EQUAL ' '
009277         MOVE ZEROS              TO W-NEXT-INDENT
009278     ELSE
009279     IF  W-TX-SC (W-TG-NDX2) EQUAL '1'
009280         MOVE W-LINE-INDENT-1    TO W-NEXT-INDENT
009281     ELSE
009282     IF  W-TX-SC (W-TG-NDX2) EQUAL '2'
009283         MOVE W-LINE-INDENT-2    TO W-NEXT-INDENT
009284     ELSE
009285     IF  W-TX-SC (W-TG-NDX2) EQUAL '3'
009286         MOVE W-LINE-INDENT-3    TO W-NEXT-INDENT
009287     ELSE
009288     IF  W-TX-SC (W-TG-NDX2) EQUAL '4'
009289         MOVE W-LINE-INDENT-4    TO W-NEXT-INDENT
009290     ELSE
009291     IF  W-TX-SC (W-TG-NDX2) EQUAL '5'
009292         MOVE W-LINE-INDENT-5    TO W-NEXT-INDENT
009293     ELSE
009294         MOVE W-WORK-INDENT      TO W-NEXT-INDENT
009295         SUBTRACT W-PARAGRAPH-INDENT
009296                                  FROM W-NEXT-INDENT.
009297
009298 7630-EXIT.
009299     EXIT.
009300                                 EJECT
009301 7640-PAGING.
009302
009303     IF  NOT W-LC-CREATE-PAGES
009304         GO TO 7640-EXIT.
009305
009306     IF  W-LINE-COUNT GREATER ZEROS
009307             AND
009308         W-RG-NDX GREATER THAN +1
009309         ADD +1                  TO W-PAGE
009310         MOVE W-PAGE             TO W-PAGE-NUMBER
009311         COMPUTE W-PAGE-LINE
009312             = W-MAX-LINES-PER-PAGE - W-LINE-COUNT + 3
009313         IF  W-PAGE-LINE GREATER THAN +2
009314             SET W-RG-NDX UP BY W-PAGE-LINE
009315             MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX)
009316         ELSE
009317             SET W-RG-NDX UP BY +2
009318             MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX).
009319
009320 7640-EXIT.
009321     EXIT.
009322                                 EJECT
009323 7700-READ-TEXT-TS.
009324**************************************************************
009325*  THIS PARAGRAPH READS THE TEMPORARY STORAGE RECORDS THAT   *
009326*  CONTAIN THE TEXT THAT IS PAST BETWEEN EL689, EL6892       *
009327*  AND EL1042.                                               *
009328**************************************************************
009329
009330     
      * EXEC CICS HANDLE CONDITION
009331*         QIDERR     (7700-TS-ERROR)
009332*         ITEMERR    (7700-TS-ERROR)
009333*    END-EXEC.
      *    MOVE '"$N<                  ! 9 #00015480' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303135343830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009334
009335     SET W-TS-NDX                TO 1.
009336     MOVE 1                      TO W-TS-ITEM.
009337
009338 7700-LOOP.
009339
009340     IF  W-TS-ITEM GREATER THAN PI-TEMP-STOR-ITEMS
009341         IF  PI-TEMP-STOR-ITEMS GREATER THAN ZEROS
009342             PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
009343             GO TO 7700-EXIT
009344         ELSE
009345             GO TO 7700-EXIT.
009346
009347     
      * EXEC CICS READQ TS
009348*         INTO     (W-TS-WORK-AREA)
009349*         QUEUE    (W-TS-NAME-TEXT)
009350*         LENGTH   (W-TS-LENGTH)
009351*         ITEM     (W-TS-ITEM)
009352*    END-EXEC.
      *    MOVE '*$II   L              ''   #00015497' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135343937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 W-TS-WORK-AREA, 
                 W-TS-LENGTH, 
                 W-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009353
009354     MOVE W-TS-WORK-AREA         TO W-TS-GROUP (W-TS-NDX).
009355     SET W-TS-NDX UP BY 1.
009356     ADD 1                       TO W-TS-ITEM.
009357     GO TO 7700-LOOP.
009358
009359 7700-TS-ERROR.
009360
009361     IF  EIBTRNID = W-TRANSACTION OR 'EX14'
009362         MOVE ER-0033            TO EMI-ERROR
009363         MOVE -1                 TO MAINTL
009364         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
009365         GO TO 8200-SEND-DATAONLY.
009366
009367 7700-EXIT.
009368      EXIT.
009369                                 EJECT
009370 7740-RESTORE-SCREEN.
009371
009372     MOVE 'C'                    TO MAINTI.
009373     MOVE +1                     TO MAINTL.
009374     MOVE AL-UANON               TO MAINTA.
009375
009376     MOVE PI-689-FORM-NUMBER     TO FORMI.
009377     MOVE +4                     TO FORML.
009378     MOVE AL-UANON               TO FORMA.
009379
009380*    IF  PI-689-PRINT-RESTRICTION GREATER THAN SPACES
009381*        MOVE PI-689-PRINT-RESTRICTION
009382*                                TO PRTRESTL
009383*        MOVE +1                 TO PRTRESTL
009384*        MOVE AL-UANON           TO PRTRESTA.
009385
009386     IF  PI-689-FOLLOW-UP-EDIT GREATER THAN SPACES
009387         MOVE PI-689-FOLLOW-UP-EDIT
009388                                 TO FOLLOWI
009389         MOVE +8                 TO FOLLOWL
009390         MOVE AL-UANON           TO FOLLOWA.
009391
009392     IF  PI-689-RESEND1-EDIT GREATER THAN SPACES
009393         MOVE PI-689-RESEND1-EDIT
009394                                 TO RESEND1I
009395         MOVE +8                 TO RESEND1L
009396         MOVE AL-UANON           TO RESEND1A.
009397
009398     IF  PI-689-ALT-PRINTER-ID GREATER THAN SPACES
009399         MOVE PI-689-ALT-PRINTER-ID
009400                                 TO PRINTERI
009401         MOVE +1                 TO PRINTERL
009402         MOVE AL-UANON           TO PRINTERA.
009403
009404     IF  PI-BYPASS-LABELS
009405          MOVE PI-LABEL-CONTROL  TO ADDRLBLI
009406          MOVE +1                TO ADDRLBLL
009407          MOVE AL-UANON          TO ADDRLBLA
009408     ELSE
009409          IF  PI-689-LBL-OVERRIDE GREATER THAN SPACES
009410               MOVE PI-689-LBL-OVERRIDE
009411                                 TO ADDRLBLI
009412               MOVE +1           TO ADDRLBLL
009413               MOVE AL-UANON     TO ADDRLBLA.
009414
009415     IF  PI-689-NUMBER-COPIES GREATER THAN ZEROS
009416         MOVE PI-689-NUMBER-COPIES   TO COPIESI
009417         MOVE +1                     TO COPIESL
009418         MOVE AL-UNNON               TO COPIESA.
009419
009420     IF  PI-689-LABEL-SOURCE GREATER THAN SPACES
009421         MOVE PI-689-LABEL-SOURCE
009422                                 TO ADDRSI
009423         MOVE +1                 TO ADDRSL
009424         MOVE AL-UANON           TO ADDRSA.
009425
009426     MOVE PI-689-DATA-SOURCE     TO DATASORI.
009427     MOVE +1                     TO DATASORL.
009428     MOVE AL-UANON               TO DATASORA.
009429
009430     IF  PI-689-CARRIER GREATER THAN SPACES
009431         MOVE PI-689-CARRIER     TO CARRIERI
009432         MOVE +1                 TO CARRIERL
009433         MOVE AL-UANON           TO CARRIERA.
009434
009435     IF  PI-689-GROUPING GREATER THAN SPACES
009436         MOVE PI-689-GROUPING    TO GROUPI
009437         MOVE +6                 TO GROUPL
009438         MOVE AL-UANON           TO GROUPA.
009439
009440     IF  PI-689-STATE GREATER THAN SPACES
009441         MOVE PI-689-STATE       TO STATEI
009442         MOVE +2                 TO STATEL
009443         MOVE AL-UANON           TO STATEA.
009444
009445     IF  PI-689-ACCOUNT GREATER THAN SPACES
009446         MOVE PI-689-ACCOUNT     TO ACCTI
009447         MOVE +10                TO ACCTL
009448         MOVE AL-UANON           TO ACCTA.
009449
009450     IF  PI-689-CERT-PRIME GREATER THAN SPACES
009451         MOVE PI-689-CERT-PRIME  TO CERTI
009452         MOVE +10                TO CERTL
009453         MOVE AL-UANON           TO CERTA.
009454
009455     IF  PI-689-CERT-PRIME GREATER THAN SPACES
009456         MOVE PI-689-CERT-SFX    TO SFXI
009457         MOVE +1                 TO SFXL
009458         MOVE AL-UANON           TO SFXA.
009459
009460     IF  PI-689-TYPE GREATER THAN SPACES
009461         MOVE PI-689-TYPE        TO TYPEI
009462         MOVE +1                 TO TYPEL
009463         MOVE AL-UANON           TO TYPEA.
009464
009465     IF  PI-689-RESP-PERSON GREATER THAN SPACES
009466         MOVE PI-689-RESP-PERSON TO RPERSONI
009467         MOVE +10                TO RPERSONL
009468         MOVE AL-UANON           TO RPERSONA.
009469
009470     IF  PI-689-ENTRY-BATCH GREATER THAN SPACES
009471         MOVE PI-689-ENTRY-BATCH TO BENTRYI
009472         MOVE +6                 TO BENTRYL
009473         MOVE AL-UANON           TO BENTRYA.
009474
009475     IF  PI-689-SEQ-EDIT GREATER THAN SPACES
009476         MOVE PI-689-SEQ-EDIT    TO SEQI
009477         MOVE +8                 TO SEQL
009478         MOVE AL-UNNON           TO SEQA.
009479
009480     IF  PI-689-BCSEQ-EDIT GREATER THAN SPACES
009481         MOVE PI-689-BCSEQ-EDIT  TO BCSEQI
009482         MOVE +4                 TO BCSEQL
009483         MOVE AL-UNNON           TO BCSEQA.
009484
009485     IF  PI-689-DATE-EDIT GREATER THAN SPACES
009486         MOVE PI-689-DATE-EDIT   TO DATEI
009487         MOVE +8                 TO DATEL
009488         MOVE AL-UNNON           TO DATEA.
009489
009490     IF PI-CERT-FORM-ID GREATER THAN SPACES
009491        MOVE PI-CERT-FORM-ID     TO CERTIDI
009492        MOVE +5                  TO CERTIDL
009493        MOVE AL-UNNON            TO CERTIDA
009494     END-IF.
009495
009496     IF PI-PRINT-NOW GREATER THAN SPACES
009497        MOVE PI-PRINT-NOW        TO PRTNOWI
009498        MOVE +1                  TO PRTNOWL
009499        MOVE AL-UNNON            TO PRTNOWA
009500     END-IF.
009501     IF PI-ENDT-ARCH-NO GREATER THAN ZERO
009502        MOVE PI-ENDT-ARCH-NO     TO ENDARCHI
009503        MOVE +8                  TO ENDARCHL
009504        MOVE AL-UNNON            TO ENDARCHA
009505     END-IF
009506
009507     MOVE PI-ENCLOSURE-CD        TO ENCI.
009508     MOVE +3                     TO ENCL.
009509     MOVE AL-UANON               TO ENCA.
009510
009511 7749-EXIT.
009512     EXIT.
009513                                 EJECT
009514 7760-PUT-TEMP-STORAGE.
009515**************************************************************
009516*  THIS PARAGRAPH WRITES THE TEMPORARY STORAGE RECORDS THAT  *
009517*  WILL CONTAIN THE CONTENTS OF THE TEXT TABLE FOR USE BY    *
009518*  EL1042 AND EL6892.                                        *
009519**************************************************************
009520
009521     SET W-TS-NDX                TO 1.
009522     MOVE 0                      TO PI-TEMP-STOR-ITEMS.
009523
009524     MOVE -1                     TO MAINTL.
009525     PERFORM 7840-WRITE-TS THRU 7840-EXIT
009526             VARYING
009527         W-TS-GROUP-WORK FROM 0 BY W-TS-NUM-REC-IN-GROUP
009528             UNTIL
009529         W-TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
009530
009531 7760-EXIT.
009532      EXIT.
009533                                 EJECT
009534 7780-DELETE-TEMP-STOR-TEXT.
009535
009536     
      * EXEC CICS HANDLE CONDITION
009537*         QIDERR      (7780-EXIT)
009538*    END-EXEC.
      *    MOVE '"$N                   ! : #00015686' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303135363836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009539
009540     
      * EXEC CICS DELETEQ TS
009541*         QUEUE       (W-TS-NAME-TEXT)
009542*    END-EXEC.
      *    MOVE '*&                    #   #00015690' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303135363930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009543
009544 7780-EXIT.
009545     EXIT.
009546                                 EJECT
009547 7840-WRITE-TS.
009548
009549     MOVE W-TS-GROUP (W-TS-NDX)  TO W-TS-WORK-AREA.
009550     SET W-TS-NDX UP BY 1.
009551     ADD 1                       TO PI-TEMP-STOR-ITEMS.
009552
009553     
      * EXEC CICS WRITEQ TS
009554*         FROM    (W-TS-WORK-AREA)
009555*         QUEUE   (W-TS-NAME-TEXT)
009556*         LENGTH  (W-TS-LENGTH)
009557*         ITEM    (PI-TEMP-STOR-ITEMS)
009558*    END-EXEC.
      *    MOVE '*" I   L              ''   #00015703' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135373033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 W-TS-WORK-AREA, 
                 W-TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009559
009560 7840-EXIT.
009561     EXIT.
009562     EJECT
009563 7900-PRINT-LETTER-NOW.
009564***************************************************************
009565*     THIS ROUTINE WILL CAUSE THE CURRENTLY CREATED LETTER    *
009566*     TO BE PRINTED ON A HARDCOPY PRINTER.                    *
009567*     THE TEXT IS EDITED FOR ANY UNRESOLVED SYMBOLS.          *
009568*     THE PRINTER ID IS OBTAINED FROM THE CONTROL FILE.       *
009569*     THE LETTER IS SAVED IN TEMP-STORAGE AND A START IS      *
009570*     ISSUED FOR THE PRINT TRANSACTION.                       *
009571***************************************************************
009572
009573*    IF (EIBAID = DFHPF4 OR DFHPF8)
009574*            AND
009575*        PI-SHOW-MODE
009576*        IF  PRINTERL GREATER ZERO
009577*            INSPECT PRINTERI CONVERTING W-LOWER-CASE TO
009578*                                                    W-UPPER-CASE
009579*            MOVE PRINTERI       TO PI-689-ALT-PRINTER-ID
009580*                                   PI-ALT-DMD-PRT-ID.
009581*
009582*    IF  EIBAID EQUAL DFHPF8
009583*            AND
009584*        PI-SHOW-MODE
009585*            AND
009586*        LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES
009587*        MOVE ER-7247            TO EMI-ERROR
009588*        MOVE -1                 TO MAINTL
009589*        MOVE AL-UABON           TO MAINTA
009590*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
009591*        GO TO 8200-SEND-DATAONLY.
009592
009593     IF  PI-TOTAL-LINES EQUAL 0
009594         MOVE ER-0187            TO EMI-ERROR
009595         MOVE -1                 TO MAINTL
009596         MOVE AL-UABON           TO MAINTA
009597         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
009598         GO TO 8200-SEND-DATAONLY.
009599
009600     IF  EMI-ERROR NOT EQUAL ER-0191
009601         IF  PI-689-CREATE-NO-SCREENS
009602*                OR
009603*            (EIBAID = DFHPF4 OR DFHPF8)
009604             SET W-RG-NDX        TO +1
009605         ELSE
009606             PERFORM 4100-SET-NDX THRU 4100-EXIT
009607             PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
009608                     VARYING
009609                 W-SC-NDX FROM 1 BY 1
009610                     UNTIL
009611                 W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
009612
009613     MOVE SPACES                 TO W-REMAINING-VAR-SW.
009614
009615     IF EIBAID NOT = DFHPF4 AND DFHPF8
009616         PERFORM 7915-SEARCH-REMAINING-VARS THRU 7915-EXIT
009617                 VARYING
009618             W-RVS-NDX FROM +1 BY +1
009619                 UNTIL
009620             W-RVS-NDX GREATER THAN +36500
009621                 OR
009622             W-REMAINING-VAR-FOUND
009623         IF  W-REMAINING-VAR-FOUND
009624             GO TO 4000-ROLL-PAGE.
009625
009626*    IF EIBAID = DFHPF4 OR DFHPF8
009627*       PERFORM 7700-READ-TEXT-TS
009628*                                THRU 7700-EXIT
009629*
009630*        PERFORM 4100-SET-NDX THRU 4100-EXIT
009631*        PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
009632*                VARYING
009633*            W-SC-NDX FROM 1 BY 1
009634*                UNTIL
009635*            W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
009636*
009637*       PERFORM 7760-PUT-TEMP-STORAGE
009638*                                THRU 7760-EXIT
009639*    END-IF
009640
009641     IF PI-689-ALT-PRINTER-ID NOT = SPACES AND LOW-VALUES
009642         MOVE PI-689-ALT-PRINTER-ID
009643                                 TO W-TS-TERM-TEXT
009644     ELSE
009645         IF PI-PROCESSOR-PRINTER NOT = SPACES AND LOW-VALUES
009646             MOVE PI-PROCESSOR-PRINTER
009647                                 TO W-TS-TERM-TEXT
009648         ELSE
009649             MOVE PI-COMPANY-ID  TO W-CNTL-COMPANY-ID
009650             MOVE '1'            TO W-CNTL-RECORD-TYPE
009651             MOVE SPACES         TO W-CNTL-GENL
009652             MOVE ZEROS          TO W-CNTL-SEQ-NO
009653             
      * EXEC CICS HANDLE CONDITION
009654*                 NOTOPEN    (8040-CNTL-NOT-OPEN)
009655*                 NOTFND     (7945-NOT-FOUND)
009656*            END-EXEC
      *    MOVE '"$JI                  ! ; #00015803' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303135383033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
009657             
      * EXEC CICS READ
009658*                 DATASET    (W-CNTL-FILE-ID)
009659*                 SET        (ADDRESS OF CONTROL-FILE)
009660*                 RIDFLD     (W-CNTL-KEY)
009661*            END-EXEC
      *    MOVE '&"S        E          (   #00015807' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303135383037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
009662             MOVE CF-FORMS-PRINTER-ID
009663                                 TO W-TS-TERM-TEXT.
009664
009665***********************************************************
009666*      CHECK TO SEE IF IT IS A PRINT REQUEST FOR PRINTING *
009667*      LETTERS ON A 3275 PRINTER. IF SO, SAVE THE SCREEN  *
009668***********************************************************
009669
009670     SET W-TS-NDX                TO 1.
009671     MOVE EIBTIME                TO W-TS-ID-TIME.
009672     MOVE W-TS-NAME-TEXT         TO PI-689-TEMP-STOR-ID.
009673     MOVE 0                      TO PI-TEMP-STOR-ITEMS.
009674
009675     PERFORM 7840-WRITE-TS THRU 7840-EXIT
009676             VARYING
009677         W-TS-GROUP-WORK FROM 0 BY W-TS-NUM-REC-IN-GROUP
009678             UNTIL
009679         W-TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
009680
009681*    IF  EIBAID EQUAL DFHPF8
009682*        MOVE '1'                TO PI-689-PRINT-SW
009683*    ELSE
009684         MOVE SPACES             TO PI-689-PRINT-SW.
009685
009686     
      * EXEC CICS HANDLE CONDITION
009687*         TERMIDERR  (7935-TERM-ERROR)
009688*         TRANSIDERR (7940-TRAN-ERROR)
009689*    END-EXEC.
      *    MOVE '"$[\                  ! < #00015836' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303135383336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009690
009691     
      * EXEC CICS START
009692*             INTERVAL    (0)
009693*             TRANSID     (W-PRINT-TRANS)
009694*             FROM        (PROGRAM-INTERFACE-BLOCK)
009695*             LENGTH      (PI-COMM-LENGTH)
009696*             TERMID      (W-TS-TERM-TEXT)
009697*    END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                1   #00015841' TO DFHEIV0
           MOVE X'3028494C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303135383431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 W-PRINT-TRANS, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 W-TS-TERM-TEXT, 
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
           
009698
009699     
      * EXEC CICS SYNCPOINT
009700*    END-EXEC.
      *    MOVE '6"                    !   #00015849' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303135383439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009701
009702     IF  PI-689-CREATE-NO-SCREENS
009703         GO TO 7900-EXIT.
009704
009705*    IF  EIBAID EQUAL DFHPF8
009706*            AND
009707*        PI-SHOW-MODE
009708*            AND
009709*        LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES
009710*        PERFORM 7920-UPDATE-INITIAL-PRINT THRU 7920-EXIT.
009711
009712     MOVE ER-0189                TO EMI-ERROR.
009713     MOVE -1                     TO MAINTL.
009714
009715     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
009716     MOVE -1                     TO MAINTL
009717     MOVE AL-UABON               TO MAINTA
009718
009719     MOVE '104A'                 TO W-TS-ID-TEXT
009720     MOVE EIBTRMID               TO W-TS-TERM-TEXT
009721                                    W-TS-TERM-SCREEN
009722
009723     GO TO 8200-SEND-DATAONLY.
009724
009725 7900-EXIT.
009726     EXIT.
009727
009728 7915-SEARCH-REMAINING-VARS.
009729
009730     IF  W-REC-CHAR (W-RVS-NDX) EQUAL '@'
009731         SET W-RVS-NDX2          TO W-RVS-NDX
009732         SET W-RVS-NDX2 UP BY +1
009733         IF  W-REC-CHAR (W-RVS-NDX2) EQUAL '@'
009734             SET W-RVS-NDX2 UP BY +1
009735             IF  W-REC-CHAR (W-RVS-NDX2) NUMERIC
009736                 SET PI-CURRENT-LINE  TO W-RVS-NDX
009737                 COMPUTE PI-CURRENT-LINE
009738                     = PI-CURRENT-LINE / 73
009739                 MOVE 'Y'        TO W-REMAINING-VAR-SW
009740                 MOVE +0         TO W-ROLL-COUNTER
009741                 MOVE ER-0191    TO EMI-ERROR
009742                 MOVE -1         TO MAINTL
009743                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009744
009745 7915-EXIT.
009746     EXIT.
009747                                 EJECT
009748 7920-UPDATE-INITIAL-PRINT.
009749
009750     
      * EXEC CICS HANDLE CONDITION
009751*         NOTOPEN    (8010-ARCH-NOT-OPEN)
009752*         NOTFND     (1070-ARCH-NOT-FOUND)
009753*         ENDFILE    (1070-ARCH-NOT-FOUND)
009754*    END-EXEC.
      *    MOVE '"$JI''                 ! = #00015900' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303135393030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009755
009756     MOVE PI-COMPANY-CD TO W-ARCH-COMPANY-CD
009757
009758     MOVE ARCHNUMI               TO W-ARCH-NUMBER
009759
009760     
      * EXEC CICS READ
009761*        DATASET (W-ARCH-FILE-ID)
009762*        SET     (ADDRESS OF LETTER-ARCHIVE)
009763*        RIDFLD  (W-ARCH-PARTIAL-KEY)
009764*        UPDATE
009765*    END-EXEC.
      *    MOVE '&"S        EU         (   #00015910' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303135393130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-PARTIAL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009766
009767     MOVE W-SAVE-BIN-DATE        TO LA-INITIAL-PRINT-DATE.
009768
009769     IF  LA-RESEND-DATE = LOW-VALUES
009770         MOVE 'C'                TO LA-STATUS.
009771
009772     
      * EXEC CICS REWRITE
009773*         FROM      (LETTER-ARCHIVE)
009774*         DATASET   (W-ARCH-FILE-ID)
009775*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00015922' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303135393232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009776
009777 7920-EXIT.
009778     EXIT.
009779                                 EJECT
009780 7935-TERM-ERROR.
009781
009782     MOVE ER-0412                TO EMI-ERROR.
009783     MOVE SPACES                 TO PI-689-PRINT-SW.
009784     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009785     MOVE -1                     TO MAINTL.
009786     GO TO 8200-SEND-DATAONLY.
009787
009788 7940-TRAN-ERROR.
009789
009790     MOVE ER-0413                TO EMI-ERROR.
009791     MOVE SPACES                 TO PI-689-PRINT-SW.
009792     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009793     MOVE -1                     TO MAINTL.
009794     GO TO 8200-SEND-DATAONLY.
009795
009796 7945-NOT-FOUND.
009797
009798     MOVE ER-0190                TO EMI-ERROR.
009799     MOVE -1                     TO MAINTL.
009800     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009801     GO TO 8200-SEND-DATAONLY.
009802                                 EJECT
009803 8010-ARCH-NOT-OPEN.
009804
009805     MOVE ER-7388                TO EMI-ERROR.
009806     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009807     MOVE -1                     TO MAINTL.
009808     GO TO 8200-SEND-DATAONLY.
009809
009810 8015-ARCT-NOT-OPEN.
009811
009812     MOVE ER-7373                TO EMI-ERROR.
009813     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009814     MOVE -1                     TO MAINTL.
009815     GO TO 8200-SEND-DATAONLY.
009816
009817 8020-ACCT-NOT-OPEN.
009818
009819     MOVE ER-0168 TO EMI-ERROR
009820     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009821     MOVE -1                     TO MAINTL.
009822     GO TO 8200-SEND-DATAONLY.
009823
009824 8030-CERT-NOT-OPEN.
009825
009826     MOVE ER-0169 TO EMI-ERROR.
009827     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009828     MOVE -1                     TO MAINTL.
009829     GO TO 8200-SEND-DATAONLY.
009830
009831 8035-CERT-NOT-FOUND.
009832
009833     MOVE ER-2369 TO EMI-ERROR.
009834     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009835     MOVE -1                     TO MAINTL.
009836     GO TO 8200-SEND-DATAONLY.
009837
009838 8040-CNTL-NOT-OPEN.
009839
009840     MOVE ER-0042                TO EMI-ERROR.
009841     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009842     MOVE -1                     TO MAINTL.
009843     GO TO 8200-SEND-DATAONLY.
009844
009845 8045-CHEK-NOT-OPEN.
009846
009847     MOVE ER-3775                TO EMI-ERROR.
009848     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009849     MOVE -1                     TO MAINTL.
009850     GO TO 8200-SEND-DATAONLY.
009851
009852 8050-TEXT-NOT-OPEN.
009853
009854     MOVE ER-0013                TO EMI-ERROR.
009855     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009856     MOVE -1                     TO MAINTL.
009857     GO TO 8200-SEND-DATAONLY.
009858
009859 8060-PNDB-NOT-OPEN.
009860
009861     MOVE ER-2216 TO EMI-ERROR.
009862     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009863     MOVE -1                     TO MAINTL.
009864     GO TO 8200-SEND-DATAONLY.
009865
009866 8070-PNDB-NOT-FOUND.
009867
009868     MOVE ER-7374 TO EMI-ERROR.
009869     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009870     MOVE -1                     TO MAINTL.
009871     GO TO 8200-SEND-DATAONLY.
009872
009873 8080-COMP-NOT-OPEN.
009874
009875     MOVE ER-2055 TO EMI-ERROR
009876     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009877     MOVE -1                     TO MAINTL.
009878     GO TO 8200-SEND-DATAONLY.
009879
009880 8085-PYAJ-NOT-OPEN.
009881
009882     MOVE ER-2232 TO EMI-ERROR
009883     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009884     MOVE -1                     TO MAINTL.
009885     GO TO 8200-SEND-DATAONLY.
009886
009887 8090-MAIL-NOT-OPEN.
009888
009889     MOVE ER-2999                TO EMI-ERROR.
009890     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
009891     MOVE -1                     TO MAINTL.
009892     GO TO 8200-SEND-DATAONLY.
009893                                 EJECT
009894 8100-SEND-INITIAL-MAP.
009895
009896     PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT
009897
009898     IF  PI-689-CREATE-NO-SCREENS
009899         MOVE '9999'             TO PI-689-ERROR
009900         GO TO 0620-RETURN-TO-CALLER.
009901
009902     PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
009903
009904     IF  NOT EMI-NO-ERRORS
009905         MOVE EMI-MESSAGE-AREA (1)
009906                                 TO ERRMSGO
009907     ELSE
009908         MOVE SPACES             TO ERRMSGO.
009909
009910     MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
009911                                 TO SYSO
009912     MOVE WS-KIXHOST             TO HOSTO
009913
009914
009915     IF PI-PROCESSOR-ID = 'LMLC' OR 'SPJA' OR 'KAWA' OR 'CAGB'
009916        OR 'DLVA' OR 'ECCA' OR 'KRHA'
009917        MOVE AL-SADOF            TO PF8HDA
009918     END-IF
009919
009920     IF NOT PI-CREATE-LABELS
009921         MOVE AL-PANON           TO ADDRLBLA.
009922
009923     
      * EXEC CICS SEND
009924*        MAP    (W-MAP)
009925*        MAPSET (W-MAPSET)
009926*        FROM   (EL689AO)
009927*        ERASE
009928*        CURSOR
009929*    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00016073' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303136303733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009930
009931     GO TO 9000-RETURN-TRANS.
009932
009933 8100-EXIT.
009934     EXIT.
009935                                 EJECT
009936 8200-SEND-DATAONLY.
009937
009938     PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT.
009939     IF  PI-689-CREATE-NO-SCREENS
009940         MOVE '9999'             TO PI-689-ERROR
009941         GO TO 0620-RETURN-TO-CALLER.
009942
009943     PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
009944
009945     IF EMI-ERROR = ER-0715
009946         DISPLAY 'ER 0715'
009947         MOVE -1          TO CERTIDL
009948         MOVE +0          TO MAINTL
009949     END-IF
009950
009951     IF  NOT EMI-NO-ERRORS
009952         MOVE EMI-MESSAGE-AREA (1)
009953                                 TO ERRMSGO
009954     ELSE
009955         MOVE SPACES             TO ERRMSGO.
009956
009957     
      * EXEC CICS SEND
009958*        MAP    (W-MAP)
009959*        MAPSET (W-MAPSET)
009960*        FROM   (EL689AO)
009961*        DATAONLY
009962*        CURSOR
009963*    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00016107' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303136313037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
009964
009965     GO TO 9000-RETURN-TRANS.
009966
009967 8200-EXIT.
009968     EXIT.
009969                                 EJECT
009970 8300-SEND-TEXT.
009971
009972     IF  PI-689-CREATE-NO-SCREENS
009973         MOVE '9999'             TO PI-689-ERROR
009974         GO TO 0620-RETURN-TO-CALLER.
009975
009976     
      * EXEC CICS SEND TEXT
009977*        FROM    (LOGOFF-TEXT)
009978*        LENGTH  (LOGOFF-LENGTH)
009979*        ERASE
009980*        FREEKB
009981*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00016126' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303136313236' TO DFHEIV0
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
           
009982
009983     GO TO 9000-RETURN-TRANS.
009984
009985 8300-EXIT.
009986     EXIT.
009987                                 EJECT
009988 8350-SEND-DATAONLY-ERASEAUP.
009989
009990     MOVE EIBTRMID               TO W-TS-TERM-TEXT
009991     PERFORM 7760-PUT-TEMP-STORAGE
009992                                 THRU 7760-EXIT
009993     IF  PI-689-CREATE-NO-SCREENS
009994         MOVE '9999'             TO PI-689-ERROR
009995         GO TO 0620-RETURN-TO-CALLER.
009996
009997     PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
009998
009999     IF EMI-ERROR = ER-0715
010000         DISPLAY 'ER 0715'
010001         MOVE -1          TO CERTIDL
010002         MOVE +0          TO MAINTL
010003     END-IF
010004
010005     IF  NOT EMI-NO-ERRORS
010006         MOVE EMI-MESSAGE-AREA (1)
010007                                 TO ERRMSGO
010008     ELSE
010009         MOVE SPACES             TO ERRMSGO.
010010
010011     
      * EXEC CICS SEND
010012*        MAP      (W-MAP)
010013*        MAPSET   (W-MAPSET)
010014*        FROM     (EL689AO)
010015*        DATAONLY
010016*        ERASEAUP
010017*        CURSOR
010018*    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00016161' TO DFHEIV0
           MOVE X'382444202020204354202041' &
                X'2020202048204C2046202C20' &
                X'2020233030303136313631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010019
010020     GO TO 9000-RETURN-TRANS.
010021
010022 8350-EXIT.
010023     EXIT.
010024
010025 8600-DEEDIT.
010026
010027     
      * EXEC CICS BIF DEEDIT
010028*         FIELD    (W-DEEDIT-FIELD)
010029*         LENGTH   (15)
010030*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00016177' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303136313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010031
010032 8600-EXIT.
010033     EXIT.
010034                                 EJECT
010035 9000-RETURN-TRANS.
010036
010037     MOVE EMI-FATAL-CTR      TO PI-689-FATAL-CTR
010038     MOVE EMI-FORCABLE-CTR   TO PI-689-FORCABLE-CTR
010039
010040     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
010041     MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO
010042     
      * EXEC CICS RETURN
010043*        TRANSID    (W-TRANSACTION)
010044*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
010045*        LENGTH     (PI-COMM-LENGTH)
010046*    END-EXEC.
      *    MOVE '.(CT                  ''   #00016192' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303136313932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010047
010048*    MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
010049*    MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
010050*    GO TO 0200-RECEIVE.
010051
010052 9000-EXIT.
010053     EXIT.
010054                                 EJECT
010055 9200-PA.
010056
010057     MOVE ER-0008                TO EMI-ERROR.
010058     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
010059     MOVE -1                     TO MAINTL.
010060     GO TO 8200-SEND-DATAONLY.
010061
010062 9200-EXIT.
010063     EXIT.
010064                                 EJECT
010065 9300-DFHCLEAR.
010066
010067     IF PI-RETURN-TO-PROGRAM = 'EL689' OR 'EL690'
010068         MOVE '2'                TO PI-ACTION.
010069
010070     IF  PI-CLEAR-MODE
010071         MOVE PI-RETURN-TO-PROGRAM
010072                                 TO W-CALL-PGM
010073         PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
010074         GO TO 9400-XCTL
010075     ELSE
010076         PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
010077         MOVE LOW-VALUES         TO EL689AO
010078                                    PI-1042-WA
010079                                    PI-689-WORK-AREA
010080         MOVE ZEROS              TO PI-CURRENT-LINE
010081                                    PI-TEMP-STOR-ITEMS
010082                                    PI-TOTAL-LINES
010083                                    PI-UPDATE-SW
010084                                    PI-689-NUMBER-LABEL-LINES
010085         MOVE SPACES             TO PI-689-PRINT-SW
010086                                    PI-689-ALT-PRINTER-ID
010087                                    PI-COMM-CONTROL
010088                                    PI-689-FORM-NUMBER
010089                                    PI-689-TEMP-STOR-ID
010090                                    PI-689-LABEL-SOURCE
010091                                    PI-689-USE-SCREEN-IND
010092         MOVE '2'                TO PI-ACTION
010093         MOVE -1                 TO MAINTL
010094         GO TO 8100-SEND-INITIAL-MAP.
010095
010096 9300-EXIT.
010097     EXIT.
010098
010099 9400-XCTL.
010100
010101     
      * EXEC CICS XCTL
010102*        PROGRAM  (W-CALL-PGM)
010103*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
010104*        LENGTH   (PI-COMM-LENGTH)
010105*    END-EXEC.
      *    MOVE '.$C                   %   #00016251' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303136323531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010106
010107 9400-EXIT.
010108     EXIT.
010109                                 EJECT
010110 9500-LINK-DATE-CONVERT.
010111
010112     IF  DC-BIN-DATE-1 EQUAL HIGH-VALUES
010113             AND
010114         DC-OPTION-CODE EQUAL ' '
010115         MOVE '99/99/99'         TO DC-GREG-DATE-1-EDIT
010116         GO TO 9500-EXIT.
010117
010118     
      * EXEC CICS LINK
010119*        PROGRAM    ('ELDATCV')
010120*        COMMAREA   (DATE-CONVERSION-DATA)
010121*        LENGTH     (DC-COMM-LENGTH)
010122*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00016268' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303136323638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010123
010124     IF  NO-CONVERSION-ERROR
010125             AND
010126         PI-COMPANY-ID EQUAL 'AUK'
010127             AND
010128         W-REVERSE-DATE
010129         MOVE DC-GREG-DATE-1-EDIT
010130                                 TO W-EDIT-DATE-1
010131         MOVE W-ED1-MM           TO W-ED2-MM
010132         MOVE W-ED1-DD           TO W-ED2-DD
010133         MOVE W-ED1-YY           TO W-ED2-YY
010134         MOVE W-EDIT-DATE-2      TO DC-GREG-DATE-1-EDIT.
010135
010136 9500-EXIT.
010137     EXIT.
010138
010139 9600-FORMAT-DATE-TIME.
010140
010141     MOVE W-SAVE-DATE            TO RUNDTEO.
010142     MOVE EIBTIME                TO W-TIME-IN.
010143     MOVE W-TIME-OUT             TO RUNTIMEO.
010144     MOVE PI-COMPANY-ID          TO COMPANYO.
010145     MOVE PI-PROCESSOR-ID        TO USERIDO.
010146     MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
010147
010148 9600-EXIT.
010149     EXIT.
010150                                 EJECT
010151 9700-PGMID-ERROR.
010152
010153     IF  PI-689-CREATE-NO-SCREENS
010154         MOVE '9999'             TO PI-689-ERROR
010155         GO TO 0620-RETURN-TO-CALLER.
010156
010157     
      * EXEC CICS  HANDLE CONDITION
010158*        PGMIDERR  (8300-SEND-TEXT)
010159*    END-EXEC.
      *    MOVE '"$L                   ! > #00016307' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3E20233030303136333037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010160
010161     MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
010162     MOVE ' '                    TO PI-ENTRY-CD-1.
010163     MOVE W-XCTL-005             TO W-CALL-PGM
010164                                    LOGOFF-PGM.
010165     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
010166
010167     PERFORM 9400-XCTL THRU 9400-EXIT.
010168
010169 9700-EXIT.
010170     EXIT.
010171
010172 9800-ABEND.
010173
010174     IF  PI-689-CREATE-NO-SCREENS
010175         MOVE '9999'             TO PI-689-ERROR
010176         GO TO 0620-RETURN-TO-CALLER.
010177
010178     MOVE W-LINK-004             TO W-CALL-PGM.
010179     MOVE DFHEIBLK               TO EMI-LINE1
010180
010181     
      * EXEC CICS  LINK
010182*        PROGRAM   (W-CALL-PGM)
010183*        COMMAREA  (EMI-LINE1)
010184*        LENGTH    (72)
010185*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00016331' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303136333331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010186
010187     GO TO 8200-SEND-DATAONLY.
010188
010189 9800-EXIT.
010190     EXIT.
010191                                 EJECT
010192 9900-ERROR-FORMAT.
010193
010194     IF  PI-689-CREATE-NO-SCREENS
010195         MOVE EMI-ERROR          TO PI-689-ERROR
010196         IF  PI-689-FATAL-ERROR
010197             GO TO 0620-RETURN-TO-CALLER
010198         ELSE
010199             GO TO 9900-EXIT.
010200
010201     IF  EMI-ERROR EQUAL ER-9097
010202         NEXT SENTENCE
010203     ELSE
010204         IF  EMI-ERRORS-COMPLETE
010205                 OR
010206             EMI-ERROR EQUAL W-LAST-ERROR
010207             GO TO 9900-EXIT.
010208
010209     MOVE W-LINK-001             TO W-CALL-PGM.
010210
010211     
      * EXEC CICS LINK
010212*        PROGRAM    (W-CALL-PGM)
010213*        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
010214*        LENGTH     (EMI-COMM-LENGTH)
010215*    END-EXEC.
      *    MOVE '."C                   (   #00016361' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303136333631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010216
010217     IF  EMI-SEVERITY (2) EQUAL 'X'
010218             AND
010219         EMI-SEVERITY (1) NOT EQUAL 'X'
010220         MOVE EMI-MESSAGE-AREA (2)
010221                                 TO EMI-MESSAGE-AREA (1).
010222
010223     MOVE EMI-ERROR              TO W-LAST-ERROR.
010224
010225 9900-EXIT.
010226     EXIT.
010227
010228 9905-INITIALIZE-SECURITY.
010229******************************************************************
010230*                                                                *
010231*       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
010232*       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
010233*       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
010234*       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
010235*       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
010236*       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
010237*       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
010238*       ERROR CONDITION AND EXIT THE PROGRAM.                    *
010239*                                                                *
010240*       NOTE:  THE CARRIER/GRP/STATE/ACCOUNT SECURITY DATA       *
010241*       IS ALSO PROVIDED BY THIS LOGIC.                          *
010242*                                                                *
010243******************************************************************
010244
010245     IF  PI-PROCESSOR-ID EQUAL 'LGXX'
010246         MOVE 'Y'                TO PI-DISPLAY-CAP
010247                                    PI-MODIFY-CAP
010248     ELSE
010249         
      * EXEC CICS READQ TS
010250*            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
010251*            INTO   (SECURITY-CONTROL)
010252*            LENGTH (SC-COMM-LENGTH)
010253*            ITEM   (1)
010254*        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00016399' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303136333939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
010255         MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
010256                                 TO PI-DISPLAY-CAP
010257         MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
010258                                 TO PI-MODIFY-CAP.
010259
010260 9905-EXIT.
010261                                 EJECT
010262 9995-SECURITY-VIOLATION.
010263
010264     MOVE EIBDATE                TO SM-JUL-DATE.
010265     MOVE EIBTRMID               TO SM-TERMID.
010266     MOVE W-THIS-PGM             TO SM-PGM.
010267     MOVE EIBTIME                TO W-TIME-IN.
010268     MOVE W-TIME-OUT             TO SM-TIME.
010269     MOVE PI-PROCESSOR-ID        TO SM-PROCESSOR-ID.
010270
010271     
      * EXEC CICS LINK
010272*         PROGRAM  ('EL003')
010273*         COMMAREA (SECURITY-MESSAGE)
010274*         LENGTH   (80)
010275*    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00016421' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303136343231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
010276
010277 9995-EXIT.
010278     EXIT.
010279
010280 9999-GOBACK.
010281
010282     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
010283
010284 9999-EXIT.
010285     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9300-DFHCLEAR,
                     9200-PA,
                     9200-PA,
                     9200-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9700-PGMID-ERROR,
                     9800-ABEND,
                     0325-MAPFAIL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8040-CNTL-NOT-OPEN,
                     7945-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9700-PGMID-ERROR,
                     0325-MAPFAIL,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0990-CARRIER-NOT-FOUND,
                     8040-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0994-STATE-NOT-FOUND,
                     8040-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2001-NOT-FOUND,
                     2001-NOT-FOUND,
                     8050-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2000-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8040-CNTL-NOT-OPEN,
                     6200-CNTL1-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8040-CNTL-NOT-OPEN,
                     6250-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8040-CNTL-NOT-OPEN,
                     6300-CNTL6-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8040-CNTL-NOT-OPEN,
                     6350-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8060-PNDB-NOT-OPEN,
                     8070-PNDB-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8030-CERT-NOT-OPEN,
                     8035-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8020-ACCT-NOT-OPEN,
                     6500-ACCT-NOT-FOUND,
                     6500-ONLY-STOP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 8080-COMP-NOT-OPEN,
                     6600-COMP-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8040-CNTL-NOT-OPEN,
                     6630-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8090-MAIL-NOT-OPEN,
                     6700-MAIL-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8040-CNTL-NOT-OPEN,
                     6750-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 8040-CNTL-NOT-OPEN,
                     6800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8045-CHEK-NOT-OPEN,
                     6850-CHEK-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 8085-PYAJ-NOT-OPEN,
                     6900-PYAJ-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7700-TS-ERROR,
                     7700-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7780-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 8040-CNTL-NOT-OPEN,
                     7945-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7935-TERM-ERROR,
                     7940-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
