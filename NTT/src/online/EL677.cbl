      *((program: EL677.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 ID DIVISION.
000003
000004 PROGRAM-ID. EL677.
000005*
000006*AUTHOR.     CSO
000007*            OMAHA, NEBRASKA.
000008
000009*DATE-COMPILED.
000010
000011*SECURITY.   *****************************************************
000012*            *                                                   *
000013*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
000014*            *                                                   *
000015*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000016*                                                                *
000017*            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
000018*            *   THE PRIOR WRITTEN PERMISSION OF CSO             *
000019*            *                                                   *
000020*            *****************************************************
000021
000022*REMARKS.    TRANSACTION - EXF3 - CHECK MAINTENANCE
000023
000024******************************************************************
000025*                   C H A N G E   L O G
000026*
000027* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000028*-----------------------------------------------------------------
000029*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000030* EFFECTIVE    NUMBER
000031*-----------------------------------------------------------------
000032* 111513  CR2013053000001  PEMA  DAILY CHECK REQUEST CHANGES
000033* 021714  CR2014021700001  PEMA  ADD TEST DB FOR OTHER THAN cid1p
000034* 030414  IR2014030400001  PEMA  CHG PYAJ VOID TRANS TO R AND +
000035* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
000036* 091615  CR2015082000001  PEMA  ADD TOTAL ENDT PROCESSING
000037* 020317  IR2016110900001  PEMA  FIXED MISC PROBLEMS
000038* 060717  CR2017032900002  PEMA  CHANGE TEST DB
000039* 111418  CR2018103100001  PEMA  ADD REISSUE CAPABILITY
000040* 040919  CR2019040900001  PEMA  FIX RETURN TO ON REISSUES
000041* 111219  CR2019110700001  PEMA  ALLOW REVERSAL OF VOID ON SAME DA
000042* 030921  CR2019012500003  PEMA  Connect to sdv-db01.cso.local
000043* 011822  CR2019012500003  PEMA  Convert to SQLSERVER 2016
000044* 070622  CR2020061200002  TANA  Add payee code
000045******************************************************************
000046
000047 ENVIRONMENT DIVISION.
000048
000049 DATA DIVISION.
000050 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000051
000052 77  FILLER  PIC X(32)  VALUE '********************************'.
000053 77  FILLER  PIC X(32)  VALUE '*    EL677 WORKING STORAGE     *'.
000054 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
000055
000056 77  s1                          pic s999 comp-3 value +0.
000057 77  ws-tot-lf-prem              pic s9(7)v99 comp-3 value +0.
000058 77  ws-tot-iss-prem             pic s9(7)v99 comp-3 value +0.
000059 77  ws-tot-iss-comm             pic s9(7)v99 comp-3 value +0.
000060 77  ws-tot-ref-comm             pic s9(7)v99 comp-3 value +0.
000061 77  ws-pyaj-browse-sw           pic x value spaces.
000062     88  pyaj-browse-started        value 'Y'.
000063 77  ws-browse-sw                pic x value spaces.
000064     88  i-say-when                 value 'Y'.
000065 77  ws-delete-sw                pic x value ' '.
000066     88  row-deleted                 value 'Y'.
000067 77  ws-sql-code                 pic s9(7) value zeros.
000068 77  ws-dis-sql-code             pic -9999999 value zeros.
000069 77  ws-match-sw                 pic x  value ' '.
000070     88  found-a-match             value 'Y'.
000071 77  ws-commit-sw                pic x value ' '.
000072     88  tbl-commited                value 'Y'.
000073
000074 01  P pointer.
000075 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000076 01  var-ptr pointer.
000077 01  env-var-len                 pic 9(4)  binary.
000078
000079 01  WS-KIXSYS.
000080     05  WS-KIX-FIL1             PIC X(10).
000081     05  WS-KIX-APPS             PIC X(10).
000082     05  WS-KIX-ENV              PIC X(10).
000083     05  WS-KIX-MYENV            PIC X(10).
000084     05  WS-KIX-SYS              PIC X(10).
000085
000086*EXEC SQL
000087*   INCLUDE SQLDA
000088*END-EXEC
000089
000092*EXEC SQL
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
000093
000095 EXEC SQL
          BEGIN DECLARE SECTION
000096 END-EXEC
000097
000098 01  ws-paid-bank-work-area.
000099     05  ws-pb-compid            pic x(9).
000100     05  ws-pb-check-no          pic x(10).
000101     05  ws-pb-bad-check-no      pic x(10).
000102     05  ws-check-amount         pic x(10).
000103
000104 01  Paid-Bank-Info.
000105     05  pb-check-no             pic 9(10).
000106     05  pb-tran-type            pic x.
000107     05  pb-bank-acct-desc       pic x(50).
000108     05  pb-amount               pic x(12).
000109     05  pb-paid-date            pic x(25).
000110
000111 01  ws-key-stuff.
000112     05  ws-compid               pic xxx.
000113     05  ws-carrier              pic x.
000114     05  ws-grouping             pic x(6).
000115     05  ws-state                pic xx.
000116     05  ws-account              pic x(10).
000117     05  ws-eff-date             pic x(10).
000118     05  ws-certificate          pic x(10).
000119     05  ws-cert-sfx             pic x.
000120     05  ws-seq-no               pic 999.
000121     05  ws-type                 pic x.
000122     05  ws-check-sub-type       pic x.
000123
000124 01  svr                         pic x(32).
000125 01  usr                         pic x(32).
000126 01  pass                        pic x(32).
000127 01  usr-pass                    pic x(64).
000128 01  ws-disp-code                pic s9(11).
000129
000130***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000131***                                                            ***
000132***  These indicators are used to determine if a variable      ***
000133***  is passed nulls from sql. The indicator will be -1        ***
000134***  if the value on sql is nulls and +0 if the value is       ***
000135***  something other than nulls. Here is an example on how     ***
000136***  to use the indicator variables.                           ***
000137***                                                            ***
000138***     EXEC SQL                                               ***
000139***        fetch checkapp into                                 ***
000140***           :db-app-status :nu-app-status,                   ***
000141***           :db-app-by     :nu-app-by,                       ***
000142***           :db-app-date   :nu-app-date,                     ***
000143***           :db-app-batch  :nu-app-batch                     ***
000144***     END-EXEC                                               ***
000145***                                                            ***
000146***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000147
000148 01  indicator-vaiables-for-nulls.
000149     05  nu-app-status           pic s9(4) comp value +0.
000150     05  nu-app-by               pic s9(4) comp value +0.
000151     05  nu-app-date             pic s9(4) comp value +0.
000152     05  nu-app-batch            pic s9(4) comp value +0.
000153
000154 01  daily-check-request-rec.
000155     05  db-compid               pic xxx.
000156     05  db-carrier              pic x.
000157     05  db-grouping             pic x(6).
000158     05  db-state                pic xx.
000159     05  db-account              pic x(10).
000160     05  db-effdate              pic x(10).
000161     05  db-certificate          pic x(10).
000162     05  db-cert-sfx             pic x.
000163     05  db-seq-no               pic 999.
000164     05  db-type                 pic x.
000165     05  db-amount-n             pic 9(7).99.
000166     05  db-amount redefines db-amount-n
000167                                 pic x(10).
000168     05  db-checkno              pic x(15).
000169     05  db-checkdate            pic x(10).
000170     05  db-checkstatus          pic 9(5).
000171     05  db-releasebatch         pic 9(5).
000172     05  db-releasedt            pic x(10).
000173     05  db-releaseby            pic x(4).
000174     05  db-payeename1           pic x(30).
000175     05  db-payeename2           pic x(30).
000176     05  db-payeeaddr1           pic x(30).
000177     05  db-payeeaddr2           pic x(30).
000178     05  db-payeecity            pic x(30).
000179     05  db-payeest              pic xx.
000180     05  db-payeezip             pic x(10).
000181     05  db-fincar               pic x.
000182     05  db-fingrp               pic x(6).
000183     05  db-finresp              pic x(10).
000184     05  db-finacct              pic x(10).
000185     05  db-preparer             pic x(4).
000186     05  db-app-status           pic x(9).
000187     05  dp-app-status-n redefines db-app-status
000188                                 pic 9(9).
000189     05  db-app-by               pic x(20).
000190     05  db-app-date             pic x(30).
000191     05  db-app-batch            pic x(10).
000192     05  db-return-to            pic x(30).
000193     05  db-insured-name         pic x(30).
000194     05  db-check-sub-type       pic x.
000195     05  db-payeecode            pic x(10).
000196
000197
000199 EXEC SQL
          END DECLARE SECTION
000200 END-EXEC
000201
000202*                            COPY ELCSCTM.
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
000203*                            COPY ELCSCRTY.
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
000204 01  ws-dummy-erchek             pic x(600).
000205 01  filler.
000206     05  WS-CHECK-AMT-TMP        PIC Z(7).99.
000207     05  WS-CHECK-AMT-TMPX REDEFINES
000208         WS-CHECK-AMT-TMP        PIC X(10).
000209 01  ws-compare-erchek-key          pic x(33) value low-values.
000210 01  ws-eracct-sw                   pic x value ' '.
000211     88  eracct-found                 value 'Y'.
000212 01  ws-eracct-start-sw             pic x value ' '.
000213     88  eracct-start                 value 'Y'.
000214 01  WS-MISC-AREA.
000215     05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000216     05  SAVE-CURRENT-DATE-MDY       PIC X(6)    VALUE SPACES.
000217     05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000218
000219 01  ws-user-defined.
000220     05  ws-user-reason         pic xxx.
000221     05  ws-user-cert-no        pic x(11).
000222     05  ws-user-name           pic x(28).
000223
000224 01  ws-hold-eracct-record       pic x(2000)  value spaces.
000225 01  STANDARD-AREAS.
000226     12  ws-connect-sw               pic x  value ' '.
000227         88  connected-to-db             value 'Y'.
000228     12  SC-ITEM                     PIC S9(4) COMP VALUE +1.
000229     12  GETMAIN-SPACE               PIC X       VALUE SPACE.
000230     12  EL677A                      PIC X(8)    VALUE 'EL677A'.
000231     12  MAPSET-NAME                 PIC X(8)    VALUE 'EL677S'.
000232     12  SCREEN-NUMBER               PIC X(4)    VALUE '677A'.
000233     12  TRANS-ID                    PIC X(4)    VALUE 'EXF3'.
000234     12  EL6311-TRANS-ID             PIC X(4)    VALUE 'EXB1'.
000235     12  EL6318-TRANS-ID             PIC X(4)    VALUE 'EXBE'.
000236     12  THIS-PGM                    PIC X(8)    VALUE 'EL677'.
000237     12  PGM-NAME                    PIC X(8)   VALUE SPACES.
000238     12  TIME-IN                     PIC S9(7)  VALUE ZEROS.
000239     12  TIME-OUT-R  REDEFINES TIME-IN.
000240         16  FILLER                  PIC X.
000241         16  TIME-OUT                PIC 99V99.
000242         16  FILLER                  PIC XX.
000243     12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
000244     12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
000245     12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
000246     12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.
000247     12  XCTL-114                    PIC X(8)    VALUE 'EL114'.
000248     12  LINK-001                    PIC X(8)    VALUE 'EL001'.
000249     12  LINK-004                    PIC X(8)    VALUE 'EL004'.
000250     12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
000251     12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
000252     12  ERACCT-FILE-ID              PIC X(8)    VALUE 'ERACCT'.
000253     12  ERCOMP-FILE-ID              PIC X(8)    VALUE 'ERCOMP'.
000254     12  ERCHEK-FILE-ID              PIC X(8)    VALUE 'ERCHEK'.
000255     12  ERPNDB-FILE-ID              PIC X(8)    VALUE 'ERPNDB'.
000256     12  ERPNDB-ALT-FILE-ID          PIC X(8)    VALUE 'ERPNDB2'.
000257     12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.
000258     12  ERMAIL-FILE-ID              PIC X(8)    VALUE 'ERMAIL'.
000259     12  ERPYAJ-FILE-ID              PIC X(8)    VALUE 'ERPYAJ'.
000260     12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +0    COMP.
000261     12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
000262     12  QID.
000263         16  QID-TERM                PIC X(4)    VALUE SPACES.
000264         16  FILLER                  PIC X(4)    VALUE '677A'.
000265
000266 01  WORK-AREAS.
000267     12  WS-AM-WORK                  PIC S9(8)V99 VALUE +0.
000268     12  WS-AM-WK REDEFINES WS-AM-WORK.
000269         16  WS-AM-NUM               PIC S9(7)V99.
000270         16  WS-AM-SIGN              PIC X.
000271     12  WS-CHECK-WORK.
000272         16  FILLER                  PIC X        VALUE SPACE.
000273         16  WS-CHECK-NO             PIC X(6)     VALUE SPACES.
000274     12  WS-CHK-PRINT-DT             PIC XX       VALUE LOW-VALUE.
000275     12  WS-SV-AMOUNT-PAID           PIC S9(7)V99 VALUE +0.
000276     12  WS-AMT                      PIC S9(7)V99 VALUE +0.
000277     12  WS-REFUND-AMOUNT            PIC S9(7)V99 VALUE +0.
000278     12  WS-DEEDIT-FIELD             PIC S9(9)V99 VALUE +0.
000279     12  WS-DT-DEEDIT-FIELD          PIC X(10)   VALUE ZERO.
000280     12  WS-DEEDIT-FIELD-DATE        REDEFINES
000281         WS-DT-DEEDIT-FIELD.
000282         16  FILLER                  PIC X(4).
000283         16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
000284     12  WS-DEEDIT-FIELD-A           PIC X(15)   VALUE ZERO.
000285     12  WS-DEEDIT-FIELD-V0          REDEFINES
000286         WS-DEEDIT-FIELD-A           PIC S9(15).
000287
000288     12  WS-SUB                      PIC S9(4)   VALUE ZERO  COMP.
000289     12  WS-CERT-FOUND-SW            PIC X       VALUE 'N'.
000290         88 WS-CERT-FOUND                        VALUE 'Y'.
000291         88 WS-CERT-NOT-FOUND                    VALUE 'N'.
000292     12  WS-PNDB-FOUND-SW            PIC X       VALUE 'N'.
000293         88 WS-PNDB-FOUND                        VALUE 'Y'.
000294         88 WS-PNDB-NOT-FOUND                    VALUE 'N'.
000295     12  WS-PROCESS-CERT-SW          PIC X       VALUE SPACE.
000296         88  WS-PROCESS-CERT                     VALUE 'Y'.
000297     12  WS-PROCESS-BENEFICIARY-SW   PIC X       VALUE SPACE.
000298         88  WS-PROCESS-BENEFICIARY              VALUE 'Y'.
000299     12  WS-TMS-ENTRY-COMMENT.
000300         16  WS-TMS-PY-CERT          PIC X(10)   VALUE SPACES.
000301         16  FILLER                  PIC XX      VALUE SPACES.
000302         16  WS-TMS-PY-PAYEE         PIC X(18)   VALUE SPACES.
000303     12  WS-ZIP-CODE.
000304         16  WS-ZIP-PRIME.
000305             20  WS-ZIP-PRI-1ST      PIC X.
000306                 88  WS-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000307             20  FILLER              PIC X(4).
000308         16  WS-ZIP-PLUS4            PIC X(4).
000309     12  WS-CANADIAN-POST-CODE1  REDEFINES  WS-ZIP-CODE.
000310         16  WS-CAN-POST1-1          PIC XXX.
000311         16  WS-CAN-POST1-2.
000312             20  WS-CAN-POST-4TH     PIC X.
000313             20  FILLER              PIC XX.
000314         16  FILLER                  PIC XXX.
000315     12  WS-CANADIAN-POST-CODE2  REDEFINES  WS-ZIP-CODE.
000316         16  WS-CAN-POST2-1          PIC XXX.
000317         16  FILLER                  PIC X.
000318         16  WS-CAN-POST2-2          PIC XXX.
000319         16  FILLER                  PIC XX.
000320
000321*                                    COPY ELCNWA.
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
000322
000323 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000324     88  RESP-NORMAL                    VALUE +0.
000325     88  resp-file-notfnd               value +12.
000326     88  RESP-NOTFND                    VALUE +13.
000327     88  resp-duprec                    value +14.
000328     88  resp-dupkey                    value +15.
000329     88  resp-invreq                    value +16.
000330     88  RESP-NOTOPEN                   VALUE +19.
000331     88  RESP-ENDFILE                   VALUE +20.
000332     88  resp-lengtherr                 value +22.
000333
000334 01  ACCESS-KEYS.
000335     12  ELCNTL-KEY.
000336         16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.
000337         16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.
000338         16 ELCNTL-ACCESS.
000339             20 ELCNTL-STATE         PIC XX      VALUE SPACES.
000340             20  FILLER              PIC X       VALUE SPACES.
000341             20 ELCNTL-CARRIER       PIC X       VALUE SPACES.
000342         16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.
000343
000344     12  ERACCT-KEY.
000345         16  ERACCT-CO               PIC X       VALUE SPACES.
000346         16  ERACCT-CARRIER          PIC X       VALUE SPACES.
000347         16  ERACCT-GROUPING         PIC X(6)    VALUE SPACES.
000348         16  ERACCT-STATE            PIC XX      VALUE SPACES.
000349         16  ERACCT-ACCOUNT          PIC X(10)   VALUE SPACES.
000350         16  ERACCT-EXP-DATE         PIC XX      VALUE SPACES.
000351         16  ERACCT-EXP-DATE-FILLER  PIC X(4)    VALUE SPACES.
000352
000353     12  SAVE-ERACCT-KEY.
000354         16  SV-ACCT-CO              PIC X       VALUE SPACES.
000355         16  SV-ACCT-CARRIER         PIC X       VALUE SPACES.
000356         16  SV-ACCT-GROUPING        PIC X(6)    VALUE SPACES.
000357         16  SV-ACCT-STATE           PIC XX      VALUE SPACES.
000358         16  SV-ACCT-ACCOUNT         PIC X(10)   VALUE SPACES.
000359         16  SV-ACCT-EXP-DATE        PIC XX      VALUE SPACES.
000360         16  SV-ACCT-EXP-DATE-FILLER PIC X(4)    VALUE SPACES.
000361
000362     12  ERCOMP-KEY.
000363         16  ERCOMP-COMP-CD          PIC X       VALUE SPACE.
000364         16  ERCOMP-CARRIER          PIC X       VALUE SPACES.
000365         16  ERCOMP-GROUPING         PIC X(6)    VALUE SPACES.
000366         16  ERCOMP-FIN-RESP         PIC X(10)   VALUE SPACES.
000367         16  ERCOMP-ACCOUNT          PIC X(10)   VALUE SPACES.
000368         16  ERCOMP-RECORD-TYPE      PIC X       VALUE SPACES.
000369
000370     12  ELCERT-KEY.
000371         16 ELCERT-COMPANY-CD        PIC X       VALUE SPACES.
000372         16 ELCERT-CARRIER           PIC X       VALUE SPACES.
000373         16 ELCERT-GROUPING          PIC X(6)    VALUE SPACES.
000374         16 ELCERT-STATE             PIC XX      VALUE SPACES.
000375         16 ELCERT-ACCOUNT           PIC X(10)   VALUE SPACES.
000376         16 ELCERT-CERT-EFF-DT       PIC XX      VALUE SPACES.
000377         16 ELCERT-CERT-PRIME        PIC X(10)   VALUE SPACES.
000378         16 ELCERT-CERT-SFX          PIC X       VALUE SPACES.
000379
000380     12  ERPNDB-PRIMARY-KEY.
000381         16 ERPNDB-COMPANY-CD        PIC X       VALUE SPACES.
000382         16 ERPNDB-ENTRY-BATCH       PIC X(6)    VALUE SPACES.
000383         16 ERPNDB-BATCH-SEQ-NO      PIC S9(4)   VALUE +0    COMP.
000384         16 ERPNDB-BATCH-CHG-SEQ-NO  PIC S9(4)   VALUE +0    COMP.
000385
000386     12  ERPNDB-ALT-KEY.
000387         16 ERPNDB-ALT-COMPANY-CD    PIC X       VALUE SPACES.
000388         16 ERPNDB-ALT-CARRIER       PIC X       VALUE SPACES.
000389         16 ERPNDB-ALT-GROUPING      PIC X(6)    VALUE SPACES.
000390         16 ERPNDB-ALT-STATE         PIC XX      VALUE SPACES.
000391         16 ERPNDB-ALT-ACCOUNT       PIC X(10)   VALUE SPACES.
000392         16 ERPNDB-ALT-CERT-EFF-DT   PIC XX      VALUE SPACES.
000393         16 ERPNDB-ALT-CERT-PRIME    PIC X(10)   VALUE SPACES.
000394         16 ERPNDB-ALT-CERT-SFX      PIC X       VALUE SPACES.
000395         16 ERPNDB-ALT-CH-SEQ-NO     PIC S9(4)   VALUE +0    COMP.
000396         16 ERPNDB-ALT-RECORD-TYPE   PIC X       VALUE SPACE.
000397
000398     12  ERCHEK-KEY.
000399         16  CHEK-COMPANY-CD         PIC X       VALUE SPACE.
000400         16  CHEK-CARRIER            PIC X       VALUE SPACE.
000401         16  CHEK-GROUPING           PIC X(6)    VALUE SPACES.
000402         16  CHEK-STATE              PIC XX      VALUE SPACES.
000403         16  CHEK-ACCOUNT            PIC X(10)   VALUE SPACES.
000404         16  CHEK-EFF-DT             PIC XX      VALUE SPACES.
000405         16  CHEK-CERT-NO            PIC X(10)   VALUE SPACES.
000406         16  CHEK-SUF-NO             PIC X       VALUE SPACES.
000407         16  CHEK-RECORD-SEQ         PIC S9(4)   VALUE ZEROS COMP.
000408
000409     12  ERCHEK-RECORD-LENGTH        PIC S9(4)   VALUE +600  COMP.
000410
000411     12  ERPYAJ-KEY.
000412         16 PYAJ-COMPANY-CD          PIC X       VALUE LOW-VALUES.
000413         16 PYAJ-CARRIER             PIC X       VALUE SPACE.
000414         16 PYAJ-GROUPING            PIC X(06)   VALUE SPACES.
000415         16 PYAJ-FIN-RESP            PIC X(10)   VALUE SPACES.
000416         16 PYAJ-ACCOUNT             PIC X(10)   VALUE SPACES.
000417         16 PYAJ-FILE-SEQ-NO         PIC S9(8)   VALUE +0  COMP.
000418         16 PYAJ-RECORD-TYPE         PIC X       VALUE SPACE.
000419
000420 01  ERROR-NUMBERS.
000421     12  ER-0000                 PIC X(4)    VALUE '0000'.
000422     12  ER-0004                 PIC X(4)    VALUE '0004'.
000423     12  ER-0013                 PIC X(4)    VALUE '0013'.
000424     12  ER-0022                 PIC X(4)    VALUE '0022'.
000425     12  ER-0023                 PIC X(4)    VALUE '0023'.
000426     12  ER-0029                 PIC X(4)    VALUE '0029'.
000427     12  ER-0070                 PIC X(4)    VALUE '0070'.
000428     12  ER-0194                 PIC X(4)    VALUE '0194'.
000429     12  ER-0195                 PIC X(4)    VALUE '0195'.
000430     12  ER-0196                 PIC X(4)    VALUE '0196'.
000431     12  ER-0197                 PIC X(4)    VALUE '0197'.
000432     12  ER-0203                 PIC X(4)    VALUE '0203'.
000433     12  ER-0215                 PIC X(4)    VALUE '0215'.
000434     12  ER-0216                 PIC X(4)    VALUE '0216'.
000435     12  ER-0433                 PIC X(4)    VALUE '0433'.
000436     12  ER-1159                 PIC X(4)    VALUE '1159'.
000437     12  ER-1162                 PIC X(4)    VALUE '1162'.
000438     12  ER-2056                 PIC X(4)    VALUE '2056'.
000439     12  ER-2208                 PIC X(4)    VALUE '2208'.
000440     12  ER-2209                 PIC X(4)    VALUE '2209'.
000441     12  ER-2210                 PIC X(4)    VALUE '2210'.
000442     12  ER-2230                 PIC X(4)    VALUE '2230'.
000443     12  ER-2237                 PIC X(4)    VALUE '2237'.
000444     12  ER-2238                 PIC X(4)    VALUE '2238'.
000445     12  ER-2394                 PIC X(4)    VALUE '2394'.
000446     12  ER-2583                 PIC X(4)    VALUE '2583'.
000447     12  ER-2600                 PIC X(4)    VALUE '2600'.
000448     12  ER-2726                 PIC X(4)    VALUE '2726'.
000449     12  ER-2907                 PIC X(4)    VALUE '2907'.
000450     12  ER-2908                 PIC X(4)    VALUE '2908'.
000451     12  ER-3044                 PIC X(4)    VALUE '3044'.
000452     12  ER-3046                 PIC X(4)    VALUE '3046'.
000453     12  er-3274                 pic x(4)    value '3274'.
000454     12  er-3450                 pic x(4)    value '3450'.
000455     12  er-3451                 pic x(4)    value '3451'.
000456     12  er-3452                 pic x(4)    value '3452'.
000457     12  er-3453                 pic x(4)    value '3453'.
000458     12  er-3454                 pic x(4)    value '3454'.
000459     12  er-3455                 pic x(4)    value '3455'.
000460     12  er-3459                 pic x(4)    value '3459'.
000461     12  er-3460                 pic x(4)    value '3460'.
000462     12  er-3461                 pic x(4)    value '3461'.
000463     12  er-3462                 pic x(4)    value '3462'.
000464     12  er-3463                 pic x(4)    value '3463'.
000465     12  er-9999                 pic x(4)    value '9999'.
000466
000467*                                COPY ELCLOGOF.
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
000468*                                COPY ELCDATE.
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
000469*                                COPY ELCATTR.
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
000470*                                COPY ELCEMIB.
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
000471*                                COPY ELCINTF.
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
000472     12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
000473         16  PI-EOF-SW               PIC X.
000474             88  PI-FILE-EOF                     VALUE 'Y'.
000475         16  PI-PREV-MAINT           PIC X.
000476         16  PI-WORK-SEQ-NO          PIC S9(9)   COMP-3.
000477         16  PI-ERCHEK-KEY.
000478             20  PI-CHEK-COMP-CD     PIC X.
000479             20  PI-CHEK-CARRIER     PIC X.
000480             20  PI-CHEK-GROUPING    PIC X(6).
000481             20  PI-CHEK-STATE       PIC XX.
000482             20  PI-CHEK-ACCOUNT     PIC X(10).
000483             20  PI-CHEK-EFF-DT      PIC XX.
000484             20  PI-CHEK-CERT-NO     PIC X(10).
000485             20  PI-CHEK-SUF-NO      PIC X.
000486             20  PI-CHEK-SEQUENCE    PIC S9(4)  COMP.
000487         16  PI-ACCOUNT-ADDRESS.
000488             20  PI-AM-NAME          PIC X(30).
000489             20  PI-AM-ADDRS         PIC X(30).
000490             20  PI-AM-CITY-st.
000491                 24  pi-am-city      PIC X(28).
000492                 24  pi-am-st        pic xx.
000493             20  PI-AM-ZIP-CODE.
000494                 24  PI-AM-ZIP-PRIME PIC X(5).
000495                 24  PI-AM-ZIP-PLUS4 PIC X(4).
000496         16  PI-PROCESS-CERT-SW          PIC X.
000497             88  PI-PROCESS-CERT                     VALUE 'Y'.
000498         16  PI-PROCESS-BENEFICIARY-SW   PIC X.
000499             88  PI-PROCESS-BENEFICIARY              VALUE 'Y'.
000500         16  PI-PAYTO1                   PIC X(30).
000501         16  PI-REFERENCE                PIC X(12).
000502         16  PI-CANC-DT                  PIC XX.
000503         16  PI-LF-REFUND                PIC S9(7)V99  COMP-3.
000504         16  PI-AH-REFUND                PIC S9(7)V99  COMP-3.
000505         16  PI-INSURED-NAME             PIC X(28).
000506         16  PI-PFKEY                    PIC XXX.
000507             88  PI-TO-EL1273-FROM-EL677        VALUE 'PF3'.
000508             88  PI-TO-EL677-FROM-EL1273        VALUE 'PF8'.
000509         16  PI-AMOUNT                   PIC S9(9)V99  COMP-3.
000510         16  PI-TYPE                     PIC X.
000511         16  pi-prev-paid                pic s9(7)v99 comp-3.
000512         16  pi-refund-on-pending-rec    pic s9(7)v99 comp-3.
000513         16  pi-ue-comm                  pic s9(7)v99 comp-3.
000514         16  pi-chek-rec-cnt             pic s999 comp-3.
000515         16  pi-am-csr                   pic x(4).
000516         16  pi-return-to                pic x(30).
000517         16  pi-prev-ded-comm            pic x.
000518         16  pi-table-name               pic x(30).
000519         16  pi-check-cashed             pic x.
000520         16  pi-endt-prm-diff            pic s9(7)v99 comp-3.
000521         16  pi-endt-com-diff            pic s9(5)v99 comp-3.
000522         16  pi-check-type               pic x.
000523             88  pi-corr-check              value 'C'.
000524             88  pi-ref-check               value 'R'.
000525         16  pi-check-cut                pic x.
000526         16  pi-prev-paid-this-month     pic s9(7)v99 comp-3.
000527         16  pi-previous-deduct-comm     pic x.
000528         16  pi-void-reissue-pass        pic x.
000529             88  pi-void-complete           value '1'.
000530             88  pi-address-selected        value '2'.
000531         16  pi-void-reissue-amt         pic s9(7)v99 comp-3.
000532         16  FILLER                      PIC X(299). *> wass 304
000533
000534*                                    COPY ELCAID.
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
000535 01  FILLER    REDEFINES DFHAID.
000536     12  FILLER                      PIC X(8).
000537     12  PF-VALUES                   PIC X       OCCURS 24.
000538     12  FILLER                      PIC X(3).
000539
000540*                                    COPY EL677S.
      *>>((file: EL677S))
000001 01  EL677AI.
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
000016     05  MAINTL PIC S9(0004) COMP.
000017     05  MAINTF PIC  X(0001).
000018     05  FILLER REDEFINES MAINTF.
000019         10  MAINTA PIC  X(0001).
000020     05  MAINTI PIC  X(0001).
000021*    -------------------------------
000022     05  DMAINT1L PIC S9(0004) COMP.
000023     05  DMAINT1F PIC  X(0001).
000024     05  FILLER REDEFINES DMAINT1F.
000025         10  DMAINT1A PIC  X(0001).
000026     05  DMAINT1I PIC  X(0009).
000027*    -------------------------------
000028     05  DMAINT2L PIC S9(0004) COMP.
000029     05  DMAINT2F PIC  X(0001).
000030     05  FILLER REDEFINES DMAINT2F.
000031         10  DMAINT2A PIC  X(0001).
000032     05  DMAINT2I PIC  X(0029).
000033*    -------------------------------
000034     05  CARRIERL PIC S9(0004) COMP.
000035     05  CARRIERF PIC  X(0001).
000036     05  FILLER REDEFINES CARRIERF.
000037         10  CARRIERA PIC  X(0001).
000038     05  CARRIERI PIC  X(0001).
000039*    -------------------------------
000040     05  GROUPL PIC S9(0004) COMP.
000041     05  GROUPF PIC  X(0001).
000042     05  FILLER REDEFINES GROUPF.
000043         10  GROUPA PIC  X(0001).
000044     05  GROUPI PIC  X(0006).
000045*    -------------------------------
000046     05  STATEL PIC S9(0004) COMP.
000047     05  STATEF PIC  X(0001).
000048     05  FILLER REDEFINES STATEF.
000049         10  STATEA PIC  X(0001).
000050     05  STATEI PIC  X(0002).
000051*    -------------------------------
000052     05  ACCTL PIC S9(0004) COMP.
000053     05  ACCTF PIC  X(0001).
000054     05  FILLER REDEFINES ACCTF.
000055         10  ACCTA PIC  X(0001).
000056     05  ACCTI PIC  X(0010).
000057*    -------------------------------
000058     05  EFFDTL PIC S9(0004) COMP.
000059     05  EFFDTF PIC  X(0001).
000060     05  FILLER REDEFINES EFFDTF.
000061         10  EFFDTA PIC  X(0001).
000062     05  EFFDTI PIC  X(0008).
000063*    -------------------------------
000064     05  CERTNOL PIC S9(0004) COMP.
000065     05  CERTNOF PIC  X(0001).
000066     05  FILLER REDEFINES CERTNOF.
000067         10  CERTNOA PIC  X(0001).
000068     05  CERTNOI PIC  X(0010).
000069*    -------------------------------
000070     05  SFXL PIC S9(0004) COMP.
000071     05  SFXF PIC  X(0001).
000072     05  FILLER REDEFINES SFXF.
000073         10  SFXA PIC  X(0001).
000074     05  SFXI PIC  X(0001).
000075*    -------------------------------
000076     05  SEQL PIC S9(0004) COMP.
000077     05  SEQF PIC  X(0001).
000078     05  FILLER REDEFINES SEQF.
000079         10  SEQA PIC  X(0001).
000080     05  SEQI PIC  99.
000081*    -------------------------------
000082     05  PAYTO1L PIC S9(0004) COMP.
000083     05  PAYTO1F PIC  X(0001).
000084     05  FILLER REDEFINES PAYTO1F.
000085         10  PAYTO1A PIC  X(0001).
000086     05  PAYTO1I PIC  X(0030).
000087*    -------------------------------
000088     05  CREATEDL PIC S9(0004) COMP.
000089     05  CREATEDF PIC  X(0001).
000090     05  FILLER REDEFINES CREATEDF.
000091         10  CREATEDA PIC  X(0001).
000092     05  CREATEDI PIC  X(0008).
000093*    -------------------------------
000094     05  CBYL PIC S9(0004) COMP.
000095     05  CBYF PIC  X(0001).
000096     05  FILLER REDEFINES CBYF.
000097         10  CBYA PIC  X(0001).
000098     05  CBYI PIC  X(0004).
000099*    -------------------------------
000100     05  PAYTO2L PIC S9(0004) COMP.
000101     05  PAYTO2F PIC  X(0001).
000102     05  FILLER REDEFINES PAYTO2F.
000103         10  PAYTO2A PIC  X(0001).
000104     05  PAYTO2I PIC  X(0030).
000105*    -------------------------------
000106     05  PAYEEL PIC S9(0004) COMP.
000107     05  PAYEEF PIC  X(0001).
000108     05  FILLER REDEFINES PAYEEF.
000109         10  PAYEEA PIC  X(0001).
000110     05  PAYEEI PIC  X(0006).
000111*    -------------------------------
000112     05  APVSTATL PIC S9(0004) COMP.
000113     05  APVSTATF PIC  X(0001).
000114     05  FILLER REDEFINES APVSTATF.
000115         10  APVSTATA PIC  X(0001).
000116     05  APVSTATI PIC  X(0009).
000117*    -------------------------------
000118     05  PAYAD1L PIC S9(0004) COMP.
000119     05  PAYAD1F PIC  X(0001).
000120     05  FILLER REDEFINES PAYAD1F.
000121         10  PAYAD1A PIC  X(0001).
000122     05  PAYAD1I PIC  X(0030).
000123*    -------------------------------
000124     05  APVDTL PIC S9(0004) COMP.
000125     05  APVDTF PIC  X(0001).
000126     05  FILLER REDEFINES APVDTF.
000127         10  APVDTA PIC  X(0001).
000128     05  APVDTI PIC  X(0008).
000129*    -------------------------------
000130     05  APVBYL PIC S9(0004) COMP.
000131     05  APVBYF PIC  X(0001).
000132     05  FILLER REDEFINES APVBYF.
000133         10  APVBYA PIC  X(0001).
000134     05  APVBYI PIC  X(0004).
000135*    -------------------------------
000136     05  PAYAD2L PIC S9(0004) COMP.
000137     05  PAYAD2F PIC  X(0001).
000138     05  FILLER REDEFINES PAYAD2F.
000139         10  PAYAD2A PIC  X(0001).
000140     05  PAYAD2I PIC  X(0030).
000141*    -------------------------------
000142     05  VOIDEDL PIC S9(0004) COMP.
000143     05  VOIDEDF PIC  X(0001).
000144     05  FILLER REDEFINES VOIDEDF.
000145         10  VOIDEDA PIC  X(0001).
000146     05  VOIDEDI PIC  X(0008).
000147*    -------------------------------
000148     05  VBYL PIC S9(0004) COMP.
000149     05  VBYF PIC  X(0001).
000150     05  FILLER REDEFINES VBYF.
000151         10  VBYA PIC  X(0001).
000152     05  VBYI PIC  X(0004).
000153*    -------------------------------
000154     05  PAYCTYL PIC S9(0004) COMP.
000155     05  PAYCTYF PIC  X(0001).
000156     05  FILLER REDEFINES PAYCTYF.
000157         10  PAYCTYA PIC  X(0001).
000158     05  PAYCTYI PIC  X(0028).
000159*    -------------------------------
000160     05  PAYSTL PIC S9(0004) COMP.
000161     05  PAYSTF PIC  X(0001).
000162     05  FILLER REDEFINES PAYSTF.
000163         10  PAYSTA PIC  X(0001).
000164     05  PAYSTI PIC  X(0002).
000165*    -------------------------------
000166     05  PRINTEDL PIC S9(0004) COMP.
000167     05  PRINTEDF PIC  X(0001).
000168     05  FILLER REDEFINES PRINTEDF.
000169         10  PRINTEDA PIC  X(0001).
000170     05  PRINTEDI PIC  X(0008).
000171*    -------------------------------
000172     05  PTOZIPL PIC S9(0004) COMP.
000173     05  PTOZIPF PIC  X(0001).
000174     05  FILLER REDEFINES PTOZIPF.
000175         10  PTOZIPA PIC  X(0001).
000176     05  PTOZIPI PIC  X(0009).
000177*    -------------------------------
000178     05  DEDCYNL PIC S9(0004) COMP.
000179     05  DEDCYNF PIC  X(0001).
000180     05  FILLER REDEFINES DEDCYNF.
000181         10  DEDCYNA PIC  X(0001).
000182     05  DEDCYNI PIC  X(0001).
000183*    -------------------------------
000184     05  CASHEDL PIC S9(0004) COMP.
000185     05  CASHEDF PIC  X(0001).
000186     05  FILLER REDEFINES CASHEDF.
000187         10  CASHEDA PIC  X(0001).
000188     05  CASHEDI PIC  X(0008).
000189*    -------------------------------
000190     05  RETTOL PIC S9(0004) COMP.
000191     05  RETTOF PIC  X(0001).
000192     05  FILLER REDEFINES RETTOF.
000193         10  RETTOA PIC  X(0001).
000194     05  RETTOI PIC  X(0030).
000195*    -------------------------------
000196     05  DPREML PIC S9(0004) COMP.
000197     05  DPREMF PIC  X(0001).
000198     05  FILLER REDEFINES DPREMF.
000199         10  DPREMA PIC  X(0001).
000200     05  DPREMI PIC  X(0005).
000201*    -------------------------------
000202     05  PREML PIC S9(0004) COMP.
000203     05  PREMF PIC  X(0001).
000204     05  FILLER REDEFINES PREMF.
000205         10  PREMA PIC  X(0001).
000206     05  PREMI PIC  X(0008).
000207*    -------------------------------
000208     05  DCOMML PIC S9(0004) COMP.
000209     05  DCOMMF PIC  X(0001).
000210     05  FILLER REDEFINES DCOMMF.
000211         10  DCOMMA PIC  X(0001).
000212     05  DCOMMI PIC  X(0005).
000213*    -------------------------------
000214     05  ISSCOMML PIC S9(0004) COMP.
000215     05  ISSCOMMF PIC  X(0001).
000216     05  FILLER REDEFINES ISSCOMMF.
000217         10  ISSCOMMA PIC  X(0001).
000218     05  ISSCOMMI PIC  X(0008).
000219*    -------------------------------
000220     05  DREFL PIC S9(0004) COMP.
000221     05  DREFF PIC  X(0001).
000222     05  FILLER REDEFINES DREFF.
000223         10  DREFA PIC  X(0001).
000224     05  DREFI PIC  X(0004).
000225*    -------------------------------
000226     05  REFL PIC S9(0004) COMP.
000227     05  REFF PIC  X(0001).
000228     05  FILLER REDEFINES REFF.
000229         10  REFA PIC  X(0001).
000230     05  REFI PIC  X(0008).
000231*    -------------------------------
000232     05  DUECOMML PIC S9(0004) COMP.
000233     05  DUECOMMF PIC  X(0001).
000234     05  FILLER REDEFINES DUECOMMF.
000235         10  DUECOMMA PIC  X(0001).
000236     05  DUECOMMI PIC  X(0007).
000237*    -------------------------------
000238     05  UECOMML PIC S9(0004) COMP.
000239     05  UECOMMF PIC  X(0001).
000240     05  FILLER REDEFINES UECOMMF.
000241         10  UECOMMA PIC  X(0001).
000242     05  UECOMMI PIC  X(0008).
000243*    -------------------------------
000244     05  DPREPDL PIC S9(0004) COMP.
000245     05  DPREPDF PIC  X(0001).
000246     05  FILLER REDEFINES DPREPDF.
000247         10  DPREPDA PIC  X(0001).
000248     05  DPREPDI PIC  X(0006).
000249*    -------------------------------
000250     05  PREPDL PIC S9(0004) COMP.
000251     05  PREPDF PIC  X(0001).
000252     05  FILLER REDEFINES PREPDF.
000253         10  PREPDA PIC  X(0001).
000254     05  PREPDI PIC  X(0008).
000255*    -------------------------------
000256     05  AMOUNTL PIC S9(0004) COMP.
000257     05  AMOUNTF PIC  X(0001).
000258     05  FILLER REDEFINES AMOUNTF.
000259         10  AMOUNTA PIC  X(0001).
000260     05  AMOUNTI PIC  S9(9)V99.
000261*    -------------------------------
000262     05  CHECKL PIC S9(0004) COMP.
000263     05  CHECKF PIC  X(0001).
000264     05  FILLER REDEFINES CHECKF.
000265         10  CHECKA PIC  X(0001).
000266     05  CHECKI PIC  X(0007).
000267*    -------------------------------
000268     05  TYPEL PIC S9(0004) COMP.
000269     05  TYPEF PIC  X(0001).
000270     05  FILLER REDEFINES TYPEF.
000271         10  TYPEA PIC  X(0001).
000272     05  TYPEI PIC  X(0001).
000273*    -------------------------------
000274     05  REASONL PIC S9(0004) COMP.
000275     05  REASONF PIC  X(0001).
000276     05  FILLER REDEFINES REASONF.
000277         10  REASONA PIC  X(0001).
000278     05  REASONI PIC  X(0025).
000279*    -------------------------------
000280     05  VREASONL PIC S9(0004) COMP.
000281     05  VREASONF PIC  X(0001).
000282     05  FILLER REDEFINES VREASONF.
000283         10  VREASONA PIC  X(0001).
000284     05  VREASONI PIC  X(0024).
000285*    -------------------------------
000286     05  STUBL PIC S9(0004) COMP.
000287     05  STUBF PIC  X(0001).
000288     05  FILLER REDEFINES STUBF.
000289         10  STUBA PIC  X(0001).
000290     05  STUBI PIC  X(0030).
000291*    -------------------------------
000292     05  TEXT1L PIC S9(0004) COMP.
000293     05  TEXT1F PIC  X(0001).
000294     05  FILLER REDEFINES TEXT1F.
000295         10  TEXT1A PIC  X(0001).
000296     05  TEXT1I PIC  X(0050).
000297*    -------------------------------
000298     05  TEXT2L PIC S9(0004) COMP.
000299     05  TEXT2F PIC  X(0001).
000300     05  FILLER REDEFINES TEXT2F.
000301         10  TEXT2A PIC  X(0001).
000302     05  TEXT2I PIC  X(0050).
000303*    -------------------------------
000304     05  TEXT3L PIC S9(0004) COMP.
000305     05  TEXT3F PIC  X(0001).
000306     05  FILLER REDEFINES TEXT3F.
000307         10  TEXT3A PIC  X(0001).
000308     05  TEXT3I PIC  X(0040).
000309*    -------------------------------
000310     05  ERRMSG1L PIC S9(0004) COMP.
000311     05  ERRMSG1F PIC  X(0001).
000312     05  FILLER REDEFINES ERRMSG1F.
000313         10  ERRMSG1A PIC  X(0001).
000314     05  ERRMSG1I PIC  X(0079).
000315*    -------------------------------
000316     05  ERRMSG2L PIC S9(0004) COMP.
000317     05  ERRMSG2F PIC  X(0001).
000318     05  FILLER REDEFINES ERRMSG2F.
000319         10  ERRMSG2A PIC  X(0001).
000320     05  ERRMSG2I PIC  X(0079).
000321*    -------------------------------
000322     05  PFENTERL PIC S9(0004) COMP.
000323     05  PFENTERF PIC  X(0001).
000324     05  FILLER REDEFINES PFENTERF.
000325         10  PFENTERA PIC  X(0001).
000326     05  PFENTERI PIC  9(2).
000327*    -------------------------------
000328     05  PF3L PIC S9(0004) COMP.
000329     05  PF3F PIC  X(0001).
000330     05  FILLER REDEFINES PF3F.
000331         10  PF3A PIC  X(0001).
000332     05  PF3I PIC  X(0016).
000333*    -------------------------------
000334     05  PF57L PIC S9(0004) COMP.
000335     05  PF57F PIC  X(0001).
000336     05  FILLER REDEFINES PF57F.
000337         10  PF57A PIC  X(0001).
000338     05  PF57I PIC  X(0030).
000339*    -------------------------------
000340     05  PF4L PIC S9(0004) COMP.
000341     05  PF4F PIC  X(0001).
000342     05  FILLER REDEFINES PF4F.
000343         10  PF4A PIC  X(0001).
000344     05  PF4I PIC  X(0018).
000345*    -------------------------------
000346     05  PF6L PIC S9(0004) COMP.
000347     05  PF6F PIC  X(0001).
000348     05  FILLER REDEFINES PF6F.
000349         10  PF6A PIC  X(0001).
000350     05  PF6I PIC  X(0013).
000351 01  EL677AO REDEFINES EL677AI.
000352     05  FILLER            PIC  X(0012).
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  DATEO PIC  X(0008).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  TIMEO PIC  99.99.
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  MAINTO PIC  X(0001).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  DMAINT1O PIC  X(0009).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  DMAINT2O PIC  X(0029).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  CARRIERO PIC  X(0001).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  GROUPO PIC  X(0006).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  STATEO PIC  X(0002).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  ACCTO PIC  X(0010).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  EFFDTO PIC  99B99B99.
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  CERTNOO PIC  X(0010).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  SFXO PIC  X(0001).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  SEQO PIC  99.
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  PAYTO1O PIC  X(0030).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  CREATEDO PIC  X(0008).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  CBYO PIC  X(0004).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  PAYTO2O PIC  X(0030).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  PAYEEO PIC  X(0006).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  APVSTATO PIC  X(0009).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  PAYAD1O PIC  X(0030).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  APVDTO PIC  X(0008).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  APVBYO PIC  X(0004).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  PAYAD2O PIC  X(0030).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  VOIDEDO PIC  X(0008).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  VBYO PIC  X(0004).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  PAYCTYO PIC  X(0028).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  PAYSTO PIC  X(0002).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  PRINTEDO PIC  99B99B99.
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  PTOZIPO PIC  X(0009).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  DEDCYNO PIC  X(0001).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  CASHEDO PIC  X(0008).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  RETTOO PIC  X(0030).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  DPREMO PIC  X(0005).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  PREMO PIC  Z(5).99.
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  DCOMMO PIC  X(0005).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  ISSCOMMO PIC  Z(5).99.
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  DREFO PIC  X(0004).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  REFO PIC  Z(5).99.
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  DUECOMMO PIC  X(0007).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  UECOMMO PIC  Z(5).99.
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  DPREPDO PIC  X(0006).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  PREPDO PIC  Z(5).99.
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  AMOUNTO PIC  Z(7).ZZ-.
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  CHECKO PIC  X(0007).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  TYPEO PIC  X(0001).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  REASONO PIC  X(0025).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  VREASONO PIC  X(0024).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  STUBO PIC  X(0030).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  TEXT1O PIC  X(0050).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  TEXT2O PIC  X(0050).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  TEXT3O PIC  X(0040).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  ERRMSG1O PIC  X(0079).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  ERRMSG2O PIC  X(0079).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  PFENTERO PIC  99.
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  PF3O PIC  X(0016).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  PF57O PIC  X(0030).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  PF4O PIC  X(0018).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  PF6O PIC  X(0013).
000527*    -------------------------------
      *<<((file: EL677S))
000541
000542 01  sqlconnect-parms.
000543     05  p-sql-server            PIC X(30).
000544     05  p-sql-database          PIC X(30).
000545     05  p-connect-return-code   pic s9(5) comp-5.
000546     05  p-sql-return-message    pic x(256).
000547
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
000549 01  DFHCOMMAREA.
000550     05  filler                  PIC X(1024).
000551
000552 01  var  pic x(30).
000553
000554
000555*                                COPY ELCCNTL.
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
000556*                                COPY ERCCHEK.
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
000557*                                COPY ERCACCT.
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
000558*                                COPY ERCCOMP.
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
000559*                                COPY ELCCERT.
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
000560*                                COPY ERCPNDB.
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
000561*                                COPY ERCPNDM.
      *>>((file: ERCPNDM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDM                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING MAILING DATA                      *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
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
000027* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
000028* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000029* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
000030******************************************************************
000031
000032 01  PENDING-MAILING-DATA.
000033     12  PM-RECORD-ID                      PIC XX.
000034         88  VALID-MA-ID                       VALUE 'PM'.
000035
000036     12  PM-CONTROL-PRIMARY.
000037         16  PM-COMPANY-CD                 PIC X.
000038         16  PM-ENTRY-BATCH                PIC X(6).
000039         16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
000040         16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
000041
000042     12  FILLER                            PIC X(14).
000043
000044     12  PM-ACCESS-CONTROL.
000045         16  PM-SOURCE-SYSTEM              PIC XX.
000046             88  PM-FROM-CREDIT                VALUE 'CR'.
000047             88  PM-FROM-VSI                   VALUE 'VS'.
000048             88  PM-FROM-WARRANTY              VALUE 'WA'.
000049             88  PM-FROM-OTHER                 VALUE 'OT'.
000050         16  PM-RECORD-ADD-DT              PIC XX.
000051         16  PM-RECORD-ADDED-BY            PIC XXXX.
000052         16  PM-LAST-MAINT-DT              PIC XX.
000053         16  PM-LAST-MAINT-BY              PIC XXXX.
000054         16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000055
000056     12  PM-PROFILE-INFO.
000057         16  PM-QUALIFY-CODE-1             PIC XX.
000058         16  PM-QUALIFY-CODE-2             PIC XX.
000059         16  PM-QUALIFY-CODE-3             PIC XX.
000060         16  PM-QUALIFY-CODE-4             PIC XX.
000061         16  PM-QUALIFY-CODE-5             PIC XX.
000062
000063         16  PM-INSURED-LAST-NAME          PIC X(15).
000064         16  PM-INSURED-FIRST-NAME         PIC X(10).
000065         16  PM-INSURED-MIDDLE-INIT        PIC X.
000066         16  PM-INSURED-ISSUE-AGE          PIC 99.
000067         16  PM-INSURED-BIRTH-DT           PIC XX.
000068         16  PM-INSURED-SEX                PIC X.
000069             88  PM-SEX-MALE                   VALUE 'M'.
000070             88  PM-SEX-FEMALE                 VALUE 'F'.
000071         16  PM-INSURED-SOC-SEC-NO         PIC X(11).
000072
000073         16  PM-ADDRESS-CORRECTED          PIC X.
000074         16  PM-JOINT-BIRTH-DT             PIC XX.
000075*        16  FILLER                        PIC X(12).
000076
000077         16  PM-ADDRESS-LINE-1             PIC X(30).
000078         16  PM-ADDRESS-LINE-2             PIC X(30).
000079         16  PM-CITY-STATE.
000080             20  PM-CITY                   PIC X(28).
000081             20  PM-STATE                  PIC XX.
000082         16  PM-ZIP.
000083             20  PM-ZIP-CODE.
000084                 24  PM-ZIP-1              PIC X.
000085                     88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000086                 24  FILLER                PIC X(4).
000087             20  PM-ZIP-PLUS4              PIC X(4).
000088         16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
000089             20  PM-CAN-POST1              PIC XXX.
000090             20  PM-CAN-POST2              PIC XXX.
000091             20  FILLER                    PIC XXX.
000092
000093         16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
000094         16  pm-city-st-zip-verified       pic x.
000095         16  FILLER                        PIC XX.
000096
000097     12  PM-CRED-BENE-INFO.
000098         16  PM-CRED-BENE-NAME             PIC X(25).
000099         16  PM-CRED-BENE-ADDR             PIC X(30).
000100         16  PM-CRED-BENE-ADDR2            PIC X(30).
000101         16  PM-CRED-BENE-CTYST.
000102             20  PM-CRED-BENE-CITY         PIC X(28).
000103             20  PM-CRED-BENE-STATE        PIC XX.
000104         16  PM-CRED-BENE-ZIP.
000105             20  PM-CB-ZIP-CODE.
000106                 24  PM-CB-ZIP-1           PIC X.
000107                     88  PM-CB-CANADIAN-POST-CODE
000108                                  VALUE 'A' THRU 'Z'.
000109                 24  FILLER                PIC X(4).
000110             20  PM-CB-ZIP-PLUS4           PIC X(4).
000111         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
000112             20  PM-CB-CAN-POST1           PIC XXX.
000113             20  PM-CB-CAN-POST2           PIC XXX.
000114             20  FILLER                    PIC XXX.
000115     12  PM-POST-CARD-MAIL-DATA.
000116         16  PM-MAIL-DATA OCCURS 7.
000117             20  PM-MAIL-TYPE              PIC X.
000118                 88  PM-12MO-MAILING           VALUE '1'.
000119                 88  PM-EXP-MAILING            VALUE '2'.
000120             20  PM-MAIL-STATUS            PIC X.
000121                 88  PM-MAIL-ST-MAILED         VALUE '1'.
000122                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
000123                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
000124             20  PM-MAIL-DATE              PIC XX.
000125     12  FILLER                            PIC XX.
000126     12  FILLER                            PIC X(12).
000127*    12  FILLER                            PIC X(30).
000128
000129******************************************************************
      *<<((file: ERCPNDM))
000562*                                COPY ERCMAIL.
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
000563*                                COPY ERCPYAJ.
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
000564
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL677' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000565 VCOBOL-DUMMY-PROCEDURE.
000566
000567     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000568     MOVE '5'                    TO  DC-OPTION-CODE.
000569     PERFORM 8500-DATE-CONVERT.
000570     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000571     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000572     MOVE DC-GREG-DATE-1-MDY     TO  SAVE-CURRENT-DATE-MDY.
000573
000574     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000575
000576     IF PI-AMOUNT NOT NUMERIC
000577         MOVE ZEROS              TO  PI-AMOUNT.
000578
000579     IF EIBCALEN = 0
000580         GO TO 8800-UNAUTHORIZED-ACCESS.
000581
000582     set P to address of KIXSYS
000583     CALL "getenv" using by value P returning var-ptr
000584     if var-ptr = null then
000585        display ' kixsys not set '
000586     else
000587        set address of var to var-ptr
000588        move 0 to env-var-len
000589        inspect var tallying env-var-len
000590          for characters before X'00'
000591        unstring var (1:env-var-len) delimited by '/'
000592           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000593              WS-KIX-SYS
000594        end-unstring
000595     end-if
000596
000597     MOVE 2                      TO  EMI-NUMBER-OF-LINES.
000598     MOVE 2                      TO  EMI-SWITCH2.
000599     MOVE EIBTRMID               TO  QID-TERM.
000600
000601     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000602         MOVE PI-CALLING-PROGRAM TO  RETURNED-FROM.
000603
000604     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000605         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000606             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
000607             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
000608             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
000609             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
000610             MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
000611             MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
000612             MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
000613             MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
000614         ELSE
000615             MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
000616             MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
000617             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
000618             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
000619             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
000620             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
000621             MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
000622             MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
000623
000624     MOVE LOW-VALUES             TO  EL677AI.
000625
000626     if eibtrnid not = trans-id
000627        evaluate true
000628           when pi-return-to-program = 'EL6315'
000629              move pi-company-cd to pi-chek-comp-cd
000630              move pi-carrier    to pi-chek-carrier
000631              move pi-grouping   to pi-chek-grouping
000632              move pi-state      to pi-chek-state
000633              move pi-account    to pi-chek-account
000634              move pi-cert-eff-dt to pi-chek-eff-dt
000635              move pi-cert-prime to pi-chek-cert-no
000636              move pi-cert-sfx   to pi-chek-suf-no
000637              move +1            to pi-chek-sequence
000638              move 'C'           to pi-check-type *> Correction
000639              move 'S'           to mainti
000640              move +1            to maintl
000641              move al-uanon      to mainta
000642              move dfhenter      to eibaid
000643              go to 5000-browse-file
000644           when pi-return-to-program = 'EL6318'
000645              move 'C'           to pi-check-type *> Correction
000646           when other
000647              move 'R'           to pi-check-type *> Refund
000648        end-evaluate
000649     end-if
000650     IF EIBTRNID NOT = TRANS-ID
000651         move pi-processor-id    to pi-return-to
000652         move +1                 to dedcynl
000653         move 'N'                to dedcyno
000654         move al-uanon           to dedcyna
000655         move dfhpf7             to eibaid
000656         move ' '                to pi-void-reissue-pass
000657         move zeros              to pi-void-reissue-amt
000658         perform 6800-browse-previous-chek-recs *> totals up prev
000659                                 thru 6800-exit
000660         perform 5600-get-erpndb thru 5600-exit *> calc refund on
000661                                                *> pending record
000662         IF PI-TO-EL677-FROM-EL1273
000663             MOVE 'S'            TO MAINTI
000664             MOVE 1              TO MAINTL
000665             MOVE AL-UABON       TO MAINTA
000666             MOVE DFHENTER       TO EIBAID
000667             GO TO 5000-BROWSE-FILE.
000668
000669     IF EIBTRNID NOT = TRANS-ID
000670        IF RETURNED-FROM = XCTL-1273 OR XCTL-114
000671           PERFORM 7200-RECOVER-TEMP-STORAGE THRU 7200-EXIT
000672           PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
000673           IF PI-PROCESS-BENEFICIARY
000674              GO TO 6600-BUILD-BENEFICIARY-SCREEN
000675           ELSE
000676              GO TO 6300-BUILD-CERT-SCREEN
000677           end-if
000678        ELSE
000679           IF EIBTRNID NOT = EL6311-TRANS-ID and el6318-trans-id
000680              GO TO 8100-SEND-INITIAL-MAP
000681           ELSE
000682              PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
000683              IF PI-PROCESS-BENEFICIARY
000684                 GO TO 6600-BUILD-BENEFICIARY-SCREEN
000685              ELSE
000686                 GO TO 6300-BUILD-CERT-SCREEN
000687              end-if
000688           end-if
000689        end-if
000690      end-if
000691
000692     MOVE PI-COMPANY-CD          TO  CHEK-COMPANY-CD
000693                                     PI-CHEK-COMP-CD.
000694
000695     
      * EXEC CICS HANDLE CONDITION
000696*        PGMIDERR  (9600-PGMID-ERROR)
000697*        ERROR     (9990-ABEND)
000698*    END-EXEC.
      *    MOVE '"$L.                  ! " #00005948' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303035393438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000699
000700     IF EIBAID = DFHCLEAR
000701         GO TO 9400-CLEAR.
000702
000703     IF PI-PROCESSOR-ID = 'LGXX'
000704         GO TO 0200-RECEIVE.
000705
000706     
      * EXEC CICS READQ TS
000707*        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
000708*        INTO   (SECURITY-CONTROL)
000709*        LENGTH (SC-COMM-LENGTH)
000710*        ITEM   (SC-ITEM)
000711*    END-EXEC.
      *    MOVE '*$II   L              ''   #00005959' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035393539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000712
000713     MOVE SC-CREDIT-DISPLAY (17)  TO PI-DISPLAY-CAP.
000714     MOVE SC-CREDIT-UPDATE  (17)  TO PI-MODIFY-CAP.
000715
000716     IF NOT DISPLAY-CAP
000717         MOVE 'READ'          TO SM-READ
000718         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000719         MOVE ER-0070         TO  EMI-ERROR
000720         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000721         GO TO 8100-SEND-INITIAL-MAP.
000722
000723 0200-RECEIVE.
000724     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000725         MOVE ER-1159            TO  EMI-ERROR
000726         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000727         MOVE -1                 TO  PFENTERL
000728         GO TO 8200-SEND-DATAONLY.
000729
000730     
      * EXEC CICS HANDLE CONDITION
000731*        MAPFAIL (8100-SEND-INITIAL-MAP)
000732*    END-EXEC.
      *    MOVE '"$?                   ! # #00005983' TO DFHEIV0
           MOVE X'22243F202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303035393833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000733
000734     
      * EXEC CICS RECEIVE
000735*        MAP      (EL677A)
000736*        MAPSET   (MAPSET-NAME)
000737*        INTO     (EL677AI)
000738*    END-EXEC.
           MOVE LENGTH OF
            EL677AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005987' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035393837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL677A, 
                 EL677AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000739
000740     IF PFENTERL GREATER ZERO
000741         IF EIBAID NOT = DFHENTER
000742             MOVE ER-0004        TO  EMI-ERROR
000743             MOVE AL-UNBOF       TO  PFENTERA
000744             MOVE -1             TO  PFENTERL
000745             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000746             GO TO 8200-SEND-DATAONLY
000747         ELSE
000748             IF (PFENTERI NUMERIC) AND
000749                (PFENTERI GREATER 0 AND LESS 25)
000750                 MOVE PF-VALUES (PFENTERI)   TO  EIBAID
000751             ELSE
000752                 MOVE ER-0029    TO  EMI-ERROR
000753                 MOVE AL-UNBOF   TO  PFENTERA
000754                 MOVE -1         TO  PFENTERL
000755                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000756                 GO TO 8200-SEND-DATAONLY.
000757
000758 0300-CHECK-PFKEYS.
000759     IF EIBAID = DFHPF23
000760         GO TO 8810-PF23.
000761
000762     IF EIBAID = DFHPF24
000763         GO TO 9200-RETURN-MAIN-MENU.
000764
000765     IF EIBAID = DFHPF12
000766         GO TO 9500-PF12.
000767
000768     IF EIBAID = DFHPF1 OR DFHPF2
000769         GO TO 5000-BROWSE-FILE.
000770
000771     if PI-TO-EL677-FROM-EL1273
000772        and (eibaid not = dfhenter)
000773        and (pi-void-reissue-pass not = '1' and '2')
000774        go to 0320-input-error
000775     end-if
000776
000777     IF EIBAID = DFHPF3
000778         PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT
000779         MOVE 'PF3'              TO  PI-PFKEY
000780         PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT
000781         IF PI-RETURN-TO-PROGRAM IS EQUAL TO 'EL1273  '
000782             GO TO 9400-CLEAR
000783         ELSE
000784             MOVE XCTL-1273      TO  PGM-NAME
000785             GO TO 9300-XCTL.
000786
000787     IF EIBAID = DFHPF4
000788         IF NOT FORCE-CAP
000789             MOVE ER-0433        TO EMI-ERROR
000790             MOVE -1             TO PFENTERL
000791             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000792             GO TO 8200-SEND-DATAONLY.
000793
000794     IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2 OR DFHPF4 OR
000795                             DFHPF5 OR DFHPF6 or dfhpf7
000796         GO TO 1000-EDIT-DATA.
000797
000798*    IF EIBAID = DFHPF7
000799*        PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT
000800*        PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT
000801*        MOVE XCTL-114           TO  PGM-NAME
000802*        GO TO 9300-XCTL.
000803
000804 0320-INPUT-ERROR.
000805     MOVE ER-0029                TO  EMI-ERROR.
000806     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000807     MOVE -1                     TO  PFENTERL.
000808     GO TO 8200-SEND-DATAONLY.
000809     EJECT
000810
000811 1000-EDIT-DATA.
000812
000813     IF MAINTI = 'S' OR 'C' OR 'A' OR 'V' OR 'D' or 'R' or 'X'
000814         MOVE AL-UANON           TO MAINTA
000815         IF MAINTI = 'C' OR 'V' OR 'R'
000816             IF PI-PREV-MAINT = 'S'  OR  'C' or 'R'
000817                 NEXT SENTENCE
000818             ELSE
000819                 MOVE ER-2056    TO  EMI-ERROR
000820                 MOVE -1         TO  MAINTL
000821                 MOVE AL-UABON   TO  MAINTA
000822                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000823                 GO TO 1100-EDIT-COMPLETE
000824         ELSE
000825             NEXT SENTENCE
000826     ELSE
000827         IF EIBAID = DFHPF5 OR DFHPF6 or dfhpf7
000828            NEXT SENTENCE
000829         ELSE
000830            MOVE ER-0023            TO  EMI-ERROR
000831            MOVE -1                 TO  MAINTL
000832            MOVE AL-UABON           TO  MAINTA
000833            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000834
000835     IF MODIFY-CAP
000836         NEXT SENTENCE
000837       ELSE
000838         IF MAINTI NOT = 'S'
000839            MOVE 'UPDATE'       TO SM-READ
000840            PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000841            MOVE ER-0070        TO EMI-ERROR
000842            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000843            GO TO 8100-SEND-INITIAL-MAP.
000844
000845     if (pi-return-to-program = 'EL1273')
000846        and (MAINTI not = 'S' and 'V' and 'R' and 'D' and 'X')
000847        move 'S'                 to mainti
000848        move +1                  to maintl
000849        move al-uanon            to mainta
000850     end-if
000851
000852     IF CARRIERI NOT = LOW-VALUES
000853         MOVE AL-UANON           TO  CARRIERA
000854         PERFORM 1200-VERIFY-CARRIER-ID THRU 1200-EXIT
000855         MOVE CARRIERI           TO  PI-CHEK-CARRIER
000856                                     PI-CARRIER
000857     ELSE
000858         MOVE -1                 TO  CARRIERL
000859         MOVE AL-UABON           TO  CARRIERA
000860         MOVE ER-0194            TO  EMI-ERROR
000861         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000862
000863     IF GROUPI NOT = LOW-VALUES
000864         MOVE AL-UANON           TO  GROUPA
000865         MOVE GROUPI             TO  PI-CHEK-GROUPING
000866                                     PI-GROUPING
000867     ELSE
000868         MOVE -1                 TO  GROUPL
000869         MOVE AL-UABON           TO  GROUPA
000870         MOVE ER-0195            TO  EMI-ERROR
000871         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000872
000873     IF STATEI NOT = LOW-VALUES
000874         MOVE AL-UANON           TO  STATEA
000875         PERFORM 1300-VERIFY-STATE-ID THRU 1390-EXIT
000876         MOVE STATEI             TO  PI-CHEK-STATE
000877                                     PI-STATE
000878     ELSE
000879         MOVE -1                 TO  STATEL
000880         MOVE AL-UABON           TO  STATEA
000881         MOVE ER-0196            TO  EMI-ERROR
000882         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000883
000884     IF EFFDTL GREATER ZERO
000885         MOVE EFFDTI             TO  WS-DT-DEEDIT-FIELD
000886         PERFORM 8700-DEEDIT
000887         IF WS-DEEDIT-FIELD-DATE-OUT IS NUMERIC
000888             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  EFFDTO
000889             INSPECT EFFDTI CONVERTING SPACES TO '/'
000890             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  DC-GREG-DATE-1-MDY
000891             MOVE '4'                TO  DC-OPTION-CODE
000892             PERFORM 8500-DATE-CONVERT
000893             IF DC-ERROR-CODE NOT = SPACES
000894                 MOVE ER-0215        TO  EMI-ERROR
000895                 PERFORM 9900-ERROR-FORMAT
000896                 MOVE -1             TO  EFFDTL
000897                 MOVE AL-UABON       TO  EFFDTA
000898               ELSE
000899                 MOVE AL-UANON       TO  EFFDTA
000900                 MOVE DC-BIN-DATE-1  TO  PI-CHEK-EFF-DT
000901                                         PI-CERT-EFF-DT
000902           ELSE
000903             MOVE ER-0215        TO  EMI-ERROR
000904             PERFORM 9900-ERROR-FORMAT
000905             MOVE -1             TO  EFFDTL
000906             MOVE AL-UABON       TO  EFFDTA
000907       ELSE
000908         MOVE ER-0216            TO  EMI-ERROR
000909         PERFORM 9900-ERROR-FORMAT
000910         MOVE -1                 TO  EFFDTL
000911         MOVE AL-UABOF           TO  EFFDTA.
000912
000913     IF ACCTI NOT = LOW-VALUES
000914         MOVE AL-UANON           TO  ACCTA
000915         PERFORM 1400-VERIFY-ACCOUNT THRU 1490-EXIT
000916         MOVE ACCTI              TO  PI-CHEK-ACCOUNT
000917                                     PI-ACCOUNT
000918     ELSE
000919         MOVE -1                 TO  ACCTL
000920         MOVE AL-UABON           TO  ACCTA
000921         MOVE ER-0197            TO  EMI-ERROR
000922         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000923
000924     IF CERTNOI NOT = LOW-VALUES
000925         MOVE AL-UANON           TO  CERTNOA
000926         MOVE CERTNOI            TO  PI-CHEK-CERT-NO
000927                                     PI-CERT-PRIME
000928     ELSE
000929         MOVE -1                 TO  CERTNOL
000930         MOVE AL-UABON           TO  CERTNOA
000931         MOVE ER-0203            TO  EMI-ERROR
000932         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000933
000934     IF SFXI  = LOW-VALUES
000935         MOVE SPACE              TO  SFXI.
000936
000937     MOVE AL-UANON               TO  SFXA.
000938
000939     MOVE SFXI                   TO  PI-CHEK-SUF-NO
000940                                     PI-CERT-SFX.
000941     IF SEQI = LOW-VALUES
000942         MOVE +1                 TO  SEQI.
000943
000944     MOVE AL-UNNON               TO  SEQA.
000945     MOVE SEQI                   TO  PI-CHEK-SEQUENCE.
000946
000947     IF PAYTO1L > 0
000948         MOVE PAYTO1I            TO  PI-PAYTO1
000949     ELSE
000950         MOVE SPACES             TO  PI-PAYTO1.
000951
000952     IF AMOUNTL > 0
000953         
      * EXEC CICS BIF DEEDIT
000954*            FIELD   (AMOUNTI)
000955*            LENGTH  (11)
000956*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006206' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036323036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMOUNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000957         MOVE AMOUNTI            TO  PI-AMOUNT
000958     ELSE
000959         MOVE ZEROS              TO  PI-AMOUNT.
000960
000961     IF TYPEL > 0
000962         MOVE TYPEI              TO  PI-TYPE
000963     ELSE
000964         MOVE SPACE              TO  PI-TYPE.
000965
000966*    IF REFL > 0
000967*        MOVE REFI               TO  PI-REFERENCE
000968*    ELSE
000969*        MOVE SPACES             TO  PI-REFERENCE.
000970
000971     if rettol not = zeros
000972        move rettoi              to pi-return-to
000973        MOVE AL-UANON            TO  RETTOA
000974     end-if
000975
000976     if dedcynl not = zeros
000977        if dedcyni = 'Y' OR 'N'
000978           move al-uanon         to dedcyna
000979           continue
000980        else
000981           move er-3451          to emi-error
000982           move -1               to dedcynl
000983           move al-uabon         to dedcyna
000984           perform 9900-error-format
000985                                 thru 9900-exit
000986        end-if
000987     end-if
000988
000989     if mainti = 'V' or 'R'
000990        if ((vreasonl > zeros)
000991           and (vreasoni (1:2) not = spaces))
000992             or (pi-void-reissue-pass = '1' or '2')
000993           continue
000994        else
000995           move er-3454          to emi-error
000996           move -1               to vreasonl
000997           move al-uabon         to vreasona
000998           perform 9900-error-format
000999                                 thru 9900-exit
001000        end-if
001001     end-if
001002
001003     if (mainti = 'A')
001004        and (pi-prev-paid > zeros)
001005        and (pi-check-type = 'C')
001006        move er-3459             to emi-error
001007        move -1                  to maintl
001008        move al-uabon            to mainta
001009        perform 9900-error-format
001010                                 thru 9900-exit
001011     end-if
001012
001013     if mainti = 'A'
001014        continue
001015     else
001016        IF MAINTI = 'S'
001017           or pi-prev-maint = 'S'
001018           GO TO 1100-EDIT-COMPLETE
001019        end-if
001020     end-if
001021
001022     if (pi-void-reissue-pass = '1')
001023        and (mainti = 'R')
001024        if emi-error = zeros
001025           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
001026              go to 7500-reissue-pass-1
001027           else
001028              if eibaid = dfhenter
001029                 move er-3460    to emi-error
001030                 move -1         to maintl
001031                 move al-uabon   to mainta
001032                 perform 9900-error-format
001033                                 thru 9900-exit
001034              end-if
001035           end-if
001036        end-if
001037     end-if
001038
001039     if (pi-void-reissue-pass = '2')
001040        and (mainti = 'R')
001041        if emi-error = zeros
001042           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
001043              go to 7500-reissue-pass-1
001044           else
001045              if eibaid = dfhenter
001046                 go to 7510-reissue-pass-2
001047              end-if
001048           end-if
001049        end-if
001050     end-if
001051
001052*    if (pi-void-reissue-pass = '1')
001053*       and (mainti = 'R')
001054*       if emi-error = zeros
001055*          if eibaid = dfhpf5 or dfhpf6 or dfhpf7
001056*             go to 7500-reissue-pass-1
001057*          else
001058*             if eibaid = dfhenter
001059*                go to 7510-reissue-pass-2
001060*                move '2'        to pi-void-reissue-pass
001061*             end-if
001062*          end-if
001063*       end-if
001064*    end-if
001065
001066
001067
001068     IF EIBAID = DFHPF5
001069        IF EMI-ERROR = ZEROS
001070          GO TO 6500-BUILD-ACCOUNT-SCREEN
001071        ELSE
001072          GO TO 1100-EDIT-COMPLETE.
001073
001074     IF EIBAID = DFHPF6
001075        IF EMI-ERROR = ZEROS
001076           GO TO 6300-BUILD-CERT-SCREEN
001077         ELSE
001078           GO TO 1100-EDIT-COMPLETE.
001079
001080     IF EIBAID = DFHPF7
001081        IF EMI-ERROR = ZEROS
001082           GO TO 6600-build-beneficiary-screen
001083         ELSE
001084           GO TO 1100-EDIT-COMPLETE.
001085
001086     if mainti = 'A'
001087        if dedcynl not = zeros
001088           if dedcyni <> pi-previous-deduct-comm
001089              perform 6700-build-common
001090                                 thru 6700-exit
001091              MOVE WS-REFUND-AMOUNT
001092                                 TO AMOUNTO
001093              MOVE AL-UANON      TO PAYTO1A
001094                                    PAYTO2A
001095                                    PAYAD1A
001096                                    PAYCTYA
001097                                    paysta
001098                                    PTOZIPA
001099              GO TO 8100-send-initial-map
001100           end-if
001101        end-if
001102     end-if
001103
001104*    if mainti = 'V'
001105*       if (vreasonl > zeros)
001106*          and (vreasoni (1:2) not = spaces)
001107*          continue
001108*       else
001109*          move er-3454          to emi-error
001110*          move -1               to vreasonl
001111*          move al-uabon         to vreasona
001112*          perform 9900-error-format
001113*                                thru 9900-exit
001114*       end-if
001115*    end-if
001116
001117     IF MAINTI = 'A'
001118        if (payto1i not = spaces and low-values)
001119           and (payad1i not = spaces and low-values)
001120           and (payctyi not = spaces and low-values)
001121           and (paysti not = spaces and low-values)
001122           and (ptozipi not = spaces and low-values)
001123           continue
001124        else
001125           MOVE -1               TO PAYTO1L
001126           MOVE ER-2907          TO EMI-ERROR
001127           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001128        end-if
001129     end-if
001130
001131     IF MAINTI = 'A'
001132         IF EIBAID NOT = DFHPF4
001133             MOVE DFHENTER       TO  EIBAID
001134             PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.
001135
001136     IF MAINTI = 'S' OR 'V' or 'R' or 'D' or 'X'
001137         GO TO 1100-EDIT-COMPLETE.
001138
001139     IF PRINTEDL GREATER THAN +0
001140         MOVE PRINTEDI           TO  WS-DEEDIT-FIELD-A
001141         PERFORM 8700-DEEDIT
001142         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
001143             MOVE WS-DEEDIT-FIELD-V0 TO  PRINTEDO
001144             INSPECT PRINTEDI CONVERTING SPACES TO '/'
001145             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
001146             MOVE '4'                TO  DC-OPTION-CODE
001147             PERFORM 8500-DATE-CONVERT
001148             IF DC-ERROR-CODE NOT = SPACES
001149                 MOVE ER-3046        TO  EMI-ERROR
001150                 PERFORM 9900-ERROR-FORMAT
001151                 MOVE -1             TO  PRINTEDL
001152                 MOVE AL-UABON       TO  PRINTEDA
001153                 MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT
001154             ELSE
001155                 MOVE AL-UANON       TO  PRINTEDA
001156                 MOVE DC-BIN-DATE-1  TO  WS-CHK-PRINT-DT
001157         ELSE
001158             MOVE ER-3046        TO  EMI-ERROR
001159             PERFORM 9900-ERROR-FORMAT
001160             MOVE -1             TO  PRINTEDL
001161             MOVE AL-UABON       TO  PRINTEDA
001162             MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT.
001163
001164     IF AMOUNTL = ZEROS
001165         GO TO 1050-CK-AMT.
001166
001167*    EXEC CICS BIF DEEDIT
001168*        FIELD   (AMOUNTI)
001169*        LENGTH  (11)
001170*    END-EXEC.
001171
001172     IF AMOUNTI NOT NUMERIC     OR
001173        AMOUNTI LESS THAN ZERO  OR
001174       (AMOUNTI = ZEROS  AND
001175               (PI-COMPANY-ID NOT = 'LAP' AND 'RMC'))
001176          MOVE ER-1159            TO  EMI-ERROR
001177          MOVE -1                 TO  AMOUNTL
001178          MOVE AL-UNBON           TO  AMOUNTA
001179          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001180          GO TO 1100-EDIT-COMPLETE.
001181
001182     MOVE AMOUNTI                TO  WS-AM-WORK.
001183
001184     IF WS-AM-SIGN = SPACES
001185         MOVE WS-AM-NUM          TO  WS-AMT
001186         ADD 0                   TO  WS-AMT
001187       ELSE
001188         MOVE AMOUNTI            TO  WS-AMT
001189         ADD 0                   TO  WS-AMT.
001190
001191     IF MAINTI = 'A'
001192        IF EIBAID = DFHENTER
001193           if (ws-amt + pi-prev-paid) >
001194              (cm-lf-premium-amt + cm-lf-alt-premium-amt +
001195                 cm-ah-premium-amt)
001196              move zeros to ws-amt
001197           end-if
001198        end-if
001199     end-if
001200
001201      MOVE WS-AMT                TO  AMOUNTO.
001202      MOVE AL-UNNON              TO  AMOUNTA.
001203
001204 1050-CK-AMT.
001205     IF AMOUNTL = ZEROS AND
001206        MAINTI  = 'A'
001207         MOVE ER-1159            TO  EMI-ERROR
001208         MOVE -1                 TO  AMOUNTL
001209         MOVE AL-UNBON           TO  AMOUNTA
001210         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001211
001212*    MOVE AL-UANON              TO  TYPEA.
001213
001214     if (amountl <> zeros)
001215        if (ws-amt <= zeros)
001216           and (pi-check-type <> 'C')
001217           MOVE ER-9999          TO EMI-ERROR
001218           MOVE -1               TO AMOUNTL
001219           MOVE AL-UNBON         TO AMOUNTA
001220           PERFORM 9900-ERROR-FORMAT
001221                                 THRU 9900-EXIT
001222        end-if
001223     end-if
001224
001225     IF TYPEL GREATER THAN ZEROS
001226         IF TYPEI = 'R' OR 'M' OR 'E' or 'C'
001227             NEXT SENTENCE
001228         ELSE
001229             MOVE ER-3044    TO  EMI-ERROR
001230             MOVE -1         TO  TYPEL
001231             MOVE AL-PABON   TO  TYPEA
001232             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001233     ELSE
001234         IF MAINTI  = 'A'
001235             MOVE ER-3044    TO  EMI-ERROR
001236             MOVE -1         TO  TYPEL
001237             MOVE AL-PABON   TO  TYPEA
001238             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001239
001240
001241 1100-EDIT-COMPLETE.
001242
001243     IF EMI-FATAL-CTR GREATER THAN +0  OR
001244       (EMI-FORCABLE-CTR GREATER THAN +0  AND
001245        EIBAID NOT = DFHPF4)
001246         GO TO 8200-SEND-DATAONLY.
001247
001248     MOVE MAINTI                 TO  PI-PREV-MAINT.
001249
001250     IF MAINTI = 'A'
001251         GO TO 2000-ADD-RECORD.
001252
001253*    IF MAINTI = 'C' OR 'V'
001254     IF MAINTI = 'V' or 'R' or 'X'
001255         GO TO 3000-CHANGE-RECORD.
001256
001257     IF MAINTI = 'S' or 'C'
001258         GO TO 5000-BROWSE-FILE.
001259
001260     if mainti = 'D'
001261        go to 3400-delete-record
001262     end-if
001263
001264     .
001265 1200-VERIFY-CARRIER-ID.
001266
001267     MOVE SPACES                 TO ELCNTL-KEY.
001268     MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.
001269     MOVE '6'                    TO ELCNTL-REC-TYPE.
001270     MOVE CARRIERI               TO ELCNTL-CARRIER.
001271     MOVE +0                     TO ELCNTL-SEQ.
001272
001273     
      * EXEC CICS HANDLE CONDITION
001274*        NOTFND   (1290-NO-CARRIER)
001275*    END-EXEC.
      *    MOVE '"$I                   ! $ #00006526' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303036353236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001276
001277     
      * EXEC CICS READ
001278*        DATASET   (ELCNTL-FILE-ID)
001279*        SET       (ADDRESS OF CONTROL-FILE)
001280*        RIDFLD    (ELCNTL-KEY)
001281*    END-EXEC.
      *    MOVE '&"S        E          (   #00006530' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036353330' TO DFHEIV0
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
           
001282
001283     GO TO 1200-EXIT.
001284
001285 1290-NO-CARRIER.
001286     MOVE ER-2208                TO EMI-ERROR.
001287     MOVE -1                     TO CARRIERL.
001288     MOVE AL-UABON               TO CARRIERA.
001289     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001290
001291 1200-EXIT.
001292      EXIT.
001293     EJECT
001294
001295 1300-VERIFY-STATE-ID.
001296     MOVE SPACES                 TO ELCNTL-KEY.
001297     MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.
001298     MOVE '3'                    TO ELCNTL-REC-TYPE.
001299     MOVE STATEI                 TO ELCNTL-STATE.
001300     MOVE +0                     TO ELCNTL-SEQ.
001301
001302     
      * EXEC CICS HANDLE CONDITION
001303*        NOTFND   (1380-NO-STATE)
001304*    END-EXEC.
      *    MOVE '"$I                   ! % #00006555' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303036353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001305
001306     
      * EXEC CICS READ
001307*        DATASET   (ELCNTL-FILE-ID)
001308*        SET       (ADDRESS OF CONTROL-FILE)
001309*        RIDFLD    (ELCNTL-KEY)
001310*    END-EXEC.
      *    MOVE '&"S        E          (   #00006559' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036353539' TO DFHEIV0
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
           
001311
001312     GO TO 1390-EXIT.
001313
001314 1380-NO-STATE.
001315     MOVE ER-2209                TO EMI-ERROR.
001316     MOVE -1                     TO STATEL.
001317     MOVE AL-UABON               TO STATEA.
001318     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001319
001320 1390-EXIT.
001321      EXIT.
001322     EJECT
001323
001324 1400-VERIFY-ACCOUNT.
001325     IF EIBAID = DFHPF5
001326         GO TO 1410-BUILD-ACCOUNT-KEY.
001327
001328     IF MAINTL GREATER +0
001329         NEXT SENTENCE
001330       ELSE
001331         GO TO 1490-EXIT.
001332
001333*    IF MAINTI = 'A' OR 'C' OR 'V'
001334     IF MAINTI = 'A' OR 'V' or 'R'
001335         NEXT SENTENCE
001336       ELSE
001337         GO TO 1490-EXIT.
001338
001339 1410-BUILD-ACCOUNT-KEY.
001340     MOVE CARRIERI               TO ERACCT-CARRIER.
001341     MOVE GROUPI                 TO ERACCT-GROUPING
001342     MOVE STATEI                 TO ERACCT-STATE.
001343     MOVE ACCTI                  TO ERACCT-ACCOUNT.
001344     MOVE PI-COMPANY-CD          TO ERACCT-CO.
001345
001346     MOVE '2'                    TO DC-OPTION-CODE.
001347     MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.
001348
001349     PERFORM 8500-DATE-CONVERT.
001350     IF DATE-CONVERSION-ERROR
001351        GO TO 1480-ACCOUNT-INVALID.
001352
001353     MOVE DC-BIN-DATE-1          TO ERACCT-EXP-DATE.
001354
001355    .
001356 1420-get-eracct.
001357
001358     MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.
001359
001360     
      * EXEC CICS HANDLE CONDITION
001361*        NOTFND   (1480-ACCOUNT-INVALID)
001362*        ENDFILE  (1480-ACCOUNT-INVALID)
001363*    END-EXEC.
      *    MOVE '"$I''                  ! & #00006613' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303036363133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001364
001365     
      * EXEC CICS STARTBR
001366*        DATASET   (ERACCT-FILE-ID)
001367*        RIDFLD    (ERACCT-KEY)
001368*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006618' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036363138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001369
001370     set eracct-start to true
001371     .
001372 1420-READNEXT-ACCOUNT-MASTER.
001373     
      * EXEC CICS READNEXT
001374*        DATASET   (ERACCT-FILE-ID)
001375*        SET       (ADDRESS OF ACCOUNT-MASTER)
001376*        RIDFLD    (ERACCT-KEY)
001377*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006626' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036363236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001378
001379*    IF PI-COMPANY-CD    NOT = AM-COMPANY-CD   OR
001380*       SV-ACCT-GROUPING NOT = AM-GROUPING     OR
001381*       SV-ACCT-CARRIER  NOT = AM-CARRIER      OR
001382*       SV-ACCT-STATE    NOT = AM-STATE        OR
001383*       SV-ACCT-ACCOUNT  NOT = AM-ACCOUNT
001384*         GO TO 1480-ACCOUNT-INVALID.
001385*
001386*    IF SV-ACCT-EXP-DATE LESS AM-EFFECTIVE-DT
001387*        GO TO 1480-ACCOUNT-INVALID.
001388*
001389*    IF SV-ACCT-EXP-DATE NOT LESS AM-EXPIRATION-DT
001390*        GO TO 1420-READNEXT-ACCOUNT-MASTER.
001391
001392     evaluate true
001393        when (save-eracct-key (1:20) not =
001394           am-control-primary (1:20))
001395           and (not eracct-found)
001396           go to 1480-account-invalid
001397        when (save-eracct-key (1:20) not =
001398           am-control-primary (1:20))
001399           and (eracct-found)
001400           move ws-hold-eracct-record
001401                                 to account-master
001402        when (sv-acct-exp-date < am-effective-dt)
001403           and (not eracct-found)
001404           go to 1480-account-invalid
001405        when sv-acct-exp-date >= am-expiration-dt
001406           go to 1420-readnext-account-master
001407        when am-expiration-dt = high-values
001408           continue
001409        when other
001410           set eracct-found to true
001411           move account-master   to ws-hold-eracct-record
001412           go to 1420-readnext-account-master
001413     end-evaluate
001414
001415     if eracct-start
001416        
      * exec cics endbr
001417*          dataset  (eracct-file-id)
001418*       end-exec
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006669' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036363639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 eracct-file-id, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001419     end-if
001420
001421*    if am-expiration-dt = high-values
001422*       continue
001423*    else
001424*       move account-master      to ws-hold-eracct-record
001425*       go to 1420-readnext-account-master
001426*    end-if
001427
001428     MOVE AM-CARRIER             TO PI-CR-CARRIER.
001429     MOVE AM-GROUPING            TO PI-CR-GROUPING.
001430     MOVE AM-STATE               TO PI-CR-STATE.
001431     MOVE AM-AGT (AM-REMIT-TO)   TO PI-CR-FIN-RESP.
001432
001433     MOVE AM-NAME                TO PI-AM-NAME.
001434     MOVE AM-ADDRS               TO PI-AM-ADDRS.
001435     MOVE AM-CITY                TO PI-AM-CITY-st
001436     MOVE AM-ZIP                 TO PI-AM-ZIP-CODE.
001437     move am-csr-code            to pi-am-csr
001438
001439     MOVE +0                     TO WS-SUB.
001440
001441 1430-FIND-ACC-AGT.
001442     ADD +1                      TO WS-SUB.
001443
001444     IF  WS-SUB GREATER +10
001445         GO TO 1480-ACCOUNT-INVALID.
001446
001447     IF (AM-COM-TYP (WS-SUB) = 'C' OR 'D' OR 'F')
001448         MOVE AM-AGT (WS-SUB)    TO  PI-CR-ACCOUNT
001449       ELSE
001450         GO TO 1430-FIND-ACC-AGT.
001451
001452     MOVE AL-UANON               TO CARRIERA
001453                                    GROUPA
001454                                    STATEA.
001455
001456     PERFORM 6000-VERIFY-COMP-MASTER THRU 6090-EXIT.
001457
001458     GO TO 1490-EXIT.
001459
001460 1480-ACCOUNT-INVALID.
001461     MOVE -1                     TO CARRIERL
001462     MOVE AL-UANON               TO CARRIERA
001463                                    GROUPA
001464                                    STATEA
001465                                    ACCTA.
001466     MOVE ER-2210                TO EMI-ERROR.
001467     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001468     GO TO 8200-SEND-DATAONLY.
001469
001470 1490-EXIT.
001471      EXIT.
001472
001473     EJECT
001474 1500-VERIFY-PENDING-BUS-REC.
001475
001476     MOVE 'N'                    TO WS-PNDB-FOUND-SW.
001477
001478     if pi-corr-check
001479        MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW
001480        go to 1590-exit
001481     end-if
001482
001483     
      * EXEC CICS HANDLE CONDITION
001484*        NOTFND   (1590-EXIT)
001485*    END-EXEC.
      *    MOVE '"$I                   ! '' #00006736' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303036373336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001486
001487
001488     MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.
001489     MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.
001490     MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.
001491     MOVE PI-STATE               TO ERPNDB-ALT-STATE.
001492     MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.
001493     MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.
001494     MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.
001495     MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.
001496     MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.
001497     MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.
001498
001499     IF ST-ACCNT-CNTL  OR
001500        ACCNT-CNTL
001501          MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
001502
001503     IF ST-ACCNT-CNTL      OR
001504        CARR-ST-ACCNT-CNTL OR
001505        ACCNT-CNTL         OR
001506        CARR-ACCNT-CNTL
001507          MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
001508
001509     IF ACCNT-CNTL OR
001510        CARR-ACCNT-CNTL
001511          MOVE SPACES             TO  ERPNDB-ALT-STATE.
001512
001513     
      * EXEC CICS READ
001514*         DATASET   (ERPNDB-ALT-FILE-ID)
001515*         SET       (ADDRESS OF PENDING-BUSINESS)
001516*         RIDFLD    (ERPNDB-ALT-KEY)
001517*     END-EXEC.
      *    MOVE '&"S        E          (   #00006766' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036373636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001518
001519     MOVE 'Y'                    TO WS-PNDB-FOUND-SW.
001520
001521     IF PB-C-PAYEE-CODE GREATER SPACES
001522        MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW.
001523
001524     MOVE PB-C-REFERENCE         TO PI-REFERENCE.
001525*    MOVE PB-CI-INSURED-NAME     TO PI-INSURED-NAME.
001526
001527     MOVE PB-C-LF-CANCEL-AMT     TO  PI-LF-REFUND.
001528     MOVE PB-C-AH-CANCEL-AMT     TO  PI-AH-REFUND.
001529
001530     IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
001531         MOVE PB-C-LF-CANCEL-DT  TO  PI-CANC-DT
001532     ELSE
001533         MOVE PB-C-AH-CANCEL-DT  TO  PI-CANC-DT.
001534
001535*    IF MAINTI = 'A' OR 'C'
001536*      IF PB-C-REFUND-CREATED
001537*          MOVE -1               TO  MAINTL
001538*          MOVE ER-3445          TO  EMI-ERROR
001539*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001540
001541 1590-EXIT.
001542      EXIT.
001543
001544     EJECT
001545 2000-ADD-RECORD.
001546
001547     
      * EXEC CICS GETMAIN
001548*        SET      (ADDRESS OF CHECK-RECORDS)
001549*        LENGTH   (600)
001550*        INITIMG  (GETMAIN-SPACE)
001551*    END-EXEC.
           MOVE 600
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00006800' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036383030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001552
001553     MOVE 'CH'                   TO CH-RECORD-ID.
001554     MOVE PI-COMPANY-CD          TO CH-COMPANY-CD.
001555     MOVE CARRIERI               TO CH-CARRIER.
001556     MOVE GROUPI                 TO CH-GROUPING.
001557     MOVE STATEI                 TO CH-STATE.
001558     MOVE ACCTI                  TO CH-ACCOUNT.
001559
001560     MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.
001561     MOVE '2'                    TO DC-OPTION-CODE.
001562     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
001563     MOVE DC-BIN-DATE-1          TO CH-CERT-EFF-DT.
001564
001565     MOVE CERTNOI                TO CH-CERT-PRIME.
001566     MOVE SFXI                   TO CH-CERT-SFX.
001567
001568     MOVE SEQI                   TO CH-SEQUENCE-NO.
001569
001570     MOVE PI-PROCESSOR-ID        TO CH-RECORDED-BY
001571                                    ch-released-by
001572     MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.
001573
001574     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
001575     MOVE '5'                    TO DC-OPTION-CODE.
001576     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
001577     MOVE DC-BIN-DATE-1          TO CH-RECORDED-DT
001578                                    ch-released-dt
001579
001580     move 'P'                    to ch-approval-status
001581
001582     MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT
001583                                    CH-VOID-DT
001584                                    CH-CREDIT-ACCEPT-DT
001585                                    CH-CANC-DT
001586                                    ch-approval-dt
001587                                    ch-check-cashed-dt
001588
001589     MOVE ZEROS                  TO CH-LF-REFUND
001590                                    CH-AH-REFUND
001591*                                   CH-DEDUCT-WITHHELD
001592*                                   CH-ADDITIONAL-CHARGE.
001593
001594     PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.
001595
001596     if pi-prev-paid not numeric
001597        move zeros               to pi-prev-paid
001598     end-if
001599
001600     if pi-prev-paid-this-month not numeric
001601        move zeros               to pi-prev-paid-this-month
001602     end-if
001603     IF EMI-FATAL-CTR GREATER THAN +0  OR
001604       (EMI-FORCABLE-CTR GREATER THAN +0  AND
001605        EIBAID NOT = DFHPF4)
001606         GO TO 8200-SEND-DATAONLY.
001607
001608     add ch-amount-paid to pi-prev-paid
001609     MOVE PI-CR-MONTH-END-DT     TO CH-CREDIT-SELECT-DT.
001610     MOVE PI-CR-CARRIER          TO CH-COMP-CARRIER.
001611     MOVE PI-CR-GROUPING         TO CH-COMP-GROUPING.
001612     MOVE PI-CR-FIN-RESP         TO CH-COMP-FIN-RESP.
001613     MOVE PI-CR-ACCOUNT          TO CH-COMP-ACCOUNT.
001614     move pi-am-csr              to ch-csr
001615
001616     .
001617 2100-WRITE-RECORD.
001618
001619     
      * exec cics read
001620*       dataset    (erchek-file-id)
001621*       into       (ws-dummy-erchek)
001622*       ridfld     (ch-control-primary)
001623*       resp       (ws-response)
001624*    end-exec
           MOVE LENGTH OF
            ws-dummy-erchek
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00006872' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036383732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 erchek-file-id, 
                 ws-dummy-erchek, 
                 DFHEIV11, 
                 ch-control-primary, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001625
001626     if RESP-NOTFND
001627        continue
001628     else
001629        add +1 to ch-sequence-no
001630        add 1 to seqi
001631        go to 2100-write-record
001632     end-if
001633
001634     PERFORM 2700-WRITE-SQL      THRU 2700-EXIT
001635
001636     
      * EXEC CICS HANDLE CONDITION
001637*        DUPREC (2200-DUPREC)
001638*    END-EXEC.
      *    MOVE '"$%                   ! ( #00006889' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303036383839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001639
001640     MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.
001641     MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL
001642                                    CH-CHECK-QUE-SEQUENCE
001643
001644     
      * EXEC CICS WRITE
001645*        DATASET  (ERCHEK-FILE-ID)
001646*        FROM     (CHECK-RECORDS)
001647*        RIDFLD   (CH-CONTROL-PRIMARY)
001648*        resp     (ws-response)
001649*    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00006897' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303036383937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 CH-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001650
001651     if resp-normal and pi-check-type = 'C'
001652        move 'Y'                 to pi-check-cut
001653     end-if
001654
001655     COMPUTE WS-JOURNAL-RECORD-LENGTH =
001656             ERCHEK-RECORD-LENGTH + 23.
001657
001658     if pi-check-type not = 'C'
001659        PERFORM 6200-UPDATE-PENDING-BUS-REC
001660                                 THRU 6200-EXIT
001661        if resp-normal
001662           MOVE 'Y'              TO PB-C-REFUND-SW
001663           add +1 to pi-chek-rec-cnt
001664           perform 6210-rewrite-pndb
001665                                 thru 6210-exit
001666*          PERFORM 2700-WRITE-SQL THRU 2700-EXIT
001667        end-if
001668     end-if
001669
001670     IF EMI-NO-ERRORS
001671         MOVE ER-0000            TO EMI-ERROR
001672     ELSE
001673         IF EMI-FORCABLE-CTR GREATER THAN +0  AND
001674            EIBAID = DFHPF4
001675             MOVE ER-2600        TO EMI-ERROR.
001676
001677     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001678
001679     MOVE LOW-VALUES             TO EL677AI.
001680
001681     MOVE PI-CHEK-CARRIER        TO CARRIERO.
001682     MOVE PI-CHEK-GROUPING       TO GROUPO.
001683     MOVE PI-CHEK-STATE          TO STATEO.
001684     MOVE PI-CHEK-ACCOUNT        TO ACCTO.
001685
001686     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
001687     MOVE SPACE                  TO DC-OPTION-CODE.
001688     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
001689     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
001690
001691     MOVE PI-CHEK-CERT-NO        TO CERTNOO.
001692     MOVE PI-CHEK-SUF-NO         TO SFXO.
001693     MOVE PI-CHEK-SEQUENCE       TO SEQO.
001694
001695     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
001696                                    ACCTA     EFFDTA  CERTNOA
001697                                    SFXA.
001698     MOVE AL-UNNON               TO SEQA.
001699     go to 5000-browse-file
001700     GO TO 8100-SEND-INITIAL-MAP.
001701
001702 2200-DUPREC.
001703     ADD +1 TO CH-SEQUENCE-NO.
001704     MOVE CH-SEQUENCE-NO         TO SEQO
001705                                    PI-CHEK-SEQUENCE
001706                                    CHEK-RECORD-SEQ.
001707     GO TO 2100-WRITE-RECORD.
001708
001709 2700-WRITE-SQL.
001710
001711     perform 2710-build-rec      thru 2710-exit
001712     perform 2720-insert-row     thru 2720-exit
001713     perform 4300-FINISH-UP-DB   thru 4300-exit
001714
001715     .
001716 2700-EXIT.
001717     EXIT.
001718
001719 2710-BUILD-REC.
001720
001721     move spaces                 to daily-check-request-rec
001722     move pi-company-id          to db-compid
001723     move ch-carrier             to db-carrier
001724                                    db-fincar
001725     move ch-grouping            to db-grouping
001726                                    db-fingrp
001727     move ch-state               to db-state
001728     move ch-account             to db-account
001729     move ch-cert-eff-dt         to dc-bin-date-1
001730     move ' '                    to dc-option-code
001731     perform 8500-date-convert   thru 8500-exit
001732     if no-conversion-error
001733        move dc-greg-date-a-edit to db-effdate
001734     else
001735        move spaces              to db-effdate
001736     end-if
001737     move ch-cert-prime          to db-certificate
001738     move ch-cert-sfx            to db-cert-sfx
001739     move ch-sequence-no         to db-seq-no
001740     move '1'                    to db-type
001741                                    db-check-sub-type
001742     if pi-check-type = 'C'
001743        move '2'                 to db-check-sub-type
001744     end-if
001745     move ch-amount-paid         to db-amount-n
001746     move zeros                  to db-checkstatus
001747     move ch-check-que-control   to db-releasebatch
001748     move save-bin-date          to dc-bin-date-1
001749     move ' '                    to dc-option-code
001750     perform 8500-date-convert   thru 8500-exit
001751     if no-conversion-error
001752        move dc-greg-date-a-edit to db-releasedt
001753     else
001754        move spaces              to db-releasedt
001755     end-if
001756     move pi-processor-id        to db-releaseby
001757     move ch-payee-name-1        to db-payeename1
001758     move ch-payee-name-2        to db-payeename2
001759     move ch-payee-address-1     to db-payeeaddr1
001760     move ch-payee-address-2     to db-payeeaddr2
001761     move ch-payee-city          to db-payeecity
001762     move ch-payee-state         to db-payeest
001763     move ch-payee-zip-code      to db-payeezip
001764     move ch-comp-fin-resp       to db-finresp
001765     move ch-comp-account        to db-finacct
001766     move ch-recorded-by         to db-preparer
001767     move ch-return-to           to db-return-to
001768     move pi-table-name          to db-insured-name
001769     MOVE CH-PAYEE-CODE          TO db-payeecode
001770
001771
001772     .
001773 2710-EXIT.
001774     EXIT.
001775
001776 2720-INSERT-ROW.
001777
001778     if not connected-to-db
001779        perform 4100-CONNECT-TO-DB
001780                                 thru 4100-exit
001781     end-if
001782
001784     EXEC SQL
              INSERT into ChkApp_Check (
001785           Company,
001786           CertCarrier,
001787           CertGroup,
001788           CertState,
001789           CertAccount,
001790           CertEffDate,
001791           CertNumber,
001792           CertNumberSuf,
001793           CheckSeqNbr,
001794           CheckType,
001795           CheckAmount,
001796           ReleaseBatchNbr,
001797           ReleaseDate,
001798           ReleasedBy,
001799           PayeeName1,
001800           PayeeName2,
001801           PayeeAddress1,
001802           PayeeAddress2,
001803           PayeeCity,
001804           PayeeState,
001805           PayeeZip,
001806           CompCarrier,
001807           CompGroup,
001808           CompFinResp,
001809           CompAccount,
001810           Preparer,
001811           ReturnTo,
001812           InsuredName,
001813           CheckSubType,
001814           PayeeCode)
001815         VALUES (
001816           :DB-CompId,
001817           :DB-Carrier,
001818           :DB-Grouping,
001819           :DB-State,
001820           :DB-Account,
001821           :DB-EffDate,
001822           :DB-Certificate,
001823           :db-cert-sfx,
001824           :DB-Seq-No,
001825           :DB-Type,
001826           :db-amount,
001827           :db-releasebatch,
001828           :db-releasedt,
001829           :db-releaseby,
001830           :db-payeename1,
001831           :db-payeename2,
001832           :db-payeeaddr1,
001833           :db-payeeaddr2,
001834           :db-payeecity,
001835           :db-payeest,
001836           :db-payeezip,
001837           :db-fincar,
001838           :db-fingrp,
001839           :db-finresp,
001840           :db-finacct,
001841           :db-preparer,
001842           :db-return-to,
001843           :db-insured-name,
001844           :db-check-sub-type,
001845           :db-payeecode)
001846     END-EXEC
001847
001848     display ' about to insert '
001849     display ' db-compid     ' db-compid
001850*    display ' db-carrier    ' db-carrier
001851*    display ' db-group      ' db-grouping
001852*    display ' db state      ' db-state
001853*    display ' accounbt      ' db-account
001854*    display ' effdt         ' db-effdate
001855     display ' cert          ' db-certificate
001856     display ' payee code ' CH-PAYEE-CODE
001857
001858*    display ' suffix        ' db-cert-sfx
001859*    display ' seq no        ' db-seq-no
001860*    display ' type          ' db-type
001861     if sqlcode = -2601
001862        add +1                   to ch-sequence-no
001863        move ch-sequence-no      to DB-Seq-No
001864        go to 2720-insert-row
001865     end-if
001866
001867     if sqlcode not = 0
001868        display "Error: cannot insert row "
001869        display ' sql return code ' sqlcode
001870        move sqlcode to ws-sql-code
001871        move ws-sql-code to ws-dis-sql-code
001872        display ' dis sql code ' ws-dis-sql-code
001873        display ' sql err mess    ' sqlerrmc
001874        move sqlcode to ws-sql-code
001875        move ws-sql-code to ws-dis-sql-code
001876        string ws-dis-sql-code ' ' sqlerrmc (1:50)
001877           into EMI-MESSAGE-AREA (1)
001878        end-string
001879        display ' msga1 ' emi-message-area (1)
001880        perform 4300-FINISH-UP-DB thru 4300-exit
001881        go to 8200-send-dataonly
001882*       goback
001883     end-if
001884
001885     .
001886 2720-EXIT.
001887     EXIT.
001888
001889 3000-CHANGE-RECORD.
001890     
      * EXEC CICS HANDLE CONDITION
001891*        NOTFND (3900-RECORD-NOTFND)
001892*    END-EXEC.
      *    MOVE '"$I                   ! ) #00007143' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303037313433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001893
001894     if mainti <> 'X'
001895        go to 3000-continue
001896     end-if
001897
001898     move pi-erchek-key          to erchek-key
001899
001900     
      * exec cics startbr
001901*        dataset     ('ERCHEK')
001902*        ridfld      (erchek-key)
001903*        resp        (ws-response)
001904*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00007153' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303037313533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erchek-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001905
001906     if not resp-normal
001907        display ' bad startbr ' ws-response
001908        go to 3000-continue
001909     end-if
001910
001911     .
001912 3000-readnext.
001913
001914     
      * exec cics readnext
001915*       dataset    ('ERCHEK')
001916*       ridfld  (erchek-key)
001917*       set     (address of check-records)
001918*       resp    (ws-response)
001919*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00007167' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303037313637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 erchek-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF check-records TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001920
001921     if not resp-normal
001922        display ' bad readnext ' ws-response
001923        go to 3000-endbr
001924     end-if
001925
001926     if ch-company-cd = pi-company-cd and
001927        ch-carrier    = pi-chek-carrier and
001928        ch-state      = pi-chek-state and
001929        ch-account    = pi-chek-account and
001930        ch-cert-eff-dt = pi-chek-eff-dt and
001931        ch-cert-prime = pi-chek-cert-no and
001932        ch-cert-sfx   = pi-chek-suf-no
001933        continue
001934     else
001935        go to 3000-endbr
001936     end-if
001937
001938     if ch-amount-paid = pi-void-reissue-amt and
001939        ch-void-dt = low-values and
001940        ch-recorded-dt = save-bin-date
001941        move er-3463             to emi-error
001942        move -1                  to maintl
001943        move al-uabon            to mainta
001944        perform 9900-error-format
001945                                 thru 9900-exit
001946        go to 8200-send-dataonly
001947     else
001948        go to 3000-readnext
001949     end-if
001950
001951     .
001952 3000-endbr.
001953
001954     
      * exec cics endbr
001955*       dataset   ('ERCHEK')
001956*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007207' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001957
001958     .
001959 3000-continue.
001960
001961     
      * EXEC CICS READ
001962*        DATASET  (ERCHEK-FILE-ID)
001963*        SET      (ADDRESS OF CHECK-RECORDS)
001964*        RIDFLD   (PI-ERCHEK-KEY)
001965*        UPDATE
001966*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007214' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037323134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001967
001968     IF CH-LF-REFUND NOT NUMERIC
001969         MOVE ZEROS              TO CH-LF-REFUND.
001970     IF CH-AH-REFUND NOT NUMERIC
001971         MOVE ZEROS              TO CH-AH-REFUND.
001972*    IF CH-DEDUCT-WITHHELD NOT NUMERIC
001973*        MOVE ZEROS              TO CH-DEDUCT-WITHHELD.
001974*    IF CH-ADDITIONAL-CHARGE NOT NUMERIC
001975*        MOVE ZEROS              TO CH-ADDITIONAL-CHARGE.
001976
001977     IF VREASONL NOT = ZEROS
001978         MOVE VREASONI           TO  CH-VOID-REASON.
001979
001980     IF MAINTI = 'V' or 'R'
001981        GO TO 3050-void-processing
001982     end-if
001983
001984     .
001985 3010-VOID-REVERSAL. *>  Mainti of X only
001986
001987     if ch-void-dt = low-values
001988        move er-3462             to emi-error
001989        move -1                  to maintl
001990        move al-uabon            to mainta
001991        
      * exec cics unlock
001992*          dataset    (ERCHEK-FILE-ID)
001993*       end-exec
      *    MOVE '&*                    #   #00007244' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037323434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001994        perform 9900-error-format
001995                                 thru 9900-exit
001996        go to 8200-send-dataonly
001997     end-if
001998
001999     if ch-void-dt <> SAVE-BIN-DATE
002000        move er-3461             to emi-error
002001        move -1                  to maintl
002002        move al-uabon            to mainta
002003        
      * exec cics unlock
002004*          dataset    (ERCHEK-FILE-ID)
002005*       end-exec
      *    MOVE '&*                    #   #00007256' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037323536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002006        perform 9900-error-format
002007                                 thru 9900-exit
002008        go to 8200-send-dataonly
002009     end-if
002010
002011     move low-values             to ch-void-dt
002012     move spaces                 to ch-void-by
002013                                    ch-void-reason
002014
002015     go to 3100-rewrite-record
002016
002017     PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.
002018
002019     PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.
002020
002021     IF EMI-FATAL-CTR GREATER THAN +0  OR
002022       (EMI-FORCABLE-CTR GREATER THAN +0  AND
002023        EIBAID NOT = DFHPF4)
002024         GO TO 8200-SEND-DATAONLY.
002025
002026     go to 3100-rewrite-record
002027
002028     .
002029 3050-void-processing.
002030
002031     perform 4100-CONNECT-TO-DB  thru 4100-exit
002032     if sqlcode = 0
002033        perform 4200-get-tbl-row thru 4200-exit
002034        if sqlcode = 0
002035           if nu-app-date = -1
002036              move -1            to maintl
002037              move er-3453       to emi-error
002038              move al-uabon      to mainta
002039              perform 9900-error-format
002040                                 thru 9900-exit
002041              perform 4300-FINISH-UP-DB thru 4300-exit
002042              go to 8200-send-dataonly
002043           end-if
002044        end-if
002045        if connected-to-db
002046           perform 4300-FINISH-UP-DB thru 4300-exit
002047        end-if
002048     end-if
002049
002050     if (ch-check-written-dt = low-values)
002051        or (ch-check-no = spaces)
002052        move -1                  to maintl
002053        move er-3455             to emi-error
002054        move al-uabon            to mainta
002055        perform 9900-error-format
002056                                 thru 9900-exit
002057        perform 4300-FINISH-UP-DB thru 4300-exit
002058        go to 8200-send-dataonly
002059     end-if
002060
002061     .
002062 3100-REWRITE-RECORD.
002063     MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.
002064
002065     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
002066     MOVE '5'                    TO DC-OPTION-CODE.
002067     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
002068
002069     IF MAINTI = 'C'
002070       MOVE PI-PROCESSOR-ID      TO CH-RECORDED-BY
002071     end-if
002072
002073     IF MAINTI = 'V' or 'R'
002074        IF (CH-VOID-DT = LOW-VALUES)
002075           and (pi-check-cashed = spaces)
002076           MOVE PI-PROCESSOR-ID  TO CH-VOID-BY
002077           MOVE DC-BIN-DATE-1    TO CH-VOID-DT
002078           PERFORM 3200-PAY-ADJS-REVERSAL      THRU 3290-EXIT
002079           if pi-chek-rec-cnt = +1
002080              PERFORM 6200-UPDATE-PENDING-BUS-REC
002081                                 THRU 6200-EXIT
002082              if resp-normal
002083                 MOVE ' '        TO PB-C-REFUND-SW
002084                 perform 6210-rewrite-pndb
002085                                 thru 6210-exit
002086              end-if
002087           end-if
002088        ELSE
002089           MOVE -1               TO MAINTL
002090           MOVE ER-2583          TO EMI-ERROR
002091           MOVE AL-UABON         TO MAINTA
002092           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002093           GO TO 8200-SEND-DATAONLY
002094        end-if
002095     end-if
002096
002097     MOVE CH-AMOUNT-PAID         TO WS-SV-AMOUNT-PAID.
002098
002099     
      * EXEC CICS REWRITE
002100*        DATASET  (ERCHEK-FILE-ID)
002101*        FROM     (CHECK-RECORDS)
002102*    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007352' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037333532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002103
002104     IF (PI-COMPANY-ID EQUAL 'LGX' OR 'TMS') AND
002105        (MAINTI EQUAL 'C')          AND
002106        (PI-PAYTO1 NOT EQUAL PAYTO1I)
002107            PERFORM 3300-UPDATE-ERPYAJ THRU 3300-EXIT.
002108
002109     if mainti = 'X'
002110        perform 3320-remove-rev-erpyaj-rec
002111                                 thru 3320-exit
002112     end-if
002113
002114     IF EMI-NO-ERRORS
002115         MOVE ER-0000            TO EMI-ERROR
002116     ELSE
002117         IF EMI-FORCABLE-CTR GREATER THAN +0  AND
002118            EIBAID = DFHPF4
002119             MOVE ER-2600        TO EMI-ERROR.
002120
002121     if mainti = 'R'
002122        if pi-void-reissue-pass = ' '
002123           move '1'              to pi-void-reissue-pass
002124        MOVE ER-3274             TO EMI-ERROR
002125        end-if
002126     end-if
002127
002128     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002129
002130     MOVE LOW-VALUES             TO EL677AI.
002131
002132     MOVE PI-CHEK-CARRIER        TO CARRIERO.
002133     MOVE PI-CHEK-GROUPING       TO GROUPO.
002134     MOVE PI-CHEK-STATE          TO STATEO.
002135     MOVE PI-CHEK-ACCOUNT        TO ACCTO.
002136
002137     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
002138     MOVE SPACE                  TO DC-OPTION-CODE.
002139     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
002140     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
002141
002142     MOVE PI-CHEK-CERT-NO        TO CERTNOO.
002143     MOVE PI-CHEK-SUF-NO         TO SFXO.
002144     MOVE PI-CHEK-SEQUENCE       TO SEQO.
002145
002146     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
002147                                     ACCTA   EFFDTA
002148                                    CERTNOA   SFXA.
002149
002150     MOVE AL-UNNON               TO SEQA.
002151     go to 5000-browse-file
002152*    GO TO 8100-SEND-INITIAL-MAP.
002153
002154     .
002155 3200-PAY-ADJS-REVERSAL.
002156
002157     IF CH-AMOUNT-PAID NOT = ZEROS
002158         PERFORM 3250-BUILD-ERPYAJ-REVERSAL THRU 3259-BUILD-EXIT
002159         PERFORM 3280-WRITE-ERPYAJ-REVERSAL THRU 3289-WRITE-EXIT.
002160
002161     GO TO 3290-EXIT
002162
002163     .
002164 3250-BUILD-ERPYAJ-REVERSAL.
002165
002166     
      * EXEC CICS GETMAIN
002167*        SET     (ADDRESS OF PENDING-PAY-ADJ)
002168*        LENGTH  (200)
002169*        INITIMG (GETMAIN-SPACE)
002170*    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007419' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037343139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002171
002172     MOVE 'PY'                   TO PY-RECORD-ID.
002173     MOVE PI-COMPANY-CD          TO PY-COMPANY-CD.
002174
002175     MOVE CH-COMP-CARRIER        TO PY-CARRIER.
002176     IF PI-ZERO-CARRIER  OR
002177        PI-ZERO-CAR-GROUP
002178         MOVE ZERO               TO PY-CARRIER.
002179
002180     MOVE CH-COMP-GROUPING       TO PY-GROUPING.
002181     IF PI-ZERO-GROUPING  OR
002182        PI-ZERO-CAR-GROUP
002183         MOVE ZERO               TO PY-GROUPING.
002184
002185     MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.
002186     MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.
002187     MOVE 'R'                    TO PY-RECORD-TYPE.
002188     MOVE EIBTIME                TO PY-FILE-SEQ-NO.
002189
002190     move '1825011300'           to py-gl-account
002191     move 'VOID REFCK'           TO py-gl-comment
002192     move ch-amount-paid         to py-entry-amt
002193*    COMPUTE PY-ENTRY-AMT = CH-AMOUNT-PAID * -1.
002194
002195     MOVE CH-CHECK-NO            TO WS-CHECK-WORK
002196     MOVE WS-CHECK-NO            TO PY-CHECK-NUMBER.
002197
002198     MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
002199     MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
002200     MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT
002201                                    PY-INPUT-DT.
002202*    MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
002203*                                   PY-CHECK-QUE-SEQUENCE.
002204     MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
002205                                    PY-BILLED-DATE
002206                                    PY-REPORTED-DT
002207                                    PY-CHECK-WRITTEN-DT
002208                                    PY-AR-DATE
002209                                    PY-GL-DATE.
002210     MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.
002211     MOVE CH-CHECK-ORIGIN-SW     TO PY-CHECK-ORIGIN-SW.
002212*    MOVE 'V'                    TO PY-VOID-SW.
002213*    MOVE CH-CHECK-REFERENCE     TO PY-REF-NO.
002214
002215     IF PI-AR-PROCESSING
002216         MOVE 'B'                TO PY-PYMT-TYPE
002217         MOVE 'A'                TO PY-PMT-APPLIED.
002218
002219 3259-BUILD-EXIT.
002220     EXIT.
002221
002222 3280-WRITE-ERPYAJ-REVERSAL.
002223
002224     
      * EXEC CICS HANDLE CONDITION
002225*        DUPREC (3287-DUPREC)
002226*    END-EXEC.
      *    MOVE '"$%                   ! * #00007477' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303037343737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002227
002228     
      * EXEC CICS WRITE
002229*        DATASET (ERPYAJ-FILE-ID)
002230*        FROM    (PENDING-PAY-ADJ)
002231*        RIDFLD  (PY-CONTROL-PRIMARY)
002232*    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007481' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037343831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002233
002234     GO TO 3289-WRITE-EXIT.
002235
002236 3287-DUPREC.
002237     ADD +1 TO PY-FILE-SEQ-NO.
002238
002239     GO TO 3280-WRITE-ERPYAJ-REVERSAL.
002240
002241 3289-WRITE-EXIT.
002242     EXIT.
002243
002244 3290-EXIT.
002245      EXIT.
002246
002247     EJECT
002248 3300-UPDATE-ERPYAJ.
002249
002250     
      * EXEC CICS GETMAIN
002251*        SET     (ADDRESS OF PENDING-PAY-ADJ)
002252*        LENGTH  (200)
002253*        INITIMG (GETMAIN-SPACE)
002254*    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007503' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037353033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002255
002256     MOVE PI-COMPANY-CD          TO PYAJ-COMPANY-CD.
002257     MOVE PI-CHEK-CARRIER        TO PYAJ-CARRIER.
002258     MOVE PI-CHEK-GROUPING       TO PYAJ-GROUPING.
002259     MOVE PI-CR-FIN-RESP         TO PYAJ-FIN-RESP.
002260     MOVE PI-CR-ACCOUNT          TO PYAJ-ACCOUNT.
002261     MOVE 'C'                    TO PYAJ-RECORD-TYPE.
002262     MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO.
002263
002264     
      * EXEC CICS HANDLE CONDITION
002265*        NOTFND   (3300-EXIT)
002266*        ENDFILE  (3300-EXIT)
002267*    END-EXEC.
      *    MOVE '"$I''                  ! + #00007517' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303037353137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002268
002269 3300-READNEXT-ERPYAJ.
002270
002271     
      * EXEC CICS READ
002272*        DATASET  (ERPYAJ-FILE-ID)
002273*        SET      (ADDRESS OF PENDING-PAY-ADJ)
002274*        RIDFLD   (ERPYAJ-KEY)
002275*        GTEQ
002276*    END-EXEC.
      *    MOVE '&"S        G          (   #00007524' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303037353234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002277
002278     MOVE PY-CONTROL-PRIMARY TO ERPYAJ-KEY.
002279
002280     IF (PI-COMPANY-CD    NOT = PY-COMPANY-CD) OR
002281        (PI-CHEK-CARRIER  NOT = PY-CARRIER)    OR
002282        (PI-CHEK-GROUPING NOT = PY-GROUPING)   OR
002283        (PI-CR-FIN-RESP   NOT = PY-FIN-RESP)   OR
002284        (PI-CR-ACCOUNT    NOT = PY-ACCOUNT)    OR
002285        (PY-RECORD-TYPE   NOT = 'C')
002286          GO TO 3300-EXIT.
002287
002288     IF WS-SV-AMOUNT-PAID NOT EQUAL PY-ENTRY-AMT
002289         ADD +1 TO PYAJ-FILE-SEQ-NO
002290         GO TO 3300-READNEXT-ERPYAJ.
002291
002292     
      * EXEC CICS READ UPDATE
002293*        DATASET   (ERPYAJ-FILE-ID)
002294*        RIDFLD    (ERPYAJ-KEY)
002295*        SET       (ADDRESS OF PENDING-PAY-ADJ)
002296*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007545' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002297
002298     MOVE PAYTO1I                TO WS-TMS-PY-PAYEE.
002299     MOVE CERTNOI                TO WS-TMS-PY-CERT.
002300
002301     MOVE WS-TMS-ENTRY-COMMENT TO PY-ENTRY-COMMENT.
002302
002303     MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
002304     MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
002305     MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT
002306
002307     
      * EXEC CICS REWRITE
002308*        DATASET  (ERPYAJ-FILE-ID)
002309*        FROM     (PENDING-PAY-ADJ)
002310*    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007560' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037353630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002311
002312 3300-EXIT.
002313      EXIT.
002314
002315 3320-remove-rev-erpyaj-rec.
002316
002317     
      * EXEC CICS GETMAIN
002318*       SET     (ADDRESS OF PENDING-PAY-ADJ)
002319*       LENGTH  (200)
002320*       INITIMG (GETMAIN-SPACE)
002321*    END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007570' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037353730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002322
002323     MOVE PI-COMPANY-CD          TO erpyaj-key
002324     MOVE ch-comp-CARRIER        TO PYAJ-CARRIER
002325     MOVE ch-comp-grouping       TO PYAJ-GROUPING
002326     MOVE ch-comp-fin-resp       TO PYAJ-FIN-RESP
002327     MOVE ch-comp-account        TO PYAJ-ACCOUNT
002328     MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO
002329
002330     MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.
002331     MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.
002332     
      * exec cics startbr
002333*       dataset    ('ERPYAJ')
002334*       ridfld     (erpyaj-key)
002335*       resp       (ws-response)
002336*    end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00007585' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303037353835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erpyaj-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002337
002338     if not resp-normal
002339        display ' no erpyaj found ' pyaj-carrier ' '
002340        pyaj-fin-resp ' ' pyaj-account
002341        go to 3320-exit
002342     end-if
002343
002344     set pyaj-browse-started to true
002345
002346     .
002347 3320-readnext.
002348
002349     
      * exec cics readnext
002350*       dataset     ('ERPYAJ')
002351*       set         (address of pending-pay-adj)
002352*       ridfld      (erpyaj-key)
002353*       resp        (ws-response)
002354*    end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00007602' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303037363032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 erpyaj-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF pending-pay-adj TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002355
002356     if not resp-normal
002357        display ' error 3320-readnext ' ws-response
002358        go to 3320-exit
002359     end-if
002360
002361     if ch-comp-carrier = py-carrier and
002362        ch-comp-fin-resp  = py-fin-resp and
002363        ch-comp-account = py-account
002364        if py-record-type = 'R' and
002365           ch-amount-paid = py-entry-amt and
002366           py-last-maint-dt = save-bin-date and
002367           py-gl-comment = 'VOID REFCK' and
002368           py-check-number = ch-check-no(2:6) and
002369           py-gl-account = '1825011300'
002370           
      * exec cics delete
002371*             dataset     ('ERPYAJ')
002372*             ridfld      (erpyaj-key)
002373*             resp        (ws-response)
002374*          end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00007623' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'204E233030303037363233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erpyaj-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002375           if not resp-normal
002376              display ' bad delete ' ws-response ' ' pyaj-account
002377           end-if
002378        else
002379           go to 3320-readnext
002380        end-if
002381     end-if
002382
002383     .
002384 3320-end-browse.
002385
002386     if pyaj-browse-started
002387        
      * exec cics endbr
002388*           dataset    ('ERPYAJ')
002389*       end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007640' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037363430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002390     end-if
002391     .
002392 3320-exit.
002393     exit.
002394
002395 3400-delete-record.
002396
002397     
      * EXEC CICS READ
002398*        DATASET  (ERCHEK-FILE-ID)
002399*        SET      (ADDRESS OF CHECK-RECORDS)
002400*        RIDFLD   (PI-ERCHEK-KEY)
002401*        UPDATE
002402*        resp     (ws-response)
002403*    END-EXEC
      *    MOVE '&"S        EU         (  N#00007650' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303037363530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002404
002405     if (ch-in-limbo or ch-approv-pending)
002406        and (ch-void-dt = low-values)
002407        and (ch-check-written-dt = low-values)
002408        and (ch-credit-accept-dt = low-values)
002409        perform 3500-check-tbl   thru 3500-exit
002410        if (row-deleted and tbl-commited)
002411*          or (emi-error = er-3450)
002412           
      * exec cics delete
002413*             dataset   (erchek-file-id)
002414*          end-exec
      *    MOVE '&(                    &   #00007665' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303037363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 erchek-file-id, 
                 PI-ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002415           move spaces to pi-check-cut
002416        end-if
002417     else
002418        move -1               to maintl
002419        move er-3452          to emi-error
002420        move al-uabon         to mainta
002421        perform 9900-error-format
002422                                 thru 9900-exit
002423        go to 8200-send-dataonly
002424     end-if
002425
002426     if pi-chek-rec-cnt = +1
002427        PERFORM 6200-UPDATE-PENDING-BUS-REC
002428                                 THRU 6200-EXIT
002429        if resp-normal
002430           MOVE ' '              TO PB-C-REFUND-SW
002431           perform 6210-rewrite-pndb
002432                                 thru 6210-exit
002433        end-if
002434     end-if
002435
002436     MOVE ER-0000            TO EMI-ERROR
002437     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002438
002439     MOVE LOW-VALUES             TO EL677AI.
002440
002441     MOVE PI-CHEK-CARRIER        TO CARRIERO.
002442     MOVE PI-CHEK-GROUPING       TO GROUPO.
002443     MOVE PI-CHEK-STATE          TO STATEO.
002444     MOVE PI-CHEK-ACCOUNT        TO ACCTO.
002445
002446     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
002447     MOVE SPACE                  TO DC-OPTION-CODE.
002448     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
002449     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
002450
002451     MOVE PI-CHEK-CERT-NO        TO CERTNOO.
002452     MOVE PI-CHEK-SUF-NO         TO SFXO.
002453     MOVE PI-CHEK-SEQUENCE       TO SEQO.
002454
002455     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
002456                                    ACCTA   EFFDTA
002457                                    CERTNOA   SFXA.
002458
002459     MOVE AL-UNNON               TO SEQA.
002460     GO TO 8100-SEND-INITIAL-MAP
002461     .
002462 3400-exit.
002463     exit.
002464
002465 3500-check-tbl.
002466
002467     perform 4100-CONNECT-TO-DB  thru 4100-exit
002468     if sqlcode = 0
002469        perform 4200-get-tbl-row thru 4200-exit
002470     end-if
002471      if sqlcode = 0
002472         if (nu-app-date = -1)
002473            or (db-app-status = '2')
002474            perform 4250-delete-row thru 4250-exit
002475         else
002476           MOVE ER-3452            TO  EMI-ERROR
002477           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002478           MOVE -1                 TO  PFENTERL
002479           GO TO 8200-SEND-DATAONLY
002480        end-if
002481      else
002482         MOVE ER-3450            TO  EMI-ERROR
002483         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002484         MOVE -1                 TO  PFENTERL
002485*        GO TO 8200-SEND-DATAONLY
002486      end-if
002487     if connected-to-db
002488        perform 4300-FINISH-UP-DB thru 4300-exit
002489     end-if
002490
002491     .
002492 3500-exit.
002493     exit.
002494
002495 3900-RECORD-NOTFND.
002496     MOVE ER-2908                TO EMI-ERROR.
002497     MOVE -1                     TO CARRIERL
002498     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
002499                                    ACCTA     EFFDTA  CERTNOA
002500                                    SFXA      SEQA.
002501     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002502     GO TO 8200-SEND-DATAONLY.
002503     EJECT
002504 4000-ADD-CHANGE.
002505
002506     IF PAYTO1L NOT = ZEROS
002507        MOVE PAYTO1I             TO CH-PAYEE-NAME-1
002508     END-IF
002509
002510     IF PAYTO2L NOT = ZEROS
002511         MOVE PAYTO2I            TO CH-PAYEE-NAME-2.
002512
002513     IF PAYAD1L NOT = ZEROS
002514         MOVE PAYAD1I            TO CH-PAYEE-ADDRESS-1.
002515
002516     IF PAYAD2L NOT = ZEROS
002517         MOVE PAYAD2I            TO CH-PAYEE-ADDRESS-2.
002518
002519
002520     IF PAYctyL NOT = ZEROS
002521         MOVE payctyi            TO CH-PAYEE-CITY.
002522
002523     IF PAYstL NOT = ZEROS
002524         MOVE paysti            TO CH-PAYEE-state.
002525
002526     IF PAYEEI GREATER SPACES
002527        MOVE PAYEEI              TO CH-PAYEE-CODE.
002528
002529     IF PTOZIPL  =  ZEROS
002530         MOVE ZEROS              TO CH-PAYEE-ZIP-CODE
002531         GO TO 4000-CK-REST.
002532
002533     MOVE PTOZIPI                TO WS-ZIP-CODE.
002534
002535     IF NOT WS-CANADIAN-POST-CODE
002536         MOVE WS-ZIP-PRIME       TO CH-PAYEE-ZIP
002537         MOVE WS-ZIP-PLUS4       TO CH-PAYEE-ZIP-EXT
002538         GO TO 4000-CK-REST.
002539
002540     MOVE SPACES                 TO CH-CANADIAN-POSTAL-CODE.
002541     IF WS-CAN-POST-4TH = SPACE OR '-'
002542         MOVE WS-CAN-POST2-1     TO CH-CAN-POSTAL-1
002543         MOVE WS-CAN-POST2-2     TO CH-CAN-POSTAL-2
002544     ELSE
002545         MOVE WS-CAN-POST1-1     TO CH-CAN-POSTAL-1
002546         MOVE WS-CAN-POST1-2     TO CH-CAN-POSTAL-2.
002547
002548 4000-CK-REST.
002549
002550     IF RETTOL NOT = ZEROS
002551        MOVE RETTOI TO CH-RETURN-TO.
002552
002553     if dedcynl not = zeros
002554        move dedcyni             to ch-deduct-commission
002555     end-if
002556
002557*    if dedcynl not = zeros
002558*       if (dedcyni = 'Y')
002559*          or (pi-prev-ded-comm = 'Y')
002560*          compute ws-amt =
002561*             pi-refund-on-pending-rec -
002562*                pi-prev-paid - pi-ue-comm
002563*       else
002564*          if dedcyni = 'N'
002565*             compute ws-amt =
002566*                pi-refund-on-pending-rec -
002567*                   pi-prev-paid-this-month
002568*          end-if
002569*       end-if
002570*    end-if
002571
002572     if pi-check-type = 'C'
002573        compute ws-amt =
002574           pi-endt-prm-diff - pi-prev-paid
002575        if (dedcyni = 'Y')
002576           or (pi-prev-ded-comm = 'Y')
002577           compute ws-amt =
002578              ws-amt - pi-ue-comm
002579        end-if
002580        if ws-amt <= zeros
002581           move pi-endt-prm-diff to ws-amt
002582        end-if
002583     end-if
002584
002585     IF AMOUNTL NOT = ZEROS
002586         MOVE WS-AMT             TO CH-AMOUNT-PAID
002587         MOVE CH-AMOUNT-PAID     TO AMOUNTO.
002588
002589     IF CHECKL NOT = ZEROS
002590         MOVE CHECKI             TO CH-CHECK-NO.
002591
002592     IF REASONL NOT = ZEROS
002593         MOVE REASONI            TO CH-REASON-FOR-CHECK.
002594
002595*    IF DEDAMTL NOT = ZEROS
002596*        MOVE WS-DED             TO CH-DEDUCT-WITHHELD
002597*                                   DEDAMTO.
002598
002599*    IF ADDLCHGL NOT = ZEROS
002600*        MOVE WS-ADDL            TO CH-ADDITIONAL-CHARGE
002601*                                   ADDLCHGO.
002602
002603     IF STUBL NOT = ZEROS
002604         MOVE STUBI              TO CH-STUB-LINE-1.
002605
002606     IF TEXT1L NOT = ZEROS
002607         MOVE TEXT1I             TO CH-TEXT-LINE-1.
002608
002609     IF TEXT2L NOT = ZEROS
002610         MOVE TEXT2I             TO CH-TEXT-LINE-2.
002611
002612     IF TEXT3L NOT = ZEROS
002613         MOVE TEXT3I             TO CH-TEXT-LINE-3.
002614
002615     IF TYPEL NOT = ZEROS
002616         MOVE TYPEI              TO CH-CHECK-ORIGIN-SW.
002617
002618*    MOVE LETTER1I               TO CH-LETTERS (1).
002619*    MOVE LETTER2I               TO CH-LETTERS (2).
002620*    MOVE LETTER3I               TO CH-LETTERS (3).
002621
002622*    MOVE SPACES                 TO CH-CHECK-REFERENCE.
002623     IF CH-CHECK-ORIGIN-SW = 'R'
002624         PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
002625*        MOVE PI-REFERENCE           TO CH-CHECK-REFERENCE
002626         MOVE PI-CANC-DT             TO CH-CANC-DT
002627         MOVE PI-LF-REFUND           TO CH-LF-REFUND
002628         MOVE PI-AH-REFUND           TO CH-AH-REFUND
002629     end-if
002630
002631     MOVE PI-INSURED-NAME        TO CH-INSURED-NAME.
002632*    IF PI-AR-PROCESSING AND
002633*       REFL NOT = ZERO
002634*        MOVE REFI                   TO CH-CHECK-REFERENCE.
002635
002636 4000-EXIT.
002637      EXIT.
002638
002639 4100-CONNECT-TO-DB.
002640
002641****  The below code is for when the db has been
002642****  converted to sql server 2016
002643     evaluate ws-kix-myenv
002644        when 'cid1p'
002645           move '//sdv-db01.cso.local:1433;'
002646                                 to p-sql-server
002647        when 'mdoff'
002648           move '//hov-tstdb01.cso.local:55330;'
002649                                 to p-sql-server
002650        when other
002651           move '//hov-tstdb01.cso.local:1433;'
002652                                 to p-sql-server
002653     end-evaluate
002654
002655     move 'CheckApproval'        to p-sql-database
002656
002657     CALL 'SQLCONNECT' USING sqlconnect-parms
002658     display ' ret code ' p-connect-return-code
002659     move p-connect-return-code  to sqlcode
002660     move p-sql-return-message   to sqlerrmc
002661
002662*
002663*     EXEC SQL
002664*        CONNECT TO :svr USER :usr-pass
002665*     END-EXEC
002666
002667     if sqlcode not = 0
002668        display "Error: cannot connect "
002669        display sqlcode
002670        display sqlerrmc
002671     end-if
002672
002673     set connected-to-db to true
002674
002675     .
002676 4100-EXIT.
002677     EXIT.
002678
002679 4200-get-tbl-row.
002680
002681***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
002682***                                                            ***
002683***  I'm only expecting one row so no cursor is declared       ***
002684***                                                            ***
002685***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
002686
002687     move pi-company-id          to ws-compid
002688     move ch-carrier             to ws-carrier
002689     move ch-grouping            to ws-grouping
002690     move ch-state               to ws-state
002691     move ch-account             to ws-account
002692     move ch-cert-eff-dt         to dc-bin-date-1
002693     move ' '                    to dc-option-code
002694     perform 8500-date-convert   thru 8500-exit
002695     if no-conversion-error
002696        move dc-greg-date-a-edit to ws-eff-date
002697     end-if
002698     move ch-cert-prime          to ws-certificate
002699     move ch-cert-sfx            to ws-cert-sfx
002700     move ch-sequence-no         to ws-seq-no
002701     move '1'                    to ws-type
002702                                    ws-check-sub-type
002703     if pi-check-type = 'C'
002704        move '2'                 to ws-check-sub-type
002705     end-if
002706
002708     exec sql
              SELECT
002709           ApprovalStatus,
002710           MaintainedBy,
002711           MaintainedDate,
002712           ApprovalBatch
002713        INTO
002714           :db-app-status :nu-app-status,
002715           :db-app-by :nu-app-by,
002716           :db-app-date :nu-app-date,
002717           :db-app-batch :nu-app-batch
002718        FROM
002719           ChkApp_Check
002720        WHERE
002721           Company           = :ws-compid
002722           and CertCarrier   = :ws-carrier
002723           and CertGroup     = :ws-grouping
002724           and CertState     = :ws-state
002725           and CertAccount   = :ws-account
002726           and CertEffDate   = :ws-eff-date
002727           and CertNumber    = :ws-certificate
002728           and CertNumberSuf = :ws-cert-sfx
002729           and CheckSeqNbr   = :ws-seq-no
002730           and CheckType     = :ws-type
002731           and CheckSubType  = :ws-check-sub-type
002732     end-exec
002733
002734     if sqlcode not = 0
002735        display "Error: cannot read row "
002736        display ' sql return code ' sqlcode
002737        display ' sql err mess    ' sqlerrmc
002738        go to 4200-exit
002739     end-if
002740
002741*    display ' status ' db-app-status
002742*    display ' by     ' db-app-by
002743*    display ' date   ' db-app-date
002744*    display ' batch  ' db-app-batch
002745
002746*    if nu-app-date = -1
002747*       display ' approval date is low-values '
002748*    else
002749*       display ' approval date is NOT low values '
002750*    end-if
002751
002752     .
002753 4200-exit.
002754     exit.
002755
002756 4250-delete-row.
002757
002759     EXEC SQL
              DELETE
002760           from ChkApp_Check
002761        WHERE
002762           Company           = :ws-compid
002763           and CertCarrier   = :ws-carrier
002764           and CertGroup     = :ws-grouping
002765           and CertState     = :ws-state
002766           and CertAccount   = :ws-account
002767           and CertEffDate   = :ws-eff-date
002768           and CertNumber    = :ws-certificate
002769           and CertNumberSuf = :ws-cert-sfx
002770           and CheckSeqNbr   = :ws-seq-no
002771           and CheckType     = :ws-type
002772           and CheckSubType  = :ws-check-sub-type
002773     END-EXEC
002774
002775     if sqlcode not = 0
002776        display "Error: cannot delete row  "
002777        display ' sql retrun code ' sqlcode
002778        display ' sql err mess    ' sqlerrmc
002779        go to 4250-exit
002780     end-if
002781
002782     set row-deleted         to true
002783
002786     EXEC SQL
      *        commit transaction
               commit
002787     END-EXEC
002788     if sqlcode not = 0
002789        display "Error: commit "
002790        display ' sql return code ' sqlcode
002791        display ' sql err mess    ' sqlerrmc
002792        go to 4250-exit
002793     end-if
002794
002795     set tbl-commited to true
002796
002797     .
002798 4250-exit.
002799     exit.
002800
002801 4300-FINISH-UP-DB.
002802
002803*    EXEC SQL
002804*        commit work release
002805*    END-EXEC
002807     EXEC SQL
               commit
002808     END-EXEC
002810     EXEC SQL
               disconnect
002811     END-EXEC
002812
002813     if sqlcode not = 0
002814        display "Error: commit release "
002815        display ' sql return code ' sqlcode
002816        display ' sql err mess    ' sqlerrmc
002817     end-if
002818
002819     .
002820 4300-EXIT.
002821     EXIT.
002822
002823 5000-BROWSE-FILE.
002824
002825     
      * EXEC CICS HANDLE CONDITION
002826*        NOTFND   (5560-END-OF-FILE)
002827*        ENDFILE  (5560-END-OF-FILE)
002828*    END-EXEC.
      *    MOVE '"$I''                  ! , #00008078' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303038303738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002829
002830     MOVE PI-ERCHEK-KEY          TO ERCHEK-KEY.
002831
002832     IF SEQI = LOW-VALUES
002833         MOVE +1                 TO CHEK-RECORD-SEQ.
002834
002835     IF EIBAID = DFHPF2
002836         GO TO 5100-BROWSE-BKWD.
002837
002838 5010-READ-LOOP.
002839     IF EIBAID = DFHPF1
002840         ADD +1                  TO CHEK-RECORD-SEQ.
002841
002842     
      * EXEC CICS READ
002843*        DATASET  (ERCHEK-FILE-ID)
002844*        SET      (ADDRESS OF CHECK-RECORDS)
002845*        RIDFLD   (ERCHEK-KEY)
002846*        GTEQ
002847*    END-EXEC.
      *    MOVE '&"S        G          (   #00008095' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303038303935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002848
002849     IF CH-COMPANY-CD NOT = PI-COMPANY-CD
002850         IF EIBAID = DFHENTER
002851             GO TO 5550-NO-RECORD
002852         ELSE
002853             GO TO 5560-END-OF-FILE.
002854
002855
002856        IF PI-CHEK-CARRIER    = CH-CARRIER     AND
002857           PI-CHEK-GROUPING   = CH-GROUPING    AND
002858           PI-CHEK-STATE      = CH-STATE       AND
002859           PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND
002860           PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND
002861           PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND
002862           PI-CHEK-SUF-NO     = CH-CERT-SFX
002863              GO TO 5500-FORMAT-SCREEN
002864            ELSE
002865              IF EIBAID = DFHPF1
002866                  GO TO 5560-END-OF-FILE
002867                ELSE
002868                  GO TO 5550-NO-RECORD.
002869
002870     IF EIBAID = DFHENTER
002871        IF CHEK-CARRIER    = CH-CARRIER     AND
002872           CHEK-GROUPING   = CH-GROUPING    AND
002873           CHEK-STATE      = CH-STATE       AND
002874           CHEK-ACCOUNT    = CH-ACCOUNT     AND
002875           CHEK-EFF-DT     = CH-CERT-EFF-DT AND
002876           CHEK-CERT-NO    = CH-CERT-PRIME  AND
002877           CHEK-SUF-NO     = CH-CERT-SFX    AND
002878           CHEK-RECORD-SEQ = CH-SEQUENCE-NO
002879              GO TO 5500-FORMAT-SCREEN
002880        ELSE
002881              GO TO 5550-NO-RECORD.
002882
002883     GO TO 5500-FORMAT-SCREEN.
002884
002885     EJECT
002886
002887 5100-BROWSE-BKWD.
002888     
      * EXEC CICS STARTBR
002889*        DATASET  (ERCHEK-FILE-ID)
002890*        RIDFLD   (ERCHEK-KEY)
002891*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008141' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038313431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002892
002893     
      * EXEC CICS READPREV
002894*        DATASET  (ERCHEK-FILE-ID)
002895*        SET      (ADDRESS OF CHECK-RECORDS)
002896*        RIDFLD   (ERCHEK-KEY)
002897*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00008146' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038313436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002898
002899 5110-READ-LOOP.
002900     IF PI-FILE-EOF
002901         MOVE SPACE              TO PI-EOF-SW
002902     ELSE
002903         
      * EXEC CICS READPREV
002904*            DATASET  (ERCHEK-FILE-ID)
002905*            SET      (ADDRESS OF CHECK-RECORDS)
002906*            RIDFLD   (ERCHEK-KEY)
002907*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00008156' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038313536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002908
002909     IF CH-COMPANY-CD NOT = PI-COMPANY-CD
002910         GO TO 5560-END-OF-FILE.
002911
002912
002913        IF PI-CHEK-CARRIER    = CH-CARRIER     AND
002914           PI-CHEK-GROUPING   = CH-GROUPING    AND
002915           PI-CHEK-STATE      = CH-STATE       AND
002916           PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND
002917           PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND
002918           PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND
002919           PI-CHEK-SUF-NO     = CH-CERT-SFX
002920              GO TO 5500-FORMAT-SCREEN
002921            ELSE
002922              GO TO 5560-END-OF-FILE.
002923
002924     GO TO 5500-FORMAT-SCREEN.
002925
002926     EJECT
002927*                          COPY ELCNPD.
      *>>((file: ELCNPD))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCNPD                             *
000005*                            VMOD=2.001                         *
000006*****************************************************************
000007
000008 5200-MOVE-NAME.
000009*                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
000010*            NAME TO A WORK AREA WITH NO EMBEDDED               *
000011*            BLANKS.                                            *
000012*                                                               *
000013*                  FIELD               VALUE                    *
000014*                                                               *
000015*                LAST NAME (CL15)      SMITH                    *
000016*                1ST NAME  (CL12)      JOHN                     *
000017*                MID NAME  (CL1)       A                        *
000018*                                                               *
000019*                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
000020*         *                                                     *
000021*         *              JOHN A. SMITH                          *
000022*         *                                                     *
000023*         *      TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
000024*         *  STORAGE COPYBOOK:                                  *
000025*         *                                                     *
000026*         *      01  WS-NAME-WORK-AREA COPY ELCNWA.             *
000027*         *******************************************************.
000028
000029     MOVE SPACES                 TO  WS-NAME-WORK
000030                                     WS-NAME-WORK2.
000031     MOVE ZERO                   TO  WS-NAME-SW.
000032     SET NWA-INDEX TO +1.
000033
000034     IF WS-INSURED-1ST-NAME = SPACES  AND
000035        WS-INSURED-MID-INIT = SPACES
000036         MOVE WS-INSURED-LAST-NAME TO WS-NAME-WORK
000037         GO TO 5200-EXIT.
000038
000039     MOVE WS-INSURED-1ST-NAME    TO  WS-NAME-WORK2.
000040     PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
000041
000042     SET NWA-INDEX UP BY +1
000043     IF WS-INSURED-MID-INIT NOT = SPACES
000044        MOVE WS-INSURED-MID-INIT   TO  WS-NW (NWA-INDEX)
000045        SET NWA-INDEX UP BY +1
000046        MOVE '.'                   TO  WS-NW (NWA-INDEX)
000047        SET NWA-INDEX UP BY +2.
000048
000049     MOVE WS-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
000050     PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
000051
000052
000053 5200-EXIT.
000054     EXIT.
000055
000056     EJECT
000057 5300-MOVE-NAME SECTION.
000058     IF WS-NAME-SW GREATER THAN +1
000059         GO TO 5390-EXIT.
000060
000061     IF WS-NAME-WORK2 = SPACES
000062         GO TO 5390-EXIT.
000063
000064     SET NWA-INDEX2 TO +1.
000065     SET NWA-INDEX3 TO +2.
000066
000067 5310-MOVE-NAME.
000068     MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
000069
000070     IF NWA-INDEX LESS THAN +30
000071        SET NWA-INDEX UP BY +1
000072     ELSE
000073        ADD +2  TO  WS-NAME-SW
000074        GO TO 5390-EXIT.
000075
000076     IF NWA-INDEX2 LESS THAN +20
000077         SET NWA-INDEX2 UP BY +1
000078         SET NWA-INDEX3 UP BY +1.
000079
000080     IF WS-NW2 (NWA-INDEX2) = SPACES AND
000081        WS-NW2 (NWA-INDEX3) = SPACES
000082        GO TO 5390-EXIT.
000083
000084     GO TO 5310-MOVE-NAME.
000085
000086 5390-EXIT.
000087     EXIT.
000088
000089     EJECT
      *<<((file: ELCNPD))
002928     EJECT
002929 5500-FORMAT-SCREEN.
002930
002931     MOVE LOW-VALUES             TO  EL677AI.
002932     if pi-void-reissue-pass = '1'
002933        move 'R'                 to mainti
002934                                    pi-prev-maint
002935     else
002936        MOVE 'S'                 TO MAINTI
002937                                    PI-PREV-MAINT
002938     end-if
002939
002940     MOVE +1                     TO  MAINTL.
002941     MOVE CH-CONTROL-PRIMARY     TO  PI-ERCHEK-KEY.
002942     MOVE CH-CARRIER             TO  CARRIERO.
002943     MOVE CH-GROUPING            TO  GROUPO.
002944     MOVE CH-STATE               TO  STATEO.
002945     MOVE CH-ACCOUNT             TO  ACCTO.
002946
002947     MOVE CH-CERT-EFF-DT         TO  DC-BIN-DATE-1.
002948     MOVE SPACE                  TO  DC-OPTION-CODE.
002949     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
002950     MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.
002951
002952     MOVE CH-CERT-PRIME          TO  CERTNOO.
002953     MOVE CH-CERT-SFX            TO  SFXO.
002954     MOVE CH-SEQUENCE-NO         TO  SEQO.
002955
002956     MOVE AL-UANON               TO  CARRIERA
002957                                     GROUPA
002958                                     STATEA
002959                                     ACCTA
002960                                     EFFDTA
002961                                     CERTNOA
002962                                     SFXA.
002963     MOVE CH-PAYEE-NAME-1         TO PI-PAYTO1.
002964
002965     if ch-approval-dt not = low-values
002966        move ch-approval-dt      to dc-bin-date-1
002967        move ' '                 to dc-option-code
002968        perform 8500-date-convert thru 8500-exit
002969        move dc-greg-date-1-edit to apvdto
002970     end-if
002971     evaluate ch-approval-status
002972        when '2'
002973           move 'PENDING '       TO APVSTATO
002974        when 'P'
002975           move 'PENDING '       TO APVSTATO
002976        when 'A'
002977           move 'APPROVED'       to apvstato
002978        when 'D'
002979           move 'DENIED'         to apvstato
002980     end-evaluate
002981     move ch-approved-by         to apvbyo
002982
002983*    IF PI-COMPANY-ID = 'TMS'
002984*        MOVE CH-PAYEE-NAME-2    TO  PAYTO1O
002985*        MOVE CH-LIENHOLDER-NAME TO  PAYTO1AO
002986*    ELSE
002987         MOVE CH-PAYEE-NAME-1    TO  PAYTO1O
002988         MOVE CH-PAYEE-NAME-2    TO  PAYTO2O.
002989
002990     MOVE CH-PAYEE-ADDRESS-1     TO  PAYAD1O.
002991     MOVE CH-PAYEE-ADDRESS-2     TO  PAYAD2O.
002992     move ch-payee-city          to payctyo
002993     move ch-payee-state         to paysto
002994
002995     MOVE CH-PAYEE-CODE          TO  PAYEEO.
002996
002997     IF CH-PAYEE-ZIP-CODE NOT = ZEROS
002998         MOVE CH-PAYEE-ZIP-CODE  TO  PTOZIPO
002999         MOVE AL-UANON           TO  PTOZIPA.
003000
003001     MOVE CH-RETURN-TO           TO  RETTOO
003002
003003*    if ch-check-cashed-dt not = spaces and low-values
003004*       move ch-check-cashed-dt  to dc-bin-date-1
003005*       MOVE SPACE               TO  DC-OPTION-CODE
003006*       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003007*       MOVE DC-GREG-DATE-1-EDIT TO  CASHEDO
003008*    end-if
003009
003010     MOVE CH-RECORDED-DT         TO  DC-BIN-DATE-1.
003011     MOVE SPACE                  TO  DC-OPTION-CODE.
003012     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
003013     MOVE DC-GREG-DATE-1-EDIT    TO  CREATEDO.
003014
003015     MOVE CH-RECORDED-BY         TO  CBYO.
003016
003017     IF CH-VOID-DT = SPACES OR LOW-VALUES OR ZEROS
003018         NEXT SENTENCE
003019     ELSE
003020         MOVE CH-VOID-DT             TO  DC-BIN-DATE-1
003021         MOVE SPACE                  TO  DC-OPTION-CODE
003022         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003023         MOVE DC-GREG-DATE-1-EDIT    TO  VOIDEDO
003024         MOVE CH-VOID-BY             TO  VBYO.
003025
003026     IF CH-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES OR ZEROS
003027         NEXT SENTENCE
003028     ELSE
003029         MOVE CH-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1
003030         MOVE SPACE                  TO  DC-OPTION-CODE
003031         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
003032         MOVE DC-GREG-DATE-1-EDIT    TO  PRINTEDI.
003033
003034     MOVE CH-AMOUNT-PAID         TO  AMOUNTO
003035                                     pi-void-reissue-amt
003036     MOVE AL-SANOF               TO  AMOUNTA.
003037     MOVE CH-CHECK-ORIGIN-SW     TO  TYPEI.
003038     MOVE AL-SANOF               TO  TYPEA.
003039     MOVE CH-CHECK-NO            TO  CHECKO.
003040     move ch-deduct-commission   to dedcyno
003041     MOVE CH-REASON-FOR-CHECK    TO  REASONO.
003042     MOVE CH-VOID-REASON         TO  VREASONO.
003043
003044     MOVE CH-STUB-LINE-1         TO  STUBO.
003045     MOVE CH-TEXT-LINE-1         TO  TEXT1O.
003046     MOVE CH-TEXT-LINE-2         TO  TEXT2O.
003047     MOVE CH-TEXT-LINE-3         TO  TEXT3O.
003048
003049     IF CH-LF-REFUND NOT NUMERIC
003050         MOVE ZEROS              TO CH-LF-REFUND.
003051     IF CH-AH-REFUND NOT NUMERIC
003052         MOVE ZEROS              TO CH-AH-REFUND.
003053
003054     if (ch-check-no not = spaces and low-values)
003055        and (ch-check-written-dt not = low-values)
003056        perform 5800-fetch-check-cashed-dt
003057                                 thru 5800-exit
003058     end-if
003059
003060     IF CH-CHECK-WRITTEN-DT GREATER LOW-VALUES OR
003061        CH-VOID-DT          GREATER LOW-VALUES
003062        MOVE AL-SANON            TO  CHECKA    REASONA  VREASONA
003063                                     STUBA     TEXT1A   TEXT2A
003064                                     TEXT3A    PAYTO1A  SFXA
003065                                     PAYTO2A   PAYAD1A  PAYctyA
003066                                     paysta    RETTOA   PAYAD2A
003067                                     PTOZIPA
003068                                     PRINTEDA
003069     ELSE
003070        MOVE AL-UANON            TO  CARRIERA  GROUPA   STATEA
003071                                     ACCTA     EFFDTA   CERTNOA
003072                                     CHECKA    REASONA  VREASONA
003073                                     STUBA     TEXT1A   TEXT2A
003074                                     TEXT3A    PAYTO1A  SFXA
003075                                     PAYTO2A   PAYAD1A  PAYctyA
003076                                     paysta    RETTOA   PAYAD2A
003077                                     PRINTEDA.
003078
003079     MOVE AL-UNNON               TO SEQA.
003080
003081     perform 5600-get-erpndb     thru 5600-exit
003082     perform 5700-get-elcert     thru 5700-exit
003083
003084*    move pi-prev-paid           to prepdo
003085
003086     move al-sadof               to drefa
003087
003088*    if WS-PNDB-FOUND
003089*       compute refo = pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
003090*    end-if
003091
003092     if WS-CERT-FOUND
003093        compute ws-tot-lf-prem = cm-lf-premium-amt +
003094           cm-lf-alt-premium-amt
003095        compute ws-tot-iss-prem  =
003096           cm-ah-premium-amt + ws-tot-lf-prem
003097        compute ws-tot-iss-comm =
003098           (ws-tot-lf-prem * cm-life-comm-pct) +
003099           (cm-ah-premium-amt * cm-ah-comm-pct)
003100        compute ws-tot-ref-comm =
003101           (pi-lf-refund * cm-life-comm-pct) +
003102           (pi-ah-refund * cm-ah-comm-pct)
003103        move ws-tot-iss-prem     to premo
003104        move ws-tot-iss-comm     to isscommo
003105*       move ws-tot-ref-comm     to uecommo
003106     end-if
003107
003108     if PI-TO-EL677-FROM-EL1273
003109        move zeros               to premo
003110                                    isscommo
003111                                    uecommo
003112                                    prepdo
003113                                    refo
003114     end-if
003115
003116     GO TO 8100-SEND-INITIAL-MAP
003117
003118     .
003119 5550-NO-RECORD.
003120     MOVE ER-1162                TO  EMI-ERROR.
003121     MOVE -1                     TO  CARRIERL.
003122     MOVE AL-UABON               TO  CARRIERA  GROUPA  STATEA
003123                                     ACCTA     EFFDTA  CERTNOA
003124                                     SFXA      SEQA.
003125     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003126
003127     IF (NOT PI-TO-EL677-FROM-EL1273)
003128        and (pi-return-to-program not = 'EL6315')
003129         GO TO 8200-SEND-DATAONLY.
003130
003131     MOVE CHEK-CARRIER           TO  CARRIERO.
003132     MOVE CHEK-GROUPING          TO  GROUPO.
003133     MOVE CHEK-STATE             TO  STATEO.
003134     MOVE CHEK-ACCOUNT           TO  ACCTO.
003135
003136     MOVE CHEK-EFF-DT            TO  DC-BIN-DATE-1.
003137     MOVE SPACE                  TO  DC-OPTION-CODE.
003138     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
003139     MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.
003140
003141     MOVE CHEK-CERT-NO           TO  CERTNOO.
003142     MOVE CHEK-SUF-NO            TO  SFXO.
003143     GO TO 8100-SEND-INITIAL-MAP.
003144
003145 5560-END-OF-FILE.
003146     IF EIBAID = DFHPF1
003147         MOVE 'Y'                TO  PI-EOF-SW
003148         MOVE ER-2237            TO  EMI-ERROR
003149     ELSE
003150     IF EIBAID = DFHENTER
003151        GO TO 5550-NO-RECORD
003152     ELSE
003153*        MOVE SPACES             TO  PI-ERCHEK-KEY
003154         MOVE ER-2238            TO  EMI-ERROR.
003155
003156     MOVE -1                     TO  MAINTL.
003157     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003158     GO TO 8200-SEND-DATAONLY.
003159     EJECT
003160
003161 5600-get-erpndb.
003162
003163     MOVE 'N'                    TO WS-PNDB-FOUND-SW.
003164
003165     MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.
003166     MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.
003167     MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.
003168     MOVE PI-STATE               TO ERPNDB-ALT-STATE.
003169     MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.
003170     MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.
003171     MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.
003172     MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.
003173     MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.
003174     MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.
003175
003176     IF ST-ACCNT-CNTL  OR
003177        ACCNT-CNTL
003178          MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
003179
003180     IF ST-ACCNT-CNTL      OR
003181        CARR-ST-ACCNT-CNTL OR
003182        ACCNT-CNTL         OR
003183        CARR-ACCNT-CNTL
003184          MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
003185
003186     IF ACCNT-CNTL OR
003187        CARR-ACCNT-CNTL
003188          MOVE SPACES             TO  ERPNDB-ALT-STATE.
003189
003190     
      * EXEC CICS READ
003191*         DATASET   (ERPNDB-ALT-FILE-ID)
003192*         SET       (ADDRESS OF PENDING-BUSINESS)
003193*         RIDFLD    (ERPNDB-ALT-KEY)
003194*         resp      (ws-response)
003195*    END-EXEC
      *    MOVE '&"S        E          (  N#00008534' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038353334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003196
003197     if resp-normal
003198        MOVE 'Y'                 TO WS-PNDB-FOUND-SW
003199        compute pi-refund-on-pending-rec =
003200           pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
003201     end-if
003202
003203     .
003204 5600-exit.
003205     exit.
003206
003207 5700-get-elcert.
003208
003209     MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
003210     MOVE PI-CARRIER             TO  ELCERT-CARRIER.
003211     MOVE PI-GROUPING            TO  ELCERT-GROUPING.
003212     MOVE PI-STATE               TO  ELCERT-STATE.
003213     MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
003214     MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
003215     MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
003216     MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.
003217
003218     MOVE 'N'                    TO  WS-CERT-FOUND-SW.
003219
003220     
      * EXEC CICS READ
003221*         DATASET   (ELCERT-FILE-ID)
003222*         SET       (ADDRESS OF CERTIFICATE-MASTER)
003223*         RIDFLD    (ELCERT-KEY)
003224*         resp      (ws-response)
003225*    END-EXEC
      *    MOVE '&"S        E          (  N#00008564' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038353634' TO DFHEIV0
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
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003226
003227     if resp-normal
003228        MOVE 'Y'                 TO WS-CERT-FOUND-SW
003229     end-if
003230
003231     .
003232 5700-exit.
003233     exit.
003234
003235 5800-fetch-check-cashed-dt.
003236
003237     move ' '                    to ws-connect-sw
003238                                    ws-match-sw
003239                                    pi-check-cashed
003240
003241****  The below code is for when the db has been
003242****  converted to sql server 2016
003243     evaluate ws-kix-myenv
003244        when 'cid1p'
003245           move '//sdv-db01.cso.local:1433;'
003246                                 to p-sql-server
003247        when 'mdoff'
003248           move '//hov-tstdb01.cso.local:55330;'
003249                                 to p-sql-server
003250        when other
003251           move '//hov-tstdb01.cso.local:1433;'
003252                                 to p-sql-server
003253     end-evaluate
003254
003255
003256     move 'PaidBankInfo'         to p-sql-database
003257
003258     CALL 'SQLCONNECT' USING sqlconnect-parms
003259     display ' ret code ' p-connect-return-code
003260     move p-connect-return-code  to sqlcode
003261     move p-sql-return-message   to sqlerrmc
003262
003263
003264
003265     if sqlcode not = 0
003266        display "Error: cannot connect to pdbnkinfo "
003267        display sqlcode
003268        display sqlerrmc
003269        go to 5800-exit
003270     end-if
003271
003272     set connected-to-db to true
003273
003274     if pi-company-id = 'CID'
003275        move 'CSO - AP%'         to ws-pb-compid
003276     else
003277        move 'AHL - AP%'         to ws-pb-compid
003278     end-if
003279
003280     move '000'                  to ws-pb-check-no (1:3)
003281     move ch-check-no            to ws-pb-check-no (4:7)
003282
003283     move spaces                 to ws-pb-bad-check-no
003284     perform varying s1 from +1 by +1 until
003285        (s1 > +10)
003286        or (ws-pb-check-no (s1:1) not = '0')
003287     end-perform
003288
003289     if s1 < +11
003290        move ws-pb-check-no (s1:11 - s1)
003291                            to ws-pb-bad-check-no (1: 11 - s1)
003292     end-if
003293
003294*    display ' compid **' ws-pb-compid '**'
003295*    display ' ckno  **' ws-pb-check-no '**'
003296
003297
003298     MOVE ch-AMOUNT-PAID      TO WS-CHECK-AMT-TMP
003299     MOVE WS-CHECK-AMT-TMPX   TO WS-CHECK-AMOUNT
003300     MOVE SPACES              TO pb-paid-date
003301
003302
003304     EXEC SQL
              CALL pbi_GetCashDate_by_TransactionNbr
003305         (
003306           @compid          = :ws-pb-compid,
003307           @checkno         = :ws-pb-check-no,
003308           @badcheckno      = :ws-pb-bad-check-no,
003309           @checkamount     = :ws-check-amount,
003310           @checkcasheddate = :pb-paid-date :nu-app-date
003311         )
003312     END-EXEC
003313*    display 'sqlcode ' sqlcode
003314*    display ' date   ' pb-paid-date
003315*    display ' nu fld ' nu-app-date
003316
003317     if sqlcode not = 0
003318        display "Error: cannot run stor proc  "
003319        move sqlcode            to ws-sql-code
003320        move ws-sql-code to ws-dis-sql-code
003321        display ' dis sql code ' ws-dis-sql-code
003322        display ' sql return code ' sqlcode
003323        display ' sql err mess    ' sqlerrmc
003324        go to 5800-disconnect
003325     end-if
003326
003327     if nu-app-date = -1 *> no check cashed date
003328        go to 5800-disconnect
003329     end-if
003330
003331***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
003332***                                                            ***
003333***  The following code works just as well as the above        ***
003334***  stored procedure and just as fast.                        ***
003335***  Originally, I had declared a cursor and fetched through   ***
003336***  the record set to find the correct check and that REALLY  ***
003337***  slowed the response down to ~ 3/4 second.                 ***
003338***                                                            ***
003339***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
003340
003341*     EXEC SQL
003342*        SELECT
003343*           PaidDate
003344*        INTO
003345*           :pb-paid-date
003346*        FROM
003347*           dbo.cso_pbi_TransactionLookup
003348*        WHERE
003349*           (BankAcctDescr like :ws-pb-compid)
003350*           and (Amount = :ws-check-amount)
003351*           and ((transactionNbr = :ws-pb-check-no)
003352*                  or
003353*               (transactionnbr = :ws-pb-bad-check-no))
003354**          and (CAST(TransactionNbr AS INT) =
003355**             :ws-pb-check-no-num)
003356**          and (right('00000' + rtrim(TransactionNbr),10)
003357**             = :ws-pb-check-no)
003358**          and (TransactionNbr  = :ws-pb-check-no)
003359*     END-EXEC
003360
003361*    display ' chkno  ' pb-check-no
003362*    display ' desc   ' pb-bank-acct-desc
003363*    display ' trn typ' pb-tran-type
003364*    display ' amt   *' pb-amount '**'
003365*    display ' pd dte ' pb-paid-date
003366
003367     string pb-paid-date (1:4)
003368            pb-paid-date (6:2)
003369            pb-paid-date (9:2)
003370        delimited by size into dc-greg-date-cymd-r
003371     end-string
003372     move 'L'                    to dc-option-code
003373     perform 8500-date-convert   thru 8500-exit
003374     if no-conversion-error
003375*       move dc-bin-date-1       to ws-bin-cashed-dt
003376        move dc-greg-date-1-edit to cashedo
003377        move 'Y' to pi-check-cashed
003378     else
003379        display ' error cvtdte cash dt ' pb-paid-date ' '
003380           dc-error-code
003381     end-if
003382
003383     .
003384 5800-disconnect.
003385
003387     EXEC SQL
              DISCONNECT ALL
003388     END-EXEC
003389
003390     if sqlcode not = 0
003391        display "Error: cannot disconnect pdbnk "
003392        display ' sql return code ' sqlcode
003393        display ' sql err mess    ' sqlerrmc
003394     end-if
003395
003396     .
003397 5800-exit.
003398     exit.
003399
003400 6000-VERIFY-COMP-MASTER.
003401     
      * EXEC CICS HANDLE CONDITION
003402*        NOTFND   (6070-NO-COMP-MSTR)
003403*    END-EXEC.
      *    MOVE '"$I                   ! - #00008745' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303038373435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003404
003405     MOVE PI-COMPANY-CD          TO  ERCOMP-COMP-CD.
003406
003407     IF NOT PI-ZERO-CARRIER AND
003408        NOT PI-ZERO-CAR-GROUP
003409         MOVE PI-CR-CARRIER      TO  ERCOMP-CARRIER
003410     ELSE
003411         MOVE ZEROS              TO  ERCOMP-CARRIER.
003412
003413     IF NOT PI-ZERO-GROUPING  AND
003414        NOT PI-ZERO-CAR-GROUP
003415         MOVE PI-CR-GROUPING     TO  ERCOMP-GROUPING
003416     ELSE
003417         MOVE ZEROS              TO  ERCOMP-GROUPING.
003418
003419     MOVE PI-CR-FIN-RESP         TO  ERCOMP-FIN-RESP.
003420     MOVE PI-CR-ACCOUNT          TO  ERCOMP-ACCOUNT.
003421     MOVE 'A'                    TO  ERCOMP-RECORD-TYPE.
003422
003423     
      * EXEC CICS READ
003424*         DATASET   (ERCOMP-FILE-ID)
003425*         SET       (ADDRESS OF COMPENSATION-MASTER)
003426*         RIDFLD    (ERCOMP-KEY)
003427*     END-EXEC.
      *    MOVE '&"S        E          (   #00008767' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038373637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003428
003429     GO TO 6090-EXIT.
003430
003431 6070-NO-COMP-MSTR.
003432     MOVE -1                     TO  CARRIERL.
003433     MOVE ER-2230                TO  EMI-ERROR.
003434     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003435     GO TO 8200-SEND-DATAONLY.
003436
003437 6090-EXIT.
003438      EXIT.
003439     EJECT
003440 6100-VERIFY-CERTIFICATE.
003441
003442     
      * EXEC CICS HANDLE CONDITION
003443*        NOTFND   (6170-NO-CERTIFICATE)
003444*    END-EXEC.
      *    MOVE '"$I                   ! . #00008786' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303038373836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003445
003446     MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
003447     MOVE PI-CARRIER             TO  ELCERT-CARRIER.
003448     MOVE PI-GROUPING            TO  ELCERT-GROUPING.
003449     MOVE PI-STATE               TO  ELCERT-STATE.
003450     MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
003451     MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
003452     MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
003453     MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.
003454
003455     MOVE 'N'                    TO  WS-CERT-FOUND-SW.
003456
003457     MOVE SPACES                 TO  PI-REFERENCE
003458*                                    PI-INSURED-NAME.
003459     MOVE LOW-VALUES             TO  PI-CANC-DT.
003460*    MOVE ZEROS                  TO  PI-LF-REFUND
003461*                                    PI-AH-REFUND.
003462
003463     
      * EXEC CICS READ
003464*         DATASET   (ELCERT-FILE-ID)
003465*         SET       (ADDRESS OF CERTIFICATE-MASTER)
003466*         RIDFLD    (ELCERT-KEY)
003467*     END-EXEC.
      *    MOVE '&"S        E          (   #00008807' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038383037' TO DFHEIV0
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
           
003468
003469     MOVE 'Y'                    TO  WS-CERT-FOUND-SW.
003470
003471*    MOVE CM-LF-ITD-CANCEL-AMT   TO  PI-LF-REFUND.
003472*    MOVE CM-AH-ITD-CANCEL-AMT   TO  PI-AH-REFUND.
003473     IF CM-LF-CANCEL-DT NOT = LOW-VALUES
003474         MOVE CM-LF-CANCEL-DT    TO  PI-CANC-DT
003475     ELSE
003476         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
003477             MOVE CM-AH-CANCEL-DT TO PI-CANC-DT.
003478*    MOVE CM-INSURED-LAST-NAME   TO  PI-INSURED-NAME.
003479
003480     GO TO 6190-EXIT.
003481
003482 6170-NO-CERTIFICATE.
003483     MOVE -1                     TO  CARRIERL.
003484     MOVE ER-2726                TO  EMI-ERROR.
003485     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003486
003487 6190-EXIT.
003488      EXIT.
003489
003490     EJECT
003491 6200-UPDATE-PENDING-BUS-REC.
003492
003493     MOVE PI-COMPANY-CD          TO  ERPNDB-ALT-COMPANY-CD.
003494     MOVE PI-CARRIER             TO  ERPNDB-ALT-CARRIER.
003495     MOVE PI-GROUPING            TO  ERPNDB-ALT-GROUPING.
003496     MOVE PI-STATE               TO  ERPNDB-ALT-STATE.
003497     MOVE PI-ACCOUNT             TO  ERPNDB-ALT-ACCOUNT.
003498     MOVE PI-CERT-PRIME          TO  ERPNDB-ALT-CERT-PRIME.
003499     MOVE PI-CERT-SFX            TO  ERPNDB-ALT-CERT-SFX.
003500     MOVE PI-CERT-EFF-DT         TO  ERPNDB-ALT-CERT-EFF-DT.
003501     MOVE ZEROS                  TO  ERPNDB-ALT-CH-SEQ-NO.
003502     MOVE '2'                    TO  ERPNDB-ALT-RECORD-TYPE.
003503
003504     IF ST-ACCNT-CNTL  OR
003505        ACCNT-CNTL
003506          MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
003507
003508     IF ST-ACCNT-CNTL      OR
003509        CARR-ST-ACCNT-CNTL OR
003510        ACCNT-CNTL         OR
003511        CARR-ACCNT-CNTL
003512          MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
003513
003514     IF ACCNT-CNTL OR
003515        CARR-ACCNT-CNTL
003516          MOVE SPACES             TO  ERPNDB-ALT-STATE.
003517
003518     
      * EXEC CICS READ
003519*         DATASET   (ERPNDB-ALT-FILE-ID)
003520*         SET       (ADDRESS OF PENDING-BUSINESS)
003521*         RIDFLD    (ERPNDB-ALT-KEY)
003522*         resp      (ws-response)
003523*     END-EXEC.
      *    MOVE '&"S        E          (  N#00008862' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038383632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003524
003525     if not resp-normal
003526        go to 6200-exit
003527     end-if
003528
003529     MOVE PB-CONTROL-PRIMARY     TO  ERPNDB-PRIMARY-KEY.
003530
003531     
      * EXEC CICS READ
003532*         DATASET   (ERPNDB-FILE-ID)
003533*         SET       (ADDRESS OF PENDING-BUSINESS)
003534*         RIDFLD    (ERPNDB-PRIMARY-KEY)
003535*         UPDATE
003536*         resp      (ws-response)
003537*     END-EXEC.
      *    MOVE '&"S        EU         (  N#00008875' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303038383735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-PRIMARY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003538
003539 6200-EXIT.
003540      EXIT.
003541
003542 6210-rewrite-pndb.
003543
003544     
      * EXEC CICS REWRITE
003545*        DATASET  (ERPNDB-FILE-ID)
003546*        FROM     (PENDING-BUSINESS)
003547*    END-EXEC
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008888' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038383838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003548
003549     .
003550 6210-exit.
003551     exit.
003552
003553 6300-BUILD-CERT-SCREEN.
003554
003555*    move 'N'                    to dedcyni
003556*                                   pi-prev-ded-comm
003557     MOVE LOW-VALUES             TO  EL677AI.
003558     perform 6700-build-common   thru 6700-exit
003559
003560     if ws-refund-amount = zeros
003561        move pi-company-cd       to pi-chek-comp-cd
003562        move pi-carrier          to pi-chek-carrier
003563        move pi-grouping         to pi-chek-grouping
003564        move pi-state            to pi-chek-state
003565        move pi-account          to pi-chek-account
003566        move pi-cert-eff-dt      to pi-chek-eff-dt
003567        move pi-cert-prime       to pi-chek-cert-no
003568        move pi-cert-sfx         to pi-chek-suf-no
003569        move +1                  to pi-chek-sequence
003570
003571             MOVE 'S'            TO MAINTI
003572             MOVE 1              TO MAINTL
003573             MOVE AL-UABON       TO MAINTA
003574             MOVE DFHENTER       TO EIBAID
003575
003576
003577        go to 5000-browse-file
003578     end-if
003579
003580     IF WS-CERT-NOT-FOUND
003581         GO TO 8100-SEND-INITIAL-MAP.
003582
003583*    IF WS-PNDB-FOUND
003584*        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +
003585*                                   PB-C-AH-CANCEL-AMT
003586*        MOVE 'R'                TO  TYPEO
003587*        IF PI-AR-PROCESSING
003588*            MOVE AL-UANON       TO  REFA
003589*            MOVE PB-C-REFERENCE TO  REFO
003590*        END-IF
003591*    ELSE
003592*        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +
003593*                                   CM-AH-ITD-CANCEL-AMT.
003594
003595     MOVE WS-REFUND-AMOUNT        TO AMOUNTO
003596     MOVE pi-check-type           TO TYPEO
003597
003598     IF PI-COMPANY-ID = 'TMS'
003599         MOVE WS-NAME-WORK         TO PAYTO1I
003600                                      PI-PAYTO1
003601         MOVE AL-UANON             TO PAYTO1A
003602     ELSE
003603         MOVE WS-NAME-WORK         TO PAYTO1I
003604                                      PI-PAYTO1
003605         MOVE AL-UANON             TO PAYTO1A.
003606
003607*    MOVE CM-BENEFICIARY           TO PAYTO1AI.
003608*    MOVE AL-UANON                 TO PAYTO1AA.
003609
003610     move 'N'                    to dedcyni
003611     move al-uanon               to dedcyna
003612     move 'INS'                  to payeeo
003613     move al-panon               to payeea
003614
003615     IF PI-MAIL-YES
003616         PERFORM 6400-MAILING-ADDRESS THRU 6490-EXIT.
003617
003618     GO TO 8100-SEND-INITIAL-MAP.
003619
003620     EJECT
003621
003622 6400-MAILING-ADDRESS.
003623     
      * EXEC CICS HANDLE CONDITION
003624*        NOTFND   (6470-NO-ADDRESS)
003625*    END-EXEC.
      *    MOVE '"$I                   ! / #00008967' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303038393637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003626
003627     
      * EXEC CICS READ
003628*        DATASET   (ERMAIL-FILE-ID)
003629*        SET       (ADDRESS OF MAILING-DATA)
003630*        RIDFLD    (ELCERT-KEY)
003631*    END-EXEC.
      *    MOVE '&"S        E          (   #00008971' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038393731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003632
003633     MOVE MA-ADDRESS-LINE-1      TO PAYAD1I.
003634     MOVE MA-ADDRESS-LINE-2      TO PAYAD2I.
003635     move ma-city                to payctyi
003636     move ma-addr-state          to paysti
003637
003638     MOVE MA-ZIP                 TO PTOZIPI.
003639     MOVE AL-UANON               TO PAYAD1A
003640                                    PAYAD2A
003641                                    PAYctyA
003642                                    paysta
003643                                    PTOZIPA.
003644
003645     GO TO 6490-EXIT.
003646
003647 6470-NO-ADDRESS.
003648     MOVE -1                     TO  PAYAD1L.
003649     MOVE ER-2394                TO  EMI-ERROR.
003650     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003651
003652 6490-EXIT.
003653      EXIT.
003654
003655     EJECT
003656
003657 6500-BUILD-ACCOUNT-SCREEN.
003658
003659     MOVE LOW-VALUES             TO  EL677AI.
003660     perform 6700-build-common   thru 6700-exit
003661
003662     MOVE WS-REFUND-AMOUNT       TO AMOUNTO
003663     MOVE PI-AM-NAME             TO PAYTO1I
003664     MOVE SPACES                 TO PAYTO2I
003665     MOVE PI-AM-ADDRS            TO PAYAD1I.
003666     MOVE PI-AM-CITY             TO PAYCTYI
003667     move pi-am-st               to paysti
003668     MOVE PI-AM-ZIP-CODE         TO PTOZIPI
003669     move 'ACCT'                 to payeeo
003670     move al-panon               to payeea
003671
003672     MOVE AL-UANON               TO PAYTO1A
003673                                    PAYTO2A
003674                                    PAYAD1A
003675                                    PAYCTYA
003676                                    paysta
003677                                    PTOZIPA
003678
003679     GO TO 8100-SEND-INITIAL-MAP
003680
003681     .
003682 6600-BUILD-BENEFICIARY-SCREEN.
003683
003684     MOVE LOW-VALUES             TO  EL677AI.
003685     perform 6700-build-common   thru 6700-exit
003686
003687     IF WS-CERT-NOT-FOUND
003688         GO TO 8100-SEND-INITIAL-MAP.
003689
003690*    IF WS-PNDB-FOUND
003691*        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +
003692*                                   PB-C-AH-CANCEL-AMT
003693*        MOVE 'R'                TO  TYPEO
003694*        IF PI-AR-PROCESSING
003695*            MOVE AL-UANON       TO  REFA
003696*            MOVE PB-C-REFERENCE TO  REFO
003697*        END-IF
003698*    ELSE
003699*        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +
003700*                                   CM-AH-ITD-CANCEL-AMT.
003701
003702     MOVE WS-REFUND-AMOUNT        TO  AMOUNTO.
003703     MOVE pi-check-type           TO  TYPEO.
003704
003705     IF PI-COMPANY-ID = 'TMS'
003706         MOVE WS-NAME-WORK         TO PAYTO1I
003707                                      PI-PAYTO1
003708         MOVE AL-UANON             TO PAYTO1A
003709     ELSE
003710         MOVE WS-NAME-WORK         TO PAYTO1I
003711                                      PI-PAYTO1
003712         MOVE AL-UANON             TO PAYTO1A.
003713
003714*    MOVE CM-BENEFICIARY           TO PAYTO1AI.
003715*    MOVE AL-UANON                 TO PAYTO1AA.
003716
003717     move 'N'                    to dedcyni
003718     move al-uanon               to dedcyna
003719     move 'BENE'                 to payeeo
003720     move al-panon               to payeea
003721     IF PI-MAIL-YES
003722         PERFORM 6620-MAILING-ADDRESS THRU 6690-EXIT.
003723
003724     GO TO 8100-SEND-INITIAL-MAP.
003725
003726     EJECT
003727
003728 6620-MAILING-ADDRESS.
003729     
      * EXEC CICS HANDLE CONDITION
003730*        NOTFND   (6670-NO-beneficiary)
003731*    END-EXEC.
      *    MOVE '"$I                   ! 0 #00009073' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303039303733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003732
003733     
      * EXEC CICS READ
003734*        DATASET   (ERMAIL-FILE-ID)
003735*        SET       (ADDRESS OF MAILING-DATA)
003736*        RIDFLD    (ELCERT-KEY)
003737*    END-EXEC.
      *    MOVE '&"S        E          (   #00009077' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039303737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003738
003739     move spaces                 to payto2i
003740     move ma-cred-bene-name      to payto1i
003741     MOVE MA-cred-bene-addr      TO PAYad1I.
003742     MOVE MA-cred-bene-addr2     TO PAYad2I.
003743     move ma-cred-bene-city      to payctyi
003744     move ma-cred-bene-state     to paysti
003745
003746     MOVE MA-cred-bene-zip       TO PTOZIPI.
003747     MOVE AL-UANON               TO payto1a
003748                                    PAYTO2A
003749                                    PAYad1A
003750                                    payad2a
003751                                    PAYctyA
003752                                    paysta
003753                                    PTOZIPA.
003754
003755     GO TO 6690-EXIT.
003756
003757 6670-NO-BENEFICIARY.
003758
003759     MOVE -1                     TO  PAYTO1L.
003760     MOVE ER-2394                TO  EMI-ERROR.
003761     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003762
003763 6690-EXIT.
003764      EXIT.
003765
003766 6700-build-common.
003767
003768     if not eracct-found
003769        move pi-company-cd       to eracct-co
003770        move pi-carrier          to eracct-carrier
003771        move pi-grouping         to eracct-grouping
003772        move pi-state            to eracct-state
003773        move pi-account          to eracct-account
003774        move pi-cert-eff-dt      to eracct-exp-date
003775        perform 1420-get-eracct  thru 1490-exit
003776     end-if
003777*    if WS-PNDB-NOT-FOUND
003778*       perform 5600-get-erpndb thru 5600-exit
003779*    end-if
003780
003781*    MOVE LOW-VALUES             TO  EL677AI.
003782     MOVE 'A'                    TO  MAINTI.
003783     MOVE AL-UANON               TO  MAINTA.
003784     MOVE +1                     TO  MAINTL.
003785
003786     MOVE PI-CARRIER             TO  CARRIERO.
003787     MOVE PI-GROUPING            TO  GROUPO.
003788     MOVE PI-STATE               TO  STATEO.
003789     MOVE PI-ACCOUNT             TO  ACCTO.
003790     MOVE PI-CERT-PRIME          TO  CERTNOO.
003791     MOVE PI-CERT-SFX            TO  SFXO.
003792     MOVE PI-AMOUNT              TO  AMOUNTO.
003793*    MOVE PI-TYPE                TO  TYPEO.
003794     move pi-check-type          to  typeo
003795     move pi-return-to           to  rettoo
003796*    MOVE PI-REFERENCE           TO  REFO.
003797     MOVE PI-PAYTO1              TO  PAYTO1O.
003798     MOVE AL-UANON               TO  CARRIERA
003799                                     GROUPA
003800                                     STATEA
003801                                     ACCTA
003802                                     CERTNOA
003803                                     SFXA
003804                                     PAYTO1A
003805                                     PAYTO2A
003806*                                    AMOUNTA
003807*                                    TYPEA
003808                                     rettoa
003809                                     payeea.
003810     move al-panon               to typea
003811     move al-panon               to amounta
003812     MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.
003813     MOVE ' '                    TO  DC-OPTION-CODE.
003814     PERFORM 8500-DATE-CONVERT.
003815
003816     IF NO-CONVERSION-ERROR
003817         MOVE DC-GREG-DATE-1-EDIT TO  EFFDTI
003818         MOVE AL-UANON            TO  EFFDTA
003819         MOVE 'Y'                 TO  PI-PROCESS-CERT-SW
003820         PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT
003821     ELSE
003822         GO TO 8200-SEND-DATAONLY.
003823
003824     IF CM-INSURED-FIRST-NAME GREATER SPACES
003825         MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME
003826         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT
003827     ELSE
003828         MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME
003829         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.
003830
003831     MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.
003832
003833     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
003834
003835
003836     move spaces to pi-insured-name
003837     string cm-insured-last-name ' '
003838            cm-insured-first-name delimited by '  '
003839        into pi-insured-name
003840     end-string
003841
003842     move spaces                 to pi-table-name
003843     string cm-insured-last-name ', '
003844            cm-insured-first-name ' '
003845            cm-insured-initial2
003846        delimited by '  '
003847        into
003848           pi-table-name
003849     end-string
003850*    MOVE WS-NAME-WORK           TO PI-INSURED-NAME.
003851
003852     if eibaid = dfhpf5
003853        move 'Y'                 to dedcyni
003854        move al-uanon            to dedcyna
003855     end-if
003856
003857    compute ws-tot-lf-prem = cm-lf-premium-amt +
003858       cm-lf-alt-premium-amt
003859    compute ws-tot-iss-prem  =
003860       cm-ah-premium-amt + ws-tot-lf-prem
003861    compute ws-tot-iss-comm =
003862       (ws-tot-lf-prem * cm-life-comm-pct) +
003863       (cm-ah-premium-amt * cm-ah-comm-pct)
003864    compute ws-tot-ref-comm =
003865       (pi-lf-refund * cm-life-comm-pct) +
003866       (pi-ah-refund * cm-ah-comm-pct)
003867
003868     move ws-tot-iss-prem        to premo
003869     move ws-tot-iss-comm        to isscommo
003870     move al-sadof               to drefa
003871                                    refa
003872*    compute refo = pi-lf-refund + pi-ah-refund
003873     move pi-prev-paid            to prepdo
003874     move ws-tot-ref-comm         to uecommo
003875                                     pi-ue-comm
003876
003877***  compute ws-refund-amount = pi-refund-on-pending-rec -
003878***     pi-prev-paid
003879     compute ws-refund-amount =
003880        pi-refund-on-pending-rec - pi-prev-paid-this-month
003881
003882     if (pi-refund-on-pending-rec + pi-prev-paid) >
003883        ws-tot-iss-prem
003884        move zeros               to ws-refund-amount
003885     end-if
003886
003887     if ((dedcyni = 'Y')
003888        or (pi-prev-ded-comm = 'Y'))
003889        and (ws-refund-amount >= ws-tot-ref-comm)
003890        compute ws-refund-amount = ws-refund-amount -
003891           ws-tot-ref-comm
003892     end-if
003893
003894     if pi-check-type = 'C'
003895*       move pi-endt-prm-diff    to refo
003896        move pi-endt-com-diff    to uecommo
003897                                    pi-ue-comm
003898        compute ws-refund-amount =
003899           pi-endt-prm-diff - pi-prev-paid
003900        if (dedcyni = 'Y')
003901           or (pi-prev-ded-comm = 'Y')
003902           compute ws-refund-amount = ws-refund-amount -
003903              pi-endt-com-diff
003904        end-if
003905        if ws-refund-amount <= zeros
003906           move pi-endt-prm-diff to ws-refund-amount
003907        end-if
003908     end-if
003909
003910     if not WS-CERT-NOT-FOUND
003911        string
003912           pi-cert-prime         ' : '
003913           cm-insured-last-name  ', '
003914           cm-insured-first-name ' : '
003915           pi-account            ' : '
003916           pi-am-csr delimited by '  ' into text1o
003917        end-string
003918        move al-uanon            to text1a
003919        MOVE 'REF'               TO WS-USER-REASON
003920        if pi-check-type = 'C'
003921           move 'COR'            to ws-user-reason
003922        end-if
003923        MOVE pi-cert-no          TO WS-USER-CERT-NO
003924        inspect ws-user-cert-no replacing leading zeros
003925           by spaces
003926        move pi-insured-name     to ws-user-name
003927
003928        perform varying s1 from +1 by +1 until
003929           (s1 > +11)
003930           or (ws-user-cert-no (s1:1)) <> ' '
003931        end-perform
003932        string ws-user-reason  ' ' delimited by size
003933               ws-user-cert-no (s1:11 - s1 + 1) ' '
003934                  delimited by size
003935               cm-insured-last-name ' '
003936               cm-insured-first-name delimited by '  '
003937           into stubo
003938        end-string
003939        move al-uanon            to stuba
003940     end-if
003941
003942     .
003943 6700-exit.
003944     exit.
003945
003946 6800-browse-previous-chek-recs.
003947
003948***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
003949***                                                            ***
003950**** this should be the first time through, we need to gather  ***
003951**** all the erchek records for this pending record, add the   ***
003952**** amounts and make sure this check request will not exceed  ***
003953**** the collected premium                                     ***
003954***                                                            ***
003955***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
003956
003957     move zeros                  to pi-prev-paid
003958                                    pi-prev-paid-this-month
003959                                    ws-browse-sw
003960                                    pi-chek-rec-cnt
003961     move spaces                 to pi-prev-ded-comm
003962     move pi-company-cd          to chek-company-cd
003963     move pi-carrier             to chek-carrier
003964     move pi-grouping            to chek-grouping
003965     move pi-state               to chek-state
003966     move pi-account             to chek-account
003967     move pi-cert-prime          to chek-cert-no
003968     move pi-cert-sfx            to chek-suf-no
003969     move pi-cert-eff-dt         to chek-eff-dt
003970     move +0                     to chek-record-seq
003971     move erchek-key             to ws-compare-erchek-key
003972
003973     
      * EXEC CICS STARTBR
003974*       DATASET  (ERCHEK-FILE-ID)
003975*       RIDFLD   (ERCHEK-KEY)
003976*       resp     (ws-response)
003977*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00009317' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303039333137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003978
003979     if not resp-normal
003980        go to 6800-exit
003981     end-if
003982
003983     perform until i-say-when
003984        
      * EXEC CICS READNEXT
003985*          DATASET   (ERCHEK-FILE-ID)
003986*          SET       (ADDRESS OF CHECK-RECORDS)
003987*          RIDFLD    (ERCHEK-KEY)
003988*          resp      (ws-response)
003989*       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00009328' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303039333238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003990        if resp-normal
003991           and erchek-key (1:33) = ws-compare-erchek-key
003992           if (ch-void-dt = low-values)
003993              and (not ch-denied)
003994              if pi-check-type = 'R'
003995                 and ch-check-origin-sw = 'C'
003996                 continue
003997              else
003998                 compute pi-prev-paid = pi-prev-paid +
003999                    ch-amount-paid
004000                 add +1 to pi-chek-rec-cnt
004001                 move ch-deduct-commission
004002                                 to pi-prev-ded-comm
004003                 if ch-credit-select-dt = pi-cr-month-end-dt
004004                    compute pi-prev-paid-this-month =
004005                       pi-prev-paid-this-month + ch-amount-paid
004006                 end-if
004007              end-if
004008           end-if
004009        else
004010           set i-say-when to true
004011        end-if
004012     end-perform
004013
004014     .
004015 6800-exit.
004016     exit.
004017
004018 7000-SET-PI-AREA.
004019     MOVE CARRIERI               TO  PI-CARRIER
004020                                     PI-CHEK-CARRIER.
004021     MOVE GROUPI                 TO  PI-GROUPING
004022                                     PI-CHEK-GROUPING.
004023     MOVE STATEI                 TO  PI-STATE
004024                                     PI-CHEK-STATE.
004025     MOVE ACCTI                  TO  PI-ACCOUNT
004026                                     PI-CHEK-ACCOUNT.
004027     MOVE CERTNOI                TO  PI-CERT-PRIME
004028                                     PI-CHEK-CERT-NO.
004029     IF SFXI NOT = LOW-VALUES
004030         MOVE SFXI               TO  PI-CERT-SFX
004031                                     PI-CHEK-SUF-NO
004032     ELSE
004033         MOVE SPACE              TO  PI-CERT-SFX
004034                                     PI-CHEK-SUF-NO.
004035
004036     MOVE EFFDTI                 TO  DC-GREG-DATE-1-EDIT.
004037     MOVE '2'                    TO  DC-OPTION-CODE.
004038     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
004039     IF DATE-CONVERSION-ERROR
004040         MOVE ER-0215            TO  EMI-ERROR
004041         MOVE -1                 TO  EFFDTL
004042         MOVE AL-UNBON           TO  EFFDTA
004043         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004044         GO TO 8200-SEND-DATAONLY
004045     ELSE
004046         MOVE DC-BIN-DATE-1      TO  PI-CERT-EFF-DT
004047                                     PI-CHEK-EFF-DT.
004048
004049 7090-EXIT.
004050      EXIT.
004051     EJECT
004052 7100-CREATE-TEMP-STORAGE.
004053     PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.
004054
004055     
      * EXEC CICS WRITEQ TS
004056*        QUEUE   (QID)
004057*        FROM    (PROGRAM-INTERFACE-BLOCK)
004058*        LENGTH  (PI-COMM-LENGTH)
004059*    END-EXEC.
      *    MOVE '*"     L              ''   #00009399' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039333939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004060
004061 7100-EXIT.
004062      EXIT.
004063
004064 7200-RECOVER-TEMP-STORAGE.
004065     
      * EXEC CICS READQ TS
004066*        QUEUE   (QID)
004067*        INTO    (PROGRAM-INTERFACE-BLOCK)
004068*        LENGTH  (PI-COMM-LENGTH)
004069*    END-EXEC.
      *    MOVE '*$I    L              ''   #00009409' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004070
004071     PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.
004072
004073 7200-EXIT.
004074      EXIT.
004075
004076 7300-DELETE-TEMP-STORAGE.
004077     
      * EXEC CICS HANDLE CONDITION
004078*        QIDERR  (7300-EXIT)
004079*    END-EXEC.
      *    MOVE '"$N                   ! 1 #00009421' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303039343231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004080
004081     
      * EXEC CICS DELETEQ TS
004082*        QUEUE  (QID)
004083*    END-EXEC.
      *    MOVE '*&                    #   #00009425' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039343235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004084
004085 7300-EXIT.
004086      EXIT.
004087     EJECT
004088
004089 7500-reissue-pass-1.
004090
004091     move '2'                    to pi-void-reissue-pass
004092
004093     move pi-void-reissue-amt    to amounto
004094
004095     if eibaid = dfhpf5
004096        MOVE PI-AM-NAME          TO PAYTO1I
004097        MOVE SPACES              TO PAYTO2I
004098        MOVE PI-AM-ADDRS         TO PAYAD1I
004099        MOVE PI-AM-CITY          TO PAYCTYI
004100        move pi-am-st            to paysti
004101        MOVE PI-AM-ZIP-CODE      TO PTOZIPI
004102        move 'ACCT'              to payeeo
004103        move al-panon            to payeea
004104
004105        MOVE AL-UANON            TO PAYTO1A
004106                                    PAYTO2A
004107                                    PAYAD1A
004108                                    PAYCTYA
004109                                    paysta
004110                                    PTOZIPA
004111                                    payeea
004112        GO TO 8100-SEND-INITIAL-MAP
004113     end-if
004114
004115     perform 5700-get-elcert     thru 5700-exit
004116     move spaces                 to pi-table-name
004117     string cm-insured-last-name ', '
004118            cm-insured-first-name ' '
004119            cm-insured-initial2
004120        delimited by '  '
004121        into
004122           pi-table-name
004123     end-string
004124
004125     IF CM-INSURED-FIRST-NAME GREATER SPACES
004126         MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME
004127         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT
004128     ELSE
004129         MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME
004130         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.
004131
004132     MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.
004133
004134     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
004135
004136     if eibaid = dfhpf7
004137        
      * EXEC CICS READ
004138*           DATASET   (ERMAIL-FILE-ID)
004139*           SET       (ADDRESS OF MAILING-DATA)
004140*           RIDFLD    (ELCERT-KEY)
004141*           resp      (ws-response)
004142*       END-EXEC
      *    MOVE '&"S        E          (  N#00009481' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039343831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004143        if resp-normal
004144           move spaces             to payto2i
004145           move ma-cred-bene-name  to payto1i
004146           MOVE MA-cred-bene-addr  TO PAYad1I
004147           MOVE MA-cred-bene-addr2 TO PAYad2I
004148           move ma-cred-bene-city  to payctyi
004149           move ma-cred-bene-state to paysti
004150           MOVE MA-cred-bene-zip   TO PTOZIPI
004151           move 'BENE'              to payeeo
004152           MOVE AL-UANON           TO payto1a
004153                                      PAYTO2A
004154                                      PAYad1A
004155                                      payad2a
004156                                      PAYctyA
004157                                      paysta
004158                                      PTOZIPA
004159                                      payeea
004160        end-if
004161     end-if
004162
004163     if eibaid = dfhpf6
004164        
      * EXEC CICS READ
004165*           DATASET   (ERMAIL-FILE-ID)
004166*           SET       (ADDRESS OF MAILING-DATA)
004167*           RIDFLD    (ELCERT-KEY)
004168*           resp      (ws-response)
004169*       END-EXEC
      *    MOVE '&"S        E          (  N#00009508' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039353038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004170        if resp-normal
004171           move spaces            to payto2i
004172           move ws-name-work      to payto1i
004173           MOVE MA-ADDRESS-LINE-1 TO PAYAD1I
004174           MOVE MA-ADDRESS-LINE-2 TO PAYAD2I
004175           move ma-city           to payctyi
004176           move ma-addr-state     to paysti
004177           move 'INS'             to payeeo
004178           MOVE MA-ZIP            TO PTOZIPI
004179           MOVE AL-UANON          TO PAYAD1A
004180                                     PAYAD2A
004181                                     PAYctyA
004182                                     paysta
004183                                     PTOZIPA
004184                                     payto1a
004185                                     payeea
004186        end-if
004187     end-if
004188
004189     GO TO 8100-SEND-INITIAL-MAP
004190
004191     .
004192 7500-exit.
004193     exit.
004194
004195 7510-reissue-pass-2.
004196
004197     
      * EXEC CICS READ
004198*        DATASET  (ERCHEK-FILE-ID)
004199*        SET      (ADDRESS OF CHECK-RECORDS)
004200*        RIDFLD   (PI-ERCHEK-KEY)
004201*        resp     (ws-response)
004202*    END-EXEC
      *    MOVE '&"S        E          (  N#00009541' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039353431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004203
004204     if not resp-normal
004205        go to 7510-exit
004206     end-if
004207
004208     if ch-void-dt not = save-bin-date
004209        display ' something went wrong here '
004210        go to 7510-exit
004211     end-if
004212
004213     MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS
004214     MOVE save-bin-date          TO CH-RECORDED-DT
004215                                    ch-released-dt
004216     move pi-processor-id        to ch-recorded-by
004217                                    ch-released-by
004218                                    ch-return-to
004219     move spaces                 to ch-check-no
004220                                    ch-void-by
004221                                    ch-void-reason
004222                                    ch-approved-by
004223
004224
004225     move 'P'                    to ch-approval-status
004226
004227     MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT
004228                                    CH-VOID-DT
004229                                    CH-CREDIT-ACCEPT-DT
004230                                    ch-approval-dt
004231                                    ch-check-cashed-dt
004232     add +1 to ch-sequence-no
004233     if ch-check-origin-sw = 'R'
004234        move 'R'                 to pi-check-type
004235     else
004236        move 'C'                 to pi-check-type
004237     end-if
004238
004239     .
004240 7510-WRITE-RECORD.
004241
004242     
      * exec cics read
004243*       dataset    (erchek-file-id)
004244*       into       (ws-dummy-erchek)
004245*       ridfld     (ch-control-primary)
004246*       resp       (ws-response)
004247*    end-exec
           MOVE LENGTH OF
            ws-dummy-erchek
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00009586' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039353836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 erchek-file-id, 
                 ws-dummy-erchek, 
                 DFHEIV11, 
                 ch-control-primary, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004248
004249     if RESP-NOTFND
004250        continue
004251     else
004252        add +1                   to ch-sequence-no
004253        add 1                    to seqi
004254        go to 7510-write-record
004255     end-if
004256
004257     MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.
004258     MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL
004259                                    CH-CHECK-QUE-SEQUENCE
004260
004261     IF PAYTO1L NOT = ZEROS
004262        MOVE PAYTO1I             TO CH-PAYEE-NAME-1
004263     END-IF
004264     IF PAYTO2L NOT = ZEROS
004265        MOVE PAYTO2I             TO CH-PAYEE-NAME-2
004266     end-if
004267     IF PAYAD1L NOT = ZEROS
004268        MOVE PAYAD1I             TO CH-PAYEE-ADDRESS-1
004269     end-if
004270     IF PAYAD2L NOT = ZEROS
004271        MOVE PAYAD2I             TO CH-PAYEE-ADDRESS-2
004272     end-if
004273     IF PAYctyL NOT = ZEROS
004274        MOVE payctyi             TO CH-PAYEE-CITY
004275     end-if
004276     IF PAYstL NOT = ZEROS
004277        MOVE paysti              TO CH-PAYEE-state
004278     end-if
004279     IF PAYEEI GREATER SPACES
004280        mOVE PAYEEI               TO CH-PAYEE-CODE
004281     end-if
004282     IF PTOZIPL  =  ZEROS
004283        MOVE ZEROS               TO CH-PAYEE-ZIP-CODE
004284     else
004285        MOVE PTOZIPI             TO WS-ZIP-CODE
004286        IF NOT WS-CANADIAN-POST-CODE
004287           MOVE WS-ZIP-PRIME     TO CH-PAYEE-ZIP
004288           MOVE WS-ZIP-PLUS4     TO CH-PAYEE-ZIP-EXT
004289        end-if
004290     end-if
004291
004292     PERFORM 2700-WRITE-SQL      THRU 2700-EXIT
004293
004294     
      * EXEC CICS WRITE
004295*        DATASET  (ERCHEK-FILE-ID)
004296*        FROM     (CHECK-RECORDS)
004297*        RIDFLD   (CH-CONTROL-PRIMARY)
004298*        resp     (ws-response)
004299*    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00009638' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303039363338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 CH-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004300
004301     COMPUTE WS-JOURNAL-RECORD-LENGTH =
004302             ERCHEK-RECORD-LENGTH + 23.
004303
004304     IF EMI-NO-ERRORS
004305        MOVE ER-0000             TO EMI-ERROR
004306     ELSE
004307        IF EMI-FORCABLE-CTR GREATER THAN +0  AND
004308           EIBAID = DFHPF4
004309           MOVE ER-2600          TO EMI-ERROR
004310        end-if
004311     end-if
004312
004313     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004314
004315     MOVE LOW-VALUES             TO EL677AI.
004316
004317     MOVE PI-CHEK-CARRIER        TO CARRIERO.
004318     MOVE PI-CHEK-GROUPING       TO GROUPO.
004319     MOVE PI-CHEK-STATE          TO STATEO.
004320     MOVE PI-CHEK-ACCOUNT        TO ACCTO.
004321
004322     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
004323     MOVE SPACE                  TO DC-OPTION-CODE.
004324     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
004325     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
004326
004327     MOVE PI-CHEK-CERT-NO        TO CERTNOO.
004328     MOVE PI-CHEK-SUF-NO         TO SFXO.
004329     MOVE PI-CHEK-SEQUENCE       TO SEQO.
004330
004331     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
004332                                    ACCTA     EFFDTA  CERTNOA
004333                                    SFXA.
004334     MOVE AL-UNNON               TO SEQA
004335     move ' ' to pi-void-reissue-pass
004336     go to 5000-browse-file
004337
004338     .
004339 7510-exit.
004340     exit.
004341
004342 8100-SEND-INITIAL-MAP.
004343     MOVE SAVE-DATE              TO  DATEO.
004344     MOVE EIBTIME                TO  TIME-IN.
004345     MOVE TIME-OUT               TO  TIMEO.
004346     MOVE -1                     TO  MAINTL
004347     move al-uanon               to mainta
004348
004349     move dedcyni                to pi-previous-deduct-comm
004350     if (PI-TO-EL677-FROM-EL1273)
004351        and (pi-void-reissue-pass = ' ')
004352        move al-sadof            to pf3a
004353                                    pf4a
004354                                    pf57a
004355                                    pf6a
004356                                    dprema
004357                                    dcomma
004358                                    drefa
004359                                    duecomma
004360                                    dprepda
004361                                    prema
004362                                    isscomma
004363                                    refa
004364                                    uecomma
004365                                    prepda
004366        move ', VOID(V)'         to dmaint1o
004367        move 'VOID & REISSUE(R), DELETE(D)'
004368                                 TO dmaint2o
004369     end-if
004370
004371     if (pi-to-el677-from-el1273)
004372        and (pi-void-reissue-pass = '1')
004373        move al-sadof            to pf3a
004374                                    pf4a
004375                                    dprema
004376                                    dcomma
004377                                    drefa
004378                                    duecomma
004379                                    dprepda
004380                                    prema
004381                                    isscomma
004382                                    refa
004383                                    uecomma
004384                                    prepda
004385        move ', VOID(V)'         to dmaint1o
004386        move 'VOID AND REISSUE(R)'
004387                                 TO dmaint2o
004388     end-if
004389
004390     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
004391     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
004392
004393     
      * EXEC CICS SEND
004394*        MAP      (EL677A)
004395*        MAPSET   (MAPSET-NAME)
004396*        FROM     (EL677AO)
004397*        ERASE
004398*        CURSOR
004399*    END-EXEC.
           MOVE LENGTH OF
            EL677AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009737' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303039373337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL677A, 
                 EL677AO, 
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
           
004400
004401     GO TO 9100-RETURN-TRAN.
004402     EJECT
004403 8200-SEND-DATAONLY.
004404
004405     if connected-to-db
004406        perform 4300-FINISH-UP-DB thru 4300-exit
004407     end-if
004408
004409     MOVE SAVE-DATE              TO  DATEO.
004410     MOVE EIBTIME                TO  TIME-IN.
004411     MOVE TIME-OUT               TO  TIMEO.
004412     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
004413     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
004414
004415     
      * EXEC CICS SEND
004416*        MAP     (EL677A)
004417*        MAPSET  (MAPSET-NAME)
004418*        FROM    (EL677AO)
004419*        DATAONLY
004420*        CURSOR
004421*    END-EXEC.
           MOVE LENGTH OF
            EL677AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009759' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303039373539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL677A, 
                 EL677AO, 
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
           
004422
004423     GO TO 9100-RETURN-TRAN.
004424     EJECT
004425 8300-SEND-TEXT.
004426     
      * EXEC CICS SEND TEXT
004427*        FROM     (LOGOFF-TEXT)
004428*        LENGTH   (LOGOFF-LENGTH)
004429*        ERASE
004430*        FREEKB
004431*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009770' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303039373730' TO DFHEIV0
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
           
004432
004433     
      * EXEC CICS RETURN
004434*    END-EXEC.
      *    MOVE '.(                    ''   #00009777' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039373737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004435
004436 8500-DATE-CONVERT.
004437     MOVE LINK-ELDATCV           TO  PGM-NAME.
004438
004439     
      * EXEC CICS LINK
004440*        PROGRAM    (PGM-NAME)
004441*        COMMAREA   (DATE-CONVERSION-DATA)
004442*        LENGTH     (DC-COMM-LENGTH)
004443*    END-EXEC.
      *    MOVE '."C                   (   #00009783' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039373833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004444
004445 8500-EXIT.
004446      EXIT.
004447
004448 8600-DEEDIT.
004449     
      * EXEC CICS BIF DEEDIT
004450*        FIELD   (WS-DEEDIT-FIELD)
004451*        LENGTH  (10)
004452*    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009793' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039373933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004453
004454 8600-EXIT.
004455      EXIT.
004456
004457 8700-DEEDIT.
004458     
      * EXEC CICS BIF DEEDIT
004459*        FIELD   (WS-DT-DEEDIT-FIELD)
004460*        LENGTH  (10)
004461*    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009802' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039383032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DT-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004462
004463 8700-EXIT.
004464      EXIT.
004465
004466 8800-UNAUTHORIZED-ACCESS.
004467     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
004468     GO TO 8300-SEND-TEXT.
004469
004470 8810-PF23.
004471     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
004472     MOVE XCTL-005               TO  PGM-NAME.
004473     GO TO 9300-XCTL.
004474
004475
004476 9100-RETURN-TRAN.
004477     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
004478     MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
004479     
      * EXEC CICS RETURN
004480*        TRANSID  (TRANS-ID)
004481*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
004482*        LENGTH   (PI-COMM-LENGTH)
004483*    END-EXEC.
      *    MOVE '.(CT                  ''   #00009823' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004484
004485     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL677' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
004486
004487 9200-RETURN-MAIN-MENU.
004488     MOVE XCTL-626               TO  PGM-NAME.
004489     GO TO 9300-XCTL.
004490
004491 9300-XCTL.
004492     
      * EXEC CICS XCTL
004493*        PROGRAM    (PGM-NAME)
004494*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
004495*        LENGTH     (PI-COMM-LENGTH)
004496*    END-EXEC.
      *    MOVE '.$C                   %   #00009836' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039383336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004497
004498 9400-CLEAR.
004499     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
004500     MOVE SPACES                 TO  PI-PFKEY.
004501     GO TO 9300-XCTL.
004502
004503 9500-PF12.
004504     MOVE XCTL-010               TO  PGM-NAME.
004505     GO TO 9300-XCTL.
004506
004507 9600-PGMID-ERROR.
004508     
      * EXEC CICS HANDLE CONDITION
004509*        PGMIDERR    (8300-SEND-TEXT)
004510*    END-EXEC.
      *    MOVE '"$L                   ! 2 #00009852' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303039383532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004511
004512     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
004513     MOVE ' '                    TO  PI-ENTRY-CD-1.
004514     MOVE XCTL-005               TO  PGM-NAME.
004515     MOVE PGM-NAME               TO  LOGOFF-PGM.
004516     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
004517
004518     GO TO 9300-XCTL.
004519
004520 9900-ERROR-FORMAT.
004521     IF NOT EMI-ERRORS-COMPLETE
004522         MOVE LINK-001           TO  PGM-NAME
004523         
      * EXEC CICS LINK
004524*            PROGRAM    (PGM-NAME)
004525*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
004526*            LENGTH     (EMI-COMM-LENGTH)
004527*        END-EXEC.
      *    MOVE '."C                   (   #00009867' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004528
004529 9900-EXIT.
004530      EXIT.
004531
004532 9990-ABEND.
004533     MOVE LINK-004               TO  PGM-NAME.
004534     MOVE DFHEIBLK               TO  EMI-LINE1.
004535     
      * EXEC CICS LINK
004536*        PROGRAM   (PGM-NAME)
004537*        COMMAREA  (EMI-LINE1)
004538*        LENGTH    (72)
004539*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009879' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039383739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004540
004541     GO TO 8200-SEND-DATAONLY.
004542
004543 9995-SECURITY-VIOLATION.
004544*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00009906' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039393036' TO DFHEIV0
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
004545
004546 9995-EXIT.
004547     EXIT.
004548

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL677' TO DFHEIV1
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
               GO TO 8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1290-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1380-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1480-ACCOUNT-INVALID,
                     1480-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1590-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2200-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3900-RECORD-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3287-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 3300-EXIT,
                     3300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5560-END-OF-FILE,
                     5560-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6070-NO-COMP-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6170-NO-CERTIFICATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6470-NO-ADDRESS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6670-NO-beneficiary
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL677' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
