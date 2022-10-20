      *((program: SOCK11.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003 PROGRAM-ID. SOCK11.
000004 AUTHOR. Cowtown.
000005 DATE-COMPILED.
000006*SECURITY.   *****************************************************
000007*            *                                                   *
000008*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
000009*            *                                                   *
000010*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000011*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
000012*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
000013*            *                                                   *
000014*            *****************************************************
000015
000016******************************************************************
000017*REMARKS.                                                        *
000018*  This program gets kicked off by C# APP SpecAcctHandling via   *
000019*     Transaction SO11. Pulls all table rows from table          *
000020*     RefSpecHand in the Logic DB into an array. Next reads the  *
000021*     account master (ERACCT) one record at a time, compares to  *
000022*     all entries in array and updates the ERACCT accordingly.   *
000023******************************************************************
000024*                   C H A N G E   L O G
000025*
000026* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000027*-----------------------------------------------------------------
000028*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000029* EFFECTIVE    NUMBER
000030*-----------------------------------------------------------------
000031* 080818   2018040600002   PEMA  New Program
000032* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
000033******************************************************************
000034 ENVIRONMENT DIVISION.
000035 data division.
000036 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000037 77  FILLER  PIC X(32) VALUE '********************************'.
000038 77  FILLER  PIC X(32) VALUE '   SOCK11   WORKING STORAGE     '.
000039 77  FILLER  PIC X(32) VALUE '********************************'.
000040*
000041* program buffers
000042*
000043 77 ws-send-msg-size             pic s9(8) comp value 256.
000044 77 ws-recv-msg-size             pic s9(8) comp value 256.
000045 77 ws-recv-buf                  pic x(256).
000046 77 ws-send-buf                  pic x(256) VALUE SPACES.
000047 77 ws-recv-total                pic s9(8) comp value 0.
000048 77 ws-recv-left                 pic s9(8) comp value 0.
000049 77 ws-seq-num                   pic s9(8) comp value 0.
000050 77 ws-flags                     pic s9(8) comp value 0.
000051 77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
000052 77  ws-comp-id                  pic xxx value spaces.
000053 77 X1                           PIC S999 COMP-3 VALUE +0.
000054 77 S1                           PIC S999 COMP-3 VALUE +0.
000055 77 S2                           PIC S999 COMP-3 VALUE +0.
000056 77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
000057 77  WS-ERACCT-SW                PIC X VALUE ' '.
000058     88  END-OF-ERACCT                 VALUE 'Y'.
000059 77  ws-socket-sw                pic x value ' '.
000060     88  end-of-socket              value 'Y'.
000061 77  ws-browse-sw                pic x value ' '.
000062     88  browse-started            value 'Y'.
000063 77  ws-connect-sw               pic x value ' '.
000064     88  connected-to-db           value 'Y'.
000065 77  ws-bin-current-dt           pic xx value low-values.
000066 77  WS-MATCH-SW                 PIC X value ' '.
000067     88  found-match               value 'Y'.
000068 77  ws-acct-reads               pic 9(7) value zeros.
000069 77  ws-acnt-reads               pic 9(7) value zeros.
000070 77  ws-acnt-updates             pic 9(7) value zeros.
000071 77  ws-acnt-deletes             pic 9(7) value zeros.
000072 77  ws-batch-job-sw             pic x value spaces.
000073     88  batch-job                  value 'Y'.
000074
000075 01  P pointer.
000076 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000077 01  var-ptr pointer.
000078 01  env-var-len                 pic 9(4)  binary.
000079 01  rc                          pic 9(9)  binary.
000080
000081 01  ws-save-report-code-1       pic x(10) value spaces.
000082 01  ws-save-report-code-2       pic x(10) value spaces.
000083 01  ws-save-report-code-3       pic x(10) value spaces.
000084 01  ws-save-user-select-2       pic x(10) value spaces.
000085 01  ws-save-user-select-5       pic x(10) value spaces.
000086
000087 01  ws-save-eracct-key.
000088     05  ws-save-comp-cd         pic x.
000089     05  ws-save-carrier         pic x.
000090     05  f                       pic x(6).
000091     05  ws-save-state           pic xx.
000092     05  ws-save-account         pic x(10).
000093
000094 01  WS-KIXSYS.
000095     05  WS-KIX-FIL1             PIC X(10).
000096     05  WS-KIX-APPS             PIC X(10).
000097     05  WS-KIX-ENV              PIC X(10).
000098     05  WS-KIX-MYENV            PIC X(10).
000099     05  WS-KIX-SYS              PIC X(10).
000100
000101*EXEC SQL
000102*   INCLUDE SQLDA
000103*END-EXEC
000104
000107*EXEC SQL
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
000108
000110 EXEC SQL
          BEGIN DECLARE SECTION
000111 END-EXEC
000112
000113 01  ws-error-message            pic x(50) value spaces.
000114 01  ws-status-date              pic x(10).
000115 01  sqlcmd                      pic x(1024).
000116 01  svr                         pic x(32).
000117 01  usr                         pic x(32).
000118 01  pass                        pic x(32).
000119 01  usr-pass                    pic x(64).
000120 01  ws-disp-code                pic s9(11).
000121 01  ws-display-response         pic s9(9) value zeros.
000122
000123***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000124***                                                            ***
000125***  These indicators are used to determine if a variable      ***
000126***  is        null.           The indicator will be -1        ***
000127***  if the value        is null  and +0 if the value is       ***
000128***  something other than null.  Here is an example on how     ***
000129***  to use the indicator variables.                           ***
000130***                                                            ***
000131***     EXEC SQL                                               ***
000132***        fetch checkapp into                                 ***
000133***           :db-app-status :nu-app-status,                   ***
000134***           :db-app-by     :nu-app-by,                       ***
000135***           :db-app-date   :nu-app-date,                     ***
000136***           :db-app-batch  :nu-app-batch                     ***
000137***     END-EXEC                                               ***
000138***                                                            ***
000139***           OR This way on an update                         ***
000140***                                                            ***
000141***     EXEC SQL                                               ***
000142***        UPDATE                                              ***
000143***           CUC_Logic_Remittance                             ***
000144***        SET                                                 ***
000145***           LogicStatus     = :ws-status-code,               ***
000146***           LogicStatusDate = :ws-status-date,               ***
000147***           BatchNumber     = :ws-batch-no :nu-batchno       ***
000148***        WHERE                                               ***
000149***           RemitId = :ws-remit-id                           ***
000150***     END-EXEC                                               ***
000151***                                                            ***
000152***    Also, when the table has a column with a data type of   ***
000153***  "BIT" and used as true/false move the 1 byte receiving    ***
000154***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
000155***  then its false. I think true would be 256.                ***
000156***                                                            ***
000157***                                                            ***
000158***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000159
000160 01  ws-bit                      pic x.
000161 01  ws-bit-comp redefines ws-bit pic s9(4) comp.
000162
000163 01  indicator-vaiables-for-nulls.
000164     05  nu-report-code1         pic s9(4) comp value +0.
000165     05  nu-report-code2         pic s9(4) comp value +0.
000166     05  nu-report-code3         pic s9(4) comp value +0.
000167     05  nu-user-select2         pic s9(4) comp value +0.
000168     05  nu-user-select5         pic s9(4) comp value +0.
000169     05  nu-carrier              pic s9(4) comp value +0.
000170     05  nu-state                pic s9(4) comp value +0.
000171     05  nu-account              pic s9(4) comp value +0.
000172     05  nu-status               pic s9(4) comp value +0.
000173
000174 01  table-row.
000175     05  tr-report-code1         pic x(10).
000176     05  tr-report-code2         pic x(10).
000177     05  tr-report-code3         pic x(10).
000178     05  tr-user-select2         pic x(10).
000179     05  tr-user-select5         pic x(10).
000180     05  tr-carrier              pic x.
000181     05  tr-state                pic xx.
000182     05  tr-account              pic x(10).
000183     05  tr-status               pic x.
000184
000186 EXEC SQL
          END DECLARE SECTION
000187 END-EXEC
000188
000189 01  a1                          pic s9(5) comp-3 value +0.
000190 01  ma1                         pic s9(5) comp-3 value +0.
000191 01  sql-table.
000192     05  sql-table-array occurs 3000.
000193         10  a1-report-code1     pic x(10).
000194         10  a1-report-code2     pic x(10).
000195         10  a1-report-code3     pic x(10).
000196         10  a1-user-select2     pic x(10).
000197         10  a1-user-select5     pic x(10).
000198         10  a1-carrier          pic x.
000199         10  a1-state            pic xx.
000200         10  a1-account          pic x(10).
000201         10  a1-status           pic x.
000202
000203 01  soc-client-in-data          pic x(50).
000204
000205 01  WS-AM-KEY.
000206     05  WS-AM-COMPANY-CD        PIC X.
000207     05  WS-AM-CARRIER           PIC X.
000208     05  WS-AM-GROUP             PIC X(6).
000209     05  WS-AM-STATE             PIC XX.
000210     05  WS-AM-ACCOUNT           PIC X(10).
000211     05  WS-AM-EXP-DT            PIC XX.
000212     05  FILLER                  PIC X(4).
000213
000214 01  WS-NT-KEY.
000215     05  WS-NT-COMPANY-CD        PIC X.
000216     05  WS-NT-CARRIER           PIC X.
000217     05  WS-NT-GROUP             PIC X(6).
000218     05  WS-NT-STATE             PIC XX.
000219     05  WS-NT-ACCOUNT           PIC X(10).
000220     05  WS-NT-rec-type          PIC X.
000221     05  ws-nt-seq-no            pic s9(4) comp.
000222
000223 01  ws-return-string.
000224     05  ws-return-error-no      pic x(4).
000225     05  ws-sc1                  pic x.
000226     05  ws-return-error-mess    pic x(45).
000227     05  ws-sc2                  pic x.
000228     05  ws-return-accts-read    pic zzzzzz9.
000229     05  ws-sc3                  pic x.
000230     05  ws-return-acnts-read    pic zzzzzz9.
000231     05  ws-sc4                  pic x.
000232     05  ws-return-acnts-updated pic zzzzzz9.
000233     05  ws-sc5                  pic x.
000234     05  ws-return-acnts-deletes pic zzzzzz9.
000235
000236 01  WS-CID-NO                   PIC X(8).
000237
000238 01  WS-DISP-RESP                PIC 9(5).
000239 01  WS-RESPONSE                 PIC S9(8)   COMP.
000240     88  RESP-NORMAL                  VALUE +00.
000241     88  RESP-NOTFND                  VALUE +13.
000242     88  resp-duprec                  value +14.
000243     88  resp-dupkey                  value +15.
000244     88  RESP-NOTOPEN                 VALUE +19.
000245     88  RESP-ENDFILE                 VALUE +20.
000246
000247*                                 COPY ERCACCT.
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
000248*                                 copy ERCACNT.
      *>>((file: ERCACNT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACNT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 120   RECFORM = FIXED                          *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*                                                                *
000017*   LOG = YES                                                    *
000018*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
000028*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
000029******************************************************************
000030 01  NOTE-FILE.
000031     12  NT-FILE-ID                  PIC XX.
000032         88  VALID-NOTE-ID              VALUE 'NT'.
000033
000034     12  NT-CONTROL-PRIMARY.
000035         16  NT-COMPANY-CD           PIC X.
000036         16  NT-ACCT-NOTE-KEY.
000037             18  NT-CARRIER              PIC X.
000038             18  NT-GROUPING             PIC X(06).
000039             18  NT-STATE                PIC XX.
000040             18  NT-ACCOUNT              PIC X(10).
000041         16  NT-RECORD-TYPE          PIC X.
000042              88  ACCT-NOTE          VALUE '1'.
000043              88  ACCT-BRANCH-LOC    VALUE '2'.
000044              88  ACCT-SHIPPING-ADDR VALUE '3'.
000045         16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
000046
000047     12  NT-LAST-MAINT-DT            PIC XX.
000048     12  NT-LAST-MAINT-BY            PIC X(4).
000049     12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
000050
000051*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
000052*     SEQUENCE NUMBERS
000053     12  NT-NOTE-INFORMATION.
000054         16  NT-NOTE-LINE            PIC X(60).
000055         16  FILLER                  PIC X(25).
000056*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
000057     12  NT-LOCATION-INFORMATION REDEFINES
000058                         NT-NOTE-INFORMATION.
000059         16  NT-BRANCH-LOC-LINE      PIC X(60).
000060         16  FILLER                  PIC X(25).
000061* Account special indicator is record type '2', sequence 3
000062     12  filler REDEFINES NT-NOTE-INFORMATION.
000063         16  nt-account-special      PIC X.
000064         16  FILLER                  PIC X(84).
000065*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
000066*     SEQUENCE NUMBER 1 IS NAME LINE 1
000067*     SEQUENCE NUMBER 2 IS NAME LINE 2
000068*     SEQUENCE NUMBER 3 IS ADDR LINE 1
000069*     SEQUENCE NUMBER 4 IS ADDR LINE 2
000070*     SEQUENCE NUMBER 5 IS ADDR LINE 3
000071*     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
000072     12  NT-SHIPPING-INFORMATION REDEFINES
000073                         NT-NOTE-INFORMATION.
000074         16  NT-SHIPPING-LINE        PIC X(60).
000075         16  NT-SHIP-STATE           PIC XX.
000076         16  NT-SHIP-ZIP             PIC X(10).
000077         16  FILLER                  PIC X(13).
000078*****************************************************************
      *<<((file: ERCACNT))
000249*                                 COPY ELCFUNDT.
      *>>((file: ELCFUNDT))
000001*****************************************************************
000002*                                                               *
000003*                            ELCFUNDT.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE *
000005*                            VMOD=2.001                         *
000006*                                                               *
000007*           COPYBOOK FOR THE FUNCTION DATE FORMAT               *
000008*                                                               *
000009*****************************************************************
000010
000011
000012 01  FUNCTION-DATE.
000013     05  WS-FN-DATE                PIC 9(8)    VALUE ZEROS.
000014     05  WS-FN-CYMD  REDEFINES  WS-FN-DATE.
000015         10  WS-FN-CCYR            PIC 9(4).
000016         10  WS-FN-CCYY  REDEFINES  WS-FN-CCYR.
000017             15  WS-FN-CC          PIC 99.
000018             15  WS-FN-YR          PIC 99.
000019         10  WS-FN-MO              PIC 99.
000020         10  WS-FN-DA              PIC 99.
000021     05  WS-FN-HOURS               PIC 99      VALUE ZEROS.
000022     05  WS-FN-MINUTES             PIC 99      VALUE ZEROS.
000023     05  WS-FN-SECONDS             PIC 99      VALUE ZEROS.
000024     05  WS-FN-HUNDSECS            PIC 99      VALUE ZEROS.
000025     05  WS-FN-GMT-IND             PIC X       VALUE SPACES.
000026         88  WS-BEHIND-GMT                     VALUE '-'.
000027         88  WS-AFTER-GMT                      VALUE '+'.
000028         88  WS-NO-GMT                         VALUE ZERO.
000029     05  WS-FN-GMT-HOURS           PIC 99      VALUE ZEROS.
000030     05  WS-FN-GMT-MINUTES         PIC 99      VALUE ZEROS.
000031
      *<<((file: ELCFUNDT))
000250*                                 COPY ELCDATE.
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
000251
000252 01  sqlconnect-parms.
000253     05  p-sql-server            PIC X(30).
000254     05  p-sql-database          PIC X(30).
000255     05  p-connect-return-code   pic s9(5) comp-5.
000256     05  p-sql-return-message    pic x(256).
000257
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
000259 01  DFHCOMMAREA.
000260   05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
000261   05 LSTN-NAME                PIC X(8).
000262   05 LSTN-SUBNAME             PIC X(8).
000263****   client-in-data must be 36 characters.  ****
000264   05 CLIENT-IN-DATA.
000265      15  CLIENT-KICK-OFF      PIC X(8).
000266      15  CLIENT-ID            PIC XXX.
000267      15  FILLER               PIC X(25).
000268   05 SOCKADDR-IN-PARM.
000269     15 SIN-FAMILY             PIC 9(4) COMP.
000270     15 SIN-PORT               PIC 9(4) COMP.
000271     15 SIN-ADDRESS            PIC 9(8) COMP.
000272     15 SIN-ZERO               PIC X(8).
000273
000274 01  var  pic x(30).
000275
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'SOCK11' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000276 VCOBOL-DUMMY-PROCEDURE.
000277
000278     display 'SOCK11:transaction data =', CLIENT-IN-DATA '**'
000279     display 'SOCK11:socket number    =', GIVE-TAKE-SOCKET.
000280     display 'SOCK11:socket name      =', lstn-name ' '
000281        lstn-subname
000282
000283     perform 0000-init           thru 0000-exit
000284     perform 0010-init-contact   thru 0010-exit
000285*    perform 0020-receive        thru 0020-exit
000286*    if end-of-socket
000287*       display ' Premature End of Socket '
000288*       go to 0300-close-socket
000289*    end-if
000290     perform 0030-build-array-from-sql
000291                                 thru 0030-exit
000292     perform 0040-process-eracct thru 0040-exit
000293*    display ' acct reads   ' ws-acct-reads
000294*    display ' acnt reads   ' ws-acnt-reads
000295*    display ' acnt updates ' ws-acnt-updates
000296*    display ' acnt deletes ' ws-acnt-deletes
000297     perform 0070-format-buffer  thru 0070-exit
000298     perform 0200-send-buffer    thru 0200-exit
000299
000300     go to 0300-close-socket
000301
000302     .
000303 0000-init.
000304
000305     MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
000306     move ws-fn-cymd             to dc-greg-date-cymd
000307     move 'L'                    to dc-option-code
000308     perform 9700-date-link      thru 9700-exit
000309     if no-conversion-error
000310        move dc-bin-date-1       to ws-bin-current-dt
000311     else
000312        display ' error current dt invalid ' dc-error-code
000313     end-if
000314
000315*    display ' current date/time '
000316*       ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
000317*          ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
000318
000319     move spaces                 to sql-table
000320                                    ws-return-string
000321     move ';'                    to ws-sc1
000322                                    ws-sc2
000323                                    ws-sc3
000324                                    ws-sc4
000325                                    ws-sc5
000326     move zeros                  to ws-return-accts-read
000327                                    ws-return-acnts-read
000328                                    ws-return-acnts-updated
000329                                    ws-return-acnts-deletes
000330
000331     set P to address of KIXSYS
000332     CALL "getenv" using by value P returning var-ptr
000333     if var-ptr = null then
000334        display ' kixsys not set '
000335     else
000336        set address of var to var-ptr
000337        move 0 to env-var-len
000338        inspect var tallying env-var-len
000339          for characters before X'00'
000340        unstring var (1:env-var-len) delimited by '/'
000341           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000342              WS-KIX-SYS
000343        end-unstring
000344     end-if
000345
000346     .
000347 0000-exit.
000348     exit.
000349
000350 0010-init-contact.
000351
000352     if client-kick-off = 'SOCKET11'
000353        continue
000354     else
000355*       display ' Must be from batch job '
000356        set batch-job to true
000357*       move '9999'              to ws-return-error-no
000358*       move 'Unknown origin, who are you? '
000359*                                to ws-return-error-mess
000360*       PERFORM 0200-SEND-BUFFER thru 0200-exit
000361*       display ' Unknown origin ' client-in-data
000362*       go to 0300-close-socket
000363     end-if
000364
000365     MOVE X'04'                  TO WS-COMP-CD
000366     MOVE 'CID'                  TO WS-COMP-ID
000367
000368*    move 'SOCKET11READY'        to ws-send-buf
000369*    move +25                    to ws-send-msg-size
000370*
000371*    display 'SOCK11:sequence number  =', ws-seq-num.
000372*    display 'SOCK11:send buffer      =', ws-send-buf(1:25)
000373*
000374*    call "send" using by value GIVE-TAKE-SOCKET,
000375*        by reference ws-send-buf,
000376*        by value ws-send-msg-size,
000377*        by value ws-flags
000378*
000379*    if return-code <= zero
000380*       display 'SOCK11:send error ' return-code
000381*       perform 0250-socket-error thru 0250-exit
000382*       go to 0300-close-socket
000383*    end-if
000384
000385     .
000386 0010-exit.
000387     exit.
000388
000389 0020-receive.
000390
000391*    display 'SOCK11:About to recv '
000392
000393     call "recv" using by value GIVE-TAKE-SOCKET
000394         by reference ws-recv-buf
000395         by value ws-recv-msg-size
000396         by value ws-flags.
000397
000398     if return-code < zero
000399        display 'SOCK11:recv error ' return-code
000400        perform 0250-socket-error thru 0250-exit
000401        set end-of-socket to true
000402        go to 0010-exit
000403     end-if
000404
000405     if return-code = zero
000406        display 'SOCK11:client disconnected',
000407        perform 0250-socket-error thru 0250-exit
000408        set end-of-socket to true
000409        go to 0010-exit
000410     end-if
000411
000412*    display 'SOCK11:Good recv  '
000413*    display 'SOCK11:return code      = ', return-code
000414*    display 'SOCK11:receive buffer   = ', ws-recv-buf(1:51)
000415
000416     move +50                    to ws-send-msg-size
000417
000418     move ws-recv-buf (1:50)     to soc-client-in-data
000419
000420     if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
000421        set end-of-socket to true
000422     end-if
000423
000424     .
000425 0020-exit.
000426     exit.
000427
000428 0030-build-array-from-sql.
000429
000430     if not connected-to-db
000431        perform 6000-connect-to-db thru 6000-exit
000432     end-if
000433
000435     EXEC SQL
              DECLARE
000436           getSpecInstr cursor for
000437        SELECT
000438           ReportCode1,
000439           ReportCode2,
000440           ReportCode3,
000441           UserSelect2,
000442           UserSelect5,
000443           Carrier,
000444           State,
000445           Account,
000446           Status
000447        FROM
000448           RefSpecHand
000449     end-exec
000450
000451     if sqlcode not = 0
000452        display "Error: cannot declare cursor "
000453        display ' sql return code ' sqlcode
000454        display ' sql err mess    ' sqlerrmc
000455        move sqlcode             to ws-display-response
000456        move '9999'              to ws-return-error-no
000457        move 'Could not declare cursor'
000458                                 to ws-return-error-mess
000459        PERFORM 0200-SEND-BUFFER thru 0200-exit
000460        go to 0300-close-socket
000461     end-if
000462
000464     EXEC SQL
              open getSpecInstr
000465     END-EXEC
000466
000467     if sqlcode not = 0
000468        display "Error: cannot open cursor "
000469        display ' sql retrun code ' sqlcode
000470        display ' sql err mess    ' sqlerrmc
000471        move '9999'              to ws-return-error-no
000472        move 'Could not Open cursor'
000473                                 to ws-return-error-mess
000474        PERFORM 0200-SEND-BUFFER thru 0200-exit
000475        go to 0300-close-socket
000476     end-if
000477
000478     perform until sqlcode not = 0
000480        EXEC SQL
                 fetch getSpecInstr into
000481              :tr-report-code1  :nu-report-code1,
000482              :tr-report-code2  :nu-report-code2,
000483              :tr-report-code3  :nu-report-code3,
000484              :tr-user-select2  :nu-user-select2,
000485              :tr-user-select5  :nu-user-select5,
000486              :tr-carrier       :nu-carrier,
000487              :tr-state         :nu-state,
000488              :tr-account       :nu-account,
000489              :tr-status        :nu-status
000490        END-EXEC
000491
000492*       display ' rpt cd1 ' tr-report-code1 ' ' nu-report-code1
000493*       display ' rpt cd2 ' tr-report-code2 ' ' nu-report-code2
000494*       display ' rpt cd3 ' tr-report-code3 ' ' nu-report-code3
000495*       display ' account ' tr-account ' ' nu-account
000496
000497        if sqlcode = 0
000498           add +1 to a1
000499           if nu-report-code1 = +0
000500              move tr-report-code1 to a1-report-code1(a1)
000501           end-if
000502           if nu-report-code2 = +0
000503              move tr-report-code2 to a1-report-code2(a1)
000504           end-if
000505           if nu-report-code3 = +0
000506              move tr-report-code3 to a1-report-code3(a1)
000507           end-if
000508           if nu-user-select2 = +0
000509              move tr-user-select2 to a1-user-select2(a1)
000510           end-if
000511           if nu-user-select5 = +0
000512              move tr-user-select5 to a1-user-select5(a1)
000513           end-if
000514           if nu-carrier = +0
000515              move tr-carrier      to a1-carrier(a1)
000516           end-if
000517           if nu-state = +0
000518              move tr-state        to a1-state(a1)
000519           end-if
000520           if nu-account = +0
000521              move tr-account      to a1-account(a1)
000522           end-if
000523           if nu-status = +0
000524              move tr-status       to a1-status(a1)
000525           end-if
000526        else
000527           if sqlcode not = 0 and 100
000528              display "Error: cannot fetch row " a1
000529              display ' sql return code ' sqlcode
000530              display ' sql err mess    ' sqlerrmc
000531              move '9999'        to ws-return-error-no
000532              move 'Could not fetch rows'
000533                                 to ws-return-error-mess
000534              PERFORM 0200-SEND-BUFFER thru 0200-exit
000535              go to 0300-close-socket
000536           end-if
000537        end-if
000538     end-perform
000539
000541     EXEC SQL
               close getSpecInstr
000542     END-EXEC
000543
000544     if sqlcode not = 0
000545        display "Error: cannot close cursor "
000546        display ' sql retrun code ' sqlcode
000547        display ' sql err mess    ' sqlerrmc
000548     end-if
000549
000550     move a1                     to ma1
000551*    display ' loaded ' a1 ' rows to table '
000552
000553     if connected-to-db
000555        EXEC SQL
                  disconnect all
000556        END-EXEC
000557        move ' '                 to ws-connect-sw
000558     end-if
000559
000560*    perform varying a1 from +1 by +1 until a1 > ma1
000561*       display ' rpt cd1 ' a1-report-code1(a1)
000562*       display ' rpt cd2 ' a1-report-code2(a1)
000563*       display ' rpt cd3 ' a1-report-code3(a1)
000564*       display '     us2 ' a1-user-select2(a1)
000565*       display '     us5 ' a1-user-select5(a1)
000566*       display '   carr  ' a1-carrier(a1)
000567*       display '  state  ' a1-state(a1)
000568*       display ' account ' a1-account(a1)
000569*       display ' status  ' a1-status(a1)
000570*    end-perform
000571
000572     .
000573 0030-exit.
000574     exit.
000575
000576 0040-process-eracct.
000577
000578     move X'04'                  to ws-am-key
000579
000580     
      * exec cics startbr
000581*       dataset       ('ERACCT')
000582*       ridfld        (ws-am-key)
000583*       resp          (ws-response)
000584*       gteq
000585*    end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001519' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303031353139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-am-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000586
000587     if not resp-normal
000588        display 'error-eracct-startbr ' ws-response
000589        go to 0040-exit
000590     end-if
000591
000592     
      * exec cics readnext
000593*       dataset       ('ERACCT')
000594*       ridfld        (ws-am-key)
000595*       into          (account-master)
000596*       resp          (ws-response)
000597*    end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001531' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 account-master, 
                 DFHEIV12, 
                 ws-am-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000598
000599     if resp-normal
000600        move am-control-primary(1:20)
000601                                 to ws-save-eracct-key
000602        perform until end-of-eracct
000603           perform 0050-check-table
000604                                 thru 0050-exit
000605*          display ' key b4 readnext ' ws-am-key (2:19)
000606           
      * exec cics readnext
000607*             dataset    ('ERACCT')
000608*             ridfld     (ws-am-key)
000609*             into       (account-master)
000610*             resp       (ws-response)
000611*          end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001545' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303031353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 account-master, 
                 DFHEIV12, 
                 ws-am-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000612           if (not resp-normal)
000613              or (am-company-cd not = X'04')
000614              set end-of-eracct to true
000615           else
000616              add 1 to ws-acct-reads
000617           end-if
000618*          if ws-acct-reads > +400
000619*             set end-of-eracct to true
000620*          end-if
000621        end-perform
000622        perform 0050-check-table thru 0050-exit
000623     end-if
000624
000625     .
000626 0040-exit.
000627     exit.
000628
000629 0050-check-table.
000630
000631     if am-control-primary(1:20) = ws-save-eracct-key
000632        move am-report-code-1    to ws-save-report-code-1
000633        move am-report-code-2    to ws-save-report-code-2
000634        move am-report-code-3    to ws-save-report-code-3
000635        move am-user-select-2    to ws-save-user-select-2
000636        move am-user-select-5    to ws-save-user-select-5
000637        go to 0050-exit
000638     end-if
000639
000640     move ' '                    to ws-match-sw
000641
000642     perform varying a1 from +1 by +1 until
000643        (a1 > ma1)
000644        or (found-match)
000645**      display ' array values ' sql-table-array(a1) ' ' a1
000646
000647        if ((a1-report-code1(a1) = spaces)
000648           or (a1-report-code1(a1) = ws-save-report-code-1))
000649                      and
000650           ((a1-report-code2(a1) = spaces)
000651           or (a1-report-code2(a1) = ws-save-report-code-2))
000652                      and
000653           ((a1-report-code3(a1) = spaces)
000654           or (a1-report-code3(a1) = ws-save-report-code-3))
000655                      and
000656           ((a1-user-select2(a1) = spaces)
000657           or (a1-user-select2(a1) = ws-save-user-select-2))
000658                      and
000659           ((a1-user-select5(a1) = spaces)
000660           or (a1-user-select5(a1) = ws-save-user-select-5))
000661                      and
000662           ((a1-carrier(a1) = spaces)
000663           or (a1-carrier(a1) = ws-save-carrier))
000664                      and
000665           ((a1-state(a1) = spaces)
000666           or (a1-state(a1) = ws-save-state))
000667                      and
000668           ((a1-account(a1) = spaces)
000669           or (a1-account(a1) = ws-save-account))
000670           perform 0060-update-eracnt
000671                                 thru 0060-exit
000672           set found-match to true
000673*          move high-values      to ws-am-exp-dt
000674        end-if
000675     end-perform
000676
000677     if not found-match  *> may have been a delete
000678        move ws-save-eracct-key  to ws-nt-key
000679        move '2'                 to ws-nt-rec-type
000680        move +3                  to ws-nt-seq-no
000681        perform 0064-read-eracnt-update
000682                                 thru 0064-exit
000683        if resp-normal  *>  found a note
000684           if nt-account-special <> 'Y'
000685              perform 0061-unlock-eracnt
000686                                 thru 0061-exit
000687           else  *> need to remove setting
000688              move ' '           to nt-account-special
000689              perform 0063-rewrite-eracnt
000690                                 thru 0063-exit
000691              if not resp-normal
000692                 display 'error-rewrite-eracnt-D ' ws-response
000693                    ' ' am-account
000694              else
000695                 add 1 to ws-acnt-deletes
000696*                display ' Updated note D' am-state ' '
000697*                   am-account
000698              end-if
000699           end-if
000700        end-if
000701     end-if
000702
000703     move am-control-primary(1:20)
000704                                 to ws-save-eracct-key
000705     move am-report-code-1       to ws-save-report-code-1
000706     move am-report-code-2       to ws-save-report-code-2
000707     move am-report-code-3       to ws-save-report-code-3
000708     move am-user-select-2       to ws-save-user-select-2
000709     move am-user-select-5       to ws-save-user-select-5
000710
000711*    move ' '                    to ws-match-sw
000712*
000713*    perform varying a1 from +1 by +1 until
000714*       (a1 > ma1)
000715*       or (found-match)
000716**      display ' array values ' sql-table-array(a1) ' ' a1
000717*
000718*       if ((a1-report-code1(a1) = spaces)
000719*          or (a1-report-code1(a1) = am-report-code-1))
000720*                     and
000721*          ((a1-report-code2(a1) = spaces)
000722*          or (a1-report-code2(a1) = am-report-code-2))
000723*                     and
000724*          ((a1-report-code3(a1) = spaces)
000725*          or (a1-report-code3(a1) = am-report-code-3))
000726*                     and
000727*          ((a1-user-select2(a1) = spaces)
000728*          or (a1-user-select2(a1) = am-user-select-2))
000729*                     and
000730*          ((a1-user-select5(a1) = spaces)
000731*          or (a1-user-select5(a1) = am-user-select-5))
000732*                     and
000733*          ((a1-carrier(a1) = spaces)
000734*          or (a1-carrier(a1) = am-carrier))
000735*                     and
000736*          ((a1-state(a1) = spaces)
000737*          or (a1-state(a1) = am-state))
000738*                     and
000739*          ((a1-account(a1) = spaces)
000740*          or (a1-account(a1) = am-account))
000741*          perform 0060-update-eracnt
000742*                                thru 0060-exit
000743*          set found-match to true
000744*          move high-values      to ws-am-exp-dt
000745*       end-if
000746*    end-perform
000747*
000748*    if not found-match  *> may have been a delete
000749*       move ws-am-key(1:20)     to ws-nt-key
000750*       move '2'                 to ws-nt-rec-type
000751*       move +3                  to ws-nt-seq-no
000752*       perform 0064-read-eracnt-update
000753*                                thru 0064-exit
000754*       if resp-normal  *>  found a note
000755*          if nt-account-special <> 'Y'
000756*             perform 0061-unlock-eracnt
000757*                                thru 0061-exit
000758*          else  *> need to remove setting
000759*             move ' '           to nt-account-special
000760*             perform 0063-rewrite-eracnt
000761*                                thru 0063-exit
000762*             if not resp-normal
000763*                display 'error-rewrite-eracnt-D ' ws-response
000764*                   ' ' am-account
000765*             else
000766*                add 1 to ws-acnt-deletes
000767*                display ' Updated note D' am-state ' '
000768*                   am-account
000769*             end-if
000770*          end-if
000771*       end-if
000772*    end-if
000773
000774     .
000775 0050-exit.
000776     exit.
000777
000778 0060-update-eracnt.
000779     if sql-table-array(a1) = spaces
000780        display ' bypass 0060, all spaces '
000781        go to 0060-exit
000782     end-if
000783*    display ' made it to 0060 ' ws-save-state ' '
000784*       ws-save-account
000785*    move ws-am-key(1:20)        to ws-nt-key
000786     move ws-save-eracct-key     to ws-nt-key
000787     move '2'                    to ws-nt-rec-type
000788     move +3                     to ws-nt-seq-no
000789     perform 0064-read-eracnt-update
000790                                 thru 0064-exit
000791
000792     if resp-normal     *>  Found a note
000793*       display ' found note ' ws-save-state ' ' ws-save-account
000794        add 1 to ws-acnt-reads
000795        if a1-status(a1) = 'A'  *> special instr active
000796           if nt-account-special = 'Y' *> note already set to Y
000797                                       *> so go on about ur bus
000798              perform 0061-unlock-eracnt
000799                                 thru 0061-exit
000800           else  *> note not active so set it to active
000801              move 'Y'           to nt-account-special
000802              perform 0063-rewrite-eracnt
000803                                 thru 0063-exit
000804              if not resp-normal
000805                 display 'error-rewrite-eracnt-A ' ws-response
000806                    ' ' ws-save-account
000807              else
000808                 add 1 to ws-acnt-updates
000809*                display ' Updated note A ' ws-save-state ' '
000810*                   ws-save-account ' ' sql-table-array(a1)
000811              end-if
000812           end-if
000813        else  *> special instr inactive
000814           if nt-account-special <> 'Y' *> note inactive
000815                                        *> so go on about ur bus
000816              perform 0061-unlock-eracnt
000817                                 thru 0061-exit
000818           else *> note active so set it to inactive
000819              move ' '           to nt-account-special
000820              perform 0063-rewrite-eracnt
000821                                 thru 0063-exit
000822              if not resp-normal
000823                 display 'error-rewrite-eracnt-I ' ws-response
000824                    ' ' ws-save-account
000825              else
000826                 add 1 to ws-acnt-updates
000827*                display ' Updated note I ' ws-save-state ' '
000828*                   ws-save-account ' ' sql-table-array(a1)
000829              end-if
000830           end-if
000831        end-if
000832     else
000833        if resp-notfnd  *> no note record
000834           if a1-status(a1) = 'A'  *> spec instr is active
000835              move 'NT'             to note-file
000836              move ws-save-eracct-key
000837                                 to nt-control-primary
000838              move '2'           to nt-record-type
000839              move +3            to nt-line-sequence
000840              move 'Y'           to nt-account-special
000841              perform 0062-write-eracnt
000842                                 thru 0062-exit
000843              if not resp-normal
000844                 display 'error-write-eracnt ' ws-response
000845              else
000846                 add 1 to ws-acnt-updates
000847*                display ' Add note ' ws-save-state ' '
000848*                   ws-save-account ' ' sql-table-array(a1)
000849              end-if
000850           end-if
000851        end-if
000852     end-if
000853
000854     .
000855 0060-exit.
000856     exit.
000857
000858 0061-unlock-eracnt.
000859
000860     
      * exec cics unlock
000861*       dataset   ('ERACNT')
000862*    end-exec
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&*                    #   #00001799' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303031373939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000863
000864     .
000865 0061-exit.
000866     exit.
000867
000868 0062-write-eracnt.
000869
000870     
      * exec cics write
000871*       dataset ('ERACNT')
000872*       from    (note-file)
000873*       ridfld  (nt-control-primary)
000874*       resp    (ws-response)
000875*    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00001809' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303031383039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 nt-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000876
000877     .
000878 0062-exit.
000879     exit.
000880
000881 0063-rewrite-eracnt.
000882
000883     
      * exec cics rewrite
000884*       dataset    ('ERACNT')
000885*       from       (note-file)
000886*       resp       (ws-response)
000887*    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00001822' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303031383232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000888
000889     .
000890 0063-exit.
000891     exit.
000892
000893 0064-read-eracnt-update.
000894
000895     
      * exec cics read
000896*       update
000897*       dataset  ('ERACNT')
000898*       RIDFLD   (WS-NT-KEY)
000899*       into     (note-file)
000900*       resp     (ws-response)
000901*    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001834' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303031383334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 WS-NT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000902
000903     .
000904 0064-exit.
000905     exit.
000906
000907 0070-format-buffer.
000908
000909     move '0000'                 to ws-return-error-no
000910     move 'Logic Updated completed successful '
000911                                 to ws-return-error-mess
000912     move ws-acct-reads          to ws-return-accts-read
000913     move ws-acnt-reads          to ws-return-acnts-read
000914     move ws-acnt-updates        to ws-return-acnts-updated
000915     move ws-acnt-deletes        to ws-return-acnts-deletes
000916
000917     .
000918 0070-exit.
000919     exit.
000920
000921 0200-send-buffer.
000922
000923     if batch-job
000924        go to 0200-exit
000925     end-if
000926
000927     move ws-return-string       to ws-send-buf
000928     display 'sock11:About to send      ' ws-send-buf
000929     display 'sock11:sequence number  =', ws-seq-num.
000930     display ' msg size ' ws-send-msg-size
000931
000932     call "send" using by value GIVE-TAKE-SOCKET,
000933         by reference ws-send-buf,
000934         by value ws-send-msg-size,
000935         by value ws-flags.
000936
000937     if return-code <= zero
000938        display 'sock11:send error ',
000939        perform 0250-socket-error thru 0250-exit
000940        go to 0300-close-socket
000941     end-if
000942
000943     .
000944 0200-exit.
000945     exit.
000946
000947 0250-socket-error.
000948
000949     display "sock11:did not complete"
000950     display 'sock11:transaction data =', CLIENT-IN-DATA '**'
000951     display 'sock11:socket number    =', GIVE-TAKE-SOCKET.
000952     display 'sock11:socket name      =', lstn-name ' '
000953        lstn-subname
000954     display ' return code = ' return-code
000955
000956     .
000957 0250-exit.
000958     exit.
000959
000960 0300-close-socket.
000961
000962*    call "close" using by value GIVE-TAKE-SOCKET .
000963     display 'sock11:done'
000964     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00001903' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031393033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000965     
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK11' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
000966
000967     .
000968 0300-exit.
000969     exit.
000970
000971 6000-CONNECT-TO-DB.
000972
000973***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000974***                                                            ***
000975***                                                            ***
000976***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000977
000978****  The below code is for when the db has been
000979****  converted to sql server 2016
000980     evaluate ws-kix-myenv
000981        when 'cid1p'
000982           move '//sdv-db01.cso.local:1433;'
000983                                 to p-sql-server
000984        when 'mdoff'
000985           move '//hov-tstdb01.cso.local:55330;'
000986                                 to p-sql-server
000987        when other
000988           move '//hov-tstdb01.cso.local:1433;'
000989                                 to p-sql-server
000990     end-evaluate
000991
000992     move 'Logic'                to p-sql-database
000993
000994     CALL 'SQLCONNECT' USING sqlconnect-parms
000995     display ' ret code ' p-connect-return-code
000996     move p-connect-return-code  to sqlcode
000997     move p-sql-return-message   to sqlerrmc
000998
000999*     EXEC SQL
001000**       CONNECT TO :svr USER :usr-pass
001001*        CONNECT TO :svr
001002*          USER     :usr
001003*          USING    :pass
001004*     END-EXEC
001005
001006     if sqlcode not = 0
001007        display "Error: cannot connect to " svr
001008        display sqlcode
001009        display sqlerrmc
001010     else
001011*       display ' Successful Connect ' sqlcode
001012        set connected-to-db to true
001013     end-if
001014
001015     .
001016 6000-EXIT.
001017     EXIT.
001018
001019 9700-DATE-LINK.
001020
001021     
      * EXEC CICS LINK
001022*         PROGRAM  ('ELDATCV')
001023*         COMMAREA (DATE-CONVERSION-DATA)
001024*         LENGTH   (DC-COMM-LENGTH)
001025*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001960' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031393630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001026
001027 9700-EXIT.
001028      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK11' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK11' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
