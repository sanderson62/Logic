      *((program: WSMESS02.cl2))
000001*$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
000002 IDENTIFICATION DIVISION.
000003 PROGRAM-ID. WSMESS02.
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
000018*     Premium Quote                                              *
000019*  Returns premium calc based on info passed to me.              *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 020617   2017020300002   PEMA  New Program
000029* 080817   2017020300002   PEMA  Limit and ben code assign changes
000030* 040622 CR2019012500003   PEMA  Migrate to SQLSERVER 2016
000031******************************************************************
000032 ENVIRONMENT DIVISION.
000033 data division.
000034 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000035 77  FILLER  PIC X(32) VALUE '********************************'.
000036 77  FILLER  PIC X(32) VALUE '   WSMESS02 WORKING STORAGE     '.
000037 77  FILLER  PIC X(32) VALUE '********************************'.
000038*
000039* program buffers
000040*
000041 77  ws-seq-num                  pic s9(8) comp value 0.
000042 77  ws-flags                    pic s9(8) comp value 0.
000043 77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
000044 77  ws-comp-id                  pic xxx value spaces.
000045 77  WS-SAVE-ACCOUNT             PIC X(10)  VALUE SPACES.
000046 77  WS-BIN-ORIG-EFF-DT          PIC XX  VALUE LOW-VALUES.
000047 77  WS-ORIG-EFF-DT              PIC X(10)  VALUE SPACES.
000048 77  WS-EFF-DATE                 PIC X(10)  VALUE SPACES.
000049 77  WS-lf-exp-date              PIC X(10)  VALUE SPACES.
000050 77  ws-ah-exp-date              pic x(10)  value spaces.
000051 77  ws-loan-exp-date            pic x(10)  value spaces.
000052 77  X1                          PIC S999 COMP-3 VALUE +0.
000053 77  S1                          PIC S999 COMP-3 VALUE +0.
000054 77  S2                          PIC S999 COMP-3 VALUE +0.
000055 77  S3                          PIC S999 COMP-3 VALUE +0.
000056 77  b1                          pic s999 comp-3 value +0.
000057 77  WS-BUILD-SW                 PIC X.
000058     88  TIME-TO-BUILD               VALUE 'Y'.
000059 77  WS-SAVE-ERACCT              PIC X(2000).
000060 77  WS-DIS-RESP                 PIC 9(05) VALUE ZEROS.
000061 77  WS-PERFORM-SW               PIC X VALUE SPACES.
000062     88  GET-RATES                    VALUE 'R'.
000063     88  GET-ACT-ACCTS                VALUE 'A'.
000064 77  ws-bin-current-dt           pic xx  value low-values.
000065 77  ws-bin-eff-dt               pic xx  value low-values.
000066 77  ws-bin-lf-exp-dt            pic xx  value low-values.
000067 77  ws-bin-ah-exp-dt            pic xx  value low-values.
000068 77  ws-bin-loan-exp-dt          pic xx  value low-values.
000069 77  ws-bin-1st-pmt-dt           pic xx  value low-values.
000070 77  ws-bin-pri-birth-dt         pic xx  value low-values.
000071 77  ws-bin-cob-birth-dt         pic xx  value low-values.
000072 77  ws-limit-att-age-dt         pic xx  value low-values.
000073 77  WS-DISP-AMT                 PIC Z,ZZZ,Z99.99.
000074 77  ws-disp-rate                pic z9.99999.
000075 77  WS-ERACCT-SW                PIC X VALUE ' '.
000076     88  END-OF-ERACCT                 VALUE 'Y'.
000077 77  WS-ERCTBL-SW                PIC X VALUE ' '.
000078     88  END-OF-ERCTBL                 VALUE 'Y'.
000079 77  WS-STATUS                   PIC X.
000080 77  ws-socket-sw                pic x value ' '.
000081     88  end-of-socket              value 'Y'.
000082 77  rec-cnt                     pic 9(5) value zeros.
000083 77  ws-stop-sw                  pic x value ' '.
000084     88  i-say-stop                 value 'Y'.
000085 77  ws-browse-sw                pic x value ' '.
000086     88  browse-started            value 'Y'.
000087 77  ws-contract-sw              pic x  value ' '.
000088     88  contract-no-assigned      value 'Y'.
000089 77  ws-error-sw                 pic x value ' '.
000090     88  error-in-one-coverage     value 'Y'.
000091 77  ws-connect-sw               pic x value ' '.
000092     88  connected-to-db           value 'Y'.
000093 77  client-id                   pic xxx.
000094 77  ws-error-sub                pic 999  value zeros.
000095 77  ws-error-sup                pic x(25) value spaces.
000096 77  ws-form-limit-name          pic x(50) value spaces.
000097 77  l1                          pic s999 comp-3 value +0.
000098 77  tx-discount-rate            pic s9v9(5) comp-3 value .00292.
000099 77  ws-discount-rate            pic s99v9(5) comp-3 value +0.
000100 77  temp-discount-rate          PIC S9V9(11) comp-3 value +0.
000101 77  ws-rate-type                pic x.
000102     88  sp-rate                   value 'S'.
000103     88  mob-rate                  value 'M'.
000104 77  ws-recalc-lf                pic x value ' '.
000105     88  recalc-lf                  value 'Y'.
000106 77  ws-recalc-di                pic x value ' '.
000107     88  recalc-di                  value 'Y'.
000108 77  ws-temp-lf-term             pic s999v99 comp-3 value +0.
000109
000110 01  P pointer.
000111 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000112 01  var-ptr pointer.
000113 01  env-var-len                 pic 9(4)  binary.
000114 01  rc                          pic 9(9)  binary.
000115
000116 01  WS-KIXSYS.
000117     05  WS-KIX-FIL1             PIC X(10).
000118     05  WS-KIX-APPS             PIC X(10).
000119     05  WS-KIX-ENV              PIC X(10).
000120     05  WS-KIX-MYENV            PIC X(10).
000121     05  WS-KIX-SYS              PIC X(10).
000122
000123 01  ws-new-lf-term              pic 999 value zeros.
000124 01  ws-new-ah-term              pic 999 value zeros.
000125 01  a-angle-n                   pic s9(7)v9(11) comp-3 value +0.
000126 01  a-angle-n-m                 pic s9(7)v9(11) comp-3 value +0.
000127 01  a-prm-angle-n               pic s9(7)v9(11) comp-3 value +0.
000128 01  a-prm-angle-n-m             pic s9(7)v9(11) comp-3 value +0.
000129 01  LIFE-FACTOR                 PIC S9(7)V9(11) COMP-3 value +0.
000130 01  SINGLE-FACTOR               PIC S9(7)V9(11) COMP-3 value +0.
000131 01  gamma                       pic s9(7)v9(11) comp-5 value +0.
000132 01  temp-value-1                pic s9(7)v9(11) comp-3 value +0.
000133 01  temp-value-2                pic s9(7)v9(11) comp-3 value +0.
000134 01  temp-value-3                pic s9(7)v9(11) comp-3 value +0.
000135 01  temp-value-4                pic s9(7)v9(11) comp-3 value +0.
000136 01  temp-value-5                pic s9(7)v9(11) comp-3 value +0.
000137 01  temp-value-6                pic s9(7)v9(11) comp-3 value +0.
000138 01  V-TO-M                      PIC S9(7)V9(11) COMP-3 value +0.
000139 01  X-TO-N                      PIC S9(7)V9(11) COMP-3 value +0.
000140 01  V-TO-N                      PIC S9(7)V9(11) COMP-3 value +0.
000141 01  X-TO-M                      PIC S9(7)V9(11) COMP-3 value +0.
000142 01  V-TO-N-M                    PIC S9(7)V9(11) COMP-3 value +0.
000143 01  X                           PIC S9(4)V9(11) COMP-3 value +0.
000144 01  mob                         pic s9(7)v9(11) comp-3 value +0.
000145 01  mnthly-rate                 pic s9(7)v9(11) comp-3 value +0.
000146 01  m                           pic s9(4)v9(4)  comp-3 value +0.
000147 01  n                           pic s9(4)v9(4)  comp-3 value +0.
000148 01  i                           pic s9(7)v9(11) comp-3 value +0.
000149 01  l                           PIC S9(7)V99    comp-3 value +0.
000150 01  pmt                         pic s9(7)v99    comp-3 value +0.
000151 01  dispmt                      pic s9(7)v99    comp-3 value +0.
000152 01  lifepmt1                    pic s9(7)v99    comp-3 value +0.
000153 01  lifepmt2                    pic s9(7)v99    comp-3 value +0.
000154 01  newpmt                      pic s9(7)v99    comp-3 value +0.
000155 01  ppy                         pic s9(5)       comp-3 value +0.
000156 01  dpp                         pic s9(5)       comp-3 value +0.
000157 01  d                           pic s9(5)       comp-3 value +0.
000158 01  plus2                       pic s9(5)       comp-3 value +0.
000159 01  lf-rate                     pic s999v9(5)   comp-3 value +0.
000160 01  ah-rate                     pic s999v9(5)   comp-3 value +0.
000161 01  DL-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
000162 01  LL-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
000163 01  AH-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
000164 01  T-FINANCED                  PIC S9(7)V99    comp-3 value +0.
000165 01  T-REG-Z-FINANCED            PIC S9(7)V99    comp-3 value +0.
000166 01  T-PAYMENTS                  PIC S9(7)V99    comp-3 value +0.
000167
000168*EXEC SQL
000169*   INCLUDE SQLDA
000170*END-EXEC
000171
000174*EXEC SQL
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
000175
000177 EXEC SQL
          BEGIN DECLARE SECTION
000178 END-EXEC
000179
000180 01  ws-dealer-state             pic xx.
000181 01  ws-dealer-id                pic x(10).
000182 01  ws-contract-eff-dt          pic x(10).
000183 01  ws-contract-no              pic x(10) value spaces.
000184 01  ws-contract-suffix          pic x     value spaces.
000185
000186 01  ws-key-stuff.
000187     05  ws-ks-contract-no       pic x(10) value spaces.
000188
000189 01  ws-error-message            pic x(50) value spaces.
000190 01  sqlcmd                      pic x(1024).
000191 01  svr                         pic x(32).
000192 01  usr                         pic x(32).
000193 01  pass                        pic x(32).
000194 01  usr-pass                    pic x(64).
000195 01  ws-disp-code                pic s9(11).
000196
000197***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000198***                                                            ***
000199***  These indicators are used to determine if a variable      ***
000200***  is        null.           The indicator will be -1        ***
000201***  if the value        is null  and +0 if the value is       ***
000202***  something other than null.  Here is an example on how     ***
000203***  to use the indicator variables.                           ***
000204***                                                            ***
000205***     EXEC SQL                                               ***
000206***        fetch checkapp into                                 ***
000207***           :db-app-status :nu-app-status,                   ***
000208***           :db-app-by     :nu-app-by,                       ***
000209***           :db-app-date   :nu-app-date,                     ***
000210***           :db-app-batch  :nu-app-batch                     ***
000211***     END-EXEC                                               ***
000212***                                                            ***
000213***           OR This way on an update                         ***
000214***                                                            ***
000215***     EXEC SQL                                               ***
000216***        UPDATE                                              ***
000217***           CUC_Logic_Remittance                             ***
000218***        SET                                                 ***
000219***           LogicStatus     = :ws-status-code,               ***
000220***           LogicStatusDate = :ws-status-date,               ***
000221***           BatchNumber     = :ws-batch-no :nu-batchno       ***
000222***        WHERE                                               ***
000223***           RemitId = :ws-remit-id                           ***
000224***     END-EXEC                                               ***
000225***                                                            ***
000226***    Also, when the table has a column with a data type of   ***
000227***  "BIT" and used as true/false move the 1 byte receiving    ***
000228***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
000229***  then its false. I think true would be 256.                ***
000230***                                                            ***
000231***                                                            ***
000232***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000233
000234 01  indicator-vaiables-for-nulls.
000235     05  nu-partial-cov          pic s9(4) comp value +0.
000236     05  nu-check-elig           pic s9(4) comp value +0.
000237     05  nu-allow-trunc          pic s9(4) comp value +0.
000238     05  nu-app-by               pic s9(4) comp value +0.
000239     05  nu-app-date             pic s9(4) comp value +0.
000240     05  nu-app-batch            pic s9(4) comp value +0.
000241     05  nu-fincar               pic s9(4) comp value +0.
000242     05  nu-batchno              pic s9(4) comp value +0.
000243     05  nu-error-message        pic s9(4) comp value +0.
000244
000245 01  sql-cert-records.
000246     05  sql-dlr-state           pic xx.
000247     05  sql-dlr-id              pic x(10).
000248     05  sql-eff-dt              pic x(10).
000249     05  sql-contr-no            pic x(10).
000250     05  sql-contr-suffix        pic x.
000251
000252 01  ws-limit-issue-age          pic 999.
000253 01  ws-att-age                  pic 999v99.
000254 01  ws-att-age-x redefines ws-att-age pic x(5).
000255 01  ws-pri-att-age              pic 999v99.
000256 01  ws-cob-att-age              pic 999v99.
000257 01  form-limit-table.
000258     05  ws-limit-name           pic x(50).
000259     05  ws-limit-cov-type       pic xx.
000260     05  ws-limit-lo-age         pic 999.
000261     05  ws-limit-hi-age         pic 999.
000262     05  ws-limit-att-age        pic 999.
000263     05  ws-limit-max-term       pic 999.
000264     05  ws-limit-max-jnt-term   pic 999.
000265     05  ws-limit-max-mo-ben     pic 9(5).
000266     05  ws-limit-max-tot-ben    pic 9(7).
000267     05  ws-limit-partial-cov    pic s9(4) comp-5.
000268     05  ws-limit-check-elig     pic s9(4) comp-5.
000269     05  ws-limit-elig-max-term  pic 999.
000270     05  ws-limit-allow-truncated pic s9(4) comp-5.
000271
000272 01  state-benefit-code-table.
000273     05  ws-sbc-min-amt          pic 999999.
000274     05  ws-sbc-state            pic xx.
000275     05  ws-sbc-cov-type         pic xx.
000276     05  ws-sbc-ben-amt          pic 999999.
000277     05  ws-sbc-sin-jnt          pic x.
000278     05  ws-sbc-dismember        pic x.
000279     05  ws-sbc-retroelim        pic x.
000280     05  ws-sbc-wait-days        pic 999.
000281     05  ws-sbc-max-bens         pic 999.
000282     05  ws-sbc-logic-ben-code   pic xx.
000283
000285 EXEC SQL
          END DECLARE SECTION
000286 END-EXEC
000287
000288 01  filler.
000289     05  ws-work-in              pic x(10).
000290     05  ws-work-out             pic x(10).
000291     05  ws-work-out-v2 redefines
000292         ws-work-out             pic 9(8)v99.
000293     05  ws-work-out-v0 redefines
000294         ws-work-out             pic 9(10).
000295     05  ws-work-out-v5 redefines
000296         ws-work-out             pic 9(5)v9(5).
000297 01  filler.
000298     05  ws-last-suffix          pic x value low-values.
000299     05  ws-tbl-last-suffix      pic x value low-values.
000300     05  filler. *> Use X1 for this table.
000301         10  ws-codes            pic x(26) value
000302         ' ABCDEFGHIJKLMNOPQRSTUVWXY'.
000303         10  ws-suffix-value redefines ws-codes
000304           occurs 26             pic x.
000305
000306 01  ws-work-date.
000307     05  ws-work-ccyy            pic x(4).
000308     05  ws-work-mm              pic xx.
000309     05  ws-work-dd              pic xx.
000310 01  ws-work-date-num redefines ws-work-date
000311                                 pic 9(8).
000312
000313 01  filler.
000314     05  ws-test-bin-dt          pic xx.
000315     05  ws-comp-bin-dt redefines ws-test-bin-dt
000316                                 pic s9(4) comp.
000317 01  ws-disp-bin-dt              pic 9(9) value zeros.
000318 01  ws-rate-file-stuff.
000319     12  ERRATE-KEY.
000320         16  RATE-COMPANY-CD     PIC X        VALUE SPACE.
000321         16  RATE-STATE-CODE.
000322             20  RATE-ST-CODE    PIC  XX      VALUE SPACES.
000323             20  RATE-ST-CLASS   PIC  XX      VALUE SPACES.
000324             20  RATE-ST-DEV     PIC  XXX     VALUE SPACES.
000325         16  RATE-L-AH-CODE.
000326             20  RATE-L-AH       PIC  X       VALUE SPACE.
000327             20  RATE-LAH-NUM    PIC  XX      VALUE ZEROS.
000328         16  RATE-LIMITS.
000329             20  RATE-HIGH-AGE   PIC  99      VALUE ZEROS.
000330             20  RATE-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
000331             20  RATE-FUTURE     PIC  XX      VALUE SPACES.
000332             20  RATE-SEX        PIC  X       VALUE '9'.
000333         16  RATE-EXPIRY-DATE    PIC 9(11)    COMP-3.
000334
000335     12  SAVE-ERRATE-KEY.
000336         16  SVRT-COMPANY-CD     PIC X        VALUE SPACE.
000337         16  SVRT-STATE-CODE.
000338             20  SVRT-ST-CODE    PIC  XX      VALUE SPACES.
000339             20  SVRT-ST-CLASS   PIC  XX      VALUE SPACES.
000340             20  SVRT-ST-DEV     PIC  XXX     VALUE SPACES.
000341         16  SVRT-L-AH-CODE.
000342             20  SVRT-L-AH       PIC  X       VALUE SPACE.
000343             20  SVRT-LAH-NUM    PIC  XX      VALUE ZEROS.
000344         16  SVRT-LIMITS.
000345             20  SVRT-HIGH-AGE   PIC  99      VALUE ZEROS.
000346             20  SVRT-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
000347             20  SVRT-FUTURE     PIC  XX      VALUE SPACES.
000348             20  SVRT-SEX        PIC  X       VALUE '9'.
000349         16  SVRT-EXPIRY-DATE    PIC  9(11)   COMP-3.
000350
000351 01  raw-message002-area.
000352     05  raw-message-num         pic x(10).
000353     05  raw-state               pic xx.
000354     05  raw-acct-no             pic x(10).
000355     05  raw-vin                 pic x(17).
000356     05  raw-lf-ben-code         pic xx.
000357     05  raw-ah-ben-code         pic xx.
000358     05  raw-earn-meth           pic x.
000359     05  raw-pri-birth-date      pic x(10).
000360     05  raw-cob-birth-date      pic x(10).
000361     05  raw-loan-amt            pic x(9).
000362     05  raw-eff-date            pic x(10).
000363     05  raw-1st-pmt-dt          pic x(10).
000364     05  raw-pmts-per-year       pic xx.
000365     05  raw-loan-term           pic xxx.
000366     05  raw-lf-term             pic XXX.
000367     05  raw-ah-term             pic xxx.
000368     05  raw-apr                 pic x(8).
000369     05  raw-lf-sin-jnt-ind      pic x.
000370     05  raw-ah-sin-jnt-ind      pic x.
000371     05  raw-dismemberment       pic x.
000372     05  raw-retro-elim          pic x.
000373     05  raw-waiting-days        pic xx.
000374     05  raw-crit-per            pic xx.
000375
000376 01  ws-rate-work-area.
000377     05  ws-rate-state           pic xx.
000378     05  ws-rate-acct-no         pic x(10).
000379     05  ws-rate-vin             pic x(17).
000380     05  ws-rate-in-lf-ben-code  pic xx.
000381     05  ws-rate-in-ah-ben-code  pic xx.
000382     05  ws-rate-earn-meth       pic x.
000383     05  ws-rate-pri-birth-date  pic x(10).
000384     05  ws-rate-cob-birth-date  pic x(10).
000385     05  ws-rate-eff-date        pic x(10).
000386     05  ws-rate-1st-pmt-dt      pic x(10).
000387     05  ws-rate-benefit-type    pic x.
000388     05  ws-rate-lf-benefit-cd   pic xx.
000389     05  ws-rate-ah-benefit-cd   pic xx.
000390     05  ws-rate-loan-amt        pic 9(6)v99.
000391     05  ws-rate-per-pmt         pic 9(7)v99.
000392     05  ws-rate-loan-pmt        pic 9(7)v99.
000393     05  ws-rate-lf-prem         pic 9(5)v99.
000394     05  ws-rate-ah-prem         pic 9(5)v99.
000395     05  ws-rate-lf-rate         pic 99v9(5).
000396     05  ws-rate-ah-rate         pic 99v9(5).
000397     05  ws-rate-apr             pic 99v9(5).
000398     05  ws-rate-pmts-per-year   pic 99.
000399     05  ws-rate-loan-term       pic 999  value zeros.
000400     05  ws-rate-lf-term         pic 999  value zeros.
000401     05  ws-rate-ah-term         pic 999  value zeros.
000402     05  ws-issue-age            pic 999  value zeros.
000403     05  ws-cob-age              pic 999  value zeros.
000404     05  ws-rate-age             pic 999  value zeros.
000405     05  ws-max-lf-benefit       pic 9(7)v99 value zeros.
000406     05  ws-max-ah-benefit       pic 9(7)v99 value zeros.
000407     05  ws-rate-tot-financed    pic s9(7)v99 value zeros.
000408     05  ws-rate-tot-pmts        pic s9(7)v99 value zeros.
000409     05  ws-rate-crit-per        pic 99.
000410     05  ws-rate-sin-jnt-lf      pic x.
000411     05  ws-rate-sin-jnt-ah      pic x.
000412     05  ws-rate-dismemberment   pic x.
000413     05  ws-rate-retro-elim      pic x.
000414     05  ws-rate-waiting-days    pic 99.
000415     05  ws-rate-fullterm        pic x.
000416
000417 01  ws-lf-limits.
000418     05  ws-lf-limit-lo-age         pic 999.
000419     05  ws-lf-limit-hi-age         pic 999.
000420     05  ws-lf-limit-att-age        pic 999.
000421     05  ws-lf-limit-max-term       pic 999.
000422     05  ws-lf-limit-max-benefit    pic 9(7).
000423     05  ws-lf-limit-partial-cov    pic s9(4) comp-5.
000424     05  ws-lf-limit-check-elig     pic s9(4) comp-5.
000425     05  ws-lf-limit-elig-max-term  pic 999.
000426     05  ws-lf-limit-allow-truncated pic s9(4) comp-5.
000427
000428 01  ws-di-limits.
000429     05  ws-di-limit-lo-age         pic 999.
000430     05  ws-di-limit-hi-age         pic 999.
000431     05  ws-di-limit-att-age        pic 999.
000432     05  ws-di-limit-max-term       pic 999.
000433     05  ws-di-limit-max-jnt-term   pic 999.
000434     05  ws-di-limit-max-mo-ben     pic 9(5).
000435     05  ws-di-limit-max-tot-ben    pic 9(7).
000436     05  ws-di-limit-partial-cov    pic s9(4) comp-5.
000437     05  ws-di-limit-check-elig     pic s9(4) comp-5.
000438     05  ws-di-limit-elig-max-term  pic 999.
000439     05  ws-di-limit-allow-truncated pic s9(4) comp-5.
000440
000441 01  ws-return-string.
000442     05  ws-return-error-no      pic x(4).
000443     05  ws-sc1                  pic x.
000444     05  ws-return-error-mess    pic x(150).
000445     05  ws-sc2                  pic x.
000446     05  ws-return-contract-no   pic x(11).
000447     05  ws-sc3                  pic x.
000448     05  ws-return-lf-max-amt    pic z,zzz,z99.99.
000449     05  ws-sc4                  pic x.
000450     05  ws-return-ah-max-amt    pic zzz,z99.99.
000451     05  ws-sc5                  pic x.
000452     05  ws-return-lf-prem       pic zzz,z99.99.
000453     05  ws-sc6                  pic x.
000454     05  ws-return-ah-prem       pic zzz,z99.99.
000455     05  ws-sc7                  pic x.
000456     05  ws-return-lf-rate       pic z9.99999.
000457     05  ws-sc8                  pic x.
000458     05  ws-return-ah-rate       pic z9.99999.
000459     05  ws-sc9                  pic x.
000460     05  ws-return-lf-exp-dt     pic x(10).
000461     05  ws-sc10                 pic x.
000462     05  ws-return-ah-exp-dt     pic x(10).
000463     05  ws-sc11                 pic x.
000464     05  ws-return-lf-benefit-cd pic xx.
000465     05  ws-sc12                 pic x.
000466     05  ws-return-ah-benefit-cd pic xx.
000467     05  ws-sc13                 pic x.
000468     05  ws-return-period-pmt    pic zzz,z99.99.
000469     05  ws-sc14                 pic x.
000470     05  ws-return-tot-financed  pic z,zzz,z99.99.
000471     05  ws-sc15                 pic x.
000472     05  ws-return-tot-pmts      pic z,zzz,z99.99.
000473     05  ws-sc16                 pic x.
000474     05  ws-return-loan-exp-dt   pic x(10).
000475     05  ws-sc17                 pic x.
000476     05  ws-return-limit-name    pic x(50).
000477     05  ws-sc18                 pic x.
000478     05  ws-return-rate-class    pic xx.
000479     05  ws-sc19                 pic x.
000480     05  ws-return-loan-pmt      pic zzz,z99.99.
000481     05  ws-sc20                 pic x.
000482     05  ws-return-principal     pic zzz,z99.99.
000483     05  ws-sc21                 pic x.
000484     05  ws-return-lf-term       pic zz9.
000485     05  ws-sc22                 pic x.
000486     05  ws-return-ah-term       pic zz9.
000487     05  ws-sc23                 pic x.
000488     05  ws-return-lf-bencd      pic xx.
000489     05  ws-sc24                 pic x.
000490     05  ws-return-max-bens      pic 99.
000491
000492
000493 01  WS-AM-KEY.
000494     05  WS-AM-COMPANY-CD        PIC X.
000495     05  WS-AM-CARRIER           PIC X.
000496     05  WS-AM-GROUP             PIC X(6).
000497     05  WS-AM-STATE             PIC XX.
000498     05  WS-AM-ACCOUNT           PIC X(10).
000499     05  WS-AM-EXP-DT            PIC XX.
000500     05  FILLER                  PIC X(4).
000501
000502 01  ws-cm5-compare-key          pic x(12).
000503 01  WS-CM5-KEY.
000504     05  WS-CM5-COMPANY-CD       PIC X.
000505     05  WS-CM5-CERT-NO          PIC X(11).
000506
000507 01  ws-cm-compare-key           pic x(33).
000508 01  WS-CM-KEY.
000509     05  WS-CM-COMPANY-CD        PIC X.
000510     05  WS-CM-CARRIER           PIC X.
000511     05  WS-CM-GROUP             PIC X(6).
000512     05  WS-CM-STATE             PIC XX.
000513     05  WS-CM-ACCOUNT           PIC X(10).
000514     05  WS-CM-EFF-DT            PIC XX.
000515     05  WS-CM-CERT-NO.
000516         10  ws-cm-cert-ten      pic x(10).
000517         10  ws-cm-cert-suffix   pic x.
000518
000519 01  WS-CS-KEY.
000520     05  WS-CS-COMPANY-CD        PIC X.
000521     05  WS-CS-CARRIER           PIC X.
000522     05  WS-CS-GROUP             PIC X(6).
000523     05  WS-CS-STATE             PIC XX.
000524     05  WS-CS-ACCOUNT           PIC X(10).
000525     05  WS-CS-EFF-DT            PIC XX.
000526     05  WS-CS-CERT-NO           PIC X(11).
000527     05  WS-CS-TRLR-TYPE         PIC X.
000528
000529 01  WS-AM-ALT-KEY.
000530     05  WS-AM-ALT-ACCOUNT       PIC X(10).
000531     05  WS-AM-ALT-EXP-DT        PIC XX.
000532
000533 01  WS-CF-KEY-SAVE              PIC X(10).
000534 01  WS-CF-KEY.
000535     05  WS-CF-COMPANY-ID        PIC XXX.
000536     05  WS-CF-RECORD-TYPE       PIC X.
000537     05  WS-CF-ACCESS            PIC X(4).
000538     05  WS-CF-SEQ-NO            PIC S9(4) COMP.
000539
000540 01  filler.
000541     05  ws-errors-table.
000542         10  filler              pic x(130) value
000543         '0000Transaction successfully completed'.
000544         10  filler              pic x(130) value
000545         '0101Amount entered contains invalid characters. Please c
000546-        'orrect and re-submit'.
000547         10  filler              pic x(130) value
000548         '0102Date entered is invalid. Please correct and re-submi
000549-        't'.
000550         10  filler              pic x(130) value
000551         '0103Customer age exceeds age limit. Not eligible for cov
000552-        'erage'.
000553         10  filler              pic x(130) value
000554         '0104Customer will exceed age limit during term of covera
000555-        'ge. Not eligible for coverage'.
000556         10  filler              pic x(130) value
000557         '0105Invalid loan term. Term must be more than 0 and less
000558-        ' than 360'.
000559         10  filler              pic x(130) value
000560         '0106Insurance term exceeds maximum term allowed. Loan is
000561-        ' not eligible for credit insurance'.
000562         10  filler              pic x(130) value
000563         '0107Insurance term exceeds maximum term allowed. Loan is
000564-        ' not eligible for Joint credit disability insurance'.
000565         10  filler              pic x(130) value
000566         '0108Insurance term exceeds maximum term allowed. Loan is
000567-        ' not eligible for credit disability insurance'.
000568         10  filler              pic x(130) value
000569         '0109Total of payments exceeds maximum allowed. Loan is n
000570-        'ot eligible for credit insurance'.
000571         10  filler              pic x(130) value
000572         '0110Monthly payment exceeds maximum allowed. Loan is not
000573-        ' eligible for disability insurance'.
000574         10  filler              pic x(130) value
000575         '0111Loan term cannot be less than insurance term. Loan i
000576-        's not eligible for credit insurance'.
000577         10  filler              pic x(130) value
000578         '0112Term times monthly payment exceeds Maximum total dis
000579-        'ability benefit. Loan is not eligible for credit disabil
000580-        'ity insurance'.
000581         10  filler              pic x(130) value
000582         '0113Cannot rate based on existing loan terms'.
000583         10  filler              pic x(130) value
000584         '0114Cannot rate life coverage based on existing loan ter
000585-        'ms'.
000586         10  filler              pic x(130) value
000587         '0115Cannot rate disability coverage based on existing lo
000588-        'an terms'.
000589         10  filler              pic x(130) value
000590         '0116Problem with rate file. Unable to rate'.
000591         10  filler              pic x(130) value
000592         '0117Calculated premium is outside of tolerance'.
000593         10  filler              pic x(130) value
000594         '0118This appears to be a duplicate record. Unable to rat
000595-        'e'.
000596         10  filler              pic x(130) value
000597         '0119Loan term and insurance term cannot be zero. Unable
000598-        'to rate'.
000599         10  filler              pic x(130) value
000600         '0120Loan term and insurance term cannot be zero. Unable
000601-        'to rate'.
000602         10  filler              pic x(130) value
000603         '0121Cannot locate limit table. Unable to rate'.
000604         10  filler              pic x(130) value
000605         '0122Invalid company id '.
000606         10  filler              pic x(130) value
000607         '0123Cannot locate life benefit code. Unable to rate'.
000608         10  filler              pic x(130) value
000609         '0124Cannot locate disability benefit code. Unable to rat
000610-        'e'.
000611         10  filler              pic x(130) value
000612         '0125Calculated total of payments do not match submitted
000613-        'total of payments. Unable to rate'.
000614         10  filler              pic x(130) value
000615         '0126Life and Disability Terms must be the same'.
000616         10  filler              pic x(130) value
000617         '0127Loan term and insurance term must be the same on Gro
000618-        'ss coverages'.
000619         10  filler              pic x(130) value
000620         '0128Total of payments does not match benefit amount prov
000621-        'ided on Gross coverage. Unable to rate'.
000622         10  filler              pic x(130) value
000623         '0129Calculated amount financed does not match provided a
000624-        'mount financed. Unable to rate'.
000625         10  filler              pic x(130) value
000626         '0130Insurance term exceeds maximum eligibility term. Loa
000627-        'n is not eligible for credit insurance'.
000628         10  filler              pic x(130) value
000629         '0131Amount exceeds maximum eligibility amount. Loan is n
000630-        'ot eligible for credit insurance'.
000631     05  filler redefines ws-errors-table occurs 32.
000632         10  ws-table-error-no   pic x(4).
000633         10  ws-table-error-mess pic x(126).
000634
000635
000636 01  ws-mess002-length           pic s9(4) comp value +1024.
000637 01  WS-MESS002-PASS-AREA        PIC X(1024).
000638 01  WS-CID-NO                   PIC X(8).
000639
000640 01  WS-DISP-RESP                PIC 9(5).
000641 01  WS-RESPONSE                 PIC S9(8)   COMP.
000642     88  RESP-NORMAL                  VALUE +00.
000643     88  RESP-NOTFND                  VALUE +13.
000644     88  resp-duprec                  value +14.
000645     88  resp-dupkey                  value +15.
000646     88  RESP-NOTOPEN                 VALUE +19.
000647     88  RESP-ENDFILE                 VALUE +20.
000648
000649*                                 COPY ERCACCT.
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
000650*                                 COPY ERCRATE.
      *>>((file: ERCRATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCRATE                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.008                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = RATES MASTER FILE                         *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 1765  RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERRATE                   RKP=2,LEN=28    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022******************************************************************
000023*                   C H A N G E   L O G
000024*
000025* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000026*-----------------------------------------------------------------
000027*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000028* EFFECTIVE    NUMBER
000029*-----------------------------------------------------------------
000030* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000031* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000032******************************************************************
000033
000034 01  RATE-RECORD.
000035     12  RT-RECORD-ID                      PIC XX.
000036         88  VALID-RT-ID                      VALUE 'RT'.
000037
000038     12  RT-CONTROL-PRIMARY.
000039         16  RT-COMPANY-CD                 PIC X.
000040         16  RT-STATE-CODE.
000041             20  RT-ST-CODE                PIC XX.
000042             20  RT-ST-CLASS               PIC XX.
000043             20  RT-ST-DEV                 PIC XXX.
000044         16  RT-L-AH-CODE.
000045             20  RT-L-AH                   PIC X.
000046             20  RT-LAH-NUM                PIC XX.
000047         16  RT-LIMITS.
000048             20  RT-HIGH-AGE               PIC 99.
000049             20  RT-HIGH-AMT               PIC 9(6).
000050             20  RT-FUTURE                 PIC XX.
000051             20  RT-SEX                    PIC X.
000052         16  RT-EXPIRY-DATE                PIC 9(11)  COMP-3.
000053
000054     12  RT-MAINT-INFORMATION.
000055         16  RT-LAST-MAINT-DT              PIC XX.
000056         16  RT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000057         16  RT-LAST-MAINT-USER            PIC X(4).
000058         16  FILLER                        PIC X(10).
000059
000060     12  RT-STRUCTURE-COMMENT              PIC X(50).
000061     12  RT-RATE-COMMENT                   PIC X(50).
000062
000063     12  CSL-RESERVED                      PIC X(10).
000064     12  FILLER                            PIC X(12).
000065
000066     12  RT-MAX-AGE                        PIC 99.
000067
000068     12  RT-LIFE-LIMS-FLDS.
000069         16  RT-LIFE-MORT-CODE             PIC X(4).
000070         16  RT-LIFE-EXCEPTIONS   OCCURS 8 TIMES.
000071             20  RT-L-EX-AGE               PIC 99.
000072             20  RT-L-EX-TERM              PIC S999       COMP-3.
000073             20  RT-L-EX-FACE              PIC S9(7)      COMP-3.
000074         16  RT-LIFE-TERM-MINS OCCURS 8.
000075             20  RT-L-MIN-TERM             PIC S999       COMP-3.
000076         16  FILLER                        PIC X(4).
000077*        16  FILLER                        PIC X(20).
000078
000079     12  RT-AH-LIMS-FLDS   REDEFINES   RT-LIFE-LIMS-FLDS.
000080         16  RT-AH-EXCEPTIONS   OCCURS 8 TIMES.
000081             20  RT-AH-AGE                 PIC 99.
000082             20  RT-AH-TERM                PIC S999       COMP-3.
000083             20  RT-AH-BEN-M               PIC S9(5)      COMP-3.
000084             20  RT-AH-BEN-F               PIC S9(7)      COMP-3.
000085
000086     12  RT-LIFE-RATES.
000087         16  RT-L-RATE  OCCURS 360 TIMES   PIC S99V9(5)   COMP-3.
000088
000089     12  RT-AH-RATES   REDEFINES   RT-LIFE-RATES.
000090         16  RT-AH-RATE  OCCURS 360 TIMES  PIC S99V9(5)   COMP-3.
000091
000092     12  RT-DAILY-RATE                     PIC S99V9(5)   COMP-3.
000093
000094     12  RT-DISCOUNT-OPTION                PIC X.
000095         88  RT-DO-NOT-USE                     VALUE ' '.
000096         88  RT-USE-DISCOUNT-FACTOR            VALUE '1'.
000097         88  RT-USE-APR-AS-DISCOUNT            VALUE '2'.
000098
000099     12  RT-DISCOUNT-RATE                  PIC S99V9(5)   COMP-3.
000100     12  RT-DISCOUNT-OB-RATE               PIC S99V9(5)   COMP-3.
000101
000102     12  RT-COMPOSITE-OPTION               PIC X.
000103         88  RT-NO-COMPOSITE                   VALUE ' '.
000104         88  RT-USE-COMPOSITE-RATE             VALUE '1'.
000105
000106     12  RT-COMPOSITE-RATE                 PIC S99V9(5)   COMP-3.
000107
000108     12  RT-CANCEL-FEE                     PIC S9(3)V99   COMP-3.
000109     12  FILLER                            PIC X(13).
000110
000111     12  RT-TYPE-RATE                      PIC X.
000112         88  RT-IS-STND                        VALUE ' ' 'S'.
000113         88  RT-IS-OB                          VALUE 'O'.
000114
000115     12  RT-SRT-ALPHA                      PIC X.
000116
000117     12  RT-CONTROL-2.
000118         16  RTC-1                         PIC X(7).
000119         16  RTC-3                         PIC X(11).
000120         16  RTC-4                         PIC 9(11) COMP-3.
000121         16  RTC-2                         PIC X(3).
000122******************************************************************
      *<<((file: ERCRATE))
000651*                                 COPY ELCCERT.
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
000652*                                 COPY ELCCRTT.
      *>>((file: ELCCRTT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTT.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 552  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
000023* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
000024* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
000025* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000027* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000028* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
000030* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000031* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
000032* 091318  CR2018073000001  PEMA  ADD Refund methods
000033* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000034******************************************************************
000035
000036 01  CERTIFICATE-TRAILERS.
000037     12  CS-RECORD-ID                      PIC XX.
000038         88  VALID-CS-ID                      VALUE 'CS'.
000039
000040     12  CS-CONTROL-PRIMARY.
000041         16  CS-COMPANY-CD                 PIC X.
000042         16  CS-CARRIER                    PIC X.
000043         16  CS-GROUPING                   PIC X(6).
000044         16  CS-STATE                      PIC XX.
000045         16  CS-ACCOUNT                    PIC X(10).
000046         16  CS-CERT-EFF-DT                PIC XX.
000047         16  CS-CERT-NO.
000048             20  CS-CERT-PRIME             PIC X(10).
000049             20  CS-CERT-SFX               PIC X.
000050         16  CS-TRAILER-TYPE               PIC X.
000051             88  COMM-TRLR           VALUE 'A'.
000052             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
000053             88  CERT-DATA-TRLR      VALUE 'C'.
000054
000055     12  CS-DATA-AREA                      PIC X(516).
000056
000057     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
000058         16  CS-BANK-COMMISSION-AREA.
000059             20  CS-BANK-COMMS       OCCURS 10.
000060                 24  CS-AGT                PIC X(10).
000061                 24  CS-COM-TYP            PIC X.
000062                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
000063                 24  CS-RECALC-LV-INDIC    PIC X.
000064                 24  FILLER                PIC X(10).
000065
000066         16  FILLER                        PIC X(256).
000067
000068     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
000069****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
000070
000071         16  CS-MB-CLAIM-DATA OCCURS 24.
000072             20  CS-CLAIM-NO               PIC X(7).
000073             20  CS-CLAIM-TYPE             PIC X.
000074                 88  CS-AH-CLM               VALUE 'A'.
000075                 88  CS-IU-CLM               VALUE 'I'.
000076                 88  CS-GP-CLM               VALUE 'G'.
000077                 88  CS-LF-CLM               VALUE 'L'.
000078                 88  CS-PR-CLM               VALUE 'P'.
000079                 88  CS-FL-CLM               VALUE 'F'.
000080                 88  CS-OT-CLM               VALUE 'O'.
000081             20  CS-INSURED-TYPE           PIC X.
000082                 88  CS-PRIM-INSURED          VALUE 'P'.
000083                 88  CS-CO-BORROWER           VALUE 'C'.
000084             20  CS-BENEFIT-PERIOD         PIC 99.
000085             20  CS-DAYS-PAID              PIC S9(5) COMP-3.
000086             20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
000087             20  CS-REMAINING-BENS         PIC S999 COMP-3.
000088         16  FILLER                        PIC X(12).
000089
000090     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
000091         16  CS-VIN-NUMBER                 PIC X(17).
000092         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
000093         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
000094         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
000095         16  cs-agent-name.
000096             20  cs-agent-fname            pic x(20).
000097             20  cs-agent-mi               pic x.
000098             20  cs-agent-lname            pic x(25).
000099         16  cs-license-no                 pic x(15).
000100         16  cs-npn-number                 pic x(10).
000101         16  cs-agent-edit-status          pic x.
000102             88  cs-ae-refer-to-manager      value 'M'.
000103             88  cs-ae-cover-sheet           value 'C'.
000104             88  cs-ae-sig-form              value 'S'.
000105             88  cs-ae-verified              value 'V'.
000106             88  cs-unidentified-signature   value 'U'.
000107             88  cs-cert-returned            value 'R'.
000108             88  cs-accept-no-commission     value 'N'.
000109         16  cs-year                       pic 9999.
000110         16  cs-make                       pic x(20).
000111         16  cs-model                      pic x(20).
000112         16  cs-future                     pic x(20).
000113         16  cs-vehicle-odometer           pic s9(7) comp-3.
000114         16  cs-claim-verification-status  pic x.
000115             88  cs-clm-ver-eligible         value 'A'.
000116             88  cs-clm-ver-partial-elig     value 'B'.
000117             88  cs-clm-ver-not-eligible     value 'C'.
000118             88  cs-clm-ver-not-elig-opn-clm value 'D'.
000119             88  cs-clm-ver-not-part-elig-rw value 'E'.
000120             88  cs-clm-ver-ND-CERT          value 'F'.
000121             88  cs-clm-ver-spec-other       value 'G'.
000122             88  cs-clam-ver-pratial-corrected
000123                                             value 'H'.
000124             88  cs-clm-ver-no-matches       value 'I'.
000125             88  cs-clm-ver-not-elig-corrected
000126                                             value 'J'.
000127             88  cs-clm-ver-needs-review     value 'R'.
000128             88  cs-clm-ver-sent-to-claims   value 'W'.
000129         16  CS-LF-REFUND-METHOD           PIC X.
000130         16  CS-AH-REFUND-METHOD           PIC X.
000131         16  FILLER                        PIC X(353). *> was 420
000132*        16  FILLER                        PIC X(496).
      *<<((file: ELCCRTT))
000653*                                 COPY ELCCNTL.
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
000654*                                 COPY ELCFUNDT.
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
000655*                                 COPY ELCDATE.
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
000656*                                 COPY ELCCALC.
      *>>((file: ELCCALC))
000001******************************************************************
000002*                                                                *
000003*                           ELCCALC.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.025                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
000008*                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
000009*                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
000010*                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
000011*                                                                *
000012*  PASSED TO ELRTRM                                              *
000013*  -----------------                                             *
000014*  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
000015*  ORIGINAL TERM                                                 *
000016*  BEGINNING DATE                                                *
000017*  ENDING DATE                                                   *
000018*  COMPANY I.D.                                                  *
000019*  ACCOUNT MASTER USER FIELD                                     *
000020*  PROCESS SWITCH (CANCEL, CLAIM)                                *
000021*  FREE LOOK DAYS                                                *
000022*                                                                *
000023*  RETURNED FROM ELRTRM                                          *
000024*  ---------------------                                         *
000025*  REMAINING TERM 1 - USED FOR EARNINGS                          *
000026*  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
000027*  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
000028*  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
000029*----------------------------------------------------------------*
000030*  PASSED TO ELRAMT                                              *
000031*  ----------------                                              *
000032*  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
000033*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000034*  ORIGINAL AMOUNT                                               *
000035*  ALTERNATE BENEFIT (BALLON)                                    *
000036*  A.P.R. - NET PAY ONLY                                         *
000037*  METHOD
000038*  PAYMENT FREQUENCY - FOR FARM PLAN                             *
000039*  COMPANY I.D.                                                  *
000040*  BENEFIT TYPE                                                  *
000041*                                                                *
000042*  RETURNED FROM ELRAMT                                          *
000043*  --------------------                                          *
000044*  REMAINING AMOUNT 1 - CURRENT                                  *
000045*  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
000046*  REMAINING AMOUNT FACTOR
000047*----------------------------------------------------------------*
000048*  PASSED TO ELRESV                                              *
000049*  -----------------                                             *
000050*  CERTIFICATE EFFECTIVE DATE                                    *
000051*  VALUATION DATE                                                *
000052*  PAID THRU DATE                                                *
000053*  BENEFIT                                                       *
000054*  INCURRED DATE                                                 *
000055*  REPORTED DATE                                                 *
000056*  ISSUE AGE                                                     *
000057*  TERM                                                          *
000058*  CDT PERCENT                                                   *
000059*  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
000060* *CLAIM TYPE (LIFE, A/H)                                        *
000061* *REMAINING BENEFIT (FROM ELRAMT)                               *
000062* *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
000063*                                                                *
000064*  RETURNED FROM ELRESV                                          *
000065*  --------------------                                          *
000066*  CDT TABLE USED                                                *
000067*  CDT FACTOR USED                                               *
000068*  PAY TO CURRENT RESERVE                                        *
000069*  I.B.N.R. - A/H ONLY                                           *
000070*  FUTURE (ACCRUED) AH ONLY                                      *
000071*----------------------------------------------------------------*
000072*  PASSED TO ELRATE                                              *
000073*  ----------------                                              *
000074*  CERT ISSUE DATE                                               *
000075*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000076*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000077*  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
000078*  STATE CODE (CLIENT DEFINED)                                   *
000079*  STATE CODE (STANDARD P.O. ABBRV)                              *
000080*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000081*  DEVIATION CODE                                                *
000082*  ISSUE AGE                                                     *
000083*  ORIGINAL BENEFIT AMOUNT                                       *
000084*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000085*  PROCESS TYPE (ISSUE OR CANCEL)                                *
000086*  BENEFIT KIND (LIFE OR A/H)                                    *
000087*  A.P.R.                                                        *
000088*  METHOD
000089*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000090*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000091*  COMPANY I.D. (3 CHARACTER)                                    *
000092*  BENEFIT CODE                                                  *
000093*  BENEFIT OVERRIDE CODE                                         *
000094*  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
000095*  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
000096*  JOINT INDICATOR (CSL ONLY)                                    *
000097*  FIRST PAYMENT DATE (CSL ONLY)                                 *
000098*  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
000099*                                                                *
000100*  RETURNED FROM ELRATE                                          *
000101*  --------------------                                          *
000102*  CALCULATED PREMIUM                                            *
000103*  PREMIUM RATE                                                  *
000104*  MORTALITY CODE                                                *
000105*  MAX ATTAINED AGE                                              *
000106*  MAX AGE                                                       *
000107*  MAX TERM                                                      *
000108*  MAX MONTHLY BENEFIT                                           *
000109*  MAX TOTAL BENIFIT                                             *
000110*  COMPOSITE RATE (OPEN-END ONLY)                                *
000111*----------------------------------------------------------------*
000112*  PASSED TO ELRFND                                              *
000113*  ----------------                                              *
000114*  CERT ISSUE DATE                                               *
000115*  REFUND DATE                                                   *
000116*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000117*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000118*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000119*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000120*  STATE CODE (CLIENT DEFINED)                                   *
000121*  STATE CODE (STANDARD P.O. ABBRV)                              *
000122*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000123*  DEVIATION CODE                                                *
000124*  ISSUE AGE                                                     *
000125*  ORIGINAL BENEFIT AMOUNT                                       *
000126*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000127*  PROCESS TYPE (CANCEL)                                         *
000128*  BENEFIT KIND (LIFE OR A/H)                                    *
000129*  A.P.R.                                                        *
000130*  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
000131*  RATING METHOD -  (CODE FROM BENEFIT)                          *
000132*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000133*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000134*  COMPANY I.D. (3 CHARACTER)                                    *
000135*  BENEFIT CODE                                                  *
000136*  BENEFIT OVERRIDE CODE                                         *
000137*                                                                *
000138*  RETURNED FROM ELRFND                                          *
000139*  --------------------                                          *
000140*  CALCULATED REFUND                                             *
000141*----------------------------------------------------------------*
000142*  PASSED TO ELEARN                                              *
000143*  ----------------                                              *
000144*  CERT ISSUE DATE                                               *
000145*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000146*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000147*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000148*  STATE CODE (CLIENT DEFINED)                                   *
000149*  STATE CODE (STANDARD P.O. ABBRV)                              *
000150*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000151*  DEVIATION CODE                                                *
000152*  ISSUE AGE                                                     *
000153*  ORIGINAL BENEFIT AMOUNT                                       *
000154*  BENEFIT KIND (LIFE OR A/H)                                    *
000155*  A.P.R.                                                        *
000156*  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
000157*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000158*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000159*  COMPANY I.D. (3 CHARACTER)                                    *
000160*  BENEFIT CODE                                                  *
000161*  BENEFIT OVERRIDE CODE                                         *
000162*                                                                *
000163*  RETURNED FROM ELEARN                                          *
000164*  --------------------                                          *
000165*  INDICATED  EARNINGS                                           *
000166*----------------------------------------------------------------*
000167*                 LENGTH = 450                                   *
000168*                                                                *
000169******************************************************************
000170******************************************************************
000171*                   C H A N G E   L O G
000172*
000173* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000174*-----------------------------------------------------------------
000175*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000176* EFFECTIVE    NUMBER
000177*-----------------------------------------------------------------
000178* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
000179* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000180* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
000181* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
000182* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
000183* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000184* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
000185* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000186* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000187* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
000188* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000189* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000190******************************************************************
000191
000192 01  CALCULATION-PASS-AREA.
000193     12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
000194                                     COMP.
000195
000196     12  CP-RETURN-CODE            PIC X             VALUE ZERO.
000197       88  NO-CP-ERROR                             VALUE ZERO.
000198       88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
000199                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
000200       88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
000201       88  CP-ERROR-IN-DATES                       VALUE '2'.
000202       88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
000203       88  CP-ERROR-IN-TERMS                       VALUE '4'.
000204       88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
000205       88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
000206       88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
000207       88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
000208       88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
000209       88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
000210       88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
000211       88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
000212       88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
000213       88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
000214       88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
000215       88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
000216       88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
000217       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
000218
000219     12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
000220       88  NO-CP-ERROR-2                           VALUE ZERO.
000221***********************  INPUT AREAS ****************************
000222
000223     12  CP-CALCULATION-AREA.
000224         16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
000225         16  CP-CERT-EFF-DT        PIC XX.
000226         16  CP-VALUATION-DT       PIC XX.
000227         16  CP-PAID-THRU-DT       PIC XX.
000228         16  CP-BENEFIT-TYPE       PIC X.
000229           88  CP-AH                               VALUE 'A' 'D'
000230                                                   'I' 'U'.
000231           88  CP-REDUCING-LIFE                    VALUE 'R'.
000232           88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
000233         16  CP-INCURRED-DT        PIC XX.
000234         16  CP-REPORTED-DT        PIC XX.
000235         16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
000236         16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
000237         16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
000238                                     COMP-3.
000239         16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
000240                                     COMP-3.
000241         16  CP-CDT-METHOD         PIC X.
000242           88  CP-CDT-ROUND-NEAR                   VALUE '1'.
000243           88  CP-CDT-ROUND-HIGH                   VALUE '2'.
000244           88  CP-CDT-INTERPOLATED                 VALUE '3'.
000245         16  CP-CLAIM-TYPE         PIC X.
000246           88  CP-AH-CLAIM                         VALUE 'A'.
000247           88  CP-LIFE-CLAIM                       VALUE 'L'.
000248         16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
000249                                     COMP-3.
000250         16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
000251                                     COMP-3.
000252         16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
000253                                     COMP-3.
000254         16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
000255                                     COMP-3.
000256         16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
000257                                     COMP-3.
000258         16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
000259                                     COMP-3.
000260         16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
000261                                     COMP-3.
000262         16  CP-REM-TERM-METHOD    PIC X.
000263           88  CP-EARN-AFTER-15TH                  VALUE '1'.
000264           88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
000265           88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
000266           88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
000267           88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
000268           88  CP-EARN-AFTER-14TH                  VALUE '6'.
000269           88  CP-EARN-AFTER-16TH                  VALUE '7'.
000270         16  CP-EARNING-METHOD     PIC X.
000271           88  CP-EARN-BY-R78                      VALUE '1' 'R'.
000272           88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
000273           88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
000274           88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
000275           88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
000276           88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
000277           88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
000278           88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
000279           88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
000280           88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
000281           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
000282           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
000283           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
000284           88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
000285         16  CP-PROCESS-TYPE       PIC X.
000286           88  CP-CLAIM                            VALUE '1'.
000287           88  CP-CANCEL                           VALUE '2'.
000288           88  CP-ISSUE                            VALUE '3'.
000289         16  CP-SPECIAL-CALC-CD    PIC X.
000290           88  CP-OUTSTANDING-BAL              VALUE 'O'.
000291           88  CP-1-MTH-INTEREST               VALUE ' '.
000292           88  CP-0-MTH-INTEREST               VALUE 'A'.
000293           88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
000294           88  CP-CRITICAL-PERIOD              VALUE 'C'.
000295           88  CP-TERM-IS-DAYS                 VALUE 'D'.
000296           88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
000297           88  CP-FARM-PLAN                    VALUE 'F'.
000298           88  CP-RATE-AS-STANDARD             VALUE 'G'.
000299           88  CP-2-MTH-INTEREST               VALUE 'I'.
000300           88  CP-3-MTH-INTEREST               VALUE 'J'.
000301           88  CP-4-MTH-INTEREST               VALUE 'K'.
000302           88  CP-BALLOON-LAST-PMT             VALUE 'L'.
000303           88  CP-MORTGAGE-REC                 VALUE 'M'.
000304           88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
000305           88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
000306           88  CP-NET-PAY-SIMPLE               VALUE 'S'.
000307           88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
000308                                                     'W' 'X'.
000309           88  CP-TRUNCATE-0-MTH               VALUE 'T'.
000310           88  CP-TRUNCATE-1-MTH               VALUE 'U'.
000311           88  CP-TRUNCATE-2-MTH               VALUE 'V'.
000312           88  CP-TRUNCATE-3-MTH               VALUE 'W'.
000313           88  CP-TRUNCATE-4-MTH               VALUE 'X'.
000314           88  CP-SUMMARY-REC                  VALUE 'Z'.
000315           88  CP-PROPERTY-BENEFIT             VALUE '2'.
000316           88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
000317           88  CP-AD-D-BENEFIT                 VALUE '4'.
000318           88  CP-CSL-METH-1                   VALUE '5'.
000319           88  CP-CSL-METH-2                   VALUE '6'.
000320           88  CP-CSL-METH-3                   VALUE '7'.
000321           88  CP-CSL-METH-4                   VALUE '8'.
000322
000323         16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
000324                                     COMP-3.
000325         16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
000326         16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
000327         16  CP-STATE              PIC XX          VALUE SPACE.
000328         16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
000329         16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
000330           88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
000331               '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
000332         16  CP-R78-OPTION         PIC X.
000333           88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
000334           88  CP-TERM-TIMES-TERM                  VALUE '1'.
000335
000336         16  CP-COMPANY-CD         PIC X             VALUE SPACE.
000337         16  CP-IBNR-RESERVE-SW    PIC X.
000338         16  CP-CLAIM-STATUS       PIC X.
000339         16  CP-RATE-FILE          PIC X.
000340         16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
000341                                     COMP-3.
000342
000343         16  CP-LIFE-OVERRIDE-CODE PIC X.
000344         16  CP-AH-OVERRIDE-CODE   PIC X.
000345
000346         16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
000347                                     COMP-3.
000348         16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
000349                                   PIC S9(5)V99 COMP-3.
000350         16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
000351                                     COMP-3.
000352         16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
000353                                     COMP-3.
000354         16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
000355                                     COMP-3.
000356         16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
000357                                  PIC S9(7)V99 COMP-3.
000358
000359         16  CP-PAID-FROM-DATE     PIC X(02).
000360         16  CP-CLAIM-CALC-METHOD  PIC X(01).
000361         16  CP-EXT-DAYS-CALC      PIC X.
000362           88  CP-EXT-NO-CHG                   VALUE ' '.
000363           88  CP-EXT-CHG-LF                   VALUE '1'.
000364           88  CP-EXT-CHG-AH                   VALUE '2'.
000365           88  CP-EXT-CHG-LF-AH                VALUE '3'.
000366         16  CP-DOMICILE-STATE     PIC XX.
000367         16  CP-CARRIER            PIC X.
000368         16  CP-REIN-FLAG          PIC X.
000369         16  CP-REM-TRM-CALC-OPTION PIC X.
000370           88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
000371                      '2' '3' '4' '5'.
000372           88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
000373           88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
000374           88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
000375           88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
000376           88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
000377           88  CP-EXT-30-DAY-MONTH          VALUE '3'.
000378           88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
000379           88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
000380         16  CP-SIG-SWITCH         PIC X.
000381         16  CP-RATING-METHOD      PIC X.
000382           88  CP-RATE-AS-R78                      VALUE '1' 'R'.
000383           88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
000384           88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
000385           88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
000386           88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
000387           88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
000388           88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
000389           88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
000390           88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
000391         16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
000392                                     COMP-3.
000393         16  CP-BEN-CATEGORY       PIC X.
000394         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000395         16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
000396                                   PIC S99V9(5) COMP-3.
000397         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000398         16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
000399                                   PIC S99V9(5) COMP-3.
000400         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
000401         16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
000402         16  CP-EXPIRE-DT          PIC XX.
000403         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
000404         16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
000405         16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
000406         16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
000407         16  CP-DDF-SPEC-CALC      PIC X.
000408             88  CP-CALC-GROSS-FEE        VALUE 'G'.
000409             88  CP-CALC-CLP              VALUE 'C'.
000410         16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
000411         16  CP-CANCEL-REASON      PIC X.
000412         16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
000413         16  CP-PMT-MODE           PIC X.
000414         16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
000415         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
000416         16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
000417         16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
000418         16  FILLER                PIC X.
000419
000420***************    OUTPUT FROM ELRESV   ************************
000421
000422         16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
000423
000424         16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
000425                                     COMP-3.
000426         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
000427                                     COMP-3.
000428         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
000429                                     COMP-3.
000430         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
000431                                     COMP-3.
000432         16  FILLER                PIC X(09).
000433***************    OUTPUT FROM ELRTRM   *************************
000434
000435         16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
000436                                     COMP-3.
000437         16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
000438                                     COMP-3.
000439         16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
000440                                     COMP-3.
000441         16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
000442                                     COMP-3.
000443         16  FILLER                PIC X(12).
000444
000445***************    OUTPUT FROM ELRAMT   *************************
000446
000447         16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
000448                                     COMP-3.
000449         16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
000450                                     COMP-3.
000451         16  FILLER                PIC X(12).
000452
000453***************    OUTPUT FROM ELRATE   *************************
000454
000455         16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
000456                                     COMP-3.
000457         16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
000458                                     COMP-3.
000459         16  CP-MORTALITY-CODE     PIC X(4).
000460         16  CP-RATE-EDIT-FLAG     PIC X.
000461             88  CP-RATES-NEED-APR                  VALUE '1'.
000462         16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
000463                                     COMP-3.
000464         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
000465         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
000466         16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
000467                                   PIC S9(7)V99 COMP-3.
000468         16  FILLER                PIC X(07).
000469
000470***************    OUTPUT FROM ELRFND   *************************
000471
000472         16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
000473                                     COMP-3.
000474         16  CP-REFUND-TYPE-USED   PIC X.
000475           88  CP-R-AS-R78                         VALUE '1'.
000476           88  CP-R-AS-PRORATA                     VALUE '2'.
000477           88  CP-R-AS-CALIF                       VALUE '3'.
000478           88  CP-R-AS-TEXAS                       VALUE '4'.
000479           88  CP-R-AS-FARM-PLAN                   VALUE '4'.
000480           88  CP-R-AS-NET-PAY                     VALUE '5'.
000481           88  CP-R-AS-ANTICIPATION                VALUE '6'.
000482           88  CP-R-AS-MEAN                        VALUE '8'.
000483           88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
000484           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
000485           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
000486           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
000487           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
000488           88  CP-R-AS-REPOSSESSION                VALUE 'R'.
000489         16  FILLER                PIC X(12).
000490
000491***************    OUTPUT FROM ELEARN   *************************
000492
000493         16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
000494                                     COMP-3.
000495         16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
000496                                     COMP-3.
000497         16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
000498                                     COMP-3.
000499         16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
000500                                     COMP-3.
000501         16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
000502                                     COMP-3.
000503         16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
000504                                     COMP-3.
000505         16  CP-EARNING-TYPE-USED  PIC X.
000506           88  CP-E-AS-SPECIAL                     VALUE 'S'.
000507           88  CP-E-AS-R78                         VALUE '1'.
000508           88  CP-E-AS-PRORATA                     VALUE '2'.
000509           88  CP-E-AS-TEXAS                       VALUE '4'.
000510           88  CP-E-AS-FARM-PLAN                   VALUE '4'.
000511           88  CP-E-AS-NET-PAY                     VALUE '5'.
000512           88  CP-E-AS-ANTICIPATION                VALUE '6'.
000513           88  CP-E-AS-MEAN                        VALUE '8'.
000514           88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
000515         16  FILLER                PIC X(12).
000516
000517***************    OUTPUT FROM ELPMNT   *************************
000518
000519         16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
000520                                     COMP-3.
000521         16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
000522                                     COMP-3.
000523         16  FILLER                PIC X(12).
000524
000525***************   MISC WORK AREAS    *****************************
000526         16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
000527                                     COMP-3.
000528         16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
000529                                     COMP-3.
000530         16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
000531                                     COMP-3.
000532         16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
000533                                     COMP-3.
000534         16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
000535                                     COMP-3.
000536         16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
000537                                     COMP-3.
000538         16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
000539             88  OPEN-RATE-FILE                   VALUE 'O'.
000540             88  CLOSE-RATE-FILE                  VALUE 'C'.
000541             88  IO-ERROR                         VALUE 'E'.
000542
000543         16  CP-FIRST-PAY-DATE     PIC XX.
000544
000545         16  CP-JOINT-INDICATOR    PIC X.
000546
000547         16  CP-RESERVE-REMAINING-TERM
000548                                   PIC S9(4)V9    VALUE ZERO
000549                                     COMP-3.
000550
000551         16  CP-INSURED-BIRTH-DT   PIC XX.
000552
000553         16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
000554                                     COMP-3.
000555
000556         16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
000557                                     COMP-3.
000558
000559         16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
000560                                     COMP-3.
000561
000562         16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
000563                                     COMP-3.
000564
000565         16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
000566                                     COMP-3.
000567
000568         16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
000569                                     COMP-3.
000570
000571         16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
000572                                     COMP-3.
000573
000574         16  CP-ROA-REFUND         PIC X          VALUE 'N'.
000575             88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
000576
000577         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
000578                                     COMP-3.
000579         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
000580                                     COMP-3.
000581         16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
000582         16  cp-extra-periods      pic 9 value zeros.
000583         16  cp-net-only-state     pic x value spaces.
000584         16  FILLER                PIC X(13).
000585******************************************************************
      *<<((file: ELCCALC))
000657
000658 01  sqlconnect-parms.
000659     05  p-sql-server            PIC X(30).
000660     05  p-sql-database          PIC X(30).
000661     05  p-connect-return-code   pic s9(5) comp-5.
000662     05  p-sql-return-message    pic x(256).
000663
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
000665 01  DFHCOMMAREA                 pic x(1024).
000666
000667 01  var  pic x(30).
000668
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'WSMESS02' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000669 VCOBOL-DUMMY-PROCEDURE.
000670
000671     display ' Entering program WSMESS02 '
000672     move dfhcommarea            to ws-mess002-pass-area
000673
000674     perform 0000-init           thru 0000-exit
000675
000676     perform 0010-process-message
000677                                 thru 0010-exit
000678*    move ' returning string goes here ' to ws-return-string
000679
000680     go to 0300-RETURN-CICS
000681
000682     .
000683 0000-init.
000684
000685     move 'CID'                  to client-id
000686
000687     MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
000688     move ws-fn-cymd             to dc-greg-date-cymd
000689     move 'L'                    to dc-option-code
000690     perform 9700-date-link      thru 9700-exit
000691     if no-conversion-error
000692        move dc-bin-date-1       to ws-bin-current-dt
000693     else
000694        display ' error current dt invalid ' dc-error-code
000695     end-if
000696
000697     set P to address of KIXSYS
000698     CALL "getenv" using by value P returning var-ptr
000699     if var-ptr = null then
000700        display ' kixsys not set '
000701     else
000702        set address of var to var-ptr
000703        move 0 to env-var-len
000704        inspect var tallying env-var-len
000705          for characters before X'00'
000706        unstring var (1:env-var-len) delimited by '/'
000707           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000708              WS-KIX-SYS
000709        end-unstring
000710     end-if
000711
000712     evaluate client-id
000713        when 'DCC'
000714           MOVE X'05'            TO WS-COMP-CD
000715           MOVE 'DCC'            TO WS-COMP-ID
000716        when 'CID'
000717           MOVE X'04'            TO WS-COMP-CD
000718           MOVE 'CID'            TO WS-COMP-ID
000719        when 'AHL'
000720           MOVE X'06'            TO WS-COMP-CD
000721           MOVE 'AHL'            TO WS-COMP-ID
000722        when other
000723           display ' Invalid company id ' client-id
000724           move 23               to ws-error-sub
000725           move client-id        to ws-error-sup
000726           perform 0180-error-handle
000727                                 thru 0180-exit
000728*          move '0113;Invalid company id ' to ws-return-string
000729           go to 0300-RETURN-CICS
000730     END-evaluate
000731
000732     move 1                      to ws-error-sub
000733     move low-values             to ws-bin-lf-exp-dt
000734                                    ws-bin-ah-exp-dt
000735                                    ws-bin-loan-exp-dt
000736     move spaces                 to ws-return-string
000737     move ';'                    to ws-sc1
000738                                    ws-sc2
000739                                    ws-sc3
000740                                    ws-sc4
000741                                    ws-sc5
000742                                    ws-sc6
000743                                    ws-sc7
000744                                    ws-sc8
000745                                    ws-sc9
000746                                    ws-sc10
000747                                    ws-sc11
000748                                    ws-sc12
000749                                    ws-sc13
000750                                    ws-sc14
000751                                    ws-sc15
000752                                    ws-sc16
000753                                    ws-sc17
000754                                    ws-sc18
000755                                    ws-sc19
000756                                    ws-sc20
000757                                    ws-sc21
000758                                    ws-sc22
000759                                    ws-sc23
000760                                    ws-sc24
000761
000762     .
000763 0000-exit.
000764     exit.
000765
000766 0010-process-message.
000767
000768     perform 0110-unstring       thru 0110-exit
000769     perform 0120-format-message thru 0120-exit
000770
000771     perform 0020-edit-received  thru 0020-exit
000772
000773     perform 0050-get-account    thru 0050-exit
000774     perform 0090-get-limits     thru 0090-exit
000775
000776     perform 0025-assign-ben-cd  thru 0025-exit
000777     perform 0030-get-lf-rate    thru 0030-exit
000778     perform 0035-get-ah-rate    thru 0035-exit
000779
000780     if ws-rate-in-lf-ben-code (1:1) = 'N' or 'T'
000781        perform 0200-calc-NP-payment
000782                                 thru 0200-exit
000783     else
000784        perform 0250-calc-GP-payment
000785                                 thru 0250-exit
000786     end-if
000787
000788*    go to 0010-exit
000789
000790     if (contract-no-assigned)
000791            or
000792        (error-in-one-coverage)
000793        continue
000794     else
000795        perform 0190-final-limit-check
000796                                 thru 0190-exit
000797        perform 0060-check-for-dup
000798                                 thru 0060-exit
000799        perform 0070-open-cursor thru 0070-exit
000800        perform 0080-process-input
000801                                 thru 0080-exit
000802     end-if
000803     perform 0040-format-buffer  thru 0040-exit
000804     set contract-no-assigned to true
000805
000806     .
000807 0010-exit.
000808     exit.
000809
000810 0020-edit-received.
000811
000812     if ws-rate-sin-jnt-lf = 'S' OR 'J'
000813        continue
000814     else
000815        move 'S'                 to ws-rate-sin-jnt-lf
000816     end-if
000817     if ws-rate-sin-jnt-ah = 'S' OR 'J'
000818        continue
000819     else
000820        move 'S'                 to ws-rate-sin-jnt-ah
000821     end-if
000822     if ws-rate-dismemberment = 'Y'
000823        continue
000824     else
000825        move 'N'                 to ws-rate-dismemberment
000826     end-if
000827
000828     if ws-rate-in-ah-ben-code = 'A'
000829        if ws-rate-retro-elim = 'R' or 'E'
000830           continue
000831        else
000832           move 'R'              to ws-rate-retro-elim
000833        end-if
000834     else
000835        move spaces              to ws-rate-retro-elim
000836     end-if
000837
000838**==============================================================**
000839**                                                              **
000840**  Validate passed effective date. Probably should add edits   **
000841**                                                              **
000842**==============================================================**
000843
000844     move ws-rate-eff-date (7:4) to ws-work-date (1:4)
000845     move ws-rate-eff-date (1:2) to ws-work-date (5:2)
000846     move ws-rate-eff-date (4:2) to ws-work-date (7:2)
000847     move ws-work-date-num       to dc-greg-date-cymd
000848     move 'L'                    to dc-option-code
000849     perform 9700-date-link      thru 9700-exit
000850     if no-conversion-error
000851        move dc-bin-date-1       to ws-bin-eff-dt
000852     else
000853        move 3                   to ws-error-sub
000854        move '- Eff Date'        to ws-error-sup
000855        perform 0180-error-handle
000856                                 thru 0180-exit
000857        go to 0300-RETURN-CICS
000858     end-if
000859
000860     if ws-bin-eff-dt > ws-bin-current-dt
000861        move 3                   to ws-error-sub
000862        move '- Future Eff'      to ws-error-sup
000863        perform 0180-error-handle
000864                                 thru 0180-exit
000865        go to 0300-RETURN-CICS
000866     end-if
000867
000868**==============================================================**
000869**                                                              **
000870**  Validate passed 1st pmt date.   Probably should add edits   **
000871**                                                              **
000872**==============================================================**
000873
000874     move ws-rate-1st-pmt-dt (7:4)
000875                                 to ws-work-date (1:4)
000876     move ws-rate-1st-pmt-dt (1:2)
000877                                 to ws-work-date (5:2)
000878     move ws-rate-1st-pmt-dt (4:2)
000879                                 to ws-work-date (7:2)
000880     move ws-work-date-num       to dc-greg-date-cymd
000881     move 'L'                    to dc-option-code
000882     perform 9700-date-link      thru 9700-exit
000883     if no-conversion-error
000884        move dc-bin-date-1       to ws-bin-1st-pmt-dt
000885     else
000886        move low-values          to ws-bin-1st-pmt-dt
000887        move 3                   to ws-error-sub
000888        move '- 1st Pmt Dt'      to ws-error-sup
000889        perform 0180-error-handle
000890                                 thru 0180-exit
000891        go to 0300-RETURN-CICS
000892     end-if
000893
000894     if ws-bin-1st-pmt-dt <= ws-bin-eff-dt
000895        move low-values          to ws-bin-1st-pmt-dt
000896        move 3                   to ws-error-sub
000897        move '- 1st Pmt !> Eff dt' to ws-error-sup
000898        perform 0180-error-handle
000899                                 thru 0180-exit
000900        go to 0300-RETURN-CICS
000901     end-if
000902
000903**==============================================================**
000904**                                                              **
000905**  Only use the code below if there is a time they don't       **
000906**  send the 1st pmt date.                                      **
000907**                                                              **
000908**==============================================================**
000909
000910     if ws-bin-1st-pmt-dt = low-values
000911        move +1                  to dc-elapsed-months
000912        move +0                  to dc-elapsed-days
000913        move '6'                 to dc-option-code
000914        perform 9700-date-link   thru 9700-exit
000915        if no-conversion-error
000916           move dc-bin-date-2    to ws-bin-1st-pmt-dt
000917        else
000918           move 3                to ws-error-sub
000919           move '- 1st Pmt Dt'   to ws-error-sup
000920           perform 0180-error-handle
000921                                 thru 0180-exit
000922           go to 0300-RETURN-CICS
000923        end-if
000924     end-if
000925
000926**==============================================================**
000927**                                                              **
000928**  Calculate the days to first payment date                    **
000929**                                                              **
000930**==============================================================**
000931
000932     move ws-bin-eff-dt          to dc-bin-date-1
000933     move ws-bin-1st-pmt-dt      to dc-bin-date-2
000934     move zeros                  to dc-elapsed-months
000935                                    dc-elapsed-days
000936     move '1'                    to dc-option-code
000937     perform 9700-date-link   thru 9700-exit
000938     if no-conversion-error
000939        move dc-elapsed-days     to d
000940     else
000941        move 30                  to d
000942     end-if
000943
000944*    if d = 31
000945*       move 30 to d
000946*    end-if
000947
000948**==============================================================**
000949**                                                              **
000950**  Sometimes a lf term is passed to this module                **
000951**   even thought there is no life coverage. I try and rate it  **
000952**                                                              **
000953**==============================================================**
000954
000955     if ws-rate-in-lf-ben-code = spaces
000956        move zeros               to ws-rate-lf-term
000957     end-if
000958
000959     .
000960 0020-calc-lf-exp.
000961**==============================================================**
000962**                                                              **
000963**  Calculate the lf expiration date to be passed back.         **
000964**                                                              **
000965**==============================================================**
000966
000967     if ws-rate-lf-term > 0
000968        move ws-bin-1st-pmt-dt   to dc-bin-date-1
000969        compute dc-elapsed-months = ws-rate-lf-term - 1
000970        move +0                  to dc-elapsed-days
000971        move '6'                 to dc-option-code
000972        perform 9700-date-link   thru 9700-exit
000973        if no-conversion-error
000974           move dc-bin-date-2    to ws-bin-lf-exp-dt
000975           move dc-greg-date-a-edit
000976                                 to ws-lf-exp-date
000977        else
000978           move 3                to ws-error-sub
000979           move '- Lf Exp Dt'    to ws-error-sup
000980           perform 0180-error-handle
000981                                 thru 0180-exit
000982           go to 0300-RETURN-CICS
000983        end-if
000984     end-if
000985
000986     .
000987 0020-calc-ah-exp.
000988**==============================================================**
000989**                                                              **
000990**  Calculate the ah expiration date to be passed back.         **
000991**                                                              **
000992**==============================================================**
000993
000994     move ws-bin-1st-pmt-dt      to dc-bin-date-1
000995     compute dc-elapsed-months = ws-rate-ah-term - 1
000996     move +0                     to dc-elapsed-days
000997     move '6'                    to dc-option-code
000998     perform 9700-date-link      thru 9700-exit
000999     if no-conversion-error
001000        move dc-bin-date-2       to ws-bin-ah-exp-dt
001001        move dc-greg-date-a-edit to ws-ah-exp-date
001002     else
001003        move 3                   to ws-error-sub
001004        move '- Ah Exp Dt'       to ws-error-sup
001005        perform 0180-error-handle
001006                                 thru 0180-exit
001007        go to 0300-RETURN-CICS
001008     end-if
001009
001010     .
001011 0020-calc-loan-exp.
001012
001013**==============================================================**
001014**                                                              **
001015**  Calculate the loan expiration date to be passed back.       **
001016**                                                              **
001017**==============================================================**
001018
001019     move ws-bin-1st-pmt-dt      to dc-bin-date-1
001020     compute dc-elapsed-months = ws-rate-loan-term - 1
001021     move +0                     to dc-elapsed-days
001022     move '6'                    to dc-option-code
001023     perform 9700-date-link      thru 9700-exit
001024     if no-conversion-error
001025        move dc-bin-date-2       to ws-bin-loan-exp-dt
001026        move dc-greg-date-a-edit to ws-loan-exp-date
001027     else
001028        move 3                   to ws-error-sub
001029        move '- Loan Exp Dt'     to ws-error-sup
001030        perform 0180-error-handle
001031                                 thru 0180-exit
001032        go to 0300-RETURN-CICS
001033     end-if
001034
001035**==============================================================**
001036**                                                              **
001037**  Validate primary borrower birth date.                       **
001038**                                                              **
001039**==============================================================**
001040
001041     move ws-rate-pri-birth-date (7:4)
001042                                 to ws-work-date (1:4)
001043     move ws-rate-pri-birth-date (1:2)
001044                                 to ws-work-date (5:2)
001045     move ws-rate-pri-birth-date (4:2)
001046                                 to ws-work-date (7:2)
001047     move ws-work-date-num       to dc-greg-date-cymd
001048     move 'L'                    to dc-option-code
001049     perform 9700-date-link      thru 9700-exit
001050     if no-conversion-error
001051        move dc-bin-date-1       to ws-bin-pri-birth-dt
001052     else
001053        move 3                   to ws-error-sub
001054        move '- Pri DOB '        to ws-error-sup
001055        perform 0180-error-handle
001056                                 thru 0180-exit
001057        go to 0300-RETURN-CICS
001058     end-if
001059
001060**==============================================================**
001061**                                                              **
001062**  Validate co borrowers birth date.                           **
001063**                                                              **
001064**==============================================================**
001065
001066     if ws-rate-cob-birth-date (7:4) = spaces
001067        move zeros               to ws-rate-cob-birth-date
001068     end-if
001069     move ws-rate-cob-birth-date (7:4)
001070                                 to ws-work-date (1:4)
001071     move ws-rate-cob-birth-date (1:2)
001072                                 to ws-work-date (5:2)
001073     move ws-rate-cob-birth-date (4:2)
001074                                 to ws-work-date (7:2)
001075     move low-values             to ws-bin-cob-birth-dt
001076     if ws-work-date-num not = zeros
001077        move ws-work-date-num    to dc-greg-date-cymd
001078        move 'L'                 to dc-option-code
001079        perform 9700-date-link   thru 9700-exit
001080        if no-conversion-error
001081           move dc-bin-date-1    to ws-bin-cob-birth-dt
001082        else
001083           move 3                to ws-error-sub
001084           move '- Cob DOB '     to ws-error-sup
001085           perform 0180-error-handle
001086                                 thru 0180-exit
001087           go to 0300-RETURN-CICS
001088        end-if
001089     end-if
001090
001091**==============================================================**
001092**                                                              **
001093**  Calculate primary borrowers age.                            **
001094**                                                              **
001095**==============================================================**
001096
001097     move ws-bin-pri-birth-dt    to dc-bin-date-1
001098     move ws-bin-eff-dt          to dc-bin-date-2
001099     move '1'                    to dc-option-code
001100     perform 9700-date-link      thru 9700-exit
001101     if no-conversion-error
001102        compute ws-issue-age = dc-elapsed-months / +12
001103     else
001104        move 3                   to ws-error-sub
001105        move '- Pri DOB '        to ws-error-sup
001106        perform 0180-error-handle
001107                                 thru 0180-exit
001108        go to 0300-RETURN-CICS
001109     end-if
001110
001111     .
001112 0020-calc-pri-att-age.
001113**==============================================================**
001114**                                                              **
001115**  Calculate primary borrowers age at expiration date          **
001116**                                                              **
001117**==============================================================**
001118
001119     move ws-bin-pri-birth-dt    to dc-bin-date-1
001120     move ws-bin-lf-exp-dt       to dc-bin-date-2
001121     if ws-bin-ah-exp-dt > dc-bin-date-2
001122        move ws-bin-ah-exp-dt    to dc-bin-date-2
001123     end-if
001124     move '1'                    to dc-option-code
001125     perform 9700-date-link      thru 9700-exit
001126     if no-conversion-error
001127        compute ws-pri-att-age =
001128           dc-elapsed-months / +12
001129     else
001130        move 3                   to ws-error-sub
001131        move '- Pri DOB '        to ws-error-sup
001132        perform 0180-error-handle
001133                                 thru 0180-exit
001134        go to 0300-RETURN-CICS
001135     end-if
001136
001137     .
001138 0020-calc-cob-age.
001139**==============================================================**
001140**                                                              **
001141**  Calculate co borrowers age.                                 **
001142**                                                              **
001143**==============================================================**
001144
001145     move zeros                  to ws-cob-age
001146     if ws-bin-cob-birth-dt not = low-values
001147        move ws-bin-cob-birth-dt to dc-bin-date-1
001148        move ws-bin-eff-dt       to dc-bin-date-2
001149        move '1'                 to dc-option-code
001150        perform 9700-date-link   thru 9700-exit
001151        if no-conversion-error
001152           compute ws-cob-age = dc-elapsed-months / +12
001153        else
001154           move 3                to ws-error-sub
001155           move '- Cob DOB '     to ws-error-sup
001156           perform 0180-error-handle
001157                                 thru 0180-exit
001158           go to 0300-RETURN-CICS
001159        end-if
001160     end-if
001161
001162     move ws-issue-age           to ws-rate-age
001163     if ws-cob-age > ws-rate-age
001164        move ws-cob-age          to ws-rate-age
001165     end-if
001166
001167     .
001168 0020-calc-cob-att-age.
001169**==============================================================**
001170**                                                              **
001171**  Calculate co borrowers age at expiration date               **
001172**                                                              **
001173**==============================================================**
001174
001175     move zeros                  to ws-cob-att-age
001176     if ws-bin-cob-birth-dt not = low-values
001177        move ws-bin-cob-birth-dt to dc-bin-date-1
001178        move ws-bin-lf-exp-dt    to dc-bin-date-2
001179        if ws-bin-ah-exp-dt > dc-bin-date-2
001180           move ws-bin-ah-exp-dt to dc-bin-date-2
001181        end-if
001182        move '1'                 to dc-option-code
001183        perform 9700-date-link   thru 9700-exit
001184        if no-conversion-error
001185           compute ws-cob-att-age =
001186              dc-elapsed-months / +12
001187        else
001188           move 3                to ws-error-sub
001189           move '- Cob DOB '     to ws-error-sup
001190           perform 0180-error-handle
001191                                 thru 0180-exit
001192           go to 0300-RETURN-CICS
001193        end-if
001194     end-if
001195
001196     move ws-cob-att-age         to ws-att-age
001197     if ws-pri-att-age > ws-att-age
001198        move ws-pri-att-age      to ws-att-age
001199     end-if
001200
001201     .
001202 0020-val-terms.
001203**==============================================================**
001204**                                                              **
001205**  Validate life and dis terms                                 **
001206**                                                              **
001207**==============================================================**
001208
001209     if (ws-rate-lf-term > 0)
001210        and (ws-rate-ah-term > 0)
001211        if ws-rate-lf-term <> ws-rate-ah-term
001212           move 27               to ws-error-sub
001213           move ' '              to ws-error-sup
001214           perform 0180-error-handle
001215                                 thru 0180-exit
001216           go to 0300-RETURN-CICS
001217        end-if
001218     end-if
001219
001220**==============================================================**
001221**                                                              **
001222**  Validate Loan term against insurance term.                  **
001223**                                                              **
001224**==============================================================**
001225
001226     if ((ws-rate-in-lf-ben-code (1:1) = 'G')
001227        and (ws-rate-lf-term > 0)
001228        and (ws-rate-lf-term <> ws-rate-loan-term))
001229                  or
001230        ((ws-rate-in-lf-ben-code (i:1) = ' ')
001231        and (ws-rate-ah-term > 0)
001232        and (ws-rate-ah-term <> ws-rate-loan-term))
001233        move 28               to ws-error-sub
001234        move ' '              to ws-error-sup
001235        perform 0180-error-handle
001236                                 thru 0180-exit
001237        go to 0300-RETURN-CICS
001238     end-if
001239
001240     .
001241 0020-exit.
001242     exit.
001243
001244 0025-assign-ben-cd.
001245
001246     if not connected-to-db
001247        perform 6000-connect-to-db thru 6000-exit
001248     end-if
001249
001250     move ws-rate-state          to ws-sbc-state
001251
001252     evaluate true
001253        when ws-rate-in-lf-ben-code (1:1) = 'N'
001254           move 'NP'             to ws-sbc-cov-type
001255        when ws-rate-in-lf-ben-code (1:1) = 'T'
001256           move 'NT'             to ws-sbc-cov-type
001257        when ws-rate-in-lf-ben-code (1:1) = 'L'
001258           move 'LL'             to ws-sbc-cov-type
001259        when ws-rate-in-lf-ben-code (1:1) = ' '
001260           go to 0025-get-ah-ben
001261        when other
001262           move 'GP'             to ws-sbc-cov-type
001263     end-evaluate
001264
001265     move ws-rate-loan-amt       to ws-sbc-ben-amt
001266     move ws-rate-sin-jnt-lf     to ws-sbc-sin-jnt
001267     move ws-rate-dismemberment  to ws-sbc-dismember
001268
001270     EXEC SQL
              SELECT TOP (1)
001271           MIN(BenefitAmt) as LessAmt,
001272           LogicBenCode
001273        INTO
001274           :ws-sbc-min-amt,
001275           :ws-sbc-logic-ben-code
001276        FROM
001277           BenefitCodeMapping
001278        WHERE
001279           State           = :ws-sbc-state
001280           and CovType     = :ws-sbc-cov-type
001281           and SinJnt      = :ws-sbc-sin-jnt
001282           and Dismem      = :ws-sbc-dismember
001283           and BenefitAmt  > :ws-sbc-ben-amt
001284        GROUP BY
001285           LogicBenCode
001286        ORDER BY
001287           LessAmt
001288     end-exec
001289
001290     if sqlcode not = 0 and 1
001291        display "Error: cannot find lf benefit code "
001292           ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
001293           ' ' ws-sbc-dismember
001294        display ' sql return code ' sqlcode
001295        display ' sql err mess    ' sqlerrmc
001296        move 24                  to ws-error-sub
001297        move spaces to ws-error-sup
001298        string
001299           ws-sbc-state ' '
001300           ws-sbc-cov-type ' '
001301           ws-sbc-sin-jnt ' '
001302           ws-sbc-dismember
001303           delimited by size into ws-error-sup
001304        perform 0180-error-handle
001305                                 thru 0180-exit
001306        go to 0300-return-cics
001307     end-if
001308
001309**==============================================================**
001310**                                                              **
001311** The below code already works, just want to try some stuff    **
001312**                                                              **
001313**==============================================================**
001314
001315*    EXEC SQL
001316*       SELECT
001317*          LogicBenCode
001318*       INTO
001319*          :ws-sbc-logic-ben-code
001320*       FROM
001321*          BenefitCodeMapping
001322*       WHERE
001323*          State           = :ws-sbc-state
001324*          and CovType     = :ws-sbc-cov-type
001325*          and SinJnt      = :ws-sbc-sin-jnt
001326*          and Dismem      = :ws-sbc-dismember
001327*          and BenefitAmt  > :ws-sbc-ben-amt
001328*    end-exec
001329*
001330*    if sqlcode not = 0
001331*       display "Error: cannot find lf benefit code "
001332*          ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
001333*          ' ' ws-sbc-dismember
001334*       display ' sql return code ' sqlcode
001335*       display ' sql err mess    ' sqlerrmc
001336*       move 19                  to ws-error-sub
001337*       move spaces to ws-error-sup
001338*       string
001339*          ws-sbc-state ' '
001340*          ws-sbc-cov-type ' '
001341*          ws-sbc-sin-jnt ' '
001342*          ws-sbc-dismember
001343*          delimited by size into ws-error-sup
001344*       perform 0180-error-handle
001345*                                thru 0180-exit
001346*       go to 0300-return-cics
001347*    end-if
001348
001349     move ws-sbc-logic-ben-code  to ws-rate-lf-benefit-cd
001350
001351     .
001352 0025-get-ah-ben.
001353
001354     if ws-rate-in-ah-ben-code (1:1) = ' '
001355        go to 0025-exit
001356     end-if
001357
001358     move ws-rate-state          to ws-sbc-state
001359     move 'DI'                   to ws-sbc-cov-type
001360     move ws-rate-loan-amt       to ws-sbc-ben-amt
001361     move ws-rate-sin-jnt-ah     to ws-sbc-sin-jnt
001362     move ws-rate-retro-elim     to ws-sbc-retroelim
001363     move ws-rate-waiting-days   to ws-sbc-wait-days
001364     move ws-rate-crit-per       to ws-sbc-max-bens
001365
001367     EXEC SQL
              SELECT TOP (1)
001368           MIN(BenefitAmt) as LessAmt,
001369           LogicBenCode
001370        INTO
001371           :ws-sbc-min-amt,
001372           :ws-sbc-logic-ben-code
001373        FROM
001374           BenefitCodeMapping
001375        WHERE
001376           State           = :ws-sbc-state
001377           and CovType     = :ws-sbc-cov-type
001378           and SinJnt      = :ws-sbc-sin-jnt
001379           and RetroElim   = :ws-sbc-retroelim
001380           and WaitDays    = :ws-sbc-wait-days
001381           and MaxBens     = :ws-sbc-max-bens
001382           and BenefitAmt  > :ws-sbc-ben-amt
001383        GROUP BY
001384           LogicBenCode
001385        ORDER BY
001386           LessAmt
001387     end-exec
001388
001389     if sqlcode not = 0
001390        display "Error: cannot find ah benefit code "
001391           ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
001392           ' ' ws-sbc-retroelim ' ' ws-sbc-wait-days ' '
001393           ws-sbc-max-bens
001394
001395        display ' sql return code ' sqlcode
001396        display ' sql err mess    ' sqlerrmc
001397        move 25                  to ws-error-sub
001398        move spaces              to ws-error-sup
001399        string
001400           ws-sbc-state ' '
001401           ws-sbc-cov-type ' '
001402           ws-sbc-sin-jnt ' '
001403           ws-sbc-retroelim ' '
001404           ws-sbc-wait-days ' '
001405           ws-sbc-max-bens
001406           delimited by size into ws-error-sup
001407        perform 0180-error-handle
001408                                 thru 0180-exit
001409        go to 0300-return-cics
001410     end-if
001411
001412     move ws-sbc-logic-ben-code  to ws-rate-ah-benefit-cd
001413
001414     .
001415 0025-exit.
001416     exit.
001417
001418 0030-get-lf-rate.
001419
001420     if (ws-rate-in-lf-ben-code (1:1) = ' ')
001421        or (ws-rate-lf-term = zeros)
001422        go to 0030-exit
001423     end-if
001424
001425*    move spaces                 to calculation-pass-area
001426     move zeros                  to cp-r-max-mon-ben
001427                                    cp-r-max-tot-ben
001428                                    cp-rate-dev-pct
001429                                    cp-original-premium
001430                                    cp-critical-months
001431                                    cp-term-or-ext-days
001432     move am-cal-table           to cp-class-CODE
001433
001434     move am-lf-deviation        to cp-deviation-code
001435
001436     move ws-rate-state          to cp-state
001437                                    cp-state-std-abbrv
001438     move 'L'                    to cp-benefit-type
001439     move ws-rate-lf-benefit-cd  to cp-benefit-cd
001440     move ws-rate-loan-amt       to cp-original-benefit
001441                                    cp-rating-benefit-amt
001442     move ws-rate-age            to cp-issue-age
001443     move ws-comp-id             to cp-company-id
001444     move ws-comp-cd             to cp-company-cd
001445
001446     move ws-rate-apr            to cp-loan-apr
001447
001448     if ws-rate-loan-term = zeros
001449        move ws-rate-lf-term     to ws-rate-loan-term
001450     end-if
001451     move ws-rate-lf-term        to cp-original-term
001452     move ws-rate-loan-term      to cp-loan-term
001453
001454     if (cp-original-term = zeros)
001455        and (cp-loan-term = zeros)
001456        move 20                  to ws-error-sub
001457        move '-Lf term'          to ws-error-sup
001458        perform 0180-error-handle
001459                                 thru 0180-exit
001460        go to 0300-return-cics
001461     end-if
001462     move 'L'                    to cp-life-override-code
001463     move 'A'                    to cp-ah-override-code
001464     move '3'                    to cp-process-type
001465     move ws-rate-earn-meth      to cp-earning-method
001466                                    cp-rating-method
001467     move 'R'                    to cp-rate-file
001468     move ws-bin-eff-dt          to cp-cert-eff-dt
001469     move ws-bin-1st-pmt-dt      to cp-first-pay-date
001470
001471     PERFORM 7000-GET-RATE       THRU 7000-EXIT
001472
001473     evaluate true
001474        when cp-return-code = '7'
001475           move 0                to cp-return-code
001476           move 14               to ws-error-sub
001477           move '-Lf Rates'      to ws-error-sup
001478           perform 0180-error-handle
001479                                 thru 0180-exit
001480           go to 0300-return-cics
001481        when cp-return-code = '6'
001482           move 0                to cp-return-code
001483           move 15               to ws-error-sub
001484           move '-Lf Rates'      to ws-error-sup
001485           perform 0180-error-handle
001486                                 thru 0180-exit
001487           go to 0300-return-cics
001488        when cp-return-code = 'D'
001489           move 0                to cp-return-code
001490           move 17               to ws-error-sub
001491           move ' '              to ws-error-sup
001492           perform 0180-error-handle
001493                                 thru 0180-exit
001494           go to 0300-return-cics
001495     end-evaluate
001496
001497     if cp-error-occured
001498        move zeros               to lf-rate
001499        move 17                  to ws-error-sub
001500        move '-Lf rate'          to ws-error-sup
001501        perform 0180-error-handle
001502                                 thru 0180-exit
001503        go to 0300-return-cics
001504     end-if
001505
001506     move 'S'                    to ws-rate-type
001507     move +0                     to ws-discount-rate
001508
001509     if (rt-discount-option <> ' ')
001510        and (rt-discount-ob-rate > zeros)
001511        move rt-discount-ob-rate to cp-premium-rate
001512        move rt-discount-rate    to ws-discount-rate
001513        if ws-discount-rate > 1
001514           if cp-state = 'PA'
001515              compute ws-discount-rate rounded =
001516                 ws-discount-rate / 100
001517           else
001518              compute ws-discount-rate rounded =
001519                 ws-discount-rate / 1200
001520           end-if
001521        end-if
001522        set mob-rate to true
001523     else
001524        move rt-l-rate (cp-original-term)
001525                                 to cp-premium-rate
001526     end-if
001527
001528     if cp-premium-rate not numeric
001529        move zeros to cp-premium-rate
001530     end-if
001531
001532**==============================================================**
001533**                                                              **
001534**      The below chunk of code is for those states that have   **
001535**   the SP equivalent of a discounted MOB rate stored in       **
001536**   Logic. All it does is calc a 12 month rate to be used      **
001537**   with the QC formulas because they expect that. I did this  **
001538**   so we wouldn't have a ton of hard coded formulas that      **
001539**   basically do the same thing.                               **
001540**                                                              **
001541**==============================================================**
001542
001543     if (ws-rate-in-lf-ben-code (1:1) = 'G')
001544        and (cp-state = 'IN' OR 'MT' OR 'ND' OR 'NJ' OR 'PA')
001545        display ' converting MOB to SP ' cp-premium-rate
001546        compute cp-premium-rate rounded =
001547           cp-premium-rate / cp-loan-term * 12
001548        display ' new rate FOR ' CP-STATE ' ' cp-premium-rate
001549     end-if
001550
001551     move cp-premium-rate        to lf-rate
001552                                    ws-rate-lf-rate
001553     move zeros                  to ws-max-lf-benefit
001554
001555     .
001556 0030-exit.
001557     exit.
001558
001559 0035-get-ah-rate.
001560
001561     move zeros to ws-rate-ah-rate
001562     if ws-rate-in-ah-ben-code (1:1) = ' '
001563        go to 0035-exit
001564     end-if
001565
001566*    move spaces                 to calculation-pass-area
001567     move zeros                  to cp-r-max-mon-ben
001568                                    cp-r-max-tot-ben
001569                                    cp-rate-dev-pct
001570                                    cp-original-premium
001571                                    cp-critical-months
001572                                    cp-term-or-ext-days
001573                                    cp-original-benefit
001574                                    cp-rating-benefit-amt
001575
001576     move am-cal-table           to cp-class-CODE
001577
001578     move am-ah-deviation        to cp-deviation-code
001579
001580     move ws-rate-state          to cp-state
001581                                    cp-state-std-abbrv
001582     move 'A'                    to cp-benefit-type
001583     move ws-rate-ah-benefit-cd  to cp-benefit-cd
001584
001585     if ws-rate-ah-term > zeros
001586        compute cp-original-benefit =
001587           ws-rate-loan-amt / ws-rate-ah-term
001588        move cp-original-benefit to cp-rating-benefit-amt
001589     end-if
001590*    move ws-rate-loan-amt       to cp-original-benefit
001591*                                   cp-rating-benefit-amt
001592     move ws-rate-age            to cp-issue-age
001593     move ws-comp-id             to cp-company-id
001594     move ws-comp-cd             to cp-company-cd
001595
001596     if ws-rate-loan-term = zeros
001597        move ws-rate-ah-term     to ws-rate-loan-term
001598     end-if
001599     move ws-rate-ah-term        to cp-original-term
001600     move ws-rate-loan-term      to cp-loan-term
001601
001602     if (cp-original-term = zeros)
001603        and (cp-loan-term = zeros)
001604        move 21                  to ws-error-sub
001605        move '-DI term'          to ws-error-sup
001606        perform 0180-error-handle
001607                                 thru 0180-exit
001608        go to 0300-return-cics
001609     end-if
001610
001611     move 'L'                    to cp-life-override-code
001612     move 'A'                    to cp-ah-override-code
001613     move '3'                    to cp-process-type
001614     move ws-rate-earn-meth      to cp-earning-method
001615                                    cp-rating-method
001616     move 'R'                    to cp-rate-file
001617     move ws-bin-eff-dt          to cp-cert-eff-dt
001618     move ws-bin-1st-pmt-dt      to cp-first-pay-date
001619
001620     PERFORM 7000-GET-RATE       THRU 7000-EXIT
001621
001622     evaluate true
001623        when cp-return-code = '7'
001624           move '0'              to cp-return-code
001625           move 14               to ws-error-sub
001626           move '-Ah Rates'      to ws-error-sup
001627           perform 0180-error-handle
001628                                 thru 0180-exit
001629           go to 0300-return-cics
001630        when cp-return-code = '6'
001631           move '0'              to cp-return-code
001632           move 16               to ws-error-sub
001633           move ' '              to ws-error-sup
001634           perform 0180-error-handle
001635                                 thru 0180-exit
001636           go to 0300-return-cics
001637        when cp-return-code = 'D'
001638           move '0'              to cp-return-code
001639           move 17               to ws-error-sub
001640           move ' '              to ws-error-sup
001641           perform 0180-error-handle
001642                                 thru 0180-exit
001643           go to 0300-return-cics
001644     end-evaluate
001645
001646     if cp-error-occured
001647        move zeros               to lf-rate
001648        move 17                  to ws-error-sub
001649        move '-DI rate'          to ws-error-sup
001650        perform 0180-error-handle
001651                                 thru 0180-exit
001652        go to 0300-return-cics
001653     end-if
001654
001655     move rt-ah-rate (cp-original-term)
001656                                 to cp-premium-rate
001657
001658    move cp-premium-rate         to ah-rate
001659                                    ws-rate-ah-rate
001660    move zeros                   to ws-max-ah-benefit
001661
001662     .
001663 0035-exit.
001664     exit.
001665
001666 0040-format-buffer.
001667
001668*    move spaces                 to ws-return-string
001669
001670     perform 0045-format-error   thru 0045-exit
001671
001672     move ';'                    to ws-sc1
001673                                    ws-sc2
001674                                    ws-sc3
001675                                    ws-sc4
001676                                    ws-sc5
001677                                    ws-sc6
001678                                    ws-sc7
001679                                    ws-sc8
001680                                    ws-sc9
001681                                    ws-sc10
001682                                    ws-sc11
001683                                    ws-sc12
001684                                    ws-sc13
001685                                    ws-sc14
001686                                    ws-sc15
001687                                    ws-sc16
001688                                    ws-sc17
001689                                    ws-sc18
001690                                    ws-sc19
001691                                    ws-sc20
001692                                    ws-sc21
001693                                    ws-sc22
001694                                    ws-sc23
001695                                    ws-sc24
001696
001697     if no-cp-error
001698        if ws-contract-suffix = spaces or low-values
001699           move ws-contract-no   to ws-return-contract-no (2:10)
001700        else
001701           move ws-contract-no   to ws-return-contract-no (1:10)
001702           move ws-contract-suffix
001703                                 to ws-return-contract-no (11:1)
001704        end-if
001705        move ws-rate-lf-prem     to ws-return-lf-prem
001706        move ws-rate-ah-prem     to ws-return-ah-prem
001707        move ws-rate-lf-rate     to ws-return-lf-rate
001708        move ws-rate-ah-rate     to ws-return-ah-rate
001709
001710        move zeros               to ws-return-principal
001711        move ws-rate-lf-term     to ws-return-lf-term
001712
001713        move ws-rate-ah-term     to ws-return-ah-term
001714        move ws-rate-in-lf-ben-code
001715                                 to ws-return-lf-bencd
001716        move ws-rate-crit-per    to ws-return-max-bens
001717
001718        move ws-max-ah-benefit   to ws-return-ah-max-amt
001719        move ws-max-lf-benefit   to ws-return-lf-max-amt
001720        move ws-lf-exp-date      to ws-return-lf-exp-dt
001721        move ws-ah-exp-date      to ws-return-ah-exp-dt
001722        move ws-loan-exp-date    to ws-return-loan-exp-dt
001723        move ws-rate-lf-benefit-cd
001724                                 to ws-return-lf-benefit-cd
001725        move ws-rate-ah-benefit-cd
001726                                 to ws-return-ah-benefit-cd
001727        move ws-rate-per-pmt     to ws-return-period-pmt
001728        move ws-rate-loan-pmt    to ws-return-loan-pmt
001729        move ws-rate-tot-financed to ws-return-tot-financed
001730        move ws-rate-tot-pmts    to ws-return-tot-pmts
001731        move ws-limit-name       to ws-return-limit-name
001732        move am-cal-table        to ws-return-rate-class
001733     else
001734        set error-in-one-coverage to true
001735        move spaces              to ws-contract-no
001736                                    ws-contract-suffix
001737     end-if
001738
001739     .
001740 0040-exit.
001741     exit.
001742
001743 0045-format-error.
001744
001745     evaluate true
001746        when cp-return-code = '0'
001747           move +1               to s1
001748        when cp-return-code = '1'
001749           move +2               to s1
001750        when cp-return-code = '2'
001751           move +3               to s1
001752        when cp-return-code = 'A'
001753           move +4               to s1
001754        when cp-return-code = 'B'
001755           move +5               to s1
001756        when cp-return-code = '4'
001757           move +6               to s1
001758        when cp-return-code = '9'
001759           move +7               to s1
001760        when cp-return-code = '8'
001761           move +8               to s1
001762*       when cp-return-code = 'H'
001763*          move +9               to s1
001764        when cp-return-code = 'C'
001765           move +10              to s1
001766        when cp-return-code = '7'
001767           move +11              to s1
001768        when cp-return-code = '6'
001769           move +12              to s1
001770        when cp-return-code = 'D'
001771           move +13              to s1
001772        when cp-return-code = 'X'
001773           move +17              to s1
001774        when cp-return-code = 'Z'
001775           move +14              to s1
001776           move cp-calc-premium  to ws-return-ah-prem
001777           move cp-premium-rate  to ws-return-ah-rate
001778           move ws-max-ah-benefit   to ws-return-ah-max-amt
001779           move ws-ah-exp-date      to ws-return-ah-exp-dt
001780           move ws-rate-ah-benefit-cd to ws-return-ah-benefit-cd
001781        when cp-return-code = 'Y'
001782           move +15              to s1
001783           move cp-calc-premium  to ws-return-ah-prem
001784           move cp-premium-rate  to ws-return-ah-rate
001785           move ws-max-ah-benefit   to ws-return-ah-max-amt
001786           move ws-ah-exp-date      to ws-return-ah-exp-dt
001787           move ws-rate-ah-benefit-cd to ws-return-ah-benefit-cd
001788     end-evaluate
001789
001790     move ws-table-error-no (s1) to ws-return-error-no
001791     move ws-table-error-mess (s1)
001792                                 to ws-return-error-mess
001793
001794     .
001795 0045-exit.
001796     exit.
001797
001798 0050-get-account.
001799
001800     move ws-comp-cd             to ws-am-company-cd
001801     move '9'                    to ws-am-carrier
001802     if ws-rate-state = 'KY'
001803        move '8'                 to ws-am-carrier
001804     end-if
001805     move '000000'               to ws-am-group
001806     move ws-rate-state          to ws-am-state
001807     move ws-rate-acct-no        to ws-am-account
001808     move ws-bin-eff-dt          to ws-am-exp-dt
001809
001810     
      * exec cics read
001811*       dataset      ('ERACCT')
001812*       ridfld       (ws-am-key)
001813*       into         (account-master)
001814*       GTEQ
001815*       resp         (ws-response)
001816*    end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV11
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005363' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303035333633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 account-master, 
                 DFHEIV11, 
                 ws-am-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001817
001818     if resp-normal
001819        and (ws-comp-cd = am-company-cd)
001820        and (ws-rate-state = am-state)
001821        and (ws-rate-acct-no = am-account)
001822        and (ws-bin-eff-dt >= am-effective-dt)
001823        and (ws-bin-eff-dt < am-expiration-dt)
001824        continue
001825     else
001826        move '0114;No account mstr found ' to ws-return-string
001827        go to 0300-RETURN-CICS
001828     end-if
001829
001830     move am-comment-line (1)    to ws-form-limit-name
001831     move spaces                 to ws-limit-name
001832     perform varying l1 from +50 by -1 until
001833        (l1 < +1)
001834        or (ws-form-limit-name (l1:1) <> ' ')
001835     end-perform
001836     if l1 > +3
001837        perform varying l1 from l1 by -1 until
001838           (l1 < +1)
001839           or (ws-form-limit-name (l1:1) = ' ')
001840        end-perform
001841        if l1 > +3
001842           subtract +1 from l1
001843           move ws-form-limit-name (1:l1)
001844                                 to ws-limit-name
001845        end-if
001846     end-if
001847
001848     .
001849 0050-exit.
001850     exit.
001851
001852 0060-check-for-dup.
001853
001854**==============================================================**
001855**                                                              **
001856**    All i'm going to do here is check for a dup using         **
001857**  the state, account, eff dt and last six of vin and a space  **
001858**  in the cert suffix. If I do find one then I will have to    **
001859**  find the last suffix and use the next available one in the  **
001860**  suffix table.                                               **
001861**                                                              **
001862**==============================================================**
001863
001864     move +1                     to x1
001865     move ' '                    to ws-browse-sw
001866
001867     move ws-comp-cd             to ws-cm-key
001868     move '9'                    to ws-cm-carrier
001869     if ws-rate-state = 'KY'
001870        move '8'                 to ws-cm-carrier
001871     end-if
001872     move '000000'               to ws-cm-group
001873     move ws-rate-state          to ws-cm-state
001874     move ws-rate-acct-no        to ws-cm-account
001875     move ws-bin-eff-dt          to ws-cm-eff-dt
001876     string
001877        '0000'
001878        ws-rate-vin (12:6)
001879        ' ' delimited by size into ws-cm-cert-no
001880     end-string
001881     move ws-cm-key              to ws-cm-compare-key
001882     move ws-cm-cert-ten         to ws-contract-no
001883     move low-values             to ws-last-suffix
001884
001885     
      * exec cics startbr
001886*       dataset      ('ELCERT')
001887*       ridfld       (ws-cm-key)
001888*       gteq
001889*       resp         (ws-response)
001890*    end-exec
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005438' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035343338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-cm-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001891
001892     evaluate true
001893        when resp-normal
001894           set browse-started to true
001895        when resp-notfnd or resp-endfile
001896           set i-say-stop to true
001897        when other
001898           move ws-response to ws-disp-resp
001899           string
001900              '9999' ';'
001901              'Bad elcert startbr ' ';'
001902              ws-disp-resp delimited by size
001903                                 into ws-return-string
001904           end-string
001905           display ' something went wrong with start br '
001906              ws-response
001907           go to 0300-RETURN-CICS
001908     end-evaluate
001909
001910     perform until i-say-stop
001911        
      * exec cics readnext
001912*          dataset      ('ELCERT')
001913*          ridfld       (ws-cm-key)
001914*          into         (certificate-master)
001915*          resp         (ws-response)
001916*       end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV12
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005464' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035343634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV12, 
                 ws-cm-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001917        if resp-normal
001918           if ws-cm-key (1:32) = ws-cm-compare-key (1:32)
001919              move ws-cm-cert-suffix
001920                                 to ws-last-suffix
001921           else
001922              set i-say-stop to true
001923           end-if
001924        else
001925           set i-say-stop to true
001926        end-if
001927     end-perform
001928
001929     if browse-started
001930        
      * exec cics endbr
001931*          dataset   ('ELCERT')
001932*       END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005483' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035343833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001933     end-if
001934
001935     if ws-last-suffix not = low-values
001936        perform varying x1 from +1 by +1 until
001937           (x1 > +26)
001938           or (ws-last-suffix = ws-suffix-value (x1))
001939        end-perform
001940        if x1 < +27
001941           move ws-suffix-value (x1 + 1)
001942                                 to ws-contract-suffix
001943        else
001944           display ' more than 26 suffix codes ' ws-last-suffix
001945           move 19               to ws-error-sub
001946           move spaces           to ws-error-sup
001947           perform 0180-error-handle
001948                                 thru 0180-exit
001949           go to 0300-return-cics
001950        end-if
001951     end-if
001952
001953     .
001954 0060-exit.
001955     exit.
001956
001957 0070-open-cursor.
001958
001959*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt
001960
001961***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
001962***                                                            ***
001963***  The dates on the sql table have values in the time        ***
001964***  so I convert it to a string and just use mm/dd/yyyy       ***
001965***  to perform the comparison.                                ***
001966***                                                            ***
001967***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
001968
001969     if not connected-to-db
001970        perform 6000-connect-to-db thru 6000-exit
001971     end-if
001972
001973     move ws-rate-state          to ws-dealer-state
001974     move ws-rate-acct-no        to ws-dealer-id
001975     move ws-rate-eff-date       to ws-contract-eff-dt
001976     move zeros                  to ws-ks-contract-no
001977     move ws-rate-vin (12:6)     to ws-ks-contract-no (5:6)
001978
001980     EXEC SQL
              DECLARE
001981           contracts cursor for
001982        SELECT
001983           DlrState,
001984           DlrId,
001985           EffDt,
001986           ContractNo,
001987           ContractSuffix
001988        FROM
001989           PendingContracts
001990        WHERE
001991           DlrState     = :ws-dealer-state
001992           and DlrId    = :ws-dealer-id
001993           and EffDt    = :ws-contract-eff-dt
001994           and ContractNo  = :ws-ks-contract-no
001995        ORDER BY
001996           dlrstate,
001997           dlrid,
001998           effdt,
001999           contractno,
002000           contractsuffix
002001     end-exec
002002
002003     if sqlcode not = 0
002004        display "Error: cannot declare cursor "
002005        display ' sql retrun code ' sqlcode
002006        display ' sql err mess    ' sqlerrmc
002007        go to 0070-exit
002008     end-if
002009
002011     EXEC SQL
              open contracts
002012     END-EXEC
002013
002014     if sqlcode not = 0
002015        display "Error: cannot open cursor "
002016        display ' sql retrun code ' sqlcode
002017        display ' sql err mess    ' sqlerrmc
002018        go to 0070-exit
002019     end-if
002020
002021     .
002022 0070-exit.
002023     exit.
002024 0080-process-input.
002025
002026     perform until sqlcode not = 0
002028        EXEC SQL
                 fetch contracts into
002029              :sql-dlr-state,
002030              :sql-dlr-id,
002031              :sql-eff-dt,
002032              :sql-contr-no,
002033              :sql-contr-suffix
002034        END-EXEC
002035
002036        if sqlcode = 0
002037           move sql-contr-suffix to ws-tbl-last-suffix
002038        else
002039           if sqlcode not = 0 and 100
002040              display "Error: cannot fetch row "
002041              display ' sql return code ' sqlcode
002042              display ' sql err mess    ' sqlerrmc
002043           end-if
002044        end-if
002045     end-perform
002046
002047     if ws-tbl-last-suffix not = low-values
002048        and (ws-tbl-last-suffix > ws-last-suffix)
002049        perform varying x1 from +1 by +1 until
002050           (x1 > +26)
002051           or (ws-tbl-last-suffix = ws-suffix-value (x1))
002052        end-perform
002053        if x1 < +27
002054           move ws-suffix-value (x1 + 1)
002055                                 to ws-contract-suffix
002056        else
002057           display ' more than 26 suffix codes ' ws-last-suffix
002058           move 19               to ws-error-sub
002059           move spaces           to ws-error-sup
002060           perform 0180-error-handle
002061                                 thru 0180-exit
002062           go to 0300-return-cics
002063        end-if
002064     end-if
002065
002067     EXEC SQL
               close contracts
002068     END-EXEC
002069
002070     if sqlcode not = 0
002071        display "Error: cannot close cursor "
002072        display ' sql retrun code ' sqlcode
002073        display ' sql err mess    ' sqlerrmc
002074     end-if
002075
002076     .
002077 0080-exit.
002078     exit.
002079
002080 0090-get-limits.
002081
002082     move zeros                  to ws-lf-limit-lo-age
002083                                    ws-lf-limit-hi-age
002084                                    ws-lf-limit-att-age
002085                                    ws-lf-limit-max-term
002086                                    ws-lf-limit-max-benefit
002087                                    ws-lf-limit-partial-cov
002088                                    ws-lf-limit-check-elig
002089                                    ws-lf-limit-elig-max-term
002090                                    ws-lf-limit-allow-truncated
002091                                    ws-di-limit-lo-age
002092                                    ws-di-limit-hi-age
002093                                    ws-di-limit-att-age
002094                                    ws-di-limit-max-term
002095                                    ws-di-limit-max-jnt-term
002096                                    ws-di-limit-max-mo-ben
002097                                    ws-di-limit-max-tot-ben
002098                                    ws-di-limit-partial-cov
002099                                    ws-di-limit-check-elig
002100                                    ws-di-limit-elig-max-term
002101                                    ws-di-limit-allow-truncated
002102
002103     if (ws-rate-lf-term = zeros)
002104        and (ws-rate-ah-term <> zeros)
002105        move 'N '                to ws-rate-in-lf-ben-code
002106     end-if
002107
002108     perform 0092-get-lf-limits  thru 0092-exit
002109     perform 0097-get-di-limits  thru 0097-exit
002110
002111     if recalc-lf
002112        perform 0020-calc-lf-exp
002113     end-if
002114     if recalc-di
002115        perform 0020-calc-ah-exp
002116        if ws-rate-ah-term < ws-rate-crit-per
002117           move zeros            to ws-rate-crit-per
002118     end-if
002119
002120     if recalc-lf or recalc-di
002121        perform 0020-calc-pri-att-age
002122        perform 0020-calc-cob-att-age
002123     end-if
002124
002125     .
002126 0090-exit.
002127     exit.
002128
002129 0092-get-lf-limits.
002130
002131     if not connected-to-db
002132        perform 6000-connect-to-db thru 6000-exit
002133     end-if
002134
002135     evaluate true
002136        when raw-lf-ben-code (1:1) = 'L'
002137           move 'LL'             to ws-limit-cov-type
002138        when raw-ah-ben-code (1:1) = ' '
002139           move 'LO'             to ws-limit-cov-type
002140        when raw-lf-ben-code (1:1) = 'N' or 'G' or 'T'
002141           move 'RL'             to ws-limit-cov-type
002142        when other   *> Assuming no life coverage
002143           go to 0092-exit
002144     end-evaluate
002145
002146     move zeros                  to ws-limit-lo-age
002147                                    ws-limit-hi-age
002148                                    ws-limit-att-age
002149                                    ws-limit-max-term
002150                                    ws-limit-max-jnt-term
002151                                    ws-limit-max-mo-ben
002152                                    ws-limit-max-tot-ben
002153                                    ws-limit-partial-cov
002154                                    ws-limit-check-elig
002155                                    ws-limit-elig-max-term
002156                                    ws-limit-allow-truncated
002157
002158     move ws-rate-age            to ws-limit-issue-age
002159
002160     .
002161 0092-get-life.
002162
002163     move 30                     to ws-limit-issue-age
002164
002166     EXEC SQL
              SELECT
002167           AttainedAge
002168        INTO
002169           :ws-limit-att-age
002170        FROM
002171           FormLimits
002172        WHERE
002173           Limit           = :ws-limit-name
002174           and CovType     = :ws-limit-cov-type
002175           and LoIssueAge <= :ws-limit-issue-age
002176           and HiIssueAge >= :ws-limit-issue-age
002177     end-exec
002178
002179     if sqlcode not = 0
002180        display "Error: cannot find limitsa " ws-limit-name
002181           ' ' ws-limit-cov-type ' ' ws-limit-issue-age
002182        display ' sql retrun code ' sqlcode
002183        display ' sql err mess    ' sqlerrmc
002184        if (ws-limit-cov-type = 'LO')
002185           and (raw-ah-ben-code (1:1) = ' ')
002186           move 'RL'             to ws-limit-cov-type
002187           go to 0092-get-life
002188        else
002189           display "Error: cannot find limits " ws-limit-name
002190              ' ' ws-limit-cov-type ' ' ws-limit-issue-age
002191           display ' sql retrun code ' sqlcode
002192           display ' sql err mess    ' sqlerrmc
002193           move 22                  to ws-error-sub
002194           move ws-limit-name       to ws-error-sup
002195           perform 0180-error-handle
002196                                    thru 0180-exit
002197           go to 0300-return-cics
002198        end-if
002199     end-if
002200
002201     move ws-rate-age            to ws-limit-issue-age
002202
002204     EXEC SQL
              SELECT
002205           AttainedAge,
002206           MaxTerm,
002207           MaxTotBen,
002208           PartialCoverage,
002209           CheckEligibility,
002210           EligibilityMaxTerm,
002211           AllowTruncated
002212        INTO
002213           :ws-limit-att-age,
002214           :ws-limit-max-term,
002215           :ws-limit-max-tot-ben,
002216           :ws-limit-partial-cov  :nu-partial-cov,
002217           :ws-limit-check-elig   :nu-check-elig,
002218           :ws-limit-elig-max-term,
002219           :ws-limit-allow-truncated  :nu-allow-trunc
002220        FROM
002221           FormLimits
002222        WHERE
002223           Limit           = :ws-limit-name
002224           and CovType     = :ws-limit-cov-type
002225           and LoIssueAge <= :ws-limit-issue-age
002226           and HiIssueAge >= :ws-limit-issue-age
002227     end-exec
002228
002229     if sqlcode not = 0
002230        display "Error: cannot find limits for age "
002231           ws-limit-name ' ' ws-limit-cov-type ' '
002232           ws-limit-issue-age
002233        display ' sql retrun code ' sqlcode
002234        display ' sql err mess    ' sqlerrmc
002235        move 4                   to ws-error-sub
002236       move ws-limit-name        to ws-error-sup
002237       perform 0180-error-handle thru 0180-exit
002238       go to 0300-return-cics
002239     end-if
002240
002241*    display ' good get on limit *'  ws-limit-name '*'
002242*       ws-limit-att-age '*' ws-limit-cov-type '*'
002243*         ws-limit-issue-age '*' ws-limit-att-age '*'
002244*         ws-limit-partial-cov '*' ws-limit-check-elig '*'
002245*         ws-limit-elig-max-term '*'
002246*         ws-limit-allow-truncated '*'
002247
002248     move ws-limit-lo-age        to ws-lf-limit-lo-age
002249     move ws-limit-hi-age        to ws-lf-limit-hi-age
002250     move ws-limit-att-age       to ws-lf-limit-att-age
002251     move ws-limit-max-term      to ws-lf-limit-max-term
002252     move ws-limit-max-tot-ben   to ws-lf-limit-max-benefit
002253     move ws-limit-partial-cov   to ws-lf-limit-partial-cov
002254     move ws-limit-check-elig    to ws-lf-limit-check-elig
002255     move ws-limit-elig-max-term to ws-lf-limit-elig-max-term
002256     move ws-limit-allow-truncated
002257                                 to ws-lf-limit-allow-truncated
002258
002259     if ws-lf-limit-check-elig = 0
002260        go to 0092-continue
002261     end-if
002262
002263     if ws-rate-loan-term <= ws-lf-limit-elig-max-term
002264        continue
002265     else
002266        move 31                  to ws-error-sub
002267        move ' '                 to ws-error-sup
002268        perform 0180-error-handle
002269                                 thru 0180-exit
002270        go to 0300-return-cics
002271     end-if
002272
002273     if ws-rate-loan-amt <= ws-lf-limit-max-benefit
002274        continue
002275     else
002276        move 32                  to ws-error-sub
002277        move '- Lf Benefit '     to ws-error-sup
002278        perform 0180-error-handle
002279                                 thru 0180-exit
002280        go to 0300-return-cics
002281     end-if
002282
002283     .
002284 0092-continue.
002285
002286     move ' '                    to ws-recalc-lf
002287     if ws-lf-limit-allow-truncated = 0
002288        go to 0092-exit
002289     end-if
002290
002291     if ws-att-age <= ws-lf-limit-att-age
002292        go to 0092-exit
002293     end-if
002294
002295*****  Calculate the date in which they will turn the
002296*****  limit attained age.
002297
002298     move ws-bin-pri-birth-dt to dc-bin-date-1
002299     if (ws-bin-cob-birth-dt < dc-bin-date-1)
002300        and (ws-bin-cob-birth-dt <> low-values)
002301        move ws-bin-cob-birth-dt to dc-bin-date-1
002302     end-if
002303     move dc-bin-date-1          to ws-test-bin-dt
002304     move ws-comp-bin-dt         to ws-disp-bin-dt
002305
002306     compute dc-elapsed-months = (ws-lf-limit-att-age * 12)
002307     move '6'                    to dc-option-code
002308     move zeros                  to dc-elapsed-days
002309     PERFORM 9700-date-link      thru 9700-exit
002310     if no-conversion-error
002311        move dc-bin-date-2       to ws-limit-att-age-dt
002312     else
002313        move low-values          to ws-limit-att-age-dt
002314     end-if
002315
002316     move dc-bin-date-2          to ws-test-bin-dt
002317     move ws-comp-bin-dt         to ws-disp-bin-dt
002318
002319     set recalc-lf to true
002320
002321     compute ws-temp-lf-term = ws-rate-lf-term -
002322        ((ws-att-age - ws-lf-limit-att-age) * 12)
002323
002324     compute ws-rate-lf-term = ws-rate-lf-term -
002325        ((ws-att-age - ws-lf-limit-att-age) * 12)
002326     move 'T '                   to ws-rate-in-lf-ben-code
002327
002328*****  Calculate the new expiration date.
002329
002330     perform 0020-calc-lf-exp
002331     if ws-bin-lf-exp-dt > ws-limit-att-age-dt
002332        subtract 1 from ws-rate-lf-term
002333     end-if
002334
002335*    if ws-rate-loan-amt <= ws-limit-max-tot-ben
002336*       continue
002337*    else
002338*       move 8                   to ws-error-sub
002339*       move '- Lf Benefit '     to ws-error-sup
002340*       perform 0180-error-handle
002341*                                thru 0180-exit
002342*       go to 0300-return-cics
002343*    end-if
002344*
002345*    if ws-rate-lf-term <= ws-limit-max-term
002346*       continue
002347*    else
002348*       move 7                   to ws-error-sub
002349*       move '- Lf Term '        to ws-error-sup
002350*       perform 0180-error-handle
002351*                                thru 0180-exit
002352*       go to 0300-return-cics
002353*    end-if
002354*    if ws-att-age <= ws-limit-att-age
002355*       continue
002356*    else
002357*       move 5                   to ws-error-sub
002358*       move ws-att-age          to ws-error-sup
002359*       perform 0180-error-handle
002360*                                thru 0180-exit
002361*       go to 0300-return-cics
002362*    end-if
002363
002364     .
002365 0092-exit.
002366     exit.
002367
002368 0097-get-di-limits.
002369
002370     if raw-ah-ben-code (1:1) = 'A'
002371        move 'DI'                to ws-limit-cov-type
002372     else   *>  Assuming no AH coverage
002373        go to 0097-exit
002374     end-if
002375
002376     if not connected-to-db
002377        perform 6000-connect-to-db thru 6000-exit
002378     end-if
002379
002380***  True = -1
002381
002382     move 30                     to ws-limit-issue-age
002383
002385     EXEC SQL
              SELECT distinct top 1
002386           AttainedAge
002387        INTO
002388           :ws-limit-att-age
002389        FROM
002390           FormLimits
002391        WHERE
002392           Limit           = :ws-limit-name
002393           and CovType     = :ws-limit-cov-type
002394           and LoIssueAge <= :ws-limit-issue-age
002395           and HiIssueAge >= :ws-limit-issue-age
002396     end-exec
002397
002398     if sqlcode not = 0
002399        display "Error: cannot find limits " ws-limit-name
002400        display ' sql retrun code ' sqlcode
002401        display ' sql err mess    ' sqlerrmc
002402        move 22                  to ws-error-sub
002403        move ws-limit-name       to ws-error-sup
002404        perform 0180-error-handle
002405                                 thru 0180-exit
002406        go to 0300-return-cics
002407     end-if
002408
002409     move ws-rate-age            to ws-limit-issue-age
002410
002412     EXEC SQL
              SELECT distinct top 1
002413           AttainedAge,
002414           MaxTerm,
002415           MaxJntTerm,
002416           MaxMoBen,
002417           MaxTotBen,
002418           PartialCoverage,
002419           CheckEligibility,
002420           EligibilityMaxTerm,
002421           AllowTruncated
002422        INTO
002423           :ws-limit-att-age,
002424           :ws-limit-max-term,
002425           :ws-limit-max-jnt-term,
002426           :ws-limit-max-mo-ben,
002427           :ws-limit-max-tot-ben,
002428           :ws-limit-partial-cov   :nu-partial-cov,
002429           :ws-limit-check-elig    :nu-check-elig,
002430           :ws-limit-elig-max-term,
002431           :ws-limit-allow-truncated  :nu-allow-trunc
002432        FROM
002433           FormLimits
002434        WHERE
002435           Limit           = :ws-limit-name
002436           and CovType     = :ws-limit-cov-type
002437           and LoIssueAge <= :ws-limit-issue-age
002438           and HiIssueAge >= :ws-limit-issue-age
002439     end-exec
002440
002441     if sqlcode not = 0
002442        display "Error: cannot find limits " ws-limit-name
002443        display ' sql retrun code ' sqlcode
002444        display ' sql err mess    ' sqlerrmc
002445        move 4                   to ws-error-sub
002446        move ws-limit-name       to ws-error-sup
002447        perform 0180-error-handle
002448                                 thru 0180-exit
002449        go to 0300-return-cics
002450     end-if
002451
002452     move ws-limit-lo-age        to ws-di-limit-lo-age
002453     move ws-limit-hi-age        to ws-di-limit-hi-age
002454     move ws-limit-att-age       to ws-di-limit-att-age
002455     move ws-limit-max-term      to ws-di-limit-max-term
002456     move ws-limit-max-jnt-term  to ws-di-limit-max-jnt-term
002457     move ws-limit-max-mo-ben    to ws-di-limit-max-mo-ben
002458     move ws-limit-max-tot-ben   to ws-di-limit-max-tot-ben
002459     move ws-limit-partial-cov   to ws-di-limit-partial-cov
002460     move ws-limit-check-elig    to ws-di-limit-check-elig
002461     move ws-limit-elig-max-term to ws-di-limit-elig-max-term
002462     move ws-limit-allow-truncated
002463                                 to ws-di-limit-allow-truncated
002464
002465     if ws-di-limit-check-elig = 0
002466        go to 0097-continue
002467     end-if
002468
002469     if ws-rate-loan-term <= ws-di-limit-elig-max-term
002470        continue
002471     else
002472        move 31                  to ws-error-sub
002473        move ' '                 to ws-error-sup
002474        perform 0180-error-handle
002475                                 thru 0180-exit
002476        go to 0300-return-cics
002477     end-if
002478
002479*    if (ws-rate-ah-term * ws-rate-per-pmt) <=
002480*                  ws-di-limit-max-tot-ben
002481*       continue
002482*    else
002483*       move 32                  to ws-error-sub
002484*       move '- Di ToT Ben '     to ws-error-sup
002485*       perform 0180-error-handle
002486*                                thru 0180-exit
002487*       go to 0300-return-cics
002488*    end-if
002489
002490     .
002491 0097-continue.
002492
002493     move ' '                    to ws-recalc-di
002494     if ws-di-limit-allow-truncated = 0
002495        go to 0097-exit
002496     end-if
002497
002498     if ws-att-age <= ws-di-limit-att-age
002499        go to 0097-exit
002500     end-if
002501
002502     compute ws-rate-ah-term = ws-rate-ah-term -
002503        ((ws-att-age - ws-di-limit-att-age) * 12)
002504
002505     set recalc-di to true
002506
002507     if (ws-rate-lf-term > zeros)
002508        and (ws-rate-ah-term <> ws-rate-lf-term)
002509        move ws-rate-lf-term     to ws-rate-ah-term
002510     end-if
002511
002512*    if ws-rate-per-pmt <= ws-limit-max-mo-ben
002513*       continue
002514*    else
002515*       move 8                   to ws-error-sub
002516*       move '- Ah Benefit '     to ws-error-sup
002517*       perform 0180-error-handle
002518*                                thru 0180-exit
002519*       go to 0300-return-cics
002520*    end-if
002521*
002522*    if ws-rate-ah-term <= ws-limit-max-term
002523*       continue
002524*    else
002525*       move 7                   to ws-error-sub
002526*       move '- Ah Term '        to ws-error-sup
002527*       perform 0180-error-handle
002528*                                thru 0180-exit
002529*       go to 0300-return-cics
002530*    end-if
002531*
002532*    if ws-att-age <= ws-limit-att-age
002533*       continue
002534*    else
002535*       move 5                   to ws-error-sub
002536*       move ws-att-age          to ws-error-sup
002537*       perform 0180-error-handle
002538*                                thru 0180-exit
002539*       go to 0300-return-cics
002540*    end-if
002541
002542     .
002543 0097-exit.
002544     exit.
002545
002546 0110-unstring.
002547
002548***____________________________________________________________***
002549**|                                                            |**
002550**|    Unstring the raw data into data elements                |**
002551**|                                                            |**
002552***____________________________________________________________***
002553
002554        unstring dfhcommarea
002555           delimited by '|' into
002556              raw-message-num
002557              raw-state
002558              raw-acct-no
002559              raw-vin
002560              raw-lf-ben-code
002561              raw-ah-ben-code
002562              raw-earn-meth
002563              raw-pri-birth-date
002564              raw-cob-birth-date
002565              raw-loan-amt
002566              raw-eff-date
002567              raw-1st-pmt-dt
002568              raw-pmts-per-year
002569              raw-loan-term
002570              raw-lf-term
002571              raw-ah-term
002572              raw-apr
002573              raw-lf-sin-jnt-ind
002574              raw-ah-sin-jnt-ind
002575              raw-dismemberment
002576              raw-retro-elim
002577              raw-waiting-days
002578              raw-crit-per
002579        end-unstring
002580
002581     .
002582 0110-exit.
002583     exit.
002584
002585 0120-format-message.
002586
002587***____________________________________________________________***
002588**|                                                            |**
002589**|    Format each data element to be consistant with          |**
002590**|  COBOL rate copybook model                                 |**
002591**|                                                            |**
002592***____________________________________________________________***
002593
002594     inspect
002595        raw-acct-no replacing leading spaces by zeros
002596     inspect
002597        raw-pmts-per-year replacing leading spaces by zeros
002598     inspect
002599        raw-loan-term replacing leading spaces by zeros
002600     inspect
002601        raw-lf-term replacing leading spaces by zeros
002602     inspect
002603        raw-ah-term replacing leading spaces by zeros
002604     inspect
002605        raw-waiting-days replacing leading spaces by zeros
002606     inspect
002607        raw-crit-per replacing leading spaces by zeros
002608
002609     inspect
002610        raw-loan-amt replacing all spaces by zeros
002611     inspect
002612        raw-apr replacing all spaces by zeros
002613
002614     move raw-loan-amt           to ws-work-in
002615     perform 0130-format-amt     thru 0130-exit
002616     move ws-work-out-v2         to ws-rate-loan-amt
002617*    display ' ben  in *' ws-work-in '*'
002618*    display ' ben  out *' ws-work-out '*'
002619
002620     move raw-loan-term          to ws-work-in
002621     perform 0140-format-term    thru 0140-exit
002622     move ws-work-out-v0         to ws-rate-loan-term
002623*    display ' ln term in *' ws-work-in '*'
002624*    display ' ln term out *' ws-work-out '*'
002625
002626     move raw-lf-term            to ws-work-in
002627     perform 0140-format-term    thru 0140-exit
002628     move ws-work-out-v0         to ws-rate-lf-term
002629*    display ' lf  term in *' ws-work-in '*'
002630*    display ' lf  term out *' ws-work-out '*'
002631
002632     move raw-ah-term            to ws-work-in
002633     perform 0140-format-term    thru 0140-exit
002634     move ws-work-out-v0         to ws-rate-ah-term
002635*    display ' ah  term in *' ws-work-in '*'
002636*    display ' ah  term out *' ws-work-out '*'
002637
002638     move raw-waiting-days       to ws-work-in
002639     perform 0140-format-term    thru 0140-exit
002640     move ws-work-out-v0         to ws-rate-waiting-days
002641*    display ' wait day in *' ws-work-in '*'
002642*    display ' wait day out *' ws-work-out '*'
002643
002644     move raw-crit-per           to ws-work-in
002645     perform 0140-format-term    thru 0140-exit
002646     move ws-work-out-v0         to ws-rate-crit-per
002647*    display ' crit per in *' ws-work-in '*'
002648*    display ' crit per out *' ws-work-out '*'
002649
002650     move raw-pmts-per-year      to ws-work-in
002651     perform 0140-format-term    thru 0140-exit
002652     move ws-work-out-v0         to ws-rate-pmts-per-year
002653*    display ' pmts per yr *' ws-work-in '*'
002654*    display ' pmts per yr  *' ws-work-out '*'
002655
002656     move raw-apr                to ws-work-in
002657     perform 0150-format-apr     thru 0150-exit
002658     move ws-work-out-v5         to ws-rate-apr
002659*    display ' ins apr  in *' ws-work-in '*'
002660*    display ' ins apr  out *' ws-work-out '*'
002661
002662     move raw-state              to ws-rate-state
002663     move raw-acct-no            to ws-rate-acct-no
002664     move raw-vin                to ws-rate-vin
002665     move raw-lf-ben-code        to ws-rate-in-lf-ben-code
002666     move raw-ah-ben-code        to ws-rate-in-ah-ben-code
002667     move raw-earn-meth          to ws-rate-earn-meth
002668     move raw-pri-birth-date     to ws-rate-pri-birth-date
002669     move raw-cob-birth-date     to ws-rate-cob-birth-date
002670     move raw-eff-date           to ws-rate-eff-date
002671     move raw-1st-pmt-dt         to ws-rate-1st-pmt-dt
002672     move raw-lf-sin-jnt-ind     to ws-rate-sin-jnt-lf
002673     move raw-ah-sin-jnt-ind     to ws-rate-sin-jnt-ah
002674     move raw-dismemberment      to ws-rate-dismemberment
002675     move raw-retro-elim         to ws-rate-retro-elim
002676     if ws-rate-crit-per = zeros
002677        move 'Y'                 to ws-rate-fullterm
002678     else
002679        move 'N'                 to ws-rate-fullterm
002680     end-if
002681
002682     .
002683 0120-exit.
002684     exit.
002685
002686 0130-format-amt.
002687
002688     move zeros                  to ws-work-out
002689     perform varying s2 from +10 by -1 until
002690        (s2 < +1)
002691        or (ws-work-in (s2:1) = '.')
002692     end-perform
002693     if s2 < +9
002694        move ws-work-in (s2 + 1:2)
002695                              to ws-work-out (9:2)
002696        move +8               to s3
002697        subtract +1 from s2
002698        perform varying s2 from s2 by -1 until
002699           (s2 < +1)
002700           or (s3 = 0)
002701           move ws-work-in (s2:1) to ws-work-out (s3:1)
002702           subtract +1 from s3
002703        end-perform
002704     end-if
002705
002706     .
002707 0130-exit.
002708     exit.
002709
002710 0140-format-term.
002711
002712     move zeros                  to ws-work-out
002713     perform varying s2 from +10 by -1 until
002714        (s2 < +1)
002715        or (ws-work-in (s2:1) numeric)
002716     end-perform
002717     if s2 > +0
002718        move ws-work-in (s2:1)
002719                              to ws-work-out (10:1)
002720        move +9               to s3
002721        subtract +1 from s2
002722        perform varying s2 from s2 by -1 until
002723           (s2 < +1)
002724           or (s3 = 7)
002725           move ws-work-in (s2:1) to ws-work-out (s3:1)
002726           subtract +1 from s3
002727        end-perform
002728     end-if
002729
002730     .
002731 0140-exit.
002732     exit.
002733
002734 0150-format-apr.
002735
002736     move zeros                  to ws-work-out
002737     perform varying s2 from +10 by -1 until
002738        (s2 < +1)
002739        or (ws-work-in (s2:1) = '.')
002740     end-perform
002741     if s2 < +5
002742        move ws-work-in (s2 + 1:5)
002743                              to ws-work-out (6:5)
002744        move +5               to s3
002745        subtract +1 from s2
002746        perform varying s2 from s2 by -1 until
002747           (s2 < +1)
002748           or (s3 = 0)
002749           move ws-work-in (s2:1) to ws-work-out (s3:1)
002750           subtract +1 from s3
002751        end-perform
002752     end-if
002753
002754     .
002755 0150-exit.
002756     exit.
002757
002758 0180-error-handle.
002759
002760     move ws-table-error-no (ws-error-sub)
002761                                 to ws-return-error-no
002762     move spaces                 to ws-return-error-mess
002763     string
002764        ws-table-error-mess (ws-error-sub)
002765        ws-error-sup
002766        delimited by '  ' into ws-return-error-mess
002767     end-string
002768
002769     .
002770 0180-exit.
002771     exit.
002772
002773 0190-final-limit-check.
002774
002775     if ws-rate-in-lf-ben-code (1:1) = ' '
002776        go to 0190-final-ah
002777     end-if
002778
002779*    display ' rate loan amt  ' ws-rate-loan-amt
002780*    display ' max lf benefit ' ws-max-lf-benefit
002781*    display ' lf prem        ' ws-rate-lf-prem
002782*    display ' ah prem        ' ws-rate-ah-prem
002783*    display ' lf limit max   ' ws-lf-limit-max-benefit
002784*    display ' att age        ' ws-att-age
002785*    display ' limit att age  ' ws-lf-limit-att-age
002786*    display ' rate lf term   ' ws-rate-lf-term
002787*    display ' limit max trm  ' ws-lf-limit-max-term
002788     if ws-lf-limit-check-elig <> 0
002789        if (ws-rate-loan-amt +
002790            ws-rate-lf-prem +
002791            ws-rate-ah-prem)  <= ws-lf-limit-max-benefit
002792           continue
002793        else
002794           move 32               to ws-error-sub
002795           move '- Lf Benefit '  to ws-error-sup
002796           perform 0180-error-handle
002797                                 thru 0180-exit
002798           go to 0300-return-cics
002799        end-if
002800     end-if
002801
002802     if (ws-max-lf-benefit <= ws-lf-limit-max-benefit)
002803        or (ws-lf-limit-max-benefit = zeros)
002804        continue
002805     else
002806        move 10                  to ws-error-sub
002807        move '- Lf Benefit '     to ws-error-sup
002808        perform 0180-error-handle
002809                                 thru 0180-exit
002810        go to 0300-return-cics
002811     end-if
002812
002813     if ws-rate-lf-term <= ws-lf-limit-max-term
002814        continue
002815     else
002816        move 9                   to ws-error-sub
002817        move '- Lf Term '        to ws-error-sup
002818        perform 0180-error-handle
002819                                 thru 0180-exit
002820        go to 0300-return-cics
002821     end-if
002822
002823     if (ws-att-age <= ws-lf-limit-att-age)
002824        or (ws-lf-limit-att-age = zeros)
002825        continue
002826     else
002827        move 5                   to ws-error-sub
002828*       move ws-att-age          to ws-error-sup
002829        move ws-att-age-x        to ws-error-sup
002830        perform 0180-error-handle
002831                                 thru 0180-exit
002832        go to 0300-return-cics
002833     end-if
002834
002835     .
002836 0190-final-ah.
002837
002838     if ws-rate-in-ah-ben-code (1:1) = ' '
002839        go to 0190-exit
002840     end-if
002841
002842*    display ' made it to 0190-final-ah ' ws-di-limit-check-elig
002843*    display ' rate loan amt      ' ws-rate-loan-amt
002844*    display ' rate ah prem       ' ws-rate-ah-prem
002845*    display ' di limit max totben' ws-di-limit-max-tot-ben
002846*    display ' rate ah term       ' ws-rate-ah-term
002847*    display ' rate per pmt       ' ws-rate-per-pmt
002848*    display ' rate loan term     ' ws-rate-loan-term
002849
002850     if ws-di-limit-check-elig <> 0
002851        if (ws-rate-loan-amt +
002852            ws-rate-ah-prem)  <= ws-di-limit-max-tot-ben
002853           continue
002854        else
002855           move 32               to ws-error-sub
002856           move '- Di Benefit '  to ws-error-sup
002857           perform 0180-error-handle
002858                                 thru 0180-exit
002859           go to 0300-return-cics
002860        end-if
002861        if ws-rate-state = 'FL'
002862           if ((ws-rate-loan-term * ws-rate-per-pmt) <=
002863                      ws-di-limit-max-tot-ben)
002864              continue
002865           else
002866              move 32            to ws-error-sub
002867              move '- Di ToT Ben ' to ws-error-sup
002868              perform 0180-error-handle
002869                                 thru 0180-exit
002870              go to 0300-return-cics
002871           end-if
002872        end-if
002873     end-if
002874
002875     if ws-max-ah-benefit <= ws-di-limit-max-mo-ben
002876        continue
002877     else
002878        move 11                  to ws-error-sub
002879        move '- Ah Benefit '     to ws-error-sup
002880        perform 0180-error-handle
002881                                 thru 0180-exit
002882        go to 0300-return-cics
002883     end-if
002884
002885     if ws-rate-sin-jnt-ah = 'J'
002886        if ws-rate-ah-term <= ws-di-limit-max-jnt-term
002887           continue
002888        else
002889           move 8                   to ws-error-sub
002890           move '- Ah TermJNT'      to ws-error-sup
002891           perform 0180-error-handle
002892                                    thru 0180-exit
002893           go to 0300-return-cics
002894        end-if
002895     else
002896        if ws-rate-ah-term <= ws-di-limit-max-term
002897           continue
002898        else
002899           move 9                   to ws-error-sub
002900           move '- Ah Term '        to ws-error-sup
002901           perform 0180-error-handle
002902                                    thru 0180-exit
002903           go to 0300-return-cics
002904        end-if
002905     end-if
002906
002907     if (ws-rate-ah-term * ws-max-ah-benefit) <=
002908                   ws-di-limit-max-tot-ben
002909        continue
002910     else
002911        move 13                  to ws-error-sub
002912        move '- Ah Tot Ben '     to ws-error-sup
002913        perform 0180-error-handle
002914                                 thru 0180-exit
002915        go to 0300-return-cics
002916     end-if
002917
002918     if ws-att-age <= ws-di-limit-att-age
002919        continue
002920     else
002921        move 5                   to ws-error-sub
002922*       move ws-att-age          to ws-error-sup
002923        move ws-att-age-x        to ws-error-sup
002924        perform 0180-error-handle
002925                                 thru 0180-exit
002926        go to 0300-return-cics
002927     end-if
002928
002929     .
002930 0190-exit.
002931     exit.
002932
002933 0200-calc-NP-payment.
002934
002935     move ws-rate-loan-amt       to l
002936     move ws-rate-loan-term      to n
002937     move ws-rate-lf-term        to m
002938     if m = zeros
002939        move ws-rate-ah-term to m
002940     end-if
002941     move ws-rate-pmts-per-year  to ppy
002942     move 30                     to dpp
002943     compute i = ws-rate-apr / (ppy *100)
002944     if i = zeros
002945        move .0000001            to i
002946     end-if
002947
002948     COMPUTE A-ANGLE-N ROUNDED =
002949         (1 - ((1 / (1 + I)) ** N)) / I
002950
002951     COMPUTE A-ANGLE-N-M ROUNDED =
002952         (1 - ((1 / (1 + I)) ** (N - M))) / I
002953
002954     COMPUTE GAMMA ROUNDED =
002955         (1 + ((D * I) / DPP)) / (1 + I)
002956
002957     COMPUTE A-PRM-ANGLE-N ROUNDED = A-ANGLE-N / GAMMA
002958     compute ws-rate-loan-pmt rounded =
002959        l / a-prm-angle-n
002960
002961***____________________________________________________________***
002962**(-                                                          -)**
002963**(-   This section is for TX Net Pay + 1                     -)**
002964**(-   This formula is designed to use the actual MOB rate    -)**
002965**(- and then applies the discount using x.                   -)**
002966**(-   This formula works for net and net truncated.          -)**
002967**(-                                                          -)**
002968***____________________________________________________________***
002969
002970     if ws-rate-state <> 'TX'
002971        go to 0200-try-oh
002972     end-if
002973
002974     COMPUTE X ROUNDED = 1 / (1 + TX-DISCOUNT-RATE)
002975
002976     COMPUTE V-TO-N ROUNDED = (1 / (1 + I)) ** N
002977     COMPUTE X-TO-M ROUNDED = X ** M
002978     COMPUTE V-TO-N-M ROUNDED = (1 / (1 + I)) ** (N - M)
002979
002980     COMPUTE TEMP-VALUE-1 ROUNDED =
002981         (1 + I) / I
002982
002983     COMPUTE TEMP-VALUE-2 ROUNDED =
002984         (X-TO-M - 1) / (X - 1)
002985
002986     COMPUTE TEMP-VALUE-3 ROUNDED =
002987         ((X-TO-M * V-TO-N-M) - V-TO-N) /
002988         ((X * (1 + I)) - 1)
002989
002990     COMPUTE TEMP-VALUE-4 ROUNDED =
002991         M * (AH-RATE / 100)
002992
002993     COMPUTE TEMP-VALUE-5 ROUNDED =
002994         A-PRM-ANGLE-N * (1 + I)
002995
002996     COMPUTE LIFE-FACTOR ROUNDED =
002997         TEMP-VALUE-1 * (TEMP-VALUE-2 - TEMP-VALUE-3) *
002998         (lf-rate / 1000)
002999
003000     COMPUTE PMT ROUNDED =
003001         L / (A-PRM-ANGLE-N - LIFE-FACTOR - TEMP-VALUE-4)
003002
003003     COMPUTE SINGLE-FACTOR ROUNDED =
003004         (LIFE-FACTOR * 100) / TEMP-VALUE-5
003005
003006     .
003007 0200-check-tx-partial.
003008
003009     if ws-lf-limit-partial-cov = 0
003010        display ' no partial coverage '
003011        move pmt                 to dispmt
003012                                    lifepmt1
003013                                    lifepmt2
003014                                    newpmt
003015        go to 0200-carry-on-tx
003016     end-if
003017
003018     if pmt < ws-di-limit-max-mo-ben
003019        move pmt                 to dispmt
003020     else
003021        move ws-di-limit-max-mo-ben
003022                                 to dispmt
003023     end-if
003024
003025     if (ws-di-limit-max-tot-ben / n) < dispmt
003026        compute dispmt rounded =
003027           (ws-di-limit-max-tot-ben / n) - .005
003028     end-if
003029
003030     compute lifepmt1 rounded =
003031        (l + (single-factor * (pmt * a-prm-angle-n * (1 + 1 * i))
003032        / 100) + dispmt * m * ah-rate / 100) / a-prm-angle-n
003033
003034     if (ws-lf-limit-max-benefit / a-prm-angle-n) < lifepmt1
003035        compute lifepmt1 rounded =
003036           ws-lf-limit-max-benefit / a-prm-angle-n
003037        move ws-lf-limit-max-benefit
003038                                 to ws-max-lf-benefit
003039     end-if
003040
003041     compute newpmt rounded =
003042        (l + (single-factor * (lifepmt1 * a-prm-angle-n *
003043        (1 + 1 * i)) / 100) + dispmt * m * ah-rate / 100) /
003044        a-prm-angle-n
003045
003046     .
003047 0200-carry-on-tx.
003048
003049     COMPUTE DL-PREMIUM ROUNDED =
003050         SINGLE-FACTOR *
003051         ((lifePMT1 * TEMP-VALUE-5) / 100)
003052
003053     COMPUTE AH-PREMIUM ROUNDED =
003054         disPMT * TEMP-VALUE-4
003055
003056*    display 'discount       ' tx-discount-rate
003057*    display ' v to n        ' v-to-n
003058*    display ' x to m        ' x-to-m
003059*    display ' v to n - m    ' v-to-n-m
003060*    display ' anglen        ' a-angle-n
003061*    display ' gamma         ' gamma
003062*    display ' anglend       ' a-prm-angle-n
003063*    display ' tv1           ' temp-value-1
003064*    display ' tv2           ' temp-value-2
003065*    display ' tv3           ' temp-value-3
003066*    display ' tv4           ' temp-value-4
003067*    display ' tv5           ' temp-value-5
003068*    display ' lf factor     ' life-factor
003069*    display ' payment       ' pmt
003070*    display ' dis pmt       ' dispmt
003071*    display ' lifepmt1      ' lifepmt1
003072*    display ' new pmt       ' newpmt
003073*    display ' single factor ' single-factor
003074*    display ' lf prem       ' dl-premium
003075*    display ' disab prem    ' ah-premium
003076
003077     go to 0200-continue
003078
003079     .
003080 0200-try-oh.
003081
003082***____________________________________________________________***
003083**(-                                                          -)**
003084**(-   This section is for OH, AK, KY, MA, MN, NH, NV         -)**
003085**(-     and WA                                               -)**
003086**(-                                                          -)**
003087***____________________________________________________________***
003088
003089     if ws-rate-state <> 'OH' and 'AK' and 'KY' and 'MA' and
003090                         'MN' and 'NH' and 'NV' and 'WA'
003091        go to 0200-try-al
003092     end-if
003093
003094     evaluate true
003095        when ws-rate-state = 'OH' or 'AK' or 'KY' or 'NH' or
003096                             'NV'
003097           move 2                to plus2
003098        when ws-rate-state = 'MN'
003099           move 1                to plus2
003100        when ws-rate-state = 'MA' or 'WA'
003101           move 0                to plus2
003102        when other
003103           move 1                to plus2
003104     end-evaluate
003105
003106     COMPUTE TEMP-VALUE-1 ROUNDED =
003107        ((M - (A-ANGLE-N - A-ANGLE-N-M)) / I)
003108     compute temp-value-2 rounded =
003109        (temp-value-1 * lf-rate * (1 + (plus2 * i))) / 1000
003110*    COMPUTE TEMP-VALUE-2 ROUNDED =
003111*       (lf-rate / 1000) * (1 + (PLUS2 * I))
003112     COMPUTE TEMP-VALUE-3 ROUNDED =
003113        M * (AH-RATE / 100)
003114
003115     COMPUTE PMT ROUNDED = L /
003116        (A-PRM-ANGLE-N -
003117        TEMP-VALUE-2 - TEMP-VALUE-3)
003118
003119*    COMPUTE PMT ROUNDED = L /
003120*       (A-PRM-ANGLE-N -
003121*       (TEMP-VALUE-1 * TEMP-VALUE-2) - TEMP-VALUE-3)
003122
003123     if ws-lf-limit-partial-cov = 0
003124        move pmt                 to dispmt
003125                                    lifepmt1
003126                                    lifepmt2
003127                                    newpmt
003128     end-if
003129
003130     .
003131 0200-carry-on-oh.
003132
003133     COMPUTE DL-PREMIUM ROUNDED =
003134        PMT * TEMP-VALUE-2
003135
003136*    COMPUTE DL-PREMIUM ROUNDED =
003137*       PMT * TEMP-VALUE-1 * TEMP-VALUE-2
003138
003139     COMPUTE AH-PREMIUM ROUNDED =
003140        PMT * m * (ah-rate / 100)
003141
003142*    COMPUTE AH-PREMIUM ROUNDED =
003143*       PMT * TEMP-VALUE-3
003144
003145*    display ' plus2      ' plus2
003146*    display ' days       ' d
003147*    display ' anglen     ' A-ANGLE-N
003148*    display ' anglend    ' a-prm-angle-n
003149*    display ' gamma      ' gamma
003150*    display ' tv1        ' temp-value-1
003151*    display ' tv2        ' temp-value-2
003152*    display ' tv3        ' temp-value-3
003153*    display ' pmt        ' pmt
003154*    display ' lf prem    ' dl-premium
003155*    display ' ah prem    ' ah-premium
003156
003157     go to 0200-continue
003158
003159     .
003160 0200-try-al.
003161
003162***____________________________________________________________***
003163**(-                                                          -)**
003164**(-   This section is for AL STD RATES.                      -)**
003165**(-                                                          -)**
003166***____________________________________________________________***
003167
003168     if ws-rate-state <> 'AL'
003169        go to 0200-try-ME
003170     end-if
003171
003172     COMPUTE TEMP-VALUE-1 ROUNDED =
003173         M - (A-ANGLE-N - A-ANGLE-N-M) + (I * M)
003174
003175     COMPUTE TEMP-VALUE-2 ROUNDED =
003176         TEMP-VALUE-1 / I * lf-RATE
003177
003178     COMPUTE TEMP-VALUE-3 ROUNDED =
003179         M * (AH-RATE / 100)
003180     COMPUTE pmt ROUNDED =
003181         L /
003182         (A-PRM-ANGLE-N - (TEMP-VALUE-2 / 1000) -
003183         TEMP-VALUE-3)
003184
003185
003186     if ws-lf-limit-partial-cov = 0
003187        move pmt                 to dispmt
003188                                    lifepmt1
003189                                    lifepmt2
003190                                    newpmt
003191     end-if
003192
003193     .
003194 0200-carry-on-al.
003195
003196     COMPUTE DL-PREMIUM ROUNDED =
003197         PMT * (TEMP-VALUE-2 / 1000)
003198     COMPUTE AH-PREMIUM ROUNDED =
003199         PMT * TEMP-VALUE-3
003200
003201     go to 0200-continue
003202
003203     .
003204 0200-try-ME.
003205
003206***____________________________________________________________***
003207**(-                                                          -)**
003208**(-   This section is for ME STD RATES.                      -)**
003209**(-                                                          -)**
003210***____________________________________________________________***
003211
003212     if ws-rate-state <> 'ME'
003213        go to 0200-try-IN
003214     end-if
003215
003216     COMPUTE TEMP-DISCOUNT-RATE ROUNDED =
003217         1 / (1 + ((0.045 * N) / 24))
003218
003219     COMPUTE TEMP-VALUE-1 ROUNDED =
003220         (N - A-ANGLE-N) / I
003221
003222     COMPUTE TEMP-VALUE-2 ROUNDED =
003223         TEMP-VALUE-1 * (lf-rate / 1000) * TEMP-DISCOUNT-RATE
003224
003225     COMPUTE TEMP-VALUE-3 ROUNDED =
003226         (N * AH-RATE) / 100
003227
003228     COMPUTE PMT ROUNDED =
003229         L / (A-PRM-ANGLE-N - TEMP-VALUE-2 - TEMP-VALUE-3)
003230
003231     if ws-lf-limit-partial-cov = 0
003232        move pmt                 to dispmt
003233                                    lifepmt1
003234                                    lifepmt2
003235                                    newpmt
003236     end-if
003237
003238     .
003239 0200-carry-on-me.
003240
003241     COMPUTE DL-PREMIUM ROUNDED =
003242         PMT * TEMP-VALUE-2
003243
003244     COMPUTE AH-PREMIUM ROUNDED =
003245         PMT * TEMP-VALUE-3
003246
003247     go to 0200-continue
003248
003249     .
003250 0200-try-IN.
003251
003252***____________________________________________________________***
003253**(-                                                          -)**
003254**(-   This section is for IN, MT, ND, NJ, AZ, RI, VT and VA  -)**
003255**(-                                                          -)**
003256***____________________________________________________________***
003257
003258     if ws-rate-state <> 'IN' and 'MT' and 'ND' and 'NJ' and
003259                         'AZ' and 'RI' and 'VT' and 'VA'
003260        go to 0200-try-PA
003261     end-if
003262
003263     evaluate true
003264        when ws-rate-state = 'MT' or 'AZ' or 'RI' or 'VT'
003265           MOVE 2                TO PLUS2
003266        when ws-rate-state = 'IN' or 'ND' or 'VA'
003267           MOVE 0                TO PLUS2
003268        when ws-rate-state = 'NJ'
003269           move 1                to plus2
003270     end-evaluate
003271
003272     COMPUTE V-TO-N ROUNDED = (1 / (1 + I)) ** N
003273
003274     COMPUTE TEMP-DISCOUNT-RATE ROUNDED =
003275         ws-DISCOUNT-RATE * 12 / PPY
003276
003277     COMPUTE X ROUNDED = 1 / (1 + TEMP-DISCOUNT-RATE)
003278     COMPUTE X-TO-N ROUNDED = X ** N
003279
003280     COMPUTE TEMP-VALUE-1 ROUNDED =
003281         (X-TO-N - V-TO-N) / ((X * (1 + I)) - 1)
003282     COMPUTE TEMP-VALUE-2 ROUNDED =
003283         (X-TO-N - 1) / (X - 1)
003284     COMPUTE TEMP-VALUE-4 ROUNDED =
003285         TEMP-VALUE-2 - TEMP-VALUE-1
003286     COMPUTE TEMP-VALUE-5 ROUNDED =
003287         1 + ((PLUS2 * PPY / 12) * I)
003288
003289     COMPUTE LIFE-FACTOR ROUNDED =
003290         (TEMP-VALUE-5 / I) * TEMP-VALUE-4 *
003291         (lf-rate / 1000)
003292
003293     COMPUTE TEMP-VALUE-3 ROUNDED =
003294         (N * AH-RATE) / 100
003295
003296     COMPUTE SINGLE-FACTOR ROUNDED =
003297         (LIFE-FACTOR * 100) / (A-ANGLE-N * TEMP-VALUE-5)
003298
003299     COMPUTE PMT ROUNDED =
003300         L / (A-PRM-ANGLE-N - LIFE-FACTOR - TEMP-VALUE-3)
003301
003302     if ws-lf-limit-partial-cov = 0
003303        move pmt                 to dispmt
003304                                    lifepmt1
003305                                    lifepmt2
003306                                    newpmt
003307     end-if
003308
003309     .
003310 0200-carry-on-in.
003311
003312     COMPUTE DL-PREMIUM ROUNDED =
003313         SINGLE-FACTOR *
003314         ((PMT * A-ANGLE-N * TEMP-VALUE-5) / 100)
003315
003316     COMPUTE AH-PREMIUM ROUNDED =
003317         PMT * TEMP-VALUE-3
003318
003319     go to 0200-continue
003320
003321     .
003322 0200-try-PA.
003323
003324***____________________________________________________________***
003325**(-                                                          -)**
003326**(-   This section is for PA.                                -)**
003327**(-                                                          -)**
003328***____________________________________________________________***
003329
003330     if ws-rate-state <> 'PA'
003331        go to 0200-try-all-other
003332     end-if
003333
003334     evaluate true
003335        when ws-rate-state = 'PA'
003336           MOVE 0                TO PLUS2
003337        when other
003338           move 1                to plus2
003339     end-evaluate
003340
003341     COMPUTE TEMP-VALUE-1 ROUNDED =
003342         1 + (ws-DISCOUNT-RATE * (M / 24))
003343
003344     COMPUTE TEMP-VALUE-2 ROUNDED =
003345         1 / (10 * TEMP-VALUE-1)
003346
003347     COMPUTE TEMP-VALUE-3 ROUNDED =
003348         (M * AH-RATE) / 100
003349
003350     COMPUTE TEMP-VALUE-4 ROUNDED =
003351         (M - (A-ANGLE-N - A-ANGLE-N-M)) / I
003352
003353     COMPUTE TEMP-VALUE-5 ROUNDED =
003354         (lf-rate / 100) * (1 + (PLUS2 * ws-rate-apr / 12))
003355
003356     COMPUTE TEMP-VALUE-6 ROUNDED =
003357         TEMP-VALUE-4 * TEMP-VALUE-2 * TEMP-VALUE-5
003358
003359     COMPUTE PMT ROUNDED =
003360         L / (A-PRM-ANGLE-N - TEMP-VALUE-6 - TEMP-VALUE-3)
003361
003362     if ws-lf-limit-partial-cov = 0
003363        move pmt                 to dispmt
003364                                    lifepmt1
003365                                    lifepmt2
003366                                    newpmt
003367     end-if
003368
003369     .
003370 0200-carry-on-PA.
003371
003372     COMPUTE DL-PREMIUM ROUNDED =
003373         PMT * TEMP-VALUE-6
003374
003375     COMPUTE AH-PREMIUM ROUNDED =
003376         PMT * TEMP-VALUE-3
003377
003378     go to 0200-continue
003379
003380     .
003381 0200-try-all-other.
003382
003383***____________________________________________________________***
003384**(-                                                          -)**
003385**(-   This works for all states other than listed above      -)**
003386**(-  Haven't figured out all the states' extra months        -)**
003387**(-  interest yet.                                           -)**
003388**(-                                                          -)**
003389***____________________________________________________________***
003390
003391     evaluate true
003392        when ws-rate-state = 'SC'
003393           move 1                to plus2
003394        when other
003395           move 2                to plus2
003396     end-evaluate
003397
003398     COMPUTE TEMP-VALUE-3 ROUNDED =
003399        M + ((D - DPP) / DPP)
003400
003401***____________________________________________________________***
003402**(-                                                          -)**
003403**(-      OK, the below formula works in conjunction with     -)**
003404**(-    other than 30 days to first payment. Since Logic      -)**
003405**(-    gives me the actual rate, I have to convert it to the -)**
003406**(-   annual rate so it works just like QC.                  -)**
003407**(-                                                          -)**
003408**(-                                                          -)**
003409***____________________________________________________________***
003410
003411     compute mnthly-rate rounded = lf-rate / m
003412     compute mob rounded = mnthly-rate * 12
003413
003414     compute mob rounded = lf-rate / (m / 12)
003415
003416*    compute mob rounded =
003417*       (lf-rate / (m + (ppy/12))) * 2
003418
003419     COMPUTE MOB ROUNDED =
003420        (mob *
003421        (TEMP-VALUE-3 / PPY)) / ((M + (PPY / 12)) / 2)
003422
003423     COMPUTE TEMP-VALUE-1 ROUNDED =
003424         (M - (A-ANGLE-N - A-ANGLE-N-M)) / I
003425
003426     COMPUTE TEMP-VALUE-2 ROUNDED =
003427         1 + (PLUS2 * I * (PPY / 12))
003428
003429*    display ' L         ' l
003430*    display ' anglen       ' a-angle-n
003431*    display ' days(d)      ' d
003432*    display ' gamma        ' gamma
003433*    display ' tv3          ' temp-value-3
003434*    display 'a-prm-angle-n ' a-prm-angle-n
003435*    display ' tv1          ' temp-value-1
003436*    display ' mob          ' mob
003437*    display ' tv2          ' temp-value-2
003438*    display ' ah rate      ' ah-rate
003439*    display ' m            ' m
003440     COMPUTE pmt ROUNDED =
003441         L / (A-PRM-ANGLE-N - (TEMP-VALUE-1 * (MOB / 100) *
003442         TEMP-VALUE-2) - ((ah-rate * M) / 100))
003443
003444     .
003445 0200-check-partial.
003446
003447     if ws-lf-limit-partial-cov = 0
003448        move pmt                 to dispmt
003449                                    lifepmt1
003450                                    lifepmt2
003451                                    newpmt
003452        go to 0200-carry-on-all-other
003453     end-if
003454
003455     if pmt < ws-di-limit-max-mo-ben
003456        move pmt                 to dispmt
003457     else
003458        move ws-di-limit-max-mo-ben
003459                                 to dispmt
003460     end-if
003461
003462     if (ws-di-limit-max-tot-ben / m) < dispmt
003463        compute dispmt rounded =
003464           (ws-di-limit-max-tot-ben / m) - .005
003465     end-if
003466
003467     compute lifepmt1 rounded =
003468        (l + pmt * temp-value-1 * mob / 100 * temp-value-2 +
003469        dispmt * (ah-rate * m) / 100) / a-prm-angle-n
003470
003471     if (ws-lf-limit-max-benefit / a-prm-angle-n) < lifepmt1
003472        compute lifepmt1 rounded =
003473           ws-lf-limit-max-benefit / a-prm-angle-n
003474        move ws-lf-limit-max-benefit
003475                                 to ws-max-lf-benefit
003476     end-if
003477
003478     compute newpmt rounded =
003479        (l +lifepmt1 * temp-value-1 * mob / 100 * temp-value-2 +
003480        dispmt * (ah-rate * m) / 100) / a-prm-angle-n
003481
003482     .
003483 0200-carry-on-all-other.
003484
003485     COMPUTE DL-PREMIUM ROUNDED = lifepmt1 *
003486         TEMP-VALUE-1 * (MOB / 100) * TEMP-VALUE-2
003487
003488     COMPUTE AH-PREMIUM ROUNDED =
003489         disPmt * M * (AH-RATE / 100)
003490
003491     move pmt                    to ws-rate-per-pmt
003492     move dl-premium             to ws-rate-lf-prem
003493     move ah-premium             to ws-rate-ah-prem
003494     move t-financed             to ws-rate-tot-financed
003495     move t-payments             to ws-rate-tot-pmts
003496
003497     .
003498 0200-continue.
003499
003500     COMPUTE T-FINANCED =
003501         L + DL-PREMIUM + AH-PREMIUM
003502
003503     COMPUTE T-PAYMENTS = N * newPmt
003504
003505     if ws-max-lf-benefit = zeros
003506        move t-financed          to ws-max-lf-benefit
003507     end-if
003508
003509     move newpmt                 to ws-rate-per-pmt
003510     move dispmt                 to ws-max-ah-benefit
003511     move dl-premium             to ws-rate-lf-prem
003512     move ah-premium             to ws-rate-ah-prem
003513     move t-financed             to ws-rate-tot-financed
003514     move t-payments             to ws-rate-tot-pmts
003515
003516*    display ' new pmt       ' newpmt
003517*    display ' dis pmt       ' dispmt
003518*    display ' lifepmt1      ' lifepmt1
003519*    display ' lf-rate       ' lf-rate
003520*    display ' ah-rate       ' ah-rate
003521*    display ' payment       ' pmt
003522*    display ' lf prem       ' ws-rate-lf-prem
003523*    display ' ah prem       ' ws-rate-ah-prem
003524*    display ' tot financed  ' ws-rate-tot-financed
003525*    display ' tot payments  ' ws-rate-tot-pmts
003526
003527     .
003528 0200-exit.
003529     exit.
003530
003531 0250-calc-GP-payment.
003532
003533*  States with special coding in quick-calc for Gross Pay
003534*   AZ
003535*   IN
003536*   MT
003537*   ND
003538*   NJ
003539*   PA
003540
003541***____________________________________________________________***
003542**(-                                                          -)**
003543**(-   Right now this should work for all states other than   -)**
003544**(-    those listed above.                                   -)**
003545**(-                                                          -)**
003546***____________________________________________________________***
003547
003548     move ws-rate-loan-amt       to l
003549     move ws-rate-loan-term      to n
003550     move ws-rate-lf-term        to m
003551     if m = zeros
003552        move ws-rate-ah-term     to m
003553     end-if
003554     move ws-rate-pmts-per-year  to ppy
003555     move 30                     to dpp
003556     compute i = ws-rate-apr / (ppy * 100)
003557
003558     if i = 0
003559        move n                   to a-angle-n
003560     else
003561        COMPUTE A-ANGLE-N ROUNDED =
003562           (1 - ((1 / (1 + I)) ** N)) / I
003563     end-if
003564
003565     COMPUTE GAMMA ROUNDED =
003566         (1 + ((D * I) / DPP)) / (1 + I)
003567
003568     COMPUTE A-PRM-ANGLE-N ROUNDED =
003569        A-ANGLE-N / GAMMA
003570
003571     compute ws-rate-loan-pmt rounded =
003572        l / a-prm-angle-n
003573
003574     COMPUTE TEMP-VALUE-1 ROUNDED =
003575        (N + ((D - DPP) / DPP)) / PPY
003576
003577     compute mnthly-rate rounded = lf-rate / n
003578     compute mob rounded = mnthly-rate * 12
003579*    compute mob rounded =
003580*       (lf-rate / n * 12)
003581
003582     COMPUTE TEMP-VALUE-2 ROUNDED = A-PRM-ANGLE-N -
003583        ((N * TEMP-VALUE-1 *
003584        (mob / 100)) +
003585        (N * (aH-RATE / 100)))
003586
003587     COMPUTE pmt ROUNDED = L / TEMP-VALUE-2
003588*    display ' anglen ' a-angle-n
003589*    display ' gamma  ' gamma
003590*    display ' prm anglen ' a-prm-angle-n
003591*    display ' tv1        ' temp-value-1
003592*    display ' mob        ' mob
003593*    display ' tv2        ' temp-value-2
003594*    display ' pmt        ' pmt
003595
003596     .
003597 0250-check-for-partial.
003598
003599     if ws-lf-limit-partial-cov = 0
003600        move pmt                 to dispmt
003601                                    lifepmt1
003602                                    lifepmt2
003603                                    newpmt
003604        go to 0250-carry-on
003605     end-if
003606
003607     if pmt < ws-di-limit-max-mo-ben
003608        move pmt                 to dispmt
003609     else
003610        move ws-di-limit-max-mo-ben
003611                                 to dispmt
003612     end-if
003613
003614     if (ws-di-limit-max-tot-ben / n) < dispmt
003615        compute dispmt rounded =
003616           (ws-di-limit-max-tot-ben / n) - .005
003617     end-if
003618
003619     compute lifepmt1 rounded =
003620        (l + (pmt * n * (n + (d - dpp) / dpp) / ppy * mob /
003621           100) + dispmt * (n * ah-rate / 100)) / A-PRM-ANGLE-N
003622
003623     if (ws-lf-limit-max-benefit / n) < lifepmt1
003624        compute lifepmt1 rounded =
003625           ws-lf-limit-max-benefit / n
003626        move ws-lf-limit-max-benefit
003627                                 to ws-max-lf-benefit
003628     end-if
003629
003630     compute newpmt rounded =
003631        (l + (lifepmt1 * n * (n + (d - dpp) / dpp) / ppy *
003632           mob / 100) + dispmt * (n * ah-rate / 100)) /
003633           A-PRM-ANGLE-N
003634
003635     .
003636 0250-carry-on.
003637
003638     COMPUTE DL-PREMIUM ROUNDED =
003639        lifepmt1 * N * TEMP-VALUE-1 *
003640        (mob / 100)
003641
003642     COMPUTE AH-PREMIUM ROUNDED =
003643        dispmt * m * (AH-RATE / 100)
003644
003645*    display ' L         ' l
003646*    display ' anglen       ' a-angle-n
003647*    display ' days(d)      ' d
003648*    display ' gamma        ' gamma
003649*    display 'a-prm-angle-n ' a-prm-angle-n
003650*    display ' tv1          ' temp-value-1
003651*    display ' mob          ' mob
003652*    display ' tv2          ' temp-value-2
003653*    display ' ah rate      ' ah-rate
003654*    display ' m            ' m
003655
003656     COMPUTE T-FINANCED =
003657         L + DL-PREMIUM + AH-PREMIUM
003658
003659     COMPUTE T-PAYMENTS = N * newPmt
003660
003661     if i = 0
003662        move t-financed          to t-payments
003663     end-if
003664
003665     if ws-max-lf-benefit = zeros
003666        move t-payments          to ws-max-lf-benefit
003667     end-if
003668
003669     move newpmt                 to ws-rate-per-pmt
003670     move dispmt                 to ws-max-ah-benefit
003671     move dl-premium             to ws-rate-lf-prem
003672     move ah-premium             to ws-rate-ah-prem
003673     move t-financed             to ws-rate-tot-financed
003674     move t-payments             to ws-rate-tot-pmts
003675
003676
003677*    display ' lf-rate       ' lf-rate
003678*    display ' ah-rate       ' ah-rate
003679*    display ' payment       ' pmt
003680*    display ' dis pmt       ' dispmt
003681*    display ' lifepmt1      ' lifepmt1
003682*    display ' new pmt       ' newpmt
003683*    display ' lf prem       ' ws-rate-lf-prem
003684*    display ' ah prem       ' ws-rate-ah-prem
003685*    display ' tot financed  ' ws-rate-tot-financed
003686*    display ' tot payments  ' ws-rate-tot-pmts
003687
003688     go to 0250-exit
003689
003690     .
003691 xxxx-code-that-works. *>!!!!!!
003692
003693***____________________________________________________________***
003694**(-                                                          -)**
003695**(-   Right now this should work for all states other than   -)**
003696**(-    listed above. Below worked prior to adding partial    -)**
003697**(-    coverage stuff.                                       -)**
003698***____________________________________________________________***
003699
003700     move ws-rate-loan-amt       to l
003701     move ws-rate-loan-term      to n
003702     move ws-rate-lf-term        to m
003703     move ws-rate-pmts-per-year  to ppy
003704     move 30                     to dpp
003705     compute i = ws-rate-apr / (ppy * 100)
003706
003707     if i = 0
003708        move n                   to a-angle-n
003709     else
003710        COMPUTE A-ANGLE-N ROUNDED =
003711           (1 - ((1 / (1 + I)) ** N)) / I
003712     end-if
003713
003714     if i = 0
003715        compute a-angle-n-m = n - m
003716     else
003717        COMPUTE A-ANGLE-N-M ROUNDED =
003718           (1 - ((1 / (1 + I)) ** (N - M))) / I
003719     end-if
003720
003721     COMPUTE GAMMA ROUNDED =
003722         (1 + ((D * I) / DPP)) / (1 + I)
003723
003724     COMPUTE A-PRM-ANGLE-N ROUNDED =
003725        A-ANGLE-N / GAMMA
003726
003727     COMPUTE TEMP-VALUE-1 ROUNDED =
003728        (N + ((D - DPP) / DPP)) / PPY
003729
003730     compute mob rounded =
003731        (lf-rate / n * 12)
003732
003733     COMPUTE TEMP-VALUE-2 ROUNDED = A-PRM-ANGLE-N -
003734        ((N * TEMP-VALUE-1 *
003735        (mob / 100)) +
003736        (N * (aH-RATE / 100)))
003737
003738     COMPUTE pmt ROUNDED = L / TEMP-VALUE-2
003739
003740     COMPUTE DL-PREMIUM ROUNDED =
003741        pmt * N * TEMP-VALUE-1 *
003742        (mob / 100)
003743
003744
003745     COMPUTE AH-PREMIUM ROUNDED =
003746        pmt * N * (AH-RATE / 100)
003747
003748*    display ' L         ' l
003749*    display ' anglen       ' a-angle-n
003750*    display ' days(d)      ' d
003751*    display ' gamma        ' gamma
003752*    display 'a-prm-angle-n ' a-prm-angle-n
003753*    display ' tv1          ' temp-value-1
003754*    display ' mob          ' mob
003755*    display ' tv2          ' temp-value-2
003756*    display ' ah rate      ' ah-rate
003757*    display ' m            ' m
003758
003759     COMPUTE T-FINANCED =
003760         L + DL-PREMIUM + AH-PREMIUM
003761
003762     COMPUTE T-PAYMENTS = N * Pmt
003763
003764     move pmt                    to ws-rate-per-pmt
003765     move dl-premium             to ws-rate-lf-prem
003766     move ah-premium             to ws-rate-ah-prem
003767     move t-financed             to ws-rate-tot-financed
003768     move t-payments             to ws-rate-tot-pmts
003769
003770*    display ' lf-rate       ' lf-rate
003771*    display ' ah-rate       ' ah-rate
003772*    display ' payment       ' pmt
003773*    display ' lf prem       ' ws-rate-lf-prem
003774*    display ' ah prem       ' ws-rate-ah-prem
003775*    display ' tot financed  ' ws-rate-tot-financed
003776*    display ' tot payments  ' ws-rate-tot-pmts
003777
003778     .
003779 0250-exit.
003780     exit.
003781
003782 0300-RETURN-CICS.
003783
003784     perform 0400-disconnect     thru 0400-exit
003785     move ws-return-string       to dfhcommarea
003786     
      * exec cics return end-exec
      *    MOVE '.(                    ''   #00007339' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037333339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003787     
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSMESS02' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
003788
003789     .
003790 0300-exit.
003791     exit.
003792
003793 0400-disconnect.
003794
003795     if connected-to-db
003797        EXEC SQL
                  disconnect all
003798        END-EXEC
003799        move ' ' to ws-connect-sw
003800     end-if
003801
003802     .
003803 0400-exit.
003804     exit.
003805
003806 6000-CONNECT-TO-DB.
003807
003808***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
003809***                                                            ***
003810***                                                            ***
003811***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
003812
003813****  The below code is for when the db has been
003814****  converted to sql server 2016
003815     evaluate ws-kix-myenv
003816        when 'cid1p'
003817           move '//sdv-db01.cso.local:1433;'
003818                                 to p-sql-server
003819        when 'mdoff'
003820           move '//hov-tstdb01.cso.local:55330;'
003821                                 to p-sql-server
003822        when other
003823           move '//hov-tstdb01.cso.local:1433;'
003824                                 to p-sql-server
003825     end-evaluate
003826
003827
003828     move 'CertManagement'       to p-sql-database
003829
003830     CALL 'SQLCONNECT' USING sqlconnect-parms
003831     display ' ret code ' p-connect-return-code
003832     move p-connect-return-code  to sqlcode
003833     move p-sql-return-message   to sqlerrmc
003834
003835*
003836*     EXEC SQL
003837**       CONNECT TO :svr USER :usr-pass
003838*        CONNECT TO :svr
003839*          USER     :usr
003840*          USING    :pass
003841*     END-EXEC
003842
003843     if sqlcode not = 0
003844        display "Error: cannot connect to " svr
003845        display sqlcode
003846        display sqlerrmc
003847     else
003848        display ' Successful Connect ' sqlcode
003849        set connected-to-db to true
003850     end-if
003851
003852     .
003853 6000-EXIT.
003854     EXIT.
003855
003856 7000-GET-RATE.
003857
003858     move all '9'                to errate-key
003859
003860     MOVE CP-COMPANY-CD          TO RATE-COMPANY-CD
003861     MOVE CP-STATE               TO RATE-ST-CODE
003862     MOVE CP-CLASS-CODE          TO RATE-ST-CLASS
003863     MOVE CP-DEVIATION-CODE      TO RATE-ST-DEV
003864     MOVE CP-ISSUE-AGE           TO RATE-HIGH-AGE
003865
003866     IF CP-RATING-BENEFIT-AMT NOT NUMERIC OR
003867        CP-RATING-BENEFIT-AMT = ZEROS
003868        MOVE CP-ORIGINAL-BENEFIT TO RATE-HIGH-AMT
003869     ELSE
003870        MOVE CP-RATING-BENEFIT-AMT
003871                                 TO RATE-HIGH-AMT
003872     end-if
003873
003874     IF CP-AH
003875         MOVE CP-AH-OVERRIDE-CODE
003876                                 TO RATE-L-AH
003877     ELSE
003878         MOVE CP-LIFE-OVERRIDE-CODE
003879                                 TO RATE-L-AH
003880     end-if
003881
003882     MOVE CP-BENEFIT-CD          TO RATE-LAH-NUM
003883     MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1
003884     MOVE SPACE                  TO DC-OPTION-CODE
003885
003886     PERFORM 9700-date-link      thru 9700-exit
003887
003888     IF NO-CONVERSION-ERROR
003889        move errate-key          to save-errate-key
003890        MOVE dc-greg-date-cymd   TO RATE-EXPIRY-DATE
003891                                    svrt-expiry-date
003892     ELSE
003893        MOVE '2'                 TO CP-RETURN-CODE
003894        GO TO 7000-exit
003895     END-IF
003896
003897     
      * EXEC CICS STARTBR
003898*       DATASET  ('ERRATE')
003899*       RIDFLD   (ERRATE-KEY)
003900*       RESP     (ws-response)
003901*       GTEQ
003902*    END-EXEC
           MOVE 'ERRATE' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00007450' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303037343530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003903
003904     if not resp-normal
003905        move '6'                 to cp-return-code
003906        go to 7000-exit
003907     end-if
003908
003909     .
003910 7000-read-loop.
003911
003912     
      * EXEC CICS READNEXT
003913*       DATASET  ('ERRATE')
003914*       into     (RATE-RECORD)
003915*       RESP     (ws-response)
003916*       RIDFLD   (ERRATE-KEY)
003917*    END-EXEC
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV12
           MOVE 'ERRATE' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00007465' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303037343635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 RATE-RECORD, 
                 DFHEIV12, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003918
003919     if not resp-normal
003920        move '6'                 to cp-return-code
003921        go to 7000-done
003922     end-if
003923
003924     IF SVRT-COMPANY-CD = RT-COMPANY-CD  AND
003925        SVRT-ST-CODE    = RT-ST-CODE     AND
003926        SVRT-ST-CLASS   = RT-ST-CLASS    AND
003927        SVRT-ST-DEV     = RT-ST-DEV      AND
003928        SVRT-L-AH       = RT-L-AH        AND
003929        SVRT-LAH-NUM    = RT-LAH-NUM
003930        continue
003931     ELSE
003932        move '6'                 to cp-return-code
003933        go to 7000-done
003934     end-if
003935
003936     IF SVRT-HIGH-AGE > RT-HIGH-AGE
003937        move '6'                 to cp-return-code
003938        go to 7000-done
003939     end-if
003940
003941     IF SVRT-HIGH-AMT >= RT-HIGH-AMT
003942        GO TO 7000-read-loop
003943     end-if
003944
003945     IF SVRT-EXPIRY-DATE > RT-EXPIRY-DATE
003946        GO TO 7000-read-loop
003947     end-if
003948
003949     IF SVRT-EXPIRY-DATE = RT-EXPIRY-DATE
003950        GO TO 7000-read-loop
003951     end-if
003952
003953     .
003954 7000-done.
003955
003956     
      * EXEC CICS ENDBR
003957*       DATASET  ('ERRATE')
003958*    END-EXEC
           MOVE 'ERRATE' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007509' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037353039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003959
003960     .
003961 7000-EXIT.
003962     EXIT.
003963
003964 9700-DATE-LINK.
003965
003966     
      * EXEC CICS LINK
003967*         PROGRAM  ('ELDATCV')
003968*         COMMAREA (DATE-CONVERSION-DATA)
003969*         LENGTH   (DC-COMM-LENGTH)
003970*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00007519' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037353139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003971
003972 9700-EXIT.
003973      EXIT.
003974

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSMESS02' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'WSMESS02' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
