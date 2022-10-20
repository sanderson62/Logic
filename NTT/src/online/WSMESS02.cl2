      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSMESS02.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

020617******************************************************************
020617*REMARKS.                                                        *
020617*     Premium Quote                                              *
020617*  Returns premium calc based on info passed to me.              *
020617******************************************************************
020617*                   C H A N G E   L O G
020617*
020617* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020617*-----------------------------------------------------------------
020617*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020617* EFFECTIVE    NUMBER
020617*-----------------------------------------------------------------
020617* 020617   2017020300002   PEMA  New Program
080817* 080817   2017020300002   PEMA  Limit and ben code assign changes.
040622* 040622 CR2019012500003   PEMA  Migrate to SQLSERVER 2016
061515******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   WSMESS02 WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77  ws-seq-num                  pic s9(8) comp value 0.
       77  ws-flags                    pic s9(8) comp value 0.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77  WS-SAVE-ACCOUNT             PIC X(10)  VALUE SPACES.
       77  WS-BIN-ORIG-EFF-DT          PIC XX  VALUE LOW-VALUES.
       77  WS-ORIG-EFF-DT              PIC X(10)  VALUE SPACES.
       77  WS-EFF-DATE                 PIC X(10)  VALUE SPACES.
080817 77  WS-lf-exp-date              PIC X(10)  VALUE SPACES.
080817 77  ws-ah-exp-date              pic x(10)  value spaces.
       77  ws-loan-exp-date            pic x(10)  value spaces.
       77  X1                          PIC S999 COMP-3 VALUE +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  S3                          PIC S999 COMP-3 VALUE +0.
       77  b1                          pic s999 comp-3 value +0.
       77  WS-BUILD-SW                 PIC X.
           88  TIME-TO-BUILD               VALUE 'Y'.
       77  WS-SAVE-ERACCT              PIC X(2000).
       77  WS-DIS-RESP                 PIC 9(05) VALUE ZEROS.
       77  WS-PERFORM-SW               PIC X VALUE SPACES.
           88  GET-RATES                    VALUE 'R'.
           88  GET-ACT-ACCTS                VALUE 'A'.
       77  ws-bin-current-dt           pic xx  value low-values.
       77  ws-bin-eff-dt               pic xx  value low-values.
       77  ws-bin-lf-exp-dt            pic xx  value low-values.
       77  ws-bin-ah-exp-dt            pic xx  value low-values.
       77  ws-bin-loan-exp-dt          pic xx  value low-values.
       77  ws-bin-1st-pmt-dt           pic xx  value low-values.
       77  ws-bin-pri-birth-dt         pic xx  value low-values.
       77  ws-bin-cob-birth-dt         pic xx  value low-values.
       77  ws-limit-att-age-dt         pic xx  value low-values.
       77  WS-DISP-AMT                 PIC Z,ZZZ,Z99.99.
       77  ws-disp-rate                pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  rec-cnt                     pic 9(5) value zeros.
       77  ws-stop-sw                  pic x value ' '.
           88  i-say-stop                 value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-contract-sw              pic x  value ' '.
           88  contract-no-assigned      value 'Y'.
       77  ws-error-sw                 pic x value ' '.
           88  error-in-one-coverage     value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  client-id                   pic xxx.
080817 77  ws-error-sub                pic 999  value zeros.
080817 77  ws-error-sup                pic x(25) value spaces.
080817 77  ws-form-limit-name          pic x(50) value spaces.
080817 77  l1                          pic s999 comp-3 value +0.
       77  tx-discount-rate            pic s9v9(5) comp-3 value .00292.
       77  ws-discount-rate            pic s99v9(5) comp-3 value +0.
       77  temp-discount-rate          PIC S9V9(11) comp-3 value +0.
       77  ws-rate-type                pic x.
           88  sp-rate                   value 'S'.
           88  mob-rate                  value 'M'.
       77  ws-recalc-lf                pic x value ' '.
           88  recalc-lf                  value 'Y'.
       77  ws-recalc-di                pic x value ' '.
           88  recalc-di                  value 'Y'.
       77  ws-temp-lf-term             pic s999v99 comp-3 value +0.       

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       01  ws-new-lf-term              pic 999 value zeros.
       01  ws-new-ah-term              pic 999 value zeros.
       01  a-angle-n                   pic s9(7)v9(11) comp-3 value +0.
       01  a-angle-n-m                 pic s9(7)v9(11) comp-3 value +0.
       01  a-prm-angle-n               pic s9(7)v9(11) comp-3 value +0.
       01  a-prm-angle-n-m             pic s9(7)v9(11) comp-3 value +0.
       01  LIFE-FACTOR                 PIC S9(7)V9(11) COMP-3 value +0.
       01  SINGLE-FACTOR               PIC S9(7)V9(11) COMP-3 value +0.
       01  gamma                       pic s9(7)v9(11) comp-5 value +0.
       01  temp-value-1                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-2                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-3                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-4                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-5                pic s9(7)v9(11) comp-3 value +0.
       01  temp-value-6                pic s9(7)v9(11) comp-3 value +0.
       01  V-TO-M                      PIC S9(7)V9(11) COMP-3 value +0.
       01  X-TO-N                      PIC S9(7)V9(11) COMP-3 value +0.
       01  V-TO-N                      PIC S9(7)V9(11) COMP-3 value +0.
       01  X-TO-M                      PIC S9(7)V9(11) COMP-3 value +0.
       01  V-TO-N-M                    PIC S9(7)V9(11) COMP-3 value +0.
       01  X                           PIC S9(4)V9(11) COMP-3 value +0.
       01  mob                         pic s9(7)v9(11) comp-3 value +0.
       01  mnthly-rate                 pic s9(7)v9(11) comp-3 value +0.
       01  m                           pic s9(4)v9(4)  comp-3 value +0.
       01  n                           pic s9(4)v9(4)  comp-3 value +0.
       01  i                           pic s9(7)v9(11) comp-3 value +0.
       01  l                           PIC S9(7)V99    comp-3 value +0.
       01  pmt                         pic s9(7)v99    comp-3 value +0.
       01  dispmt                      pic s9(7)v99    comp-3 value +0.
       01  lifepmt1                    pic s9(7)v99    comp-3 value +0.
       01  lifepmt2                    pic s9(7)v99    comp-3 value +0.
       01  newpmt                      pic s9(7)v99    comp-3 value +0.
       01  ppy                         pic s9(5)       comp-3 value +0.
       01  dpp                         pic s9(5)       comp-3 value +0.
       01  d                           pic s9(5)       comp-3 value +0.
       01  plus2                       pic s9(5)       comp-3 value +0.
       01  lf-rate                     pic s999v9(5)   comp-3 value +0.
       01  ah-rate                     pic s999v9(5)   comp-3 value +0.
       01  DL-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
       01  LL-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
       01  AH-PREMIUM                  PIC S9(7)V99    comp-3 value +0.
       01  T-FINANCED                  PIC S9(7)V99    comp-3 value +0.
       01  T-REG-Z-FINANCED            PIC S9(7)V99    comp-3 value +0.
       01  T-PAYMENTS                  PIC S9(7)V99    comp-3 value +0.

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-dealer-state             pic xx.
       01  ws-dealer-id                pic x(10).
       01  ws-contract-eff-dt          pic x(10).
       01  ws-contract-no              pic x(10) value spaces.
       01  ws-contract-suffix          pic x     value spaces.

       01  ws-key-stuff.
           05  ws-ks-contract-no       pic x(10) value spaces.

       01  ws-error-message            pic x(50) value spaces.
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is        null.           The indicator will be -1        ***
      ***  if the value        is null  and +0 if the value is       ***
      ***  something other than null.  Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***           OR This way on an update                         ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        UPDATE                                              ***
      ***           CUC_Logic_Remittance                             ***
      ***        SET                                                 ***
      ***           LogicStatus     = :ws-status-code,               ***
      ***           LogicStatusDate = :ws-status-date,               ***
      ***           BatchNumber     = :ws-batch-no :nu-batchno       ***
      ***        WHERE                                               ***
      ***           RemitId = :ws-remit-id                           ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***    Also, when the table has a column with a data type of   ***
      ***  "BIT" and used as true/false move the 1 byte receiving    ***
      ***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
      ***  then its false. I think true would be 256.                ***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-partial-cov          pic s9(4) comp value +0.
           05  nu-check-elig           pic s9(4) comp value +0.
           05  nu-allow-trunc          pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-batchno              pic s9(4) comp value +0.
           05  nu-error-message        pic s9(4) comp value +0.

       01  sql-cert-records.
           05  sql-dlr-state           pic xx.
           05  sql-dlr-id              pic x(10).
           05  sql-eff-dt              pic x(10).
           05  sql-contr-no            pic x(10).
           05  sql-contr-suffix        pic x.

080817 01  ws-limit-issue-age          pic 999.
       01  ws-att-age                  pic 999v99.
NTTIns 01  ws-att-age-x redefines ws-att-age pic x(5).
080817 01  ws-pri-att-age              pic 999v99.
080817 01  ws-cob-att-age              pic 999v99.
080817 01  form-limit-table.
080817     05  ws-limit-name           pic x(50).
080817     05  ws-limit-cov-type       pic xx.
080817     05  ws-limit-lo-age         pic 999.
080817     05  ws-limit-hi-age         pic 999.
080817     05  ws-limit-att-age        pic 999.
080817     05  ws-limit-max-term       pic 999.
080817     05  ws-limit-max-jnt-term   pic 999.
080817     05  ws-limit-max-mo-ben     pic 9(5).
080817     05  ws-limit-max-tot-ben    pic 9(7).
080817     05  ws-limit-partial-cov    pic s9(4) comp-5.
           05  ws-limit-check-elig     pic s9(4) comp-5.
           05  ws-limit-elig-max-term  pic 999.
           05  ws-limit-allow-truncated pic s9(4) comp-5.
080817
080817 01  state-benefit-code-table.
           05  ws-sbc-min-amt          pic 999999.
080817     05  ws-sbc-state            pic xx.
080817     05  ws-sbc-cov-type         pic xx.
           05  ws-sbc-ben-amt          pic 999999.
080817     05  ws-sbc-sin-jnt          pic x.
080817     05  ws-sbc-dismember        pic x.
080817     05  ws-sbc-retroelim        pic x.
080817     05  ws-sbc-wait-days        pic 999.
080817     05  ws-sbc-max-bens         pic 999.
080817     05  ws-sbc-logic-ben-code   pic xx.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  filler.
           05  ws-work-in              pic x(10).
           05  ws-work-out             pic x(10).
           05  ws-work-out-v2 redefines
               ws-work-out             pic 9(8)v99.
           05  ws-work-out-v0 redefines
               ws-work-out             pic 9(10).
           05  ws-work-out-v5 redefines
               ws-work-out             pic 9(5)v9(5).
       01  filler.
           05  ws-last-suffix          pic x value low-values.
           05  ws-tbl-last-suffix      pic x value low-values.
           05  filler.  *> Use X1 for this table.
               10  ws-codes            pic x(26) value
               ' ABCDEFGHIJKLMNOPQRSTUVWXY'.
               10  ws-suffix-value redefines ws-codes
                 occurs 26             pic x.

       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).

       01  filler.
           05  ws-test-bin-dt          pic xx.
           05  ws-comp-bin-dt redefines ws-test-bin-dt
                                       pic s9(4) comp.
       01  ws-disp-bin-dt              pic 9(9) value zeros.
       01  ws-rate-file-stuff.
           12  ERRATE-KEY.
               16  RATE-COMPANY-CD     PIC X        VALUE SPACE.
               16  RATE-STATE-CODE.
                   20  RATE-ST-CODE    PIC  XX      VALUE SPACES.
                   20  RATE-ST-CLASS   PIC  XX      VALUE SPACES.
                   20  RATE-ST-DEV     PIC  XXX     VALUE SPACES.
               16  RATE-L-AH-CODE.
                   20  RATE-L-AH       PIC  X       VALUE SPACE.
                   20  RATE-LAH-NUM    PIC  XX      VALUE ZEROS.
               16  RATE-LIMITS.
                   20  RATE-HIGH-AGE   PIC  99      VALUE ZEROS.
                   20  RATE-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
                   20  RATE-FUTURE     PIC  XX      VALUE SPACES.
                   20  RATE-SEX        PIC  X       VALUE '9'.
               16  RATE-EXPIRY-DATE    PIC 9(11)    COMP-3.

           12  SAVE-ERRATE-KEY.
               16  SVRT-COMPANY-CD     PIC X        VALUE SPACE.
               16  SVRT-STATE-CODE.
                   20  SVRT-ST-CODE    PIC  XX      VALUE SPACES.
                   20  SVRT-ST-CLASS   PIC  XX      VALUE SPACES.
                   20  SVRT-ST-DEV     PIC  XXX     VALUE SPACES.
               16  SVRT-L-AH-CODE.
                   20  SVRT-L-AH       PIC  X       VALUE SPACE.
                   20  SVRT-LAH-NUM    PIC  XX      VALUE ZEROS.
               16  SVRT-LIMITS.
                   20  SVRT-HIGH-AGE   PIC  99      VALUE ZEROS.
                   20  SVRT-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
                   20  SVRT-FUTURE     PIC  XX      VALUE SPACES.
                   20  SVRT-SEX        PIC  X       VALUE '9'.
               16  SVRT-EXPIRY-DATE    PIC  9(11)   COMP-3.
     
       01  raw-message002-area.
           05  raw-message-num         pic x(10).
           05  raw-state               pic xx.
           05  raw-acct-no             pic x(10).
           05  raw-vin                 pic x(17).
           05  raw-lf-ben-code         pic xx.
           05  raw-ah-ben-code         pic xx.
           05  raw-earn-meth           pic x.
           05  raw-pri-birth-date      pic x(10).
           05  raw-cob-birth-date      pic x(10).
           05  raw-loan-amt            pic x(9).
           05  raw-eff-date            pic x(10).
           05  raw-1st-pmt-dt          pic x(10).
           05  raw-pmts-per-year       pic xx.
           05  raw-loan-term           pic xxx.
           05  raw-lf-term             pic XXX.
           05  raw-ah-term             pic xxx.
           05  raw-apr                 pic x(8).
           05  raw-lf-sin-jnt-ind      pic x.
           05  raw-ah-sin-jnt-ind      pic x.
           05  raw-dismemberment       pic x.
           05  raw-retro-elim          pic x.
           05  raw-waiting-days        pic xx.
           05  raw-crit-per            pic xx.

       01  ws-rate-work-area.
           05  ws-rate-state           pic xx.
           05  ws-rate-acct-no         pic x(10).
           05  ws-rate-vin             pic x(17).
           05  ws-rate-in-lf-ben-code  pic xx.
           05  ws-rate-in-ah-ben-code  pic xx.
           05  ws-rate-earn-meth       pic x.
           05  ws-rate-pri-birth-date  pic x(10).
           05  ws-rate-cob-birth-date  pic x(10).
           05  ws-rate-eff-date        pic x(10).
           05  ws-rate-1st-pmt-dt      pic x(10).
           05  ws-rate-benefit-type    pic x.
           05  ws-rate-lf-benefit-cd   pic xx.
           05  ws-rate-ah-benefit-cd   pic xx.
           05  ws-rate-loan-amt        pic 9(6)v99.
           05  ws-rate-per-pmt         pic 9(7)v99.
           05  ws-rate-loan-pmt        pic 9(7)v99.
           05  ws-rate-lf-prem         pic 9(5)v99.
           05  ws-rate-ah-prem         pic 9(5)v99.
           05  ws-rate-lf-rate         pic 99v9(5).
           05  ws-rate-ah-rate         pic 99v9(5).
           05  ws-rate-apr             pic 99v9(5).
           05  ws-rate-pmts-per-year   pic 99.
           05  ws-rate-loan-term       pic 999  value zeros.
           05  ws-rate-lf-term         pic 999  value zeros.
           05  ws-rate-ah-term         pic 999  value zeros.
           05  ws-issue-age            pic 999  value zeros.
           05  ws-cob-age              pic 999  value zeros.
           05  ws-rate-age             pic 999  value zeros.
           05  ws-max-lf-benefit       pic 9(7)v99 value zeros.
           05  ws-max-ah-benefit       pic 9(7)v99 value zeros.
           05  ws-rate-tot-financed    pic s9(7)v99 value zeros.
           05  ws-rate-tot-pmts        pic s9(7)v99 value zeros.
           05  ws-rate-crit-per        pic 99.
           05  ws-rate-sin-jnt-lf      pic x.
           05  ws-rate-sin-jnt-ah      pic x.
           05  ws-rate-dismemberment   pic x.
           05  ws-rate-retro-elim      pic x.
           05  ws-rate-waiting-days    pic 99.
           05  ws-rate-fullterm        pic x.

       01  ws-lf-limits.
           05  ws-lf-limit-lo-age         pic 999.
           05  ws-lf-limit-hi-age         pic 999.
           05  ws-lf-limit-att-age        pic 999.
           05  ws-lf-limit-max-term       pic 999.
           05  ws-lf-limit-max-benefit    pic 9(7).
           05  ws-lf-limit-partial-cov    pic s9(4) comp-5.
           05  ws-lf-limit-check-elig     pic s9(4) comp-5.
           05  ws-lf-limit-elig-max-term  pic 999.
           05  ws-lf-limit-allow-truncated pic s9(4) comp-5.

       01  ws-di-limits.
           05  ws-di-limit-lo-age         pic 999.
           05  ws-di-limit-hi-age         pic 999.
           05  ws-di-limit-att-age        pic 999.
           05  ws-di-limit-max-term       pic 999.
           05  ws-di-limit-max-jnt-term   pic 999.
           05  ws-di-limit-max-mo-ben     pic 9(5).
           05  ws-di-limit-max-tot-ben    pic 9(7).
           05  ws-di-limit-partial-cov    pic s9(4) comp-5.
           05  ws-di-limit-check-elig     pic s9(4) comp-5.
           05  ws-di-limit-elig-max-term  pic 999.
           05  ws-di-limit-allow-truncated pic s9(4) comp-5.

       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(150).
           05  ws-sc2                  pic x.
           05  ws-return-contract-no   pic x(11).
           05  ws-sc3                  pic x.
           05  ws-return-lf-max-amt    pic z,zzz,z99.99.
           05  ws-sc4                  pic x.
           05  ws-return-ah-max-amt    pic zzz,z99.99.
           05  ws-sc5                  pic x.
           05  ws-return-lf-prem       pic zzz,z99.99.
           05  ws-sc6                  pic x.
           05  ws-return-ah-prem       pic zzz,z99.99.
           05  ws-sc7                  pic x.
           05  ws-return-lf-rate       pic z9.99999.
           05  ws-sc8                  pic x.
           05  ws-return-ah-rate       pic z9.99999.
           05  ws-sc9                  pic x.
           05  ws-return-lf-exp-dt     pic x(10).
           05  ws-sc10                 pic x.
           05  ws-return-ah-exp-dt     pic x(10).
           05  ws-sc11                 pic x.
           05  ws-return-lf-benefit-cd pic xx.
           05  ws-sc12                 pic x.
           05  ws-return-ah-benefit-cd pic xx.
           05  ws-sc13                 pic x.
           05  ws-return-period-pmt    pic zzz,z99.99.
           05  ws-sc14                 pic x.
           05  ws-return-tot-financed  pic z,zzz,z99.99.
           05  ws-sc15                 pic x.
           05  ws-return-tot-pmts      pic z,zzz,z99.99.
           05  ws-sc16                 pic x.
           05  ws-return-loan-exp-dt   pic x(10).
           05  ws-sc17                 pic x.
           05  ws-return-limit-name    pic x(50).
           05  ws-sc18                 pic x.
           05  ws-return-rate-class    pic xx.
           05  ws-sc19                 pic x.
           05  ws-return-loan-pmt      pic zzz,z99.99.
           05  ws-sc20                 pic x.
           05  ws-return-principal     pic zzz,z99.99.
           05  ws-sc21                 pic x.
           05  ws-return-lf-term       pic zz9.
           05  ws-sc22                 pic x.
           05  ws-return-ah-term       pic zz9.
           05  ws-sc23                 pic x.
           05  ws-return-lf-bencd      pic xx.
           05  ws-sc24                 pic x.
           05  ws-return-max-bens      pic 99.


       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  ws-cm5-compare-key          pic x(12).
       01  WS-CM5-KEY.
           05  WS-CM5-COMPANY-CD       PIC X.                                       
           05  WS-CM5-CERT-NO          PIC X(11).

       01  ws-cm-compare-key           pic x(33).
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO.
               10  ws-cm-cert-ten      pic x(10).
               10  ws-cm-cert-suffix   pic x.

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.

       01  WS-AM-ALT-KEY.
           05  WS-AM-ALT-ACCOUNT       PIC X(10).
           05  WS-AM-ALT-EXP-DT        PIC XX.

       01  WS-CF-KEY-SAVE              PIC X(10).
       01  WS-CF-KEY.
           05  WS-CF-COMPANY-ID        PIC XXX.
           05  WS-CF-RECORD-TYPE       PIC X.
           05  WS-CF-ACCESS            PIC X(4).
           05  WS-CF-SEQ-NO            PIC S9(4) COMP.
           
       01  filler.
           05  ws-errors-table.
               10  filler              pic x(130) value                    
               '0000Transaction successfully completed'.                  
               10  filler              pic x(130) value                    
               '0101Amount entered contains invalid characters. Please c
      -        'orrect and re-submit'.
               10  filler              pic x(130) value                    
               '0102Date entered is invalid. Please correct and re-submi
      -        't'.
               10  filler              pic x(130) value                    
               '0103Customer age exceeds age limit. Not eligible for cov
      -        'erage'.
               10  filler              pic x(130) value                    
               '0104Customer will exceed age limit during term of covera
      -        'ge. Not eligible for coverage'.
               10  filler              pic x(130) value                    
               '0105Invalid loan term. Term must be more than 0 and less
      -        ' than 360'.
               10  filler              pic x(130) value
               '0106Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for credit insurance'.
               10  filler              pic x(130) value
               '0107Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for Joint credit disability insurance'.
               10  filler              pic x(130) value
               '0108Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for credit disability insurance'.
               10  filler              pic x(130) value
               '0109Total of payments exceeds maximum allowed. Loan is n
      -        'ot eligible for credit insurance'.
               10  filler              pic x(130) value                    
               '0110Monthly payment exceeds maximum allowed. Loan is not
      -        ' eligible for disability insurance'.
               10  filler              pic x(130) value                    
               '0111Loan term cannot be less than insurance term. Loan i
      -        's not eligible for credit insurance'.
               10  filler              pic x(130) value                    
               '0112Term times monthly payment exceeds Maximum total dis
      -        'ability benefit. Loan is not eligible for credit disabil
      -        'ity insurance'.
               10  filler              pic x(130) value
               '0113Cannot rate based on existing loan terms'.
               10  filler              pic x(130) value                    
               '0114Cannot rate life coverage based on existing loan ter
      -        'ms'.
               10  filler              pic x(130) value                    
               '0115Cannot rate disability coverage based on existing lo
      -        'an terms'.
               10  filler              pic x(130) value                    
               '0116Problem with rate file. Unable to rate'.              
               10  filler              pic x(130) value                    
               '0117Calculated premium is outside of tolerance'.          
               10  filler              pic x(130) value                    
               '0118This appears to be a duplicate record. Unable to rat
      -        'e'.
               10  filler              pic x(130) value                    
               '0119Loan term and insurance term cannot be zero. Unable 
      -        'to rate'.
               10  filler              pic x(130) value                    
               '0120Loan term and insurance term cannot be zero. Unable 
      -        'to rate'.
               10  filler              pic x(130) value
               '0121Cannot locate limit table. Unable to rate'.
               10  filler              pic x(130) value
               '0122Invalid company id '.
               10  filler              pic x(130) value                    
               '0123Cannot locate life benefit code. Unable to rate'.     
               10  filler              pic x(130) value                    
               '0124Cannot locate disability benefit code. Unable to rat
      -        'e'.
               10  filler              pic x(130) value                    
               '0125Calculated total of payments do not match submitted 
      -        'total of payments. Unable to rate'.
               10  filler              pic x(130) value                    
               '0126Life and Disability Terms must be the same'.
               10  filler              pic x(130) value                    
               '0127Loan term and insurance term must be the same on Gro
      -        'ss coverages'.
               10  filler              pic x(130) value                    
               '0128Total of payments does not match benefit amount prov
      -        'ided on Gross coverage. Unable to rate'.
               10  filler              pic x(130) value                    
               '0129Calculated amount financed does not match provided a
      -        'mount financed. Unable to rate'.
               10  filler              pic x(130) value
               '0130Insurance term exceeds maximum eligibility term. Loa
      -        'n is not eligible for credit insurance'.
               10  filler              pic x(130) value
               '0131Amount exceeds maximum eligibility amount. Loan is n
      -        'ot eligible for credit insurance'.
           05  filler redefines ws-errors-table occurs 32.
               10  ws-table-error-no   pic x(4).
               10  ws-table-error-mess pic x(126).


       01  ws-mess002-length           pic s9(4) comp value +1024.
       01  WS-MESS002-PASS-AREA        PIC X(1024).
       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ERCACCT.
                                        COPY ERCRATE.
                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       linkage section.
       01  DFHCOMMAREA                 pic x(1024).

       01  var  pic x(30).

       procedure division.

           display ' Entering program WSMESS02 '
           move dfhcommarea            to ws-mess002-pass-area

           perform 0000-init           thru 0000-exit

           perform 0010-process-message
                                       thru 0010-exit
      *    move ' returning string goes here ' to ws-return-string

           go to 0300-RETURN-CICS

           .
       0000-init.

           move 'CID'                  to client-id

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           evaluate client-id
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when other
                 display ' Invalid company id ' client-id
                 move 23               to ws-error-sub
                 move client-id        to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
      *          move '0113;Invalid company id ' to ws-return-string
                 go to 0300-RETURN-CICS
           END-evaluate

           move 1                      to ws-error-sub
           move low-values             to ws-bin-lf-exp-dt
                                          ws-bin-ah-exp-dt
                                          ws-bin-loan-exp-dt
           move spaces                 to ws-return-string
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
                                          ws-sc8
                                          ws-sc9
                                          ws-sc10
                                          ws-sc11
                                          ws-sc12
                                          ws-sc13
                                          ws-sc14
                                          ws-sc15
                                          ws-sc16
                                          ws-sc17
                                          ws-sc18
                                          ws-sc19
                                          ws-sc20
                                          ws-sc21
                                          ws-sc22
                                          ws-sc23
                                          ws-sc24

           .
       0000-exit.
           exit.

       0010-process-message.

           perform 0110-unstring       thru 0110-exit
           perform 0120-format-message thru 0120-exit

           perform 0020-edit-received  thru 0020-exit

           perform 0050-get-account    thru 0050-exit
           perform 0090-get-limits     thru 0090-exit

           perform 0025-assign-ben-cd  thru 0025-exit
           perform 0030-get-lf-rate    thru 0030-exit
           perform 0035-get-ah-rate    thru 0035-exit

           if ws-rate-in-lf-ben-code (1:1) = 'N' or 'T'
              perform 0200-calc-NP-payment 
                                       thru 0200-exit
           else
              perform 0250-calc-GP-payment
                                       thru 0250-exit
           end-if

      *    go to 0010-exit

           if (contract-no-assigned)
                  or
              (error-in-one-coverage)
              continue
           else
              perform 0190-final-limit-check
                                       thru 0190-exit
              perform 0060-check-for-dup
                                       thru 0060-exit
              perform 0070-open-cursor thru 0070-exit
              perform 0080-process-input
                                       thru 0080-exit
           end-if
           perform 0040-format-buffer  thru 0040-exit
           set contract-no-assigned to true

           .
       0010-exit.
           exit.

       0020-edit-received.

           if ws-rate-sin-jnt-lf = 'S' OR 'J'
              continue
           else
              move 'S'                 to ws-rate-sin-jnt-lf
           end-if
           if ws-rate-sin-jnt-ah = 'S' OR 'J'
              continue
           else
              move 'S'                 to ws-rate-sin-jnt-ah
           end-if
           if ws-rate-dismemberment = 'Y'
              continue
           else
              move 'N'                 to ws-rate-dismemberment
           end-if

           if ws-rate-in-ah-ben-code = 'A'
              if ws-rate-retro-elim = 'R' or 'E'
                 continue
              else
                 move 'R'              to ws-rate-retro-elim
              end-if
           else
              move spaces              to ws-rate-retro-elim
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed effective date. Probably should add edits   **
      **                                                              **
      **==============================================================**

           move ws-rate-eff-date (7:4) to ws-work-date (1:4)
           move ws-rate-eff-date (1:2) to ws-work-date (5:2)
           move ws-rate-eff-date (4:2) to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-dt
           else
              move 3                   to ws-error-sub
              move '- Eff Date'        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           if ws-bin-eff-dt > ws-bin-current-dt
              move 3                   to ws-error-sub
              move '- Future Eff'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed 1st pmt date.   Probably should add edits   **
      **                                                              **
      **==============================================================**

           move ws-rate-1st-pmt-dt (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-1st-pmt-dt (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-1st-pmt-dt (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-1st-pmt-dt
           else
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt Dt'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           if ws-bin-1st-pmt-dt <= ws-bin-eff-dt
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt !> Eff dt' to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Only use the code below if there is a time they don't       **
      **  send the 1st pmt date.                                      **
      **                                                              **
      **==============================================================**

           if ws-bin-1st-pmt-dt = low-values
              move +1                  to dc-elapsed-months
              move +0                  to dc-elapsed-days
              move '6'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-2    to ws-bin-1st-pmt-dt
              else
                 move 3                to ws-error-sub
                 move '- 1st Pmt Dt'   to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the days to first payment date                    **
      **                                                              **
      **==============================================================**

           move ws-bin-eff-dt          to dc-bin-date-1
           move ws-bin-1st-pmt-dt      to dc-bin-date-2
           move zeros                  to dc-elapsed-months
                                          dc-elapsed-days
           move '1'                    to dc-option-code
           perform 9700-date-link   thru 9700-exit
           if no-conversion-error
              move dc-elapsed-days     to d
           else
              move 30                  to d
           end-if

      *    if d = 31
      *       move 30 to d
      *    end-if

      **==============================================================**
      **                                                              **
      **  Sometimes a lf term is passed to this module                **
      **   even thought there is no life coverage. I try and rate it  **
      **                                                              **
      **==============================================================**

           if ws-rate-in-lf-ben-code = spaces
              move zeros               to ws-rate-lf-term
           end-if

           .
       0020-calc-lf-exp.
      **==============================================================**
      **                                                              **
      **  Calculate the lf expiration date to be passed back.         **
      **                                                              **
      **==============================================================**

           if ws-rate-lf-term > 0
              move ws-bin-1st-pmt-dt   to dc-bin-date-1
              compute dc-elapsed-months = ws-rate-lf-term - 1
              move +0                  to dc-elapsed-days
              move '6'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-2    to ws-bin-lf-exp-dt
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-date
              else
                 move 3                to ws-error-sub
                 move '- Lf Exp Dt'    to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

           .
       0020-calc-ah-exp.
      **==============================================================**
      **                                                              **
      **  Calculate the ah expiration date to be passed back.         **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-ah-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-ah-exp-dt
              move dc-greg-date-a-edit to ws-ah-exp-date
           else
              move 3                   to ws-error-sub
              move '- Ah Exp Dt'       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           .
       0020-calc-loan-exp.

      **==============================================================**
      **                                                              **
      **  Calculate the loan expiration date to be passed back.       **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-loan-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-loan-exp-dt
              move dc-greg-date-a-edit to ws-loan-exp-date
           else
              move 3                   to ws-error-sub
              move '- Loan Exp Dt'     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate primary borrower birth date.                       **
      **                                                              **
      **==============================================================**

           move ws-rate-pri-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-pri-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-pri-birth-date (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-pri-birth-dt
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate co borrowers birth date.                           **
      **                                                              **
      **==============================================================**

           if ws-rate-cob-birth-date (7:4) = spaces
              move zeros               to ws-rate-cob-birth-date
           end-if
           move ws-rate-cob-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-cob-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-cob-birth-date (4:2)
                                       to ws-work-date (7:2)
           move low-values             to ws-bin-cob-birth-dt
           if ws-work-date-num not = zeros
              move ws-work-date-num    to dc-greg-date-cymd
              move 'L'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-1    to ws-bin-cob-birth-dt
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age.                            **
      **                                                              **
      **==============================================================**

           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-eff-dt          to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-issue-age = dc-elapsed-months / +12
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           .
       0020-calc-pri-att-age.
      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age at expiration date          **
      **                                                              **
      **==============================================================**

           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-lf-exp-dt       to dc-bin-date-2
           if ws-bin-ah-exp-dt > dc-bin-date-2
              move ws-bin-ah-exp-dt    to dc-bin-date-2
           end-if
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-pri-att-age =
                 dc-elapsed-months / +12
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           .
       0020-calc-cob-age.
      **==============================================================**
      **                                                              **
      **  Calculate co borrowers age.                                 **
      **                                                              **
      **==============================================================**

           move zeros                  to ws-cob-age
           if ws-bin-cob-birth-dt not = low-values
              move ws-bin-cob-birth-dt to dc-bin-date-1
              move ws-bin-eff-dt       to dc-bin-date-2
              move '1'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 compute ws-cob-age = dc-elapsed-months / +12
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

           move ws-issue-age           to ws-rate-age
           if ws-cob-age > ws-rate-age
              move ws-cob-age          to ws-rate-age
           end-if

           .
       0020-calc-cob-att-age.
      **==============================================================**
      **                                                              **
      **  Calculate co borrowers age at expiration date               **
      **                                                              **
      **==============================================================**

           move zeros                  to ws-cob-att-age
           if ws-bin-cob-birth-dt not = low-values
              move ws-bin-cob-birth-dt to dc-bin-date-1
              move ws-bin-lf-exp-dt    to dc-bin-date-2
              if ws-bin-ah-exp-dt > dc-bin-date-2
                 move ws-bin-ah-exp-dt to dc-bin-date-2
              end-if
              move '1'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 compute ws-cob-att-age =
                    dc-elapsed-months / +12
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

           move ws-cob-att-age         to ws-att-age
           if ws-pri-att-age > ws-att-age
              move ws-pri-att-age      to ws-att-age
           end-if

           .
       0020-val-terms.
      **==============================================================**
      **                                                              **
      **  Validate life and dis terms                                 **
      **                                                              **
      **==============================================================**

           if (ws-rate-lf-term > 0)
              and (ws-rate-ah-term > 0)
              if ws-rate-lf-term <> ws-rate-ah-term
                 move 27               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Validate Loan term against insurance term.                  **
      **                                                              **
      **==============================================================**

           if ((ws-rate-in-lf-ben-code (1:1) = 'G')
              and (ws-rate-lf-term > 0)
              and (ws-rate-lf-term <> ws-rate-loan-term))
                        or
              ((ws-rate-in-lf-ben-code (i:1) = ' ')
              and (ws-rate-ah-term > 0)
              and (ws-rate-ah-term <> ws-rate-loan-term))
              move 28               to ws-error-sub
              move ' '              to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

           .
       0020-exit.
           exit.

       0025-assign-ben-cd.

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           move ws-rate-state          to ws-sbc-state

           evaluate true
              when ws-rate-in-lf-ben-code (1:1) = 'N'
                 move 'NP'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = 'T'
                 move 'NT'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = 'L'
                 move 'LL'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = ' '
                 go to 0025-get-ah-ben
              when other
                 move 'GP'             to ws-sbc-cov-type
           end-evaluate

           move ws-rate-loan-amt       to ws-sbc-ben-amt
           move ws-rate-sin-jnt-lf     to ws-sbc-sin-jnt
           move ws-rate-dismemberment  to ws-sbc-dismember

           EXEC SQL
              SELECT TOP (1)
                 MIN(BenefitAmt) as LessAmt,
                 LogicBenCode
              INTO
                 :ws-sbc-min-amt,
                 :ws-sbc-logic-ben-code
              FROM
                 BenefitCodeMapping
              WHERE
                 State           = :ws-sbc-state
                 and CovType     = :ws-sbc-cov-type
                 and SinJnt      = :ws-sbc-sin-jnt
                 and Dismem      = :ws-sbc-dismember
                 and BenefitAmt  > :ws-sbc-ben-amt
              GROUP BY
                 LogicBenCode
              ORDER BY
                 LessAmt
           end-exec

           if sqlcode not = 0 and 1
              display "Error: cannot find lf benefit code "
                 ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
                 ' ' ws-sbc-dismember
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 24                  to ws-error-sub
              move spaces to ws-error-sup
              string
                 ws-sbc-state ' '
                 ws-sbc-cov-type ' '
                 ws-sbc-sin-jnt ' '
                 ws-sbc-dismember
                 delimited by size into ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

      **==============================================================**
      **                                                              **
      ** The below code already works, just want to try some stuff    **
      **                                                              **
      **==============================================================**

      *    EXEC SQL
      *       SELECT
      *          LogicBenCode
      *       INTO
      *          :ws-sbc-logic-ben-code
      *       FROM
      *          BenefitCodeMapping
      *       WHERE
      *          State           = :ws-sbc-state
      *          and CovType     = :ws-sbc-cov-type
      *          and SinJnt      = :ws-sbc-sin-jnt
      *          and Dismem      = :ws-sbc-dismember
      *          and BenefitAmt  > :ws-sbc-ben-amt
      *    end-exec
      *
      *    if sqlcode not = 0
      *       display "Error: cannot find lf benefit code "
      *          ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
      *          ' ' ws-sbc-dismember
      *       display ' sql return code ' sqlcode
      *       display ' sql err mess    ' sqlerrmc
      *       move 19                  to ws-error-sub
      *       move spaces to ws-error-sup
      *       string
      *          ws-sbc-state ' '
      *          ws-sbc-cov-type ' '
      *          ws-sbc-sin-jnt ' '
      *          ws-sbc-dismember
      *          delimited by size into ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if

           move ws-sbc-logic-ben-code  to ws-rate-lf-benefit-cd

           .
       0025-get-ah-ben.

           if ws-rate-in-ah-ben-code (1:1) = ' '
              go to 0025-exit
           end-if

           move ws-rate-state          to ws-sbc-state
           move 'DI'                   to ws-sbc-cov-type
           move ws-rate-loan-amt       to ws-sbc-ben-amt
           move ws-rate-sin-jnt-ah     to ws-sbc-sin-jnt
           move ws-rate-retro-elim     to ws-sbc-retroelim
           move ws-rate-waiting-days   to ws-sbc-wait-days
           move ws-rate-crit-per       to ws-sbc-max-bens

           EXEC SQL
              SELECT TOP (1)
                 MIN(BenefitAmt) as LessAmt,
                 LogicBenCode
              INTO
                 :ws-sbc-min-amt,
                 :ws-sbc-logic-ben-code
              FROM
                 BenefitCodeMapping
              WHERE
                 State           = :ws-sbc-state
                 and CovType     = :ws-sbc-cov-type
                 and SinJnt      = :ws-sbc-sin-jnt
                 and RetroElim   = :ws-sbc-retroelim
                 and WaitDays    = :ws-sbc-wait-days
                 and MaxBens     = :ws-sbc-max-bens
                 and BenefitAmt  > :ws-sbc-ben-amt
              GROUP BY
                 LogicBenCode
              ORDER BY
                 LessAmt
           end-exec

           if sqlcode not = 0
              display "Error: cannot find ah benefit code "
                 ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
                 ' ' ws-sbc-retroelim ' ' ws-sbc-wait-days ' '
                 ws-sbc-max-bens

              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 25                  to ws-error-sub
              move spaces              to ws-error-sup
              string
                 ws-sbc-state ' '
                 ws-sbc-cov-type ' '
                 ws-sbc-sin-jnt ' '
                 ws-sbc-retroelim ' '
                 ws-sbc-wait-days ' '
                 ws-sbc-max-bens
                 delimited by size into ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-sbc-logic-ben-code  to ws-rate-ah-benefit-cd

           .
       0025-exit.
           exit.

       0030-get-lf-rate.

           if (ws-rate-in-lf-ben-code (1:1) = ' ')
              or (ws-rate-lf-term = zeros)
              go to 0030-exit
           end-if

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
           move am-cal-table           to cp-class-CODE

           move am-lf-deviation        to cp-deviation-code

           move ws-rate-state          to cp-state
                                          cp-state-std-abbrv
           move 'L'                    to cp-benefit-type
           move ws-rate-lf-benefit-cd  to cp-benefit-cd
           move ws-rate-loan-amt       to cp-original-benefit
                                          cp-rating-benefit-amt
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           move ws-rate-apr            to cp-loan-apr

           if ws-rate-loan-term = zeros
              move ws-rate-lf-term     to ws-rate-loan-term
           end-if
           move ws-rate-lf-term        to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           if (cp-original-term = zeros)
              and (cp-loan-term = zeros)
              move 20                  to ws-error-sub
              move '-Lf term'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if
           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move ws-rate-earn-meth      to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date

           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           evaluate true
              when cp-return-code = '7'
                 move 0                to cp-return-code
                 move 14               to ws-error-sub
                 move '-Lf Rates'      to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = '6'
                 move 0                to cp-return-code
                 move 15               to ws-error-sub
                 move '-Lf Rates'      to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = 'D'
                 move 0                to cp-return-code
                 move 17               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
           end-evaluate

           if cp-error-occured
              move zeros               to lf-rate
              move 17                  to ws-error-sub
              move '-Lf rate'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move 'S'                    to ws-rate-type
           move +0                     to ws-discount-rate

           if (rt-discount-option <> ' ')
              and (rt-discount-ob-rate > zeros)
              move rt-discount-ob-rate to cp-premium-rate
              move rt-discount-rate    to ws-discount-rate
              if ws-discount-rate > 1
                 if cp-state = 'PA'
                    compute ws-discount-rate rounded =
                       ws-discount-rate / 100
                 else
                    compute ws-discount-rate rounded = 
                       ws-discount-rate / 1200
                 end-if
              end-if
              set mob-rate to true
           else
              move rt-l-rate (cp-original-term)
                                       to cp-premium-rate
           end-if

           if cp-premium-rate not numeric
              move zeros to cp-premium-rate
           end-if

      **==============================================================**
      **                                                              **
      **      The below chunk of code is for those states that have   **
      **   the SP equivalent of a discounted MOB rate stored in       **
      **   Logic. All it does is calc a 12 month rate to be used      **
      **   with the QC formulas because they expect that. I did this  **
      **   so we wouldn't have a ton of hard coded formulas that      **
      **   basically do the same thing.                               **
      **                                                              **
      **==============================================================**

           if (ws-rate-in-lf-ben-code (1:1) = 'G')
              and (cp-state = 'IN' OR 'MT' OR 'ND' OR 'NJ' OR 'PA')
              display ' converting MOB to SP ' cp-premium-rate
              compute cp-premium-rate rounded = 
                 cp-premium-rate / cp-loan-term * 12
              display ' new rate FOR ' CP-STATE ' ' cp-premium-rate
           end-if

           move cp-premium-rate        to lf-rate
                                          ws-rate-lf-rate
           move zeros                  to ws-max-lf-benefit

           .
       0030-exit.
           exit.

       0035-get-ah-rate.

           move zeros to ws-rate-ah-rate
           if ws-rate-in-ah-ben-code (1:1) = ' '
              go to 0035-exit
           end-if

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
                                          cp-original-benefit
                                          cp-rating-benefit-amt

           move am-cal-table           to cp-class-CODE

           move am-ah-deviation        to cp-deviation-code

           move ws-rate-state          to cp-state
                                          cp-state-std-abbrv
           move 'A'                    to cp-benefit-type
           move ws-rate-ah-benefit-cd  to cp-benefit-cd

           if ws-rate-ah-term > zeros
              compute cp-original-benefit =
                 ws-rate-loan-amt / ws-rate-ah-term
              move cp-original-benefit to cp-rating-benefit-amt
           end-if
      *    move ws-rate-loan-amt       to cp-original-benefit
      *                                   cp-rating-benefit-amt
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           if ws-rate-loan-term = zeros
              move ws-rate-ah-term     to ws-rate-loan-term
           end-if
           move ws-rate-ah-term        to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           if (cp-original-term = zeros)
              and (cp-loan-term = zeros)
              move 21                  to ws-error-sub
              move '-DI term'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move ws-rate-earn-meth      to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date
           
           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           evaluate true
              when cp-return-code = '7'
                 move '0'              to cp-return-code
                 move 14               to ws-error-sub
                 move '-Ah Rates'      to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = '6'
                 move '0'              to cp-return-code
                 move 16               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = 'D'
                 move '0'              to cp-return-code
                 move 17               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
           end-evaluate

           if cp-error-occured
              move zeros               to lf-rate
              move 17                  to ws-error-sub
              move '-DI rate'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move rt-ah-rate (cp-original-term)
                                       to cp-premium-rate

          move cp-premium-rate         to ah-rate
                                          ws-rate-ah-rate
          move zeros                   to ws-max-ah-benefit

           .
       0035-exit.
           exit.

       0040-format-buffer.

      *    move spaces                 to ws-return-string

           perform 0045-format-error   thru 0045-exit

           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
                                          ws-sc8
                                          ws-sc9
                                          ws-sc10
                                          ws-sc11
                                          ws-sc12
                                          ws-sc13
                                          ws-sc14
                                          ws-sc15
                                          ws-sc16
                                          ws-sc17
                                          ws-sc18
                                          ws-sc19
                                          ws-sc20
                                          ws-sc21
                                          ws-sc22
                                          ws-sc23
                                          ws-sc24

           if no-cp-error
              if ws-contract-suffix = spaces or low-values
                 move ws-contract-no   to ws-return-contract-no (2:10)
              else
                 move ws-contract-no   to ws-return-contract-no (1:10)
                 move ws-contract-suffix
                                       to ws-return-contract-no (11:1)
              end-if
              move ws-rate-lf-prem     to ws-return-lf-prem
              move ws-rate-ah-prem     to ws-return-ah-prem
              move ws-rate-lf-rate     to ws-return-lf-rate
              move ws-rate-ah-rate     to ws-return-ah-rate

              move zeros               to ws-return-principal
              move ws-rate-lf-term     to ws-return-lf-term

              move ws-rate-ah-term     to ws-return-ah-term
              move ws-rate-in-lf-ben-code
                                       to ws-return-lf-bencd
              move ws-rate-crit-per    to ws-return-max-bens

              move ws-max-ah-benefit   to ws-return-ah-max-amt
              move ws-max-lf-benefit   to ws-return-lf-max-amt
              move ws-lf-exp-date      to ws-return-lf-exp-dt
              move ws-ah-exp-date      to ws-return-ah-exp-dt
              move ws-loan-exp-date    to ws-return-loan-exp-dt
              move ws-rate-lf-benefit-cd
                                       to ws-return-lf-benefit-cd
              move ws-rate-ah-benefit-cd
                                       to ws-return-ah-benefit-cd
              move ws-rate-per-pmt     to ws-return-period-pmt
              move ws-rate-loan-pmt    to ws-return-loan-pmt
              move ws-rate-tot-financed to ws-return-tot-financed
              move ws-rate-tot-pmts    to ws-return-tot-pmts
              move ws-limit-name       to ws-return-limit-name
              move am-cal-table        to ws-return-rate-class
           else
              set error-in-one-coverage to true
              move spaces              to ws-contract-no
                                          ws-contract-suffix
           end-if

           .
       0040-exit.
           exit.

       0045-format-error.

           evaluate true
              when cp-return-code = '0'
                 move +1               to s1
              when cp-return-code = '1'
                 move +2               to s1
              when cp-return-code = '2'
                 move +3               to s1
              when cp-return-code = 'A'
                 move +4               to s1
              when cp-return-code = 'B'
                 move +5               to s1
              when cp-return-code = '4'
                 move +6               to s1
              when cp-return-code = '9'
                 move +7               to s1
              when cp-return-code = '8'
                 move +8               to s1
      *       when cp-return-code = 'H'
      *          move +9               to s1
              when cp-return-code = 'C'
                 move +10              to s1
              when cp-return-code = '7'
                 move +11              to s1
              when cp-return-code = '6'
                 move +12              to s1
              when cp-return-code = 'D'
                 move +13              to s1
              when cp-return-code = 'X'
                 move +17              to s1
              when cp-return-code = 'Z'
                 move +14              to s1
                 move cp-calc-premium  to ws-return-ah-prem
                 move cp-premium-rate  to ws-return-ah-rate
                 move ws-max-ah-benefit   to ws-return-ah-max-amt
                 move ws-ah-exp-date      to ws-return-ah-exp-dt
                 move ws-rate-ah-benefit-cd to ws-return-ah-benefit-cd
              when cp-return-code = 'Y'
                 move +15              to s1
                 move cp-calc-premium  to ws-return-ah-prem
                 move cp-premium-rate  to ws-return-ah-rate
                 move ws-max-ah-benefit   to ws-return-ah-max-amt
                 move ws-ah-exp-date      to ws-return-ah-exp-dt
                 move ws-rate-ah-benefit-cd to ws-return-ah-benefit-cd
           end-evaluate

           move ws-table-error-no (s1) to ws-return-error-no
           move ws-table-error-mess (s1)
                                       to ws-return-error-mess

           .
       0045-exit.
           exit.
           
       0050-get-account.

           move ws-comp-cd             to ws-am-company-cd
           move '9'                    to ws-am-carrier
           if ws-rate-state = 'KY'
              move '8'                 to ws-am-carrier
           end-if
           move '000000'               to ws-am-group
           move ws-rate-state          to ws-am-state
           move ws-rate-acct-no        to ws-am-account
           move ws-bin-eff-dt          to ws-am-exp-dt

           exec cics read
              dataset      ('ERACCT')
              ridfld       (ws-am-key)
              into         (account-master)
              GTEQ
              resp         (ws-response)
           end-exec

           if resp-normal
              and (ws-comp-cd = am-company-cd)
              and (ws-rate-state = am-state)
              and (ws-rate-acct-no = am-account)
              and (ws-bin-eff-dt >= am-effective-dt)
              and (ws-bin-eff-dt < am-expiration-dt)
              continue
           else
              move '0114;No account mstr found ' to ws-return-string
              go to 0300-RETURN-CICS
           end-if

           move am-comment-line (1)    to ws-form-limit-name
           move spaces                 to ws-limit-name
           perform varying l1 from +50 by -1 until
              (l1 < +1)
              or (ws-form-limit-name (l1:1) <> ' ')
           end-perform
           if l1 > +3
              perform varying l1 from l1 by -1 until
                 (l1 < +1)
                 or (ws-form-limit-name (l1:1) = ' ')
              end-perform
              if l1 > +3
                 subtract +1 from l1
                 move ws-form-limit-name (1:l1)
                                       to ws-limit-name
              end-if
           end-if

           .
       0050-exit.
           exit.

       0060-check-for-dup.

      **==============================================================**
      **                                                              **
      **    All i'm going to do here is check for a dup using         **
      **  the state, account, eff dt and last six of vin and a space  **
      **  in the cert suffix. If I do find one then I will have to    **
      **  find the last suffix and use the next available one in the  **
      **  suffix table.                                               **
      **                                                              **
      **==============================================================**

           move +1                     to x1
           move ' '                    to ws-browse-sw

           move ws-comp-cd             to ws-cm-key
           move '9'                    to ws-cm-carrier
           if ws-rate-state = 'KY'
              move '8'                 to ws-cm-carrier
           end-if
           move '000000'               to ws-cm-group
           move ws-rate-state          to ws-cm-state
           move ws-rate-acct-no        to ws-cm-account
           move ws-bin-eff-dt          to ws-cm-eff-dt
           string
              '0000'
              ws-rate-vin (12:6)
              ' ' delimited by size into ws-cm-cert-no
           end-string
           move ws-cm-key              to ws-cm-compare-key
           move ws-cm-cert-ten         to ws-contract-no
           move low-values             to ws-last-suffix

           exec cics startbr
              dataset      ('ELCERT')
              ridfld       (ws-cm-key)
              gteq 
              resp         (ws-response)
           end-exec

           evaluate true
              when resp-normal
                 set browse-started to true
              when resp-notfnd or resp-endfile
                 set i-say-stop to true
              when other
                 move ws-response to ws-disp-resp
                 string
                    '9999' ';'
                    'Bad elcert startbr ' ';'
                    ws-disp-resp delimited by size
                                       into ws-return-string
                 end-string
                 display ' something went wrong with start br '
                    ws-response
                 go to 0300-RETURN-CICS
           end-evaluate

           perform until i-say-stop
              exec cics readnext
                 dataset      ('ELCERT')
                 ridfld       (ws-cm-key)
                 into         (certificate-master)
                 resp         (ws-response)
              end-exec
              if resp-normal
                 if ws-cm-key (1:32) = ws-cm-compare-key (1:32)
                    move ws-cm-cert-suffix
                                       to ws-last-suffix
                 else
                    set i-say-stop to true
                 end-if
              else
                 set i-say-stop to true
              end-if
           end-perform

           if browse-started
              exec cics endbr
                 dataset   ('ELCERT')
              END-EXEC
           end-if

           if ws-last-suffix not = low-values
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 19               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           .
       0060-exit.
           exit.

       0070-open-cursor.

pemtst*    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           move ws-rate-state          to ws-dealer-state
           move ws-rate-acct-no        to ws-dealer-id
           move ws-rate-eff-date       to ws-contract-eff-dt
           move zeros                  to ws-ks-contract-no
           move ws-rate-vin (12:6)     to ws-ks-contract-no (5:6)
           
           EXEC SQL
              DECLARE
                 contracts cursor for
              SELECT
                 DlrState,
                 DlrId,
                 EffDt,
                 ContractNo,
                 ContractSuffix
              FROM
                 PendingContracts
              WHERE
                 DlrState     = :ws-dealer-state
                 and DlrId    = :ws-dealer-id
                 and EffDt    = :ws-contract-eff-dt
                 and ContractNo  = :ws-ks-contract-no
              ORDER BY
                 dlrstate,
                 dlrid,
                 effdt,
                 contractno,
                 contractsuffix
           end-exec

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           EXEC SQL
              open contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if

           .
       0070-exit.
           exit.
       0080-process-input.

           perform until sqlcode not = 0
              EXEC SQL
                 fetch contracts into
                    :sql-dlr-state,
                    :sql-dlr-id,
                    :sql-eff-dt,
                    :sql-contr-no,
                    :sql-contr-suffix
              END-EXEC

              if sqlcode = 0
                 move sql-contr-suffix to ws-tbl-last-suffix
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " 
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                 end-if
              end-if
           end-perform

           if ws-tbl-last-suffix not = low-values
              and (ws-tbl-last-suffix > ws-last-suffix)
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-tbl-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 19               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           EXEC SQL
               close contracts
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       0080-exit.
           exit.

       0090-get-limits.

           move zeros                  to ws-lf-limit-lo-age      
                                          ws-lf-limit-hi-age      
                                          ws-lf-limit-att-age     
                                          ws-lf-limit-max-term    
                                          ws-lf-limit-max-benefit 
                                          ws-lf-limit-partial-cov
                                          ws-lf-limit-check-elig
                                          ws-lf-limit-elig-max-term
                                          ws-lf-limit-allow-truncated
                                          ws-di-limit-lo-age      
                                          ws-di-limit-hi-age      
                                          ws-di-limit-att-age     
                                          ws-di-limit-max-term    
                                          ws-di-limit-max-jnt-term
                                          ws-di-limit-max-mo-ben  
                                          ws-di-limit-max-tot-ben 
                                          ws-di-limit-partial-cov
                                          ws-di-limit-check-elig
                                          ws-di-limit-elig-max-term
                                          ws-di-limit-allow-truncated

           if (ws-rate-lf-term = zeros)
              and (ws-rate-ah-term <> zeros)
              move 'N '                to ws-rate-in-lf-ben-code
           end-if

           perform 0092-get-lf-limits  thru 0092-exit
           perform 0097-get-di-limits  thru 0097-exit

           if recalc-lf
              perform 0020-calc-lf-exp
           end-if
           if recalc-di
              perform 0020-calc-ah-exp
              if ws-rate-ah-term < ws-rate-crit-per
                 move zeros            to ws-rate-crit-per
           end-if

           if recalc-lf or recalc-di
              perform 0020-calc-pri-att-age
              perform 0020-calc-cob-att-age
           end-if

           .
       0090-exit.
           exit.

       0092-get-lf-limits.

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           evaluate true
              when raw-lf-ben-code (1:1) = 'L' 
                 move 'LL'             to ws-limit-cov-type
              when raw-ah-ben-code (1:1) = ' '
                 move 'LO'             to ws-limit-cov-type
              when raw-lf-ben-code (1:1) = 'N' or 'G' or 'T'
                 move 'RL'             to ws-limit-cov-type
              when other   *> Assuming no life coverage
                 go to 0092-exit
           end-evaluate

           move zeros                  to ws-limit-lo-age
                                          ws-limit-hi-age
                                          ws-limit-att-age
                                          ws-limit-max-term
                                          ws-limit-max-jnt-term
                                          ws-limit-max-mo-ben
                                          ws-limit-max-tot-ben
                                          ws-limit-partial-cov
                                          ws-limit-check-elig
                                          ws-limit-elig-max-term
                                          ws-limit-allow-truncated

           move ws-rate-age            to ws-limit-issue-age

           .
       0092-get-life.

           move 30                     to ws-limit-issue-age

           EXEC SQL
              SELECT
                 AttainedAge
              INTO
                 :ws-limit-att-age
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec

           if sqlcode not = 0
              display "Error: cannot find limitsa " ws-limit-name
                 ' ' ws-limit-cov-type ' ' ws-limit-issue-age
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              if (ws-limit-cov-type = 'LO')
                 and (raw-ah-ben-code (1:1) = ' ')
                 move 'RL'             to ws-limit-cov-type
                 go to 0092-get-life
              else
                 display "Error: cannot find limits " ws-limit-name
                    ' ' ws-limit-cov-type ' ' ws-limit-issue-age
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 move 22                  to ws-error-sub
                 move ws-limit-name       to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           move ws-rate-age            to ws-limit-issue-age

           EXEC SQL
              SELECT
                 AttainedAge,
                 MaxTerm,
                 MaxTotBen,
                 PartialCoverage,
                 CheckEligibility,
                 EligibilityMaxTerm,
                 AllowTruncated
              INTO
                 :ws-limit-att-age,
                 :ws-limit-max-term,
                 :ws-limit-max-tot-ben,
                 :ws-limit-partial-cov  :nu-partial-cov,
                 :ws-limit-check-elig   :nu-check-elig,
                 :ws-limit-elig-max-term,
                 :ws-limit-allow-truncated  :nu-allow-trunc
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec

           if sqlcode not = 0
              display "Error: cannot find limits for age "
                 ws-limit-name ' ' ws-limit-cov-type ' '
                 ws-limit-issue-age
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 4                   to ws-error-sub
             move ws-limit-name        to ws-error-sup
             perform 0180-error-handle thru 0180-exit
             go to 0300-return-cics
           end-if

      *    display ' good get on limit *'  ws-limit-name '*'
      *       ws-limit-att-age '*' ws-limit-cov-type '*'
      *         ws-limit-issue-age '*' ws-limit-att-age '*'
      *         ws-limit-partial-cov '*' ws-limit-check-elig '*'
      *         ws-limit-elig-max-term '*'
      *         ws-limit-allow-truncated '*'

           move ws-limit-lo-age        to ws-lf-limit-lo-age
           move ws-limit-hi-age        to ws-lf-limit-hi-age
           move ws-limit-att-age       to ws-lf-limit-att-age
           move ws-limit-max-term      to ws-lf-limit-max-term
           move ws-limit-max-tot-ben   to ws-lf-limit-max-benefit
           move ws-limit-partial-cov   to ws-lf-limit-partial-cov
           move ws-limit-check-elig    to ws-lf-limit-check-elig
           move ws-limit-elig-max-term to ws-lf-limit-elig-max-term
           move ws-limit-allow-truncated
                                       to ws-lf-limit-allow-truncated

           if ws-lf-limit-check-elig = 0
              go to 0092-continue
           end-if

           if ws-rate-loan-term <= ws-lf-limit-elig-max-term
              continue
           else
              move 31                  to ws-error-sub
              move ' '                 to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-loan-amt <= ws-lf-limit-max-benefit
              continue
           else
              move 32                  to ws-error-sub
              move '- Lf Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           .
       0092-continue.

           move ' '                    to ws-recalc-lf
           if ws-lf-limit-allow-truncated = 0
              go to 0092-exit
           end-if

           if ws-att-age <= ws-lf-limit-att-age
              go to 0092-exit
           end-if

      *****  Calculate the date in which they will turn the
      *****  limit attained age.

           move ws-bin-pri-birth-dt to dc-bin-date-1
           if (ws-bin-cob-birth-dt < dc-bin-date-1)
              and (ws-bin-cob-birth-dt <> low-values)
              move ws-bin-cob-birth-dt to dc-bin-date-1
           end-if
           move dc-bin-date-1          to ws-test-bin-dt
           move ws-comp-bin-dt         to ws-disp-bin-dt

           compute dc-elapsed-months = (ws-lf-limit-att-age * 12)
           move '6'                    to dc-option-code
           move zeros                  to dc-elapsed-days
           PERFORM 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-limit-att-age-dt
           else
              move low-values          to ws-limit-att-age-dt
           end-if

           move dc-bin-date-2          to ws-test-bin-dt
           move ws-comp-bin-dt         to ws-disp-bin-dt

           set recalc-lf to true

           compute ws-temp-lf-term = ws-rate-lf-term -
              ((ws-att-age - ws-lf-limit-att-age) * 12)

           compute ws-rate-lf-term = ws-rate-lf-term -
              ((ws-att-age - ws-lf-limit-att-age) * 12)
           move 'T '                   to ws-rate-in-lf-ben-code

      *****  Calculate the new expiration date.

           perform 0020-calc-lf-exp
           if ws-bin-lf-exp-dt > ws-limit-att-age-dt
              subtract 1 from ws-rate-lf-term
           end-if

      *    if ws-rate-loan-amt <= ws-limit-max-tot-ben
      *       continue
      *    else
      *       move 8                   to ws-error-sub
      *       move '- Lf Benefit '     to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if
      *
      *    if ws-rate-lf-term <= ws-limit-max-term
      *       continue
      *    else
      *       move 7                   to ws-error-sub
      *       move '- Lf Term '        to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if
      *    if ws-att-age <= ws-limit-att-age
      *       continue
      *    else
      *       move 5                   to ws-error-sub
      *       move ws-att-age          to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if

           .
       0092-exit.
           exit.

       0097-get-di-limits.

           if raw-ah-ben-code (1:1) = 'A'
              move 'DI'                to ws-limit-cov-type
           else   *>  Assuming no AH coverage
              go to 0097-exit
           end-if

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

      ***  True = -1

           move 30                     to ws-limit-issue-age

           EXEC SQL
              SELECT distinct top 1
                 AttainedAge
              INTO
                 :ws-limit-att-age
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec

           if sqlcode not = 0
              display "Error: cannot find limits " ws-limit-name
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 22                  to ws-error-sub
              move ws-limit-name       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-rate-age            to ws-limit-issue-age

           EXEC SQL
              SELECT distinct top 1
                 AttainedAge,
                 MaxTerm,
                 MaxJntTerm,
                 MaxMoBen,
                 MaxTotBen,
                 PartialCoverage,
                 CheckEligibility,
                 EligibilityMaxTerm,
                 AllowTruncated
              INTO
                 :ws-limit-att-age,
                 :ws-limit-max-term,
                 :ws-limit-max-jnt-term,
                 :ws-limit-max-mo-ben,
                 :ws-limit-max-tot-ben,
                 :ws-limit-partial-cov   :nu-partial-cov,
                 :ws-limit-check-elig    :nu-check-elig,
                 :ws-limit-elig-max-term,
                 :ws-limit-allow-truncated  :nu-allow-trunc
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec

           if sqlcode not = 0
              display "Error: cannot find limits " ws-limit-name
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 4                   to ws-error-sub
              move ws-limit-name       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-limit-lo-age        to ws-di-limit-lo-age
           move ws-limit-hi-age        to ws-di-limit-hi-age
           move ws-limit-att-age       to ws-di-limit-att-age
           move ws-limit-max-term      to ws-di-limit-max-term
           move ws-limit-max-jnt-term  to ws-di-limit-max-jnt-term
           move ws-limit-max-mo-ben    to ws-di-limit-max-mo-ben
           move ws-limit-max-tot-ben   to ws-di-limit-max-tot-ben
           move ws-limit-partial-cov   to ws-di-limit-partial-cov
           move ws-limit-check-elig    to ws-di-limit-check-elig
           move ws-limit-elig-max-term to ws-di-limit-elig-max-term
           move ws-limit-allow-truncated
                                       to ws-di-limit-allow-truncated

           if ws-di-limit-check-elig = 0
              go to 0097-continue
           end-if

           if ws-rate-loan-term <= ws-di-limit-elig-max-term
              continue
           else
              move 31                  to ws-error-sub
              move ' '                 to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

      *    if (ws-rate-ah-term * ws-rate-per-pmt) <=
      *                  ws-di-limit-max-tot-ben
      *       continue
      *    else
      *       move 32                  to ws-error-sub
      *       move '- Di ToT Ben '     to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if

           .
       0097-continue.

           move ' '                    to ws-recalc-di
           if ws-di-limit-allow-truncated = 0
              go to 0097-exit
           end-if

           if ws-att-age <= ws-di-limit-att-age
              go to 0097-exit
           end-if

           compute ws-rate-ah-term = ws-rate-ah-term -
              ((ws-att-age - ws-di-limit-att-age) * 12)

           set recalc-di to true

           if (ws-rate-lf-term > zeros)
              and (ws-rate-ah-term <> ws-rate-lf-term)
              move ws-rate-lf-term     to ws-rate-ah-term
           end-if

      *    if ws-rate-per-pmt <= ws-limit-max-mo-ben
      *       continue
      *    else
      *       move 8                   to ws-error-sub
      *       move '- Ah Benefit '     to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if
      *
      *    if ws-rate-ah-term <= ws-limit-max-term
      *       continue
      *    else
      *       move 7                   to ws-error-sub
      *       move '- Ah Term '        to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if
      *
      *    if ws-att-age <= ws-limit-att-age
      *       continue
      *    else
      *       move 5                   to ws-error-sub
      *       move ws-att-age          to ws-error-sup
      *       perform 0180-error-handle
      *                                thru 0180-exit
      *       go to 0300-return-cics
      *    end-if

           .
       0097-exit.
           exit.

       0110-unstring.

      ***____________________________________________________________***
      **|                                                            |**
      **|    Unstring the raw data into data elements                |**
      **|                                                            |**
      ***____________________________________________________________***

              unstring dfhcommarea
                 delimited by '|' into
                    raw-message-num    
                    raw-state          
                    raw-acct-no        
                    raw-vin            
                    raw-lf-ben-code    
                    raw-ah-ben-code    
                    raw-earn-meth      
                    raw-pri-birth-date 
                    raw-cob-birth-date 
                    raw-loan-amt       
                    raw-eff-date       
                    raw-1st-pmt-dt     
                    raw-pmts-per-year  
                    raw-loan-term      
                    raw-lf-term        
                    raw-ah-term        
                    raw-apr            
                    raw-lf-sin-jnt-ind 
                    raw-ah-sin-jnt-ind 
                    raw-dismemberment  
                    raw-retro-elim     
                    raw-waiting-days   
                    raw-crit-per       
              end-unstring

           .
       0110-exit.
           exit.

       0120-format-message.

      ***____________________________________________________________***
      **|                                                            |**
      **|    Format each data element to be consistant with          |**
      **|  COBOL rate copybook model                                 |**
      **|                                                            |**
      ***____________________________________________________________***

           inspect
              raw-acct-no replacing leading spaces by zeros
           inspect
              raw-pmts-per-year replacing leading spaces by zeros
           inspect
              raw-loan-term replacing leading spaces by zeros
           inspect
              raw-lf-term replacing leading spaces by zeros
           inspect
              raw-ah-term replacing leading spaces by zeros
           inspect
              raw-waiting-days replacing leading spaces by zeros
           inspect
              raw-crit-per replacing leading spaces by zeros

           inspect
              raw-loan-amt replacing all spaces by zeros
           inspect
              raw-apr replacing all spaces by zeros

           move raw-loan-amt           to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-loan-amt
      *    display ' ben  in *' ws-work-in '*'
      *    display ' ben  out *' ws-work-out '*'

           move raw-loan-term          to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-loan-term
      *    display ' ln term in *' ws-work-in '*'
      *    display ' ln term out *' ws-work-out '*'

           move raw-lf-term            to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-lf-term
      *    display ' lf  term in *' ws-work-in '*'
      *    display ' lf  term out *' ws-work-out '*'

           move raw-ah-term            to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-ah-term
      *    display ' ah  term in *' ws-work-in '*'
      *    display ' ah  term out *' ws-work-out '*'

           move raw-waiting-days       to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-waiting-days
      *    display ' wait day in *' ws-work-in '*'
      *    display ' wait day out *' ws-work-out '*'

           move raw-crit-per           to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-crit-per
      *    display ' crit per in *' ws-work-in '*'
      *    display ' crit per out *' ws-work-out '*'

           move raw-pmts-per-year      to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-pmts-per-year
      *    display ' pmts per yr *' ws-work-in '*'
      *    display ' pmts per yr  *' ws-work-out '*'

           move raw-apr                to ws-work-in
           perform 0150-format-apr     thru 0150-exit
           move ws-work-out-v5         to ws-rate-apr
      *    display ' ins apr  in *' ws-work-in '*'
      *    display ' ins apr  out *' ws-work-out '*'

           move raw-state              to ws-rate-state
           move raw-acct-no            to ws-rate-acct-no
           move raw-vin                to ws-rate-vin
           move raw-lf-ben-code        to ws-rate-in-lf-ben-code
           move raw-ah-ben-code        to ws-rate-in-ah-ben-code
           move raw-earn-meth          to ws-rate-earn-meth
           move raw-pri-birth-date     to ws-rate-pri-birth-date
           move raw-cob-birth-date     to ws-rate-cob-birth-date
           move raw-eff-date           to ws-rate-eff-date
           move raw-1st-pmt-dt         to ws-rate-1st-pmt-dt
           move raw-lf-sin-jnt-ind     to ws-rate-sin-jnt-lf
           move raw-ah-sin-jnt-ind     to ws-rate-sin-jnt-ah
           move raw-dismemberment      to ws-rate-dismemberment
           move raw-retro-elim         to ws-rate-retro-elim
           if ws-rate-crit-per = zeros
              move 'Y'                 to ws-rate-fullterm
           else
              move 'N'                 to ws-rate-fullterm
           end-if

           .
       0120-exit.
           exit.

       0130-format-amt.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +9
              move ws-work-in (s2 + 1:2)
                                    to ws-work-out (9:2)
              move +8               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0130-exit.
           exit.

       0140-format-term.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) numeric)
           end-perform
           if s2 > +0
              move ws-work-in (s2:1)
                                    to ws-work-out (10:1)
              move +9               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 7)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0140-exit.
           exit.

       0150-format-apr.

           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +5
              move ws-work-in (s2 + 1:5)
                                    to ws-work-out (6:5)
              move +5               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if

           .
       0150-exit.
           exit.

       0180-error-handle.

           move ws-table-error-no (ws-error-sub)
                                       to ws-return-error-no
           move spaces                 to ws-return-error-mess
           string
              ws-table-error-mess (ws-error-sub)
              ws-error-sup
              delimited by '  ' into ws-return-error-mess
           end-string

           .
       0180-exit.
           exit.

       0190-final-limit-check.

           if ws-rate-in-lf-ben-code (1:1) = ' '
              go to 0190-final-ah
           end-if

      *    display ' rate loan amt  ' ws-rate-loan-amt
      *    display ' max lf benefit ' ws-max-lf-benefit
      *    display ' lf prem        ' ws-rate-lf-prem
      *    display ' ah prem        ' ws-rate-ah-prem
      *    display ' lf limit max   ' ws-lf-limit-max-benefit
      *    display ' att age        ' ws-att-age
      *    display ' limit att age  ' ws-lf-limit-att-age
      *    display ' rate lf term   ' ws-rate-lf-term
      *    display ' limit max trm  ' ws-lf-limit-max-term
           if ws-lf-limit-check-elig <> 0
              if (ws-rate-loan-amt +
                  ws-rate-lf-prem +
                  ws-rate-ah-prem)  <= ws-lf-limit-max-benefit
                 continue
              else
                 move 32               to ws-error-sub
                 move '- Lf Benefit '  to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           if (ws-max-lf-benefit <= ws-lf-limit-max-benefit)
              or (ws-lf-limit-max-benefit = zeros)
              continue
           else
              move 10                  to ws-error-sub
              move '- Lf Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-lf-term <= ws-lf-limit-max-term
              continue
           else
              move 9                   to ws-error-sub
              move '- Lf Term '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if (ws-att-age <= ws-lf-limit-att-age)
              or (ws-lf-limit-att-age = zeros)
              continue
           else
              move 5                   to ws-error-sub
NTTDel*       move ws-att-age          to ws-error-sup
NTTIns        move ws-att-age-x        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           .
       0190-final-ah.

           if ws-rate-in-ah-ben-code (1:1) = ' '
              go to 0190-exit
           end-if

      *    display ' made it to 0190-final-ah ' ws-di-limit-check-elig
      *    display ' rate loan amt      ' ws-rate-loan-amt
      *    display ' rate ah prem       ' ws-rate-ah-prem
      *    display ' di limit max totben' ws-di-limit-max-tot-ben
      *    display ' rate ah term       ' ws-rate-ah-term
      *    display ' rate per pmt       ' ws-rate-per-pmt
      *    display ' rate loan term     ' ws-rate-loan-term

           if ws-di-limit-check-elig <> 0
              if (ws-rate-loan-amt +
                  ws-rate-ah-prem)  <= ws-di-limit-max-tot-ben
                 continue
              else
                 move 32               to ws-error-sub
                 move '- Di Benefit '  to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
              if ws-rate-state = 'FL'
                 if ((ws-rate-loan-term * ws-rate-per-pmt) <=
                            ws-di-limit-max-tot-ben)
                    continue
                 else
                    move 32            to ws-error-sub
                    move '- Di ToT Ben ' to ws-error-sup
                    perform 0180-error-handle
                                       thru 0180-exit
                    go to 0300-return-cics
                 end-if
              end-if
           end-if

           if ws-max-ah-benefit <= ws-di-limit-max-mo-ben
              continue
           else
              move 11                  to ws-error-sub
              move '- Ah Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-sin-jnt-ah = 'J'
              if ws-rate-ah-term <= ws-di-limit-max-jnt-term
                 continue
              else
                 move 8                   to ws-error-sub
                 move '- Ah TermJNT'      to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           else
              if ws-rate-ah-term <= ws-di-limit-max-term
                 continue
              else
                 move 9                   to ws-error-sub
                 move '- Ah Term '        to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           if (ws-rate-ah-term * ws-max-ah-benefit) <=
                         ws-di-limit-max-tot-ben
              continue
           else
              move 13                  to ws-error-sub
              move '- Ah Tot Ben '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-att-age <= ws-di-limit-att-age
              continue
           else
              move 5                   to ws-error-sub
NTTDel*       move ws-att-age          to ws-error-sup
NTTIns        move ws-att-age-x        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           .
       0190-exit.
           exit.

       0200-calc-NP-payment.

           move ws-rate-loan-amt       to l
           move ws-rate-loan-term      to n
           move ws-rate-lf-term        to m
           if m = zeros
              move ws-rate-ah-term to m
           end-if
           move ws-rate-pmts-per-year  to ppy
           move 30                     to dpp
           compute i = ws-rate-apr / (ppy *100)
           if i = zeros
              move .0000001            to i
           end-if

           COMPUTE A-ANGLE-N ROUNDED =
               (1 - ((1 / (1 + I)) ** N)) / I

           COMPUTE A-ANGLE-N-M ROUNDED =
               (1 - ((1 / (1 + I)) ** (N - M))) / I

           COMPUTE GAMMA ROUNDED =
               (1 + ((D * I) / DPP)) / (1 + I)

           COMPUTE A-PRM-ANGLE-N ROUNDED = A-ANGLE-N / GAMMA
           compute ws-rate-loan-pmt rounded =
              l / a-prm-angle-n

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for TX Net Pay + 1                     -)**
      **(-   This formula is designed to use the actual MOB rate    -)**
      **(- and then applies the discount using x.                   -)**
      **(-   This formula works for net and net truncated.          -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'TX'
              go to 0200-try-oh
           end-if

           COMPUTE X ROUNDED = 1 / (1 + TX-DISCOUNT-RATE)

           COMPUTE V-TO-N ROUNDED = (1 / (1 + I)) ** N
           COMPUTE X-TO-M ROUNDED = X ** M
           COMPUTE V-TO-N-M ROUNDED = (1 / (1 + I)) ** (N - M)

           COMPUTE TEMP-VALUE-1 ROUNDED =
               (1 + I) / I

           COMPUTE TEMP-VALUE-2 ROUNDED =
               (X-TO-M - 1) / (X - 1)

           COMPUTE TEMP-VALUE-3 ROUNDED =
               ((X-TO-M * V-TO-N-M) - V-TO-N) /
               ((X * (1 + I)) - 1)

           COMPUTE TEMP-VALUE-4 ROUNDED =
               M * (AH-RATE / 100)

           COMPUTE TEMP-VALUE-5 ROUNDED =
               A-PRM-ANGLE-N * (1 + I)

           COMPUTE LIFE-FACTOR ROUNDED =
               TEMP-VALUE-1 * (TEMP-VALUE-2 - TEMP-VALUE-3) *
               (lf-rate / 1000)

           COMPUTE PMT ROUNDED =
               L / (A-PRM-ANGLE-N - LIFE-FACTOR - TEMP-VALUE-4)

           COMPUTE SINGLE-FACTOR ROUNDED =
               (LIFE-FACTOR * 100) / TEMP-VALUE-5

           .
       0200-check-tx-partial.

           if ws-lf-limit-partial-cov = 0
              display ' no partial coverage '
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
              go to 0200-carry-on-tx
           end-if

           if pmt < ws-di-limit-max-mo-ben
              move pmt                 to dispmt
           else
              move ws-di-limit-max-mo-ben
                                       to dispmt
           end-if

           if (ws-di-limit-max-tot-ben / n) < dispmt
              compute dispmt rounded =
                 (ws-di-limit-max-tot-ben / n) - .005
           end-if

           compute lifepmt1 rounded =
              (l + (single-factor * (pmt * a-prm-angle-n * (1 + 1 * i))
              / 100) + dispmt * m * ah-rate / 100) / a-prm-angle-n

           if (ws-lf-limit-max-benefit / a-prm-angle-n) < lifepmt1
              compute lifepmt1 rounded =
                 ws-lf-limit-max-benefit / a-prm-angle-n
              move ws-lf-limit-max-benefit
                                       to ws-max-lf-benefit
           end-if

           compute newpmt rounded =
              (l + (single-factor * (lifepmt1 * a-prm-angle-n *
              (1 + 1 * i)) / 100) + dispmt * m * ah-rate / 100) /
              a-prm-angle-n

           .
       0200-carry-on-tx.

           COMPUTE DL-PREMIUM ROUNDED =
               SINGLE-FACTOR *
               ((lifePMT1 * TEMP-VALUE-5) / 100)

           COMPUTE AH-PREMIUM ROUNDED =
               disPMT * TEMP-VALUE-4

      *    display 'discount       ' tx-discount-rate
      *    display ' v to n        ' v-to-n
      *    display ' x to m        ' x-to-m
      *    display ' v to n - m    ' v-to-n-m
      *    display ' anglen        ' a-angle-n
      *    display ' gamma         ' gamma
      *    display ' anglend       ' a-prm-angle-n
      *    display ' tv1           ' temp-value-1
      *    display ' tv2           ' temp-value-2
      *    display ' tv3           ' temp-value-3
      *    display ' tv4           ' temp-value-4
      *    display ' tv5           ' temp-value-5
      *    display ' lf factor     ' life-factor
      *    display ' payment       ' pmt
      *    display ' dis pmt       ' dispmt
      *    display ' lifepmt1      ' lifepmt1
      *    display ' new pmt       ' newpmt
      *    display ' single factor ' single-factor
      *    display ' lf prem       ' dl-premium
      *    display ' disab prem    ' ah-premium

           go to 0200-continue

           .
       0200-try-oh.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for OH, AK, KY, MA, MN, NH, NV         -)**
      **(-     and WA                                               -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'OH' and 'AK' and 'KY' and 'MA' and
                               'MN' and 'NH' and 'NV' and 'WA'
              go to 0200-try-al
           end-if

           evaluate true
              when ws-rate-state = 'OH' or 'AK' or 'KY' or 'NH' or
                                   'NV'
                 move 2                to plus2
              when ws-rate-state = 'MN'
                 move 1                to plus2
              when ws-rate-state = 'MA' or 'WA'
                 move 0                to plus2
              when other
                 move 1                to plus2
           end-evaluate

           COMPUTE TEMP-VALUE-1 ROUNDED =
              ((M - (A-ANGLE-N - A-ANGLE-N-M)) / I)
           compute temp-value-2 rounded = 
              (temp-value-1 * lf-rate * (1 + (plus2 * i))) / 1000
      *    COMPUTE TEMP-VALUE-2 ROUNDED =
      *       (lf-rate / 1000) * (1 + (PLUS2 * I))
           COMPUTE TEMP-VALUE-3 ROUNDED =
              M * (AH-RATE / 100)

           COMPUTE PMT ROUNDED = L /
              (A-PRM-ANGLE-N -
              TEMP-VALUE-2 - TEMP-VALUE-3)

      *    COMPUTE PMT ROUNDED = L /
      *       (A-PRM-ANGLE-N -
      *       (TEMP-VALUE-1 * TEMP-VALUE-2) - TEMP-VALUE-3)

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
           end-if

           .
       0200-carry-on-oh.

           COMPUTE DL-PREMIUM ROUNDED =
              PMT * TEMP-VALUE-2

      *    COMPUTE DL-PREMIUM ROUNDED =
      *       PMT * TEMP-VALUE-1 * TEMP-VALUE-2

           COMPUTE AH-PREMIUM ROUNDED =
              PMT * m * (ah-rate / 100)

      *    COMPUTE AH-PREMIUM ROUNDED =
      *       PMT * TEMP-VALUE-3

      *    display ' plus2      ' plus2
      *    display ' days       ' d
      *    display ' anglen     ' A-ANGLE-N
      *    display ' anglend    ' a-prm-angle-n
      *    display ' gamma      ' gamma
      *    display ' tv1        ' temp-value-1
      *    display ' tv2        ' temp-value-2
      *    display ' tv3        ' temp-value-3
      *    display ' pmt        ' pmt
      *    display ' lf prem    ' dl-premium
      *    display ' ah prem    ' ah-premium

           go to 0200-continue

           .
       0200-try-al.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for AL STD RATES.                      -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'AL'
              go to 0200-try-ME
           end-if

           COMPUTE TEMP-VALUE-1 ROUNDED =
               M - (A-ANGLE-N - A-ANGLE-N-M) + (I * M)
      
           COMPUTE TEMP-VALUE-2 ROUNDED =
               TEMP-VALUE-1 / I * lf-RATE
      
           COMPUTE TEMP-VALUE-3 ROUNDED =
               M * (AH-RATE / 100)
           COMPUTE pmt ROUNDED =
               L /
               (A-PRM-ANGLE-N - (TEMP-VALUE-2 / 1000) -
               TEMP-VALUE-3)


           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
           end-if

           .
       0200-carry-on-al.

           COMPUTE DL-PREMIUM ROUNDED =
               PMT * (TEMP-VALUE-2 / 1000)
           COMPUTE AH-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-3

           go to 0200-continue

           .
       0200-try-ME.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for ME STD RATES.                      -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'ME'
              go to 0200-try-IN
           end-if

           COMPUTE TEMP-DISCOUNT-RATE ROUNDED =
               1 / (1 + ((0.045 * N) / 24))

           COMPUTE TEMP-VALUE-1 ROUNDED =
               (N - A-ANGLE-N) / I

           COMPUTE TEMP-VALUE-2 ROUNDED =
               TEMP-VALUE-1 * (lf-rate / 1000) * TEMP-DISCOUNT-RATE

           COMPUTE TEMP-VALUE-3 ROUNDED =
               (N * AH-RATE) / 100

           COMPUTE PMT ROUNDED =
               L / (A-PRM-ANGLE-N - TEMP-VALUE-2 - TEMP-VALUE-3)

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
           end-if

           .
       0200-carry-on-me.

           COMPUTE DL-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-2

           COMPUTE AH-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-3

           go to 0200-continue

           .
       0200-try-IN.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for IN, MT, ND, NJ, AZ, RI, VT and VA  -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'IN' and 'MT' and 'ND' and 'NJ' and
                               'AZ' and 'RI' and 'VT' and 'VA'
              go to 0200-try-PA
           end-if

           evaluate true
              when ws-rate-state = 'MT' or 'AZ' or 'RI' or 'VT'
                 MOVE 2                TO PLUS2
              when ws-rate-state = 'IN' or 'ND' or 'VA'
                 MOVE 0                TO PLUS2
              when ws-rate-state = 'NJ'
                 move 1                to plus2
           end-evaluate

           COMPUTE V-TO-N ROUNDED = (1 / (1 + I)) ** N
*
           COMPUTE TEMP-DISCOUNT-RATE ROUNDED =
               ws-DISCOUNT-RATE * 12 / PPY

           COMPUTE X ROUNDED = 1 / (1 + TEMP-DISCOUNT-RATE)
           COMPUTE X-TO-N ROUNDED = X ** N

           COMPUTE TEMP-VALUE-1 ROUNDED =
               (X-TO-N - V-TO-N) / ((X * (1 + I)) - 1)
           COMPUTE TEMP-VALUE-2 ROUNDED =
               (X-TO-N - 1) / (X - 1)
           COMPUTE TEMP-VALUE-4 ROUNDED =
               TEMP-VALUE-2 - TEMP-VALUE-1
           COMPUTE TEMP-VALUE-5 ROUNDED =
               1 + ((PLUS2 * PPY / 12) * I)

           COMPUTE LIFE-FACTOR ROUNDED =
               (TEMP-VALUE-5 / I) * TEMP-VALUE-4 *
               (lf-rate / 1000)

           COMPUTE TEMP-VALUE-3 ROUNDED =
               (N * AH-RATE) / 100

           COMPUTE SINGLE-FACTOR ROUNDED =
               (LIFE-FACTOR * 100) / (A-ANGLE-N * TEMP-VALUE-5)

           COMPUTE PMT ROUNDED =
               L / (A-PRM-ANGLE-N - LIFE-FACTOR - TEMP-VALUE-3)

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
           end-if

           .
       0200-carry-on-in.

           COMPUTE DL-PREMIUM ROUNDED =
               SINGLE-FACTOR *
               ((PMT * A-ANGLE-N * TEMP-VALUE-5) / 100)

           COMPUTE AH-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-3

           go to 0200-continue

           .
       0200-try-PA.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This section is for PA.                                -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           if ws-rate-state <> 'PA'
              go to 0200-try-all-other
           end-if

           evaluate true
              when ws-rate-state = 'PA'
                 MOVE 0                TO PLUS2
              when other
                 move 1                to plus2
           end-evaluate

           COMPUTE TEMP-VALUE-1 ROUNDED =
               1 + (ws-DISCOUNT-RATE * (M / 24))

           COMPUTE TEMP-VALUE-2 ROUNDED =
               1 / (10 * TEMP-VALUE-1)

           COMPUTE TEMP-VALUE-3 ROUNDED =
               (M * AH-RATE) / 100

           COMPUTE TEMP-VALUE-4 ROUNDED =
               (M - (A-ANGLE-N - A-ANGLE-N-M)) / I

           COMPUTE TEMP-VALUE-5 ROUNDED =
               (lf-rate / 100) * (1 + (PLUS2 * ws-rate-apr / 12))

           COMPUTE TEMP-VALUE-6 ROUNDED =
               TEMP-VALUE-4 * TEMP-VALUE-2 * TEMP-VALUE-5

           COMPUTE PMT ROUNDED =
               L / (A-PRM-ANGLE-N - TEMP-VALUE-6 - TEMP-VALUE-3)

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
           end-if

           .
       0200-carry-on-PA.

           COMPUTE DL-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-6

           COMPUTE AH-PREMIUM ROUNDED =
               PMT * TEMP-VALUE-3

           go to 0200-continue

           .
       0200-try-all-other.

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   This works for all states other than listed above      -)**
      **(-  Haven't figured out all the states' extra months        -)**
      **(-  interest yet.                                           -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           evaluate true
              when ws-rate-state = 'SC'
                 move 1                to plus2
              when other
                 move 2                to plus2
           end-evaluate

           COMPUTE TEMP-VALUE-3 ROUNDED =
              M + ((D - DPP) / DPP)

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-      OK, the below formula works in conjunction with     -)**
      **(-    other than 30 days to first payment. Since Logic      -)**
      **(-    gives me the actual rate, I have to convert it to the -)**
      **(-   annual rate so it works just like QC.                  -)**
      **(-                                                          -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           compute mnthly-rate rounded = lf-rate / m
           compute mob rounded = mnthly-rate * 12

           compute mob rounded = lf-rate / (m / 12)

      *    compute mob rounded =
      *       (lf-rate / (m + (ppy/12))) * 2

           COMPUTE MOB ROUNDED =
              (mob *
              (TEMP-VALUE-3 / PPY)) / ((M + (PPY / 12)) / 2)

           COMPUTE TEMP-VALUE-1 ROUNDED =
               (M - (A-ANGLE-N - A-ANGLE-N-M)) / I

           COMPUTE TEMP-VALUE-2 ROUNDED =
               1 + (PLUS2 * I * (PPY / 12))

      *    display ' L         ' l
      *    display ' anglen       ' a-angle-n
      *    display ' days(d)      ' d
      *    display ' gamma        ' gamma
      *    display ' tv3          ' temp-value-3
      *    display 'a-prm-angle-n ' a-prm-angle-n
      *    display ' tv1          ' temp-value-1
      *    display ' mob          ' mob
      *    display ' tv2          ' temp-value-2
      *    display ' ah rate      ' ah-rate
      *    display ' m            ' m
           COMPUTE pmt ROUNDED =
               L / (A-PRM-ANGLE-N - (TEMP-VALUE-1 * (MOB / 100) *
               TEMP-VALUE-2) - ((ah-rate * M) / 100))

           .
       0200-check-partial.

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
              go to 0200-carry-on-all-other
           end-if

           if pmt < ws-di-limit-max-mo-ben
              move pmt                 to dispmt
           else
              move ws-di-limit-max-mo-ben
                                       to dispmt
           end-if

           if (ws-di-limit-max-tot-ben / m) < dispmt
              compute dispmt rounded =
                 (ws-di-limit-max-tot-ben / m) - .005
           end-if

           compute lifepmt1 rounded =
              (l + pmt * temp-value-1 * mob / 100 * temp-value-2 +
              dispmt * (ah-rate * m) / 100) / a-prm-angle-n

           if (ws-lf-limit-max-benefit / a-prm-angle-n) < lifepmt1
              compute lifepmt1 rounded =
                 ws-lf-limit-max-benefit / a-prm-angle-n
              move ws-lf-limit-max-benefit
                                       to ws-max-lf-benefit
           end-if

           compute newpmt rounded =
              (l +lifepmt1 * temp-value-1 * mob / 100 * temp-value-2 +
              dispmt * (ah-rate * m) / 100) / a-prm-angle-n

           .
       0200-carry-on-all-other.

           COMPUTE DL-PREMIUM ROUNDED = lifepmt1 *
               TEMP-VALUE-1 * (MOB / 100) * TEMP-VALUE-2

           COMPUTE AH-PREMIUM ROUNDED =
               disPmt * M * (AH-RATE / 100)
 
           move pmt                    to ws-rate-per-pmt
           move dl-premium             to ws-rate-lf-prem
           move ah-premium             to ws-rate-ah-prem
           move t-financed             to ws-rate-tot-financed
           move t-payments             to ws-rate-tot-pmts

           .
       0200-continue.

           COMPUTE T-FINANCED =
               L + DL-PREMIUM + AH-PREMIUM

           COMPUTE T-PAYMENTS = N * newPmt

           if ws-max-lf-benefit = zeros
              move t-financed          to ws-max-lf-benefit
           end-if

           move newpmt                 to ws-rate-per-pmt
           move dispmt                 to ws-max-ah-benefit
           move dl-premium             to ws-rate-lf-prem
           move ah-premium             to ws-rate-ah-prem
           move t-financed             to ws-rate-tot-financed
           move t-payments             to ws-rate-tot-pmts

      *    display ' new pmt       ' newpmt
      *    display ' dis pmt       ' dispmt
      *    display ' lifepmt1      ' lifepmt1
      *    display ' lf-rate       ' lf-rate
      *    display ' ah-rate       ' ah-rate
      *    display ' payment       ' pmt
      *    display ' lf prem       ' ws-rate-lf-prem
      *    display ' ah prem       ' ws-rate-ah-prem
      *    display ' tot financed  ' ws-rate-tot-financed
      *    display ' tot payments  ' ws-rate-tot-pmts

           .
       0200-exit.
           exit.

       0250-calc-GP-payment.

      *  States with special coding in quick-calc for Gross Pay
      *   AZ
      *   IN
      *   MT
      *   ND
      *   NJ
      *   PA

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   Right now this should work for all states other than   -)**
      **(-    those listed above.                                   -)**
      **(-                                                          -)**
      ***____________________________________________________________***

           move ws-rate-loan-amt       to l
           move ws-rate-loan-term      to n
           move ws-rate-lf-term        to m
           if m = zeros
              move ws-rate-ah-term     to m
           end-if
           move ws-rate-pmts-per-year  to ppy
           move 30                     to dpp
           compute i = ws-rate-apr / (ppy * 100)

           if i = 0
              move n                   to a-angle-n
           else
              COMPUTE A-ANGLE-N ROUNDED =
                 (1 - ((1 / (1 + I)) ** N)) / I
           end-if

           COMPUTE GAMMA ROUNDED =
               (1 + ((D * I) / DPP)) / (1 + I)

           COMPUTE A-PRM-ANGLE-N ROUNDED =
              A-ANGLE-N / GAMMA

           compute ws-rate-loan-pmt rounded =
              l / a-prm-angle-n

           COMPUTE TEMP-VALUE-1 ROUNDED =
              (N + ((D - DPP) / DPP)) / PPY

           compute mnthly-rate rounded = lf-rate / n
           compute mob rounded = mnthly-rate * 12
      *    compute mob rounded =
      *       (lf-rate / n * 12)

           COMPUTE TEMP-VALUE-2 ROUNDED = A-PRM-ANGLE-N -
              ((N * TEMP-VALUE-1 *
              (mob / 100)) +
              (N * (aH-RATE / 100)))

           COMPUTE pmt ROUNDED = L / TEMP-VALUE-2
      *    display ' anglen ' a-angle-n
      *    display ' gamma  ' gamma
      *    display ' prm anglen ' a-prm-angle-n
      *    display ' tv1        ' temp-value-1
      *    display ' mob        ' mob
      *    display ' tv2        ' temp-value-2
      *    display ' pmt        ' pmt

           .
       0250-check-for-partial.

           if ws-lf-limit-partial-cov = 0
              move pmt                 to dispmt
                                          lifepmt1
                                          lifepmt2
                                          newpmt
              go to 0250-carry-on
           end-if

           if pmt < ws-di-limit-max-mo-ben
              move pmt                 to dispmt
           else
              move ws-di-limit-max-mo-ben
                                       to dispmt
           end-if

           if (ws-di-limit-max-tot-ben / n) < dispmt
              compute dispmt rounded =
                 (ws-di-limit-max-tot-ben / n) - .005
           end-if

           compute lifepmt1 rounded =
              (l + (pmt * n * (n + (d - dpp) / dpp) / ppy * mob /
                 100) + dispmt * (n * ah-rate / 100)) / A-PRM-ANGLE-N

           if (ws-lf-limit-max-benefit / n) < lifepmt1
              compute lifepmt1 rounded =
                 ws-lf-limit-max-benefit / n
              move ws-lf-limit-max-benefit
                                       to ws-max-lf-benefit
           end-if

           compute newpmt rounded =
              (l + (lifepmt1 * n * (n + (d - dpp) / dpp) / ppy *
                 mob / 100) + dispmt * (n * ah-rate / 100)) /
                 A-PRM-ANGLE-N

           .
       0250-carry-on.

           COMPUTE DL-PREMIUM ROUNDED =
              lifepmt1 * N * TEMP-VALUE-1 *
              (mob / 100)

           COMPUTE AH-PREMIUM ROUNDED =
              dispmt * m * (AH-RATE / 100)

      *    display ' L         ' l
      *    display ' anglen       ' a-angle-n
      *    display ' days(d)      ' d
      *    display ' gamma        ' gamma
      *    display 'a-prm-angle-n ' a-prm-angle-n
      *    display ' tv1          ' temp-value-1
      *    display ' mob          ' mob
      *    display ' tv2          ' temp-value-2
      *    display ' ah rate      ' ah-rate
      *    display ' m            ' m
 
           COMPUTE T-FINANCED =
               L + DL-PREMIUM + AH-PREMIUM

           COMPUTE T-PAYMENTS = N * newPmt

           if i = 0
              move t-financed          to t-payments
           end-if

           if ws-max-lf-benefit = zeros
              move t-payments          to ws-max-lf-benefit
           end-if

           move newpmt                 to ws-rate-per-pmt
           move dispmt                 to ws-max-ah-benefit
           move dl-premium             to ws-rate-lf-prem
           move ah-premium             to ws-rate-ah-prem
           move t-financed             to ws-rate-tot-financed
           move t-payments             to ws-rate-tot-pmts


      *    display ' lf-rate       ' lf-rate
      *    display ' ah-rate       ' ah-rate
      *    display ' payment       ' pmt
      *    display ' dis pmt       ' dispmt
      *    display ' lifepmt1      ' lifepmt1
      *    display ' new pmt       ' newpmt
      *    display ' lf prem       ' ws-rate-lf-prem
      *    display ' ah prem       ' ws-rate-ah-prem
      *    display ' tot financed  ' ws-rate-tot-financed
      *    display ' tot payments  ' ws-rate-tot-pmts

           go to 0250-exit

           .
       xxxx-code-that-works.  *>!!!!!!

      ***____________________________________________________________***
      **(-                                                          -)**
      **(-   Right now this should work for all states other than   -)**
      **(-    listed above. Below worked prior to adding partial    -)**
      **(-    coverage stuff.                                       -)**
      ***____________________________________________________________***

           move ws-rate-loan-amt       to l
           move ws-rate-loan-term      to n
           move ws-rate-lf-term        to m
           move ws-rate-pmts-per-year  to ppy
           move 30                     to dpp
           compute i = ws-rate-apr / (ppy * 100)

           if i = 0
              move n                   to a-angle-n
           else
              COMPUTE A-ANGLE-N ROUNDED =
                 (1 - ((1 / (1 + I)) ** N)) / I
           end-if

           if i = 0
              compute a-angle-n-m = n - m
           else
              COMPUTE A-ANGLE-N-M ROUNDED =
                 (1 - ((1 / (1 + I)) ** (N - M))) / I
           end-if

           COMPUTE GAMMA ROUNDED =
               (1 + ((D * I) / DPP)) / (1 + I)

           COMPUTE A-PRM-ANGLE-N ROUNDED =
              A-ANGLE-N / GAMMA

           COMPUTE TEMP-VALUE-1 ROUNDED =
              (N + ((D - DPP) / DPP)) / PPY

           compute mob rounded =
              (lf-rate / n * 12)

           COMPUTE TEMP-VALUE-2 ROUNDED = A-PRM-ANGLE-N -
              ((N * TEMP-VALUE-1 *
              (mob / 100)) +
              (N * (aH-RATE / 100)))

           COMPUTE pmt ROUNDED = L / TEMP-VALUE-2

           COMPUTE DL-PREMIUM ROUNDED =
              pmt * N * TEMP-VALUE-1 *
              (mob / 100)


           COMPUTE AH-PREMIUM ROUNDED =
              pmt * N * (AH-RATE / 100)

      *    display ' L         ' l
      *    display ' anglen       ' a-angle-n
      *    display ' days(d)      ' d
      *    display ' gamma        ' gamma
      *    display 'a-prm-angle-n ' a-prm-angle-n
      *    display ' tv1          ' temp-value-1
      *    display ' mob          ' mob
      *    display ' tv2          ' temp-value-2
      *    display ' ah rate      ' ah-rate
      *    display ' m            ' m
 
           COMPUTE T-FINANCED =
               L + DL-PREMIUM + AH-PREMIUM

           COMPUTE T-PAYMENTS = N * Pmt

           move pmt                    to ws-rate-per-pmt
           move dl-premium             to ws-rate-lf-prem
           move ah-premium             to ws-rate-ah-prem
           move t-financed             to ws-rate-tot-financed
           move t-payments             to ws-rate-tot-pmts

      *    display ' lf-rate       ' lf-rate
      *    display ' ah-rate       ' ah-rate
      *    display ' payment       ' pmt
      *    display ' lf prem       ' ws-rate-lf-prem
      *    display ' ah prem       ' ws-rate-ah-prem
      *    display ' tot financed  ' ws-rate-tot-financed
      *    display ' tot payments  ' ws-rate-tot-pmts

           .
       0250-exit.
           exit.

       0300-RETURN-CICS.

           perform 0400-disconnect     thru 0400-exit
           move ws-return-string       to dfhcommarea
           exec cics return end-exec
           goback

           .
       0300-exit.
           exit.

       0400-disconnect.

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' ' to ws-connect-sw
           end-if

           .
       0400-exit.
           exit.

       6000-CONNECT-TO-DB.
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate


           move 'CertManagement'       to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

      *
      *     EXEC SQL
NTTDel**       CONNECT TO :svr USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
              display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if

           .
       6000-EXIT.
           EXIT.

       7000-GET-RATE.

           move all '9'                to errate-key
       
           MOVE CP-COMPANY-CD          TO RATE-COMPANY-CD
           MOVE CP-STATE               TO RATE-ST-CODE
           MOVE CP-CLASS-CODE          TO RATE-ST-CLASS
           MOVE CP-DEVIATION-CODE      TO RATE-ST-DEV
           MOVE CP-ISSUE-AGE           TO RATE-HIGH-AGE

           IF CP-RATING-BENEFIT-AMT NOT NUMERIC OR
              CP-RATING-BENEFIT-AMT = ZEROS
              MOVE CP-ORIGINAL-BENEFIT TO RATE-HIGH-AMT
           ELSE
              MOVE CP-RATING-BENEFIT-AMT
                                       TO RATE-HIGH-AMT
           end-if

           IF CP-AH
               MOVE CP-AH-OVERRIDE-CODE
                                       TO RATE-L-AH
           ELSE
               MOVE CP-LIFE-OVERRIDE-CODE
                                       TO RATE-L-AH
           end-if

           MOVE CP-BENEFIT-CD          TO RATE-LAH-NUM
           MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACE                  TO DC-OPTION-CODE
     
           PERFORM 9700-date-link      thru 9700-exit
     
           IF NO-CONVERSION-ERROR
              move errate-key          to save-errate-key
              MOVE dc-greg-date-cymd   TO RATE-EXPIRY-DATE
                                          svrt-expiry-date
           ELSE
              MOVE '2'                 TO CP-RETURN-CODE
              GO TO 7000-exit
           END-IF

           EXEC CICS STARTBR
              DATASET  ('ERRATE')
              RIDFLD   (ERRATE-KEY)
              RESP     (ws-response)
              GTEQ
           END-EXEC

           if not resp-normal
              move '6'                 to cp-return-code
              go to 7000-exit
           end-if

           .
       7000-read-loop.

           EXEC CICS READNEXT
              DATASET  ('ERRATE')
              into     (RATE-RECORD)
              RESP     (ws-response)
              RIDFLD   (ERRATE-KEY)
           END-EXEC

           if not resp-normal
              move '6'                 to cp-return-code
              go to 7000-done
           end-if

           IF SVRT-COMPANY-CD = RT-COMPANY-CD  AND
              SVRT-ST-CODE    = RT-ST-CODE     AND
              SVRT-ST-CLASS   = RT-ST-CLASS    AND
              SVRT-ST-DEV     = RT-ST-DEV      AND
              SVRT-L-AH       = RT-L-AH        AND
              SVRT-LAH-NUM    = RT-LAH-NUM
              continue
           ELSE
              move '6'                 to cp-return-code
              go to 7000-done
           end-if

           IF SVRT-HIGH-AGE > RT-HIGH-AGE
              move '6'                 to cp-return-code
              go to 7000-done
           end-if

           IF SVRT-HIGH-AMT >= RT-HIGH-AMT
              GO TO 7000-read-loop
           end-if
     
           IF SVRT-EXPIRY-DATE > RT-EXPIRY-DATE
              GO TO 7000-read-loop
           end-if
     
           IF SVRT-EXPIRY-DATE = RT-EXPIRY-DATE
              GO TO 7000-read-loop
           end-if

           .
       7000-done.

           EXEC CICS ENDBR
              DATASET  ('ERRATE')
           END-EXEC

           .
       7000-EXIT.
           EXIT.

       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

