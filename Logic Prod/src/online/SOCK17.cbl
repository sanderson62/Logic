       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK17.
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
      ******************************************************************
      *REMARKS.                                                        *
      *        Reads the pending issue record passed to it.            *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111020 CR2020061000002   PEMA  New Program
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK17   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77 ws-send-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(19200) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  WS-ERALPH-SW                PIC X VALUE SPACES.
           88  END-OF-ERALPH                VALUE 'Y'.
       77  WS-ERPNDB5-SW               PIC X VALUE SPACES.
           88  END-OF-ERPNDB5               VALUE 'Y'.
       77  ERPNDB-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  WS-PREV-PNDB-KEY            PIC X(23) VALUE LOW-VALUES.
       77  c1                          pic s999 value +0 comp-3.
       77  a1                          pic s999 value +0 comp-3.
       77  p1                          pic s999 value +0 comp-3.
       77  n1                          pic s999 value +0 comp-3.
       77  t1                          pic s999 value +0 comp-3.
       77  ws-match-sw                 pic x value spaces.
           88  no-matching-alpha           value 'N'.
       77  ws-save-issue-key           pic x(36)  value spaces.
       77  ws-last-name                pic x(15)  value spaces.
       77  ws-elcert-last-name         pic x(15)  value spaces.
       77  ws-first-three              pic xxx  value spaces.
       77  ws-age                      pic 999 value zeros.
       77  ws-cov-sw                   pic x value spaces.
           88  processing-primary         value 'P'.
           88  processing-secondary        value 'S'.
       77  ws-erpndb2-startbr-sw       pic x  value spaces.
           88  erpndb2-startbr           value 'Y'.
       77  ws-eralph2-startbr-sw       pic x  value spaces.
           88  eralph2-startbr           value 'Y'.
       77  ws-pend-match               pic x value ' '.
           88  pend-match                 value 'Y'.
       77  ws-elmstr5-startbr-sw       pic x  value spaces.
           88  elmstr5-startbr           value 'Y'.
       77  save-bin-date               pic xx value low-values.
       77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
       77  note-count                  pic s999 comp-3 value +0.
       77  ws-build-note-sw            pic x value ' '.
           88  finished-with-notes      value 'Y'.
       77  ws-ercnot-sw                pic x  value spaces.
           88  ercnot-startbr            value 'Y'.
       77  ws-current-bin-dt           pic xx value low-values.
       77  ws-work-age                 pic s999 comp-3 value zeros.
       77  ws-cm-pri-curr-age          pic s999 comp-3 value +0.
       77  ws-cm-cob-curr-age          pic s999 comp-3 value +0.
       77  ws-p5-pri-curr-age          pic s999 comp-3 value +0.
       77  ws-p5-cob-curr-age          pic s999 comp-3 value +0.
       77  ws-alph-curr-age            pic s999 comp-3 value +0.
       01  cert-note-records-holder.
           05  cert-note-record occurs 300.
               10  filler              pic x(48).
               10  cnr-rest            pic x(102).
      ******************************************************************
       01  ws-hold-elcert              pic x(450) value spaces.
       01  ws-return-stuff.
           05  ws-eralph-table occurs 25.
               10  ws-cov-type         pic x.
               10  ws-tbl-last-name    pic x(15).
               10  ws-first-name       pic x(10).
               10  ws-eff-dt           pic x(10).
               10  ws-eralph-cert      pic x(11).
               10  ws-eralph-age       pic 99.
               10  ws-eralph-prm-sec   pic x.
               10  ws-eralph-lf-cd     pic xx.
               10  ws-eralph-lf-remamt pic 9(9).99.
               10  ws-eralph-lf-remterm pic 999.
               10  ws-lf-exp-dt        pic x(10).
               10  ws-eralph-ah-cd     pic xx.
               10  ws-eralph-ah-amt    pic 9(7).99.
               10  ws-eralph-ah-remamt pic 9(9).99.
               10  ws-eralph-ah-remterm pic 999.
               10  ws-ah-exp-dt        pic x(10).
               10  ws-pend             pic x.
               10  ws-claim            pic x.
               10  ws-delimiter        pic x.
       01  ws-alpha-table.
           05  ws-eralph-print-table occurs 25.
               10  pt-eralph-cert      pic x(11).
               10  pt-eralph-lf-cd     pic xx.
               10  pt-eralph-lf-remamt pic -zzz,zz9,999.99.
               10  pt-eralph-ah-cd     pic xx.
               10  pt-eralph-ah-amt    pic -z,zzz,z99.99.
               10  pt-eralph-ah-remamt pic -zzz,zzz,999.99.
       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.
       01  ws-alpha-last-name          pic x(15) value spaces.
       01  ws-p5-last-name             pic x(15) value spaces.
       01  ws-p5-jnt-last-name         pic x(15) value spaces.
       01  ws-name-in                  pic x(15) value spaces.
       01  ws-name-out                 pic x(15) value spaces.
       01  ws-age-from-pndb            pic 99.
       01  ws-lf-tot-benefit           pic s9(9)v99 comp-3 value +0.
       01  ws-ah-tot-benefit           pic s9(9)v99 comp-3 value +0.
       01  pt-lf-tot-benefit           pic -zzz,zzz,999.99.
       01  pt-ah-tot-benefit           pic -zzz,zzz,999.99.
       01  WS-ERPNDB-REC-HOLD          PIC X(585) VALUE SPACES.
       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +515.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERPNDB5-FILE-STATUS     PIC XX    VALUE ZEROS.
           05  ERALPH-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELMSTR5-FILE-STATUS     PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  lf-joint-ind            pic x   value spaces.
           05  ah-joint-ind            pic x   value spaces.
           05  ws-lf-ben-code          pic xx.
           05  ws-ah-ben-code          pic xx.
           05  ws-lf-calc-cd           pic x   value spaces.
           05  ws-lf-coverage-type     pic x   value spaces.
           05  ws-lf-earnings-calc     pic x   value spaces.
           05  ws-lf-kind              pic x   value spaces.
           05  ws-lf-ben-descrip       pic x(10) value spaces.
           05  ws-ah-calc-cd           pic x   value spaces.
           05  ws-ah-coverage-type     pic x   value spaces.
           05  ws-ah-earnings-calc     pic x   value spaces.
           05  ws-ah-kind              pic x   value spaces.
           05  ws-ah-ben-descrip       pic x(10) value spaces.
       01  ws-elcntl-key.
           05  ws-elcntl-company-id    pic xxx.
           05  ws-elcntl-rec-type      pic x.
               88  ws-elcntl-lf-ben-cd    value '4'.
               88  ws-elcntl-ah-ben-cd    value '5'.
           05  filler                  pic xx.
           05  ws-elcntl-hi-ben-cd     pic xx.
           05  ws-elcntl-seq-no        pic s9(4) comp.
       01  ws-erpndb-key.
           05  ws-erpndb-company-cd    pic x.
           05  ws-erpndb-batch         pic x(6).
           05  ws-erpndb-seq-no        pic s9(4) comp.
           05  ws-erpndb-chg-seq-no    pic s9(4) comp.
       01  ws-elcert2-orig-key        pic x(18).
       01  ws-elcert-orig-key         pic x(33).
       01  ws-elcert-key.
           05  ws-elcert-company-cd   pic x.
           05  ws-elcert-carrier      pic x.
           05  ws-elcert-grouping     pic x(6).
           05  ws-elcert-state        pic xx.
           05  ws-elcert-account      pic x(10).
           05  ws-elcert-eff-dt       pic xx.
           05  ws-elcert-cert-no      pic x(11).
       01  ws-elcert2-key.
           05  ws-elcert2-company-cd  pic x.
           05  ws-elcert2-last-name   pic x(15).
           05  ws-elcert2-initials    pic xx.
       01  WS-CZ-KEY.
           05  WS-CZ-COMPANY-CD        PIC X.
           05  WS-CZ-CARRIER           PIC X.
           05  WS-CZ-GROUP             PIC X(6).
           05  WS-CZ-STATE             PIC XX.
           05  WS-CZ-ACCOUNT           PIC X(10).
           05  WS-CZ-EFF-DT            PIC XX.
           05  WS-CZ-CERT-NO           PIC X(11).
           05  WS-CZ-REC-TYPE          PIC X.
           05  ws-cz-note-seq          pic s9(4) comp.
       01  ws-erpndb2-key.
           05  ws-erpndb2-company-cd   pic x.
           05  ws-erpndb2-carrier      pic x.
           05  ws-erpndb2-grouping     pic x(6).
           05  ws-erpndb2-state        pic xx.
           05  ws-erpndb2-account      pic x(10).
           05  ws-erpndb2-eff-dt       pic xx.
           05  ws-erpndb2-cert-no      pic x(11).
           05  ws-erpndb2-seq-no       pic s9(4) comp.
           05  ws-erpndb2-rec-type     pic x.
       01  ws-erpndb5-key.
           05  ws-erpndb5-company-cd   pic x.
           05  ws-erpndb5-carrier      pic x.
           05  ws-erpndb5-grouping     pic x(6).
           05  ws-erpndb5-state        pic xx.
           05  ws-erpndb5-account      pic x(10).
           05  ws-erpndb5-eff-dt       pic xx.
           05  ws-erpndb5-cert-no      pic x(11).
           05  ws-erpndb5-seq-no       pic s9(4) comp.
           05  ws-erpndb5-rec-type     pic x.
       01  ws-elmstr5-key.
           05  ws-elmstr5-company-cd  pic x.
           05  ws-elmstr5-cert-no     pic x(11).
       01  ws-eralph-aix-key.
           05  ws-eralph-aix-company-cd pic x.
           05  ws-eralph-aix-account   pic x(10).
           05  ws-eralph-aix-lname     pic x(15).
           05  ws-eralph-aix-1st-three pic xxx.
           05  filler                  pic x(8).
       01  ws-passed-key-area.
           05  ws-passed-pend-key.
               10  ws-pass-company-cd  pic x.
               10  ws-pass-carrier     pic x.
               10  ws-pass-grouping    pic x(6).
               10  ws-pass-state       pic xx.
               10  ws-pass-account     pic x(10).
               10  ws-pass-eff-dt      pic xx.
               10  ws-pass-cert-no     pic x(11).
               10  ws-pass-seq-no      pic s9(4) comp.
               10  ws-pass-rec-type    pic x.
           05  ws-return-area.
               10  ws-exceed-limit-yn  pic x.
               10  ws-has-claim-yn     pic x.
           05  filler                  pic x(62).
      *                                copy ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
      *                                copy ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
      *                                copy ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      *                                copy ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
      *                                copy ERCALPH.
      ******************************************************************
      *                                                                *
      *                            ERCALPH.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ALPHA FILE                                *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 215  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERALPH                         RKP=2,LEN=34   *
      *       ALTERNATE PATH1 = ERALPH2 (FULL CONTROL)  RKP=36,LEN=37  *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
050815*                   C H A N G E   L O G
050815*
050815* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
050815*-----------------------------------------------------------------
050815*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
050815* EFFECTIVE    NUMBER
050815*-----------------------------------------------------------------
050815* 102004    2015022600002  PEMA  NEW FILE AND COPYBOOK
050815******************************************************************
       01  ALPHA-FILE-REC.
           12  AF-RECORD-ID                      PIC XX.
               88  VALID-AF-ID                      VALUE 'AF'.
           12  AF-CONTROL-PRIMARY.
               16  AF-COMPANY-CD                 PIC X.
               16  AF-CARRIER                    PIC X.
               16  AF-GROUPING.
                   20  AF-GRP-PREFIX             PIC XXX.
                   20  AF-GRP-PRIME              PIC XXX.
               16  AF-STATE                      PIC XX.
               16  AF-ACCOUNT.
                   20  AF-ACCT-PREFIX            PIC X(4).
                   20  AF-ACCT-PRIME             PIC X(6).
               16  AF-DT                         PIC XX.
               16  AF-CERT-NO.
                   20  AF-CERT.
                       24  AF-CERT-PREFIX        PIC XXX.
                       24  AF-CERT-PRIME         PIC X(7).
                   20  AF-CERT-SUFFIX            PIC X.
               16  AF-ALPHA-TYPE-CODE            PIC X.
                   88  AF-INSURED-ALPHA             VALUE 'I'.
                   88  AF-JOINT-ALPHA               VALUE 'J'.
           12  AF-CONTROL-BY-ACCT-NAME.
               16  AF-COMPANY-CD-A1              PIC X.
               16  AF-ACCOUNT-A1                 PIC X(10).
               16  AF-NAME.
                   20  AF-LNAME                  PIC X(15).
                   20  AF-FNAME.
                       24  AF-1ST-INIT-FNAME     PIC X.
                       24  FILLER                PIC X(9).
                   20  AF-INIT                   PIC X.
           12  AF-INSURED-INFO.
               16  AF-AGE                        PIC 99.
               16  AF-SEX                        PIC X.
               16  AF-LIFE-DATA.
                   20  AF-LF-TYP                 PIC XX.
                   20  AF-LF-TERM                PIC S999       COMP-3.
                   20  AF-LF-REMTERM             PIC S999       COMP-3.
                   20  AF-LF-AMT                 PIC S9(9)V99   COMP-3.
                   20  AF-LF-REMAMT              PIC S9(9)V99   COMP-3.
                   20  AF-LF-PRM                 PIC S9(7)V99   COMP-3.
                   20  AF-LF-AMT-ALT             PIC S9(9)V99   COMP-3.
                   20  AF-LF-REMAMT-ALT          PIC S9(9)V99   COMP-3.
                   20  AF-LF-PRM-ALT             PIC S9(7)V99   COMP-3.
               16  AF-AH-DATA.
                   20  AF-AH-TYP                 PIC XX.
                   20  AF-AH-TERM                PIC S999       COMP-3.
                   20  AF-AH-REMTERM             PIC S999       COMP-3.
                   20  AF-AH-AMT                 PIC S9(7)V99   COMP-3.
                   20  AF-AH-REMAMT              PIC S9(7)V99   COMP-3.
                   20  AF-AH-PRM                 PIC S9(7)V99   COMP-3.
               16  AF-APR                        PIC S999V9(4)  COMP-3.
               16  AF-IND-GRP                    PIC X.
               16  AF-SOC-NO                     PIC X(11).
               16  AF-LF-STATUS                  PIC X.
               16  AF-LF-PRE-PLST                PIC X.
               16  AF-LF-CNCL                    PIC XX.
               16  AF-DEATH                      PIC XX.
               16  AF-LF-EXIT                    PIC XX.
               16  AF-LF-EXPIRES                 PIC XX.
               16  AF-AH-STATUS                  PIC X.
               16  AF-AH-PRE-PLST                PIC X.
               16  AF-AH-CNCL                    PIC XX.
               16  AF-LUMP-SUM                   PIC XX.
               16  AF-AH-EXIT                    PIC XX.
               16  AF-AH-EXPIRES                 PIC XX.
               16  AF-ENTRY                      PIC XX.
               16  AF-ENTRY-STATUS               PIC X.
               16  FILLER                        PIC X(39).
      ******************************************************************
      *                                COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
012220* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
012220         16  PB-I-LETTER-REQD             PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
                   88  PB-CASH-CERT                 VALUE 'C'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
      *                                COPY ERCPNDB
      *     REPLACING LEADING ==PB== BY ==P5==
      *     PENDING-BUSINESS BY P5-PENDING-BUSINESS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
012220* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
122002******************************************************************
00032
00033  01  P5-PENDING-BUSINESS.
00034      12  P5-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  P5-CONTROL-PRIMARY.
00038          16  P5-COMPANY-CD                PIC X.
00039          16  P5-ENTRY-BATCH               PIC X(6).
00040          16  P5-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  P5-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  P5-CONTROL-BY-ACCOUNT.
00044          16  P5-COMPANY-CD-A1             PIC X.
00045          16  P5-CARRIER                   PIC X.
00046          16  P5-GROUPING.
00047              20  P5-GROUPING-PREFIX       PIC XXX.
00048              20  P5-GROUPING-PRIME        PIC XXX.
00049          16  P5-STATE                     PIC XX.
00050          16  P5-ACCOUNT.
00051              20  P5-ACCOUNT-PREFIX        PIC X(4).
00052              20  P5-ACCOUNT-PRIME         PIC X(6).
00053          16  P5-CERT-EFF-DT               PIC XX.
00054          16  P5-CERT-NO.
00055              20  P5-CERT-PRIME            PIC X(10).
00056              20  P5-CERT-SFX              PIC X.
00057          16  P5-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  P5-RECORD-TYPE               PIC X.
00060              88  P5-MAILING-DATA                VALUE '0'.
00061              88  P5-ISSUE                       VALUE '1'.
00062              88  P5-CANCELLATION                VALUE '2'.
00063              88  P5-BATCH-TRAILER               VALUE '9'.
00064
00065      12  P5-CONTROL-BY-ORIG-BATCH.
00066          16  P5-ORIGINAL-COMPANY-CD       PIC X.
00067          16  P5-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  P5-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  P5-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  P5-CONTROL-BY-CSR.
00072          16  P5-CSR-COMPANY-CD            PIC X.
00073          16  P5-CSR-ID                    PIC X(4).
00074          16  P5-CSR-ENTRY-BATCH           PIC X(6).
00075          16  P5-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  P5-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  P5-LAST-MAINT-DT                 PIC XX.
00082      12  P5-LAST-MAINT-BY                 PIC X(4).
00083      12  P5-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  P5-RECORD-BODY                   PIC X(375).
00086
00087      12  P5-ISSUE-RECORD   REDEFINES P5-RECORD-BODY.
00088          16  P5-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  P5-I-NAME.
00091              20  P5-I-INSURED-LAST-NAME   PIC X(15).
00092              20  P5-I-INSURED-FIRST-NAME.
00093                  24  P5-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  P5-I-INSURED-MIDDLE-INIT PIC X.
00096          16  P5-I-AGE                     PIC S99   COMP-3.
00097          16  P5-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  P5-I-BIRTHDAY                PIC XX.
00099          16  P5-I-INSURED-SEX             PIC X.
00100              88  P5-SEX-MALE     VALUE 'M'.
00101              88  P5-SEX-FEMALE   VALUE 'F'.
00102
00103          16  P5-I-LF-TERM                 PIC S999   COMP-3.
00104          16  P5-I-AH-TERM                 PIC S999   COMP-3.
00105          16  P5-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  P5-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  P5-I-SKIP-CODE               PIC X.
00108              88  P5-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  P5-SKIP-JULY              VALUE '1'.
00110              88  P5-SKIP-AUGUST            VALUE '2'.
00111              88  P5-SKIP-SEPTEMBER         VALUE '3'.
00112              88  P5-SKIP-JULY-AUG          VALUE '4'.
00113              88  P5-SKIP-AUG-SEPT          VALUE '5'.
00114              88  P5-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  P5-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  P5-SKIP-JUNE              VALUE '8'.
00117              88  P5-SKIP-JUNE-JULY         VALUE '9'.
00118              88  P5-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  P5-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  P5-I-TERM-TYPE               PIC X.
00121              88  P5-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  P5-PAID-WEEKLY            VALUE 'W'.
00123              88  P5-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  P5-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  P5-PAID-13-YEARLY         VALUE 'T'.
00126          16  P5-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  P5-I-POLICY-FORM-NO          PIC X(12).
00128          16  P5-I-DATA-ENTRY-SW           PIC X.
00129              88  P5-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  P5-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  P5-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  P5-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  P5-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  P5-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  P5-I-AH-CLP                  PIC S9(5)V99 COMP-3.
012220         16  P5-I-LETTER-REQD             PIC X.
00136
00137          16  P5-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  P5-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  P5-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  P5-I-LF-BENEFIT-CD   REDEFINES P5-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  P5-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  P5-I-AMOUNT-FINANCED REDEFINES
100703                  P5-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  P5-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  P5-I-UNPAID-CASH-PRICE REDEFINES
100703                  P5-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  P5-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  P5-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  P5-I-CLP-AMOUNT REDEFINES
100703                  P5-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  P5-I-LF-CALC-FLAG            PIC X.
00148              88 P5-COMP-LF-PREM               VALUE '?'.
00149          16  P5-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  P5-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  P5-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  P5-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  P5-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  P5-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  P5-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  P5-I-LF-ABBR                 PIC XXX.
00157          16  P5-I-LF-INPUT-CD             PIC XX.
00158
00159          16  P5-I-AH-BENEFIT-CD           PIC XX.
00160              88  P5-VALID-AH                 VALUE '01' THRU '89'.
00161              88  P5-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  P5-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  P5-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  P5-I-AH-CALC-FLAG            PIC X.
00166              88 P5-COMP-AH-PREM                  VALUE '?'.
00167          16  P5-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  P5-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  P5-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  P5-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  P5-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  P5-I-AH-ABBR                 PIC XXX.
00173          16  P5-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  P5-I-SPECIAL-REIN-CODE       PIC X.
00176          16  P5-I-REIN-TABLE              PIC XXX.
00177          16  P5-I-BUSINESS-TYPE           PIC 99.
00178          16  P5-I-INDV-GRP-CD             PIC X.
00179          16  P5-I-MORT-CODE.
00180              20  P5-I-TABLE               PIC X.
00181              20  P5-I-INTEREST            PIC XX.
00182              20  P5-I-MORT-TYP            PIC X.
00183          16  P5-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  P5-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  P5-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  P5-I-INDV-GRP-OVRD           PIC X.
00187          16  P5-I-RATE-CLASS-OVRD         PIC XX.
00188          16  P5-I-SIG-SW                  PIC X.
00189              88  P5-POLICY-SIGNED             VALUE 'Y'.
00190          16  P5-I-RATE-CLASS              PIC XX.
00191          16  P5-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  P5-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  P5-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  P5-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  P5-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  P5-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  P5-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  P5-I-BENEFIT-TYPE            PIC XXX.
00199          16  P5-I-OB-FLAG                 PIC X.
00200              88  P5-I-OB                      VALUE 'B'.
00201              88  P5-I-SUMMARY                 VALUE 'Z'.
00202          16  P5-I-ENTRY-STATUS            PIC X.
00203              88  P5-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  P5-I-NORMAL-ENTRY            VALUE '1'.
00206              88  P5-I-POLICY-PENDING          VALUE '2'.
00207              88  P5-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  P5-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  P5-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  P5-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  P5-I-REIN-ONLY               VALUE '9'.
00210              88  P5-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  P5-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  P5-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  P5-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  P5-I-INT-CODE                PIC X.
00215              88  P5-ADD-ON-INTEREST           VALUE 'A'.
00216              88  P5-SIMPLE-INTEREST           VALUE 'S'.
00217          16  P5-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  P5-I-SOC-SEC-NO              PIC X(11).
00219          16  P5-I-MEMBER-NO               PIC X(12).
00220          16  P5-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  P5-I-OLD-LOF                 PIC XXX.
00222          16  P5-I-LF-EXPIRE-DT            PIC XX.
00223          16  P5-I-AH-EXPIRE-DT            PIC XX.
00224          16  P5-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  P5-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  P5-I-LIFE-INDICATOR          PIC X.
00227              88  P5-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  P5-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  P5-I-DDF-IU-RATE-UP REDEFINES P5-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  P5-I-MAIL-ADDRS-SW           PIC X.
00230              88 P5-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 P5-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  P5-I-1ST-PMT-DT              PIC XX.
00233          16  P5-I-JOINT-INSURED.
00234              20 P5-I-JOINT-LAST-NAME      PIC X(15).
00235              20 P5-I-JOINT-FIRST-NAME.
00236                 24  P5-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 P5-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  P5-I-BENEFICIARY-NAME.
100703             20  P5-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  P5-I-LAST-ADD-ON-DT          PIC XX.
011904         16  P5-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES P5-I-REFERENCE.
011904             20  P5-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  P5-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  P5-I-CLP-STATE           PIC XX.
00242          16  P5-I-UNDERWRITING-STATUS     PIC X.
00243              88  P5-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  P5-I-POLICY-DECLINED         VALUE 'D'.
00245              88  P5-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  P5-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  P5-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  P5-I-RESIDENT-STATE          PIC XX.
00249          16  P5-I-RATE-CODE               PIC X(4).
00250          16  P5-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  P5-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  P5-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  P5-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  P5-I-BANK-NOCHRGB            PIC 99.
040504         16  P5-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  P5-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  P5-CANCEL-RECORD   REDEFINES P5-RECORD-BODY.
00254          16  P5-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  P5-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  P5-C-CANCEL-ORIGIN           PIC X.
00257              88  P5-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  P5-C-LF-CANCEL-DT            PIC XX.
00259          16  P5-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  P5-C-LF-CALC-REQ             PIC X.
00261              88 P5-COMP-LF-CANCEL            VALUE '?'.
00262          16  P5-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  P5-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  P5-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  P5-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  P5-C-AH-CANCEL-DT            PIC XX.
00267          16  P5-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  P5-C-AH-CALC-REQ             PIC X.
00269              88 P5-COMP-AH-CANCEL            VALUE '?'.
00270          16  P5-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  P5-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  P5-C-LAST-NAME               PIC X(15).
00273          16  P5-C-REFUND-SW               PIC X.
00274              88  P5-C-REFUND-CREATED          VALUE 'Y'.
00275              88  P5-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  P5-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  P5-C-PAYEE-CODE              PIC X(6).
00278          16  P5-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  P5-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  P5-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  P5-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  P5-C-REFERENCE               PIC X(12).
PEMMOD         16  P5-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  P5-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  P5-C-POST-CARD-IND           PIC X.
081606         16  P5-C-CANCEL-REASON           PIC X.
072308         16  P5-C-REF-INTERFACE-SW        PIC X.
071211         16  P5-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  P5-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  P5-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  P5-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  P5-CANCELED-CERT-DATA.
00287              20  P5-CI-INSURED-NAME.
00288                  24  P5-CI-LAST-NAME      PIC X(15).
00289                  24  P5-CI-INITIALS       PIC XX.
00290              20  P5-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  P5-CI-INSURED-SEX        PIC X.
00292              20  P5-CI-LF-TERM            PIC S999        COMP-3.
00293              20  P5-CI-LF-BENEFIT-CD      PIC XX.
00294              20  P5-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  P5-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  P5-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  P5-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  P5-CI-AH-TERM            PIC S999        COMP-3.
00299              20  P5-CI-AH-BENEFIT-CD      PIC XX.
00300              20  P5-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  P5-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  P5-CI-RATE-CLASS         PIC XX.
00303              20  P5-CI-RATE-DEV-LF        PIC XXX.
00304              20  P5-CI-RATE-DEV-AH        PIC XXX.
00305              20  P5-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  P5-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  P5-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  P5-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  P5-CI-LF-ABBR            PIC X(3).
00310              20  P5-CI-AH-ABBR            PIC X(3).
00311              20  P5-CI-OB-FLAG            PIC X.
00312                  88  P5-CI-OB                VALUE 'B'.
00313              20  P5-CI-LF-POLICY-STATUS   PIC X.
00314                  88  P5-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  P5-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  P5-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  P5-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  P5-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  P5-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  P5-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  P5-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  P5-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  P5-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  P5-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  P5-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  P5-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  P5-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  P5-CI-AH-POLICY-STATUS   PIC X.
00328                  88  P5-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  P5-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  P5-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  P5-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  P5-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  P5-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  P5-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  P5-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  P5-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  P5-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  P5-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  P5-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  P5-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  P5-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  P5-CI-PAY-FREQUENCY      PIC 99.
00342              20  P5-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  P5-CI-SOC-SEC-NO         PIC X(11).
00344              20  P5-CI-MEMBER-NO          PIC X(12).
00345              20  P5-CI-INT-CODE           PIC X.
00346                  88  P5-CI-ADD-ON                  VALUE 'A'.
00347                  88  P5-CI-SIMPLE                  VALUE 'S'.
00348              20  P5-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  P5-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  P5-CI-COMP-EXCP-SW       PIC X.
00351                  88  P5-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  P5-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  P5-CI-ENTRY-STATUS       PIC X.
00354              20  P5-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  P5-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  P5-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  P5-CI-DEATH-DT           PIC XX.
00358              20  P5-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  P5-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  P5-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  P5-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  P5-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  P5-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  P5-CI-ENTRY-DT              PIC XX.
00365              20  P5-CI-ENTRY-BATCH           PIC X(6).
00366              20  P5-CI-LF-EXPIRE-DT          PIC XX.
00367              20  P5-CI-AH-EXPIRE-DT          PIC XX.
00368              20  P5-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  P5-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  P5-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  P5-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  P5-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  P5-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  P5-CI-INDV-GRP-CD           PIC X.
100703             20  P5-CI-BENEFICIARY-NAME.
100703                 24  P5-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  P5-CI-NOTE-SW               PIC X.
00377              20  P5-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  P5-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  P5-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  P5-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  P5-CI-LOAN-OFFICER          PIC X(5).
032306             20  P5-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  P5-CI-FIRST-NAME            PIC X(10).
071211             20  P5-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  P5-MAIL-RECORD    REDEFINES P5-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  P5-M-INSURED-LAST-NAME       PIC X(15).
00386          16  P5-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  P5-M-INSURED-MID-INIT        PIC X.
00388          16  P5-M-INSURED-AGE             PIC 99.
00389          16  P5-M-INSURED-BIRTHDAY        PIC XX.
00390          16  P5-M-INSURED-SEX             PIC X.
00391          16  P5-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  P5-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  P5-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  P5-M-INSURED-CITY-STATE.
051810             20  P5-M-INSURED-CITY        PIC X(28).
051810             20  P5-M-INSURED-STATE       PIC XX.
00395          16  P5-M-INSURED-ZIP-CODE.
00396              20  P5-M-INSURED-ZIP-PRIME.
00397                  24  P5-M-INSURED-ZIP-1   PIC X.
00398                      88  P5-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  P5-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  P5-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         P5-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  P5-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  P5-M-JOINT-BIRTHDAY          PIC XX.
               16  P5-M-CRED-BENE-NAME          PIC X(30).
               16  P5-M-CRED-BENE-ADDR1         PIC X(30).
               16  P5-M-CRED-BENE-ADDR2         PIC X(30).
               16  P5-M-CRED-BENE-CITYST.
                   20  P5-M-CRED-BENE-CITY      PIC X(28).
                   20  P5-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  P5-BATCH-RECORD   REDEFINES P5-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  P5-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  P5-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  P5-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  P5-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  P5-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  P5-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  P5-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  P5-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  P5-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  P5-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  P5-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  P5-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  P5-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  P5-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  P5-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  P5-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  P5-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  P5-ACCOUNT-NAME              PIC X(30).
00430          16  P5-PREM-REF-RPT-FLAG         PIC X.
00431          16  P5-REFERENCE                 PIC X(12).
00432          16  P5-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  P5-RECORD-STATUS.
00436          16  P5-CREDIT-SELECT-DT          PIC XX.
00437          16  P5-CREDIT-ACCEPT-DT          PIC XX.
00438          16  P5-BILLED-DT                 PIC XX.
00439          16  P5-BILLING-STATUS            PIC X.
00440              88  P5-ENTRY-REVERSED            VALUE 'R'.
00441              88  P5-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  P5-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  P5-RECORD-BILL               PIC X.
00444              88  P5-RECORD-ON-HOLD            VALUE 'H'.
00445              88  P5-RECORD-RETURNED           VALUE 'R'.
00446              88  P5-RECORD-ENDORSED           VALUE 'E'.
00447              88  P5-OVERRIDE-LIFE             VALUE 'L'.
00448              88  P5-OVERRIDE-AH               VALUE 'A'.
00449              88  P5-OVERRIDE-BOTH             VALUE 'B'.
00450          16  P5-BATCH-ENTRY               PIC X.
00451              88  P5-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  P5-REIN-ONLY-CERT            VALUE 'R'.
00453              88  P5-REISSUED-CERT             VALUE 'E'.
                   88  P5-CASH-CERT                 VALUE 'C'.
122002             88  P5-MONTHLY-CERT              VALUE 'M'.
00454              88  P5-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  P5-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  P5-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  P5-FORCE-CODE                PIC X.
00458              88  P5-FORCE-OFF                 VALUE ' ' '0'.
00459              88  P5-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  P5-CANCEL-FORCE              VALUE '8'.
00461              88  P5-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  P5-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  P5-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  P5-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  P5-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  P5-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  P5-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  P5-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  P5-FATAL-FLAG                PIC X.
00468              88  P5-FATAL-ERRORS              VALUE 'X'.
00469          16  P5-FORCE-ER-CD               PIC X.
00470              88  P5-FORCE-ERRORS              VALUE 'F'.
00471              88  P5-UNFORCED-ERRORS           VALUE 'X'.
00472          16  P5-WARN-ER-CD                PIC X.
00473              88  P5-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  P5-OUT-BAL-CD                PIC X.
00476              88  P5-OUT-OF-BAL                VALUE 'O'.
00477          16  P5-LIFE-OVERRIDE-L1          PIC X.
00478          16  P5-AH-OVERRIDE-L1            PIC X.
00479          16  P5-INPUT-DT                  PIC XX.
00480          16  P5-INPUT-BY                  PIC X(4).
00481          16  P5-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  P5-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  P5-TOLERANCE-REJECT-SW       PIC X.
00484          16  P5-LF-EARNING-METHOD         PIC X.
00485          16  P5-AH-EARNING-METHOD         PIC X.
00486          16  P5-LF-TERM-CALC-METHOD       PIC X.
00487          16  P5-AH-TERM-CALC-METHOD       PIC X.
00488          16  P5-REIN-CD                   PIC XXX.
00489          16  P5-LF-REFUND-TYPE            PIC X.
00490          16  P5-AH-REFUND-TYPE            PIC X.
00491          16  P5-ACCT-EFF-DT               PIC XX.
00492          16  P5-ACCT-EXP-DT               PIC XX.
00493          16  P5-COMPANY-ID                PIC X(3).
00494          16  P5-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  P5-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  P5-SV-CARRIER                PIC X.
00497          16  P5-SV-GROUPING               PIC X(6).
00498          16  P5-SV-STATE                  PIC XX.
00499          16  P5-CONFIRMATION-REPT-DT      PIC XX.
00500          16  P5-GA-BILLING-INFO.
00501              20  P5-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  P5-SV-REMIT-TO  REDEFINES
00504              P5-GA-BILLING-INFO           PIC X(10).
00505          16  P5-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  P5-I-LOAN-OFFICER            PIC X(5).
081606         16  P5-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  P5-COMMON-ERRORS.
00514          16  P5-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
      *                                COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
      *                                COPY ELCDTECX.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCDTECX.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *      WORKING STORAGE COPY CODE FOR APPLICATIONS CALLING        *
00008 *      THE DATE CARD DISK FILE. ( READ ROUTINE = ELCDTERX,       *
00009 *      AND DISK FILE FD = ELCDTEFX)                              *
00010 *                                                                *
00011 ******************************************************************
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 300 TO 900
090803* 090803    2003080800002  PEMA  ADD CATEGORY FOR SUPER GAP
051304* 051304    2003080800002  SMVA  ADD REFUND METHOD
071714* 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
052218* 052218  CR2014061700002  PEMA  INCREASE MORT TABLES TABLE
092602******************************************************************
00012  01  DATE-CARD.
00013      12  DATX-COD                    PIC  X(4).
00014      12  RUN-DATE                    PIC  9(11)  COMP-3.
00015      12  ALPH-DATE                   PIC  X(18).
00016      12  EP-DT                       PIC  9(11)  COMP-3.
00017      12  PEND-ACT-FILE               PIC  XX.
00018      12  EP-SW                       PIC  X.
00019          88  NO-EP-EXTRACTS          VALUE SPACE.
00020      12  TAPE-BATCHES                PIC  X.
00021          88  TP-PREL-BAL             VALUE '1'.
00022      12  BIN-RUN-DATE                PIC  XX.
00023      12  RUN-CENTURY.
00024          16 RUN-CENTURY-N            PIC  99.
00025      12  FILLER                      PIC  X.
00026      12  CLAS-MORT-OVRD              PIC  X(4).
00027      12  FILLER                      PIC  XXX.
00028      12  COMPANY-NAME                PIC  X(30).
00029
00030  01  FILLER                          PIC  X(7)
00031                                           VALUE 'OPTIONS'.
00032  01  DATE-CARD-OPTIONS.
00033      12  DATE-OPTIONS-ID             PIC  X(4).
00034      12  DTE-MIN-PREM                PIC  99V99.
00035      12  DTE-LANGUAGE-IND            PIC  X.
00036          88  DTE-LANGUAGE-IS-FR           VALUE 'F'.
00037          88  DTE-LANGUAGE-IS-ENG          VALUE 'E'.
00038      12  DTE-MIN-AGE                 PIC  99.
00039      12  DTE-DEFAULT-AGE             PIC  99.
00040      12  FILLER                      PIC  X.
00041      12  DTE-MAX-TERM                PIC  999.
00042      12  DTE-COMP-VG                 PIC  X.
00043      12  DTE-REM-TRM                 PIC  X.
00044      12  DTE-PRM-CK                  PIC S99V99.
00045      12  DTE-PRORATA                 PIC  X.
00046      12  DTE-CLM-REJ                 PIC  X.
00047      12  DTE-REF-REJ                 PIC  X.
00048      12  DTE-COM-TBL-USED            PIC  X.
00049      12  FILLER                      PIC  X.
00050      12  DTE-QTR-CO                  PIC  X.
00051      12  DTE-REF-CK                  PIC S99V99.
00052      12  DTE-CLM-CK                  PIC S99V99.
00053      12  DTE-CONV-DT                 PIC 9(11) COMP-3.
00054      12  DTE-DEFAULT-SEX             PIC  X.
00055      12  DTE-EPL-FORMAT              PIC  X.
00056      12  FILLER                      PIC  X.
00057      12  DTE-JT-AGE                  PIC  X.
00058      12  DTE-KEY-BIRTH               PIC  X.
00059      12  DTE-REINSURANCE             PIC  X.
00060      12  DTE-CLAIM-SORT              PIC  X.
00061      12  DTE-WRT-OFF                 PIC  S99V99.
00062      12  DTE-R78                     PIC  X.
00063      12  DTE-DLY-BILL                PIC  X.
00064      12  DTE-ALT-MORT-CODE           PIC  X(4).
00065      12  DTE-CLAIM-PAID-THRU-TO      PIC  X.
00066      12  DTE-COMPENSATION-ACCESS     PIC  X.
00067      12  DTE-MORTG-ACCESS-CNTL       PIC  X.
00068      12  DTE-MP-ALT-MORT-CODE        PIC  X(4).
00069      12  DTE-MORTALITY-AGE-CALC-METHOD
00070                                      PIC  X.
00071          88  DTE-USE-TABLE-ASSIGNED-METHOD VALUE '1'
00072                                                  ' '.
00073          88  DTE-USE-ALL-AGE-LAST          VALUE '2'.
00074          88  DTE-USE-ALL-AGE-NEAR          VALUE '3'.
00075      12  DTE-RESERVE-OPTION-SWITCH   PIC  X.
00076          88  DTE-OPT-RESERVE-METHOD-AUTH       VALUE 'Y'.
00077          88  DTE-OPT-RESERVE-METHOD-UNAUTH     VALUE 'N' ' '.
00078      12  DTE-REM-TRM-CALC-OPTION     PIC  X.
00079      12  DTE-EXPERIENCE-RETENTION-AGE
00080                                      PIC  9.
00081      12  DTE-SYSTEM.
00082          16  DTE-SYS-A-CREDIT        PIC  X.
00083          16  DTE-SYS-B-PEND-CLAIM    PIC  X.
00084          16  DTE-SYS-C-CONFIRMATIONS PIC  X.
00085          16  DTE-SYS-D-DEMAND-BILL   PIC  X.
00086          16  DTE-SYS-E-CLASIC-CLAIMS PIC  X.
00087          16  DTE-SYS-F-CLASIC-CREDIT PIC  X.
00088          16  DTE-SYS-G-AR-USED       PIC  X.
00089          16  DTE-SYS-H               PIC  X.
00090          16  DTE-SYS-I               PIC  X.
00091          16  DTE-SYS-J               PIC  X.
00092          16  DTE-SYS-K               PIC  X.
00093          16  DTE-SYS-L               PIC  X.
00094          16  DTE-SYS-M               PIC  X.
00095          16  DTE-SYS-N               PIC  X.
00096          16  DTE-SYS-O               PIC  X.
00097          16  DTE-SYS-P               PIC  X.
00098          16  DTE-SYS-Q               PIC  X.
00099          16  DTE-SYS-R               PIC  X.
00100          16  DTE-SYS-S               PIC  X.
00101          16  DTE-SYS-T               PIC  X.
00102          16  DTE-SYS-U               PIC  X.
00103          16  DTE-SYS-V               PIC  X.
00104          16  DTE-SYS-W               PIC  X.
00105          16  DTE-SYS-X               PIC  X.
00106          16  DTE-SYS-Y               PIC  X.
00107          16  DTE-SYS-Z               PIC  X.
00108      12  FILLER                      REDEFINES DTE-SYSTEM.
00109          16  DTE-SYS-CODE            OCCURS 26 TIMES
00110                                      PIC  X.
00111      12  DTE-CLIENT                  PIC  XXX.
00112
00113  01  CLASIC-SYSTEM-CODES.
00114      12  DTE-COLC-ID                 PIC  X(4).
00115      12  DTE-CLASIC-COMPANY-CD       PIC  X.
00116      12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.
00117 *    12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.
00118      12  FILLER                      PIC  X.
00119      12  CLASIC-REIN-MAINT           PIC  XX.
00120      12  CLASIC-COMP-MAINT           PIC  XX.
00121      12  CLASIC-ACCT-MAINT           PIC  XX.
00122      12  CLASIC-CTBL-MAINT           PIC  XX.
00123      12  CLASIC-RATE-MAINT           PIC  XX.
00124      12  CLASIC-CREDIT-EOM-DT        PIC  XX.
00125      12  CLASIC-CLAIMS-EOM-DT        PIC  XX.
00126
00127      12  LIFE-OVERRIDE-L1            PIC  X.
00128      12  LIFE-OVERRIDE-L2            PIC  XX.
00129      12  LIFE-OVERRIDE-L6            PIC  X(6).
00130      12  LIFE-OVERRIDE-L12           PIC  X(12).
00131
00132      12  AH-OVERRIDE-L1              PIC  X.
00133      12  AH-OVERRIDE-L2              PIC  XX.
00134      12  AH-OVERRIDE-L6              PIC  X(6).
00135      12  AH-OVERRIDE-L12             PIC  X(12).
00136
00137      12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).
00138      12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).
00139
00140      12  CLASIC-MORTG-EOM-DT         PIC  XX.
00141      12  CLASIC-AR-EOM-DT            PIC  XX.
00142
00143      12  FILLER                      PIC  X(11)      VALUE SPACE.
00144
00145  01  DATE-CARD-FACTORS.
00146      12  DATE-FACTOR-ID              PIC  X(4).
00147      12  FAC-1                       PIC S999V9(5).
00148      12  FAC-2                       PIC S999V9(5).
00149      12  FAC-3                       PIC S999V9(5).
00150      12  FAC-4                       PIC S999V9(5).
00151      12  FAC-5                       PIC S999V9(5).
00152      12  FAC-6                       PIC S999V9(5).
00153
00154  01  FILLER                          PIC  X(12)
00155                                           VALUE 'COMPANY NAME'.
00156  01  COMPANY-NAME-TABLE.
00157      12  C-N-TBL                     OCCURS 6 TIMES.
00158          16  CNT-ID                  PIC  X.
00159          16  CNT-NAME                PIC  X(30).
00160
00161  01  FILLER                          PIC  X(11)
00162                                           VALUE 'STATE NAMES'.
00163  01  STATE-NAMES.
00164      12  STATE-NAME-FLD              OCCURS 75 TIMES.
00165          16  STATE-SUB               PIC  XX.
00166          16  STATE-PIC1.
00167              20  STATE-ABBR          PIC  XX.
00168              20  FILLER              PIC  XXX.
00169              20  STATE-PIC           PIC  X(20).
00170              20  STATE-CALL-EARN     PIC  X.
00171              20  STATE-CALL-BREAK    PIC  X.
00172              20  STATE-PRIM-FAC-DEV  PIC  XXX.
00173
00174  01  FILLER                          PIC  X(13)
00175                                           VALUE 'CARRIER NAMES'.
00176  01  CARRIER-NAMES.
00177      12  CARRIER-NAME-FLD            OCCURS 25 TIMES.
00178          16  CARRIER-SUB             PIC  X.
00179          16  CARRIER-DOM-ST          PIC  XX.
00180          16  CARRIER-PIC             PIC  X(30).
00181          16  CARRIER-UEP-PCT         PIC  S9V9(4) COMP-3.
00182          16  CARRIER-R78-PCT         PIC  S9V9(4) COMP-3.
00183          16  CARRIER-PRO-PCT         PIC  S9V9(4) COMP-3.
011904*        16  FILLER                  PIC  X.
011904         16  CARRIER-SEC-PAY         PIC  X.
011904             88  SEC-PAY-CARRIER          VALUE 'Y'.
00185
00186  01  FILLER                          PIC  X(9)
00187                                           VALUE 'INS TYPES'.
00188  01  CLAS-INS-TYPES.
092602*    12 CLAS-ALL-TYPES               OCCURS 300 TIMES.
092602     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.
00190          16  CLAS-I-BEN              PIC  XX.
00191          16  CLAS-I-AB3.
00192              20  FILLER              PIC  X.
00193              20  CLAS-I-AB2.
00194                  24  FILLER          PIC  X.
00195                  24  CLAS-I-AB1      PIC  X.
00196          16  CLAS-I-AB3-AH REDEFINES CLAS-I-AB3.
00197              20  CLAS-EXCLUSION      PIC  XX.
00198              20  FILLER              PIC  X.
00199          16  CLAS-I-AB10.
00200              20  FILLER              PIC  X(9).
00201              20  CLAS-I-REIN-YN      PIC  X.
00202          16  CLAS-I-COMMENT          PIC  X(10).
00203          16  CLAS-I-JOINT            PIC  X.
00204          16  CLAS-I-RL-AH            PIC  X.
00205          16  CLAS-I-CALC-TYPE.
00206              20  CLAS-I-BAL          PIC  X.
00207          16  CLAS-I-EP               PIC  X.
00208          16  CLAS-CO-BEN-I-G-CD      PIC  X.
00209          16  CLAS-CO-REM-TERM-CALC   PIC  X.
090803         16  CLAS-I-BEN-CATEGORY     PIC  X.
051304         16  CLAS-I-REFUND-METHOD    PIC  X.
071714         16  CLAS-I-MAX-BENEFITS     PIC 99.
071714         16  FILLER                  PIC  XXX.
00211
00212  01  FILLER                          PIC  X(16)
00213                                      VALUE 'MORTALITY TABLES'.
00214  01  CLAS-MORTALITY-CODES.
052218     12  CLAS-MORT-FLD               OCCURS 130 TIMES
00216                                      INDEXED BY CLAS-MORT-NDX.
00217          16  CLAS-MORT-CODE          PIC  X(4).
00218          16  CLAS-MORT-J-CODE        PIC  X.
00219          16  CLAS-MORT-J-FACT        PIC S9V9(4).
00220          16  CLAS-MORT-DESC.
00221              20  CLAS-RESERVE-ADJ    PIC  999.
00222              20  FILLER              PIC  XX.
00223              20  CLAS-YEAR           PIC  X(4).
00224              20  FILLER              PIC  X.
00225              20  CLAS-TABLE-TYPE     PIC  XXX.
00226              20  FILLER              PIC  X.
00227              20  CLAS-AGE-METHOD     PIC  XX.
00228              20  FILLER              PIC  X.
00229              20  CLAS-INTEREST       PIC  99.99.
00230              20  FILLER              PIC  X(5).
00231
00232  01  FILLER                          PIC  X(15)
00233                                      VALUE 'BUSINESS TABLES'.
00234  01  CLAS-BUSINESS-CLASSES.
00235      12  CLAS-BUSC-FLD               OCCURS 50 TIMES.
00236          16  CLAS-BUSC-CODE          PIC  99.
00237          16  CLAS-BUSC-GROUP         PIC  X.
00238          16  CLAS-BUSC-DESC          PIC  X(19).
00239          16  CLAS-BUSC-EXCL          PIC  X.
00240          16  FILLER                  PIC  X(4).
00241
00242  01  FILLER                          PIC  X(7)
00243                                      VALUE 'INDEXES'.
00244  01  CLAS-INDEX-TBL.
00245      12  CLAX-ID                     PIC  X(4).
00246      12  CLAS-STARTC                 PIC S9(4) COMP.
00247      12  CLAS-MAXC                   PIC S9(4) COMP.
00248      12  CLAS-STARTL                 PIC S9(4) COMP.
00249      12  CLAS-MAXL                   PIC S9(4) COMP.
00250      12  CLAS-STARTA                 PIC S9(4) COMP.
00251      12  CLAS-MAXA                   PIC S9(4) COMP.
00252      12  CLAS-STARTM                 PIC S9(4) COMP.
00253      12  CLAS-MAXM                   PIC S9(4) COMP.
00254      12  CLAS-STARTB                 PIC S9(4) COMP.
00255      12  CLAS-MAXB                   PIC S9(4) COMP.
00256      12  CLAS-STARTS                 PIC S9(4) COMP.
00257      12  CLAS-MAXS                   PIC S9(4) COMP.
00258      12  CLAS-STARTE                 PIC S9(4) COMP.
00259      12  CLAS-MAXE                   PIC S9(4) COMP.
00260      12  CLAS-STARTCN                PIC S9(4) COMP.
00261      12  CLAS-MAXCN                  PIC S9(4) COMP.
00262
00263  01  CLAS-TYPE-MISC.
00264      12  CLAS-INDEXC                 PIC S9(4) COMP.
00265      12  CLAS-INDEXL                 PIC S9(4) COMP.
00266      12  CLAS-INDEXA                 PIC S9(4) COMP.
00267      12  CLAS-INDEXM                 PIC S9(4) COMP.
00268      12  CLAS-INDEXB                 PIC S9(4) COMP.
00269      12  CLAS-INDEXS                 PIC S9(4) COMP.
00270      12  CLAS-INDEXE                 PIC S9(4) COMP.
00271      12  CLAS-INDEX                  PIC S9(4) COMP.
00272      12  CLAS-INDEXEN                PIC S9(4) COMP.
00273      12  CLAS-INDEXCN                PIC S9(4) COMP.
00274      12  CLAS-INDEXON                PIC S9(4) COMP.
00275
00276  01  FILLER                          PIC  X(13)
00277                                      VALUE 'MISCELLANEOUS'.
00278  01  CLAS-MISC.
00279      12  DTE-FICH                    PIC  X.
00280          88  FICH-NO                 VALUE SPACE.
00281          88  FICH-ONLY               VALUE '1'.
00282          88  FICH-BOTH               VALUE '2'.
00283      12  FICH-OPEN                   PIC  X.
00284      12  REPT-OPEN                   PIC  X.
00285      12  DTE-PGM-OPT                 PIC  9.
00286      12  DTE-PRT-OPT                 PIC  X.
00287      12  DTE-FMT-OPT                 PIC  9.
00288      12  DTE-PRC-OPT                 PIC  9.
00289      12  DTE-TOT-OPT                 PIC  9.
00290      12  CLAS-LOOK                   PIC  XX.
00291      12  DTE-TOT-LINES               PIC S9(8)      COMP.
00292      12  STATE-L                     PIC  XX.
00293      12  CLAS-CN                     PIC  99.
00294      12  CLAS-CO                     PIC  99.
00295      12  DTE-VSAM-FLAGS                              VALUE ZERO.
00296          16  DTE-F-1                 PIC  X.
00297          16  DTE-F-2                 PIC  X.
00298      12  DTE-ABEND-WORK.
00299          16  DTE-ABEND-CD-1          PIC  XX         VALUE SPACES.
00300          16  DTE-ABEND-CD-2          PIC  XX         VALUE SPACES.
00301
00302  01  FILLER                          PIC  X(12)
00303                                      VALUE 'WORKING DATE'.
00304  01  WS-DATE-AND-TIME.
00305      12  WS-ACCEPT-DATE.
00306          16  WS-AD-YY                PIC  99.
00307          16  WS-AD-MM                PIC  99.
00308          16  WS-AD-DD                PIC  99.
00309      12  WS-CURRENT-DATE.
00310          16  WS-CD-MM                PIC  99.
00311          16  FILLER                  PIC  X          VALUE '/'.
00312          16  WS-CD-DD                PIC  99.
00313          16  FILLER                  PIC  X          VALUE '/'.
00314          16  WS-CD-YY                PIC  99.
00315      12  WS-TIME-OF-DAY.
00316          16  WS-TIME                 PIC  9(6).
00317          16  WS-HUN-SEC              PIC  99.
00318
00319  01  FILLER                          PIC  X(12)
00320                                      VALUE 'CARRIER CLMS'.
00321  01  CARRIER-OPT-CLAIM-DATA.
00322      12  CARRIER-FLDS                OCCURS 25 TIMES.
00323          16  CARRIER-IBNR-SWITCH     PIC  X.
00324          16  CARRIER-IBNR-PERCENT    PIC S9V9(4) COMP-3.
00325          16  CARRIER-CIDA-DISCOUNT   PIC S9V9(4) COMP-3.
00326
00327  01  FILLER                          PIC  X(10)
00328                                      VALUE 'STATE TLRS'.
00329  01  STATE-TARGET-LOSS-RATIOS.
00330      12  STATE-TLR-FLD               OCCURS 75 TIMES.
00331          16  STATE-TARGET-LOSS-RATIO PIC S9V9(4) COMP-3.
00332          16  STATE-CALC-INTEREST     PIC S9V9(4) COMP-3.
00333
00334  01  FILLER                          PIC  X(13)
00335                                      VALUE 'BUSINESS TLRS'.
00336  01  BUSINESS-TRGT-LOSS-RATIO-MODS.
00337      12  BUS-TLRM-FLD                OCCURS 99 TIMES.
00338          16  BUS-TRGT-LOSS-RATIO-MOD PIC S9V9(4) COMP-3.
00339
00340  01  FILLER                          PIC  X(13)
00341                                      VALUE 'MISC CLM DATA'.
00342  01  MISC-OPT-CLM-RSV-DATA.
00343      12  COMPANY-CALC-INTEREST       PIC S9V9(4) COMP-3.
00344      12  COMPANY-CIDA-DISCOUNT       PIC S9V9(4) COMP-3.
00345      12  COMPANY-CRDB-TABLE-SELECTION
00346                                      PIC  X.
00347      12  COMPANY-IBNR-AH-FACTOR      PIC S9V9(4) COMP-3.
00348      12  COMPANY-IBNR-LAG-MONTH      PIC S999    COMP-3.
00349      12  COMPANY-IBNR-LIFE-FACTOR    PIC S9V9(4) COMP-3.
00350      12  COMPANY-OPTION-START-DATE   PIC  XX.
00351      12  INDEXBS                     PIC S9(4)   COMP.
00352      12  INDEXCA                     PIC S9(4)   COMP.
00353      12  INDEXST                     PIC S9(4)   COMP.
00354      12  FILLER                      PIC  X(75).
00355 ******************************************************************
CIDMOD*01  MISC-CARR-PCT-DATA.
CIDMOD*    12  DD-CARR1-UEP-PCT            PIC S9V9(4) COMP-3.
CIDMOD*    12  DD-CARR1-R78-PCT            PIC S9V9(4) COMP-3.
CIDMOD*    12  DD-CARR1-PRO-PCT            PIC S9V9(4) COMP-3.
CIDMOD*    12  DD-CARR2-UEP-PCT            PIC S9V9(4) COMP-3.
CIDMOD*    12  DD-CARR2-R78-PCT            PIC S9V9(4) COMP-3.
CIDMOD*    12  DD-CARR2-PRO-PCT            PIC S9V9(4) COMP-3.
00355 ******************************************************************
      *                                COPY ELCDTEVR.
00001 *****************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDTEVR.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.001                          *
00006 *                                                                *
00007 *   WORKING STORAGE FOR DATE VARIABLES CREATED                   *
00008 *   FOR THE YEAR 2000 DATE MODIFICATION                          *
00009 *                                                                *
00010 ******************************************************************
00011  01  WS-DATE-CARD-DATE.
00012      05  WS-RUN-DATE.
00013          10  FILLER                  PIC  9(03).
00014          10  RUN-CCYY                PIC  9(04).
00015          10  RUN-CCYR  REDEFINES  RUN-CCYY.
00016              15  RUN-CC              PIC  99.
00017              15  RUN-YR              PIC  99.
00018          10  RUN-MO                  PIC  99.
00019          10  RUN-DA                  PIC  99.
00020      05  WS-RUN-DATE-N REDEFINES
00021             WS-RUN-DATE              PIC 9(11).
00022      05  WS-EP-DT.
00023          10  FILLER                  PIC  9(03).
00024          10  EP-CCYY                 PIC  9(04).
00025          10  EP-CCYR  REDEFINES  EP-CCYY.
00026              15  EP-CC               PIC  99.
00027              15  EP-YR               PIC  99.
00028          10  EP-MO                   PIC  99.
00029          10  EP-DA                   PIC  99.
00030      05  WS-EP-DT-N REDEFINES
00031             WS-EP-DT                 PIC 9(11).
00032      05  WS-DTE-CONV-DT.
00033          10  FILLER                  PIC  9(03).
00034          10  DTE-CONV-CCYY           PIC  9(04).
00035          10  DTE-CONV-CCYR  REDEFINES  DTE-CONV-CCYY.
00036              15  DTE-CONV-CC         PIC  99.
00037              15  DTE-CONV-YR         PIC  99.
00038          10  DTE-CONV-MO             PIC  99.
00039          10  DTE-CONV-DA             PIC  99.
00040      05  WS-DTE-CONV-DT-N REDEFINES
00041             WS-DTE-CONV-DT           PIC  9(11).
00042  01  WS-BALANCE-FORWARD-DATE.
00043      05  WS-BF-LAST-ACTIVITY-DATE.
00044          10  FILLER                            PIC 999.
00045          10  BF-ACT-CCYY                       PIC 9(04).
00046          10  BF-ACT-CCYR  REDEFINES  BF-ACT-CCYY.
00047              15  BF-ACT-CC                     PIC 99.
00048              15  BF-ACT-YEAR                   PIC 99.
00049          10  BF-ACT-MONTH                      PIC 99.
00050          10  BF-ACT-DAY                        PIC 99.
00051      05  WS-BF-LAST-ACTIVITY-DATE-N REDEFINES
00052             WS-BF-LAST-ACTIVITY-DATE           PIC 9(11).
00053      05  WS-BF-CURRENT-LAST-STMT-DT.
00054          10  FILLER                            PIC 999.
00055          10  BF-CURRENT-LAST-STMT-CEN          PIC 99.
00056          10  BF-CURRENT-LAST-STMT-YEAR         PIC 99.
00057          10  BF-CURRENT-LAST-STMT-MONTH        PIC 99.
00058          10  BF-CURRENT-LAST-STMT-DAY          PIC 99.
00059      05  WS-BF-CURRENT-LAST-STMT-DT-N REDEFINES
00060             WS-BF-CURRENT-LAST-STMT-DT        PIC 9(11).
00061  01  WS-ADD-CONF-TRANSACTIONS-DTE.
00062      05  WS-AC-CERT-EFF-DATE.
00063          10  FILLER                            PIC 999.
00064          10  AC-CERT-CCYY                      PIC 9(04).
00065          10  AC-CERT-CCYR  REDEFINES  AC-CERT-CCYY.
00066              15  AC-CERT-CC                    PIC 99.
00067              15  AC-CERT-YR                    PIC 99.
00068          10  AC-CERT-MO                        PIC 99.
00069          10  AC-CERT-DA                        PIC 99.
00070      05  WS-AC-CERT-EFF-DATE-N REDEFINES
00071             WS-AC-CERT-EFF-DATE                PIC 9(11).
00072      05  WS-AC-LIFE-CANCEL-DATE.
00073          10  FILLER                            PIC 999.
00074          10  AC-LIFE-CANCEL-CCYY               PIC 9(04).
00075          10  AC-LIFE-CANCEL-CCYR REDEFINES AC-LIFE-CANCEL-CCYY.
00076              15  AC-LIFE-CANCEL-CC             PIC 99.
00077              15  AC-LIFE-CANCEL-YR             PIC 99.
00078          10  AC-LIFE-CANCEL-MO                 PIC 99.
00079          10  AC-LIFE-CANCEL-DA                 PIC 99.
00080      05  WS-AC-LIFE-CANCEL-DATE-N REDEFINES
00081             WS-AC-LIFE-CANCEL-DATE             PIC 9(11).
00082      05  WS-AC-AH-CANCEL-DATE.
00083          10  FILLER                            PIC 999.
00084          10  AC-AH-CANCEL-CCYY                 PIC 9(04).
00085          10  AC-AH-CANCEL-CCYR  REDEFINES  AC-AH-CANCEL-CCYY.
00086              15  AC-AH-CANCEL-CC               PIC 99.
00087              15  AC-AH-CANCEL-YR               PIC 99.
00088          10  AC-AH-CANCEL-MO                   PIC 99.
00089          10  AC-AH-CANCEL-DA                   PIC 99.
00090      05  WS-AC-AH-CANCEL-DATE-N REDEFINES
00091             WS-AC-AH-CANCEL-DATE               PIC 9(11).
00092  01  WS-RATE-RECORD.
00093          05  WS-RT-EXPIRY-DATE.
00094              10  FILLER                        PIC 999.
00095              10  RT-EXP-CCYY                   PIC 9(04).
00096              10  RT-EXP-CCYR  REDEFINES  RT-EXP-CCYY.
00097                  15  RT-EXP-CC                 PIC 99.
00098                  15  RT-EXP-YR                 PIC 99.
00099              10  RT-EXP-MO                     PIC 99.
00100              10  RT-EXP-DA                     PIC 99.
00101          05  WS-RT-EXPIRY-DATE-N REDEFINES
00102                 WS-RT-EXPIRY-DATE              PIC 9(11).
      *                                COPY ERCCNOT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCCNOT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE NOTES                    *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
00017 ******************************************************************
00018
00019  01  CERT-NOTE-FILE.
00020      12  CZ-RECORD-ID                PIC  XX.
00021          88  VALID-CZ-ID                  VALUE 'CZ'.
00022
00023      12  CZ-CONTROL-PRIMARY.
00024          16  CZ-COMPANY-CD           PIC X.
00025          16  CZ-CARRIER              PIC X.
00026          16  CZ-GROUPING.
00027              20 CZ-GROUPING-PREFIX   PIC XXX.
00028              20 CZ-GROUPING-PRIME    PIC XXX.
00029          16  CZ-STATE                PIC XX.
00030          16  CZ-ACCOUNT.
00031              20 CZ-ACCOUNT-PREFIX    PIC X(4).
00032              20 CZ-ACCOUNT-PRIME     PIC X(6).
00033          16  CZ-CERT-EFF-DT          PIC XX.
00034          16  CZ-CERT-NO.
00035              20  CZ-CERT-PRIME       PIC X(10).
00036              20  CZ-CERT-SFX         PIC X.
00037          16  CZ-RECORD-TYPE          PIC X.
00038              88  CERT-NOTE           VALUE '1'.
                   88  CLAIM-CERT-NOTE     VALUE '2'.
00039          16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
00040
00041      12  CZ-LAST-MAINT-DT            PIC XX.
00042      12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00043      12  CZ-LAST-MAINT-USER          PIC X(4).
00044
00045      12  CZ-NOTE-INFORMATION.
00046          16  CZ-NOTE                 PIC X(63).
00047          16  FILLER                  PIC X(39).
00048 ******************************************************************
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  client-comp-id       pic xxx.
            15  client-carrier       pic x.
            15  client-state         pic xx.
            15  client-account       pic x(10).
            15  client-eff-dt        pic 9(08).
            15  client-cert-no       pic x(11).
            15  filler               pic x.
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'SOCK17' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
           perform 0000-INITIALIZE     thru 0000-exit
           if resp-normal
              PERFORM 0050-PROCESS-INPUT
                                       THRU 0050-EXIT
           else
              display ' certificate   not found ' client-carrier ' '
              client-state ' ' client-account ' ' client-cert-no
              move ' issue rec not found '
                                       to ws-return-stuff
           end-if
           perform 0200-send-buffer    thru 0200-exit
           perform 0300-close-socket   thru 0300-exit
           
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00005168' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK17' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
           .
       0000-INITIALIZE.
           
      * exec cics
      *       asktime
      *    end-exec
      *    MOVE '0"                    "   #00005172' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           perform 9700-date-convert   thru 9700-exit
           IF DATE-CONVERSION-ERROR
              display ' error converting eibdate '
              MOVE LOW-VALUES          TO save-bin-date
           ELSE
              MOVE DC-BIN-DATE-1       TO save-bin-date
                                          ws-current-bin-dt
           end-if
           move client-comp-id         to ws-comp-id
           evaluate true
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate
           move spaces                 to ws-alpha-table
           PERFORM 0040-READ-ELCERT    THRU 0040-EXIT
           if resp-normal
              perform 0045-get-ben-codes
                                       thru 0045-exit
           end-if
           .
       0000-EXIT.
           EXIT.
       0040-READ-ELCERT.
           move ws-comp-cd             to ws-elcert-company-cd
           move client-carrier         to ws-elcert-carrier
           move '000000'               to ws-elcert-grouping
           move client-state           to ws-elcert-state
           move client-account         to ws-elcert-account
           move client-eff-dt          to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-elcert-eff-dt
           end-if
           move client-cert-no         to ws-elcert-cert-no
           
      * exec cics read
      *       dataset     ('ELCERT')
      *       into        (certificate-master)
      *       ridfld      (ws-elcert-key)
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005216' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV11, 
                 ws-elcert-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0040-EXIT.
           EXIT.
       0045-get-ben-codes.
           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
              if c1 < +9
                 move cf-joint-indicator (c1)
                                       to lf-joint-ind
              end-if
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
              if c1 < +9
                 move cf-joint-indicator (c1)
                                       to ah-joint-ind
              end-if
           end-if
           .
       0045-exit.
           exit.
       0050-PROCESS-INPUT.
           move cm-control-primary     to ws-elcert-orig-key
           perform 0100-check-cancel   thru 0100-exit
           if ((cm-lf-benefit-cd <> spaces and zeros)
              or (cm-ah-benefit-cd <> spaces and zeros))
              and (cm-entry-status <> '5' and '9' and 'D' and 'V')
              display ' must not be cancelled '
              move +0                  to a1
              move spaces              to ws-alpha-table
                                          ws-return-stuff
              move 'P'                 to ws-cov-sw
              move cm-insured-last-name
                                       to ws-name-in
              perform 0650-set-last-name
                                       thru 0650-exit
              move ws-name-out         to ws-last-name
      *       display ' ws last name ' ws-last-name
              move cm-insured-first-name (1:3)
                                       to ws-first-three
      *       display ' ws 1st 3 ' ws-first-three
              perform 0700-calc-cur-cm-ages
                                       thru 0700-exit
              move ws-cm-pri-curr-age  to ws-age
      *       display ' pri age ' ws-age
              perform 0150-process-continue
                                       thru 0150-exit
              if ((lf-joint-ind = 'J')
                 or (ah-joint-ind = 'J'))
                         and
                    (cm-jt-last-name not = spaces)
                 move ws-hold-elcert   to certificate-master
                 move ' '              to WS-ERPNDB5-SW
                 move 'S'              to ws-cov-sw
                 move cm-jt-last-name  to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-last-name
                 move cm-jt-first-name (1:3)
                                       to ws-first-three
                 move ws-cm-cob-curr-age
                                       to ws-age
      *          display ' COB age ' ws-age
                 perform 0150-process-continue
                                       thru 0150-exit
              end-if
           else
              display ' certificate cancelled ' client-carrier ' '
              client-state ' ' client-account ' ' client-cert-no
              move ' certificate cancelled '
                                       to ws-return-stuff
           end-if
           .
       0050-EXIT.
           EXIT.
       0100-check-cancel.
      *** check to see if the certificate   is cancelled
           move certificate-master     to ws-hold-elcert
           if cm-lf-benefit-cd <> '00' and '  '
              if cm-lf-cancel-dt <> low-values
                 move '00'             to cm-lf-benefit-cd
              end-if
           end-if
           if cm-ah-benefit-cd <> '00' and '  '
              if cm-ah-cancel-dt <> low-values
                 move '00'             to cm-ah-benefit-cd
              end-if
           end-if
           .
       0100-exit.
           exit.
       0150-process-continue.
      ****  First, move elcert  record to first occurance of table
           move ' '                    to ws-match-sw
           move low-values             to ws-eralph-aix-key
           move cm-company-cd          TO ws-eralph-aix-company-cd
           MOVE cm-ACCOUNT             TO ws-eralph-aix-account
           MOVE ws-last-name           to ws-eralph-aix-lname
           move ws-first-three         to ws-eralph-aix-1st-three
           add +1                      to a1
           move ws-cov-sw              to ws-cov-type (a1)
           if cm-lf-policy-pending or
              cm-ah-policy-pending
              move 'Y'                 to ws-pend (a1)
           end-if
           move ws-age                 to ws-eralph-age (a1)
           if processing-primary
              move cm-insured-first-name
                                       to ws-first-name (a1)
           else
              move cm-jt-first-name    to ws-first-name (a1)
           end-if
           move ws-last-name           to ws-tbl-last-name (a1)
           move cm-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move cm-cert-no             to ws-eralph-cert (a1)
           move cm-lf-benefit-cd       to ws-eralph-lf-cd (a1)
           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
               and (cm-lf-benefit-cd <> '00' and '  ')
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              move ws-eralph-lf-remamt (a1)
                                       to ws-lf-tot-benefit
              move cp-remaining-term-2 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
                                          ws-eralph-lf-remterm (a1)
           end-if
           if cm-lf-loan-expire-dt = low-values or spaces
              move spaces              to ws-lf-exp-dt (a1)
           else
              move cm-lf-loan-expire-dt to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
           end-if
           move cm-ah-benefit-cd       to ws-eralph-ah-cd (a1)
           if ((ws-cov-sw = 'P')
                    or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
               and (cm-ah-benefit-cd <> '00' and '  ')
              perform 0510-get-ah-rem  thru 0510-exit
              move cp-remaining-amt    to ws-eralph-ah-remamt (a1)
              move cp-remaining-term-2 to ws-eralph-ah-remterm (a1)
              move cm-ah-benefit-amt   to ws-eralph-ah-amt (a1)
                                          ws-ah-tot-benefit
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
                                          ws-eralph-ah-remterm (a1)
           end-if
           if cm-ah-loan-expire-dt = low-values or spaces
              move spaces              to ws-ah-exp-dt (a1)
           else
              move cm-ah-loan-expire-dt to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
              end-if
           end-if
           move ';'                    to ws-delimiter (a1)
           perform 0170-ERpndb5        thru 0170-exit
           if erpndb2-startbr
              
      * exec cics endbr
      *          dataset     ('ELCERT')
      *       end-exec
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005400' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           compute p1 = a1 + 1
           move spaces                 to ws-eralph2-startbr-sw
                                          ws-eralph-sw
           
      * exec cics startbr
      *       dataset     ('ERALPH2')
      *       ridfld      (ws-eralph-aix-key)
      *       gteq
      *       resp        (ws-response)
      *    end-exec
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005407' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-eralph-aix-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (resp-endfile)
              or (resp-notfnd)
              set no-matching-alpha to true
              go to 0150-exit
           else
              if not resp-normal
                 display ' error-eralph-start ' ws-response
                 set no-matching-alpha to true
                 go to 0150-exit
              end-if
           end-if
           set eralph2-startbr to true
           perform 0155-read-eralph    thru 0155-exit
           if (not resp-normal)
              and (not resp-dupkey)
              display ' Error-eralph-1stread ' ws-response
              go to 0150-exit
           end-if
           perform 0160-accum-eralph   thru 0160-exit until
              (af-company-cd-a1  not = ws-comp-cd)
              or (af-account-a1  not = client-account)
              or (ws-alpha-last-name not = ws-last-name)
              or (af-fname (1:3) not = ws-first-three)
              or (end-of-eralph)
              or ((not resp-normal) and (not resp-dupkey))
           if eralph2-startbr
              
      * exec cics endbr
      *          dataset     ('ERALPH2')
      *       end-exec
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005440' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           if a1 = +1
              perform 0166-check-for-ah-claim
                                       thru 0166-exit
           end-if
           if a1 > +1
              perform varying t1 from +1 by +1 until t1 > +20
                 move ws-eralph-cert (t1)
                                       to pt-eralph-cert (t1)
                 move ws-eralph-lf-cd (t1)
                                       to pt-eralph-lf-cd (t1)
                 move ws-eralph-lf-remamt (t1)
                                       to pt-eralph-lf-remamt (t1)
                 move ws-eralph-ah-cd (t1)
                                       to pt-eralph-ah-cd (t1)
                 move ws-eralph-ah-amt (t1)
                                       to pt-eralph-ah-amt (t1)
                 move ws-eralph-ah-remamt (t1)
                                       to pt-eralph-ah-remamt (t1)
              end-perform
           end-if
           .
       0150-exit.
           exit.
       0155-READ-ERALPH.
           
      * exec cics readnext
      *       dataset      ('ERALPH2')
      *       ridfld       (ws-eralph-aix-key)
      *       into         (alpha-file-rec)
      *       resp         (ws-response)
      *    end-exec
           MOVE LENGTH OF
            alpha-file-rec
             TO DFHEIV12
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005475' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303035343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 alpha-file-rec, 
                 DFHEIV12, 
                 ws-eralph-aix-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (not resp-normal)
              and (not resp-dupkey)
              display 'setting end-of-eralph to true '
              set end-of-eralph to true
              go to 0155-exit
           end-if
           if (ws-comp-cd = ws-eralph-aix-company-cd)
              and (client-account = ws-eralph-aix-account)
              continue
           else
              set end-of-eralph to true
              go to 0155-exit
           end-if
           move af-lname               to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-alpha-last-name
           evaluate true
              when (ws-last-name = ws-alpha-last-name)
                 and (ws-first-three not = ws-eralph-aix-1st-three)
                 go to 0155-read-eralph
              when (ws-last-name not = ws-alpha-last-name)
                 set end-of-eralph to true
              when other
                 continue
           end-evaluate
           .
       0155-EXIT.
           EXIT.
       0160-accum-eralph.
      ***** calc the current age on the alpha record here
      ***** then compare it to the pb ages, if there not within
      ***** a few years then go to 0160-continue.
      *** check to see if there is a pending cancel for the alpha rec **
      *** if not, then accumulate alpha's in table.
           if ws-elcert-orig-key = af-control-primary(1:33)
              go to 0160-continue
           end-if
           move af-control-primary     to ws-elcert-key
           perform 0600-get-elcert     thru 0600-exit
           if resp-normal
              if cm-lf-benefit-cd not = '00' and '  '
                 if cm-lf-cancel-dt <> low-values
                    move '00'             to af-lf-typ
                 end-if
              end-if
              if cm-ah-benefit-cd not = '00' and '  '
                 if cm-ah-cancel-dt <> low-values
                    move '00'             to af-ah-typ
                 end-if
              end-if
           end-if
           if (af-lf-typ = '00' or spaces)
              and (af-ah-typ = '00' or spaces)
              go to 0160-continue
           end-if
           perform 0720-calc-cur-alph-age
                                       thru 0720-exit
           if (ws-age - ws-alph-curr-age < 4
              and >= 0)
                            or
              (ws-alph-curr-age - ws-age < 4
              and >= 0)
              add +1 to a1
           else
              go to 0160-continue
           end-if
           move zeros to ws-eralph-lf-remamt (a1)
                         ws-eralph-lf-remterm (a1)
                         ws-eralph-ah-amt (a1)
                         ws-eralph-ah-remamt (a1)
                         ws-eralph-ah-remterm (a1)
           move ' '                    to ws-pend (a1)
           move af-fname               to ws-first-name (a1)
           move ws-alph-curr-age       to ws-eralph-age (a1)
           move ws-last-name           to ws-tbl-last-name (a1)
           if af-alpha-type-code = 'I'
              move 'P'                 to ws-eralph-prm-sec (a1)
           else
              move 'C'                 to ws-eralph-prm-sec (a1)
           end-if
           move af-dt                  to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           if af-lf-typ not = '00' and '  '
              move ws-cov-sw           to ws-cov-type (a1)
              move af-cert-no          to ws-eralph-cert (a1)
              move af-lf-typ           to ws-eralph-lf-cd (a1)
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + cp-remaining-amt
              move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + af-lf-remamt + af-lf-remamt-alt
              move af-lf-expires       to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
              move ';'                 to ws-delimiter (a1)
           end-if
           if af-ah-typ not = '00' and '  '
              move ws-cov-sw           to ws-cov-type (a1)
              move af-cert-no          to ws-eralph-cert (a1)
              move af-ah-typ           to ws-eralph-ah-cd (a1)
              if af-ah-status not = '8'
                 move af-ah-amt        to ws-eralph-ah-amt (a1)
                 perform 0510-get-ah-rem
                                       thru 0510-exit
                 move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
                    ws-ah-tot-benefit + af-ah-amt
                 move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
                 move af-ah-expires    to dc-bin-date-1
                 move ' '              to dc-option-code
                 perform 9700-date-convert
                                       thru 9700-exit
                 if no-conversion-error
                    move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
                 end-if
              end-if
              move ';'                 to ws-delimiter (a1)
              perform 0165-check-for-ah-claim
                                       thru 0165-exit
           end-if
           .
       0160-continue.
           perform 0155-read-eralph    thru 0155-exit
           .
       0160-exit.
           exit.
       0165-check-for-ah-claim.
           move ws-comp-cd             to ws-elmstr5-company-cd
           move af-cert-no             to ws-elmstr5-cert-no
           move spaces                 to ws-elmstr5-startbr-sw
           
      * exec cics startbr
      *       dataset         ('ELMSTR5')
      *       ridfld          (ws-elmstr5-key)
      *       gteq
      *       resp            (ws-response)
      *    end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005622' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              set elmstr5-startbr to true
           else
              if (resp-notfnd)
                 or (resp-endfile)
                 go to 0165-exit
              else
                 display 'error-elmstr5-startbr '
                 ws-response ' ' af-cert-no
              end-if
           end-if
           .
       0165-read-next.
           
      * exec cics readnext
      *       dataset     ('ELMSTR5')
      *       ridfld      (ws-elmstr5-key)
      *       into        (claim-master)
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            claim-master
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005641' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303035363431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 claim-master, 
                 DFHEIV12, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal or resp-dupkey
              continue
           else
              display ' error-elmstr5-readnext ' ws-response ' '
                 af-cert-no
              go to 0165-endbr
           end-if
           if (cl-company-cd-a4 not = af-company-cd)
              or (cl-cert-no-a4 not = af-cert-no)
              go to 0165-endbr
           end-if
           if (cl-cert-carrier = af-carrier)
              and (cl-cert-grouping = af-grouping)
              and (cl-cert-state    = af-state)
              and (cl-cert-account  = af-account)
              and (cl-cert-eff-dt   = af-dt)
              and (cl-cert-no-a4    = af-cert-no)
      *       move cl-incurred-dt      to dc-bin-date-1
      *       move pb-cert-eff-dt      to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-date-convert thru 9700-exit
      *       if (no-conversion-error)
      *          and (dc-elapsed-months > 11)
      *             cl-claim-status ' ' cl-total-paid-amt
                 move 'Y'              to ws-claim (a1)
      *       end-if
           end-if
           go to 0165-read-next
           .
       0165-endbr.
           if elmstr5-startbr
              
      * exec cics endbr
      *          dataset     ('ELMSTR5')
      *       end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005679' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0165-exit.
           exit.
       0166-check-for-ah-claim.
           move ws-elcert-orig-key to ws-elcert-key
           move ws-comp-cd             to ws-elmstr5-company-cd
           move ws-elcert-cert-no      to ws-elmstr5-cert-no
           move spaces                 to ws-elmstr5-startbr-sw
           
      * exec cics startbr
      *       dataset         ('ELMSTR5')
      *       ridfld          (ws-elmstr5-key)
      *       gteq
      *       resp            (ws-response)
      *    end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005691' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              set elmstr5-startbr to true
           else
              if (resp-notfnd)
                 or (resp-endfile)
                 go to 0166-exit
              else
                 display 'error-elmstr5-startbr '
                 ws-response ' ' ws-elcert-cert-no
              end-if
           end-if
           .
       0166-read-next.
           
      * exec cics readnext
      *       dataset     ('ELMSTR5')
      *       ridfld      (ws-elmstr5-key)
      *       into        (claim-master)
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            claim-master
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005710' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303035373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 claim-master, 
                 DFHEIV12, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal or resp-dupkey
              continue
           else
              display ' error-elmstr5-readnext ' ws-response ' '
                 cm-cert-no
              go to 0166-endbr
           end-if
           if (cl-company-cd-a4 not = ws-elcert-company-cd)
              or (cl-cert-no-a4 not = ws-elcert-cert-no)
              go to 0166-endbr
           end-if
           if (cl-cert-carrier = ws-elcert-carrier)
              and (cl-cert-grouping = ws-elcert-grouping)
              and (cl-cert-state    = ws-elcert-state)
              and (cl-cert-account  = ws-elcert-account)
              and (cl-cert-eff-dt   = ws-elcert-eff-dt)
              and (cl-cert-no-a4    = ws-elcert-cert-no)
      *       move cl-incurred-dt      to dc-bin-date-1
      *       move pb-cert-eff-dt      to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-date-convert thru 9700-exit
      *       if (no-conversion-error)
      *          and (dc-elapsed-months > 11)
      *             cl-claim-status ' ' cl-total-paid-amt
                 move 'Y'              to ws-claim (a1)
      *       end-if
           end-if
           go to 0166-read-next
           .
       0166-endbr.
           if elmstr5-startbr
              
      * exec cics endbr
      *          dataset     ('ELMSTR5')
      *       end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005748' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0166-exit.
           exit.
       0170-erpndb5.
      ***  this routine reads all the elcert  records that
      ***  match the acct#, last name & 1st init
      ***  and if it's not cancelled add to the accum table
           move spaces                 to ws-erpndb2-startbr-sw
           move low-values             to ws-elcert-eff-dt
                                          ws-elcert-cert-no
           
      * exec cics startbr
      *       dataset      ('ELCERT')
      *       ridfld       (ws-elcert-key)
      *       gteq
      *       resp         (ws-response)
      *    end-exec
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005762' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303035373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elcert-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              display ' error-erpndb5-start ' ws-response ' '
                 ws-elcert-key (2:19)
              go to 0170-exit
           end-if
           set erpndb2-startbr to true
           perform 0175-read-erpndb5   thru 0175-exit
           if not resp-normal
              display ' Error-erpndb5-1stread '
                 ws-passed-pend-key (2:19) ' '
                 ws-passed-pend-key (22:11)
              go to 0170-exit
           end-if
           perform 0180-accum-erpndb5  thru 0180-exit until
              (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
              or (end-of-erpndb5)
              or (not resp-normal)
      *    end-perform
           .
       0170-exit.
           exit.
       0175-READ-ERPNDB5.
           
      * exec cics readnext
      *       dataset    ('ELCERT')
      *       ridfld     (ws-elcert-key)
      *       into       (certificate-master)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV12
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005790' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303035373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV12, 
                 ws-elcert-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (resp-endfile)
              or (resp-notfnd)
              or (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
      *       or (ws-last-name <> ws-elcert-last-name)
              set end-of-erpndb5       to true
           else
              if not resp-normal
                 display ' error-erpndb5-readnext ' ws-response
                    ' ' ws-erpndb5-key (2:19)
                 set end-of-erpndb5    to true
              end-if
           end-if
           if not end-of-erpndb5
              if ws-cov-sw = 'P'
                 move cm-insured-last-name
                                       to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-elcert-last-name
              else
                 move cm-jt-last-name  to ws-name-in
                 perform 0650-set-last-name
                                       thru 0650-exit
                 move ws-name-out      to ws-elcert-last-name
              end-if
           end-if
           .
       0175-EXIT.
           EXIT.
       0180-accum-erpndb5.
           if ws-elcert-key = ws-elcert-orig-key
              go to 0180-read  *> I found myself
           end-if
           if ws-last-name <> ws-elcert-last-name
              go to 0180-read
           end-if
           if cm-lf-policy-pending or
              cm-ah-policy-pending
              continue
           else
              go to 0180-read  *> Will be on elaph
           end-if
           if ((ws-cov-sw = 'P')
              and (cm-insured-first-name(1:3) = ws-first-three))
                            or
              ((ws-cov-sw = 'S')
              and (cm-jt-first-name(1:3) = ws-first-three))
              continue
           else
              go to 0180-read
           end-if
           move spaces                 to ws-p5-last-name
                                          ws-p5-jnt-last-name
           move zeros                  to ws-p5-pri-curr-age
                                          ws-p5-cob-curr-age
           move ' '                    to ws-pend-match
           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
           end-if
           if (cm-lf-benefit-cd <> '00' and spaces)
              and (cm-lf-cancel-dt <> low-values)
              move '00'                to cm-lf-benefit-cd
           end-if
           if (cm-ah-benefit-cd <> '00' and spaces)
              and (cm-ah-cancel-dt <> low-values)
              move '00'                to cm-ah-benefit-cd
           end-if
           if ((cm-lf-benefit-cd = '00' or spaces)
              and (cm-ah-benefit-cd = '00' or spaces))
                       or
              (cm-entry-status = '5' or '9' or
                                'D' or 'V')
              go to 0180-read   *>  Must be cancelled or something
           end-if
           move cm-insured-last-name to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-p5-last-name
           move cm-jt-last-name   to ws-name-in
           perform 0650-set-last-name  thru 0650-exit
           move ws-name-out            to ws-p5-jnt-last-name
           perform 0710-calc-cur-p5-ages
                                    thru 0710-exit
           if ws-cov-sw = 'P'
              if (ws-age - ws-p5-pri-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'P'           to ws-eralph-prm-sec (a1)
                 move ws-p5-pri-curr-age
                                    to ws-eralph-age (a1)
                 move cm-insured-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if
           if ws-cov-sw = 'S'
              if (ws-age - ws-p5-cob-curr-age < 4 and > -4)
                 add +1 to a1
                 set pend-match to true
                 move 'Y'           to ws-pend (a1)
                 move 'C'           to ws-eralph-prm-sec (a1)
                 move ws-p5-cob-curr-age
                                    to ws-eralph-age (a1)
                 move cm-jt-first-name
                                    to ws-first-name (a1)
                 perform 0185-build-common
                                    thru 0185-exit
              end-if
           end-if
           .
       0180-read.
           perform 0175-read-erpndb5   thru 0175-exit
           .
       0180-exit.
           exit.
       0185-build-common.
           move ws-cov-sw              to ws-cov-type (a1)
           move ws-last-name           to ws-tbl-last-name (a1)
           move cm-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-dt (a1)
           end-if
           move cm-cert-no             to ws-eralph-cert  (a1)
           move cm-lf-benefit-cd     to ws-eralph-lf-cd (a1)
           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (lf-joint-ind = 'J')))
               and (cm-lf-benefit-cd <> '00' and '  ')
              perform 0500-get-lf-rem  thru 0500-exit
              move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
              compute ws-lf-tot-benefit =
                 ws-lf-tot-benefit + cp-remaining-amt
              move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
           else
              move zeros               to ws-eralph-lf-remamt (a1)
                                          ws-eralph-lf-remterm (a1)
           end-if
           if cm-lf-loan-expire-dt = low-values or spaces
              move spaces              to ws-lf-exp-dt (a1)
           else
              move cm-lf-loan-expire-dt   to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-lf-exp-dt (a1)
              end-if
           end-if
           move cm-ah-benefit-cd     to ws-eralph-ah-cd (a1)
           if ((ws-cov-sw = 'P')
                      or
              ((ws-cov-sw = 'S')
               and (ah-joint-ind = 'J')))
               and (cm-ah-benefit-cd <> '00' and '  ')
                 perform 0510-get-ah-rem
                                       thru 0510-exit
                 move cp-remaining-amt to ws-eralph-ah-remamt (a1)
                 compute ws-ah-tot-benefit =
                    ws-ah-tot-benefit + cm-ah-benefit-amt
                 move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
              move cm-ah-benefit-amt to ws-eralph-ah-amt (a1)
           else
              move zeros               to ws-eralph-ah-amt (a1)
                                          ws-eralph-ah-remamt (a1)
                                          ws-eralph-ah-remterm (a1)
           end-if
           if cm-ah-loan-expire-dt = low-values or spaces
              move spaces              to ws-ah-exp-dt (a1)
           else
              move cm-ah-loan-expire-dt   to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-date-convert thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to ws-ah-exp-dt (a1)
              end-if
           end-if
           move ';'                    to ws-delimiter (a1)
           .
       0185-exit.
           exit.
       0200-send-buffer.
           move ws-return-stuff        to ws-send-buf
      *    display 'SOCK06:sequence number  =', ws-seq-num.
      *    display 'SOCK06:send buffer      =', ws-send-buf(1:80).
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
           if return-code <= zero
              display 'SOCK06:send error ',
              go to 0200-socket-error
           end-if
           go to 0200-exit
           .
       0200-socket-error.
           if ws-seq-num <> 0
              display "SOCK06:did not complete"
           end-if
           .
       0200-exit.
           exit.
       0300-close-socket.
      *    display 'SOCK06:closing socket'.
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *    display 'SOCK06:done'
           .
       0300-exit.
           exit.
       0400-get-lf-code.
           move ws-comp-id             to ws-elcntl-key
           set ws-elcntl-lf-ben-cd     to true
           move ws-lf-ben-code         to ws-elcntl-hi-ben-cd
           move zeros                  to ws-elcntl-seq-no
           
      * exec cics read
      *       dataset     ('ELCNTL')
      *       into        (control-file)
      *       ridfld      (ws-elcntl-key)
      *       gteq
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00006023' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 control-file, 
                 DFHEIV11, 
                 ws-elcntl-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              perform varying c1 from +1 by +1 until
                 (c1 > +8)
                 or (cf-benefit-code (c1) = ws-lf-ben-code)
              end-perform
              if c1 < +9
                 MOVE CF-BENEFIT-ALPHA (c1)
                                       TO WS-lf-KIND
                 MOVE CF-SPECIAL-CALC-CD (c1)
                                       TO WS-lf-CALC-CD
                 MOVE CF-BENEFIT-DESCRIP (c1)
                                       TO WS-lf-BEN-DESCRIP
                 MOVE CF-LF-COVERAGE-TYPE (c1)
                                       TO WS-LF-COVERAGE-TYPE
                 MOVE CF-CO-EARNINGS-CALC (c1)
                                       TO WS-lf-EARNINGS-CALC
              end-if
           end-if
           .
       0400-exit.
           exit.
       0410-get-ah-code.
           move ws-comp-id             to ws-elcntl-key
           set ws-elcntl-ah-ben-cd     to true
           move ws-ah-ben-code         to ws-elcntl-hi-ben-cd
           move zeros                  to ws-elcntl-seq-no
           
      * exec cics read
      *       dataset     ('ELCNTL')
      *       into        (control-file)
      *       ridfld      (ws-elcntl-key)
      *       gteq
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00006056' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 control-file, 
                 DFHEIV11, 
                 ws-elcntl-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              perform varying c1 from +1 by +1 until
                 (c1 > +8)
                 or (cf-benefit-code (c1) = ws-ah-ben-code)
              end-perform
              if c1 < +9
                 MOVE CF-BENEFIT-ALPHA (c1)
                                       TO WS-ah-KIND
                 MOVE CF-SPECIAL-CALC-CD (c1)
                                       TO WS-ah-CALC-CD
                 MOVE CF-BENEFIT-DESCRIP (c1)
                                       TO WS-ah-BEN-DESCRIP
                 MOVE CF-LF-COVERAGE-TYPE (c1)
                                       TO WS-ah-COVERAGE-TYPE
                 MOVE CF-CO-EARNINGS-CALC (c1)
                                       TO WS-ah-EARNINGS-CALC
              end-if
           .
       0410-exit.
           exit.
       0500-get-lf-rem.
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
           MOVE WS-lf-EARNINGS-CALC    TO CP-EARNING-METHOD
                                          CP-RATING-METHOD
           MOVE WS-lf-CALC-CD          TO CP-SPECIAL-CALC-CD
           MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
           MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
           MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
           IF cm-lf-orig-term = 0
              MOVE 1                   TO CP-ORIGINAL-TERM
           ELSE
              MOVE cm-lf-orig-term     TO CP-ORIGINAL-TERM
           end-if
           MOVE cm-loan-term           TO CP-LOAN-TERM
           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
           MOVE '4'                    TO CP-REM-TERM-METHOD
           MOVE ws-comp-id             TO CP-COMPANY-ID
           MOVE ws-comp-cd             TO CP-COMPANY-CD
           PERFORM 9800-LINK-REM-TERM  thru 9800-exit
           MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
           MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
           MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
           MOVE cm-loan-apr            TO CP-LOAN-APR
           MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
           MOVE cm-rate-class          TO CP-CLASS-CODE
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           perform 9500-link-rem-amt   thru 9500-exit
           .
       0500-exit.
           exit.
       0510-get-ah-rem.
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-ah-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
           MOVE WS-ah-EARNINGS-CALC    TO CP-EARNING-METHOD
                                          CP-RATING-METHOD
           MOVE WS-ah-CALC-CD          TO CP-SPECIAL-CALC-CD
           MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
           MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
           MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
           MOVE cm-ah-orig-term        TO CP-ORIGINAL-TERM
           MOVE cm-loan-term           TO CP-LOAN-TERM
           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
           MOVE '4'                    TO CP-REM-TERM-METHOD
           MOVE ws-comp-id             TO CP-COMPANY-ID
           MOVE ws-comp-cd             TO CP-COMPANY-CD
           PERFORM 9800-LINK-REM-TERM  thru 9800-exit
           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
           compute cp-remaining-amt =
              cm-ah-benefit-amt * cp-remaining-term-3
      *    perform 9500-link-rem-amt   thru 9500-exit
           .
       0510-exit.
           exit.
       0600-get-elcert.
           
      * exec cics read
      *       dataset   ('ELCERT')
      *       ridfld    (ws-elcert-key)
      *       into      (certificate-master)
      *       resp      (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006140' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV11, 
                 ws-elcert-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              display ' bad read on elcert ' ws-response
              go to 0600-exit
           end-if
           if cm-lf-benefit-cd not = '00' and spaces
              move cm-lf-benefit-cd    to ws-lf-ben-code
              perform 0400-get-lf-code thru 0400-exit
           end-if
           if cm-ah-benefit-cd not = '00' and spaces
              move cm-ah-benefit-cd    to ws-ah-ben-code
              perform 0410-get-ah-code thru 0410-exit
           end-if
           .
       0600-exit.
           exit.
       0650-set-last-name.
           move ws-name-in             to ws-name-out
           perform varying n1 from +13 by -1 until n1 < +3
              if (ws-name-in (n1:3) = ' SR' or ' JR' or ' II' or
                 ' IV' or ' VI' or ' I ' or ' V ')
                 or (ws-name-in (n1:4) = ' III')
                 or (ws-name-in (n1:5) = ' IIII')
                 or (ws-name-in (14:2) = ' I')
                 or (ws-name-in (14:2) = ' V')
                 move ws-name-in (1:n1 - 1)
                                       to ws-name-out
                 move +3               to n1
              end-if
           end-perform
           .
       0650-exit.
           exit.
       0700-calc-cur-cm-ages.
           move zeros                  to ws-cm-pri-curr-age
                                          ws-cm-cob-curr-age
           if (cm-insured-issue-age not numeric)
                     or
              (cm-insured-issue-age = zeros)
              move 42                  to cm-insured-issue-age
           end-if
           move cm-insured-issue-age   to ws-cm-pri-curr-age
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-issue-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-cm-pri-curr-age
           end-if
           .
       0700-joint-stuff.
           if cm-jt-last-name = spaces
              and cm-jt-first-name = spaces
              go to 0700-exit
           end-if
           if (cm-insured-joint-age not numeric)
              move zeros               to cm-insured-joint-age
           end-if
           move cm-insured-joint-age   to ws-cm-cob-curr-age
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-joint-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-cm-cob-curr-age
           end-if
           .
       0700-exit.
           exit.
       0710-calc-cur-p5-ages.
           move zeros                  to ws-p5-pri-curr-age
                                          ws-p5-cob-curr-age
           if (cm-insured-issue-age not numeric)
                     or
              (cm-insured-issue-age = zeros)
              move 42                  to cm-insured-issue-age
           end-if
           move cm-insured-issue-age   to ws-p5-pri-curr-age
      *    if p5-i-birthday <> low-values
      *       move p5-i-birthday       to dc-bin-date-1
      *       move ws-current-bin-dt   to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-DATE-CONVERT
      *                                thru 9700-exit
      *       if no-conversion-error
      *          compute ws-work-age = dc-elapsed-months / 12
      *          move ws-work-age      to ws-p5-pri-curr-age
      *          go to 0710-joint-stuff
      *       end-if
      *    end-if
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-issue-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-p5-pri-curr-age
           end-if
           .
       0710-joint-stuff.
           if cm-jt-last-name = spaces
              and cm-jt-first-name = spaces
              go to 0710-exit
           end-if
           if (cm-insured-joint-age not numeric)
              move zeros               to cm-insured-joint-age
           end-if
           move cm-insured-joint-age   to ws-p5-cob-curr-age
      *    if p5-i-joint-birthday <> low-values
      *       move p5-i-joint-birthday to dc-bin-date-1
      *       move ws-current-bin-dt   to dc-bin-date-2
      *       move '1'                 to dc-option-code
      *       perform 9700-DATE-CONVERT
      *                                thru 9700-exit
      *       if no-conversion-error
      *          compute ws-work-age = dc-elapsed-months / 12
      *          move ws-work-age      to ws-p5-cob-curr-age
      *          go to 0710-exit
      *       end-if
      *    end-if
           move cm-cert-eff-dt         to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age =
                 cm-insured-joint-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-p5-cob-curr-age
           end-if
           .
       0710-exit.
           exit.
       0720-calc-cur-alph-age.
           move zeros                  to ws-alph-curr-age
           if (af-age not numeric)
                     or
              (af-age = zeros)
              move 42                  to af-age
           end-if
           move af-age                 to ws-alph-curr-age
           move af-dt                  to dc-bin-date-1
           move ws-current-bin-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-DATE-CONVERT   thru 9700-exit
           if no-conversion-error
              compute ws-work-age = af-age + (dc-elapsed-months / 12)
              move ws-work-age         to ws-alph-curr-age
           end-if
           .
       0720-exit.
           exit.
       9500-LINK-REM-AMT.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELRAMT')
      *        COMMAREA  (CALCULATION-PASS-AREA)
      *        LENGTH    (CP-COMM-LENGTH)
      *    END-EXEC
           MOVE 'ELRAMT' TO DFHEIV1
      *    MOVE '."C                   (   #00006302' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       9500-EXIT.
           EXIT.
       9700-DATE-CONVERT.
           
      * EXEC CICS LINK
      *         PROGRAM  ('ELDATCV')
      *         COMMAREA (DATE-CONVERSION-DATA)
      *         LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006311' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
            EXIT.
       9800-LINK-REM-TERM.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELRTRM')
      *        COMMAREA  (CALCULATION-PASS-AREA)
      *        LENGTH    (CP-COMM-LENGTH)
      *    END-EXEC
           MOVE 'ELRTRM' TO DFHEIV1
      *    MOVE '."C                   (   #00006319' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       9800-EXIT.
           EXIT.
       ABEND-PGM.
      *                                COPY ELCABEND.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELCABEND.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD 2.002
00006 *                                                               *
00007 *                THIS SECTION DISPLAYS THE NECESSARY MESSAGES   *
00008 *            AND THEN ABENDS.                                   *
CIDMOD*                                                               *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCABEND                        *
CIDMOD*                                                               *
00009 *****************************************************************
00010 *APS-010.
00011      DISPLAY WS-ABEND-MESSAGE.
00012      DISPLAY WS-ABEND-MESSAGE UPON CONSOLE.
00013
00014      IF WS-ABEND-FILE-STATUS NOT = ZERO
00015          DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS
00016          DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS
00017                                  UPON CONSOLE.
00018
00019      IF WS-RETURN-CODE NOT = ZERO
00020          DISPLAY 'RETURN CODE = '  WS-RETURN-CODE
00021          DISPLAY 'RETURN CODE = '  WS-RETURN-CODE
00022                                  UPON CONSOLE.
00023
00024      DISPLAY 'PROGRAM WILL NOW ABEND **************'
00025      DISPLAY 'PROGRAM WILL NOW ABEND **************'
00026                                  UPON CONSOLE.
00027
00028      DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.
           CALL 'ABORTME'.
00029
00030  APS-EXIT.
00031      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK17' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK17' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
